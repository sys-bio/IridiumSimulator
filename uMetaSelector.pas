unit uMetaSelector;

{ The experiment selector, shared by every analysis panel that can be
  driven by a metadata block.

  Each panel needs the same control — a dropdown of the experiments this
  model defines for that panel's task kind, with a '—' row that returns
  the user's own settings — and the same behaviour around it: preserve
  the selection across a re-parse, refuse to apply an unusable
  experiment while still showing it and saying why, and suppress the
  change event while the list is being rebuilt.

  What differs between panels is only what "apply" means, which is why
  that part is a callback and everything else lives here. This was three
  near-copies before; the copies drifted, and a bug in the '—' semantics
  had to be found and fixed twice.

  The helper owns a small TLayout strip holding the label and the combo.
  A panel that needs more in that strip (the steady-state panel shows a
  summary of what Compute will apply, since it has no controls a preset
  could fill) adds controls to Host and raises its Height. }

interface

uses
  System.SysUtils, System.Classes, System.Types,
  FMX.Types, FMX.Controls, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.Dialogs,
  uMetaExperiments;

type
  { AWasUnset is True when the '—' row was selected immediately before
    this apply — that is, when everything now on the panel is the user's
    own work and should be re-captured before the preset overwrites it. }
  TMetaApplyEvent = procedure(AExp: TMetaExperiment;
                              AWasUnset: Boolean) of object;

  TMetaExperimentSelector = class
  private
    FHost:     TLayout;
    FCaption:  TLabel;
    FCombo:    TComboBox;
    FLabels:   TStringList;   { combo row -> experiment label; row 0 = '' }
    FActive:   string;
    FKind:     TMetaExperimentKind;
    FSuppress: Boolean;
    FOnApply:   TMetaApplyEvent;
    FOnRestore: TNotifyEvent;
    FGetSet:    TFunc<TMetaExperimentSet>;
    procedure DoComboChange(Sender: TObject);
    function  CurrentSet: TMetaExperimentSet;
  public
    { AOwner owns the created controls (pass the frame). AGetSet is how
      the helper reaches the current experiment set — a function rather
      than a stored reference because the set is rebuilt wholesale on
      every re-parse. }
    constructor Create(AOwner: TComponent; AKind: TMetaExperimentKind;
                       const AGetSet: TFunc<TMetaExperimentSet>);
    destructor  Destroy; override;

    { Put the strip into AParent immediately above ASibling, so it reads
      before the controls it fills in. }
    procedure Place(AParent: TFmxObject; ASibling: TControl);

    { Refill from ASet, preserving the current selection BY LABEL — the
      experiment objects are rebuilt on every re-parse, so an index or a
      reference would go stale. Hides the strip when this model defines
      no experiments of this kind: an empty dropdown on every ordinary
      model is clutter that explains nothing. }
    procedure Rebuild(ASet: TMetaExperimentSet);

    { Select and apply the first usable experiment of this kind. What a
      model open does. }
    procedure ApplyFirstUsable(ASet: TMetaExperimentSet);

    { Select and apply the experiment named ALabel, exactly as picking it
      from the dropdown would. False when this set has no usable
      experiment of that label — the caller decides what to say, since
      the reason (renamed, deleted, or unusable) is theirs to explain.
      By label, never by index: the set is rebuilt on every re-parse. }
    function  ApplyLabel(ASet: TMetaExperimentSet;
                         const ALabel: string): Boolean;

    { Show ALabel as the selection without firing OnApply. For a panel
      that has just applied an experiment by another route. }
    procedure ShowLabel(const ALabel: string);

    { The controls no longer describe the named experiment, so stop
      claiming they do. Silent — this is the '—' state arising on its
      own, not the user choosing it, so OnRestore must NOT fire. }
    procedure MarkDiverged;

    { The selected experiment, or nil for '—' / unknown. }
    function  ActiveExperiment: TMetaExperiment;

    property ActiveLabel: string read FActive;
    { The strip. Add extra controls here and raise Height to suit. }
    property Host: TLayout read FHost;
    property Caption: TLabel read FCaption;
    { Set around programmatic control changes so the panel's own edit
      handlers do not mistake them for the user diverging. }
    property Suppressed: Boolean read FSuppress write FSuppress;

    property OnApply:   TMetaApplyEvent read FOnApply   write FOnApply;
    property OnRestore: TNotifyEvent    read FOnRestore write FOnRestore;
  end;

implementation

const
  { Row 0. Shown selected whenever the controls do not match any named
    experiment — which happens by itself the moment the user edits one —
    and selectable to put their own settings back. }
  NO_EXPERIMENT_ROW = '—  (my own settings)';

constructor TMetaExperimentSelector.Create(AOwner: TComponent;
  AKind: TMetaExperimentKind; const AGetSet: TFunc<TMetaExperimentSet>);
begin
  inherited Create;
  FKind   := AKind;
  FGetSet := AGetSet;
  FLabels := TStringList.Create;

  FHost := TLayout.Create(AOwner);
  FHost.Height  := 52;
  FHost.Visible := False;

  FCaption := TLabel.Create(AOwner);
  FCaption.Parent := FHost;
  FCaption.Align  := TAlignLayout.Top;
  FCaption.Height := 18;
  FCaption.Margins.Rect := RectF(8, 2, 8, 0);
  FCaption.Text := 'Experiment (from the model file)';

  FCombo := TComboBox.Create(AOwner);
  FCombo.Parent := FHost;
  FCombo.Align  := TAlignLayout.Top;
  FCombo.Height := 26;
  FCombo.Margins.Rect := RectF(8, 0, 8, 4);
  FCombo.OnChange := DoComboChange;
end;

destructor TMetaExperimentSelector.Destroy;
begin
  { The controls belong to the frame that owns them; only the mapping is
    ours. }
  FLabels.Free;
  inherited;
end;

procedure TMetaExperimentSelector.Place(AParent: TFmxObject;
  ASibling: TControl);
begin
  if AParent = nil then Exit;
  FHost.Parent := AParent;
  FHost.Align  := TAlignLayout.Top;
  { Align=Top stacks siblings in child order, and the strip was appended
    after every existing control; move it in front of the one it
    describes. }
  if ASibling <> nil then
    FHost.Index := ASibling.Index;
end;

function TMetaExperimentSelector.CurrentSet: TMetaExperimentSet;
begin
  if Assigned(FGetSet) then
    Result := FGetSet()
  else
    Result := nil;
end;

procedure TMetaExperimentSelector.Rebuild(ASet: TMetaExperimentSet);
var
  Exps: TArray<TMetaExperiment>;
  E:    TMetaExperiment;
  Keep: string;
  Idx:  Integer;
begin
  if ASet = nil then
    Exps := []
  else
    Exps := ASet.ForKind(FKind);

  Keep := FActive;

  FSuppress := True;
  try
    FCombo.BeginUpdate;
    try
      FCombo.Clear;
      FLabels.Clear;

      FCombo.Items.Add(NO_EXPERIMENT_ROW);
      FLabels.Add('');

      for E in Exps do
      begin
        { Unusable experiments are listed, not hidden. Naming what was
          skipped and why is conformance C5, and a row that says so is a
          better warning than a line in a log — it appears where the user
          looks for the thing that is missing. }
        FCombo.Items.Add(E.DisplayText);
        FLabels.Add(E.LabelText);
      end;
    finally
      FCombo.EndUpdate;
    end;

    Idx := FLabels.IndexOf(Keep);
    if (Keep = '') or (Idx < 0) then
    begin
      Idx     := 0;
      FActive := '';
    end;
    FCombo.ItemIndex := Idx;
  finally
    FSuppress := False;
  end;

  FHost.Visible := Length(Exps) > 0;
end;

procedure TMetaExperimentSelector.ApplyFirstUsable(ASet: TMetaExperimentSet);
var
  Exp:      TMetaExperiment;
  WasUnset: Boolean;
begin
  if ASet = nil then Exit;
  Exp := ASet.FirstUsable(FKind);
  if Exp = nil then Exit;

  WasUnset := FActive = '';
  ShowLabel(Exp.LabelText);
  if Assigned(FOnApply) then
    FOnApply(Exp, WasUnset);
end;

function TMetaExperimentSelector.ApplyLabel(ASet: TMetaExperimentSet;
  const ALabel: string): Boolean;
var
  Exp:      TMetaExperiment;
  WasUnset: Boolean;
begin
  Result := False;
  if (ASet = nil) or (ALabel = '') then Exit;

  Exp := ASet.FindByLabel(ALabel);
  if (Exp = nil) or (Exp.Kind <> FKind) or (not Exp.Usable) then Exit;

  WasUnset := FActive = '';
  ShowLabel(ALabel);
  if Assigned(FOnApply) then
    FOnApply(Exp, WasUnset);
  Result := True;
end;

procedure TMetaExperimentSelector.ShowLabel(const ALabel: string);
var
  Idx: Integer;
begin
  FActive := ALabel;
  FSuppress := True;
  try
    Idx := FLabels.IndexOf(ALabel);
    if Idx < 0 then Idx := 0;
    FCombo.ItemIndex := Idx;
  finally
    FSuppress := False;
  end;
end;

procedure TMetaExperimentSelector.MarkDiverged;
begin
  if FActive = '' then Exit;
  FActive := '';
  FSuppress := True;
  try
    FCombo.ItemIndex := 0;
  finally
    FSuppress := False;
  end;
end;

function TMetaExperimentSelector.ActiveExperiment: TMetaExperiment;
var
  ASet: TMetaExperimentSet;
begin
  Result := nil;
  if FActive = '' then Exit;
  ASet := CurrentSet;
  if ASet = nil then Exit;
  Result := ASet.FindByLabel(FActive);
end;

procedure TMetaExperimentSelector.DoComboChange(Sender: TObject);
var
  Idx:      Integer;
  ASet:     TMetaExperimentSet;
  Exp:      TMetaExperiment;
  WasUnset: Boolean;
begin
  if FSuppress then Exit;

  Idx := FCombo.ItemIndex;
  if (Idx < 0) or (Idx >= FLabels.Count) then Exit;

  { Row 0: give the user their own settings back. }
  if FLabels[Idx] = '' then
  begin
    FActive := '';
    if Assigned(FOnRestore) then
      FOnRestore(Self);
    Exit;
  end;

  ASet := CurrentSet;
  if ASet = nil then Exit;
  Exp := ASet.FindByLabel(FLabels[Idx]);
  if Exp = nil then Exit;

  if not Exp.Usable then
  begin
    { Listed so it can be explained, never applied — its fields are not
      trustworthy. Put the selection back rather than leaving it showing
      a row that did nothing. }
    ShowMessage('This experiment cannot be used: ' + Exp.Reason);
    ShowLabel(FActive);
    Exit;
  end;

  WasUnset := FActive = '';
  FActive  := Exp.LabelText;
  if Assigned(FOnApply) then
    FOnApply(Exp, WasUnset);
end;

end.
