    unit uFrameSliderContainer;

{ Slider container frame with a parameter-picker panel on the left.
  Left panel:  TEdit (EditFilter) + TListBox (ListBoxParams).
  Right panel: TVertScrollBox (VertScrollBox1) containing one row per
               active slider.

  Workflow
  --------
  1. Host calls LoadParams(names, values) once after a model is loaded.
     This populates the listbox and stores the initial values; no sliders
     are created yet.
  2. User types in EditFilter to narrow the list, then clicks a name to
     add that slider.  The parameter is NOT removed from the listbox so
     the full catalogue is always visible; a second click on an already-
     active parameter is silently ignored.
  3. Each slider row has a small "x" button on its left edge.  Clicking it
     removes just that row.
  4. Right-clicking the parameter label still opens the range-edit dialog.
  5. The host may still call BuildSliders / ClearSliders / RefreshValues
     as before.  BuildSliders now also reloads the listbox from the names
     supplied, replacing any previous catalogue.

  Continuous vs release-only updates
  ----------------------------------
  The OnSliderChanged event fires while the user moves a slider. By
  default this is continuous (every thumb position change), which suits
  time course and parameter scan frames. Setting ReleaseOnlyMode := True
  switches new and existing rows to fire OnSliderChanged only once, on
  release -- the underlying mechanism is FMX TTrackBar.Tracking. The
  steady state frame uses this mode because steady-state solves are less
  robust than time-course integration and we don't want to hammer them
  continuously. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.ListBox, FMX.Objects, System.Generics.Defaults;

type
  TSliderChangedEvent = procedure(Sender: TObject; const ASliderString: string;
                                  const AValue: Single) of object;

  TSliderRow = record
    Layout:    TLayout;
    RemoveBtn: TButton;
    Lbl:       TLabel;
    Track:     TTrackBar;
    ParamName: string;
  end;

  TFrameSliderContainer = class(TFrame)
    VertScrollBox1: TVertScrollBox;
    LayoutLeft:     TLayout;
    Splitter1:      TSplitter;
    EditFilter:     TEdit;
    ListBoxParams:  TListBox;
    Layout1: TLayout;
    btnAddAllParameters: TButton;
    btnResetParameters: TButton;
  private
    FRows:            TArray<TSliderRow>;
    FOwned:           TObjectList<TComponent>;
    FOnSliderChanged: TSliderChangedEvent;
    FReleaseOnlyMode: Boolean;

    { Full catalogue: parallel arrays kept in sync }
    { Full catalogue: parallel arrays kept in sync. FAllParamNames[i] and
     FAllParamValues[i] MUST refer to the same parameter — any mutation
     that reorders one must reorder the other identically. }
    FAllParamNames:  TArray<string>;
    FAllParamValues: TArray<Double>;

    { Optional "locked" parameter -- shown in the listbox but greyed and
      not clickable, and if it has an active slider that slider's track
      is disabled. Used by the parameter-scan frame to lock the
      currently-scanning parameter. Empty string = no lock. }
    FLockedParam:    string;

    { -- internal helpers -- }
    procedure RebuildListBox(const AFilter: string);
    procedure AddSliderRow(const AName: string; const AInitValue: Double);
    procedure RemoveSliderRow(const AParamName: string);
    function  RowIndexOf(const AParamName: string): Integer;
    function  InitialValueOf(const AName: string): Double;
    function  IsActive(const AName: string): Boolean;

    function  GetParamPanelVisible: Boolean;
    procedure SetReleaseOnlyMode(Value: Boolean);

    { -- widget callbacks -- }
    procedure DoTrackBarChange(Sender: TObject);
    procedure DoLabelMouseDown(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Single);
    procedure DoRemoveBtnClick(Sender: TObject);
    procedure DoListBoxItemClick(const Sender: TCustomListBox;
                                 const Item: TListBoxItem);
    procedure DoFilterChange(Sender: TObject);
    procedure DoBtnAddAllClick(Sender: TObject);
    procedure DoBtnResetClick(Sender: TObject);

    function  FormatLabelText(const AName: string;
                              const AValue: Single): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { -- primary API -- }

    { Load (or replace) the full parameter catalogue. Populates the
      listbox; does NOT create any sliders -- the user picks them. }
    procedure LoadParams(const AParamNames:  TArray<string>;
                         const AInitValues:  TArray<Double>);

    { Legacy bulk-build: creates one slider for every parameter, just as
      before. Also refreshes the listbox catalogue. }
    procedure BuildSliders(const AParamNames:   TArray<string>;
                           const AInitialValues: TArray<Double>;
                           const AMaxValue:      Single = 1.0);

    procedure ClearSliders;

    procedure ToggleParamPanel;

    { Update slider thumb positions without firing OnSliderChanged. }
    procedure RefreshValues(const ANames:  TArray<string>;
                            const AValues: TArray<Double>);

    procedure GetSliderValues(out ANames: TArray<string>; out AValues: TArray<Double>);

    { Lock (disable) the slider/listbox entry for a single parameter, and
      simultaneously release any previous lock. Pass '' to clear.
      Safe to call whether the parameter has an active slider row yet
      or not -- if it doesn't, the lock simply takes effect in the
      listbox (greyed, non-clickable) and on any future row created for
      that name. }
    procedure SetLockedParam(const AParamName: string);

    property OnSliderChanged: TSliderChangedEvent read  FOnSliderChanged
                                                  write FOnSliderChanged;

    { When False (default): OnSliderChanged fires continuously while the
      user drags a slider. When True: OnSliderChanged fires only once,
      when the user releases. Setting this property updates both newly-
      created rows and any existing ones. }
    property ReleaseOnlyMode: Boolean read FReleaseOnlyMode
                                      write SetReleaseOnlyMode;

    property ParamPanelVisible: Boolean read GetParamPanelVisible;
    property LockedParam: string read FLockedParam;
  end;

implementation

{$R *.fmx}

const
  ROW_H   = 44;
  BTN_W   = 28;   { width of the left-side button column }
  BTN_H   = 18;   { visual height of the button itself   }

procedure SetTrackRange(ATrack: TTrackBar; AMin, AMax, AValue: Single);
const
  STEPS_ACROSS_RANGE = 200;
begin
  ATrack.BeginUpdate;
  try
    ATrack.Min := AMin;
    ATrack.Max := AMax;
    ATrack.Value := EnsureRange(AValue, AMin, AMax);
    ATrack.Frequency := (AMax - AMin) / STEPS_ACROSS_RANGE;
  finally
    ATrack.EndUpdate;
  end;
end;

{ -- modal range-edit dialog --------------------------------------------- }

function PromptForRange(const ATitle: string;
                        var AMin, AMax: Single): Boolean;
var
  F:            TForm;
  LblMin:       TLabel;
  LblMax:       TLabel;
  EdMin:        TEdit;
  EdMax:        TEdit;
  BtnOK:        TButton;
  BtnCancel:    TButton;
  V1, V2:       Single;
begin
  Result := False;
  F := TForm.CreateNew(nil);
  try
    F.Caption      := ATitle;
    F.BorderStyle  := TFmxFormBorderStyle.None;
    F.BorderIcons  := [TBorderIcon.biSystemMenu];
    F.Position     := TFormPosition.MainFormCenter;
    F.ClientWidth  := 260;
    F.ClientHeight := 90;

    LblMin             := TLabel.Create(F);
    LblMin.Parent      := F;
    LblMin.Position.X  := 16;
    LblMin.Position.Y  := 16;
    LblMin.Width       := 40;
    LblMin.Height      := 18;
    LblMin.Text        := 'Min:';

    EdMin             := TEdit.Create(F);
    EdMin.Parent      := F;
    EdMin.Position.X  := 60;
    EdMin.Position.Y  := 12;
    EdMin.Width       := 180;
    EdMin.Height      := 26;
    EdMin.Text        := FloatToStr(AMin);

    LblMax             := TLabel.Create(F);
    LblMax.Parent      := F;
    LblMax.Position.X  := 16;
    LblMax.Position.Y  := 50;
    LblMax.Width       := 40;
    LblMax.Height      := 18;
    LblMax.Text        := 'Max:';

    EdMax             := TEdit.Create(F);
    EdMax.Parent      := F;
    EdMax.Position.X  := 60;
    EdMax.Position.Y  := 46;
    EdMax.Width       := 180;
    EdMax.Height      := 26;
    EdMax.Text        := FloatToStr(AMax);

    BtnCancel             := TButton.Create(F);
    BtnCancel.Parent      := F;
    BtnCancel.Position.X  := 80;
    BtnCancel.Position.Y  := 88;
    BtnCancel.Width       := 75;
    BtnCancel.Height      := 26;
    BtnCancel.Text        := 'Cancel';
    BtnCancel.ModalResult := mrCancel;
    BtnCancel.Cancel      := True;

    BtnOK             := TButton.Create(F);
    BtnOK.Parent      := F;
    BtnOK.Position.X  := 165;
    BtnOK.Position.Y  := 88;
    BtnOK.Width       := 75;
    BtnOK.Height      := 26;
    BtnOK.Text        := 'OK';
    BtnOK.ModalResult := mrOk;
    BtnOK.Default     := True;

    if F.ShowModal <> mrOk then Exit;

    if not TryStrToFloat(EdMin.Text, V1) or
       not TryStrToFloat(EdMax.Text, V2) then
    begin
      ShowMessage('Please enter valid numbers for Min and Max.');
      Exit;
    end;

    if V1 >= V2 then
    begin
      ShowMessage('Min must be less than Max.');
      Exit;
    end;

    AMin   := V1;
    AMax   := V2;
    Result := True;
  finally
    F.Free;
  end;
end;

function TFrameSliderContainer.GetParamPanelVisible: Boolean;
begin
  Result := (ControlsCount > 0) and Controls[0].Visible;
end;

procedure TFrameSliderContainer.ToggleParamPanel;
begin
  for var i := 0 to Self.ControlsCount - 1 do
  begin
    Self.Controls[i].Visible := not Self.Controls[i].Visible;
  end;
end;

procedure TFrameSliderContainer.SetReleaseOnlyMode(Value: Boolean);
var
  I: Integer;
begin
  if FReleaseOnlyMode = Value then Exit;
  FReleaseOnlyMode := Value;
  { Reconfigure any existing rows so the mode applies consistently. }
  for I := 0 to High(FRows) do
    if FRows[I].Track <> nil then
      FRows[I].Track.Tracking := not Value;
end;

{ -- TFrameSliderContainer ----------------------------------------------- }

constructor TFrameSliderContainer.Create(AOwner: TComponent);
begin
  inherited;
  { Panel starts hidden }
  for var i := 0 to ControlsCount - 1 do
    Controls[i].Visible := False;

  FOwned := TObjectList<TComponent>.Create(True);

  EditFilter.TextPrompt  := 'Filter parameters...';
  EditFilter.OnChangeTracking := DoFilterChange;

  ListBoxParams.OnItemClick := DoListBoxItemClick;

  btnAddAllParameters.OnClick := DoBtnAddAllClick;
  btnResetParameters.OnClick  := DoBtnResetClick;
end;

destructor TFrameSliderContainer.Destroy;
begin
  FOwned.Free;
  inherited;
end;

{ -- formatting ---------------------------------------------------------- }

function TFrameSliderContainer.FormatLabelText(const AName: string;
  const AValue: Single): string;
begin
  Result := Format('%s = %.4g', [AName, AValue]);
end;

{ -- catalogue helpers --------------------------------------------------- }

function TFrameSliderContainer.InitialValueOf(const AName: string): Double;
var
  I: Integer;
begin
  Result := 0.0;
  for I := 0 to High(FAllParamNames) do
    if FAllParamNames[I] = AName then
    begin
      if I < Length(FAllParamValues) then
        Result := FAllParamValues[I];
      Exit;
    end;
end;

function TFrameSliderContainer.IsActive(const AName: string): Boolean;
begin
  Result := RowIndexOf(AName) >= 0;
end;

function TFrameSliderContainer.RowIndexOf(const AParamName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FRows) do
    if FRows[I].ParamName = AParamName then
    begin
      Result := I;
      Exit;
    end;
end;

{ -- listbox population -------------------------------------------------- }

procedure TFrameSliderContainer.RebuildListBox(const AFilter: string);
var
  I:    Integer;
  Name: string;
  Lo:   string;
  Item: TListBoxItem;
begin
  Lo := AFilter.ToLower;

  ListBoxParams.BeginUpdate;
  try
    ListBoxParams.Clear;
    for I := 0 to High(FAllParamNames) do
    begin
      Name := FAllParamNames[I];
      if (Lo <> '') and (Pos(Lo, Name.ToLower) = 0) then
        Continue;

      Item        := TListBoxItem.Create(ListBoxParams);
      Item.Text   := Name;
      Item.Parent := ListBoxParams;

      { Grey out names that already have an active slider, or that are
        the currently-locked parameter. }
      if IsActive(Name) or
         ((FLockedParam <> '') and SameText(Name, FLockedParam)) then
        Item.TextSettings.FontColor := TAlphaColorRec.Gray
      else
        Item.TextSettings.FontColor := TAlphaColorRec.Null;
    end;
  finally
    ListBoxParams.EndUpdate;
  end;
end;

{ -- slider row construction --------------------------------------------- }

procedure TFrameSliderContainer.AddSliderRow(const AName: string;
  const AInitValue: Double);
var
  Row:      TSliderRow;
  RangeMax: Single;
  RangeMin: Single;
const
  ROW_H   = 44;
  BTN_W   = 24;
begin
  if IsActive(AName) then Exit;

  { Range: x10 / /10 around initial value.
    Guard against zero: fall back to [0, 1]. }
  if AInitValue = 0.0 then
  begin
    RangeMin := 0.0;
    RangeMax := 1.0;
  end
  else if AInitValue > 0.0 then
  begin
    RangeMin := AInitValue / 10.0;
    RangeMax := AInitValue * 10.0;
  end
  else
  begin
    RangeMin := AInitValue * 10.0;
    RangeMax := AInitValue / 10.0;
  end;

  Row.ParamName := AName;

  { -- outer layout -- }
  Row.Layout                := TLayout.Create(Self);
  Row.Layout.Parent         := VertScrollBox1;
  Row.Layout.Align          := TAlignLayout.Top;
  Row.Layout.Height         := ROW_H;
  Row.Layout.Margins.Top    := 4;
  Row.Layout.Margins.Left   := 4;
  Row.Layout.Margins.Right  := 8;

  var BtnLayout := TLayout.Create(Self);
  BtnLayout.Parent        := Row.Layout;
  BtnLayout.Align         := TAlignLayout.Left;
  BtnLayout.Width         := BTN_W;
  BtnLayout.Margins.Right := 4;

  Row.RemoveBtn            := TButton.Create(Self);
  Row.RemoveBtn.Parent     := BtnLayout;
  Row.RemoveBtn.Align      := TAlignLayout.None;
  Row.RemoveBtn.Width      := BTN_W - 4;
  Row.RemoveBtn.Height     := BTN_H;
  Row.RemoveBtn.Position.X := 0;
  Row.RemoveBtn.Position.Y := (Row.Layout.Height - BTN_H) - 27;
  Row.RemoveBtn.Text       := 'x';
  Row.RemoveBtn.TagString  := AName;
  Row.RemoveBtn.Hint       := 'Delete slider';
  Row.RemoveBtn.OnClick    := DoRemoveBtnClick;

  { -- inner layout for label + trackbar -- }
  Row.Lbl             := TLabel.Create(Self);
  Row.Lbl.Parent      := Row.Layout;
  Row.Lbl.Align       := TAlignLayout.Top;
  Row.Lbl.Height      := 20;
  Row.Lbl.HitTest     := True;
  Row.Lbl.Cursor      := crHandPoint;
  Row.Lbl.Hint        := 'Right-click to edit min/max';
  Row.Lbl.ShowHint    := True;
  Row.Lbl.TextSettings.HorzAlign := TTextAlign.Trailing;
  Row.Lbl.Text        := FormatLabelText(AName, AInitValue);
  Row.Lbl.TagString   := AName;
  Row.Lbl.OnMouseDown := DoLabelMouseDown;

  Row.Track             := TTrackBar.Create(Self);
  Row.Track.Parent      := Row.Layout;
  Row.Track.Align       := TAlignLayout.Client;
  Row.Track.Margins.Top := -8;
  //Row.Track.Min         := RangeMin;
  //Row.Track.Max         := RangeMax;
  //Row.Track.Value       := AInitValue;
  //Row.Track.Frequency := (RangeMax - RangeMin) / 200;

  SetTrackRange(Row.Track, RangeMin, RangeMax, AInitValue);
  Row.Track.Tracking    := not FReleaseOnlyMode;
  Row.Track.OnChange    := DoTrackBarChange;

  { cross-link label <-> trackbar for event handlers }
  Row.Track.TagObject := Row.Lbl;
  Row.Lbl.TagObject   := Row.Track;

  FOwned.Add(Row.Layout);

  SetLength(FRows, Length(FRows) + 1);
  FRows[High(FRows)] := Row;

  { If this row corresponds to the currently-locked parameter, disable
    its trackbar so the user can see (and not change) the value. }
  if (FLockedParam <> '') and SameText(AName, FLockedParam) then
    Row.Track.Enabled := False;
end;

{ -- slider row removal -------------------------------------------------- }

procedure TFrameSliderContainer.RemoveSliderRow(const AParamName: string);
var
  Idx: Integer;
  I:   Integer;
begin
  Idx := RowIndexOf(AParamName);
  if Idx < 0 then Exit;

  { Shift focus to the scrollbox before destroying anything.
    Without this, FMX's focus chain is left pointing at a dead control
    and the app appears to freeze until the window is refreshed. }
  VertScrollBox1.SetFocus;

  FOwned.Remove(FRows[Idx].Layout);

  for I := Idx to High(FRows) - 1 do
    FRows[I] := FRows[I + 1];
  SetLength(FRows, Length(FRows) - 1);

  RebuildListBox(EditFilter.Text);
end;

{ -- public API ---------------------------------------------------------- }

procedure TFrameSliderContainer.LoadParams(const AParamNames: TArray<string>;
                                           const AInitValues: TArray<Double>);
var
  Idx: TArray<Integer>;
  I, N: Integer;
begin
  N := Length(AParamNames);
  SetLength(FAllParamNames,  N);
  SetLength(FAllParamValues, N);

  { Sort an index array by name, then permute both source arrays through it.
    Keeps FAllParamNames[i] and FAllParamValues[i] aligned after sorting. }
  SetLength(Idx, N);
  for I := 0 to N - 1 do Idx[I] := I;

  TArray.Sort<Integer>(Idx,
    TComparer<Integer>.Construct(
      function(const L, R: Integer): Integer
      begin
        Result := CompareStr(AParamNames[L], AParamNames[R]);
      end));

  for I := 0 to N - 1 do
  begin
    FAllParamNames[I] := AParamNames[Idx[I]];
    if Idx[I] < Length(AInitValues) then
      FAllParamValues[I] := AInitValues[Idx[I]]
    else
      FAllParamValues[I] := 0.0;
  end;

  RebuildListBox(EditFilter.Text);
end;

{ Removes slider rows only. The catalogue (FAllParamNames/FAllParamValues)
  is not touched — call LoadParams from the orchestrator if a model reload
  may have changed the parameter set. }
procedure TFrameSliderContainer.ClearSliders;
begin
  { NOTE: handler lifecycle (FOnSliderChanged, FReleaseOnlyMode) is
    owner-managed — the host orchestrating tab switches re-binds via
    each frame's AttachToSliders after calling ClearSliders. Clearing
    them here would orphan rows created by the user via "Add all" or
    the listbox after a tab switch, since those paths don't re-bind. }
  VertScrollBox1.BeginUpdate;
  try
    FOwned.Clear;
    FRows := [];
  finally
    VertScrollBox1.EndUpdate;
  end;
  FLockedParam := '';
  RebuildListBox(EditFilter.Text);
end;


procedure TFrameSliderContainer.BuildSliders(
  const AParamNames:    TArray<string>;
  const AInitialValues: TArray<Double>;
  const AMaxValue:      Single);
var
  I:    Integer;
  Init: Double;
begin
  ClearSliders;
  if Length(AParamNames) = 0 then Exit;

  FAllParamNames  := Copy(AParamNames);
  FAllParamValues := Copy(AInitialValues);
  TArray.Sort<string>(FAllParamNames);

  VertScrollBox1.BeginUpdate;
  try
    for I := 0 to High(AParamNames) do
    begin
      Init := 0.0;
      if I < Length(AInitialValues) then Init := AInitialValues[I];
      AddSliderRow(AParamNames[I], Init);
    end;
  finally
    VertScrollBox1.EndUpdate;
  end;

  RebuildListBox(EditFilter.Text);
end;

procedure TFrameSliderContainer.RefreshValues(const ANames:  TArray<string>;
                                              const AValues: TArray<Double>);
var
  I, J:         Integer;
  SavedHandler: TSliderChangedEvent;
begin
  if Length(FRows) = 0 then Exit;

  SavedHandler     := FOnSliderChanged;
  FOnSliderChanged := nil;
  try
    for I := 0 to High(ANames) do
    begin
      if I >= Length(AValues) then Break;
      J := RowIndexOf(ANames[I]);
      if J >= 0 then
        FRows[J].Track.Value := EnsureRange(AValues[I],
                                            FRows[J].Track.Min,
                                            FRows[J].Track.Max);
    end;
  finally
    FOnSliderChanged := SavedHandler;
  end;
end;

{ -- widget event handlers ----------------------------------------------- }

procedure TFrameSliderContainer.DoFilterChange(Sender: TObject);
begin
  RebuildListBox(EditFilter.Text);
end;

procedure TFrameSliderContainer.DoListBoxItemClick(
  const Sender: TCustomListBox; const Item: TListBoxItem);
var
  Name:   string;
  Filter: string;
begin
  Name := Item.Text;
  if IsActive(Name) then Exit;
  if (FLockedParam <> '') and SameText(Name, FLockedParam) then
    Exit;

  AddSliderRow(Name, InitialValueOf(Name));

  { Defer the listbox rebuild — Clear() would free the TListBoxItem still
    referenced by the FMX MouseUp handler that's about to run, crashing
    on macOS where the allocator is stricter than Windows'. Same pattern
    as DoRemoveBtnClick. }
  Filter := EditFilter.Text;
  TThread.ForceQueue(nil, procedure
  begin
    RebuildListBox(Filter);
  end);
end;

//procedure TFrameSliderContainer.DoListBoxItemClick(
//  const Sender: TCustomListBox; const Item: TListBoxItem);
//var
//  Name: string;
//begin
//  Name := Item.Text;
//  if IsActive(Name) then Exit;
//  if (FLockedParam <> '') and SameText(Name, FLockedParam) then
//    Exit;
//  AddSliderRow(Name, InitialValueOf(Name));
//  RebuildListBox(EditFilter.Text);
//end;

procedure TFrameSliderContainer.DoRemoveBtnClick(Sender: TObject);
var
  Name: string;
begin
  if not (Sender is TButton) then Exit;
  Name := TButton(Sender).TagString;
  TThread.ForceQueue(nil, procedure
  begin
    RemoveSliderRow(Name);
  end);
end;

procedure TFrameSliderContainer.DoTrackBarChange(Sender: TObject);
var
  Track: TTrackBar;
  Lbl:   TLabel;
begin
  if not (Sender is TTrackBar) then Exit;
  Track := TTrackBar(Sender);
  if not (Assigned(Track.TagObject) and (Track.TagObject is TLabel)) then Exit;
  Lbl := TLabel(Track.TagObject);

  Lbl.Text := FormatLabelText(Lbl.TagString, Track.Value);

  if Assigned(FOnSliderChanged) then
    FOnSliderChanged(Self, Lbl.TagString, Track.Value);
end;

{ -- "Add all" button ---------------------------------------------------- }

procedure TFrameSliderContainer.DoBtnAddAllClick(Sender: TObject);
var
  I: Integer;
begin
  if Length(FAllParamNames) = 0 then Exit;

  VertScrollBox1.BeginUpdate;
  try
    for I := 0 to High(FAllParamNames) do
      if not IsActive(FAllParamNames[I]) then
        AddSliderRow(FAllParamNames[I], InitialValueOf(FAllParamNames[I]));
  finally
    VertScrollBox1.EndUpdate;
  end;

  RebuildListBox(EditFilter.Text);
end;

{ -- "Reset" button ------------------------------------------------------ }

procedure TFrameSliderContainer.DoBtnResetClick(Sender: TObject);
var
  I:        Integer;
  InitVal:  Double;
  Row:      TSliderRow;
begin
  if Length(FRows) = 0 then Exit;

  for I := 0 to High(FRows) do
  begin
    Row     := FRows[I];
    InitVal := InitialValueOf(Row.ParamName);

    if InitVal < Row.Track.Min then Row.Track.Min := InitVal;
    if InitVal > Row.Track.Max then Row.Track.Max := InitVal;

    Row.Track.OnChange := nil;
    try
      Row.Track.Value := InitVal;
    finally
      Row.Track.OnChange := DoTrackBarChange;
    end;

    Row.Lbl.Text := FormatLabelText(Row.ParamName, InitVal);

    if Assigned(FOnSliderChanged) then
      FOnSliderChanged(Self, Row.ParamName, InitVal);
  end;
end;

procedure TFrameSliderContainer.DoLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Lbl:           TLabel;
  Track:         TTrackBar;
  NewMin, NewMax: Single;
begin
  if Button <> TMouseButton.mbRight then Exit;
  if not (Sender is TLabel) then Exit;
  Lbl := TLabel(Sender);
  if not (Assigned(Lbl.TagObject) and (Lbl.TagObject is TTrackBar)) then Exit;
  Track := TTrackBar(Lbl.TagObject);

  NewMin := Track.Min;
  NewMax := Track.Max;

  if PromptForRange('Edit range -- ' + Lbl.TagString, NewMin, NewMax) then
  begin
    Track.BeginUpdate;
    try
      //Track.Min   := NewMin;
      //Track.Max   := NewMax;
      //Track.Value := EnsureRange(Track.Value, NewMin, NewMax);
      //Track.Frequency := (NewMax - NewMin) / 200;
      SetTrackRange(Track, NewMin, NewMax, Track.Value);
    finally
      Track.EndUpdate;
    end;
    Lbl.Text := FormatLabelText(Lbl.TagString, Track.Value);
  end;
end;

procedure TFrameSliderContainer.GetSliderValues(out ANames: TArray<string>;
                                                out AValues: TArray<Double>);
var
  I: Integer;
begin
  SetLength(ANames,  Length(FRows));
  SetLength(AValues, Length(FRows));
  for I := 0 to High(FRows) do
  begin
    ANames[I]  := FRows[I].ParamName;
    AValues[I] := FRows[I].Track.Value;
  end;
end;

{ -- locked-parameter support -------------------------------------------- }

procedure TFrameSliderContainer.SetLockedParam(const AParamName: string);
var
  OldIdx, NewIdx: Integer;
begin
  if SameText(AParamName, FLockedParam) then Exit;

  if FLockedParam <> '' then
  begin
    OldIdx := RowIndexOf(FLockedParam);
    if OldIdx >= 0 then
      FRows[OldIdx].Track.Enabled := True;
  end;

  FLockedParam := AParamName;

  if FLockedParam <> '' then
  begin
    NewIdx := RowIndexOf(FLockedParam);
    if NewIdx >= 0 then
      FRows[NewIdx].Track.Enabled := False;
  end;

  RebuildListBox(EditFilter.Text);
end;

end.
