unit uFrameMetadata;

{ The metadata view: everything the model's simulation-metadata block
  defines, and everything wrong with it.

  This panel exists because the format's one genuinely silent failure mode
  is invisibility. A metadata block lives inside a comment, so a user who
  did not write the file has no reason to know it is there, and a block
  that was not recognised as one (spec 3.1 — it must begin with '@')
  looks exactly like a block that had nothing to say.

  It is also where conformance C5 is discharged properly: warnings must
  name the command that was skipped and say why, and distinguish "unknown
  to the format" from "known but not supported by this build", because
  the user's remedy differs.

  The controls are built in code: the panel is a read-only report whose
  whole content is generated, so a designer file would carry one memo and
  a heading and add a second place to keep in step. The .fmx beside this
  unit is therefore all but empty — but it must exist, because TFrame's
  constructor calls InitInheritedComponent and raises EResNotFound for any
  frame class with no form resource.

  It computes nothing and it plots nothing, so ActiveAnalysisKey has no
  entry for it: the shell's plot-styling and loaded-data bookkeeping key
  off that name and correctly skip a panel that has neither. }

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts,
  FMX.Memo, FMX.Memo.Types, FMX.ScrollBox, FMX.Controls.Presentation,
  uAnalysisTypes, uMetaExperiments,
  Sim.Meta, Sim.Meta.Types, Sim.Meta.Model;

type
  TFrameMetadata = class(TFrame)
  private
    FContext:  IAnalysisContext;
    FMemo:     TMemo;
    FHeading:  TLabel;
    FRefresh:  TButton;
    procedure DoRefreshClick(Sender: TObject);
    procedure BuildUI;
    procedure AppendExperiments(ALines: TStringList);
    procedure AppendSkipped(ALines: TStringList);
    procedure AppendDiagnostics(ALines: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetContext(const AContext: IAnalysisContext);

    { Re-render from whatever the shell currently holds. Cheap, so the
      shell may call it on every re-parse. }
    procedure Refresh;
  end;

implementation

{$R *.fmx}

const
  KIND_NAMES: array[TMetaExperimentKind] of string =
    ('time course', 'steady state', 'parameter scan');

constructor TFrameMetadata.Create(AOwner: TComponent);
begin
  inherited;
  BuildUI;
end;

procedure TFrameMetadata.BuildUI;
var
  Bar: TLayout;
begin
  Bar := TLayout.Create(Self);
  Bar.Parent := Self;
  Bar.Align  := TAlignLayout.Top;
  Bar.Height := 40;

  FHeading := TLabel.Create(Self);
  FHeading.Parent := Bar;
  FHeading.Align  := TAlignLayout.Client;
  FHeading.Margins.Rect := RectF(8, 0, 4, 0);
  FHeading.VertTextAlign := TTextAlign.Center;
  FHeading.Text := 'Simulation metadata';

  FRefresh := TButton.Create(Self);
  FRefresh.Parent := Bar;
  FRefresh.Align  := TAlignLayout.Right;
  FRefresh.Width  := 80;
  FRefresh.Margins.Rect := RectF(4, 7, 8, 7);
  FRefresh.Text    := 'Refresh';
  FRefresh.OnClick := DoRefreshClick;

  FMemo := TMemo.Create(Self);
  FMemo.Parent   := Self;
  FMemo.Align    := TAlignLayout.Client;
  FMemo.Margins.Rect := RectF(8, 0, 8, 8);
  FMemo.ReadOnly := True;
  FMemo.WordWrap := False;
  FMemo.ShowScrollBars := True;
end;

procedure TFrameMetadata.SetContext(const AContext: IAnalysisContext);
begin
  FContext := AContext;
  Refresh;
end;

procedure TFrameMetadata.DoRefreshClick(Sender: TObject);
begin
  Refresh;
end;

procedure TFrameMetadata.Refresh;
var
  Meta:  TSimulationMetadata;
  Lines: TStringList;
  Info:  TMetaInfoCommand;
begin
  if (FContext = nil) or (FMemo = nil) then Exit;
  Meta := FContext.Metadata;

  Lines := TStringList.Create;
  try
    if (Meta = nil) or (not Meta.HasMetadata) then
    begin
      FHeading.Text := 'Simulation metadata — none in this model';
      Lines.Add('This model has no simulation-metadata block.');
      Lines.Add('');
      Lines.Add('A block is an ordinary Antimony block comment whose first');
      Lines.Add('non-whitespace character is ''@'':');
      Lines.Add('');
      Lines.Add('  /*');
      Lines.Add('  @simulate: { timestart: 0, timeend: 50, points: 500 }');
      Lines.Add('  @plot: { y: [S1, S2] }');
      Lines.Add('  */');
      Lines.Add('');
      Lines.Add('Because it lives in a comment the model stays valid');
      Lines.Add('Antimony, and tools that do not understand the block');
      Lines.Add('ignore it.');
      { A block that was written but not recognised still produces a
        diagnostic, so show those even here — that case is exactly why
        this panel is worth having. }
      AppendDiagnostics(Lines);
      FMemo.Lines.Assign(Lines);
      Exit;
    end;

    FHeading.Text := 'Simulation metadata';

    Info := Meta.Meta;
    if Info <> nil then
    begin
      if Info.Title <> ''       then Lines.Add('Title:       ' + Info.Title);
      if Info.Author <> ''      then Lines.Add('Author:      ' + Info.Author);
      if Info.Description <> '' then Lines.Add('Description: ' + Info.Description);
      if Lines.Count > 0 then Lines.Add('');
    end;

    AppendExperiments(Lines);
    AppendSkipped(Lines);
    AppendDiagnostics(Lines);

    FMemo.Lines.Assign(Lines);
  finally
    Lines.Free;
  end;
end;

procedure TFrameMetadata.AppendExperiments(ALines: TStringList);
var
  ASet: TMetaExperimentSet;
  I:    Integer;
  E:    TMetaExperiment;
  P:    TPlotCommand;
  O:    TOutputCommand;
begin
  ASet := FContext.MetaExperiments;
  if (ASet = nil) or (ASet.Count = 0) then
  begin
    ALines.Add('EXPERIMENTS');
    ALines.Add('  (none — the block defines no simulate, scan or');
    ALines.Add('   steadystate command)');
    ALines.Add('');
    Exit;
  end;

  ALines.Add('EXPERIMENTS');
  ALines.Add('');
  for I := 0 to ASet.Count - 1 do
  begin
    E := ASet[I];
    ALines.Add(Format('  %s   [%s]', [E.LabelText, KIND_NAMES[E.Kind]]));

    if not E.Usable then
      { Named, with the reason. This is the C5 obligation, and it is the
        whole point of keeping an unusable command rather than dropping
        it. }
      ALines.Add('    NOT USED: ' + E.Reason)
    else if E.Task.SettingsSummary <> '' then
      ALines.Add('    ' + E.Task.SettingsSummary);

    for P in E.Plots do
      ALines.Add('    plot:   ' + P.SettingsSummary);
    for O in E.Outputs do
      ALines.Add('    output: ' + O.SettingsSummary);

    if Length(E.SkippedPlots) > 0 then
      ALines.Add('    note:   Iridium has one plot surface and draws the ' +
                 'first plot only.');

    ALines.Add('');
  end;
end;

procedure TFrameMetadata.AppendSkipped(ALines: TStringList);
var
  ASet: TMetaExperimentSet;
  S:    TMetaSkippedCommand;
begin
  ASet := FContext.MetaExperiments;
  if (ASet = nil) or (Length(ASet.Skipped) = 0) then Exit;

  ALines.Add('NOT SUPPORTED BY THIS BUILD');
  ALines.Add('');
  { The distinction that matters here is between a command the format
    does not define (probably a typo) and one it does define that Iridium
    cannot realise (try another tool) — the library words the reason
    accordingly. }
  for S in ASet.Skipped do
  begin
    ALines.Add('  ' + S.Display);
    ALines.Add('    ' + S.Reason);
  end;
  ALines.Add('');
end;

procedure TFrameMetadata.AppendDiagnostics(ALines: TStringList);
var
  Meta: TSimulationMetadata;
  I:    Integer;
begin
  Meta := FContext.Metadata;
  if (Meta = nil) or (Meta.Diagnostics.Count = 0) then Exit;

  ALines.Add(Format('DIAGNOSTICS  (%d error(s), %d warning(s))',
    [Meta.Diagnostics.ErrorCount, Meta.Diagnostics.WarningCount]));
  ALines.Add('');
  { Formatted by the library: 'file.ant(42,7): warning META0107: ...'.
    The codes are the stable part — they are what to look up and what to
    report — while the wording will keep improving. }
  for I := 0 to Meta.Diagnostics.Count - 1 do
    ALines.Add('  ' + Meta.Diagnostics.Format(I));
  ALines.Add('');
end;

end.
