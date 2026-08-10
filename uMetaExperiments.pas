unit uMetaExperiments;

{ Grouping layer between Sim.Meta and Iridium's analysis panels.

  The metadata library hands back a flat list of commands in document
  order. That is the right shape for the format (a @plot must not be
  nested inside the task it draws — conformance C3), but it is not the
  shape the GUI thinks in: what a user did was run a time course AND draw
  it, which is one thing, not two.

  An "experiment" is therefore a task command plus the @plot and @output
  commands whose source resolves to it. Each experiment routes to the
  panel that owns its task kind, and each panel is populated from the
  first experiment of its kind. Nothing here computes anything and nothing
  here switches panels: a metadata block is a library of presets, not a
  script.

  Commands that cannot be realised (an @bifurcation, a scan over a steady
  state) are kept, not dropped, with the reason attached. Conformance C5
  requires naming what was skipped and why, and a greyed row in the
  selector saying why is a better warning than a line in a log. }

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Sim.Meta, Sim.Meta.Types, Sim.Meta.Model;

type
  { Which analysis panel owns an experiment. Deliberately not a set: a
    task command has exactly one natural home. }
  TMetaExperimentKind = (mekTimeCourse, mekSteadyState, mekScan);

  TMetaExperiment = class
  private
    FPlots:   TList<TPlotCommand>;
    FOutputs: TList<TOutputCommand>;
    function GetPlots: TArray<TPlotCommand>;
    function GetOutputs: TArray<TOutputCommand>;
  public
    { The task itself. Never nil — an experiment exists because a task
      command was found. Not owned: Sim.Meta owns every command. }
    Task:   TTaskCommand;
    Kind:   TMetaExperimentKind;
    { False => this build cannot realise the task, or the task had an
      error and its fields are not trustworthy. Show it, disabled, with
      Reason; never apply it to a panel. }
    Usable: Boolean;
    Reason: string;

    constructor Create(ATask: TTaskCommand; AKind: TMetaExperimentKind);
    destructor  Destroy; override;

    procedure AddPlot(APlot: TPlotCommand);
    procedure AddOutput(AOutput: TOutputCommand);

    { AutoLabel: what the user wrote, else a generated 'simulate1'. Always
      populated, so a selector never has a blank row. }
    function LabelText: string;
    { 'wt' or 'wt — no continuation algorithm', for a selector row. }
    function DisplayText: string;

    { The plot Iridium will actually draw. Iridium has one plot surface,
      so where an experiment defines several it draws the first and warns
      naming the rest (spec 13). Nil if the experiment defines none. }
    function FirstPlot: TPlotCommand;
    { The ones it will not draw — for the warning that must name them. }
    function SkippedPlots: TArray<TPlotCommand>;

    property Plots:   TArray<TPlotCommand>   read GetPlots;
    property Outputs: TArray<TOutputCommand> read GetOutputs;
  end;

  { A command the block defines but this build will not realise. Kept so
    the user can be told, per C5, rather than left wondering. }
  TMetaSkippedCommand = record
    Name:    string;   { 'bifurcation' }
    Display: string;   { '@bifurcation at line 42' }
    Reason:  string;
  end;

  TMetaExperimentSet = class
  private
    FItems:   TObjectList<TMetaExperiment>;
    FSkipped: TArray<TMetaSkippedCommand>;
    FMeta:    TSimulationMetadata;
    function  ExperimentForTaskLabel(const ALabel: string): TMetaExperiment;
    procedure Build;
    function  GetCount: Integer;
    function  GetItem(AIndex: Integer): TMetaExperiment;
  public
    { Reads AMeta but does not own it. AMeta must outlive this object —
      every experiment points at commands AMeta owns. }
    constructor Create(AMeta: TSimulationMetadata);
    destructor  Destroy; override;

    function ForKind(AKind: TMetaExperimentKind): TArray<TMetaExperiment>;
    { The one a panel is populated from on model open: the first usable
      experiment of that kind, or nil. }
    function FirstUsable(AKind: TMetaExperimentKind): TMetaExperiment;
    function FindByLabel(const ALabel: string): TMetaExperiment;
    function CountUsable: Integer;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TMetaExperiment read GetItem; default;
    { Commands the format defines but this build cannot realise. }
    property Skipped: TArray<TMetaSkippedCommand> read FSkipped;
  end;

{ Diagnostics whose position falls inside ACmd's source span, formatted
  one per line. Matching on the span rather than the line keeps a
  diagnostic with the command that caused it even where several commands
  share a line. }
function DiagnosticsForCommand(AMeta: TSimulationMetadata;
                               ACmd: TMetaCommandBase): TArray<string>;

implementation

{ ── TMetaExperiment ──────────────────────────────────────────────────── }

constructor TMetaExperiment.Create(ATask: TTaskCommand;
  AKind: TMetaExperimentKind);
begin
  inherited Create;
  Task    := ATask;
  Kind    := AKind;
  FPlots   := TList<TPlotCommand>.Create;
  FOutputs := TList<TOutputCommand>.Create;
  Usable  := ATask.Supported and ATask.Valid;
end;

destructor TMetaExperiment.Destroy;
begin
  { Lists of borrowed references — Sim.Meta owns the commands. }
  FOutputs.Free;
  FPlots.Free;
  inherited;
end;

procedure TMetaExperiment.AddPlot(APlot: TPlotCommand);
begin
  FPlots.Add(APlot);
end;

procedure TMetaExperiment.AddOutput(AOutput: TOutputCommand);
begin
  FOutputs.Add(AOutput);
end;

function TMetaExperiment.GetPlots: TArray<TPlotCommand>;
begin
  Result := FPlots.ToArray;
end;

function TMetaExperiment.GetOutputs: TArray<TOutputCommand>;
begin
  Result := FOutputs.ToArray;
end;

function TMetaExperiment.LabelText: string;
begin
  Result := Task.AutoLabel;
  if Result = '' then
    Result := Task.Name;
end;

function TMetaExperiment.DisplayText: string;
begin
  Result := LabelText;
  if (not Usable) and (Reason <> '') then
    Result := Result + '  —  ' + Reason;
end;

function TMetaExperiment.FirstPlot: TPlotCommand;
var
  P: TPlotCommand;
begin
  for P in FPlots do
    if P.Supported and P.Valid then
      Exit(P);
  Result := nil;
end;

function TMetaExperiment.SkippedPlots: TArray<TPlotCommand>;
var
  First: TPlotCommand;
  P:     TPlotCommand;
begin
  Result := [];
  First := FirstPlot;
  for P in FPlots do
    if (P <> First) and P.Supported and P.Valid then
      Result := Result + [P];
end;

{ ── diagnostics ──────────────────────────────────────────────────────── }

function DiagnosticsForCommand(AMeta: TSimulationMetadata;
  ACmd: TMetaCommandBase): TArray<string>;
var
  I:   Integer;
  Pos: Integer;
begin
  Result := [];
  if (AMeta = nil) or (ACmd = nil) then Exit;
  if ACmd.EndOffset <= ACmd.StartOffset then Exit;

  for I := 0 to AMeta.Diagnostics.Count - 1 do
  begin
    Pos := AMeta.Diagnostics[I].Pos.Offset;
    if (Pos >= ACmd.StartOffset) and (Pos < ACmd.EndOffset) then
      Result := Result + [AMeta.Diagnostics.Format(I)];
  end;
end;

{ First diagnostic message inside ACmd's span, for the one-line reason a
  selector row can show. Prefers an error: where a command has both, the
  error is why it is unusable. }
function FirstProblem(AMeta: TSimulationMetadata;
  ACmd: TMetaCommandBase): string;
var
  I:    Integer;
  Pos:  Integer;
  Warn: string;
  D:    TDiagnostic;
begin
  Result := '';
  Warn   := '';
  if (AMeta = nil) or (ACmd = nil) then Exit;
  if ACmd.EndOffset <= ACmd.StartOffset then Exit;

  for I := 0 to AMeta.Diagnostics.Count - 1 do
  begin
    D   := AMeta.Diagnostics[I];
    Pos := D.Pos.Offset;
    if (Pos < ACmd.StartOffset) or (Pos >= ACmd.EndOffset) then Continue;

    if D.Kind = dkError then
      Exit(D.Message);
    if (D.Kind = dkWarning) and (Warn = '') then
      Warn := D.Message;
  end;
  Result := Warn;
end;

{ ── TMetaExperimentSet ───────────────────────────────────────────────── }

constructor TMetaExperimentSet.Create(AMeta: TSimulationMetadata);
begin
  inherited Create;
  FMeta  := AMeta;
  FItems := TObjectList<TMetaExperiment>.Create(True);
  if FMeta <> nil then
    Build;
end;

destructor TMetaExperimentSet.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TMetaExperimentSet.ExperimentForTaskLabel(
  const ALabel: string): TMetaExperiment;
var
  E: TMetaExperiment;
begin
  for E in FItems do
    if (E.Task.AutoLabel = ALabel) or
       ((E.Task.CmdLabel <> '') and (E.Task.CmdLabel = ALabel)) then
      Exit(E);
  Result := nil;
end;

procedure TMetaExperimentSet.Build;
var
  Cmd:     TMetaCommandBase;
  Exp:     TMetaExperiment;
  Kind:    TMetaExperimentKind;
  Src:     TArray<string>;
  Skip:    TMetaSkippedCommand;
  Latest:  TMetaExperiment;

  { The experiment a @plot / @output belongs to. The validator resolves
    every source to an AutoLabel, including the implicit
    preceding-task form, so the label path covers almost everything; the
    fall back to the most recent task is for a command whose source could
    not be resolved at all. A source list spanning several tasks (an
    overlay) belongs to its first source — rare, and splitting it across
    panels would be worse than picking one. }
  function OwnerOf(const ASource: TArray<string>): TMetaExperiment;
  begin
    Result := nil;
    if Length(ASource) > 0 then
      Result := ExperimentForTaskLabel(ASource[0]);
    if Result = nil then
      Result := Latest;
  end;

  { A drawing command with no experiment to belong to. }
  procedure AddOrphan(ACmd: TMetaCommandBase);
  var
    S: TMetaSkippedCommand;
  begin
    S.Name    := ACmd.Name;
    S.Display := ACmd.DisplayName;
    S.Reason  := 'no @simulate, @scan or @steadystate for it to draw — '
               + 'add a task command before it, or a source: naming one';
    FSkipped := FSkipped + [S];
  end;

begin
  Latest := nil;

  for Cmd in FMeta.Commands do
  begin
    { A task opens an experiment, whether or not it is usable: an
      unusable one still has to be shown and explained. }
    if Cmd is TTaskCommand then
    begin
      if Cmd is TSimulateCommand then
        Kind := mekTimeCourse
      else if Cmd is TSteadyStateCommand then
        Kind := mekSteadyState
      else
        Kind := mekScan;

      Exp := TMetaExperiment.Create(TTaskCommand(Cmd), Kind);
      if not Exp.Usable then
      begin
        Exp.Reason := FirstProblem(FMeta, Cmd);
        if Exp.Reason = '' then
          if not Cmd.Supported then
            Exp.Reason := 'not supported by this build'
          else
            Exp.Reason := 'the command has an error';
      end;
      FItems.Add(Exp);
      Latest := Exp;
      Continue;
    end;

    { A @plot / @output with nothing to attach to — no preceding task and
      no resolvable source — has no experiment to live in, so no panel
      would ever show it. Record it rather than dropping it: a block whose
      only command is a bare @plot otherwise fails completely silently,
      which reads as "the metadata block was not read at all". }
    if Cmd is TPlotCommand then
    begin
      Src := TPlotCommand(Cmd).Source;
      Exp := OwnerOf(Src);
      if Exp <> nil then
        Exp.AddPlot(TPlotCommand(Cmd))
      else
        AddOrphan(Cmd);
      Continue;
    end;

    if Cmd is TOutputCommand then
    begin
      Src := TOutputCommand(Cmd).Source;
      Exp := OwnerOf(Src);
      if Exp <> nil then
        Exp.AddOutput(TOutputCommand(Cmd))
      else
        AddOrphan(Cmd);
      Continue;
    end;

    { Everything the format does not define, or this build cannot
      realise: @bifurcation, @sensitivity, @figure. Recorded rather than
      dropped so the user can be told what was skipped (C5). @meta is
      neither a task nor skipped — it is file-level and the facade
      exposes it directly. }
    if (Cmd is TUnknownCommand) or (not Cmd.Supported) then
    begin
      Skip.Name    := Cmd.Name;
      Skip.Display := Cmd.DisplayName;
      if Cmd is TUnknownCommand then
        Skip.Reason := TUnknownCommand(Cmd).Reason
      else
        Skip.Reason := '';
      if Skip.Reason = '' then
        Skip.Reason := FirstProblem(FMeta, Cmd);
      if Skip.Reason = '' then
        Skip.Reason := 'not supported by this build';
      FSkipped := FSkipped + [Skip];
    end;
  end;
end;

function TMetaExperimentSet.ForKind(
  AKind: TMetaExperimentKind): TArray<TMetaExperiment>;
var
  E: TMetaExperiment;
begin
  Result := [];
  for E in FItems do
    if E.Kind = AKind then
      Result := Result + [E];
end;

function TMetaExperimentSet.FirstUsable(
  AKind: TMetaExperimentKind): TMetaExperiment;
var
  E: TMetaExperiment;
begin
  for E in FItems do
    if (E.Kind = AKind) and E.Usable then
      Exit(E);
  Result := nil;
end;

function TMetaExperimentSet.FindByLabel(
  const ALabel: string): TMetaExperiment;
begin
  Result := ExperimentForTaskLabel(ALabel);
end;

function TMetaExperimentSet.CountUsable: Integer;
var
  E: TMetaExperiment;
begin
  Result := 0;
  for E in FItems do
    if E.Usable then
      Inc(Result);
end;

function TMetaExperimentSet.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TMetaExperimentSet.GetItem(AIndex: Integer): TMetaExperiment;
begin
  Result := FItems[AIndex];
end;

end.
