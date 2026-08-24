unit uFrameParameterScan;

{ TFrameParameterScan — control panel for parameter scan simulations. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.Edit,
  FMX.NumberBox, FMX.TabControl,
  FMX.ScrollBox,
  uAnalysisTypes,
  uMetaExperiments,
  uMetaSelector,
  uMetaSetValues,
  uMetaScriptGen,
  Sim.Meta.Model,
  uRR2DSimpleMatrix,
  uColorList,
  FMX.EditBox, FMX.Objects;

type
  TScanRangeMode  = (srmLinear, srmLog, srmList);
  TOutputMeasure  = (omEndpoint, omPeakValue, omTimeToPeak, omTimeCourseOverlay);

  { Every control a @scan preset writes into, so that stepping back to the
    '—' row can return the panel exactly as the user left it. Captured
    once, before the first preset is applied. }
  TScanPanelState = record
    Parameter:   string;
    RangeMode:   TScanRangeMode;
    ScanStart:   string;
    ScanEnd:     string;
    ScanPoints:  string;
    ValueList:   string;
    TimeStart:   string;
    TimeEnd:     string;
    NumPoints:   string;
    SampleTime:  string;
    Measure:     TOutputMeasure;
    Observables: TArray<string>;
  end;

  TFrameParameterScan = class(TFrame, IPythonScriptExporter,
                              IMetaScriptProvider)
    grpParameter:          TGroupBox;
    lblParameter:          TLabel;
    cbParameter:           TComboBox;

    grpScanRange:          TGroupBox;
    rbLinear:              TRadioButton;
    rbLog:                 TRadioButton;
    rbList:                TRadioButton;
    layoutSweep:           TLayout;
      lblStart:            TLabel;
      lblEnd:              TLabel;
      lblNPoints:          TLabel;
    layoutList:            TLayout;
      lblValueList:        TLabel;
      edtValueList:        TEdit;

    grpSimulation:         TGroupBox;
    lblTimeStart:          TLabel;
    lblTimeEnd:            TLabel;
    lblNumPoints:          TLabel;
    lblSampleTime:         TLabel;

    grpOutputMeasure:       TGroupBox;
    rbEndpoint:            TRadioButton;
    rbPeakValue:           TRadioButton;
    rbTimeToPeak:          TRadioButton;
    rbTimeCourseOverlay:   TRadioButton;

    grpObservable:         TGroupBox;
    TabControl1:           TTabControl;
    tabFloating:           TTabItem;
      lstFloating:         TListBox;
    tabBoundary:           TTabItem;
      lstBoundary:         TListBox;
    tabFluxes:             TTabItem;
      lstFluxes:           TListBox;
    tabRatesOfChange:      TTabItem;
      lstRatesOfChange:    TListBox;

    layoutRunControls:     TLayout;
    btnRunScan:            TButton;
    btnResetScan:          TButton;
    pbScanProgress:        TProgressBar;
    cboColorpalette: TComboBox;
    btnScanSliders: TSpeedButton;
    Image1: TImage;
    Label1: TLabel;
    edtTimeStart: TEdit;
    edtTimeEnd: TEdit;
    edtNumPoints: TEdit;
    edtScanStart: TEdit;
    edtScanEnd: TEdit;
    edtScanNPoints: TEdit;
    edtSampleTime: TEdit;
    chkProgressBar: TCheckBox;
    btnScanUnSelectAll: TButton;
    btnScanSelectAll: TButton;

    procedure rbRangeModeChange(Sender: TObject);
    procedure rbOutputMeasureChange(Sender: TObject);
    procedure lstObservableCheckChanged(Sender: TObject);
    procedure btnRunScanClick(Sender: TObject);
    procedure btnResetScanClick(Sender: TObject);
    procedure cboColorpaletteChange(Sender: TObject);
    procedure btnScanSlidersClick(Sender: TObject);
    procedure btnRunScanMouseLeave(Sender: TObject);
    procedure edtTimeStartKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtTimeEndKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtNumPointsKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtTimeStartExit(Sender: TObject);
    procedure edtTimeEndExit(Sender: TObject);
    procedure edtNumPointsExit(Sender: TObject);
    procedure edtScanStartKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtScanStartExit(Sender: TObject);
    procedure edtScanEndKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtScanEndExit(Sender: TObject);
    procedure edtScanNPointsKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtScanNPointsExit(Sender: TObject);
    procedure edtSampleTimeKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtSampleTimeExit(Sender: TObject);
    procedure btnScanSelectAllClick(Sender: TObject);
    procedure btnScanUnSelectAllClick(Sender: TObject);

  private
    FContext:          IAnalysisContext;
    FHasData:          Boolean;
    FSelectedObsNames: TArray<string>;
    FRunningScan:      Boolean;   { re-entrancy guard for slider-driven scans }

    { ── metadata presets ───────────────────────────────────────────────
      The @scan experiments this model defines. As on the time-course
      panel, the block fills these controls and never runs anything. The
      dropdown and everything generic about it lives in uMetaSelector. }
    FSelector:     TMetaExperimentSelector;
    FUserState:    TScanPanelState;
    FHasUserState: Boolean;

    { A preset applied while no model was loaded. The parameter combo and
      observable lists are empty until the first load, so the request is
      held and replayed once they exist. }
    FPendingExperiment: string;
    { The experiment whose unmet 'set:' values have already been reported,
      so the message appears once rather than on every Run Scan. }
    FReportedSetFor:    string;
    { What to write back to undo the selected experiment's 'set:' values. }
    FActiveSetRestore:  TSetValueRestoreArray;
    { The experiment whose @plot has already been reported on, so the
      "drew the first only" / "y names something not scanned" notice
      appears once rather than on every Run Scan. Cleared by a re-parse. }
    FWarnedPlotFor:     string;
    procedure ApplyPendingExperiment;

    { Overlay the selected experiment's @plot onto the freshly drawn scan.
      AMeasure is the mode the sweep ran in: it decides how the series are
      named, and so what a 'series:' block can match. Returns True when the
      command wrote an 'xlabel:', which is the caller's cue to leave the
      x-axis title alone rather than overwrite it with the derived one. }
    function  ApplyPlotMetadata(AMeasure: TOutputMeasure): Boolean;

    procedure ApplyExperiment(AExp: TMetaExperiment; AWasUnset: Boolean);
    procedure RestoreUserState(Sender: TObject);
    function  GetMetaExperiments: TMetaExperimentSet;
    function  CapturePanelState: TScanPanelState;
    procedure RestorePanelState(const AState: TScanPanelState);
    { Model id for a name as the metadata spells it, honouring
      RoadRunner's '[A]' form for species. '' if the model has no such
      name. }
    function  ResolveModelName(const AName: string): string;

    procedure SessionStateChanged  (Sender: TObject);
    procedure SessionModelReloaded (Sender: TObject; AParameterSetChanged: Boolean);

    procedure PopulateParameterCombo;
    procedure PopulateObservableLists;
    procedure ClearObservableLists;
    procedure UpdateRangeMode;
    procedure UpdateMeasureMode;
    procedure UpdateSelectedObsLabel;

    { The observable list belonging to the tab the user is looking at, or nil
      if the active tab isn't one of the four observable tabs. }
    function  ActiveObservableList: TListBox;
    procedure SetAllChecked(AList: TListBox; AChecked: Boolean);

    procedure CheckNumberKeys (edt : TEdit; var Key: Word;  var KeyChar: WideChar; Shift: TShiftState);
    procedure CheckNumberKeysInteger (edt : TEdit; var Key: Word;  var KeyChar: WideChar; Shift: TShiftState);

    { Slider support }
    procedure DoParameterChange(Sender: TObject);
    procedure OnSliderChanged(Sender: TObject;
                              const ASliderString: string;
                              const AValue: Single);

    function  ActiveRangeMode:    TScanRangeMode;
    function  ActiveOutputMeasure: TOutputMeasure;
    function  BuildScanValues:    TArray<Double>;
    function  ParseValueList(const AText: string): TArray<Double>;

    function GetPythonScript(const AntimonyText: string): string;

    { IMetaScriptProvider — this panel as an '@simulate' the scan repeats
      plus the '@scan' itself. }
    function GetMetaCommands(const ATaskLabel: string;
                             out APlotY: TArray<string>
                            ): TArray<TMetaCommandBase>;

    { Extract a scalar from one simulation result matrix.
        AData       – T2DMatrix returned by simulateEx
        AColName    – observable column name to read
        AMeasure    – what scalar to extract
        ASampleTime – for omEndpoint: time closest to this value  }
    function  ExtractScalar(AData: T2DMatrix;
                            const AColName: string;
                            AMeasure: TOutputMeasure;
                            ASampleTime: Double): Double;

    { Find the column index for AColName in AData.columnHeader.
      Returns -1 if not found. }
    function  FindColumn(AData: T2DMatrix; const AColName: string): Integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SetContext(const AContext: IAnalysisContext);
    procedure UpdateScanParameterLock;
    procedure AttachToSliders;
    procedure RefreshFromModelIfStale;

    { The shell re-parsed the model's metadata block. AApply = True only
      when a model was OPENED. See uFrameTimeCourse.MetadataChanged. }
    procedure MetadataChanged(AApply: Boolean);

    { Apply the named experiment to this panel and compute it — what
      Metadata ▸ Run Experiment dispatches here. }
    procedure RunExperiment(const ALabel: string);

    { The selected experiment's 'set:' values, applied for the duration of
      one sweep. Returns what RestoreSetValues needs to undo them. }
    procedure ApplySelectedSetValues(AExp: TMetaExperiment);
    procedure ClearSelectedSetValues;
    procedure RefreshSlidersFromEngine;

    { Replace the checked observables with those in ANames (matched against the
      four observable lists; names not present here are ignored). Used by the
      shell to seed the scan selection from the time-course selection on this
      panel's first appearance. }
    procedure SetCheckedObservables(const ANames: TArray<string>);
  end;

implementation

{$R *.fmx}

uses
  System.Math,
  uRoadRunner,
  uMetaSymbolProvider,
  uPlotSeries;

const
  TIME_COLUMN_LABEL = 'time';

var  ColorList: TColorList;

procedure TFrameParameterScan.CheckNumberKeys (edt : TEdit; var Key: Word;  var KeyChar: WideChar; Shift: TShiftState);
var
  CurrentText: string;
begin
  CurrentText := edt.Text;

  // 1. Always allow control keys (Backspace, Delete, Arrow keys, Enter, Tab)
  if KeyChar = #0 then
    Exit;

  // 2. Reject any character not in the allowed scientific notation set
  if not CharInSet(KeyChar, ['0'..'9', '.', 'e', 'E', '+', '-']) then
  begin
    Key := 0;     // Discard the hardware key stroke
    KeyChar := #0; // Discard the character token
    Exit;
  end;

  // 3. Prevent duplicate decimal points
  if (KeyChar = '.') and CurrentText.Contains('.') then
  begin
    Key := 0;
    KeyChar := #0;
    Exit;
  end;
end;

procedure TFrameParameterScan.CheckNumberKeysInteger (edt : TEdit; var Key: Word;  var KeyChar: WideChar; Shift: TShiftState);
var
  CurrentText: string;
begin
  CurrentText := edt.Text;

  // 1. Always allow control keys (Backspace, Delete, Arrow keys, Enter, Tab)
  if KeyChar = #0 then
    Exit;

  // 2. Reject any character not in the allowed scientific notation set
  if not CharInSet(KeyChar, ['0'..'9', '+']) then
  begin
    Key := 0;     // Discard the hardware key stroke
    KeyChar := #0; // Discard the character token
    Exit;
  end;
end;

{ ── Context wiring ───────────────────────────────────────────────────────── }

{ ── metadata presets ────────────────────────────────────────────────────── }

constructor TFrameParameterScan.Create(AOwner: TComponent);
begin
  inherited;
  FSelector := TMetaExperimentSelector.Create(Self, mekScan,
                                              GetMetaExperiments);
  FSelector.OnApply   := ApplyExperiment;
  FSelector.OnRestore := RestoreUserState;
  { Above the parameter group, where the values it fills in begin.
    grpParameter.Parent rather than a named container: the frame's
    background has no published field to reach it by. }
  FSelector.Place(grpParameter.Parent, grpParameter);
end;

destructor TFrameParameterScan.Destroy;
begin
  FSelector.Free;
  inherited;
end;

function TFrameParameterScan.GetMetaExperiments: TMetaExperimentSet;
begin
  if FContext = nil then
    Result := nil
  else
    Result := FContext.MetaExperiments;
end;

procedure TFrameParameterScan.RestoreUserState(Sender: TObject);
begin
  { Before the early exit: the engine is put back whether or not there is a
    panel-state snapshot to restore. }
  ClearSelectedSetValues;

  if not FHasUserState then Exit;
  FSelector.Suppressed := True;
  try
    RestorePanelState(FUserState);
  finally
    FSelector.Suppressed := False;
  end;

  { The plot's appearance is as much part of "my own settings" as the
    numbers were: a title or a log axis a preset switched on has to go
    with it. }
  if FContext <> nil then
    FContext.PlotRestoreUserStyle;
end;

function TFrameParameterScan.CapturePanelState: TScanPanelState;
begin
  Result.Parameter := '';
  if cbParameter.ItemIndex >= 0 then
    Result.Parameter := cbParameter.Items[cbParameter.ItemIndex];

  if rbLog.IsChecked then Result.RangeMode := srmLog
  else if rbList.IsChecked then Result.RangeMode := srmList
  else Result.RangeMode := srmLinear;

  Result.ScanStart  := edtScanStart.Text;
  Result.ScanEnd    := edtScanEnd.Text;
  Result.ScanPoints := edtScanNPoints.Text;
  Result.ValueList  := edtValueList.Text;
  Result.TimeStart  := edtTimeStart.Text;
  Result.TimeEnd    := edtTimeEnd.Text;
  Result.NumPoints  := edtNumPoints.Text;
  Result.SampleTime := edtSampleTime.Text;

  if rbPeakValue.IsChecked then Result.Measure := omPeakValue
  else if rbTimeToPeak.IsChecked then Result.Measure := omTimeToPeak
  else if rbTimeCourseOverlay.IsChecked then Result.Measure := omTimeCourseOverlay
  else Result.Measure := omEndpoint;

  Result.Observables := Copy(FSelectedObsNames);
end;

procedure TFrameParameterScan.RestorePanelState(const AState: TScanPanelState);
var
  Idx: Integer;
begin
  Idx := cbParameter.Items.IndexOf(AState.Parameter);
  if Idx >= 0 then
    cbParameter.ItemIndex := Idx;

  rbLinear.IsChecked := AState.RangeMode = srmLinear;
  rbLog.IsChecked    := AState.RangeMode = srmLog;
  rbList.IsChecked   := AState.RangeMode = srmList;

  edtScanStart.Text   := AState.ScanStart;
  edtScanEnd.Text     := AState.ScanEnd;
  edtScanNPoints.Text := AState.ScanPoints;
  edtValueList.Text   := AState.ValueList;
  edtTimeStart.Text   := AState.TimeStart;
  edtTimeEnd.Text     := AState.TimeEnd;
  edtNumPoints.Text   := AState.NumPoints;
  edtSampleTime.Text  := AState.SampleTime;

  rbEndpoint.IsChecked          := AState.Measure = omEndpoint;
  rbPeakValue.IsChecked         := AState.Measure = omPeakValue;
  rbTimeToPeak.IsChecked        := AState.Measure = omTimeToPeak;
  rbTimeCourseOverlay.IsChecked := AState.Measure = omTimeCourseOverlay;

  UpdateRangeMode;
  UpdateMeasureMode;
  SetCheckedObservables(AState.Observables);

  { Results on screen were produced from other settings. }
  FHasData := False;
end;

function TFrameParameterScan.ResolveModelName(const AName: string): string;
var
  Names: TArray<string>;
  N:     string;
begin
  { RoadRunner reports a floating species as '[A]' while the model file
    calls it 'A'; parameters are unbracketed in both. Accept either
    spelling and answer with the model's own. }
  Result := '';
  if (FContext = nil) or (not FContext.Session.IsLoaded) then Exit;

  Names := FContext.Session.GetTunableNames;
  for N in Names do
    if (N = AName) or (N = '[' + AName + ']') then
      Exit(N);
end;

{ While an experiment is selected the engine holds its 'set:' values — see
  the long note in uFrameTimeCourse. Applied when it is selected, removed
  when it is left, never around a sweep. }

procedure TFrameParameterScan.ApplySelectedSetValues(AExp: TMetaExperiment);
var
  Unmet: TStringList;
begin
  if (FContext = nil) or (not FContext.Session.IsLoaded) then Exit;

  ClearSelectedSetValues;

  if (AExp = nil) or (not AExp.Usable) or (AExp.Task = nil) then Exit;
  if Length(AExp.Task.SetValues) = 0 then Exit;

  Unmet := TStringList.Create;
  try
    FActiveSetRestore :=
      ApplySetValues(FContext.Session.RoadRunner, AExp.Task, Unmet);
    if (Unmet.Count > 0) and (FReportedSetFor <> AExp.LabelText) then
    begin
      FReportedSetFor := AExp.LabelText;
      ShowMessage('Experiment ' + AExp.LabelText +
        ': some ''set:'' values could not be applied.' + sLineBreak +
        sLineBreak + Unmet.Text);
    end;
  finally
    Unmet.Free;
  end;

  RefreshSlidersFromEngine;
end;

procedure TFrameParameterScan.ClearSelectedSetValues;
begin
  if Length(FActiveSetRestore) = 0 then Exit;

  if (FContext <> nil) and FContext.Session.IsLoaded then
    RestoreSetValues(FContext.Session.RoadRunner, FActiveSetRestore);
  FActiveSetRestore := nil;
  RefreshSlidersFromEngine;
end;

procedure TFrameParameterScan.RefreshSlidersFromEngine;
begin
  if FContext = nil then Exit;
  if not FContext.Session.IsLoaded then Exit;
  if not FContext.SliderContainer.ParamPanelVisible then Exit;

  FContext.SliderContainer.RefreshValues(FContext.Session.GetTunableNames,
                                         FContext.Session.GetTunableValues);
end;

procedure TFrameParameterScan.RunExperiment(const ALabel: string);
begin
  if (FContext = nil) or (FSelector = nil) then Exit;

  { Load before applying: the block has just been edited, so the preset
    must be validated against the incoming model, not the outgoing one,
    and the reload must not land after the apply and rebuild the
    selectors underneath it. }
  try
    if not FContext.Session.EnsureLoaded then
    begin
      ShowMessage('Cannot load model: ' + FContext.Session.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Model load failed: ' + E.Message);
      Exit;
    end;
  end;

  FSelector.ApplyLabel(FContext.MetaExperiments, ALabel);
  btnRunScanClick(nil);
end;

procedure TFrameParameterScan.MetadataChanged(AApply: Boolean);
begin
  if (FContext = nil) or (FSelector = nil) then Exit;

  { A fresh parse may have changed an experiment's @plot commands, so
    whatever we declined to honour last time is worth saying again. }
  FWarnedPlotFor := '';

  FSelector.Rebuild(FContext.MetaExperiments);
  if AApply then
    FSelector.ApplyFirstUsable(FContext.MetaExperiments);
end;

procedure TFrameParameterScan.ApplyExperiment(AExp: TMetaExperiment;
  AWasUnset: Boolean);
var
  Scan:   TScanCommand;
  ASet:   TMetaExperimentSet;
  SrcExp: TMetaExperiment;
  Sim:    TSimulateCommand;
  Vals:   TArray<Double>;
  Wanted: TArray<string>;
  ModelId, S: string;
  I, Idx: Integer;
  Fmt:    TFormatSettings;
begin
  if (AExp = nil) or (not AExp.Usable) then Exit;
  if not (AExp.Task is TScanCommand) then Exit;
  Scan := TScanCommand(AExp.Task);
  Fmt  := TFormatSettings.Invariant;

  { Capture the user's own settings before overwriting them.

    Re-captured whenever we are leaving the '—' row, not only the first
    time: while '—' is selected the panel IS the user's settings, so
    anything they changed there — an observable ticked, a range edited —
    is theirs and must come back when they return to it. Capturing only
    once meant '—' replayed whatever they had before the very first
    preset and silently discarded everything since. }
  if (not FHasUserState) or AWasUnset then
  begin
    FUserState    := CapturePanelState;
    { The plot's appearance too. A @plot switches on log axes, grids and
      titles, and those outlive the series they were applied to — so
      without this, stepping back to '—' returns the numbers but leaves
      the file's log axis and title in place. }
    if FContext <> nil then
      FContext.PlotCaptureUserStyle;
    FHasUserState := True;
  end;

  FSelector.Suppressed := True;
  try
    { The scanned parameter. A @scan may name a species, in which case its
      initial value is scanned, so the species form has to resolve too. }
    ModelId := ResolveModelName(Scan.Parameter);
    if ModelId = '' then
      ModelId := Scan.Parameter;
    Idx := cbParameter.Items.IndexOf(ModelId);
    if Idx >= 0 then
      cbParameter.ItemIndex := Idx;

    { Range. ScanValues materialises both forms, but the panel has a
      distinct list mode, so the two are kept apart here to round-trip
      what the user wrote rather than flattening a range into values. }
    if Scan.HasRange then
    begin
      rbList.IsChecked   := False;
      rbLog.IsChecked    := Scan.LogSpacing;
      rbLinear.IsChecked := not Scan.LogSpacing;
      edtScanStart.Text   := FloatToStr(Scan.RangeStart, Fmt);
      edtScanEnd.Text     := FloatToStr(Scan.RangeEnd, Fmt);
      edtScanNPoints.Text := IntToStr(Scan.RangePoints);
    end
    else
    begin
      rbLinear.IsChecked := False;
      rbLog.IsChecked    := False;
      rbList.IsChecked   := True;
      Vals := Scan.ScanValues;
      S := '';
      for I := 0 to High(Vals) do
      begin
        if I > 0 then S := S + ', ';
        S := S + FloatToStr(Vals[I], Fmt);
      end;
      edtValueList.Text := S;
    end;
    UpdateRangeMode;

    { What to extract at each scan point. }
    rbEndpoint.IsChecked          := Scan.Measure = mkSampleAt;
    rbPeakValue.IsChecked         := Scan.Measure = mkPeakValue;
    rbTimeToPeak.IsChecked        := Scan.Measure = mkTimeToPeak;
    rbTimeCourseOverlay.IsChecked := Scan.Measure = mkTimecourse;
    if Scan.Measure = mkSampleAt then
      edtSampleTime.Text := FloatToStr(Scan.SampleAt, Fmt);
    UpdateMeasureMode;

    { The time course each scan point runs is described by the task the
      scan repeats, not by the @scan itself — so the time settings come
      from its source. }
    ASet := FContext.MetaExperiments;
    if (ASet <> nil) and (Length(Scan.Source) > 0) then
    begin
      SrcExp := ASet.FindByLabel(Scan.Source[0]);
      if (SrcExp <> nil) and (SrcExp.Task is TSimulateCommand) then
      begin
        Sim := TSimulateCommand(SrcExp.Task);
        edtTimeStart.Text := FloatToStr(Sim.TimeStart, Fmt);
        edtTimeEnd.Text   := FloatToStr(Sim.TimeEnd, Fmt);
        edtNumPoints.Text := IntToStr(Sim.Points);
      end;
    end;

    { Observables. SetCheckedObservables matches list text exactly, so
      offer both spellings and let it take whichever the lists hold. }
    Wanted := [];
    for S in Scan.Observables do
    begin
      Wanted := Wanted + [S];
      ModelId := ResolveModelName(S);
      if (ModelId <> '') and (ModelId <> S) then
        Wanted := Wanted + [ModelId]
      else
        Wanted := Wanted + ['[' + S + ']'];
    end;
    if Length(Wanted) > 0 then
      SetCheckedObservables(Wanted);

    { The selector already holds this label — it is what dispatched here. }
  finally
    FSelector.Suppressed := False;
  end;

  { If there was no model to resolve names against, the parameter combo
    and observable lists were empty and could not take the request. Hold
    it for the first load, which is when they are built. }
  if (FContext <> nil) and FContext.Session.IsLoaded then
    FPendingExperiment := ''
  else
    FPendingExperiment := AExp.LabelText;

  { The experiment's 'set:' values go into the engine now and stay until it
    is left, so the sliders and the next sweep agree about them. }
  ApplySelectedSetValues(AExp);

  { Settings changed, so any scan on screen is stale. Nothing is
    recomputed: the user presses Run Scan when they want a result. }
  FHasData := False;
  UpdateScanParameterLock;
end;

procedure TFrameParameterScan.ApplyPendingExperiment;
var
  ASet: TMetaExperimentSet;
  Exp:  TMetaExperiment;
  Want: string;
begin
  Want := FPendingExperiment;
  FPendingExperiment := '';        { consumed either way, so it cannot
                                     go on overriding the user }
  if (Want = '') or (FContext = nil) then Exit;

  ASet := FContext.MetaExperiments;
  if ASet = nil then Exit;
  Exp := ASet.FindByLabel(Want);
  if (Exp <> nil) and Exp.Usable then
    { Not a transition off '—': the user's state was captured when this
      experiment was first chosen, and re-capturing now would record the
      preset's own values as theirs. }
    ApplyExperiment(Exp, False);
end;

procedure TFrameParameterScan.SetContext(const AContext: IAnalysisContext);
begin
  FContext := AContext;
  if FContext <> nil then
  begin
    FContext.Session.AddStateListener   (SessionStateChanged);
    FContext.Session.AddReloadedListener(SessionModelReloaded);
  end;

  cboColorPalette.Items.Assign(GetPaletteNames);

  var Idx := cboColorPalette.Items.IndexOf('BlueRed');
  if Idx < 0 then Idx := 0;   { fall back to first available if name changes }
  cboColorPalette.ItemIndex := Idx;
  ColorList.SetPalette(cboColorPalette.Items[Idx], 12);

  { React to scan-parameter changes so we can re-lock the slider panel. }
  cbParameter.OnChange := DoParameterChange;
end;

{ ── Session callbacks ────────────────────────────────────────────────────── }

procedure TFrameParameterScan.SessionStateChanged(Sender: TObject);
begin
  if FContext.Session.IsDirty then
    FHasData := False;

  { Model gone (File ▸ New, a failed parse, a model swap). }
  if not FContext.Session.IsLoaded then
  begin
    { Dropped, NOT restored: those values describe an engine that is gone. }
    FActiveSetRestore := nil;

    { The lists and the parameter combo name quantities the next model may
      not have, so they go with it. Only on unload — a dirty source is
      still the same model. }
    FHasData := False;
    ClearObservableLists;
    cbParameter.Clear;
  end;
end;

procedure TFrameParameterScan.SessionModelReloaded(Sender: TObject;
                                                    AParameterSetChanged: Boolean);
begin
  FHasData := False;
  PopulateParameterCombo;
  PopulateObservableLists;

  { A preset applied before the model was loaded could not reach the
    parameter combo or the observable lists — they were empty, because
    Iridium loads lazily and a model that has only been opened has no
    names yet. This is the first moment they exist, so apply it now.
    Re-running the whole apply is simpler than remembering which
    individual controls did not take, and it is idempotent. }
  if FPendingExperiment <> '' then
    ApplyPendingExperiment
  else
  begin
    { A reload rebuilt the engine from the model text, so the selected
      experiment's 'set:' values are no longer in it and the restore data
      describing the old engine is stale. Re-apply from scratch. (The
      pending path above goes through ApplyExperiment, which does this
      itself.) }
    FActiveSetRestore := nil;
    if FSelector <> nil then
      ApplySelectedSetValues(FSelector.ActiveExperiment);
  end;

  { If the slider panel is currently showing, the previous lock may now
    point at a parameter that no longer exists, or the auto-selected
    new scan parameter needs to be locked. }
  if (FContext <> nil) and FContext.SliderContainer.ParamPanelVisible then
    UpdateScanParameterLock;
end;

{ ── @scan generation ─────────────────────────────────────────────────────── }

function TFrameParameterScan.GetMetaCommands(const ATaskLabel: string;
  out APlotY: TArray<string>): TArray<TMetaCommandBase>;
var
  Sim:  TSimulateCommand;
  Scan: TScanCommand;
  Fmt:  TFormatSettings;
  N:    string;
begin
  Result  := nil;
  APlotY  := nil;
  Fmt     := TFormatSettings.Invariant;

  if (cbParameter.ItemIndex < 0) or
     (cbParameter.ItemIndex >= cbParameter.Items.Count) then Exit;
  if Length(FSelectedObsNames) = 0 then Exit;

  { The time course each scan point runs. A '@scan' carries no time keys —
    'start'/'end'/'points' there are the swept parameter's range — so the
    run it repeats has to be a task of its own for 'source' to name.
    Reading it back is the mirror of ApplyExperiment, which fills these
    same three edits from the scan's source. }
  Sim := TSimulateCommand.Create;
  Sim.Name      := 'simulate';
  Sim.CmdLabel  := ATaskLabel + '_run';
  Sim.TimeStart := StrToFloatDef(edtTimeStart.Text, 0, Fmt);
  Sim.TimeEnd   := StrToFloatDef(edtTimeEnd.Text, 10, Fmt);
  Sim.Points    := Max(2, StrToIntDef(edtNumPoints.Text, 100));
  Sim.Spelling  := csPoints;
  MarkWritten(Sim, 'timestart');
  MarkWritten(Sim, 'timeend');
  MarkWritten(Sim, 'points');

  Scan := TScanCommand.Create;
  Scan.Name     := 'scan';
  Scan.CmdLabel := ATaskLabel;
  Scan.Source   := [Sim.CmdLabel];
  MarkWritten(Scan, 'source');

  { The model's own spelling, not RoadRunner's: the block is read beside
    the model file, where a species is 'S1' and never '[S1]'. }
  Scan.Parameter := CanonicalModelName(
    cbParameter.Items[cbParameter.ItemIndex]);

  case ActiveRangeMode of
    srmList:
      begin
        Scan.HasRange := False;
        Scan.Values   := ParseValueList(edtValueList.Text);
        MarkWritten(Scan, 'values', True);
      end;
  else
    Scan.HasRange    := True;
    Scan.RangeStart  := StrToFloatDef(edtScanStart.Text, 0, Fmt);
    Scan.RangeEnd    := StrToFloatDef(edtScanEnd.Text, 1, Fmt);
    Scan.RangePoints := Max(2, StrToIntDef(edtScanNPoints.Text, 10));
    Scan.LogSpacing  := ActiveRangeMode = srmLog;
  end;

  for N in FSelectedObsNames do
  begin
    Scan.Observables := Scan.Observables + [CanonicalModelName(N)];
    APlotY           := APlotY + [CanonicalModelName(N)];
  end;
  MarkWritten(Scan, 'observables', True);

  case ActiveOutputMeasure of
    omPeakValue:  Scan.Measure := mkPeakValue;
    omTimeToPeak: Scan.Measure := mkTimeToPeak;
    omTimeCourseOverlay: Scan.Measure := mkTimecourse;
  else
    Scan.Measure  := mkSampleAt;
    Scan.SampleAt := StrToFloatDef(edtSampleTime.Text, Sim.TimeEnd, Fmt);
  end;

  Result := [Sim, Scan];
end;

{ ── @plot ───────────────────────────────────────────────────────────────── }

function TFrameParameterScan.ApplyPlotMetadata(AMeasure: TOutputMeasure): Boolean;
var
  Exp:      TMetaExperiment;
  P:        TPlotCommand;
  Skipped:  TArray<TPlotCommand>;
  Notes:    TArray<string>;
  Names, Y: string;
  I:        Integer;

  { Is AName one of the observables this sweep actually reported? The
    metadata spells a species 'S1' and the engine '[S1]', so try both
    forms — the same two spellings ApplyExperiment offers going the other
    way. 'time' belongs to no observable category and is always legal. }
  function IsReported(const AName: string): Boolean;
  var
    N: string;
  begin
    Result := SameText(AName, TIME_COLUMN_LABEL);
    if Result then Exit;
    for N in FSelectedObsNames do
      if (N = AName) or (N = '[' + AName + ']') then
        Exit(True);
  end;

begin
  Result := False;
  if (FContext = nil) or (FSelector = nil) then Exit;

  Exp := FSelector.ActiveExperiment;
  if (Exp = nil) or (not Exp.Usable) then Exit;

  P := Exp.FirstPlot;
  if P = nil then Exit;

  { Rebase before overlaying, exactly as the time-course panel does: a
    @plot is applied key by key, and "a key this command didn't write"
    has to mean the user's own baseline rather than whatever the previous
    experiment's @plot left behind. Without the reset, styling
    accumulates across experiments and no file describes its own figure.

    The rebase is also why this runs BEFORE the caller sets the derived
    x-axis title: the snapshot it restores carries an axis title of its
    own — the previous run's — so a title set first would simply be
    undone here. The caller sets it afterwards, and skips it when this
    returns True. }
  FContext.PlotRestoreUserStyle;
  FContext.PlotApplyMetaStyle(P);
  Result := P.WasWritten('xlabel');

  { ── What we could not honour ──────────────────────────────────────────
    Collected into one notice shown once per experiment. A dialog per Run
    Scan would be punishment, and the user cannot act on it any faster
    the fifth time. }
  if FWarnedPlotFor = Exp.LabelText then Exit;

  Notes := [];

  { C6: one plot surface, so only the first @plot is drawn. }
  Skipped := Exp.SkippedPlots;
  if Length(Skipped) > 0 then
  begin
    Names := '';
    for I := 0 to High(Skipped) do
    begin
      if Names <> '' then Names := Names + ', ';
      Names := Names + Skipped[I].DisplayName;
    end;
    Notes := Notes + ['Iridium has a single plot surface, so it drew the ' +
      'first plot and did not draw: ' + Names + '.'];
  end;

  { 'y:' does not select on a scan — 'observables:' is required on @scan
    and already fills the checklist, and the checklist is the single
    authority for what the sweep reports (the same rule the steady-state
    Observables list follows). So 'y' is redundant here and is honoured by
    being consistent; where it is not, the figure differs from what the
    file asks for and that must be said rather than silently ignored. }
  Names := '';
  for Y in P.Y do
    if not IsReported(Y) then
    begin
      if Names <> '' then Names := Names + ', ';
      Names := Names + Y;
    end;
  if Names <> '' then
    Notes := Notes + ['The plot asks to draw ' + Names + ', which the scan ' +
      'does not report. On a scan the ''observables:'' list decides what is ' +
      'computed; add the name there to see it.'];

  { An overlay sweep names each trace '[S1]  X0=0.5' — one per observable
    per scan point — so a 'series:' block keyed on an observable name
    matches none of them. Chart-level keys are unaffected. }
  if (AMeasure = omTimeCourseOverlay) and (P.Series.Count > 0) then
    Notes := Notes + ['Per-series styling was not applied: a time-course ' +
      'overlay draws one trace per scan point, so its series are named ' +
      'after the observable and the parameter value together rather than ' +
      'after the observable alone.'];

  { Spec 11.7: an overlay scan has no legend worth placing — every scan
    point would need its own entry — so a 'legendposition' on one is
    reported rather than quietly honoured on a legend nobody can read. }
  if (AMeasure = omTimeCourseOverlay) and
     (P.LegendPosition <> lpDefault) then
    Notes := Notes + ['The plot sets a legend position, but a time-course ' +
      'overlay gives every scan point its own trace — there is no legend ' +
      'worth placing. Use a scalar measure (sample at, peak value, time ' +
      'to peak) for a figure with one entry per observable.'];

  if Length(Notes) = 0 then Exit;

  FWarnedPlotFor := Exp.LabelText;
  Names := '';
  for I := 0 to High(Notes) do
    Names := Names + Notes[I] + sLineBreak + sLineBreak;
  ShowMessage('Experiment ' + Exp.LabelText + ':' + sLineBreak + sLineBreak +
              Trim(Names));
end;

{ ── Population helpers ───────────────────────────────────────────────────── }

procedure TFrameParameterScan.PopulateParameterCombo;
var
  Names:    TArray<string>;
  N:        string;
  PrevName: string;
  RR:       TRoadRunner;
  Ids:      TStringList;
  I:        Integer;
begin
  PrevName := '';
  if cbParameter.ItemIndex >= 0 then
    PrevName := cbParameter.Items[cbParameter.ItemIndex];

  cbParameter.BeginUpdate;
  try
    cbParameter.Clear;
    if (FContext = nil) or (not FContext.Session.IsLoaded) then Exit;

    { Global parameters. }
    Names := FContext.Session.GetTunableNames;
    for N in Names do
      cbParameter.Items.Add(N);

    if PrevName <> '' then
      cbParameter.ItemIndex := cbParameter.Items.IndexOf(PrevName);
    if cbParameter.ItemIndex < 0 then
      cbParameter.ItemIndex := 0;
  finally
    cbParameter.EndUpdate;
  end;
end;

procedure TFrameParameterScan.ClearObservableLists;
begin
  lstFloating.Clear;
  lstBoundary.Clear;
  lstFluxes.Clear;
  lstRatesOfChange.Clear;
  SetLength(FSelectedObsNames, 0);
end;

procedure TFrameParameterScan.PopulateObservableLists;
var
  RR:   TRoadRunner;
  Ids:  TStringList;
  PrevChecked: TStringList;

  procedure CollectChecked(AList: TListBox; ATarget: TStringList);
  var J: Integer;
  begin
    for J := 0 to AList.Count - 1 do
      if AList.ListItems[J].IsChecked then
        ATarget.Add(AList.ListItems[J].Text);
  end;

  procedure FillList(AList: TListBox; AIds: TStringList;
                     APreserve: TStringList);
  var
    J:    Integer;
    Item: TListBoxItem;
  begin
    AList.BeginUpdate;
    AList.Clear;
    for J := 0 to AIds.Count - 1 do
    begin
      Item        := TListBoxItem.Create(AList);
      Item.Parent := AList;
      Item.Text   := AIds[J];
      Item.IsChecked := APreserve.IndexOf(AIds[J]) >= 0;
    end;
    AList.EndUpdate;
  end;

begin
  if (FContext = nil) or (not FContext.Session.IsLoaded) then
  begin
    ClearObservableLists;
    Exit;
  end;

  PrevChecked := TStringList.Create;
  try
    PrevChecked.Sorted := True;
    PrevChecked.CaseSensitive := True;
    CollectChecked(lstFloating,      PrevChecked);
    CollectChecked(lstBoundary,      PrevChecked);
    CollectChecked(lstFluxes,        PrevChecked);
    CollectChecked(lstRatesOfChange, PrevChecked);

    RR := FContext.Session.RoadRunner;

    Ids := RR.getFloatingSpeciesIds;
    try FillList(lstFloating, Ids, PrevChecked); finally Ids.Free; end;

    Ids := RR.getBoundarySpeciesIds;
    try FillList(lstBoundary, Ids, PrevChecked); finally Ids.Free; end;

    Ids := RR.getReactionIds;
    try FillList(lstFluxes, Ids, PrevChecked); finally Ids.Free; end;

    Ids := RR.getRatesOfChangeIds;
    try FillList(lstRatesOfChange, Ids, PrevChecked); finally Ids.Free; end;

    { On first-ever population (nothing was checked before), default
      to first floating species so the user has something to scan. }
    if (PrevChecked.Count = 0) and (lstFloating.Count > 0) then
      lstFloating.ListItems[0].IsChecked := True;

    UpdateSelectedObsLabel;
  finally
    PrevChecked.Free;
  end;
end;

{ ── UI state ─────────────────────────────────────────────────────────────── }

procedure TFrameParameterScan.UpdateSelectedObsLabel;
var
  Names: TArray<string>;
  I:     Integer;

  procedure CollectChecked(AList: TListBox);
  var J: Integer;
  begin
    for J := 0 to AList.Count - 1 do
      if AList.ListItems[J].IsChecked then
      begin
        SetLength(Names, Length(Names) + 1);
        Names[High(Names)] := AList.ListItems[J].Text;
      end;
  end;

begin
  SetLength(Names, 0);
  CollectChecked(lstFloating);
  CollectChecked(lstBoundary);
  CollectChecked(lstFluxes);
  CollectChecked(lstRatesOfChange);

  FSelectedObsNames := Names;
end;

function TFrameParameterScan.ActiveObservableList: TListBox;
begin
  if      TabControl1.ActiveTab = tabFloating      then Result := lstFloating
  else if TabControl1.ActiveTab = tabBoundary      then Result := lstBoundary
  else if TabControl1.ActiveTab = tabFluxes        then Result := lstFluxes
  else if TabControl1.ActiveTab = tabRatesOfChange then Result := lstRatesOfChange
  else                                                  Result := nil;
end;

{ Check/uncheck every item in AList in one pass. BeginUpdate keeps the listbox
  from repainting per item, and FSelectedObsNames is rebuilt once at the end
  rather than relying on per-item check events. }
procedure TFrameParameterScan.SetAllChecked(AList: TListBox; AChecked: Boolean);
var
  I: Integer;
begin
  if AList = nil then Exit;

  AList.BeginUpdate;
  try
    for I := 0 to AList.Count - 1 do
      AList.ListItems[I].IsChecked := AChecked;
  finally
    AList.EndUpdate;
  end;

  UpdateSelectedObsLabel;
end;

procedure TFrameParameterScan.UpdateRangeMode;
var
  IsSweep: Boolean;
begin
  IsSweep := rbLinear.IsChecked or rbLog.IsChecked;
  layoutSweep.Visible := IsSweep;
  layoutList.Visible  := not IsSweep;
end;

procedure TFrameParameterScan.UpdateMeasureMode;
var
  IsEndpoint: Boolean;
begin
  IsEndpoint := rbEndpoint.IsChecked;
  edtSampleTime.Enabled := IsEndpoint;
  lblSampleTime.Enabled := IsEndpoint;
end;

{ ── Radio button handlers ────────────────────────────────────────────────── }

procedure TFrameParameterScan.rbRangeModeChange(Sender: TObject);
begin
  UpdateRangeMode;
end;

procedure TFrameParameterScan.rbOutputMeasureChange(Sender: TObject);
begin
  UpdateMeasureMode;
end;

{ ── Observable list click ────────────────────────────────────────────────── }

procedure TFrameParameterScan.lstObservableCheckChanged(Sender: TObject);
begin
  UpdateSelectedObsLabel;
end;

{ ── Accessors ────────────────────────────────────────────────────────────── }

function TFrameParameterScan.ActiveRangeMode: TScanRangeMode;
begin
  if rbLog.IsChecked       then Result := srmLog
  else if rbList.IsChecked then Result := srmList
  else                          Result := srmLinear;
end;

function TFrameParameterScan.ActiveOutputMeasure: TOutputMeasure;
begin
  if rbPeakValue.IsChecked            then Result := omPeakValue
  else if rbTimeToPeak.IsChecked      then Result := omTimeToPeak
  else if rbTimeCourseOverlay.IsChecked then Result := omTimeCourseOverlay
  else                                     Result := omEndpoint;
end;

{ ── Scan value construction ──────────────────────────────────────────────── }

function TFrameParameterScan.ParseValueList(const AText: string): TArray<Double>;
var
  Parts: TArray<string>;
  I:     Integer;
  V:     Double;
begin
  Parts := AText.Split([',']);
  SetLength(Result, Length(Parts));
  for I := 0 to High(Parts) do
  begin
    if not TryStrToFloat(Parts[I].Trim, V) then
    begin
      SetLength(Result, 0);
      Exit;
    end;
    Result[I] := V;
  end;
end;

function TFrameParameterScan.BuildScanValues: TArray<Double>;
var
  VStart, VEnd: Double;
  N, I:         Integer;
  Step:         Double;
begin
  SetLength(Result, 0);
  case ActiveRangeMode of

    srmLinear:
    begin
      VStart := strtofloat (edtScanStart.Text); // has already been valided
      VEnd   := strtofloat (edtScanEnd.Text);
      N      := strtoint (edtScanNPoints.Text);
      if N < 2 then Exit;
      SetLength(Result, N);
      Step := (VEnd - VStart) / (N - 1);
      for I := 0 to N - 1 do
        Result[I] := VStart + I * Step;
    end;

    srmLog:
    begin
      VStart := strtofloat (edtScanStart.Text);
      VEnd   := strtofloat (edtScanEnd.Text);
      N      := strtoint(edtScanNPoints.Text);
      if (N < 2) or (VStart <= 0) or (VEnd <= 0) then Exit;
      SetLength(Result, N);
      Step := (Log10(VEnd) - Log10(VStart)) / (N - 1);
      for I := 0 to N - 1 do
        Result[I] := Power(10, Log10(VStart) + I * Step);
    end;

    srmList:
      Result := ParseValueList(edtValueList.Text);
  end;
end;

procedure TFrameParameterScan.cboColorpaletteChange(Sender: TObject);
begin
  ColorList.SetPalette (cboColorPalette.Items[cboColorPalette.itemindex], 12);
  { Reassign colours on the existing series rather than re-running the
    scan. For large models the simulate-and-replot cost dominates, and
    a palette swap doesn't change the data — only how it's drawn. }
  if FHasData then
  begin
    ColorList.Restart;
    FContext.PlotRecolorSimulationSeries(
      function: TAlphaColor
      begin
        Result := ColorList.NextColor;
      end);
  end;
end;

function TFrameParameterScan.FindColumn(AData: T2DMatrix;
                                        const AColName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to AData.columnHeader.Count - 1 do
    if SameText(AData.columnHeader[I], AColName) then
      Exit(I);
end;

{ ── Scalar extraction from one simulation run ────────────────────────────── }

function TFrameParameterScan.ExtractScalar(AData: T2DMatrix;
                                           const AColName: string;
                                           AMeasure: TOutputMeasure;
                                           ASampleTime: Double): Double;
var
  Col, TimeCol: Integer;
  Row:          Integer;
  Val, BestVal: Double;
  BestRow:      Integer;
  TimeDiff:     Double;
  BestTimeDiff: Double;
begin
  Result := 0;
  Col := FindColumn(AData, AColName);
  if Col < 0 then Exit;

  case AMeasure of

    omEndpoint:
    begin
      { Find the row whose time value is closest to ASampleTime. }
      TimeCol      := FindColumn(AData, TIME_COLUMN_LABEL);
      BestRow      := AData.r - 1;  { default to last row }
      BestTimeDiff := MaxDouble;
      if TimeCol >= 0 then
        for Row := 0 to AData.r - 1 do
        begin
          TimeDiff := Abs(AData[Row, TimeCol] - ASampleTime);
          if TimeDiff < BestTimeDiff then
          begin
            BestTimeDiff := TimeDiff;
            BestRow      := Row;
          end;
        end;
      Result := AData[BestRow, Col];
    end;

    omPeakValue:
    begin
      BestVal := AData[0, Col];
      for Row := 1 to AData.r - 1 do
      begin
        Val := AData[Row, Col];
        if Val > BestVal then BestVal := Val;
      end;
      Result := BestVal;
    end;

    omTimeToPeak:
    begin
      TimeCol := FindColumn(AData, TIME_COLUMN_LABEL);
      BestVal := AData[0, Col];
      BestRow := 0;
      for Row := 1 to AData.r - 1 do
      begin
        Val := AData[Row, Col];
        if Val > BestVal then
        begin
          BestVal := Val;
          BestRow := Row;
        end;
      end;
      if TimeCol >= 0 then
        Result := AData[BestRow, TimeCol]
      else
        Result := BestRow;  { fall back to row index if no time column }
    end;

  end; { case }
end;

{ ── Run Scan ────────────────────────────────────────────────────────────────────────────── }

procedure TFrameParameterScan.btnRunScanClick(Sender: TObject);
var
  RR:           TRoadRunner;
  ParamName:    string;
  ScanValues:   TArray<Double>;
  OrigParamVal: Double;
  Measure:      TOutputMeasure;
  TStart, TEnd: Double;
  SampleTime:   Double;
  NSimPoints:   Integer;
  I, Row:       Integer;
  SimData:      T2DMatrix;
  ResultMatrix: T2DMatrix;   { scalar-mode: N rows x 2 cols }
  ScalarVal:    Double;
  ObsColIdx:    Integer;
  TimeColIdx:   Integer;
  YNames:       TArray<string>;
  ParamColName: string;
  SeriesLabel:  string;
begin
  ColorList.Restart;
  { ── 1. Validate ── }
  if FContext = nil then Exit;

  { Load (or reload, if the source was edited) the model on demand so the user
    doesn't have to visit the time-course panel first. A successful load fires
    the session's reloaded event, which populates the parameter combo and the
    observable lists below. }
  try
    if not FContext.Session.EnsureLoaded then
    begin
      ShowMessage('Cannot load model: ' + FContext.Session.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Model load failed: ' + E.Message);
      Exit;
    end;
  end;

  if cbParameter.ItemIndex < 0 then
  begin
    ShowMessage('Please select a parameter to scan.');
    Exit;
  end;

  if Length(FSelectedObsNames) = 0 then
  begin
    ShowMessage('Please check at least one observable.');
    Exit;
  end;

  ScanValues := BuildScanValues;
  if Length(ScanValues) = 0 then
  begin
    ShowMessage('No scan values. Check the range settings, eg if you are doing a log scan make sure you do not start at zero.');
    Exit;
  end;

  { ── 2. Gather settings ── }
  ParamName  := cbParameter.Items[cbParameter.ItemIndex];
  Measure    := ActiveOutputMeasure;
  TStart     := strtofloat (edtTimeStart.Text);
  TEnd       := strtofloat (edtTimeEnd.Text);
  SampleTime := strtofloat (edtSampleTime.Text);
  NSimPoints := Max(2, strtoint (edtNumPoints.Text));

  { The sample time only means anything for the endpoint measure, and it has to
    fall inside the simulated interval. ExtractScalar picks the row with the
    closest time, so a sample time past the end would silently clamp to the last
    point; extend the run instead so the requested time is actually simulated.
    A sample time before the start can't be fixed this way, so it's an error. }
  if Measure = omEndpoint then
  begin
    if SampleTime > TEnd then
    begin
      TEnd := SampleTime;
      edtTimeEnd.Text := FloatToStr(TEnd);
    end;
    if SampleTime < TStart then
    begin
      ShowMessage(Format('Sample time (%g) is less than the simulation ' +
        'start time (%g). Choose a sample time within the simulation interval.',
        [SampleTime, TStart]));
      Exit;
    end;
  end;

  RR := FContext.Session.RoadRunner;

  { Tell RoadRunner which columns to return. simulateEx otherwise yields only
    its default selection (time + floating species), so observables that are
    reaction fluxes, boundary species, or rates of change — e.g. scanning A and
    watching the flux J1 — never appear in the result matrix and the scan looks
    empty. Mirror the time-course frame: time first, then every selected
    observable, deduplicated. }
  var Selection := TStringList.Create;
  try
    Selection.CaseSensitive := True;   { SBML ids are case-sensitive }
    Selection.Add(TIME_COLUMN_LABEL);
    for var N := 0 to High(FSelectedObsNames) do
      if Selection.IndexOf(FSelectedObsNames[N]) < 0 then
        Selection.Add(FSelectedObsNames[N]);
    RR.setTimeCourseSelectionListEx(Selection);
  finally
    Selection.Free;
  end;

  { ── 3. Save original parameter value ── }
  OrigParamVal := RR.getValue(AnsiString(ParamName));

  { No 'set:' handling here: the selected experiment's values are already in
    the engine, written when it was selected. The loop resets between
    points, which is why species were written through their init() form as
    well — that is the form a reset preserves. }

  { ── 4. Prepare storage ── }
  ResultMatrix := nil;

  SimData      := nil;

  if Measure <> omTimeCourseOverlay then
  begin
    ParamColName := ParamName;
    ResultMatrix := T2DMatrix.Create(Length(ScanValues), Length(FSelectedObsNames) + 1);
    ResultMatrix.columnHeader.Add(ParamColName);
    for var N := 0 to High(FSelectedObsNames) do
      ResultMatrix.columnHeader.Add(FSelectedObsNames[N]);
  end;

  if chkProgressBar.IsChecked then
     begin
     pbScanProgress.Min     := 0;
     pbScanProgress.Max     := Length(ScanValues);
     pbScanProgress.Value   := 0;
     pbScanProgress.Visible := True;
     end;

  try
    { Snapshot current plot styling before we destroy this scan's series, so
      the user's edits survive the rebuild and a later return to this frame.
      Skipped on the slider path: OnSliderChanged has already snapshotted and
      then cleared the series, so snapshotting again here would overwrite the
      good styling with an empty set. }
    if not FRunningScan then
      FContext.PlotBeginRebuild;

    { ── 5. Scan loop ── }
    { For overlay mode, clear the plot first so traces don't accumulate
      across multiple Run Scan clicks. }
    if Measure = omTimeCourseOverlay then
      FContext.PlotClearSimulationSeries;

    for I := 0 to High(ScanValues) do
    begin
      RR.setValue(AnsiString(ParamName), ScanValues[I]);
      RR.reset;

      SimData := RR.simulateEx(TStart, TEnd, NSimPoints);
      try
        TimeColIdx := FindColumn(SimData, TIME_COLUMN_LABEL);

        { Process each selected observable for this scan value. }
        for var OIdx := 0 to High(FSelectedObsNames) do
        begin
          ObsColIdx := FindColumn(SimData, FSelectedObsNames[OIdx]);
          if ObsColIdx < 0 then Continue;

          if Measure = omTimeCourseOverlay then
          begin
            { One series per observable per scan value.
              Label: "obsName  param=value" }
            SeriesLabel := FSelectedObsNames[OIdx] + '  ' +
                           ParamName + '=' + FormatFloat('0.####', ScanValues[I]);
            var S := TPlotSeries.Create(SeriesLabel, ColorList.NextColor);
            S.MarkerVisible := False;
            for Row := 0 to SimData.r - 1 do
              S.AddXY(SimData[Row, TimeColIdx], SimData[Row, ObsColIdx]);
            FContext.PlotAddSeries(S);
          end
          else
          begin
            { Scalar modes: accumulate into the column for this observable. }
            ScalarVal := ExtractScalar(SimData, FSelectedObsNames[OIdx], Measure, SampleTime);
            ResultMatrix[I, 0]       := ScanValues[I];
            ResultMatrix[I, OIdx + 1] := ScalarVal;
          end;
        end;

      finally
        SimData.Free;
        SimData := nil;
      end;

      if chkProgressBar.IsChecked then
         pbScanProgress.Value := I + 1;

      { ProcessMessages is here so button-triggered scans show a live progress
      bar, stay cancelable, and don't appear hung to the OS. Suppressed during
      slider-driven scans (FRunningScan set by OnSliderChanged) — those complete
      in fractions of a second and the intermediate paints were causing visible
      flicker on macOS during fast slider drags. See section [whatever] of the
      session summary, [date]. }
      if not FRunningScan then
        Application.ProcessMessages;

    end;

    { ── 6. Restore original parameter value ── }
    RR.setValue(AnsiString(ParamName), OrigParamVal);
    RR.reset;

    { ── 7. Plot ── }
    if Measure = omTimeCourseOverlay then
      FContext.PlotRedraw
    else if ResultMatrix <> nil then
    begin
      YNames := Copy(FSelectedObsNames);
      FContext.PlotData(ResultMatrix, ParamColName, YNames);
      FreeAndNil(ResultMatrix);
    end;

    { Re-apply this frame's saved styling to the freshly built series. }
    FContext.PlotEndRebuild;

    { Set the x-axis title AFTER PlotEndRebuild — and after the @plot, which
      also restores a styling snapshot. Either one carries the previous run's
      x-axis title, so a time-course-overlay scan's 'time' would otherwise
      clobber the parameter name on a following endpoint scan, and vice versa.
      Failing an explicit 'xlabel:', the scan x-axis is mode-derived: time for
      overlay traces, the scanned parameter for scalar measures. }
    if not ApplyPlotMetadata(Measure) then
    begin
      if Measure = omTimeCourseOverlay then
        FContext.PlotSetXAxisTitle(TIME_COLUMN_LABEL)
      else
        FContext.PlotSetXAxisTitle(ParamName);
    end;

    FContext.PlotRedraw;

    FHasData := True;

  except
    on E: Exception do
    begin
      RR.setValue(AnsiString(ParamName), OrigParamVal);
      RR.reset;
      FreeAndNil(ResultMatrix);
      ShowMessage('Scan error: ' + E.Message);
    end;
  end;

  pbScanProgress.Visible := False;
end;


procedure TFrameParameterScan.btnRunScanMouseLeave(Sender: TObject);
begin
  TButton(Sender).Enabled := False;
  TButton(Sender).Enabled := True;
end;

procedure TFrameParameterScan.btnScanSelectAllClick(Sender: TObject);
begin
  { Only the visible tab's list — the other tabs hold different kinds of
    observable and the user can't see what they'd be agreeing to. }
  SetAllChecked(ActiveObservableList, True);
end;

procedure TFrameParameterScan.btnScanSlidersClick(Sender: TObject);
var
  Names:  TArray<string>;
  Values: TArray<Double>;
begin
  if FContext = nil then Exit;

  try
    if not FContext.Session.EnsureLoaded then
    begin
      ShowMessage('Cannot load model: ' + FContext.Session.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Model load failed: ' + E.Message);
      Exit;
    end;
  end;

  if not FContext.SliderContainer.ParamPanelVisible then
  begin
    Names  := FContext.Session.GetTunableNames;
    Values := FContext.Session.GetTunableValues;
    FContext.SliderContainer.LoadParams(Names, Values);
  end;

  { Lock the currently-selected scan parameter so the user can't drive
    it from a slider while it's being swept. }
  UpdateScanParameterLock;

  FContext.SliderContainer.ToggleParamPanel;
end;

procedure TFrameParameterScan.btnScanUnSelectAllClick(Sender: TObject);
begin
  SetAllChecked(ActiveObservableList, False);
end;

{ ── Slider integration ───────────────────────────────────────────────────── }

procedure TFrameParameterScan.UpdateScanParameterLock;
var
  ParamName: string;
begin
  if FContext = nil then Exit;

  { The lock belongs to whichever frame currently owns the slider panel.
    If this frame isn't the active one, leave the lock alone — ShowAnalysisFrame
    will set it correctly when the user switches to us. }
  if not Self.Visible then Exit;

  if cbParameter.ItemIndex >= 0 then
    ParamName := cbParameter.Items[cbParameter.ItemIndex]
  else
    ParamName := '';
  FContext.SliderContainer.SetLockedParam(ParamName);
end;


procedure TFrameParameterScan.DoParameterChange(Sender: TObject);
begin
  { Whenever the user picks a different scan parameter, swap the lock:
    the old scan parameter's slider (if any) becomes editable again,
    and the new one is locked. Only matters when the slider panel is
    actually showing for this frame, but SetLockedParam is cheap and
    safe to call regardless. }
  UpdateScanParameterLock;
end;

procedure TFrameParameterScan.edtNumPointsExit(Sender: TObject);
var Value : Integer;
begin
 if not TryStrToInt(edtNumPoints.Text.Trim, Value) then
     begin
     showmessage ('Number of point not entered correctly, must be an integer');
     edtNumPoints.SetFocus;
     end;
end;

procedure TFrameParameterScan.edtNumPointsKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeysInteger(edtNumPoints, Key, KeyChar, Shift);
end;

procedure TFrameParameterScan.edtSampleTimeExit(Sender: TObject);
var Value, TEnd : Double;
begin
 if not TryStrToFloat(edtSampleTime.Text.Trim, Value) then
     begin
     showmessage ('Sample time value not entered correctly');
     edtSampleTime.SetFocus;
     Exit;
     end;

 { Sampling past the end of the run is a request for a longer run, so push the
   end time out to match rather than silently sampling the last point. }
 if TryStrToFloat(edtTimeEnd.Text.Trim, TEnd) and (Value > TEnd) then
    edtTimeEnd.Text := FloatToStr(Value);
end;

procedure TFrameParameterScan.edtSampleTimeKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtSampleTime, Key, KeyChar, Shift);
end;

procedure TFrameParameterScan.edtScanEndExit(Sender: TObject);
var Value : Double;
begin
 if not TryStrToFloat(edtScanEnd.Text.Trim, Value) then
     begin
     showmessage ('Scan time end number not entered correctly');
     edtScanEnd.SetFocus;
     end;
end;

procedure TFrameParameterScan.edtScanEndKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtScanEnd, Key, KeyChar, Shift);
end;

procedure TFrameParameterScan.edtScanNPointsExit(Sender: TObject);
var Value : Integer;
begin
 if not TryStrToInt(edtScanNPoints.Text.Trim, Value) then
     begin
     showmessage ('Scan number of poionts not entered correctly, it should be an integer');
     edtScanNPoints.SetFocus;
     end;
end;

procedure TFrameParameterScan.edtScanNPointsKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeysInteger(edtScanNPoints, Key, KeyChar, Shift);
end;

procedure TFrameParameterScan.edtScanStartExit(Sender: TObject);
var Value : Double;
begin
 if not TryStrToFloat(edtScanStart.Text.Trim, Value) then
     begin
     showmessage ('Scan time start number not entered correctly');
     edtScanStart.SetFocus;
     end;
end;

procedure TFrameParameterScan.edtScanStartKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtScanStart, Key, KeyChar, Shift);
end;

procedure TFrameParameterScan.edtTimeEndExit(Sender: TObject);
var Value : Double;
begin
 if not TryStrToFloat(edtTimeEnd.Text.Trim, Value) then
     begin
     showmessage ('Time end number not entered correctly');
     edtTimeEnd.SetFocus;
     end;
end;

procedure TFrameParameterScan.edtTimeEndKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtTimeEnd, Key, KeyChar, Shift);
end;

procedure TFrameParameterScan.edtTimeStartExit(Sender: TObject);
var Value : Double;
begin
 if not TryStrToFloat(edtTimeStart.Text.Trim, Value) then
     begin
     showmessage ('Time start number not entered correctly');
     edtTimeStart.SetFocus;
     end;
end;

procedure TFrameParameterScan.edtTimeStartKeyDown(Sender: TObject;
  var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtTimeStart, Key, KeyChar, Shift);
end;

procedure TFrameParameterScan.OnSliderChanged(Sender: TObject;
  const ASliderString: string; const AValue: Single);
begin
  if FContext = nil then Exit;
  if not FContext.Session.IsLoaded then Exit;

  { Re-entrancy guard. btnRunScanClick pumps Application.ProcessMessages
    inside its loop, which can deliver further slider events. Drop them
    while a scan is in flight — one scan at a time. }
  if FRunningScan then Exit;

  { A slider may move a quantity the experiment's 'set:' section wrote: the
    value lives in the engine rather than being re-imposed per sweep, so
    sliding explores the model FROM the experiment's conditions and the
    experiment stays selected. }
  FContext.Session.SetParameterValue(ASliderString, AValue);

  FRunningScan := True;
  try
    { Snapshot styling BEFORE the clear below: PlotBeginRebuild captures the
      styling of the series that are live at that moment, so clearing first
      would store an empty set and the user's colour edits would be lost on
      every slider move. btnRunScanClick skips its own PlotBeginRebuild while
      FRunningScan is set, so this snapshot is the one that survives. }
    FContext.PlotBeginRebuild;

    { btnRunScanClick clears simulation series at different points
      depending on mode (start for overlay, end via PlotData for
      scalar). That's fine when triggered by the button, but with rapid
      slider events it leaves a window where old series are still
      present while the next scan is building, so curves pile up.
      Clearing explicitly here guarantees a clean canvas every time. }
    FContext.PlotClearSimulationSeries;
    btnRunScanClick(nil);
  finally
    FRunningScan := False;
  end;
end;

{ ── Reset ────────────────────────────────────────────────────────────────── }

procedure TFrameParameterScan.btnResetScanClick(Sender: TObject);
begin
  FHasData               := False;
  pbScanProgress.Value   := 0;
  pbScanProgress.Visible := False;
end;


{ Called when this frame becomes the visible analysis. Edits made in the editor
  while another panel was showing leave the parameter combo and the observable
  lists describing the previous model, so re-parse and let SessionModelReloaded
  repopulate them.

  Failure is deliberately silent: switching to this panel with a half-typed
  model should not pop a dialog. The lists simply keep their previous contents,
  and btnRunScanClick reports the parse error when the user actually runs. }
procedure TFrameParameterScan.RefreshFromModelIfStale;
begin
  if FContext = nil then Exit;
  if not FContext.Session.IsDirty then Exit;

  try
    FContext.Session.EnsureLoaded;
  except
    { Reported at Run Scan time, not on a tab switch. }
  end;
end;


procedure TFrameParameterScan.SetCheckedObservables(const ANames: TArray<string>);

  function Wanted(const AName: string): Boolean;
  var N: string;
  begin
    for N in ANames do
      if N = AName then Exit(True);
    Result := False;
  end;

  procedure ApplyTo(AList: TListBox);
  var I: Integer;
  begin
    for I := 0 to AList.Count - 1 do
      AList.ListItems[I].IsChecked := Wanted(AList.ListItems[I].Text);
  end;

begin
  ApplyTo(lstFloating);
  ApplyTo(lstBoundary);
  ApplyTo(lstFluxes);
  ApplyTo(lstRatesOfChange);
  UpdateSelectedObsLabel;   { rebuild FSelectedObsNames from the new checks }
end;

procedure TFrameParameterScan.AttachToSliders;
begin
  if FContext = nil then Exit;
  FContext.SliderContainer.OnSliderChanged := OnSliderChanged;
  FContext.SliderContainer.ReleaseOnlyMode := False;
end;


function ColorToPyHex(C: TAlphaColor): string;
begin
  Result := Format('''#%.2x%.2x%.2x''',
    [TAlphaColorRec(C).R, TAlphaColorRec(C).G, TAlphaColorRec(C).B]);
end;


function TFrameParameterScan.GetPythonScript(const AntimonyText: string): string;
const
  IND = '    ';   { Python 4-space indent }
var
  SB:        TStringBuilder;
  ScanParam: string;
  Measure:   TOutputMeasure;
  RangeMode: TScanRangeMode;
  Fmt:       TFormatSettings;
  I:         Integer;
  Vals:      TArray<Double>;

  function FStr(V: Double): string;
  begin
    Result := FloatToStr(V, Fmt);
  end;

begin
  { Invariant locale so we emit '0.5' not '0,5' regardless of system locale. }
  Fmt := TFormatSettings.Invariant;

  if FContext = nil then
    Exit('# Iridium: no analysis context.');
  if not FContext.Session.IsLoaded then
    Exit('# Iridium: no model loaded.');
  if cbParameter.ItemIndex < 0 then
    Exit('# Iridium: no scan parameter selected.');
  if Length(FSelectedObsNames) = 0 then
    Exit('# Iridium: no observables selected.');

  ScanParam := cbParameter.Items[cbParameter.ItemIndex];
  Measure   := ActiveOutputMeasure;
  RangeMode := ActiveRangeMode;

  SB := TStringBuilder.Create;
  try
    { ── Header ───────────────────────────────────────────────────────────── }
    SB.AppendLine('# Python script generated by Iridium parameter scan.');
    SB.AppendLine('# Reproduces the scan using Tellurium.');
    SB.AppendLine;
    SB.AppendLine('import tellurium as te');
    SB.AppendLine('import matplotlib.pyplot as plt');
    SB.AppendLine('import numpy as np');
    SB.AppendLine;

    { ── Model ────────────────────────────────────────────────────────────── }
    SB.AppendLine('# ── Model ─────────────────────────────────────────────');
    SB.AppendLine('r = te.loada(r"""');
    SB.AppendLine(AntimonyText.TrimRight);
    SB.AppendLine('""")');
    SB.AppendLine;

    { ── Selection ────────────────────────────────────────────────────────── }
    SB.AppendLine('# ── Selection ─────────────────────────────────────────');
    SB.Append('selection = [''time''');
    for I := 0 to High(FSelectedObsNames) do
    begin
      SB.Append(', ''');
      SB.Append(FSelectedObsNames[I]);
      SB.Append('''');
    end;
    SB.AppendLine(']');
    SB.AppendLine;

    { ── Time course settings ─────────────────────────────────────────────── }
    SB.AppendLine('# ── Time course settings ──────────────────────────────');
    SB.AppendLine('time_start = ' + edtTimeStart.Text);
    SB.AppendLine('time_end   = ' + edtTimeEnd.Text);
    SB.AppendLine('num_points = ' + edtNumPoints.Text);
    SB.AppendLine;

    { ── Scan parameter and range ─────────────────────────────────────────── }
    SB.AppendLine('# ── Scan parameter and range ──────────────────────────');
    SB.AppendLine('scan_param = ''' + ScanParam + '''');
    case RangeMode of
      srmLinear:
        SB.AppendLine(Format('scan_values = np.linspace(%s, %s, %s)',
          [edtScanStart.Text,
           edtScanEnd.Text,
           edtScanNPoints.Text]));

      srmLog:
        SB.AppendLine(Format('scan_values = np.logspace(np.log10(%s), np.log10(%s), %s)',
          [edtScanStart.Text,
           edtScanEnd.Text,
           edtScanNPoints.Text]));

      srmList:
        begin
          Vals := ParseValueList(edtValueList.Text);
          SB.Append('scan_values = [');
          for I := 0 to High(Vals) do
          begin
            if I > 0 then SB.Append(', ');
            SB.Append(FStr(Vals[I]));
          end;
          SB.AppendLine(']');
        end;
    end;
    SB.AppendLine;

    { ── Colors (read directly from on-screen series) ────────────────────── }
    var SeriesInfo := FContext.PlotGetSimulationSeriesInfo;

    if Length(SeriesInfo) = 0 then
    begin
      SB.AppendLine('# No prior scan in the plot; matplotlib defaults will be used.');
      SB.AppendLine('colors = None');
      SB.AppendLine;
    end
    else
    begin
      case Measure of
        omTimeCourseOverlay:
        begin
          { GUI iteration: outer scan value, inner observable (line 612 of
            uFrameParameterScan). Script's loop nests the same way, so a flat
            list indexed in iteration order matches by construction. }
          SB.AppendLine('# Colors below match the on-screen plot exactly,');
          SB.AppendLine('# read from the live series. Edit to restyle.');
          SB.Append('colors = [');
          for I := 0 to High(SeriesInfo) do
          begin
            if I > 0 then SB.Append(', ');
            SB.Append(ColorToPyHex(SeriesInfo[I].LineColor));
          end;
          SB.AppendLine(']');
          SB.AppendLine;
        end;

        omEndpoint, omPeakValue, omTimeToPeak:
        begin
          { Scalar modes: one series per observable, series name == observable
            name. Emit a dict so lookup is by name, robust to whatever order
            PlotData uses internally. }
          SB.AppendLine('# Colors keyed by observable name, read from the live plot.');
          SB.AppendLine('colors = {');
          for I := 0 to High(SeriesInfo) do
          begin
            SB.Append(IND);
            SB.Append('''');
            SB.Append(SeriesInfo[I].Name);
            SB.Append(''': ');
            SB.Append(ColorToPyHex(SeriesInfo[I].LineColor));
            if I < High(SeriesInfo) then SB.Append(',');
            SB.AppendLine;
          end;
          SB.AppendLine('}');
          SB.AppendLine;
        end;
      end;
    end;
    { ── Scan loop (varies by output measure) ─────────────────────────────── }
    SB.AppendLine('# ── Scan ──────────────────────────────────────────────');

    case Measure of
      omTimeCourseOverlay:
      begin
      SB.AppendLine('color_idx = 0');
      SB.AppendLine('for k in scan_values:');
      SB.AppendLine(IND + 'r.reset()');
      SB.AppendLine(IND + 'r.setValue(scan_param, k)');
      SB.AppendLine(IND + 'm = r.simulate(time_start, time_end, num_points, selection)');
      SB.AppendLine(IND + 'for j in range(1, len(selection)):');
      SB.AppendLine(IND + IND + 'c = colors[color_idx] if colors else None');
      SB.AppendLine(IND + IND + 'plt.plot(m[:, 0], m[:, j], color=c,');
      SB.AppendLine(IND + IND + '         label=f''{selection[j]}, {scan_param}={k:.3g}'')');
      SB.AppendLine(IND + IND + 'color_idx += 1');
      end;

      omEndpoint, omPeakValue, omTimeToPeak:
      begin
        case Measure of
          omEndpoint:    SB.AppendLine('# Output: endpoint value of each observable vs scan parameter.');
          omPeakValue:   SB.AppendLine('# Output: peak value of each observable vs scan parameter.');
          omTimeToPeak:  SB.AppendLine('# Output: time to peak of each observable vs scan parameter.');
        end;

        SB.AppendLine('results = {sp: [] for sp in selection[1:]}');
        SB.AppendLine('for k in scan_values:');
        SB.AppendLine(IND + 'r.reset()');
        SB.AppendLine(IND + 'r.setValue(scan_param, k)');
        SB.AppendLine(IND + 'm = r.simulate(time_start, time_end, num_points, selection)');
        SB.AppendLine(IND + 'for j, sp in enumerate(selection[1:]):');

        case Measure of
          omEndpoint:
            SB.AppendLine(IND + IND + 'results[sp].append(float(m[-1, j + 1]))');
          omPeakValue:
            SB.AppendLine(IND + IND + 'results[sp].append(float(np.max(m[:, j + 1])))');
          omTimeToPeak:
            SB.AppendLine(IND + IND + 'results[sp].append(float(m[np.argmax(m[:, j + 1]), 0]))');
        end;

        SB.AppendLine;
        SB.AppendLine('for sp in results:');
        SB.AppendLine(IND + 'c = colors[sp] if colors else None');
        SB.AppendLine(IND + 'plt.plot(scan_values, results[sp], marker=''o'', color=c, label=sp)');
        SB.AppendLine;
        SB.AppendLine('plt.xlabel(scan_param)');
        case Measure of
          omEndpoint:    SB.AppendLine('plt.ylabel(''Endpoint value'')');
          omPeakValue:   SB.AppendLine('plt.ylabel(''Peak value'')');
          omTimeToPeak:  SB.AppendLine('plt.ylabel(''Time to peak'')');
        end;
      end;
    end;

    SB.AppendLine('plt.legend()');
    SB.AppendLine('plt.show()');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;


initialization
  ColorList:= TColorList.Create;
finalization
  ColorList.Free;
end.
