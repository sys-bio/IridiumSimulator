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
  uRR2DSimpleMatrix,
  uColorList,
  FMX.EditBox, FMX.Objects;

type
  TScanRangeMode  = (srmLinear, srmLog, srmList);
  TOutputMeasure  = (omEndpoint, omPeakValue, omTimeToPeak, omTimeCourseOverlay);

  TFrameParameterScan = class(TFrame, IPythonScriptExporter)
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
    btnExportPython: TButton;

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
    procedure btnExportPythonClick(Sender: TObject);

  private
    FContext:          IAnalysisContext;
    FHasData:          Boolean;
    FSelectedObsNames: TArray<string>;
    FRunningScan:      Boolean;   { re-entrancy guard for slider-driven scans }

    procedure SessionStateChanged  (Sender: TObject);
    procedure SessionModelReloaded (Sender: TObject; AParameterSetChanged: Boolean);

    procedure PopulateParameterCombo;
    procedure PopulateObservableLists;
    procedure ClearObservableLists;
    procedure UpdateRangeMode;
    procedure UpdateMeasureMode;
    procedure UpdateSelectedObsLabel;

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
    procedure SetContext(const AContext: IAnalysisContext);
    procedure UpdateScanParameterLock;
    procedure AttachToSliders;
  end;

implementation

{$R *.fmx}

uses
  System.Math,
  uRoadRunner,
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
end;

procedure TFrameParameterScan.SessionModelReloaded(Sender: TObject;
                                                    AParameterSetChanged: Boolean);
begin
  FHasData := False;
  PopulateParameterCombo;
  PopulateObservableLists;
  { If the slider panel is currently showing, the previous lock may now
    point at a parameter that no longer exists, or the auto-selected
    new scan parameter needs to be locked. }
  if (FContext <> nil) and FContext.SliderContainer.ParamPanelVisible then
    UpdateScanParameterLock;
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
  if (FContext = nil) or (not FContext.Session.IsLoaded) then
  begin
    ShowMessage('Please load a model first by running a time course simulation.');
    Exit;
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

  RR := FContext.Session.RoadRunner;

  { ── 3. Save original parameter value ── }
  OrigParamVal := RR.getValue(AnsiString(ParamName));

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
var Value : Double;
begin
 if not TryStrToFloat(edtSampleTime.Text.Trim, Value) then
     begin
     showmessage ('Sample time value not entered correctly');
     edtSampleTime.SetFocus;
     end;
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

  FContext.Session.SetParameterValue(ASliderString, AValue);

  FRunningScan := True;
  try
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

procedure TFrameParameterScan.btnExportPythonClick(Sender: TObject);
begin
   FContext.CopyTextToTextWindow (GetPythonScript(FContext.Session.GetCurrentAntimonyText));
end;

procedure TFrameParameterScan.btnResetScanClick(Sender: TObject);
begin
  FHasData               := False;
  pbScanProgress.Value   := 0;
  pbScanProgress.Visible := False;
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
