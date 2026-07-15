unit uFrameSteadyState;

{ Steady-state analysis frame -- CONTROL PANEL ONLY.

  Output (the six grids) is rendered into the host scrollbox on the
  right-hand TabControl1 / tbSteadyState tab, reached via the analysis
  context (IAnalysisContext.SteadyStateHost). The frame holds the
  control surface: Compute button, Scaling combo, and Sliders button.

  Six output sections are stacked vertically in the host scrollbox:
    - Concentrations of floating species
    - Jacobian
    - Eigenvalues of the Jacobian
    - Scaled or unscaled elasticities
    - Flux control coefficients (scaled or unscaled)
    - Concentration control coefficifsectionents (scaled or unscaled)

  The Scaling combo refetches the three scaling-dependent matrices
  without recomputing the steady state. Slider integration uses
  OnSliderReleased (recompute only on thumb release) -- steady-state
  solves are less robust than time-course integrations.

  Note: getUnscaledConcentrationControlCoefficientMatrix must be added
  as a public wrapper in uRoadRunner.pas. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, System.Rtti,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Grid, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Layouts, FMX.ListBox, FMX.Edit,
  uAnalysisTypes, uFrameSliderContainer, uRR2DSimpleMatrix, FMX.EditBox,
  FMX.Text,
  System.Generics.Collections,
  ufBar3DWindow,
  FMX.Platform,
  FMX.SpinBox,
  FMX.Objects,
  uRoadRunner, System.Skia, FMX.Skia;

const
  ZERO_FLUX_THRESHOLD = 1E-8;

type
  TSteadyStateSection = record
    Layout: TLayout;
    HeaderHost: TLayout;
    Header: TLabel;
    Btn3D:      TSpeedButton;
    Grid:   TStringGrid;
  end;

  TFrameSteadyState = class(TFrame)
    Layout1: TLayout;
    Label1: TLabel;
    LayoutToolbar: TLayout;
    btnCompute: TButton;
    cbScaling: TComboBox;
    btnSave: TButton;
    spnDecimals: TSpinBox;
    Line1: TLine;
    Label2: TLabel;
    btnCopytoClipboard: TButton;
    btnTimeCourseSliders: TSpeedButton;
    Image1: TImage;
    btnConfigSteadyStateSolver: TSpeedButton;
    SkSvgConfig: TSkSvg;
    procedure btnComputeClick(Sender: TObject);
    procedure cbScalingChange(Sender: TObject);
    procedure btnSlidersClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure spnDecimalsChange(Sender: TObject);
    procedure btnCopytoClipboardClick(Sender: TObject);
    procedure btnConfigSteadyStateSolverClick(Sender: TObject);
  private
    FContext: IAnalysisContext;
    FHasData: Boolean;

    FSectionsBuilt: Boolean;
    FSecConcentrations: TSteadyStateSection;
    FSecJacobian:       TSteadyStateSection;
    FSecEigenvalues:    TSteadyStateSection;
    FSecElasticities:   TSteadyStateSection;
    FSecFluxCC:         TSteadyStateSection;
    FSecConcCC:         TSteadyStateSection;

    FGradientPos: TGradient;
    FGradientNeg: TGradient;

    FDecimalPlaces: Integer;

   { Map from matrix kind to its currently-open 3D window, if any. A
      missing key means no window is open for that matrix. The dictionary
      does NOT own the forms -- forms own themselves (caFree on close)
      and notify us via OnFormClosed so we can drop the stale entry. }
    F3DWindows: TDictionary<TBar3DMatrixKind, TfrmBar3D>;

    function  Scaled: Boolean;
    function  SpeciesIds:  TArray<string>;
    function  ReactionIds: TArray<string>;

    procedure SessionStateChanged(Sender: TObject);
    procedure SessionModelReloaded(Sender: TObject;
                                   AParameterSetChanged: Boolean);

    function  EnsureSteadyState: Boolean;
    procedure RecomputeAll;
    procedure RefetchScalingDependent;
    procedure ClearAllGrids;

    function AnyFluxesZero (ZeroFluxThreshold : Double; out FluxId : String) : Boolean;

    function CSVEscape(const S: string): string;
    procedure WriteCSV(AWriter: TTextWriter);
    procedure SaveToCSV(const APath: string);
    function GetCSVText: string;
    procedure WriteSectionToCSV(AWriter: TTextWriter;
                                    const ATitle: string;
                                    const Sec: TSteadyStateSection);

    procedure RepopulateAllFromCurrentState;

    procedure BuildOutputSectionsIfNeeded;
    procedure CreateSection(var Sec: TSteadyStateSection;
                            const ATitle: string;
                            AHost: TScrollBox);

    procedure PopulateConcentrations;
    procedure PopulateJacobian;
    procedure PopulateEigenvalues;
    procedure PopulateElasticities;
    procedure PopulateFluxCC;
    procedure PopulateConcentrationCC;

    procedure ApplyColoring(const Sec: TSteadyStateSection; M: T2DMatrix);
    procedure FitSectionToContent(const Sec: TSteadyStateSection);
    procedure DoColoredCellDraw(Sender: TObject;
                                const Canvas: TCanvas;
                                const Column: TColumn;
                                const Bounds: TRectF;
                                const Row: Integer;
                                const Value: TValue;
                                const State: TGridDrawStates);

    procedure OnSliderReleased(Sender: TObject;
                               const ASliderString: string;
                               const AValue: Single);

    procedure AttachBar3DButton(var Sec: TSteadyStateSection;
                                AKind: TBar3DMatrixKind);
    procedure btn3DClick(Sender: TObject);
    function  GetOrCreate3DWindow(AKind: TBar3DMatrixKind): TfrmBar3D;
    procedure On3DWindowClosed(Sender: TObject);

   { Build the matrix + labels + Z-label for a given kind. Returns False
      if the model isn't ready. Caller owns the returned matrix. }
    function  BuildMatrixFor(AKind: TBar3DMatrixKind;
                             out M: T2DMatrix;
                             out ARowLabels, AColLabels: TArray<string>;
                             out AZLabel: string): Boolean;
    procedure Refresh3DWindow(AKind: TBar3DMatrixKind);
    procedure RefreshAllOpen3DWindows;
    procedure CloseAll3DWindows;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    destructor Destroy; override;
    procedure SetContext(const AContext: IAnalysisContext);
    procedure AttachToSliders;
  end;

implementation

{$R *.fmx}

uses
  ufConfigureSteadyState;

const
  SECTION_HEIGHT     = 200;
  SECTION_HEIGHT_MIN = 140;
  SECTION_HEIGHT_MAX = 600;
  SECTION_HEADER_H   = 24;
  SECTION_PADDING    = 8;


procedure TFrameSteadyState.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  { Self-cleaning dict: if a tracked 3D window dies by ANY path
    (user close, Application-owner shutdown, explicit Free), drop
    its entry here before the pointer can go stale. This is the
    safety net OnFormClosed can't provide -- OnFormClosed only
    fires on the Close path, not on direct destruction. }
  if (Operation = opRemove) and
     (F3DWindows <> nil) and
     (AComponent is TfrmBar3D) then
    F3DWindows.Remove(TfrmBar3D(AComponent).Kind);
end;

{ -- grid helpers ---------------------------------------------------------

  We avoid disposing TStringColumn instances individually -- this AVs in
  some FMX/Delphi combinations, possibly because the grid's internal
  column list and the TFmxObject Children list don't stay in sync during
  teardown, leaving dangling pointers visible to a subsequent render.

  Instead, when a grid's column shape needs to change (different model,
  different species count), we replace the entire grid: create a fresh
  one in the section's layout, then defer destruction of the old grid
  via TThread.ForceQueue so the current layout/event cycle finishes
  first. The grid's destructor takes care of its own columns in a single
  internally-coherent operation. }

procedure RecreateSectionGrid(var Sec: TSteadyStateSection);
var
  OldGrid: TStringGrid;
begin
  OldGrid := Sec.Grid;

  Sec.Grid := TStringGrid.Create(Sec.Layout);
  Sec.Grid.Parent := Sec.Layout;
  Sec.Grid.Align  := TAlignLayout.Client;
  Sec.Grid.RowCount := 0;


  if OldGrid <> nil then
  begin
    OldGrid.Parent := nil;
    TThread.ForceQueue(nil,
      procedure
      begin
        OldGrid.DisposeOf;
      end);
  end;
end;


function FormatNumber(V: Double; ADecimals: Integer): string;
const
  UPPER_FIXED = 1.0e15;
var
  AbsV, LowerFixed: Double;
  FmtFixed: string;
begin
  if IsNan(V) then Exit('NaN');
  if IsInfinite(V) then
    if V > 0 then Exit('Inf') else Exit('-Inf');

  if ADecimals = 0 then
    FmtFixed := '0'
  else
    FmtFixed := '0.' + StringOfChar('0', ADecimals);

  AbsV := Abs(V);
  if AbsV = 0 then
    Exit(FormatFloat(FmtFixed, 0));

  LowerFixed := IntPower(10.0, -ADecimals);

  if (AbsV < LowerFixed) or (AbsV >= UPPER_FIXED) then
    { Mantissa with ADecimals digits after the point, 2-digit exponent }
    Result := FloatToStrF(V, ffExponent, ADecimals + 1, 2)
  else
    Result := FormatFloat(FmtFixed, V);
end;


function FormatCell(V: Double; ADecimals: Integer): string;
begin
  Result := FormatNumber(V, ADecimals);
end;


function StringListToArray(L: TStringList): TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, L.Count);
  for I := 0 to L.Count - 1 do
    Result[I] := L[I];
end;

procedure SetupMatrixGrid(var Sec: TSteadyStateSection;
                          const RowIds, ColIds: array of string);
var
  I:        Integer;
  Col:      TStringColumn;
  WantCols: Integer;
  G:        TStringGrid;
begin
  WantCols := Length(ColIds) + 1;

  if (Sec.Grid <> nil) and
     (Sec.Grid.ColumnCount > 0) and
     (Sec.Grid.ColumnCount <> WantCols) then
    RecreateSectionGrid(Sec);

  G := Sec.Grid;
  G.BeginUpdate;
  try
    if G.ColumnCount = 0 then
    begin
      Col := TStringColumn.Create(G);
      Col.Header := '';
      Col.Width  := 90;
      G.AddObject(Col);

      for I := 0 to High(ColIds) do
      begin
        Col := TStringColumn.Create(G);
        Col.Header := ColIds[I];
        Col.Width  := 90;
        G.AddObject(Col);
      end;
    end
    else
    begin
      { Same shape as before -- refresh headers in case the ids reordered. }
      TStringColumn(G.Columns[0]).Header := '';
      for I := 0 to High(ColIds) do
        TStringColumn(G.Columns[I + 1]).Header := ColIds[I];
    end;

    G.RowCount := Length(RowIds);
    for I := 0 to High(RowIds) do
      G.Cells[0, I] := RowIds[I];
  finally
    G.EndUpdate;
  end;
end;

procedure SetupKeyValueGrid(var Sec: TSteadyStateSection;
                            const KeyHeader, ValueHeader: string;
                            ARowCount: Integer);
var
  Col: TStringColumn;
  G:   TStringGrid;
begin
  if (Sec.Grid <> nil) and
     (Sec.Grid.ColumnCount > 0) and
     (Sec.Grid.ColumnCount <> 2) then
    RecreateSectionGrid(Sec);

  G := Sec.Grid;
  G.BeginUpdate;
  try
    if G.ColumnCount = 0 then
    begin
      Col := TStringColumn.Create(G);
      Col.Header := KeyHeader;
      Col.Width  := 120;
      G.AddObject(Col);

      Col := TStringColumn.Create(G);
      Col.Header := ValueHeader;
      Col.Width  := 160;
      G.AddObject(Col);
    end
    else
    begin
      TStringColumn(G.Columns[0]).Header := KeyHeader;
      TStringColumn(G.Columns[1]).Header := ValueHeader;
    end;

    G.RowCount := ARowCount;
  finally
    G.EndUpdate;
  end;
end;

procedure SetupEigenvalueGrid(var Sec: TSteadyStateSection;
                              AHasImag: Boolean;
                              ARowCount: Integer);
var
  Col:      TStringColumn;
  WantCols: Integer;
  G:        TStringGrid;
begin
  if AHasImag then WantCols := 3 else WantCols := 2;

  if (Sec.Grid <> nil) and
     (Sec.Grid.ColumnCount > 0) and
     (Sec.Grid.ColumnCount <> WantCols) then
    RecreateSectionGrid(Sec);

  G := Sec.Grid;
  G.BeginUpdate;
  try
    if G.ColumnCount = 0 then
    begin
      Col := TStringColumn.Create(G);
      Col.Header := 'Index';
      Col.Width  := 60;
      G.AddObject(Col);

      Col := TStringColumn.Create(G);
      Col.Header := 'Real';
      Col.Width  := 140;
      G.AddObject(Col);

      if AHasImag then
      begin
        Col := TStringColumn.Create(G);
        Col.Header := 'Imaginary';
        Col.Width  := 140;
        G.AddObject(Col);
      end;
    end;

    G.RowCount := ARowCount;
  finally
    G.EndUpdate;
  end;
end;

procedure FillMatrixCells(AGrid: TStringGrid; M: T2DMatrix; ADecimals: Integer);
var
  R, C: Integer;
begin
  if M = nil then Exit;
  AGrid.BeginUpdate;
  try
    for R := 0 to M.r - 1 do
      for C := 0 to M.c - 1 do
        AGrid.Cells[C + 1, R] := FormatNumber(M[R, C], ADecimals);
  finally
    AGrid.EndUpdate;
  end;
end;

{ -- lifecycle / context -------------------------------------------------- }

destructor TFrameSteadyState.Destroy;
begin
  { Do NOT call CloseAll3DWindows here. At main-form-close time the
    3D windows are owned by Application and will be freed by it; calling
    Close on them mid-shutdown either AVs (if they're already gone) or
    triggers FMX Close machinery in a half-dead environment.
    FreeNotification keeps F3DWindows from holding stale pointers. }
  F3DWindows.Free;
  FGradientPos.Free;
  FGradientNeg.Free;
  inherited;
end;

procedure TFrameSteadyState.SetContext(const AContext: IAnalysisContext);
begin
  FContext := AContext;
  if FContext <> nil then
  begin
    FContext.Session.AddStateListener(SessionStateChanged);
    FContext.Session.AddReloadedListener(SessionModelReloaded);
  end;

  if cbScaling.Count = 0 then
  begin
    cbScaling.Items.Add('Scaled');
    cbScaling.Items.Add('Unscaled');
    cbScaling.ItemIndex := 0;
  end;

  if FGradientPos = nil then
  begin
    FGradientPos := TGradient.Create;
    FGradientPos.Style  := TGradientStyle.Linear;
    FGradientPos.Color  := TAlphaColorRec.White;
    FGradientPos.Color1 := TAlphaColorRec.Yellowgreen;
  end;
  if FGradientNeg = nil then
  begin
    FGradientNeg := TGradient.Create;
    FGradientNeg.Style  := TGradientStyle.Linear;
    FGradientNeg.Color  := TAlphaColorRec.White;
    FGradientNeg.Color1 := TAlphaColorRec.Tomato;
  end;

   FDecimalPlaces := 4;
  spnDecimals.Min       := 0;
  spnDecimals.Max       := 12;
  spnDecimals.ValueType := TNumValueType.Integer;
  spnDecimals.Value     := FDecimalPlaces;
end;

function TFrameSteadyState.Scaled: Boolean;
begin
  Result := cbScaling.ItemIndex <= 0;
end;

function TFrameSteadyState.SpeciesIds: TArray<string>;
var
  L: TStringList;
begin
  L := FContext.Session.RoadRunner.getFloatingSpeciesIds;
  try
    Result := StringListToArray(L);
  finally
    L.Free;
  end;
end;

procedure TFrameSteadyState.spnDecimalsChange(Sender: TObject);
begin
 FDecimalPlaces := Round(spnDecimals.Value);
  if FHasData then
    RepopulateAllFromCurrentState;
end;

function TFrameSteadyState.ReactionIds: TArray<string>;
var
  L: TStringList;
begin
  L := FContext.Session.RoadRunner.getReactionIds;
  try
    Result := StringListToArray(L);
  finally
    L.Free;
  end;
end;

{ -- output section construction ----------------------------------------- }

procedure TFrameSteadyState.CreateSection(var Sec: TSteadyStateSection;
                                          const ATitle: string;
                                          AHost: TScrollBox);
begin
  Sec.Layout := TLayout.Create(AHost);
  Sec.Layout.Parent := AHost;
  Sec.Layout.Align := TAlignLayout.Top;
  Sec.Layout.Height := SECTION_HEIGHT;
  Sec.Layout.Margins.Left := 8;
  Sec.Layout.Margins.Right := 8;
  Sec.Layout.Margins.Top := 4;
  Sec.Layout.Margins.Bottom := 10;

   { Header host -- holds the title label (Align:Client) plus any
      per-section toolbar buttons (Align:Right). Splitting this off
      keeps the section construction symmetric whether or not a 3D
      button is later attached. }
  Sec.HeaderHost := TLayout.Create(Sec.Layout);
  Sec.HeaderHost.Parent := Sec.Layout;
  Sec.HeaderHost.Align := TAlignLayout.Top;
  Sec.HeaderHost.Height := SECTION_HEADER_H;

  Sec.Header := TLabel.Create(Sec.Layout);
  Sec.Header.Parent := Sec.Layout;
  Sec.Header.Align := TAlignLayout.Top;
  Sec.Header.Height := 24;
  Sec.Header.Text := ATitle;
  Sec.Header.StyledSettings :=
    Sec.Header.StyledSettings - [TStyledSetting.Size, TStyledSetting.Style];
  Sec.Header.TextSettings.Font.Size := 14;
  Sec.Header.TextSettings.Font.Style := [TFontStyle.fsBold];

  Sec.Grid := TStringGrid.Create(Sec.Layout);
  Sec.Grid.Parent := Sec.Layout;
  Sec.Grid.Align := TAlignLayout.Client;
  Sec.Grid.RowCount := 0;
end;


procedure TFrameSteadyState.BuildOutputSectionsIfNeeded;
  var
    Host: TScrollBox;
  begin
    if FSectionsBuilt then Exit;
    if FContext = nil then Exit;

    Host := FContext.SteadyStateHost;
    if Host = nil then Exit;

    CreateSection(FSecConcentrations, 'Concentrations',                    Host);
    CreateSection(FSecJacobian,       'Jacobian',                          Host);
    CreateSection(FSecEigenvalues,    'Eigenvalues',                       Host);
    CreateSection(FSecElasticities,   'Elasticities',                      Host);
    CreateSection(FSecFluxCC,         'Flux Control Coefficients',         Host);
    CreateSection(FSecConcCC,         'Concentration Control Coefficients', Host);

    { 3D view is offered only for the three signed coefficient matrices.
      Concentrations (1D), Jacobian (rarely interpreted entry-wise), and
      Eigenvalues (complex) intentionally get no button. }
    AttachBar3DButton(FSecElasticities, mkElasticities);
    AttachBar3DButton(FSecFluxCC,       mkFluxCC);
    AttachBar3DButton(FSecConcCC,       mkConcCC);

    FSectionsBuilt := True;
  end;

{ -- session callbacks ---------------------------------------------------- }

procedure TFrameSteadyState.SessionStateChanged(Sender: TObject);
begin
  if (FContext = nil) then Exit;
  if FContext.Session.IsDirty or (not FContext.Session.IsLoaded) then
  begin
    FHasData := False;
    ClearAllGrids;
  end;
end;

procedure TFrameSteadyState.SessionModelReloaded(Sender: TObject;
                                                  AParameterSetChanged: Boolean);
begin
  FHasData := False;
  ClearAllGrids;
  CloseAll3DWindows;     { NEW: species/reaction counts may have changed }
end;

{ -- compute / populate --------------------------------------------------- }

function TFrameSteadyState.EnsureSteadyState: Boolean;
begin
  Result := False;
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

  try
    FContext.Session.RoadRunner.steadyState;
  except
    on E: Exception do
    begin
      ShowMessage('Steady-state failed: ' + E.Message);
      Exit;
    end;
  end;

  Result := True;
end;

procedure TFrameSteadyState.btnComputeClick(Sender: TObject);
begin
  RecomputeAll;
end;

procedure TFrameSteadyState.btnConfigSteadyStateSolverClick(Sender: TObject);
begin
  frmConfigSteadyState := TfrmConfigSteadyState.Create (nil);
  try
    frmConfigSteadyState.SetContext (FContext);
    frmConfigSteadyState.ShowModal;
  finally
    frmConfigSteadyState.Free;
  end;
end;

procedure TFrameSteadyState.btnCopytoClipboardClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if not FHasData then Exit;
  if TPlatformServices.Current.SupportsPlatformService(
       IFMXClipboardService, Svc) then
    Svc.SetClipboard(GetCSVText);
end;

procedure TFrameSteadyState.cbScalingChange(Sender: TObject);
begin
  if FHasData then
    RefetchScalingDependent;
end;

function TFrameSteadyState.AnyFluxesZero (ZeroFluxThreshold : Double; out FluxId : String) : Boolean;
var i : Integer;
    nReactions : Integer;
begin
  nReactions := FContext.Session.RoadRunner.getNumberOfReactions;
  for i := 0 to nReactions - 1 do
      if abs (FContext.Session.RoadRunner.getReactionRates[i]) < ZeroFluxThreshold then
         begin
         FluxId := FContext.Session.RoadRunner.getReactionIds()[i];
         exit (True);
         end;
  Exit (False);
end;


procedure TFrameSteadyState.RecomputeAll;
var FluxId : String;
begin
  if not EnsureSteadyState then
  begin
    FHasData := False;
    Exit;
  end;

  BuildOutputSectionsIfNeeded;

  PopulateConcentrations;
  PopulateJacobian;
  PopulateEigenvalues;
  PopulateElasticities;
  if AnyFluxesZero (ZERO_FLUX_THRESHOLD, FluxId) then
     showmessage ('Warning: The reaction flux ' + FluxId + ' is zero, the flux control coefficients will be undefined')
  else
     PopulateFluxCC;
  PopulateConcentrationCC;
  FHasData := True;

  FitSectionToContent(FSecConcentrations);
  FitSectionToContent(FSecJacobian);
  FitSectionToContent(FSecEigenvalues);
  FitSectionToContent(FSecElasticities);
  if not AnyFluxesZero (ZERO_FLUX_THRESHOLD, FluxId) then
     FitSectionToContent(FSecFluxCC);
  FitSectionToContent(FSecConcCC);

  if FContext <> nil then
    FContext.ShowSteadyStateTab;

  btnSave.Enabled := True;

  RefreshAllOpen3DWindows;   { NEW: after FHasData := True, before
                                  ShowSteadyStateTab is fine }
end;

procedure TFrameSteadyState.RefetchScalingDependent;
begin
  PopulateElasticities;
  PopulateFluxCC;
  PopulateConcentrationCC;
  RefreshAllOpen3DWindows;   { NEW: scaling change applies to the 3D views }
end;

procedure TFrameSteadyState.ClearAllGrids;

  procedure ClearOne(const Sec: TSteadyStateSection);
  begin
    if Sec.Grid = nil then Exit;
    Sec.Grid.BeginUpdate;
    try
      Sec.Grid.RowCount := 0;
    finally
      Sec.Grid.EndUpdate;
    end;
  end;

begin
  if not FSectionsBuilt then Exit;
  ClearOne(FSecConcentrations);
  ClearOne(FSecJacobian);
  ClearOne(FSecEigenvalues);
  ClearOne(FSecElasticities);
  ClearOne(FSecFluxCC);
  ClearOne(FSecConcCC);

  FitSectionToContent(FSecConcentrations);
  FitSectionToContent(FSecJacobian);
  FitSectionToContent(FSecEigenvalues);
  FitSectionToContent(FSecElasticities);
  FitSectionToContent(FSecFluxCC);
  FitSectionToContent(FSecConcCC);

  btnSave.Enabled := False;
end;

{ -- cell coloring ------------------------------------------------------- }

function MaxAbsValue(M: T2DMatrix): Double;
var
  R, C: Integer;
  V:    Double;
begin
  Result := 0.0;
  if M = nil then Exit;
  for R := 0 to M.r - 1 do
    for C := 0 to M.c - 1 do
    begin
      V := Abs(M[R, C]);
      if V > Result then Result := V;
    end;
end;

{ Configure a section's grid for value-coloured cells. The grid's
  TagFloat caches the per-matrix normalization constant (max abs value)
  so the draw event can scale each cell's intensity. Called every time
  a colored matrix is populated -- cheap and idempotent, and necessary
  because RecreateSectionGrid may have replaced Sec.Grid since the last
  populate. }
procedure TFrameSteadyState.ApplyColoring(const Sec: TSteadyStateSection;
                                          M: T2DMatrix);
begin
  if Sec.Grid = nil then Exit;
  Sec.Grid.DefaultDrawing   := False;
  Sec.Grid.OnDrawColumnCell := DoColoredCellDraw;
  Sec.Grid.TagFloat         := MaxAbsValue(M);
end;

procedure TFrameSteadyState.DoColoredCellDraw(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  G:          TStringGrid;
  Text:       string;
  V:          Double;
  MaxAbs:     Double;
  Normalized: Single;
  BgBrush:    TBrush;
  FillBounds: TRectF;
begin
  if not (Sender is TStringGrid) then
  begin
    Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
    Exit;
  end;
  G      := TStringGrid(Sender);
  MaxAbs := G.TagFloat;
  Text   := Value.ToString;

  { Row-label cells (leftmost column) contain species/reaction names that
    won't parse as floats -- they fall through to default drawing. Same
    when MaxAbs is zero (degenerate matrix). }
  if (MaxAbs <= 0) or (not TryStrToFloat(Text, V)) then
  begin
    Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
    Exit;
  end;

  Normalized := EnsureRange(Abs(V) / MaxAbs, 0.0, 1.0);

  if V > 0 then
    BgBrush := TBrush.Create(TBrushKind.Solid,
                             FGradientPos.InterpolateColor(Normalized))
  else
    BgBrush := TBrush.Create(TBrushKind.Solid,
                             FGradientNeg.InterpolateColor(Normalized));
  try
    FillBounds := Bounds;
    FillBounds.Inflate(3, 3);   { or 3, 3 if you want more bleed }

    Canvas.FillRect(FillBounds, 0, 0, [], 1, BgBrush);

    { Render the cell text in black ourselves. The column's default text
      color (used by DefaultDrawCell) comes from the active FMX style
      and may be light/white -- unreadable on our colored background. }
    Canvas.Fill.Color := TAlphaColorRec.Black;
    Canvas.FillText(Bounds, Text, False, 1.0, [],
                    TTextAlign.Center, TTextAlign.Center);
  finally
    BgBrush.Free;
  end;
end;

{ Resize a section's layout so the grid shows all of its data without
  internal scrolling, up to SECTION_HEIGHT_MAX. Below SECTION_HEIGHT_MIN
  we clamp to the minimum so a 2-row grid doesn't look squashed. The
  grid's column header band takes one row's worth of height, so we add
  one extra row to the count when sizing. }
procedure TFrameSteadyState.FitSectionToContent(const Sec: TSteadyStateSection);
var
  RowH:  Single;
  GridH: Single;
  Total: Single;
begin
  if (Sec.Grid = nil) or (Sec.Layout = nil) then Exit;

  RowH := Sec.Grid.RowHeight;
  if RowH <= 0 then RowH := 24;     { defensive default }

  GridH := (Sec.Grid.RowCount + 1) * RowH + SECTION_PADDING;
  Total := SECTION_HEADER_H + GridH;

  if Total < SECTION_HEIGHT_MIN then Total := SECTION_HEIGHT_MIN;
  if Total > SECTION_HEIGHT_MAX then Total := SECTION_HEIGHT_MAX;

  Sec.Layout.Height := Total;
end;

{ -- populate individual grids ------------------------------------------- }


procedure TFrameSteadyState.PopulateConcentrations;
var
  Species: TArray<string>;
  G:       TStringGrid;
  I:       Integer;
begin
  Species := SpeciesIds;
  SetupKeyValueGrid(FSecConcentrations, 'Species', 'Concentration',
                    Length(Species));
  G := FSecConcentrations.Grid;
  G.BeginUpdate;
  try
    for I := 0 to High(Species) do
    begin
      G.Cells[0, I] := Species[I];
      G.Cells[1, I] := FormatCell(FContext.Session.RoadRunner.getFloatingSpeciesByIndex(I), FDecimalPlaces);
    end;
  finally
    G.EndUpdate;
  end;
end;

procedure TFrameSteadyState.PopulateJacobian;
var
  Species: TArray<string>;
  DSpecies: TArray<string>;
  M:       T2DMatrix;
begin
  Species := SpeciesIds;
  M := FContext.Session.RoadRunner.getFullJacobian;
  try
    Setlength (DSpecies, length (Species));
    for var i := 0 to length (Species) - 1 do
        DSpecies[i] := 'D_' + Species[i];

    SetupMatrixGrid(FSecJacobian, Species, DSpecies);
    FillMatrixCells(FSecJacobian.Grid, M, FDecimalPlaces);
    ApplyColoring(FSecJacobian, M);
  finally
    M.Free;
  end;
end;

procedure TFrameSteadyState.PopulateEigenvalues;
var
  M:       T2DMatrix;
  HasImag: Boolean;
  G:       TStringGrid;
  I:       Integer;
begin
  M := FContext.Session.RoadRunner.getEigenvalues;
  try
    HasImag := M.c >= 2;
    SetupEigenvalueGrid(FSecEigenvalues, HasImag, M.r);
    G := FSecEigenvalues.Grid;
    G.BeginUpdate;
    try
      for I := 0 to M.r - 1 do
      begin
        G.Cells[0, I] := IntToStr(I + 1);
        G.Cells[1, I] := FormatCell(M[I, 0], FDecimalPlaces);
        if HasImag then
          G.Cells[2, I] := FormatCell(M[I, 1], FDecimalPlaces);
      end;
    finally
      G.EndUpdate;
    end;
  finally
    M.Free;
  end;
end;

procedure TFrameSteadyState.PopulateElasticities;
var
  Species:   TArray<string>;
  Reactions: TArray<string>;
  M:         T2DMatrix;
begin
  Species   := SpeciesIds;
  Reactions := ReactionIds;

  if Scaled then
    M := FContext.Session.RoadRunner.getScaledElasticityMatrix
  else
    M := FContext.Session.RoadRunner.getUnScaledElasticityMatrix;
  try
    SetupMatrixGrid(FSecElasticities, Reactions, Species);
    FillMatrixCells(FSecElasticities.Grid, M, FDecimalPlaces);
    ApplyColoring(FSecElasticities, M);
  finally
    M.Free;
  end;
end;

procedure TFrameSteadyState.PopulateFluxCC;
var
  Reactions: TArray<string>;
  EReactions: TArray<string>;
  M:         T2DMatrix;
begin
  Reactions := ReactionIds;

  if Scaled then
    M := FContext.Session.RoadRunner.getScaledFluxControlCoefficientMatrix
  else
    M := FContext.Session.RoadRunner.getUnscaledFluxControlCoefficientMatrix;
  try
    Setlength (EReactions, length (Reactions));
    for var i := 0 to length (Reactions) - 1 do
        EReactions[i] := 'E_' + Reactions[i];

    SetupMatrixGrid(FSecFluxCC, Reactions, EReactions);
    FillMatrixCells(FSecFluxCC.Grid, M, FDecimalPlaces);
    ApplyColoring(FSecFluxCC, M);
  finally
    M.Free;
  end;
end;

procedure TFrameSteadyState.PopulateConcentrationCC;
var
  Species:   TArray<string>;
  Reactions: TArray<string>;
  M:         T2DMatrix;
begin
  Species   := SpeciesIds;
  Reactions := ReactionIds;

  if Scaled then
    M := FContext.Session.RoadRunner
            .getScaledConcentrationControlCoefficientMatrix
  else
    M := FContext.Session.RoadRunner
            .getUnscaledConcentrationControlCoefficientMatrix;
  try
    for var i := 0 to length (Reactions) - 1 do
        Reactions[i] := 'E_' + Reactions[i];

    SetupMatrixGrid(FSecConcCC, Species, Reactions);
    FillMatrixCells(FSecConcCC.Grid, M, FDecimalPlaces);
    ApplyColoring(FSecConcCC, M);
  finally
    M.Free;
  end;
end;

{ -- slider integration -------------------------------------------------- }

procedure TFrameSteadyState.btnSlidersClick(Sender: TObject);
var
  Names:  TArray<string>;
  Values: TArray<Double>;
begin
  if FContext = nil then Exit;

  if not EnsureSteadyState then Exit;

  if not FContext.SliderContainer.ParamPanelVisible then
  begin
    Names  := FContext.Session.GetTunableNames;
    Values := FContext.Session.GetTunableValues;
    FContext.SliderContainer.LoadParams(Names, Values);
  end;

  FContext.SliderContainer.ToggleParamPanel;
end;

procedure TFrameSteadyState.OnSliderReleased(Sender: TObject;
  const ASliderString: string; const AValue: Single);
begin
  if FContext = nil then Exit;

  FContext.Session.SetParameterValue(ASliderString, AValue);
  RecomputeAll;
end;


function TFrameSteadyState.CSVEscape(const S: string): string;
begin
  { RFC 4180: quote a field if it contains comma, quote, CR, or LF;
    double any embedded quotes. Numbers and plain identifiers pass
    through unchanged, which is what we want for downstream parsers. }
  if (Pos(',',  S) > 0) or
     (Pos('"',  S) > 0) or
     (Pos(#13,  S) > 0) or
     (Pos(#10,  S) > 0) then
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"'
  else
    Result := S;
end;

procedure TFrameSteadyState.WriteSectionToCSV(AWriter: TTextWriter;
                                              const ATitle: string;
                                              const Sec: TSteadyStateSection);
var
  G:     TStringGrid;
  R, C:  Integer;
  Line:  string;
  Hdr:   string;
begin
  if (Sec.Grid = nil) or (Sec.Grid.ColumnCount = 0) then Exit;

  G := Sec.Grid;

  AWriter.WriteLine('# ' + ATitle);

  { Header row: walk the columns and emit each header. The leftmost
    column of matrix grids has an empty header by convention -- that
    just produces a leading comma, which is what we want (so the
    column-label row aligns with the data columns underneath). }
  Line := '';
  for C := 0 to G.ColumnCount - 1 do
  begin
    if G.Columns[C] is TStringColumn then
      Hdr := TStringColumn(G.Columns[C]).Header
    else
      Hdr := '';
    if C > 0 then Line := Line + ',';
    Line := Line + CSVEscape(Hdr);
  end;
  AWriter.WriteLine(Line);

  { Data rows -- read Cells directly so what we save matches what the
    user sees on screen (including FormatCell's number formatting). }
  for R := 0 to G.RowCount - 1 do
  begin
    Line := '';
    for C := 0 to G.ColumnCount - 1 do
    begin
      if C > 0 then Line := Line + ',';
      Line := Line + CSVEscape(G.Cells[C, R]);
    end;
    AWriter.WriteLine(Line);
  end;

  AWriter.WriteLine('');   { blank line separates sections }
end;

procedure TFrameSteadyState.WriteCSV(AWriter: TTextWriter);
var
  ScalingS: string;
begin
  if not FHasData then Exit;

  if Scaled then ScalingS := 'Scaled' else ScalingS := 'Unscaled';

  AWriter.WriteLine('# Steady-State Analysis Results');
  AWriter.WriteLine('# Generated: ' +
                    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  AWriter.WriteLine('# Scaling: ' + ScalingS);
  AWriter.WriteLine('');

  WriteSectionToCSV(AWriter, 'Concentrations',
                    FSecConcentrations);
  WriteSectionToCSV(AWriter, 'Jacobian',
                    FSecJacobian);
  WriteSectionToCSV(AWriter, 'Eigenvalues',
                    FSecEigenvalues);
  WriteSectionToCSV(AWriter, 'Elasticities (' + ScalingS + ')',
                    FSecElasticities);
  WriteSectionToCSV(AWriter, 'Flux Control Coefficients (' + ScalingS + ')',
                    FSecFluxCC);
  WriteSectionToCSV(AWriter, 'Concentration Control Coefficients (' +
                    ScalingS + ')', FSecConcCC);
end;

procedure TFrameSteadyState.SaveToCSV(const APath: string);
var
  Writer: TStreamWriter;
begin
  Writer := TStreamWriter.Create(APath, False, TEncoding.UTF8);
  try
    WriteCSV(Writer);
  finally
    Writer.Free;
  end;
end;

function TFrameSteadyState.GetCSVText: string;
var
  Writer: TStringWriter;
begin
  Writer := TStringWriter.Create;
  try
    WriteCSV(Writer);
    Result := Writer.ToString;
  finally
    Writer.Free;
  end;
end;

procedure TFrameSteadyState.btnSaveClick(Sender: TObject);
var
  Dlg:         TSaveDialog;
  DefaultName: string;
  ScalingS:    string;
begin
 if not FHasData then Exit;

  if Scaled then ScalingS := 'scaled' else ScalingS := 'unscaled';
  DefaultName := Format('steadystate_%s_%s.csv',
                  [ScalingS,
                   FormatDateTime('yyyymmdd_hhnnss', Now)]);

  Dlg := TSaveDialog.Create(nil);
  try
    Dlg.Filter     := 'CSV files (*.csv)|*.csv|All files (*.*)|*.*';
    Dlg.DefaultExt := 'csv';
    Dlg.FileName   := DefaultName;
    Dlg.Options    := Dlg.Options + [TOpenOption.ofOverwritePrompt];

    if Dlg.Execute then
    begin
      try
        SaveToCSV(Dlg.FileName);
      except
        on E: Exception do
          ShowMessage('Save failed: ' + E.Message);
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TFrameSteadyState.RepopulateAllFromCurrentState;
begin
  { Steady state is already solved -- the Populate* methods only read
    matrices from RoadRunner, they don't re-trigger steadyState. So
    this is cheap: just re-renders the six grids with the new format
    (and re-applies coloring, which is fine -- MaxAbs is unchanged). }
  PopulateConcentrations;
  PopulateJacobian;
  PopulateEigenvalues;
  PopulateElasticities;
  PopulateFluxCC;
  PopulateConcentrationCC;
end;

procedure TFrameSteadyState.AttachToSliders;
begin
  if FContext = nil then Exit;
  { Steady-state uses release-only updates: solves are less robust than
    time-course integration and we don't want to hammer them on every
    thumb pixel. Handler is named OnSliderReleased to document that. }
  FContext.SliderContainer.OnSliderChanged := OnSliderReleased;
  FContext.SliderContainer.ReleaseOnlyMode := True;
end;

procedure TFrameSteadyState.AttachBar3DButton(var Sec: TSteadyStateSection;
                                                AKind: TBar3DMatrixKind);
 begin
    Sec.Btn3D := TSpeedButton.Create(Sec.HeaderHost);
    Sec.Btn3D.Parent := Sec.HeaderHost;
    Sec.Btn3D.Align := TAlignLayout.Right;
    Sec.Btn3D.Width := 88;
    Sec.Btn3D.Margins.Right := 4;
    Sec.Btn3D.Margins.Top := 1;
    Sec.Btn3D.Margins.Bottom := 1;
    Sec.Btn3D.Text := '3D View';
    Sec.Btn3D.Hint := 'Open this matrix as a 3D bar chart';
    Sec.Btn3D.ShowHint := True;
    Sec.Btn3D.Tag := Ord(AKind);
    Sec.Btn3D.OnClick := btn3DClick;
 end;


function TFrameSteadyState.BuildMatrixFor(AKind: TBar3DMatrixKind;
                                            out M: T2DMatrix;
                                            out ARowLabels, AColLabels: TArray<string>;
                                            out AZLabel: string): Boolean;
  var
    RR: TRoadRunner;
    EReactions: TArray<string>;
    I: Integer;
  begin
    Result := False;
    M := nil;
    if FContext = nil then Exit;
    if not FContext.Session.IsLoaded then Exit;

    RR := FContext.Session.RoadRunner;

    case AKind of
      mkElasticities:
        begin
          ARowLabels := ReactionIds;     { rows are reactions }
          AColLabels := SpeciesIds;      { cols are species   }
          if Scaled then
            M := RR.getScaledElasticityMatrix
          else
            M := RR.getUnscaledElasticityMatrix;
        end;

      mkFluxCC:
        begin
          ARowLabels := ReactionIds;
          AColLabels := ReactionIds;
          SetLength(EReactions, Length(AColLabels));
          for I := 0 to High(AColLabels) do
            EReactions[I] := 'E_' + AColLabels[I];
          AColLabels := EReactions;
          if Scaled then
            M := RR.getScaledFluxControlCoefficientMatrix
          else
            M := RR.getUnscaledFluxControlCoefficientMatrix;
        end;

      mkConcCC:
        begin
          ARowLabels := SpeciesIds;
          AColLabels := ReactionIds;
          SetLength(EReactions, Length(AColLabels));
          for I := 0 to High(AColLabels) do
            EReactions[I] := 'E_' + AColLabels[I];
          AColLabels := EReactions;
          if Scaled then
            M := RR.getScaledConcentrationControlCoefficientMatrix
          else
            M := RR.getUnscaledConcentrationControlCoefficientMatrix;
        end;
    end;

    AZLabel := Bar3DKindZLabel(AKind, Scaled);
    Result  := M <> nil;
end;


function TFrameSteadyState.GetOrCreate3DWindow(AKind: TBar3DMatrixKind): TfrmBar3D;
  begin
    if F3DWindows = nil then
      F3DWindows := TDictionary<TBar3DMatrixKind, TfrmBar3D>.Create;

    if F3DWindows.TryGetValue(AKind, Result) then Exit;

    Result := TfrmBar3D.Create(Application);
    Result.OnFormClosed := On3DWindowClosed;
    F3DWindows.Add(AKind, Result);
    Result.FreeNotification(Self);
end;


  procedure TFrameSteadyState.On3DWindowClosed(Sender: TObject);
  var
    W: TfrmBar3D;
  begin
    if not (Sender is TfrmBar3D) then Exit;
    W := TfrmBar3D(Sender);
    if F3DWindows <> nil then
      F3DWindows.Remove(W.Kind);
  end;


  procedure TFrameSteadyState.btn3DClick(Sender: TObject);
  var
    Kind: TBar3DMatrixKind;
    M: T2DMatrix;
    RowLabels, ColLabels: TArray<string>;
    ZLabel: string;
    Wnd: TfrmBar3D;
  begin
    if not FHasData then
    begin
      { Be polite: a click before Compute should just trigger one and
        come back, rather than silently doing nothing. }
      RecomputeAll;
      if not FHasData then Exit;
    end;

    Kind := TBar3DMatrixKind((Sender as TSpeedButton).Tag);

    if not BuildMatrixFor(Kind, M, RowLabels, ColLabels, ZLabel) then Exit;
    try
      Wnd := GetOrCreate3DWindow(Kind);
      Wnd.LoadMatrix(Kind, M, RowLabels, ColLabels, ZLabel, Scaled);
      Wnd.Show;
      Wnd.BringToFront;
    finally
      M.Free;
    end;
  end;


  procedure TFrameSteadyState.Refresh3DWindow(AKind: TBar3DMatrixKind);
  var
    Wnd: TfrmBar3D;
    M: T2DMatrix;
    RowLabels, ColLabels: TArray<string>;
    ZLabel: string;
  begin
    if F3DWindows = nil then Exit;
    if not F3DWindows.TryGetValue(AKind, Wnd) then Exit;
    if not FHasData then Exit;

    if not BuildMatrixFor(AKind, M, RowLabels, ColLabels, ZLabel) then Exit;
    try
      Wnd.LoadMatrix(AKind, M, RowLabels, ColLabels, ZLabel, Scaled);
    finally
      M.Free;
    end;
  end;


  procedure TFrameSteadyState.RefreshAllOpen3DWindows;
  begin
    Refresh3DWindow(mkElasticities);
    Refresh3DWindow(mkFluxCC);
    Refresh3DWindow(mkConcCC);
  end;



  procedure TFrameSteadyState.CloseAll3DWindows;
  var
    Wnd: TfrmBar3D;
    Kinds: TArray<TBar3DMatrixKind>;
    K: TBar3DMatrixKind;
  begin
    if F3DWindows = nil then Exit;
    { Snapshot keys -- Close fires OnFormClosed which mutates the dict. }
    Kinds := F3DWindows.Keys.ToArray;
    for K in Kinds do
      if F3DWindows.TryGetValue(K, Wnd) then
        Wnd.Close;
  end;




end.
