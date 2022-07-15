unit ufMoreSteadyState;

interface

uses
  System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, System.Rtti,
  FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, FMX.TabControl, uRRTypes,
  FMX.Objects, FMX.ListBox, uController;

type
  TfrmMoreSteadyState = class(TForm)
    Layout1: TLayout;
    btnClose: TButton;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    GridSS: TStringGrid;
    TabControl1: TTabControl;
    tbJacobian: TTabItem;
    tbFluxControl: TTabItem;
    tbConcControl: TTabItem;
    tbElasticities: TTabItem;
    SteadyStateGrid: TStringColumn;
    StringColumn2: TStringColumn;
    Label2: TLabel;
    Layout5: TLayout;
    btnSolve: TButton;
    Label1: TLabel;
    lblss: TLabel;
    Line1: TLine;
    StyleBook1: TStyleBook;
    Layout6: TLayout;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    Layout7: TLayout;
    Layout8: TLayout;
    layoutElast: TLayout;
    Label5: TLabel;
    Label6: TLabel;
    Image2: TImage;
    Image3: TImage;
    Label7: TLabel;
    Label8: TLabel;
    Image4: TImage;
    Label9: TLabel;
    Label10: TLabel;
    cboView: TComboBox;
    cboFCC: TComboBox;
    cboCCC: TComboBox;
    cboElasticity: TComboBox;
    btnSaveCSV: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSolveClick(Sender: TObject);
    procedure GridElasticitiesDrawColumnCell(Sender: TObject;
      const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
      const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure GridCCCDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure GridFCCDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure GridJacobianDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure FormShow(Sender: TObject);
    procedure btnSaveCSVClick(Sender: TObject);
  private
    { Private declarations }
    fluxControlCoefficients: T2DMatrix;
    ConcControlCoefficients : T2DMatrix;
    gridJacobian: TStringGrid;
    gridFCC : TStringGrid;
    gridCCC : TStringGrid;
    gridElasticities : TStringGrid;
    procedure setupGrids;
    procedure removeGrid;
    procedure computeElasticities;
    procedure computeCCC;
    procedure computeFCC;
  public
    { Public declarations }
    controller : TController;
  end;

var
  frmMoreSteadyState: TfrmMoreSteadyState;
  gradientPlus, gradientNegative: TGradient;

implementation

{$R *.fmx}

Uses Math, IOUtils, uColorList, uRoadRunner;

var
  maxWidth, decimalPlaces: Integer;

procedure TfrmMoreSteadyState.btnCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmMoreSteadyState.removeGrid;
begin

end;


procedure TfrmMoreSteadyState.setupGrids;
var i, n, m : integer;
begin
  gridJacobian := TStringGrid.Create(self);
  gridJacobian.Parent := tbJacobian;
  gridJacobian.Align := TAlignLayout.Client;
  gridJacobian.OnDrawColumnCell := GridJacobianDrawColumnCell;

  m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;
  gridJacobian.ClearColumns;
  gridJacobian.RowCount := m + 1;
  for i := 0 to m do
    gridJacobian.AddObject(TStringColumn.Create(gridJacobian));


  GridFCC := TStringGrid.Create(self);
  GridFCC.Parent := tbFluxControl;
  GridFCC.Align := TAlignLayout.Client;
  GridFCC.OnDrawColumnCell := GridFCCDrawColumnCell;

  n := controller.simulator.roadrunner.getNumberOfReactions;
  GridFCC.RowCount := n;
  for i := 0 to n do
      GridFCC.AddObject(TStringColumn.Create(GridFCC));

  GridCCC := TStringGrid.Create(self);
  GridCCC.Parent := tbConcControl;
  GridCCC.Align := TAlignLayout.Client;
  GridCCC.OnDrawColumnCell := GridCCCDrawColumnCell;

  m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;
  GridCCC.RowCount := m;
  for i := 0 to n do
    GridCCC.AddObject(TStringColumn.Create(GridCCC));

  gridElasticities := TStringGrid.Create(self);
  gridElasticities.Parent := tbElasticities;
  gridElasticities.Align := TAlignLayout.Client;
  gridElasticities.OnDrawColumnCell := GridElasticitiesDrawColumnCell;

   m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;
   GridElasticities.RowCount := n;
   for i := 0 to m do
       GridElasticities.AddObject(TStringColumn.Create(GridElasticities));
end;


procedure TfrmMoreSteadyState.computeElasticities;
var
  n, m, i, j: Integer;
  listR, listF: TStringList;
  elasticities: T2DMatrix;
begin
  elasticities := controller.simulator.roadrunner.getScaledElasticityMatrix;
  try
    n := controller.simulator.roadrunner.getNumberOfReactions;
    m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;
    GridElasticities.RowCount := n;

    listR := controller.simulator.roadrunner.getReactionIds;
    listF := controller.simulator.roadrunner.getFloatingSpeciesIds;
    try
      for i := 0 to m - 1 do
        GridElasticities.Columns[i + 1].Header := listF[i];
      for i := 0 to n - 1 do
        GridElasticities.Cells[0, i] := listR[i];

    finally
      listR.Free;
      listF.Free;
    end;
    for i := 1 to n do
      for j := 1 to m do
        GridElasticities.Cells[j, i - 1] :=
          Format('%6.8f', [elasticities[i - 1, j - 1]]);
  finally
    elasticities.Free;
  end;
end;


procedure TfrmMoreSteadyState.computeCCC;
var
  n, m, i, j: Integer;
  listR, listF: TStringList;
begin
  ConcControlCoefficients := controller.simulator.roadrunner.getScaledConcentrationControlCoefficientMatrix;
  try
    n := controller.simulator.roadrunner.getNumberOfReactions;
    m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;

    listR := controller.simulator.roadrunner.getReactionIds;
    listF := controller.simulator.roadrunner.getFloatingSpeciesIds;
    try
      GridCCC.Columns[0].Header := 'Enzyme ->';
      for i := 0 to m - 1 do
        GridCCC.Cells[0, i] := listF[i];

      for i := 0 to n - 1 do
        GridCCC.Columns[i + 1].Header := listR[i];

    finally
      listR.Free;
      listF.Free;
    end;
    for i := 1 to n do
      for j := 0 to m - 1 do
        GridCCC.Cells[i, j] := Format('%6.8f', [ConcControlCoefficients[j, i - 1]]);
  finally
    //CC.free;
  end;
end;


function formatValue(Value: double; sciformat: boolean): string;
begin
  if sciformat then
    result := Format('%*.*g', [maxWidth, decimalPlaces, Value])
  else
    result := Format('%*.*f', [maxWidth, decimalPlaces, Value]);
end;


procedure TfrmMoreSteadyState.computeFCC;
var
  n, i, j: Integer;
  listR: TStringList;
begin
  try
    fluxControlCoefficients := controller.simulator.roadrunner.getScaledFluxControlCoefficientMatrix;
    n := controller.simulator.roadrunner.getNumberOfReactions;

    listR := controller.simulator.roadrunner.getReactionIds;
    try
      GridFCC.Columns[0].Header := 'Enzyme ->';
      for i := 0 to n - 1 do
      begin
        GridFCC.Columns[i + 1].Header := listR[i];
        GridFCC.Cells[0, i] := listR[i]
      end;
    finally
      listR.Free;
    end;
    for i := 1 to n do
      for j := 1 to n do
        GridFCC.Cells[j, i - 1] := Format('%6.8f', [fluxControlCoefficients[i - 1, j - 1]]);
  finally
    fluxControlCoefficients.free;
  end;
end;


procedure TfrmMoreSteadyState.btnSaveCSVClick(Sender: TObject);
var listR, listF : TStringList;
    astr : string;
begin

  astr := '';
  listR := controller.simulator.roadrunner.getReactionIds;
  listF := controller.simulator.roadrunner.getFloatingSpeciesIds;
  astr := listR[0];
  for var i := 1 to listR.Count - 1 do
      astr := astr + ', ' + listR[i];
  astr := astr + sLineBreak;

  for var i := 0 to ConcControlCoefficients.r - 1 do
      begin
      astr := astr + listF[i] + ', ';
      astr := astr + floattostr (ConcControlCoefficients[i,0]);
      for var j := 1 to ConcControlCoefficients.c - 1  do
          begin
          astr := astr + ', ' + floattostr (ConcControlCoefficients[i,j]);
          end;
      astr := astr + sLineBreak;
      end;

  TFile.WriteAllText('CC.csv', astr);
end;

procedure TfrmMoreSteadyState.btnSolveClick(Sender: TObject);
var
  d: double;
  name: string;
  i, j, p: Integer;
  count: Integer;
  varStr, parStr, str: string;
  Eigenvalues: pointer;
  CC: pointer;
  n, m: Integer;
  rowNum: Integer;
  Value: double;
  s: AnsiString;
  strList, floats: TStringList;
  floatValues: TArray<double>;
  Jacobian: T2DMatrix;
begin
  if controller.simulator.roadrunner.getNumberOfReactions() < 1 then
    exit;

  lblss.Text := floattostr(controller.simulator.roadrunner.steadyState);
  strList := controller.simulator.roadrunner.getFloatingSpeciesIds;
  for i := 0 to strList.count - 1 do
    GridSS.Cells[0, i] := strList[i];

  floatValues := controller.simulator.roadrunner.getFloatingSpeciesConcentrations;

  for i := 0 to strList.count - 1 do
    GridSS.Cells[1, i] := Format('%6.8f', [floatValues[i]]);
  SteadyStateGrid.Repaint;

  Jacobian := controller.simulator.roadrunner.getFullJacobian();
  m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;

  try
    floats := controller.simulator.roadrunner.getFloatingSpeciesIds();
    for i := 1 to m do
        gridJacobian.Columns[i].Header := floats[i - 1];
    for i := 0 to m - 1 do
        gridJacobian.Cells[0, i] := floats[i];
  finally
    floats.Free;
  end;

  for i := 1 to m do
     for j := 1 to m do
       gridJacobian.Cells[j, i - 1] := Format('%6.8f', [Jacobian[i - 1, j - 1]]);

  computeFCC;
  computeCCC;
  computeElasticities;
end;

procedure TfrmMoreSteadyState.FormCreate(Sender: TObject);
begin
  GridSS.Columns[0].Header := 'Variable';
  GridSS.Columns[1].Header := 'Value';
end;


procedure TfrmMoreSteadyState.FormShow(Sender: TObject);
begin
  setupGrids;
  btnSolveClick(Sender);
end;


procedure TfrmMoreSteadyState.GridCCCDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  bgBrush: TBrush;
  floatValue: double;
  astr: string;
begin
  astr := Value.toString;
  if not TryStrToFloat(astr, floatValue) then
    Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  GridElasticities.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientPlus.InterpolateColor(floatValue))
    // ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgBrush.Free;
end;


procedure TfrmMoreSteadyState.GridElasticitiesDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  bgBrush: TBrush;
  floatValue: double;
  astr: string;
begin
  astr := Value.toString;
  if not TryStrToFloat(astr, floatValue) then
    Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  GridElasticities.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientPlus.InterpolateColor(floatValue))
    // ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgBrush.Free;
end;


procedure TfrmMoreSteadyState.GridFCCDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  bgBrush: TBrush;
  floatValue: double;
  astr: string;
begin
  astr := Value.toString;
  if not TryStrToFloat(astr, floatValue) then
    Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  GridElasticities.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientPlus.InterpolateColor(floatValue))
    // ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgBrush.Free;
end;

procedure TfrmMoreSteadyState.GridJacobianDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  bgBrush: TBrush;
  floatValue: double;
  astr: string;
begin
  astr := Value.toString;
  if not TryStrToFloat(astr, floatValue) then
    Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  GridElasticities.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientPlus.InterpolateColor(floatValue))
    // ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid,
      gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgBrush.Free;
end;

initialization

maxWidth := 8;
decimalPlaces := 3;
gradientPlus := TGradient.Create;
gradientPlus.Style := TGradientStyle.Linear;
gradientPlus.color := clawhite;
gradientPlus.color1 := claYellowGreen;

decimalPlaces := 3;
gradientNegative := TGradient.Create;
gradientNegative.Style := TGradientStyle.Linear;
gradientNegative.color := clawhite;
gradientNegative.color1 := claTomato;

end.
