unit ufStructuralAnalysis;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects,
  FMX.TabControl, System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,
  uController;

type
  TfrmStructuralAnalysis = class(TForm)
    Layout1: TLayout;
    btnClose: TButton;
    Layout2: TLayout;
    Label2: TLabel;
    Line1: TLine;
    Layout3: TLayout;
    TabControl1: TTabControl;
    tbStoichiometryMatrix: TTabItem;
    tbNr: TTabItem;
    tbLink: TTabItem;
    gridStoichiometryMatrix: TStringGrid;
    StyleBook1: TStyleBook;
    tbConservation: TTabItem;
    gridReducedStoich: TStringGrid;
    gridLinkMatrix: TStringGrid;
    gridConservationMatrix: TStringGrid;
    procedure btnCloseClick(Sender: TObject);
    procedure gridStoichiometryMatrixDrawColumnCell(Sender: TObject;
      const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
      const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure FormShow(Sender: TObject);
    procedure gridConservationMatrixDrawColumnCell(Sender: TObject;
      const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
      const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure gridLinkMatrixDrawColumnCell(Sender: TObject;
      const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
      const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure gridReducedStoichDrawColumnCell(Sender: TObject;
      const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
      const Row: Integer; const Value: TValue; const State: TGridDrawStates);
  private
    { Private declarations }
    procedure getStoichiometryMatrix;
    procedure getConservationMatrix;
    procedure getReducedStoichMatrix;
    procedure getLinkMatrix;
  public
    { Public declarations }
    controller : TController;
  end;

var
  frmStructuralAnalysis: TfrmStructuralAnalysis;

implementation

{$R *.fmx}

Uses System.UIConsts, uRoadRunner, uMatrix, uRRTypes;

var
  gradientPlus, gradientNegative : TGradient;
  maxWidth, decimalPlaces : integer;

procedure TfrmStructuralAnalysis.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TfrmStructuralAnalysis.getReducedStoichMatrix;
var n, m, i, j : integer;
    listR, listF : TStringList;
    st : T2DMatrix;
begin
  gridReducedStoich.ClearColumns;
  st := controller.simulator.roadrunner.getNrMatrix;
  try
    n := controller.simulator.roadrunner.getNumberOfReactions;
    m := controller.simulator.roadrunner.getNumberOfIndependentSpecies;
    gridReducedStoich.RowCount := m;

    listR := controller.simulator.roadrunner.getReactionIds;
    listF := controller.simulator.roadrunner.getFloatingSpeciesIds;
    try
      for i := 0 to n do
          gridReducedStoich.AddObject(TStringColumn.Create(gridReducedStoich));

      for i := 0 to n - 1 do
          gridReducedStoich.Columns[i+1].Header := listR[i];
      for i := 0 to m - 1 do
          gridReducedStoich.Cells[0, i] := listF[i];

    finally
      listR.Free;
      listF.Free;
    end;
    for i := 0 to m - 1do
        for j := 0 to n - 1 do
            gridReducedStoich.Cells[j+1,i] := Format ('%6.2f', [st[i, j]]);
  finally
     st.Free;
  end;
end;


procedure TfrmStructuralAnalysis.getLinkMatrix;
var n, m, i, j : integer;
    listF : TStringList;
    st : T2DMatrix;
begin
  gridLinkMatrix.ClearColumns;
  st := controller.simulator.roadrunner.getLinkMatrix;
  try
    n := st.r;
    m := st.c;
    gridLinkMatrix.RowCount := n;

    listF := controller.simulator.roadrunner.getFloatingSpeciesIds;
    try
      for i := 0 to m do
          gridLinkMatrix.AddObject(TStringColumn.Create(gridReducedStoich));

      //for i := 0 to m - 1 do
      //    gridReducedStoich.Columns[i+1].Header := listF[i];
      for i := 0 to listF.Count - 1 do
          gridLinkMatrix.Cells[0, i] := listF[i];

    finally
      listF.Free;
    end;
    for i := 0 to n - 1do
        for j := 0 to m - 1 do
            gridLinkMatrix.Cells[j+1,i] := Format ('%6.2f', [st[i, j]]);
  finally
     st.Free;
  end;
end;


procedure TfrmStructuralAnalysis.FormShow(Sender: TObject);
begin
  getStoichiometryMatrix;
  getReducedStoichMatrix;
  getLinkMatrix;
  getConservationMatrix;
end;


procedure TfrmStructuralAnalysis.getConservationMatrix;
var st : T2DMatrix;
    m, n : integer;
    i, j : integer;
    listF : TStringList;
begin
  gridConservationMatrix.ClearColumns;
  st := controller.simulator.roadrunner.getConservationMatrix;
  try
    n := st.r;
    m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;
    gridConservationMatrix.RowCount := n;

    listF := controller.simulator.roadrunner.getFloatingSpeciesIds;
    try
      for i := 0 to m do
        gridConservationMatrix.AddObject(TStringColumn.Create(gridConservationMatrix));

      for i := 0 to m - 1 do
          gridConservationMatrix.Columns[i+1].Header := listF[i];
      for i := 0 to n - 1 do
          gridConservationMatrix.Cells[0, i] := '(' + inttostr (i+1) + ')';

    finally
      listF.Free;
    end;
    for i := 0 to n - 1 do
        for j := 0 to m - 1 do
            gridConservationMatrix.Cells[j+1,i] := Format ('%6.2f', [st[i, j]]);
  finally
     st.Free;
  end;
end;


procedure TfrmStructuralAnalysis.getStoichiometryMatrix;
var n, m, i, j : integer;
    listR, listF : TStringList;
    st : T2DMatrix;
begin
  gridStoichiometryMatrix.ClearColumns;
  st := controller.simulator.roadrunner.getStoichiometryMatrix;
  try
    n := controller.simulator.roadrunner.getNumberOfReactions;
    m := controller.simulator.roadrunner.getNumberOfFloatingSpecies;
    GridStoichiometryMatrix.RowCount := m;

    listR := controller.simulator.roadrunner.getReactionIds;
    listF := controller.simulator.roadrunner.getFloatingSpeciesIds;
    try
      for i := 0 to n do
        gridStoichiometryMatrix.AddObject(TStringColumn.Create(GridStoichiometryMatrix));

      for i := 0 to n - 1 do
          gridStoichiometryMatrix.Columns[i+1].Header := listR[i];
      for i := 0 to m - 1 do
          gridStoichiometryMatrix.Cells[0, i] := listF[i];

    finally
      listR.Free;
      listF.Free;
    end;
    for i := 0 to n - 1 do
        for j := 0 to m - 1 do
            GridStoichiometryMatrix.Cells[i+1, j] := Format ('%6.2f', [st[j, i]]);
  finally
     st.Free;
  end;
end;


procedure TfrmStructuralAnalysis.gridConservationMatrixDrawColumnCell(
  Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
  const Bounds: TRectF; const Row: Integer; const Value: TValue;
  const State: TGridDrawStates);
var bgBrush : TBrush;
    floatValue : double;
    astr : string;
begin
  astr := value.toString;
  if astr[1] = '(' then
     begin
     Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
     exit;
     end;

  if not TryStrToFloat (astr, floatValue) then
     Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  gridConservationMatrix.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientPlus.InterpolateColor(floatvalue))//  ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgbrush.Free;
end;


procedure TfrmStructuralAnalysis.gridLinkMatrixDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var bgBrush : TBrush;
    floatValue : double;
    astr : string;
begin
  astr := value.toString;
  if not TryStrToFloat (astr, floatValue) then
     Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  gridLinkMatrix.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientPlus.InterpolateColor(floatvalue))//  ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgbrush.Free;
end;


procedure TfrmStructuralAnalysis.gridReducedStoichDrawColumnCell(
  Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
  const Bounds: TRectF; const Row: Integer; const Value: TValue;
  const State: TGridDrawStates);
var bgBrush : TBrush;
    floatValue : double;
    astr : string;
begin
  astr := value.toString;
  if not TryStrToFloat (astr, floatValue) then
     Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  gridReducedStoich.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientPlus.InterpolateColor(floatvalue))//  ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgbrush.Free;
end;

procedure TfrmStructuralAnalysis.gridStoichiometryMatrixDrawColumnCell(
  Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
  const Bounds: TRectF; const Row: Integer; const Value: TValue;
  const State: TGridDrawStates);
var bgBrush : TBrush;
    floatValue : double;
    astr : string;
begin
  astr := value.toString;
  if not TryStrToFloat (astr, floatValue) then
     Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);

  GridStoichiometryMatrix.DefaultDrawing := False;
  if floatValue > 0 then
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientPlus.InterpolateColor(floatvalue))//  ColorBetween(claWhite, claYellowGreen, trunc (floatValue*300)))
  else
    bgBrush := TBrush.Create(TBrushKind.Solid, gradientNegative.InterpolateColor(-floatValue));

  Canvas.FillRect(Bounds, 0, 0, [], 1, bgBrush);
  Column.DefaultDrawCell(Canvas, Bounds, Row, Value, State);
  bgbrush.Free;
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
  gradientNegative.color := claWhite;
  gradientNegative.color1 := claTomato;
end.
