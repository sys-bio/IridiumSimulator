unit ufMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GDIPOBJ, GDIPAPI, uPlottingPanel, uDataSeries, ExtCtrls,
  uSymbolDetails, IOUtils;

type
  TBox = record left, top, w, h : integer; end;

  TfrmMain = class(TForm)
    pnlTop: TPanel;
    btnSubGraph1: TButton;
    btnPlotDataSin: TButton;
    btnSubGraph2: TButton;
    Button2: TButton;
    Label1: TLabel;
    btnSubGraphs: TButton;
    Label2: TLabel;
    btnAddGraph: TButton;
    plt: TRRGraph;
    btnDemo: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSubGraph1Click(Sender: TObject);
    procedure btnSubGraph2Click(Sender: TObject);
    procedure btnPlotDataSinClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnSubGraphsClick(Sender: TObject);
    procedure btnAddGraphClick(Sender: TObject);
    procedure pltResize(Sender: TObject);
    procedure btnDemoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

Uses uSubGraph;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  //CurrentlyPrinting := false;
  //plt := TGraph.Create (self);
  //plt.Align := alClient;
  //plt.parent := graphPanel;
  //plt.Left := 60;
  //plt.Top := 80;
  //plt.visible := true;
end;


procedure TfrmMain.pltResize(Sender: TObject);
begin
  plt.update;
end;

procedure TfrmMain.btnAddGraphClick(Sender: TObject);
begin
  plt.startAddInteractiveGraph;
end;


procedure TfrmMain.btnSubGraph1Click(Sender: TObject);
var sg : TSubGraph;
begin
  sg := plt.new(0.1, 0.2, 0.5, 0.5);
  plt.ParentBackground := true;
  sg.FId := (Sender as TButton).Name;
  sg.Xmin := 0; sg.Xmax := 10;
  sg.Ymin := -1.1; sg.Ymax := 1.1;
  sg.LogXAxis := false;
  //plt.VScrollBar.visible := false;
  //plt.HScrollBar.visible := false;
  plt.update;
end;


procedure TfrmMain.btnSubGraph2Click(Sender: TObject);
var sg : TSubGraph;
begin
  sg := plt.new(0.65, 0.65, 0.25, 0.25);
  sg.Xmin := -30; sg.Xmax := 30;
  sg.Ymin := -100; sg.Ymax := 100;
  sg.FId := (Sender as TButton).Name;
  plt.update;
end;

procedure TfrmMain.btnSubGraphsClick(Sender: TObject);
var i, j : integer;
    dataSeries : TDataSeries;
    sg : TSubGraph;
    leftStart, bottomStart : double;
begin
  leftStart := 0.05;
  bottomStart := 0.1;

  for i := 0 to 2 do
      begin
      leftStart := 0.05;
      for j := 0 to 2 do
          begin
          sg := plt.new(leftStart, bottomStart, 0.25, 0.2);
          sg.Xmin := 0; sg.Xmax := 50;
          sg.Ymin := 0; sg.Ymax := 1;
          leftStart := leftStart + 0.3;
          end;
      bottomStart := bottomStart + 0.33;
      end;

  for i := 0 to plt.FSubgraphs.Count - 1 do
      begin
      dataSeries := TDataSeries.Create;
      dataSeries.Add (TDataBlock.Create (20, 1));
      for j := 0 to dataSeries[0].nRows - 1 do
         begin
         dataSeries[0].XData[j] := j*2.6;
         dataSeries[0].YData[j, 0] := random;
         end;

      plt.subgraphs[i].dataSeries := dataSeries;
      plt.subgraphs[i].dataSeries[0].YSymbols[0].Symbol := SolidDiamond;
      plt.subgraphs[i].dataSeries[0].YSymbols[0].SymbolRadiusInCms := 0.075;
      end;
  plt.update;
end;


procedure TfrmMain.btnDemoClick(Sender: TObject);
var i, j : integer;
    leftStart, bottomStart : double;
    sg : TSubGraph;
    startX : double;
    hstep : double;
    xx : integer;
    nPoints : integer;
begin
  bottomStart := 0.1;
  for i := 0 to 1 do
      begin
      leftStart := 0.1;
      for j := 0 to 1 do
          begin
          sg := plt.new(leftStart, bottomStart, 0.35, 0.36);
          sg.Xmin := 0; sg.Xmax := 5;
          sg.Ymin := -2; sg.Ymax := 2;
          leftStart := leftStart + 0.45;
          end;
      bottomStart := bottomStart + 0.48;
      end;

  startX := 0;
  plt.subgraphs[0].dataSeries.Add (TDataBlock.Create (50, 1));
  for i := 0 to plt.subgraphs[0].dataSeries[0].nRows - 1 do
      begin
      plt.subgraphs[0].dataSeries[0].XData[i] := startX;
      plt.subgraphs[0].dataSeries[0].YData[i,0] := sin (0.4*(startX/10)*57.29);
      startX := startX + 0.1;
      end;

  plt.subgraphs[0].dataSeries[0].YSymbols[0].Symbol := SolidDiamond;

  // =====================
  startX := 0;
  plt.subgraphs[1].dataSeries.Add (TDataBlock.Create (50, 1));
  for i := 0 to plt.subgraphs[1].dataSeries[0].nRows - 1 do
      begin

      plt.subgraphs[1].dataSeries[0].XData[i] := startX;
      plt.subgraphs[1].dataSeries[0].YData[i,0] := cos (0.4*(startX/10)*57.29);
      startX := startX + 0.1;
      end;

  plt.subgraphs[1].dataSeries[0].YSymbols[0].Symbol := SolidCircle;
  plt.subgraphs[1].dataSeries[0].YSymbols[0].SymbolRadiusInCms := 0.09;

  // =====================
  plt.subgraphs[2].dataSeries.Add (TDataBlock.Create (120, 1));
  startX := -30;
  for i := 0 to plt.subgraphs[2].dataSeries[0].nRows - 1 do
      begin
      plt.subgraphs[2].dataSeries[0].XData[i] := startX;
      plt.subgraphs[2].dataSeries[0].YData[i,0] := startX*startX*sin (startX)/10;
      startX := startX + 0.5;
      end;

  plt.subgraphs[2].Xmin := -30;  plt.subgraphs[2].Xmax := 30;
  plt.subgraphs[2].Ymin := -100;  plt.subgraphs[2].Ymax := 100;

  plt.subgraphs[2].dataSeries[0].YSymbols[0].Symbol := SolidCircle;
  plt.subgraphs[2].dataSeries[0].YSymbols[0].SymbolRadiusInCms := 0.09;
  plt.subgraphs[2].dataSeries[0].YSymbols[0].SymbolFillColor := aclCoral;
  plt.subgraphs[2].dataSeries[0].YSymbols[0].SymbolOutlineColor := aclOrangeRed;
  plt.subgraphs[2].dataSeries[0].YSymbols[0].SymbolOutlineInCms := 0.02;
  plt.subgraphs[2].dataSeries[0].YLines[0].LineColor := aclRed;

  // =====================
  nPoints := 400;

  plt.subgraphs[3].dataSeries.Add (TDataBlock.Create (nPoints, 2));

  plt.subgraphs[3].Xmin := -30;  plt.subgraphs[3].Xmax := 30;
  plt.subgraphs[3].Ymin := -100; plt.subgraphs[3].Ymax := 100;
  startX := -30;
  hstep := (plt.subgraphs[3].XMax - plt.subgraphs[3].XMin)/nPoints;
  xx := plt.subgraphs[3].dataSeries[0].nRows;
  for i := 0 to xx - 1 do
      begin
      plt.subgraphs[3].dataSeries[0].XData[i] := startX;
      plt.subgraphs[3].dataSeries[0].YData[i,0] := startX*startX*sin (startX)/10;
      startX := startX + hstep;
      end;
  startX := -30;
  for i := 0 to plt.subgraphs[3].dataSeries[0].nRows - 1 do
      begin
      plt.subgraphs[3].dataSeries[0].XData[i] := startX;
      plt.subgraphs[3].dataSeries[0].YData[i,1] := 4*exp (-0.1*startX)/1;
      startX := startX + hstep;
      end;

  plt.subgraphs[3].dataSeries[0].YSymbols[0].Symbol := SolidCircle;
  plt.subgraphs[3].dataSeries[0].YSymbols[0].SymbolRadiusInCms := 0.01;
  plt.subgraphs[3].dataSeries[0].YSymbols[0].SymbolFillColor := aclCoral;
  plt.subgraphs[3].dataSeries[0].YSymbols[0].SymbolOutlineColor := aclOrangeRed;
  plt.subgraphs[3].dataSeries[0].YSymbols[0].SymbolOutlineInCms := 0.02;
  plt.subgraphs[3].dataSeries[0].YLines[0].LineColor := aclBlueViolet;
  plt.subgraphs[3].dataSeries[0].YLines[0].LineSizeInCms := 0.09;

  plt.subgraphs[3].dataSeries[0].YSymbols[1].Symbol := SolidCircle;
  plt.subgraphs[3].dataSeries[0].YSymbols[1].SymbolRadiusInCms := 0.01;
  plt.subgraphs[3].dataSeries[0].YSymbols[1].SymbolFillColor := aclCoral;
  plt.subgraphs[3].dataSeries[0].YSymbols[1].SymbolOutlineColor := aclOrangeRed;
  plt.subgraphs[3].dataSeries[0].YSymbols[1].SymbolOutlineInCms := 0.02;
  plt.subgraphs[3].dataSeries[0].YLines[1].LineColor := aclRed;

  plt.update;
end;

procedure TfrmMain.btnPlotDataSinClick(Sender: TObject);
var i : integer;
    dataSeries : TDataSeries;
begin
  dataSeries := TDataSeries.Create;

  dataSeries.Add (TDataBlock.Create (120, 1));
  for i := 0 to dataSeries[0].nRows - 1 do
      begin
      dataSeries[0].XData[i] := i/10;
      dataSeries[0].YData[i,0] := sin (0.04*(i/10)*57.29);
      end;

  plt.subgraphs[0].dataSeries := dataSeries;
  plt.subgraphs[0].dataSeries[0].YSymbols[0].Symbol := SolidDiamond;
  plt.refresh;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var i : integer;
    dataSeries : TDataSeries;
    startX : double;
begin
  dataSeries := TDataSeries.Create;
  startX := -30;

  dataSeries.Add (TDataBlock.Create (100, 1));
  for i := 0 to dataSeries[0].nRows - 1 do
      begin
      dataSeries[0].XData[i] := startX;
      dataSeries[0].YData[i,0] :=  startX*startX*sin (startX)/10;
      startX := startX + 0.6;
      end;

  plt.subgraphs[1].dataSeries := dataSeries;
  plt.subgraphs[1].dataSeries[0].YLines[0].LineColor := aclRed;
  plt.subgraphs[1].dataSeries[0].YSymbols[0].SymbolVisible := false;
  plt.refresh;
end;

end.
