unit ufFloatingPlotViewer;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes, System.Variants,
  FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  uRRTypes,
  uRR2DSimpleMatrix,
  FMX.ListBox,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  uController,
  uColorList,
  uFormViewer,
  uSimulator,
  uViewerTypes,
  Rtti, System.Skia,
  SkPlotPaintBox,
  uPlotSeries,
  FMX.Skia;

type
  TIntArray = array of integer;

  TNotifySimulateEvent = procedure(simulator : TSimulator) of object;

  TfrmFloatingPlotViewer = class(TFormViewer)
    Layout1: TLayout;
    Layout2: TLayout;
    GroupBox2: TGroupBox;
    cbXAxis: TComboBox;
    GroupBox3: TGroupBox;
    btnSetTimeCourseSelection: TButton;
    Rectangle4: TRectangle;
    lstYAxis: TListBox;
    btnEditGraph: TButton;
    Plot: TSkPlotPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure lstYAxisChange(Sender: TObject);
    procedure cbXAxisChange(Sender: TObject);
    procedure btnEditGraphClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    YColumnChoice : array of boolean;
    floatingSpeciesIds : TStringList;
    colorList : TColorList;
    viewerPackage : TViewerPackage;
    fireEvent : boolean;
    function extractColumnIndexes: TIntArray;
    function getNumberOfSelectedColumns: integer;
    procedure updateXYSelection;
    procedure ViewerUpdate  (sender : TObject; viewerPackage : TViewerPackage);
    procedure ViewerClear (Sender : TObject);
    procedure ViewerSetProperty (name : string; value : TValue);
    procedure ViewerModelHasChanged (sender : TObject);
  public
    { Public declarations }
    controller : TController;
    simulationData : T2DMatrix;

    procedure plotSimulationData;
  end;

var
  frmFloatingPlotViewer: TfrmFloatingPlotViewer;

implementation

{$R *.fmx}

Uses uSymbolDetails, uRRCommon;

procedure TfrmFloatingPlotViewer.ViewerModelHasChanged (sender : TObject);
begin
  updateXYSelection;
end;

procedure TfrmFloatingPlotViewer.lstYAxisChange(Sender: TObject);
var i : integer;
begin
  if fireEvent then
    begin
    for i := 0 to lstYAxis.Items.Count - 1 do
        YColumnChoice[i] := (lstYAxis.listitems[i] as Tlistboxitem).IsChecked;
    plotSimulationData;
    end;
end;


procedure TfrmFloatingPlotViewer.btnEditGraphClick(Sender: TObject);
begin
  //plt.startPropertyEditor(self, 0, TSubGraphSelectedObjectType.coGraphingArea);
end;


procedure TfrmFloatingPlotViewer.cbXAxisChange(Sender: TObject);
begin
  //if fireEvent then
  begin
    plotSimulationData;
  end;
end;


function TfrmFloatingPlotViewer.extractColumnIndexes: TIntArray;
var
  i, nc, Count: integer;
begin
nc := getNumberOfSelectedColumns;
Count := 0;
setLength(result, nc);
for i := 0 to length(YColumnChoice) - 1 do
  if YColumnChoice[i] then
    begin
    result[Count] := i;
    inc(Count);
    end;
end;


procedure TfrmFloatingPlotViewer.ViewerUpdate (sender : TObject; viewerPackage : TViewerPackage);
var i : integer;
begin
  self.controller := sender as TController;
  simulationData := controller.simulator.simulationData;
  self.viewerPackage := viewerPackage;
  fireEvent := False;
  try
    lstYAxis.BeginUpdate;
    lstYAxis.Clear;
    for i := 0 to length (self.viewerPackage.YColumnNames) - 1 DO
        lstYAxis.Items.Add (self.viewerPackage.YColumnNames[i]);

    setLength (YColumnChoice, length (self.viewerPackage.YColumnNames));
    YColumnChoice[0] := false;
    for i := 1 to length (self.viewerPackage.YColumnNames) - 1 do
      begin
        (lstYAxis.listitems[i] as Tlistboxitem).IsChecked := true;
        YColumnChoice[i] := true;
      end;
    lstYAxis.EndUpdate;

    plotSimulationData;
  finally
    fireEvent := True;
  end;
end;


procedure TfrmFloatingPlotViewer.ViewerClear (Sender : TObject);
begin
  Plot.ClearSeries;
end;


procedure TfrmFloatingPlotViewer.ViewerSetProperty (name : string; value : TValue);
begin
  if name = 'legend' then
     begin
     Plot.LegendStyle.visible := Value.AsBoolean;
     end;
end;


procedure TfrmFloatingPlotViewer.FormCreate(Sender: TObject);
begin
  inherited;
  colorList := TColorList.Create;
  OnViewerChange := ViewerUpdate;
  OnViewerClear := ViewerClear;
  OnViewerSetProperty := ViewerSetProperty;
end;


procedure TfrmFloatingPlotViewer.FormShow(Sender: TObject);
begin
  controller.RegisterFormViewer(self);
end;


function TfrmFloatingPlotViewer.getNumberOfSelectedColumns: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to length(YColumnChoice) - 1 do
    if YColumnChoice[i] then
      inc(result);
end;


procedure TfrmFloatingPlotViewer.updateXYSelection;
var
  i: integer;
begin
  fireEvent := false;
  try
    lstYAxis.Clear;
    cbXAxis.Clear;
    for i := 0 to length (viewerPackage.YColumnNames) - 1 do
      begin
      lstYAxis.Items.Add(viewerPackage.YColumnNames[i]);
      cbXAxis.Items.Add(viewerPackage.YColumnNames[i]);
      cbXAxis.itemindex := 0;
      end;
    setLength(YColumnChoice, length (viewerPackage.YColumnNames));
    if length (viewerPackage.YColumnNames) = 0 then
      exit;

    (lstYAxis.listitems[0] as Tlistboxitem).IsChecked := false;
    YColumnChoice[0] := false;
    for i := 1 to length (viewerPackage.YColumnNames) - 1 do
      begin
      (lstYAxis.listitems[i] as Tlistboxitem).IsChecked := true;
      YColumnChoice[i] := true;
      end;
    lstYAxis.itemindex := -1;
  finally
    fireEvent := true;
  end;
end;


procedure TfrmFloatingPlotViewer.plotSimulationData;
var
  minv: double;
  i, j, NumYColumns: integer;
  YColumnSelectionCount: integer;
  YColumnIndexes: TIntArray;
  numRows: integer;
  VariableName : String;
  Series : TPlotSeries;
begin
  if simulationData = nil then
     exit;

  if not simulationData.valid then
     exit;

  // A data series is a list of data blocks.
  // Each data block is a list of columns
  Plot.ClearSeries;

  NumYColumns := GetNumberOfSelectedColumns;
  YColumnIndexes := ExtractColumnIndexes;

  NumRows :=  simulationData.r;
  NumYColumns := simulationData.c - 1;

  masterColorList.SetPalette(viewerPackage.palette, NumYColumns+1);
  masterColorList.restart;
  masterColorList.nextColor;

  for i := 0 to NumYColumns - 1 do
      begin
      VariableName := viewerPackage.YColumnNames[YColumnIndexes[i]];
      Series := TPlotSeries.Create(VariableName, claBlue);
      Series.LineColor := masterColorList.nextColor;
      Series.MarkerVisible := False;
      for j := 0 to NumRows - 1 do
        begin
        Series.AddXY (getElement(simulationData, j, viewerPackage.XColumnIndex), getElement(simulationData, j, YColumnIndexes[i]));
        end;

      Plot.AddSeries(Series);
      end;

  Plot.XAxisTitle.Text := viewerPackage.XAxisTitle;


  Plot.XAxisTitle.Text := viewerPackage.XAxisTitle;

  //plt.subgraphs[0].properties.AutoXScaling := viewerPackage.autoXScale;
  //plt.subgraphs[0].properties.AutoYScaling := viewerPackage.autoYScale;

//  plt.subgraphs[0].XAxisTitle := cbXAxis.Items[cbXAxis.itemindex];
//  if chkAutoscaleX.IsChecked then
//    plt.subgraphs[0].properties.AutoXScaling := True
//  else
//    plt.subgraphs[0].Xmax := strtofloat(edtTimeEnd.Text);
//
//  if chkAutoScaleY.IsChecked then
//    plt.subgraphs[0].properties.AutoYScaling := true
//  else
//    plt.subgraphs[0].properties.AutoYScaling := false;

  Plot.Redraw;
end;


end.
