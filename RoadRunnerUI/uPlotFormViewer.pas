unit uPlotFormViewer;

// This unit deals with the inplace plotting panel on the main window
// It creates a viewer that can be registered with the controller


interface

Uses SysUtils,
     Classes,
     Rtti,
     FMX.Forms,
     FMX.Types,
     FMX.StdCtrls,
     FMX.Layouts,
     FMX.Dialogs,
     System.UITypes,
     uPlottingPanel,
     uSubGraph,
     uSymbolDetails,
     uRRDataSeries,
     uRRTypes,
     uController,
     uViewerTypes,
     uViewer;

type
  TIntArray = array of integer;

  TPlotFormViewer = class (TViewer)
    private
       savePDFDialog : TSaveDialog;
       procedure pltReportCoordinates(mx, my, gx, gy: Single);
       procedure OnButtonClick (Sender : TObject);
       procedure OnButtonExportPDFClick (Sender : TObject);
    public
      plt : TRRGraph;
      simulationData : T2Dmatrix;
      parentLayout : TLayout;
      baseLayout : TLayout;
      viewerPackage : TViewerPackage;
      lblWorldCoords : TLabel;
      lblMouseCoords : TLabel;

      procedure exportAsPDF (fileName : string);
      procedure plotSimulationData;
      function  getNumberOfSelectedColumns: integer;
      function  extractColumnIndexes: TIntArray;
      procedure ViewerUpdate (Sender : TObject; viewerPackage : TViewerPackage);
      procedure ViewerClear (Sender : TObject);
      procedure ViewerSetProperty (name : string; value : TValue);
      constructor Create (controller : TController; parentLayout : TLayout);
  end;

implementation

Uses ufMain, uColorList;

constructor TPlotFormViewer.Create (controller : TController; parentLayout : TLayout);
var bottomLayout : TLayout;
    btn, exportBtn : TButton;
begin
  baseLayout := TLayout.Create(parentLayout.parent);
  baseLayout.Parent := parentLayout;
  baseLayout.Align := TAlignLayout.Client;
  bottomLayout := TLayout.Create(baseLayout);
  bottomLayout.Parent := baseLayout;
  bottomLayout.Height := 64;
  bottomLayout.Align := TAlignLayout.Bottom;

  // Add control to bottom layout panel
  lblWorldCoords := TLabel.Create (bottomLayout);
  lblWorldCoords.parent := bottomLayout;
  lblWorldCoords.Width := 172;
  lblWorldCoords.Text := 'World Coordinates';

  lblMouseCoords := TLabel.Create (bottomLayout);
  lblMouseCoords.parent := bottomLayout;
  lblMouseCoords.Position.Y := 16;
  lblMouseCoords.Width := 256;
  lblMouseCoords.Text := 'Mouse Coordinates';

  btn := TButton.Create(bottomLayout);
  btn.parent := bottomLayout;
  btn.Text := 'Graph Properties';
  btn.Width := 124;
  btn.Height := 32;
  btn.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];
  btn.Position.X := bottomLayout.Width - btn.Width;
  btn.Position.Y := (bottomLayout.Height / 2) - btn.Height/2;
  btn.OnClick := OnButtonClick;

  exportBtn := TButton.Create(bottomLayout);
  exportBtn.parent := bottomLayout;
  exportBtn.Text := 'Export to PDF';
  exportBtn.Width := 124;
  exportBtn.Height := 32;
  exportBtn.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];
  exportBtn.Position.Y := (bottomLayout.Height / 2) - btn.Height/2;
  exportBtn.Position.X := btn.Position.X - btn.width - 12;
  exportBtn.OnClick := OnButtonExportPDFClick;

  savePDFDialog := TSaveDialog.Create(nil);
  savePDFDialog.DefaultExt := '.pdf';
  savePDFDialog.Filter := 'PDF File|*.pdf|Any files|*.*';

  plt := TRRGraph.Create(baseLayout);
  plt.parent := baseLayout;
  plt.Align := TAlignLayout.Client;
  plt.OnReportCoordinates := pltReportCoordinates;
  plt.addDefaultSubgraph;

  OnViewerChange := ViewerUpdate;  // gets fired if there is new data
  OnViewerClear := ViewerClear;
  OnViewerSetProperty := ViewerSetProperty;
  self.parentLayout := parentLayout;
  controller.RegisterViewer(self);
end;


procedure TPlotFormViewer.OnButtonClick (Sender : TObject);
begin
  plt.startPropertyEditor(nil, 0);
end;


procedure TPlotFormViewer.OnButtonExportPDFClick (Sender : TObject);
begin
  if savePDFDialog.Execute then
     exportAsPDF (savePDFDialog.fileName);
end;


procedure TPlotFormViewer.ViewerUpdate (Sender : TObject; viewerPackage : TViewerPackage);
begin
  simulationData := (sender as TController).simulator.simulationData;
  self.viewerPackage := viewerPackage;
  plotSimulationData;
end;


procedure TPlotFormViewer.ViewerClear (Sender : TObject);
var
  dbs: TDataBlocks;
begin
  dbs := plt.subgraphs[0].properties.dataBlocks;
  dbs.Clear;
end;

procedure TPlotFormViewer.ViewerSetProperty (name : string; value : TValue);
begin
  if name = 'legend' then
     begin
     plt.getSubgraph(0).properties.legend.visible := Value.AsBoolean;
     exit;
     end;
  if name = 'UserScale_Xmin' then
     begin
     plt.getSubgraph(0).properties.UserScale_Xmin := Value.AsExtended;
     exit;
     end;
  if name = 'UserScale_Xmax' then
     begin
     plt.getSubgraph(0).properties.UserScale_Xmax := Value.AsExtended;
     exit;
     end;
  if name = 'UserScale_Ymin' then
     begin
     plt.getSubgraph(0).properties.UserScale_Ymin := Value.AsExtended;
     exit;
     end;
  if name = 'UserScale_Ymax' then
     begin
     plt.getSubgraph(0).properties.UserScale_Ymax := Value.AsExtended;
     exit;
     end;
  if name = 'AutoYScaling' then
     begin
     plt.getSubgraph(0).properties.AutoYScaling := Value.AsBoolean;
     exit;
     end;
end;


procedure TPlotFormViewer.pltReportCoordinates(mx, my, gx, gy: Single);
begin
  lblWorldCoords.Text := 'World X: ' + Format('%8.5f', [gx]) + ', WY: ' + Format('%8.5f', [gy]);
  lblMouseCoords.Text := 'Mouse X: ' + Format('%8.1f', [mx]) + ', MY: ' + Format('%8.1f', [my]);
end;


procedure TPlotFormViewer.exportAsPDF (fileName : string);
begin
  plt.exportToPDF(FileName);
end;


function TPlotFormViewer.getNumberOfSelectedColumns: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to length(viewerPackage.YColumnChoice) - 1 do
    if viewerPackage.YColumnChoice[i] then
      inc(result);
end;


function TPlotFormViewer.extractColumnIndexes: TIntArray;
var
  i, nc, Count: integer;
begin
  nc := getNumberOfSelectedColumns;
  Count := 0;
  setLength(result, nc);
  for i := 0 to length(viewerPackage.YColumnChoice) - 1 do
    if viewerPackage.YColumnChoice[i] then
      begin
        result[Count] := i;
        inc(Count);
      end;
end;


procedure TPlotFormViewer.plotSimulationData;
var
  dbs: TDataBlocks;
  db: TDataBlock;
  i, j, ns: integer;
  YColumnIndexes: TIntArray;
  numRows: integer;
  sg: TSubgraph;
begin
  // A data series is a list of data blocks.
  // Each data block is a list of columns
  sg := plt.getSubgraph(0);
  sg.properties.Legend.visible := viewerPackage.showLegend;
  dbs := sg.properties.dataBlocks;
  dbs.Clear;
  ns := getNumberOfSelectedColumns;
  if ns = 0 then
     begin
     plt.Redraw;
     exit;
     end;

  dbs[0].name := 'simData';

  // Create the columns in the data block
  dbs[0].createDataColumn('Time', simulationData.r);

  YColumnIndexes := extractColumnIndexes;
  for i := 0 to ns - 1 do
    dbs[0].createDataColumn(viewerPackage.YColumnNames[YColumnIndexes[i]], simulationData.r);

  numRows := dbs[0].columns[0].getNumRows();
  // Copy over the x column first
  for i := 0 to numRows - 1 do
    dbs[0].columns[0].setDataValue(i, getElement(simulationData, i, viewerPackage.XColumnIndex));
  dbs[0].xaxisColumn := 'Time';

  // Copy the y columns next
  for i := 0 to numRows - 1 do
    begin
      for j := 1 to dbs[0].columns.Count - 1 do
        dbs[0].columns[j].setDataValue(i, getElement(simulationData, i, YColumnIndexes[j - 1]));
    end;

  masterColorList.SetPalette(viewerPackage.palette, dbs[0].columns.Count);
  masterColorList.restart;
  for j := 0 to sg.properties.dataBlocks[0].columns.Count - 1 do
    begin
      dbs[0].columns[j].symbol.symType := TSymbolType.Empty;
      dbs[0].columns[j].lineDetails.color := masterColorList.nextColor;
    end;

  plt.subgraphs[0].XAxisTitle := viewerPackage.XAxisTitle;

  if not viewerPackage.autoXScale then
     plt.subgraphs[0].properties.UserScale_Xmax := viewerPackage.timeEnd;

  plt.subgraphs[0].properties.AutoXScaling := viewerPackage.autoXScale;
  plt.subgraphs[0].properties.AutoYScaling := viewerPackage.autoYScale;
  plt.redraw;
end;



end.
