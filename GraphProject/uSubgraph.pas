unit uSubgraph;

interface

Uses SysUtils, System.Classes,
  Generics.Collections, FMX.Graphics,
  uRRCommon, Math,
  uRRDataSeries,
  uSymbolDetails,
  uLineDetails,
  uGObject,
  System.UIConsts,
  System.UITypes,
  System.Types,
  System.Math.Vectors,
  skia,
  skia.FMX,
  skia.FMX.Graphics,
  uProperties;

type
  TSubgraph = class(TPersistent)

  private
    FId: String;

    // Used by fx() and fy()
    xscale, yscale, xfactor, yfactor: double; // world/device scaling factors
    fontScalingFactor: double;

    FSubGraphProperties: TSubGraphProperties;

    XminComputedLimit, XmaxComputedLimit, YminComputedLimit, YmaxComputedLimit: double;
    AutoScale_Xmin, AutoScale_Xmax, AutoScale_Ymin, AutoScale_Ymax: Extended;

    // AxesLabelsFont : TTextType;
    LPaint: ISkPaint;

    procedure initializeDefaults();

    procedure determineOrigin;
    procedure setAxesLimits;
    procedure autoScaleXAxis(HowtoScale: THowtoScale);
    procedure autoScaleYAxis(HowtoScale: THowtoScale);
    procedure findDataLimits;

    procedure computeScalingFactors(rect: TRectF);
    function fx(wx: double): double;
    function fy(wy: double): double;
    function computePhysicalSize(sizeCm: double): single;

    function labelXString(d: double): string;
    function labelYString(d: double): string;

    procedure drawAxesLines(ACanvas: ISkCanvas; DrawXAxis, DrawYAxis: boolean);
    procedure drawXAxisTicks(ACanvas: ISkCanvas);

    procedure drawXAxisMajorTick(ACanvas: ISkCanvas; Origin, MajorVal: Extended; LPaint: ISkPaint);
    procedure drawXMinorTicks(ACanvas: ISkCanvas; startvalue, stepValue, Origin, min, max: double; nticks, direction: integer);
    procedure drawXAxisLogTicks(ACanvas: ISkCanvas);
    procedure drawLogXMinorTicks(ACanvas: ISkCanvas; startvalue, stepValue, Origin, min, max: double; nticks, direction: integer);
    procedure drawAllMajorXGridLines(ACanvas: ISkCanvas);
    procedure drawXGridLine(ACanvas: ISkCanvas; x, ymin, ymax: double; LPaint: ISkPaint);

    procedure drawYAxisLogTicks(ACanvas: ISkCanvas);
    procedure drawYAxisTicks(ACanvas: ISkCanvas);
    procedure drawYAxisMajorTick(ACanvas: ISkCanvas; Origin, MajorVal: Extended);
    procedure drawAllMajorYGridLines(ACanvas: ISkCanvas);
    procedure drawYMinorTicks(ACanvas: ISkCanvas; startvalue, stepValue, Origin, min, max: double; nticks, direction: integer);
    procedure drawLogYMinorTicks(canvas: TCanvas; startvalue, stepValue, Origin, min, max: double; nticks, direction: integer);
    procedure drawYGridLine(ACanvas: ISkCanvas; y, xmin, xmax: double; LPaint: ISkPaint);

    procedure drawYAxisTitle(ACanvas: ISkCanvas);

    procedure drawSelectedGraphingArea(ACanvas: ISkCanvas);
    procedure drawYLabel(ACanvas: ISkCanvas; value: double; x, y: single);
    procedure drawLegend(ACanvas: ISkCanvas);

    procedure drawXErrorBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dx: single; Style: TErrorBarStyle;
      CapStyle: TErrorBarCapStyle);
    procedure drawXErrorHalfBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dx: single; Style: TErrorBarStyle;
      CapStyle: TErrorBarCapStyle);

    procedure drawYErrorBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dy: single; Style: TErrorBarStyle;
      CapStyle: TErrorBarCapStyle);
    procedure drawYErrorHalfBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dy: single; Style: TErrorBarStyle;
      CapStyle: TErrorBarCapStyle);

    procedure plotSymbols(ACanvas: ISkCanvas);
    procedure plotLines(ACanvas: ISkCanvas);

    procedure drawLine(ACanvas: ISkCanvas; x1, y1, x2, y2: single; lineDetails: TlineDetails);
    procedure drawSymbol(ACanvas: ISkCanvas; x, y: double; xerr, yerr: TErrorDatum; symbol: TSymbol);

    procedure drawSymbolAtDeviceCoords(ACanvas: ISkCanvas; x, y: single; symbol: TSymbol);

    procedure drawSquare(ACanvas: ISkCanvas; x1, y1, x2, y2: single; symbol: TSymbol);
    procedure drawPolygon(ACanvas: ISkCanvas; Points: TPolygon; symbol: TSymbol);
    procedure drawEllipse(ACanvas: ISkCanvas; x1, y1, x2, y2: single; symbol: TSymbol);
    procedure drawCircle(ACanvas: ISkCanvas; x, y, diameter: single; symbol: TSymbol; filled: boolean);

    procedure drawLegendFrame(ACanvas: ISkCanvas; r: TRectF; Legend: TLegend);

    procedure drawGraphBorder(ACanvas: ISkCanvas);
    procedure drawSolidLine(ACanvas: ISkCanvas; x1, y1, x2, y2: single; color: TAlphaColor; lineThickness: single);

    procedure drawXLabel(ACanvas: ISkCanvas; value: double; x, y: single);
    procedure drawText(ACanvas: ISkCanvas; box: TBox; text: TTextType);

    procedure drawMainTitleSelected(ACanvas: ISkCanvas);
    procedure drawXAxisTitleSelected(ACanvas: ISkCanvas);
    procedure drawYAxisTitleSelected(ACanvas: ISkCanvas);

    procedure createGraphObjects;

  public
    parentGraph: TObject;

    XAxisLengthInPixels: single; // These two not used yet
    YAxisLengthInPixels: single;

    nXLogMinorTicks, nYLogMinorTicks: integer;

    MajorTickLengthInSkiaUnits: single;
    MinorTickLengthInSkiaUnits: single;

    AxisThicknessInCms: double;

    // Fractional distances follow, measured relative to the width or height of the ObjRoot
    // Vertical fractional distance from x axis for x axis label
    XLblVertOffSet: double;
    // Horizontal fractional distance from y axis for y axis label
    YLblHorizOffSet: double;

    MajorXTicks, MinorXTicks: boolean;
    MajorXTicksStyle, MinorXTicksStyle: TTickStyle;
    MajorYTicks, MinorYTicks: boolean;
    MajorYTicksStyle, MinorYTicksStyle: TTickStyle;

    XLabelNearAxis: boolean; { By default, x labels hug edge of graph }
    YLabelNearAxis: boolean; { " }
    XAxisNearEdge: boolean;
    YAxisNearEdge: boolean;
    drawXAxisLine: boolean; // Axis line itself
    drawYAxisLine: boolean;
    drawXLabelsOnAxes: boolean; // Labels next to the axis
    drawYLabelsOnAxes: boolean;

    Magnification: double; // 1.728;//2.0736;
    MaxMag: double;
    MinMag: double;
    Scaling: boolean;
    ViewXOffSet: integer;
    ViewYOffSet: integer;

    MinDistanceBetweenLegendItemsCms: double;

    MajorTickInCms: double; { 2mm }
    MinorTickInCms: double;

    // Not yet used
    XAxisLengthInCms: double; { 4 inches }
    YAxisLengthInCms: double; { 4 inches }

    GridLineStyle: TStrokeDash;

    XOrigin, YOrigin: double;

    selectedObjectType: TSubGraphSelectedObjectType;
    selectedObject: TGraphObject;

    constructor Create(panel: TObject);
    destructor Destroy; override;

    function getGraphDeviceDrawingArea: TRectF;
    function getLogicalBoundingBox(Id: integer): TLogicalBox;
    function getTextBoundingBox(ACanvas: ISkCanvas; textDetails: TTextType): TBox;
    function getYAxisTitleBoundingBox(ACanvas: ISkCanvas; textDetails: TTextType): TBox;

    function deviceToWorld(x, y: single): TPointF;
    function deltaDeviceToDeltaWorld(x, y: single): TPointF;
    function legend_relativeToDevice: TBox;
    // function relativeToDevice(graphObject: TGraphObject): TBox;
    function relativeToDevice2(graphObject: TGraphBase): TBox;

    procedure unselectAllObjects;

    procedure paint(ACanvas: ISkCanvas);

    function onSubGraph(ACanvas: ISkCanvas; x, y: single; var graphObject: TGraphBase): boolean;
    function IsOnMainTitle(ACanvas: ISkCanvas; x, y: single): boolean;
    function IsOnXAxisTitle(ACanvas: ISkCanvas; x, y: single): boolean;
    function IsOnXAxis(ACanvas: ISkCanvas; x, y: single): boolean;
    function IsOnYAxisTitle(ACanvas: ISkCanvas; x, y: single): boolean;
    function IsOnLegend(ACanvas: ISkCanvas; x, y: single): boolean;

    procedure setId(str: string);
    procedure setMainTitle(str: string);
    function getMainTitle: string;

    function getDataBlocks: TDataBlocks;

    procedure setXAxisTitle(str: string);
    function getXAxisTitle: string;
    procedure setYAxisTitle(str: string);
    function getYAxisTitle: string;

    procedure setGraphBorder(value: boolean);
    function getGraphBorder: boolean;

    procedure setWorldXMin(value: double);
    function getWorldXMin: double;
    procedure setWorldXMax(value: double);
    function getWorldXMax: double;
    procedure setWorldYMin(value: double);
    function getWorldYMin: double;
    procedure setWorldYMax(value: double);
    function getWorldYMax: double;

    procedure setRx(value: double);
    function getRx: double;
    procedure setRw(value: double);
    function getRw: double;
    procedure setRy(value: double);
    function getRy: double;
    procedure setRh(value: double);
    function getRh: double;

    property rxmin: double read getRx write setRx;
    property rymin: double read getRy write setRy;
    property rwidth: double read getRw write setRw;
    property rheight: double read getRh write setRh;

  published
    property Id: string read FId write setId;
    property xmin: double read getWorldXMin write setWorldXMin;
    property xmax: double read getWorldXMax write setWorldXMax;
    property ymin: double read getWorldYMin write setWorldYMin;
    property ymax: double read getWorldYMax write setWorldYMax;
    property MainTitle: string read getMainTitle write setMainTitle;
    property XAxisTitle: string read getXAxisTitle write setXAxisTitle;
    property YAxisTitle: string read getYAxisTitle write setYAxisTitle;
    property GraphBorder: boolean read getGraphBorder write setGraphBorder;
    property series: TDataBlocks read getDataBlocks;
    property properties: TSubGraphProperties read FSubGraphProperties write FSubGraphProperties;
  end;

implementation

Uses FMX.Dialogs, uPlottingPanel, uClipping, FMX.TextLayout, uRRUtilities, FMX.Types;

constructor TSubgraph.Create(panel: TObject);
begin
  inherited Create;
  properties := TSubGraphProperties.Create;
  parentGraph := panel;
  FId := 'SG' + inttostr(random(10000));

  initializeDefaults();
  createGraphObjects;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  properties.dataBlocks := TDataBlocks.Create;
  computeScalingFactors(getGraphDeviceDrawingArea());
  selectedObjectType := coNone;
end;


destructor TSubgraph.Destroy;
begin
  properties.Free;
  inherited Destroy;
end;


procedure TSubgraph.setWorldXMin(value: double);
begin
  properties.UserScale_Xmin := value;
  (parentGraph as TRRGraph).redraw;
end;

function TSubgraph.getWorldXMin: double;
begin
  result := properties.FWorldXmin;
end;

procedure TSubgraph.setWorldXMax(value: double);
begin
  properties.UserScale_Xmax := value;
  (parentGraph as TRRGraph).redraw;
end;

function TSubgraph.getWorldXMax: double;
begin
  result := properties.FWorldXmax;
end;

procedure TSubgraph.setWorldYMin(value: double);
begin
  properties.UserScale_Ymin := value;
  (parentGraph as TRRGraph).redraw;
end;

function TSubgraph.getWorldYMin: double;
begin
  result := properties.FWorldYmin;
end;

procedure TSubgraph.setWorldYMax(value: double);
begin
  properties.UserScale_Ymax := value;
  (parentGraph as TRRGraph).redraw;
end;

function TSubgraph.getWorldYMax: double;
begin
  result := properties.FWorldYmax;
end;

procedure TSubgraph.setRx(value: double);
begin
  properties.graphObjects[graphingAreaId].logicalBox.left := value;
end;

function TSubgraph.getRx: double;
begin
  result := properties.graphObjects[graphingAreaId].logicalBox.left;
end;

procedure TSubgraph.setRw(value: double);
begin
  properties.graphObjects[graphingAreaId].logicalBox.w := value;
end;

function TSubgraph.getRw: double;
begin
  result := properties.graphObjects[graphingAreaId].logicalBox.w;
end;

procedure TSubgraph.setRy(value: double);
begin
  properties.graphObjects[graphingAreaId].logicalBox.top := value;
end;

function TSubgraph.getRy: double;
begin
  result := properties.graphObjects[graphingAreaId].logicalBox.top;
end;

procedure TSubgraph.setRh(value: double);
begin
  properties.graphObjects[graphingAreaId].logicalBox.h := value;
end;

function TSubgraph.getRh: double;
begin
  result := properties.graphObjects[graphingAreaId].logicalBox.h;
end;

function TSubgraph.getLogicalBoundingBox(Id: integer): TLogicalBox;
begin
  result := properties.graphObjects[Id].logicalBox;
end;

procedure TSubgraph.createGraphObjects;
var
  graphObjects: TObjectList<TGraphObject>;
  gobj: TGraphObject;
begin
  properties.graphObjects := TObjectList<TGraphObject>.Create;
  graphObjects := properties.graphObjects;

  graphingAreaId := graphObjects.Add(TGraphObject.Create(coGraphingArea));
  graphObjects[graphingAreaId].logicalBox.left := 0.2;
  graphObjects[graphingAreaId].logicalBox.top := 0.8;
  graphObjects[graphingAreaId].logicalBox.w := 0.7;
  graphObjects[graphingAreaId].logicalBox.h := 0.6;
  graphObjects[graphingAreaId].reSizable := True;

  // *********************************************************
  // Coords relative to the left/**top** corner of graph drawing area
  // ie 0.5, 0.2 means 20% from the top 50% in from the left.

  properties.MainTitleObject := TMainTitle.Create;
  properties.XAxisTitleObject := TXAxisTitle.Create;
  properties.YAxisTitleObject := TYAxisTitle.Create;

  // The axis itself
  xaxisId := graphObjects.Add(TGraphObject.Create(coXAxis));
  graphObjects[xaxisId].logicalBox.left := 0.1;
  graphObjects[xaxisId].logicalBox.top := 0.2;
  graphObjects[xaxisId].logicalBox.w := 0.5;
  graphObjects[xaxisId].logicalBox.h := 0.5;

  // The Y axis itself - to be done
  yaxisId := graphObjects.Add(TGraphObject.Create(coYAxis));

  properties.Legend := TLegend.Create(coLegend);
end;

procedure TSubgraph.initializeDefaults();
begin
  XminComputedLimit := 0;
  XmaxComputedLimit := 1.0;
  YminComputedLimit := 0;
  YmaxComputedLimit := 1.0;

  XLabelNearAxis := false; { By default, x labels hug edge of graph }
  YLabelNearAxis := false; { " }
  XAxisNearEdge := True;
  YAxisNearEdge := True;
  drawXAxisLine := True; // Axis line itself
  drawYAxisLine := True;
  drawXLabelsOnAxes := True; // Labels next to the axis
  drawYLabelsOnAxes := True;

  MajorTickLengthInSkiaUnits := 7;
  MinorTickLengthInSkiaUnits := 5;

  Magnification := DEFAULT_MAGNIFICATION; // 1.728;//2.0736;  // #$#
  MaxMag := 5.0;
  MinMag := 0.2;
  Scaling := false;
  ViewXOffSet := 0;
  ViewYOffSet := 0;

  AxisThicknessInCms := 0.02;
  // TickThicknessInCms := 0.006;

  MinDistanceBetweenLegendItemsCms := DEFAULT_MinDistanceBetweenLegendItemsCms;

  MajorTickInCms := 0.2; { 2mm }
  MinorTickInCms := 0.12;

  { Not yet used }
  XAxisLengthInCms := 10.16; { 4 inches }
  YAxisLengthInCms := 10.16; { 4 inches }

  // properties.XMajorTickColor := claBlack;
  // properties.YMajorTickColor := claBlack;

  // properties.XMinorTickColor := claBlack;
  // properties.YMinorTickColor := claBlack;

  GridLineStyle := TStrokeDash.Solid;

  XLblVertOffSet := 0.04;
  YLblHorizOffSet := 0.04;

  nXLogMinorTicks := 10;
  nYLogMinorTicks := 10;

  // properties.AutoYScaling := false;
  MajorXTicks := True;
  MinorXTicks := True;
  MajorXTicksStyle := tsIn;
  MinorXTicksStyle := tsIn;
  MajorYTicks := True;
  MinorYTicks := True;
  MajorYTicksStyle := tsIn;
  MinorYTicksStyle := tsIn;

  Magnification := DEFAULT_MAGNIFICATION;
  drawXLabelsOnAxes := True;
  drawYLabelsOnAxes := True;
end;

// Returns device coordinates
function TSubgraph.getGraphDeviceDrawingArea: TRectF;
var
  panel: TRRGraph;
  rBox: TLogicalBox;
begin
  panel := parentGraph as TRRGraph;
  rBox := getLogicalBoundingBox(graphingAreaId);

  result.left := rBox.left * panel.Width;
  result.top := panel.Height - rBox.top * panel.Height;
  result.Right := result.left + panel.Width * rBox.w;
  result.Bottom := result.top + panel.Height * rBox.h;
end;

function TSubgraph.relativeToDevice2(graphObject: TGraphBase): TBox;
var
  rBox: TLogicalBox;
  rBoxGraphingArea: TLogicalBox;
  panel: TRRGraph;
  w0, w1, w2, w3: single;
  h0, h1, h2, h3, h4, h5: single;
begin
  if graphObject.ObjType = coGraphingArea then
    begin
      result := rectToBox(getGraphDeviceDrawingArea);
      exit;
    end;

  panel := parentGraph as TRRGraph;
  rBoxGraphingArea := getLogicalBoundingBox(graphingAreaId);
  rBox := graphObject.logicalBox;

  w0 := panel.Width;
  w1 := rBoxGraphingArea.w * w0; // Width in pixels of graphing area

  w2 := w1 * rBox.left; // Width in pixels of distance of Id object to edge of graphing area
  w3 := w0 * rBoxGraphingArea.left; // Distance from edge to graphing area

  result.left := w2 + w3; // Correct
  result.w := w1 * rBox.w;

  h0 := panel.Height;
  h1 := rBoxGraphingArea.h * h0; // Height in pixels of graphing area

  h2 := h1 * rBox.top; // Height in pixels of distance of Id object to edge of graphing area
  h3 := h0 * (1 - rBoxGraphingArea.top); // Distance from edge to graphing area

  result.top := (h2 + h3);
  result.h := h1 * rBox.h;
end;

function TSubgraph.legend_relativeToDevice: TBox;
var
  rBox: TLogicalBox;
  rBoxGraphingArea: TLogicalBox;
  panel: TRRGraph;
  w0, w1, w2, w3: single;
  h0, h1, h2, h3, h4, h5: single;
begin
  panel := parentGraph as TRRGraph;
  rBoxGraphingArea := getLogicalBoundingBox(graphingAreaId);
  rBox := properties.Legend.logicalBox;

  w0 := panel.Width;
  w1 := rBoxGraphingArea.w * w0; // Width in pixels of graphing area

  w2 := w1 * rBox.left; // Width in pixels of distance of Id object to edge of graphing area
  w3 := w0 * rBoxGraphingArea.left; // Distance from edge to graphing are

  result.left := w2 + w3; // Correct
  result.w := w1 * rBox.w;

  h0 := panel.Height;
  h1 := rBoxGraphingArea.h * h0; // Height in pixels of graphing area

  h2 := h1 * rBox.top; // Height in pixels of distance of Id object to edge of graphing area
  h3 := h0 * (1 - rBoxGraphingArea.top); // Distance from edge to graphing area

  result.top := (h2 + h3);
  result.h := h1 * rBox.h;
end;

// function TSubgraph.relativeToDevice(graphObject: TGraphObject): TBox;
// var
// rBox: TLogicalBox;
// rBoxGraphingArea: TLogicalBox;
// panel: TRRGraph;
// w0, w1, w2, w3: single;
// h0, h1, h2, h3, h4, h5: single;
// begin
// if graphObject.ObjType = coGraphingArea then
// begin
// result := rectToBox(getGraphDeviceDrawingArea);
// exit;
// end;
//
// panel := parentGraph as TRRGraph;
// rBoxGraphingArea := getLogicalBoundingBox(graphingAreaId);
// rBox := graphObject.logicalBox;
//
// w0 := panel.Width;
// w1 := rBoxGraphingArea.w * w0; // Width in pixels of graphing area
//
// w2 := w1 * rBox.left; // Width in pixels of distance of Id object to edge of graphing area
// w3 := w0 * rBoxGraphingArea.left; // Distance from edge to graphing are
//
// result.left := w2 + w3; // Correct
// result.w := w1 * rBox.w;
//
// h0 := panel.Height;
// h1 := rBoxGraphingArea.h * h0; // Height in pixels of graphing area
//
// h2 := h1 * rBox.top; // Height in pixels of distance of Id object to edge of graphing area
// h3 := h0 * (1 - rBoxGraphingArea.top); // Distance from edge to graphing area
//
// result.top := (h2 + h3);
// result.h := h1 * rBox.h;
// end;

procedure TSubgraph.unselectAllObjects;
var
  i: integer;
begin
  for i := 0 to properties.graphObjects.Count - 1 do
    properties.graphObjects[i].selected := false;
end;

procedure TSubgraph.findDataLimits;
var
  c, n, i, j, k: integer;
  index: integer;
  d: double;
  str: shortstring;
  ds: TDataColumns;
  XData, YData: TDataColumn;
begin
  ds := properties.dataBlocks[0].columns;
  XmaxComputedLimit := -1E30;
  XminComputedLimit := 1E30;
  YmaxComputedLimit := -1E30;
  YminComputedLimit := 1E30;

  // X Column first
  XData := ds.find(properties.dataBlocks[0].xaxisColumn, index);
  if XData = nil then
    raise Exception.Create('X axis not specified in data block: ' + properties.dataBlocks[0].xaxisColumn);

  if length(XData.data) = 0 then
    exit;

  XmaxComputedLimit := XData.data[0];
  XminComputedLimit := XData.data[0];

  // X Column first
  for i := 0 to ds.Count - 1 do
    begin
      for j := 0 to length(ds[i].data) - 1 do
        begin
          if XminComputedLimit > XData.data[j] then
            XminComputedLimit := XData.data[j];
          if XmaxComputedLimit < XData.data[j] then
            XmaxComputedLimit := XData.data[j];
        end;
    end;

  if XminComputedLimit = XmaxComputedLimit then { Just in case }
    begin
      if XminComputedLimit = 0 then
        XmaxComputedLimit := 1.0
      else if XmaxComputedLimit > 0 then
        XminComputedLimit := 0;
      if XminComputedLimit < 0 then
        XmaxComputedLimit := 0;
    end;

  // Y Columns next
  for i := 0 to ds.Count - 1 do
    begin
      if ds[i].name <> properties.dataBlocks[0].xaxisColumn then
        begin
          for j := 0 to length(ds[i].data) - 1 do
            begin
              if YminComputedLimit > ds[i].data[j] then
                YminComputedLimit := ds[i].data[j];
              if YmaxComputedLimit < ds[i].data[j] then
                YmaxComputedLimit := ds[i].data[j];
            end;
        end;
    end;

  if YminComputedLimit = YmaxComputedLimit then { Just in case }
    begin
      if YminComputedLimit = 0 then
        YmaxComputedLimit := 1.0
      else if YmaxComputedLimit > 0 then
        YminComputedLimit := 0;
      if YminComputedLimit < 0 then
        YmaxComputedLimit := 0;
    end;
end;

procedure TSubgraph.autoScaleXAxis(HowtoScale: THowtoScale);
begin
  // Assume no adjustment to begin with
  AutoScale_Xmin := XminComputedLimit;
  AutoScale_Xmax := XmaxComputedLimit;

  if (HowtoScale = hLowerLimit) or (HowtoScale = hBothLimits) then
    begin
      if properties.LogXAxis then
        begin
          { Error if Xmin < 0 }
          if XminComputedLimit > 0 then
            AutoScale_Xmin := RoundDown(XminComputedLimit)
          else
            showmessage('Error!, No negative X values in Log Axes mode');
        end
      else
        begin
          if XminComputedLimit > 1 then
            AutoScale_Xmin := RoundDownUnits(XminComputedLimit);
          if (XminComputedLimit <= 1) and (XminComputedLimit > 0) then
            AutoScale_Xmin := 0;
          if XminComputedLimit < 0 then
            AutoScale_Xmin := -RoundUp(abs(XminComputedLimit));
        end;
    end;

  if (HowtoScale = hUpperLimit) or (HowtoScale = hBothLimits) then
    begin
      if XmaxComputedLimit > 1 then
        AutoScale_Xmax := RoundUp(XmaxComputedLimit);
      if (XmaxComputedLimit <= 1) and (XmaxComputedLimit > 0) then
        AutoScale_Xmax := RoundUp(XmaxComputedLimit);
      if XmaxComputedLimit < 0 then
        AutoScale_Xmax := -RoundDown(abs(XmaxComputedLimit));
    end;
end;

procedure TSubgraph.autoScaleYAxis(HowtoScale: THowtoScale);
begin
  // Assume no adjustment to begin with
  AutoScale_Ymin := YminComputedLimit;
  AutoScale_Ymax := YmaxComputedLimit;

  if (HowtoScale = hLowerLimit) or (HowtoScale = hBothLimits) then
    begin
      if properties.LogYAxis then
        begin
          { Error if Ymin < 0 }
          if YminComputedLimit > 0 then
            AutoScale_Ymin := RoundDown(YminComputedLimit)
          else
            showmessage('Error!, No negative Y values in Log Axes mode');
        end
      else
        begin
          if YminComputedLimit > 1 then
            AutoScale_Ymin := RoundDownUnits(YminComputedLimit);
          if (YminComputedLimit <= 1) and (YminComputedLimit > 0) then
            AutoScale_Ymin := 0;
          if YminComputedLimit < 0 then
            AutoScale_Ymin := -RoundUp(abs(YminComputedLimit));
        end;
    end;

  if (HowtoScale = hUpperLimit) or (HowtoScale = hBothLimits) then
    begin
      if YmaxComputedLimit > 1 then
        AutoScale_Ymax := RoundUp(YmaxComputedLimit);
      if (YmaxComputedLimit <= 1) and (YmaxComputedLimit > 0) then
        AutoScale_Ymax := RoundUp(YmaxComputedLimit);
      if YmaxComputedLimit < 0 then
        AutoScale_Ymax := -RoundDown(abs(YmaxComputedLimit));
    end;
end;

procedure TSubgraph.setAxesLimits;
begin
  { Scale axes according to user selection }
  if properties.AutoXScaling and (properties.dataBlocks.Count <> 0) then
    begin
      autoScaleXAxis(hBothLimits);
      properties.FWorldXmin := AutoScale_Xmin;
      properties.FWorldXmax := AutoScale_Xmax;
    end
  else
    begin
      properties.FWorldXmin := properties.UserScale_Xmin;
      properties.FWorldXmax := properties.UserScale_Xmax;
    end;

  if properties.AutoYScaling and (properties.dataBlocks.Count <> 0) then
    begin
      autoScaleYAxis(hBothLimits);
      properties.FWorldYmin := AutoScale_Ymin;
      properties.FWorldYmax := AutoScale_Ymax;
    end
  else
    begin
      properties.FWorldYmin := properties.UserScale_Ymin;
      properties.FWorldYmax := properties.UserScale_Ymax;
    end;
end;

{ This procedure decides where the axes will be positioned relative to each other
  Only called when the World coordinates have been assigned }
procedure TSubgraph.determineOrigin;
begin
  XOrigin := properties.FWorldXmin;
  if properties.FWorldXmin = 0 then
    XOrigin := 0.0;
  { If the x scale is all negative, position the origin on the left side
    not the right side as one might expect, might look odd to the user otherwise }
  { This should be configurable }
  { if AutoScale_Xmax <= 0 then X_Origin := AutoScale_Xmin; }
  if (properties.FWorldXmin < 0) and (properties.FWorldXmax > 0) then
    XOrigin := 0.0;

  YOrigin := properties.FWorldYmin;
  if properties.FWorldYmin = 0 then
    YOrigin := 0.0;
  { if AutoScale_Ymax <= 0 then Y_Origin := AutoScale_Ymin; }
  if (properties.FWorldYmin < 0) and (properties.FWorldYmax > 0) then
    YOrigin := 0.0;
end;

function TSubgraph.IsOnXAxisTitle(ACanvas: ISkCanvas; x, y: single): boolean;
var
  box: TBox;
begin
  result := false;
  box := properties.XAxisTitleObject.textProperties.box;
  if (x > box.left) and (x < box.left + box.w) then
    if (y > box.top) and (y < box.top + box.h) then
      result := True;
end;

function TSubgraph.IsOnXAxis(ACanvas: ISkCanvas; x, y: single): boolean;
var
  box: TBox;
begin
  result := false;
  // box := graphObjects[xaxisId].box;
  if (x > box.left) and (x < box.left + box.w) then
    if (y > box.top) and (y < box.top + box.h) then
      result := True;
end;

function TSubgraph.IsOnYAxisTitle(ACanvas: ISkCanvas; x, y: single): boolean;
var
  box: TBox;
begin
  result := false;
  box := properties.YAxisTitleObject.textProperties.box;
  if (x > box.left) and (x < box.left + box.h) then  // h and w are swapped because the y axis
    if (y > box.top) and (y < box.top + box.w) then  // title is positioned vertically
      result := True;
end;

function TSubgraph.IsOnLegend(ACanvas: ISkCanvas; x, y: single): boolean;
var
  rBox: TLogicalBox;
  box: TBox;
  panel: TRRGraph;
  rBoxGraphingArea: TLogicalBox;
  w0, w1, w2, w3: single;
  h0, h1, h2, h3, h4, h5: single;
begin
  result := false;
  // rbox := getLogicalBoundingBox (legendId);
  // box := relativeToDevice (properties.legend as TGraphObject);

  panel := parentGraph as TRRGraph;
  rBoxGraphingArea := getLogicalBoundingBox(graphingAreaId);
  rBox := properties.Legend.logicalBox;

  w0 := panel.Width;
  w1 := rBoxGraphingArea.w * w0; // Width in pixels of graphing area

  w2 := w1 * rBox.left; // Width in pixels of distance of Id object to edge of graphing area
  w3 := w0 * rBoxGraphingArea.left; // Distance from edge to graphing are

  box.left := w2 + w3; // Correct
  box.w := w1 * rBox.w;

  h0 := panel.Height;
  h1 := rBoxGraphingArea.h * h0; // Height in pixels of graphing area

  h2 := h1 * rBox.top; // Height in pixels of distance of Id object to edge of graphing area
  h3 := h0 * (1 - rBoxGraphingArea.top); // Distance from edge to graphing area

  box.top := (h2 + h3);
  box.h := h1 * rBox.h;

  if (x > box.left) and (x < box.left + box.w) then
    if (y > box.top) and (y < box.top + box.h) then
      result := True;
end;

function TSubgraph.IsOnMainTitle(ACanvas: ISkCanvas; x, y: single): boolean;
var
  box: TBox;
begin
  result := false;
  box := properties.MainTitleObject.textProperties.box;
  if (x > box.left) and (x < box.left + box.w) then
    if (y > box.top) and (y < box.top + box.h) then
      result := True;
end;

// Returns the graph object that the mouse is currently hovering over
function TSubgraph.onSubGraph(ACanvas: ISkCanvas; x, y: single; var graphObject: TGraphBase): boolean;
var
  rectf: TRectF;
begin
  rectf := getGraphDeviceDrawingArea;
  graphObject := nil;

  result := false;

  if IsOnXAxisTitle(ACanvas, x, y) then
    begin
      graphObject := properties.XAxisTitleObject;
      exit(True);
    end;

  if IsOnMainTitle(ACanvas, x, y) then
    begin
      graphObject := properties.MainTitleObject;
      exit(True);
    end;

  if IsOnYAxisTitle(ACanvas, x, y) then
    begin
      result := True;
      graphObject := properties.YAxisTitleObject;
      exit (True);
    end;

  if IsOnLegend(ACanvas, x, y) then
    begin
      graphObject := properties.Legend;
      exit(True);
    end;

  if PtInRect(rectf, pointf(x, y)) then
    begin
      graphObject := properties.graphObjects[graphingAreaId];
      exit(True);
    end;

end;

// Converts a size in cms to pixels units according to the current device
function TSubgraph.computePhysicalSize(sizeCm: double): single;
begin
  result := Magnification * sizeCm / CmsInOneInch * CurrentXPixelsPerInch;
  if (result < 1) and (result > 0.1) then
    result := 0; // avoid antialiasing artifacts
end;

procedure TSubgraph.computeScalingFactors(rect: TRectF);
var
  dx, dy, sc: double;
  vxmin, vxmax, vymin, vymax: single;
begin
  // Dimensions of the graphing area in device corrdinates (pixels)
  vxmin := rect.left;
  vxmax := rect.Right;
  vymin := rect.top;
  vymax := rect.Bottom;

  if properties.LogXAxis then
    dx := ln(properties.FWorldXmax) - ln(properties.FWorldXmin)
  else
    dx := properties.FWorldXmax - properties.FWorldXmin;

  xscale := (vxmax - vxmin) / dx;
  if properties.LogXAxis then
    xfactor := (ln(properties.FWorldXmax) * vxmin - ln(properties.FWorldXmin) * vxmax) / dx
  else
    xfactor := (properties.FWorldXmax * vxmin - properties.FWorldXmin * vxmax) / dx;

  if properties.LogYAxis then
    dy := ln(properties.FWorldYmax) - ln(properties.FWorldYmin)
  else
    dy := properties.FWorldYmax - properties.FWorldYmin;

  yscale := (vymax - vymin) / dy;
  fontScalingFactor := (parentGraph as TRRGraph).Height / (vymax - vymin);

  if properties.LogYAxis then
    yfactor := (ln(properties.FWorldYmax) * vymin - ln(properties.FWorldYmin) * vymax) / dy
  else
    yfactor := (properties.FWorldYmax * vymin - properties.FWorldYmin * vymax) / dy;
end;

// Convert world coordinates to physical coordinates
function TSubgraph.fx(wx: double): double;
var
  w: double;
begin
  if properties.LogXAxis then
    w := Magnification * (xscale * ln(wx) + xfactor)
  else
    w := Magnification * (xscale * wx + xfactor);
  if abs(w) > 20000 then
    begin
      if w > 0 then
        result := 20000
      else
        result := -20000;
    end
  else
    result := w;
end;

// Convert world coordinates to physical coordinates, note reversal of y axis
function TSubgraph.fy(wy: double): double;
var
  deviceYMin, deviceYMax: double;
  w: double;
  heightOfGraph: double;
  panel: TRRGraph;
begin
  panel := parentGraph as TRRGraph;

  deviceYMax := panel.Height - properties.graphObjects[graphingAreaId].logicalBox.top * panel.Height;
  heightOfGraph := properties.graphObjects[graphingAreaId].logicalBox.h * panel.Height;

  deviceYMin := deviceYMax + heightOfGraph;
  if properties.LogYAxis then
    w := Magnification * ((deviceYMax + deviceYMin) - (yscale * ln(wy) + yfactor))
  else
    w := Magnification * ((deviceYMax + deviceYMin) - (yscale * wy + yfactor));
  if abs(w) > 20000 then
    begin
      if w > 0 then
        result := 20000
      else
        result := -20000;
    end
  else
    result := w;
end;

function TSubgraph.deltaDeviceToDeltaWorld(x, y: single): TPointF;
var
  p1, p2: TPointF;
begin
  p1 := deviceToWorld(0, 0);
  p2 := deviceToWorld(x, y);
  result.x := abs(p2.x - p1.x);
  result.y := abs(p2.y - p1.y);
end;

// Convert a device oordinate to a world coordinate
function TSubgraph.deviceToWorld(x, y: single): TPointF;
var
  deviceYMin, deviceYMax: single;
  heightOfGraph: single;
  panel: TRRGraph;
begin
  panel := parentGraph as TRRGraph;

  x := (x / Magnification);
  y := (y / Magnification);

  deviceYMax := panel.Height - properties.graphObjects[graphingAreaId].logicalBox.top * panel.Height;
  heightOfGraph := properties.graphObjects[graphingAreaId].logicalBox.h * panel.Height;
  deviceYMin := deviceYMax + heightOfGraph;

  if properties.LogXAxis then
    result.x := exp((x - xfactor) / xscale)
  else
    result.x := (x - xfactor) / xscale;

  if properties.LogYAxis then
    result.y := exp(((deviceYMax + deviceYMin) - y - yfactor) / yscale)
  else
    result.y := ((deviceYMax + deviceYMin) - yfactor - y) / yscale;
end;

procedure TSubgraph.drawYAxisTitle(ACanvas: ISkCanvas);
var
  x, y: double;
  LBlob: ISkTextBlob;
  ABounds: TRectF;
  LPathBuilder: ISkPathBuilder;
  LDest: TPointF;
  twidth: single;
  horizDisplacement: integer;

  textProperties: TTextType;
  r: TRectF;
  pt: TPointF;
  box: TBox;
  gdArea: TRectF;
  gdHeight: single;
begin
  textProperties := properties.YAxisTitleObject.textProperties;
  if textProperties.value = '' then
    exit;

  twidth := textProperties.computeDimensions(LPaint).x;

  // Update the logical dimensions based on the current Y axis text
  gdArea := getGraphDeviceDrawingArea;
  gdHeight := gdArea.Bottom - gdArea.top;
  properties.YAxisTitleObject.logicalBox.top := ((gdHeight / 2) - twidth / 2) / gdHeight;

  box := relativeToDevice2(properties.YAxisTitleObject);

  horizDisplacement := 16;

  x := box.left;
  y := box.top;

  textProperties.box.left := x - horizDisplacement;
  textProperties.box.top := y - pt.y; // pt.y is the height of the text

  LPaint.color := textProperties.fontColor;
  LPaint.Style := TSkPaintStyle.Fill;

  LDest.x := x - horizDisplacement;
  LDest.y := y;
  LBlob := TSkTextBlob.MakeFromText(textProperties.value, textProperties.font);
  ACanvas.Save;
  try
    ACanvas.Rotate(90);
    LDest := LDest * TMatrix.CreateRotation(DegToRad(-90));
    ACanvas.DrawSimpleText(textProperties.value, LDest.x, LDest.y, textProperties.font, LPaint);
  finally
    ACanvas.Restore;
  end;
end;

procedure TSubgraph.paint(ACanvas: ISkCanvas);
var
  box: TBox;
  panel: TRRGraph;
  p: TPointF;
  textProperties: TTextType;
  twidth: single;
  x, y: double;
  LBlob: ISkTextBlob;
  font: ISkFont;
  typeface: ISkTypeface;
  r: TRectF;
  pt: TPointF;
  ds: TDataColumns;
  index: integer;
  gdArea: TRectF;
  gdWidth: single;
begin
  panel := parentGraph as TRRGraph;

  ds := properties.dataBlocks[0].columns;
  // If there is nothing to plot then don't try to find the data limits
  if ds.find(properties.dataBlocks[0].xaxisColumn, index) <> nil then
    if properties.AutoXScaling or properties.AutoYScaling then
      findDataLimits;

  setAxesLimits;
  determineOrigin;
  computeScalingFactors(getGraphDeviceDrawingArea());

  r := getGraphDeviceDrawingArea;

  // MajorTickLength := computePhysicalSize(MajorTickInCms);
  // MinorTickLength := computePhysicalSize(MinorTickInCms);
  XAxisLengthInPixels := computePhysicalSize(XAxisLengthInCms);
  YAxisLengthInPixels := computePhysicalSize(YAxisLengthInCms);

  box := relativeToDevice2(properties.graphObjects[graphingAreaId]);
  LPaint.color := properties.GraphBackgroundColor;
  LPaint.Style := TSkPaintStyle.Fill;
  ACanvas.DrawRect(TRectF.Create(box.left, box.top, box.left + box.w, box.top + box.h), LPaint);

  if properties.bDrawMainTitle then
    begin
      textProperties := properties.MainTitleObject.textProperties; // graphObjects[mainTitleId].textProperties;
      if textProperties.value <> '' then
        begin
          LPaint.color := textProperties.fontColor;
          LPaint.Style := TSkPaintStyle.Fill;

          pt := textProperties.computeDimensions(LPaint);

          box := relativeToDevice2(properties.MainTitleObject);

          x := box.left;
          y := box.top;
          LBlob := TSkTextBlob.MakeFromText(textProperties.value, textProperties.font);
          ACanvas.DrawTextBlob(LBlob, x, y, LPaint);
          textProperties.box.left := x;
          textProperties.box.top := y - pt.y; // pt.y is the height of the text
        end;
      // if SelectedObjectType = coMainTitle then
      // drawMainTitleSelected(canvas);
    end;

  if properties.bDrawXAxisTitle then
    begin
      textProperties := properties.XAxisTitleObject.textProperties;
      if textProperties.value <> '' then
        begin
          LPaint.color := textProperties.fontColor;
          LPaint.Style := TSkPaintStyle.Fill;

          twidth := textProperties.computeDimensions(LPaint).x;

          // Update the logical dimensions based on the current X axis text
          // Doing this means a user can't move the text left or right, only up and down.
          gdArea := getGraphDeviceDrawingArea;
          gdWidth := gdArea.Right - gdArea.left;
          properties.XAxisTitleObject.logicalBox.left := ((gdWidth / 2) - twidth / 2) / gdWidth;
          box := relativeToDevice2(properties.XAxisTitleObject);

          x := box.left;
          y := box.top;

          LBlob := TSkTextBlob.MakeFromText(textProperties.value, textProperties.font);
          ACanvas.DrawTextBlob(LBlob, x, y, LPaint);

          textProperties.box.left := x;
          textProperties.box.top := y - pt.y; // pt.y is the height of the text
        end;

      // if SelectedObjectType = coXAxisTitle then
      // drawXAxisTitleSelected(ACanvas);
    end;

  if properties.bDrawYAxisTitle then
    begin
      drawYAxisTitle(ACanvas);

      // //if SelectedObject = coXAxisTitle then
      // //   drawXAxisTitleSelected(canvas);
    end;

  if properties.LogYAxis then
    drawYAxisLogTicks(ACanvas)
  else
    drawYAxisTicks(ACanvas);

  if properties.LogXAxis then
    drawXAxisLogTicks(ACanvas)
  else
    drawXAxisTicks(ACanvas);

  if FSubGraphProperties.XGridLines then
    drawAllMajorXGridLines(ACanvas);

  if FSubGraphProperties.YGridLines then
    drawAllMajorYGridLines(ACanvas);

  if not properties.graphObjects[graphingAreaId].selected then
    begin
      drawAxesLines(ACanvas, True, True);

      if FSubGraphProperties.GraphBorder then
        drawGraphBorder(ACanvas);
    end
  else
    drawSelectedGraphingArea(ACanvas);

  ACanvas.Save;
  try
    ACanvas.ClipRect(r);

    plotLines(ACanvas);
    plotSymbols(ACanvas);
  finally
    ACanvas.Restore;
  end;
  drawLegend(ACanvas);
end;

// Returns screen coordinates.
function TSubgraph.getTextBoundingBox(ACanvas: ISkCanvas; textDetails: TTextType): TBox;
var
  go: TGraphObject;
  panel: TRRGraph;
begin
  panel := parentGraph as TRRGraph;

  // HMS
  // go := properties.graphObjects[mainTitleId];
  // canvas.Font.Size := textDetails.size;
  // canvas.Font.Style := textDetails.style;
  // canvas.Font.Family := textDetails.fontName;
  //
  result.left := round((rwidth / 2 + rxmin) * panel.Width);
  result.top := panel.Height - round((rymin + go.logicalBox.top) * panel.Height);
end;

function TSubgraph.getYAxisTitleBoundingBox(ACanvas: ISkCanvas; textDetails: TTextType): TBox;
begin
  // HMS
  // canvas.Font.Size := textDetails.size;
  // canvas.Font.Style := textDetails.style;
  // canvas.Font.Family := textDetails.fontName;
  //
  // result := getTextBoundingBox(ACanvas, properties.graphObjects[yaxisTitleId].textProperties);
  // result.left :=  fx (properties.graphObjects[yaxisTitleId].rbox.left);
  // result.top := fy (properties.graphObjects[yaxisTitleId].rbox.top);
end;

procedure TSubgraph.setXAxisTitle(str: string);
begin
  properties.XAxisTitleObject.textProperties.value := str;
end;

function TSubgraph.getXAxisTitle: string;
begin
  result := properties.XAxisTitleObject.textProperties.value;
end;

procedure TSubgraph.setYAxisTitle(str: string);
begin
  properties.YAxisTitleObject.textProperties.value := str;
end;

function TSubgraph.getYAxisTitle: string;
begin
  result := properties.YAxisTitleObject.textProperties.value;
end;

procedure TSubgraph.setGraphBorder(value: boolean);
begin
  properties.GraphBorder := value;
  (parentGraph as TRRGraph).redraw;
end;

function TSubgraph.getGraphBorder: boolean;
begin
  result := properties.GraphBorder;
end;

procedure TSubgraph.setId(str: string);
begin
  FId := str;
end;

function TSubgraph.getDataBlocks: TDataBlocks;
begin
  result := properties.dataBlocks;
end;

procedure TSubgraph.setMainTitle(str: string);
begin
  properties.MainTitleObject.textProperties.value := str;
end;

function TSubgraph.getMainTitle: string;
begin
  result := properties.MainTitleObject.textProperties.value;
end;

procedure TSubgraph.drawMainTitleSelected(ACanvas: ISkCanvas);
var
  box: TBox;
  r: TRectF;
begin
  box := properties.MainTitleObject.textProperties.box;
  r := rectf(box.left, box.top, box.left + box.w, box.top + box.h);
  InflateRect(r, -0.5, -0.5);
  ACanvas.DrawRect(r, LPaint);
end;

procedure TSubgraph.drawXAxisTitleSelected(ACanvas: ISkCanvas);
var
  box: TBox;
begin
  // box := getCenteredTextBoundingBox (ACanvas, xaxisTitleId);
  // HMS canvas.DrawFocusRect (rect (box.left, box.top, box.left + box.w, box.top + box.h));
end;

procedure TSubgraph.drawYAxisTitleSelected(ACanvas: ISkCanvas);
var
  box: TBox;
begin
  // box := getYAxisTitleBoundingBox (ACanvas, properties.graphObjects[yaxisTitleId].textProperties);
  // HMS canvas.DrawFocusRect (rect (box.left, box.top, box.left + box.w, box.top + box.h));
end;

{ Plot symbols for all data sets }
procedure TSubgraph.plotSymbols(ACanvas: ISkCanvas);
var
  i, j, k, n, nXColumns, nYColumns: integer;
  x, y, deltaX, deltaY: double;
  columns: TDataColumns;
  xaxisColumn: string;
  XData, YData: TDataColumn;
  index: integer;
begin
  columns := properties.dataBlocks[0].columns;

  if columns = nil then
    exit;

  if columns.Count = 0 then
    exit;

  if properties.LogXAxis then
    deltaX := 0.0
  else
    deltaX := 0.005 * (properties.FWorldXmax - properties.FWorldXmin);

  if properties.LogYAxis then
    deltaY := 0.0
  else
    deltaY := 0.005 * (properties.FWorldYmax - properties.FWorldYmin);

  xaxisColumn := properties.dataBlocks[0].xaxisColumn;
  XData := columns.find(xaxisColumn, index);
  if XData = nil then
    begin
      XData := columns[0];
      xaxisColumn := XData.name;
    end;

  if length(XData.data) = 0 then
    exit;

  for i := 0 to columns.Count - 1 do
    begin
      if columns[i].name <> xaxisColumn then
        begin
          YData := columns[i];

          for k := 0 to length(columns[i].data) - 1 do
            begin
              if YData.symbol.visible then
                begin
                  x := XData.data[k];
                  y := YData.data[k];
                  if (x >= (properties.FWorldXmin - deltaX)) and (x <= (properties.FWorldXmax + deltaX)) and
                    (y >= (properties.FWorldYmin - deltaY)) and (y <= (properties.FWorldYmax + deltaY)) then
                    drawSymbol(ACanvas, x, y, columns[i].XErrors[k], columns[i].YErrors[k], YData.symbol);
                end;
            end;
        end;
    end;
end;

procedure TSubgraph.drawXErrorBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dx: single; Style: TErrorBarStyle;
  CapStyle: TErrorBarCapStyle);
var
  endpoint, cap: single;
begin
  // canvas.Stroke.Color := symbol.errorBarColor;
  // canvas.Stroke.Thickness := computePhysicalSize (symbol.errorBarLengthInCms);
  // cap := computePhysicalSize (symbol.errorBarCapWidthInCms);
  //
  // if (Style = ErrBarDown) or (Style = ErrBarBoth) then
  // begin
  // endpoint := wx+dx;
  // canvas.DrawLine (pointf (wx, wy), pointf (endpoint, wy), 1);
  // if CapStyle = WithCap then
  // begin
  // begin
  // canvas.DrawLine (pointf (wx+dx, wy), pointf (wx+dx, wy-cap), 1);
  // canvas.DrawLine (pointf (wx+dx, wy), pointf (wx+dx, wy+cap), 1);
  // end;
  // end;
  // end;
  //
  // if (Style = ErrBarUp) or (Style = ErrBarBoth) then
  // begin
  // endpoint := wx-dx;
  // canvas.DrawLine (pointf (wx, wy), pointf (endpoint, wy), 1);
  // if CapStyle = WithCap then
  // begin
  // begin
  // canvas.DrawLine (pointf (wx-dx, wy), pointf (wx-dx, wy-cap), 1);
  // canvas.DrawLine (pointf (wx-dx, wy), pointf (wx-dx, wy+cap), 1);
  // end;
  // end;
  // end;
end;

procedure TSubgraph.drawXErrorHalfBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dx: single; Style: TErrorBarStyle;
  CapStyle: TErrorBarCapStyle);
var
  endpoint, cap: single;
begin
  // canvas.Stroke.Color := symbol.errorBarColor;
  // canvas.Stroke.Thickness := computePhysicalSize (symbol.errorBarLengthInCms);
  // cap := computePhysicalSize (symbol.errorBarCapWidthInCms);
  //
  // if (Style = ErrBarDown) or (Style = ErrBarBoth) then
  // begin
  // endpoint := wx+dx;
  // canvas.DrawLine (pointf (wx, wy), pointf (endpoint, wy), 1);
  // if CapStyle = WithCap then
  // begin
  // begin
  // canvas.DrawLine (pointf (wx+dx, wy), pointf (wx+dx, wy-cap), 1);
  // canvas.DrawLine (pointf (wx+dx, wy), pointf (wx+dx, wy+cap), 1);
  // end;
  // end;
  // end;
end;

procedure TSubgraph.drawYErrorBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dy: single; Style: TErrorBarStyle;
  CapStyle: TErrorBarCapStyle);
var
  endpoint, cap: single;
begin
  // HMS
  // canvas.Stroke.Color := symbol.errorBarColor;
  // canvas.Stroke.Thickness := computePhysicalSize (symbol.errorBarLengthInCms);
  // cap := computePhysicalSize (symbol.errorBarCapWidthInCms);
  // if (Style = ErrBarUp) or (Style = ErrBarBoth) then
  // begin
  // endpoint := wy+dy;
  // //if wy+dy <= arect.top then begin clip := true; endpoint := arect.top; end;
  // canvas.DrawLine (pointf (wx, wy), pointf (wx, endpoint), 1);
  // if CapStyle = WithCap then
  // begin
  // //if not clip then
  // begin
  // canvas.DrawLine (pointf (wx, wy+dy), pointf (wx-cap, wy+dy), 1);
  // canvas.DrawLine (pointf (wx, wy+dy), pointf (wx+cap, wy+dy), 1);
  // end;
  // end;
  // end;
  //
  // //clip := false;
  // if (Style = ErrBarDown) or (Style = ErrBarBoth) then
  // begin
  // endpoint := wy-dy;
  // //if wy-dy >= arect.bottom then begin clip := true; endpoint := arect.bottom; end;
  // canvas.DrawLine (pointf (wx, wy), pointf (wx, endpoint), 1);
  // if CapStyle = WithCap then
  // begin
  // //if not clip then
  // begin
  // canvas.DrawLine (pointf (wx, wy-dy), pointf (wx-cap, wy-dy), 1);
  // canvas.DrawLine (pointf (wx, wy-dy), pointf (wx+cap, wy-dy), 1);
  // end;
  // end;
  // end;
end;

procedure TSubgraph.drawYErrorHalfBar(ACanvas: ISkCanvas; symbol: TSymbol; wx, wy, dy: single; Style: TErrorBarStyle;
  CapStyle: TErrorBarCapStyle);
var
  endpoint, cap: single;
begin
  // HMS
  // canvas.Stroke.Color := symbol.errorBarColor;
  // canvas.Stroke.Thickness := computePhysicalSize (symbol.errorBarLengthInCms);
  // cap := computePhysicalSize (symbol.errorBarCapWidthInCms);
  // if (Style = ErrBarUp) or (Style = ErrBarBoth) then
  // begin
  // endpoint := wy+dy;
  // //if wy+dy <= arect.top then begin clip := true; endpoint := arect.top; end;
  // canvas.DrawLine (pointf (wx, wy), pointf (wx, endpoint), 1);
  // if CapStyle = WithCap then
  // begin
  // //if not clip then
  // begin
  // canvas.DrawLine (pointf (wx, wy+dy), pointf (wx-cap, wy+dy), 1);
  // canvas.DrawLine (pointf (wx, wy+dy), pointf (wx+cap, wy+dy), 1);
  // end;
  // end;
  // end;
end;

procedure TSubgraph.drawSymbol(ACanvas: ISkCanvas; x, y: double; xerr, yerr: TErrorDatum; symbol: TSymbol);
var
  wx, wy, CapPixelSize: single;
  arect: TRectF;
  x1clipped, y1clipped, x2clipped, y2clipped: integer;
  oldWidth: integer;
begin
  wx := fx(x);
  wy := fy(y);
  if (xerr.errorType <> etNone) and symbol.errorBarVisible then
    case xerr.errorType of
      etSymmetric:
        begin
          drawXErrorHalfBar(ACanvas, symbol, wx, wy, fx(x + xerr.value) - wx, ErrBarBoth, WithCap);
          drawXErrorHalfBar(ACanvas, symbol, wx, wy, fx(x - xerr.value) - wx, ErrBarBoth, WithCap);
        end;
      etASymmetric:
        begin
          if xerr.lower <> 0 then
            drawXErrorHalfBar(ACanvas, symbol, wx, wy, fx(x - xerr.lower) - wx, ErrBarBoth, WithCap);
          if xerr.upper <> 0 then
            drawXErrorHalfBar(ACanvas, symbol, wx, wy, fx(x + xerr.upper) - wx, ErrBarBoth, WithCap);
        end;
      // etOneSided:
      // DrawXErrorBar (canvas, symbol, wx, wy, fx (x+xerr.value) - wx, ErrBarBoth, WithCap);
    end;

  if (yerr.errorType <> etNone) and symbol.errorBarVisible then
    case yerr.errorType of
      etSymmetric:
        begin
          drawYErrorHalfBar(ACanvas, symbol, wx, wy, fy(y + yerr.value) - wy, ErrBarBoth, WithCap);
          drawYErrorHalfBar(ACanvas, symbol, wx, wy, fy(y - yerr.value) - wy, ErrBarBoth, WithCap);
        end;
      etASymmetric:
        begin
          if yerr.upper <> 0 then
            drawYErrorHalfBar(ACanvas, symbol, wx, wy, fy(y + yerr.upper) - wy, ErrBarBoth, WithCap);
          if yerr.lower <> 0 then
            drawYErrorHalfBar(ACanvas, symbol, wx, wy, fy(y - yerr.lower) - wy, ErrBarBoth, WithCap);
        end;
      // etOneSided:
      // DrawYErrorBar (canvas, symbol, wx, wy, fy (y+yerr.value) - wy, ErrBarBoth, WithCap);
    end;

  drawSymbolAtDeviceCoords(ACanvas, wx, wy, symbol);
end;

// Plot lines for all data sets
procedure TSubgraph.plotLines(ACanvas: ISkCanvas);
var
  i, j, k, nYColumns: integer;
  x1, x2, y1, y2: double;
  arect: TRect;
  p: TPolygon;
  Save: TCanvasSaveState;
  poly: TPolygon;
  ds: TDataBlock;
  rectf: TRectF;
  rowCounter: integer;
  thickness: single;
  XData, YData: TDataColumn;
  xaxisColumn: string;
  index: integer;
  pt1, pt2: TPointF;
  PathEffect: ISkPathEffect;
begin
  ds := properties.dataBlocks[0];
  if ds = nil then
    exit;

  xaxisColumn := properties.dataBlocks[0].xaxisColumn;
  XData := ds.columns.find(xaxisColumn, index);
  if XData = nil then
    exit;

  if length(XData.data) = 0 then
    exit;

  LPaint.PathEffect := nil;
  rectf := getGraphDeviceDrawingArea;
  ACanvas.Save;
  try
    ACanvas.ClipRect(getGraphDeviceDrawingArea());
    for i := 0 to properties.dataBlocks.Count - 1 do
      begin
        if properties.dataBlocks[i].columns.Count = 0 then
          exit;

        xaxisColumn := properties.dataBlocks[i].xaxisColumn;
        XData := ds.columns.find(xaxisColumn, index);
        // Default x axis to the first column if none if defined.
        if XData = nil then
          begin
            XData := ds.columns[0];
            xaxisColumn := XData.name;
          end;
        for j := 0 to ds.columns.Count - 1 do
          begin
            if ds.columns[j].lineDetails.visible then
              begin
                if ds.columns[j].name <> xaxisColumn then
                  begin
                    YData := ds.columns[j];

                    thickness := YData.lineDetails.ThicknessInSkiaUnits;
                    LPaint.color := YData.lineDetails.color;
                    LPaint.StrokeWidth := thickness;
                    LPaint.StrokeCap := TSkStrokeCap.round;
                    x1 := XData.data[0];
                    y1 := YData.data[0];

                    for k := 1 to length(XData.data) - 1 do
                      begin
                        x2 := XData.data[k];
                        y2 := YData.data[k];

                        pt1.x := fx(x1);
                        pt1.y := fy(y1);
                        pt2.x := fx(x2);
                        pt2.y := fy(y2);

                        case YData.lineDetails.Style of
                          lsSolid:
                            begin
                              ACanvas.drawLine(pt1, pt2, LPaint);
                            end;
                          lsDash:
                            begin
                              PathEffect := TSkPathEffect.MakeDash([15, 5, 15, 5], 1);
                              LPaint.PathEffect := PathEffect;
                              ACanvas.drawLine(pt1, pt2, LPaint);
                            end;
                          lsDot:
                            begin
                              PathEffect := TSkPathEffect.MakeDash([2, 2, 2, 2], 1);
                              LPaint.PathEffect := PathEffect;
                              ACanvas.drawLine(pt1, pt2, LPaint);
                            end
                        else
                          ACanvas.drawLine(pt1, pt2, LPaint);
                        end;
                        x1 := x2;
                        y1 := y2;
                      end;
                  end;
              end;
          end;
      end;
  finally
    ACanvas.Restore();
  end;
end;

procedure TSubgraph.drawLine(ACanvas: ISkCanvas; x1, y1, x2, y2: single; lineDetails: TlineDetails);
begin
  // if CurrentlyPrinting then
  // begin
  // canvas.moveto (x1, y1); canvas.lineto (x2, y2);
  // end
  // else
  begin
    LPaint.color := lineDetails.color;
    LPaint.StrokeWidth := lineDetails.ThicknessInSkiaUnits;
    // Make suree like joins are smooth
    LPaint.StrokeCap := TSkStrokeCap.round;
    // HMS
    // canvas.Stroke.Dash := lineDetails.Style;
    try
      ACanvas.drawLine(pointf(x1, y1), pointf(x2, y2), LPaint);
    finally
    end;
  end;
end;

procedure TSubgraph.drawPolygon(ACanvas: ISkCanvas; Points: TPolygon; symbol: TSymbol);
begin
  // canvas.Stroke.Color := symbol.outlineColor;
  // canvas.Stroke.Thickness := computePhysicalSize (symbol.outlineInCms);
  //
  // canvas.DrawPolygon(Points, 1.0);
  // case symbol.gradientType of
  // gtNone :
  // begin
  // canvas.Fill.Color := symbol.fillColor;
  // canvas.FillPolygon(POints, 1.0);
  // end;
  // gtVertLinear :
  // begin
  // Canvas.Fill.Gradient.Color := symbol.fillColorStart;
  // Canvas.Fill.Gradient.Color1 := symbol.fillColorEnd;
  // Canvas.Fill.Kind := TBrushKind.Gradient;
  // Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
  // Canvas.Stroke.Color := symbol.outlineColor;
  // canvas.FillPolygon(Points, 1.0);
  // end;
  // end;
end;

procedure TSubgraph.drawCircle(ACanvas: ISkCanvas; x, y, diameter: single; symbol: TSymbol; filled: boolean);
var
  pt: TPointF;
begin
  pt.x := x;
  pt.y := y;
  if filled then
    begin
      LPaint.Style := TSkPaintStyle.Fill;
      LPaint.color := symbol.fillColor;
      ACanvas.drawCircle(pt, diameter, LPaint);
    end;

  LPaint.AntiAlias := True;
  LPaint.color := symbol.outlineColor;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := computePhysicalSize(symbol.outlineInCms);
  ACanvas.drawCircle(pt, diameter, LPaint)
end;

procedure TSubgraph.drawEllipse(ACanvas: ISkCanvas; x1, y1, x2, y2: single; symbol: TSymbol);
begin
  LPaint.color := symbol.outlineColor;
  LPaint.StrokeWidth := computePhysicalSize(symbol.outlineInCms);
  //
  ACanvas.DrawOval(rectf(x1, y1, x2, y2), LPaint);
  //
  // case symbol.gradientType of
  // gtNone :
  // begin
  // canvas.Fill.Color := symbol.fillColor;
  // canvas.FillEllipse (rectf (x1, y1, x2, y2), 1.0);
  // end;
  // gtVertLinear :
  // begin
  // Canvas.Fill.Gradient.Color := symbol.fillColorStart;
  // Canvas.Fill.Gradient.Color1 := symbol.fillColorEnd;
  // Canvas.Fill.Kind := TBrushKind.Gradient;
  // Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
  // Canvas.Stroke.Color := symbol.outlineColor;
  // canvas.FillEllipse (rectf (x1, y1, x2, y2), 1.0);
  // end;
  // end;
end;

procedure TSubgraph.drawSquare(ACanvas: ISkCanvas; x1, y1, x2, y2: single; symbol: TSymbol);
begin
  LPaint.color := symbol.outlineColor;
  LPaint.StrokeWidth := computePhysicalSize(symbol.outlineInCms);

  // HMS
  ACanvas.DrawRect(rectf(x1, y1, x2, y2), LPaint);

  // case symbol.gradientType of
  // gtNone :
  // begin
  // canvas.Fill.Color := symbol.fillColor;
  // canvas.FillRect (rectf (x1, y1, x2, y2), 0, 0, [], 1.0);
  // end;
  // gtVertLinear :
  // begin
  // Canvas.Fill.Gradient.Color := symbol.fillColorStart;
  // Canvas.Fill.Gradient.Color1 := symbol.fillColorEnd;
  // Canvas.Fill.Kind := TBrushKind.Gradient;
  // Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
  // Canvas.Stroke.Color := symbol.outlineColor;
  // canvas.FillRect (rectf (x1, y1, x2, y2), 0, 0, [], 1.0);
  // end;
  // end;
end;

{ Draw a symbol at the indicated device coordinates, x, y }
procedure TSubgraph.drawSymbolAtDeviceCoords(ACanvas: ISkCanvas; x, y: single; symbol: TSymbol);
var
  x1, x2, y1, y2, XOffSet, YOffSet, ExtX, ExtY, SymbolXPixelSize, SymbolYPixelSize, oldWidth: single;
  oldbrush, oldpen: TAlphaColor;
  outlineThickness: single;
  h, h1, h2: single;
  LRect: TRectF;
  LPathBuilder: ISkPathBuilder;
  LPath: ISKPath;
  pt: TPointF;
begin
  LPaint.AntiAlias := True;
  // Compute radius of Symbol in pixels units for current device
  SymbolXPixelSize := computePhysicalSize(symbol.diameterInCms);
  SymbolYPixelSize := computePhysicalSize(symbol.diameterInCms);
  XOffSet := trunc(SymbolXPixelSize); { Set Symbol size - should be user selectable }
  YOffSet := trunc(SymbolYPixelSize); { Set Symbol size - should be user selectable }
  x1 := x - XOffSet;
  x2 := x + XOffSet;
  y1 := y - YOffSet;
  y2 := y + YOffSet;

  { Check if DataSet has been selected by mouse, if so draw selected point }
  // if DataColumnProperty.Selected and Selectable then
  // begin
  // oldPen := canvas.pen.color;
  // canvas.pen.color := clRed;
  // canvas.rectangle (wxmin-1, wymin-1, wxmax+1, wymax+1);
  // canvas.pen.color := oldpen;
  // end;

  ExtX := round(SymbolXPixelSize * 1.6);
  ExtY := round(SymbolYPixelSize * 1.6);
  // Width of symbol outline, NOT width of symbol !

  // LPaint.Style := TSkPaintStyle.Stroke;
  // LPaint.Color := symbol.outlineColor;
  // LPaint.StrokeWidth := round (outlineThickness);

  outlineThickness := computePhysicalSize(symbol.outlineInCms);
  case symbol.symType of
    EmptyCircle:
      drawCircle(ACanvas, x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2, SymbolXPixelSize, symbol, false);
    SolidCircle:
      drawCircle(ACanvas, x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2, SymbolXPixelSize, symbol, True);
    EmptySquare:
      begin
        LRect := TRectF.Create(x1, y1, x2, y2);
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.color := symbol.outlineColor;
        LPaint.StrokeWidth := outlineThickness;
        ACanvas.DrawRect(LRect, LPaint);
      end;
    SolidSquare:
      begin
        LRect := TRectF.Create(x1, y1, x2, y2);
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.color := symbol.fillColor;
        ACanvas.DrawRect(rectf(x1, y1, x2, y2), LPaint);
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.color := symbol.outlineColor;
        LPaint.StrokeWidth := outlineThickness;
        ACanvas.DrawRect(LRect, LPaint);
      end;
    Cross:
      begin
        h := y1 + ((y2 - y1) / 2);
        drawSolidLine(ACanvas, x1, h, x2, h, symbol.fillColor, outlineThickness);
        h := x1 + ((x2 - x1) / 2);
        drawSolidLine(ACanvas, h, y1, h, y2, symbol.fillColor, outlineThickness);
      end;
    DiagonalCross:
      begin
        drawSolidLine(ACanvas, x1, y1, x2, y2, symbol.fillColor, outlineThickness);
        drawSolidLine(ACanvas, x2, y1, x1, y2, symbol.fillColor, outlineThickness);
      end;
    CrossedCircle:
      begin
        drawEllipse(ACanvas, x1, y1, x2, y2, symbol);
        // Extend lines beyond circle
        x1 := x - ExtX;
        x2 := x + ExtX;
        y1 := y - ExtY;
        y2 := y + ExtY;
        h := y1 + ((y2 - y1) / 2);
        drawSolidLine(ACanvas, x1, h, x2, h, symbol.fillColor, outlineThickness);
        h := x1 + ((x2 - x1) / 2);
        drawSolidLine(ACanvas, h, y1, h, y2, symbol.fillColor, outlineThickness);
      end;
    DiagCrossedCircle:
      begin
        drawEllipse(ACanvas, x1, y1, x2, y2, symbol);
        // Extend lines beyond circle
        x1 := x - ExtX;
        x2 := x + ExtX;
        y1 := y - ExtY;
        y2 := y + ExtY;
        drawSolidLine(ACanvas, x1, y1, x2, y2, symbol.fillColor, outlineThickness);
        drawSolidLine(ACanvas, x2, y1, x1, y2, symbol.fillColor, outlineThickness);
      end;
    EmptyDiamond:
      begin
        LPaint.color := symbol.outlineColor;
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := outlineThickness;
        ACanvas.DrawPoints(TSkDrawPointsMode.Polygon, [pointf(x, y2), pointf(x2, y), pointf(x, y1), pointf(x1, y), pointf(x, y2)
          ], LPaint);
      end;
    SolidDiamond:
      begin
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.color := symbol.fillColor;

        LPathBuilder := TSkPathBuilder.Create;
        LPathBuilder.MoveTo(x, y2);
        LPathBuilder.LineTo(x2, y);
        LPathBuilder.LineTo(x, y1);
        LPathBuilder.LineTo(x1, y);
        LPathBuilder.LineTo(x, y2);
        LPathBuilder.Close;
        LPath := LPathBuilder.Detach;
        ACanvas.DrawPath(LPath, LPaint);

        LPaint.color := symbol.outlineColor;
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := round(outlineThickness);

        LPathBuilder := TSkPathBuilder.Create;
        LPathBuilder.MoveTo(x, y2);
        LPathBuilder.LineTo(x2, y);
        LPathBuilder.LineTo(x, y1);
        LPathBuilder.LineTo(x1, y);
        LPathBuilder.LineTo(x, y2);
        LPathBuilder.Close;
        LPath := LPathBuilder.Detach;
        ACanvas.DrawPath(LPath, LPaint);
        ACanvas.DrawPoints(TSkDrawPointsMode.Polygon, [pointf(x, y2), pointf(x2, y), pointf(x, y1), pointf(x1, y), pointf(x, y2)
          ], LPaint);
      end;
    EmptyTriangle:
      begin
        LPaint.color := symbol.outlineColor;
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := outlineThickness;
        ACanvas.DrawPoints(TSkDrawPointsMode.Polygon, [pointf(x, y1), pointf(x2, y2), pointf(x1, y2), pointf(x, y1)], LPaint);
      end;
    SolidTriangle:
      begin
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.color := symbol.fillColor;

        LPathBuilder := TSkPathBuilder.Create;
        LPathBuilder.MoveTo(x, y1);
        LPathBuilder.LineTo(x2, y2);
        LPathBuilder.LineTo(x1, y2);
        LPathBuilder.LineTo(x, y1);
        LPathBuilder.Close();
        LPath := LPathBuilder.Detach;
        ACanvas.DrawPath(LPath, LPaint);

        LPaint.color := symbol.outlineColor;
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := outlineThickness;
        ACanvas.DrawPoints(TSkDrawPointsMode.Polygon, [pointf(x, y1), pointf(x2, y2), pointf(x1, y2), pointf(x, y1)], LPaint);
      end;
    SolidDownTriangle:
      begin
        // Use Polygon as it fills interior with Brush colour
        LPaint.Style := TSkPaintStyle.Fill;
        LPaint.color := symbol.fillColor;

        LPathBuilder := TSkPathBuilder.Create;
        LPathBuilder.MoveTo(x, y2);
        LPathBuilder.LineTo(x1, y1);
        LPathBuilder.LineTo(x2, y1);
        LPathBuilder.LineTo(x, y2);
        LPathBuilder.Close();
        LPath := LPathBuilder.Detach;
        ACanvas.DrawPath(LPath, LPaint);

        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.color := symbol.outlineColor;
        LPaint.StrokeWidth := outlineThickness;
        ACanvas.DrawPoints(TSkDrawPointsMode.Polygon, [pointf(x, y2), pointf(x1, y1), pointf(x2, y1), pointf(x, y2)], LPaint);
      end;
    Dots:
      drawEllipse(ACanvas, x - 1, y - 1, x + 1, y + 1, symbol);
    Empty:
      begin
      end;
  end;
end;

procedure TSubgraph.drawGraphBorder(ACanvas: ISkCanvas);
begin
  LPaint.color := properties.GraphBorderColor;
  LPaint.StrokeWidth := properties.GraphBorderThicknessInSkiaUnits;
  LPaint.AntiAlias := false;
  LPaint.StrokeCap := TSkStrokeCap.Square;
  LPaint.Style := TSkPaintStyle.Stroke;

  // Lower
  ACanvas.drawLine(pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmin)),
    pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmin)), LPaint);

  // Upper
  ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmax)),
    pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmax)), LPaint);

  // Left
  ACanvas.drawLine(pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmax)),
    pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmin)), LPaint);

  // Right
  ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmin)),
    pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmax)), LPaint);

  LPaint.AntiAlias := True;
end;

// Also draws minor grid lines if required
procedure TSubgraph.drawXAxisTicks(ACanvas: ISkCanvas);
var
  MajorVal, MinorVal, MajorStep, MinorStep, XLabelLoc, XAxisOrigin: Extended;
  i, j: integer;
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.color := properties.XMajorGridColor;
  LPaint.StrokeWidth := properties.XMajorGridThicknessInSkiaUnits;
  LPaint.StrokeCap := TSkStrokeCap.Square;

  if XLabelNearAxis then
    XLabelLoc := YOrigin
  else
    XLabelLoc := properties.FWorldYmin;

  if XAxisNearEdge then
    XAxisOrigin := properties.FWorldYmin
  else
    XAxisOrigin := YOrigin;

  if (properties.FWorldXmin < 0) and (properties.FWorldXmax > 0) then
    begin
      // This diversion is to make sure that a tick mark goes through the Zero point
      MajorVal := 0.0;
      MajorStep := (properties.FWorldXmax - properties.FWorldXmin) / (properties.NumberOfXTicks - 1);

      // Draw tick marks to the right of the origin first, starting at the origin
      while MajorVal <= properties.FWorldXmax do
        begin
          drawXAxisMajorTick(ACanvas, XAxisOrigin, MajorVal, LPaint);
          drawXLabel(ACanvas, MajorVal, fx(MajorVal), fy(XLabelLoc));

          if drawXAxisLine then
            begin
              MinorStep := (MajorVal + MajorStep - MajorVal) / properties.NumXMinorTicks;
              MinorVal := MajorVal + MinorStep;
              if MinorVal < properties.FWorldXmax then
                drawXMinorTicks(ACanvas, MinorVal, MinorStep, XAxisOrigin, properties.FWorldXmin, properties.FWorldXmax,
                  properties.NumXMinorTicks, +1);
            end;

          MajorVal := MajorVal + MajorStep;
        end;

      // Next draw tick marks to the left of origin, starting at tick closest to origin }
      MajorVal := 0.0;
      while MajorVal >= properties.FWorldXmin do
        begin
          drawXAxisMajorTick(ACanvas, XAxisOrigin, MajorVal, LPaint);
          drawXLabel(ACanvas, MajorVal, fx(MajorVal), fy(XLabelLoc));

          if drawXAxisLine then
            begin
              MinorStep := (MajorVal + MajorStep - MajorVal) / properties.NumXMinorTicks;
              MinorVal := MajorVal - MinorStep;
              if MinorVal > properties.FWorldXmin - MinorStep then
                drawXMinorTicks(ACanvas, MinorVal, MinorStep, XAxisOrigin, properties.FWorldXmin, properties.FWorldXmax,
                  properties.NumXMinorTicks, -1);
            end;

          MajorVal := MajorVal - MajorStep;
        end;
    end
  else
    begin
      // Ticks only to the right OR left of the origin, but not both
      MajorVal := properties.FWorldXmin;
      MajorStep := (properties.FWorldXmax - properties.FWorldXmin) / (properties.NumberOfXTicks - 1);
      // Don't include last tick, otherwise you'll have moinor tick marks beyond axis
      for i := 1 to properties.NumberOfXTicks - 1 do
        begin
          drawXAxisMajorTick(ACanvas, XAxisOrigin, MajorVal, LPaint);
          drawXLabel(ACanvas, MajorVal, fx(MajorVal), fy(XLabelLoc));

          if drawXAxisLine then
            begin
              MinorStep := (MajorVal + MajorStep - MajorVal) / properties.NumXMinorTicks;
              MinorVal := MajorVal + MinorStep;
              if MinorVal < properties.FWorldXmax then
                drawXMinorTicks(ACanvas, MinorVal, MinorStep, XAxisOrigin, properties.FWorldXmin, properties.FWorldXmax,
                  properties.NumXMinorTicks, +1);
            end;

          MajorVal := MajorVal + MajorStep;
        end;
      // ... and finally add the last major tick
      drawXAxisMajorTick(ACanvas, XAxisOrigin, MajorVal, LPaint);
      drawXLabel(ACanvas, MajorVal, fx(MajorVal), fy(XLabelLoc));
    end;
end;

procedure TSubgraph.drawXAxisLogTicks(ACanvas: ISkCanvas);
var
  t, B, MajorVal, MinorVal, MinorStep, MajorStep, XLabelLoc: Extended;
  Temp: double;
  Count, i, j: integer;
  XAxisOrigin: Extended;
begin
  if XLabelNearAxis then
    XLabelLoc := YOrigin
  else
    XLabelLoc := properties.FWorldYmin;

  if XAxisNearEdge then
    XAxisOrigin := properties.FWorldYmin
  else
    XAxisOrigin := YOrigin;

  // Find starting decade
  t := Log10(properties.FWorldXmin);
  if t < 0 then
    t := t - 1; { if WorldYmin < 1.0 }
  MajorVal := power(10, trunc(1.001 * t));
  // MajorVal = the first decade of the scale, 1, 10, or 100 etc

  // How many major decade markers are there?
  Temp := MajorVal;
  Count := 0;
  while Temp <= properties.FWorldXmax do
    begin
      Count := Count + 1;
      Temp := Temp * 10;
    end;

  // step equals the interval between MINOR ticks marks
  MinorStep := (MajorVal * 10 - MajorVal) / (properties.NumXMinorTicks - 1);
  MinorVal := MajorVal + MinorStep; // remember, MajorVal is positioned at the first major tick mark

  // Now fill the complete decade intervals with minors
  Count := Count - 1;
  for i := 1 to Count do
    begin
      // Only draw a tick if its positioned within the axes limits
      if (MajorVal <= properties.FWorldXmax) and (MajorVal + 1E-16 >= properties.FWorldXmin) then
        begin
          // DrawXAxisMajorTick (Canvas, Y_Origin, MajorVal);
          drawXAxisMajorTick(ACanvas, XAxisOrigin, MajorVal, LPaint);
          drawXLabel(ACanvas, MajorVal, fx(MajorVal), fy(XLabelLoc));
          if drawXAxisLine then
            drawLogXMinorTicks(ACanvas, MinorVal, MinorStep, XAxisOrigin, properties.FWorldXmin, properties.FWorldXmax,
              nXLogMinorTicks, +1);
        end;
      MajorVal := MajorVal * 10; { Next decade }
      MinorStep := MinorStep * 10;
      MinorVal := MajorVal + MinorStep;
      { Start next set of minors at new major tick + step }
    end;
  drawXLabel(ACanvas, MajorVal, fx(MajorVal), fy(XLabelLoc));
end;

// Draw tick marks and optional minor grid lines
procedure TSubgraph.drawXMinorTicks(ACanvas: ISkCanvas; startvalue, stepValue, Origin, min, max: double;
  nticks, direction: integer);
var
  i: integer;
  gridThickness: single;
  x: double;
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  gridThickness := properties.XMinorGridThicknessInSkiaUnits;
  LPaint.StrokeWidth := gridThickness;
  LPaint.color := properties.XMinorGridColor; // XMinorTickColor;

  for i := 1 to nticks - 1 do // Don't add the last minor tick
    begin
      if not((startvalue < min) or (startvalue > max)) then
        begin
          // Was Y_Origin
          if MinorXTicks then
            begin
              case MinorXTicksStyle of
                tsIn:
                  begin
                    if not FSubGraphProperties.XMinorGridLines then
                      begin
                        // Minus tick length because the Y direction starts at 0 in top right corner
                        ACanvas.drawLine(pointf(fx(startvalue), fy(Origin)),
                          pointf(fx(startvalue), fy(Origin) - MinorTickLengthInSkiaUnits), LPaint);
                        if FSubGraphProperties.GraphBorder then
                          begin
                            ACanvas.drawLine(pointf(fx(startvalue), fy(properties.FWorldYmax)),
                              pointf(fx(startvalue), fy(properties.FWorldYmax) + MinorTickLengthInSkiaUnits), LPaint);
                          end;
                      end;
                  end;
                tsOut:
                  begin
                    ACanvas.drawLine(pointf(fx(startvalue), fy(Origin)),
                      pointf(fx(startvalue), fy(Origin) + MinorTickLengthInSkiaUnits), LPaint);
                    if FSubGraphProperties.GraphBorder then
                      begin
                        ACanvas.drawLine(pointf(fx(startvalue), fy(properties.FWorldYmax)),
                          pointf(fx(startvalue), fy(properties.FWorldYmax) - MinorTickLengthInSkiaUnits), LPaint);
                      end;
                  end;
                tsInOut:
                  begin
                    ACanvas.drawLine(pointf(fx(startvalue), fy(Origin)),
                      pointf(fx(startvalue), fy(Origin) - MinorTickLengthInSkiaUnits), LPaint);
                    ACanvas.drawLine(pointf(fx(startvalue), fy(Origin)),
                      pointf(fx(startvalue), fy(Origin) + MinorTickLengthInSkiaUnits), LPaint);

                    if FSubGraphProperties.GraphBorder then
                      begin
                        ACanvas.drawLine(pointf(fx(startvalue), fy(properties.FWorldYmax)),
                          pointf(fx(startvalue), fy(properties.FWorldYmax) + MinorTickLengthInSkiaUnits), LPaint);
                        ACanvas.drawLine(pointf(fx(startvalue), fy(properties.FWorldYmax)),
                          pointf(fx(startvalue), fy(properties.FWorldYmax) - MinorTickLengthInSkiaUnits), LPaint);
                      end;
                  end;
              end;
            end;

          if FSubGraphProperties.XMinorGridLines then
            begin
              LPaint.StrokeWidth := gridThickness;
              LPaint.color := properties.XMinorGridColor;
              // HMS LPaint.Stroke.Dash := GridLineStyle;
              ACanvas.drawLine(pointf(fx(startvalue), fy(properties.FWorldYmin)), pointf(fx(startvalue), fy(properties.FWorldYmax)
                ), LPaint);
            end;
        end;
      startvalue := startvalue + direction * stepValue;
    end;
end;

// Draw tick marks and optional minor grid lines
procedure TSubgraph.drawLogXMinorTicks(ACanvas: ISkCanvas; startvalue, stepValue, Origin, min, max: double;
  nticks, direction: integer);
var
  i: integer;
  oldcolor: TAlphaColor;
  gridThickness: single;
begin
  // HMS
  // oldcolor := canvas.stroke.Color;
  // canvas.stroke.Dash := TStrokeDash.Solid;
  // gridThickness := computePhysicalSize (MinorGridThicknessInCms);
  // for i := 1 to nticks - 1 do // Don't add the last minor tick
  // begin
  // canvas.stroke.Color := properties.XMinorTickColor;
  // if not((startvalue < min) or (startvalue > max)) then
  // begin
  // // Was Y_Origin
  // if MinorXTicks then
  // begin
  // case MinorXTicksStyle of
  // tsIn :
  // begin
  // canvas.DrawLine(pointf (fx(startvalue), fy(Origin)), pointf (fx(startvalue), fy(Origin) - MinorTickLength), 1.0);
  // if Fproperties.FGraphBorder then
  // begin
  // canvas.DrawLine(pointf (fx(startvalue), fy(properties.FWorldYmax)), pointf (fx(startvalue), fy(properties.FWorldYmax) + MinorTickLength), 1.0);
  // end;
  // end;
  // tsOut :
  // begin
  // canvas.DrawLine(pointf (fx(startvalue), fy(Origin)), pointf (fx(startvalue), fy(Origin) + MinorTickLength), 1.0);
  // if Fproperties.FGraphBorder then
  // begin
  // canvas.DrawLine(pointf (fx(startvalue), fy(properties.FWorldYmax)), pointf (fx(startvalue), fy(properties.FWorldYmax) - MinorTickLength), 1.0);
  // end;
  // end;
  // tsInOut :
  // begin
  // canvas.DrawLine(pointf (fx(startvalue), fy(Origin)), pointf (fx(startvalue), fy(Origin) - MinorTickLength), 1.0);
  // canvas.DrawLine(pointf (fx(startvalue), fy(Origin)), pointf (fx(startvalue), fy(Origin) + MinorTickLength), 1.0);
  // if Fproperties.FGraphBorder then
  // begin
  // canvas.DrawLine(pointf (fx(startvalue), fy(properties.FWorldYmax)), pointf (fx(startvalue), fy(properties.FWorldYmax) + MinorTickLength), 1.0);
  // canvas.DrawLine(pointf (fx(startvalue), fy(properties.FWorldYmax)), pointf (fx(startvalue), fy(properties.FWorldYmax) - MinorTickLength), 1.0);
  // end;
  // end;
  // end;
  // end;
  // if Fproperties.XMinorGridLines then
  // begin
  // canvas.stroke.Thickness := gridThickness;
  // canvas.stroke.Color := properties.XMinorGridColor;
  // canvas.Stroke.Dash := GridLineStyle;
  // canvas.DrawLine(pointf (fx(startvalue), fy(properties.FWorldYmin)), pointf (fx(startvalue), fy(properties.FWorldYmax)), 1.0);
  // end;
  // end;
  // startvalue := startvalue + direction * stepValue;
  // end;
  // canvas.stroke.Color := oldcolor;
end;

procedure TSubgraph.drawAllMajorXGridLines(ACanvas: ISkCanvas);
var
  MajorVal, MinorVal, MajorStep, MinorStep: Extended;
  i, j: integer;
  LPaint: ISkPaint;
  AxisPixelThickness: single;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  AxisPixelThickness := properties.XMajorGridThicknessInSkiaUnits;
  LPaint.StrokeWidth := AxisPixelThickness;
  LPaint.color := properties.XMajorGridColor; // The X grids are the vertical ones

  if (properties.FWorldXmin < 0) and (properties.FWorldXmax > 0) then
    begin
      // This diversion is to make sure that a tick mark goes through the Zero point
      MajorVal := 0.0;
      MajorStep := (properties.FWorldXmax - properties.FWorldXmin) / (properties.NumberOfXTicks - 1);

      // Draw tick marks to the right of the origin first, starting at the origin
      while MajorVal <= properties.FWorldXmax do
        begin
          drawXGridLine(ACanvas, MajorVal, properties.FWorldYmin, properties.FWorldYmax, LPaint);
          MajorVal := MajorVal + MajorStep;
        end;
      // Next draw tick marks to the left of origin, starting at tick closest to origin
      MajorVal := 0.0;
      while MajorVal >= properties.FWorldXmin do
        begin
          drawXGridLine(ACanvas, MajorVal, properties.FWorldYmin, properties.FWorldYmax, LPaint);
          MajorVal := MajorVal - MajorStep;
        end;
    end
  else
    begin
      // Ticks only to the right OR left of the origin, but not both
      MajorVal := properties.FWorldXmin;
      MajorStep := (properties.FWorldXmax - properties.FWorldXmin) / (properties.NumberOfXTicks - 1);
      // Don't include last tick, otherwise you'll have minor tick marks beyond axis
      for i := 1 to properties.NumberOfXTicks - 1 do
        begin
          drawXGridLine(ACanvas, MajorVal, properties.FWorldYmin, properties.FWorldYmax, LPaint);
          MajorVal := MajorVal + MajorStep;
        end;
      // ... and finally add the last major tick
      drawXGridLine(ACanvas, MajorVal, properties.FWorldYmin, properties.FWorldYmax, LPaint);
    end;
end;

procedure TSubgraph.drawXAxisMajorTick(ACanvas: ISkCanvas; Origin, MajorVal: Extended; LPaint: ISkPaint);
begin
  if drawXAxisLine then
    begin
      if MajorXTicks then
        begin
          case MajorXTicksStyle of
            tsIn:
              begin
                // Was Y_Origin
                ACanvas.drawLine(pointf(fx(MajorVal), fy(Origin)),
                  pointf(fx(MajorVal), fy(Origin) - MajorTickLengthInSkiaUnits), LPaint);
                if FSubGraphProperties.GraphBorder then
                  begin
                    ACanvas.drawLine(pointf(fx(MajorVal), fy(properties.FWorldYmax)),
                      pointf(fx(MajorVal), fy(properties.FWorldYmax) + MajorTickLengthInSkiaUnits), LPaint);
                  end;
              end;
            tsOut:
              begin
                ACanvas.drawLine(pointf(fx(MajorVal), fy(Origin)),
                  pointf(fx(MajorVal), fy(Origin) + MajorTickLengthInSkiaUnits), LPaint);
                if FSubGraphProperties.GraphBorder then
                  begin
                    ACanvas.drawLine(pointf(fx(MajorVal), fy(properties.FWorldYmax)),
                      pointf(fx(MajorVal), fy(properties.FWorldYmax) - MajorTickLengthInSkiaUnits), LPaint);
                  end;
              end;
            tsInOut:
              begin
                // Was Y_Origin
                ACanvas.drawLine(pointf(fx(MajorVal), fy(Origin)),
                  pointf(fx(MajorVal), fy(Origin) - MajorTickLengthInSkiaUnits), LPaint);
                ACanvas.drawLine(pointf(fx(MajorVal), fy(Origin)),
                  pointf(fx(MajorVal), fy(Origin) + MajorTickLengthInSkiaUnits), LPaint);
                if FSubGraphProperties.GraphBorder then
                  begin
                    ACanvas.drawLine(pointf(fx(MajorVal), fy(properties.FWorldYmax)),
                      pointf(fx(MajorVal), fy(properties.FWorldYmax) + MajorTickLengthInSkiaUnits), LPaint);
                    ACanvas.drawLine(pointf(fx(MajorVal), fy(properties.FWorldYmax)),
                      pointf(fx(MajorVal), fy(properties.FWorldYmax) - MajorTickLengthInSkiaUnits), LPaint);
                  end;
              end;
          end;
        end;
    end;
end;

procedure TSubgraph.drawYAxisLogTicks(ACanvas: ISkCanvas);
var
  t, B, MajorVal, MinorVal, MinorStep, YAxisOrigin: Extended;
  Temp: double;
  Count, i, j: integer;
  EWorldYmax, YLabelLoc: Extended;
begin
  // HMS
  // // Find starting decade
  // t := Log10(properties.FWorldYmin);
  // if t < 0 then
  // t := t - 1; // if WorldYmin < 1.0
  // MajorVal := power(10, trunc(1.001 * t)); // MajorVal = the first decade of the scale, 1, 10, or 100 etc
  //
  // if YLabelNearAxis then
  // YLabelLoc := XOrigin
  // else
  // YLabelLoc := properties.FWorldXmin;
  //
  // if YAxisNearEdge then
  // YAxisOrigin := properties.FWorldXmin
  // else
  // YAxisOrigin := XOrigin;
  //
  // // How many major decade markers are there?
  // Temp := MajorVal;
  // count := 0;
  // while Temp <= properties.FWorldYmax do
  // begin
  // count := count + 1;
  // Temp := Temp * 10;
  // end;
  //
  // // step equals the interval between MINOR ticks marks
  // MinorStep := (MajorVal * 10 - MajorVal) / (nYMinorTicks - 1);
  // MinorVal := MajorVal + MinorStep; // remember, MajorVal is positioned at the first major tick mark
  //
  // // Now fill the complete decade intervals with minors
  // count := count - 1;
  // for i := 1 to count do
  // begin
  // // Only draw a tick if its positioned within the axes limits
  // if (MajorVal <= properties.FWorldYmax) and (MajorVal + 1E-16 >= properties.FWorldYmin) then
  // begin
  // drawYAxisMajorTick(canvas, XOrigin, MajorVal);
  // drawYLabel(canvas, MajorVal, fx(YLabelLoc), fy(MajorVal));
  // if drawYAxisLine then
  // drawLogYMinorTicks(canvas, MinorVal, MinorStep, YAxisOrigin, properties.FWorldXmin, properties.FWorldXmax, nYLogMinorTicks, +1);
  // end;
  // MajorVal := MajorVal * 10; { Next decade }
  // MinorStep := MinorStep * 10;
  // MinorVal := MajorVal + MinorStep;
  // { Start next set of minors at new major tick + step }
  // end;
  // drawYLabel(canvas, MajorVal, fx(YLabelLoc), fy(MajorVal));
end;

// Draw tick marks and optional minor grid lines
procedure TSubgraph.drawLogYMinorTicks(canvas: TCanvas; startvalue, stepValue, Origin, min, max: double;
  nticks, direction: integer);
var
  i: integer;
  oldcolor: TAlphaColor;
  gridThickness: single;
begin
  oldcolor := canvas.Stroke.color;
  canvas.Stroke.Dash := TStrokeDash.Solid;
  gridThickness := properties.YMinorGridThicknessInSkiaUnits;
  for i := 1 to nticks - 1 do // Don't add the last minor tick
    begin
      canvas.Stroke.color := properties.YMinorGridColor;
      if not((startvalue < min) or (startvalue > max)) then
        begin
          // Was X_Origin
          if MinorXTicks then
            case MinorYTicksStyle of
              tsIn:
                begin
                  canvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) + MinorTickLengthInSkiaUnits,
                    fy(startvalue)), 1.0);
                  if FSubGraphProperties.GraphBorder then
                    begin
                      canvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                        pointf(fx(properties.FWorldXmax) - MinorTickLengthInSkiaUnits, fy(startvalue)), 1.0);
                    end;
                end;
              tsOut:
                begin
                  canvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) - MinorTickLengthInSkiaUnits,
                    fy(startvalue)), 1.0);
                  if FSubGraphProperties.GraphBorder then
                    begin
                      canvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                        pointf(fx(properties.FWorldXmax) + MinorTickLengthInSkiaUnits, fy(startvalue)), 1.0);
                    end;
                end;
              tsInOut:
                begin
                  canvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) + MinorTickLengthInSkiaUnits,
                    fy(startvalue)), 1.0);
                  canvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) - MinorTickLengthInSkiaUnits,
                    fy(startvalue)), 1.0);
                  if FSubGraphProperties.GraphBorder then
                    begin
                      canvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                        pointf(fx(properties.FWorldXmax) - MinorTickLengthInSkiaUnits, fy(startvalue)), 1.0);
                      canvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                        pointf(fx(properties.FWorldXmax) + MinorTickLengthInSkiaUnits, fy(startvalue)), 1.0);
                    end;
                end;
            end;
          if FSubGraphProperties.YMinorGridLines then
            begin
              canvas.Stroke.Dash := TStrokeDash.Solid;
              canvas.Stroke.thickness := gridThickness;
              canvas.Stroke.color := properties.YMinorGridColor;
              canvas.drawLine(pointf(fx(properties.FWorldXmin), fy(startvalue)),
                pointf(fx(properties.FWorldXmax), fy(startvalue)), 1.0);
            end;
        end;
      startvalue := startvalue + direction * stepValue;
    end;
  canvas.Stroke.color := oldcolor;
end;

procedure TSubgraph.drawYAxisTicks(ACanvas: ISkCanvas);
var
  MajorVal, MajorStep, MinorVal, MinorStep, YLabelLoc, YAxisOrigin: Extended;
  i, j: integer;
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.color := properties.YMajorGridColor;
  LPaint.StrokeWidth := properties.YMajorGridThicknessInSkiaUnits; // computePhysicalSize(properties.YMajorGridThicknessInCms);
  LPaint.StrokeCap := TSkStrokeCap.Square;

  if YLabelNearAxis then
    YLabelLoc := XOrigin
  else
    YLabelLoc := properties.FWorldXmin;

  if YAxisNearEdge then
    YAxisOrigin := properties.FWorldXmin
  else
    YAxisOrigin := XOrigin;

  if (properties.FWorldYmin < 0) and (properties.FWorldYmax > 0) then
    begin
      { There is a zero on the axis and a tick must go through it }
      MajorVal := 0.0;
      MajorStep := (properties.FWorldYmax - properties.FWorldYmin) / (properties.NumberOfYTicks - 1);

      { Draw tick marks above the origin first, starting at the origin }
      while MajorVal <= properties.FWorldYmax do
        begin
          drawYAxisMajorTick(ACanvas, YAxisOrigin, MajorVal);
          drawYLabel(ACanvas, MajorVal, fx(YLabelLoc), fy(MajorVal));

          if drawYAxisLine then
            begin
              MinorStep := (MajorVal + MajorStep - MajorVal) / properties.NumYMinorTicks;
              MinorVal := MajorVal + MinorStep;
              if MinorVal < properties.FWorldYmax then
                drawYMinorTicks(ACanvas, MinorVal, MinorStep, YAxisOrigin, properties.FWorldYmin, properties.FWorldYmax,
                  properties.NumYMinorTicks, +1);
            end;

          MajorVal := MajorVal + MajorStep;
        end;
      { Next draw tick marks below the origin, starting at tick closest to origin }
      MajorVal := 0.0;
      MinorStep := MajorStep / properties.NumYMinorTicks;
      while MajorVal >= properties.FWorldYmin do
        begin
          drawYAxisMajorTick(ACanvas, YAxisOrigin, MajorVal);
          drawYLabel(ACanvas, MajorVal, fx(YLabelLoc), fy(MajorVal));

          if drawYAxisLine then
            begin
              MinorVal := MajorVal - MinorStep;
              if MinorVal > properties.FWorldYmin then
                drawYMinorTicks(ACanvas, MinorVal, MinorStep, YAxisOrigin, properties.FWorldYmin, properties.FWorldYmax,
                  properties.NumYMinorTicks, -1);
            end;

          MajorVal := MajorVal - MajorStep;
        end;
    end
  else
    begin
      { Ticks only to the right OR left of the origin, but not both }
      MajorVal := properties.FWorldYmin;
      MajorStep := (properties.FWorldYmax - properties.FWorldYmin) / (properties.NumberOfYTicks - 1);
      { Don't include last tick, otherwise you'll have moinor tick marks beyond axis }
      for i := 1 to properties.NumberOfYTicks - 1 do
        begin
          drawYAxisMajorTick(ACanvas, YAxisOrigin, MajorVal);
          drawYLabel(ACanvas, MajorVal, fx(YLabelLoc), fy(MajorVal));

          if drawYAxisLine then
            begin
              MinorStep := (MajorVal + MajorStep - MajorVal) / properties.NumYMinorTicks;
              MinorVal := MajorVal + MinorStep;

              if MinorVal < properties.FWorldYmax then
                drawYMinorTicks(ACanvas, MinorVal, MinorStep, YAxisOrigin, properties.FWorldYmin, properties.FWorldYmax,
                  properties.NumYMinorTicks, +1);
            end;

          MajorVal := MajorVal + MajorStep;
        end;
      { ... and finally add the last major tick }
      drawYAxisMajorTick(ACanvas, YAxisOrigin, MajorVal);
      drawYLabel(ACanvas, MajorVal, fx(YLabelLoc), fy(MajorVal));
    end;
end;

procedure TSubgraph.drawYAxisMajorTick(ACanvas: ISkCanvas; Origin, MajorVal: Extended);
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.color := properties.YMajorGridColor;
  LPaint.StrokeWidth := properties.YMajorTickWidthInSkiaUnits; // computePhysicalSize(properties.YMajorTickWidthInCms);
  if drawYAxisLine then
    begin
      if MajorYTicks then
        begin
          case MajorYTicksStyle of
            tsIn:
              begin
                ACanvas.drawLine(pointf(fx(Origin), fy(MajorVal)), pointf(fx(Origin) + MajorTickLengthInSkiaUnits, fy(MajorVal)
                  ), LPaint);
                if FSubGraphProperties.GraphBorder then
                  begin
                    ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(MajorVal)),
                      pointf(fx(properties.FWorldXmax) - MajorTickLengthInSkiaUnits, fy(MajorVal)), LPaint);
                  end;
              end;
            tsOut:
              begin
                ACanvas.drawLine(pointf(fx(Origin), fy(MajorVal)), pointf(fx(Origin) - MajorTickLengthInSkiaUnits, fy(MajorVal)
                  ), LPaint);
                if FSubGraphProperties.GraphBorder then
                  begin
                    ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(MajorVal)),
                      pointf(fx(properties.FWorldXmax) + MajorTickLengthInSkiaUnits, fy(MajorVal)), LPaint);
                  end;
              end;
            tsInOut:
              begin
                ACanvas.drawLine(pointf(fx(Origin), fy(MajorVal)), pointf(fx(Origin) + MajorTickLengthInSkiaUnits, fy(MajorVal)
                  ), LPaint);
                ACanvas.drawLine(pointf(fx(Origin), fy(MajorVal)), pointf(fx(Origin) - MajorTickLengthInSkiaUnits, fy(MajorVal)
                  ), LPaint);
                if FSubGraphProperties.GraphBorder then
                  begin
                    ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(MajorVal)),
                      pointf(fx(properties.FWorldXmax) - MajorTickLengthInSkiaUnits, fy(MajorVal)), LPaint);
                    ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(MajorVal)),
                      pointf(fx(properties.FWorldXmax) + MajorTickLengthInSkiaUnits, fy(MajorVal)), LPaint);
                  end;
              end;
          end;
        end;
    end;
end;

procedure TSubgraph.drawYMinorTicks(ACanvas: ISkCanvas; startvalue, stepValue, Origin, min, max: double;
  nticks, direction: integer);
var
  i: integer;
  oldcolor: TAlphaColor;
  gridThickness: single;
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  gridThickness := properties.YMinorGridThicknessInSkiaUnits;
  LPaint.StrokeWidth := gridThickness;
  LPaint.color := properties.YMinorGridColor;

  for i := 1 to nticks - 1 do // Don't add the last minor tick
    begin
      if not((startvalue < min) or (startvalue > max)) then
        begin
          // Was X_Origin
          if MinorYTicks then
            begin
              case MinorYTicksStyle of
                tsIn:
                  begin
                    // For some reason drawing the ticks and grid line makes the tick darker??
                    if not FSubGraphProperties.YMinorGridLines then
                      begin
                        ACanvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) + MinorTickLengthInSkiaUnits,
                          fy(startvalue)), LPaint);
                        if FSubGraphProperties.GraphBorder then
                          ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                            pointf(fx(properties.FWorldXmax) - MinorTickLengthInSkiaUnits, fy(startvalue)), LPaint);
                      end;
                  end;
                tsOut:
                  begin
                    ACanvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) - MinorTickLengthInSkiaUnits,
                      fy(startvalue)), LPaint);
                    if FSubGraphProperties.GraphBorder then
                      ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                        pointf(fx(properties.FWorldXmax) + MinorTickLengthInSkiaUnits, fy(startvalue)), LPaint);
                  end;
                tsInOut:
                  begin
                    ACanvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) + MinorTickLengthInSkiaUnits,
                      fy(startvalue)), LPaint);
                    ACanvas.drawLine(pointf(fx(Origin), fy(startvalue)), pointf(fx(Origin) - MinorTickLengthInSkiaUnits,
                      fy(startvalue)), LPaint);
                    if FSubGraphProperties.GraphBorder then
                      begin
                        ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                          pointf(fx(properties.FWorldXmax) - MinorTickLengthInSkiaUnits, fy(startvalue)), LPaint);
                        ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(startvalue)),
                          pointf(fx(properties.FWorldXmax) + MinorTickLengthInSkiaUnits, fy(startvalue)), LPaint);
                      end;
                  end;
              end;
            end;

          if FSubGraphProperties.YMinorGridLines then
            begin
              LPaint.StrokeWidth := gridThickness;
              LPaint.color := properties.YMinorGridColor;
              ACanvas.drawLine(pointf(fx(properties.FWorldXmin), fy(startvalue)), pointf(fx(properties.FWorldXmax), fy(startvalue)
                ), LPaint);
            end;
        end;
      startvalue := startvalue + direction * stepValue;
    end;
end;

procedure TSubgraph.drawAllMajorYGridLines(ACanvas: ISkCanvas);
var
  MajorVal, MajorStep, MinorVal, MinorStep: Extended;
  i, j: integer;
  LPaint: ISkPaint;
  AxisPixelThickness: single;
begin
  LPaint := TSkPaint.Create;
  LPaint.color := properties.YMajorGridColor; // The Y grids are the horizontal ones
  AxisPixelThickness := properties.YMajorGridThicknessInSkiaUnits; // computePhysicalSize(properties.YMajorGridThicknessInCms);
  LPaint.StrokeWidth := AxisPixelThickness;
  if (properties.FWorldYmin < 0) and (properties.FWorldYmax > 0) then
    begin
      // There is a zero on the axis and a tick must go through it
      MajorVal := 0.0;
      MajorStep := (properties.FWorldYmax - properties.FWorldYmin) / (properties.NumberOfYTicks - 1);

      // Draw tick marks above the origin first, starting at the origin
      while MajorVal <= properties.FWorldYmax do
        begin
          if not(Math.SameValue(MajorVal, properties.FWorldXmin)) then
            drawYGridLine(ACanvas, MajorVal, properties.FWorldXmin, properties.FWorldXmax, LPaint);
          MajorVal := MajorVal + MajorStep;
        end;
      // Next draw tick marks below the origin, starting at tick closest to origin
      MajorVal := 0.0;
      MinorStep := MajorStep / properties.NumYMinorTicks;
      while MajorVal >= properties.FWorldYmin do
        begin
          drawYGridLine(ACanvas, MajorVal, properties.FWorldXmin, properties.FWorldXmax, LPaint);
          MajorVal := MajorVal - MajorStep;
        end;
    end
  else
    begin
      // Ticks only to the right OR left of the origin, but not both
      MajorVal := properties.FWorldYmin;
      MajorStep := (properties.FWorldYmax - properties.FWorldYmin) / (properties.NumberOfYTicks - 1);
      // Don't include last tick, otherwise you'll have moinor tick marks beyond axis
      for i := 1 to properties.NumberOfYTicks - 1 do
        begin
          if not(Math.SameValue(MajorVal, properties.FWorldXmin)) then
            drawYGridLine(ACanvas, MajorVal, properties.FWorldXmin, properties.FWorldXmax, LPaint);
          MajorVal := MajorVal + MajorStep;
        end;
      // ... and finally add the last major line
      drawYGridLine(ACanvas, MajorVal, properties.FWorldXmin, properties.FWorldXmax, LPaint);
    end;
end;

procedure TSubgraph.drawXGridLine(ACanvas: ISkCanvas; x, ymin, ymax: double; LPaint: ISkPaint);
begin
  if (x <> XOrigin) and (x <> properties.FWorldXmax) then
    begin
      try
        ACanvas.drawLine(pointf(fx(x), fy(ymin)), pointf(fx(x), fy(ymax)), LPaint);
      finally
      end;
    end;
end;

procedure TSubgraph.drawYGridLine(ACanvas: ISkCanvas; y, xmin, xmax: double; LPaint: ISkPaint);
begin
  if (y <> YOrigin) and (y <> properties.FWorldYmax) then
    begin
      try
        ACanvas.drawLine(pointf(fx(xmin), fy(y)), pointf(fx(xmax), fy(y)), LPaint);
      finally
      end;
    end;
end;

procedure TSubgraph.drawSelectedGraphingArea(ACanvas: ISkCanvas);
var
  aleft, atop, aRight, aBottom: single;
  pathEffect : ISkPathEffect;
begin
  if properties.graphObjects[graphingAreaId].selected then
    begin
      aleft := fx(properties.FWorldXmin);
      aRight := fx(properties.FWorldXmax);
      aBottom := fy(properties.FWorldYmin);
      atop := fy(properties.FWorldYmax);

      PathEffect := TSkPathEffect.MakeDash([15, 5, 15, 5], 1);
      LPaint.PathEffect := PathEffect;

      LPaint.color := properties.GraphBorderColor;
      LPaint.StrokeWidth := properties.GraphBorderThicknessInSkiaUnits;
      LPaint.AntiAlias := false;
      LPaint.StrokeCap := TSkStrokeCap.Square;
      LPaint.Style := TSkPaintStyle.Stroke;

      // Lower
      ACanvas.drawLine(pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmin)),
        pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmin)), LPaint);

      // Upper
      ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmax)),
        pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmax)), LPaint);

      // Left
      ACanvas.drawLine(pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmax)),
        pointf(fx(properties.FWorldXmin), fy(properties.FWorldYmin)), LPaint);

      // Right
      ACanvas.drawLine(pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmin)),
        pointf(fx(properties.FWorldXmax), fy(properties.FWorldYmax)), LPaint);

      LPaint.AntiAlias := True;
      LPaint.PathEffect := nil;

      LPaint.color := claBlack;
      LPaint.StrokeWidth := properties.GraphBorderThicknessInSkiaUnits;
      LPaint.AntiAlias := false;
      LPaint.StrokeCap := TSkStrokeCap.Square;
      LPaint.Style := TSkPaintStyle.Stroke;
      LPaint.Style := TSkPaintStyle.Fill;
      ACanvas.DrawRect(TRectF.Create(aleft, atop, aleft+8, atop+8), LPaint);
      ACanvas.DrawRect(TRectF.Create(aright, atop, aright-8, atop+8), LPaint);
      ACanvas.DrawRect(TRectF.Create(aright, abottom, aright-8, abottom-8), LPaint);
      ACanvas.DrawRect(TRectF.Create(aleft, abottom, aleft+8, abottom-8), LPaint);

      // // Don't use DrawRect, its too slow
      // drawDottedBox (canvas, aleft, atop, aRight, aBottom);
      // canvas.FillRect (rectf (aleft, atop, aleft+5, atop+5), 0, 0, [], 1.0);
      // canvas.FillRect (rectf (aright-5, atop, aright, atop+5), 0, 0, [], 1.0);

      // canvas.FillRect (rectf (aright-5, abottom, aright, abottom-5), 0, 0, [], 1.0);
      // canvas.FillRect (rectf (aleft, abottom, aleft+5, abottom-5), 0, 0, [], 1.0);
      // exit;
    end;
end;

procedure TSubgraph.drawAxesLines(ACanvas: ISkCanvas; DrawXAxis, DrawYAxis: boolean);
var
  AxisPixelThickness: single;
  XAxisOrigin, YAxisOrigin: double;
  aBox: TBox;
begin
  AxisPixelThickness := computePhysicalSize(AxisThicknessInCms);
  if FSubGraphProperties.XGridLines then
    AxisPixelThickness := AxisPixelThickness;

  if XAxisNearEdge then
    XAxisOrigin := properties.FWorldYmin
  else
    XAxisOrigin := YOrigin;

  if YAxisNearEdge then
    YAxisOrigin := properties.FWorldXmin
  else
    YAxisOrigin := XOrigin;

  if DrawXAxis then
    begin
      LPaint.StrokeWidth := AxisPixelThickness;
      LPaint.color := properties.XAxisColor;
      LPaint.AntiAlias := false;
      LPaint.StrokeCap := TSkStrokeCap.Square;
      ACanvas.drawLine(pointf(fx(properties.FWorldXmin), fy(XAxisOrigin)), pointf(fx(properties.FWorldXmax), fy(XAxisOrigin)
        ), LPaint);
    end;

  AxisPixelThickness := computePhysicalSize(AxisThicknessInCms);
  if FSubGraphProperties.YGridLines then
    AxisPixelThickness := AxisPixelThickness;
  if DrawYAxis then
    begin
      LPaint.StrokeWidth := AxisPixelThickness;
      LPaint.color := properties.YAxisColor;
      LPaint.AntiAlias := false;
      LPaint.StrokeCap := TSkStrokeCap.Square;
      ACanvas.drawLine(pointf(fx(YAxisOrigin), fy(properties.FWorldYmin)), pointf(fx(YAxisOrigin), fy(properties.FWorldYmax)
        ), LPaint);
    end;
  LPaint.AntiAlias := True;
end;

procedure TSubgraph.drawXLabel(ACanvas: ISkCanvas; value: double; x, y: single);
var
  str: string;
  OffSet: single;
  XOffSet, textHeight: double;
  wOffSet: double;
  leftSide, topSide, widthOfGraph, heightOfGraph: single;
  textDetails: TTextType;
  box: TBox;
  typeface: ISkTypeface;
  font: ISkFont;
  twidth, tHeight: single;
  ABounds: TRectF;
  LBlob: ISkTextBlob;
  rec: TRect;
  LPaint: ISkPaint;
begin
  leftSide := trunc(rxmin * (parentGraph as TRRGraph).Width);
  topSide := (parentGraph as TRRGraph).Height - trunc((rymin + rheight) * (parentGraph as TRRGraph).Height);
  widthOfGraph := round((parentGraph as TRRGraph).Width * rwidth);
  heightOfGraph := round((parentGraph as TRRGraph).Width * rheight);

  if not drawXLabelsOnAxes then
    exit;

  XOffSet := 0;
  str := labelXString(value);
  // // Prevent collision with the y axis
  // if str = '0' then
  // begin
  // if XAxisNearEdge then
  // XOffset := 0
  // else
  // XOffset := 0.018;
  // end
  // else
  // XOffset := 0;
  //

  LPaint := TSkPaint.Create;

  typeface := TSkTypeface.MakeFromName('Arial', TSkFontStyle.Normal);
  font := TSkFont.Create(typeface, 12, 1);
  // textDetails.font := font;

  font.MeasureText(str, ABounds, LPaint);
  twidth := ABounds.Width;
  tHeight := ABounds.Height;

  y := y + 1.5 * trunc(tHeight);
  x := x - 1.5 * trunc(twidth); // Center text on tick mark
  rec.left := trunc(x);
  rec.top := trunc(y);
  rec.Bottom := trunc(y + tHeight + 1);
  rec.Right := trunc(x + twidth + 1);

  LPaint.color := TAlphaColors.Black;
  LPaint.Style := TSkPaintStyle.Fill;

  LBlob := TSkTextBlob.MakeFromText(str, font);
  ACanvas.DrawTextBlob(LBlob, rec.Right, rec.top, LPaint);
end;

procedure TSubgraph.drawYLabel(ACanvas: ISkCanvas; value: double; x, y: single);
var
  str: string;
  YOffSet: double;
  ABounds: TRectF;
  t: TSkTextLayout;
  typeface: ISkTypeface;
  font: ISkFont;
  twidth, tHeight: single;
  LBlob: ISkTextBlob;
  rec: TRect;
begin
  str := labelYString(value);

  // Prevent collision with the x axis
  if str = '0' then
    begin
      if YAxisNearEdge then
        YOffSet := 0
      else
        YOffSet := 0.018;
    end
  else
    YOffSet := 0;

  LPaint.color := TAlphaColors.Black;
  LPaint.Style := TSkPaintStyle.Fill;

  typeface := TSkTypeface.MakeFromName('Arial', TSkFontStyle.Normal);
  font := TSkFont.Create(typeface, 12, 1);

  font.MeasureText(str, ABounds, LPaint);
  twidth := ABounds.Width;
  tHeight := ABounds.Height;

  y := y + tHeight / 2;
  rec.left := trunc(x);
  rec.top := trunc(y);

  LPaint.color := TAlphaColors.Black;
  LPaint.Style := TSkPaintStyle.Fill;

  LBlob := TSkTextBlob.MakeFromText(str, font);
  // x-tWidth ensures right-alignment
  ACanvas.DrawTextBlob(LBlob, x - twidth - 10, y, LPaint);

  // t := TSkTextLayout.Create(nil);
  // t.BeginUpdate;
  // t.Text := str;
  // t.Color := TAlphaColors.Black;
  // t.Font.Size := 12;
  // t.Font.Family := 'Arial';
  // t.Font.Style := [];
  // t.EndUpdate;
  // t.MaxSize := TPointF.Create(t.Width, t.Height);
  // t.TopLeft := TPointF.Create (x - t.Width - 10, y - t.Height/2);
  // t.RenderLayout(ACanvas);
end;

function TSubgraph.labelXString(d: double): string;
begin
  if abs(d) = 0.0 then
    begin
      result := '0';
      exit;
    end;
  if ((abs(d) > 0.000999) and (abs(d) < 99999)) then
    begin
      if round(d) = d then
        result := floattostrF(d, ffFixed, 5, 0)
      else
        result := floattostrF(d, ffGeneral, properties.XLabelPrecision, 3)
        { result := floattostrF (d, ffFixed, XLabelPrecision, 2) }
    end
  else
    result := floattostrF(d, ffExponent, 2, 0);
end;

function TSubgraph.labelYString(d: double): string;
begin
  if abs(d) = 0.0 then
    begin
      result := '0';
      exit;
    end;
  if ((abs(d) > 0.000999) and (abs(d) < 99999)) then
    begin
      if round(d) = d then
        result := floattostrF(d, ffFixed, 5, 0)
      else
        result := floattostrF(d, ffGeneral, properties.YLabelPrecision, 2)
        { result := floattostrF (d, ffFixed, XLabelPrecision, 2) }
    end
  else
    result := floattostrF(d, ffExponent, 2, 0);
end;

procedure TSubgraph.drawText(ACanvas: ISkCanvas; box: TBox; text: TTextType);
var
  x, y: double;
  twidth: double;
  LBlob: ISkTextBlob;
  ABounds: TRectF;
begin
  LPaint.color := TAlphaColors.Black;
  LPaint.Style := TSkPaintStyle.Fill;

  LBlob := TSkTextBlob.MakeFromText(text.value, text.font);
  ACanvas.DrawTextBlob(LBlob, box.left, box.top, LPaint);

  // textLayout := TTextLayoutManager.DefaultTextLayout.Create;
  // textLayout.BeginUpdate;
  // textLayout.Text := text.value;
  // textLayout.Color := text.color;
  // textLayout.Font.Size := text.size/fontScalingFactor;
  // textLayout.Font.Family := text.fontName;
  // textLayout.Font.Style := text.style;
  // textLayout.TopLeft := TPointF.Create (box.left, box.top);
  // textLayout.EndUpdate;
  //
  // textLayout.RenderLayout (Canvas);
end;

procedure TSubgraph.drawSolidLine(ACanvas: ISkCanvas; x1, y1, x2, y2: single; color: TAlphaColor; lineThickness: single);
var
  oldThick: single;
begin
  oldThick := LPaint.StrokeWidth;
  LPaint.StrokeWidth := lineThickness;

  if (parentGraph as TRRGraph).CurrentlyPrinting then
    begin
      ACanvas.drawLine(pointf(x1, y1), pointf(x2, y2), LPaint);
    end
  else
    begin
      try
        LPaint.StrokeWidth := lineThickness;
        ACanvas.drawLine(pointf(x1, y1), pointf(x2, y2), LPaint);
      finally
      end;
    end;
  LPaint.StrokeWidth := oldThick;
end;

procedure TSubgraph.drawLegendFrame(ACanvas: ISkCanvas; r: TRectF; Legend: TLegend);
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.color := Legend.outlineColor;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := Legend.frameBorderThickness;
  ACanvas.DrawRect(r, LPaint);
end;

procedure FillLegendRectangle(ACanvas: ISkCanvas; r: TRectF; InteriorColor: TAlphaColor);
var
  LPaint: ISkPaint;
begin
  LPaint := TSkPaint.Create;
  LPaint.color := InteriorColor;
  LPaint.Style := TSkPaintStyle.Fill;
  ACanvas.DrawRect(r, LPaint);
end;

procedure TSubgraph.drawLegend(ACanvas: ISkCanvas);
var
  aBox, txtPosition, legendBoxSize: TBox;
  i, j: integer;
  legendHeight, TextH, TextW, dummy, longestText: single; // str : string;
  hspace, lineLength, lineStart: single;
  x, y, wd: single;
  panel: TRRGraph;
  rBoxGraphingArea: TLogicalBox;
  myBox: TBox;
  txt: TTextType;
  xColumnIndex: integer;
  pt: TPointF;
  HosizontalGapDistance: single;
  LTextPaint: ISkPaint;

  LBlob: ISkTextBlob;
  r: TRectF;
  topLeftcorner: single;

  PathEffect: ISkPathEffect;
  widthOfLegend: single;
  Legend: TLegend;
begin
  if properties.dataBlocks.Count = 0 then
    exit;

  if not properties.Legend.visible then
    exit;

  Legend := properties.Legend;
  panel := parentGraph as TRRGraph;

  hspace := 0.3 / 2.54 * CurrentXPixelsPerInch; // Leave space of 3mm between text and symbol
  lineLength := computePhysicalSize(Legend.lineLengthInCms);
  // 1.2/2.54 * CurrentXPixelsPerInch; // Length of line sample segment on either side of symbol
  HosizontalGapDistance := 1.1;

  // How many lines will there be in the legend ?
  // How many datasets are there to plot?
  // What is the longest piece of text in the legend ?

  longestText := 0; // Stored longest piece of text in the legend

  // Find the longest piece of text
  txt := TTextType.Create('arial');
  for i := 0 to properties.dataBlocks.Count - 1 do
    begin
      for j := 0 to properties.dataBlocks[i].columns.Count - 1 do
        begin
          txt.value := properties.dataBlocks[i].columns[j].name;
          pt := txt.computeDimensions(LTextPaint);
          wd := pt.x;
          if wd > longestText then
            longestText := wd;
        end;
    end;

  widthOfLegend := longestText + hspace + lineLength;

  rBoxGraphingArea := getLogicalBoundingBox(graphingAreaId);
  // This will only compute the top/left corner of the legend box
  aBox := legend_relativeToDevice;
  topLeftcorner := aBox.top;
  legendBoxSize := aBox;

  LTextPaint := TSkPaint.Create;

  // Find the height of the legend
  legendHeight := 0;
  for i := 0 to properties.dataBlocks.Count - 1 do
    begin
      xColumnIndex := properties.dataBlocks[i].find(properties.dataBlocks[i].xaxisColumn);
      for j := 0 to properties.dataBlocks[i].columns.Count - 1 do
        begin
          if xColumnIndex <> j then
            begin
              txt.value := properties.dataBlocks[i].columns[j].name;
              pt := txt.computeDimensions(LTextPaint);
              TextW := pt.x;
              TextH := pt.y;

              if abs(TextH) < 1E-6 then
                TextH := 12;

              legendHeight := legendHeight + HosizontalGapDistance * 1.3 * TextH;
            end;
        end;
    end;

  if legendHeight = 0 then
    exit;

  hspace := computePhysicalSize(Legend.frameGapInCms);
  r := rectf(aBox.left - hspace, topLeftcorner - hspace / 2, aBox.left + widthOfLegend + hspace,
    topLeftcorner + legendHeight + hspace);

  if Legend.frameVisible then
    drawLegendFrame(ACanvas, r, Legend);

  FillLegendRectangle(ACanvas, r, Legend.InteriorColor);

  // Now draw the legend contents
  legendHeight := 0;
  aBox := legend_relativeToDevice;
  topLeftcorner := aBox.top;
  legendBoxSize := aBox;

  for i := 0 to properties.dataBlocks.Count - 1 do
    begin
      xColumnIndex := properties.dataBlocks[i].find(properties.dataBlocks[i].xaxisColumn);
      for j := 0 to properties.dataBlocks[i].columns.Count - 1 do
        begin
          // Don't show the x axis column
          if xColumnIndex <> j then
            begin
              txt.value := properties.dataBlocks[i].columns[j].name;
              pt := txt.computeDimensions(LPaint);
              TextW := pt.x;
              TextH := pt.y;

              if abs(TextH) < 1E-6 then
                TextH := 12;

              txtPosition := aBox;
              txtPosition.top := aBox.top + TextH;
              drawText(ACanvas, txtPosition, txt);

              // Draw line segment
              lineStart := aBox.left + longestText + 10;
              LPaint.color := properties.dataBlocks[i].columns[j].lineDetails.color;
              LPaint.StrokeWidth := properties.dataBlocks[i].columns[j].lineDetails.ThicknessInSkiaUnits;
              LPaint.PathEffect := nil;
              case properties.dataBlocks[i].columns[j].lineDetails.Style of
                lsDash:
                  begin
                    PathEffect := TSkPathEffect.MakeDash([15, 5, 15, 5], 1);
                    LPaint.PathEffect := PathEffect;
                  end;
                lsDot:
                  begin
                    PathEffect := TSkPathEffect.MakeDash([2, 2, 2, 2], 1);
                    LPaint.PathEffect := PathEffect;
                  end
              else
                begin
                  LPaint.PathEffect := nil;
                end;
              end;
              ACanvas.drawLine(pointf(lineStart, txtPosition.top - TextH / 2 + 2),
                pointf(lineStart + lineLength, txtPosition.top - TextH / 2 + 2), LPaint);

              // // Draw symbol on line segment
              x := lineStart + lineLength;
              y := aBox.top + TextH;
              x := x - lineLength / 2;
              y := y - TextH / 2;
              drawSymbolAtDeviceCoords(ACanvas, x, y, properties.dataBlocks[i].columns[j].symbol);
              aBox.top := aBox.top + HosizontalGapDistance * 1.3 * TextH;
              legendHeight := legendHeight + HosizontalGapDistance * 1.3 * TextH;
            end;
        end;
    end;

  // Now that we know the actual size of the legend box,
  // update the relative width and height of the legend box
  // This allows us to to a pt in legend mouse detect
  Legend.logicalBox.w := widthOfLegend / (rBoxGraphingArea.w * panel.Width);
  Legend.logicalBox.h := aBox.top / (rBoxGraphingArea.h * panel.Height);
end;

end.
