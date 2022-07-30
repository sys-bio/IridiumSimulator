unit UScrollingChart;

interface

uses SysUtils, Classes, System.Types, System.UIConsts, System.UITypes,
     skia, skia.FMX, FMX.Graphics, System.Contnrs, FMX.Dialogs, FMX.StdCtrls, FMX.Types,
     System.Generics.Defaults, UDataSource, UStage, UGlobalData;
type

  TScrollingChart = class (TSkCustomControl)
    private
      timer: TTimer;
      time: Double;
      obj: TObject;
      Sh: TShiftState;
      FMouseMoveEvent: TMouseMoveEvent;
      // methods to published properties
      function GetAxisColor: TAlphaColor;
      procedure SetAxisColor(Val: TAlphaColor);
      function GetBackgroundColor: TAlphaColor;
      procedure SetBackgroundColor(val: TAlphaColor);
      function GetGridColor: TAlphaColor;
      procedure SetGridColor(Val: TAlphaColor);
      function GetPanelBackgroundColor: TAlphaColor;
      procedure SetPanelBackgroundColor(val: TAlphaColor);
      function GetStroke: Double;
      procedure SetStroke(val: double);
      function GetGridStrokeWidth: Single;
      procedure SetGridStrokeWidth(Val: Single);
      function GetYAxisTicks: Integer;
      procedure SetYAxisTicks(val: Integer);
      function GetXAxisTicks: Integer;
      procedure SetXAxisTicks(val: Integer);
      function GetReferenceLegend: TPivot;
      procedure SetReferenceLegend(val: TPivot);
      function GetChartTitle: String;
      procedure SetChartTitle(val: String);
      function GetXAxisCaption: String;
      procedure SetXAxisCaption(val: String);
      function GetYAxisCaption: String;
      procedure SetYAxisCaption(val: String);
      function GetChartTitleColor: TAlphaColor;
      procedure SetChartTitleColor(val: TAlphaColor);
      procedure SetDeltaX(val: Single);
      function GetDeltaX: Single;
      function GetLegendFontColor: TAlphaColor;
      procedure SetLegendFontColor(val: TAlphaColor);
      function GetLegendBackgroundColor: TAlphaColor;
      procedure SetLegendBackgroundColor(val: TAlphaColor);
      function GetLegendPosition: TValuesLegend;
      procedure SetLegendPosition(val: TValuesLegend);
      function GetLegendPosX: Single;
      procedure SetLegendPosX(val: Single);
      function GetLegendPosY: Single;
      procedure SetLegendPosY(val: Single);
      function GetLegendVisible: Boolean;
      procedure SetLegendVisible(val: Boolean);
      function GetShowStrips: Boolean;
      procedure SetShowStrips(val: Boolean);

      //General
      procedure SetFMouseMoveEvent(val: TMouseMoveEvent);
      procedure SetDefaultValues;
      function IndexOf(aname: String): Integer;
      procedure Init;
      procedure Finalize;
      procedure OnComplete(X, Y: single);
      procedure FreeSeries;
      procedure InitStage;


      //Timer
      procedure SetDefaultTimer;
      procedure SetTimer(Value: TTimer);
      function GetTimer: TTimer;
      function GetInterval: Cardinal;
      procedure SetInterval(Value: Cardinal);
      procedure SetOnTimer(Value: TNotifyEvent);
      function GetOnTimer: TNotifyEvent;
      procedure SetEnabledTimer(Value: Boolean);
      function GetEnabledTimer: Boolean;


   strict protected
      procedure ChartOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
      procedure ChartOnMouseLeave(Sender: TObject);
      procedure FDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
      procedure Resize; override;

    public
      globaldata: TGlobalData;
      xAxis, yAxis: TInfoAxis;
      plane: TPlaneXY;
      legend: TLegend;
      title: TTitle;
      grid: TGridInfo;
      series: TSeries;
      stage: TStage;
      autoScaleUp, autoScaleDown: Boolean;
      xMinIni: Double;

      constructor Create (AOwner: TComponent); override;
      destructor  Destroy; override;

      function AddSerie(functionTime: TFunctionTime; aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie; overload;
      function AddSerie(aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie; overload;

      function AddSerieByName(aname: String; functionTime: TFunctionTime = nil): TDataSerie;
      procedure DeleteSerie(index: Integer);
      procedure DeleteSeries;
      procedure Pause;
      procedure Resume;
      procedure Restart;
      procedure RestartSeries;
      procedure Run(t: double);
      procedure SaveToFile(FileName: String);
      procedure SaveToPDF(FileName: String);

      procedure SetXAxisRange(xMin, xMax: double);
      procedure SetYAxisRange(yMin, yMax: double);
      procedure Update;


      property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
      property Enabled: Boolean read GetEnabledTimer write SetEnabledTimer default false;
      property Interval: Cardinal read GetInterval write SetInterval default 100;


      property XAxisTicks: Integer read GetXAxisTicks write SetXAxisTicks;
      property YAxisTicks: Integer read GetYAxisTicks write SetYAxisTicks;

      property ReferenceLegend: TPivot read GetReferenceLegend write SetReferenceLegend;


    published

      property AxisColor: TAlphaColor read GetAxisColor write SetAxisColor;
      property AxisStrokeWidth: Double read GetStroke write SetStroke;
      property BackgroundColor: TAlphaColor read GetBackgroundColor write SetBackgroundColor;
      property ChartTitle: String read GetChartTitle write SetChartTitle;
      property ChartTitleColor: TAlphaColor read GetChartTitleColor write SetChartTitleColor;
      property DeltaX: Single read GetDeltaX write SetDeltaX;
      property ExternalTimer: TTimer read GetTimer write SetTimer default nil;
      property GridColor: TAlphaColor read GetGridColor write SetGridColor;
      property GridStrokeWidth: Single read GetGridStrokeWidth write SetGridStrokeWidth;

      property LegendFontColor: TAlphaColor read GetLegendFontColor write SetLegendFontColor;
      property LegendBackgroundColor: TAlphaColor read GetLegendBackgroundColor write SetLegendBackgroundColor;
      property LegendReference: TValuesLegend read GetLegendPosition write SetLegendPosition;
      property LegendPosX: Single read GetLegendPosX write SetLegendPosX;
      property LegendPosY: Single read GetLegendPosY write SetLegendPosY;
      property LegendVisible: Boolean read GetLegendVisible write SetLegendVisible;
      property OnMouseWorldMove: TMouseMoveEvent read FMouseMoveEvent write SetFMouseMoveEvent;
      property PlotPanelBackgroundColor: TAlphaColor read GetPanelBackgroundColor write SetPanelBackgroundColor;
      property ShowStrips: Boolean read GetShowStrips write SetShowStrips;
      property XAxisCaption: String read GetXAxisCaption write SetXAxisCaption;
      property YAxisCaption: String read GetYAxisCaption write SetYAxisCaption;

end;



procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('UofW', [TScrollingChart]);
end;

function TScrollingChart.GetShowStrips: Boolean;
begin
  Result := grid.ShowStrips;
end;

procedure TScrollingChart.SetShowStrips(val: Boolean);
begin
  if val <> grid.ShowStrips then
    begin
      grid.ShowStrips := val;
      Redraw;
    end;
end;

function TScrollingChart.GetLegendVisible: Boolean;
begin
  Result := legend.visible;
end;

procedure TScrollingChart.SetLegendVisible(val: Boolean);
begin
  if val <> legend.visible then
    begin
      legend.visible := val;
      Redraw;
    end;
end;

function TScrollingChart.GetLegendFontColor: TAlphaColor;
begin
  Result := legend.FontColor;
end;

procedure TScrollingChart.SetLegendFontColor(val: TAlphaColor);
begin
   if val <> legend.FontColor then
     begin
       legend.FontColor := val;
       Redraw;
     end;
end;

function TScrollingChart.GetLegendBackgroundColor: TAlphaColor;
begin
   Result := legend.backgroundColor;
end;

procedure TScrollingChart.SetLegendBackgroundColor(val: TAlphaColor);
begin
   if val <> legend.backgroundColor then
     begin
       legend.backgroundColor := val;
       Redraw;
     end;
end;

function TScrollingChart.GetDeltaX: Single;
begin
  Result := globalData.DeltaX;
end;

procedure TScrollingChart.SetDeltaX(val: Single);
begin
   if val <> globalData.DeltaX then
     begin
       globalData.DeltaX := val;
       Redraw;
     end;
end;

function TScrollingChart.GetReferenceLegend: TPivot;
begin
  Result := legend.pivot;
end;

function TScrollingChart.GetXAxisCaption: String;
begin
  Result := xAxis.caption;
end;

procedure TScrollingChart.SetXAxisCaption(val: String);
begin
  if val <> xAxis.caption then
    begin
      xAxis.caption := val;
      redraw;
    end;
end;

function TScrollingChart.GetYAxisCaption: String;
begin
  Result := yAxis.caption;
end;

procedure TScrollingChart.SetYAxisCaption(val: String);
begin
  if val <> yAxis.caption then
    begin
      yAxis.caption := val;
      redraw;
    end;
end;

function TScrollingChart.GetChartTitleColor: TAlphaColor;
begin
  Result := title.fontColor;
end;

procedure TScrollingChart.SetChartTitleColor(val: TAlphaColor);
begin
  if val <> title.fontColor then
    begin
      title.fontColor := val;
      Redraw;
    end;
end;

function TScrollingChart.GetChartTitle: String;
begin
  Result := title.text;
end;

procedure TScrollingChart.SetChartTitle(val: String);
begin
  if val <> title.text then
    begin
      title.text := val;
      redraw;
    end;
end;

procedure TScrollingChart.SetReferenceLegend(val: TPivot);
var
  k: Integer;
begin
  k := legend.getPos(val);
  if k <> legend.Position then
    begin
      legend.Position := k;
      Redraw;
    end;
end;

function TScrollingChart.GetLegendPosX: Single;
begin
  Result := legend.x;
end;

procedure TScrollingChart.SetLegendPosX(val: Single);
begin
  if val <> legend.x then
    begin
      legend.x := val;
      Redraw;
    end;
end;

function TScrollingChart.GetLegendPosY: Single;
begin
   Result := legend.y;
end;

procedure TScrollingChart.SetLegendPosY(val: Single);
begin
  if val <> legend.y then
    begin
      legend.y := val;
      Redraw;
    end;
end;

function TScrollingChart.GetLegendPosition: TValuesLegend;
begin
  Result := legend.Position;
end;

procedure TScrollingChart.SetLegendPosition(val: TValuesLegend);
begin
  if val <> legend.Position then
    begin
      legend.Position := val;
      Redraw;
    end;
end;

function TScrollingChart.GetYAxisTicks: Integer;
begin
  Result := yAxis.maxTicks;
end;
procedure TScrollingChart.SetYAxisTicks(val: Integer);
begin
  if val <> yAxis.maxTicks then
    begin
      yAxis.maxTicks := val;
      Redraw;
    end;
end;
function TScrollingChart.GetXAxisTicks: Integer;
begin
  Result := xAxis.maxTicks;
end;
procedure TScrollingChart.SetXAxisTicks(val: Integer);
begin
  if val <> xAxis.maxTicks then
    begin
      xAxis.maxTicks := val;
      redraw;
    end;
end;

function TScrollingChart.GetBackgroundColor: TAlphaColor;
begin
  Result := globaldata.BackgroundColor;
end;

procedure TScrollingChart.SetBackgroundColor(val: TAlphaColor);
begin
  if val <> globaldata.BackgroundColor then
    begin
      globaldata.BackgroundColor := val;
      redraw;
    end;
end;

function TScrollingChart.GetPanelBackgroundColor: TAlphaColor;
begin
  Result := globaldata.plotPanelBackgroundColor;
end;

procedure TScrollingChart.SetPanelBackgroundColor(val: TAlphaColor);
begin
  if val <> globaldata.plotPanelBackgroundColor then
    begin
      globaldata.plotPanelBackgroundColor := val;
      redraw;
    end;
end;

procedure TScrollingChart.SetFMouseMoveEvent(val: TMouseMoveEvent);
begin
  FMouseMoveEvent := val;
  if Assigned(FMouseMoveEvent) then
    begin
      HitTest := true;
      OnMouseMove := chartOnMouseMove;
      OnMouseLeave := chartOnMouseLeave;
    end
  else
    begin
      HitTest := false;
      OnMouseMove := nil;
      OnMouseLeave := nil;
      if stage <> nil then stage.rightBox.graph.FOnComplete := nil;
    end;
end;

procedure TScrollingChart.chartOnMouseLeave(Sender: TObject);
begin
  if stage <> nil then
    stage.rightBox.graph.FOnComplete := nil;
end;

procedure TScrollingChart.OnComplete(X, Y: single);
begin
  FMouseMoveEvent(Obj, Sh, X, Y);

  // If you don't want the OnMouseWorldMove Event to detect
  // relative movement between the mouse and the plane.
  // Uncomment this code

  {
  if stage <> nil then
    stage.rightBox.graph.FOnComplete := nil;
    }
end;

procedure TScrollingChart.chartOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  if stage = nil then Exit;

  if (Assigned(FMouseMoveEvent)) and (stage.rightBox.graph.isInside(X, Y)) then
     begin

       if timer.Enabled then
         begin

           if not Assigned(stage.rightBox.graph.FOnComplete) then
             stage.rightBox.graph.FOnComplete:= OnComplete;
           stage.rightBox.graph.scanXY(X, Y);
           Obj := Sender;
           Sh := Shift;

         end
       else
         begin
           stage.rightBox.graph.FOnComplete := nil;
           P := stage.rightBox.graph.worldCoordinates(X, Y);
           FMouseMoveEvent(Sender, Shift, P.X, P.Y);
         end;

     end
   else
     if Assigned(stage.rightBox.graph.FOnComplete) then
       stage.rightBox.graph.FOnComplete := nil;

end;

procedure TScrollingChart.saveToPDF(FileName: String);
var
  LPDFStream: TStream;
  LDocument: ISkDocument;
  LCanvas, currentCanvas: ISkCanvas;
  status: Boolean;
begin
  LPDFStream := TFileStream.Create(fileName, fmCreate);
  status := timer.Enabled;
  try
    timer.Enabled := false;
    currentCanvas := stage.data.canvas;
    LDocument := TSkDocument.MakePDF(LPDFStream);
    try
      LCanvas := LDocument.BeginPage(Width, Height);
      try
        LCanvas.Save;
        try
          LCanvas.Clear(backgroundColor);
          stage.data.canvas := LCanvas;
          stage.Draw;
       finally
         LCanvas.Restore;
       end;
      finally
        LDocument.EndPage;
      end;
    finally
      LDocument.Close;
    end;
  finally
    LPDFStream.Free;
    stage.data.canvas := currentCanvas;
    Redraw;
    timer.Enabled := status;
  end;

end;

procedure TScrollingChart.saveToFile(FileName: String);
var
  i, j: Integer;
  col: PDataCol;
  row: PData;
  L, aux: TstringList;
begin
  aux := TStringList.Create;
  L := TStringList.Create;
  L.Add('"Time",');
  for i := 0 to length(series) - 1 do
    begin
      L[L.Count - 1] := L[L.Count - 1] + '"' + series[i].name + '"';
      if i <> length(series) - 1 then L[L.Count - 1] := L[L.Count - 1] + ',';
    end;

  for i :=0 to globaldata.dataSource.cols.Count - 1 do
    begin
      col := globaldata.dataSource.cols[i];
      for j := 0 to length(series) - 1 do aux.Values[series[j].name] := '""';

      L.Add(globaldata.niceNumX(col^.x) + ',');
      for j := 0 to col^.rows.Count - 1 do
        begin
          row := col^.rows[j];
          aux.Values[row^.serie.name] := globaldata.niceNumY(row^.y);
        end;

      for j := 0 to length(series) - 1 do
        begin
          L[L.Count - 1] := L[L.Count - 1] + aux.Values[series[j].name];
          if j <> length(series) - 1 then L[L.Count - 1] := L[L.Count - 1] + ',';
        end;
    end;

  aux.Free;
  L.SaveToFile(FileName);
  L.Free;
end;

function TScrollingChart.GetStroke: Double;
begin
  Result := yAxis.lineWidth;
end;

procedure TScrollingChart.SetStroke(val: double);
begin
  if val <> xAxis.lineWidth then
    begin
      xAxis.lineWidth := val;
      yAxis.lineWidth := val;
      redraw;
    end;
end;

function TScrollingChart.GetAxisColor: TAlphaColor;
begin
  Result := xAxis.color;
end;

procedure TScrollingChart.SetAxisColor(Val: TAlphaColor);
begin
  if val <> xAxis.color then
    begin
      xAxis.color := val;
      yAxis.color := val;
      redraw;
    end;
end;

function TScrollingChart.GetGridStrokeWidth: Single;
begin
  Result := grid.lineWidth;
end;

procedure TScrollingChart.SetGridStrokeWidth(Val: Single);
begin
  if val <> grid.lineWidth then
    begin
      grid.lineWidth := Val;
      redraw;
    end;
end;

function TScrollingChart.GetGridColor: TAlphaColor;
begin
  Result := grid.color;
end;

procedure TScrollingChart.SetGridColor(Val: TAlphaColor);
begin
  if val <> grid.color then
    begin
      grid.color := val;
      redraw;
    end;
end;

procedure TScrollingChart.SetDefaultTimer;
begin
  if timer = nil then
    begin
      timer := TTimer.Create(self);
      timer.Enabled := false;
      timer.Interval := 100;
    end;
end;

procedure TScrollingChart.SetTimer(Value: TTimer);
begin
  timer := Value;
end;

procedure TScrollingChart.restart;
begin
  if stage = nil then exit;

  restartSeries;
  globaldata.dataSource.reset;
  plane.x := xMinIni;
  stage.rightBox.bottomBox.axisX.reset;
  stage.rightBox.graph.gridX.reset;
end;

function TScrollingChart.GetTimer: TTimer;
begin
  Result := timer;
end;

procedure TScrollingChart.SetOnTimer(Value: TNotifyEvent);
begin
  SetDefaultTimer;
  timer.OnTimer := Value;
end;

function TScrollingChart.GetOnTimer: TNotifyEvent;
begin
  Result := timer.OnTimer;
end;

procedure TScrollingChart.SetEnabledTimer(Value: Boolean);
begin
  SetDefaultTimer;
  timer.Enabled := Value;
end;

function TScrollingChart.GetEnabledTimer: Boolean;
begin
  Result := timer.Enabled;
end;

procedure TScrollingChart.SetInterval(Value: Cardinal);
begin
  SetDefaultTimer;
  timer.Interval := Value;
end;

function TScrollingChart.GetInterval: Cardinal;
begin
  Result := timer.Interval;
end;

procedure TScrollingChart.pause;
begin
  timer.Enabled := false;
end;

procedure TScrollingChart.resume;
begin
  timer.Enabled := true;
end;

function TScrollingChart.IndexOf(aname: String): Integer;
var
  j: Integer;
begin
  for j := 0 to length(series) - 1 do
     if series[j].name.ToLower = aname.ToLower then
       begin
         Result := j;
         Exit;
       end;
  Result := -1;
end;

function TScrollingChart.addSerieByName(aname: String; functionTime: TFunctionTime = nil): TDataSerie;
begin
   Result:= TDataSerie.Create;
   Result.name := aname;
   Result.functionTime := functionTime;
   Result.dataSource := globalData.dataSource;
   series := series + [Result];
end;


function TScrollingChart.addSerie(aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie;
var
  temp: String;
begin
  if aname = TConst.DEFAULT_NAME_SERIES then
    temp := Format(aname, [length(series) + 1])
  else
    temp := aname;

  if IndexOf(temp) = -1 then
      Result := addSerieByName(temp)
  else
    begin
      ShowMessage(TConst.ERROR_NAME_SERIE);
      Result := nil;
    end;
end;


function TScrollingChart.addSerie(functionTime: TFunctionTime; aname: String = TConst.DEFAULT_NAME_SERIES): TDataSerie;
var
  temp: String;
begin
  if aname = TConst.DEFAULT_NAME_SERIES then
    temp := Format(aname, [length(series) + 1])
  else
    temp := aname;

  if IndexOf(temp) = -1 then
      Result := addSerieByName(temp, functionTime)
  else
    begin
      ShowMessage(TConst.ERROR_NAME_SERIE);
      Result := nil;
    end;
end;

procedure TScrollingChart.deleteSerie(index: Integer);
begin
  if (index >= 0) and (index <= length(series) - 1) then
    begin
      globalData.dataSource.deleteSerie(series[index]);
      series[index].Destroy;
      Delete(series, index, 1);
      Redraw;
    end;
end;

procedure TScrollingChart.deleteSeries;
var
  j: Integer;
begin
  globalData.dataSource.deleteAllSeries;
  for j := length(series) - 1 downto 0 do
    begin
      series[j].Destroy;
      Delete(series, j, 1);
    end;
  Redraw;
end;

procedure TScrollingChart.restartSeries;
var
  j: Integer;
begin
  globalData.dataSource.deleteAllSeries;
  for j := length(series) - 1 downto 0 do series[j].init;
end;

procedure TScrollingChart.run(t: double);
var
  j: Integer;
  y: double;
begin
  globaldata.dataSource.addX(t);

  for j := 0 to length(series) - 1 do
    if Assigned(series[j].functionTime) then
      begin
        y := series[j].functionTime(t);
        series[j].addY(y);
      end;

  update;
end;

procedure TScrollingChart.update;
begin
  Redraw;
  stage.checkLimits;
  time := time + timer.Interval/1000;
end;

procedure TScrollingChart.setDefaultValues;
begin
  setXAxisRange(TConst.DEFAULT_X_MIN, TConst.DEFAULT_X_MAX);
  setYAxisRange(TConst.DEFAULT_Y_MIN, TConst.DEFAULT_Y_MAX);

  //autoScale := false;
  autoScaleUp := false;
  autoScaleDown := false;
  time := 0;
end;

procedure TScrollingChart.setXAxisRange(xMin, xMax: double);
begin
  xMinIni := xMin;
  time := xMin;
  plane.setXAxisRange(xMin, xMax);
end;

procedure TScrollingChart.setYAxisRange(yMin, yMax: double);
begin
  plane.initialValueYMin := yMin;
  plane.initialValueYMax := yMax;
  plane.setYAxisRange(yMin, yMax);
end;

procedure TScrollingChart.Resize;
begin
  inherited;

  if globalData = nil then Exit;

  if stage <> nil then
    begin
      globalData.chartWidth := Width;
      globalData.chartHeight := Height;

      stage.width := Width - 2*TConst.MARGIN_X;
      stage.height := Height - 2*TConst.MARGIN_Y;

      stage.x := TConst.MARGIN_X;
      stage.y := TConst.MARGIN_Y;
      stage.resize;

    end;
end;

constructor TScrollingChart.Create (AOwner: TComponent);
begin
  inherited;
  width := 320;
  height := 240;
  Init;
end;

procedure TScrollingChart.Finalize;
begin
     //timer.Enabled := false;
  OnDraw := nil;
  OnResize := nil;
  if globalData <> nil then globalData.Destroy;
  if stage <> nil then stage.Destroy;
  FreeSeries;
  if plane <> nil then plane.Destroy;
  if legend <> nil then legend.Destroy;
   //timer.destroy;
end;

procedure TScrollingChart.Init;
begin
  HitTest := false;
   if globalData = nil then
     begin
       globalData := TGlobalData.Create;
       title := globalData.title;
       globalData.dataSource.Redraw := Redraw;

       plane := TPlaneXY.Create;
       xAxis := plane.xAxis;
       yAxis := plane.yAxis;
       grid := plane.grid;

       setDefaultValues;

       series:= [];
       legend := TLegend.Create;
       timer := nil;

       OnDraw := FDraw;
   end;
end;

procedure TScrollingChart.initStage;
begin
  if stage = nil then
    begin
      SetDefaultTimer;
      //globalData.dataSource.setInitValues(timer.Interval/1000, FDeltaX);
      stage := TStage.Create(Width - 2*TConst.MARGIN_X, Height - 2*TConst.MARGIN_Y, self);
      stage.x := TConst.MARGIN_X;
      stage.y := TConst.MARGIN_Y;
    end;
end;

procedure TScrollingChart.FDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  if globalData = nil then Exit;

  ACanvas.Save;
  try
    globalData.chartWidth := Width;
    globalData.chartHeight := Height;
    globalData.canvas := ACanvas;
    InitStage;
    ACanvas.ClipRect(ADest, TSkClipOp.Intersect, true);
    ACanvas.Clear(backgroundColor);
    stage.Draw;

  finally
    ACanvas.Restore;
  end;
end;


procedure TScrollingChart.FreeSeries;
var
  j: Integer;
begin
  for j := 0 to length(series) - 1 do series[j].Destroy;
  series := nil;
end;

destructor  TScrollingChart.Destroy;
begin
   Finalize;
   inherited;
end;

initialization

end.
