unit UStage;

interface
  uses
   Math, System.Types, System.UITypes, System.UIConsts, System.Contnrs,
   Skia, FMX.skia, System.SysUtils, UContainer, UComps, UDataSource,
   FMX.Dialogs, FMX.StdCtrls, UGlobalData;

type


  TStage = class (TContainer)
     leftBox: TLeftBox;
     rightBox: TRightBox;
     titleBox: TTitleBox;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure checkLimits;
     procedure resize;
     procedure borderDraw;
  end;

implementation


destructor TStage.Destroy;
begin
   leftBox.Destroy;
   rightBox.Destroy;
   titleBox.Destroy;
end;

procedure TStage.checkLimits;
var
  xMin, xMax: Double;
  plane: TPlaneXY;
  dataSource: TDataSource;
begin
  dataSource := data.dataSource;
  if dataSource.cols.Count = 0 then Exit;

  plane := FPlane;
  if dataSource.isOut(plane.width) then
    begin
      dataSource.removeFirst;
      dataSource.getNewLimits(xMin, xMax);
      plane.x := xMin;
      rightBox.bottomBox.axisX.update;
      rightBox.graph.gridX.update;
    end;
end;

procedure TStage.resize;
begin
   titleBox.x := 0;
   titleBox.y := 0;
   titleBox.width := width;


   leftBox.width := TConst.WIDTH_Y_AXIS;
   leftBox.height := height - titleBox.height;
   leftBox.x := 0;
   leftBox.y := titleBox.height;


   rightBox.width := width - leftBox.width;
   rightBox.height := height - titleBox.height;
   rightBox.x := leftBox.width;
   rightBox.y := titleBox.height;

   leftBox.resize;
   rightBox.resize;

end;

constructor TStage.Create(w, h: double; P: TObject);
var
  wLeftBox: single;
  title: TTitle;
begin
  parent := P;
  width := w;
  height := h;
  title := data.title;

  //backgroundColor := clayellow; //DEFAULT_BACKGROUND_COLOR;

  titleBox := TTitleBox.Create(w, 2*title.pad + title.fontSize, self);
  titleBox.x := 0;
  titleBox.y := 0;

  wLeftBox := TConst.WIDTH_Y_AXIS;

  leftBox := TLeftBox.Create(wLeftBox, h - titleBox.height, self);
  leftBox.backgroundColor := claAzure;
  leftBox.x := 0;
  leftBox.y := titleBox.height;

  rightBox := TRightBox.Create(w - wLeftBox, h - titleBox.height, self);
  rightBox.backgroundColor := data.plotPanelBackgroundColor;
  rightBox.x := wLeftBox;
  rightBox.y := titleBox.height;
end;

procedure TStage.borderDraw;
var
  paint: ISkPaint;
  R: TRectF;
begin
  paint := TSkPaint.Create;
  paint.AntiAlias := true;
  paint.StrokeWidth := FPlane.yAxis.lineWidth;
  paint.color := FPlane.yAxis.color;
  paint.Style := TSkPaintStyle.Stroke;

  R := TRectF.Create(TPointF.Create(0, 0), data.chartWidth, data.chartHeight);
  data.canvas.DrawRect(R, paint);
end;

procedure TStage.Draw;
var
  plane: TPlaneXY;
  dataSource: TDataSource;
begin
  plane := FPlane;
  dataSource := data.dataSource;
  if (abs(dataSource.maxY - dataSource.minY) < abs(MAX_VALUE_AXIS_Y - MIN_VALUE_AXIS_Y)) then
    begin

      if (data.autoScaleUp) and (not data.autoScaleDown) then
          plane.setYAxisRange(plane.initialValueYMin, dataSource.maxY)
      else
      if (data.autoScaleUp) and (data.autoScaleDown) then
          plane.setYAxisRange(dataSource.minY, dataSource.maxY)
      else
      if (not data.autoScaleUp) and (data.autoScaleDown) then
          plane.setYAxisRange(dataSource.minY, plane.initialValueYMax)
      else
          plane.setYAxisRange(plane.initialValueYMin, plane.initialValueYMax);

    end;

  //clear;

  rightBox.Draw;
  titleBox.draw;
  leftBox.Draw;

  borderDraw;

end;

end.
