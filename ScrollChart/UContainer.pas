unit UContainer;

interface
uses
  System.Classes, System.UITypes, System.UIConsts, System.SysUtils,
  System.Types, Skia, FMX.skia, UGlobalData;

  type
    TOrientation = (horizontal, vertical);
    TPosition = (start, half, last);

    TContainer = class (TObject)
      FRoot: TObject;
      //fdata: TGlobalData;
      x, y, width, height: double;
      //scaleX, scaleY: double;
      parent: TObject;
      name: string;
      //childs: TObjectList;
      color, backgroundColor : TAlphaColor;
      lineWidth: single;
      constructor Create(w, h: double; P: TObject); virtual;

      procedure Clear;
      function getFRoot: TObject;
      function getGlobalPosition: TPointF;

      function getPlane: TPlaneXY;
      procedure setPlane(value: TPlaneXY);

      function getLegend: TLegend;
      procedure setLegend(value: TLegend);


      function getAGlobalData: TGlobalData;
      procedure setGlobalData(value: TGlobalData);
      property data: TGlobalData read getAGlobalData write setGlobalData;
      property FPlane: TPlaneXY read getPlane write setPlane;
      property legend: TLegend read getLegend write setLegend;


      property Root: TObject read getFRoot;
  end;

  TMyLine = class (TContainer)
      forientation: TOrientation;
      fpos: TPosition;
      constructor Create(orientation: TOrientation; pos: TPosition; P: TObject); reintroduce; overload;
      procedure drawLine;
  end;

  TMask = class(TContainer)
      clientWindow: TContainer;
      constructor Create(w, h: double; client: TContainer; P: TObject); reintroduce; overload;
      procedure Draw;
  end;

  TMyText = class (TContainer)
      FText: String;
      typeface : ISkTypeface;
      font: ISkFont;
      hAlign: THAlign;
      vAlign: TVAlign;
      margin: Single;
      rotate: Boolean;
      LBlob: ISkTextBlob;
      P: TPointF;
      ABounds: TRectF;
      constructor Create(fontName: String; Size: Integer; _hAlign: THAlign; _vAlign: TVAlign;_margin: Single; P: TObject); reintroduce; overload;
      procedure writeText;
      function MakeFromText: TPointF;
      function MakeFromTextTransform: TPointF;
  end;

implementation

uses
  UScrollingChart;

constructor TMask.Create(w, h: double; client: TContainer; P: TObject);
begin
   parent := P;
   width := w;
   height := h;
   clientWindow := client;
end;

procedure TMask.Draw;
var
  paint: ISkPaint;
  R: TRectF;
  topLeft: TPointF;
  P: TPointF;
  //blender: ISkBlender;
begin

  P := clientWindow.getGlobalPosition + TPointF.Create(x, y);
  paint := TSkPaint.Create;
  //blender := TSkBlender.MakeMode(TSkBlendMode.Clear);

  //paint.Blender := blender;

  paint.AntiAlias := false;
  paint.StrokeWidth := 1;
  paint.color := data.BackgroundColor;
  paint.Style := TSkPaintStyle.Fill;

  topLeft := TPointF.Create(0, 0);
  R := TRectF.Create(topLeft, data.chartWidth, P.y);

  data.canvas.DrawRect(R, paint);


  topLeft := TPointF.Create(0, P.y + height);
  R := TRectF.Create(topLeft, data.chartWidth, data.chartHeight - P.y - height);
  data.canvas.DrawRect(R, paint);


  topLeft := TPointF.Create(0, P.y);
  R := TRectF.Create(topLeft, P.x, height);
  data.canvas.DrawRect(R, paint);


  topLeft := TPointF.Create(P.x + width, P.y);
  R := TRectF.Create(topLeft, data.chartWidth - P.x - width, height);
  data.canvas.DrawRect(R, paint);
end;

procedure TContainer.setGlobalData(value: TGlobalData);
var
  chart: TScrollingChart;
begin
  chart := Root as TScrollingChart;
  chart.globaldata := value;
end;

function TContainer.getFRoot: TObject;
var
  C: TContainer;
begin
  if FRoot = nil then
    begin
      FRoot := parent;
      while not (FRoot is TScrollingChart) do
        begin
          C := FRoot as TContainer;
          FRoot := C.parent;
        end;
    end;

  Result := FRoot;
end;

function TContainer.getLegend: TLegend;
var
  chart: TScrollingChart;
begin
  chart := Root as TScrollingChart;
  Result := chart.legend;
end;

procedure TContainer.setLegend(value: TLegend);
var
  chart: TScrollingChart;
begin
  chart := Root as TScrollingChart;
  chart.legend := value;
end;

function TContainer.getPlane: TPlaneXY;
var
  chart: TScrollingChart;
begin
  chart := Root as TScrollingChart;
  Result := chart.plane;
end;

procedure TContainer.setPlane(value: TPlaneXY);
var
  chart: TScrollingChart;
begin
  chart := Root as TScrollingChart;
  chart.plane := value;
end;

function TContainer.getAGlobalData: TGlobalData;
var
  chart: TScrollingChart;
begin
  chart := Root as TScrollingChart;
  chart.globalData.series := chart.series;
  chart.globaldata.autoScaleUp := chart.autoScaleUp;
  chart.globaldata.autoScaleDown := chart.autoScaleDown;
  Result := chart.globalData;
end;

function TContainer.getGlobalPosition: TPointF;
var
  P: TContainer;
  Q: TObject;
begin
  Result := TPoint.Zero;
  Q := Parent;
  while Q is TContainer do
    begin
      P := Q as TContainer;
      Result := Result + TPointF.Create(P.x, P.y);
      Q := P.parent;
    end;
end;

constructor TContainer.Create(w, h: double; P: TObject);
begin
   parent := P;
   width := w;
   height := h;
   backgroundColor := claWhitesmoke; //Default color
   x := 0;
   y := 0;
   color := claBlack;
   lineWidth := 1;
end;

procedure TContainer.Clear;
var
  paint: ISkPaint;
  R: TRectF;
  topLeft: TPointF;
begin
  if data.canvas <> nil then
    begin
      paint := TSkPaint.Create;
      paint.AntiAlias := true;
      paint.StrokeWidth := 1;
      paint.color := backgroundColor;
      paint.Style := TSkPaintStyle.Fill;
      topLeft := getGlobalPosition + TPointF.Create(x, y);
      R := TRectF.Create(topLeft, width, height);
      data.canvas.DrawRect(R, paint);
    end;
end;

constructor TMyLine.Create(orientation: TOrientation; pos: TPosition; P: TObject);
begin
  parent := P;
  forientation := orientation;
  fpos := pos;
end;

procedure TMyLine.drawLine;
var
  paint: ISkPaint;
  origen, destino, P: TPointF;
begin
  //data := getGlobalData;
  P := getGlobalPosition;

  paint := TSkPaint.Create;
  paint.AntiAlias := true;
  paint.Color := color;
  paint.StrokeWidth := lineWidth;
  paint.Style := TSkPaintStyle.Fill;

  if (forientation = vertical ) and (fpos = half) then
    begin
       origen := TPointF.Create(x + width/2, y);
       destino := TPointF.Create(x + width/2, y + height);
    end
  else
  if (forientation = vertical ) and (fpos = start) then
    begin
      origen := TPointF.Create(x, y);
      destino := TPointF.Create(x, y + height);
    end
  else
  if (forientation = vertical ) and (fpos = last) then
    begin
      origen := TPointF.Create(x + width, y);
      destino := TPointF.Create(x + width, y + height);
    end
  else
  if (forientation = horizontal ) and (fpos = start) then
    begin
      origen := TPointF.Create(x, y);
      destino := TPointF.Create(x + width, y);
    end
  else
  if (forientation = horizontal ) and (fpos = half) then
    begin
      origen := TPointF.Create(x, y + height/2);
      destino := TPointF.Create(x + width, y + height/2);
    end
  else
  if (forientation = horizontal ) and (fpos = last) then
    begin
      origen := TPointF.Create(x, y + height);
      destino := TPointF.Create(x + width, y + height);
    end;

  origen := P + origen;
  destino := P + destino;
  //if data.canvas <> nil then
  data.canvas.DrawLine(origen, destino, paint);
end;

constructor TMyText.Create(fontName: String; Size: Integer; _hAlign: THAlign; _vAlign: TVAlign;_margin: Single; P: TObject);
begin
  parent := P;
  typeface := TSkTypeface.MakeFromName(fontName, TSkFontStyle.Normal);
  font := TSkFont.Create(typeface, Size, 1);
  hAlign := _hAlign;
  vAlign := _vAlign;
  backgroundColor := claAqua;
  rotate := false;
  FText := '';
  color := claBlack;
  margin := _margin;
end;

function TMyText.MakeFromText: TPointF;
var
  tx, ty: single;
begin
  LBlob := TSkTextBlob.MakeFromText(FText, font);
  tx := P.x + x;
  ty := P.y + y + ABounds.Height;

  if (hAlign = center) and (vAlign = top) then
    begin
       tx := tx + (width - ABounds.Width)/2;
       ty := ty + margin;
    end
  else
  if (hAlign = left) and (vAlign = top) then
    begin
       tx := tx + margin;
       ty := ty + margin;
    end
  else
  if (hAlign = right) and (vAlign = top) then
    begin
       tx := tx + width - ABounds.Width - margin;
       ty := ty + margin;
    end
  else
  if (hAlign = left) and (vAlign = middle) then
    begin
       tx := tx + margin;
       ty := ty + (height - ABounds.height)/2;
    end
  else
  if (hAlign = center) and (vAlign = middle) then
    begin
       tx := tx + (width - ABounds.Width)/2;
       ty := ty + (height - ABounds.height)/2;
    end
  else
  if (hAlign = right) and (vAlign = middle) then
    begin
       tx := tx + width - ABounds.Width - margin;
       ty := ty + (height - ABounds.height)/2;
    end
  else
  if (hAlign = left) and (vAlign = bottom) then
    begin
       tx := tx + margin;
       ty := ty + height - margin;
    end
  else
  if (hAlign = center) and (vAlign = bottom) then
    begin
       tx := tx + (width - ABounds.Width)/2;
       ty := ty + height - margin;
    end
  else
  if (hAlign = right) and (vAlign = bottom) then
    begin
       tx := tx + width - ABounds.Width - margin;
       ty := ty + height - margin;
    end;

   Result.x := tx;
   Result.y := ty;
end;

function TMyText.MakeFromTextTransform: TPointF;
var
  AMatrices: TArray<TSkRotationScaleMatrix>;
  i: Integer;
  w: single;
begin
  AMatrices := [];
  for I := 0 to length(FText) - 1 do
    AMatrices := AMatrices + [TSkRotationScaleMatrix.CreateDegrees(1, -90, 0, -i*ABounds.Height, 0, 0)];
  LBlob := TSkTextBlob.MakeFromTextTransform(FText, AMatrices, font);
  w := ABounds.Height*(length(FText) - 1);
  Result.x := P.x + x - ABounds.Height;
  Result.y := P.y + y + w;
  Result.x := Result.x + (width - ABounds.Height)/2;
  Result.y := Result.y + (height - w)/2;
end;


procedure TMyText.writeText;
var
  paint: ISkPaint;
  t : TPointF;
begin
  P := getGlobalPosition;

  paint := TSkPaint.Create;
  paint.AntiAlias := true;
  paint.Color := color;
  paint.Style := TSkPaintStyle.Fill;
  font.MeasureText(FText, ABounds, paint);
  font.Hinting := TSkFontHinting.Full;

  if rotate then
    t := MakeFromTextTransform
  else
    t := MakeFromText;

  if LBlob <> nil then
    data.canvas.DrawTextBlob(LBlob, t.x, t.y, paint);
end;


end.
