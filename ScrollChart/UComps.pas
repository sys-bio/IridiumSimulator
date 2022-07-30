unit UComps;

interface
  uses
    Math, System.Types, System.UITypes, System.UIConsts, System.Contnrs,
    Skia, Skia.FMX, System.SysUtils, UContainer, UDataSource,
    System.Classes, UGlobalData, FMX.Dialogs;

  type

  TBlockY = class (TContainer)
     tick: TMyLine;
     number: TMyText;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure resize;
  end;

  TAxisY = class (TContainer)
      hLine: TMyLine;
      block: TBlockY;
      maxTicks: Integer;
      constructor Create(w, h: double; P: TObject); override;
      destructor Destroy; override;
      procedure Draw;
      procedure resize;
  end;

  TGridAxisY = class (TContainer)
      hLine: TMyLine;
      maxTicks, zeroIndex: Integer;
      cl: TAlphaColor;
      constructor Create(w, h: double; P: TObject); override;
      destructor Destroy; override;
      procedure Draw;
      function getZeroIndex: Integer;
      procedure resize;
  end;

  TLeftBox = class (TContainer)
     axisY: TAxisY;
     labelY: TMyText;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure resize;
  end;

  TContBlocksX = class (TContainer)
     childs: TObjectList;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure addChild(posX, posY: double; index: Integer; child: TContainer);
     procedure removeChild(index: Integer);
     procedure  eraseAll;
     procedure Draw;
  end;

  TGridXContainer = class (TContainer)
     childs: TObjectList;
     constructor Create(w, h: double; P: TObject); override;
     procedure addChild(posX, posY: double; child: TContainer);
     procedure removeChild(index: Integer);
     procedure  eraseAll;
     procedure Draw;
     destructor Destroy; override;
  end;

  TBlockX = class (TContainer)
     tick: TMyLine;
     number: TMyText;
     life, index: Integer;
     constructor Create(w, h: double; P: TObject); override;
     destructor Destroy; override;
     procedure Draw;
     procedure resize;
  end;

  TAxisX = class (TContainer)
      hLine: TMyLine;
      block: TBlockX;
      maxTicks, life: Integer;
      blocksContainer: TContBlocksX;
      offsetX: double;
      dtbyTick, widthBlock: double;
      rBlack, lBlack: TContainer;
      u: TPoint;
      dir: byte;
      constructor Create(w, h: double; P: TObject); override;
      procedure reset;
      destructor Destroy; override;
      procedure Draw;
      procedure resize;
      procedure update;
      procedure initBlocks;
  end;

  TBottomBox = class (TContainer)
     axisX: TAxisX;
     labelX: TMyText;
     constructor Create(w, h: double; P: TObject); override;
     procedure Draw;
     destructor Destroy; override;
     procedure resize;
  end;

  TLegendBox = class (TContainer)
     font: ISkFont;
     pivot, offSet: TPointF;
     box: TRectF;
     constructor Create(w, h: double; P: TObject); override;
     procedure draw;
     procedure resize;
  end;

  TTitleBox = class (TContainer)
     font: ISkFont;
     title: TTitle;
     pivotF, offSet: TPointF;
     constructor Create(w, h: double; P: TObject); override;
     procedure draw;
     //procedure resize;
  end;


  TMyBlockLine = class(TMyline)
    index, life: Integer;
  end;

  TGridBox = class (TContainer)
     const
       a: Single = 0.05;
     var
       block: TMyBlockLine;
       offsetX: double;
       dtbyTick, widthBlock: double;
       life, maxTicks: Integer;
       gridXContainer: TGridXContainer;
       cl: Array[0..1] of TAlphaColor;
     constructor Create(w, h: double; P: TObject); override;
     procedure draw;
     procedure resize;
     procedure initBlocks;
     destructor Destroy; override;
     procedure update;
     procedure reset;
  end;

  TGraph = class (TContainer)
     FOnComplete: TOnCompleteEvent;
     scale, globalPos: TPointF;
     mask: TMask;
     legendBox: TLegendBox;
     gridX: TGridBox;
     gridY: TGridAxisY;
     xMouse, yMouse: Single;
     constructor Create(w, h: double; P: TObject); override;
     procedure draw;
     procedure resize;
     function calculateScale: TPointF;
     function realtoScreen(x_w, y_w: double): TPointF;
     function ScreenToReal(locX, locY: double): TPointF; //Real
     destructor Destroy; override;
     function isInside(globalX, globalY: Single): Boolean;
     function worldCoordinates(globalX, globalY: Single): TPointF;
     procedure scanXY(globalX, globalY: Single);
     procedure BorderDraw;
  end;

  TRightBox = class (TContainer)
     graph: TGraph;
     bottomBox: TBottomBox;
     constructor Create(w, h: double; P: TObject); override;
     procedure Draw;
     destructor Destroy; override;
     procedure resize;
  end;


implementation

constructor TGridAxisY.Create(w, h: double; P: TObject);
begin
  parent := P;
  width := w;
  height := h;
  maxTicks := FPlane.yAxis.maxTicks;

  hLine := TMyLine.Create(horizontal, start, self);
  hLine.x := 0;
  hLine.y := 0;
  hLine.width := width;
  hLine.height := height/maxTicks;

end;

function TGridAxisY.getZeroIndex: Integer;
var
  i: Integer;
  val, delta, nicenum: double;
  plane: TPlaneXY;
begin
  plane := FPlane;
  delta := plane.height/maxTicks;
  val := plane.y + plane.height;
  for i := 0 to maxTicks do
    begin
      nicenum := StrToFloat(data.niceNumY(val));
      if IsZero(nicenum) then
        begin
          Result := i;
          Exit;
        end;
      val := val - delta;
    end;
  Result := -1;
end;

procedure TGridAxisY.resize;
begin
  hLine.width := width;
  hLine.height := height/maxTicks;
end;

procedure TGridAxisY.Draw;
var
  i: Integer;
  dy: double;
begin
  cl := MakeColor(FPlane.grid.color, 0.90);
  dy := 0;
  zeroIndex := getZeroIndex;
  for i := 0 to maxTicks do
    begin
       hLine.x := 0;
       hLine.y := dy;
       if zeroIndex = i then
         begin
           hLine.lineWidth := FPlane.yAxis.lineWidth;
           hLine.color := FPlane.yAxis.color;
         end
       else
         begin
           hLine.lineWidth := FPlane.grid.lineWidth;
           hLine.color := cl;
         end;

       hLine.drawLine;
       dy := dy  + hLine.height;
    end;
end;

destructor TGridAxisY.Destroy;
begin
   hLine.Destroy;
end;

constructor TGridXContainer.Create(w, h: double; P: TObject);
begin
  parent := P;
  childs := TObjectList.Create;
  childs.OwnsObjects := false;
end;

procedure TGridXContainer.addChild(posX, posY: double; child: TContainer);
begin
   child.x := posX;
   child.y := posY;
   childs.Add(child);
end;

procedure TGridXContainer.removeChild(index: Integer);
  var
  block: TMyBlockLine;
begin
  block := childs[index] as TMyBlockLine;
  block.Destroy;
  childs.Delete(index);
end;

procedure TGridXContainer.eraseAll;
var
  i: Integer;
begin
  for i := childs.Count - 1 downto 0 do removeChild(i);
end;

procedure TGridXContainer.Draw;
var
  i: Integer;
  block: TMyBlockLine;
  mcl: TAlphaColor;
begin
  mcl := MakeColor(FPlane.grid.color, 0.90);

  for i := 0 to childs.Count - 1 do
    begin
      block := childs[i] as TMyBlockLine;
      if Fplane.grid.ShowStrips then block.clear;
      block.color := mcl;
      block.lineWidth := FPlane.grid.lineWidth;
      block.DrawLine;
    end;
end;

destructor TGridXContainer.Destroy;
begin
  eraseAll;
  childs.Free;
end;

procedure TContBlocksX.addChild(posX, posY: double; index: Integer; child: TContainer);
begin
   child.x := posX;
   child.y := posY;
   if index = 0 then
     childs.Insert(0, child)
   else
     childs.Add(child);
end;

procedure TContBlocksX.removeChild(index: Integer);
var
  block: TBlockX;
begin
  block := childs[index] as TBlockX;
  block.Destroy;
  childs.Delete(index);
end;

constructor TContBlocksX.Create(w, h: double; P: TObject);
begin
  parent := P;
  childs := TObjectList.Create;
  childs.OwnsObjects := false;
end;

procedure TContBlocksX.Draw;
var
  i: Integer;
  block: TBlockX;
begin

  for i := 0 to childs.Count - 1 do
    begin
      block := childs[i] as TBlockX;
      block.tick.color := Fplane.xAxis.color;
      block.tick.lineWidth := Fplane.xAxis.lineWidth;
      block.number.color := Fplane.xAxis.color;
      block.Draw;
    end;

end;

destructor TContBlocksX.Destroy;
begin
  eraseAll;
  childs.Free;
end;

procedure  TContBlocksX.eraseAll;
var
  i: Integer;
begin
  for i := childs.Count - 1 downto 0 do removeChild(i);
end;


constructor TLeftBox.Create(w, h: double; P: TObject);
var
  yAxis: TInfoAxis;
begin
  parent := P;
  yAxis := FPlane.yAxis;
  width := w;
  height := h;

  axisY := TAxisY.Create(w/2, h - TConst.HEIGHT_X_AXIS, self);
  axisY.backgroundColor := claWhite;
  axisY.x := w/2;
  axisY.y := 0;


  labelY := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, left, top, 0, self);
  labelY.width := w/2;
  labelY.height := h - TConst.HEIGHT_X_AXIS;
  labelY.x := 0;
  labelY.y := 0;
  labelY.color := yAxis.color;
  labelY.FText := yAxis.caption;
  labelY.rotate := true;
end;

procedure TLeftBox.resize;
begin
  axisY.width := width/2;
  axisY.height := height - TConst.HEIGHT_X_AXIS;
  axisY.x := width/2;
  axisY.y := 0;

  axisY.resize;

  labelY.width := width/2;
  labelY.height := height - TConst.HEIGHT_X_AXIS;
  labelY.x := 0;
  labelY.y := 0;
end;

destructor TLeftBox.Destroy;
begin
   axisY.Destroy;
   labelY.Destroy;
end;

procedure TLeftBox.Draw;
begin
   //clear;
   axisY.Draw;
   labelY.color := FPlane.yAxis.color;
   labelY.FText := FPlane.yAxis.caption;
   labelY.writeText;
end;

constructor TAxisY.Create(w, h: double; P: TObject);
begin
   parent := P;

   width := w;
   height := h;
   maxTicks := FPlane.yAxis.maxTicks;

   hLine := TMyLine.Create(vertical, last, self);
   hLine.width := TConst.LENGTH_BOX_TICK_Y;
   hLine.height := h;
   hLine.backgroundColor := claYellow;
   hLine.color := FPlane.yAxis.color;

   hLine.lineWidth := FPlane.yAxis.lineWidth;
   hLine.x := w - hLine.width;
   hLine.y := 0;


   block := TBlockY.Create(w, h/maxTicks, self);
   block.width := w;
   block.height := h/maxTicks;
   block.x := hLine.x + hLine.width - block.width;
end;

procedure TAxisY.resize;
begin
   hLine.width := TConst.LENGTH_BOX_TICK_Y;
   hLine.height := height;
   hLine.x := width - hLine.width;
   hLine.y := 0;

   block.width := width;
   block.height := height/maxTicks;
   block.x := hLine.x + hLine.width - block.width;

   block.resize;
end;

procedure TAxisY.Draw;
var
  i: Integer;
  val, delta, dy: double;
  plane: TPlaneXY;
begin
 // hLine.Clear;
  plane := FPlane;

  delta := plane.height/maxTicks;
  val := plane.y + plane.height;
  dy := -block.height/2;

  for i := 0 to maxTicks do
    begin
       block.x := 0;
       block.y := dy;
       block.backgroundColor := claBlanchedalmond;
       block.number.FText := data.niceNumY(val);

       //block.Clear;
       block.tick.lineWidth := plane.yAxis.lineWidth;
       block.tick.color := plane.yAxis.color;
       block.number.color := plane.yAxis.color;

       block.Draw;

       val := val - delta;
       dy := dy  + block.height;
    end;

  hLine.color := plane.yAxis.color;
  hLine.lineWidth := plane.yAxis.lineWidth;
  hLine.drawLine;
end;

destructor TAxisY.Destroy;
begin
   hLine.Destroy;
   block.Destroy;
end;

constructor TBlockY.Create(w, h: double; P: TObject);
begin
   parent := P;
   width := w;
   height := h;

   tick := TMyLine.Create(horizontal, half, self);
   tick.width := w*TConst.tickPercentAxisY;
   tick.height := h;
   tick.x := w - tick.width;
   tick.y := 0;
   tick.color := FPlane.yAxis.color;
   tick.lineWidth := FPlane.yAxis.lineWidth;
   tick.backgroundColor := claWhite;

   number := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, right, middle, TConst.MARGIN_FONT, self);
   number.width := w - tick.width;
   number.height := h;
   number.x := 0;
   number.y := 0;
   number.color := tick.color;
end;

procedure TBlockY.resize;
begin
   tick.width := width*TConst.tickPercentAxisY;
   tick.height := height;
   tick.x := width - tick.width;
   tick.y := 0;

   number.width := width - tick.width;
   number.height := height;
   number.x := 0;
   number.y := 0;
end;

procedure TBlockY.Draw;
begin
  tick.drawLine;
  number.writeText;
end;

destructor TBlockY.Destroy;
begin
   tick.destroy;
   number.destroy;
end;

constructor TTitleBox.Create(w, h: double; P: TObject);
var
  typeface : ISkTypeface;
begin
  parent := P;
  width := w;
  height := h;
  title := data.title;
  backgroundColor := title.backgroundColor;
  typeface := TSkTypeface.MakeFromName(title.fontName, TSkFontStyle.Normal);
  font := TSkFont.Create(typeface, title.fontSize, 1);
end;

constructor TLegendBox.Create(w, h: double; P: TObject);
var
  typeface : ISkTypeface;
begin
  parent := P;
  width := w;
  height := h;
  backgroundColor := legend.backgroundColor;
  typeface := TSkTypeface.MakeFromName(legend.fontName, TSkFontStyle.Normal);
  font := TSkFont.Create(typeface, legend.fontSize, 1);
  offSet := data.getPivot(width, height, legend.reference);
end;

procedure TLegendBox.resize;
begin
  offSet := data.getPivot(width, height, legend.reference);
end;

procedure TTitleBox.Draw;
var
  P, t: TPointF;
  paint: ISkPaint;
  ABounds: TRectF;
  ref: TPivot;
  LBlob: ISkTextBlob;
begin
  title := data.title;
  P := getGlobalPosition;
  paint := TSkPaint.Create;
  paint.AntiAlias := true;
  paint.Style := TSkPaintStyle.Fill;

  font.MeasureText(title.text, ABounds, paint);

  height := title.pad + ABounds.Height + title.pad;
  ref.horizontal := title.align;
  ref.vertical := middle;

  offSet := data.getPivot(width, height, ref);
  pivotF := data.getPivot(ABounds.Width, ABounds.Height, ref);

  backgroundColor := title.backgroundColor;
  //clear;
  paint.color := title.fontcolor;
  paint.Style := TSkPaintStyle.Fill;
  paint.StrokeWidth := 1;

  LBlob := TSkTextBlob.MakeFromText(title.text, font);
  t := P + offSet - pivotF;
  data.canvas.DrawTextBlob(LBlob, t.x, t.y + ABounds.Height, paint);
end;

procedure TLegendBox.draw;
 var
   paint: ISkPaint;
   d: TGlobalData;
   LBlob: ISkTextBlob;
   w, h, tx, ty, widthBox, heightBox: double;
   i, n: Integer;
   P, topLeft: TPointF;
   ABounds: TRectF;
   leg: TLegend;
 begin
   d := data;
   leg := legend;
   w := 0;
   h := 0;

   P := getGlobalPosition;

   paint := TSkPaint.Create;
   paint.AntiAlias := true;
   paint.StrokeJoin := TSkStrokeJoin.Round;

   paint.Style := TSkPaintStyle.Fill;
   n := 0;
   for i := 0 to length(d.series) - 1 do
     if d.series[i].visible then
       begin
         font.MeasureText(d.series[i].name, ABounds, paint);
         if ABounds.Width > w then w := ABounds.Width;
         h := h + ABounds.Height;
         n := n + 1;
       end;

   widthBox := leg.pad + w + leg.space + leg.widthLine + leg.pad;
   heightBox := leg.pad + h + (n - 1)*leg.gap + leg.pad;
   if heightBox < 2*leg.pad then heightBox := 2*leg.pad;

   offSet := data.getPivot(width, height, leg.reference);
   pivot := data.getPivot(widthBox, heightBox, leg.pivot);


   topLeft := P + offSet - pivot + TPointF.Create(leg.x, leg.y);
   paint.color := MakeColor(leg.backgroundColor, 0.80);
   paint.Style := TSkPaintStyle.Fill;
   paint.StrokeWidth := 1;
   box := TRectF.Create(topLeft, widthBox, heightBox);
   data.canvas.DrawRect(box, paint);

   tx := box.topLeft.X + leg.pad;
   ty := box.topLeft.Y + leg.pad;
   for i := 0 to length(d.series) - 1 do
     if d.series[i].visible then
       begin
         paint.Color := leg.fontColor;
         font.MeasureText(d.series[i].name, ABounds, paint);
         LBlob := TSkTextBlob.MakeFromText(d.series[i].name, font);

         d.canvas.DrawTextBlob(LBlob, tx, ty + ABounds.Height, paint);

         paint.StrokeWidth := 2; //d.series[i].lineWidth;
         paint.Color := d.series[i].color;
         d.canvas.DrawLine(
             tx + w + leg.space,
             ty + ABounds.Height/2,
             tx + w + leg.space + leg.widthLine,
             ty + ABounds.Height/2,
             paint);

         ty := ty + ABounds.Height + leg.gap;
       end;

 end;

 constructor TGridBox.Create(w, h: double; P: TObject);
 begin
   cl[0] := MakeColor(claBlack, a);
   cl[1] := MakeColor(claWhite, a);

   parent := P;
   width := w;
   height := h;
   gridXContainer := TGridXContainer.Create(w, h, self);
   initBlocks;
 end;

procedure TGridBox.initBlocks;
var
  i: Integer;
  //val: double;
  plane: TPlaneXY;
  mcl: TAlphaColor;
begin
  plane := FPlane;
  maxTicks := plane.xAxis.maxTicks;
  offsetX := 0;
  widthBlock := width/maxTicks;
  //val := plane.x;
  dtbyTick := plane.width/maxTicks;
  life := round(dtbyTick/data.DeltaX);

  mcl := MakeColor(FPlane.grid.color, 0.90);

  for i := 0 to maxTicks do
     begin
       block := TMyBlockLine.Create(TOrientation.vertical, TPosition.half, gridXContainer);
       block.width := widthBlock;
       block.height := height;
       if i = 0 then block.life := Round(0.5*life) else block.life := life;
       block.index := i;
       block.backgroundColor := cl[i mod 2];
       block.color := mcl;
       block.lineWidth := plane.grid.lineWidth;
       gridXContainer.addChild(block.width*i, 0, block);
     end;

   gridXContainer.x := -widthBlock/2 - offsetX;
   gridXContainer.y := 0;
end;

 procedure TGridBox.draw;
 begin
   widthBlock := width/maxTicks;
   gridXContainer.x := -widthBlock/2 - offsetX;
   gridXContainer.Draw;
 end;

 procedure TGridBox.resize;
 var
   i: Integer;
 begin
   widthBlock := width/maxTicks;
   gridXContainer.x := -widthBlock/2 - offsetX;
   gridXContainer.y := 0;
   for i := 0 to gridXContainer.childs.Count - 1 do
    begin
      block := gridXContainer.childs[i] as TMyBlockLine;
      block.width := widthBlock;
      block.height := height;
      block.x := block.width*block.index;
      block.y := 0;
      //block.resize;
    end;
end;

procedure TGridBox.reset;
begin
  gridXContainer.eraseAll;
  initBlocks;
end;

procedure TGridBox.update;
var
  b, last: TMyBlockLine;
  plane: TPlaneXY;
  mcl: TAlphaColor;
begin
  plane := FPlane;
  offsetX := (plane.X*width)/plane.width;
  b := gridXContainer.childs[0] as TMyBlockLine;
  b.life := b.life - 1;
  mcl := MakeColor(FPlane.grid.color, 0.90);

  if b.life = 1 then
    begin
      widthBlock := width/maxTicks;
      b := TMyBlockLine.Create(TOrientation.vertical, TPosition.half, gridXContainer);
      b.width := widthBlock;
      b.height := height;
      b.life := life;
      last := gridXContainer.childs[gridXContainer.childs.Count - 1] as TMyBlockLine;
      b.index := last.index + 1;
      b.backgroundColor := cl[b.index mod 2];
      b.color := mcl;
      b.lineWidth := plane.grid.lineWidth;
      gridXContainer.addChild(b.width*b.index, 0, b);
    end
  else if b.life = 0 then
    begin
      gridXContainer.removeChild(0);
    end;
end;

destructor TGridBox.Destroy;
begin
  gridXContainer.Destroy;
end;


destructor TGraph.Destroy;
begin
  gridX.Destroy;
  gridY.Destroy;
  mask.Destroy;
  legendBox.Destroy;
end;

constructor TGraph.Create(w, h: double; P: TObject);
begin
  parent := P;
  width := w;
  height := h;

  mask := TMask.Create(w, h, self, self);
  mask.x := 0;
  mask.y := 0;

  legendBox := TLegendBox.Create(w, h, self);
  legendBox.x := 0;
  legendBox.y := 0;

  gridX := TGridBox.Create(w, h, self);
  gridX.x := 0;
  gridX.y := 0;

  gridY := TGridAxisY.Create(w, h, self);
  gridY.x := 0;
  gridY.y := 0;
end;

procedure TGraph.resize;
begin
  mask.x := 0;
  mask.y := 0;
  mask.width := width;
  mask.height := height;

  legendBox.x := 0;
  legendBox.y := 0;
  legendBox.width := width;
  legendBox.height := height;
  legendBox.resize;

  gridX.x := 0;
  gridX.y := 0;
  gridX.width := width;
  gridX.height := height;
  gridX.resize;


  gridY.x := 0;
  gridY.y := 0;
  gridY.width := width;
  gridY.height := height;
  gridY.resize;

end;

function TGraph.RealToScreen(x_w, y_w: double): TPointF; //Pixels
begin
  Result.x := x + (x_w - FPlane.X)/scale.X;
  Result.y := height - (y + (y_w - FPlane.Y)/scale.y);
  Result := globalPos + Result;
end;

function TGraph.ScreenToReal(locX, locY: double): TPointF; //Real
begin
  Result.x := locX*scale.X + FPlane.x;
  Result.y := (Height - locY)*scale.Y + FPlane.y;
end;

function TGraph.calculateScale: TPointF;
begin
  result.x := FPlane.width / width;
  result.y := FPlane.Height / height;
end;

function TGraph.isInside(globalX, globalY: Single): Boolean;
begin
   globalPos := getGlobalPosition;
   Result := TRectF.Create(globalPos, width, height).Contains(TPointF.Create(globalX, globalY));
end;

function TGraph.worldCoordinates(globalX, globalY: Single): TPointF;
begin
  scale := calculateScale;
  Result := ScreenToReal(globalX - globalPos.X, globalY - globalPos.Y);
end;

procedure TGraph.scanXY(globalX, globalY: Single);
begin
  xMouse := globalX;
  yMouse := globalY;
end;

procedure TGraph.BorderDraw;
var
  paint: ISkPaint;
  R: TRectF;
begin
  paint := TSkPaint.Create;
  paint.AntiAlias := true;
  paint.StrokeWidth := FPlane.yAxis.lineWidth;
  paint.color := FPlane.yAxis.color;
  paint.Style := TSkPaintStyle.Stroke;
  R := TRectF.Create(TPointF.Create(globalPos.X, globalPos.Y), width, height);
  data.canvas.DrawRect(R, paint);
end;

procedure TGraph.draw;
var
  paint: ISkPaint;
  i, j: Integer;
  Q: PDataCol;
  yL: TList;
  D: PData;
  serie: TDataSerie;
  pt: TPointF;
  path: ISKPath;
  temp: TGlobalData;
begin
  temp := data;
  clear;
  gridY.draw;
  gridX.draw;

  //if temp.dataSource.cols.Count < 2 then Exit;

  scale := calculateScale;
  globalPos := getGlobalPosition;

  paint := TSkPaint.Create(TSkPaintStyle.Stroke);
  paint.StrokeJoin := TSkStrokeJoin.Round;
  paint.AntiAlias := true;

  for i := 0 to temp.dataSource.cols.Count - 1 do
    begin
      Q := temp.dataSource.cols[i];
      yL := Q^.rows;
      for j := 0 to yL.Count - 1 do
        begin
          D := yL[j];
          serie := D^.serie;
          if (serie.visible) and (serie.count >= 2) then
            begin
              pt := RealToScreen(Q^.x, D^.y);
              if serie.LPath = nil then
                begin
                  serie.LPath := TSkPathBuilder.Create;
                  serie.LPath.MoveTo(pt);
                end
              else
                serie.LPath.LineTo(pt);
            end;
        end;
    end;


  for j := 0 to length(temp.series) - 1 do
    begin
      serie := temp.series[j];
      if (serie.visible) and (serie.count >= 2) then
        begin
          paint.StrokeWidth := serie.lineWidth;
          paint.Color := serie.color;
          path := serie.LPath.Detach;
          temp.canvas.DrawPath(path, paint);
          serie.LPath := nil;
        end;
    end;

   mask.Draw;

   if legend.visible then legendBox.draw;

   BorderDraw;

   if Assigned(FOnComplete) then
     begin
       pt := ScreenToReal(xMouse - globalPos.X, yMouse - globalPos.Y);
       FOnComplete(pt.X, pt.Y);
     end;

end;

constructor TBottomBox.Create(w, h: double; P: TObject);
var
  xAxis: TInfoAxis;
begin
  parent := P;
  xAxis := FPlane.xAxis;
  width := w;
  height := h;

  axisX := TAxisX.Create(w, h/2, self);
  axisX.backgroundColor := data.BackgroundColor;


  axisX.x := 0;
  axisX.y := 0;

  labelX := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, center, bottom, TConst.MARGIN_FONT, self);
  labelX.width := w;
  labelX.height := h/2;
  labelX.x := 0;
  labelX.y := axisX.height;
  labelX.color := xAxis.color;
  labelX.FText := xAxis.caption;
end;

procedure TBottomBox.resize;
begin
  axisX.width := width;
  axisX.height := height/2;
  axisX.x := 0;
  axisX.y := 0;
  axisX.resize;

  labelX.width := width;
  labelX.height := height/2;
  labelX.x := 0;
  labelX.y := axisX.height;
end;

destructor TBottomBox.Destroy;
begin
  axisX.Destroy;
  labelX.Destroy;
end;

procedure TBottomBox.Draw;
begin
   Clear;
   //axisX.backgroundColor := data.BackgroundColor;
   axisX.Draw;
   labelX.color := FPlane.xAxis.color;
   labelX.FText := FPlane.xAxis.caption;
   labelX.writeText;
end;

procedure TAxisX.update;
var
  b, last: TBlockX;
  v: TPoint;
begin
  v := data.switch([0, blocksContainer.childs.Count - 1], dir);
  u := data.switch([1, -1], dir);

  offsetX := u.Y*(FPlane.X*width)/FPlane.width;


  b := blocksContainer.childs[v.X] as TBlockX;
  b.life := b.life - 1;

  if b.life = 1 then
    begin
      b := TBlockX.Create(width/maxTicks, height, blocksContainer);
      b.life := life;
      last := blocksContainer.childs[v.Y] as TBlockX;
      b.index := last.index + u.X;
      b.backgroundColor := claBlanchedalmond;
      b.color := hLine.color;
      b.number.FText := data.NiceNumX(b.index*dtbyTick);
      blocksContainer.addChild(b.width*b.index, 0, v.Y, b);
    end
  else if b.life = 0 then
    begin
      blocksContainer.removeChild(v.X);
    end;
end;

procedure TAxisX.Draw;
var
  widthBlock: double;
begin
  //clear;
  hLine.color := FPlane.xAxis.color;
  hLine.lineWidth := FPlane.xAxis.lineWidth;
  hLine.drawLine;
  widthBlock := width/maxTicks;
  blocksContainer.x := -widthBlock/2 + offsetX;
  blocksContainer.Draw;
  rBlack.backgroundColor := data.BackgroundColor;
  lBlack.backgroundColor := data.BackgroundColor;
  rBlack.clear;
  LBlack.clear;
end;

procedure TAxisX.reset;
begin
  blocksContainer.eraseAll;
  initBlocks;
end;

procedure TAxisX.initBlocks;
var
  i: Integer;
  val: double;
begin
  u := data.switch([1, -1], dir);
  offsetX := 0;

  widthBlock := width/maxTicks;
  val := FPlane.x;
  dtbyTick := FPlane.width/maxTicks;
  life := round(dtbyTick/data.DeltaX);

  for i := 0 to maxTicks do
     begin
       block := TBlockX.Create(widthBlock, height, blocksContainer);

       if i = 0 then block.life := Round(0.5*life) else block.life := life;
       block.index := i;
       block.backgroundColor := claBlanchedalmond;
       block.color := hLine.color;

       block.number.FText := data.niceNumX(val);
       val := val + dtbyTick;
       blocksContainer.addChild(block.width*i, 0, i, block);
     end;

   blocksContainer.x := -widthBlock/2 + offsetX;
   blocksContainer.y := 0;
end;

constructor TAxisX.Create(w, h: double; P: TObject);
begin
   parent := P;
   dir := 0;

   width := w;
   height := h;
   maxTicks := FPlane.xAxis.maxTicks;

   hLine := TMyLine.Create(horizontal, start, self);
   hLine.width := w;
   hLine.height := TConst.LENGTH_BOX_TICK_X;
   hLine.parent := self;

   hLine.backgroundColor := claYellow;
   hLine.color := FPlane.xAxis.color;

   hLine.lineWidth := FPlane.XAxis.lineWidth;
   hLine.x := 0;
   hLine.y := 0;

   blocksContainer := TContBlocksX.Create(w, h, self);
   blocksContainer.parent := self;

   initBlocks;


   rBlack := TContainer.Create(TConst.MARGIN_X, h*2 + TConst.MARGIN_Y, self);
   rBlack.backgroundColor := data.BackgroundColor;
   rBlack.y := 0;
   rBlack.x := width + TConst.FONT_SIZE/2;


   LBlack := TContainer.Create(TConst.MARGIN_X + TConst.WIDTH_Y_AXIS, h*2 + TConst.MARGIN_Y, self);
   LBlack.backgroundColor := data.BackgroundColor;
   LBlack.y := 0;
   LBlack.x := -LBlack.width - TConst.FONT_SIZE/2;

end;

procedure TAxisX.resize;
var
  i: Integer;
begin
   hLine.width := width;
   hLine.height := TConst.LENGTH_BOX_TICK_X;

   widthBlock := width/maxTicks;
   blocksContainer.x := -widthBlock/2 + offsetX;
   blocksContainer.y := 0;


   for i := 0 to blocksContainer.childs.Count - 1 do
    begin
      block := blocksContainer.childs[i] as TBlockX;
      block.width := widthBlock;
      block.height := height;
      block.x := block.width*block.index;
      block.y := 0;
      block.resize;
    end;


   rBlack.y := 0;
   rBlack.x := width + TConst.FONT_SIZE/2;
   rBlack.width := TConst.MARGIN_X;
   rBlack.height := height*2 + TConst.MARGIN_y;


   LBlack.width := TConst.MARGIN_X + TConst.WIDTH_Y_AXIS;
   LBlack.height := height*2 + TConst.MARGIN_Y;

   LBlack.y := 0;
   LBlack.x := -LBlack.width - TConst.FONT_SIZE/2;

end;

destructor TAxisX.Destroy;
begin
   hLine.Destroy;
   blocksContainer.Destroy;
   rBlack.Destroy;
   LBlack.Destroy;
end;

constructor TBlockX.Create(w, h: double; P: TObject);
begin
   parent := P;

   width := w;
   height := h;

   tick := TMyLine.Create(vertical, half, self);
   tick.width := w;
   tick.height := TConst.tickPercentAxisX*h;
   tick.x := 0;
   tick.y := 0;
   tick.color := FPlane.xAxis.color;
   tick.lineWidth := FPlane.XAxis.lineWidth;
   tick.backgroundColor := claWhite;


   number := TMyText.Create(TConst.FONT_NAME, TConst.FONT_SIZE, center, top, TConst.MARGIN_FONT, self);
   number.width := w;
   number.height := h - tick.height;
   number.x := 0;
   number.y := tick.height;
   number.backgroundColor := claGainsboro;
   number.color := tick.color;
end;

procedure TBlockX.resize;
begin
   tick.width := width;
   tick.height := TConst.tickPercentAxisX*height;

   number.width := width;
   number.height := height - tick.height;
   number.x := 0;
   number.y := tick.height;
end;

procedure TBlockX.Draw;
begin
  //tick.Clear;
  tick.drawLine;

  //number.Clear;
  number.writeText;
end;

destructor TBlockX.Destroy;
begin
   tick.destroy;
   number.destroy;
end;

destructor  TRightBox.Destroy;
begin
  graph.Destroy;
  bottomBox.Destroy;
end;

constructor TRightBox.Create(w, h: double; P: TObject);
var
  hBottombox: double;
begin
  parent := P;
  name := 'TRightBox';
  width := w;
  height := h;
  hBottombox := TConst.HEIGHT_X_AXIS;

  bottomBox := TBottomBox.Create(w, hBottombox, self);
  bottomBox.backgroundColor := data.BackgroundColor;
  bottomBox.x := 0;
  bottomBox.y := h - hBottombox;

  graph := TGraph.Create(w, h - hBottombox, self);
  graph.name := 'graph';
  graph.backgroundColor := data.plotPanelBackgroundColor;
  graph.x := 0;
  graph.y := 0;
end;

procedure TRightBox.resize;
begin
  bottomBox.width := width;
  bottomBox.height := TConst.HEIGHT_X_AXIS;
  bottomBox.x := 0;
  bottomBox.y := height - bottomBox.height;

  graph.width := width;
  graph.height := bottomBox.y;
  graph.x := 0;
  graph.y := 0;

  bottomBox.resize;
  graph.resize;
end;

procedure TRightBox.Draw;
begin
  clear;
  graph.backgroundColor := data.plotPanelBackgroundColor;
  graph.Draw;
  bottomBox.backgroundColor := data.BackgroundColor;
  bottomBox.Draw;
end;



end.
