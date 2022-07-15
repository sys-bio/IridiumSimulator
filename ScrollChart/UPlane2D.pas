unit UPlane2D;

interface
  type
    TPlane2D = class (TObject)
      parent: TObject;
      minX, maxX, minY, maxY: double;
      x, y, width, height: double;
      pivotX, pivotY: double;
      deltaX, deltaY: double;
      maxTicksX, maxTicksY: Integer;
      procedure calculate;
      procedure setYAxisRange(yMin, yMax: double);
      procedure setXAxisRange(xMin, xMax: double);
      procedure setMaxTicksX(n: Integer);
      procedure setMaxTicksY(n: Integer);
      constructor Create(xmin, xmax, ymin, ymax: double; FmaxTicksX, FmaxTicksY: Integer);
  end;

implementation

constructor TPlane2D.Create(xMin, xMax, yMin, yMax: double; FmaxTicksX, FmaxTicksY: Integer);
begin
  minY := yMin;
  maxY := yMax;
  minX := xMin;
  maxX := xMax;
  maxTicksX := FmaxTicksX;
  maxTicksY := FmaxTicksY;

  calculate;
end;

procedure TPlane2D.calculate;
begin
  width := maxX - minX;
  height:= maxY - minY;
  deltaX := width/maxTicksX;
  deltaY := height/maxTicksY;
end;

procedure TPlane2D.setYAxisRange(yMin, yMax: double);
begin
  minY := yMin;
  maxY := yMax;
  calculate;
end;

procedure TPlane2D.setXAxisRange(xMin, xMax: double);
begin
  minX := xMin;
  maxX := xMax;
  calculate;
end;

procedure TPlane2D.setMaxTicksX(n: Integer);
begin
  maxTicksX := n;
end;
procedure TPlane2D.setMaxTicksY(n: Integer);
begin
  maxTicksY := n;
end;


end.
