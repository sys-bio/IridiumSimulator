unit uPlotMapper;

interface

Uses SysUtils,
     Types;

type
  TPlotMapper = record
  public
    DataRect:  TRectF;  // Data coordinate space
    PixelRect: TRectF;  // Screen pixel space

    LogX, LogY: Boolean;  // Log scale flags (affect MapX/MapY)

    function UnmapX(APixel: Single): Double;
    function UnmapY(APixel: Single): Double;

    function MapX(AValue: Double): Single;
    function MapY(AValue: Double): Single;
    function MapPoint(const APt: TPointF): TPointF;
    function IsInBounds(const APt: TPointF): Boolean;
  end;

implementation

Uses Math;

function TPlotMapper.UnmapX(APixel: Single): Double;
var
  Min, Max: Double;
begin
  if LogX then
  begin
    Min    := Log10(Math.Max(DataRect.Left,  1e-9));
    Max    := Log10(Math.Max(DataRect.Right, 1e-9));
    Result := Power(10, Min + (APixel - PixelRect.Left) * (Max - Min) / PixelRect.Width);
  end
  else
    Result := DataRect.Left +
              (APixel - PixelRect.Left) * (DataRect.Width / PixelRect.Width);
end;

function TPlotMapper.UnmapY(APixel: Single): Double;
var
  Min, Max: Double;
begin
  // Mirror of MapY: pixel bottom = data minimum
  if LogY then
  begin
    Min    := Log10(Math.Max(DataRect.Top,    1e-9));
    Max    := Log10(Math.Max(DataRect.Bottom, 1e-9));
    Result := Power(10, Min + (PixelRect.Bottom - APixel) * (Max - Min) / PixelRect.Height);
  end
  else
    Result := DataRect.Top +
              (PixelRect.Bottom - APixel) * (DataRect.Height / PixelRect.Height);
end;


function TPlotMapper.MapX(AValue: Double): Single;
var
  V, Min, Max: Double;
begin
  if LogX then begin
    V   := Log10(Math.Max(AValue,          1e-9));
    Min := Log10(Math.Max(DataRect.Left,   1e-9));
    Max := Log10(Math.Max(DataRect.Right,  1e-9));
    Result := PixelRect.Left + (V - Min) * (PixelRect.Width / (Max - Min));
  end else
    Result := PixelRect.Left + (AValue - DataRect.Left) * (PixelRect.Width / DataRect.Width);
end;

function TPlotMapper.MapY(AValue: Double): Single;
var
  V, Min, Max: Double;
begin
  // Inverted Y: data minimum maps to pixel bottom
  if LogY then begin
    V   := Log10(Math.Max(AValue,         1e-9));
    Min := Log10(Math.Max(DataRect.Top,   1e-9));
    Max := Log10(Math.Max(DataRect.Bottom,1e-9));
    Result := PixelRect.Bottom - (V - Min) * (PixelRect.Height / (Max - Min));
  end else
    Result := PixelRect.Bottom - (AValue - DataRect.Top) * (PixelRect.Height / DataRect.Height);
end;

function TPlotMapper.MapPoint(const APt: TPointF): TPointF;
begin
  Result := TPointF.Create(MapX(APt.X), MapY(APt.Y));
end;

function TPlotMapper.IsInBounds(const APt: TPointF): Boolean;
begin
  Result := (APt.X >= DataRect.Left)  and (APt.X <= DataRect.Right) and
            (APt.Y >= DataRect.Top)   and (APt.Y <= DataRect.Bottom);
end;

end.
