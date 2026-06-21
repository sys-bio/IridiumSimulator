unit uPlotSeries;

interface

Uses SysUtils,
     Classes,
     System.UIConsts,
     System.UITypes,
     Generics.Collections,
     Types,
     Skia,
     uPlotMapper;

const
  SERIES_TYPE_SIMULATION = 0;
  SERIES_TYPE_DATA = 1;

type
  TSourceType = (stCSVFile, stFunction);
  TMarkerShape = (symPoint, symSquare, symCircle, symCross, symTimes, symDiamond, symTriangle);
  TLineStyle = (ltSolid, ltDashDash, ltDotDot);

  TDataList = TList<TPointF>;

 TPlotSeries = class
  public
    Name: string;
    SeriesType : Integer; // put what ever you want here to identify a particular type of series
    Data: TDataList;   // Stores a list of X,Y pairs

    SeriesVisible : Boolean;

    // Line Styling
    LineColor: TAlphaColor;
    LineWidth: Single;
    LineVisible : Boolean;
    LineStyle : TLineStyle;

    // Marker Styling
    MarkerSize: Single;
    MarkerFillColor: TAlphaColor;
    MarkerStrokeColor: TAlphaColor;
    MarkerStrokeWidth: Single;
    MarkerShape : TMarkerShape;
    MarkerVisible : Boolean;

    constructor Create(const AName: string; AStrokeColor: TAlphaColor; ShowMarkers : Boolean = True);
    destructor Destroy; override;
    procedure DrawMarker (ACanvas : ISKCanvas; P : TPointF; Size : Single; LPaint : ISKPaint);
    procedure Draw(const ACanvas: ISkCanvas; const AMapper: TPlotMapper);
    function AddXY (X, Y : Double) : Integer;
  end;

 const
    MarkerShapeNames: array[TMarkerShape] of string = (
      'Point',
      'Square',
      'Circle',
      'Cross',
      'Times',
      'Diamond',
      'Triangle'
  );

    LineStyleNames: array[TLineStyle] of string = (
      'Solid',
      'Dash-Dash',
      'Dot-Dot'
  );

  function MarkerStrToMarkerShape (MarkerStr : String) : TMarkerShape;

implementation

// uPlotDefaults is listed here, in the implementation uses, so that
// uPlotSeries itself remains free of JSON plumbing and the circular-
// reference risk is avoided (uPlotDefaults uses uPlotSeries for the
// TMarkerShape / TLineStyle types only).
uses uPlotDefaults;

function MarkerStrToMarkerShape (MarkerStr : String) : TMarkerShape;
begin
  if MarkerStr = 'circle' then
     exit (symCircle);
  if MarkerStr = 'square' then
     exit (symSquare);
  if MarkerStr = 'point' then
     exit (symPoint);
end;

constructor TPlotSeries.Create(const AName: string; AStrokeColor: TAlphaColor; ShowMarkers : Boolean = True);
begin
  inherited Create;

  Name       := AName;
  SeriesType := SERIES_TYPE_SIMULATION;

  SeriesVisible := True;

  // Line styling � values come from the global PlotDefaults record.
  // PlotDefaults is initialised from the built-in constants at unit
  // start, and may be overridden by loading a JSON file before the
  // first series is created.
  LineColor   := AStrokeColor;           // color always comes from the caller
  LineWidth   := PlotDefaults.LineWidth;
  LineVisible := True;
  LineStyle   := PlotDefaults.LineStyle;

  // Marker styling
  MarkerVisible     := ShowMarkers;
  MarkerSize        := PlotDefaults.MarkerSize;
  MarkerFillColor   := PlotDefaults.MarkerFillColor;
  MarkerStrokeColor := AStrokeColor;    // stroke color tracks the series color
  MarkerStrokeWidth := PlotDefaults.MarkerStrokeWidth;
  MarkerShape       := PlotDefaults.MarkerShape;

  Data := TDataList.Create;
end;

destructor TPlotSeries.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;


procedure TPlotSeries.DrawMarker (ACanvas : ISKCanvas; P : TPointF; Size : Single; LPaint : ISKPaint);
var R : TRectF;
    Radius : Single;
    LPathBuilder : ISkPathBuilder;
    LPath: ISkPath;
    HOffSet, VOffSet : Single;
begin
   case MarkerShape of
         symCircle :
            begin
            // Draw Fill
            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            ACanvas.DrawCircle(P, Size, LPaint);

            // Draw Stroke (Outline)
            LPaint.Style := TSkPaintStyle.Stroke;
            LPaint.Color := MarkerStrokeColor;
            ACanvas.DrawCircle(P, Size, LPaint);
            end;

        symSquare :
            begin
            // Draw Fill
            Size := Size*1.5;
            R.Left := P.X - Size/2;
            R.Right := P.X + Size/2;
            R.Top := P.Y - Size/2;
            R.Bottom := P.Y + Size/2;

            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            ACanvas.DrawRect(R, LPaint);

            // Draw Stroke (Outline)
            LPaint.Style := TSkPaintStyle.Stroke;
            LPaint.Color := MarkerStrokeColor;
            ACanvas.DrawRect(R, LPaint);
            end;

        symCross :
            begin
            Radius := 1.7*(Size/2);
            LPaint.StrokeWidth := 2;
            LPaint.Color := MarkerStrokeColor;
            ACanvas.DrawLine(P.X - Radius, P.Y, P.X + Radius, P.Y, LPaint);
            ACanvas.DrawLine(P.X, P.Y - Radius, P.X, P.Y + Radius, LPaint);
            end;

        symTimes :
            begin
            var Offset := ((3.75*Size/2) / 2) * 0.70710678;
            LPaint.StrokeWidth := 2;
            LPaint.Color := MarkerStrokeColor;
            ACanvas.DrawLine(P.X - Offset, P.Y - Offset, P.X + Offset, P.Y + Offset, LPaint);
            ACanvas.DrawLine(P.X - Offset, P.Y + Offset, P.X + Offset, P.Y - Offset, LPaint);
            end;

        symDiamond :
            begin
            Radius := 2.2*(Size/2);
            LPathBuilder := TSkPathBuilder.Create;
            LPathBuilder.MoveTo(P.X, P.Y - Radius);       // Start at Top
            LPathBuilder.LineTo(P.X + Radius, P.Y);       // Line to Right
            LPathBuilder.LineTo(P.X, P.Y + Radius);       // Line to Bottom
            LPathBuilder.LineTo(P.X - Radius, P.Y);       // Line to Left
            LPathBuilder.Close;                       // Close the path loop
            LPath := LPathBuilder.Detach;

            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            ACanvas.DrawPath(LPath, LPaint);

            LPaint.Style := TSkPaintStyle.Stroke;
            LPaint.Color := MarkerStrokeColor;
            ACanvas.DrawPath(LPath, LPaint);
            end;

        symPoint :
            begin
            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            ACanvas.DrawCircle(P, 2, LPaint);
            end;

        symTriangle :
            begin
            Radius := 2.4*(Size/2);
            HOffset := Radius * 0.8660254; // Horizontal spread from center
            VOffset := Radius * 0.5;       // Vertical drop below center
            LPathBuilder := TSkPathBuilder.Create;
            LPathBuilder.MoveTo(P.X, P.Y - Radius);             // Top Vertex
            LPathBuilder.LineTo(P.X + HOffset, P.Y + VOffset);   // Bottom-Right Vertex
            LPathBuilder.LineTo(P.X - HOffset, P.Y + VOffset);   // Bottom-Left Vertex
            LPathBuilder.Close;                              // Closes back to Top                      // Close the path loop
            LPath := LPathBuilder.Detach;

            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            LPaint.StrokeJoin := TSkStrokeJoin.Miter;
            ACanvas.DrawPath(LPath, LPaint);

            LPaint.Style := TSkPaintStyle.Stroke;
            LPaint.Color := MarkerStrokeColor;
            ACanvas.DrawPath(LPath, LPaint);
            end;
   end;
end;


procedure TPlotSeries.Draw(const ACanvas: ISkCanvas; const AMapper: TPlotMapper);
var
  LPaint: ISkPaint;
  I: Integer;
  P1, P2: TPointF;
  R : TRectF;
  LIntervals : TArray<single>;
begin
  if Data.Count < 2 then Exit;

  if not SeriesVisible then Exit;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  // 1. DRAW THE LINE
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.Color := LineColor;
  LPaint.StrokeWidth := LineWidth;
  LPaint.StrokeCap := TSkStrokeCap.Round;

  if LineStyle = ltDashDash then
    LPaint.PathEffect := TSkPathEffect.MakeDash([8, 4], 0);

  if LineVisible then
     begin
     for I := 0 to Data.Count - 2 do
       begin

       if LineStyle <> TLineStyle.ltSolid then
          begin
          LPaint.StrokeCap := TSkStrokeCap.Butt;
          case LineStyle of
             TLineStyle.ltDashDash: LIntervals := [5, 4, 5, 4];
             TLineStyle.ltDotDot:  begin LIntervals := [1, 4, 1, 4]; LPaint.StrokeCap := TSkStrokeCap.Round; end;
          end;
          LPaint.PathEffect := TSkPathEffect.MakeDash(LIntervals, 0);
          end;

       // In log mode, a point with X<=0 (or Y<=0) has no representable
       // position on the axis. Skip any segment that touches such a point;
       // the mapper would otherwise clamp it to 1e-9 and the line would
       // shoot off to the edge of the plot area.
       if AMapper.LogX and ((Data[I].X <= 0) or (Data[I+1].X <= 0)) then Continue;
       if AMapper.LogY and ((Data[I].Y <= 0) or (Data[I+1].Y <= 0)) then Continue;

       P1 := AMapper.MapPoint(Data[I]);
       P2 := AMapper.MapPoint(Data[I+1]);
       ACanvas.DrawLine(P1.X, P1.Y, P2.X, P2.Y, LPaint);
       end;
     end;
  LPaint.PathEffect := nil;

  // Reset path effect for markers
  LPaint.PathEffect := nil;

  // 2. DRAW THE MARKERS
  if MarkerVisible then
    begin
    for I := 0 to Data.Count - 1 do
    begin
      // Same log-mode exclusion as the line loop.
      if AMapper.LogX and (Data[I].X <= 0) then Continue;
      if AMapper.LogY and (Data[I].Y <= 0) then Continue;

      P1 := AMapper.MapPoint(Data[I]);

      LPaint.StrokeWidth := MarkerStrokeWidth;
      DrawMarker (ACanvas, P1, MarkerSize, LPaint);
    end;
    end;
end;


function TPlotSeries.AddXY (X, Y : Double) : Integer;
begin
  Result := Data.Add(TPointF.Create (X, Y));
end;



end.
