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
  TMarkerShape = (symPoint, symSquare, symCircle);
  TLineStyle = (ltSolid, ltDashDash);

  TDataList = TList<TPointF>;

 TPlotSeries = class
  public
    Name: string;
    SeriesType : Integer; // put what ever you want here to identify a particular type of series
    Data: TDataList;   // Stores a list of X,Y pairs

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
    procedure Draw(const ACanvas: ISkCanvas; const AMapper: TPlotMapper);
    function AddXY (X, Y : Double) : Integer;
  end;

 const
    MarkerShapeNames: array[TMarkerShape] of string = (
      'Point',
      'Square',
      'Circle'
  );

    LineStyleNames: array[TLineStyle] of string = (
      'Solid',
      'Dash-Dash'
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

  // Line styling — values come from the global PlotDefaults record.
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


procedure TPlotSeries.Draw(const ACanvas: ISkCanvas; const AMapper: TPlotMapper);
var
  LPaint: ISkPaint;
  I: Integer;
  P1, P2: TPointF;
  R : TRectF;
begin
  if Data.Count < 2 then Exit;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  // 1. DRAW THE LINE
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.Color := LineColor;
  LPaint.StrokeWidth := LineWidth;

  if LineStyle = ltDashDash then
    LPaint.PathEffect := TSkPathEffect.MakeDash([8, 4], 0);

  if LineVisible then
     begin
     for I := 0 to Data.Count - 2 do
       begin
       P1 := AMapper.MapPoint(Data[I]);
       P2 := AMapper.MapPoint(Data[I+1]);
       ACanvas.DrawLine(P1.X, P1.Y, P2.X, P2.Y, LPaint);
       end;
     end;

  // Reset path effect for markers
  LPaint.PathEffect := nil;

  // 2. DRAW THE MARKERS
  if MarkerVisible then
    begin
    for I := 0 to Data.Count - 1 do
    begin
      P1 := AMapper.MapPoint(Data[I]);

      case MarkerShape of
         symCircle :
            begin
            // Draw Fill
            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            ACanvas.DrawCircle(P1, MarkerSize, LPaint);

            // Draw Stroke (Outline)
            LPaint.Style := TSkPaintStyle.Stroke;
            LPaint.Color := MarkerStrokeColor;
            LPaint.StrokeWidth := MarkerStrokeWidth;
            ACanvas.DrawCircle(P1, MarkerSize, LPaint);
            end;

        symSquare :
            begin
            // Draw Fill
            R.Left := P1.X - MarkerSize/2;
            R.Right := P1.X + MarkerSize/2;
            R.Top := P1.Y - MarkerSize/2;
            R.Bottom := P1.Y + MarkerSize/2;

            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            ACanvas.DrawRect(R, LPaint);

            // Draw Stroke (Outline)
            LPaint.Style := TSkPaintStyle.Stroke;
            LPaint.Color := MarkerStrokeColor;
            LPaint.StrokeWidth := MarkerStrokeWidth;
            ACanvas.DrawRect(R, LPaint);
            end;

        symPoint :
            begin
            LPaint.Style := TSkPaintStyle.Fill;
            LPaint.Color := MarkerFillColor;
            ACanvas.DrawCircle(P1, 2, LPaint);
            end;
      end;

    end;
    end;
end;


function TPlotSeries.AddXY (X, Y : Double) : Integer;
begin
  Result := Data.Add(TPointF.Create (X, Y));
end;



end.
