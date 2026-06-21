unit uPlotDefaults;

// ---------------------------------------------------------------------------
//  uPlotDefaults
//
//  Owns a single global TPlotDefaults record that supplies the initial
//  styling values used by TPlotSeries.Create.
//
//  Call TPlotDefaultsLoader.LoadFromFile(path) once at application start
//  (or whenever the user changes the defaults file).  If the file is absent
//  or malformed, TPlotDefaultsLoader.ResetToBuiltIn restores the hard-wired
//  values that the original code used.
//
//  JSON schema (all keys optional; missing keys keep the built-in value):
//
//  {
//    "series": {
//      "lineWidth"         : 2.0,
//      "lineStyle"         : "Solid",       // "Solid" | "Dash-Dash"
//      "markerSize"        : 6.0,
//      "markerShape"       : "Circle",      // "Circle" | "Square" | "Point"
//      "markerStrokeWidth" : 1.5,
//      "markerFillColor"   : "#FFFFFFFF"    // #AARRGGBB or #RRGGBB
//    }
//  }
// ---------------------------------------------------------------------------

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.UITypes,
  System.UIConsts,
  uPlotSeries;   // for TMarkerShape, TLineStyle

type
  // -------------------------------------------------------------------------
  //  TPlotDefaults — the plain record that TPlotSeries.Create reads
  // -------------------------------------------------------------------------
  TPlotDefaults = record
    LineWidth:         Single;
    LineStyle:         TLineStyle;
    MarkerSize:        Single;
    MarkerShape:       TMarkerShape;
    MarkerStrokeWidth: Single;
    MarkerFillColor:   TAlphaColor;
  end;

  // -------------------------------------------------------------------------
  //  TPlotDefaultsLoader — static class; no instances needed
  // -------------------------------------------------------------------------
  TPlotDefaultsLoader = class
  private
    class function  ParseColor(const S: string; out Color: TAlphaColor): Boolean;
    class function  ParseLineStyle(const S: string; out LS: TLineStyle): Boolean;
    class function  ParseMarkerShape(const S: string; out MS: TMarkerShape): Boolean;
    class procedure ApplySeriesObject(Obj: TJSONObject);
  public
    /// Restore every field to the original hard-wired values.
    class procedure ResetToBuiltIn;

    /// Load from a JSON file.  Returns True on success.
    /// On any error (file missing, JSON invalid, unknown key) the method
    /// leaves the already-loaded defaults untouched and returns False.
    /// Call ResetToBuiltIn first if you want a clean slate before loading.
    class function  LoadFromFile(const AFileName: string): Boolean;
  end;

// ---------------------------------------------------------------------------
//  Global singleton — read by TPlotSeries.Create
// ---------------------------------------------------------------------------
var
  PlotDefaults: TPlotDefaults;

implementation

// ---------------------------------------------------------------------------
//  Built-in constant defaults (mirror the original TPlotSeries.Create values)
// ---------------------------------------------------------------------------
const
  BUILTIN_LINE_WIDTH          = 3.0;
  BUILTIN_LINE_STYLE          = ltSolid;
  BUILTIN_MARKER_SIZE         = 6.0;
  BUILTIN_MARKER_SHAPE        = symCircle;
  BUILTIN_MARKER_STROKE_WIDTH = 1.5;
  BUILTIN_MARKER_FILL_COLOR   = TAlphaColors.White;

// ---------------------------------------------------------------------------
//  TPlotDefaultsLoader
// ---------------------------------------------------------------------------

class procedure TPlotDefaultsLoader.ResetToBuiltIn;
begin
  PlotDefaults.LineWidth         := BUILTIN_LINE_WIDTH;
  PlotDefaults.LineStyle         := BUILTIN_LINE_STYLE;
  PlotDefaults.MarkerSize        := BUILTIN_MARKER_SIZE;
  PlotDefaults.MarkerShape       := BUILTIN_MARKER_SHAPE;
  PlotDefaults.MarkerStrokeWidth := BUILTIN_MARKER_STROKE_WIDTH;
  PlotDefaults.MarkerFillColor   := BUILTIN_MARKER_FILL_COLOR;
end;

// Parse "#AARRGGBB" or "#RRGGBB" into a TAlphaColor.
class function TPlotDefaultsLoader.ParseColor(const S: string;
                                              out Color: TAlphaColor): Boolean;
var
  Hex: string;
  V:   Int64;
begin
  Result := False;
  Hex := S.Trim;
  if Hex.StartsWith('#') then
    Hex := Hex.Substring(1);

  case Length(Hex) of
    6:  Hex := 'FF' + Hex;   // add full-opacity alpha
    8:  {already AARRGGBB — ok};
  else
    Exit;
  end;

  if not TryStrToInt64('$' + Hex, V) then Exit;

  Color  := TAlphaColor(V);
  Result := True;
end;

class function TPlotDefaultsLoader.ParseLineStyle(const S: string;
                                                   out LS: TLineStyle): Boolean;
var
  U: string;
begin
  U := S.Trim.ToLower;
  if (U = 'solid') then          begin LS := ltSolid;    Result := True; end
  else if (U = 'dash-dash') or
          (U = 'dashdash') then  begin LS := ltDashDash; Result := True; end
  else                           Result := False;
end;

class function TPlotDefaultsLoader.ParseMarkerShape(const S: string;
                                                     out MS: TMarkerShape): Boolean;
var
  U: string;
begin
  U := S.Trim.ToLower;
  if      U = 'circle' then begin MS := symCircle; Result := True; end
  else if U = 'square' then begin MS := symSquare; Result := True; end
  else if U = 'point'  then begin MS := symPoint;  Result := True; end
  else                       Result := False;
end;

// Walk the "series" JSON object and overwrite only the keys present.
class procedure TPlotDefaultsLoader.ApplySeriesObject(Obj: TJSONObject);
var
  Pair:  TJSONPair;
  Key:   string;
  LS:    TLineStyle;
  MS:    TMarkerShape;
  Color: TAlphaColor;
begin
  if Obj = nil then Exit;

  for Pair in Obj do
  begin
    Key := Pair.JsonString.Value.Trim.ToLower;

    if Key = 'linewidth' then
    begin
      if Pair.JsonValue is TJSONNumber then
        PlotDefaults.LineWidth := (Pair.JsonValue as TJSONNumber).AsDouble;
    end

    else if Key = 'linestyle' then
    begin
      if Pair.JsonValue is TJSONString then
        ParseLineStyle(Pair.JsonValue.Value, LS);
        // silence unknown strings — keep current value
        PlotDefaults.LineStyle := LS;
    end

    else if Key = 'markersize' then
    begin
      if Pair.JsonValue is TJSONNumber then
        PlotDefaults.MarkerSize := (Pair.JsonValue as TJSONNumber).AsDouble;
    end

    else if Key = 'markershape' then
    begin
      if Pair.JsonValue is TJSONString then
        if ParseMarkerShape(Pair.JsonValue.Value, MS) then
          PlotDefaults.MarkerShape := MS;
    end

    else if Key = 'markerstrokewidth' then
    begin
      if Pair.JsonValue is TJSONNumber then
        PlotDefaults.MarkerStrokeWidth := (Pair.JsonValue as TJSONNumber).AsDouble;
    end

    else if Key = 'markerfillcolor' then
    begin
      if Pair.JsonValue is TJSONString then
        if ParseColor(Pair.JsonValue.Value, Color) then
          PlotDefaults.MarkerFillColor := Color;
    end;

    // Unknown keys are silently ignored — forward-compatible.
  end;
end;

class function TPlotDefaultsLoader.LoadFromFile(const AFileName: string): Boolean;
var
  JSON:       TJSONValue;
  Root:       TJSONObject;
  SeriesNode: TJSONValue;
begin
  Result := False;

  if not TFile.Exists(AFileName) then Exit;

  try
    JSON := TJSONObject.ParseJSONValue(TFile.ReadAllText(AFileName));
  except
    Exit;   // I/O error — leave defaults alone
  end;

  if JSON = nil then Exit;
  try
    if not (JSON is TJSONObject) then Exit;
    Root := JSON as TJSONObject;

    SeriesNode := Root.GetValue('series');
    if (SeriesNode <> nil) and (SeriesNode is TJSONObject) then
      ApplySeriesObject(SeriesNode as TJSONObject);

    Result := True;
  finally
    JSON.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  Unit initialisation — set built-in defaults so PlotDefaults is always valid
// ---------------------------------------------------------------------------
initialization
  TPlotDefaultsLoader.ResetToBuiltIn;

end.nd.
