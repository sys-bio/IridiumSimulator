unit SkPlotPaintBox;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Rtti,
  Generics.Collections,
  Types,
  System.UIConsts,
  System.UITypes,
  FMX.Types,
  FMX.Menus,
  FMX.Controls,
  FMX.Forms,
  FMX.Platform,
  FMX.Graphics,
  Skia,
  FMX.Skia,
  uCSVReaderForPlotter,
  uColorManager,
  uPlotSeries,
  uPlotMapper,
  uPlotDefaults;

type
  TOnReportCoordinates = procedure(mousex, mousey, Worldx, Worldy : single) of object;

  // Drawing of tick mark options
  TTickmarkDrawing = (tmOut, tmIn, tmBoth);

  // -----------------------------------------------------------------------
  //  TAxisLimits — manual axis range override
  // -----------------------------------------------------------------------
  TAxisLimits = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FMinX, FMaxX: Double;
    FMinY, FMaxY: Double;
    procedure SetMinX(const Value: Double);
    procedure SetMaxX(const Value: Double);
    procedure SetMinY(const Value: Double);
    procedure SetMaxY(const Value: Double);
  protected
    procedure Changed; virtual;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property MinX: Double read FMinX write SetMinX;
    property MaxX: Double read FMaxX write SetMaxX;
    property MinY: Double read FMinY write SetMinY;
    property MaxY: Double read FMaxY write SetMaxY;
  end;

  // -----------------------------------------------------------------------
  //  TAxisStyle — Things like tick styling
  // -----------------------------------------------------------------------
  TAxisStyle = class (TPersistent)
     private
      FOnChange: TNotifyEvent;

      FLogX,  FLogY:     Boolean;
      FXMajorTicksVisible : Boolean;
      FXMinorTicksVisible : Boolean;
      FYMajorTicksVisible : Boolean;
      FYMinorTicksVisible : Boolean;

      FXMajorTickLength: Single;
      FXMinorTickLength: Single;
      FYMajorTickLength: Single;
      FYMinorTickLength: Single;

      FXTickDrawing : TTickmarkDrawing;
      FYTickDrawing : TTickmarkDrawing;

      procedure SetLogX(Value: Boolean);
      procedure SetLogY(Value: Boolean);

      procedure SetXMajorTicksVisible (Value : Boolean);
      procedure SetXMinorTicksVisible (Value : Boolean);
      procedure SetYMajorTicksVisible (Value : Boolean);
      procedure SetYMinorTicksVisible (Value : Boolean);

      procedure SetXMajorTickLength(Value: Single);
      procedure SetXMinorTickLength(Value: Single);
      procedure SetYMajorTickLength(Value: Single);
      procedure SetYMinorTickLength(Value: Single);

      procedure SetXTickDrawing (Value : TTickmarkDrawing);
      procedure SetYTickDrawing (Value : TTickmarkDrawing);

     protected
      procedure Changed; virtual;
     public
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      constructor Create;
     published
      property LogX: Boolean read FLogX write SetLogX;
      property LogY: Boolean read FLogY write SetLogY;
      property XMajorTicksVisible: Boolean read FXMajorTicksVisible write SetXMajorTicksVisible default True;
      property XMinorTicksVisible: Boolean read FXMinorTicksVisible write SetXMinorTicksVisible default True;
      property YMajorTicksVisible: Boolean read FYMajorTicksVisible write SetYMajorTicksVisible default True;
      property YMinorTicksVisible: Boolean read FYMinorTicksVisible write SetYMinorTicksVisible default True;

      property XMajorTickLength: Single read FXMajorTickLength write SetXMajorTickLength;
      property XMinorTickLength: Single read FXMinorTickLength write SetXMinorTickLength;
      property YMajorTickLength: Single read FYMajorTickLength write SetYMajorTickLength;
      property YMinorTickLength: Single read FYMinorTickLength write SetYMinorTickLength;

      property XTickDrawing: TTickmarkDrawing read FXTickDrawing write SetXTickDrawing default tmOut;
      property YTickDrawing: TTickmarkDrawing read FYTickDrawing write SetYTickDrawing default tmOut;
  end;

  // -----------------------------------------------------------------------
  //  TTextProperty — title / label text with styling
  // -----------------------------------------------------------------------
  TTextProperty = class(TPersistent)
  private
    FText:     String;
    FVisible:  Boolean;
    FColor:    TAlphaColor;
    FFontSize: Single;

    procedure SetFontSize (Size : Single);
  public
    constructor Create(AText: String; AFontSize : Single);
  published
    property Text:     String      read FText     write FText;
    property Visible:  Boolean     read FVisible  write FVisible;
    property Color:    TAlphaColor read FColor    write FColor;
    property FontSize: Single      read FFontSize write SetFontSize;
  end;

  // -----------------------------------------------------------------------
  //  TGridStyle — all grid rendering preferences, split by axis
  // -----------------------------------------------------------------------
  TGridStyle = class(TPersistent)
  private
    FOnChange: TNotifyEvent;

    FXMajorVisible: Boolean;
    FYMajorVisible: Boolean;
    FXMinorVisible: Boolean;
    FYMinorVisible: Boolean;

    FXMajorColor: TAlphaColor;
    FYMajorColor: TAlphaColor;
    FXMinorColor: TAlphaColor;
    FYMinorColor: TAlphaColor;

    FXMajorWidth: Single;
    FYMajorWidth: Single;
    FXMinorWidth: Single;
    FYMinorWidth: Single;

    FXMinorDivisions: Integer;
    FYMinorDivisions: Integer;

    FXMajorDivisions: Integer;
    FYMajorDivisions: Integer;

    procedure SetXMajorVisible(Value: Boolean);
    procedure SetYMajorVisible(Value: Boolean);
    procedure SetXMinorVisible(Value: Boolean);
    procedure SetYMinorVisible(Value: Boolean);
    procedure SetXMajorColor(Value: TAlphaColor);
    procedure SetYMajorColor(Value: TAlphaColor);
    procedure SetXMinorColor(Value: TAlphaColor);
    procedure SetYMinorColor(Value: TAlphaColor);
    procedure SetXMajorWidth(Value: Single);
    procedure SetYMajorWidth(Value: Single);
    procedure SetXMinorWidth(Value: Single);
    procedure SetYMinorWidth(Value: Single);
    procedure SetXMinorDivisions(Value: Integer);
    procedure SetYMinorDivisions(Value: Integer);
    procedure SetXMajorDivisions(Value: Integer);
    procedure SetYMajorDivisions(Value: Integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property XMajorVisible: Boolean    read FXMajorVisible write SetXMajorVisible;
    property YMajorVisible: Boolean    read FYMajorVisible write SetYMajorVisible;
    property XMinorVisible: Boolean    read FXMinorVisible write SetXMinorVisible;
    property YMinorVisible: Boolean    read FYMinorVisible write SetYMinorVisible;
    property XMajorColor:   TAlphaColor read FXMajorColor  write SetXMajorColor;
    property YMajorColor:   TAlphaColor read FYMajorColor  write SetYMajorColor;
    property XMinorColor:   TAlphaColor read FXMinorColor  write SetXMinorColor;
    property YMinorColor:   TAlphaColor read FYMinorColor  write SetYMinorColor;
    property XMajorWidth:   Single      read FXMajorWidth  write SetXMajorWidth;
    property YMajorWidth:   Single      read FYMajorWidth  write SetYMajorWidth;
    property XMinorWidth:   Single      read FXMinorWidth  write SetXMinorWidth;
    property YMinorWidth:   Single      read FYMinorWidth  write SetYMinorWidth;
    property XMinorDivisions: Integer   read FXMinorDivisions write SetXMinorDivisions;
    property YMinorDivisions: Integer   read FYMinorDivisions write SetYMinorDivisions;
    property XMajorDivisions: Integer   read FXMajorDivisions write SetXMajorDivisions;
    property YMajorDivisions: Integer   read FYMajorDivisions write SetYMajorDivisions;
  end;

  // -----------------------------------------------------------------------
  //  TLegendStyle — all legend rendering preferences
  // -----------------------------------------------------------------------
  TLegendLocation = (llTopRight, llTopLeft, llBottomRight, llBottomLeft);

  TLegendStyle = class(TPersistent)
  private
    FVisible:           Boolean;
    FBorderVisible:     Boolean;
    FBorderColor:       TAlphaColor;
    FBorderWidth:       Single;
    FBackgroundColor:   TAlphaColor;
    FBackgroundOpacity: Single;
    FLocation:          TLegendLocation;
  public
    constructor Create;
  published
    property Visible:           Boolean         read FVisible           write FVisible;
    property BorderVisible:     Boolean         read FBorderVisible     write FBorderVisible;
    property BorderColor:       TAlphaColor     read FBorderColor       write FBorderColor;
    property BorderWidth:       Single          read FBorderWidth       write FBorderWidth;
    property BackgroundColor:   TAlphaColor     read FBackgroundColor   write FBackgroundColor;
    property BackgroundOpacity: Single          read FBackgroundOpacity write FBackgroundOpacity;
    property Location:          TLegendLocation read FLocation          write FLocation;
  end;

  // -----------------------------------------------------------------------
  //  TSkPlotPaintBox — the chart component
  // -----------------------------------------------------------------------
  TPlotSeriesList = class (TObjectList<TPlotSeries>)
       function Find (Name : string; Out Index : Integer) : Boolean;
  end;

  TSkPlotPaintBox = class(TSkPaintBox)
  private
    FSubAxisProperty:  TAxisLimits;
    FAxisStyle:        TAxisStyle;
    FSeriesList:       TPlotSeriesList;
    FGridStyle:        TGridStyle;
    FLegendStyle:      TLegendStyle;

    FAutoX, FAutoY:    Boolean;
    FOriginOnAxis:     Boolean;

    FChartTitle: TTextProperty;
    FXAxisTitle: TTextProperty;
    FYAxisTitle: TTextProperty;

    FXAxisFontSize  : Single;
    FYAxisFontSize  : Single;

    FPlotAreaColor:     TAlphaColor;
    FPlotBorderColor:   TAlphaColor;
    FPlotBorderWidth:   Single;
    FPlotBorderVisible: Boolean;

    FBackgroundColor : TAlphaColor;

    FOnReportCoordinates : TOnReportCoordinates;

    FLastMapper:  TPlotMapper;
    FHasMapper:   Boolean;

    FPaintBoxMenu: TPopupMenu;

    // Legend drag support
    FLegendOffset:      TPointF;   // cumulative drag offset from anchor position
    FLegendRect:        TRectF;    // last-drawn legend rect, used for hit-testing
    FDraggingLegend:    Boolean;
    FLegendDragStart:   TPointF;   // mouse position when drag began
    FLegendOffsetStart: TPointF;   // FLegendOffset value when drag began

    // Series defaults
    FDefaultsFile: String;
    procedure SetDefaultsFile(const Value: String);

    procedure DoCopyImageToClipboard (Sender: TObject);

    procedure SetAxisStyleProperty(Value: TAxisStyle);

    procedure SetSubAxisProperty(Value: TAxisLimits);
    procedure SubMaxXChanged(Sender: TObject);
    procedure AxisStyleChanged(Sender: TObject);
    procedure GridStyleChanged(Sender: TObject);

    function GetSceneScale: Single;

    procedure DrawLegend(const ACanvas: ISkCanvas; const AMapper: TPlotMapper);
    procedure DrawGrid(const ACanvas: ISkCanvas; const AMapper: TPlotMapper;
                       ABorderColor: TAlphaColor; ABorderWidth: Single;
                       ABorderVisible: Boolean);
    function  GetEffectiveDataBounds: TRectF;
    function  CalculateDataBounds: TRectF;
    procedure RenderChart(const ACanvas: ISkCanvas; const ADest: TRectF);
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
                   const AOpacity: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    procedure AddSeries(NewSeries: TPlotSeries);
    procedure ClearSeries;
    procedure SetOriginOnAxis(Value: Boolean);
    procedure ExportToPng(FileName: String; ScaleFactor: Single);
    procedure ExportToPdf(FileName: String);
    procedure ResetLegendPosition;
    // Reload series styling defaults from DefaultsFile (or built-ins if the
    // file cannot be found).  Call this before adding series to the chart.
    procedure ReloadDefaults;
  published
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function LoadData(const FileName: String; LineVisible : Boolean; MarkerVisible: Boolean; ClearSeries : Boolean) : TStringList;
    procedure ExportCSV(FileName: String; SharedXColumn: Boolean = True);
    procedure ExportCSVSeries(Directory: String);
    function ExportCSVSeriesAsString (const ADecimalPlaces: Integer; MinColumnWidth : Integer = 12) : String;

    property OnReportCoordinates : TOnReportCoordinates read FOnReportCoordinates write FOnReportCoordinates;

    property AxisLimits: TAxisLimits read FSubAxisProperty write SetSubAxisProperty;
    property AxisStyle: TAxisStyle read FAxisStyle write SetAxisStyleProperty;
    property GridStyle:      TGridStyle   read FGridStyle;
    property LegendStyle:    TLegendStyle read FLegendStyle;

    property AutoXScaling: Boolean read FAutoX write FAutoX;
    property AutoYScaling: Boolean read FAutoY write FAutoY;

    property ChartTitle: TTextProperty read FChartTitle write FChartTitle;
    property XAxisTitle: TTextProperty read FXAxisTitle write FXAxisTitle;
    property YAxisTitle: TTextProperty read FYAxisTitle write FYAxisTitle;

    property XAxisFontSize: Single read FXAxisFontSize write FXAxisFontSize;
    property YAxisFontSize: Single read FYAxisFontSize write FYAxisFontSize;

    property OriginOnAxis: Boolean read FOriginOnAxis write SetOriginOnAxis;

    property PlotAreaColor:     TAlphaColor read FPlotAreaColor     write FPlotAreaColor;
    property PlotBorderColor:   TAlphaColor read FPlotBorderColor   write FPlotBorderColor;
    property PlotBorderWidth:   Single      read FPlotBorderWidth   write FPlotBorderWidth;
    property PlotBorderVisible: Boolean     read FPlotBorderVisible write FPlotBorderVisible;
    property BackGroundColor:   TAlphaColor read FBackgroundColor   write FBackgroundColor;

    property Series: TPlotSeriesList read FSeriesList;

    // Path to a JSON file that overrides the default series styling.
    // Setting this property immediately reloads the defaults.
    property DefaultsFile: String read FDefaultsFile write SetDefaultsFile;
  end;

  const
  TickDrawingNames: array[TTickmarkDrawing] of string = (
      'Draw Out',
      'Draw In',
      'Draw Both ways'
  );

implementation

uses FMX.Dialogs, Math;

// -----------------------------------------------------------------------
//  Utility
// -----------------------------------------------------------------------

function CalculateNiceStep(ARange: Double; ATargetSteps: Integer): Double;
var
  RawStep, Exponent, Fraction: Double;
begin
  if ARange <= 0 then Exit(1.0);
  RawStep  := ARange / ATargetSteps;
  Exponent := Floor(Log10(RawStep));
  Fraction := RawStep / Power(10, Exponent);
  if      Fraction < 1.5 then Fraction := 1
  else if Fraction < 3   then Fraction := 2
  else if Fraction < 7   then Fraction := 5
  else                        Fraction := 10;
  Result := Fraction * Power(10, Exponent);
end;

// -----------------------------------------------------------------------
//  Series List
// -----------------------------------------------------------------------

function TPlotSeriesList.Find (Name : string; out Index : Integer) : Boolean;
begin
  for var i := 0 to Count - 1 do
      if SameText (Items[i].Name, Name) then
         begin
         Index := i;
         Exit (True);
         end;
  Exit(False)
end;

// -----------------------------------------------------------------------
//  TTextProperty
// -----------------------------------------------------------------------

constructor TTextProperty.Create(AText: String; AFontSize : Single);
begin
  inherited Create;
  FText     := AText;
  FVisible  := True;
  FColor    := claBlack;
  FFontSize := AFontSize;
end;

procedure TTextProperty.SetFontSize (Size : Single);
begin
  FFontSize := Size;
end;

// -----------------------------------------------------------------------
//  TAxisLimits
// -----------------------------------------------------------------------

procedure TAxisLimits.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TAxisLimits.SetMinX(const Value: Double);
begin
  if FMinX <> Value then begin FMinX := Value; Changed; end;
end;

procedure TAxisLimits.SetMaxX(const Value: Double);
begin
  if FMaxX <> Value then begin FMaxX := Value; Changed; end;
end;

procedure TAxisLimits.SetMinY(const Value: Double);
begin
  if FMinY <> Value then begin FMinY := Value; Changed; end;
end;

procedure TAxisLimits.SetMaxY(const Value: Double);
begin
  if FMaxY <> Value then begin FMaxY := Value; Changed; end;
end;

// -----------------------------------------------------------------------
//  TAxisStyle
// -----------------------------------------------------------------------


constructor TAxisStyle.Create;
begin
  inherited Create;
  FLogX  := False;
  FLogY  := False;
  FXMajorTicksVisible := True;
  FXMinorTicksVisible := True;
  FYMajorTicksVisible := True;
  FYMinorTicksVisible := True;

  FXMajorTickLength := 8;
  FXMinorTickLength := 5;
  FYMajorTickLength := 8;
  FYMinorTickLength := 5;

  FXTickDrawing := tmOut;
  FYTickDrawing := tmOut;
end;


procedure TAxisStyle.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TAxisStyle.SetLogX(Value: Boolean);
begin
  if FLogX <> Value then begin FLogX := Value; Changed; end;
end;

procedure TAxisStyle.SetLogY(Value: Boolean);
begin
  if FLogY <> Value then begin FLogY := Value; Changed; end;
end;


procedure TAxisStyle.SetXMajorTicksVisible (Value : Boolean);
begin
  if FXMajorTicksVisible <> Value then begin FXMajorTicksVisible := Value; Changed; end;
end;


procedure TAxisStyle.SetXMinorTicksVisible (Value : Boolean);
begin
  if FXMinorTicksVisible <> Value then begin FXMinorTicksVisible := Value; Changed; end;
end;


procedure TAxisStyle.SetYMajorTicksVisible (Value : Boolean);
begin
  if FYMajorTicksVisible <> Value then begin FYMajorTicksVisible := Value; Changed; end;
end;


procedure TAxisStyle.SetYMinorTicksVisible (Value : Boolean);
begin
  if FYMinorTicksVisible <> Value then begin FYMinorTicksVisible := Value; Changed; end;
end;

procedure TAxisStyle.SetXMajorTickLength(Value: Single);
begin
  if (FXMajorTickLength <> Value) and (Value >= 0) then
  begin FXMajorTickLength := Value; Changed; end;
end;

procedure TAxisStyle.SetXMinorTickLength(Value: Single);
begin
  if (FXMinorTickLength <> Value) and (Value >= 0) then
  begin FXMinorTickLength := Value; Changed; end;
end;

procedure TAxisStyle.SetYMajorTickLength(Value: Single);
begin
  if (FYMajorTickLength <> Value) and (Value >= 0) then
  begin FYMajorTickLength := Value; Changed; end;
end;

procedure TAxisStyle.SetYMinorTickLength(Value: Single);
begin
  if (FYMinorTickLength <> Value) and (Value >= 0) then
  begin FYMinorTickLength := Value; Changed; end;
end;


procedure TAxisStyle.SetXTickDrawing (Value : TTickmarkDrawing);
begin
  if FXTickDrawing <> Value then
     begin FXTickDrawing := Value; Changed; end
end;


procedure TAxisStyle.SetYTickDrawing (Value : TTickmarkDrawing);
begin
  if FYTickDrawing <> Value then
     begin FYTickDrawing := Value; Changed; end
end;

// -----------------------------------------------------------------------
//  TGridStyle
// -----------------------------------------------------------------------

constructor TGridStyle.Create;
begin
  inherited Create;
  FXMajorVisible := True;
  FYMajorVisible := True;
  FXMinorVisible := False;
  FYMinorVisible := False;
  FXMajorColor   := TAlphaColors.Gray;
  FYMajorColor   := TAlphaColors.Gray;
  FXMinorColor   := TAlphaColorRec.LightGrey;
  FYMinorColor   := TAlphaColorRec.LightGrey;
  FXMajorWidth   := 1.0;
  FYMajorWidth   := 1.0;
  FXMinorWidth   := 0.5;
  FYMinorWidth   := 0.5;
  FXMinorDivisions := 5;
  FYMinorDivisions := 5;
  FXMajorDivisions := 5;
  FYMajorDivisions := 5;
end;

procedure TGridStyle.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGridStyle.SetXMajorVisible(Value: Boolean);
begin
  if FXMajorVisible <> Value then begin FXMajorVisible := Value; Changed; end;
end;

procedure TGridStyle.SetYMajorVisible(Value: Boolean);
begin
  if FYMajorVisible <> Value then begin FYMajorVisible := Value; Changed; end;
end;

procedure TGridStyle.SetXMinorVisible(Value: Boolean);
begin
  if FXMinorVisible <> Value then begin FXMinorVisible := Value; Changed; end;
end;

procedure TGridStyle.SetYMinorVisible(Value: Boolean);
begin
  if FYMinorVisible <> Value then begin FYMinorVisible := Value; Changed; end;
end;

procedure TGridStyle.SetXMajorColor(Value: TAlphaColor);
begin
  if FXMajorColor <> Value then begin FXMajorColor := Value; Changed; end;
end;

procedure TGridStyle.SetYMajorColor(Value: TAlphaColor);
begin
  if FYMajorColor <> Value then begin FYMajorColor := Value; Changed; end;
end;

procedure TGridStyle.SetXMinorColor(Value: TAlphaColor);
begin
  if FXMinorColor <> Value then begin FXMinorColor := Value; Changed; end;
end;

procedure TGridStyle.SetYMinorColor(Value: TAlphaColor);
begin
  if FYMinorColor <> Value then begin FYMinorColor := Value; Changed; end;
end;

procedure TGridStyle.SetXMajorWidth(Value: Single);
begin
  if FXMajorWidth <> Value then begin FXMajorWidth := Value; Changed; end;
end;

procedure TGridStyle.SetYMajorWidth(Value: Single);
begin
  if FYMajorWidth <> Value then begin FYMajorWidth := Value; Changed; end;
end;

procedure TGridStyle.SetXMinorWidth(Value: Single);
begin
  if FXMinorWidth <> Value then begin FXMinorWidth := Value; Changed; end;
end;

procedure TGridStyle.SetYMinorWidth(Value: Single);
begin
  if FYMinorWidth <> Value then begin FYMinorWidth := Value; Changed; end;
end;

procedure TGridStyle.SetXMinorDivisions(Value: Integer);
begin
  if (FXMinorDivisions <> Value) and (Value >= 2) then
  begin
    FXMinorDivisions := Value;
    Changed;
  end;
end;

procedure TGridStyle.SetYMinorDivisions(Value: Integer);
begin
  if (FYMinorDivisions <> Value) and (Value >= 2) then
  begin
    FYMinorDivisions := Value;
    Changed;
  end;
end;

procedure TGridStyle.SetXMajorDivisions(Value: Integer);
begin
  if (FXMajorDivisions <> Value) and (Value >= 1) then
  begin
    FXMajorDivisions := Value;
    Changed;
  end;
end;

procedure TGridStyle.SetYMajorDivisions(Value: Integer);
begin
  if (FYMajorDivisions <> Value) and (Value >= 1) then
  begin
    FYMajorDivisions := Value;
    Changed;
  end;
end;

// -----------------------------------------------------------------------
//  TLegendStyle
// -----------------------------------------------------------------------

constructor TLegendStyle.Create;
begin
  inherited Create;
  FVisible           := True;
  FBorderVisible     := True;
  FBorderColor       := TAlphaColors.Black;
  FBorderWidth       := 1.0;
  FBackgroundColor   := TAlphaColors.White;
  FBackgroundOpacity := 0.86;
  FLocation          := llTopRight;
end;

// -----------------------------------------------------------------------
//  TSkPlotPaintBox
// -----------------------------------------------------------------------

constructor TSkPlotPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  HitTest := True;

  FSubAxisProperty := TAxisLimits.Create;
  FSubAxisProperty.FMinX := 0.0;  FSubAxisProperty.FMaxX := 1.0;
  FSubAxisProperty.FMinY := 0.0;  FSubAxisProperty.FMaxY := 1.0;
  FSubAxisProperty.OnChange := SubMaxXChanged;

  FXAxisFontSize  := 14;
  FYAxisFontSize  := 14;

  FGridStyle := TGridStyle.Create;
  FGridStyle.OnChange := GridStyleChanged;

  FAxisStyle := TAxisStyle.Create;
  FAxisStyle.OnChange := AxisStyleChanged;

  FLegendStyle := TLegendStyle.Create;

  FSeriesList := TPlotSeriesList.Create;

  FChartTitle := TTextProperty.Create('Data Plot', 16);
  FXAxisTitle := TTextProperty.Create('X Axis', 14);
  FYAxisTitle := TTextProperty.Create('Y Axis', 14);

  Self.Width  := 600;
  Self.Height := 300;

  FAutoX := True;
  FAutoY := True;
  FOriginOnAxis := False;

  FPlotAreaColor     := TAlphaColors.White;
  FPlotBorderColor   := TAlphaColors.Black;
  FPlotBorderWidth   := 1.5;
  FPlotBorderVisible := True;
  FBackgroundColor   := TAlphaColors.White;

  // Legend drag initialisation
  FLegendOffset      := TPointF.Zero;
  FLegendRect        := TRectF.Empty;
  FDraggingLegend    := False;
  FLegendDragStart   := TPointF.Zero;
  FLegendOffsetStart := TPointF.Zero;

  // Defaults file — empty means use built-in values only
  FDefaultsFile := '';

  SetOriginOnAxis(True);
end;

destructor TSkPlotPaintBox.Destroy;
begin
  FSubAxisProperty.Free;
  FAxisStyle.Free;
  FGridStyle.Free;
  FLegendStyle.Free;
  FSeriesList.Free;
  FChartTitle.Free;
  FXAxisTitle.Free;
  FYAxisTitle.Free;
  inherited Destroy;
end;

//procedure TSkPlotPaintBox.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
//                                const AOpacity: Single);
//begin
//  inherited Draw(ACanvas, ADest, AOpacity);
//  RenderChart(ACanvas, ADest);
//end;

function TSkPlotPaintBox.GetSceneScale: Single;
begin
  if Scene <> nil then
    Result := Scene.GetSceneScale
  else
    Result := 1.0;
end;

procedure TSkPlotPaintBox.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
                                const AOpacity: Single);
var
  LSurface: ISkSurface;
  LImage:   ISkImage;
  LScale:   Single;
  PW, PH:   Integer;       { physical pixels }
begin
  inherited Draw(ACanvas, ADest, AOpacity);

  LScale := GetSceneScale;
  PW := Round(ADest.Width  * LScale);
  PH := Round(ADest.Height * LScale);
  if (PW <= 0) or (PH <= 0) then Exit;

  LSurface := TSkSurface.MakeRaster(PW, PH);
  if LSurface = nil then
  begin
    RenderChart(ACanvas, ADest);
    Exit;
  end;

  { Scale the offscreen canvas so RenderChart can draw in logical units
    (its existing coordinate system) but the result lands at physical
    resolution. Same trick ExportToPng uses with ScaleFactor. }
  LSurface.Canvas.Scale(LScale, LScale);
  RenderChart(LSurface.Canvas, TRectF.Create(0, 0, ADest.Width, ADest.Height));

  LImage := LSurface.MakeImageSnapshot;
  if LImage <> nil then
    { Destination rect is in logical points — same as ADest. The image is
      at physical pixel resolution. CoreAnimation maps physical pixels of
      the image to physical pixels of the backing store 1:1. No sampling. }
    ACanvas.DrawImageRect(LImage,
                          TRectF.Create(0, 0, PW, PH),       { source: full image, physical }
                          ADest,                              { dest: logical points }
                          TSkSamplingOptions.Create(TSkFilterMode.Nearest,
                                                    TSkMipmapMode.None));
end;


procedure TSkPlotPaintBox.SetAxisStyleProperty(Value: TAxisStyle);
begin
  FAxisStyle.Assign(Value);
end;

procedure TSkPlotPaintBox.SetSubAxisProperty(Value: TAxisLimits);
begin
  FSubAxisProperty.Assign(Value);
end;

procedure TSkPlotPaintBox.SubMaxXChanged(Sender: TObject);
begin
  Redraw;
end;

procedure TSkPlotPaintBox.AxisStyleChanged(Sender: TObject);
begin
  Redraw;
end;

procedure TSkPlotPaintBox.GridStyleChanged(Sender: TObject);
begin
  Redraw;
end;

procedure TSkPlotPaintBox.SetOriginOnAxis(Value: Boolean);
begin
  if FOriginOnAxis <> Value then begin FOriginOnAxis := Value; Redraw; end;
end;

procedure TSkPlotPaintBox.AddSeries(NewSeries: TPlotSeries);
begin
  FSeriesList.Add(NewSeries);
end;

procedure TSkPlotPaintBox.ClearSeries;
begin
  FSeriesList.Clear;
end;

procedure TSkPlotPaintBox.ResetLegendPosition;
begin
  FLegendOffset := TPointF.Zero;
  Redraw;
end;

procedure TSkPlotPaintBox.ReloadDefaults;
begin
  // Always start from the built-in values so a missing key in the JSON
  // file does not inherit a stale value from a previous load.
  TPlotDefaultsLoader.ResetToBuiltIn;

  if FDefaultsFile <> '' then
    TPlotDefaultsLoader.LoadFromFile(FDefaultsFile);
  // Existing series are unaffected; the new defaults apply to any
  // series created after this call.
end;

procedure TSkPlotPaintBox.SetDefaultsFile(const Value: String);
begin
  if FDefaultsFile <> Value then
  begin
    FDefaultsFile := Value;
    ReloadDefaults;
  end;
end;

procedure TSkPlotPaintBox.ExportToPng(FileName: String; ScaleFactor: Single);
var
  LSurface: ISkSurface;
  LCanvas:  ISkCanvas;
  LImage:   ISkImage;
begin
  LSurface := TSkSurface.MakeRaster(Round(Width * ScaleFactor),
                                    Round(Height * ScaleFactor));
  if LSurface = nil then Exit;
  LCanvas := LSurface.Canvas;
  LCanvas.Scale(ScaleFactor, ScaleFactor);
  RenderChart(LCanvas, TRectF.Create(0, 0, Width, Height));
  LImage := LSurface.MakeImageSnapshot;
  if LImage <> nil then
    LImage.EncodeToFile(FileName);
end;

procedure TSkPlotPaintBox.ExportToPdf(FileName: String);
var
  LDocument: ISkDocument;
  LCanvas:   ISkCanvas;
  LStream:   TStream;
begin
  LStream := TFileStream.Create(FileName, fmCreate);
  try
    LDocument := TSkDocument.MakePDF(LStream);
    LCanvas   := LDocument.BeginPage(Width, Height);
    try
      RenderChart(LCanvas, TRectF.Create(0, 0, Width, Height));
    finally
      LDocument.EndPage;
    end;
    LDocument.Close;
    ShowMessage('PDF exported successfully to ' + FileName);
  finally
    LStream.Free;
  end;
end;


procedure TSkPlotPaintBox.DoCopyImageToClipboard (Sender: TObject);
var
  LSurface: ISkSurface;
  LCanvas:  ISkCanvas;
  LImage:   ISkImage;
  ScaleFactor : Single;
  FmxBitmap: TBitmap;
  ClipboardService: IFMXClipboardService;
begin
  ScaleFactor := 2;
  LSurface := TSkSurface.MakeRaster(Round(Width * ScaleFactor),
                                    Round(Height * ScaleFactor));
  if LSurface = nil then Exit;
  LCanvas := LSurface.Canvas;
  LCanvas.Scale(ScaleFactor, ScaleFactor);
  RenderChart(LCanvas, TRectF.Create(0, 0, Width, Height));
  LImage := LSurface.MakeImageSnapshot;

  if LImage <> nil then
  begin
    // 1. Convert the Skia image into an FMX TBitmap
    FmxBitmap := SkImageToBitmap(LImage);
    try
      // 2. Request the system clipboard service from FireMonkey
      if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardService) then
      begin
        // 3. Send the image to the OS clipboard wrapper using a TValue wrapper
        ClipboardService.SetClipboard(TValue.From<TBitmap>(FmxBitmap));
      end;
    finally
      // 4. Free the temporary FMX bitmap wrapper
      FmxBitmap.Free;
    end;
  end;
end;

function TSkPlotPaintBox.LoadData(const FileName: String; LineVisible : Boolean; MarkerVisible: Boolean; ClearSeries : Boolean) : TStringList;
var
  CSV:  TCSV;
  i, j: Integer;
  ps:   TPlotSeries;
begin
  CSV := TCSV.Create(nil);
  try
    CSV.ReadCSV(FileName);
    if ClearSeries then
       FSeriesList.Clear;
    Result := TStringList.Create;
    for i := 1 to CSV.Cols - 1 do
    begin
      ps := TPlotSeries.Create(CSV.header[i], TColorManager.NextColor);
      Result.AddObject(CSV.header[i], ps);
      ps.LineVisible := LineVisible;
      ps.MarkerVisible := MarkerVisible;
      for j := 0 to CSV.Rows - 1 do
        ps.AddXY(CSV.data[j, 0].number, CSV.data[j, i].number);
      FSeriesList.Add(ps);
    end;
    Redraw;
  finally
    CSV.Free;
  end;
end;


function StrToFloatLocale(const S: string): Double;
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  Result := StrToFloat(S, FS);
end;


function FloatToStrLocale(Value: Double; DecimalPlaces: Integer = 5): string;
var
  FS: TFormatSettings;
  AbsVal: Double;
begin
  FS := TFormatSettings.Create('en-US');
  AbsVal := Abs(Value);

  // Logic: Use standard for "normal" numbers, Scientific for extremes
  if (AbsVal > 0) and ((AbsVal < 0.001) or (AbsVal >= 1000000)) then
    Result := FormatFloat('0.' + StringOfChar('0', DecimalPlaces) + 'E+00', Value, FS)
  else
    Result := FormatFloat('0.' + StringOfChar('0', DecimalPlaces), Value, FS);
end;



// Format a single tick-label value for an axis.
// AStep is the spacing between major ticks; it sets the number of decimals
// needed so two adjacent labels look different. Values that are very small
// or very large fall back to scientific notation.
function FormatAxisLabel(Value, AStep: Double): string;
var
  AbsVal, AbsStep: Double;
  Decimals:       Integer;
begin
  AbsVal  := Abs(Value);
  AbsStep := Abs(AStep);

  // Treat near-zero as exact zero so we don't print "1.00E-17" at the origin.
  if (AbsStep > 0) and (AbsVal < AbsStep * 1e-9) then
    Exit('0');

  // Use scientific notation when the magnitude (or the step) is tiny or huge.
  // The step matters too: a range like 0.001..0.005 needs scientific even
  // though 0.005 itself isn't astronomically small.
  if ((AbsVal > 0) and ((AbsVal < 1e-3) or (AbsVal >= 1e6))) or
     ((AbsStep > 0) and ((AbsStep < 1e-3) or (AbsStep >= 1e6))) then
  begin
    Result := FloatToStrF(Value, ffExponent, 3, 2);
    Exit;
  end;

  // Linear regime: pick enough decimals to distinguish adjacent ticks.
  if AbsStep >= 1 then
    Decimals := 0
  else if AbsStep > 0 then
    Decimals := Max(0, -Floor(Log10(AbsStep)))
  else
    Decimals := 2;

  if Decimals = 0 then
    Result := FormatFloat('0', Value)
  else
    Result := FormatFloat('0.' + StringOfChar('0', Decimals), Value);
end;


procedure TSkPlotPaintBox.ExportCSV(FileName: String; SharedXColumn: Boolean = True);
var
  SL      : TStringList;
  Header  : string;
  Row     : string;
  I, J    : Integer;
  Series  : TPlotSeries;
  NumRows : Integer;
begin
  if (FSeriesList = nil) or (FSeriesList.Count = 0) then
    Exit;

  SL := TStringList.Create;
  try
    if SharedXColumn then
      begin
        // --- Shared X column: time, S1, S2, ... ---
        Header := 'time';
        for I := 0 to FSeriesList.Count - 1 do
          Header := Header + ',' + FSeriesList[I].Name;
        SL.Add(Header);

        NumRows := 0;
        for I := 0 to FSeriesList.Count - 1 do
          if FSeriesList[I].Data.Count > NumRows then
            NumRows := FSeriesList[I].Data.Count;

        for J := 0 to NumRows - 1 do
          begin
            // X comes from the first series that has data at row J
            Row := '';
            for I := 0 to FSeriesList.Count - 1 do
              if J < FSeriesList[I].Data.Count then
                begin
                  Row := FloatToStrLocale(FSeriesList[I].Data[J].X);
                  Break;
                end;
            for I := 0 to FSeriesList.Count - 1 do
              begin
                Series := FSeriesList[I];
                if J < Series.Data.Count then
                  Row := Row + ',' + FloatToStrLocale(Series.Data[J].Y)
                else
                  Row := Row + ',';
              end;
            SL.Add(Row);
          end;
      end
    else
      begin
        // --- Paired X columns: time_S1, S1, time_S2, S2, ... ---
        Header := '';
        for I := 0 to FSeriesList.Count - 1 do
          begin
            if I > 0 then Header := Header + ',';
            Header := Header + 'time_' + FSeriesList[I].Name + ',' + FSeriesList[I].Name;
          end;
        SL.Add(Header);

        NumRows := 0;
        for I := 0 to FSeriesList.Count - 1 do
          if FSeriesList[I].Data.Count > NumRows then
            NumRows := FSeriesList[I].Data.Count;

        for J := 0 to NumRows - 1 do
          begin
            Row := '';
            for I := 0 to FSeriesList.Count - 1 do
              begin
                if I > 0 then Row := Row + ',';
                Series := FSeriesList[I];
                if J < Series.Data.Count then
                  Row := Row + FloatToStrLocale(Series.Data[J].X) + ',' +
                               FloatToStrLocale(Series.Data[J].Y)
                else
                  Row := Row + ',';
              end;
            SL.Add(Row);
          end;
      end;

    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;


procedure TSkPlotPaintBox.ExportCSVSeries(Directory: String);
var
  I    : Integer;
  SL   : TStringList;
  J    : Integer;
  Series : TPlotSeries;
  FileName : String;
begin
  if (FSeriesList = nil) or (FSeriesList.Count = 0) then Exit;

  SL := TStringList.Create;
  try
    for I := 0 to FSeriesList.Count - 1 do
    begin
      Series := FSeriesList[I];
      SL.Clear;
      SL.Add('time,' + Series.Name);
      for J := 0 to Series.Data.Count - 1 do
        SL.Add(FloatToStr(Series.Data[J].X) + ',' + FloatToStr(Series.Data[J].Y));
      FileName := IncludeTrailingPathDelimiter(Directory) + Series.Name + '.csv';
      SL.SaveToFile(FileName);
    end;
  finally
    SL.Free;
  end;
end;


function TSkPlotPaintBox.ExportCSVSeriesAsString(const ADecimalPlaces: Integer; MinColumnWidth : Integer = 12): String;
var
  I, J: Integer;
  SL: TStringList;
  ColWidths: TArray<Integer>;
  FormattedVal: String;
  RowStr: String;
begin
  if (FSeriesList = nil) or (FSeriesList.Count = 0) then Exit;

  SetLength(ColWidths, FSeriesList.Count + 1);
  for I := 0 to High(ColWidths) do ColWidths[I] := MinColumnWidth;

  // 1. PRE-SCAN: Only find max width, but keep it reasonable
  for I := 0 to FSeriesList.Count - 1 do
  begin
    if Length(FSeriesList[I].Name) > ColWidths[I+1] then
      ColWidths[I+1] := Length(FSeriesList[I].Name);

    for J := 0 to FSeriesList[I].Data.Count - 1 do
    begin
      FormattedVal := FloatToStrLocale(FSeriesList[I].Data[J].Y, ADecimalPlaces);
      if Length(FormattedVal) > ColWidths[I+1] then
        ColWidths[I+1] := Length(FormattedVal);
    end;
  end;

  // 2. BUILD
  SL := TStringList.Create;
  try
    // Header
    RowStr := 'time'.PadLeft(ColWidths[0]);
    for I := 0 to FSeriesList.Count - 1 do
      RowStr := RowStr + ',' + FSeriesList[I].Name.PadLeft(ColWidths[I+1]);
    SL.Add(RowStr);

    // Rows
    for J := 0 to FSeriesList[0].Data.Count - 1 do
    begin
      RowStr := FloatToStrLocale(FSeriesList[0].Data[J].X, ADecimalPlaces).PadLeft(ColWidths[0]);
      for I := 0 to FSeriesList.Count - 1 do
      begin
        if J < FSeriesList[I].Data.Count then
          FormattedVal := FloatToStrLocale(FSeriesList[I].Data[J].Y, ADecimalPlaces)
        else
          FormattedVal := '';
        RowStr := RowStr + ',' + FormattedVal.PadLeft(ColWidths[I+1]);
      end;
      SL.Add(RowStr);
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;





// -----------------------------------------------------------------------
//  DrawLegend
// -----------------------------------------------------------------------

// Returns AColor with its alpha channel replaced by AOpacity (0.0–1.0).
function BlendOpacity(AColor: TAlphaColor; AOpacity: Single): TAlphaColor;
var
  Alpha: Byte;
begin
  Alpha  := Round(EnsureRange(AOpacity, 0.0, 1.0) * 255);
  Result := (TAlphaColor(Alpha) shl 24) or (AColor and $00FFFFFF);
end;

procedure TSkPlotPaintBox.DrawLegend(const ACanvas: ISkCanvas;
                                     const AMapper: TPlotMapper);
var
  LPaint, LTextPaint: ISkPaint;
  LFont:       ISkFont;
  LegendRect:  TRectF;
  I:           Integer;
  ItemY, MaxWidth: Single;
  LegendWidth, LegendHeight: Single;
  Series: TPlotSeries;
  P1: TPointF;
  BgColor: TAlphaColor;
  NumberOfSeries, SeriesCount : Integer;
  LIntervals: TArray<Single>;
begin
  if not FLegendStyle.Visible then Exit;
  if FSeriesList.Count = 0 then Exit;

  LTextPaint := TSkPaint.Create(TSkPaintStyle.Fill);
  LTextPaint.Color := TAlphaColors.Black;
  LFont := TSkFont.Create(nil, 12);

  LPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LPaint.AntiAlias := True;

  // Measure the widest label to size the box
  MaxWidth := 0;
  for Series in FSeriesList do
    if Series.SeriesVisible then
       MaxWidth := Max(MaxWidth, LFont.MeasureText(Series.Name));

  LegendWidth  := MaxWidth + 70;
  // Count how many series are visible, determines legend height
  NumberOfSeries := 0;
  for Series in FSeriesList do
      if Series.SeriesVisible then
         Inc (NumberOfSeries);

  LegendHeight := NumberOfSeries * 22 + 10;
  LegendRect   := TRectF.Create(0, 0, LegendWidth, LegendHeight);

  // Position according to FLegendStyle.Location
  case FLegendStyle.Location of
    llTopRight:
      LegendRect.Offset(AMapper.PixelRect.Right  - LegendWidth  - 15,
                        AMapper.PixelRect.Top    + 15);
    llTopLeft:
      LegendRect.Offset(AMapper.PixelRect.Left   + 15,
                        AMapper.PixelRect.Top    + 15);
    llBottomRight:
      LegendRect.Offset(AMapper.PixelRect.Right  - LegendWidth  - 15,
                        AMapper.PixelRect.Bottom - LegendHeight - 15);
    llBottomLeft:
      LegendRect.Offset(AMapper.PixelRect.Left   + 15,
                        AMapper.PixelRect.Bottom - LegendHeight - 15);
  end;

  // Apply the user drag offset, then cache the rect for hit-testing
  LegendRect.Offset(FLegendOffset.X, FLegendOffset.Y);
  FLegendRect := LegendRect;

  // Background fill
  BgColor := BlendOpacity(FLegendStyle.BackgroundColor,
                           FLegendStyle.BackgroundOpacity);
  LPaint.Style := TSkPaintStyle.Fill;
  LPaint.Color := BgColor;
  ACanvas.DrawRect(LegendRect, LPaint);

  // Border
  if FLegendStyle.BorderVisible then
  begin
    LPaint.Style       := TSkPaintStyle.Stroke;
    LPaint.Color       := FLegendStyle.BorderColor;
    LPaint.StrokeWidth := FLegendStyle.BorderWidth;
    ACanvas.DrawRect(LegendRect, LPaint);
  end;

  // Series entries
  SeriesCount := 0;
  for I := 0 to FSeriesList.Count - 1 do
  begin
    Series := FSeriesList[I];
    if Series.SeriesVisible then
       begin
       ItemY  := LegendRect.Top + 20 + (SeriesCount * 22);
       Inc (SeriesCount);

    if Series.LineVisible then
       begin
       LPaint.Style       := TSkPaintStyle.Stroke;
       LPaint.Color       := Series.LineColor;
       LPaint.StrokeWidth := 2.0;
       LPaint.StrokeCap := TSkStrokeCap.Round;

       if Series.LineStyle <> TLineStyle.ltSolid  then
          begin
          case Series.LineStyle of
             TLineStyle.ltDashDash: LIntervals := [5, 5, 5, 5];
             TLineStyle.ltDotDot:  LIntervals := [3, 3, 3, 3];
          end;
          LPaint.StrokeCap := TSkStrokeCap.Round;
          LPaint.StrokeJoin := TSkStrokeJoin.Round;
          LPaint.PathEffect := TSkPathEffect.MakeDash(LIntervals, 0);
          end;

       ACanvas.DrawLine(LegendRect.Left + 15, ItemY - 4,
                       LegendRect.Left + 45, ItemY - 4, LPaint);
       end;
       LPaint.PathEffect := nil;

    if Series.MarkerVisible then
    begin
      P1 := TPointF.Create(LegendRect.Left + 30, ItemY - 4);
      LPaint.StrokeWidth := 1;
      Series.DrawMarker (ACanvas, P1, 3.5, LPaint);
      //LPaint.Color := Series.MarkerFillColor;
      //ACanvas.DrawCircle(P1, 3.5, LPaint);
      //LPaint.Style       := TSkPaintStyle.Stroke;
      //LPaint.Color       := Series.MarkerStrokeColor;
      //LPaint.StrokeWidth := 1;
      //ACanvas.DrawCircle(P1, 3.5, LPaint);
    end;

    if Series.SeriesVisible then
       ACanvas.DrawSimpleText(Series.Name, LegendRect.Left + 55, ItemY,
                           LFont, LTextPaint);
       end;
  end;
end;

// -----------------------------------------------------------------------
//  DrawGrid
// -----------------------------------------------------------------------

procedure TSkPlotPaintBox.DrawGrid(const ACanvas: ISkCanvas;
                                   const AMapper: TPlotMapper;
                                   ABorderColor: TAlphaColor;
                                   ABorderWidth: Single;
                                   ABorderVisible: Boolean);
var
  LXMajorPaint, LYMajorPaint: ISkPaint;
  LXMinorPaint, LYMinorPaint: ISkPaint;
  LTickPaint, LTextPaint:     ISkPaint;
  LFont, LTitleFont:          ISkFont;
  LX, LY, TextWidth, TitleWidth: Single;
  ValX, ValY, StepX, StepY, MinorVal: Double;
  MinorStepX, MinorStepY, MinorValX, MinorValY: Double;
  LabelText: string;
  K: Integer;
begin
  // X major grid (dashed vertical lines)
  LXMajorPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LXMajorPaint.Color       := FGridStyle.XMajorColor;
  LXMajorPaint.StrokeWidth := FGridStyle.XMajorWidth;
  LXMajorPaint.PathEffect  := TSkPathEffect.MakeDash([4, 4], 0);

  // Y major grid (dashed horizontal lines)
  LYMajorPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LYMajorPaint.Color       := FGridStyle.YMajorColor;
  LYMajorPaint.StrokeWidth := FGridStyle.YMajorWidth;
  LYMajorPaint.PathEffect  := TSkPathEffect.MakeDash([4, 4], 0);

  // X minor grid
  LXMinorPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LXMinorPaint.Color       := FGridStyle.XMinorColor;
  LXMinorPaint.StrokeWidth := FGridStyle.XMinorWidth;
  LXMinorPaint.AntiAlias   := True;

  // Y minor grid
  LYMinorPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LYMinorPaint.Color       := FGridStyle.YMinorColor;
  LYMinorPaint.StrokeWidth := FGridStyle.YMinorWidth;
  LYMinorPaint.AntiAlias   := True;

  // Axis lines, tick marks, and border
  LTickPaint := TSkPaint.Create(TSkPaintStyle.Stroke);
  LTickPaint.Color       := ABorderColor;
  LTickPaint.StrokeWidth := ABorderWidth;
  LTickPaint.AntiAlias   := True;

  LTextPaint := TSkPaint.Create(TSkPaintStyle.Fill);
  LTextPaint.Color     := TAlphaColors.Black;
  LTextPaint.AntiAlias := True;

  LFont      := TSkFont.Create(nil, 14);
  LTitleFont := TSkFont.Create(TSkTypeface.MakeDefault, 16);

  // -----------------------------------------------------------------------
  //  X-AXIS
  // -----------------------------------------------------------------------
  if AMapper.LogX then
    begin
      var LogLeft  := Log10(Max(1e-9, AMapper.DataRect.Left));
      var LogRight := Log10(Max(1e-9, AMapper.DataRect.Right));
      var NumDecades := Max(1, Ceil(LogRight - LogLeft));
      var DecadeStride := Max(1, Round(NumDecades / Max(1, FGridStyle.XMajorDivisions)));
      var DecadeFactor := Power(10, DecadeStride);

      ValX := Power(10, Floor(LogLeft));
      while (ValX <= AMapper.DataRect.Right) and (ValX > 0) do
      begin
        // Minor ticks (decade subdivisions) only meaningful when stride = 1
        if DecadeStride = 1 then
        begin
          for K := 2 to 9 do
          begin
            MinorVal := ValX * K;
            if (MinorVal > AMapper.DataRect.Left) and
               (MinorVal <= AMapper.DataRect.Right) then
            begin
              LX := AMapper.MapX(MinorVal);
              if FGridStyle.XMinorVisible then
                ACanvas.DrawLine(LX, AMapper.PixelRect.Top,
                                 LX, AMapper.PixelRect.Bottom, LXMinorPaint);
              if FAxisStyle.XMinorTicksVisible then
                 begin
                 case FAxisStyle.FXTickDrawing of
                   tmIn: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMinorTickLength, LTickPaint);
                   tmOut: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMinorTickLength, LTickPaint);
                   tmBoth: begin
                           ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMinorTickLength, LTickPaint);
                           ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMinorTickLength, LTickPaint);
                           end;
                   end;
                 end;
            end;
          end;
        end;

        if ValX >= AMapper.DataRect.Left then
        begin
          LX := AMapper.MapX(ValX);
          if FGridStyle.XMajorVisible then
            ACanvas.DrawLine(LX, AMapper.PixelRect.Top,
                            LX, AMapper.PixelRect.Bottom, LXMajorPaint);
          if AxisStyle.XMajorTicksVisible then
             begin
             case FAxisStyle.FXTickDrawing of
               tmIn: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMinorTickLength, LTickPaint);
               tmOut: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMinorTickLength, LTickPaint);
               tmBoth: begin
                       ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMinorTickLength, LTickPaint);
                       ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMinorTickLength, LTickPaint);
                       end;
             end;
             end;
          LabelText := FormatAxisLabel(ValX, ValX);
          LFont.Size := XAxisFontSize;
          TextWidth := LFont.MeasureText(LabelText);
          ACanvas.DrawSimpleText(LabelText, LX - (TextWidth / 2),
                                 AMapper.PixelRect.Bottom + FAxisStyle.XMajorTickLength + 14, LFont, LTextPaint);
        end;
        ValX := ValX * DecadeFactor;
      end;
    end
  else
  begin
    // Linear X — minor lines first so major lines paint over them
    StepX := CalculateNiceStep(AMapper.DataRect.Width, FGridStyle.XMajorDivisions);

    if (FGridStyle.XMinorDivisions > 1) then
    begin
      MinorStepX := StepX / FGridStyle.XMinorDivisions;
      MinorValX  := Ceil(AMapper.DataRect.Left / MinorStepX) * MinorStepX;
      while MinorValX <= AMapper.DataRect.Right do
      begin
        if Abs(MinorValX / StepX - Round(MinorValX / StepX)) > 1e-9 then
        begin
          LX := AMapper.MapX(MinorValX);
          if  FGridStyle.XMinorVisible then
             ACanvas.DrawLine(LX, AMapper.PixelRect.Top,
                           LX, AMapper.PixelRect.Bottom, LXMinorPaint);
          if FAxisStyle.FXMinorTicksVisible then
            begin
            case FAxisStyle.FXTickDrawing of
               tmIn: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMinorTickLength, LTickPaint);
               tmOut: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMinorTickLength, LTickPaint);
               tmBoth: begin
                       ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMinorTickLength, LTickPaint);
                       ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMinorTickLength, LTickPaint)
                       end;
            end;
            end;
        end;
        MinorValX := MinorValX + MinorStepX;
      end;
    end;

    ValX := Ceil(AMapper.DataRect.Left / StepX) * StepX;
    while ValX <= AMapper.DataRect.Right do
    begin
      LX := AMapper.MapX(ValX);
      if FGridStyle.XMajorVisible then
        ACanvas.DrawLine(LX, AMapper.PixelRect.Top,
                         LX, AMapper.PixelRect.Bottom, LXMajorPaint);
      if FAxisStyle.FXMajorTicksVisible then
         begin
          case FAxisStyle.FXTickDrawing of
             tmIn: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMajorTickLength, LTickPaint);
             tmOut: ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMajorTickLength, LTickPaint);
             tmBoth: begin
                     ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom + FAxisStyle.XMajorTickLength, LTickPaint);
                     ACanvas.DrawLine(LX, AMapper.PixelRect.Bottom, LX, AMapper.PixelRect.Bottom - FAxisStyle.XMajorTickLength, LTickPaint)
                     end;
          end;
          end;
      LabelText := FormatAxisLabel(ValX, StepX);
      LFont.Size := XAxisFontSize;
      TextWidth := LFont.MeasureText(LabelText);
      ACanvas.DrawSimpleText(LabelText, LX - (TextWidth / 2),
                             AMapper.PixelRect.Bottom + FAxisStyle.XMajorTickLength + 14, LFont, LTextPaint);
      ValX := ValX + StepX;
    end;
  end;

  // -----------------------------------------------------------------------
  //  Y-AXIS
  // -----------------------------------------------------------------------
  if AMapper.LogY then
    begin
      var LogTop    := Log10(Max(1e-9, AMapper.DataRect.Top));
      var LogBottom := Log10(Max(1e-9, AMapper.DataRect.Bottom));
      var NumDecades := Max(1, Ceil(LogBottom - LogTop));
      var DecadeStride := Max(1, Round(NumDecades / Max(1, FGridStyle.YMajorDivisions)));
      var DecadeFactor := Power(10, DecadeStride);

      ValY := Power(10, Floor(LogTop));
      while (ValY <= AMapper.DataRect.Bottom) and (ValY > 0) do
      begin
        if DecadeStride = 1 then
        begin
          for K := 2 to 9 do
          begin
            MinorVal := ValY * K;
            if (MinorVal > AMapper.DataRect.Top) and
               (MinorVal <= AMapper.DataRect.Bottom) then
            begin
              LY := AMapper.MapY(MinorVal);
              if FGridStyle.YMinorVisible then
                ACanvas.DrawLine(AMapper.PixelRect.Left, LY,
                                 AMapper.PixelRect.Right, LY, LYMinorPaint);
              if FAxisStyle.FYMinorTicksVisible then
                 begin
                 case FAxisStyle.FYTickDrawing of
                  tmIn: ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                  tmOut: ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                  tmBoth: begin
                          ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                          ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                          end;
                 end;
                 end;
            end;
          end;
        end;

        if ValY >= AMapper.DataRect.Top then
        begin
          LY := AMapper.MapY(ValY);
          if FGridStyle.YMajorVisible then
            ACanvas.DrawLine(AMapper.PixelRect.Left, LY,
                             AMapper.PixelRect.Right, LY, LYMajorPaint);
          if FAxisStyle.FYMajorTicksVisible then
            begin
            case FAxisStyle.FYTickDrawing of
              tmIn:  ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
              tmOut: ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
              tmBoth: begin
                      ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                      ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint)
                      end;
              end;
            end;
          LabelText := FormatAxisLabel(ValY, ValY);
          LFont.Size := YAxisFontSize;
          TextWidth := LFont.MeasureText(LabelText);
          ACanvas.DrawSimpleText(LabelText,
                                 AMapper.PixelRect.Left - FAxisStyle.YMajorTickLength - 4 - TextWidth,
                                 LY + 4, LFont, LTextPaint);
        end;
        ValY := ValY * DecadeFactor;
      end;
    end
  else
  begin
    // Linear Y — minor lines first
    StepY := CalculateNiceStep(AMapper.DataRect.Height, FGridStyle.YMajorDivisions);

    if FGridStyle.YMinorDivisions > 1 then
    begin
      MinorStepY := StepY / FGridStyle.YMinorDivisions;
      MinorValY  := Ceil(AMapper.DataRect.Top / MinorStepY) * MinorStepY;
      while MinorValY <= AMapper.DataRect.Bottom do
      begin
        if Abs(MinorValY / StepY - Round(MinorValY / StepY)) > 1e-9 then
        begin
          LY := AMapper.MapY(MinorValY);
          if FGridStyle.YMinorVisible then
             ACanvas.DrawLine(AMapper.PixelRect.Left, LY,
                           AMapper.PixelRect.Right, LY, LYMinorPaint);
          if FAxisStyle.FYMinorTicksVisible then
             begin
             case FAxisStyle.FYTickDrawing of
              tmIn: ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
              tmOut: ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
              tmBoth: begin
                      ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                      ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMinorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                      end;
             end;
             end;
        end;
        MinorValY := MinorValY + MinorStepY;
      end;
    end;

    ValY := Ceil(AMapper.DataRect.Top / StepY) * StepY;
    while ValY <= AMapper.DataRect.Bottom do
    begin
      LY := AMapper.MapY(ValY);
      if FGridStyle.YMajorVisible then
        ACanvas.DrawLine(AMapper.PixelRect.Left, LY,
                         AMapper.PixelRect.Right, LY, LYMajorPaint);
      if FAxisStyle.FYMajorTicksVisible then
         begin
         case FAxisStyle.FYTickDrawing of
              tmIn: ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
              tmOut:ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
              tmBoth: begin
                      ACanvas.DrawLine(AMapper.PixelRect.Left - FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                      ACanvas.DrawLine(AMapper.PixelRect.Left + FAxisStyle.YMajorTickLength, LY, AMapper.PixelRect.Left, LY, LTickPaint);
                      end;
         end;
         end;
      LabelText := FormatAxisLabel(ValY, StepY);
      LFont.Size := YAxisFontSize;
      TextWidth := LFont.MeasureText(LabelText);
      ACanvas.DrawSimpleText(LabelText,
                             AMapper.PixelRect.Left - FAxisStyle.YMajorTickLength - 4 - TextWidth,
                             LY + 4, LFont, LTextPaint);
      ValY := ValY + StepY;
    end;
  end;

  // -----------------------------------------------------------------------
  //  AXES AND BORDER
  // -----------------------------------------------------------------------

  // Left edge = Y axis — always visible
  ACanvas.DrawLine(AMapper.PixelRect.Left, AMapper.PixelRect.Top,
                   AMapper.PixelRect.Left, AMapper.PixelRect.Bottom, LTickPaint);

  // Bottom edge = X axis — always visible
  ACanvas.DrawLine(AMapper.PixelRect.Left,  AMapper.PixelRect.Bottom,
                   AMapper.PixelRect.Right, AMapper.PixelRect.Bottom, LTickPaint);

  // Top and right edges — optional border
  if ABorderVisible then
  begin
    ACanvas.DrawLine(AMapper.PixelRect.Left,  AMapper.PixelRect.Top,
                     AMapper.PixelRect.Right, AMapper.PixelRect.Top, LTickPaint);
    ACanvas.DrawLine(AMapper.PixelRect.Right, AMapper.PixelRect.Top,
                     AMapper.PixelRect.Right, AMapper.PixelRect.Bottom, LTickPaint);
  end;

  // -----------------------------------------------------------------------
  //  TITLES
  // -----------------------------------------------------------------------

  if FChartTitle.Visible then
  begin
    LTextPaint.Color := FChartTitle.Color;
    LTitleFont.Size  := FChartTitle.FontSize;
    TitleWidth := LTitleFont.MeasureText(FChartTitle.Text);
    ACanvas.DrawSimpleText(FChartTitle.Text,
                           AMapper.PixelRect.CenterPoint.X - (TitleWidth / 2),
                           AMapper.PixelRect.Top - 40,
                           LTitleFont, LTextPaint);
  end;

  if FXAxisTitle.Visible then
  begin
    LTextPaint.Color := FXAxisTitle.Color;
    LFont.Size       := FXAxisTitle.FontSize;
    TitleWidth := LFont.MeasureText(FXAxisTitle.Text);
    ACanvas.DrawSimpleText(FXAxisTitle.Text,
                           AMapper.PixelRect.CenterPoint.X - (TitleWidth / 2),
                           AMapper.PixelRect.Bottom + 50,
                           LFont, LTextPaint);
  end;

  if FYAxisTitle.Visible then
  begin
    LTextPaint.Color := FYAxisTitle.Color;
    LFont.Size       := FYAxisTitle.FontSize;
    TitleWidth := LFont.MeasureText(FYAxisTitle.Text);
    ACanvas.Save;
    try
      ACanvas.Translate(AMapper.PixelRect.Left - 45,
                        AMapper.PixelRect.CenterPoint.Y + (TitleWidth / 2));
      ACanvas.Rotate(270);
      ACanvas.DrawSimpleText(FYAxisTitle.Text, 0, 0, LFont, LTextPaint);
    finally
      ACanvas.Restore;
    end;
  end;
end;

// -----------------------------------------------------------------------
//  Data bounds
// -----------------------------------------------------------------------

function TSkPlotPaintBox.CalculateDataBounds: TRectF;
var
  Series: TPlotSeries;
  Point:  TPointF;
  MinX, MaxX, MinY, MaxY: Single;
  FirstPoint: Boolean;
begin
  // Default fallback range (also used when log mode hides every point)
  Result := TRectF.Create(0, 0, 1, 1);
  if (FSeriesList = nil) or (FSeriesList.Count = 0) then Exit;

  FirstPoint := True;
  for Series in FSeriesList do
    for Point in Series.Data do
    begin
      // In log mode, points with non-positive coordinates have no
      // representation on the axis and are silently excluded from the
      // bounds calculation. Drawing code applies the same rule so the
      // hidden points never produce visual artifacts.
      if AxisStyle.LogX and (Point.X <= 0) then Continue;
      if AxisStyle.LogY and (Point.Y <= 0) then Continue;

      if FirstPoint then
      begin
        MinX := Point.X; MaxX := Point.X;
        MinY := Point.Y; MaxY := Point.Y;
        FirstPoint := False;
      end
      else
      begin
        if Point.X < MinX then MinX := Point.X;
        if Point.X > MaxX then MaxX := Point.X;
        if Point.Y < MinY then MinY := Point.Y;
        if Point.Y > MaxY then MaxY := Point.Y;
      end;
    end;

  if not FirstPoint then
    Result := TRectF.Create(MinX, MinY, MaxX, MaxY);

  // Guard against degenerate ranges (single point, or all points filtered
  // out by log-mode exclusion). Use a one-decade window centered on the
  // value so the axis stays readable.
  if AxisStyle.LogX and (Result.Right <= Result.Left) then
  begin
    if Result.Left <= 0 then
    begin
      Result.Left  := 1.0;
      Result.Right := 10.0;
    end
    else
      Result.Right := Result.Left * 10;
  end;

  if AxisStyle.LogY and (Result.Bottom <= Result.Top) then
  begin
    if Result.Top <= 0 then
    begin
      Result.Top    := 1.0;
      Result.Bottom := 10.0;
    end
    else
      Result.Bottom := Result.Top * 10;
  end;
end;

function TSkPlotPaintBox.GetEffectiveDataBounds: TRectF;
begin
  Result := CalculateDataBounds;

  if not FAutoY then
  begin
    Result.Top    := FSubAxisProperty.MinY;
    Result.Bottom := FSubAxisProperty.MaxY;
  end;

  if not FAutoX then
  begin
    Result.Left  := FSubAxisProperty.MinX;
    Result.Right := FSubAxisProperty.MaxX;
  end;
end;

// -----------------------------------------------------------------------
//  RenderChart
// -----------------------------------------------------------------------

procedure TSkPlotPaintBox.RenderChart(const ACanvas: ISkCanvas;
                                      const ADest: TRectF);
var
  Mapper:    TPlotMapper;
  LPaint:    ISkPaint;
  LMarkerFill, LMarkerStroke: ISkPaint;
  Series:    TPlotSeries;
  RawBounds: TRectF;
  PadX, PadY: Single;
  LogPad: Double;
begin
  // Fill entire paintbox background
  LPaint := TSkPaint.Create;
  LPaint.Color := FBackgroundColor;
  LPaint.Style := TSkPaintStyle.Fill;
  ACanvas.DrawRect(ADest, LPaint);

  ACanvas.Save;
  try
    RawBounds := GetEffectiveDataBounds;
    PadX := RawBounds.Width  * 0.05;
    PadY := RawBounds.Height * 0.05;
    Mapper.DataRect := RawBounds;

    if FOriginOnAxis then
    begin
      if RawBounds.Left >= 0 then
        Mapper.DataRect.Left := Max(0.0, RawBounds.Left - PadX)
      else
        Mapper.DataRect.Left := RawBounds.Left - PadX;

      Mapper.DataRect.Right := RawBounds.Right + PadX;

      if RawBounds.Top >= 0 then
        Mapper.DataRect.Top := Max(0.0, RawBounds.Top - PadY)
      else
        Mapper.DataRect.Top := RawBounds.Top - PadY;

      Mapper.DataRect.Bottom := RawBounds.Bottom + PadY;
    end
    else
      Mapper.DataRect.Inflate(PadX, PadY);

    // Log-space padding overrides the linear padding above. Linear padding
    // on a log axis can drive the lower bound below zero, after which the
    // mapper clamps it to 1e-9 and the axis appears to extend to absurdly
    // small values. A multiplicative pad keeps the visual breathing room
    // proportional in log space.
    if AxisStyle.LogX and (RawBounds.Left > 0) and (RawBounds.Right > RawBounds.Left) then
    begin
      LogPad := 0.05 * (Log10(RawBounds.Right) - Log10(RawBounds.Left));
      Mapper.DataRect.Left  := Power(10, Log10(RawBounds.Left)  - LogPad);
      Mapper.DataRect.Right := Power(10, Log10(RawBounds.Right) + LogPad);
    end;

    if AxisStyle.LogY and (RawBounds.Top > 0) and (RawBounds.Bottom > RawBounds.Top) then
    begin
      LogPad := 0.05 * (Log10(RawBounds.Bottom) - Log10(RawBounds.Top));
      Mapper.DataRect.Top    := Power(10, Log10(RawBounds.Top)    - LogPad);
      Mapper.DataRect.Bottom := Power(10, Log10(RawBounds.Bottom) + LogPad);
    end;

    Mapper.PixelRect := ADest;
    Mapper.PixelRect.Inflate(-80, -70);
    Mapper.LogX := AxisStyle.LogX;
    Mapper.LogY := AxisStyle.LogY;

    // Fill plot area background
    LPaint.Style := TSkPaintStyle.Fill;
    LPaint.Color := FPlotAreaColor;
    ACanvas.DrawRect(Mapper.PixelRect, LPaint);

    FLastMapper := Mapper;
    FHasMapper  := True;

    // Grid, axes, ticks, labels, titles
    DrawGrid(ACanvas, Mapper, FPlotBorderColor, FPlotBorderWidth, FPlotBorderVisible);

    // Series — clipped to plot area
    ACanvas.Save;
    try
      ACanvas.ClipRect(Mapper.PixelRect);
      for Series in FSeriesList do
        Series.Draw(ACanvas, Mapper);
    finally
      ACanvas.Restore;
    end;

    // Marker paints (kept for series compatibility)
    LMarkerFill := TSkPaint.Create(TSkPaintStyle.Fill);
    LMarkerFill.Color     := TAlphaColors.White;
    LMarkerFill.AntiAlias := True;

    LMarkerStroke := TSkPaint.Create(TSkPaintStyle.Stroke);
    LMarkerStroke.Color       := TAlphaColors.Dodgerblue;
    LMarkerStroke.StrokeWidth := 2;
    LMarkerStroke.AntiAlias   := True;

    DrawLegend(ACanvas, Mapper);

  finally
    ACanvas.Restore;
  end;
end;

// -----------------------------------------------------------------------
//  Mouse handling
// -----------------------------------------------------------------------

procedure TSkPlotPaintBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
     Item1, Item2: TMenuItem;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = TMouseButton.mbLeft) and
     FLegendStyle.Visible and
     FLegendRect.Contains(TPointF.Create(X, Y)) then
  begin
    FDraggingLegend    := True;
    FLegendDragStart   := TPointF.Create(X, Y);
    FLegendOffsetStart := FLegendOffset;
    // Capture the mouse so we keep receiving events even if the cursor
    // moves quickly outside the legend bounds
    Capture;
  end;
  if Button = TMouseButton.mbRight then
     begin
     if FPaintBoxMenu = nil then
        begin
        FPaintBoxMenu := TPopupMenu.Create(Self);
        FPaintBoxMenu.Parent := Self;
        Item1 := TMenuItem.Create(FPaintBoxMenu);
        Item1.Text := 'Copy Image to Clipboard';
        Item1.OnClick := DoCopyImageToClipboard;
        FPaintBoxMenu.AddObject(Item1);
        end;
     FPaintBoxMenu.Popup(Screen.MousePos.X, Screen.MousePos.Y);
     end;
end;

procedure TSkPlotPaintBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Single);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FDraggingLegend then
  begin
    FDraggingLegend := False;
    ReleaseCapture;
  end;
end;

procedure TSkPlotPaintBox.MouseMove(Shift: TShiftState; X, Y: Single);
var
  WorldX, WorldY: Single;
begin
  inherited MouseMove(Shift, X, Y);

  // Update cursor to signal that the legend is draggable
  if FLegendStyle.Visible and FLegendRect.Contains(TPointF.Create(X, Y)) then
    Cursor := crHandPoint
  else if not FDraggingLegend then
    Cursor := crDefault;

  // Handle active drag
  if FDraggingLegend then
  begin
    FLegendOffset.X := FLegendOffsetStart.X + (X - FLegendDragStart.X);
    FLegendOffset.Y := FLegendOffsetStart.Y + (Y - FLegendDragStart.Y);
    Redraw;
    Exit;  // suppress coordinate reporting while dragging
  end;

  if Assigned(FOnReportCoordinates) and FHasMapper then
  begin
    WorldX := FLastMapper.UnmapX(X);
    WorldY := FLastMapper.UnmapY(Y);
    FOnReportCoordinates(X, Y, WorldX, WorldY);
  end;
end;

end.
