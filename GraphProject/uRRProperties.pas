unit uRRProperties;

interface

Uses SysUtils,
     Classes,
     Generics.Collections,
     System.UIConsts,
     System.UITypes,
     uRRDataSeries,
     uGObject;

type
  TSubGraphProperties = class(TPersistent)
  private
    FXGridLines: boolean;
    FYGridLines: boolean;
  public
    graphObjects: TObjectList<TGraphObject>;
    dataBlocks: TDataBlocks;

    Legend: TLegend;

    GraphBorder: boolean;
    GraphBackgroundColor: TAlphaColor;
    GraphBorderThicknessInSkiaUnits: double;
    GraphBorderColor: TAlphaColor;

    XMajorGridColor: TAlphaColor;
    YMajorGridColor: TAlphaColor;

    XMinorGridColor: TAlphaColor;
    YMinorGridColor: TAlphaColor;

    XMinorGridLines: boolean;
    YMinorGridLines: boolean;

    NumXMinorTicks, NumYMinorTicks: integer;
    NumberOfXTicks, NumberOfYTicks: integer;

    XLabelPrecision, YLabelPrecision: integer;
    // XMajorTickColor: TAlphaColor;
    // YMajorTickColor: TAlphaColor;

    // XMinorTickColor: TAlphaColor;
    // YMinorTickColor: TAlphaColor;

    XAxisColor: TAlphaColor; // Does not include the major grid colors
    YAxisColor: TAlphaColor;

    bDrawMainTitle: boolean;
    bDrawXAxisTitle: boolean;
    bDrawYAxisTitle: boolean;

    MainTitleObject: TMainTitle;
    XAxisTitleObject: TXAxisTitle;
    YAxisTitleObject: TGraphObject;

    LogXAxis, LogYAxis: boolean;

    FAutoXScaling, FAutoYScaling: boolean;

    XMajorGridThicknessInSkiaUnits: double;
    YMajorGridThicknessInSkiaUnits: double;

    XMinorGridThicknessInSkiaUnits: double;
    YMinorGridThicknessInSkiaUnits: double;

    XMajorTickWidthInSkiaUnits: double;
    YMajorTickWidthInSkiaUnits: double;

    FWorldXmax, FWorldXmin, FWorldYmax, FWorldYmin: double;

    // These are the values the user specifies, can be overridden by autoscaling
    UserScale_Xmin, UserScale_Xmax, UserScale_Ymin, UserScale_Ymax: double;

    constructor Create;
    destructor Destroy; override;
  published
    property XGridLines: boolean read FXGridLines write FXGridLines;
    property YGridLines: boolean read FYGridLines write FYGridLines;
    property AutoXScaling: boolean read FAutoXScaling write FAutoXScaling;
    property AutoYScaling: boolean read FAutoYScaling write FAutoYScaling;
  end;

implementation


constructor TSubGraphProperties.Create;
begin
  XLabelPrecision := 3; YLabelPrecision := 3;

  UserScale_Xmin := 0.0;
  UserScale_Xmax := 10.0;
  UserScale_Ymin := 0.0;
  UserScale_Ymax := 10.0;

  // These are properties which set the FWorldXMin etc values
  FWorldXmin := UserScale_Xmin;
  FWorldXmax := UserScale_Xmax;
  FWorldYmin := UserScale_Ymin;
  FWorldYmax := UserScale_Ymax;

  XMajorGridThicknessInSkiaUnits := 1;
  YMajorGridThicknessInSkiaUnits := 1;
  YMajorGridThicknessInSkiaUnits := 1;

  XMinorGridThicknessInSkiaUnits := 1;
  YMinorGridThicknessInSkiaUnits := 1;

  XMajorTickWidthInSkiaUnits := 1;
  YMajorTickWidthInSkiaUnits := 1;

  GraphBorderColor := claBlack;
  GraphBorderThicknessInSkiaUnits := 1;
  GraphBackgroundColor := claGhostwhite;

  XAxisColor := claBlack; // Does not include the major grid colors
  YAxisColor := claBlack;

  XMajorGridColor := claBlack;
  YMajorGridColor := claBlack;

  // Color of the minor grid lines
  XMinorGridColor := claLightGray; // $FFDAC4C4;
  YMinorGridColor := claLightGray; // $FFDAC4C4;

  LogXAxis := false;
  LogYAxis := false;

  bDrawMainTitle := True;
  bDrawXAxisTitle := True;
  bDrawYAxisTitle := True;

  NumXMinorTicks := 5;
  NumYMinorTicks := 5;

  NumberOfXTicks := 5;
  NumberOfYTicks := 5;

  LogXAxis := false;
  LogYAxis := false;
  AutoXScaling := false;

  GraphBorder := True;
  XGridLines := false;
  YGridLines := false;
  XMinorGridLines := false;
  YMinorGridLines := false;
end;


destructor TSubGraphProperties.Destroy;
begin
  dataBlocks.Free;
  inherited;
end;

end.
