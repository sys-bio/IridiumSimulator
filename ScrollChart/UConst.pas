unit UConst;

interface
  uses
    System.UITypes, System.Types, System.UIConsts, UDataSource, System.Contnrs,
    Skia.FMX, Skia, UGlobalData;

  const
    X_AXIS_SIZE = 60;
    Y_AXIS_SIZE = 60;

    DEFAULT_COLOR: TAlphaColor = claBrown;
    DEFAULT_WIDTH: Single = 2;

    DEFAULT_X_MIN = 0;
    DEFAULT_X_MAX = 10;

    DEFAULT_Y_MIN = -2;
    DEFAULT_Y_MAX = 2;

    MARGIN_X = 80;
    MARGIN_y = 40;

    MAX_TICKS_Y = 10;
    MAX_TICKS_X = 10;

    DEFAULT_BACKGROUND_COLOR = claBlack;
    PLOT_AREA_BACKGROUND_COLOR = $FF100c08;
    FONT_COLOR = claWhite;
    FONT_NAME = 'ARIAL';
    FONT_SIZE = 16;
    LINE_COLOR = claWhite;

    LENGTH_BOX_TICK_X = 24;
    tickPercentAxisX = 0.5;
    MARGIN_FONT = 6;

    LENGTH_BOX_TICK_Y = 24;
    tickPercentAxisY = 0.5;

  var
    MyCanvas: ISkCanvas;
    dataSource: TDataSource;
    series: TObjectList;
    plane: TPlaneXY;


implementation

end.
