unit uCommon;

interface

Uses Windows, FMX.Graphics, System.UIConsts, System.UITypes, System.Types;

const
  GRID_DASH_SIZE = 0.1; { in mm }
  DEFAULT_SYMBOL_RADIUS = 0.12;
  { Units are cms and equals the radius of the symbol }
  DEFAULT_SYMBOL_THICKNESS = 0.03;
  { Units are cms and equals the thickness of the symbol outline }
  DEFAULT_LINE_THICKNESS = 1.5; // 0.03514598035*2.25;
  DEFAULT_ERRORBAR_CAP_WIDTHInCms = 0.075;
  DEFAULT_ERRORBAR_THICKNESSInCms = 0.02;
  DEFAULT_ERRORBAR_COLOR = claBlack;
  DEFAULT_SYMBOL_OUTLINE_COLOR = claBlack; // TColor($FFFFCC);  // Light blue
  DEFAULT_SYMBOL_FILL_COLOR = claBlue; // TColor ($000080);
  DEFAULT_SYMBOL_GRADIENT_FILL_START_COLOR = claAntiqueWhite;
  DEFAULT_LINE_COLOR = TColor($000080); // TColor ($0066FF);  // Orange
  DEFAULT_FRAME_THICKNESS = 0.03;
  DEFAULT_FRAME_GAP = 0.12;
  DEFAULT_MinDistanceBetweenLegendItemsCms = 0.2;
  DEFAULT_MAGNIFICATION = 1.0;//1.728;

var
  CmsInOneInch    : double = 2.54;
  CurrentXPixelsPerInch : integer;  { Updated according to current canvas }
  CurrentYPixelsPerInch : integer;  { Updated according to current canvas }

type
   THowtoScale = (hLowerLimit, hUpperLimit, hBothLimits);

   T1DStringArray = array of string;
   T1DArray = array of double;
   T2DArray = array of T1DArray;

   TBox = record left, top, w, h : single; end;
   TRBox = record left, bottom, w, h : double; end;
   TTickStyle = (tsIn, tsOut, tsInOut);

   TSubGraphSelectedObject = (coNone, coGraphingArea, coMainTitle, coXAxisTitle, coYAxisTitle, coXAxis, coYAxis, coLegend);

   TBounds = record left, top, width, height : single; end;

   TFontCaps = packed record
                 fontName : shortstring;
                 size : single;
                 style : TFontStyles;
                 color : TAlphaColor;
                 value : AnsiString;
                end;

   TTextDetails = packed record
                 fontName : AnsiString;
                 size : single;
                 style : TFontStyles;
                 color : TAlphaColor;
                 value : AnsiString;
                 visible : boolean;
                 rx, ry : double;
                end;

    TFrameType = record
          Framed : boolean;    // Set true if framed
          Color : TAlphaColor;
          lineThicknessInCms : double;
          gapWidthInCms : double;
    end;

   TWorldPoint = record
         x : double;
         y : double;
   end;

  pCanvasAttributes = ^TCanvasAttributes;
  TCanvasAttributes = record
       origin : TPointF;
       scalingFactor : double;
       printerScalingFactor : double;
       canvas : TCanvas;
  end;


procedure getCurrentPixelsPerInch(canvas: TCanvas; Printing: boolean);
function  getWd (canvas : TCanvas; str: string): single;
function  getHt (canvas: TCanvas; str: string): single;

function createVector (size : integer) : T1DArray;
function createMatrix (nRows, nCols : integer) : T2DArray;

procedure freeMatrix (m : T2DArray);

function RoundUp (d : double) : double;
function RoundDown (d : double) : double;
function RoundDownUnits (d : double) : double;

function getTextBoundingBox (canvas : TCanvas; text : TTextDetails) : TBox;
function findMaxValue (data : T1DArray) : double;

implementation

Uses Classes, SysUtils, Math;

function createVector (size : integer) : T1DArray;
begin
  setLength (result, size);
end;


function createMatrix (nRows, nCols : integer) : T2DArray;
begin
  setLength (result, nRows, nCols);
end;


procedure freeMatrix (m : T2DArray);
begin
  setLength (m, 0);
end;

function findMaxValue (data : T1DArray) : double;
var i : integer;
begin
end;

{ Removes the units digit from a positive number, always rounds down, thus
  163 -> 160, 6.2 -> 6, 16789.3 -> 16780. Also accepts negative numbers, so
  that -163 -> -160 }
function RoundDownUnits (d : double) : double;
var y : double;
begin
  result := trunc (d/10)*10;
end;


function power (x : Extended; n : Extended) : Extended;
var int_of_n : Extended; intn : integer;
begin
  if n = 0.0 then begin result := 1.0; exit; end; { save going through exp etc }
  if x > 0 then
     result := exp (n*ln(x))
  else if x = 0 then
       result := 0.0
  else if x < 0 then
     begin
     { check if n is an integer }
     int_of_n := int (n);
     intn := round (int_of_n);
     if int_of_n = n then    { if it's an integer }
        begin
        result := exp (n*ln(abs(x)));
        { now work out the sign }
        if (odd (intn)) and (int_of_n <> 0) then result := result * (-1);
        end
     else
        begin
          raise Exception.Create ('Error calculating root of negative number in power');
        end;
     end;
end;


{ Rounds up a real positive number, eg 1.68 becomes 2, 54.6 becomes 60,
123.4 becomes 200, 5780 becomes 6000 }
function RoundUp (d : double) : double;
var y, tmp : double;
begin
  if d > 1 then
     begin
     y := power (10, trunc (Log10(d)));
     if y <> d then
        result := y * (trunc (d/y) + 1)
     else
        result := d;
     exit;
     end;
  if d > 0 then
     begin
     y := power (10, (1 + trunc (abs (Log10(d)))));
     tmp := y * d;
     tmp := RoundUp (tmp);
     result := tmp / y;
     end;
end;



{ Rounds down real positive numbers, eg 1.68 to 1, 54.6 to 50, 123.4 to 100,
5780 to 5000 }
function RoundDown (d : double) : double;
var y, tmp : double;
begin
  if d > 1 then
     begin
     y := power (10, trunc (Log10(d)));
     result := y * (trunc (d / y));
     exit;
     end;
  if d > 0 then
     begin
     y := power (10, (1 + trunc (abs (Log10(d)))));
     tmp := y * d;
     tmp := RoundDown (tmp);
     result := tmp / y;
     end;
end;


//function convertFontStyleToGDIPlusStyle (fontStyle : TFontStyles) : integer;
//begin
//  if fsBold in fontStyle then
//     result := FontStyleBold
//  else
//  if fsItalic in fontStyle then
//     result := FontStyleItalic
//  else
//  if fsUnderline in fontStyle then
//     result := FontStyleUnderline
//  else
//  if fsStrikeOut in fontStyle then
//     result := FontStyleStrikeout
//  else
//     result := FontStyleRegular;
//end;


function getTextBoundingBox (canvas : TCanvas; text : TTextDetails) : TBox;
//var mRect, layout : TRectF;
//    font : TGPFont;
//    str : string;
//    FontFamily : TGPFontFamily;
//    s : TStatus;
begin
//  try
//    result.left := 0; result.top := 0;
//
//    FontFamily := TGPFontFamily.Create(text.fontName);
//    Font       := TGPFont.Create(FontFamily, text.size, convertFontStyleToGDIPlusStyle (text.style), UnitPixel);
//
//    str := string (text.value);
//
//    layout.X := 0; layout.Y := 0;
//    layout.Width := 0; layout.Height := 0;
//    s := graphics.MeasureString(str, length (str), font, layout, mRect);
//
//    //mRect := graphics.MeasureString(str, length (str), font, layout, sft);  // mRect : TGPRectF
//    result.w := round (mRect.Width);
//    result.h := round (mRect.Height);
//  finally
//    font.free;
//  end;
end;


procedure getCurrentPixelsPerInch(canvas: TCanvas; Printing: boolean);
//var
  //LogPixInchW, LogPixInchH: integer;
begin
  if Printing then
  begin
//    if PrinterInstalled then
//    begin
//      CurrentXPixelsPerInch := GetDeviceCaps(Printer.handle, LOGPIXELSX);
//      CurrentYPixelsPerInch := GetDeviceCaps(Printer.handle, LOGPIXELSY);
//    end
//    else
//    begin
//      CurrentXPixelsPerInch := FAKE_PRINTER_DPI;
//      CurrentYPixelsPerInch := FAKE_PRINTER_DPI;
//    end;
  end
  else
  begin
    CurrentXPixelsPerInch := GetDeviceCaps(GetDC(0), LOGPIXELSX);
    CurrentYPixelsPerInch := GetDeviceCaps(GetDC(0), LOGPIXELSY);
  end;
end;


{ Use this routine to get the text width, can be using in all cases }
function GetWd(canvas: TCanvas; str: string): single;
begin
 result := canvas.TextWidth(str);
end;

{ Use this routine to get the text height, can be using in all cases }
function GetHt(canvas: TCanvas; str: string): single;
begin
  result := canvas.TextHeight(str);
end;



end.
