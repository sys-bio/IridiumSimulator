unit uGObject;

interface

Uses Classes, SysUtils, FMX.Graphics, uRRCommon, FMX.Types,
     System.UIConsts, System.UITypes, System.Types, skia;

type
  TDirection = (NE, NW, SE, SW);

  TTextType = class (TObject)
     value : string;
     fontColor : TAlphaColor;  // Color is separate from ISKFont, it is set by the paint object
     visible   : boolean;
     box : TBox;   // Bounding box, computed at draw time

     typeface : ISkTypeface;
     font     : ISkFont;
     procedure setFontSize (size : double);
     function computeDimensions (LPaint : ISkPaint) : TPointF;
     constructor Create (fontName : string; fontSize : double);
  end;


  TGraphBase = class (TObject)
       ObjType    : TSubGraphSelectedObjectType;
       logicalBox : TLogicalBox;  // Stores logical coordinates
       selected   : boolean;      // Set true if surrounded by selected box }
       dBox       : TBox;         // Temp store for device coordinates
       reSizable  : boolean;      // Rectangle can be resized by mouse if true }
       textProperties : TTextType;//TTextDetails;
       visible : boolean;

       function IsResizable : boolean;
       constructor Create (objType : TSubGraphSelectedObjectType);
   end;


   TGraphObject = class (TGraphBase)
         movable    : boolean;   // The rectangle can be moved by mouse if true
         active     : boolean;   // Set true if the object is being used

         constructor Create (objType : TSubGraphSelectedObjectType);
   end;

   TMainTitle = class (TGraphObject)
        constructor  Create;
   end;

   TXAxisTitle = class (TGraphObject)
        constructor Create;
   end;


   TYAxisTitle = class (TGraphObject)
        constructor Create;
   end;

   TXAxisObject = class (TGraphObject)
        constructor Create;
   end;

   TYAxisObject = class (TGraphObject)
        constructor Create;
   end;

   TLegend = class (TGraphObject)
         frameVisible : boolean;
         frameGapInCms : single;
         lineLengthInCms : single;
         frameBorderThickness : single;
         outlineColor : TAlphaColor;
         interiorColor : TAlphaColor;
         lineThicknessInCms : double;
         gapWidthInCms : double;
         textProperties : TTextType;
         constructor Create (objType : TSubGraphSelectedObjectType);
   end;

   function PtOverHandle (aBox : TBox; x, y : single; var direction : TDirection) : boolean;

implementation

Uses uSubgraph;

constructor TGraphBase.Create (objType : TSubGraphSelectedObjectType);
begin
  self.ObjType := objType;
end;


function TGraphBase.IsResizable : boolean;
begin
  result := reSizable;
end;

// ------------------------------------------------------------------


constructor TGraphObject.Create (objType: TSubGraphSelectedObjectType);
begin
  inherited Create (objType);
end;


// Returns true if mouse is over one of the handles of rectangle aBox
function PtOverHandle (aBox : TBox; x, y : single; var direction : TDirection) : boolean;
var i, left, right, top, bottom : single; p : TPointF;
begin
  p.x := x; p.y := y;
  left := aBox.left; right  := aBox.left + aBox.w;
  top  := aBox.top;  bottom := aBox.top + aBox.h;

  // Check each handle in turn
  if PtInRect (rectf (left, top, left+8, top+8), p) then {NW}
     begin
     result := true; direction := NW;
     exit;
     end;
  if PtInRect (rectf (right-8, top, right, top+8), p) then {NE}
     begin
     result := true; direction := NE;
     exit;
     end;
  if PtInRect (rectf (right-8, bottom-8, right, bottom), p) then {SE}
     begin
     result := true; direction := SE;
     exit;
     end;
  if PtInRect (rectf (left, bottom-8, left+8, bottom), p) then {SW}
     begin
     result := true; direction := SW;
     exit;
     end;
  result := false;
end;


constructor TTextType.Create (fontName : string; fontSize : double);
begin
  typeface := TSkTypeface.MakeFromName(fontName, TSkFontStyle.Normal);
  setFontSize (fontSize);
end;

// -------------------------------------------------------------

procedure TTextType.setFontSize (size : double);
begin
  font := TSkFont.Create(typeface, size, 1);
end;


function TTextType.computeDimensions (LPaint : ISkPaint) : TPointF;
var ABounds : TRectF;
begin
 font.MeasureText(value, ABounds, LPaint);
 box.w := ABounds.Width;
 box.h := ABounds.Height;

 result.X := ABounds.Width;
 result.Y := ABounds.Height;
end;


// ----------------------------------------------------------

constructor TLegend.Create (objType : TSubGraphSelectedObjectType);
begin
  self.ObjType := objType;
  frameVisible := DEFAULT_FRAME_VISIBLE;
  frameBorderThickness := DEFAULT_FRAME_THICKNESS;

  logicalBox.left := 0.65;
  logicalBox.top  := 0.05;
  visible := true;
  outlineColor := DEFAULT_LEGEND_OUTLINE_COLOR;
  interiorColor := claGhostwhite;//  claBeige;
  visible := true;
  lineThicknessInCms := 0.02;

  textProperties := TTextType.Create('Arial', DEFAULT_LEGEND_FONT_SIZE);
  textProperties.fontColor := DEFAULT_LEGEND_TEXT_COLOR;

  frameGapInCms := 0.15;
  lineLengthInCms := 1;
end;


// ----------------------------------------------------------

constructor TMainTitle.Create;
begin
  inherited Create (coMainTitle);

  logicalBox.left := 0.41;
  logicalBox.top  := -0.1;
  logicalBox.w := 0;
  logicalBox.h := 0;

  textProperties := TTextType.Create('Arial', DEFAULT_MAIN_TITLE_FONT_SIZE);
  textProperties.fontcolor := DEFAULT_MAIN_TITLE_COLOR;
  textProperties.value := 'Main Title';
  visible := true;
end;


constructor TXAxisTitle.Create;
begin
  inherited Create (coXAxisTitle);

  logicalBox.left := 0.45;
  logicalBox.top  := 1.225;  // May 2024. increased from 1.125 to make more space

  textProperties := TTextType.Create('Arial', DEFAULT_XAXIS_TITLE_FONT_SIZE);
  textProperties.fontcolor := DEFAULT_XAXIS_COLOR;
  textProperties.value := 'X Axis';
  visible := true;
end;


constructor TYAxisTitle.Create;
begin
  inherited Create (coYAxisTitle);

  logicalBox.left := -0.1;
  logicalBox.top  := 0.5;

  textProperties := TTextType.Create('Arial', DEFAULT_YAXIS_TITLE_FONT_SIZE);
  textProperties.fontcolor := DEFAULT_YAXIS_COLOR;
  textProperties.value := 'Y Axis';
  visible := true;
end;


constructor TXAxisObject.Create;
begin
  inherited Create (coXAxis);

  logicalBox.left := 0.0;
  logicalBox.top  := 1;
  logicalBox.w := 1;
  logicalBox.h := 0.1;

  visible := true;
end;


constructor TYAxisObject.Create;
begin
  inherited Create (coYAxis);

  logicalBox.left := -0.1;
  logicalBox.top  := 0;
  logicalBox.w := 0.1;
  logicalBox.h := 1.0;

  visible := true;
end;

end.
