unit uGObject;

interface

Uses Classes, SysUtils, FMX.Graphics, uRRCommon, FMX.Types,
     System.UIConsts, System.UITypes, System.Types, skia;

type
  TDirection = (NE, NW, SE, SW);

  TTextType = class (TObject)
     value : string;
     fontName  : string;
     fontSize  : double;
     fontColor : TAlphaColor;
     visible   : boolean;
     box : TBox;   // Bounding box, computed at draw time

     typeface : ISkTypeface;
     font     : ISkFont;
     procedure setFontSize (size : double);
     function computeDimensions (LPaint : ISkPaint) : TPointF;
     constructor Create (fontName : string);
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
  textProperties := TTextType.Create('Arial');
  textProperties.typeface := TSkTypeface.MakeFromName('Arial', TSkFontStyle.Normal);
  textProperties.font := TSkFont.Create(textProperties.typeface, 12, 1);
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


constructor TTextType.Create (fontName : string);
begin
  self.fontname := fontName;
  typeface := TSkTypeface.MakeFromName(fontName, TSkFontStyle.Normal);
  setFontSize (12);
end;

// -------------------------------------------------------------

procedure TTextType.setFontSize (size : double);
begin
  fontSize := size;
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
  frameVisible := False;
  frameBorderThickness := 1;

  logicalBox.left := 0.65;
  logicalBox.top  := 0.05;
  visible := true;
  outlineColor := claBlack;
  interiorColor := claGhostwhite;//  claBeige;
  visible := true;
  lineThicknessInCms := 0.02;

  textProperties := TTextType.Create('Arial');
  textProperties.typeface := TSkTypeface.MakeFromName('Arial', TSkFontStyle.Normal);
  textProperties.font := TSkFont.Create(textProperties.typeface, 12, 1);

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

  textProperties.fontName := 'Arial';
  //gObj.textProperties.style := [TFontStyle.fsBold];
  textProperties.fontcolor := claBlack;
  textProperties.value := 'Main Title';
  textProperties.setFontSize(15);
  visible := true;
end;


constructor TXAxisTitle.Create;
begin
  inherited Create (coXAxisTitle);

  logicalBox.left := 0.45;
  logicalBox.top  := 1.125;

  textProperties.fontName := 'Arial';
  //graphObjects[xaxisTitleId].textProperties.style := [TFontStyle.fsBold];
  textProperties.fontcolor := claBlack;
  textProperties.value := 'X Axis';
  textProperties.setFontSize(14);
  visible := true;
end;


constructor TYAxisTitle.Create;
begin
  inherited Create (coYAxisTitle);

  logicalBox.left := -0.1;
  logicalBox.top  := 0.5;

  //graphObjects[yaxisTitleId].textProperties.style := [TFontStyle.fsBold];
  textProperties.fontcolor := claBlack;
  textProperties.value := 'Y Axis';
  textProperties.setFontSize(14);
  visible := true;
end;


end.
