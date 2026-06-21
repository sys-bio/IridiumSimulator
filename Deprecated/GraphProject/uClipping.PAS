unit uClipping;

{ Line Clipping Routines originally written for EasyGraph VB but later translated
for Fplot H M Sauro 1995 }


interface

uses FMX.Dialogs,
     System.Types,
     SysUtils,
     Classes,
     FMX.Graphics,
     FMX.Controls,
     FMX.Forms,
     FMX.StdCtrls,
     FMX.ExtCtrls,
     FMX.Menus;

function clipLine (rect : TRectF; x1, y1, x2, y2 : single;
               var x1clipped, y1clipped, x2clipped, y2clipped : single) : boolean;


function rclip_line (var x1, y1, x2, y2 : double) : boolean;
procedure SetClippingRegion (iClipxl, iClipxr, iClipyb, iClipyt : double);

implementation

var
  Clipxl, Clipxr, Clipyb, Clipyt : double;



procedure find_area_code (rect : TRectF; x, y : single; var ax, ay : single);
begin
  ax := 0; ay := 0;
  if x < rect.left then ax := -1;
  if x > rect.right then ax := 1;
  if y < rect.top then ay := -1;
  if y > rect.bottom then ay := 1;
end;

{ Procedure to clip line to a specified rectangle. Clip the line
 joining (x1, y1) to (x2, y2). NOTE that all coordinates here
 are DEVICE coorindates. Clipping is to the rectangle
 given by rect and the routine returns the clipped coordinates
 in the xclipped variables. The routine returns false if the line
 is not within the clipping rectangle }

 { returns true if there is a line to draw, otherwise false }
function clipLine (rect : TRectF; x1, y1, x2, y2 : single;
         var x1clipped, y1clipped, x2clipped, y2clipped : single) : boolean;
var
   ax1, ax2, ay1, ay2 : single;
   slope, invslope : double;
   xedge, xprime, yedge, yprime : single;
begin
 try
   find_area_code(rect, x1, y1, ax1, ay1);
   find_area_code(rect, x2, y2, ax2, ay2);
   if not ((ax1 * ax2 = 1) or (ay1 * ay2 = 1)) then
      begin
      if ((x2-x1) <> 0) and ((y2-y1) <> 0) then
         begin
         slope := (y2-y1)/(x2-x1);
         invslope := 1/slope;
         end;

      if ax1 <> 0 then
         begin
         xedge := rect.left;
         if ax1 = 1 then xedge := rect.right;
         xprime := xedge;
         yprime := y1 + round(slope*(xprime - x1));
         x1 := xprime; y1 := yprime;
         find_area_code(rect, x1, y1, ax1, ay1);
         end;

      if ay1 <> 0 then
         begin
         yedge := rect.top;
         if ay1 = 1 then yedge := rect.bottom;
         yprime := yedge;
         { use real types here to avoid funny integer side effects }
         xprime := x1 + round(invslope*(yprime-y1));
         {if xprime < rect.left then xprime := rect.left;}
         x1 := xprime; y1 := yprime;
         end;

      if ax2 <> 0 then
         begin
         xedge := rect.left;
         if ax2 = 1 then xedge := rect.right;
         xprime := xedge;
         yprime := y1 + round(slope*(xprime - x1));
         x2 := xprime; y2 := yprime;
         find_area_code(rect, x2, y2, ax2, ay2);
         end;

      if ay2 <> 0 then
         begin
         yedge := rect.top;
         if ay2 = 1 then yedge := rect.bottom;
         yprime := yedge;
         xprime := x1 + round(invslope*(yprime - y1));
         x2 := xprime; y2 := yprime;
         end;

      x1clipped := x1; y1clipped := y1;
      x2clipped := x2; y2clipped := y2;
      result := true;
      end
   else
      result := false;

{ClipErrorHandler:
   MsgBox "A fatal error has occured while clipping output
   to the window. Please contact author with offending data.", 48, "Fatal Error"
   Call GeneralExitPoint}

 except
   on EIntError do
      showmessage ('General integer error in clip_line');
   on EZeroDivide do
      showmessage ('Floating divide by zero error in clip_line');
   on EOverflow do
      showmessage ('Floating overflow error in clip_line');
   on EUnderflow do
      showmessage ('Floating underflow error in clip_line');

   end;
end;


procedure SetClippingRegion (iClipxl, iClipxr, iClipyb, iClipyt : double);
begin
  Clipxl := iClipxl; Clipxr := iClipxr;
  Clipyb := iClipyb; Clipyt := iClipyt;
end;


{ Returns false if the line is completely outside the clipping rectangle
otherwise returns true with coordinates set to the clipped line }

function rclip_line (var x1, y1, x2, y2 : double) : boolean;
type edge = (left, right, bottom, top);
     Toutcode = set of edge;
var
     c, c1, c2 : Toutcode; x, y : double;


function Code (x, y : double) : Toutcode;
var c : Toutcode;
begin
  c := [];
  if x < Clipxl then c := [left] else if x > Clipxr then c := [right];
  if y < Clipyb then c := c + [bottom]
  else
  if y > Clipyt then
  c := c + [top];
  result := c;
end;


begin
  c1 := Code (x1, y1); c2 := Code (x2, y2);

  while (c1 <> []) or (c2 <> []) do
        begin
        if c1*c2 <> [] then begin result := false; exit; end;
        c := c1; if c = [] then c := c2;
        if left in c then
           begin
           y := y1 + (y2 - y1)*(Clipxl - x1)/(x2 - x1);
           x := Clipxl;
           end
        else
        if right in c then
           begin
           y := y1 + (y2 - y1)*(Clipxr - x1)/(x2 - x1);
           x := Clipxr;
           end
        else
        if bottom in c then
           begin
           x := x1 + (x2 - x1)*(Clipyb - y1)/(y2 - y1);
           y := Clipyb;
           end
        else
        if top in c then
           begin
           x := x1 + (x2 - x1)*(Clipyt - y1)/(y2 - y1);
           y := Clipyt;
           end;

        if c = c1 then
           begin
           x1 := x; y1 := y; c1 := Code (x, y);
           end
        else
           begin
           x2 := x; y2 := y; c2 := Code (x, y);
           end;
        end;
  result := true;
end;


end.
