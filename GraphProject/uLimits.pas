unit uLimits;

interface

uses SysUtils;

type
  THowtoScale = (hUpperLimit, hLowerLimit, hBothLimits);

var
  Grid_Xmin, Grid_Xmax, Grid_Ymin, Grid_Ymax : double;
  AutoScale_Xmin, AutoScale_Xmax, AutoScale_Ymin, AutoScale_Ymax : double;
  UserScale_Xmin, UserScale_Xmax, UserScale_Ymin, UserScale_Ymax : double;
  X_Origin, Y_Origin : double;
  AutoXScaling, AutoYScaling : boolean;


procedure FindDataLimits;
function  Log10 (d : double) : double;
function  power (x : Extended; n : Extended) : Extended;
procedure AutoScaleYAxis (HowtoScale : THowtoScale);
procedure AutoScaleXAxis (HowtoScale : THowtoScale);

implementation


procedure FindDataLimits;
var c, n, i, j : integer; d : double; str : shortstring;
begin
  Grid_Xmax := -1e30; Grid_Xmin := 1e30;
  Grid_Ymax := -1e30; Grid_Ymin := 1e30;
  CollectStatistics (CurrentStringTable);

  { Work our way through the X columns }
  c := GetnColumns (CurrentStringTable);
  for j := 1 to c do
      begin
      n := GetStats (CurrentStringTable, snPerCol, j);
      if GetColumnType (CurrentStringTable, j) = sXData then
         begin
         { Found one, so work out limits }
         for i := 1 to n do
             begin
             GetStringTableItem (CurrentStringTable, i, j, str);
             if str <> 'Missing' then
                begin
                d := SafeConvertToFloat (str);
                if d > Grid_Xmax then Grid_Xmax := d;
                if d < Grid_Xmin then Grid_Xmin := d;
                end;
             end;
         if Grid_Xmin = Grid_Xmax then { Just in case }
            begin
            if Grid_Xmin = 0 then
               Grid_Xmax := 1.0
            else
              if Grid_Xmax > 0 then Grid_Xmin := 0;
            if Grid_Xmin < 0 then Grid_Xmax := 0;
            end
         end
      else
         begin
         { Must therefore be a Y column }
         for i := 1 to n do
             begin
             GetStringTableItem (CurrentStringTable, i, j, str);
             if str <> 'Missing' then
                begin
                d := SafeConvertToFloat (str);
                if d > Grid_Ymax then Grid_Ymax := d;
                if d < Grid_Ymin then Grid_Ymin := d;
                end;
             end;
         if Grid_Ymin = Grid_Ymax then { Just in case }
            begin
            if Grid_Ymin = 0 then
               Grid_Ymax := 1.0
            else
              if Grid_Ymax > 0 then Grid_Ymin := 0;
            if Grid_Ymin < 0 then Grid_Ymax := 0;
            end
         end;
      end;
end;

function Log10 (d : double) : double;
begin
  result := ln (d)/ln (10);
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
          raise EMathError.Create ('Error calculating root of negative number in power');
        end;
     end;
end;


{ Removes the units digit from a positive number, always rounds down, thus
  163 -> 160, 6.2 -> 6, 16789.3 -> 16780. Also accepts negative numbers, so
  that -163 -> -160 }
function RoundDownUnits (d : double) : double;
var y : double;
begin
  result := trunc (d/10)*10;
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


procedure AutoScaleXAxis (HowtoScale : THowtoScale);
begin
  { Assume no adjustment to begin with }
  AutoScale_Xmin := Grid_Xmin; AutoScale_Xmax := Grid_Xmax;

  if (HowtoScale = hLowerLimit) or (HowtoScale = hBothLimits) then
     begin
     if LoggingX then
        begin
        { Error if Xmin < 0 }
        if Grid_Xmin > 0 then
           AutoScale_Xmin := RoundDown (Grid_Xmin)
        else
           showmessage ('Error!, No negative X values in Log Axes mode');
        end
     else
        begin
        if Grid_Xmin > 1 then AutoScale_Xmin := RoundDownUnits (Grid_Xmin);
        if (Grid_Xmin <= 1) and (Grid_Xmin > 0) then AutoScale_Xmin := 0;
        if Grid_Xmin < 0 then AutoScale_Xmin := -RoundUp (abs(Grid_Xmin));
        end;
     end;

  if (HowtoScale = hUpperLimit) or (HowtoScale = hBothLimits) then
     begin
     if Grid_Xmax > 1 then AutoScale_Xmax := RoundUp (Grid_Xmax);
     if (Grid_Xmax <= 1) and (Grid_Xmax > 0) then AutoScale_Xmax := RoundUp (Grid_Xmax);
     if Grid_Xmax < 0 then AutoScale_Xmax := -RoundDown (abs(Grid_Xmax));
     end;
end;


procedure AutoScaleYAxis (HowtoScale : THowtoScale);
begin
  { Assume no adjustment to begin with }
  AutoScale_Ymin := Grid_Ymin; AutoScale_Ymax := Grid_Ymax;

  if (HowtoScale = hLowerLimit) or (HowtoScale = hBothLimits) then
     begin
     if LoggingY then
        begin
        { Error if Ymin < 0 }
        if Grid_Ymin > 0 then
           AutoScale_Ymin := RoundDown (Grid_Ymin)
        else
           showmessage ('Error!, No negative Y values in Log Axes mode');
        end
     else
        begin
        if Grid_Ymin > 1 then AutoScale_Ymin := RoundDownUnits (Grid_Ymin);
        if (Grid_Ymin <= 1) and (Grid_Ymin > 0) then AutoScale_Ymin := 0;
        if Grid_Ymin < 0 then AutoScale_Ymin := -RoundUp (abs(Grid_Ymin));
        end;
     end;

  if (HowtoScale = hUpperLimit) or (HowtoScale = hBothLimits) then
     begin
     if Grid_Ymax > 1 then AutoScale_Ymax := RoundUp (Grid_Ymax);
     if (Grid_Ymax <= 1) and (Grid_Ymax > 0) then AutoScale_Ymax := RoundUp (Grid_Ymax);
     if Grid_Ymax < 0 then AutoScale_Ymax := -RoundDown (abs(Grid_Ymax));
     end;
end;

end.

