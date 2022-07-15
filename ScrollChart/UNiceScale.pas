unit UNiceScale;

interface
   uses Math, System.Types, System.UITypes, System.UIConsts, System.Contnrs,
   Skia, Skia.FMX, System.SysUtils;


type



//  TBox = class (TContainer)
//
//     constructor Create(w, h: double; bkg: TAlphaColor);
//     //constructor Destroy;
//     procedure Draw;
//  end;









  TNiceScale = class (TObject)
    private
      minPoint, maxPoint : double;
      range : double;
     procedure calculate;
    public
     nicemin, niceMax : double;
     maxTicks : double;
     tickSpacing : double;
     delta, width: double;
     function niceNum(range : double; round : boolean) : double;
     constructor Create (amin, amax : double);
     procedure setMaxTicks(maxT: double);
     procedure setMinMaxPoints(minP, maxP: double);
  end;

implementation













//constructor TBox.Create(w, h: double; bkg: TAlphaColor);
//begin
//  width := w;
//  height := h;
//  backgroundColor := bkg;
//end;
//
//procedure TBox.Draw;
//begin
//   Clear;
//end;


















//constructor TGraph.Create(w, h: double);
//begin
//   //width := w;
//   //height := h;
//   //backgroundColor := claWhite;
//end;






constructor TNiceScale.Create (amin, amax : double);
begin
  maxTicks := 10;
  width := amax - amin;
  delta := width/maxTicks;
  setMinMaxPoints(amin, amax);
end;

procedure TNiceScale.calculate;
begin
  range := niceNum(maxPoint - minPoint, false);
  tickSpacing := niceNum(range / (maxTicks - 1), true);
  niceMin := Math.floor(minPoint / tickSpacing) * tickSpacing;
  niceMax := Math.ceil(maxPoint / tickSpacing) * tickSpacing;
end;

function TNiceScale.niceNum(range : double; round : boolean) : double;
var
  exponent : double; // exponent of range
  fraction  : double; // fractional part of range
  niceFraction : double; // nice, rounded fraction
begin
  exponent := Math.floor(Math.log10(range));
  fraction := range / Math.power(10, exponent);
  if (round) then
     begin
     if (fraction < 1.5) then
         niceFraction := 1
     else if (fraction < 3) then
          niceFraction := 2
     else if (fraction < 7) then
          niceFraction := 5
     else
          niceFraction := 10;
     end
 else
     begin
     if (fraction <= 1) then
        niceFraction := 1
      else if (fraction <= 2) then
        niceFraction := 2
      else if (fraction <= 5) then
        niceFraction := 5
      else
        niceFraction := 10;
    end;
   result := niceFraction * Math.power(10, exponent);
end;

procedure TNiceScale.setMaxTicks(maxT: double);
begin
  maxTicks := maxT;
  calculate();
end;

procedure TNiceScale.setMinMaxPoints(minP, maxP : double);
begin
  minPoint := minP;
  maxPoint := maxP;
  calculate();
end;

end.
