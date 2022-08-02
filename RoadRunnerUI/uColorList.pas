unit uColorList;

interface

Uses System.UIConsts, System.UITypes, Classes, SysUtils, FMX.Graphics, Generics.Collections;

type
  TColorArray = array of TAlphaColor;

  TColorGradient = class (TObject)
     name : string;
     color : TAlphaColor;
     color1 : TAlphaColor;
     constructor Create (color, color1 : TAlphaColor; name : string);
  end;


  TColorList = class(TObject)
  private
    ColorIndex: integer;
    CurrentColorPalette : string;
    ColorPercent : double;
    Gradient : TGradient;
  public
    Values: TStringList;
    ColorPercentIncrement : double;
    constructor Create;
    destructor  Destroy; override;
    procedure   AddColors;
    function    NextColor: TAlphaColor;
    procedure   Restart;
    procedure   SetPalette (palette : string; numberOfLevels : integer);
    function    GetColor(index: integer): TAlphaColor;
    property    Value[index: integer]: TAlphaColor read GetColor; default;
  end;

  TColorItem = class(TObject)
    Color: TAlphaColor;
    constructor Create(Color: TAlphaColor);
  end;

function ColorBetween(const ColorA, ColorB: TAlphaColor; Percent: integer): TAlphaColor;
function ColorsBetween(const ColorA, ColorB: TAlphaColor; const Count: integer): TColorArray;

function getPaletteNames : TStringList;

var
  masterColorList: TColorList;
  colorPalettes : TList<TColorGradient>;

implementation


constructor TColorGradient.Create (color, color1 : TAlphaColor; name : string);
begin
  self.color := color;
  self.color1 := color1;
  self.name := name;
end;


function getPaletteNames : TStringList;
var i : integer;
begin
  result := TStringList.Create;
  for i := 0 to colorPalettes.Count - 1 do
      result.Add(colorPalettes[i].name);
end;


function ColorBetween(const ColorA, ColorB: TAlphaColor; Percent: integer): TAlphaColor;
var
  R1, G1, B1: Byte;
  R2, G2, B2: Byte;
  ColorARec, ColorBRec, res: TAlphaColorRec;
begin
  Percent := abs(Percent);
  if Percent >= 100 then
    begin
      Result := ColorB;
      exit;
    end;

  ColorARec := TAlphaColorRec.Create(ColorA);
  ColorBRec := TAlphaColorRec.Create(ColorB);

  R1 := ColorARec.R;
  G1 := ColorARec.G;
  B1 := ColorARec.B;
  R2 := ColorBRec.R;
  G2 := ColorBRec.G;
  B2 := ColorBRec.B;

  res.R := Percent * (R2 - R1) div 100 + R1;
  res.G := Percent * (G2 - G1) div 100 + G1;
  res.B := Percent * (B2 - B1) div 100 + B1;
  res.A := 1;

  Result := res.Color;
end;


function ColorsBetween(const ColorA, ColorB: TAlphaColor; const Count: integer): TColorArray;
var
  X: integer;
begin
  SetLength(Result, Count);
  for X := 0 to Count - 1 do
    Result[X] := ColorBetween(ColorA, ColorB, Round((X / (Count - 1)) * 100));
  // Note the divide by count-1
end;


constructor TColorItem.Create(Color: TAlphaColor);
begin
  self.Color := Color;
end;


constructor TColorList.Create;
begin
  CurrentColorPalette := 'Default';
  ColorPercent := 0;
  ColorPercentIncrement := 0.075;
  Gradient := TGradient.Create;
  Values := TStringList.Create;
  Values.Sorted := False;
  addColors;
  Restart;
end;


destructor TColorList.Destroy;
begin
  Values.Free;
  Gradient.Free;
end;


procedure TColorList.SetPalette (palette : string; numberOfLevels : integer);
var i : integer;
begin
  ColorPercentIncrement := 1/numberOfLevels;
  if palette = 'Default' then
     begin
     CurrentColorPalette := 'Default';
     end
  else
     begin
     for i := 0 to colorPalettes.Count - 1 do
         if colorPalettes[i].name = palette then
            begin
            gradient.Style := TGradientStyle.Linear;
            gradient.Color := colorPalettes[i].color;
            gradient.Color1 := colorPalettes[i].color1;
            CurrentColorPalette := palette;
            exit;
            end;
     end;
end;


function TColorList.GetColor(index: integer): TAlphaColor;
begin
  Result := (Values.Objects[index] as TColorItem).Color;
end;


function TColorList.NextColor: TAlphaColor;
begin
  if currentColorPalette = 'Default' then
     begin
     Result := (Values.Objects[ColorIndex] as TColorItem).Color;
     inc(ColorIndex);
     if ColorIndex >= Values.Count then
        Restart;
    end
  else
    begin
    if ColorPercent > 1 then
       ColorPercent := 0;
    Result := Gradient.InterpolateColor(ColorPercent);
    ColorPercent := ColorPercent + ColorPercentIncrement;
    end;
end;


procedure TColorList.Restart;
begin
  if currentColorPalette = 'Default' then
     ColorIndex := 0
  else
     ColorPercent := 0.0;
end;


procedure TColorList.AddColors;
begin
  Values.AddObject('LightSalmon', TColorItem.Create(claLightSalmon));
  Values.AddObject('Red', TColorItem.Create(claRed));
  Values.AddObject('Blue', TColorItem.Create(claBlue));
  Values.AddObject('Green', TColorItem.Create(claGreen));

  //masterColorList.Values.AddObject('C1', TColorItem.Create(TAlphaColor($FFe60049)));
  Values.AddObject('C2', TColorItem.Create(TAlphaColor(claDarkViolet)));

  //masterColorList.Values.AddObject('C3', TColorItem.Create(TAlphaColor($FF0bb4ff)));
  Values.AddObject('C3', TColorItem.Create(TAlphaColor($FF0bb4ff)));

  Values.AddObject('C4', TColorItem.Create(TAlphaColor($FF50e991)));
  Values.AddObject('C5', TColorItem.Create(TAlphaColor(claRoyalBlue)));

  //masterColorList.Values.AddObject('C5', TColorItem.Create(TAlphaColor($FFe6d800)));

  Values.AddObject('C6', TColorItem.Create(TAlphaColor($FF9b19f5)));
  Values.AddObject('C7', TColorItem.Create(TAlphaColor($FFffa300)));
  //masterColorList.Values.AddObject('C8', TColorItem.Create(TAlphaColor($FFdc0ab4)));
  //masterColorList.Values.AddObject('C9', TColorItem.Create(TAlphaColor($FFb3d4ff)));
 // masterColorList.Values.AddObject('C9', TColorItem.Create(TAlphaColor($FF00bfa0)));

//  masterColorList.Values.AddObject('C1', TColorItem.Create(TAlphaColor($FFea5545)));
//  masterColorList.Values.AddObject('C2', TColorItem.Create(TAlphaColor($FFf46a9b)));
//  masterColorList.Values.AddObject('C3', TColorItem.Create(TAlphaColor($FFef9b20)));
//  masterColorList.Values.AddObject('C4', TColorItem.Create(TAlphaColor($FFedbf33)));
//  masterColorList.Values.AddObject('C5', TColorItem.Create(TAlphaColor($FFdc0ab4)));
//  masterColorList.Values.AddObject('C6', TColorItem.Create(TAlphaColor($FFbdcf32)));
//  masterColorList.Values.AddObject('C7', TColorItem.Create(TAlphaColor($FF87bc45)));
//  masterColorList.Values.AddObject('C8', TColorItem.Create(TAlphaColor($FF27aeef)));
//  masterColorList.Values.AddObject('C9', TColorItem.Create(TAlphaColor($FFb33dc6)));


  //masterColorList.Values.AddObject('Chocolate', TColorItem.Create(claChocolate));
 // masterColorList.Values.AddObject('Purple', TColorItem.Create(claPurple));
  //masterColorList.Values.AddObject('Orange', TColorItem.Create(claOrange));

  //masterColorList.Values.AddObject('Coral', TColorItem.Create(claCoral));

 // masterColorList.Values.AddObject('LightCoral', TColorItem.Create(claLightCoral));
  //masterColorList.Values.AddObject('OrangeRed', TColorItem.Create(claOrangeRed));
  //masterColorList.Values.AddObject('Salmon', TColorItem.Create(claSalmon));
  //masterColorList.Values.AddObject('Peru', TColorItem.Create(claPeru));

  Values.AddObject('DarkTurquoise', TColorItem.Create(claDarkTurquoise));
  Values.AddObject('DarkSalmon', TColorItem.Create(claDarkSalmon));
  Values.AddObject('RosyBrown', TColorItem.Create(claRosyBrown));
  Values.AddObject('DarkOrange', TColorItem.Create(claDarkOrange));
  Values.AddObject('LightGoldenrodYellow', TColorItem.Create(claLightGoldenrodYellow));
  Values.AddObject('IndianRed', TColorItem.Create(claIndianRed));
  Values.AddObject('Olive', TColorItem.Create(claOlive));
  Values.AddObject('SandyBrown', TColorItem.Create(claSandyBrown));

  Values.AddObject('DarkRed', TColorItem.Create(claDarkRed));
  Values.AddObject('Maroon', TColorItem.Create(claMaroon));
  Values.AddObject('Brown', TColorItem.Create(claBrown));
  Values.AddObject('ForestGreen', TColorItem.Create(claForestGreen));
  Values.AddObject('Firebrick', TColorItem.Create(claFirebrick));
  Values.AddObject('SaddleBrown', TColorItem.Create(claSaddleBrown));
  Values.AddObject('Sienna', TColorItem.Create(claSienna));
  Values.AddObject('PaleGoldenrod', TColorItem.Create(claPaleGoldenrod));

  Values.AddObject('LightSkyBlue', TColorItem.Create(claLightSkyBlue));
  Values.AddObject('Chartreuse', TColorItem.Create(claChartreuse));
  Values.AddObject('Indigo', TColorItem.Create(claIndigo));
  Values.AddObject('Aquamarine', TColorItem.Create(claAquamarine));
  Values.AddObject('SeaGreen', TColorItem.Create(claSeaGreen));
  Values.AddObject('GoldenRod', TColorItem.Create(claGoldenRod));
  Values.AddObject('Khaki', TColorItem.Create(claKhaki));
  Values.AddObject('OliveDrab', TColorItem.Create(claOliveDrab));
  Values.AddObject('YellowGreen', TColorItem.Create(claYellowGreen));
  Values.AddObject('Gold', TColorItem.Create(claGold));
  Values.AddObject('LawnGreen', TColorItem.Create(claLawnGreen));
  Values.AddObject('PaleGreen', TColorItem.Create(claPaleGreen));
  Values.AddObject('MediumAquamarine', TColorItem.Create(claMediumAquamarine));
  Values.AddObject('MediumSeaGreen', TColorItem.Create(claMediumSeaGreen));
  Values.AddObject('DarkGoldenRod', TColorItem.Create(claDarkGoldenRod));
  Values.AddObject('DarkKhaki', TColorItem.Create(claDarkKhaki));
  Values.AddObject('DarkOliveGreen', TColorItem.Create(claDarkOliveGreen));
  Values.AddObject('Darkgreen', TColorItem.Create(claDarkgreen));
  Values.AddObject('Magenta', TColorItem.Create(claMagenta));
  Values.AddObject('Crimson', TColorItem.Create(claCrimson));
  Values.AddObject('LimeGreen', TColorItem.Create(claLimeGreen));
  Values.AddObject('SpringGreen', TColorItem.Create(claSpringGreen));
  Values.AddObject('Snow', TColorItem.Create(claSnow));
  Values.AddObject('MediumSpringGreen', TColorItem.Create(claMediumSpringGreen));
  Values.AddObject('Tomato', TColorItem.Create(claTomato));
  Values.AddObject('DarkSeaGreen', TColorItem.Create(claDarkSeaGreen));
  Values.AddObject('LightSeaGreen', TColorItem.Create(claLightSeaGreen));
  Values.AddObject('PaleTurquoise', TColorItem.Create(claPaleTurquoise));
  Values.AddObject('LightCyan', TColorItem.Create(claLightCyan));
  Values.AddObject('LightBlue', TColorItem.Create(claLightBlue));
  Values.AddObject('CornFlowerBlue', TColorItem.Create(claCornFlowerBlue));
  Values.AddObject('DarkBlue', TColorItem.Create(claDarkBlue));
  Values.AddObject('MediumTurquoise', TColorItem.Create(claMediumTurquoise));
  Values.AddObject('Turquoise', TColorItem.Create(claTurquoise));
  Values.AddObject('Aqua', TColorItem.Create(claAqua));
  Values.AddObject('PowderBlue', TColorItem.Create(claPowderBlue));
  Values.AddObject('SkyBlue', TColorItem.Create(claSkyBlue));
  Values.AddObject('RoyalBlue', TColorItem.Create(claRoyalBlue));
  Values.AddObject('MediumBlue', TColorItem.Create(claMediumBlue));
  Values.AddObject('MidnightBlue', TColorItem.Create(claMidnightBlue));
  Values.AddObject('CadetBlue', TColorItem.Create(claCadetBlue));
  Values.AddObject('DarkCyan', TColorItem.Create(claDarkCyan));
  Values.AddObject('DeepskyBlue', TColorItem.Create(claDeepskyBlue));
  Values.AddObject('DodgerBlue', TColorItem.Create(claDodgerBlue));

  Values.AddObject('Navy', TColorItem.Create(claNavy));
  Values.AddObject('DarkViolet', TColorItem.Create(claDarkViolet));
  Values.AddObject('DarkOrchid', TColorItem.Create(claDarkOrchid));
  Values.AddObject('DarkMagenta', TColorItem.Create(claDarkMagenta));
  Values.AddObject('MediumVioletRed', TColorItem.Create(claMediumVioletRed));
  Values.AddObject('PaleVioletRed', TColorItem.Create(claPaleVioletRed));
  Values.AddObject('BlueViolet', TColorItem.Create(claBlueViolet));
  Values.AddObject('MediumOrchid', TColorItem.Create(claMediumOrchid));
  Values.AddObject('MediumPurple', TColorItem.Create(claMediumPurple));
  Values.AddObject('DeepPink', TColorItem.Create(claDeepPink));
  Values.AddObject('LightPink', TColorItem.Create(claLightPink));
  Values.AddObject('Orchid', TColorItem.Create(claOrchid));
  Values.AddObject('Plum', TColorItem.Create(claPlum));
  Values.AddObject('Thistle', TColorItem.Create(claThistle));
  Values.AddObject('HotPink', TColorItem.Create(claHotPink));
  Values.AddObject('Pink', TColorItem.Create(claPink));
  Values.AddObject('LightSteelBlue', TColorItem.Create(claLightSteelBlue));
  Values.AddObject('MediumSlateBlue', TColorItem.Create(claMediumSlateBlue));
  Values.AddObject('LightSlateGray', TColorItem.Create(claLightSlateGray));
  Values.AddObject('White', TColorItem.Create(claWhite));
  Values.AddObject('Lightgrey', TColorItem.Create(claLightgrey));
  Values.AddObject('Gray', TColorItem.Create(claGray));
  Values.AddObject('SteelBlue', TColorItem.Create(claSteelBlue));
  Values.AddObject('SlateBlue', TColorItem.Create(claSlateBlue));
  Values.AddObject('SlateGray', TColorItem.Create(claSlateGray));
  Values.AddObject('WhiteSmoke', TColorItem.Create(claWhiteSmoke));
  Values.AddObject('Silver', TColorItem.Create(claSilver));
  Values.AddObject('DimGray', TColorItem.Create(claDimGray));
  Values.AddObject('MistyRose', TColorItem.Create(claMistyRose));
  Values.AddObject('DarkSlateBlue', TColorItem.Create(claDarkSlateBlue));
  Values.AddObject('DarkSlategray', TColorItem.Create(claDarkSlategray));
  Values.AddObject('Gainsboro', TColorItem.Create(claGainsboro));
  Values.AddObject('DarkGray', TColorItem.Create(claDarkGray));
  Values.AddObject('Black', TColorItem.Create(claBlack));
  Values.AddObject('Violet', TColorItem.Create(claViolet));
  Values.AddObject('Moccasin', TColorItem.Create(claMoccasin));
  Values.AddObject('Fuchsia', TColorItem.Create(claFuchsia));

  Values.AddObject('GhostWhite', TColorItem.Create(claGhostWhite));
  Values.AddObject('Lavender', TColorItem.Create(claLavender));
  Values.AddObject('Seashell', TColorItem.Create(claSeashell));
  Values.AddObject('LightYellow', TColorItem.Create(claLightYellow));
  Values.AddObject('PapayaWhip', TColorItem.Create(claPapayaWhip));
  Values.AddObject('NavajoWhite', TColorItem.Create(claNavajoWhite));
  Values.AddObject('Burlywood', TColorItem.Create(claBurlywood));
  Values.AddObject('Azure', TColorItem.Create(claAzure));
  Values.AddObject('Mintcream', TColorItem.Create(claMintcream));
  Values.AddObject('Honeydew', TColorItem.Create(claHoneydew));
  Values.AddObject('Linen', TColorItem.Create(claLinen));
  Values.AddObject('LemonChiffon', TColorItem.Create(claLemonChiffon));
  Values.AddObject('BlanchedAlmond', TColorItem.Create(claBlanchedAlmond));
  Values.AddObject('Bisque', TColorItem.Create(claBisque));
  Values.AddObject('PeachPuff', TColorItem.Create(claPeachPuff));
  Values.AddObject('Tan', TColorItem.Create(claTan));
  Values.AddObject('Yellow', TColorItem.Create(claYellow));
  Values.AddObject('Lime', TColorItem.Create(claLime));
  Values.AddObject('Cyan', TColorItem.Create(claCyan));
  Values.AddObject('Teal', TColorItem.Create(claTeal));
  Values.AddObject('GreenYellow', TColorItem.Create(claGreenYellow));
  Values.AddObject('LightGreen', TColorItem.Create(claLightGreen));

  Values.AddObject('Silver', TColorItem.Create(claSilver));
  Values.AddObject('FloralWhite', TColorItem.Create(claFloralWhite));
  Values.AddObject('White', TColorItem.Create(claWhite));
  Values.AddObject('Gray', TColorItem.Create(claGray));
  Values.AddObject('Ivory', TColorItem.Create(claIvory));
  Values.AddObject('CornSilk', TColorItem.Create(claCornSilk));
  Values.AddObject('Beige', TColorItem.Create(claBeige));
  Values.AddObject('OldLace', TColorItem.Create(claOldLace));
  Values.AddObject('AntiqueWhite', TColorItem.Create(claAntiqueWhite));
  Values.AddObject('Skyblue', TColorItem.Create(claSkyBlue));
  Values.AddObject('Wheat', TColorItem.Create(claWheat));
  Values.AddObject('LavenderBlush', TColorItem.Create(claLavenderBlush));
  Values.AddObject('AliceBlue', TColorItem.Create(claAliceBlue));
end;


initialization
  masterColorList := TColorList.Create;

  masterColorList.Values.AddObject('LightSalmon', TColorItem.Create(claLightSalmon));
  masterColorList.Values.AddObject('Red', TColorItem.Create(claRed));
  masterColorList.Values.AddObject('Blue', TColorItem.Create(claBlue));
  masterColorList.Values.AddObject('Green', TColorItem.Create(claGreen));

  //masterColorList.Values.AddObject('C1', TColorItem.Create(TAlphaColor($FFe60049)));
  masterColorList.Values.AddObject('C2', TColorItem.Create(TAlphaColor(claDarkViolet)));

  //masterColorList.Values.AddObject('C3', TColorItem.Create(TAlphaColor($FF0bb4ff)));
  masterColorList.Values.AddObject('C3', TColorItem.Create(TAlphaColor($FF0bb4ff)));

  masterColorList.Values.AddObject('C4', TColorItem.Create(TAlphaColor($FF50e991)));
  masterColorList.Values.AddObject('C5', TColorItem.Create(TAlphaColor(claRoyalBlue)));

  //masterColorList.Values.AddObject('C5', TColorItem.Create(TAlphaColor($FFe6d800)));

  masterColorList.Values.AddObject('C6', TColorItem.Create(TAlphaColor($FF9b19f5)));
  masterColorList.Values.AddObject('C7', TColorItem.Create(TAlphaColor($FFffa300)));
  //masterColorList.Values.AddObject('C8', TColorItem.Create(TAlphaColor($FFdc0ab4)));
  //masterColorList.Values.AddObject('C9', TColorItem.Create(TAlphaColor($FFb3d4ff)));
 // masterColorList.Values.AddObject('C9', TColorItem.Create(TAlphaColor($FF00bfa0)));

//  masterColorList.Values.AddObject('C1', TColorItem.Create(TAlphaColor($FFea5545)));
//  masterColorList.Values.AddObject('C2', TColorItem.Create(TAlphaColor($FFf46a9b)));
//  masterColorList.Values.AddObject('C3', TColorItem.Create(TAlphaColor($FFef9b20)));
//  masterColorList.Values.AddObject('C4', TColorItem.Create(TAlphaColor($FFedbf33)));
//  masterColorList.Values.AddObject('C5', TColorItem.Create(TAlphaColor($FFdc0ab4)));
//  masterColorList.Values.AddObject('C6', TColorItem.Create(TAlphaColor($FFbdcf32)));
//  masterColorList.Values.AddObject('C7', TColorItem.Create(TAlphaColor($FF87bc45)));
//  masterColorList.Values.AddObject('C8', TColorItem.Create(TAlphaColor($FF27aeef)));
//  masterColorList.Values.AddObject('C9', TColorItem.Create(TAlphaColor($FFb33dc6)));


  //masterColorList.Values.AddObject('Chocolate', TColorItem.Create(claChocolate));
 // masterColorList.Values.AddObject('Purple', TColorItem.Create(claPurple));
  //masterColorList.Values.AddObject('Orange', TColorItem.Create(claOrange));

  //masterColorList.Values.AddObject('Coral', TColorItem.Create(claCoral));

 // masterColorList.Values.AddObject('LightCoral', TColorItem.Create(claLightCoral));
  //masterColorList.Values.AddObject('OrangeRed', TColorItem.Create(claOrangeRed));
  //masterColorList.Values.AddObject('Salmon', TColorItem.Create(claSalmon));
  //masterColorList.Values.AddObject('Peru', TColorItem.Create(claPeru));

  masterColorList.Values.AddObject('DarkTurquoise', TColorItem.Create(claDarkTurquoise));
  masterColorList.Values.AddObject('DarkSalmon', TColorItem.Create(claDarkSalmon));
  masterColorList.Values.AddObject('RosyBrown', TColorItem.Create(claRosyBrown));
  masterColorList.Values.AddObject('DarkOrange', TColorItem.Create(claDarkOrange));
  masterColorList.Values.AddObject('LightGoldenrodYellow', TColorItem.Create(claLightGoldenrodYellow));
  masterColorList.Values.AddObject('IndianRed', TColorItem.Create(claIndianRed));
  masterColorList.Values.AddObject('Olive', TColorItem.Create(claOlive));
  masterColorList.Values.AddObject('SandyBrown', TColorItem.Create(claSandyBrown));

  masterColorList.Values.AddObject('DarkRed', TColorItem.Create(claDarkRed));
  masterColorList.Values.AddObject('Maroon', TColorItem.Create(claMaroon));
  masterColorList.Values.AddObject('Brown', TColorItem.Create(claBrown));
  masterColorList.Values.AddObject('ForestGreen', TColorItem.Create(claForestGreen));
  masterColorList.Values.AddObject('Firebrick', TColorItem.Create(claFirebrick));
  masterColorList.Values.AddObject('SaddleBrown', TColorItem.Create(claSaddleBrown));
  masterColorList.Values.AddObject('Sienna', TColorItem.Create(claSienna));
  masterColorList.Values.AddObject('PaleGoldenrod', TColorItem.Create(claPaleGoldenrod));

  masterColorList.Values.AddObject('LightSkyBlue', TColorItem.Create(claLightSkyBlue));
  masterColorList.Values.AddObject('Chartreuse', TColorItem.Create(claChartreuse));
  masterColorList.Values.AddObject('Indigo', TColorItem.Create(claIndigo));
  masterColorList.Values.AddObject('Aquamarine', TColorItem.Create(claAquamarine));
  masterColorList.Values.AddObject('SeaGreen', TColorItem.Create(claSeaGreen));
  masterColorList.Values.AddObject('GoldenRod', TColorItem.Create(claGoldenRod));
  masterColorList.Values.AddObject('Khaki', TColorItem.Create(claKhaki));
  masterColorList.Values.AddObject('OliveDrab', TColorItem.Create(claOliveDrab));
  masterColorList.Values.AddObject('YellowGreen', TColorItem.Create(claYellowGreen));
  masterColorList.Values.AddObject('Gold', TColorItem.Create(claGold));
  masterColorList.Values.AddObject('LawnGreen', TColorItem.Create(claLawnGreen));
  masterColorList.Values.AddObject('PaleGreen', TColorItem.Create(claPaleGreen));
  masterColorList.Values.AddObject('MediumAquamarine', TColorItem.Create(claMediumAquamarine));
  masterColorList.Values.AddObject('MediumSeaGreen', TColorItem.Create(claMediumSeaGreen));
  masterColorList.Values.AddObject('DarkGoldenRod', TColorItem.Create(claDarkGoldenRod));
  masterColorList.Values.AddObject('DarkKhaki', TColorItem.Create(claDarkKhaki));
  masterColorList.Values.AddObject('DarkOliveGreen', TColorItem.Create(claDarkOliveGreen));
  masterColorList.Values.AddObject('Darkgreen', TColorItem.Create(claDarkgreen));
  masterColorList.Values.AddObject('Magenta', TColorItem.Create(claMagenta));
  masterColorList.Values.AddObject('Crimson', TColorItem.Create(claCrimson));
  masterColorList.Values.AddObject('LimeGreen', TColorItem.Create(claLimeGreen));
  masterColorList.Values.AddObject('SpringGreen', TColorItem.Create(claSpringGreen));
  masterColorList.Values.AddObject('Snow', TColorItem.Create(claSnow));
  masterColorList.Values.AddObject('MediumSpringGreen', TColorItem.Create(claMediumSpringGreen));
  masterColorList.Values.AddObject('Tomato', TColorItem.Create(claTomato));
  masterColorList.Values.AddObject('DarkSeaGreen', TColorItem.Create(claDarkSeaGreen));
  masterColorList.Values.AddObject('LightSeaGreen', TColorItem.Create(claLightSeaGreen));
  masterColorList.Values.AddObject('PaleTurquoise', TColorItem.Create(claPaleTurquoise));
  masterColorList.Values.AddObject('LightCyan', TColorItem.Create(claLightCyan));
  masterColorList.Values.AddObject('LightBlue', TColorItem.Create(claLightBlue));
  masterColorList.Values.AddObject('CornFlowerBlue', TColorItem.Create(claCornFlowerBlue));
  masterColorList.Values.AddObject('DarkBlue', TColorItem.Create(claDarkBlue));
  masterColorList.Values.AddObject('MediumTurquoise', TColorItem.Create(claMediumTurquoise));
  masterColorList.Values.AddObject('Turquoise', TColorItem.Create(claTurquoise));
  masterColorList.Values.AddObject('Aqua', TColorItem.Create(claAqua));
  masterColorList.Values.AddObject('PowderBlue', TColorItem.Create(claPowderBlue));
  masterColorList.Values.AddObject('SkyBlue', TColorItem.Create(claSkyBlue));
  masterColorList.Values.AddObject('RoyalBlue', TColorItem.Create(claRoyalBlue));
  masterColorList.Values.AddObject('MediumBlue', TColorItem.Create(claMediumBlue));
  masterColorList.Values.AddObject('MidnightBlue', TColorItem.Create(claMidnightBlue));
  masterColorList.Values.AddObject('CadetBlue', TColorItem.Create(claCadetBlue));
  masterColorList.Values.AddObject('DarkCyan', TColorItem.Create(claDarkCyan));
  masterColorList.Values.AddObject('DeepskyBlue', TColorItem.Create(claDeepskyBlue));
  masterColorList.Values.AddObject('DodgerBlue', TColorItem.Create(claDodgerBlue));

  masterColorList.Values.AddObject('Navy', TColorItem.Create(claNavy));
  masterColorList.Values.AddObject('DarkViolet', TColorItem.Create(claDarkViolet));
  masterColorList.Values.AddObject('DarkOrchid', TColorItem.Create(claDarkOrchid));
  masterColorList.Values.AddObject('DarkMagenta', TColorItem.Create(claDarkMagenta));
  masterColorList.Values.AddObject('MediumVioletRed', TColorItem.Create(claMediumVioletRed));
  masterColorList.Values.AddObject('PaleVioletRed', TColorItem.Create(claPaleVioletRed));
  masterColorList.Values.AddObject('BlueViolet', TColorItem.Create(claBlueViolet));
  masterColorList.Values.AddObject('MediumOrchid', TColorItem.Create(claMediumOrchid));
  masterColorList.Values.AddObject('MediumPurple', TColorItem.Create(claMediumPurple));
  masterColorList.Values.AddObject('DeepPink', TColorItem.Create(claDeepPink));
  masterColorList.Values.AddObject('LightPink', TColorItem.Create(claLightPink));
  masterColorList.Values.AddObject('Orchid', TColorItem.Create(claOrchid));
  masterColorList.Values.AddObject('Plum', TColorItem.Create(claPlum));
  masterColorList.Values.AddObject('Thistle', TColorItem.Create(claThistle));
  masterColorList.Values.AddObject('HotPink', TColorItem.Create(claHotPink));
  masterColorList.Values.AddObject('Pink', TColorItem.Create(claPink));
  masterColorList.Values.AddObject('LightSteelBlue', TColorItem.Create(claLightSteelBlue));
  masterColorList.Values.AddObject('MediumSlateBlue', TColorItem.Create(claMediumSlateBlue));
  masterColorList.Values.AddObject('LightSlateGray', TColorItem.Create(claLightSlateGray));
  masterColorList.Values.AddObject('White', TColorItem.Create(claWhite));
  masterColorList.Values.AddObject('Lightgrey', TColorItem.Create(claLightgrey));
  masterColorList.Values.AddObject('Gray', TColorItem.Create(claGray));
  masterColorList.Values.AddObject('SteelBlue', TColorItem.Create(claSteelBlue));
  masterColorList.Values.AddObject('SlateBlue', TColorItem.Create(claSlateBlue));
  masterColorList.Values.AddObject('SlateGray', TColorItem.Create(claSlateGray));
  masterColorList.Values.AddObject('WhiteSmoke', TColorItem.Create(claWhiteSmoke));
  masterColorList.Values.AddObject('Silver', TColorItem.Create(claSilver));
  masterColorList.Values.AddObject('DimGray', TColorItem.Create(claDimGray));
  masterColorList.Values.AddObject('MistyRose', TColorItem.Create(claMistyRose));
  masterColorList.Values.AddObject('DarkSlateBlue', TColorItem.Create(claDarkSlateBlue));
  masterColorList.Values.AddObject('DarkSlategray', TColorItem.Create(claDarkSlategray));
  masterColorList.Values.AddObject('Gainsboro', TColorItem.Create(claGainsboro));
  masterColorList.Values.AddObject('DarkGray', TColorItem.Create(claDarkGray));
  masterColorList.Values.AddObject('Black', TColorItem.Create(claBlack));
  masterColorList.Values.AddObject('Violet', TColorItem.Create(claViolet));
  masterColorList.Values.AddObject('Moccasin', TColorItem.Create(claMoccasin));
  masterColorList.Values.AddObject('Fuchsia', TColorItem.Create(claFuchsia));

  masterColorList.Values.AddObject('GhostWhite', TColorItem.Create(claGhostWhite));
  masterColorList.Values.AddObject('Lavender', TColorItem.Create(claLavender));
  masterColorList.Values.AddObject('Seashell', TColorItem.Create(claSeashell));
  masterColorList.Values.AddObject('LightYellow', TColorItem.Create(claLightYellow));
  masterColorList.Values.AddObject('PapayaWhip', TColorItem.Create(claPapayaWhip));
  masterColorList.Values.AddObject('NavajoWhite', TColorItem.Create(claNavajoWhite));
  masterColorList.Values.AddObject('Burlywood', TColorItem.Create(claBurlywood));
  masterColorList.Values.AddObject('Azure', TColorItem.Create(claAzure));
  masterColorList.Values.AddObject('Mintcream', TColorItem.Create(claMintcream));
  masterColorList.Values.AddObject('Honeydew', TColorItem.Create(claHoneydew));
  masterColorList.Values.AddObject('Linen', TColorItem.Create(claLinen));
  masterColorList.Values.AddObject('LemonChiffon', TColorItem.Create(claLemonChiffon));
  masterColorList.Values.AddObject('BlanchedAlmond', TColorItem.Create(claBlanchedAlmond));
  masterColorList.Values.AddObject('Bisque', TColorItem.Create(claBisque));
  masterColorList.Values.AddObject('PeachPuff', TColorItem.Create(claPeachPuff));
  masterColorList.Values.AddObject('Tan', TColorItem.Create(claTan));
  masterColorList.Values.AddObject('Yellow', TColorItem.Create(claYellow));
  masterColorList.Values.AddObject('Lime', TColorItem.Create(claLime));
  masterColorList.Values.AddObject('Cyan', TColorItem.Create(claCyan));
  masterColorList.Values.AddObject('Teal', TColorItem.Create(claTeal));
  masterColorList.Values.AddObject('GreenYellow', TColorItem.Create(claGreenYellow));
  masterColorList.Values.AddObject('LightGreen', TColorItem.Create(claLightGreen));

  masterColorList.Values.AddObject('Silver', TColorItem.Create(claSilver));
  masterColorList.Values.AddObject('FloralWhite', TColorItem.Create(claFloralWhite));
  masterColorList.Values.AddObject('White', TColorItem.Create(claWhite));
  masterColorList.Values.AddObject('Gray', TColorItem.Create(claGray));
  masterColorList.Values.AddObject('Ivory', TColorItem.Create(claIvory));
  masterColorList.Values.AddObject('CornSilk', TColorItem.Create(claCornSilk));
  masterColorList.Values.AddObject('Beige', TColorItem.Create(claBeige));
  masterColorList.Values.AddObject('OldLace', TColorItem.Create(claOldLace));
  masterColorList.Values.AddObject('AntiqueWhite', TColorItem.Create(claAntiqueWhite));
  masterColorList.Values.AddObject('Skyblue', TColorItem.Create(claSkyBlue));
  masterColorList.Values.AddObject('Wheat', TColorItem.Create(claWheat));
  masterColorList.Values.AddObject('LavenderBlush', TColorItem.Create(claLavenderBlush));
  masterColorList.Values.AddObject('AliceBlue', TColorItem.Create(claAliceBlue));

   colorPalettes := TList<TColorGradient>.Create;
   colorPalettes.Add(TColorGradient.Create(claBlue, claRed, 'Default'));
   colorPalettes.Add(TColorGradient.Create(claBlue, claRed, 'BlueRed'));
   colorPalettes.Add(TColorGradient.Create(claRed, claGreen, 'RedGreen'));
   colorPalettes.Add(TColorGradient.Create(claBlack, claWhite, 'BlackWhite'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFFF7E5F), TAlphaColor($FFFEB47B), 'SunSet'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFFF00CC), TAlphaColor($FF333399), 'CosmicFusion'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFDE6161), TAlphaColor($FF2657EB), 'Nepal'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFEF32D9), TAlphaColor($FF89FFFD), 'AzurePop'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FF4ECDC4), TAlphaColor($FF556270), 'GreenPale'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFBDC3C7), TAlphaColor($FF2C3E50), 'GreyShades'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFFFD89B), TAlphaColor($FF19547B), 'Jupiter'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFF79D00), TAlphaColor($FF64F38C), 'Sherbert'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FF8E0E00), TAlphaColor($FF1F1C18), 'RedBlack'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FFFC00FF), TAlphaColor($FF00DBDE), 'Timber'));
   colorPalettes.Add(TColorGradient.Create(TAlphaColor($FF2E3192), TAlphaColor($FF1BFFFF), 'OceanBlue'));

finalization
  masterColorList.Free;
end.
