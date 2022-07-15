                                                               unit uSymbolDetails;

interface

Uses Classes, SysUtils, FMX.Graphics, System.UIConsts, System.UITypes, Json;

const
  DEFAULT_SYMBOL_OUTLINE_COLOR = claBlue;
  DEFAULT_SYMBOL_FILL_COLOR    = claLightBlue;
  DEFAULT_SYMBOL_GRADIENT_FILL_START_COLOR = claAntiqueWhite;
  DEFAULT_SYMBOL_GRADIENT_FILL_END_COLOR   = claRed;
  // Units are in cms
  DEFAULT_SYMBOL_DIAMETER = 0.2;
  DEFAULT_SYMBOL_THICKNESS = 0.03;
  DEFAULT_ERRORBAR_THICKNESS = 0.03;
  DEFAULT_ERRORBAR_CAPWIDTH = 0.1;
  DEFAULT_ERRORBAR_COLOR = claBlack;


type
  //TSymbolType = (EmptyCircle, SolidCircle, EmptySquare, SolidSquare,
  //                 Cross, DiagonalCross, CrossedCircle, DiagCrossedCircle,
  //                 EmptyDiamond, SolidDiamond, EmptyTriangle, SolidTriangle,
  //                 SolidDownTriangle, Dots, Empty);

  TErrorBarStyle = (ErrBarUp, ErrBarDown, ErrBarBoth, ErrBarNone);
  TErrorBarCapStyle = (WithCap, NoCap);

  TSymbolType = (SolidCircle, EmptyCircle, SolidSquare, EmptySquare, Cross, DiagonalCross,
                 CrossedCircle, DiagCrossedCircle, SolidDiamond, EmptyDiamond,
                 SolidTriangle, EmptyTriangle, SolidDownTriangle,
                 Dots, Empty);

  TSymbolGradientType = (gtUnknown, gtNone, _gtLinear, gtHorizLinear, gtVertLinear, gtRadial);

  TSymbol = class (TObject)
         symType : TSymbolType;         // Symbol used with this data set
         diameterInCms : double;       // Symbol size
         outlineInCms : double;
         fillColor : TAlphaColor;
         fillColorStart : TAlphaColor; // Used if Gradient set to true
         fillColorEnd : TAlphaColor;
         outlineColor : TAlphaColor;
         gradientType : TSymbolGradientType;   // If true, use gradient color

         visible : boolean;

         errorBarColor : TAlphaColor;
         errorBarCapWidthInCms : single;
         errorBarLengthInCms : single;
         errorBarVisible : boolean;

         constructor Create;
         function saveToJson : TJSONObject;
         constructor readFromJson (obj : TJSONObject);
  end;

  function getListOfSymbolNames : TStringList;
  function strToSymbolType (str : string) : TSymbolType;
  function nextSymbol : TSymbolType;


implementation

Uses System.TypInfo, rtti;

var listOfSymbolNames : TStringList;
    i : TSymbolType; str : string;
    data : PTypeData;
    nextSymbolCount : integer;

constructor TSymbol.Create;
begin
   symType := SolidCircle;      // Symbol used with this data set
   diameterInCms  := DEFAULT_SYMBOL_DIAMETER; // Symbol size
   outlineInCms   := DEFAULT_SYMBOL_THICKNESS;
   fillColor      := DEFAULT_SYMBOL_FILL_COLOR;
   fillColorStart := DEFAULT_SYMBOL_GRADIENT_FILL_START_COLOR; // Used if Gradient set to true
   fillColorEnd   := DEFAULT_SYMBOL_GRADIENT_FILL_END_COLOR;
   outlineColor   := DEFAULT_SYMBOL_OUTLINE_COLOR;
   gradientType := gtNone;
   visible  := true;

   errorBarCapWidthInCms := DEFAULT_ERRORBAR_CAPWIDTH;
   errorBarLengthInCms   := DEFAULT_ERRORBAR_THICKNESS;
   errorBarColor         := DEFAULT_ERRORBAR_COLOR;
   errorBarVisible := true;
end;


function TSymbol.saveToJson : TJSONObject;
begin
  result := TJsonObject.Create;
  result.AddPair('fillColor', IntToHex (fillColor));
  result.AddPair('outlineColor', IntToHex (outlineColor));
  result.AddPair('outlineInCms', floattostr (outlineInCms));
  result.AddPair('diameterInCms', floattostr (diameterInCms));
  result.AddPair('symType', TRttiEnumerationType.GetName(symType));
  result.AddPair('visible', BoolToStr(visible, True));
end;


constructor TSymbol.readFromJson (obj : TJSONObject);
begin
  fillColor := StrToInt('$' + obj.GetValue<string>('fillColor'));
  outlineColor := StrToInt('$' + Obj.GetValue<string>('outlineColor'));
  diameterInCms := strtofloat (Obj.GetValue<string>('diameterInCms'));
  outlineInCms := strtofloat (Obj.GetValue<string>('outlineInCms'));
  symType := TRttiEnumerationType.GetValue<TSymbolType>(Obj.GetValue<string>('symType'));
  visible := StrToBool(obj.GetValue<string>('visible'));
end;


function nextSymbol : TSymbolType;
begin
  if nextSymbolCount > Integer(High(TSymbolType)) then
     nextSymbolCount := 0;
  result := TSymbolType (nextSymbolCount);
  inc (nextSymbolCount);
end;


function getListOfSymbolNames : TStringList;
begin
  result := listOfSymbolNames;
end;

function strToSymbolType (str : string) : TSymbolType;
begin
  result := TSymbolType (listOfSymbolNames.IndexOf(str));
end;


initialization
  nextSymbolCount := 0;
  listOfSymbolNames := TStringList.Create;
  for i := Low (TSymbolType) to High (TSymbolType) do
      listOfSymbolNames.add (GetEnumName(TypeInfo(TSymbolType), ord (i)));
end.


