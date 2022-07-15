         unit uLineDetails;

interface

Uses Classes, SysUtils, FMX.Graphics, System.UIConsts, System.UITypes, Json;

const
  DEFAULT_LINE_THICKNESS = 2;
  DEFAULT_LINE_COLOR = claOrange;
  DEFAULT_LINE_STYLE = 0;

type
  TLineStyle = (lsSolid, lsDash, lsDot, lsDashDot, lsDashDotDot, lsCustom);

   TLineDetails = class (TObject)
       Style : TLineStyle;
       ThicknessInSkiaUnits : double;
       Color : TAlphaColor;
       Visible : boolean;
       constructor Create;
       function  saveToJson : TJSONObject;
       constructor readFromJson (obj : TJSONObject);
   end;

  function getListOfLineStyles : TStringList;

implementation

Uses System.TypInfo, rtti;

var listOfLinesStyleNames : TStringList;
    i : integer;

constructor TLineDetails.Create;
begin
  Color := DEFAULT_LINE_COLOR;
  Visible := true;
  Style := TLineStyle (DEFAULT_LINE_STYLE);
  ThicknessInSkiaUnits := DEFAULT_LINE_THICKNESS;
end;

function TLineDetails.saveToJson : TJSONObject;
begin
  result := TJsonObject.Create;
  result.AddPair('color', IntToHex (color));
  result.AddPair('thickness', floatToStr (ThicknessInSkiaUnits));
  result.AddPair('style', TRttiEnumerationType.GetName(style));
  result.AddPair('visible', BoolToStr(visible, True));
end;


constructor TLineDetails.readFromJson (obj : TJSONObject);
begin
  color := StrToInt('$' + obj.GetValue<string>('color'));
  ThicknessInSkiaUnits := strtofloat (Obj.GetValue<string>('thickness'));
  style := TRttiEnumerationType.GetValue<TLineStyle>(Obj.GetValue<string>('style'));
  visible := StrToBool(obj.GetValue<string>('visible'));
end;


function getListOfLineStyles : TStringList;
begin
  result := listOfLinesStyleNames;
end;

initialization
  listOfLinesStyleNames := TStringList.Create;
  for i := 0 to 4 do
      listOfLinesStyleNames.add (GetEnumName(TypeInfo(TStrokeDash), ord (i)));
end.

