unit Syntax.Code.Antimony;

interface

uses
  System.SysUtils, System.StrUtils, Syntax.Code, FMX.TextLayout, FMX.Graphics,
  System.UIConsts, System.UITypes;

type
  TCodeSyntaxAntimony = class(TCodeSyntax)
  private
    FAtRuleKey: TKeyWord;
    FPropertyKey: TKeyWord;
    FNumberKey: TKeyWord;
    FHexColorKey: TKeyWord;
    FFunctionKey: TKeyWord;
    FImportantKey: TKeyWord;
    FVarKey: TKeyWord;
    FAttrKey: TKeyWord;
    FPseudoKey: TKeyWord;
    FUrlKey: TKeyWord;
    FSelectorKey: TKeyWord;
    FClassKey: TKeyWord;
    FIdKey: TKeyWord;
    FCombinatorKey: TKeyWord;
    FBraceKey: TKeyWord;
    FCommaKey: TKeyWord;
    FMediaFeatureKey: TKeyWord;
    FAtRuleNameKey: TKeyWord;

  private
    FKeyWords: TKeyWords;
    FStringKey, FNumKey, FCommentKey, FDirectiveKey: TKeyWord;

  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

type
  TSimpleRange = record
    Start: Integer;
    Len: Integer;
    function Contains(const Index: Integer): Boolean;
    function EndIndex: Integer;
  end;

function TSimpleRange.Contains(const Index: Integer): Boolean;
begin
  Result := (Index >= Start) and (Index < Start + Len);
end;

function TSimpleRange.EndIndex: Integer;
begin
  Result := Start + Len;
end;

function IsWS(const C: Char): Boolean;
begin
  Result := CharInSet(C, [#9, #10, #13, ' ']);
end;

function IsIdentStart(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z', 'A'..'Z', '_', '-']);
end;

function IsIdentChar(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']);
end;

function IsHexDigit(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['0'..'9', 'a'..'f', 'A'..'F']);
end;

{ TCodeSyntaxCSS }

constructor TCodeSyntaxAntimony.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  var KeyWord: TKeyWord;
  FKeyWords := TKeyWords.Create;

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['var', 'string', 'const', 'function',
    'or', 'and', 'xor', 'div', 'mod', 'is', 'type', 'unit', 'not', 'program',  'label',
    'is', 'const',  'to'];
  KeyWord.Color := $FF7FAAFF;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['begin', 'end', 'title', 'define_source', 'model', 'plot'];
  KeyWord.Color := $FFC22700;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  //KeyWord.Font.Size := 25;
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['at', 'identity', 'for', 'repeat', 'until', 'while'];
  KeyWord.Color := $FFFF9900;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FF7FAAFF;
  FStringKey.Font.Assign(FDefaultFont);

  FNumKey := TKeyWord.Create;
  FNumKey.Color := clayellow;//$FFFF7F85;
  FNumKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := $FF88E775;
  FCommentKey.Font.Assign(FDefaultFont);

  FDirectiveKey := TKeyWord.Create;
  FDirectiveKey.Color := $FF7FAAFF;
  FDirectiveKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxAntimony.Destroy;
begin
  FImportantKey.Free;
  FFunctionKey.Free;
  FHexColorKey.Free;
  FNumberKey.Free;
  FCommentKey.Free;
  FStringKey.Free;
  FPropertyKey.Free;
  FAtRuleKey.Free;
  FVarKey.Free;
  FAttrKey.Free;
  FPseudoKey.Free;
  FUrlKey.Free;
  FSelectorKey.Free;
  FClassKey.Free;
  FIdKey.Free;
  FCombinatorKey.Free;
  FBraceKey.Free;
  FCommaKey.Free;
  FMediaFeatureKey.Free;
  FAtRuleNameKey.Free;
  inherited;
end;


function TCodeSyntaxAntimony.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
const
  Seps =[' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '&'];
begin
  if FCached.TryGetValue(Index, Result) then
    Exit;
  try
    var Buf: string := '';
    var IsString: Boolean := False;
    var IsComment: Boolean := False;
    for var C := 0 to Line.Length do
    begin
      if Line.IsEmpty then
        Continue;
      if IsComment then
      begin
        if Line.Chars[C] = '}' then
        begin
          IsComment := False;
          if not Buf.IsEmpty then
          begin
            //if Buf.StartsWith('{$') then
            //  Result := Result + [
            //    TTextAttributedRangeData.Create(
             //   TTextRange.Create(C - Buf.Length, Buf.Length + 1),
            //    TTextAttribute.Create(FDirectiveKey.Font, FDirectiveKey.Color)
            //    )]
            //else
              Result := Result + [
                TTextAttributedRangeData.Create(
                TTextRange.Create(C - Buf.Length, Buf.Length + 1),
                TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
                )];
            Buf := '';
          end;
          Continue;
        end;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;
      if Line.Chars[C] = '#' then
      begin
        Result := Result + [
          TTextAttributedRangeData.Create(
          TTextRange.Create(C, Line.Length - C),
          TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
          )];
        Exit;
      end;
//      if Line.Chars[C] = '$' then
//      begin
//        Result := Result + [
//          TTextAttributedRangeData.Create(
//          TTextRange.Create(C, Line.Length - C),
//          TTextAttribute.Create(FDirectiveKey.Font, FDirectiveKey.Color)
//          )];
//        Exit;
//      end;
      if IsString then
      begin
        if Line.Chars[C] = '''' then
        begin
          IsString := False;
          if not Buf.IsEmpty then
          begin
            Result := Result + [
              TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length + 1),
              TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
              )];
            Buf := '';
          end;
          Continue;
        end;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;
      if C <> Line.Length then
      begin
        if (Line.Chars[C] = '/') and (Line.Chars[C + 1] = '/') then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
            TTextRange.Create(C, Line.Length - C),
            TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
            )];
          Exit;
        end;
        if Line.Chars[C] = '{' then
        begin
          IsComment := True;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;
        if Line.Chars[C] = '''' then
        begin
          IsString := True;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;
      end;

      if (C = Line.Length) or CharInSet(Line.Chars[C], Seps) then
      begin
        if not Buf.IsEmpty then
        begin
          var KeyWord: TKeyWord;
          var Num: Extended;
          if (TryStrToFloat(Buf, Num)) then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FNumKey.Font, FNumKey.Color)
              )];
          end
          else if Buf.StartsWith('$') then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FDirectiveKey.Font, FDirectiveKey.Color)
              )];
          end
          else if Buf.StartsWith('#') then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
              )];
          end
          else if FKeyWords.FindWord(Buf, KeyWord) then
          begin
            Result := Result + [TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length),
              TTextAttribute.Create(KeyWord.Font, KeyWord.Color)
              )];
          end;

          Buf := '';
        end;
      end
      else
        Buf := Buf + Line.Chars[C];
    end;
  finally
    FCached.AddOrSetValue(Index, Result);
  end;
end;

initialization
  TCodeSyntax.RegisterSyntax(['ant'], TCodeSyntaxAntimony);

end.
