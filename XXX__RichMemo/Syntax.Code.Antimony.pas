unit Syntax.Code.Antimony;

interface

uses
  System.SysUtils, Syntax.Code, System.Generics.Collections, FMX.TextLayout,
  FMX.Graphics, System.UITypes, System.UIConsts, FMX.Dialogs, IOUtils;

type
  TCodeSyntaxPython = class(TCodeSyntax)
  private
    FKeyWords: TKeyWords;
    FStringKey, FNumKey, FCommentKey, FCallKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

{ TCodeSyntaxPython }

constructor TCodeSyntaxPython.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  var KeyWord: TKeyWord;
  FKeyWords := TKeyWords.Create;

  KeyWord := TKeyWord.Create;

  KeyWord.Word := ['model', 'species', 'compartment', 'begin', 'end', 'or', 'and', 'xor'];
  KeyWord.Color := $FF1E8BF0; //$FFF47067;  //red
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['==', '=', '**', 'false', 'true'];
  KeyWord.Color := $FF1E8BF0;   //blue
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FF87D0FF;
  FStringKey.Font.Assign(FDefaultFont);

  FNumKey := TKeyWord.Create;
  FNumKey.Color := $FF46A9FF;
  FNumKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := claLightgreen; //claGreenyellow; //claChartreuse;//claAquamarine;// $FF46A9FF;
  FCommentKey.Font.Assign(FDefaultFont);

  FCallKey := TKeyWord.Create;
  FCallKey.Color := $FFDCBDFB;
  FCallKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxPython.Destroy;
begin
  FStringKey.Free;
  FCallKey.Free;
  FCommentKey.Free;
  FNumKey.Free;
  FKeyWords.Free;
  inherited;
end;

function TCodeSyntaxPython.GetAttributesForLine(const Line: string;
  const Index: Integer): TArray<TTextAttributedRangeData>;
const
  Seps = [' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '&'];
begin
  //try
    if FCached.TryGetValue(Index, Result) then
      Exit;
    try
      var Buf:       string  := '';
      var IsString:  Boolean := False;
      var IsComment: Boolean := False;

      for var C := 0 to Line.Length - 1 do
      begin
        { Inside a single-quoted string }
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
                  TTextAttribute.Create(FStringKey.Font, FStringKey.Color))];
              Buf := '';
            end;
            Continue;
          end;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;

        { Open single-quoted string }
        if Line.Chars[C] = '''' then
        begin
          IsString := True;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;

        { # comment to end of line }
        if Line.Chars[C] = '#' then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
              TTextRange.Create(C, Line.Length - C),
              TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color))];
          Exit;
        end;

        { // comment to end of line — guarded against reading past end }
        if (Line.Chars[C] = '/') and
           (C + 1 < Line.Length) and
           (Line.Chars[C + 1] = '/') then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
              TTextRange.Create(C, Line.Length - C),
              TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color))];
          Exit;
        end;

        { Inside a double-quoted string (named IsComment historically) }
        if IsComment then
        begin
          if Line.Chars[C] = '"' then
          begin
            IsComment := False;
            if not Buf.IsEmpty then
            begin
              Result := Result + [
                TTextAttributedRangeData.Create(
                  TTextRange.Create(C - Buf.Length, Buf.Length + 1),
                  TTextAttribute.Create(FStringKey.Font, FStringKey.Color))];
              Buf := '';
            end;
            Continue;
          end;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;

        { Open double-quoted string }
        if Line.Chars[C] = '"' then
        begin
          IsComment := True;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;

        { Separator — flush accumulated identifier as call / number / keyword }
        if CharInSet(Line.Chars[C], Seps) then
        begin
          if not Buf.IsEmpty then
          begin
            var KeyWord: TKeyWord;
            var FL:      Extended;

            { Function-call detection: separator that opened the call is '(' }
            if Line.Chars[C] = '(' then
              Result := Result + [
                TTextAttributedRangeData.Create(
                  TTextRange.Create(C - Buf.Length, Buf.Length),
                  TTextAttribute.Create(FCallKey.Font, FCallKey.Color))];

            if TryStrToFloat(Buf.Replace('.', ','), FL) or Buf.StartsWith('$') then
              Result := Result + [
                TTextAttributedRangeData.Create(
                  TTextRange.Create(C - Buf.Length, Buf.Length),
                  TTextAttribute.Create(FNumKey.Font, FNumKey.Color))]
            else if Buf.StartsWith('#') then
              Result := Result + [
                TTextAttributedRangeData.Create(
                  TTextRange.Create(C - Buf.Length, Buf.Length),
                  TTextAttribute.Create(FStringKey.Font, FStringKey.Color))]
            else if FKeyWords.FindWord(Buf, KeyWord) then
              Result := Result + [
                TTextAttributedRangeData.Create(
                  TTextRange.Create(C - Buf.Length, Buf.Length),
                  TTextAttribute.Create(KeyWord.Font, KeyWord.Color))];

            Buf := '';
          end;
        end
        else
          Buf := Buf + Line.Chars[C];
      end;

      { End-of-line flush. The original loop ran one extra iteration with
        C = Line.Length to handle this; that's now done here, after the
        loop, so the indexed reads inside the loop are always in bounds.
        No '(' check needed -- there is no character past end-of-line. }
      if (not Buf.IsEmpty) and (not IsString) and (not IsComment) then
      begin
        var KeyWord: TKeyWord;
        var FL:      Extended;

        if TryStrToFloat(Buf.Replace('.', ','), FL) or Buf.StartsWith('$') then
          Result := Result + [
            TTextAttributedRangeData.Create(
              TTextRange.Create(Line.Length - Buf.Length, Buf.Length),
              TTextAttribute.Create(FNumKey.Font, FNumKey.Color))]
        else if Buf.StartsWith('#') then
          Result := Result + [
            TTextAttributedRangeData.Create(
              TTextRange.Create(Line.Length - Buf.Length, Buf.Length),
              TTextAttribute.Create(FStringKey.Font, FStringKey.Color))]
        else if FKeyWords.FindWord(Buf, KeyWord) then
          Result := Result + [
            TTextAttributedRangeData.Create(
              TTextRange.Create(Line.Length - Buf.Length, Buf.Length),
              TTextAttribute.Create(KeyWord.Font, KeyWord.Color))];
      end;
    finally
      FCached.AddOrSetValue(Index, Result);
    end;
  //except
  //  on E: Exception do
  //  TFile.AppendAllText(TPath.Combine(TPath.GetHomePath, 'iridium_highlighter.log'),
  //    Format('[%s] %s: %s'#10,
  //      [FormatDateTime('hh:nn:ss.zzz', Now), E.ClassName, E.Message]));
  //end;
end;

//function TCodeSyntaxPython.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
//const
//  Seps =[' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '&'];
//begin
//try
//  if FCached.TryGetValue(Index, Result) then
//    Exit;
//  try
//    var Buf: string := '';
//    var IsString: Boolean := False;
//    var IsComment: Boolean := False;
//    for var C := 0 to Line.Length - 1 do
//    begin
//      if Line.IsEmpty then
//        Continue;
//      if IsString then
//      begin
//        if Line.Chars[C] = '''' then
//        begin
//          IsString := False;
//          if not Buf.IsEmpty then
//          begin
//            Result := Result + [
//              TTextAttributedRangeData.Create(
//              TTextRange.Create(C - Buf.Length, Buf.Length + 1),
//              TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
//              )];
//            Buf := '';
//          end;
//          Continue;
//        end;
//        Buf := Buf + Line.Chars[C];
//        Continue;
//      end;
//      if Line.Chars[C] = '''' then
//      begin
//        IsString := True;
//        Buf := Buf + Line.Chars[C];
//        Continue;
//      end;
//      if Line.Chars[C] = '#' then
//      begin
//        Result := Result + [
//          TTextAttributedRangeData.Create(
//          TTextRange.Create(C, Line.Length - C),
//          TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
//          )];
//        Exit;
//      end;
//      if (Line.Chars[C] = '/') and (Line.Chars[C + 1] = '/') then
//        begin
//          Result := Result + [
//            TTextAttributedRangeData.Create(
//            TTextRange.Create(C, Line.Length - C),
//            TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
//            )];
//          Exit;
//        end;
//
//      if IsComment then
//      begin
//        if Line.Chars[C] = '"' then
//        begin
//          IsComment := False;
//          if not Buf.IsEmpty then
//          begin
//            Result := Result + [
//              TTextAttributedRangeData.Create(
//              TTextRange.Create(C - Buf.Length, Buf.Length + 1),
//              TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
//              )];
//            Buf := '';
//          end;
//          Continue;
//        end;
//        Buf := Buf + Line.Chars[C];
//        Continue;
//      end;
//      if Line.Chars[C] = '"' then
//      begin
//        IsComment := True;
//        Buf := Buf + Line.Chars[C];
//        Continue;
//      end;
//
//      if (C = Line.Length) or CharInSet(Line.Chars[C], Seps) then
//      begin
//        if not Buf.IsEmpty then
//        begin
//          var KeyWord: TKeyWord;
//          var FL: Extended;
//          if Line.Chars[C] = '(' then
//          begin
//            Result := Result + [
//              TTextAttributedRangeData.Create(
//              TTextRange.Create(C - Buf.Length, Buf.Length),
//              TTextAttribute.Create(FCallKey.Font, FCallKey.Color)
//              )];
//          end;
//          if (TryStrToFloat(Buf.Replace('.', ','), FL) or Buf.StartsWith('$')) then
//          begin
//            Result := Result + [TTextAttributedRangeData.Create(
//              TTextRange.Create(C - Buf.Length, Buf.Length),
//              TTextAttribute.Create(FNumKey.Font, FNumKey.Color)
//              )];
//          end
//          else if Buf.StartsWith('#') then
//          begin
//            Result := Result + [TTextAttributedRangeData.Create(
//              TTextRange.Create(C - Buf.Length, Buf.Length),
//              TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
//              )];
//          end
//          else if FKeyWords.FindWord(Buf, KeyWord) then
//          begin
//            Result := Result + [TTextAttributedRangeData.Create(
//              TTextRange.Create(C - Buf.Length, Buf.Length),
//              TTextAttribute.Create(KeyWord.Font, KeyWord.Color)
//              )];
//          end;
//
//          Buf := '';
//        end;
//      end
//      else
//        Buf := Buf + Line.Chars[C];
//    end;
//  finally
//    FCached.AddOrSetValue(Index, Result);
//  end;
//except
//  on E:Exception do
//     showmessage ('Error: ' + E.message);
//end;
//end;

initialization
  TCodeSyntax.RegisterSyntax(['ant', 'ant'], TCodeSyntaxPython);
end.


// Claude solution but didn't work

//function TCodeSyntaxPython.GetAttributesForLine(const Line: string;
//  const Index: Integer): TArray<TTextAttributedRangeData>;
//const
//  Seps = [' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '&'];
//begin

//  try
//    if FCached.TryGetValue(Index, Result) then
//      Exit;
//    try
//      var Buf:       string  := '';
//      var IsString:  Boolean := False;
//      var IsComment: Boolean := False;
//
//      for var C := 0 to Line.Length - 1 do
//      begin
//        { Inside a single-quoted string }
//        if IsString then
//        begin
//          if Line.Chars[C] = '''' then
//          begin
//            IsString := False;
//            if not Buf.IsEmpty then
//            begin
//              Result := Result + [
//                TTextAttributedRangeData.Create(
//                  TTextRange.Create(C - Buf.Length, Buf.Length + 1),
//                  TTextAttribute.Create(FStringKey.Font, FStringKey.Color))];
//              Buf := '';
//            end;
//            Continue;
//          end;
//          Buf := Buf + Line.Chars[C];
//          Continue;
//        end;
//
//        { Open single-quoted string }
//        if Line.Chars[C] = '''' then
//        begin
//          IsString := True;
//          Buf := Buf + Line.Chars[C];
//          Continue;
//        end;
//
//        { # comment to end of line }
//        if Line.Chars[C] = '#' then
//        begin
//          Result := Result + [
//            TTextAttributedRangeData.Create(
//              TTextRange.Create(C, Line.Length - C),
//              TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color))];
//          Exit;
//        end;
//
//        { // comment to end of line — guarded against reading past end }
//        if (Line.Chars[C] = '/') and
//           (C + 1 < Line.Length) and
//           (Line.Chars[C + 1] = '/') then
//        begin
//          Result := Result + [
//            TTextAttributedRangeData.Create(
//              TTextRange.Create(C, Line.Length - C),
//              TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color))];
//          Exit;
//        end;
//
//        { Inside a double-quoted string (named IsComment historically) }
//        if IsComment then
//        begin
//          if Line.Chars[C] = '"' then
//          begin
//            IsComment := False;
//            if not Buf.IsEmpty then
//            begin
//              Result := Result + [
//                TTextAttributedRangeData.Create(
//                  TTextRange.Create(C - Buf.Length, Buf.Length + 1),
//                  TTextAttribute.Create(FStringKey.Font, FStringKey.Color))];
//              Buf := '';
//            end;
//            Continue;
//          end;
//          Buf := Buf + Line.Chars[C];
//          Continue;
//        end;
//
//        { Open double-quoted string }
//        if Line.Chars[C] = '"' then
//        begin
//          IsComment := True;
//          Buf := Buf + Line.Chars[C];
//          Continue;
//        end;
//
//        { Separator — flush accumulated identifier as call / number / keyword }
//        if CharInSet(Line.Chars[C], Seps) then
//        begin
//          if not Buf.IsEmpty then
//          begin
//            var KeyWord: TKeyWord;
//            var FL:      Extended;
//
//            { Function-call detection: separator that opened the call is '(' }
//            if Line.Chars[C] = '(' then
//              Result := Result + [
//                TTextAttributedRangeData.Create(
//                  TTextRange.Create(C - Buf.Length, Buf.Length),
//                  TTextAttribute.Create(FCallKey.Font, FCallKey.Color))];
//
//            if TryStrToFloat(Buf.Replace('.', ','), FL) or Buf.StartsWith('$') then
//              Result := Result + [
//                TTextAttributedRangeData.Create(
//                  TTextRange.Create(C - Buf.Length, Buf.Length),
//                  TTextAttribute.Create(FNumKey.Font, FNumKey.Color))]
//            else if Buf.StartsWith('#') then
//              Result := Result + [
//                TTextAttributedRangeData.Create(
//                  TTextRange.Create(C - Buf.Length, Buf.Length),
//                  TTextAttribute.Create(FStringKey.Font, FStringKey.Color))]
//            else if FKeyWords.FindWord(Buf, KeyWord) then
//              Result := Result + [
//                TTextAttributedRangeData.Create(
//                  TTextRange.Create(C - Buf.Length, Buf.Length),
//                  TTextAttribute.Create(KeyWord.Font, KeyWord.Color))];
//
//            Buf := '';
//          end;
//        end
//        else
//          Buf := Buf + Line.Chars[C];
//      end;
//
//      { End-of-line flush. The original loop ran one extra iteration with
//        C = Line.Length to handle this; that's now done here, after the
//        loop, so the indexed reads inside the loop are always in bounds.
//        No '(' check needed -- there is no character past end-of-line. }
//      if (not Buf.IsEmpty) and (not IsString) and (not IsComment) then
//      begin
//        var KeyWord: TKeyWord;
//        var FL:      Extended;
//
//        if TryStrToFloat(Buf.Replace('.', ','), FL) or Buf.StartsWith('$') then
//          Result := Result + [
//            TTextAttributedRangeData.Create(
//              TTextRange.Create(Line.Length - Buf.Length, Buf.Length),
//              TTextAttribute.Create(FNumKey.Font, FNumKey.Color))]
//        else if Buf.StartsWith('#') then
//          Result := Result + [
//            TTextAttributedRangeData.Create(
//              TTextRange.Create(Line.Length - Buf.Length, Buf.Length),
//              TTextAttribute.Create(FStringKey.Font, FStringKey.Color))]
//        else if FKeyWords.FindWord(Buf, KeyWord) then
//          Result := Result + [
//            TTextAttributedRangeData.Create(
//              TTextRange.Create(Line.Length - Buf.Length, Buf.Length),
//              TTextAttribute.Create(KeyWord.Font, KeyWord.Color))];
//      end;
//    finally
//      FCached.AddOrSetValue(Index, Result);
//    end;
//  except
//    on E: Exception do
//      ShowMessage('Error: ' + E.Message);
//  end;
//end;

