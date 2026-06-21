unit uMathParser;

interface

uses
  System.SysUtils, System.Math;

type
  TMathParser = class
  private
    FPos: Integer;
    FText: string;
    FVariableX: Double;
    function Eat(CharToEat: Char): Boolean;
    function ParseExpression: Double;
    function ParseTerm: Double;
    function ParseFactor: Double;
  public
    function Evaluate(const AExpression: string; const XValue: Double = 0): Double;
  end;

implementation

function TMathParser.Evaluate(const AExpression: string; const XValue: Double): Double;
begin
  FText := AExpression;
  FPos := 1;
  FVariableX := XValue;
  Result := ParseExpression;
  if FPos <= Length(FText) then
    raise Exception.CreateFmt('Unexpected character at position %d', [FPos]);
end;

function TMathParser.Eat(CharToEat: Char): Boolean;
begin
  // Skip whitespace
  while (FPos <= Length(FText)) and (FText[FPos] = ' ') do Inc(FPos);

  if (FPos <= Length(FText)) and (FText[FPos] = CharToEat) then
  begin
    Inc(FPos);
    Exit(True);
  end;
  Result := False;
end;

function TMathParser.ParseExpression: Double;
begin
  Result := ParseTerm;
  while True do
  begin
    if Eat('+') then Result := Result + ParseTerm
    else if Eat('-') then Result := Result - ParseTerm
    else Break;
  end;
end;

function TMathParser.ParseTerm: Double;
begin
  Result := ParseFactor;
  while True do
  begin
    if Eat('*') then Result := Result * ParseFactor
    else if Eat('/') then Result := Result / ParseFactor
    else Break;
  end;
end;

function TMathParser.ParseFactor: Double;
var
  StartPos: Integer;
  Func: string;
begin
  // Unary operators
  if Eat('+') then Exit(ParseFactor);
  if Eat('-') then Exit(-ParseFactor);

  if Eat('(') then
  begin
    Result := ParseExpression;
    if not Eat(')') then raise Exception.Create('Missing closing parenthesis');
  end
  else
  begin
    StartPos := FPos;
    // Handle numbers, variable X, and functions
    if ((FText[FPos] >= '0') and (FText[FPos] <= '9')) or (FText[FPos] = '.') then
    begin
      while (FPos <= Length(FText)) and (((FText[FPos] >= '0') and (FText[FPos] <= '9')) or (FText[FPos] = '.')) do Inc(FPos);
      Result := StrToFloat(Copy(FText, StartPos, FPos - StartPos));
    end
    else if (FText[FPos] >= 'a') and (FText[FPos] <= 'z') then
    begin
      while (FPos <= Length(FText)) and (FText[FPos] >= 'a') and (FText[FPos] <= 'z') do Inc(FPos);
      Func := Copy(FText, StartPos, FPos - StartPos).ToLower;
      if Func = 'x' then Result := FVariableX
      else if Func = 'sin' then Result := Sin(ParseFactor)
      else if Func = 'cos' then Result := Cos(ParseFactor)
      else raise Exception.Create('Unknown function/variable: ' + Func);
    end
    else
      raise Exception.Create('Unexpected character: ' + FText[FPos]);
  end;

  // Power operator (highest precedence after factor identified)
  if Eat('^') then Result := Power(Result, ParseFactor);
end;

end.

