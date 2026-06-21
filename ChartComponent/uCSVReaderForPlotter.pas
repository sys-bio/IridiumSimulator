unit uCSVReaderForPlotter;

{* @file    uCSVReader.pas
   @brief   Generate purpose CSV format reader class
   @author  Herbert M Sauro (2010) }

{-----------------------------------------------------------------

  This file is part of Snowflake.
  Please visit http://snowflake.org for more information.

  Copyright (C) 2011 Herbert M Sauro

  Licensed to the Apache Software Foundation (ASF) under one
  or more contributor license agreements.  See the NOTICE file
  distributed with this work for additional information
  regarding copyright ownership.  The ASF licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.

------------------------------------------------------------------}

// This library can read stndard CSV files with a simple extension to accomodate errors.
// Error are indicated in square brackets after the value. Three types of error can be indicated:
// Symmetric error, positive error, negative error  or assymetric postive and negative errors
//
// In addition the format also allows missing data to be specified using the character sequence NA

// Example:

//  "Temp", "Conc", "More"
//  -1, 1.2, -8
//  2, 3.4, NA
//  3, 5.6, 6
//  4 [+0.1,-0.1], 7.8 [+0.3,-0.2]
//  5 [-3]-8, 2
//  6,9,-1
//  7[0.5], 4[0.21], -7[0.1]

interface

Uses Classes, SysUtils, FMX.Dialogs, Math;

const
  EOF_CHAR = $7F;
  CR       = #13;
  LF       = #10;
  TAB      = #9;
  MAX_DIGIT_COUNT  = 2;

type
  ECSVException = class(Exception);

  TCSVErrorCode = (cOK, cFail);
  TCharCode   = (cLETTER, cDIGIT, cPOINT, cQuote, cSPECIAL, cMINUS, cPLUS, cComma, cTAB, cSemiColon, cSpace, cETX);
  TTokenCodes = (tEMPTY, tError, tIDENTIFIER, tNUMBER, tPLUS, tMINUS,
                 tTAB, tComma, tSemiColon, tOpenSquareBracket, tCloseSquareBracket, tSpace, tEOL, tEndOfData);

  TCSVErrorType = (etNone, etSymmetric, etASymmetric);

  TCSVValueStatus = (dsDefined, dsUnDefined);
  TASymmetricValue = record status : TCSVValueStatus; upper, lower : double; end;
  TSymmetricValue = record status : TCSVValueStatus; value : double; end;
  TCSVNumber = record status : TCSVValueStatus; number : double; end;
  TDatum = record
            value : TCSVNumber;
            errorType : TCSVErrorType;
            symmetricValue : TSymmetricValue;
            asymetricValue : TASymmetricValue;
  end;

  TDatumArray = array of TDatum;
  TDatumMatrix = array of TDatumArray;
  TCSVArray = class (TObject)
        values : TDatumMatrix;

        function getValue (r, c : integer) : TCSVNumber;
        function getSymmetricError (r, c : integer) : TSymmetricValue;
        function getASymmetricError (r, c : integer) : TASymmetricValue;
        function getErrorType (r, c : integer) : TCSVErrorType;

        constructor Create (RowCount, ColCount : integer);
        destructor Destroy; override;
  end;

  TCSV = class (TComponent)
           private
             FReader: TStreamReader;
             Fch     : char;
             Fchar_table  : array[0..255] of TCharCode;
             FToken, Fprevious_token  : TTokenCodes;
             Fname, FPrevious_name : string;
             Fvalue : double;
             FRowCount, FColCount : integer;
             FCollectData : boolean;
             FData : TCSVArray;
             FHeader : TStringList;
             FDecimalSeparator: Char;

             procedure LoadFromStream;
             procedure InitialiseScanner;
             procedure nextchar;
             procedure ungetToken;
             procedure getSpecial;
             procedure getToken (IgnoreBlanks : boolean);
             procedure eatWhiteSpace;
             procedure eatSpaces;
             procedure getWord;
             procedure GetHeaderField;
             procedure getHeaderName;
             procedure getNumber;
             procedure getHeaderRow;
             procedure getDataRow;
             function  getRowCount : integer;
             function  getColCount : integer;
             function  getData (r, c : integer) : TCSVNumber;
             function  getDataStr (r, c : integer) : string;
             function  getSymmetricValue (r, c : integer) : TSymmetricValue;
             function  getASymmetricValue (r, c : integer) : TASymmetricValue;
             function  getErrorType (r, c : integer) : TCSVErrorType;
             function  getErrorStr (r, c : integer) : string;
             function  getHeader (c : integer) : string;
             function  getNumberOfHeaders : integer;
             procedure getDatum (var value : TCSVNumber; var symmetricValue : TSymmetricValue; var asymetricValues : TASymmetricValue);
             procedure readErrorData (var symmetricValue : TSymmetricValue; var asymmetricValue : TASymmetricValue);
             function  parseErrorValue : double;

           public
             constructor create (AOwner : TComponent); override;
             destructor  destroy; override;

             function ReadCSV (filename : string) : TCSVErrorCode;
             function GetColumnIndex (HeaderString : String) : Integer;
             property rows : integer read getRowCount;
             property cols : integer read getColCount;
             property data[r, c : integer] : TCSVNumber  read getData; default;
             property dataStr[r, c : integer] : string read getDataStr;
             property symmetricError[r, c : integer] : TSymmetricValue read getSymmetricValue;
             property asymmetricError[r, c : integer] : TASymmetricValue read getASymmetricValue;
             property errorType[r, c : integer] : TCSVErrorType read getErrorType;
             property errorStr[r, c : integer] : string read getErrorStr;
             property header[c : integer] : string read getHeader;
             property numberOfHeaders : integer read getNumberOfHeaders;
         end;

implementation


function convertFloatToStr (value : double) : string;
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('en-US');
  Result := FloatToStr(value, FS);
end;


// ---------------------------------------------------------------


constructor TCSVArray.Create (RowCount, ColCount : integer);
begin
  inherited Create;
  setLength (values, RowCount, ColCount);
end;


destructor TCSVArray.Destroy;
begin
  setLength (values, 0, 0);
  inherited;
end;


function TCSVArray.getValue (r, c : integer) : TCSVNumber;
begin
  result := values[r, c].value;
end;

function TCSVArray.getSymmetricError (r, c : integer) : TSymmetricValue;
begin
  result := values[r, c].symmetricValue;
end;

function TCSVArray.getASymmetricError (r, c : integer) : TASymmetricValue;
begin
  result := values[r, c].asymetricValue;
end;

function TCSVArray.getErrorType (r, c : integer) : TCSVErrorType;
begin
  result := values[r,c].errorType;
                                  end;

// ---------------------------------------------------------------


constructor TCSV.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  FHeader := TStringList.Create;
end;


destructor TCSV.destroy;
begin
  FHeader.free;
  FData.Free;
  inherited Destroy;
end;


function TCSV.ReadCSV (filename : string) : TCSVErrorCode;
var FStream : TFileStream;
begin
  FStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  FReader := TStreamReader.Create(FStream, TEncoding.UTF8, True);
  try
    InitialiseScanner;
    FRowCount := 0; FColCount := 0; FCollectData := false;
    LoadFromStream;
  finally
    FStream.free;
  end;

  { OK now we know the size, lets do it again but this time collect the data }
  FData := TCSVArray.Create (FRowCount, FColCount);
  FRowCount := 0; FColCount := 0; FCollectData := true;
  FStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  FReader := TStreamReader.Create(FStream, TEncoding.UTF8, True);
  try
    LoadFromStream;
  finally
    FStream.free;
  end;
  result := cOK;
end;


function TCSV.GetColumnIndex (HeaderString : String) : Integer;
var i  : Integer;
begin
  for i := 0 to numberOfHeaders - 1 do
      if header[i].ToLower = HeaderString.ToLower then
         exit (i);
  exit (-1);
end;


procedure TCSV.InitialiseScanner;
var ch : char;
begin
  FDecimalSeparator := FormatSettings.DecimalSeparator;

  for ch := chr(0) to chr(255) do Fchar_table[ord(ch)] := cSPECIAL;
  for ch := '0' to '9' do Fchar_table[ord(ch)] := cDIGIT;
  for ch := 'A' to 'Z' do Fchar_table[ord(ch)] := cLETTER;
  for ch := 'a' to 'z' do Fchar_table[ord(ch)] := cLETTER;
  Fchar_table[ord('_')] := cLETTER;   // <-- add this line
  Fchar_table[ord('.')] := cPOINT;
  Fchar_table[ord('"')] := cQuote;
  Fchar_table[ord('-')] := cMINUS;
  Fchar_table[ord('+')] := cPLUS;
  Fchar_table[ord(' ')] := cSpace;
  Fchar_table[ord(',')] := cComma;
  Fchar_table[ord(';')] := cSemiColon;
  Fchar_table[ord(TAB)] := cTAB;
  Fchar_table[EOF_CHAR] := cETX;
end;


function TCSV.getRowCount : integer;
begin
  result := FRowCount;
end;


function TCSV.getColCount : integer;
begin
  result := FColCount;
end;


function TCSV.getData (r, c : integer) : TCSVNumber;
begin
  result := FData.getValue (r, c);
end;


function TCSV.getDataStr (r, c : integer) : string;
begin
  result := convertFloatToStr (FData.getValue(r, c).number);
end;


function TCSV.getSymmetricValue (r, c : integer) : TSymmetricValue;
begin
  result := FData.getSymmetricError (r, c);
end;


function TCSV.getASymmetricValue (r, c : integer) : TASymmetricValue;
begin
  result := FData.getASymmetricError (r, c);
end;


function TCSV.getErrorType (r, c : integer) : TCSVErrorType;
begin
  result := FData.getErrorType (r, c);
end;


function TCSV.getErrorStr (r, c : integer) : string;
begin
  result := convertFloatToStr (FData.getSymmetricError(r, c).value);
end;


function TCSV.getHeader (c : integer) : string;
begin
  { StringList is indexed from 0 but user indexes from 1 }
  result := FHeader[c];
end;


{ Get the number of header strings reasd in }
function TCSV.getNumberOfHeaders : integer;
begin
  result := FHeader.count;
end;


{ Update ch to next character in input stream }
procedure TCSV.nextchar;
var
  i: Integer;
begin
  repeat
    i := FReader.Read;
    if i = -1 then
    begin
      Fch := Char(EOF_CHAR);
      exit;
    end;
    Fch := Char(i);
  until Fch <> LF;   // skip bare LF characters
end;


procedure TCSV.getNumber;
var single_digit, scale, evalue : Extended; exponent_sign, digit_sign, digit_count : integer;
begin
  Fvalue := 0.0; evalue := 0.0; exponent_sign := 1; Fname := Fch;
  { check for decimal point just in case user has typed some thing like .5 }
  digit_sign := 1;
  if Fch = '-' then
     begin
     digit_sign := -1; nextchar;
     end;
  if Fch = '+' then
     begin
     digit_sign := 1; nextchar;
     end;

  if Fch <> FDecimalSeparator then
     repeat
       single_digit := ord (Fch) - ord ('0');
       Fvalue := 10*Fvalue + single_digit;
       nextchar; Fname := Fname + Fch;
     until Fchar_table[ord(Fch)] <> cDIGIT;

  scale := 1;
  if Fch = FDecimalSeparator then
     begin
     { start collecting fractional part }
     nextchar; Fname := Fname + Fch;
     if Fchar_table[ord(Fch)] <> cDIGIT then
        raise ECSVException.Create ('Syntax error during reading of data from file: expecting number after decimal point');

     while Fchar_table[ord(Fch)] = cDIGIT do
       begin
       scale := scale * 0.1;
       single_digit := ord (Fch) - ord ('0');
       Fvalue := Fvalue + (single_digit * scale);
       nextchar; Fname := Fname + Fch;
       end;
     end;
  { next check for scientific notation }
  if (Fch = 'e') or (Fch = 'E') then
     begin
     nextchar; Fname := Fname + Fch;
     if (Fch = '-') or (Fch = '+') then
        begin
        if Fch = '-' then exponent_sign := -1;
        nextchar; Fname := Fname + Fch;
        end;
     { accumulate exponent, check that firat ch is a digit }
     if Fchar_table[ord(Fch)] <> cDIGIT then
        raise ECSVException.Create ('Syntax error during reading of data from file: number expected in exponent');

     digit_count := 0;
     repeat
       inc (digit_count);
       single_digit := ord (Fch) - ord ('0');
       evalue := 10*evalue + single_digit;
       nextchar; Fname := Fname + Fch;
     until (Fchar_table[ord(Fch)] <> cDIGIT) or (digit_count > MAX_DIGIT_COUNT);

     if digit_count > MAX_DIGIT_COUNT then
        raise ECSVException.Create ('Syntax error during reading of data from file: too many digits in exponent');

     evalue := evalue * exponent_sign;
     evalue := power (10.0, evalue);
     Fvalue := Fvalue * evalue;
     end;
  Fvalue := Fvalue * digit_sign;  { take care of any unary minus }
  Ftoken := tNUMBER;
  FName := Copy (FName, 1, Length (FName) - 1);
  //dec (FName[0]);  { Stip off last ch }
end;


// Scan in an identifier token of the form "xyz"
procedure TCSV.getWord;
begin
  Fname := ''; nextChar; // Jump over first "
  while Fchar_table [ord(Fch)] <> cQuote do
        begin
          Fname := Fname + Fch;
          nextChar;
        end;
  nextchar; // absorb the "
  FToken := tIdentifier;
end;


procedure TCSV.getHeaderName;
begin
  Fname := '';
  // Read until delimiter or end of line
  while not (Fch in [',', ';', CR, LF, Char(EOF_CHAR)]) do
  begin
    Fname := Fname + Fch;
    nextChar;
  end;
  Fname := Trim(Fname);  // strip leading/trailing spaces
  FToken := tIdentifier;
end;


// Get special tokens
procedure TCSV.getSpecial;
begin
  case Fch of
     TAB : Ftoken := tTAB;
     ',' : Ftoken := tComma;
     ' ' : Ftoken := tSpace;
     ';' : Ftoken := tSemiColon;
     '['  : Ftoken := tOpenSquareBracket;
     ']'  : Ftoken := tCloseSquareBracket;
     CR   : Ftoken := tEOL; // End of line detected
  else
     raise Exception.Create ('Unrecognized token detected in csv file: <' + Fch + ', ' + inttostr (Ord(FCh)) + '> at line: ' + inttostr (FRowCount));
  end;
  nextchar;
end;


{ Get the next token }
procedure TCSV.getToken (IgnoreBlanks : boolean);
  var i : integer;
begin
  { check if a token has been pushed back into the token stream, if so use it first }
  if Fprevious_token <> tEMPTY then
     begin
     Ftoken := Fprevious_token;
     Fname  := Fprevious_name;
     Fprevious_token := tEMPTY;
     exit;
     end;

  { skip any blanks }
  {if IgnoreBlanks then while Fch = ' ' do nextchar; }

  case Fchar_table[ord(Fch)] of
     cQuote  : GetWord;
     cLETTER : begin
            Fname := '';
            while Fchar_table[ord(Fch)] in [cLETTER, cDIGIT] do
            begin
              Fname := Fname + Fch;
              nextChar;
            end;
            if SameText(Fname, 'NA') then
              FToken := tNumber
            else
              FToken := tIdentifier;   { stray identifier in data row }
          end;
     cPOINT  : GetNumber; { just in case user has entered .5 etc }
     cDIGIT  : GetNumber;
     cMINUS  : GetNumber;
     cPLUS   : GetNumber;
     cSpace  : begin Ftoken := tSpace; nextchar; end;
     cETX    : Ftoken := tEndOfData;
   else
     getSpecial;
  end;
end;


{ push token back into token stream }
procedure TCSV.ungetToken;
begin
  Fprevious_token := Ftoken;
  Fprevious_name  := Fname;
end;

procedure TCSV.getHeaderField;
{ Reads one header field directly from the character stream.
  Rules:
    - Leading whitespace is skipped.
    - If the field starts with ", read until the matching closing "
      (treating "" as an escaped quote per RFC 4180). Then skip any
      trailing whitespace up to the next delimiter.
    - Otherwise read until comma / semicolon / CR / LF / EOF, then
      trim trailing whitespace.
  Does NOT consume the trailing delimiter -- the caller decides what
  to do with it. Stores the result in Fname. }
var
  ch: Char;
begin
  Fname := '';

  while Fch = ' ' do nextChar;

  if Fch = '"' then
  begin
    nextChar;  { consume opening quote }
    while True do
    begin
      if Fch = Char(EOF_CHAR) then Break;
      if Fch = '"' then
      begin
        nextChar;
        if Fch = '"' then           { escaped "" inside quoted field }
        begin
          Fname := Fname + '"';
          nextChar;
        end
        else
          Break;                    { real closing quote }
      end
      else
      begin
        Fname := Fname + Fch;
        nextChar;
      end;
    end;
    { Skip trailing whitespace up to the delimiter }
    while Fch = ' ' do nextChar;
  end
  else
  begin
    while not (Fch in [',', ';', CR, LF, Char(EOF_CHAR)]) do
    begin
      Fname := Fname + Fch;
      nextChar;
    end;
    Fname := TrimRight(Fname);
  end;
end;


procedure TCSV.GetHeaderRow;
begin
  { Read first header directly from the stream. The initial getToken
    call in LoadFromStream already advanced Fch past any BOM, so we
    just start reading. }
  getHeaderField;
  if Fname <> '' then
  begin
    FHeader.Add(Fname);
    inc(FColCount);
  end;

  while Fch in [',', ';'] do
  begin
    nextChar;
    getHeaderField;
    if Fname <> '' then
    begin
      FHeader.Add(Fname);
      inc(FColCount);
    end;
  end;

  while Fch in [CR, LF] do nextChar;
  getToken(true);   { re-sync the token stream for the data-row scanner }
end;


function TCSV.parseErrorValue : double;
begin
  if Ftoken <> tNumber then
     raise ECSVException.Create ('Syntax error during reading of error value. At row: ' + inttostr (FRowCount));

  result := FValue;
end;

procedure TCSV.eatWhiteSpace;
begin
  while (Ftoken = tSpace) or (Ftoken = tTAB) do
        getToken (true);
end;


procedure TCSV.eatSpaces;
begin
  while (Ftoken = tSpace) do
        getToken (true);
end;


procedure TCSV.readErrorData (var symmetricValue : TSymmetricValue; var asymmetricValue : TASymmetricValue);
begin
  symmetricValue.status := dsUnDefined;
  asymmetricValue.status := dsUnDefined;

  // Ignore any spaces
  eatWhiteSpace;
  symmetricValue.status := dsDefined;
  symmetricValue.value := parseErrorValue;
  asymmetricValue.status := dsUnDefined;
  getToken (true);
  eatWhiteSpace;
  if (Ftoken = tComma) then
     begin
     // This means we have an upper and lower set of error values
     eatSpaces;
     getToken (true);
     asymmetricValue.upper := parseErrorValue;
     asymmetricValue.lower := symmetricValue.value;
     asymmetricValue.status := dsDefined;
     symmetricValue.status := dsUnDefined;
     end
  else
     begin
     if Ftoken <> tCloseSquareBracket then
        raise ECSVException.Create ('Expecting closing square bracket in error term. At Row: ' + inttostr (FRowCount));
     getToken (true);
     exit;
     end;
  getToken (true);
  if Ftoken <> tCloseSquareBracket then
     raise ECSVException.Create ('Expecting closing square bracket in error term. At Row: ' + inttostr (FRowCount));
  getToken (true);
 end;


procedure TCSV.getDatum(var value: TCSVNumber; var symmetricValue: TSymmetricValue;
                        var asymetricValues: TASymmetricValue);
begin
  symmetricValue.value := 0.0;
  symmetricValue.status := dsUnDefined;
  asymetricValues.upper := 0.0;
  asymetricValues.lower := 0.0;
  asymetricValues.status := dsUnDefined;

  if (Ftoken = tComma) or (Ftoken = tEOL) or (Ftoken = tEndOfData) then
    begin
    // Empty field — do NOT consume the token, let the caller handle it
    value.number := 0;
    value.status := dsUnDefined;
    exit;
    end;

  if Ftoken <> tNumber then
    begin
    value.number := 0;
    value.status := dsUnDefined;
    getToken(true);
    exit;
    end;

  value.number := FValue;
  value.status := TCSVValueStatus.dsDefined;
  getToken(true);
  eatWhiteSpace;
  if Ftoken = tOpenSquareBracket then
    begin
    getToken(true);
    readErrorData(symmetricValue, asymetricValues);
    end;
end;


procedure TCSV.getDataRow;
var col : integer; Sepch : char;
    value : TCSVNumber; symmetricValue : TSymmetricValue; asymmetricValue : TASymmetricValue;
begin
  while Ftoken = tSpace do
        GetToken (true); { ignore any leading blanks }
  inc (FRowCount); Col := 1;
  GetDatum (value, symmetricValue, asymmetricValue);
  { Store data }
  if FCollectData then
     begin
     FData.values[FRowCount-1, Col-1].value.number := Value.number;
     FData.values[FRowCount-1, Col-1].value.status := Value.status;

     FData.values[FRowCount-1, Col-1].symmetricValue := symmetricValue;
     FData.values[FRowCount-1, Col-1].asymetricValue := asymmetricValue;

     if symmetricValue.status = dsDefined then
        FData.values[FRowCount-1, Col-1].errorType := etSymmetric;
     if asymmetricValue.status = dsDefined then
        FData.values[FRowCount-1, Col-1].errorType := etASymmetric;
    end;

  while Ftoken <> tEOL do
        begin
        if (Ftoken = tComma) or (Ftoken = tTAB) then
           getToken (true);
        eatWhiteSpace;
        if Ftoken <> tEOL then
           begin
           GetDatum (value, symmetricValue, asymmetricValue);
           if (Ftoken = tEndOfData) and (value.status = dsUnDefined) then
             break;
           // Store data if asked to
           inc (col); { count the columns to make sure there aren't too many }
           if (FColCount > 0) and (col > FColCount) then
              begin
              raise ECSVException.Create ('Syntax error during reading of data from file: Too many datum points on line: ' + inttostr (FRowCount));
              end;
           if FCollectData then
              begin
              FData.values[FRowCount-1, Col-1].value := Value;
              FData.values[FRowCount-1, Col-1].symmetricValue := symmetricValue;
              FData.values[FRowCount-1, Col-1].asymetricValue := asymmetricValue;

              if symmetricValue.status = dsDefined then
                 FData.values[FRowCount-1, Col-1].errorType := etSymmetric;
              if asymmetricValue.status = dsDefined then
                 FData.values[FRowCount-1, Col-1].errorType := etASymmetric;
              end;

           // Now safe to stop if we've run out of input.
           if Ftoken = tEndOfData then
              break;
           end;
        end;
  // If no headers were present then set FColCount here
  // according to number of data points read
  if FColCount = 0 then FColCount := Col;
end;


procedure TCSV.LoadFromStream;
var i : integer;
begin
  if FCollectData then
    FHeader := TStringList.Create;

  nextChar;

  { Skip a possible UTF-8 BOM if TStreamReader didn't strip it. }
  if Fch = #$FEFF then nextChar;

  { Decide header vs headerless based on the first real character,
    independent of token classification. }
  while Fch = ' ' do nextChar;
  if Fchar_table[ord(Fch)] in [cLETTER, cQuote] then
    getHeaderRow
  else
    getToken(true);   { headerless: prime the token stream for data }

  while (Ftoken <> tEndOfData) do
  begin
    while Ftoken = tEOL do
      getToken(true);
    if Ftoken <> tEndOfData then
      getDataRow;
  end;

  if (FHeader.Count = 0) and (FColCount > 0) then
  begin
    FHeader.Add('X');
    for i := 1 to FColCount - 1 do
      FHeader.Add('Y' + IntToStr(i));
  end;
end;


end.
