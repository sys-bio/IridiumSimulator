unit uSBWScanner;

// ------------------------------------------------------------------------
// TSBWScanner - general purpose tokeniser
// Written 11 Feb 98 (HMS)
// Version 1.0
// Version 1.1
// Now integrates with console component to receive input from terminal 
// ------------------------------------------------------------------------


// This is Jarnac, an open source biochemical network simulator
// Developed under Delphi for Windows and Linux platforms.
// It is distributed under LGPL

// Copyright (C) 1999-2000 Herbert M Sauro

// This application/server is free software; you can redistribute it
// and/or  modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.

// This application/server is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.

// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

// Author Contact Information:

// email: hsauro@cds.caltech.edu or hsauro@fssc.demon.co.uk
// Snail mail:
// Home: 1115 E Cordova St, Apt 108
//       Pasadena
//       CA, 91106
//       USA
// or
//
// Work: Control and Dynamical Systems, MC 107-81
//       Steele Building
//       Wilson Ave
//       California Institute of Technlogy
//       Pasadena, CA, 91125


{ Console Usage:

   p := TSBWScanner.Create;
   p.setConsole (Console1);
   p.NextToken;   // Fetch first token
   if p.Token = tPlus then ......

   p.free;

   If an identifier or a string is scanned then the value can be found in
   p.TokenString.

   If an integer or float is scanned then the value can be found in
   p.TokenInteger or p.TokenFloat respectively

   Identifiers allowed to have underscores, eg Start_Now

   Blanks are ignored and comments can be embeded in the test and are of the form:

   // a comment
   #  .....;
   /* another comment
      another line in the comment
   */

   LookAhead:

   You can look ahead in the stream for the next token and put the token
   back into the stream using UnGetToken
}

interface

uses Windows, SysUtils, Classes, math;

const
  EOFCHAR = $7F;        // Deemed end of string marker, used internally
  MAX_DIGIT_COUNT  = 3; // Max # of digits in exponent of floating point number
  KEYLISTLEN = 12;      // Number of string lists stored in FKeyWordList
  LF = #10;
  CR = #13;

type
  { ********************* Lexical scanner types etc *********************** }
  ETokenStackError = class (Exception);

  TCharCode  = (cLETTER, cDIGIT, cPOINT, cDoubleQuote, cUNDERSCORE, cSPECIAL, cETX);
  TTokenCode = (tEMPTY, tIdentifier, tFloat, tInteger, tComplexToken, tPlus, tMinus, tMult,
                tPower, tRParen, tLParen, tLBracket, tRBracket, tLcBracket,
                tRcBracket, tSlash, tDollar, tLineFeed,
                tError, tEquals, tEquivalence, tUnknown, tApostrophy, tSemicolon, tColon,
                tComma, tArrow, tEqualsArrow, tHashComment, tStartComment, tStartCComment,
                tEndComment, tEOL, tString, tAndToken, tOrToken, tNotToken, tXorToken, tEnd,
                tEndToken, tIfToken, tThenToken, tElseToken, tEscapeDoubleQuote,
                tdefnToken, tUnaryMinus, tLessThan, tLessThanOrEqual, tQuestion,
                tMoreThan, tMoreThanOrEqual, tNotEqual, tEndofStream,

                tReadFile, tReturn, tQuitToken, tBeginToken,
                tVersion, tWriteFile, tOpenFile, tCloseFile, tForToken,
                tDoToken, tToToken, tDownToToken, tWhileToken, tNewLine,
                tAddHelpToken, tWriteLnFile, tWriteMath, tHaltToken,
                tWriteRawMatrix, tCommentToken,
                tVarToken, tExtToken, tDivToken, tFullStop, tFullFullStop,
                tRepeatToken, tWriteToken, tReturnToken, tNotesToken,
                tModuleToken, tUntilToken, tOfToken, tPrintToken, tReadToken,
                tPrintlnToken, tImportToken, tFuncToken, tTypeToken, tDeclareToken,
                tDefaultModelToken, tGraphToken, tGraphSurfaceToken, tOdetoken, tKineticLawToken,
                tFromToken, tTitleToken, tDateToken, tAuthorToken,
                tTryToken, tExceptToken, tFinallyToken, tRaiseToken, tBreakToken,
                tExternToken, tInToken, tVolToken, tAlgToken);

  TTokenSet = set of TTokenCode;

  TInputDevice = (tStreamDevice, tConsoleDevice);
  EScannerError = class (Exception);
  TStreamOwner = (oExternal, oInternal);

  TTokenElement = record
                    FToken        : TTokenCode;
                    FTokenString  : string;
                    FTokenFloat   : double;
                    FTokenInteger : Integer;
                  end;

  TBuffer = array[0..255] of char;
  TSBWScanner = class (TObject)
             private
               FStreams : array[1..24] of TStream;
               StreamStackPtr : integer;

               Queue : array[0..255] of TTokenElement;
               QueueEnd : integer;
               IsPoppedToken : Boolean;

               FToken,        FPreviousToken : TTokenCode;
               FTokenFloat,   FPreviousFloat : Extended;
               FTokenInteger, FPreviousInteger : integer;
               FTokenString,  FPreviousTokenString : string;

               UnGetCharAvailable : Boolean;
               UnGetCharacter : char;
               Fch : char;
               FCharTable  : array[0..255] of TcharCode;  // char conversion table

               FKeyWordList : array[1..KEYLISTLEN] of TStringList;
               FBuiltInRoutines : TStringList;

               FCollectStream : Boolean;
               FStreamString : string;

               yylineno : integer;
               Buffer : TBuffer;        // Type ahead buffer
               BufferPtr : integer;     // Current position in type-ahead buffer
               BufferLength : integer;  // Length of buffer once read in
               CurrentLine : integer;
               FInputDevice : TInputDevice;
               FStreamOwner : TStreamOwner;

               procedure InitialiseScanner;
               function  LookAhead_ch : char;
               function  IsDoubleQuote (ch : char) : Boolean;
               procedure skipBlanks;
               procedure getWord;
               procedure getString;
               procedure getNumber;
               procedure getSpecial;
               procedure AddKeyWords;
               procedure AddBuiltInRoutines;
               procedure DestroyBuiltInRoutines;
               function  IsBuiltInRoutine (FTokenString : string; var Token : TTokenCode) : Boolean;

               procedure InternalNextToken;

               function  getChar : Char;
               procedure UnGetChar (c : Char);
               //nction  readLine : integer;
               procedure newLine;
             public
               IgnoreNewLines : boolean;
               JustComeOffStream : boolean;
               yyStream : TStream;
               QuiTSBWScanner : boolean;
               FromQueue : Boolean; // If false, scanner reads from stream else from token queue
               constructor create;
               destructor  destroy; override;

               procedure PushStream (Stream : TStream);
               function  PopStream : TStream;

               function  IsQueueEmpty : Boolean;
               procedure GetTokenFromQueue;
               procedure AddTokenToQueue;

               procedure EatNewLine;
               function  CurrentChar : char;
               function  NextChar : char;
               procedure NextToken;
               procedure UnGetToken;
               procedure Get_TextBlock;
               procedure RestoreConsole;
               procedure SetStream (s : TStream);
               function  ReadUpTo (ch : char) : string;
               function  TokenToString : string; overload;
               function  TokenToString (T : TTokenCode) : string; overload;
               function  TokenLiteral : string;
               function  getCurrentText : string;
               function  LineNo : integer;

               function  IsKeyWord (FTokenString : string; var Token : TTokenCode) : boolean;
               //procedure SetConsole (c : TMConsole); overload;
               procedure NewStream (Filename : string);
               procedure StarTSBWScanner;
               procedure CollectStream;
               procedure StopCollecting;
               property  Token : TTokenCode read FToken write FToken;
               property  TokenString : string read FTokenString write FTokenString;
               property  TokenFloat : Extended read FTokenFloat;
               property  TokenInteger : integer read FTokenInteger;
               property  StreamString : string read fStreamString;
            end;


//var yyConsole : TMConsole;

implementation

// Clear any pending exception bits in the status word
procedure ClearExceptions; assembler;
asm
  fclex
end;


function MyPower(Base, Exponent: Extended): Extended;
begin
  ClearExceptions;
  if Exponent = 0.0 then
    Result := 1.0               { n**0 = 1 }
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0               { 0**n = 0, n > 0 }
  else if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := IntPower(Base, Integer(Trunc(Exponent)))
  else
    Result := Exp(Exponent * Ln(Base))
end;


{ ------------------------------------------------------------------------- }
{ Token Queue used to perform token look-ahead                             }
{ ------------------------------------------------------------------------- }


function TSBWScanner.IsQueueEmpty : Boolean;
begin
  if QueueEnd = -1 then
     Result := True
  else Result := False;
end;


procedure TSBWScanner.GetTokenFromQueue;
var i : integer;
begin
  try
  if QueueEnd > -1 then
     begin
     FToken := Queue[0].FToken;
     FTokenString  := Queue[0].FTokenString;
     FTokenFloat   := Queue[0].FTokenFloat;
     FTokenInteger := Queue[0].FTokenInteger;

     // Inefficient I know, but easy to program
     // Move all the queue elements down one slot
     for i := 1 to QueueEnd do
         Queue[i-1] := Queue[i];

     Dec (QueueEnd);
     end;
 except
    on E: Exception do
    raise ETokenStackError.Create (E.message);
  end;
end;


procedure TSBWScanner.AddTokenToQueue;
begin
  try
  Inc (QueueEnd);
  Queue[QueueEnd].FToken := FToken;
  Queue[QueueEnd].FTokenString := FTokenString;
  Queue[QueueEnd].FTokenFloat := FTokenFloat;
  Queue[QueueEnd].FTokenInteger := FTokenInteger;
 except
    on E: Exception do
    raise ETokenStackError.Create (E.message);
  end;
end;


{ ------------------------------------------------------------------------- }
{ -------              SCANNER ROUTINES FOLLOW                              }
{ ------------------------------------------------------------------------- }


constructor TSBWScanner.Create;
var i : integer;
begin
  inherited Create;
  FStreamOwner := oInternal;
  InitialiseScanner;
  for i := 1 to KEYLISTLEN do
      FKeyWordList[i] := TStringList.Create;
  AddKeyWords;
  AddBuiltInRoutines;
  QueueEnd := -1; // For the token look-ahead
  FromQueue := True; // Default to is get tokens from queue first then stream

  IgnoreNewLines := False;
  JustComeOffStream := False;
  QuiTSBWScanner := False;
  StreamStackPtr := 0;
  BufferPtr := 0;
  UnGetCharAvailable := False;
  UnGetCharacter := #0;
  FCollectStream := False;
  FStreamString := '';
  yyStream := nil;
end;


destructor TSBWScanner.Destroy;
var i : integer;
begin
  for i := 1 to KEYLISTLEN do
      FKeyWordList[i].free;
  DestroyBuiltInRoutines;
  yyStream.Free;
  inherited Destroy;
end;


// Stream stack allows us to switch stream and return at a later time
// to the previous stream
procedure TSBWScanner.PushStream (Stream : TStream);
begin
  inc (StreamStackPtr);
  FStreams[StreamStackPtr] := Stream;
end;


function TSBWScanner.PopStream : TStream;
begin
  if StreamStackPtr = 0 then
     result := Nil
  else
     begin
     result := FStreams[StreamStackPtr];
     dec (StreamStackPtr);
     end;
end;


// Switches the scanner to a new input stream attached to a file, when
// the scanner comes to the end of this new stream it then reverts
// to the stream it originally switched from.
procedure TSBWScanner.newStream (Filename : string);
begin
  if yyStream = Nil then
     yyStream := TFileStream.Create (filename, fmOpenRead)
  else
     begin
     // Stuff whatever is left in the read buffer back into the current stream
     yyStream.Position := yyStream.Position - (BufferLength - BufferPtr);
     PushStream (yyStream);  // Save the old stream
     try
       yyStream := TFileStream.Create (filename, fmOpenReadWrite);
     except
       on Exception do
          begin
          //yyConsole.WriteString ('Unable to open the file ' + '[' + FileName + ']'+ ' for reading' + #13#10);
          yyStream := PopStream;
          end;
     end;
     end;
  FInputDevice := tStreamDevice;
  StarTSBWScanner;
  NextToken;
end;


// Attach the scanner to a Memo based scanner
//procedure TSBWScanner.setConsole (c : TMConsole);
//begin
//  yyConsole := c;
//  FInputDevice := tConsoleDevice;
//end;


// Initialise the character translation table
procedure TSBWScanner.InitialiseScanner;
var ch : char;
begin
  for ch := chr(0) to chr(255) do FCharTable[ord(ch)] := cSPECIAL;
  for ch := '0' to '9' do FCharTable[ord(ch)] := cDIGIT;
  for ch := 'A' to 'Z' do FCharTable[ord(ch)] := cLETTER;
  for ch := 'a' to 'z' do FCharTable[ord(ch)] := cLETTER;
  FCharTable[ord('.')] := cPOINT;
  FCharTable[ord('"')] := cDOUBLEQUOTE;
  FCharTable[ord('_')] := cUNDERSCORE;
  FCharTable[EOFCHAR] := cETX;
end;



procedure TSBWScanner.StarTSBWScanner;
begin
  yylineno := 1;
  BufferPtr := 0;
  NextChar;
end;


procedure TSBWScanner.CollectStream;
begin
  FStreamString := Fch;
  FCollectStream := True;
end;


procedure TSBWScanner.StopCollecting;
begin
  FCollectStream := False;
end;


procedure TSBWScanner.AddBuiltInRoutines;
begin
  FBuiltInRoutines := TStringList.Create;
  FBuiltInRoutines.AddObject ('print', TObject (tPrintToken));
  FBuiltInRoutines.AddObject ('println', TObject (tPrintlnToken));
  FBuiltInRoutines.AddObject ('newline', TObject (tNewline));

  FBuiltInRoutines.Sort;
end;


procedure TSBWScanner.DestroyBuiltInRoutines;
begin
  FBuiltInRoutines.Free;
end;


// Some predefined keywords
procedure TSBWScanner.AddKeyWords;
var i : Integer;
begin
  FKeyWordList[2].AddObject ('if', TObject (tIfToken));
  FKeyWordList[2].AddObject ('do', TObject (tDoToken));
  FKeyWordList[2].AddObject ('to', TObject (tToToken));
  FKeyWordList[2].AddObject ('or', TObject (tOrToken));
  FKeyWordList[2].AddObject ('of', TObject (tOfToken));
  FKeyWordList[2].AddObject ('in', TObject (tInToken));

  FKeyWordList[3].AddObject ('end', TObject (tEndToken));
  FKeyWordList[3].AddObject ('for', TObject (tForToken));
  FKeyWordList[3].AddObject ('and', TObject (tAndToken));
  FKeyWordList[3].AddObject ('xor', TObject (tXorToken));
  FKeyWordList[3].AddObject ('not', TObject (tNotToken));
  FKeyWordList[3].AddObject ('var', TObject (tVarToken));
  FKeyWordList[3].AddObject ('ext', TObject (tExtToken));
  FKeyWordList[3].AddObject ('div', TObject (tDivToken));
  FKeyWordList[3].AddObject ('ode', TObject (tOdeToken));
  FKeyWordList[3].AddObject ('try', TObject (tTryToken));
  FKeyWordList[3].AddObject ('vol', TObject (tVolToken));
  FKeyWordList[3].AddObject ('alg', TObject (tAlgToken));

  FKeyWordList[4].AddObject ('then', TObject (tThenToken));
  FKeyWordList[4].AddObject ('defn', TObject (tDefnToken));
  FKeyWordList[4].AddObject ('else', TObject (tElseToken));
  FKeyWordList[4].AddObject ('quit', TObject (tQuitToken));
  FKeyWordList[4].AddObject ('type', TObject (tTypeToken));
  FKeyWordList[4].AddObject ('read', TObject (tReadToken));
  FKeyWordList[4].AddObject ('from', TObject (tFromToken));
  FKeyWordList[4].AddObject ('date', TObject (tDateToken));
  FKeyWordList[4].AddObject ('halt', TObject (tHaltToken));

  FKeyWordList[5].AddObject ('begin', TObject (tBeginToken));
  FKeyWordList[5].AddObject ('while', TObject (tWhileToken));
  FKeyWordList[5].AddObject ('until', TObject (tUntilToken));
  FKeyWordList[5].AddObject ('print', TObject (tPrintToken));
  FKeyWordList[5].AddObject ('graph', TObject (tGraphToken));
  FKeyWordList[5].AddObject ('write', TObject (tWriteToken));
  FKeyWordList[5].AddObject ('title', TObject (tTitleToken));
  FKeyWordList[5].AddObject ('notes', TObject (tNotesToken));
  FKeyWordList[5].AddObject ('raise', TObject (tRaiseToken));
  FKeyWordList[5].AddObject ('break', TObject (tBreakToken));

  FKeyWordList[6].AddObject ('repeat', TObject (tRepeatToken));
  FKeyWordList[6].AddObject ('return', TObject (tReturnToken));
  FKeyWordList[6].AddObject ('module', TObject (tModuleToken));
  FKeyWordList[6].AddObject ('import', TObject (tImportToken));
  FKeyWordList[6].AddObject ('downto', TObject (tDownToToken));
  FKeyWordList[6].AddObject ('author', TObject (tAuthorToken));
  FKeyWordList[6].AddObject ('except', TObject (tExceptToken));
  FKeyWordList[6].AddObject ('extern', TObject (tExternToken));

  FKeyWordList[7].AddObject ('println', TObject (tPrintlnToken));
  FKeyWordList[7].AddObject ('declare', TObject (tDeclareToken));
  FKeyWordList[7].AddObject ('addhelp', TObject (tAddHelpToken));
  FKeyWordList[7].AddObject ('comment', TObject (tCommentToken));
  FKeyWordList[7].AddObject ('finally', TObject (tFinallyToken));

  FKeyWordList[8].AddObject ('function', TObject (tFuncToken));

  FKeyWordList[10].AddObject ('kineticlaw', TObject (tKineticLawToken));

  FKeyWordList[12].AddObject ('defaultmodel', TObject (tDefaultModelToken));
  FKeyWordList[12].AddObject ('graphsurface', TObject (tGraphSurfaceToken));

  for i := 1 to KEYLISTLEN do
      FKeyWordList[i].Sort;
end;


function TSBWScanner.IsKeyWord (FTokenString : string; var Token : TTokenCode) : boolean;
var l, index : integer;
begin
  result := False;
  l := length (FTokenString);
  // Don't bother checking on keyword longer than KEYLISTLEN, it will cause
  // an access violoation if you.
  if l <= KEYLISTLEN then
     begin
     if FKeyWordList[l].Find(FTokenString, Index) then
        begin
        Token := TTokenCode (FKeyWordList[l].Objects[Index]);
        result := True;
        end;
     end
  else
     result := False;
end;


function TSBWScanner.LineNo : integer;
begin
  Result := yylineno;
end;


function TSBWScanner.GetCurrentText : string;
var i : integer;
begin
  result := '';
  for i := 0 to BufferPtr - 1 do
      result := result + Buffer[i];
end;


// Read a line of text from the input device (reads up to end of line char)
// Also does some basic line editing, eg backspace
{unction TSBWScanner.readLine : integer;
var key : char;
begin
  repeat
    key := yyConsole.readchar;
    Buffer[BufferPtr] := Key;
    inc (BufferPtr);
  until key = #13;

  result := BufferPtr;
end;}


// get a single char form the input device, stream or console
function TSBWScanner.getChar : char;
begin
  if UnGetCharAvailable then
     begin
     Result := UnGetCharacter;
     UnGetCharAvailable := False;
     if FCollectStream and (Result <> char (EOFCHAR)) then
        FStreamString := FStreamString + Result;
     Exit;
     end;

  // If the buffer is empty, read a new chuck of text from the
  // input stream, this might be a stream or console
  if BufferPtr = 0 then
     begin
     if FInputDevice = tStreamDevice then
        begin
        BufferLength := yyStream.Read(Buffer, 255);
        while BufferLength = 0 do
              begin
              yyStream.Free;
              yyStream := nil;
              FInputDevice := tConsoleDevice;
              JustComeOffStream := True;
              Result := char (EOFCHAR);
              Exit;
              end;
        end
     else
        begin
        // Read a line of text from the console
        //BufferLength := ReadLine;
        //BufferPtr := 0;
        end;
  end;

  result := Buffer[BufferPtr];
  inc (BufferPtr);
  if BufferPtr >= BufferLength then
     BufferPtr := 0;  // Indicates the buffer is empty

  if FCollectStream and (Result <> char (EOFCHAR)) then
     FStreamString := FStreamString + Result;
end;


procedure TSBWScanner.UnGetChar (c : Char);
begin
  dec (BufferPtr);
  if BufferPtr <= 0 then
     begin
     BufferPtr := 0;
     UnGetCharacter := c;
     UnGetCharAvailable := True;
     if FCollectStream and (Length (FStreamString) > 0) then
        FStreamString := Copy (FStreamString, 1, Length (FStreamString) - 1 );
     Exit;
     end
  else
     UnGetCharAvailable := False;

  Buffer[BufferPtr] := c;
  if FCollectStream and (Length (FStreamString) > 0) then
     FStreamString := Copy (FStreamString, 1, Length (FStreamString) - 1 );
end;



procedure TSBWScanner.RestoreConsole;
var S : TStream;
begin
  yyStream.Free;
  yyStream := Nil;
  S := PopStream;
  while S <> Nil do
        begin
        S.free;
        S := PopStream;
        end;

  FInputDevice := tConsoleDevice;
  JustComeOffStream := True;
  IgnoreNewLines := False;
end;


procedure TSBWScanner.SetStream (s : TStream);
begin
  FInputDevice := tStreamDevice;
  yyStream := s;
  FStreamOwner := oExternal;
end;


function TSBWScanner.ReadUpTo (ch : char) : string;
begin
end;


procedure TSBWScanner.newLine;
begin
  //yyConsole.WriteString (#13#10);
end;


function TSBWScanner.CurrentChar : char;
begin
  Result := Fch;
end;


procedure TSBWScanner.EatNewLine;
begin
  while Fch = #13 do
        begin
        Fch := getChar;  // Lineno already incremented
        while Fch = #10 do
              Fch := getChar;
        end;
end;


{ Update ch to next character in input stream }
function TSBWScanner.Nextchar : char;
begin
  Fch := GetChar;
  if IgnoreNewLines then
     begin
     while Fch = #13 do
           begin
           Inc (yylineno);
           Fch := GetChar;
           while Fch = #10 do
                 Fch := GetChar;
           end;
     end
  else
     begin
     // filter out any linefeed characters
     if IgnoreNewLines then
        while Fch = #10 do
              Fch := GetChar;
     if Fch = #13 then
        inc (yylineno);
     end;
  Result := Fch;
end;


// Look ahead to the next ch without advancing position pointer
function  TSBWScanner.LookAhead_ch : char;
begin
  result := GetChar;
end;


// Skip blanks, null characters and tabs
procedure TSBWScanner.skipBlanks;
begin
  while Fch <= ' ' do
        begin
        if (Fch = #10) or (Fch = #13) then
           Exit;
        NextChar;
        end;
end;


function TSBWScanner.IsBuiltInRoutine (FTokenString : string; var Token : TTokenCode) : Boolean;
var index : integer;
begin
  result := False;
  if FBuiltInRoutines.Find(LowerCase (FTokenString), Index) then
     begin
     Token := TTokenCode (FBuiltInRoutines.Objects[Index]);
     result := True;
     end
  else
     result := False;
end;



// Scan in a number token, will distinguish between integer and float
// (including scientific notation) Valid numbers include:
// 23, 1.2, 0.3, .1234345, 1e3, 1e-6, 3.45667E-12
// Note: negative numbers not supported here, instead the '-' sign is read
// in as a separate token in its own right, therefore -1.2 yields two
// tokens when scanned, tMinus followed by tFloat. To obtain the negative number
// you would need to multiply TokenFloat by (-1). It was done this way so that
// unary minuses in things such as "-(1.2)" could be handled.
procedure TSBWScanner.getNumber;
var single_digit : integer; scale, evalue : Extended; exponent_sign, digit_count : integer;
begin
  FTokenInteger := 0; FTokenFloat := 0.0;
  evalue := 0.0; exponent_sign := 1;

  // Assume it's an integer
  FToken := tINTEGER;
  // check for decimal point just in case user has typed some thing like .5
  if Fch <> '.' then
     try
       repeat
         single_digit := ord (Fch) - ord ('0');
         FTokenInteger := 10*FTokenInteger + single_digit;
         nextchar;
       until FCharTable[ord(Fch)] <> cDIGIT;
     except
       raise Exception.Create ('Integer Overflow - constant value too large to read');
     end;

  scale := 1;
  if Fch = '.' then
     begin
     // Then it's a float. Start collecting fractional part
     FToken := tFLOAT; FTokenFloat := FTokenInteger;
     nextchar;
     if FCharTable[ord(Fch)] <> cDIGIT then
        raise EScannerError.Create ('Syntax error: expecting number after decimal point');

     try
       while FCharTable[ord(Fch)] = cDIGIT do
         begin
         scale := scale * 0.1;
         single_digit := ord (Fch) - ord ('0');
         FTokenFloat := FTokenFloat + (single_digit * scale);
         nextchar;
         end;
     except
       raise Exception.Create ('Floating point overflow - constant value too large to read in');
     end;
     end;

  // Next check for scientific notation
  if (Fch = 'e') or (Fch = 'E') then
     begin
     // Then it's a float. Start collecting exponent part
     if FToken = tInteger then
        begin
        FToken := tFLOAT;
        FTokenFloat := FTokenInteger;
        end;
     nextchar;
     if (Fch = '-') or (Fch = '+') then
        begin
        if Fch = '-' then exponent_sign := -1;
        nextchar;
        end;
     { accumulate exponent, check that first ch is a digit }
     if FCharTable[ord(Fch)] <> cDIGIT then
        begin
        raise EScannerError.Create ('Syntax error: number expected in exponent');
        end;

     digit_count := 0;
     try
       repeat
         inc (digit_count);
         single_digit := ord (Fch) - ord ('0');
         evalue := 10*evalue + single_digit;
         nextchar;
       until (FCharTable[ord(Fch)] <> cDIGIT) or (digit_count > MAX_DIGIT_COUNT);
     except
       raise Exception.Create ('Floating point overflow - Constant value too large to read');
     end;

     if digit_count > MAX_DIGIT_COUNT then
        begin
        raise EScannerError.Create ('Syntax error: too many digits in exponent');
        end;

     evalue := evalue * exponent_sign;
     if evalue > 300 then
        raise EScannerError.Create ('Exponent overflow while parsing floating point number');
     evalue := mypower (10.0, evalue);
     FTokenFloat := FTokenFloat * evalue;
     end;
  if (Fch = 'i') or (Fch = 'j') then
     begin
     if FToken = tInteger then
        FTokenFloat := FTokenInteger;
     FToken := tComplexToken;
     nextChar;
     end;
end;


procedure TSBWScanner.get_TextBlock;
begin
  FTokenString := '';
  while (Fch <> ';') and (Fch <> char (EOFCHAR)) do
     begin
     FTokenString := FTokenString + Fch;  // Inefficient but convenient
     nextchar;
     end;
end;


{ Scan in an identifier token }
procedure TSBWScanner.getWord;
var str : string; Ig : Boolean;
begin
  Ig := IgnoreNewLines;
  IgnoreNewLines := False;
  FTokenString := '';
  while (FCharTable[ord(Fch)] = cLETTER)
     or (FCharTable[ord(Fch)] = cDIGIT)
     or (FCharTable[ord(Fch)] = cUNDERSCORE) do
        begin
        FTokenString := FTokenString + Fch;  // Inefficient but convenient
        nextchar;
        end;

  if IsKeyWord (FTokenString, FToken) then
     begin
     str := FTokenString;
     Token := FToken;
     end
  else
     Token := tIdentifier;

  if not IsBuiltInRoutine (FTokenString, FToken) then
     begin
     str := FTokenString;
     Token := FToken;
     end;
  IgnoreNewLines := Ig;
  if IgnoreNewLines and (Fch = #13) then
     EatNewLine;
end;


function TSBWScanner.IsDoubleQuote (ch : char) : Boolean;
begin
  Result := False;
  if FCharTable[Ord(ch)] = cDoubleQuote then
     Result := True;
end;

// Get a token of the form "abc"
procedure TSBWScanner.getString;
var OldIgnoreNewLines : boolean;
begin
  FTokenString := '';
  NextChar;

  FToken := tString;
  while Fch <> char (EOFCHAR) do
        begin
        if Fch = '\' then
           begin
           NextChar;
           case Fch of
               '\' : FTokenString := FTokenString + '\';
               'n' : FTokenString := FTokenString + #13#10;
               'r' : FTokenString := FTokenString + #13;
               'f' : FTokenString := FTokenString + #10;
               't' : FTokenString := FTokenString + Format ('%6s', [' ']);
            else
               raise EScannerError.Create ('Scanner error: Unrecognised control code in string');
            end;
            NextChar;
           end
        else
        begin
        OldIgnoreNewLines := IgnoreNewLines;
        if IsDoubleQuote (Fch) then
           begin
           // Just in case the double quote is at the end of a line and another string
           // start immediately in the next line, if we ignore newlines we'll
           // pick up a double quote rather than the end of a string
           IgnoreNewLines := False;
           NextChar;
           if IsDoubleQuote (Fch) then
              begin
              FTokenString := FTokenString + Fch;
              NextChar;
              end
           else
              begin
              if OldIgnoreNewLines then
                 begin
                 if Fch = #13 then NextChar;
                 if Fch = #10 then NextChar;
                 end;
              IgnoreNewLines := OldIgnoreNewLines;
              Exit;
              end;
           end
        else
           begin
           FTokenString := FTokenString + Fch;
           NextChar;
           end;
        IgnoreNewLines := OldIgnoreNewLines;
        end;
        end;

  if Fch = char (EOFCHAR) then
     raise EScannerError.Create ('String without terminating quotation mark');
end;


{ Get special tokens }
procedure TSBWScanner.getSpecial;
var lch : char; Ig : boolean; oldyylineno : integer;
begin
  case Fch of
char($FF) : Ftoken := tEndofStream;
char($00) : begin Ftoken := tEmpty;     NextChar; end;
     '+'  : begin Ftoken := tPlus;      NextChar; end;
     '^'  : begin Ftoken := tPower;     NextChar; end;
     '('  : begin Ftoken := tLparen;    NextChar; end;
     ')'  : begin Ftoken := tRparen;    NextChar; end;
     '['  : begin Ftoken := tLBracket;  NextChar; end;
     ']'  : begin Ftoken := tRBracket;  NextChar; end;
     '{'  : begin Ftoken := tLcBracket; NextChar; end;
     '}'  : begin Ftoken := tRcBracket; NextChar; end;
     '$'  : begin Ftoken := tDollar;    NextChar; end;
     '?'  : begin
            Ftoken := tQuestion;
            FTokenString := '?';
            NextChar;
            end;
     '.'  : begin
            if NextChar = '.' then
               begin
               Ftoken := tFullFullStop;
               NextChar;
               end
            else
               Ftoken := tFullStop;
            end;
     '>'  : begin
            if NextChar = '=' then
               begin
               Ftoken := tMoreThanOrEqual;
               NextChar;
               end
            else
               Ftoken := tMoreThan;
            end;

     '<'  : begin
            lch := NextChar;
            if lch = '=' then
               begin
               Ftoken := tLessThanOrEqual;
               NextChar;
               end
            else
            if lch = '>' then
               begin
               Ftoken := tNotEqual;
               NextChar;
               end
            else
               Ftoken := tLessThan;
            end;

     '='  : begin
            lch := NextChar;
            if lch = '>' then
               begin
               Ftoken := tEqualsArrow;
               NextChar;
               end
            else
            if lch = '=' then
               begin
               Ftoken := tEquivalence;
               NextChar;
               end
            else
               Ftoken := tEquals;
            end;
     ';'  : begin Ftoken := tSemicolon;  NextChar; end;
     ':'  : begin Ftoken := tColon;      NextChar; end;
     ','  : begin Ftoken := tComma;      NextChar; end;
     '''' : begin Ftoken := tApostrophy; NextChar; end;
     '-'  : begin
            if NextChar = '>' then
               begin
               Ftoken := tArrow;
               NextChar;
               end
            else
               Ftoken := tMinus;
            end;
     // Comment lines
     '#'  : begin
            while Fch <> ';' do
               NextChar;  // dump it
            Ftoken := tHashComment;
            NextChar;
            end;
     '\'  : begin
            // look ahead at next ch
            lch := NextChar;
            if lch = '"' then
               begin
               Ftoken := tEscapeDoubleQuote;
               NextChar; // dump it
               end

            end;
     '/'  : begin
            // look ahead at next ch
            lch := NextChar;
            if lch = '*' then
               begin
               Ftoken := tStartComment;
               NextChar; // dump it
               end
            else
            if lch = '/' then
               begin
               Ftoken := tStartCComment;
               Ig := IgnoreNewLines;
               oldyyLineNo := yyLineno;
               IgnoreNewLines := False;
               NextChar;
               IgnoreNewLines := Ig;
               // This is to prevent //#10#10 from being
               // counted as two lines rather than one
               if oldyyLineno < yylineno then
                  yylineno := oldyylineno;
               end
            else
               Ftoken := tSlash;
            end;
     '*'  : begin
            if NextChar = '/' then
               begin
               Ftoken := tEndComment;
               NextChar; // dump it
               end
            else
               Ftoken := tMult;
            end;
     LF   : begin
            FToken := tLineFeed;
            NextChar;
            end;
     CR   : begin
            if FInputDevice = tStreamDevice then
               begin
               FToken := tEOL;  // End of line marker
               Fch := char ($00);
               end
            else
               begin
               FToken := tEOL;  // End of line marker
               Fch := char ($00);
               end;
            end
  else
     begin
     Ftoken := tError;
     NextChar;
     end;
  end;
end;



procedure TSBWScanner.InternalNextToken;
begin
  if QuiTSBWScanner then
     begin
     QuiTSBWScanner := False;
     IgnoreNewLines := True;
     BufferPtr := 0;
     raise EScannerError.Create ('Ctrl-C exception');
     end;

  { check if a token has been pushed back into the token stream, if so use it first }
  if FPreviousToken <> tEMPTY then
     begin
     Ftoken         := FPreviousToken;
     FTokenString   := FPreviousTokenString;
     FTokenFloat    := FPreviousFloat;
     FTokenInteger  := FPreviousInteger;
     FPreviousToken := tEMPTY;
     exit;
     end;

  // Check if there is anything in the token queue, if so then then the item
  // from the queue and exit. If not, read as normal from the stream.
  // Checking the queue before reading from the stream can be turned off and on
  // by setting the FromQueue Flag.
  if FromQueue then
     begin
     if not IsQueueEmpty then
        begin
        GetTokenFromQueue;
        Exit;
        end;
     end;

  skipBlanks;
  FTokenString := '';

  case FCharTable[ord(Fch)] of
     cLETTER,
  cUNDERSCORE     : getWord;
     cDIGIT       : getNumber;
     cDoubleQuote : getString;
//tEscapeDoubleQuote: getString;
     cETX         : Ftoken := tEnd;
   else
     getSpecial;
  end;
  //if FToken = tCommentToken then
  //   InternalNextToken;
end;


{ Get the next token }
procedure TSBWScanner.NextToken;
begin
  // Intercept comments here and ignore them
  InternalNextToken;

  while FToken in [tStartComment, tHashComment, tStartCComment] do
  begin
  if FToken = tStartComment then
     begin
     InternalNextToken;
     while (FToken <> tEndComment) and (FToken <> tEND) do
           InternalNextToken;
     if FToken = tEND then
        raise EScannerError.Create ('Comment not terminated');
     InternalNextToken;  // get the real next token
     end
  else
  if FToken = tHashComment then
     InternalNextToken
  else
  if FToken = tStartCComment then
     begin
     // Comment which ends with an end of line char
     while (Fch <> #13) and (Fch <>  char (EOFCHAR)) do
           Fch := GetChar;
     while (Fch = #10) and (Fch <>  char (EOFCHAR)) do
           Fch := GetChar;
     while Fch = #13 do
           begin
           Inc (yylineno);
           getChar;  // Dump the linefeed
           Fch := getChar;
           end;
     InternalNextToken;  // get the real next token
     end;
  end;
end;


// Allows one token look ahead
// Push token back into token stream
procedure TSBWScanner.UnGetToken;
begin
  FPreviousToken       := Ftoken;
  FPreviousTokenString := FTokenString;
  FPreviousInteger     := FTokenInteger;
  FPreviousFloat       := FTokenFloat;
end;


{ -------------------------------------------------------------------- }
{ Some debugging routines }

function TSBWScanner.TokenToString (T : TTokenCode) : string;
begin
  case T of
       tEmpty      : result := 'Empty token';
       tIdentifier : result := 'Identifier <' + FTokenString + '>';
       tInteger    : result := 'Integer <' + inttostr (FTokenInteger) + '>';
       tFloat      : result := 'Float <' + floattostr (FTokenFloat) + '>';
       tString     : result := 'String <' + FTokenString + '>';
       tMinus      : result := 'Minus';
       tPlus       : result := 'Plus';
       tMult       : result := 'Multiply';
       tSlash      : result := 'Slash';
       tPower      : result := 'Power (^)';
       tRParen     : result := 'Right parentheses '')''';
       tLParen     : result := 'Left parentheses ''(''';
       tRBracket   : result := 'Right bracket '']''';
       tLBracket   : result := 'Left bracket ''[''';
       tLcBracket  : result := 'Left Curley bracket';
       tRcBracket  : result := 'Right Curley bracket';
       tEquals     : result := 'Equals';
       tApostrophy : result := 'Apostrphy';
       tSemicolon  : result := ''';''';
       tColon      : result := ''':''';
       tComma      : result := ''',''';
       tDollar     : result := '''$''';
       tArrow      : result := '''->''';
       tEqualsArrow: result := '''=>''';
       tLessThan   : Result := '''<''';
       tMoreThan   : Result := '''>''';
       tFullStop   : Result := '''.''';
       tFullFullStop : Result := '''..''';
       tEndToken   : result := 'Key Word: End';
       tIfToken    : result := 'Key Word: If';
       tThenToken  : result := 'Key Word: Then';
       tQuitToken  : Result := 'Key Word: Quit';
       tdefnToken  : Result := 'Key Word: Defn';
       tModuleToken: Result := 'Key Word: Module';
       tDoToken    : Result := 'Key Word: Do';
       tPrintToken : Result := 'Key Word: print';
       tPrintLnToken: Result:= 'Key Word: println';
       tElseToken  : Result := 'Key Word: Else';
       tRepeatToken : Result := 'Key Word: Repear';
       tUntilToken  : Result := 'Key Word: Until';
       tExceptToken : Result := 'Key Word: Except';
       tAddHelpToken : Result := 'Key Word: AddHelp';
       tInToken : Result := 'Key Word: In';
       tEndComment : Result := 'End of Comment Marker';

       tEOL        : Result := 'End of Line';
       tError      : result := 'Error!';
       tEnd        : result := 'End of Stream';
       tLineFeed   : result := 'LineFeed';

  else
       result := 'Unrecognised token in TokenToString';
  end;
end;


{ Returns a string representation of the most recently read Token }
function TSBWScanner.TokenToString : string;
begin
  Result := TokenToString (token);
end;


function TSBWScanner.TokenLiteral : string;
begin
  case Token of
       tEmpty      : result := '';
       tIdentifier : result := FTokenString;
       tInteger    : result := InttoStr (FTokenInteger);
       tFloat      : result := Format ('%g', [FTokenFloat]);
     tComplexToken : result := Format ('%gi', [FTokenFloat]);
       tString     : result := FTokenString;
       tMinus      : result := '-';
       tPlus       : result := '+';
       tMult       : result := '*';
       tSlash      : result := '/';
       tPower      : result := '^';
       tRParen     : result := ')';
       tLParen     : result := '(';
       tRBracket   : result := ']';
       tLBracket   : result := '[';
       tLcBracket  : result := '{';
       tRcBracket  : result := '}';
       tEquals     : result := '=';
       tApostrophy : result := '''';
       tSemicolon  : result := ';';
       tColon      : result := ':';
       tComma      : result := ',';
       tDollar     : result := '$';
       tArrow      : result := '->';
       tEqualsArrow: result := '=>';
       tLessThan   : Result := '<';
       tMoreThan   : Result := '<';
       tFullStop   : Result := '.';
       tFullFullStop: Result := '..';
       tEndToken   : result := 'end';
       tIfToken    : result := 'if';
       tThenToken  : result := 'then';
       tQuitToken  : Result := 'quit';
       tdefnToken  : Result := 'defn';
       tDoToken    : Result := 'do';
       tToToken    : REsult := 'to';
       tOrToken    : Result := 'or';
       tForToken   : Result := 'for';
       tAndToken   : Result := 'and';
       tNotToken   : Result := 'not';
       tElseToken  : Result := 'else';
       tBeginToken : Result := 'begin';
       tWhileToken : Result := 'while';
       tUntilToken : Result := 'until';
       tRepeatToken: Result := 'repeat';
       tInToken    : Result := 'in';
       tAddHelpToken : Result := 'AddHelp';

       tEOL        : Result := 'End of Line';
       tError      : result := 'Error!';
       tEnd        : result := 'End of Stream';
   else
       result := 'Unrecognised token in TokenLiteral';
  end;

end;


end.
