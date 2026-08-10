unit uAntimonyAPI;

interface

Uses Classes, SysUtils, uCommonTypes;

const
  allFormulas : integer = 2;
  varSpecies :  integer = 11;
  constSpecies : integer = 15;

var
   DLLLoaded : boolean;

   function loadAntimonyLibrary (var errMsg : string) : boolean;

   { libantimony and libSBML speak UTF-8 throughout. Every function here
     takes and returns ordinary Delphi strings and converts at the
     boundary — see Utf8PtrToString. Nothing in this unit may declare an
     AnsiString parameter or result for library text: that type carries
     the SYSTEM codepage, so the compiler would transcode UTF-8 to CP1252
     (or whatever the machine is set to) in one direction and misread it
     in the other. }
   function ant_loadSBMLString (const str : string) : integer;
   function ant_loadAntimonyString (const str : string) : integer;
   function ant_loadAntimonyStringWithException (const str : string) : integer;
   function getSBMLFromAntimony (const str : string) : TModelErrorState;
   function getAntimonyFromSBML (const str : string) : string;

   function printAllDataFor : string;

   function getNumReactions : integer;
   function getSymbolsEquations (return_type : integer) : TArray<string>;
   function getSymbolNamesOfType (return_type : integer) : TArray<string>;
   function getNumSymbolsOfType (return_type : integer) : integer;

   //char ** 	getSymbolNamesOfType (const char *moduleName, return_type rtype)


implementation


Uses
  {$IFDEF POSIX}
  Posix.Dlfcn,
  {$ELSE}
  Winapi.Windows,
  {$ENDIF}
  System.AnsiStrings,
  FMX.Dialogs;

type
  TIntCharFunc = function (str : PAnsiChar) : integer; cdecl;
  TCharFunc = function : PAnsiChar; cdecl;
  TCharCharFunc = function (str : PAnsiChar) : PAnsiChar; cdecl;

  TCharCharReturnCharInt = function (str : PAnsiChar; returnValue : integer) : PPAnsiChar; cdecl;
  TIntCharInt = function (str : PAnsiChar; returnValue : integer) : integer; cdecl;

  TAnsiCharPtrArray = array[0..0] of PAnsiChar;
  PAnsiCharPtrArray = ^TAnsiCharPtrArray;

var FLibHandle : HModule;
    libAntimonyName : string;

    ant_libLoadSBMLString : TIntCharFunc;
    libLoadString : TIntCharFunc;
    libLoadAntimonyString : TIntCharFunc;
    libGetSBMLString : TCharCharFunc;
    libGetAntimonyString : TCharCharFunc;
    libGetMainModuleName : TCharFunc;
    libGetNumReactions : TIntCharFunc;
    libGetlastError : TCharFunc;
    libGetSymbolNamesOfType : TCharCharReturnCharInt;
    libGetSymbolNamesOfType2 : TCharCharReturnCharInt;
    libGetNumSymbolsOfType : TIntCharInt;
    libPrintAllDataFor : TCharCharFunc;

    libGetSymbolEquationsOfType : TCharCharReturnCharInt;


function GetProcAddress(AModule: HMODULE; AName: System.PChar): Pointer;
begin
  {$IFDEF MSWINDOWS}
  Result := Winapi.Windows.GetProcAddress(AModule, AName);
  {$ELSE}
  Result := System.SysUtils.GetProcAddress(AModule, AName);
  {$ENDIF}

  if Result = nil then
    raise Exception.CreateFmt('"%s" function address could not be retrieved from antimony library', [AName]) at ReturnAddress;
end;


{ A UTF-8 byte string from the library, decoded.

  The bytes must NOT be routed through AnsiString: that type carries the
  system codepage, so the compiler would treat UTF-8 bytes as CP1252 (or
  whatever the machine is set to) and mangle every non-ASCII character.
  The same mistake in the other direction — passing a UnicodeString to a
  parameter declared AnsiString — is what made downloaded BioModels fail
  to convert: characters like 'τ' or a curly apostrophe became single
  high bytes, which libSBML rejected as an invalid UTF-8 sequence and
  reported as "XML content is not well-formed" at the first such line. }
function Utf8PtrToString (p : PAnsiChar) : string;
var
  u : UTF8String;
  n : Integer;
begin
  Result := '';
  if p = nil then
    Exit;
  n := System.AnsiStrings.StrLen (p);
  SetLength (u, n);
  if n > 0 then
    Move (p^, PAnsiChar (u)^, n);
  Result := string (u);
end;


function ant_loadSBMLString (const str : string) : integer;
var p : PAnsiChar;
    err : integer;
    utf8 : UTF8String;
begin
  utf8 := UTF8String (str);
  err := ant_libLoadSBMLString (PAnsiChar (utf8));
  if err = -1 then
     begin
     p := libGetLastError;
     raise Exception.Create (Utf8PtrToString (p));
     end;
  result := err;
end;


function ant_loadAntimonyStringWithException (const str : string) : integer;
var utf8 : UTF8String;
begin
  utf8 := UTF8String (str);
  result := libLoadAntimonyString (PAnsiChar (utf8));
end;


function ant_loadAntimonyString (const str : string) : integer;
var p : PAnsiChar;
    err : integer;
    utf8 : UTF8String;
begin
  utf8 := UTF8String (str);
  err := libLoadAntimonyString (PAnsiChar (utf8));
  if err = -1 then
     begin
     p := libGetLastError;
     raise Exception.Create (Utf8PtrToString (p));
     end;
  result := err;
end;


function getSBMLFromAntimony (const str : string) : TModelErrorState;
var p : PAnsiChar;
    err : integer;
    pendingErr : string;
    utf8 : UTF8String;
begin
  utf8 := UTF8String (str);
  err := libLoadString (PAnsiChar (utf8));
  if err = -1 then
     begin
     p := libGetLastError;
     result.errMsg := Utf8PtrToString (p);
     result.ok := false;
     exit;
     end;

  { libantimony returns a module handle on success but may still have
    populated the error buffer with non-fatal issues (e.g. symbol used
    without initial value). These produce SBML that won't simulate, so
    treat them as load failures here. }
  p := libGetLastError;
  pendingErr := Utf8PtrToString (p);
  if pendingErr <> '' then
     begin
     result.errMsg := pendingErr;
     result.ok := false;
     exit;
     end;

  p := libGetSBMLString (libGetMainModuleName());
  result.sbmlStr := Utf8PtrToString (p);
  result.ok := True;
end;


function getAntimonyFromSBML (const str : string) : string;
var p : PAnsiChar;
    utf8 : UTF8String;
begin
  utf8 := UTF8String (str);
  if ant_libLoadSBMLString (PAnsiChar (utf8)) = -1 then
     begin
     p := libGetLastError;
     raise Exception.Create ('Antimony load error: ' + Utf8PtrToString (p));
     end;
  p := libGetAntimonyString (libGetMainModuleName());
  result := Utf8PtrToString (p);
end;


function printAllDataFor : string;
var p : PAnsiChar;
begin
  p := libPrintAllDataFor (libGetMainModuleName());
  result := Utf8PtrToString (p);
end;


function getNumReactions : integer;
begin
  result := libGetNumReactions (libGetMainModuleName());
end;


function getNumSymbolsOfType (return_type : integer) : integer;
begin
  result := libGetNumSymbolsOfType (libGetMainModuleName(), return_type);
end;


function getSymbolNamesOfType (return_type : integer) : TArray<string>;
var
  StringPtrArray: PAnsiCharPtrArray;
  i, numSymbols : Integer;
begin
  numSymbols := libGetNumSymbolsOfType (libGetMainModuleName(), return_type);

  SetLength(Result, numSymbols);
  StringPtrArray := PAnsiCharPtrArray(libGetSymbolNamesOfType (libGetMainModuleName(), return_type));
  if StringPtrArray <> nil then
  begin
    for i := 0 to numSymbols - 1 do
    begin
      { Utf8PtrToString, not string(): the cast would read these bytes in
        the system codepage, so an identifier or equation containing a
        non-ASCII character would come back mangled — and identifiers are
        matched by name all over Iridium. }
      Result[i] := Utf8PtrToString (StringPtrArray^[i]);
    end;
  end;
end;


function getSymbolsEquations (return_type : integer) : TArray<string>;
var
  StringPtrArray: PAnsiCharPtrArray;
  i, numSymbols : Integer;
begin
 numSymbols := libGetNumSymbolsOfType (libGetMainModuleName(), return_type);

  SetLength(Result, numSymbols);
  StringPtrArray := PAnsiCharPtrArray(libGetSymbolEquationsOfType (libGetMainModuleName(), return_type));
  if StringPtrArray <> nil then
  begin
    for i := 0 to numSymbols - 1 do
    begin
      { Utf8PtrToString, not string(): the cast would read these bytes in
        the system codepage, so an identifier or equation containing a
        non-ASCII character would come back mangled — and identifiers are
        matched by name all over Iridium. }
      Result[i] := Utf8PtrToString (StringPtrArray^[i]);
    end;
  end;

end;


function loadAntimonyLibrary (var errMsg : string) : boolean;
var path : string;
    DLErrorMsg : string;
begin
  path := ExtractFilePath(ParamStr(0)) + libAntimonyName;

  try
    FLibHandle := SafeLoadLibrary('' + libAntimonyName);
{$IF DEFINED(MACOS)}
    if FLibHandle = 0 then
       begin
       DLErrorMsg := string(dlerror);
       raise Exception.Create('antimony library could not be loaded: ' + DLErrorMsg);
       end;
{$ELSE}
    if FLibHandle = 0 then
       begin
       errMsg := 'Antimony library could not be loaded';
       { Exit, or the assignment below overwrites this and the failure is
         reported by whichever GetProcAddress raises first — which says
         a function is missing rather than that the DLL is absent. }
       result := False;
       exit;
       end;
{$ENDIF}

    result := true;

    @ant_libLoadSBMLString := GetProcAddress(FLibHandle, 'loadSBMLString');
    @libLoadString  := GetProcAddress (FLibHandle, 'loadString');
    @libGetSBMLString := GetProcAddress (FLibHandle, 'getSBMLString');
    @libGetAntimonyString := GetProcAddress (FLibHandle, 'getAntimonyString');
    @libLoadAntimonyString := GetProcAddress (FLibHandle, 'loadAntimonyString');
    @libGetMainModuleName := GetProcAddress (FLibHandle, 'getMainModuleName');
    @libGetNumReactions := GetProcAddress (FLibHandle, 'getNumReactions');
    @libGetlastError := GetProcAddress (FLibHandle, 'getLastError');
    @libGetSymbolEquationsOfType := GetProcAddress (FLibHandle, 'getSymbolEquationsOfType');

    @libPrintAllDataFor := GetProcAddress (FLibHandle, 'printAllDataFor');

    @libGetNumSymbolsOfType := GetProcAddress (FLibHandle, 'getNumSymbolsOfType');
    @libGetSymbolNamesOfType := GetProcAddress (FLibHandle, 'getSymbolNamesOfType');
  except
     on E: Exception do
        begin
        errMsg := e.message;
        result := false;
        exit;
        end;
  end;
end;

initialization
  {$IFDEF POSIX}
     libAntimonyName := 'libantimony.dylib';
  {$ELSE}
    libAntimonyName := 'libantimony.dll';
  {$ENDIF}
end.
