unit uAntimonyAPI;

interface

Uses Classes, SysUtils, uModelInputManager;

const
  allFormulas : integer = 2;
  varSpecies :  integer = 11;
  constSpecies : integer = 15;

var
   DLLLoaded : boolean;

   function loadAntimonyLibrary (var errMsg : string) : boolean;

   function ant_loadSBMLString (str : AnsiString) : integer;
   function ant_loadAntimonyString (str : AnsiString) : integer;
   function ant_loadAntimonyStringWithException (str : AnsiString) : integer;
   function getSBMLFromAntimony (str : AnsiString) : TModelErrorState;
   function getAntimonyFromSBML (str : AnsiString) : AnsiString;

   function printAllDataFor : AnsiString;

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


function ant_loadSBMLString (str : AnsiString) : integer;
var p : PAnsiChar;
    err : integer;
begin
  err := ant_libLoadSBMLString (PAnsiChar (str));
  if err = -1 then
     begin
     p := libGetLastError;
     raise Exception.Create (AnsiString (p));
     end;
  result := err;
end;


function ant_loadAntimonyStringWithException (str : AnsiString) : integer;
var p : PAnsiChar;
    err : integer;
begin
  err := libLoadAntimonyString (PAnsiChar (str));
  result := err;
end;


function ant_loadAntimonyString (str : AnsiString) : integer;
var p : PAnsiChar;
    err : integer;
begin
  err := libLoadAntimonyString (PAnsiChar (str));
  if err = -1 then
     begin
     p := libGetLastError;
     raise Exception.Create (AnsiString (p));
     end;
  result := err;
end;


function getSBMLFromAntimony (str : AnsiString) : TModelErrorState;
var p : PAnsiChar;
    err : integer;
begin
  err := libLoadString (PAnsiChar (str));
  if err = -1 then
     begin
     p := libGetLastError;
     result.errMsg := AnsiString (p);
     result.ok := false;
     exit;
     end;
  p := libGetSBMLString (libGetMainModuleName());
  result.sbmlStr := AnsiString (p);
  result.ok := True;
end;


function getAntimonyFromSBML (str : AnsiString) : AnsiString;
var p : PAnsiChar;
begin
  if ant_libLoadSBMLString (PAnsiChar (str)) = -1 then
     begin
     p := libGetLastError;
     raise Exception.Create ('Antimony load error: ' + AnsiString (p));
     end;
  p := libGetAntimonyString (libGetMainModuleName());
  result := AnsiString (p);
end;


function printAllDataFor : AnsiString;
var p : PAnsiChar;
begin
  p := libPrintAllDataFor (libGetMainModuleName());
  result := AnsiString (p);
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
      if StringPtrArray^[i] <> nil then
        Result[i] := string(StringPtrArray^[i])
      else
        Result[i] := '';
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
      if StringPtrArray^[i] <> nil then
        Result[i] := string(StringPtrArray^[i])
      else
        Result[i] := '';
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
        raise Exception.Create('antimony library could not be loaded');
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
