unit uAntimonyAPI;

interface

Uses Classes, SysUtils, uModelInputManager;

var
   DLLLoaded : boolean;

   function loadAntimonyLibrary (var errMsg : string) : boolean;

   function ant_loadSBMLString (str : AnsiString) : integer;
   function ant_loadAntimonyString (str : AnsiString) : integer;
   function getSBMLFromAntimony (str : AnsiString) : TModelErrorState;
   function getAntimonyFromSBML (str : AnsiString) : AnsiString;


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

var FLibHandle : HModule;
    libAntimonyName : string;

    ant_libLoadSBMLString : TIntCharFunc;
    libLoadString : TIntCharFunc;
    loadAntimonyString : TIntCharFunc;
    libGetSBMLString : TCharCharFunc;
    libGetAntimonyString : TCharCharFunc;
    libGetMainModuleName : TCharFunc;
    libGetlastError : TCharFunc;


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


function ant_loadAntimonyString (str : AnsiString) : integer;
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
    @loadAntimonyString := GetProcAddress (FLibHandle, 'loadAntimonyString');
    @libGetMainModuleName := GetProcAddress (FLibHandle, 'getMainModuleName');
    @libGetlastError := GetProcAddress (FLibHandle, 'getLastError');
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
