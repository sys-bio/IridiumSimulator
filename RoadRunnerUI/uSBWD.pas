unit uSBWD;


// SBWD DLL Interface

interface

Uses SysUtils, Classes,
{$ifdef MSWINDOWS}
     Windows,
{$endif}
     uSBWUtils, uSBWCommon;
                           
const
   SBW_UniqueModule = 0;
   SBW_SelfManagedModule = 1;
   SBW_SBWManagedModule = 2;
                              

type
  SBWModuleListener = procedure (Id : integer); cdecl;
  SBWSimpleListener = procedure(); cdecl;
  MySBWFunctionHandler = function (DataStream : TSBWDataStream) : SBWDataBlockWriter of object;
  MySBWMethodHandler = procedure (DataStream : TSBWDataStream) of object;

  TUserData = record
                Method : MySBWMethodHandler;
                Func : MySBWFunctionHandler;
                dataType : TCallType;
              end;
  PUserData = ^TUserData;

  SBWHandler = procedure (from : integer; readerObj : SBWDataBlockReader; writerResult : SBWDataBlockWriter; userData : PUserData); cdecl;

  SBWModuleManagementType = integer;

  TSBWModuleDescriptor = record
  	name : PAnsiChar;
  	displayName : PAnsiChar;
  	managementType : SBWModuleManagementType;
  	commandLine : PAnsiChar;
        helpStr : PAnsiChar;
  end;
  PSBWModuleDescriptor = ^TSBWModuleDescriptor;

  TSBWServiceDescriptor = record
               serviceName : PAnsiChar;
	serviceDisplayName : PAnsiChar;
	   serviceCategory : PAnsiChar;
	            module : TSBWModuleDescriptor;
                   helpStr : PAnsiChar;
  end;
  PSBWServiceDescriptor = ^TSBWServiceDescriptor;

  TIntArray = array[0..255] of integer;
  PIntArray = ^TIntArray;

  TSBWServiceDescriptorArray = array[0..99999] of TSBWServiceDescriptor;
  PSBWServiceDescriptorArray = ^TSBWServiceDescriptorArray;

  TSBWModuleDescriptorArray = array[0..999999] of TSBWModuleDescriptor;
  PSBWModuleDescriptorArray = ^TSBWModuleDescriptorArray;

  //PSBWGetServiceDescriptor = ^TSBWGetServiceDescriptorArray;

  PSBWSignature = Pointer;
  PSBWSignatureType = Pointer;
  PSBWSignatureArgument = Pointer;
  TSBWSignatureArgumentArray = array[0..31] of PSBWSignatureArgument;
  PSBWSignatureArguments = ^TSBWSignatureArgumentArray;

  TMessageObject = class (TObject)
        len : integer;
        pData, root : PAnsiChar;
        ErrorCondition : boolean;
        constructor Create (args : SBWDataBlockReader);
        destructor Destroy; override;
        function   ExtractDouble : double;

        function   ReturnToCaller (const Args : array of const) : SBWDataBlockWriter;
  end;

  PInteger = ^Integer;

  TSBWConnect = function : boolean; cdecl;
  TSBWDisconnect = procedure; cdecl;
  TSBWConnectOnHost = function (hostNameOrIPAddress : PAnsiChar) : boolean;
  TSBWCreateSBWDataBlockWriter = function : SBWDataBlockWriter; cdecl;
  TSBWMethodCallRaw = function (moduleInstanceId, serviceId, method : integer; rawDataArgs : Pointer; var rawDataLength : integer) : PAnsiChar; cdecl;
  TSBWMethodSendRaw = function (moduleInstanceId, serviceId, methodId : integer; rawDataArgs : PAnsiChar; rawDataLength : integer) : boolean; cdecl;
  TSBWWriteRaw = procedure (SBWDataBlockWriter : Cardinal; rawData : PAnsiChar; rawDataLength : integer); cdecl;
  TSBWReadRaw = function  (SBWDataBlockReader : Cardinal; var rawDataLength : integer) : PAnsiChar; cdecl;

  TSBWCreateModuleImpl = function (uniqueName : PAnsiChar; nameForDisplay : PAnsiChar; SBWModuleManagementType : integer; helpStr : PAnsiChar) : boolean; cdecl;
  TSBWGetModuleInstance = function (moduleIdentificationName : PAnsiChar) : integer; cdecl;
  TSBWModuleFindServiceByName = function (moduleInstanceId : integer; serviceName : PAnsiChar) : integer; cdecl;
  TSBWServiceGetMethod = function (moduleInstanceId, serviceId : integer; signature : PAnsiChar) : integer; cdecl;
  TSBWFindServices  = function (serviceCategory : PAnsiChar; var numberOfServices : integer; recursive : integer) : PSBWServiceDescriptorArray; cdecl;
  TSBWFindLocalServices  = function (serviceCategory : PAnsiChar; var numberOfServices : integer; recursive : integer) : PSBWServiceDescriptorArray; cdecl;
  TSBWGetServiceInModuleInstance = function (service : PSBWServiceDescriptor; var moduleIdentifier, serviceIdentifier : integer) : integer; cdecl;
  TSBWGetExistingModuleInstances = function (var numberOfModuleInstances : integer) : PIntArray; cdecl;

  TSBWModuleImplEnableServices = function : boolean; cdecl;

  TSBWModuleGetModuleDescriptor = function  (moduleInstanceId : integer) : PSBWModuleDescriptor; cdecl;
  TSBWFreeModuleDescriptor = procedure (md : PSBWModuleDescriptor); cdecl;
  TSBWGetModuleDescriptors = function (includeRunning : integer; localCopy : integer; var numberOfDescriptors : integer) : PSBWModuleDescriptorArray; cdecl;
  TSBWGetServiceDescriptors = function (moduleIdName : PAnsiChar; var numberOfDescriptors : integer) : PSBWServiceDescriptorArray; cdecl;

  TSBWExceptionGetMessage = function : PAnsiChar; cdecl;
  TSBWExceptionGetDetailedMessage = function : PAnsiChar; cdecl;
  TSBWExceptionGetCode = function : integer; cdecl;
  TSBWSetException = procedure (userMessage, detailedMessage : PAnsiChar); cdecl;
  TSBWExceptionClear = procedure; cdecl;

  TSBWRegisterModuleShutdownListener = procedure (callBack : SBWModuleListener); cdecl;
  TSBWRegisterModuleStartupListener = procedure (callBack : SBWModuleListener); cdecl;
  TSBWRegisterShutdownListener = procedure (callBack : SBWSimpleListener); cdecl;
  TSBWRegisterRegistrationChangeListener = procedure (callBack : SBWSimpleListener); cdecl;
  TSBWModuleShutdown = function (moduleInstanceId : integer) : boolean; cdecl;

  TSBWServiceGetDescriptor = function (moduleInstanceId, serviceId : integer) : PSBWServiceDescriptor; cdecl;

  TSBWModuleImplRegister = function  : boolean; cdecl;

  TSBWModuleGetNumberOfServices = function (moduleInstanceId : integer) : integer; cdecl;
  TSBWMethodGetName = function (moduleInstanceId, serviceId, methodId : integer) : PAnsiChar; cdecl;
  TSBWMethodGetHelp = function (moduleInstanceId, serviceId, methodId : integer) : PAnsiChar; cdecl;

  TSBWModuleImplAddService = function (serviceName : PAnsiChar; serviceDisplayName : PAnsiChar; category : PAnsiChar; helpStr : PAnsiChar) : boolean; cdecl;
  TSBWModuleImplSetHandler = function (serviceName : PAnsiChar; methodPtr : SBWHandler; userData : PUserData; signature : PAnsiChar; synchronized : boolean; helpStr : PAnsiChar) : boolean; cdecl;
  TSBWModuleImplGetHandler = function (serviceName, methodName : PAnsiChar; var userData : TUserData) : SBWHandler; cdecl;
  TSBWModuleImplSetCommandLine = function (cmd : PAnsiChar) : integer; cdecl;
  TSBWModuleImplSetHost = function (cmd : PAnsiChar) : integer; cdecl;

  TSBWMethodGetSignature = function (ModuleId, SrviceId, MethodId : integer) : PAnsiChar; cdecl;
  TSBWMethodGetSignatureString = function (ModuleId, ServiceId, MethodId : integer) : PAnsiChar; cdecl;
  TSBWSignatureGetArguments = function (sig : Pointer; var numberOfArguments : integer) : PSBWSignatureArguments; cdecl;
  TSBWSignatureElementGetType = function (sigArgument : Pointer) : Pointer; cdecl;
  TSBWSignatureElementGetName = function (sigArgument : PSBWSignatureArguments) : PAnsiChar; cdecl;
  TSBWSignatureTypeGetType = function (SBWSignatureType : Pointer) : integer; cdecl;
  TSBWSignatureGetReturnType = function (signature : Pointer) : PSBWSignatureType; cdecl;
  TSBWSignatureGetName = function (signature : Pointer) : PAnsiChar; cdecl;

  TSBWFreeSBWSignature = procedure (x : PSBWSignature); cdecl;
  TSBWFreeSBWSignatureType = procedure (x : PSBWSignatureType); cdecl;
  TSBWFreeSBWSignatureArgumentArray = procedure (size : integer; var x : PSBWSignatureArgument); cdecl;

  TSBWServiceGetNumberOfMethods = function  (ModuleId, ServiceId : integer) : integer; cdecl;

  TSBWGetThisModule = function : integer; cdecl;

  TSBWGetCategories = function (serviceCategory : PAnsiChar; var numberOfCategories : integer) : Pointer;  cdecl;
  TSBWModuleFindServicesByCategory = function (moduleInstanceId : integer; category : PAnsiChar; var numberOfServices : integer) : PInteger; cdecl;

  TSBWFree = procedure (Data : PAnsiChar); cdecl;
  TSBWFreeArray = procedure (Data : PAnsiChar); cdecl;
  TSBWFreeServiceDescriptor = procedure (SBWServiceDescriptor : Pointer); cdecl;
  TSBWFreeServiceDescriptorArray = procedure (size : integer; SBWServiceDescriptors : Pointer); cdecl;


var
  SBWConnect : TSBWConnect;
  SBWDisconnect : TSBWDisconnect;
  SBWConnectOnHost : TSBWConnectOnHost;
  SBWCreateSBWDataBlockWriter : TSBWCreateSBWDataBlockWriter;
  SBWMethodCallRaw : TSBWMethodCallRaw;
  SBWMethodSendRaw : TSBWMethodSendRaw;
  SBWWriteRaw : TSBWWriteRaw;
  SBWReadRaw : TSBWReadRaw;

  SBWCreateModuleImpl : TSBWCreateModuleImpl;
  SBWGetModuleInstance : TSBWGetModuleInstance;
  SBWModuleFindServiceByName : TSBWModuleFindServiceByName;
  SBWServiceGetMethod : TSBWServiceGetMethod;
  SBWFindServices : TSBWFindServices;
  SBWFindLocalServices : TSBWFindLocalServices;
  SBWGetServiceInModuleInstance : TSBWGetServiceInModuleInstance;
  SBWModuleImplEnableServices : TSBWModuleImplEnableServices;
  SBWModuleGetModuleDescriptor : TSBWModuleGetModuleDescriptor;
  SBWFreeModuleDescriptor : TSBWFreeModuleDescriptor;
  SBWGetModuleDescriptors  : TSBWGetModuleDescriptors;
  SBWGetServiceDescriptors : TSBWGetServiceDescriptors;
  SBWExceptionGetMessage : TSBWExceptionGetMessage;
  SBWExceptionGetDetailedMessage : TSBWExceptionGetDetailedMessage;
  SBWExceptionGetCode : TSBWExceptionGetCode;
  SBWSetException : TSBWSetException;
  SBWExceptionClear : TSBWExceptionClear;

  SBWRegisterModuleShutdownListener : TSBWRegisterModuleShutdownListener;
  SBWRegisterModuleStartupListener : TSBWRegisterModuleStartupListener;
  SBWRegisterShutdownListener : TSBWRegisterShutdownListener;
  SBWRegisterRegistrationChangeListener : TSBWRegisterRegistrationChangeListener;
  SBWModuleShutdown : TSBWModuleShutdown;

  SBWServiceGetDescriptor : TSBWServiceGetDescriptor;
  SBWGetExistingModuleInstances : TSBWGetExistingModuleInstances;

  SBWModuleImplRegister : TSBWModuleImplRegister;
  SBWModuleGetNumberOfServices : TSBWModuleGetNumberOfServices;
  SBWMethodGetName : TSBWMethodGetName;
  SBWMethodGetHelp : TSBWMethodGetHelp;

  SBWModuleImplAddService : TSBWModuleImplAddService;
  SBWModuleImplSetHandler : TSBWModuleImplSetHandler;
  SBWModuleImplGetHandler : TSBWModuleImplGetHandler;
  SBWModuleImplSetCommandLine : TSBWModuleImplSetCommandLine;
  SBWModuleImplSetHost : TSBWModuleImplSetHost;

  SBWMethodGetSignature : TSBWMethodGetSignature;
  SBWMethodGetSignatureString : TSBWMethodGetSignatureString;
  SBWSignatureGetArguments : TSBWSignatureGetArguments;
  SBWSignatureElementGetType : TSBWSignatureElementGetType;
  SBWSignatureElementGetName : TSBWSignatureElementGetName;
  SBWSignatureTypeGetType : TSBWSignatureTypeGetType;
  SBWSignatureGetReturnType : TSBWSignatureGetReturnType;
  SBWSignatureGetName : TSBWSignatureGetName;

  SBWFreeSBWSignature : TSBWFreeSBWSignature;
  SBWFreeSBWSignatureType : TSBWFreeSBWSignatureType;
  SBWFreeSBWSignatureArgumentArray : TSBWFreeSBWSignatureArgumentArray;

  SBWServiceGetNumberOfMethods : TSBWServiceGetNumberOfMethods;

  SBWGetThisModule : TSBWGetThisModule;

  SBWGetCategories : TSBWGetCategories;
  SBWModuleFindServicesByCategory : TSBWModuleFindServicesByCategory;

  SBWFree : TSBWFree;
  SBWFreeArray : TSBWFreeArray;
  SBWFreeServiceDescriptor : TSBWFreeServiceDescriptor;
  SBWFreeServiceDescriptorArray : TSBWFreeServiceDescriptorArray;


  // --------------------------------------------------------------------------

function ReturnToCaller (const Args : array of const) : SBWDataBlockWriter;

// --------------------------------------------------------------------

function LoadSBW_DLL : boolean;
procedure UnLoadSBW_DLL;

implementation

var LibHandle : THandle;



function LoadSBW_DLL : boolean;
begin
  result := True;
{$ifdef MSWINDOWS}
  //LibHandle := LoadLibrary ('y:/experimental/sbw.dll');// - use with care/C++Broker/bin/sbwd.dll');
  LibHandle := LoadLibrary ('sbw.dll');
{$else}
  LibHandle := LoadLibrary (PChar (GetEnvironmentVariable ('HOME') + '/SBW/lib/libsbw.so'));
{$endif}
  if LibHandle = 0 then
     result := False
  else
     begin
     @SBWConnect := GetProcAddress (LibHandle, 'SBWConnect');
     @SBWDisconnect := GetProcAddress (LibHandle, 'SBWDisconnect');
     @SBWConnectOnHost := GetProcAddress (LibHandle, 'SBWConnectOnHost');
     @SBWCreateSBWDataBlockWriter := GetProcAddress (LibHandle, 'SBWCreateSBWDataBlockWriter');

     @SBWMethodCallRaw := GetProcAddress (LibHandle, 'SBWMethodCallRaw');
     @SBWMethodSendRaw := GetProcAddress (LibHandle, 'SBWMethodSendRaw');
     @SBWWriteRaw := GetProcAddress (LibHandle, 'SBWWriteRaw');
     @SBWReadRaw := GetProcAddress (LibHandle, 'SBWReadRaw');

     @SBWCreateModuleImpl := GetProcAddress (LibHandle, 'SBWCreateModuleImpl');
     @SBWGetModuleInstance := GetProcAddress (LibHandle, 'SBWGetModuleInstance');
     @SBWModuleFindServiceByName := GetProcAddress (LibHandle, 'SBWModuleFindServiceByName');
     @SBWServiceGetMethod := GetProcAddress (LibHandle, 'SBWServiceGetMethod');
     @SBWFindServices := GetProcAddress (LibHandle, 'SBWFindServices');
     @SBWFindLocalServices := GetProcAddress (LibHandle, 'SBWFindLocalServices');
     @SBWGetServiceInModuleInstance := GetProcAddress (LibHandle, 'SBWGetServiceInModuleInstance');
     @SBWModuleImplEnableServices := GetProcAddress (LibHandle, 'SBWModuleImplEnableServices');
     @SBWModuleGetModuleDescriptor := GetProcAddress (LibHandle, 'SBWModuleGetModuleDescriptor');
     @SBWFreeModuleDescriptor := GetProcAddress (LibHandle, 'SBWFreeModuleDescriptor');
     @SBWGetModuleDescriptors := GetProcAddress (LibHandle, 'SBWGetModuleDescriptors');
     @SBWGetServiceDescriptors := GetProcAddress (LibHandle, 'SBWGetServiceDescriptors');

     @SBWExceptionGetMessage := GetProcAddress (LibHandle, 'SBWExceptionGetMessage');
     @SBWExceptionGetDetailedMessage := GetProcAddress (LibHandle, 'SBWExceptionGetDetailedMessage');
     @SBWExceptionGetCode := GetProcAddress (LibHandle, 'SBWExceptionGetCode');
     @SBWSetException := GetProcAddress (LibHandle, 'SBWSetException');
     @SBWExceptionClear := GetProcAddress (LibHandle, 'SBWExceptionClear');

     @SBWRegisterModuleShutdownListener := GetProcAddress (LibHandle, 'SBWRegisterModuleShutdownListener');
     @SBWRegisterModuleStartupListener := GetProcAddress (LibHandle, 'SBWRegisterModuleStartupListener');
     @SBWRegisterShutdownListener := GetProcAddress (LibHandle, 'SBWRegisterShutdownListener');
     @SBWRegisterRegistrationChangeListener := GetProcAddress (LibHandle, 'SBWRegisterRegistrationChangeListener');
     @SBWModuleShutdown := GetProcAddress (LibHandle, 'SBWModuleShutdown');

     @SBWGetExistingModuleInstances := GetProcAddress (LibHandle, 'SBWGetExistingModuleInstances');
     @SBWServiceGetDescriptor := GetProcAddress (LibHandle, 'SBWServiceGetDescriptor');
     @SBWModuleImplRegister  := GetProcAddress (LibHandle, 'SBWModuleImplRegister');
     @SBWModuleGetNumberOfServices  := GetProcAddress (LibHandle, 'SBWModuleGetNumberOfServices');
     @SBWMethodGetName  := GetProcAddress (LibHandle, 'SBWMethodGetName');
     @SBWMethodGetHelp  := GetProcAddress (LibHandle, 'SBWMethodGetHelp');
     @SBWModuleImplAddService  := GetProcAddress (LibHandle, 'SBWModuleImplAddService');
     @SBWModuleImplSetHandler  := GetProcAddress (LibHandle, 'SBWModuleImplSetHandler');
     @SBWModuleImplGetHandler  := GetProcAddress (LibHandle, 'SBWModuleImplGetHandler');
     @SBWModuleImplSetCommandLine  := GetProcAddress (LibHandle, 'SBWModuleImplSetCommandLine');
     @SBWModuleImplSetHost  := GetProcAddress (LibHandle, 'SBWModuleImplSetHost');
     @SBWMethodGetSignature  := GetProcAddress (LibHandle, 'SBWMethodGetSignature');
     @SBWMethodGetSignatureString  := GetProcAddress (LibHandle, 'SBWMethodGetSignatureString');
     @SBWSignatureGetArguments  := GetProcAddress (LibHandle, 'SBWSignatureGetArguments');
     @SBWSignatureElementGetType  := GetProcAddress (LibHandle, 'SBWSignatureElementGetType');
     @SBWSignatureElementGetName := GetProcAddress (LibHandle, 'SBWSignatureElementGetName');
     @SBWSignatureTypeGetType  := GetProcAddress (LibHandle, 'SBWSignatureTypeGetType');
     @SBWSignatureGetReturnType  := GetProcAddress (LibHandle, 'SBWSignatureGetReturnType');
     @SBWSignatureGetName := GetProcAddress (LibHandle, 'SBWSignatureGetName');

     @SBWFreeSBWSignature  := GetProcAddress (LibHandle, 'SBWFreeSBWSignature');
     @SBWFreeSBWSignatureType  := GetProcAddress (LibHandle, 'SBWFreeSBWSignatureType');
     @SBWFreeSBWSignatureArgumentArray  := GetProcAddress (LibHandle, 'SBWFreeSBWSignatureArgumentArray');
     @SBWServiceGetNumberOfMethods  := GetProcAddress (LibHandle, 'SBWServiceGetNumberOfMethods');
     @SBWGetThisModule  := GetProcAddress (LibHandle, 'SBWGetThisModule');
     @SBWGetCategories  := GetProcAddress (LibHandle, 'SBWGetCategories');
     @SBWModuleFindServicesByCategory  := GetProcAddress (LibHandle, 'SBWModuleFindServicesByCategory');
     @SBWFree := GetProcAddress (LibHandle, 'SBWFree');
     @SBWFreeArray  := GetProcAddress (LibHandle, 'SBWFreeArray');
     @SBWFreeServiceDescriptor  := GetProcAddress (LibHandle, 'SBWFreeServiceDescriptor');
     @SBWFreeServiceDescriptorArray := GetProcAddress (LibHandle, 'SBWFreeServiceDescriptorArray');
     end;
end;


procedure UnLoadSBW_DLL;
begin
  FreeLibrary (LibHandle);
end;


// ------------------------------------------------------------------------


constructor TMessageObject.Create (args : SBWDataBlockReader);
begin
  inherited Create;
  ErrorCondition := False;
  pData := PAnsiChar (SBWReadRaw (args, len));
  root := pData;
end;


destructor TMessageObject.Destroy;
begin
  SBWFreeArray (root);
  inherited Destroy;
end;


function TMessageObject.ExtractDouble : double;
var str1, str2 : string;
begin
  try
    if Byte (pData^) <> dtDouble then
       raise Exception.Create ('Expecting ' + DataTypeToString (byte (pData^))); 
    inc (pData);  // Move pass DataType byte
    Move (pData^, Result, SizeOfDouble);
    pData := pData + SizeOfDouble;
  except
    on E: Exception do
       begin
       ErrorCondition := True;
       str1 := 'Expecting a double in the data message but ' + DataTypeToString (byte (pData^));
       str2 := 'Error raised during Double Extraction from DataStream: ' + E.message;
       SBWSetException(PAnsiChar (str1), PAnsiChar (str2));
       end;
  end;
end;


function TMessageObject.ReturnToCaller (const Args : array of const) : SBWDataBlockWriter;
var ds : TSBWDataStream;
begin
  if ErrorCondition then
     begin
     result := Cardinal (nil);
     Exit;
     end;

  ds := TSBWDataStream.Create (Args);
  try
    result := SBWCreateSBWDataBlockWriter();
    SBWWriteRaw(result, PAnsiChar (ds.Data.d), ds.Data.len);
  finally
    ds.Free;
  end;
end;


function ReturnToCaller (const Args : array of const) : SBWDataBlockWriter;
var ds : TSBWDataStream;
begin
  ds := TSBWDataStream.Create (Args);
  try
    result := SBWCreateSBWDataBlockWriter();
    SBWWriteRaw(result, PAnsiChar (ds.Data.d), ds.Data.len);
  finally
    ds.Free;
  end;
end;

// ------------------------------------------------------------------------

end.
