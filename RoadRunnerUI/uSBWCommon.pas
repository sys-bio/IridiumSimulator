unit uSBWCommon;

// SBW
// Common constants, types etc

interface

Uses SysUtils, Classes, contnrs, SyncObjs, uSBWComplex;

const
   SBW_BrokerId = -1;
   SBW_SystemServiceId = -1;
   SBW_ServicesId = 0;
   SBW_MethodsId = 1;

   SystemMethodsId = 0;
   BrokerServiceId = 0;

   dtTerminator = $FF;
   dtNull = dtTerminator;
   // See DataBlockType.h for values
   dtByte = Byte (0);
   dtInteger = Byte(1);
   dtDouble  = Byte(2);
   dtBoolean = Byte(3);
   dtString  = Byte(4);
   dtArray   = Byte(5);
   dtList    = Byte(6);
   dtVoid    = Byte(7);
   dtComplex = Byte(8);
   dtError   = Byte(-2);

   // Additional codes to deal with Delphi/C++ Builder SBW extensions
   //dtComplex = Byte(9);
   dtDMatrix = Byte(10);
   dtCMatrix = Byte(11);

   dtDArray = Byte (254);
   dtIArray = Byte (253);
   dtDDArray = Byte (252);
   dtIIArray = Byte (251);

   dmatrixEncodingString = '{byte __dmatrix}';
   cmatrixEncodingString = '{byte __cmatrix}';

   SizeOfByte = sizeof (Byte);
   SizeOfDouble = sizeof (Double);
   SizeOfInteger = sizeof (Integer);
   SizeOfBoolean = sizeof (Boolean);
   SizeOfComplex = sizeof (TSBWComplex);

   CALL_TIMEOUT = 30000;

type
   TExceptionCode = (ApplicationExceptionCode = 0, //*< generated by a module ie not raised by the  infastructure */
    	RawExceptionCode = 1, //*< generated by a OS or unhandled exception */
	    CommunicationExceptionCode = 2, //*< communication between modules has been disrupted */
	    ModuleStartExceptionCode = 3, //*< unable to start module when a new module instance was required */
	    TypeMismatchExceptionCode = 4, //*< the data contained in a datablock doesn't correspond to the requested type */
	    IncompatibleMethodSignatureExceptionCode = 5, //*< two method signatures do not match */
	    ModuleIdSyntaxExceptionCode = 6, //*< the syntax of a module instance identifier string is incorrect */
	    IncorrectCategorySyntaxExceptionCode = 7, //*< the syntax of a service category string is incorrect */
	    ServiceNotFoundExceptionCode = 9, //*< the requested service doesn't exist */
	    MethodTypeNotBlockTypeExceptionCode = 10, //*< thrown during communications if a supplied class uses types which are not data block types (not raised in C++ library) */
	    MethodAmbiguousExceptionCode = 11, //*< the given signature matches more than one method on a service */
	    UnsupportedObjectTypeExceptionCode = 12, //*< the given valid type is not supported by a client library */
	    MethodNotFoundExceptionCode = 13, //*< the given signature or name doesn't match any method on a service */
	    SignatureSyntaxExceptionCode = 14, //*< the syntax of the given signature is incorrect */
	    ModuleDefinitionExceptionCode = 15,	 //*< exception thrown when an attempt to define a new module fails */
	    ModuleNotFoundExceptionCode = 16, //*< the requested module doesn't exist */
	    BrokerStartExceptionCode = 17 //*< unable to start broker */
  );


  SBWDataBlockReader = Cardinal;
  SBWDataBlockWriter = Cardinal;

  TByteArray = array[0..MaxInt-1] of byte;
  PByteArray = ^TByteArray;
   
  TCallType = (utFunction, utMethod);
  
  ESBWException = class (Exception)
         errorCode : integer;
         constructor Create (errorCode : integer; errorMsg : string); overload;
         constructor Create (errorMsg : string); overload;
  end;
  ESBWConnectError = class (ESBWException);
  ESBWExtractionError = class (ESBWException);

  PDataStream = PAnsiChar;

  TStringRow = array[0..31] of string;
  PStringRow = ^TStringRow;
  PDoubleRow = array[0..0] of double;

  PArray = array[0..0] of Pointer;
  IntArray = array of integer;
  TServiceRecord = class (TObject)
         ModuleId : integer;
         ServiceId : integer;
         name : string;
         helpStr : string;
  end;

  TMethodRecord = class (TObject)
          ModuleId : integer;
          ServiceId : integer;
          MethodId : integer;
          helpStr : string;

          moduleName : string;
          serviceName : string;
          methodName : string;
  end;

  TModuleDescriptorObj = class (TObject)
        name : string;
  	displayName : string;
  	managementType : string;
  	commandLine : string;
        helpStr : string;
        destructor Destroy; override;
  end;

    TServiceDescriptorRecord = record
              serviceName : string;
	     serviceDisplayName : string;               // humanly readable service name
	        serviceCategory : string;               //category of service
	                 module : TModuleDescriptorObj; // module that implements service
                   	help : string;                // help string (documentation) for service
  end;
  PServiceDescriptorRecord = TServiceDescriptorRecord;

  TServiceDescriptorObj = class (TObject)
                    serviceName : string;                // service identification name
	     serviceDisplayName : string;               // humanly readable service name
	        serviceCategory : string;               //category of service
	                 module : TModuleDescriptorObj; // module that implements service
                    	help : string;                // help string (documentation) for service
                destructor Destroy; override;
  end;


  TData = record
            d : PByteArray;
            len : integer;
            ptr : integer;
          end;

implementation


constructor ESBWException.Create (errorCode : integer; errorMsg : string);
begin
  inherited Create (errorMsg);
  self.errorCode := errorCode;
end;


constructor ESBWException.Create (errorMsg : string);
begin
  inherited Create (errorMsg);
  self.errorCode := 0;
end;


destructor TModuleDescriptorObj.Destroy;
begin
  inherited Destroy;
end;


destructor TServiceDescriptorObj.Destroy;
begin
  module.Free;
  inherited Destroy;
end;


end.
