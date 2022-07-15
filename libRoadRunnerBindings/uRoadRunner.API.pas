unit uRoadRunner.API;

{ Copyright 2012-2022 Herbert M Sauro

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

   In plain english this means:

   You CAN freely download and use this software, in whole or in part, for personal,
   company internal, or commercial purposes;

   You CAN use the software in packages or distributions that you create.
   You SHOULD include a copy of the license in any redistribution you may make;
   You are NOT required include the source of software, or of any modifications you may
   have made to it, in any redistribution you may assemble that includes it.

   YOU CANNOT:

   redistribute any piece of this software without proper attribution;
}

interface

Uses SysUtils, Classes, Generics.Collections, IOUtils, uRRList,
     uRRTypes, uRoadRunner
     //{$IFDEF VER220} // Delphi XE
     //  Dialogs
     //{$ELSE}   // > Delphi XE
    //    FMX.Dialogs
    //{$ENDIF}
    ;
type
  TRoadRunnerAPI = class
    private class var
       FLibHandle : HModule;
    public const
    {$IF DEFINED(MSWINDOWS)}
    LibName = 'libRoadRunner\bin\roadrunner_c_api.dll';
    {$ELSEIF DEFINED(MACOS)}
      {$IF DEFINED(IOS) and DEFINED(CPUARM)}
      LibName = 'sk4d.a';
      {$ELSE}
      LibName = 'libroadrunner_c_api.dylib';
      {$ENDIF}
    {$ELSE}
    LibName = 'libsk4d.so';
    {$ENDIF}
    public
       class procedure Initialize;
       class procedure Terminate;

       class var libGetVersionStr : function : PAnsiChar; cdecl;
       class var libGetVersionInt : function : integer; cdecl;
       class var libGetLastError : function : PAnsiChar; cdecl;
       class var libGetCopyright : function : PAnsiChar; cdecl;
       class var libGetlibSBMLVersion : function : PAnsiChar; cdecl;

       class var libCreateRRInstance : function : Pointer; cdecl;
       class var libFreeRRInstance : procedure (instance : Pointer); cdecl;
       class var libLoadSBMLFromString : function (rrHandle : Pointer; str : PAnsiChar) : boolean; cdecl;
       class var libLoadSBMLFromFile : function (rrHandle : Pointer; str : PAnsiChar) : boolean; cdecl;
       class var libGetSBML : function (rrHandle : Pointer) : PAnsiChar; cdecl;

       class var libSetCurrentIntegrator : function (rrHandle : Pointer; str : PAnsiChar) : integer; cdecl;

       class var libGetFloatingSpeciesIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;
       class var libGetBoundarySpeciesIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;
       class var libGetReactionIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;
       class var libGetCompartmentIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;
       class var libGetRatesOfChangeIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;
       class var libGetEigenvalueIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;

       class var libSetCompartmentByIndex     : function (rrHandle : Pointer; index : integer; value : double) : boolean; cdecl;
       class var libSetFloatingSpeciesByIndex : function (rrHandle : Pointer; index : integer; value : double) : boolean; cdecl;
       class var libSetBoundarySpeciesByIndex : function (rrHandle : Pointer; index : integer; value : double) : boolean; cdecl;
       class var libSetGlobalParameterByIndex : function (rrHandle : Pointer; index : integer; value : double) : boolean; cdecl;
       class var libGetCompartmentByIndex     : function (rrHandle : Pointer; index : integer; var value : double) : boolean; cdecl;
       class var libGetGlobalParameterByIndex : function (rrHandle : Pointer; index : integer; var value : double) : boolean; cdecl;
       class var libGetFloatingSpeciesByIndex : function (rrHandle : Pointer; index : integer; var value : double) : boolean; cdecl;
       class var libGetBoundarySpeciesByIndex : function (rrHandle : Pointer; index : integer; var value : double) : boolean; cdecl;
       class var libGetBoundarySpeciesConcentrations : function (rrHandle : Pointer) : PRRDoubleVectorHandle; cdecl;
       class var libSetFloatingSpeciesConcentrations : function (rrHandle : Pointer; values : PRRDoubleVectorHandle) : boolean; cdecl;
       class var libSetBoundarySpeciesConcentrations : function (rrHandle : Pointer; values : PRRDoubleVectorHandle) : boolean; cdecl;
       class var libGetNumberOfDependentSpecies : function (rrHandle : Pointer) : integer; cdecl;
       class var libGetNumberOfIndependentSpecies : function (rrHandle : Pointer) : integer; cdecl;

       class var libGetElasticityIds : function (rrHandle : Pointer) : PRRListRecordHandle; cdecl;
       class var libGetGlobalParameterIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;

       class var libGetNumberOfReactions : function (handle : Pointer) : integer; cdecl;
       class var libGetNumberOfFloatingSpecies : function (handle : Pointer) : integer; cdecl;
       class var libGetNumberOfGlobalParameters : function (handle : Pointer) : integer; cdecl;
       class var libGetNumberOfCompartments : function (handle : Pointer) : integer; cdecl;
       class var libGetNumberOfBoundarySpecies : function (handle : Pointer) : integer; cdecl;

       class var libReset : function (rrHandle : Pointer) : boolean; cdecl;
       class var libSetValue : function (rrHandle : Pointer; speciesId : PAnsiChar; value : double) : boolean; cdecl;
       class var libGetValue : function (rrHandle : Pointer; speciesId : PAnsiChar; value : PDouble) : boolean; cdecl;

       class var libSetFloatingSpeciesInitialConcentrations : function (rrHandle : Pointer; value : Pointer) : boolean; cdecl;
       class var libGetFloatingSpeciesInitialConcentrations : function (rrHandle : Pointer) : PRRDoubleVectorHandle; cdecl;

       class var libGetTimeCourseSelectionList : function (internalRRHandle : Pointer) : PRRStringArray; cdecl;
       class var libSetTimeCourseSelectionListEx : function (rrHandle : Pointer; len : integer; list : PPAnsiChar) : boolean; cdecl;

       class var libSetSteadyStateSelectionListEx : function (rrHandle : Pointer; len : integer; list : PPAnsiChar) : boolean; cdecl;
       class var libSetSteadyStateSelectionList : function (rrHandle : Pointer; str : PAnsiChar) : boolean; cdecl;
       class var libGetSteadyStateSelectionList : function (internalRRHandle : Pointer) : PRRStringArray; cdecl;

       class var libGetFloatingSpeciesConcentrations : function (rrHandle : Pointer) : PRRDoubleVectorHandle; cdecl;
       class var libGetFloatingSpeciesInitialConditionIds : function(rrHandle : Pointer) : PRRStringArray; cdecl;

       class var libSimulate :function (rrHandle : Pointer) : Pointer; cdecl;
       class var libSimulateEx : function (rrHandle : Pointer; timeStart : double; timeEnd : double; numberOfPoints : integer) : PRRCData; cdecl;
       class var libSetTimeStart : function (rrHandle : Pointer; value : double) : boolean; cdecl;
       class var libSetTimeEnd  : function (rrHandle : Pointer; value : double) : boolean; cdecl;
       class var libSetNumberOfPoints : function (rrHandle : Pointer; value : integer) : boolean; cdecl;
       class var libOneStep : function (rrHandle : Pointer; currentTime, stepSize : double; var newTime : double) : integer; cdecl;
       class var libGetReactionRates : function (rrHandle : Pointer) : PRRDoubleVectorHandle; cdecl;
       class var libGetReactionRate : function (rrHandle : Pointer; index : integer) : double; cdecl;
       class var libGetRatesOfChange : function (rrHandle : Pointer) : PRRDoubleVectorHandle; cdecl;

       class var libSteadyState : function (rrHandle : Pointer; var value : double) : boolean; cdecl;
       class var libComputeSteadyStateValues : function (rrHandle : Pointer): PRRDoubleVectorHandle; cdecl;

       class var libSetComputeAndAssignConservationLaws : function (rrHandle : pointer; value : boolean) : boolean; cdecl;
       class var libGetComputeAndAssignConservationLaws : function (internalRRHandle : Pointer; var value : integer) : integer; cdecl;

       class var libGetEigenvalues : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetEigenvectors : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetEigenvaluesMatrix   : function (mat : PRRMatrixHandle) : PRRMatrixHandle; cdecl;

       class var libGetScaledElasticityMatrix : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetCCCMatrix : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetFCCMatrix : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetFullJacobian : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetReducedJacobian : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;

       class var libGetStoichiometryMatrix : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetLinkMatrix          : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetNrMatrix            : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetL0Matrix            : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetConservationMatrix  : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetuFCCMatrix          : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetuCCCMatrix          : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;
       class var libGetuEEMatrix           : function (rrHandle : Pointer) : PRRMatrixHandle; cdecl;

       class var libGetList : function (item : PRRListItemRecord) : PRRListRecordHandle;
       class var libGetListItem : function (list : PRRListRecordHandle; index : integer) : PRRListItemRecord;
       class var libGetStringListItem : function (list : PRRListItemRecord) : PAnsiChar;
       class var libGetIntegerListItem : function (list : PRRListRecordHandle; index : integer) : integer;
       class var libGetListLength : function (list : PRRListRecordHandle) : integer; cdecl;
       class var libIsListItemString : function (item : PRRListItemRecord) : integer; cdecl;
       class var libIsListItemInteger : function (item : PRRListItemRecord) : integer; cdecl;
       class var libIsListItemDouble : function (item : PRRListItemRecord) : integer; cdecl;
       class var libIsListItemList : function (item : PRRListItemRecord) : integer; cdecl;

       class var libGetMatrixNumRows : function (matrix : PRRMatrixHandle) : integer; cdecl;
       class var libGetMatrixNumCols : function (matrix : PRRMatrixHandle) : integer; cdecl;
       class var libGetMatrixElement : function (matrix : PRRMatrixHandle; r, c : integer; var element : double) : integer; cdecl;

       class var libCreateVector : function (size : integer) : PRRDoubleVectorHandle;  cdecl;
       class var libFreeMatrix : function (matrix : PRRMatrixHandle) : boolean; cdecl;
       class var libFreeStringArray : function (handle : PRRStringArray) : boolean; cdecl;
       class var libFreeRRList : procedure (handle : PRRListRecordHandle); cdecl;
       class var libFreeRCData : function (ptr : pointer{PRRCData}) : boolean; cdecl;

       class var libFreeDoubleVector : function (vector : PRRDoubleVectorHandle) : boolean ; cdecl;
  end;

  //TVoidCharFunc = function : PAnsiChar; cdecl;   // char* func(void)
  THandleCharFunc = function (handle : Pointer) : PAnsiChar; cdecl;   //char* func(void)
  TVoidBoolFunc = function (rrHandle : Pointer) : boolean; cdecl; // bool func (void);
  TVoidIntFunc = function : integer; cdecl;
  THandleIntFunc = function (handle : Pointer) : integer; cdecl;
  TVoidDoubleFunc = function : double; cdecl;
  TIntHandleChar = function (rrHandle : pointer; str : PAnsiChar) : Integer; cdecl;
  TBoolBoolFunc = function (rrHandle : pointer; value : boolean) : boolean; cdecl;
  THandlePointerFunc = function (rrHandle : Pointer) : Pointer; cdecl; // void* func(void)
  TCharBoolFunc = function (rrHandle : Pointer; str : PAnsiChar) : boolean; cdecl;  // bool func (char *)
  TDoubleBoolFunc = function (value : double) : boolean; cdecl; // bool func (double)
  TIntBoolFunc = function (value : integer) : boolean; cdecl;   // bool func (double)
  TVarIntBoolFunc = function (var value : integer) : boolean; cdecl;   // bool func (double)
  THandleIntDoubleFunc = function (rrHandle : Pointer; index : integer) : double; cdecl;
  THandleStringListFunc = function(rrHandle : Pointer) : PRRStringArray; cdecl;
  TGetSetMatrix = function (rrHandle : Pointer; mat : PRRMatrixHandle) : PRRMatrixHandle; cdecl;
  TFreeRRCData = function (ptr : pointer{PRRCData}) : boolean; cdecl;
  TFreeRRInstance = procedure (instance : Pointer); cdecl;
  THandleVectorFunc = function (rrHandle : Pointer) : PRRDoubleVectorHandle; cdecl;
  TSetSelectionList = function (rrHandle : Pointer; list : PAnsiChar) : boolean; cdecl;
  TSetSelectionListEx = function (rrHandle : Pointer; len : integer; list : PPAnsiChar) : boolean; cdecl;
  TGetReactionIds = THandlePointerFunc;
  TFreeStringArray = function (handle : PRRStringArray) : boolean; cdecl;
  TFreeRRMatrix = function (matrix : PRRMatrixHandle) : boolean; cdecl;
  TFreeRRDoubleVector = function (vector : PRRDoubleVectorHandle) : boolean ; cdecl;
  TFreeListPtr = procedure (handle : PRRListRecordHandle); cdecl;
  TGetMCA = function (rrHandle : Pointer; variable : PAnsiChar; parameter : PAnsiChar; var value : double) : boolean; cdecl;
var
   DLLLoaded : boolean;
   loggingEnabled : boolean = false;
   loggingTmpFileName : AnsiString = '';
   rrHandle : Pointer;
   internalRRHandle : Pointer;

function  hasError : boolean;
//function  createRRInstance : Pointer;
//function  createInternalRRInstance : pointer;
//procedure freeRRInstance; overload;
procedure freeRRInstance (myInstance : Pointer); overload;


{$REGION 'Documentation'}
///	<summary>
///	  Get copyright string
///	</summary>
///	<returns>
///	  Copyright string
///	</returns>
{$ENDREGION}
function  enableLoggingToFileWithPath (path : AnsiString) : boolean;
function  setLogLevel (debugLevel : AnsiString) : boolean;
function  getLogFileName : AnsiString;
function  setLogFileName (value : AnsiString) : boolean;
function  setTempFolder (name : AnsiString) : boolean;
{$REGION 'Documentation'}
///	<summary>
///	  Returns the generated C Code for the model
///	</summary>
{$ENDREGION}
procedure clear;
function  evalModel : boolean;


function  setSteadyStateSelectionList (strList : TStringList) : boolean;

function  getuCC (variable : AnsiString; parameter : AnsiString) : double;
function  getCC (variable : AnsiString; parameter : AnsiString) : double;
function  getuEE (variable : AnsiString; parameter : AnsiString) : double;
function  getEE (variable : AnsiString; parameter : AnsiString) : double;

function  getAvailableSymbolsII : TRRList;
function  getAvailableTimeCourseSymbols : TRRList;
function  getAvailableSteadStateSymbols : TRRList;
function  getComputeAndAssignConservationLaws : Boolean;

procedure setConfigBoolean (key : string; value : Boolean);
procedure setConfigInteger (key : string; value : integer);
procedure setConfigDouble (key : string; value : double);
function  getConfigBoolean (key : string) : Boolean;
function  getConfigInteger (key : string) : Integer;
function  getConfigDouble (key : string) : Double;

procedure setRoadRunnerLibraryName (newLibName : AnsiString);
function  loadRoadRunner (var errMsg : AnsiString; methodList : TStringList) : boolean;
procedure releaseRoadRunnerLibrary;
procedure setMatrixElement ();  // Not implemented
// -----------------------------------------------------------------
// Integrator options API
// -----------------------------------------------------------------
function  getListOfRegisteredIntegrators : TStringList;
function  getNumberOfIntegrators : integer;
function  setIntegrator (name : string) : boolean;
function  getCurrentIntegratorName : string;
function  getIntegratorDescription : string;
function  getIntegratorHint : string;
function  getNumberOfIntegratorParameters : integer;
function 	getCurrentIntegratorNthParameterName (index : integer) : string;
function  getListOfIntegratorParameterNames : TStringList;
function  getIntegratorParameterDescription (parameterName : string) : string;
function  getIntegratorParameterHint (parameterName : string) : string;
// 0-STRING, 1-BOOL, 2-INT32, 3-UINT32, 4-INT64, 5-UINT64, 6-FLOAT, 7-DOUBLE, 8-CHAR, 9-UCHAR, 10-EMPTY
function  getIntegratorParameterType (parameterName : string) : integer;
function  getIntegratorParameterInt (parameterName : string) : integer;
procedure setIntegratorParameterInt (parameterName : string; value : integer);
function  getIntegratorParameterUInt (parameterName : string) : UInt32;
procedure setIntegratorParameterUInt (parameterName : string; value : UInt32);
function  getIntegratorParameterDouble (parameterName : string) : double;
procedure setIntegratorParameterDouble (parameterName : string; value : double);
function  getIntegratorParameterString (parameterName : string) : string;
procedure setIntegratorParameterString (parameterName : string; value : string);
function  getIntegratorParameterBoolean (parameterName : string) : Boolean;
procedure setIntegratorParameterBoolean (parameterName : string; value : Boolean);
// -----------------------------------------------------------------
// Steady State options API
// -----------------------------------------------------------------
function  getListOfRegisteredSteadyStateSolvers : TStringList;
function  getNumberOfSteadyStateSolvers : integer;
procedure setSteadyStateSolver (name : string);
function  getCurrentSteadyStateSolverName : string;
function  getSteadyStateSolverDescription : string;
function  getSteadyStateSolverHint : string;
function  getNumberOfSteadyStateSolverParameters : integer;
function  getListOfSteadyStateSolverParameterNames : TStringList;
function  getCurrentSteadyStateNthParameterName (index : integer) : string;
function  getSteadyStateSolverParameterDescription (parameterName : string) : string;
function  getSteadyStateSolverParameterHint (parameterName : string) : string;
// 0-STRING, 1-BOOL, 2-INT32, 3-UINT32, 4-INT64, 5-UINT64, 6-FLOAT, 7-DOUBLE, 8-CHAR, 9-UCHAR, 10-EMPTY
function  getSteadyStateSolverParameterType (parameterName : string) : integer;

function  getSteadyStateSolverParameterInt (parameterName : string) : integer;
procedure setSteadyStateSolverParameterInt (parameterName : string; value : integer);
function  getSteadyStateSolverParameterUInt (parameterName : string) : UInt32;
procedure setSteadyStateSolverParameterUInt (parameterName : string; value : UInt32);
function  getSteadyStateSolverParameterDouble (parameterName : string) : double;
procedure setSteadyStateSolverParameterDouble (parameterName : string; value : double);
function  getSteadyStateSolverParameterString (parameterName : string) : string;
procedure setSteadyStateSolverParameterString (parameterName : string; value : string);
function  getSteadyStateSolverParameterBoolean (parameterName : string) : Boolean;
procedure setSteadyStateSolverParameterBoolean (parameterName : string; value : Boolean);
function  solverTypeToString (code : Integer) : string;

implementation

Uses
  {$IFDEF POSIX}
     Posix.Dlfcn,
  {$ELSE}
     Windows,
  {$ENDIF}
  FMX.Dialogs;

type
  TLibGetAvailableSymbols = function (rrHandle : Pointer) : PRRListRecordHandle; cdecl;
  TlibSetInitialConditions = function (rrHandle : Pointer; vec : PRRDoubleVectorHandle) : boolean; cdecl;
  TlibComputeSteadyStateValues = function (rrHandle : Pointer): PRRDoubleVectorHandle; cdecl;

var DLLHandle : HMODULE;
    libName : string;
    currentLoggingPath : AnsiString;
    libHasError : TVoidBoolFunc;
    libEnableLoggingToConsole : TVoidBoolFunc;
    libEnableLoggingToFile : function : integer; cdecl;
    libEnableLoggingToFileWithPath : function (str : PAnsiChar) : integer; cdecl;
    libSetLogLevel : function (str : PAnsiChar) : integer; cdecl;
    libGetLogFileName : function : PAnsiChar; cdecl;
    libSetLogFileName : function (str : PAnsiChar) : Integer; cdecl;
    libSetTempFolder : function (rrHandle : Pointer; folder : PAnsiChar) : boolean; cdecl;
    libFreeRRInstance : TFreeRRInstance;
    libFreeRCData : TFreeRRCData;
    libSimulate : THandlePointerFunc;
    libGetCapabilities : THandleCharFunc;
    libSetCapabilities : TCharBoolFunc;
    libEvalModel : TVoidBoolFunc;

    libSetSteadyStateSelectionList : TCharBoolFunc;
    libGetTimeCourseSelectionList  : function (internalRRHandle : Pointer) : PRRStringArray; cdecl;
    libGetAvailableTimeCourseSymbols : TLibGetAvailableSymbols;
    libGetAvailableSteadyStateSymbols : TLibGetAvailableSymbols;
    libSetInitialConditions : TlibSetInitialConditions;

    libgetuCC                 : TGetMCA;
    libgetuEE                 : TGetMCA;
    libgetCC                  : TGetMCA;
    libgetEE                  : TGetMCA;

    libSetConfigBool          : function (key : PAnsiChar; value : integer) : integer; cdecl;
    libSetConfigInt           : function (key : PAnsiChar; value : integer) : integer; cdecl;
    libSetConfigDouble        : function (key : PAnsiChar; value : double) : integer; cdecl;
    libGetConfigBool          : function (key : PAnsiChar) : integer; cdecl;
    libGetConfigInt           : function (key : PAnsiChar) : integer; cdecl;
    libGetConfigDouble        : function (key : PAnsiChar) : double; cdecl;

    // -----------------------------------------------------------------
    // C API Integrator Options methods
    // -----------------------------------------------------------------
    libGetNumRegisteredIntegrators : function : integer; cdecl;
    libGetRegisteredIntegratorName : function (n : integer) : PAnsiChar; cdecl;
    libGetRegisteredIntegratorHint : function (n : integer) : PAnsiChar; cdecl;
    libGetRegisteredIntegratorDescription : function (n : integer) : PAnsiChar; cdecl;
    libGetNumberOfIntegrators : THandleIntFunc;
    //libSetCurrentIntegrator : TIntHandleChar;
    libGetCurrentIntegratorName : function (internalRRHandle : Pointer) : PAnsiChar; cdecl;
    libGetCurrentIntegratorDescription : THandleCharFunc;
    libGetCurrentIntegratorHint : THandleCharFunc;
    libGetNumberOfCurrentIntegratorParameters : THandleIntFunc;
    libGetCurrentIntegratorNthParameterName : function (internalRRHAndle : Pointer; index : Integer): PAnsiChar; cdecl;
    libGetListOfCurrentIntegratorParameterNames : THandleStringListFunc;
    libGetCurrentIntegratorParameterDescription  : function (internalRRHandle : Pointer; name : PAnsiChar) : PAnsiChar; cdecl;
    libGetCurrentIntegratorParameterHint  : function (internalRRHandle : Pointer; name : PAnsiChar) : PAnsiChar; cdecl;
    libGetCurrentIntegratorParameterType : function (internalRRHandle : pointer; name : PAnsiChar) : Integer; cdecl;
    libGetCurrentIntegratorParameterInt :    function (internalRRHandle : Pointer; name : PAnsiChar) : integer; cdecl;
    libSetCurrentIntegratorParameterInt :    function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : integer) : Boolean; cdecl;
    libGetCurrentIntegratorParameterUInt :    function (internalRRHandle : Pointer; name : PAnsiChar) : cardinal; cdecl;
    libSetCurrentIntegratorParameterUInt :    function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : cardinal) : Boolean; cdecl;
    libGetCurrentIntegratorParameterDouble : function (internalRRHandle : Pointer; parameterName : PAnsiChar) : double; cdecl;
    libSetCurrentIntegratorParameterDouble : function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : double) : boolean; cdecl;
    libGetCurrentIntegratorParameterString : function (internalRRHandle : Pointer; parameterName : PAnsiChar) : PAnsiChar; cdecl;
    libSetCurrentIntegratorParameterString : function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : PAnsiChar) : boolean; cdecl;
    libGetCurrentIntegratorParameterBoolean :function (internalRRHandle : Pointer; parameterName : PAnsiChar) : Boolean; cdecl;
    libSetCurrentIntegratorParameterBoolean :function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : integer)  : Boolean; cdecl;
    // -----------------------------------------------------------------
    // C API SteadyState Solver Options methods
    // -----------------------------------------------------------------
    libGetNumRegisteredSteadyStateSolvers : function : integer; cdecl;
    libGetRegisteredSteadyStateSolverName : function (n : integer) : PAnsiChar; cdecl;
    libGetRegisteredSteadyStateSolverHint : function (n : integer) : PAnsiChar; cdecl;
    libGetRegisteredSteadyStateSolverDescription : function (n : integer) : PAnsiChar; cdecl;
    libGetNumberOfSteadyStateSolvers : THandleIntFunc;
    libSetCurrentSteadyStateSolver : TIntHandleChar;
    libGetCurrentSteadyStateSolverName : THandleCharFunc;
    libGetCurrentSteadyStateSolverDescription : function (handle : Pointer) : PAnsiChar; cdecl;
    libGetCurrentSteadyStateSolverHint : THandleCharFunc;
    libGetNumberOfCurrentSteadyStateSolverParameters : THandleIntFunc;
    libGetListOfCurrentSteadyStateSolverParameterNames : THandleStringListFunc;
    libGetCurrentSteadyStateSolverNthParameterName : function (internalRRHAndle : Pointer; index : Integer): PAnsiChar; cdecl;
    libGetCurrentSteadyStateSolverParameterDescription  : function (internalRRHandle : Pointer; name : PAnsiChar) : PAnsiChar; cdecl;
    libGetCurrentSteadyStateSolverParameterHint  : function (internalRRHandle : Pointer; name : PAnsiChar) : PAnsiChar; cdecl;
    libGetCurrentSteadyStateSolverParameterType : function (internalRRHandle : pointer; name : PAnsiChar) : Integer; cdecl;
    libGetCurrentSteadyStateSolverParameterInt :    function (internalRRHandle : Pointer; name : PAnsiChar) : integer; cdecl;
    libSetCurrentSteadyStateSolverParameterInt :    function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : integer) : Boolean; cdecl;
    libGetCurrentSteadyStateSolverParameterUInt :    function (internalRRHandle : Pointer; name : PAnsiChar) : cardinal; cdecl;
    libSetCurrentSteadyStateSolverParameterUInt :    function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : cardinal) : Boolean; cdecl;
    libGetCurrentSteadyStateSolverParameterDouble : function (internalRRHandle : Pointer; parameterName : PAnsiChar) : double; cdecl;
    libSetCurrentSteadyStateSolverParameterDouble : function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : double) : boolean; cdecl;
    libGetCurrentSteadyStateSolverParameterString : function (internalRRHandle : Pointer; parameterName : PAnsiChar) : PAnsiChar; cdecl;
    libSetCurrentSteadyStateSolverParameterString : function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : PAnsiChar) : boolean; cdecl;
    libGetCurrentSteadyStateSolverParameterBoolean :function (internalRRHandle : Pointer; parameterName : PAnsiChar) : Boolean; cdecl;
    libSetCurrentSteadyStateSolverParameterBoolean :function (internalRRHandle : Pointer; parameterName : PAnsiChar; value : integer)  : Boolean; cdecl;
    // Generic methods for options API
    // ----------------------------------------------------------------
    libSolverTypeToString : function (code : integer) : PAnsiChar; cdecl;
    // ----------------------------------------------------------------
    //libCreateVector : function (size : integer) : PRRDoubleVectorHandle;  cdecl;
    libCreateRRMatrix : function (row, col : integer) : PRRMatrixHandle; cdecl;
    libSetMatrixElement : function (m : PRRMatrixHandle; r, c : integer; value : double) : boolean; cdecl;
    libGetListItem : function (list : pointer; index : integer) : PRRListItemRecord; cdecl;
    libFreeText : TCharBoolFunc;
    libFreeDoubleVector : TFreeRRDoubleVector;



function GetProcAddress(AModule: HMODULE; AName: System.PChar): Pointer;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.GetProcAddress(AModule, AName);
  {$ELSE}
  Result := System.SysUtils.GetProcAddress(AModule, AName);
  {$ENDIF}

  if Result = nil then
    raise Exception.CreateFmt('"%s" function address could not be retrieved from roadrunner library', [AName]) at ReturnAddress;
end;


function loadMethod (methodName : string) : Pointer;
begin
  result := GetProcAddress(TRoadRunnerAPI.FLibHandle, PChar (methodName));

   if not Assigned (result) then
      begin
      if IsConsole then
         begin
         writeln ('Failed to load roadrunner method: ' + methodName);
         Halt
         end
      else
        raise Exception.Create('Failed to load roadrunner, unable to locate' +
             ' the following method: ' + methodName);
      end
end;


// ###
class procedure TRoadRunnerAPI.Initialize;
var DLErrorMsg : string;
begin
  TRoadRunnerAPI.FLibHandle := SafeLoadLibrary(TRoadRunnerAPI.LibName);
{$IF DEFINED(MACOS)}
  if TRoadRunnerAPI.FLibHandle = 0 then
     begin
      DLErrorMsg := string(dlerror);
      raise Exception.Create('roadrunner library could not be loaded: ' + DLErrorMsg);
     end;
  {$ELSE}
  if TRoadRunnerAPI.FLibHandle = 0 then
     begin
      raise Exception.Create('roadrunner library could not be loaded');
     end;
{$ENDIF}


  @libGetVersionStr := loadMethod ('getVersionStr');
  @libGetVersionInt := loadMethod ('getVersion');
  libGetLastError  := loadMethod ('getLastError');
  @libGetCopyright  := loadMethod ('getCopyright');
  @libGetlibSBMLVersion := loadMethod ('getlibSBMLVersion');
  @libGetSBML := loadMethod ('getSBML');

  @libCreateRRInstance := loadMethod ('createRRInstance');
  @libFreeRRInstance   := loadMethod ('freeRRInstance');

  @libLoadSBMLFromFile   := loadMethod ('loadSBMLFromFile');
  @libLoadSBMLFromString := loadMethod ('loadSBML');

  @libGetFloatingSpeciesIds := loadMethod ('getFloatingSpeciesIds');
  @libGetBoundarySpeciesIds := loadMethod('getBoundarySpeciesIds');
  @libGetReactionIds := loadMethod('getReactionIds');
  @libGetElasticityIds  := loadMethod ('getElasticityCoefficientIds');
  @libGetGlobalParameterIds := loadMethod ('getGlobalParameterIds');
  @libGetCompartmentIds := loadMethod ('getCompartmentIds');
  @libGetRatesOfChangeIds := loadMethod ('getRatesOfChangeIds');
  @libGetEigenvalueIds  := loadMethod ('getEigenvalueIds');

  @libGetNumberOfReactions := loadMethod ('getNumberOfReactions');
  @libGetNumberOfFloatingSpecies := loadMethod ('getNumberOfFloatingSpecies');
  @libGetNumberOfGlobalParameters := loadMethod ('getNumberOfGlobalParameters');
  @libGetNumberOfBoundarySpecies := loadMethod ('getNumberOfBoundarySpecies');
  @libGetNumberOfDependentSpecies   := loadMethod ('getNumberOfDependentSpecies');
  @libGetNumberOfIndependentSpecies := loadMethod ('getNumberOfIndependentSpecies');
  @libGetNumberOfCompartments := loadMethod('getNumberOfCompartments');

  @libSetCurrentIntegrator := loadMethod ('setCurrentIntegrator');

  @libReset := loadMethod ('reset');
  @libSetValue := loadMethod ('setValue');
  @libGetValue := loadMethod ('getValue');

  @libGetTimeCourseSelectionList := loadMethod ('getTimeCourseSelectionList');
  @libSetTimeCourseSelectionListEx := loadMethod ('setTimeCourseSelectionListEx');

  @libSetFloatingSpeciesInitialConcentrations := loadMethod ('setFloatingSpeciesInitialConcentrations');
  @libGetFloatingSpeciesInitialConcentrations := loadMethod ('getFloatingSpeciesInitialConcentrations');

  @libGetFloatingSpeciesInitialConditionIds := loadMethod ('getFloatingSpeciesInitialConditionIds');
  // There is no Set because its npt applicable

  @libSetFloatingSpeciesConcentrations := loadMethod ('setFloatingSpeciesConcentrations');
  @libGetFloatingSpeciesConcentrations := loadMethod ('getFloatingSpeciesConcentrations');

  @libSetBoundarySpeciesConcentrations := loadMethod ('setBoundarySpeciesConcentrations');
  @libGetBoundarySpeciesConcentrations := loadMethod ('getBoundarySpeciesConcentrations');

  @libSetComputeAndAssignConservationLaws := loadMethod ('setComputeAndAssignConservationLaws');
  @libGetComputeAndAssignConservationLaws := loadMethod  ('getComputeAndAssignConservationLaws');

  @libGetSteadyStateSelectionList  := loadMethod ('getSteadyStateSelectionList');
  @libSetSteadyStateSelectionListEx := loadMethod ('setSteadyStateSelectionListEx');

  @libSetCompartmentByIndex       := loadMethod ('setCompartmentByIndex');
  @libSetFloatingSpeciesByIndex   := loadMethod ('setFloatingSpeciesByIndex');
  @libSetBoundarySpeciesByIndex   := loadMethod ('setBoundarySpeciesByIndex');
  @libSetGlobalParameterByIndex   := loadMethod ('setGlobalParameterByIndex');
  @libGetCompartmentByIndex       := loadMethod ('getCompartmentByIndex');
  @libGetFloatingSpeciesByIndex   := loadMethod ('getFloatingSpeciesByIndex');
  @libGetBoundarySpeciesByIndex   := loadMethod ('getBoundarySpeciesByIndex');
  @libGetGlobalParameterByIndex   := loadMethod ('getGlobalParameterByIndex');

  @libSimulate  := loadMethod ('simulate');
  @libSimulateEx := loadMethod ('simulateEx');
  @libSetTimeStart := loadMethod ('setTimeStart');
  @libSetTimeEnd   := loadMethod ('setTimeEnd');
  @libSetNumberOfPoints := loadMethod ('setNumPoints');
  @libOneStep   := loadMethod ('oneStep');
  @libGetReactionRates := loadMethod ('getReactionRates');
  @libGetReactionRate  := loadMethod ('getReactionRate');
  @libGetRatesOfChange  := loadMethod ('getRatesOfChange');

  @libSteadyState := loadMethod ('steadyState');
  @libComputeSteadyStateValues := loadMethod ('computeSteadyStateValues');

  @libGetScaledElasticityMatrix := loadMethod ('getScaledElasticityMatrix');
  @libGetCCCMatrix := loadMethod ('getScaledConcentrationControlCoefficientMatrix');
  @libGetFCCMatrix  := loadMethod ('getScaledFluxControlCoefficientMatrix');
  @libGetFullJacobian := loadMethod ('getFullJacobian');
  @libGetReducedJacobian := loadMethod('getReducedJacobian');
  @libGetuCCCMatrix  := loadMethod ('getUnscaledConcentrationControlCoefficientMatrix');
  @libGetuFCCMatrix  := loadMethod ('getUnscaledFluxControlCoefficientMatrix');

  @libGetStoichiometryMatrix := loadMethod ('getStoichiometryMatrix');
  @libGetLinkMatrix          := loadMethod ('getLinkMatrix');
  @libGetNrMatrix            := loadMethod ('getNrMatrix');
  @libGetL0Matrix            := loadMethod ('getL0Matrix');
  @libGetConservationMatrix  := loadMethod ('getConservationMatrix');

  @libGetEigenvalues := loadMethod ('getEigenvalues');
  @libGetEigenvectors := loadMethod ('getEigenVectors');
  @libGetEigenvaluesMatrix := loadMethod ('getEigenvaluesMatrix');

  @libGetlist := loadMethod('getList');
  @libGetIntegerListItem := loadMethod('getIntegerListItem');
  @libGetStringListItem := loadMethod('getStringListItem');
  @libGetListItem := loadMethod('getListItem');
  @libGetListLength := loadMethod('getListLength');
  @libIsListItemString := loadMethod('isListItemString');
  @libIsListItemInteger := loadMethod('isListItemInteger');
  @libIsListItemDouble := loadMethod('isListItemDouble');
  @libIsListItemList := loadMethod('isListItemList');
  @libFreeRRList := loadMethod('freeRRList');

  @libGetMatrixNumRows := loadMethod ('getMatrixNumRows');
  @libGetMatrixNumCols := loadMethod ('getMatrixNumCols');
  @libGetMatrixElement := loadMethod ('getMatrixElement');

  @libCreateVector := loadMethod('createVector');

  @libFreeDoubleVector := loadMethod('freeVector');
  @libFreeMatrix := loadMethod('freeMatrix');
  @libFreeStringArray := loadMethod('freeStringArray');
  @libFreeRCData  := loadMethod ('freeRRCData');
end;


class procedure TRoadRunnerAPI.Terminate;
begin
  FreeLibrary(FLibHandle);
end;


// Utility Routines
// --------------------------------------------------------------
function getArrayOfStrings (pList: PRRStringArray) : TStringList;
var nStrings : integer;
    i, j : integer;
    element : PAnsiCharArray;
    str : AnsiString;
begin
  nStrings := pList^.count;
  result := TStringList.Create;
  for i := 0 to nStrings - 1 do
      begin
      element := pList^.strList[i];
      j := 0; str := '';
      while element[j] <> #0 do
          begin
          str := str + element[j];
          inc (j);
          end;
      result.Add (str);
      end;
end;

//function loadIntoMatrix (matrix : PRRMatrixHandle) : T2DMatrix; overload;
//var nr, nc : integer;
//    i, j : integer;
//begin
//  nr := matrix^.RSize;
//  nc := matrix^.CSize;
//  result := T2DMatrix.Create (nr, nc);
//  for i := 0 to nr - 1 do
//      for j := 0 to nc - 1 do
//          result[i+1,j+1] := matrix^.data[i*nc + j];
//end;


//
//function loadInTo2DArray (matrix : PRRMatrixHandle) : T2DDoubleArray;
//var nr, nc : integer;
//    i, j : integer;
//begin
//  nr := matrix^.RSize;
//  nc := matrix^.CSize;
//  setLength (result, nr, nc);
//  for i := 0 to nr - 1 do
//      for j := 0 to nc - 1 do
//          result[i,j] := matrix^.data[i*nc + j];
//end;


function extractList (list : PRRListRecordHandle) : TRRList;
var i : integer;
    item : PRRListItemRecord;
begin
  result := TRRList.Create;
  for i := 0 to list^.count - 1 do
      begin
      item := libGetListItem (list, i);
      case item^.ItemType of
        litList : result.Add (TRRListItem.Create (extractList (item^.lValue)));
        litString : result.Add (TRRListItem.Create (AnsiString (item^.sValue)));
        litInteger : result.Add (TRRListItem.Create (item^.iValue));
        litDouble : result.Add (TRRListItem.Create (item^.dValue));
      end;
  end;
end;


function RRStringArrayToStringList (ptr :  PRRStringArray) : TStringList;
var i : integer;
    str : AnsiString;
begin
  result := TStringList.Create;
  if ptr = nil then
     exit;
  for i := 0 to ptr.count - 1 do
      begin
      str := ptr.strList[i]^;
      result.Add (str);
      end;
end;
// -----------------------------------------------------------------
// For doumentation, see the C API docs at:
//      http://libroadrunner.org
// -----------------------------------------------------------------


procedure freeRRInstance (myInstance : Pointer);
begin
  if myInstance <> nil then
     libFreeRRInstance (myInstance);
end;


function hasError : boolean;
begin
  result := libHasError (internalRRHandle);
end;


function enableLoggingToFile : boolean;
begin
  result := boolean (libEnableLoggingToFile);
  loggingEnabled := true;
end;


function enableLoggingToFileWithPath (path : AnsiString) : boolean;
begin
  result := boolean (libEnableLoggingToFileWithPath (PAnsichar (path)));
  loggingEnabled := true;
end;


function setLogLevel (debugLevel : AnsiString) : boolean;
begin
  result := boolean (libSetLogLevel (PAnsiChar (debugLevel)));
end;


function getLogFileName : AnsiString;
begin
  result := libGetLogFileName;
end;

function setLogFileName (value : AnsiString) : boolean;
begin
  currentLoggingPath := value;
end;

function setTempFolder (name : AnsiString) : boolean;
begin
  result := libSetTempFolder (internalRRHandle, PAnsiChar (name));
end;


function getComputeAndAssignConservationLaws : Boolean;
var value : Integer;
begin
  // HMS
  //if not boolean (libGetComputeAndAssignConservationLaws (internalRRHandle, value)) then
  //   raise Exception.Create ('Error calling getComputeAndAssignConservationLaws' + getLastError());
  result := boolean (value);
end;


function loadSBMLFromFile (fileName : string) : boolean;
var mystr : AnsiString;
begin
  mystr := AnsiString (fileName);
  if FileExists (fileName) then
     result := TRoadRunnerAPI.libLoadSBMLFromFile (internalRRHandle, PAnsiChar (mystr))
  else
     raise Exception.Create ('Unable to locate SBML file [' + fileName + ']');
end;


procedure clear;
begin
  TRoadRunnerAPI.libFreeRRInstance (internalRRHandle);
  createInternalRRInstance;
end;


function getBoundarySpeciesInitialConcentrations : TDoubleArray; var p : PRRDoubleVectorHandle; i : integer;
begin
  p := TRoadRunnerAPI.libGetBoundarySpeciesConcentrations (internalRRHandle);
  try
    if p = nil then
       begin
       setLength (result, 0);
       exit;
       end;
    setLength (result, p^.count);
    for i := 0 to p^.count - 1 do
        result[i] := p^.data[i];
  finally
    libFreeDoubleVector (p);
  end;
end;


function evalModel : boolean;
begin
  result := libEvalModel (internalRRHandle);
end;


function getElasticityIds : TRRList;
var p : PRRListRecordHandle;
    l : TRRList;
    i, j, k : integer;
begin
  p := TRoadRunnerAPI.libGetElasticityIds (internalRRHandle);
  try
    result := extractList (p);
    //result := TStringList.Create;
    //for i := 0 to l.Count - 1 do
    //    for j := 0 to l[i].list[1].list.Count - 1 do
    //        result.Add(l[i].list[1].list[j].sValue);
  finally
    TRoadRunnerAPI.libFreeRRList (p);
  end;
end;


function setSteadyStateSelectionList (strList : TStringList) : boolean;
var i : integer;
    str : AnsiString;
    p : PRRListRecordHandle;
begin
  if strList.Count > 0 then
     begin
     str := strList[0];
     for i := 1 to strList.Count - 1 do
         str := str + ' ' + strList[i];
     libSetSteadyStateSelectionList (internalRRHandle, PAnsiChar (str));
     p := libGetAvailableSteadyStateSymbols (internalRRHandle);
     end;
  result := true;
end;


function getuCC (variable : AnsiString; parameter : AnsiString) : double;
begin
  if not libgetuCC (internalRRHandle, PAnsiChar (variable), PAnsiChar (parameter), result) then
     raise Exception.Create ('Error in getCC function');
end;


function getCC (variable : AnsiString; parameter : AnsiString) : double;
begin
  if not libgetCC (internalRRHandle, PAnsiChar (variable), PAnsiChar (parameter), result) then
     raise Exception.Create ('Error in getCC function');
end;


function getuEE (variable : AnsiString; parameter : AnsiString) : double;
begin
  if not libgetuEE (internalRRHandle, PAnsiChar (variable), PAnsiChar (parameter), result) then
     raise Exception.Create ('Error in getCC function');
end;


function getEE (variable : AnsiString; parameter : AnsiString) : double;
begin
  if not libgetEE (internalRRHandle, PAnsiChar (variable), PAnsiChar (parameter), result) then
     raise Exception.Create ('Error in getCC function');
end;


function getAvailableTimeCourseSymbols : TRRList;
var ptr : PRRListRecordHandle;
begin
  ptr := libGetAvailableTimeCourseSymbols (internalRRHandle);
  result := extractList (ptr);
end;


function getAvailableSteadStateSymbols : TRRList;
var ptr : PRRListRecordHandle;
begin
  ptr := libGetAvailableSteadyStateSymbols (internalRRHandle);
  result := extractList (ptr);
end;


// Deprecated
function getAvailableSymbolsII : TRRList;
var subList : TRRList; st : TStringList;
    i : integer;
    ptr : PRRListRecordHandle;
begin
  ptr := libGetAvailableTimeCourseSymbols (internalRRHandle);
  result := extractList (ptr);
  exit;
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('time'));
  result.Add (TRRListItem.Create (subList));
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Floating Species'));
  // To be fixed
  //st := uRoadRunner.getFloatingSpeciesIds ();
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;
  // to be fixed
  //st := getBoundarySpeciesIds ();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Boundary Species'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;
  {st := getFloatingSpeciesAmountIds();
  subList := TRRList.Create;
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}
  {st := getBoundarySpeciesAmountIds();
  subList := TRRList.Create;
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  lresultist.Add (TRRListItem.Create (subList));
  st.Free;}
  // to be fixed
  //st := getGlobalParameterIds ();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Global Parameter Ids'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;
  {st := getCompartmentIds();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Compartments'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}
  // to be fixed
  //st := getReactionIds ();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Reaction Ids'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;
  {st := getRatesOfChangeIds();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Rate of Change Ids'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}
  {st := getElasticityCoefficientIds();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Elasticity Coefficients'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}
  {st := getUnscaledElasticityCoefficientIds();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Unscaled Elasticity Coefficients'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}
  {st := getEigenValueIds();
  subList := TRRList.Create;
  subList.Add (TRRListItem.Create ('Eigenvalues'));
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;}
  {p := libGetAvailableSymbols;
  setLength (result, p^.count);
  for i := 0 to p^.count - 1 do
      begin
      result[i].labelStr := p^.list[i].labelStr;
      result[i].stringList := getArrayOfStrings  (@(p^.list[i]));
      end;}
end;

procedure setMatrixElement ();
begin
end;
// ---------------------------------------------------------------------
// Integrator options API
// ---------------------------------------------------------------------

function getListOfRegisteredIntegrators : TStringList;
var ansiStr : AnsiString;
    p : PAnsiChar;
    n, i : integer;
begin
  n := libGetNumRegisteredIntegrators ();
  result := TStringList.Create;
  for i := 0 to n - 1 do
      begin
      p := libGetRegisteredIntegratorName (i);
      ansiStr := AnsiString (p);
      Result.Add(ansiStr)
      end;
end;

function getNumberOfIntegrators : integer;
begin
  result := libGetNumberOfIntegrators (internalRRHandle);
end;


function setIntegrator (name : string) : boolean;
var _name : AnsiString;
    res : integer;
begin
  _name := AnsiString (name);
  res := TRoadRunnerAPI.libSetCurrentIntegrator (internalRRHandle, PAnsiChar (_name));
  if res = 0 then
     result := False
  else
     result := True;
end;

function getCurrentIntegratorName : string;
var name : AnsiString;
    p : PAnsiChar;
begin
    p := libGetCurrentIntegratorName (internalRRHandle);
    name := AnsiString (p);
    result := name;
end;

function getIntegratorDescription : string;
var p : PAnsiChar;
begin
  p := libGetCurrentIntegratorDescription (internalRRHandle);
  result := AnsiString (p);
end;

function getIntegratorHint : string;
var p : PAnsiChar;
begin
  p := libGetCurrentIntegratorHint (internalRRHandle);
  result := AnsiString (p);
end;
function getNumberOfIntegratorParameters : integer;
begin
  result := libGetNumberOfCurrentIntegratorParameters (internalRRHandle);
end;
function getCurrentIntegratorNthParameterName (index : integer) : string;
var p : PAnsiChar;
begin
  p := libGetCurrentIntegratorNthParameterName (internalRRHandle, index);
  result := AnsiString (p);
end;
function getListOfIntegratorParameterNames : TStringList;
var ptr : PRRStringArray;
begin
  ptr := libGetListOfCurrentIntegratorParameterNames (internalRRHandle);
  result := RRStringArrayToStringList (ptr)
end;

function getIntegratorParameterDescription (parameterName : string) : string;
var p : PAnsiChar;
    str : AnsiString;
begin
  str := parameterName;
  p := libGetCurrentIntegratorParameterDescription (internalRRHandle, PAnsiChar (str));
  result := AnsiString (p);
end;
function getIntegratorParameterHint (parameterName : string) : string;
var p : PAnsiChar;
    str : AnsiString;
begin
  str := parameterName;
  p := libGetCurrentIntegratorParameterHint (internalRRHandle, PAnsiChar (str));
  result := AnsiString (p);
end;
function getIntegratorParameterType (parameterName : string) : integer;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  Result := libGetCurrentIntegratorParameterType (internalRRHandle, PAnsiChar (_name));
end;
function  getIntegratorParameterInt (parameterName : string) : integer;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentIntegratorParameterInt (internalRRHandle, PAnsiChar (_name));
end;

procedure setIntegratorParameterInt (parameterName : string; value : integer);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  if not libSetCurrentIntegratorParameterInt (internalRRHandle, PAnsiChar (_name), value) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setIntegratorParameterInt: ' + str);
     end;
end;

function  getIntegratorParameterUInt (parameterName : string) : UInt32;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentIntegratorParameterUInt (internalRRHandle, PAnsiChar (_name));
end;

procedure setIntegratorParameterUInt (parameterName : string; value : UInt32);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  if not libSetCurrentIntegratorParameterUInt (internalRRHandle, PAnsiChar (_name), value) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setIntegratorParameterUInt: ' + str);
     end;
end;

function  getIntegratorParameterDouble (parameterName : string) : double;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentIntegratorParameterDouble (internalRRHandle, PAnsiChar (_name));
end;

procedure setIntegratorParameterDouble (parameterName : string; value : double);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  if not libSetCurrentIntegratorParameterDouble (internalRRHandle, PAnsiChar (_name), value) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setIntegratorParameterDouble: ' + str);
     end;
end;

function  getIntegratorParameterString (parameterName : string) : string;
var _name : AnsiString;
    ptr : PAnsiChar;
begin
  _name := AnsiString (parameterName);
  ptr := libGetCurrentIntegratorParameterString (internalRRHandle, PAnsiChar (_name));
  result := string (ptr);
end;

procedure setIntegratorParameterString (parameterName : string; value : string);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  str := AnsiString (value);
  if not libSetCurrentIntegratorParameterString (internalRRHandle, PAnsiChar (_name), PAnsiChar(str)) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setIntegratorParameterString: ' + str);
     end;
end;
function getIntegratorParameterBoolean (parameterName : string) : Boolean;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentIntegratorParameterBoolean (internalRRHandle, PAnsiChar (_name));
end;

procedure setIntegratorParameterBoolean (parameterName : string; value : boolean);
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  // HMS
  //if not libSetCurrentIntegratorParameterBoolean (internalRRHandle, PAnsiChar (_name), integer (value)) then
  //   begin
  //   raise Exception.Create('Error in setIntegratorParameterBoolean: ' + getLastError());
  //   end;
end;

// ---------------------------------------------------------------------
// Steady State options API
// ---------------------------------------------------------------------
function getListOfRegisteredSteadyStateSolvers : TStringList;
var ansiStr : AnsiString;
    p : PAnsiChar;
    n, i : integer;
begin
  n := libGetNumRegisteredSteadyStateSolvers  ();
  result := TStringList.Create;
  for i := 0 to n - 1 do
      begin
      p := libGetRegisteredSteadyStateSolverName (i);
      ansiStr := AnsiString (p);
      Result.Add(ansiStr)
      end;
end;

function  getNumberOfSteadyStateSolvers : integer;
begin
  result := libGetNumberOfSteadyStateSolvers (internalRRHandle);
end;

procedure setSteadyStateSolver (name : string);
var _name : AnsiString;
    res : integer;
begin
  _name := AnsiString (name);
  res := libSetCurrentSteadyStateSolver (internalRRHandle, PAnsiChar (_name));
  // HMS
  //if res = 0 then
  //   raise Exception.Create('Failed to set Steady State Solver: ' + getLastError);
end;

function  getCurrentSteadyStateSolverName : string;
var name : AnsiString;
    p : PAnsiChar;
begin
    p := libGetCurrentSteadyStateSolverName (internalRRHandle);
    name := AnsiString (p);
    result := name;
end;

function  getSteadyStateSolverDescription : string;
 var p : PAnsiChar;
begin
  p := libGetCurrentSteadyStateSolverDescription (internalRRHandle);
  result := AnsiString (p);
end;

function getSteadyStateSolverHint : string;
 var p : PAnsiChar;
begin
  p := libGetCurrentSteadyStateSolverHint (internalRRHandle);
  result := AnsiString (p);
end;

function  getNumberOfSteadyStateSolverParameters : integer;
begin
  result := libGetNumberOfCurrentSteadyStateSolverParameters (internalRRHandle);
end;

function  getListOfSteadyStateSolverParameterNames : TStringList;
var ptr : PRRStringArray;
begin
  ptr := libGetListOfCurrentSteadyStateSolverParameterNames (internalRRHandle);
  result := RRStringArrayToStringList (ptr)
end;

function getCurrentSteadyStateNthParameterName (index : integer) : string;
var p : PAnsiChar;
begin
  p := libGetCurrentSteadyStateSolverNthParameterName (internalRRHandle, index);
  result := AnsiString (p);
end;

function  getSteadyStateSolverParameterDescription (parameterName : string) : string;
var p : PAnsiChar;
    str : AnsiString;
begin
  str := parameterName;
  p := libGetCurrentSteadyStateSolverParameterDescription (internalRRHandle, PAnsiChar (str));
  result := AnsiString (p);
end;
function getSteadyStateSolverParameterHint (parameterName : string) : string;
var p : PAnsiChar;
    str : AnsiString;
begin
  str := parameterName;
  p := libGetCurrentSteadyStateSolverParameterDescription (internalRRHandle, PAnsiChar (str));
  result := AnsiString (p);
end;

// 0-STRING, 1-BOOL, 2-INT32, 3-UINT32, 4-INT64, 5-UINT64, 6-FLOAT, 7-DOUBLE, 8-CHAR, 9-UCHAR, 10-EMPTY
function  getSteadyStateSolverParameterType (parameterName : string) : integer;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  Result := libGetCurrentSteadyStateSolverParameterType (internalRRHandle, PAnsiChar (_name));
end;
function  getSteadyStateSolverParameterInt (parameterName : string) : integer;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentSteadyStateSolverParameterInt (internalRRHandle, PAnsiChar (_name));
end;

procedure setSteadyStateSolverParameterInt (parameterName : string; value : integer);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  if not libSetCurrentSteadyStateSolverParameterInt (internalRRHandle, PAnsiChar (_name), value) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setSteadyStateSolverParameterInt: ' + str);
     end;
end;

function  getSteadyStateSolverParameterUInt (parameterName : string) : UInt32;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentSteadyStateSolverParameterUInt (internalRRHandle, PAnsiChar (_name));
end;

procedure setSteadyStateSolverParameterUInt (parameterName : string; value : UInt32);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  if not libSetCurrentSteadyStateSolverParameterUInt (internalRRHandle, PAnsiChar (_name), value) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setSteadyStateSolverParameterUInt: ' + str);
     end;
end;
function  getSteadyStateSolverParameterDouble (parameterName : string) : double;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentSteadyStateSolverParameterDouble (internalRRHandle, PAnsiChar (_name));
end;

procedure setSteadyStateSolverParameterDouble (parameterName : string; value : double);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  if not libSetCurrentSteadyStateSolverParameterDouble (internalRRHandle, PAnsiChar (_name), value) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setSteadyStateSolverParameterDouble: ' + str);
     end;
end;
function  getSteadyStateSolverParameterString (parameterName : string) : string;
var _name : AnsiString;
    ptr : PAnsiChar;
begin
  _name := AnsiString (parameterName);
  ptr := libGetCurrentSteadyStateSolverParameterString (internalRRHandle, PAnsiChar (_name));
  result := string (ptr);
end;

procedure setSteadyStateSolverParameterString (parameterName : string; value : string);
var _name, str : AnsiString;
begin
  _name := AnsiString (parameterName);
  str := AnsiString (value);
  if not libSetCurrentSteadyStateSolverParameterString (internalRRHandle, PAnsiChar (_name), PAnsiChar(str)) then
     begin
     str := AnsiString (TRoadRunnerAPI.libGetLastError());
     raise Exception.Create('Error in setSteadyStateParameterString: ' + str);
     end;
end;

function  getSteadyStateSolverParameterBoolean (parameterName : string) : Boolean;
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  result := libGetCurrentSteadyStateSolverParameterBoolean (internalRRHandle, PAnsiChar (_name));
end;

procedure setSteadyStateSolverParameterBoolean (parameterName : string; value : Boolean);
var _name : AnsiString;
begin
  _name := AnsiString (parameterName);
  // HMS
  //if not libSetCurrentSteadyStateSolverParameterBoolean (internalRRHandle, PAnsiChar (_name), integer (value)) then
  //   raise Exception.Create('Error in setCurrentSteadyStateSolverParameterBoolean: ' + getLastError());
end;

procedure setConfigBoolean (key : string; value : Boolean);
var s : AnsiString;
    ib : integer;
begin
  s := AnsiString (key);
  ib := integer (value);
  // HMS
  //if libSetConfigBool (PAnsiChar (s), ib) = 0 then
  //   raise Exception.Create('Error in SetConfigBoolean: ' + getLastError());
end;

procedure setConfigInteger (key : string; value : integer);
var s : AnsiString;
begin
  s := AnsiString (key);
  // HMS
  //if libSetConfigInt (PAnsiChar (s), value) = 0 then
  //   raise Exception.Create('Error in SetConfigInteger: ' + getLastError());
end;

procedure setConfigDouble (key : string; value : double);
var s : AnsiString;
begin
  s := AnsiString (key);
  // HMS
  //if libSetConfigDouble (PAnsiChar (s), value) = 0 then
  //   raise Exception.Create('Error in SetConfigDouble: ' + getLastError());
end;

function   getConfigBoolean (key : string) : Boolean;
var s : AnsiString;
    ib : integer;
begin
  s := AnsiString (key);
  ib := libGetConfigBool (PAnsiChar (s));
  result := boolean (ib);
end;

function   getConfigInteger (key : string) : Integer;
var s : AnsiString;
begin
  s := AnsiString (key);
  result := libGetConfigInt (PAnsiChar (s));
end;

function   getConfigDouble (key : string) : Double;
var s : AnsiString;
begin
  s := AnsiString (key);
  result := libGetConfigDouble (PAnsiChar (s));
end;

// ---------------------------------------------------------------------

function solverTypeToString (code : Integer) : string;
var p : PAnsiChar;
begin
  p := libSolverTypeToString (code);
  result := AnsiString (p);
end;

// ---------------------------------------------------------------------
procedure setRoadRunnerLibraryName (newLibName : AnsiString);
begin
  libName := newLibName;
end;


function loadSingleMethod (methodName : string; var errMsg : AnsiString; var success : boolean; methodList : TStringList) : Pointer;
begin
   result := GetProcAddress(dllHandle, PChar (methodName));
   if not Assigned (result) then
      begin
      methodList.Add (methodName + ': ****************** FAILED');
      errMsg := 'Failed to load method: ' + methodName;
      success := false;
      end
   else
      methodList.Add (methodName + ': found');
end;


// Deprecated load methods
function loadMethods (var errMsg : AnsiString; methodList : TStringList) : boolean;
begin
   result := true;
   try
   @libHasError      := loadSingleMethod ('hasError', errMsg, result, methodList);
   @libSetLogLevel   := loadSingleMethod ('setLogLevel', errMsg, result, methodList);
   @libEnableLoggingToConsole := loadSingleMethod ('enableLoggingToConsole', errMsg, result, methodList);
   @libEnableLoggingToFile := loadSingleMethod ('enableLoggingToFile', errMsg, result, methodList);
   @libEnableLoggingToFileWithPath := loadSingleMethod ('enableLoggingToFileWithPath', errMsg, result, methodList);
   @libGetLogFileName := loadSingleMethod ('getLogFileName', errMsg, result, methodList);
   @libSetTempFolder := loadSingleMethod ('setTempFolder', errMsg, result, methodList);
   @libEvalModel          := loadSingleMethod ('evalModel', errMsg, result, methodList);
   @libGetTimeCourseSelectionList  := loadSingleMethod ('getTimeCourseSelectionList', errMsg, result, methodList);
   @libSetSteadyStateSelectionList   := loadSingleMethod ('setSteadyStateSelectionList', errMsg, result, methodList);
   @libGetAvailableTimeCourseSymbols  := loadSingleMethod ('getAvailableTimeCourseSymbols', errMsg, result, methodList);
   @libGetAvailableSteadyStateSymbols := loadSingleMethod ('getAvailableSteadyStateSymbols', errMsg, result, methodList);

   @libgetuCC                 := loadSingleMethod ('getuCC', errMsg, result, methodList);
   @libgetuEE                 := loadSingleMethod ('getuEE', errMsg, result, methodList);
   @libgetCC                  := loadSingleMethod ('getCC', errMsg, result, methodList);
   @libgetEE                  := loadSingleMethod ('getEE', errMsg, result, methodList);

   @libGetListItem            := loadSingleMethod ('getListItem', errMsg, result, methodList);
   @libFreeRCData             := loadSingleMethod ('freeRRCData', errMsg, result, methodList);
   @libFreeText         := loadSingleMethod ('freeText', errMsg, result, methodList);
   @libCreateRRMatrix   := loadSingleMethod ('createRRMatrix', errMsg, result, methodList);
   @libSetMatrixElement := loadSingleMethod ('setMatrixElement', errMsg, result, methodlist);
   @libFreeDoubleVector := loadSingleMethod ('freeVector', errMsg, result, methodList);
   @libSetConfigBool    := loadSingleMethod ('setConfigBool', errMsg, result, methodList);
   @libSetConfigInt     := loadSingleMethod ('setConfigInt', errMsg, result, methodList);
   @libSetConfigDouble  := loadSingleMethod ('setConfigDouble', errMsg, result, methodList);
   @libGetConfigBool    := loadSingleMethod ('getConfigBool', errMsg, result, methodList);
   @libGetConfigInt     := loadSingleMethod ('getConfigInt', errMsg, result, methodList);
   @libGetConfigDouble  := loadSingleMethod ('getConfigDouble', errMsg, result, methodList);
   // -----------------------------------------------------------------
   // Integrator Options API
   // -----------------------------------------------------------------
   @libGetNumRegisteredIntegrators := loadSingleMethod ('getNumRegisteredIntegrators', errMsg, result, methodList);
   @libGetRegisteredIntegratorName := loadSingleMethod ('getRegisteredIntegratorName', errMsg, result, methodList);
   @libGetRegisteredIntegratorHint := loadSingleMethod ('getRegisteredIntegratorHint', errMsg, result, methodList);
   @libGetRegisteredIntegratorDescription := loadSingleMethod ('getRegisteredIntegratorDescription', errMsg, result, methodList);

   @libGetCurrentIntegratorName := loadSingleMethod ('getCurrentIntegratorName', errMsg, result, methodList);
   @libGetCurrentIntegratorDescription := loadSingleMethod ('getCurrentIntegratorDescription', errMsg, result, methodList);
   @libGetCurrentIntegratorHint := loadSingleMethod ('getCurrentIntegratorHint', errMsg, result, methodList);
   //@libSetCurrentIntegrator := loadSingleMethod ('setCurrentIntegrator', errMsg, result, methodList);
   @libGetNumberOfCurrentIntegratorParameters := loadSingleMethod ('getNumberOfCurrentIntegratorParameters', errMsg, result, methodList);
   @libGetCurrentIntegratorNthParameterName := loadSingleMethod ('getCurrentIntegratorNthParameterName', errMsg, result, methodList);
   @libGetListOfCurrentIntegratorParameterNames := loadSingleMethod ('getListOfCurrentIntegratorParameterNames', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterDescription := loadSingleMethod ('getCurrentIntegratorParameterDescription', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterHint :=  loadSingleMethod ('getCurrentIntegratorParameterHint', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterType := loadSingleMethod ('getCurrentIntegratorParameterType', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterInt := loadSingleMethod ('getCurrentIntegratorParameterInt', errMsg, result, methodList);
   @libSetCurrentIntegratorParameterInt := loadSingleMethod ('setCurrentIntegratorParameterInt', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterUInt := loadSingleMethod ('getCurrentIntegratorParameterUInt', errMsg, result, methodList);
   @libSetCurrentIntegratorParameterUInt := loadSingleMethod ('setCurrentIntegratorParameterUInt', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterDouble := loadSingleMethod ('getCurrentIntegratorParameterDouble', errMsg, result, methodList);
   @libSetCurrentIntegratorParameterDouble := loadSingleMethod ('setCurrentIntegratorParameterDouble', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterString := loadSingleMethod ('getCurrentIntegratorParameterString', errMsg, result, methodList);
   @libSetCurrentIntegratorParameterString := loadSingleMethod ('setCurrentIntegratorParameterString', errMsg, result, methodList);
   @libGetCurrentIntegratorParameterBoolean := loadSingleMethod ('getCurrentIntegratorParameterBoolean', errMsg, result, methodList);
   @libSetCurrentIntegratorParameterBoolean := loadSingleMethod ('setCurrentIntegratorParameterBoolean', errMsg, result, methodList);
   // -----------------------------------------------------------------
   // Steady State Solver Options API
   // -----------------------------------------------------------------
   @libGetNumRegisteredSteadyStateSolvers := loadSingleMethod ('getNumRegisteredSteadyStateSolvers', errMsg, result, methodList);
   @libGetRegisteredSteadyStateSolverName := loadSingleMethod ('getRegisteredSteadyStateSolverName', errMsg, result, methodList);
   @libGetRegisteredSteadyStateSolverHint := loadSingleMethod ('getRegisteredSteadyStateSolverHint', errMsg, result, methodList);
   @libGetRegisteredSteadyStateSolverDescription := loadSingleMethod ('getRegisteredSteadyStateSolverDescription', errMsg, result, methodList);

   @libGetCurrentSteadyStateSolverName := loadSingleMethod ('getCurrentSteadyStateSolverName', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverDescription := loadSingleMethod ('getCurrentSteadyStateSolverDescription', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverHint := loadSingleMethod ('getCurrentSteadyStateSolverHint', errMsg, result, methodList);
   @libSetCurrentSteadyStateSolver := loadSingleMethod ('setCurrentSteadyStateSolver', errMsg, result, methodList);
   @libGetNumberOfCurrentSteadyStateSolverParameters := loadSingleMethod ('getNumberOfCurrentSteadyStateSolverParameters', errMsg, result, methodList);
   @libGetListOfCurrentSteadyStateSolverParameterNames := loadSingleMethod ('getListOfCurrentSteadyStateSolverParameterNames', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverNthParameterName := loadSingleMethod ('getCurrentSteadyStateSolverNthParameterName', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterDescription := loadSingleMethod ('getCurrentSteadyStateSolverParameterDescription', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterHint :=  loadSingleMethod ('getCurrentSteadyStateSolverParameterHint', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterType := loadSingleMethod ('getCurrentSteadyStateSolverParameterType', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterInt := loadSingleMethod ('getCurrentSteadyStateSolverParameterInt', errMsg, result, methodList);
   @libSetCurrentSteadyStateSolverParameterInt := loadSingleMethod ('setCurrentSteadyStateSolverParameterInt', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterUInt := loadSingleMethod ('getCurrentSteadyStateSolverParameterUInt', errMsg, result, methodList);
   @libSetCurrentSteadyStateSolverParameterUInt := loadSingleMethod ('setCurrentSteadyStateSolverParameterUInt', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterDouble := loadSingleMethod ('getCurrentSteadyStateSolverParameterDouble', errMsg, result, methodList);
   @libSetCurrentSteadyStateSolverParameterDouble := loadSingleMethod ('setCurrentSteadyStateSolverParameterDouble', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterString := loadSingleMethod ('getCurrentSteadyStateSolverParameterString', errMsg, result, methodList);
   @libSetCurrentSteadyStateSolverParameterString := loadSingleMethod ('setCurrentSteadyStateSolverParameterString', errMsg, result, methodList);
   @libGetCurrentSteadyStateSolverParameterBoolean := loadSingleMethod ('getCurrentSteadyStateSolverParameterBoolean', errMsg, result, methodList);
   @libSetCurrentSteadyStateSolverParameterBoolean := loadSingleMethod ('setCurrentSteadyStateSolverParameterBoolean', errMsg, result, methodList);


   // -----------------------------------------------------------------
   // Generic Solver Options API
   // -----------------------------------------------------------------
   @libSolverTypeToString := loadSingleMethod ('solverTypeToString', errMsg, result, methodList);
   except
     on E: Exception do
        begin
        errMsg := e.message;
        result := false;
        exit;
        end;
   end;
end;


// This is depracated
function loadRoadRunner (var errMsg : AnsiString; methodList : TStringList) : boolean;
var errStr : string;
    fullPath: WideString;
    aString: PChar;
    path : AnsiString;
    oldCurrentDir : string;
    dirPath : string;
begin
  DLLLoaded := false;
  result := true;
  path := ExtractFilePath(ParamStr(0)) + libName;
  if FileExists (path) then
     begin
     fullPath := WideString (path);
     oldCurrentDir := TDirectory.GetCurrentDirectory;
     try
       dirPath := extractFilePath (Path);
       TDirectory.SetCurrentDirectory (dirPath);
       DllHandle := LoadLibrary(PWideChar (fullpath));
       if DllHandle <> 0 then
           begin
           if loadMethods (errMsg, methodList) then
              begin
              DLLLoaded := True;
              result := true;
              end
           else
              result := false;
           end
       else
           begin
           {$IFDEF POSIX}
             errStr := string(dlerror());
           {$ELSE}
             {$IFDEF VER220} // Delphi XE
               errStr := SysErrorMessage(Windows.GetLastError);
             {$ELSE}
               errStr := SysErrorMessage(Windows.GetLastError);
             {$ENDIF}
           {$ENDIF}
             DLLLoaded := False;
           errMsg := 'Failed to load roadRunner at:[' + ExtractFilePath (path) + ']: ' + errStr;
           result := false;
           end;
     finally
       TDirectory.SetCurrentDirectory(oldCurrentDir);
     end;
     end
  else
     begin
     DLLLoaded := False;
     errMsg := 'Unable to locate roadRunner library at:[' + path + ']' + sLineBreak + libName;
     result := false;
     end;
end;


procedure releaseRoadRunnerLibrary;
begin
  DLLLoaded := false;
  freeLibrary (DLLHandle);
end;


initialization
  {$IFDEF POSIX}
     libName := 'libroadrunner_c_api.dylib';
  {$ELSE}
     libName := 'libRoadRunner\bin\roadrunner_c_api.dll';
  {$ENDIF}
end.
