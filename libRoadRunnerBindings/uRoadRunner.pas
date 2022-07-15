unit uRoadRunner;

{ Copyright 2012 Herbert M Sauro

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
     uRRTypes;


type
  TPointerFunc = function : Pointer; cdecl;
  TVoidCharFunc = function : PAnsiChar; cdecl;   // char* func(void)
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
  TCreateRRInstance = TPointerFunc;
  TFreeRRCData = function (ptr : pointer{PRRCData}) : boolean; cdecl;
  TFreeRRInstance = procedure (instance : Pointer); cdecl;
  THandleVectorFunc = function (rrHandle : Pointer) : PRRDoubleVectorHandle; cdecl;
  TSetSelectionList = function (rrHandle : Pointer; list : PAnsiChar) : boolean; cdecl;
  TSetSelectionListEx = function (rrHandle : Pointer; len : integer; list : PPAnsiChar) : boolean; cdecl;
  TFreeStringArray = function (handle : PRRStringArray) : boolean; cdecl;
  TFreeRRDoubleVector = function (vector : PRRDoubleVectorHandle) : boolean ; cdecl;
  TFreeListPtr = procedure (handle : PRRListRecordHandle); cdecl;
  TGetMCA = function (rrHandle : Pointer; variable : PAnsiChar; parameter : PAnsiChar; var value : double) : boolean; cdecl;


  TRoadRunner = class (TObject)
    private
       roadRunnerInstance : pointer;
       function loadIntoRRMatrix  (mat : T2DMatrix) : PRRMatrixHandle;
    public
       function  getVersion : integer;
       function  getVersionStr : string;
       function  getCopyright : AnsiString;
       function  getlibSBMLVersion : AnsiString;

       function  getLastError : string;
       function  loadSBMLFromString (sbmlStr : string) : boolean;
       function  loadSBMLFromFile (fileName : string) : boolean;
       function  getSBML : AnsiString;
       procedure reset;


       function  getNumberOfReactions : integer;
       function  getNumberOfFloatingSpecies : integer;
       function  getNumberOfBoundarySpecies : integer;
       function  getNumberOfGlobalParameters : integer;
       function  getNumberOfCompartments : integer;

       function  getCompartmentIds : TStringList;
       function  getReactionIds : TStringList;
       function  getBoundarySpeciesIds : TStringList;
       function  getFloatingSpeciesIds : TStringList;
       function  getGlobalParameterIds : TStringList;
       function  getEigenvalueIds : TStringList;
       function  getRatesOfChangeIds : TStringList;
       function  getElasticityIds : TRRList;
       function  getFloatingSpeciesInitialConditionIds : TStringList;

       function  getBoundarySpeciesConcentrations : TDoubleArray;
       function  setFloatingSpeciesConcentrations (value : TDoubleArray) : boolean;
       function  setBoundarySpeciesConcentrations (value : TDoubleArray) : boolean;

       function  getNumberOfDependentSpecies : integer;
       function  getNumberOfIndependentSpecies : integer;

       procedure setTimeStart (value : double);
       procedure setTimeEnd (value : double);
       procedure setNumberOfPoints (value : integer);

       function  setComputeAndAssignConservationLaws (value : boolean) : boolean;

       function  setSteadyStateSelectionListEx (list : TStringList) : boolean;
       function  getSteadyStateSelectionList: TStringList;
       function  setTimeCourseSelectionListEx (list : TStringList) : boolean;

       function  setFloatingSpeciesInitialConcentrations (value : TDoubleArray) : boolean;
       function  getFloatingSpeciesInitialConcentrations : TDoubleArray;

       function  getReactionRates : TDoubleArray;
       function  getRatesOfChange : TDoubleArray;
       function  getFullJacobian : T2DMatrix;
       function  getReducedJacobian : T2DMatrix;
       function  getL0Matrix : T2DMatrix;
       function  getConservationMatrix : T2DMatrix;
       function  getStoichiometryMatrix : T2DMatrix;
       function  getLinkMatrix : T2DMatrix;
       function  getNrMatrix : T2DMatrix;
       function  getEigenvalues : T2DMatrix;
       function  getEigenvectors : T2DMatrix;
       function  getEigenvaluesMatrix (m : T2DMatrix) : T2DMatrix;

       function  getValue (Id : AnsiString) : double;
       function  setValue (Id : AnsiString; value : double) : boolean;

       function  steadyState : double;
       function  computeSteadyStateValues : TDoubleArray;

       function  getFloatingSpeciesConcentrations : TArray<double>;

       function  getTimeCourseSelectionList: TStringList;
       function  oneStep (currentTime : double; stepSize : double) : double;
       function  simulate : T2DMatrix;
       function  simulateEx (timeStart: double; timeEnd : double; numberOfPoints : integer) : T2DMatrix; //T2DDoubleArray;

       function  getScaledElasticityMatrix : T2DMatrix;
       function  getScaledConcentrationControlCoefficientMatrix : T2DMatrix;
       function  getScaledFluxControlCoefficientMatrix : T2DMatrix;

       constructor Create;
       destructor  Destroy; override;
  end;

var
   DLLLoaded : boolean;
   loggingEnabled : boolean = false;
   loggingTmpFileName : AnsiString = '';
   rrHandle : Pointer;

function  hasError : boolean;
function  createInternalRRInstance : Pointer;

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
{$REGION 'Documentation'}
///	<summary>
///	  Returns the generated C Code for the model
///	</summary>
{$ENDREGION}
function  evalModel : boolean;
function  setCompartmentByIndex     (index : integer; value : double) : boolean;
function  setFloatingSpeciesByIndex (index : integer; value : double) : boolean;
function  setBoundarySpeciesByIndex (index : integer; value : double) : boolean;
function  setGlobalParameterByIndex (index : integer; value : double) : boolean;
function  getCompartmentByIndex     (index : integer) : double;
function  getFloatingSpeciesByIndex (index : integer) : double;
function  getBoundarySpeciesByIndex (index : integer) : double;
function  getGlobalParameterByIndex (index : integer) : double;
function 	getUnscaledFluxControlCoefficientMatrix : T2DMatrix;
function  getUnScaledConcentrationControlCoefficientMatrix : T2DMatrix;
function  getUnScaledElasticityMatrix : T2DMatrix;
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
function  loadRoadRunner (var errMsg : AnsiString) : boolean;
procedure releaseRoadRunnerLibrary;
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

// This is tempoary until  restructuring code is complete
var
   internalRRHandle : Pointer;

implementation

Uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  uRoadRunner.API;

type
  TLibGetAvailableSymbols = function (rrHandle : Pointer) : PRRListRecordHandle; cdecl;
  TlibSetInitialConditions = function (rrHandle : Pointer; vec : PRRDoubleVectorHandle) : boolean; cdecl;
  TlibComputeSteadyStateValues = function (rrHandle : Pointer): PRRDoubleVectorHandle; cdecl;

var DLLHandle : HMODULE;
    libName : string;
    currentLoggingPath : AnsiString;

    // All these method pointers have to be moved to uRoadRunner.API

    libHasError : TVoidBoolFunc;
    libEnableLoggingToConsole : TVoidBoolFunc;
    libEnableLoggingToFile : function : integer; cdecl;
    libEnableLoggingToFileWithPath : function (str : PAnsiChar) : integer; cdecl;
    libSetLogLevel : function (str : PAnsiChar) : integer; cdecl;    libGetLogFileName : function : PAnsiChar; cdecl;
    libSetLogFileName : function (str : PAnsiChar) : Integer; cdecl;
    libFreeRRInstance : TFreeRRInstance;
    libSetFloatingSpeciesInitialConcentrations : function (rrHandle : Pointer; value : Pointer) : boolean; cdecl;
    libGetCapabilities : THandleCharFunc;
    libSetCapabilities : TCharBoolFunc;
    libEvalModel : TVoidBoolFunc;


    libSetSteadyStateSelectionList : TCharBoolFunc;
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
    libSetCurrentIntegrator : TIntHandleChar;
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
    libFreeText : TCharBoolFunc;

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


function extractList (list : PRRListRecordHandle) : TRRList;
var i : integer;
    item : PRRListItemRecord;
begin
  result := TRRList.Create;
  if list = nil then
     exit (result);

  for i := 0 to list^.count - 1 do
      begin
      item := TRoadRunnerAPI.libGetListItem (list, i);
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
// Start of TRoadRunner Class
// -----------------------------------------------------------------

constructor TRoadRunner.Create;
begin
  inherited;
  roadRunnerInstance := TRoadRunnerAPI.libCreateRRInstance();
  if roadRunnerInstance = nil then
     raise Exception.Create('Error in libCreateRRInstance: ' + getLastError());
end;


destructor TRoadRunner.Destroy;
begin
  if roadRunnerInstance <> nil then
     libFreeRRInstance (roadRunnerInstance);
  inherited;
end;

function TRoadRunner.getVersion : integer;
begin
  result := TRoadRunnerAPI.libGetVersionInt ();
end;


function TRoadRunner.getVersionStr : string;
var p : PAnsiChar;
begin
  p := TRoadRunnerAPI.libGetVersionStr();
  result := AnsiString (p);
end;


function TRoadRunner.getCopyright : AnsiString;
var p : PAnsiChar;
begin
  p := TRoadRunnerAPI.libGetCopyright();
  result := AnsiString (p);
end;


function TRoadRunner.getlibSBMLVersion : AnsiString;
var p : PAnsiChar;
begin
  p := TRoadRunnerAPI.libGetlibSBMLVersion();
  result := AnsiString (p);
end;



function TRoadRunner.getLastError : string;
begin
  result := TRoadRunnerAPI.libGetLastError();
end;


function TRoadRunner.loadIntoRRMatrix  (mat : T2DMatrix) : PRRMatrixHandle;
var i, j : integer;
    r, c : integer;
    str : AnsiString;
begin
  r := mat.r; c := mat.c;
  result := libCreateRRMatrix (r, c);
  if result = nil then
     raise Exception.Create(getLastError());

  for i := 0 to r - 1 do
      for j := 0 to c - 1 do
          if libSetMatrixElement (result, i, j, mat[i,j]) = False then
             begin
             str := getLastError();
             raise Exception.Create ('Error while calling setMatrixElement: ' + str);
             end;
end;


function TRoadRunner.getSBML : AnsiString;
var p : PAnsiChar;
begin
  p := TRoadRunnerAPI.libGetSBML (roadRunnerInstance);
  result := AnsiString (p);
end;


procedure TRoadRunner.reset;
begin
  if TRoadRunnerAPI.libReset (roadRunnerInstance) = False then
     raise Exception.Create('Error in reset: ' + getLastError()) ;
end;


function TRoadRunner.loadSBMLFromString (sbmlStr : string) : boolean;
begin
  result := TRoadRunnerAPI.libLoadSBMLFromString (roadRunnerInstance, PAnsiChar (UTF8Encode(sbmlStr)));
end;


function TRoadRunner.loadSBMLFromFile (fileName : string) : boolean;
var mystr : AnsiString;
begin
  mystr := AnsiString (fileName);
  if FileExists (fileName) then
     begin
     result := TRoadRunnerAPI.libLoadSBMLFromFile (roadRunnerInstance, PAnsiChar (mystr))
     end
  else
     raise Exception.Create ('Unable to locate SBML file [' + fileName + ']');
end;


function TRoadRunner.getNumberOfFloatingSpecies : integer;
begin
  result := TRoadRunnerAPI.libGetNumberOfFloatingSpecies (roadRunnerInstance);
end;


function TRoadRunner.getNumberOfGlobalParameters : integer;
begin
  result := TRoadRunnerAPI.libGetNumberOfGlobalParameters (roadRunnerInstance);
end;


function TRoadRunner.getNumberOfCompartments : integer;
begin
  result := TRoadRunnerAPI.libGetNumberOfCompartments (roadRunnerInstance);
end;


function TRoadRunner.getNumberOfReactions : integer;
begin
  result := TRoadRunnerAPI.libGetNumberOfReactions (roadRunnerInstance);
end;


function TRoadRunner.getNumberOfBoundarySpecies : integer;
begin
  result := TRoadRunnerAPI.libGetNumberOfBoundarySpecies (roadRunnerInstance);
end;


function TRoadRunner.getNumberOfDependentSpecies : integer;
begin
  result := TRoadRunnerAPI.libGetNumberOfDependentSpecies (roadRunnerInstance);
end;


function TRoadRunner.getNumberOfIndependentSpecies : integer;
begin
  result := TRoadRunnerAPI.libGetNumberOfIndependentSpecies (roadRunnerInstance);
end;


function TRoadRunner.getCompartmentIds : TStringList;
var pList : PRRStringArray;
begin
  pList := TRoadRunnerAPI.libGetCompartmentIds (roadRunnerInstance);
  if pList <> nil then
     try
       result := getArrayOfStrings(pList);
     finally
       TRoadRunnerAPI.libFreeStringArray (pList);
     end
  else
    result := TStringList.Create;
end;


function TRoadRunner.getReactionIds : TStringList;
var pList : PRRStringArray;
begin
  pList := TRoadRunnerAPI.libGetReactionIds (roadRunnerInstance);
  if pList <> nil then
     try
       result := getArrayOfStrings(pList);
     finally
       TRoadRunnerAPI.libFreeStringArray (pList);
     end
  else
     result := TStringList.Create;
end;


function TRoadRunner.getBoundarySpeciesIds : TStringList;
var p : PRRStringArray; i : integer;
begin
  p := TRoadRunnerAPI.libGetBoundarySpeciesIds (roadRunnerInstance);
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings(p);
    for i := 0 to result.Count - 1 do
        result[i] := '[' + result[i] + ']';
  finally
    TRoadRunnerAPI.libFreeStringArray (p);
  end;
end;


function TRoadRunner.getFloatingSpeciesIds : TStringList;
var p : PRRStringArray; i : integer;
begin
  p :=  TRoadRunnerAPI.libGetFloatingSpeciesIds (roadRunnerInstance);
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings(p);
    for i := 0 to result.Count - 1 do
        result[i] := '[' + result[i] + ']';
  finally
    TRoadRunnerAPI.libFreeStringArray (p);
  end;
end;


function TRoadRunner.getGlobalParameterIds : TStringList;
var p : PRRStringArray;
begin
  p := TRoadRunnerAPI.libGetGlobalParameterIds (roadRunnerInstance);
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings (p);
  finally
   TRoadRunnerAPI. libFreeStringArray (p);
  end;
end;


function TRoadRunner.getRatesOfChangeIds : TStringList;
var p : PRRStringArray;
begin
  p := TRoadRunnerAPI.libGetRatesOfChangeIds (roadRunnerInstance);
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings (p);
  finally
    TRoadRunnerAPI.libFreeStringArray (p);
  end;
end;


function TRoadRunner.getEigenvalueIds : TStringList;
var p : PRRStringArray;
begin
  p := TRoadRunnerAPI.libGetEigenvalueIds (roadRunnerInstance);
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings (p);
  finally
    TRoadRunnerAPI.libFreeStringArray (p);
  end;
end;


function TRoadRunner.getElasticityIds : TRRList;
var p : PRRListRecordHandle;
    l : TRRList;
    i, j, k : integer;
begin
  p := TRoadRunnerAPI.libGetElasticityIds (roadRunnerInstance);
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


function TRoadRunner.getFloatingSpeciesInitialConditionIds : TStringList;
var p : PRRStringArray;
begin
  p := TRoadRunnerAPI.libGetFloatingSpeciesInitialConditionIds (roadRunnerInstance);
  try
    if p = nil then
       result := TStringList.Create
    else
       result := getArrayOfStrings(p);
  finally
    TRoadRunnerAPI.libFreeStringArray (p);
  end;
end;



function TRoadRunner.getFloatingSpeciesConcentrations : TArray<double>;
var p : PRRDoubleVectorHandle; i : integer;
begin
  p := TRoadRunnerAPI.libGetFloatingSpeciesConcentrations (roadRunnerInstance);
  try
    setLength (result, p^.count);
    for i := 0 to p^.count - 1 do
        result[i] := p^.data[i];
  finally
    if p^.count > 0 then
       TRoadRunnerAPI.libFreeDoubleVector (p);
   end;
end;


function TRoadRunner.setFloatingSpeciesInitialConcentrations (value : TDoubleArray) : boolean;
var p : PRRDoubleVectorHandle;  i : integer;
begin
 p := TRoadRunnerAPI.libCreateVector (length (value));
 try
   for i := 0 to length (value) - 1 do
       p^.data[i] := value[i];
   result := TRoadRunnerAPI.libSetFloatingSpeciesInitialConcentrations (roadRunnerInstance, p);
 finally
   TRoadRunnerAPI.libFreeDoubleVector (p);
 end;
end;


function TRoadRunner.getFloatingSpeciesInitialConcentrations : TDoubleArray;
var p : PRRDoubleVectorHandle; i : integer;
begin
  p := TRoadRunnerAPI.libGetFloatingSpeciesInitialConcentrations (roadRunnerInstance);
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
    // HMS Crashes if p is empty: TRoadRunnerAPI.libFreeDoubleVector (p);
  end;
end;


function TRoadRunner.getTimeCourseSelectionList: TStringList;
var ptr : PRRStringArray;
begin
  ptr := TRoadRunnerAPI.libGetTimeCourseSelectionList (roadRunnerInstance);
  result := RRStringArrayToStringList (ptr)
end;


function TRoadRunner.setComputeAndAssignConservationLaws (value : boolean) : boolean;
begin
  result := TRoadRunnerAPI.libSetComputeAndAssignConservationLaws (roadRunnerInstance, value);
end;


function TRoadRunner.getValue (Id : AnsiString) : double;
var d: double;
    s : AnsiString;
    p : PDouble;
begin
  s := Id;
  p := @d;
  if not TRoadRunnerAPI.libGetValue (roadRunnerInstance, PAnsiChar (s), p) then
     raise Exception.Create ('Error: ' + getLastError());
  result := d;
end;


function TRoadRunner.setValue (Id : AnsiString; value : double) : boolean;
begin
  result := TRoadRunnerAPI.libSetValue (roadRunnerInstance, PAnsiChar (Id), value);
end;


function TRoadRunner.getReactionRates : TDoubleArray;
var p : PRRDoubleVectorHandle; i : integer;
begin
  p := TRoadRunnerAPI.libGetReactionRates (roadRunnerInstance);
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
    TRoadRunnerAPI.libFreeDoubleVector (p);
  end;
end;


function TRoadRunner.getStoichiometryMatrix : T2DMatrix;
var st : PRRMatrixHandle;
    i : integer;
    nRows, nCols : integer;
    astr : string;
begin
  st := TRoadRunnerAPI.libGetStoichiometryMatrix (roadRunnerInstance);
  try
    if st = nil then
       begin
       astr := TRoadRunnerAPI.libGetLastError();
       raise Exception.Create (astr);
       end;
     result := T2DMatrix.Create (st.RSize, st.CSize);
     Move(st.data^, result.data^, (st.RSize*st.CSize)*SizeOf(double));
  finally
     TRoadRunnerAPI.libFreeMatrix (st);
  end;
end;


function TRoadRunner.getFullJacobian : T2DMatrix;
var p : PRRMatrixHandle;
begin
  p := TRoadRunnerAPI.libGetFullJacobian (roadRunnerInstance);
  if p = nil then
     raise Exception.Create ('No Jacobian matrix: ' + getLastError());
  try
     result := T2DMatrix.Create (p.RSize, p.CSize);
     Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
    TRoadRunnerAPI.libFreeMatrix (p);
  end;
end;


function TRoadRunner.getReducedJacobian : T2DMatrix;
var p : PRRMatrixHandle;
    str : AnsiString;
begin
  p := TRoadRunnerAPI.libGetReducedJacobian (roadRunnerInstance);
  if p = nil then
     begin
     str := TRoadRunnerAPI.libGetLastError();
     raise Exception.Create (str);
     end;
  try
    result := T2DMatrix.Create (p.RSize, p.CSize);
    Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
   TRoadRunnerAPI. libFreeMatrix (p);
  end;
end;


function TRoadRunner.getLinkMatrix : T2DMatrix;
var p : PRRMatrixHandle;
    i, j : integer;
begin
  p := TRoadRunnerAPI.libGetLinkMatrix (roadRunnerInstance);
  try
    if p = nil then
       begin
       result := T2DMatrix.Create (0, 0);
       exit;
       end;
    result := T2DMatrix.Create (p.RSize, p.CSize);
    Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
    TRoadRunnerAPI.libFreeMatrix (p);
  end;
end;


function TRoadRunner.getNrMatrix : T2DMatrix;
var p : PRRMatrixHandle;
begin
  p := TRoadRunnerAPI.libGetNrMatrix (roadRunnerInstance);
  try
    if p = nil then
       begin
       result := T2DMatrix.Create (0, 0);
       exit;
       end;
    result := T2DMatrix.Create (p.RSize, p.CSize);
    Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
    TRoadRunnerAPI.libFreeMatrix (p);
  end;
end;


function TRoadRunner.getL0Matrix : T2DMatrix;
var p : PRRMatrixHandle;
begin
  p := TRoadRunnerAPI.libGetL0Matrix (roadRunnerInstance);
  try
    if p = nil then
       begin
       result := T2DMatrix.Create (0, 0);
       exit;
       end;
    result := T2DMatrix.Create (p.RSize, p.CSize);
    Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
    TRoadRunnerAPI.libFreeMatrix (p);
  end;
end;


function TRoadRunner.getConservationMatrix : T2DMatrix;
var p : PRRMatrixHandle;
begin
  p := TRoadRunnerAPI.libGetConservationMatrix (roadRunnerInstance);
  try
    if p = nil then
       begin
       result := T2DMatrix.Create (0, 0);
       exit;
       end;
    result := T2DMatrix.Create (p.RSize, p.CSize);
    Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
    TRoadRunnerAPI.libFreeMatrix (p);
  end;
end;


function TRoadRunner.getBoundarySpeciesConcentrations : TDoubleArray;
var p : PRRDoubleVectorHandle; i : integer;
begin
  p := TRoadRunnerAPI.libGetBoundarySpeciesConcentrations (roadRunnerInstance);
  try
    setLength (result, p^.count);
    for i := 0 to p^.count - 1 do
        result[i] := p^.data[i];
  finally
    if p^.count > 0 then
       TRoadRunnerAPI.libFreeDoubleVector (p);
  end;
end;


function TRoadRunner.setFloatingSpeciesConcentrations (value : TDoubleArray) : boolean;
var p : PRRDoubleVectorHandle;  i : integer;
begin
 p := TRoadRunnerAPI.libCreateVector (length (value));
 for i := 0 to length (value) - 1 do
     p^.data[i] := value[i];
 result := TRoadRunnerAPI.libSetFloatingSpeciesConcentrations (roadRunnerInstance, p);
 TRoadRunnerAPI.libFreeDoubleVector (p);
end;


function TRoadRunner.setBoundarySpeciesConcentrations (value : TDoubleArray) : boolean;
var p : PRRDoubleVectorHandle;  i : integer;
begin
 p := TRoadRunnerAPI.libCreateVector (length (value));
 for i := 0 to length (value) - 1 do
     p^.data[i] := value[i];
 result := TRoadRunnerAPI.libSetBoundarySpeciesConcentrations (roadRunnerInstance, p);
 TRoadRunnerAPI.libFreeDoubleVector (p);
end;



function TRoadRunner.getRatesOfChange : TDoubleArray;
var p : PRRDoubleVectorHandle; i : integer;
begin
  i := getNumberOfReactions();
  p := TRoadRunnerAPI.libGetRatesOfChange (roadRunnerInstance);
  try
    if p = nil then
       begin
       getLastError();
       setLength (result, 0);
       exit;
       end;
    setLength (result, p^.count);
    for i := 0 to p^.count - 1 do
        result[i] := p^.data[i];
  finally
    TRoadRunnerAPI.libFreeDoubleVector (p);
  end;
end;


function TRoadRunner.getEigenvalues : T2DMatrix;
var p : PRRMatrixHandle;
begin
  p := TRoadRunnerAPI.libGetEigenvalues (roadRunnerInstance);
  if p = nil then
     raise Exception.Create ('No Eigenvalue matrix:' + getLastError());
  try
    result := T2DMatrix.Create (p.RSize, p.CSize);
    Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
    TRoadRunnerAPI.libFreeMatrix (p);
  end;
end;


function TRoadRunner.getEigenvectors : T2DMatrix;
var p : PRRMatrixHandle;
begin
  p := TRoadRunnerAPI.libGetEigenvectors (roadRunnerInstance);
  if p = nil then
     raise Exception.Create ('No Eigenvectors: ' + getLastError());
  try
    result := T2DMatrix.Create (p.RSize, p.CSize);
    Move(p.data^, result.data^, (p.RSize*p.CSize)*SizeOf(double));
  finally
    TRoadRunnerAPI.libFreeMatrix (p);
  end;
end;


function  TRoadRunner.getEigenvaluesMatrix (m : T2DMatrix) : T2DMatrix;
var p1, p2 : PRRMatrixHandle;
begin
  p1 := loadIntoRRMatrix (m);
  if p1 = nil then
     exit;
  p2 := TRoadRunnerAPI.libGetEigenvaluesMatrix (p1);
  if p2 = nil then
     exit;
  result := T2DMatrix.Create (p2.RSize, p2.CSize);
  Move(p2.data^, result.data^, (p2.RSize*p2.CSize)*SizeOf(double));
end;


function TRoadRunner.setTimeCourseSelectionListEx (list : TStringList) : boolean;
var p : array of PAnsiChar;
    t : array of AnsiString;
   i : integer;
begin
  if list.Count = 0 then exit;

  setlength (p, list.count);
  setlength (t, list.count);

  for i := 0 to list.Count - 1 do
      begin
      t[i] := AnsiString (list[i]);
      p[i] := PAnsiChar (t[i]);
      end;
  if not TRoadRunnerAPI.libSetTimeCourseSelectionListEx (roadRunnerInstance, list.Count, @p[0]) then
     raise Exception.Create ('Internal error while calling setTimeCourseSelectionListEx: ' + getLastError());
end;


function TRoadRunner.getSteadyStateSelectionList: TStringList;
var ptr : PRRStringArray;
begin
  ptr := TRoadRunnerAPI.libGetSteadyStateSelectionList (roadRunnerInstance);
  if ptr = nil then
     raise Exception.Create(getLastError());
  result := RRStringArrayToStringList (ptr);
end;



procedure TRoadRunner.setTimeStart (value : double);
begin
  if not TRoadRunnerAPI.libSetTimeStart (roadRunnerInstance, value) then
     raise Exception.Create ('Internal error while calling setTimeStart');
end;


procedure TRoadRunner.setTimeEnd (value : double);
begin
  if not TRoadRunnerAPI.libSetTimeEnd (roadRunnerInstance, value) then
     raise Exception.Create ('Error while calling setTimeEnd');
end;


procedure TRoadRunner.setNumberOfPoints (value : integer);
begin
  if not TRoadRunnerAPI.libSetNumberOfPoints (roadRunnerInstance, value) then
     raise Exception.Create ('Error while calling setNumberOfPoints');
end;




function TRoadRunner.steadyState : double;
var errMsg : AnsiString;
begin
  if not TRoadRunnerAPI.libSteadyState (roadRunnerInstance, result) then
     begin
     errMsg := getLastError;
     raise Exception.Create (errMsg);
     end;
end;


function TRoadRunner.setSteadyStateSelectionListEx (list : TStringList) : boolean;
var p : array of PAnsiChar;
    t : array of AnsiString;
   i : integer;
begin
  if list.Count = 0 then exit;

  setlength (p, list.count);
  setlength (t, list.count);

  for i := 0 to list.Count - 1 do
      begin
      t[i] := AnsiString (list[i]);
      p[i] := PAnsiChar (t[i]);
      end;
  if not TRoadRunnerAPI.libSetSteadyStateSelectionListEx (roadRunnerInstance, list.Count, @p[0]) then
     raise Exception.Create ('Internal error while calling setSteadyStateSelectionListEx: ' + getLastError());
end;


 function TRoadRunner.computeSteadyStateValues : TDoubleArray;
var p : PRRDoubleVectorHandle; i : integer;
    errMsg : AnsiString;
    ans : double;
begin
  if not TRoadRunnerAPI.libSteadyState (roadRunnerInstance, ans) then
     begin
     errMsg := getLastError;
     raise Exception.Create (errMsg);
     end;
  p := TRoadRunnerAPI.libComputeSteadyStateValues (roadRunnerInstance);
  try
    setLength (result, p.count);
    for i := 0 to p.count - 1 do
        result[i] := p.data[i];
  finally
    TRoadRunnerAPI.libFreeDoubleVector (p);
  end;
end;



function TRoadRunner.oneStep (currentTime : double; stepSize : double) : double;
var newTime : double;
begin
  newTime := 0.0;
  if TRoadRunnerAPI.libOneStep (roadRunnerInstance, currentTime, stepSize, newTime) = 0 then
     raise Exception.Create ('Error in oneStep');
  result := newTime;
end;


{$POINTERMATH ON}
// This is to keep a handle on allocated arrays returned to caller.
// It appears the if we return the data only as a result, the data is
// deallocated.
//var tmpArray : T2DDoubleArray;
function TRoadRunner.simulate : T2DMatrix;
var RRResult : PRRCData;
    i, j : integer;
    nr, nc : integer;
    x : double;
    astr : AnsiString;
begin
  RRResult := TRoadRunnerAPI.libSimulate (roadRunnerInstance);
  if RRResult = nil then
     raise Exception.Create (getLastError());
  try
    nr := RRResult^.RSize;
    nc := RRResult^.CSize;
    //setLength (result, nr, nc);
    //result := tmpArray;
    result := T2DMatrix.Create (nr, nc);
    Move(RRResult^.data^, result.data^, (nr*nc)*SizeOf(double));

    for i := 0 to nc - 1 do
        begin
        astr := AnsiString (RRResult^.ColumnHeaders^[i]);
        result.columnHeader.Add(astr);
        end;

    //for i := 0 to nr - 1 do
    //    for j := 0 to nc - 1 do
    //        result[i,j] := RRResult^.data[i*nc + j];
  finally
     TRoadRunnerAPI.libFreeRCData (RRResult);
  end;
end;


function TRoadRunner.simulateEx (timeStart: double; timeEnd : double; numberOfPoints : integer) : T2DMatrix; // T2DDoubleArray;
var RRResult : PRRCData;
    i, j : integer;
    nr, nc : integer;
    astr : string;
begin
  RRResult := TRoadRunnerAPI.libSimulateEx (roadRunnerInstance, timeStart, timeEnd, numberOfPoints);
  if RRResult = nil then
     raise Exception.Create (getLastError());
  try
    nr := RRResult^.RSize;
    nc := RRResult^.CSize;
    result := T2DMatrix.Create (nr, nc);
    Move(RRResult^.data^, result.data^, (nr*nc)*SizeOf(double));
    for i := 0 to nc - 1 do
        begin
        astr := AnsiString (RRResult^.ColumnHeaders^[i]);
        result.columnHeader.Add(astr);
        end;

    //for i := 0 to nr - 1 do
    //    for j := 0 to nc - 1 do
    //        result[i,j] := RRResult^.data[i*nc + j];
  finally
    TRoadRunnerAPI.libFreeRCData (RRResult);
  end;
end;


function TRoadRunner.getScaledElasticityMatrix : T2DMatrix;
var p1 : PRRMatrixHandle;
begin
  p1 := TRoadRunnerAPI.libGetScaledElasticityMatrix (roadRunnerInstance);
  if p1 = nil then
     raise Exception.Create ('Error in libgetEEMatrix function' + getLastError);
  result := T2DMatrix.Create (p1.RSize, p1.CSize);
  Move(p1.data^, result.data^, (p1.RSize*p1.CSize)*SizeOf(double));
end;


function TRoadRunner.getScaledConcentrationControlCoefficientMatrix : T2DMatrix;
var p1 : PRRMatrixHandle;
begin
  p1 := TRoadRunnerAPI.libGetCCCMatrix (roadRunnerInstance);
  if p1 = nil then
     raise Exception.Create ('Error in FCC function' + getLastError);
  result := T2DMatrix.Create (p1.RSize, p1.CSize);
  Move(p1.data^, result.data^, (p1.RSize*p1.CSize)*SizeOf(double));
end;


function TRoadRunner.getScaledFluxControlCoefficientMatrix : T2DMatrix;
var p1 : PRRMatrixHandle;
begin
  p1 := TRoadRunnerAPI.libGetFCCMatrix (roadRunnerInstance);
  if p1 = nil then
     raise Exception.Create ('Error in libGetFCCMatrix function' + getLastError);
  result := T2DMatrix.Create (p1.RSize, p1.CSize);
  Move(p1.data^, result.data^, (p1.RSize*p1.CSize)*SizeOf(double));
end;


// -----------------------------------------------------------------

// -----------------------------------------------------------------
// For doumentation, see the C API docs at:
//      http://libroadrunner.org
// -----------------------------------------------------------------

function XgetLastError() : string;
begin
  result := 'Not yet implemented';
end;



function createInternalRRInstance : Pointer;
begin
  internalRRHandle := TRoadRunnerAPI.libCreateRRInstance();
  result := internalRRHandle;
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


function getComputeAndAssignConservationLaws : Boolean;
var value : Integer;
begin
  if not boolean (TRoadRunnerAPI.libGetComputeAndAssignConservationLaws (internalRRHandle, value)) then
     raise Exception.Create ('Error calling getComputeAndAssignConservationLaws' + XgetLastError());
  result := boolean (value);
end;


function setBoundarySpeciesInitialConcentrations (value : TDoubleArray) : boolean;
var p : PRRDoubleVectorHandle;  i : integer;
begin
 p := TRoadRunnerAPI.libCreateVector (length (value));
 for i := 0 to length (value) - 1 do
     p^.data[i] := value[i];
 result := TRoadRunnerAPI.libSetBoundarySpeciesConcentrations (internalRRHandle, p);
 TRoadRunnerAPI.libFreeDoubleVector (p);
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
    TRoadRunnerAPI.libFreeDoubleVector (p);
  end;
end;


function evalModel : boolean;
begin
  result := libEvalModel (internalRRHandle);
end;


function getTimeCourseSelectionList: TStringList;
var ptr : PRRStringArray;
begin
  ptr := TRoadRunnerAPI.libGetTimeCourseSelectionList (internalRRHandle);
  result := RRStringArrayToStringList (ptr)
end;


function setCompartmentByIndex (index : integer; value : double) : boolean;
begin
  result := TRoadRunnerAPI.libSetCompartmentByIndex (internalRRHandle, index, value);
end;


function setFloatingSpeciesByIndex (index : integer; value : double) : boolean;
begin
  result := TRoadRunnerAPI.libSetFloatingSpeciesByIndex (internalRRHandle, index, value);
end;


function setBoundarySpeciesByIndex (index : integer; value : double) : boolean;
begin
  result := TRoadRunnerAPI.libSetBoundarySpeciesByIndex (internalRRHandle, index, value);
end;

function setGlobalParameterByIndex (index : integer; value : double) : boolean;
begin
  result := TRoadRunnerAPI.libSetGlobalParameterByIndex (internalRRHandle, index, value);
end;


function getCompartmentByIndex (index : integer) : double;
begin
  if not TRoadRunnerAPI.libGetCompartmentByIndex (internalRRHandle, index, result) then
     raise Exception.Create ('Index out of range in getCompartmentByIndex');
end;


function getFloatingSpeciesByIndex (index : integer) : double;
begin
  if not TRoadRunnerAPI.libGetFloatingSpeciesByIndex (internalRRHandle, index, result) then
     raise Exception.Create ('Index out of range in getFloatingSpeciesByIndex');
end;


function getBoundarySpeciesByIndex (index : integer) : double;
begin
  if not TRoadRunnerAPI.libGetBoundarySpeciesByIndex (internalRRHandle, index, result) then
     raise Exception.Create ('Index out of range in getBoundarySpeciesByIndex');
end;


function getGlobalParameterByIndex (index : integer) : double;
begin
  if not TRoadRunnerAPI.libGetGlobalParameterByIndex (internalRRHandle, index, result) then
     raise Exception.Create ('Index out of range in getGlobalParameterByIndex');
end;


function getUnscaledFluxControlCoefficientMatrix : T2DMatrix;
var p1 : PRRMatrixHandle;
begin
  p1 := TRoadRunnerAPI.libGetuFCCMatrix (internalRRHandle);
  if p1 = nil then
     raise Exception.Create(XgetLastError());

  result := T2DMatrix.Create (p1.RSize, p1.CSize);
  Move(p1.data^, result.data^, (p1.RSize*p1.CSize)*SizeOf(double));
end;


function getUnScaledConcentrationControlCoefficientMatrix : T2DMatrix;
var p1 : PRRMatrixHandle;
begin
  p1 := TRoadRunnerAPI.libGetuCCCMatrix (internalRRHandle);
  if p1 = nil then
     raise Exception.Create ('Error in libGetFCCMatrix function' + XgetLastError);
  result := T2DMatrix.Create (p1.RSize, p1.CSize);
  Move(p1.data^, result.data^, (p1.RSize*p1.CSize)*SizeOf(double));
end;


function getUnScaledElasticityMatrix : T2DMatrix;
var p1 : PRRMatrixHandle;
begin
  p1 := TRoadRunnerAPI.libGetuEEMatrix (internalRRHandle);
  if p1 = nil then
     raise Exception.Create ('Error in libgetuEEMatrix function' + XgetLastError);
  result := T2DMatrix.Create (p1.RSize, p1.CSize);
  Move(p1.data^, result.data^, (p1.RSize*p1.CSize)*SizeOf(double));
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
  //st := getFloatingSpeciesIds ();
  for i := 0 to st.Count - 1 do
      subList.Add (TRRListItem.Create (st[i]));
  result.Add (TRRListItem.Create (subList));
  st.Free;
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


function getReactionRate (index : integer) : double;
begin
  result := TRoadRunnerAPI.libGetReactionRate (internalRRHandle, index);
end;


//function mGetConservationMatrix : TMatrix;
//var st : PRRMatrixHandle;
//begin
//  st := TRoadRunnerAPI.libGetConservationMatrix (internalRRHandle);
//  try
//    if st = nil then
//       begin
//       result := nil;
//       exit;
//       end;
//    result := loadInToMatrix (st);
//  finally
//    TRoadRunnerAPI.libFreeMatrix (st);
//  end;
//end;

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
     str := XgetLastError();
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
     str := XgetLastError();
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
     str := XgetLastError();
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
     str := XgetLastError();
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
  if not libSetCurrentIntegratorParameterBoolean (internalRRHandle, PAnsiChar (_name), integer (value)) then
     begin
     raise Exception.Create('Error in setIntegratorParameterBoolean: ' + XgetLastError());
     end;
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
  if res = 0 then
     raise Exception.Create('Failed to set Steady State Solver: ' + XgetLastError);
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
     str := XgetLastError();
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
     str := XgetLastError();
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
     str := XgetLastError();
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
     str := XgetLastError();
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
  if not libSetCurrentSteadyStateSolverParameterBoolean (internalRRHandle, PAnsiChar (_name), integer (value)) then
     raise Exception.Create('Error in setCurrentSteadyStateSolverParameterBoolean: ' + XgetLastError());
end;


procedure setConfigBoolean (key : string; value : Boolean);
var s : AnsiString;
    ib : integer;
begin
  s := AnsiString (key);
  ib := integer (value);
  if libSetConfigBool (PAnsiChar (s), ib) = 0 then
     raise Exception.Create('Error in SetConfigBoolean: ' + XgetLastError());
end;


procedure setConfigInteger (key : string; value : integer);
var s : AnsiString;
begin
  s := AnsiString (key);
  if libSetConfigInt (PAnsiChar (s), value) = 0 then
     raise Exception.Create('Error in SetConfigInteger: ' + XgetLastError());
end;


procedure setConfigDouble (key : string; value : double);
var s : AnsiString;
begin
  s := AnsiString (key);
  if libSetConfigDouble (PAnsiChar (s), value) = 0 then
     raise Exception.Create('Error in SetConfigDouble: ' + XgetLastError());
end;


function getConfigBoolean (key : string) : Boolean;
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

// This has to be moved to uRoadRunner.API and deprecated
function loadMethods (var errMsg : AnsiString) : boolean;
var methodList : TStringList;
begin
   methodList := TStringList.Create;
   result := true;
   try
   @libHasError      := loadSingleMethod ('hasError', errMsg, result, methodList);
   @libSetLogLevel   := loadSingleMethod ('setLogLevel', errMsg, result, methodList);
   @libEnableLoggingToConsole := loadSingleMethod ('enableLoggingToConsole', errMsg, result, methodList);
   @libEnableLoggingToFile := loadSingleMethod ('enableLoggingToFile', errMsg, result, methodList);
   @libEnableLoggingToFileWithPath := loadSingleMethod ('enableLoggingToFileWithPath', errMsg, result, methodList);
   @libGetLogFileName := loadSingleMethod ('getLogFileName', errMsg, result, methodList);
   @libSetFloatingSpeciesInitialConcentrations := loadSingleMethod ('setFloatingSpeciesInitialConcentrations', errMsg, result, methodList);
   @libEvalModel          := loadSingleMethod ('evalModel', errMsg, result, methodList);
   //@libGetSteadyStateSelectionList  := loadSingleMethod ('getSteadyStateSelectionList', errMsg, result, methodList);

   @libGetAvailableTimeCourseSymbols  := loadSingleMethod ('getAvailableTimeCourseSymbols', errMsg, result, methodList);
   @libGetAvailableSteadyStateSymbols := loadSingleMethod ('getAvailableSteadyStateSymbols', errMsg, result, methodList);
   @libgetuCC                 := loadSingleMethod ('getuCC', errMsg, result, methodList);
   @libgetuEE                 := loadSingleMethod ('getuEE', errMsg, result, methodList);
   @libgetCC                  := loadSingleMethod ('getCC', errMsg, result, methodList);
   @libgetEE                  := loadSingleMethod ('getEE', errMsg, result, methodList);

   @libFreeRRInstance         := loadSingleMethod ('freeRRInstance', errMsg, result, methodList);
   @libFreeText         := loadSingleMethod ('freeText', errMsg, result, methodList);
   @libCreateRRMatrix   := loadSingleMethod ('createRRMatrix', errMsg, result, methodList);
   @libSetMatrixElement := loadSingleMethod ('setMatrixElement', errMsg, result, methodlist);

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
   @libSetCurrentIntegrator := loadSingleMethod ('setCurrentIntegrator', errMsg, result, methodList);
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
   methodList.Free;
end;


// Delete this
function loadRoadRunner (var errMsg : AnsiString) : boolean;
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
           if loadMethods (errMsg) then
              begin
              DLLLoaded := True;
              result := true;
              end
           else
              result := false;
           end
       else
           begin
             {$IFDEF VER220} // Delphi XE
               //errStr := SysErrorMessage(Windows.GetLastError);
             {$ELSE}
               //errStr := SysErrorMessage(Winapi.Windows.GetLastError);
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


// Delete this
procedure releaseRoadRunnerLibrary;
begin
  DLLLoaded := false;
  freeLibrary (DLLHandle);
end;


// Delete these
initialization
  {$IFDEF POSIX}
     libName := 'libroadrunner_c_api.dylib';
  {$ELSE}
    libName := 'libRoadRunner\bin\roadrunner_c_api.dll';
  {$ENDIF}
end.
