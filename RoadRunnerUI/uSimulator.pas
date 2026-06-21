unit uSimulator;

interface

Uses SysUtils,
     Classes,
     uRoadRunner,
     uRRTypes,
     uRR2DSimpleMatrix,
     Generics.Collections,
     uScanArguments,
     uModelInputManager,
     uParameterScan;

type
  TSimulator = class (TObject)

   private

   public
     timeEnd : double;
     timeStart : double;
     numberOfPoints : integer;

     scanControl : TScanControl;

     selectionList : TStringList;
     roadrunner : TRoadRunner;
     simulationData : T2DMatrix;   // simulator data is owned by this class.

     procedure setSelectionList (selectionList : TStringList);
     procedure  simulate;
     procedure runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
     procedure runSteadyStateScan (selectionList : TStringList; scanArguments : TScanArguments);
     function  loadSBMLFromString (sbmlStr : string) : boolean;
     procedure updateModel (astr : string);
     procedure InvalidateSimulationData;

     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses IOUtils, uAntimonyAPI, FMX.Dialogs;

constructor TSimulator.Create;
begin
  inherited;
  roadRunner := TRoadRunner.Create;
  selectionList := TStringList.Create;
  scanControl := TScanControl.Create (self);
end;


destructor TSimulator.Destroy;
begin
  selectionList.Free;
  scanControl.Free;
  if roadrunner <> nil then
     roadrunner.Free;
  inherited;
end;


procedure TSimulator.setSelectionList (selectionList : TStringList);
begin
  self.selectionList.Assign (selectionList);
  roadrunner.setTimeCourseSelectionListEx(self.selectionList);
  if not Assigned (scanControl) then
     scanControl := TScanControl.Create (self);
end;


procedure TSimulator.simulate;
begin
  if Assigned (simulationData) then
     begin
     simulationData.Free;
     simulationData := nil;
     end;

  simulationData := roadrunner.simulateEx(timeStart, timeEnd, numberOfPoints);
  simulationData.valid := True;
end;


procedure TSimulator.runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
begin
  try
    scanControl.timeStart := scanArguments.FTimeStart;
    scanControl.timeEnd := scanArguments.FTimeEnd;
    scanControl.numberOfPoints := scanArguments.FNumberOfPoints;
    scanControl.parameterId := scanArguments.FParameterId;
    scanControl.parameterId := scanArguments.FParameterId;
    scanControl.numOfScans := scanArguments.FNumberOfScans;
    scanControl.logScan := scanArguments.FLogScan;
    scanControl.minValue := scanArguments.minValue;
    scanControl.maxValue :=  scanArguments.maxValue;
    scanControl.scanValuesType := scanArguments.FScanValuesType;
    scanControl.listOfValues := scanArguments.FListValues;

    simulationData := scanControl.doTimeCourseSingleParameterScan(selectionList, scanArguments);
  finally
    // Do not free scancontrol!
  end;
end;


procedure TSimulator.runSteadyStateScan (selectionList : TStringList; scanArguments : TScanArguments);
begin
  try
    // Single scan
    scanControl.parameterId := scanArguments.FParameterId;
    scanControl.numOfScans := scanArguments.FNumberOfScans;
    scanControl.logScan := scanArguments.FLogScan;
    scanControl.minValue := scanArguments.minValue;
    scanControl.maxValue :=  scanArguments.maxValue;
    scanControl.scanValuesType := scanArguments.FScanValuesType;

    simulationData := scanControl.doSteadyStateSingleParameterScan(selectionList);
  finally
    // Do not free scancontrol!
  end;
end;


procedure TSimulator.updateModel (astr : string);
begin

end;


function TSimulator.loadSBMLFromString (sbmlStr : string) : boolean;
begin
  result := roadrunner.loadSBMLFromString(sbmlStr);
end;


procedure TSimulator.InvalidateSimulationData;
begin
  if simulationData <> nil then
     simulationData.valid := False;
end;

//function TSimulator.loadAntModelFromString (antimonyStr : string) : TModelErrorState;
//var antStr, sbmlStr, errMsg : string;
//    modelErrorState : TModelErrorState;
//begin
//  modelErrorState := uAntimonyAPI.getSBMLFromAntimony(antimonyStr);
//  if not modelErrorState.ok then
//     begin
//     exit (modelErrorState);
//     end;
//
//  if not roadrunner.loadSBMLFromString (modelErrorState.sbmlStr) then
//     begin
//     modelErrorState.errMsg := roadrunner.getLastError();
//     modelErrorState.ok := False;
//     result := modelErrorState;
//     end
//  else
//    result := modelErrorState;
//end;

//
//function TSimulator.loadAntModelFromFile (fileName : string) : boolean;
//var antStr, sbmlStr, errMsg : string;
//begin
//  antStr := TFile.ReadAllText(fileName);
//  sbmlStr := uAntimonyAPI.getSBMLFromAntimony(antStr);
//  if not roadrunner.loadSBMLFromString(sbmlStr) then
//     begin
//     errMsg := roadrunner.getLastError();
//     exit (False);
//     end
//  else
//    exit (True);
//end;


end.
