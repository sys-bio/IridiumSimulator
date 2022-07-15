unit uSimulator;

interface

Uses SysUtils,
     Classes,
     uRoadRunner,
     uRRTypes,
     Generics.Collections,
     uScanArguments,
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
     simulationData : T2DMatrix;

     procedure setSelectionList (selectionList : TStringList);
     function  simulate : T2DMatrix;
     procedure runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
     procedure runSteadyStateScan (selectionList : TStringList; scanArguments : TScanArguments);
     function  loadSBMLFromString (sbmlStr : string) : boolean;
     function  loadAntModelFromString (antimonyStr : string) : boolean;
     procedure updateModel (astr : string);

     constructor Create;
     destructor  Destroy; override;
  end;


implementation

Uses IOUtils, uAntimonyAPI;

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
  scanControl := TScanControl.Create (self);
end;


function TSimulator.simulate : T2DMatrix;
var i : integer;
begin
  result := roadrunner.simulateEx(timeStart, timeEnd, numberOfPoints);
  simulationData := result;
end;

procedure TSimulator.runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
begin
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

    simulationData := scanControl.doTimeCourseSingleParameterScan(selectionList, scanArguments);
end;


procedure TSimulator.runSteadyStateScan (selectionList : TStringList; scanArguments : TScanArguments);
var
  i: integer;
  scanValues1, scanValues2: TDoubleArray;
  m, scanResult: T2DMatrix;
  parameterStr: string;
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

    // else
    // begin
    // // scanValues2 := getScanValues(strtofloat (edtMin2.text), strtofloat (edtMax2.text), strtoint (edtNumScanPoints2.Text),
    // // chkLogScan2.checked, edtManualValues2.Text, chkScanManual2.checked);
    // // for i := 0 to lstOutputScan.Count - 1 do
    // // selectionList.Add (scanSelectionList[i]);
    // //
    // // m := doSteadyStateDoubleParameterScan (cboScan1.Items[cboScan1.ItemIndex], cboScan2.Items[cboScan2.ItemIndex], selectionList, scanValues1, scanValues2);
    // // selectionList.Insert (0, parameterStr);
    // //
    // // tmp := TCMatrix.Create (length (scanValues1), 1);
    // // tmp.setColumnName(1, parameterStr);
    // // for i := 0 to length (scanValues1) - 1 do
    // // tmp[i+1,1] := complex (scanValues1[i], 0);
    // //
    // // scanResult := TCMatrix.Create;
    // // scanResult.augment (tmp, m);
    // //
    // // buildScanChart (selectionList);
    // // UpdateScanViewer (scanResult, 1, selectionList, true);
    // end;

  finally
    //scanControl.Free;
  end;
end;


procedure TSimulator.updateModel (astr : string);
begin

end;


function TSimulator.loadSBMLFromString (sbmlStr : string) : boolean;
begin
  result := roadrunner.loadSBMLFromString(sbmlStr);
end;


function TSimulator.loadAntModelFromString (antimonyStr : string) : boolean;
var antStr, sbmlStr, errMsg : string;
begin
  sbmlStr := uAntimonyAPI.getSBMLFromAntimony(antimonyStr);
  if not roadrunner.loadSBMLFromString(sbmlStr) then
     begin
     errMsg := roadrunner.getLastError();
     exit (False);
     end
  else
    exit (True);
end;

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
