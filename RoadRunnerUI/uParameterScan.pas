unit uParameterScan;

interface

Uses Classes,
     SysUtils,
     uMatrix,
     System.UITypes,
     uRRTypes,
     uScanArguments,
     //uController,
     //uSimulator,
     uViewerTypes;

type
  TScanControl = class (TObject)
    private
       scanArguments : TScanArguments;
       StopScan : boolean;

       function    getScanValues (min, max : double; numberOfScans : integer) : TDoubleArray;

       procedure   setTimeStart (timeStart : double);
       function    getTimeStart : double;

       procedure   setTimeEnd (timeEnd : double);
       function    getTimeEnd : double;

       procedure   setNumberOfPoints (numberOfPoints : integer);
       function    getNumberOfPoints : integer;

       procedure   setParameterId (parameterId : string);
       function    getParameterId : string;

       procedure   setLogScan (logScan : boolean);
       function    getLogScan : boolean;

       procedure   setScanType (scanType : TScanType);
       function    getScanType : TScanType;

       procedure   setNumScans (numScans : integer);
       function    getNumScans : integer;

       procedure   setMinScanValue (minValue : double);
       function    getMinScanValue : double;

       procedure   setMaxScanValue (maxValue : double);
       function    getMaxScanValue : double;
     public
       //controller : TController;
       simulator : TObject;

       function  doTimeCourseSingleParameterScan (scanSelectionList : TStringList; scanArguments : TScanArguments) : T2DMatrix;
       function  doSteadyStateSingleParameterScan(selectionList: TStringList) : T2DMatrix;

       property  timeStart : double read getTimeStart write setTimeStart;
       property  timeEnd : double read getTimeEnd write setTimeEnd;
       property  numberOfPoints : integer read getNumberOfPoints write setNumberOfPoints;
       property  logScan : boolean read getLogScan write setLogScan;
       property  scanValuesType : TScanType read getScanType write setScanType;
       property  parameterId : string read getParameterId write setParameterId;
       property  minValue : double read getMinScanValue write setMinScanValue;
       property  maxValue : double read getMaxScanValue write setMaxScanValue;

       property  numOfScans : integer write setNumScans;

       constructor Create (simulator : TObject);
  end;

implementation

Uses FMX.Dialogs, StrUtils, uRoadRunner, uSimulator;

constructor TScanControl.Create (simulator : TObject);
begin
  scanArguments := TScanArguments.Create;
  self.simulator := simulator;
  StopScan := False;
end;


procedure TScanControl.setMinScanValue (minValue : double);
begin
  scanArguments.FMinValue := minValue;
end;

function TScanControl.getMinScanValue : double;
begin
  result := scanArguments.FMinValue;
end;

procedure TScanControl.setMaxScanValue (maxValue : double);
begin
  scanArguments.FMaxValue := maxValue;
end;

function TScanControl.getMaxScanValue : double;
begin
  result := scanArguments.FMaxValue;
end;

procedure  TScanControl.setTimeStart (timeStart : double);
begin
  scanArguments.FTimeStart := timeStart;
end;


function TScanControl.getTimeStart : double;
begin
  result := scanArguments.FTimeStart;
end;


procedure TScanControl.setTimeEnd (timeEnd : double);
begin
  scanArguments.FTimeEnd := timeEnd;
end;


function TScanControl.getTimeEnd : double;
begin
  result := scanArguments.FTimeEnd;
end;

procedure TScanControl.setNumberOfPoints (numberOfPoints : integer);
begin
  scanArguments.FNumberOfPoints := numberOfPoints;
end;

function TScanControl.getNumberOfPoints : integer;
begin
  result := scanArguments.FNumberOfPoints;
end;

procedure  TScanControl.setParameterId (parameterId : string);
begin
  scanArguments.FParameterId := parameterId;
end;

function TScanControl.getParameterId : string;
begin
  result := scanArguments.FParameterId;
end;

procedure TScanControl.setLogScan (logScan : boolean);
begin
  scanArguments.FLogScan := logScan;
end;

function TScanControl.getLogScan : boolean;
begin
  result := scanArguments.FLogScan;
end;

procedure  TScanControl.setScanType (scanType : TScanType);
begin
  scanArguments.FScanValuesType := scanType;
end;

function TScanControl.getScanType : TScanType;
begin
  result := scanArguments.FScanValuesType;
end;

procedure TScanControl.setNumScans (numScans : integer);
begin
  scanArguments.FNumberOfScans := numScans;
end;

function  TScanControl.getNumScans : integer;
begin
  result := scanArguments.FNumberOfScans;
end;



function TScanControl.getScanValues (min, max : double; numberOfScans : integer) : TDoubleArray;
var aMin, aMax : double;
    stepSize : double;
    i : integer;
    current : double;
begin
  if scanArguments.FScanValuesType = stList then
     result := scanArguments.FListValues
  else
     begin
     if scanArguments.FLogScan then
        begin
        aMin := Ln (min);
        aMax := Ln (max);
        end
     else
        begin
        aMin := min; aMax := max;
        end;

     if numberOfScans <= 0 then
        begin
        setLength (result, 0);
        setlength (result, 1);
        result[0] := min;
        exit;
        end;

     stepsize := (aMax - aMin) / (scanArguments.FNumberOfScans-1);
     setLength (result, scanArguments.FNumberOfScans);
     for i := 0 to scanArguments.FNumberOfScans - 1 do
         begin
         current := aMin + stepsize * i;
         if scanArguments.FLogScan then
            current := Exp (current);
         result[i] := current;
         end;
     end;
end;


function TScanControl.doTimeCourseSingleParameterScan (scanSelectionList : TStringList; scanArguments : TScanArguments) : T2DMatrix;
var oldSelectionList : TStringList;
    oldFirst : double;
    i, j, n : integer;
    shortSelectionList : TStringList;
    intermediateResult : T2DMatrix;
    initialConditions : TDoubleArray;
    scanValues : TDoubleArray;
    selectionList : TStringList;
begin
  selectionList := TStringList.Create;
  for i := 0 to scanSelectionList.Count - 1 do
      selectionList.Add (scanSelectionList[i]);
  scanValues := getScanValues(scanArguments.FMinValue, scanArguments.FMaxValue, scanArguments.FNumberOfScans);

  if length (scanValues) = 0 then
     begin
     FMX.Dialogs.MessageDlg ('No values to scan', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
     result.empty := True;
     exit (result);
     end;

  if timeEnd < timeStart then
     begin
     FMX.Dialogs.MessageDlg ('Time end should be greater than time start', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
     result.empty := True;
     exit (result);
     end;

  if numberOfPoints < 2 then
     begin
     FMX.Dialogs.MessageDlg ('The number of simulation points should not be less than 2', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
     result.empty := True;
     exit (result);
     end;

  initialConditions := TSimulator(simulator).roadRunner.getFloatingSpeciesInitialConcentrations;
  shortSelectionList := TStringList.Create;

  try
    oldSelectionList := TSimulator(simulator).roadRunner.getTimeCourseSelectionList;
    oldFirst := TSimulator(simulator).roadrunner.getValue (scanArguments.FParameterId);

    TSimulator(simulator).roadRunner.setTimeCourseSelectionListEx(selectionList);
    TSimulator(simulator).roadRunner.setTimeStart(timeStart);
    TSimulator(simulator).roadRunner.setTimeEnd (timeEnd);
    TSimulator(simulator).roadRunner.setNumberOfPoints (numberOfPoints);
    TSimulator(simulator).roadRunner.setFloatingSpeciesInitialConcentrations (initialConditions);

    TSimulator(simulator).roadrunner.setValue (scanArguments.FParameterId, scanValues[0]);

    result := TSimulator(simulator).roadrunner.Simulate;

    result.columnHeader[0] := 'Time';
    for i := 1 to selectionList.Count - 1 do
        result.columnHeader[i] := selectionList[i] + '(' + scanArguments.FParameterId + '=' + floattostr (scanValues[0]) + ')';

      for i := 1 to selectionList.Count - 1 do  // miss out first column
          shortSelectionList.Add (selectionList[i]);
    TSimulator(simulator).roadRunner.setTimeCourseSelectionListEx(shortSelectionList);

    n := length (scanValues);
    for i := 1 to n - 1 do
        begin
        try
          if StopScan then
             exit;

          TSimulator(simulator).roadrunner.reset();
          TSimulator(simulator).roadrunner.setValue (scanArguments.FParameterId, scanValues[i]);
          intermediateResult := TSimulator(simulator).roadRunner.SimulateEx(scanArguments.FTimeStart, scanArguments.FTimeEnd, scanArguments. FNumberOfPoints);
          for j := 0 to shortSelectionList.Count - 1 do
              intermediateResult.columnHeader[j] := selectionList[j+1] + '(' + scanArguments.FParameterId + '=' + floattostr (scanValues[i]) + ')';

          result.AugmentColumns(intermediateResult);
          //result := TMatrix.Augment (result, intermediateResult);
          intermediateResult.Free;
        finally
        end;
        end;

    TSimulator(simulator).roadrunner.setValue (scanArguments.FParameterId, oldFirst);

    TSimulator(simulator).roadRunner.setTimeCourseSelectionListEx(oldSelectionList);
    scanSelectionList.Clear;
    for i := 0 to result.c - 1 do
        scanSelectionList.Add (result.columnHeader[i]);

    TSimulator(simulator).roadRunner.setFloatingSpeciesInitialConcentrations (initialConditions);

  finally
    shortSelectionList.free;
    setLength (initialConditions, 0);
    oldSelectionList.free;
  end;
end;


function TScanControl.doSteadyStateSingleParameterScan(selectionList: TStringList) : T2DMatrix;
var
  oldSelectionList: TStringList;
  oldFirst: double;
  i, j, n: integer;
  ssvalues: TDoubleArray;
  simValues: T2DMatrix;
  d: double;
  str: AnsiString;
  scanValues : TDoubleArray;
  oldValue : double;
begin
  scanValues := getScanValues(scanArguments.FMinValue, scanArguments.FMaxValue, scanArguments.FNumberOfScans);

  oldSelectionList := TSimulator(simulator).roadrunner.getSteadyStateSelectionList;
  try
    oldFirst := TSimulator(simulator).roadrunner.getValue(scanArguments.FParameterId);
    TSimulator(simulator).roadrunner.setSteadyStateSelectionListEx (selectionList);

    oldValue := TSimulator(simulator).roadrunner.getValue(scanArguments.FParameterId);
    n := length(scanValues);
    result := T2DMatrix.Create(n, selectionList.Count); // +1 for scan parameter column

    // Get names of y variables into the matrix header
    for i := 0 to selectionList.Count - 1 do
       result.columnHeader.Add(selectionList[i]);

    for i := 0 to n - 1 do
         begin
          if StopScan then
           exit;

          TSimulator(simulator).roadrunner.setValue(scanArguments.FParameterId, scanValues[i]);
          simValues := TSimulator(simulator).roadrunner.simulate;
          TSimulator(simulator).roadrunner.steadyState;
          ssvalues := TSimulator(simulator).roadrunner.computeSteadyStateValues;
          for j := 0 to selectionList.Count - 1 do
            result[i, j] := ssvalues[j];
          setLength(ssvalues, 0);
          end;

      TSimulator(simulator).roadrunner.setValue(scanArguments.FParameterId, oldFirst);
      TSimulator(simulator).roadrunner.setSteadyStateSelectionListEx(oldSelectionList);
  finally
    oldSelectionList.Free;
    TSimulator(simulator).roadrunner.setValue(scanArguments.FParameterId, oldValue);

  end;
end;



end.
