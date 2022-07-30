unit uScanArguments;

interface

Uses uRRTypes;

type
  TScanRunType = (srSimulation, srSteadyState);
  TScanType = (stRange, stList);

  // This is populated by the GUI
  TScanArguments = class (TObject)
    public
      FScanRun : TScanRunType;
      FScanValuesType : TScanType;

      FTimeStart, FTimeEnd : double;
      FNumberOfPoints : integer;

      FMinValue : double;
      FMaxValue : double;

      FParameterId : string;
      FNumberOfScans : integer;

      FLogScan : boolean;
      FListValues : TDoubleArray;

      property  minValue : double read FMinValue write FMinValue;
      property  maxValue : double read FMaxValue write FMaxValue;

      procedure convertListToDoubles (listStrValues : string);

      property  listOfScanValues : string write convertListToDoubles;
      constructor Create;
  end;


implementation

Uses SysUtils, StrUtils;

constructor TScanArguments.Create;
begin
  FScanRun := srSimulation;
  FTimeStart := 0.0; FTimeEnd := 10.0;
  FNumberOfPoints := 100;
  FLogScan := False;

  FScanValuesType := stRange;
  minValue := 0.1;
  maxValue := 1;
  FNumberOfScans := 3;
end;


procedure  TScanArguments.convertListToDoubles (listStrValues : string);
var valueStr : TArray<string>;
    i : integer;
begin
  listStrValues := Trim (listStrValues);
  valueStr := splitString (listStrValues, ' ');

  setLength (FListValues, length (valueStr));
  for i := 0 to length (valueStr) - 1 do
      FListValues[i] := strtofloat (valueStr[i]);
end;

end.
