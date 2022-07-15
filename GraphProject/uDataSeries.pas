unit uDataSeries;

interface

Uses SysUtils, Generics.Collections, uCommon, uSymbolDetails, uLineDetails;

type
  // Data block has one x column and one or more y columns
  TDataBlock = class (TObject)
       name : string;
       nRows, nYColumns : integer;
       XData : T1DArray;
       YData : T2DArray;
       YSymbols : array of TSymbolDetails;
       YLines : array of TLineDetails;
       XErrors : T1DArray;
       YErrors : T2DArray;

       constructor Create (nRows, numYColumns : integer); overload;
       constructor Create (name : string; nRows, numYColumns : integer); overload;
       destructor Destroy;
  end;

  TDataSeries = TObjectList<TDataBlock>;

implementation

Uses math;

constructor TDataBlock.Create (name : string; nRows, numYColumns : integer);
begin
  Create (nRows, numYColumns);
  self.name := name;
end;


constructor TDataBlock.Create (nRows, numYColumns : integer);
var i : integer;
begin
  name := 'db_' + inttostr (math.RandomRange (0, 1000000));
  XData := createVector (nRows);
  YData := createMatrix (nRows, numYColumns);

  setlength (YSymbols, numYColumns);
  for i := 0 to numYColumns - 1 do
      YSymbols[i] := TSymbolDetails.Create;

  setlength (YLines, numYColumns);
  for i := 0 to numYColumns - 1 do
      YLines[i] := TLineDetails.Create;

  self.nRows := nRows;
  self.nYColumns := numYColumns;
end;


destructor TDataBlock.Destroy;
begin
  setLength (XData, 0);
  freeMatrix (YData);
end;


end.
