unit uRRDataSeries;

interface

Uses SysUtils, Generics.Collections, uRRCommon, uSymbolDetails, uLineDetails, Json;

// Top Level -> TData

// TData -> One or more data blocks

// TDatablock -> two or more data columns
// A data block can have a designated 'x' column

// TDataColumn -> values

type
  TDataPoints = array of double;

  TErrorType = (etNone, etSymmetric, etASymmetric);
  TErrorDatum = record
      errorType : TErrorType;
      value, upper, lower : double;
  end;

  T1DErrorData = array of TErrorDatum;
  T2DErrorData = array of T1DErrorData;

  TDataColumn = class (TObject)
     name : string;
     //visible : boolean;
     lineDetails : TLineDetails;
     symbol : TSymbol;
     data : TDataPoints;
     XErrors : T1DErrorData;
     YErrors : T1DErrorData;
     function    getNumRows : integer;
     constructor Create (name : string; nRows : integer);
     procedure   setDataValue (rowi : integer; value : double);
     function    saveToJson : TJSONObject;
     constructor readFromJson (obj : TJSONObject);
     destructor  Destroy; override;
  end;

  // Stores all the columns of data by name
  TDataColumns = class (TObjectList<TDataColumn>)
      function find (name : string; var index : integer) : TDataColumn;
      //destructor Destroy; override;
  end;

  TDataBlock = class (TObject)
      name : string;
      xaxisColumn : string;
      columns : TDataColumns;

      procedure   Clear;
      function    find (name : string) : integer;
      function    getColumn (name : string) : TDataColumn;
      function    createDataColumn (name : string; npoints : integer)  : integer;
      constructor Create; overload;
      constructor Create (name : string; nRows, numYColumns : integer); overload;
      destructor  Destroy; override;
  end;

  TDataBlocks = class (TObjectList<TDataBlock>)
      function  find (name : string) : integer;
      procedure Clear;
      constructor Create;
      destructor Destroy; override;
  end;

implementation

Uses math, rtti;

constructor TDataColumn.Create (name : string; nRows : integer);
begin
  inherited Create;
  self.name := name;
  setLength (data, nRows);
  SetLength (XErrors, nRows);
  setLength (YErrors, nRows);
  symbol := TSymbol.Create;
  lineDetails :=  TLineDetails.Create;
  //visible := True;
end;


destructor TDataColumn.Destroy;
begin
  setLength (data, 0);
  setLength (XErrors, 0);
  setLength (YErrors, 0);
  symbol.Free;
  lineDetails.Free;
  inherited;
end;

function TDataColumn.getNumRows : integer;
begin
  result := length (data);
end;

procedure TDataColumn.setDataValue (rowi : integer; value : double);
begin
  data[rowi] := value;
end;


function TDataColumn.saveToJson : TJSONObject;
var obj : TJSONObject;
    dataArray : TJSONArray;
    i : integer;
begin
  //obj := TJSONObject.Create;
  //obj.AddPair ('name', name);
  //obj.AddPair ('symbol', symbol.saveToJson());
  //obj.AddPair ('line', lineDetails.saveToJson());

  //dataArray := TJSONArray.Create;
  //for i := 0 to length (data) - 1 do
  //    dataArray.Add(data[i]);
  //obj.AddPair('values', dataArray);

  //result := obj;
end;


constructor TDataColumn.readFromJson (obj : TJSONObject);
var symObj : TJSONObject;
    lineObj : TJSONObject;
    ar : TJSONArray;
    i : integer;
begin
//  name := obj.GetValue<string>('name');
//  symObj := obj.Get('symbol').JsonValue as TJSONObject;
//  symbol := TSymbol.readFromJson(symObj);
//
//  lineObj := obj.Get('line').JsonValue as TJSONObject;
//  lineDetails := TLineDetails.readFromJson (lineObj);
//
//  ar := Obj.GetValue ('values') as TJSONArray;
//  setlength (data, ar.Count);
//  for i := 0 to ar.Count - 1 do
//      data[i] := ar[i].GetValue<double>;
//
//  SetLength (XErrors, ar.Count);
//  setLength (YErrors, ar.Count);
end;


// ---------------------------------------------------------------------------------

constructor TDataBlock.Create;
begin
  inherited;
  columns := TDataColumns.Create;
end;


constructor TDataBlock.Create (name : string; nRows, numYColumns : integer);
var i : integer;
begin
  Create;
  self.name := name;
  for i := 0 to numYColumns - 1 do
      begin
      name := 'Data' + inttostr (i);
      columns.Add(TDataColumn.Create (name, nRows));
      end;
  xaxisColumn := 'Data0';
end;


destructor TDataBlock.Destroy;
begin
  columns.Clear;
  inherited;
end;


function TDataBlock.createDataColumn (name : string; npoints : integer) : integer;
var dataColumn : TDataColumn;
begin
  dataColumn := TDataColumn.Create (name, npoints);
  result := columns.Add(dataColumn);
end;


function TDataBlock.getColumn (name : string) : TDataColumn;
var index : integer;
begin
  result := columns.find(name, index);
  if result = nil then
     raise Exception.Create('Specified column: ' + name + ' does not exist');
end;

procedure TDataBlock.Clear;
begin
  columns.Free;
end;


function TDataBlock.find (name : string) : integer;
var i : integer;
begin
  result := -1;
  for i := 0 to self.Columns.Count - 1 do
      if self.columns[i].name = name then
         begin
         exit (i);
         end;
end;

// ---------------------------------------------------------------

constructor TDataBlocks.Create;
var db : TDataBlock;
begin
  inherited;
  db := TDataBlock.Create;
  //db := TDataBlock.Create(10, 2);
  self.Add(db);
end;


procedure TDataBlocks.Clear;
var i : integer;
begin
  for i := 0 to Count - 1 do
      items[i].columns.Clear;
end;


destructor TDataBlocks.Destroy;
begin
  inherited;
end;


function TDataBlocks.find (name : string) : integer;
var i : integer;
begin
  result := -1;
  for i := 0 to self.Count - 1 do
      if self[i].name = name then
         begin
         exit (i);
         end;
end;


function TDataColumns.find (name : string; var index: integer) : TDataColumn;
var i : integer;
begin
  result := nil;
  for i := 0 to self.Count - 1 do
      if self[i].name = name then
         begin
         index := i;
         exit (items[i]);
         end;
end;

//constructor TDataColumn.Create (name : string; nRows, numYColumns : integer);
//begin
//  Create (nRows, numYColumns);
//  self.name := name;
//  visible := true;
//end;
//
//
//constructor TDataColumn.Create (nRows, numYColumns : integer);
//var i : integer;
//begin
//  name := 'db_' + inttostr (math.RandomRange (0, 1000000));
//  XData := createVector (nRows);
//  YData := TYColumns.Create (nRows, numYColumns);
//  SetLength (XErrors, nRows);
//  SetLength (YErrors, nRows, numYColumns);
//
//  setlength (YSymbols, numYColumns);
//  for i := 0 to numYColumns - 1 do
//      YSymbols[i] := TSymbol.Create;
//
//  setlength (YLines, numYColumns);
//  for i := 0 to numYColumns - 1 do
//      YLines[i] := TLineDetails.Create;
//
//  // Give default names to the Y columns
//  for i := 0 to numYColumns - 1 do
//      YData.names[i] := 'y_' + inttostr (math.RandomRange (0, 1000000));
//
//  self.nRows := nRows;
//  self.nYColumns := numYColumns;
//  visible := true;
//end;



end.

