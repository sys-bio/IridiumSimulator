unit uRRTypes;

interface

Uses Classes, SysUtils;

type
  TDoubleArray = array of double;

  T2DArray = array[0..100000] of double;
  P2Darray = ^T2Darray;

  // A simple matrix type that captures the roadrunner RRMatrix data.
  // indexing is from zero
  T2DMatrix = record
     empty : boolean;
     r, c : integer;
     data : P2DArray;
     columnHeader : TStringList;
     procedure Free;
     function  getValue (i, j : integer) : double;
     procedure setValue (i, j : integer; value : double);
     procedure  AugmentColumns (m : T2DMatrix);
     property  Value[i, j : integer] : double read getValue write setValue; default;
     constructor Create (nr, nc : integer);
  end;

  // Result type returned by simulate() and simulateEx()
  THeaderlist = array[0..10000] of PAnsiChar;
  PHeaderList = ^THeaderList;
  TRRCData = record
     RSize : integer;
     CSize : integer;
     Data : PDouble; //array of double;
     Weights : pointer;
     ColumnHeaders : PHeaderList;
  end;
  PRRCData = ^TRRCData;


  // Vector of double values
  TRRDoubleVector = record
       count : integer;
       data : array of double;
  end;
  PRRDoubleVectorHandle = ^TRRDoubleVector;


  // This let us index the block of memory containing doubles
  TDoubleStaticArray = array[0..10000] of double;
  PDoubleArray = ^TDoubleStaticArray;

  // Simple Matrix Type returned by roadrunner
  TRRMatrix = record
    RSize : integer;
    CSize : integer;
    data : PDoubleArray;
  end;
  PRRMatrixHandle = ^TRRMatrix;


  TRRCCodeAPI = record
    Header : PAnsiChar;
    Source : PAnsiChar;
  end;
  TRRCCode = record
    Header : AnsiString;
    Source : AnsiString;
  end;
  PRRCCodeHandle = ^TRRCCodeAPI;

  TListItemType = (litString, litInteger, litDouble, litList);

  PRRListItemRecord = ^TRRListItemRecord;
  PRRListRecordHandle = ^TRRListRecord;

  TRRListItemRecordArray = array[0..1000] of PRRListRecordHandle;
  PRRListItemRecordArray = ^TRRListItemRecordArray;

  TRRListItemRecord = record
      ItemType : TListItemType;
      case TListItemType of
         litInteger: (iValue : integer);
         litDouble:  (dValue : double);
         litString:  (sValue : PAnsiChar);
         litList:    (lValue : PRRListRecordHandle);
  end;

  TRRListRecord = record
      count : integer;
      Items : PRRListItemRecordArray;
  end;
  TAnsiCharArray = array[0..100000] of AnsiChar;
  PAnsiCharArray = ^TAnsiCharArray;
  TAnsiCharArrayArray = array[0..100000] of PAnsiCharArray;  // Array of char*
  PAnsiCharArrayArray = ^TAnsiCharArrayArray;
  TRRStringArray = record
    count : integer;
    strList : PAnsiCharArrayArray;
  end;
  PRRStringArray = ^TRRStringArray;


function getElement (data : T2DMatrix; i, j : integer) : double;

implementation



function getElement (data : T2DMatrix; i, j : integer) : double;
begin
  result := data.data[i*data.c + j];
end;


{ T2DMatrix }

function T2DMatrix.getValue(i, j: integer): double;
begin
  result := data[i*c + j];
end;


procedure T2DMatrix.setValue(i, j: integer; value: double);
begin
  data[i*c + j] := value;
end;


constructor T2DMatrix.Create (nr, nc : integer);
begin
  empty := False;
  data := getMemory (nr*nc*sizeof (double));
  self.r := nr;
  self.c := nc;
  columnHeader := TStringList.Create;
end;


procedure T2DMatrix.AugmentColumns (m : T2DMatrix);
var i, j, self_c : integer;
    new : T2DMatrix;
begin
  if self.r <> m.r then
     raise Exception.Create ('Matrix arguments must have the same row size in the augmentColumns function');
  self_c := c;
  new := T2DMatrix.Create(self.r, self.c + m.c);
  for i := 0 to self.r - 1 do
      for j := 0 to self.c - 1 do
          new[i,j] := self[i,j];

  for i := 0 to self.r - 1 do
      begin
      for j := 0 to m.c - 1 do
          new[i, self_c + j] := m[i,j];
      end;

  for i := 0 to self.columnHeader.Count - 1  do
      new.columnHeader.Add(self.columnHeader[i]);

  for i := 0 to m.c - 1 do
      new.columnHeader.Add(m.columnHeader[i]);
  self.r := new.r;
  self.c := new.c;
  FreeMemory (self.data);
  self.columnHeader.Free;
  self.data := new.data;
  self.columnHeader := new.columnHeader;
end;

procedure T2DMatrix.free;
begin
  FreeMemory (data);
  columnHeader.Free;
end;

end.
