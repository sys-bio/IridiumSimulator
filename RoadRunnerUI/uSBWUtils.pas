unit uSBWUtils;

// Utility routines

// DataStream Manipulation routines to add and extract data items from
// a datastream.


interface

Uses
   SysUtils, Classes, Contnrs, Variants, FMX.Dialogs, FMX.StdCtrls,
   uSBWCommon, uSBWList, uSBWArray, uSBWComplex, uSBWScanner;


type
  TSBWReturnType = SBWDataBlockWriter;

  // Class type for building and decontructing datastream. A
  // DataStream is a sequence of DataItems
  TSBWDataStream = class (TObject)
      private
       FData : TData;
       MaxLength : integer;
       Delta : integer;
       procedure CheckMaxSize (nl : integer);

       // Copy values into a data stream
       procedure CopyByte (value : byte);
       procedure CopyBoolean (value: boolean);
       procedure CopyInteger (value: integer);
       procedure CopyDouble (value : double);
       procedure CopyComplex (value : TSBWComplex);
       procedure CopyString (value : AnsiString);

       // Copy a value out of a datastream
       function  CopyStrOut : AnsiString;
       function  CopyDoubleOut : double;
       function  CopyIntOut : integer;
       function  CopyByteOut : byte;
       function  CopyComplexOut : TSBWComplex;
     public
       fromId : integer;
       writerObject : Cardinal;
       nItems : integer;
       Thread : TThread;
       procedure Clear; overload;
       procedure Clear (initialSize : integer); overload;
       procedure add (b : byte); overload;
       procedure add (value : boolean); overload;
       procedure add (value : integer); overload;
       procedure add (value : double); overload;
       procedure add (value : AnsiString); overload;
       procedure add (value : TSBWComplex); overload;
       procedure add (ar : TSBWArray); overload;
       procedure add (value : TSBWList); overload;
       procedure add1DArray (value : array of byte); overload;
       procedure add1DArray (value : array of Boolean); overload;
       procedure add1DArray (value : array of integer); overload;
       procedure add1DArray (value : array of double); overload;
       procedure add1DArray (value : array of TSBWComplex); overload;
       procedure add1DArray (value : array of AnsiString); overload;

       procedure add2DArray (value : TSBW2DDoubleArray); overload;
       procedure add2DArray (value : TSBW2DIntArray); overload;

       function  getByte() : Byte;
       function  getInteger() : integer;
       function  getDouble() : double;
       function  getScalar() : double;
       function  getString() : AnsiString;
       function  getBoolean() : boolean;
       function  getList() : TSBWList;
       function  getArray() : TSBWArray;
       function  getComplex() : TSBWComplex;

       function  peek() : integer;

       function  isByte() : boolean;
       function  isInteger() : boolean;
       function  isDouble() : boolean;
       function  isString() : boolean;
       function  isBoolean() : boolean;
       function  isArray() : boolean;
       function  isList() : boolean;
       function  isComplex() : boolean;

       function  pack (const value : integer) :  SBWDataBlockWriter; overload;
       function  pack (const value : double) :  SBWDataBlockWriter; overload;
       function  pack (const value : boolean) :  SBWDataBlockWriter; overload;
       function  pack (const value : AnsiString) :  SBWDataBlockWriter; overload;
       function  pack (const value : TSBWComplex) :  SBWDataBlockWriter; overload;
       function  pack (const Args : array of const) :  SBWDataBlockWriter; overload;
       function  pack (const vec : TSBW1DDoubleArray) : SBWDataBlockWriter; overload;
       function  pack (const mat : TSBW2DDoubleArray) : SBWDataBlockWriter; overload;
       function  pack (const mat : TSBW1DIntArray) : SBWDataBlockWriter; overload;
       function  pack (const mat : TSBW2DIntArray) : SBWDataBlockWriter; overload;
       function  pack (const str : TSBW1DStrArray) : SBWDataBlockWriter;    overload;
       function  pack (const value : TSBWList) :  SBWDataBlockWriter; overload;
       function  pack (const value : TSBW1DListArray) :  SBWDataBlockWriter; overload;

       function  getLength() : integer;
       function  getMessage() : PByteArray;
       procedure Reset();

       procedure BuildArgumentList (const Args : array of const);
       procedure BuildFromDataItemList (SBWList : TSBWList);

       constructor Create (from : integer; writerObject : SBWDataBlockWriter; Args : SBWDataBlockReader); overload;
       constructor Create (initialSize : integer); overload;
       constructor Create (const Args : array of const); overload;
       constructor setMsg (msg : PAnsiChar; len : integer);
       destructor Destroy; override;
       property Data : TData read FData;
       property  len : integer read getLength;
       property  ptr : PByteArray read getMessage;
  end;


  TServiceList = TObjectList;

  TMethodList = TObjectList;
  TServiceItem = class (TObject)
             serviceName : AnsiString;
             serviceDisplayName : AnsiString;
             category : AnsiString;
             helpStr : AnsiString;
             methodList : TMethodList;
             constructor Create (serviceName, serviceDisplayName, category, helpStr : AnsiString);
             destructor Destroy; override;
  end;

  TMethodItem = class (TObject)
          signature : AnsiString;
          helpStr : AnsiString;
          methodName : AnsiString;
          returnType : AnsiString;
          argList : TStringList;
          constructor Create (signature, helpStr : AnsiString);
          destructor Destroy; override;
  end;

function getDataType (var pData : PDataStream) : byte;

// Data Extraction Routines
function ExtractBoolean (var pData : PDataStream) : boolean;
function ExtractInteger (var pData : PDataStream) : integer;
function ExtractDouble (var pData : PDataStream) : double;
function ExtractString (var pData : PDataStream) : AnsiString;
function ExtractComplex (var pData : PDataStream) : TSBWComplex;
function ExtractListData (var pData : PDataStream; n : integer) : TSBWList; // doesn't check for list type byte
function ExtractList (var pData : PDataStream) : TSBWList;
function ExtractArray (var pData : PDataStream) : TSBWArray;


// Data Adding Routines
function addBoolean (var Data : TData; value : boolean) : TData;
function addInteger (var Data : TData; value : integer) : TData;
function addDouble (var Data : TData; value : double) : TData;
function addString (var Data : TData; value : AnsiString) : TData;
function addList (var Data : TData; const value : variant) : TData; overload;
function addList (var Data : TData; const Value : TSBWList) : TData; overload;
function addTerminator (var Data : TData) : TData;

function AddArray (var Data : TData; nDim : integer; dims : array of integer) : TData;
function AddDoubleToArray (var Data : TData; value : double) : TData;
function AddStringToArray (var Data : TData; name : AnsiString) : TData;

function MergeData (D1, D2 : TData) : TData;

function DataTypeToString (DataType : byte) : AnsiString;
function BoolToStr (value : boolean) : AnsiString;


function convertSigByte (sigByte : byte) : AnsiString;
function getPascalTypeString (sigByte : byte) : AnsiString;
function booleanToStr (b : boolean) : AnsiString;
function complexToStr (c : TSBWComplex) : AnsiString;
function strToComplex (cstr : AnsiString) : TSBWComplex;

implementation

uses uSBWD {$IFDEF VER140}, Variants {$ENDIF};


function convertSigByte (sigByte : byte) : AnsiString;
begin
  case sigByte of
     dtByte    : result := 'byte';
     dtInteger : result := 'int';
     dtBoolean : result := 'boolean';
     dtDouble  : result := 'double';
     dtString  : result := 'string';
     dtArray   : result := 'array[]';
     dtList    : result := 'list{}';
     dtComplex : result := 'complex';
     dtDMatrix : result := dmatrixEncodingString;
     dtCMatrix : result := cmatrixEncodingString;
     dtVoid    : result := 'void';
     dtDArray  : result := 'double[]';
     dtIArray  : result := 'Int[]';
     dtDDArray  : result := 'double[][]';
     dtIIArray  : result := 'Int[][]';
  else
     result := 'byte';
  end;
end;


function getPascalTypeString (sigByte : byte) : AnsiString;
begin
  case sigByte of
     dtByte    : result := 'byte';
     dtInteger : result := 'integer';
     dtBoolean : result := 'boolean';
     dtDouble  : result := 'double';
     dtString  : result := 'string';
     dtComplex : result := 'TSBWComplex';
     dtDMatrix : result := dmatrixEncodingString;
     dtCMatrix : result := cmatrixEncodingString;
     dtVoid    : result := 'void';
     dtDArray  : result := 'double[]';
     dtIArray  : result := 'Int[]';
     dtDDArray : result := 'double[][]';
     dtIIArray : result := 'Int[][]';
  else
     result := 'byte';
  end;
end;


function booleanToStr (b : boolean) : AnsiString;
begin
  if b then
     result := 'true'
  else result := 'false';
end;


function complexToStr (c : TSBWComplex) : AnsiString;
begin
  result := floatToStr (c.r);
  if c.i >= 0 then
     result := result + '+';
  result := result + floatToStr (c.i) + 'i';
end;


function getComplex (sc : TSBWScanner) : TSBWComplex;
var foundBracket : boolean;
begin
   result := complex (0, 0);
   foundBracket := False;
   if sc.Token = tLParen then
      begin
      sc.nextToken;
      foundBracket := True;
      end;

  case sc.Token of
     tInteger : result.r := sc.TokenInteger;
     tFloat   : result.r := sc.TokenFloat;
     tComplexToken : result.i := sc.TokenFloat;
  else
     raise Exception.Create ('Expecting scalar real part in complex number');
  end;

  sc.NextToken;
  if sc.Token = tComma then
     begin
     sc.nextToken;
     case sc.Token of
       tInteger : result.i := sc.TokenInteger;
       tFloat   : result.i := sc.TokenFloat;
  tComplexToken : result.i := sc.TokenFloat;
     else
       raise Exception.Create ('Expecting scalar immaginary part in complex number');
     end;
     sc.nextToken;
     if foundBracket and (sc.Token <> tRParen) then
        raise Exception.Create ('Expecting closing parentheses in complex value');
     if (sc.Token = tRParen) and (not foundBracket) then
        raise Exception.Create ('Found closing parentheses in complex value withut openening parenthesis');
     end;
end;


function strToComplex (cstr : AnsiString) : TSBWComplex;
var sc : TSBWScanner;
begin
  sc := TSBWScanner.create;
  try
    sc.SetStream (TStringStream.Create(cstr));
    sc.NextToken;
    result := getComplex (sc)
  finally
    sc.free;
  end;
end;


// -----------------------------------------------------------------------


constructor TServiceItem.Create (serviceName, serviceDisplayName, category, helpStr : AnsiString);
begin
  inherited Create;
  Self.serviceName := serviceName;
  Self.serviceDisplayName := serviceDisplayName;
  Self.category := category;
  Self.helpStr := helpStr;
  methodList := TMethodList.Create;
end;


destructor TServiceItem.Destroy;
begin
  methodList.Free;
  inherited Destroy;
end;


// -----------------------------------------------------------------------

constructor TMethodItem.Create (signature, helpStr : AnsiString);
begin
  inherited Create;
  Self.signature := signature;
  Self.helpStr := helpStr;
  argList := TStringList.Create;
end;

destructor TMethodItem.Destroy;
begin
  argList.Free;
  inherited Destroy;
end;


// -----------------------------------------------------------------------


constructor TSBWDataStream.Create (initialSize : integer);
begin
  inherited Create;
  MaxLength := initialSize; Delta := 64;
  if initialSize = 0 then
     FData.d := nil
  else FData.d := AllocMem (initialSize);
  FData.Len := 0;
  FData.ptr := 0;
end;


// Only to be used during receipt of data from SBW !!!
constructor TSBWDataStream.Create (from : integer; writerObject : SBWDataBlockWriter; Args : SBWDataBlockReader);
var len : integer; p : PAnsiChar;
begin
  self.fromId := from;
  self.writerObject := writerObject;
  p := SBWReadRaw (Args, len);
  try
    FData.d := AllocMem (len);
    Move (p^, FData.d^, len);
    FData.len := 0;
    FData.ptr := 0;
  finally
    SBWFreeArray (p);  // p is owned by the DLL !!!!
  end;
end;


constructor TSBWDataStream.Create (const Args : array of const);
begin
  Create (72);
  BuildArgumentList (Args);
end;


constructor TSBWDataStream.setMsg (msg : PAnsiChar; len : integer);
begin
  Create (0);
  FreeMem (FData.d);
  FData.d := AllocMem (len);
  Move (msg^, FData.d^, len);
  FData.len := len;
  FData.ptr := 0;
end;


destructor TSBWDataStream.Destroy;
begin
  FreeMem (FData.d);
  inherited Destroy;
end;


procedure TSBWDataStream.CheckMaxSize (nl : integer);
begin
  if nl > MaxLength then
     begin
     MaxLength := nl + Delta;
     ReAllocMem (FData.d, MaxLength);
     end;
end;


// Copy routines: These routines DO NOT check for sufficient space
// ---------------------------------------------------------------

procedure TSBWDataStream.CopyByte (value : Byte);
begin
  FData.d[Data.Len] := value;
  inc (FData.Len);
end;


procedure TSBWDataStream.CopyComplex (value : TSBWComplex);
begin
  Move (value, FData.d[FData.len], Sizeof (TSBWComplex));
  inc (FData.Len, SizeOfComplex);
end;


// Copies a raw integer to the data stream, increments
// the data stream length


procedure TSBWDataStream.CopyBoolean (value: boolean);
begin
  Move (value, FData.d[FData.len], Sizeof (Boolean));
  inc (FData.Len, SizeOfBoolean);
end;


procedure TSBWDataStream.CopyInteger (value: integer);
begin
  Move (value, FData.d[FData.len], Sizeof (Integer));
  inc (FData.Len, SizeOfInteger);
end;


procedure TSBWDataStream.CopyDouble (value : double);
begin
  Move (value, FData.d[FData.len], SizeOfDouble);
  inc (FData.Len, SizeOfDouble);
end;


procedure TSBWDataStream.CopyString (value : AnsiString);
var n, i : integer;
    ansivalue : AnsiString;
begin
  ansiValue := value;
  n := length (ansiValue);
  if n > 0 then
     begin
     for i := 0 to n - 1 do
         FData.d[FData.Len + i] := byte (ansiValue[i+1]);
     //Move (value[1], FData.d[FData.Len], length (value));
     inc (FData.Len, n);
     end;
end;



// Copy Out routines
// ---------------------------------------------------------------


// Copyies a string from the data stream
function TSBWDataStream.CopyStrOut : AnsiString;
var len : integer;
begin
  result := '';
  // Get length of string
  len := CopyIntOut();
  if len > 0 then
     begin
     SetLength (Result, len); // Don't forget space for the null character
     Move (FData.d[FData.ptr], result[1], len);
     FData.ptr := FData.ptr + len;
     end;
end;


// Copies a double from the data stream
function TSBWDataStream.CopyDoubleOut : double;
begin
  Move (FData.d[FData.ptr], result, SizeOfDouble);
  inc (FData.ptr, SizeOfDouble);
end;


// Copies a integer from the data stream
function TSBWDataStream.CopyIntOut : integer;
begin
  Move (FData.d[FData.ptr], result, SizeOfInteger);
  inc (FData.ptr, SizeOfInteger);
end;


// Copies a byte from the data stream
function TSBWDataStream.CopyByteOut : byte;
begin
    Move (FData.d[FData.ptr], result, 1);  // get the array data type
    inc (FData.ptr);
end;


function TSBWDataStream.CopyComplexOut : TSBWComplex;
begin
   Move (FData.d[FData.ptr], result, SizeOfComplex);  // get the array data type
   inc (FData.ptr, SizeOfComplex);
end;


// -----------------------------------------------------------------

procedure TSBWDataStream.Clear;
begin
  FreeMem (FData.d);
  FData.d := nil;
  FData.Len := 0;
end;

procedure TSBWDataStream.Clear (initialSize : integer);
begin
  FreeMem (FData.d);
  FData.d := AllocMem (initialSize);
  FData.Len := 0;
end;


function TSBWDataStream.getLength() : integer;
begin
  result := FData.len;
end;


// Returns a pointer to the data stream itself
function TSBWDataStream.getMessage() : PByteArray;
begin
  result := FData.d;
end;


procedure TSBWDataStream.Reset();
begin
  FData.ptr := 0;
end;


// add routines: These routines add the various data types to the data stream
// --------------------------------------===========-------------------------


procedure TSBWDataStream.add (b : byte);
begin
  CheckMaxSize (FData.Len + 1);
  CopyByte (b);
  inc (nItems);
end;


procedure TSBWDataStream.add (value : TSBWComplex);
var len : integer;
begin
  len := sizeof (TSBWComplex) + 1;
  CheckMaxSize (FData.Len + len);
  CopyByte (dtComplex);
  CopyComplex (value);
  inc (nItems);
end;


procedure TSBWDataStream.add (value : boolean);
var len : integer;
begin
  len := SizeOfBoolean + 1;
  CheckMaxSize (FData.Len + len);
  FData.d[Data.Len] := dtBoolean;
  inc (FData.Len);
  Move (value, FData.d[Data.len], SizeofBoolean);
  inc (FData.Len, SizeOfBoolean);
  inc (nItems);
end;


procedure TSBWDataStream.add (value : integer);
var len : integer;
begin
  len := SizeOfInteger + 1;
  CheckMaxSize (FData.Len + Len);
  CopyByte (dtInteger);
  CopyInteger (value);
  inc (nItems);
end;


procedure TSBWDataStream.add (value : Double);
var len : integer;
begin
  len := Sizeof (double) + 1;
  CheckMaxSize (FData.Len + len);
  CopyByte (dtDouble);
  CopyDouble (value);
  inc (nItems);
end;


procedure TSBWDataStream.add (value : AnsiString);
var len, lstr : integer;
begin
  lstr := Length (value);
  // dtString, Length, Characters
  len := lstr + SizeOfInteger + 1;
  CheckMaxSize (FData.Len + Len);
  CopyByte (dtString);
  CopyInteger (lstr);
  CopyString (value);
  inc (nItems);
end;


// Give the list value, construct a data stream from it
procedure TSBWDataStream.add (value : TSBWList);
var i, n : integer;
begin
  n := value.Count;
  CheckMaxSize (FData.Len + 5);
  CopyByte (dtList);
  CopyInteger (n);

  for i := 0 to n - 1 do
      begin
        case value[i].DataType of
          dtByte :
            Add (value[i].b);
          dtBoolean :
            Add (value[i].bool);
          dtInteger:
            Add (value[i].i);
          dtDouble :
            Add (value[i].d);
          dtComplex :
            Add (value[i].c);
          dtString:
            Add (value[i].str);
          dtList :
            Add (value[i].Lt);
          dtArray : begin
                    case value[i].ar.dType of
                      dtInteger :
                          begin
                          case value[i].Ar.nDimensions of
                              1 : add1DArray (value[i].Ar.Int1);
                              2 : add2DArray (value[i].Ar.Int2);
                          end;
                          end;
                      dtDouble :
                          begin
                          case value[i].Ar.nDimensions of
                              1 : add1DArray (value[i].ar.Double1);
                              2 : add2DArray(value[i].Ar.Double2);
                          end;
                          end;
                    end;
                    end;
         else
           raise ESBWException.Create (0, 'Unrecognised item type while constructing data strsam from a List');
         end;
      end;
  inc (nItems);
end;


// Specific add array implementations

procedure TSBWDataStream.add (ar : TSBWArray);
begin
  case ar.nDimensions of
     1 : case ar.dType of
            dtByte    : add1DArray (ar.Byte1);
            dtBoolean : add1DArray (ar.Bool1);
            dtInteger : add1DArray (ar.Int1);
            dtDouble  : add1DArray (ar.Double1);
            dtString  : add1DArray (ar.Str1);
            dtComplex : add1DArray (ar.Complex1);
         end;
     2 : case ar.dType of
            //dtByte    : add2DArray (ar.Byte2);
            //dtBoolean : add2DArray (ar.Bool2);
            dtInteger : add2DArray (ar.Int2);
            dtDouble  : add2DArray (ar.Double2);
            //dtString  : add2DArray (ar.Str2);
            //dtComplex : add2DArray (ar.Complex2);
         end;
  else raise ESBWException.Create ('Non supported array to add, only 1 and 2 dimensional arrays supported');
  end;
end;


procedure TSBWDataStream.add1DArray (value : array of byte);
var len, nDim, Arraydimension, i : integer;
begin
  nDim := 1; ArrayDimension := Length (value);
  len := 2 + 4 + 4*nDim + SizeOfByte * ArrayDimension;
  CheckMaxSize (FData.Len + len);

  CopyByte (dtArray);
  CopyByte (dtByte);
  CopyInteger (nDim);
  CopyInteger (ArrayDimension);

  for i := 0 to High (value) do
      CopyByte (value [i]);
end;


procedure TSBWDataStream.add1DArray (value : array of Boolean);
var len, nDim, Arraydimension, i : integer;
begin
  nDim := 1; ArrayDimension := Length (value);
  len := 2 + 4 + 4*nDim + SizeOfBoolean * ArrayDimension;
  CheckMaxSize (FData.Len + len);

  CopyByte (dtArray);
  CopyByte (dtBoolean);
  CopyInteger (nDim);
  CopyInteger (ArrayDimension);

  for i := 0 to High (value) do
      CopyBoolean (value [i]);
end;


procedure TSBWDataStream.add1DArray (value : array of integer);
var len, nDim, Arraydimension, i : integer;
begin
  nDim := 1; ArrayDimension := Length (value);
  len := 2 + 4 + 4*nDim + SizeOfInteger * ArrayDimension;
  CheckMaxSize (FData.Len + len);

  CopyByte (dtArray);
  CopyByte (dtInteger);
  CopyInteger (nDim);
  CopyInteger (ArrayDimension);

  for i := 0 to High (value) do
      CopyInteger (value [i]);
end;


procedure TSBWDataStream.add1DArray (value : array of double);
var len, nDim, Arraydimension, i : integer;
begin
  nDim := 1; ArrayDimension := Length (value);
  // 2 for array type and type byte
  // 4 for an integer to hold the number of dimensions
  len := 2 + 4 + 4*nDim + SizeOfDouble * ArrayDimension;
  CheckMaxSize (FData.Len + len);

  CopyByte (dtArray);
  CopyByte (dtDouble);
  CopyInteger (nDim);
  CopyInteger (ArrayDimension);

  for i := 0 to High (value) do
      CopyDouble (value [i]);
end;


procedure TSBWDataStream.add1DArray (value : array of TSBWComplex);
var len, nDim, Arraydimension, i : integer;
begin
  nDim := 1; ArrayDimension := Length (value);
  len := 2 + 4 + 4*nDim + SizeOfComplex * ArrayDimension;
  CheckMaxSize (FData.Len + len);

  CopyByte (dtArray);
  CopyBYte (dtComplex);
  CopyInteger (nDim);
  CopyInteger (ArrayDimension);

  for i := 0 to High (value) do
      CopyComplex (value [i]);
end;


procedure TSBWDataStream.add1DArray (value : array of AnsiString);
var len, nDim, Arraydimension, i : integer;
begin
  nDim := 1; ArrayDimension := Length (value);
  len := 2 + 4 + 4*nDim;
  for i := 0 to Length (value) - 1 do
      len := len + Length (value[i]) + SizeOfInteger;
  CheckMaxSize (FData.Len + len);

  CopyByte (dtArray);
  CopyByte (dtString);
  CopyInteger (nDim);
  CopyInteger (ArrayDimension);

  for i := 0 to High (value) do
      begin
      len := length (value[i]);
      CopyInteger (len);
      CopyString (value[i]);
      end;
end;


procedure TSBWDataStream.add2DArray (value : TSBW2DDoubleArray);
var len, nDim, rowDimension, colDimension, i, j : integer;
begin
  nDim := 2;
  rowDimension := Length (value);
  if rowDimension = 0 then
     colDimension := 0
  else colDimension := Length (value[0]);
  len := 2 + 4 + 4*nDim + SizeOfDouble * rowDimension * colDimension;
  CheckMaxSize (FData.Len + len + 1);

  CopyByte (dtArray);
  CopyByte (dtDouble);
  CopyInteger (nDim);
  CopyInteger (rowDimension);
  CopyInteger (colDimension);

  // Copy the array row by row
  for i := 0 to High (value) do
      for j := 0 to High (value[i]) do
          CopyDouble (value [i,j]);
end;


procedure TSBWDataStream.add2DArray (value : TSBW2DIntArray);
var len, nDim, rowDimension, colDimension, i, j : integer;
begin
  nDim := 2;
  rowDimension := Length (value);
  if rowDimension = 0 then
     colDimension := 0
  else colDimension := Length (value[0]);
  len := 2 + 4 + 4*nDim + SizeOfDouble * rowDimension * colDimension;
  CheckMaxSize (FData.Len + len);

  CopyByte (dtArray);
  CopyByte (dtInteger);
  CopyInteger (nDim);
  CopyInteger (rowDimension);
  CopyInteger (colDimension);

  // Copy the array row by row
  for i := 0 to High (value) do
      for j := 0 to High (value[i]) do
          CopyInteger (value [i,j]);
end;



// ---------------------------------------------------------------


function TSBWDataStream.peek() : integer;
begin
  result := FData.d[FData.ptr];
end;


function TSBWDataStream.isByte() : boolean;
begin
  result := peek() = dtByte;
end;


function TSBWDataStream.isInteger() : boolean;
begin
  result := peek() = dtInteger;
end;

function TSBWDataStream.isDouble() : boolean;
begin
  result := peek() = dtDouble;
end;

function TSBWDataStream.isComplex() : boolean;
begin
  result := peek() = dtComplex;
end;

function TSBWDataStream.isString() : boolean;
begin
  result := peek() = dtString;
end;

function TSBWDataStream.isBoolean() : boolean;
begin
  result := peek() = dtBoolean;
end;

function TSBWDataStream.isArray() : boolean;
begin
  result := peek() = dtArray;
end;

function TSBWDataStream.isList() : boolean;
begin
  result := peek() = dtList;
end;


// ------------------------------------------------------------------------

// get various types out of a data stream, does type checking


function TSBWDataStream.getByte() : byte;
begin
  try
    if not isByte() then
       raise ESBWExtractionError.Create ('expecting byte while retrieving data from data stream');
    inc (FData.ptr);  // Move pass Datatype byte
    Move (FData.d[FData.ptr], Result, SizeOfByte);
    FData.ptr := FData.ptr + SizeOfByte;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting byte type in data message:: ' + E.message);
  end;
end;


function TSBWDataStream.getInteger() : integer;
begin
  try
    if not isInteger () then
       raise ESBWExtractionError.Create ('expecting integer while retrieving data from data stream');
    inc (FData.ptr);  // Move pass Datatype byte
    Move (FData.d[FData.ptr], Result, SizeOfInteger);
    FData.ptr := FData.ptr + SizeOfInteger;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting integer type in data message: ' + E.message);
  end;
end;


function TSBWDataStream.getDouble() : double;
begin
  try
    if not isDouble() then
       raise ESBWExtractionError.Create ('expecting byte while retrieving data from data stream');
    inc (FData.ptr);  // Move pass DataType byte
    Move (FData.d[FData.ptr], Result, SizeOfDouble);
    FData.ptr := FData.ptr + SizeOfDouble;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting double type in data message: ' + E.message);
  end;
end;


function TSBWDataStream.getScalar() : double;
begin
  try
    if isDouble() then
       result := getDouble()
    else if isInteger () then
       result := getInteger()
    else
       raise ESBWExtractionError.Create ('expecting scalar type in data message');
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting scalar value in data message: ' + E.message);

  end;
end;


function TSBWDataStream.getComplex() : TSBWComplex;
begin
  try
    if not isComplex() then
       raise ESBWExtractionError.Create ('expecting complex number while retrieving data from data stream');
    inc (FData.ptr);  // Move pass DataType byte
    Move (FData.d[FData.ptr], Result, SizeOfComplex);
    FData.ptr := FData.ptr + SizeOfComplex;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting complex type in data message: ' + E.message);
  end;
end;


function TSBWDataStream.getString() : AnsiString;
var len : integer;
begin
  try
    // Three reads, one to read the type,
    //              one to read the length of the string
    //              one to read the string itself
    if not isString() then
       raise ESBWExtractionError.Create ('expecting string while retrieving data from data stream');
    inc (FData.ptr);  // Move pass DataType byte
    Move (FData.d[FData.ptr], len, SizeOfInteger);  // Get length of string
    FData.ptr := FData.ptr + SizeOfInteger;
    SetLength (result, len);
    if len > 0 then
       Move (FData.d[FData.ptr], result[1], len);
    FData.ptr := FData.ptr + len;
   except
    on E: Exception do
       raise ESBWExtractionError.Create ('Error raised during String extraction from DataStream: ' + E.message);
  end;
end;


function TSBWDataStream.getBoolean() : boolean;
begin
  try
    if not isBoolean() then
       raise ESBWExtractionError.Create ('expecting byte while retrieving data from data stream');
    inc (FData.ptr);  // Move pass Datatype byte
    Move (FData.d[FData.ptr], Result, SizeOfBoolean);
    FData.ptr := FData.ptr + SizeOfBoolean;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting boolean type in data message: ' + E.message);
  end;
end;


function TSBWDataStream.getList() : TSBWList;
var nItems, i : integer;
begin
  try
    if not isList() then
       raise ESBWExtractionError.Create ('expecting list while retrieving data from data stream');

    result := TSBWList.Create;
    inc (FData.ptr);  // Move pass Datatype byte

    nItems := CopyIntOut();
    for i := 0 to nItems - 1 do
        begin
        case peek() of
            dtInteger :
               result.Add (TSBWListItem.Create (getInteger()));
            dtBoolean :
               result.Add (TSBWListItem.Create (getBoolean()));
            dtDouble :
                result.Add (TSBWListItem.Create (getDouble ()));
            dtComplex :
                result.Add (TSBWListItem.Create (getComplex()));
            dtString :
               result.Add (TSBWListItem.Create (getString ()));
            dtArray :
               result.Add (TSBWListItem.Create (getArray ()));
            dtList :
               result.Add (TSBWListItem.Create (getList ()));
            dtTerminator :
               begin end;
        else
           ESBWException.Create ('Unrecognised data item in list');
        end;
        end;

  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting list type in data message: ' + E.message);
  end;
end;


function TSBWDataStream.getArray() : TSBWArray;
var i, j : integer;
    da : TSBW1DDoubleArray;
begin
  //ct.Acquire;
  try
    if not isArray() then
       raise ESBWExtractionError.Create ('Expecting array data while reading data message from module');

    inc (FData.ptr);  // Move pass Datatype byte

    result := TSBWArray.Create (da);
    result.dType := CopyByteOut();
    result.nDimensions := CopyIntOut();
    SetLength (result.nElements, result.nDimensions);
    for i := 0 to result.nDimensions - 1 do
        result.nElements[i] := CopyIntOut();

    case result.dType of
       dtByte :
         case result.nDimensions of
            1 : begin
                SetLength (result.Byte1, result.nElements[0]);
                for i := 0 to result.nElements[0] - 1 do
                    result.Byte1[i] := CopyByteOut();
                end;
         end;
       dtInteger :
         case result.nDimensions of
            1 : begin
                SetLength (result.Int1, result.nElements[0]);
                for i := 0 to result.nElements[0] - 1 do
                    result.Int1[i] := CopyIntOut();
                end;
            2 : begin
                SetLength (result.Int2, result.nElements[0]);
                for i := 0 to result.nElements[0] - 1 do
                    begin
                    SetLength (result.Int2[i], result.nElements[1]);
                    for j := 0 to result.nElements[1] - 1 do
                        result.Int2[i,j] := CopyIntOut();
                    end;
                end;
          end;
       dtDouble :
         case result.nDimensions of
            1 : begin
                SetLength (result.Double1, result.nElements[0]);
                for i := 0 to result.nElements[0] - 1 do
                    result.Double1[i] := CopyDoubleOut();
                end;
            2 : begin
                SetLength (result.Double2, result.nElements[0]);
                for i := 0 to result.nElements[0] - 1 do
                    begin
                    SetLength (result.Double2[i], result.nElements[1]);
                    for j := 0 to result.nElements[1] - 1 do
                        result.Double2[i,j] := CopyDoubleOut();
                    end;
                end;
          end;

       dtString :
         case result.nDimensions of
            1 : begin
                SetLength (result.Str1, result.nElements[0]);
                for i := 0 to result.nElements[0] - 1 do
                    result.Str1[i] := CopyStrOut();
                end;
            2 : begin
                SetLength (result.Str2, result.nElements[0]);
                for i := 0 to result.nElements[0] - 1 do
                    begin
                    SetLength (result.Double2[i], result.nElements[1]);
                    for j := 0 to result.nElements[1] - 1 do
                        result.Str2[i,j] := CopyStrOut();
                    end;
                end;
          end;

       dtList :
          case result.nDimensions of
             1 : begin
                 SetLength (result.List1, result.nElements[0]);
                 for i := 0 to result.nElements[0] - 1 do
                     begin
                     result.List1[i] := TSBWList.Create;

                     nItems := CopyIntOut();
                     for j := 0 to nItems - 1 do
                         case peek() of
                            dtInteger :
                               (result.List1[i] as TSBWList).Add (TSBWListItem.Create (getInteger()));
                            dtBoolean :
                               (result.List1[i] as TSBWList).Add (TSBWListItem.Create (getBoolean()));
                            dtDouble :
                                (result.List1[i] as TSBWList).Add (TSBWListItem.Create (getDouble ()));
                            dtComplex :
                                (result.List1[i] as TSBWList).Add (TSBWListItem.Create (getComplex()));
                            dtString :
                               (result.List1[i] as TSBWList).Add (TSBWListItem.Create (getString ()));
                            dtArray :
                               (result.List1[i] as TSBWList).Add (TSBWListItem.Create (getArray ()));
                            dtList :
                               (result.List1[i] as TSBWList).Add (TSBWListItem.Create (getList ()));
                            dtTerminator :
                               begin end;
                        else
                           ESBWException.Create ('Unrecognised data item in list');
                        end;
                     end;
                 end;
             2 : begin

                 end;
          end;
    end;

  except
    on E: Exception do
       raise ESBWExtractionError.Create ('expecting array type in data message: ' + E.message);
  end;
  //ct.Release;
end;


// Packing routines, use these to return data from services methods
// Usage: pack ([1,2,3]), pack([1, "hello", 3.14, True])
//        pack ([Lt, 3.14]) <- adds a alit and a double to the data stream
// ----------------------------------------------------------------------------

function TSBWDataStream.pack (const value : integer) : SBWDataBlockWriter;
begin
  clear;
  add (value);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const value : double) :  SBWDataBlockWriter;
begin
  clear;
  add (value);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const value : boolean) :  SBWDataBlockWriter;
begin
  clear;
  add (value);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const value : AnsiString) :  SBWDataBlockWriter;
begin
  clear;
  add (value);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const value : TSBWComplex) :  SBWDataBlockWriter;
begin
  clear;
  add (value);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const value : TSBWList) :  SBWDataBlockWriter;
begin
  clear;
  add (value);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const value : TSBW1DListArray) :  SBWDataBlockWriter;
begin
  clear;
end;


function TSBWDataStream.pack (const Args : array of const) : SBWDataBlockWriter;
begin
  BuildArgumentList (Args);
  // Note that writerObject originates from the SBW DLL
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const vec : TSBW1DDoubleArray) : SBWDataBlockWriter;
begin
  Clear;
  add1DArray (vec);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const mat : TSBW2DDoubleArray) : SBWDataBlockWriter;
begin
  Clear;
  add2DArray (mat);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const mat : TSBW1DIntArray) : SBWDataBlockWriter;
begin
  Clear;
  add1DArray (mat);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;



function TSBWDataStream.pack (const mat : TSBW2DIntArray) : SBWDataBlockWriter;
begin
  Clear;
  add2DArray (mat);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


function TSBWDataStream.pack (const str : TSBW1DStrArray) : SBWDataBlockWriter;
begin
  Clear;
  add1DArray (str);
  SBWWriteRaw(writerObject, PAnsiChar (Data.d), Data.len);
  result := writerObject;
end;


//----------------------------------------------------------------------

procedure TSBWDataStream.BuildArgumentList (const Args : array of const);
var i : integer;
begin
  Clear (72);
  for i := 0 to high(Args) do
      begin
      with Args[i] do
        case VType of
         vtChar :
           add (byte (VChar));
         vtBoolean :
           begin
           if VBoolean then
              Add (True)
           else Add (False);
           end;
         vtInteger:
           Add (VInteger);
         vtExtended:
           Add (VExtended^);
         vtPointer :
           add2DArray (TSBW2DDoubleArray (VPointer));
         vtString:
           Add (Args[i].VWideChar);
         vtAnsiString:
           Add (AnsiString(VAnsiString));
         vtUnicodeString:
           Add (string (vUnicodeString));
         vtObject :
           if (VObject is TSBWList) then
              Add (VObject as TSBWList)
           else raise ESBWException.create ('Error while building argument list, unrecognised argument');
        end;
      end;
end;


procedure TSBWDataStream.BuildFromDataItemList (SBWList : TSBWList);
var i : integer;
begin
  for i := 0 to SBWList.Count - 1 do
      case SBWList[i].DataType of
           dtInteger :
               Add (SBWList[i].i);
           dtDouble :
               Add (SBWList[i].d);
           dtString :
               Add (SBWList[i].str);
           dtBoolean :
               Add (SBWList[i].bool);
      else
           raise Exception.Create ('Internal Error: Unknown datatype in BuildFromDataItemList procedure');
      end;
end;


// ------------------------------------------------------------------------


function GetDataType (var pData : PDataStream) : byte;
begin
  result := Byte (pData^);
end;


// ------------------------------------------------------------------------



function ExtractByte (var pData : PDataStream) : Byte;
begin
  try
    if Byte (pData^) <> dtByte then
       raise ESBWExtractionError.Create ('Expecting Byte during ExtractByte');
    inc (pData);  // Move pass Datatype byte
    Move (pData^, Result, SizeOfByte);
    pData := pData + SizeOfByte;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('Error raised during Byte Extraction from DataStream: ' + E.message);
  end;
end;


function ExtractBoolean (var pData : PDataStream) : boolean;
begin
  try
    if Byte (pData^) <> dtBoolean then
       raise ESBWExtractionError.Create ('Expecting Boolean during ExtractBoolean');
    inc (pData);  // Move pass Datatype byte
    Move (pData^, Result, SizeOfBoolean);
    pData := pData + SizeOfBoolean;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('Error raised during Boolean Extraction from DataStream: ' + E.message);
  end;
end;


function ExtractInteger (var pData : PDataStream) : integer;
begin
  try
    if Byte (pData^) <> dtInteger then
       raise ESBWExtractionError.Create ('Expecting Integer during ExtractInteger');
    inc (pData);  // Move pass Datatype byte
    Move (pData^, Result, SizeOfInteger);
    pData := pData + SizeOfInteger;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('Error raised during Integer Extraction from DataStream: ' + E.message);
  end;
end;


function ExtractDouble (var pData : PDataStream) : double;
begin
  try
    if Byte (pData^) <> dtDouble then
       raise ESBWExtractionError.Create ('Expecting Double during ExtractDouble');
    inc (pData);  // Move pass DataType byte
    Move (pData^, Result, SizeOfDouble);
    pData := pData + SizeOfDouble;
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('Error raised during Double Extraction from DataStream: ' + E.message);
  end;
end;


function ExtractString (var pData : PDataStream) : AnsiString;
var len : integer;
    ansiResult : AnsiString;
begin
  try
    // Three reads, one to read the type,
    //              one to read the length of the string
    //              one to read the string itself
    if Byte (pData^) <> dtString then
       raise ESBWExtractionError.Create ('Expecting String during ExtractString');
    inc (pData);  // Move pass DataType byte
    Move (pData^, len, SizeOfInteger);  // Get length of string
    pData := pData + SizeOfInteger;
    SetLength (ansiResult, len);
    if len > 0 then
       Move (pData^, ansiResult[1], len);
    pData := pData + len;
    result := ansiResult;
   except
    on E: Exception do
       raise ESBWExtractionError.Create ('Error raised during String Extraction from DataStream: ' + E.message);
  end;
end;


function ExtractComplex (var pData : PDataStream) : TSBWComplex;
begin
  try
    if Byte (pData^) <> dtComplex then
       raise ESBWExtractionError.Create ('Expecting Complex during ExtractComplex');
    inc (pData);  // Move pass DataType byte
    Move (pData^, Result, Sizeof (TSBWComplex));
    pData := pData + SizeOf (TSBWComplex);
  except
    on E: Exception do
       raise ESBWExtractionError.Create ('Error raised during Complex Extraction from DataStream: ' + E.message);
  end;
end;


function ExtractArray (var pData : PDataStream) : TSBWArray;
var i, j, k, nDimensions : integer;
    len : integer;
    str : AnsiString;
begin
  result := TSBWArray.Create;
  inc (pData);
  Move (pData^, result.dType, SizeofByte);
  inc (pData);
  Move (pData^, nDimensions, SizeOfInteger);
  result.nDimensions := nDimensions;
  setLength (result.nElements, nDimensions);
  for k := 0 to nDimensions - 1 do
      begin
      inc (pData, SizeOfInteger);
      Move (pData^, result.nElements[k], SizeOfInteger);
      end;

  case result.dType of
        dtInteger :
          begin
          inc (pData, SizeOfInteger);  // Jump to first row of data
          case nDimensions of
            1 : begin
                setLength (result.Int1, result.nElements[0]);
                for j := 0 to result.nElements[0] - 1 do
                    begin
                    Move (pData^, (result.Int1)[j], SizeOfInteger);
                    inc (pData, SizeOfInteger);
                    end;
                end;
             2 : begin
                 setLength (result.Int2, result.nElements[0], result.nElements[1]);
                 for i := 0 to result.nElements[0] - 1 do
                     begin
                     for j := 0 to result.nElements[1] - 1 do
                         begin
                         Move (pData^, (result.Int2)[i, j], SizeOfInteger);
                         inc (pData, SizeOfInteger);
                         end;
                     end;
                 end;
          end;
          end;

        dtDouble :
          begin
          inc (pData, SizeOfInteger);  // Jump to first row of data
          case nDimensions of
             1 : begin
                 setLength (result.Double1, result.nElements[0]);
                 for j := 0 to result.nElements[0] - 1 do
                     begin
                     Move (pData^, (result.Double1)[j], SizeOfDouble);
                     inc (pData, SizeOfDouble);
                     end;
                 end;
             2 : begin
                 setLength (result.Double2, result.nElements[0], result.nElements[1]);
                 for i := 0 to result.nElements[0] - 1 do
                     begin
                     for j := 0 to result.nElements[1] - 1 do
                         begin
                         Move (pData^, (result.Double2)[i, j], SizeOfDouble);
                         inc (pData, SizeOfDouble);
                         end;
                     end;
                 end;
          end;
          end;

        dtComplex :
          begin
          inc (pData, SizeOfInteger);  // Jump to first row of data
          case nDimensions of
             1 : begin
                 setLength (result.Complex1, result.nElements[0]);
                 for j := 0 to result.nElements[0] - 1 do
                     begin
                     Move (pData^, (result.Complex1)[j], SizeOfComplex);
                     inc (pData, SizeOfComplex);
                     end;
                 end;
             2 : begin
                 setLength (result.Complex2, result.nElements[0], result.nElements[1]);
                 for i := 0 to result.nElements[0] - 1 do
                     begin
                     for j := 0 to result.nElements[1] - 1 do
                         begin
                         Move (pData^, (result.Complex2)[i, j], SizeOfComplex);
                         inc (pData, SizeOfComplex);
                         end;
                     end;
                 end;
          end;
          end;

        dtString :
         begin
         inc (pData, SizeOfInteger);  // Jump to first row of data   
         case nDimensions of
             1 : begin
                 setLength (result.Str1, result.nElements[0]);
                 for j := 0 to result.nElements[0] - 1 do
                     begin
                     Move (pData^, len, SizeOfInteger);  // Get length of string
                     pData := pData + SizeOfInteger;
                     if len > 0 then
                        begin
                        setLength (str, len);
                        Move (pData^, str[1], len);
                        result.Str1[j] := str;
                        end;
                     pData := pData + len;
                     end;
                 end;
         else
           raise ESBWExtractionError.Create ('String arrays other than 1-Dimensional, not yet supported');

          end;
          end;
          
         dtList :
          begin
          inc (pData, SizeOfInteger);  // Jump to first row of data
          setLength (result.List1, result.nElements[0]);
          for i := 0 to result.nElements[0] - 1 do
            case nDimensions of
               1 : result.List1[i] := ExtractList (pData);
            else
               raise ESBWExtractionError.Create ('List arrays other than 1-Dimensional, not yet supported');
            end;
          end;
       else
         raise ESBWExtractionError.Create ('Error raised during Array Extraction, unsupported type inside array: ' + convertSigByte (result.dType) + '');
      end;
end;


{function Extract1DArray (var pData : PDataStream) : TSBW1DArray;
var n, j, k : integer;
    dims : array of integer; Dimensions : integer;
    lst : TSBWList;
begin
  result := TSBW1DArray.Create;
  inc (pData);
  Move (pData^, result.ArrayType, SizeofByte);
  inc (pData);
  Move (pData^, Dimensions, SizeOfInteger);
  SetLength (dims, Dimensions);
  for k := 0 to Dimensions - 1 do
      begin
      inc (pData, SizeOfInteger);
      Move (pData^, dims[k], SizeOfInteger);
      end;

  SetLength (result.Row, 1);
  inc (pData, SizeOfInteger);  // Jump to first row of data
  case result.ArrayType of
        dtInteger :
          begin
          SetLength (result.Row, dims[0]);
          for j := 0 to dims[0] - 1 do
              begin
              Move (pData^, (result.Row)[j].i, SizeOfInteger);
              inc (pData, SizeOfInteger);
              end;
          end;

        dtDouble :
          begin
          SetLength (result.Row, dims[0]);
          for j := 0 to dims[0] - 1 do
              begin
              Move (pData^, (result.Row)[j].d, SizeOfDouble);
              inc (pData, SizeOfDouble);
              end;
          end;

        dtString :
          begin
          SetLength (result.Row, dims[0]);
          for j := 0 to dims[0] - 1 do
              begin
              Move (pData^, n, SizeOfInteger);
              inc (pData, SizeOfInteger);
              result.Row[j].str := TStrObject.Create (n);
              Move (pData^, (result.Row)[j].str.value[1], n);
              inc (pData, n);
              end;
          end;

        dtList :
          begin
            {SetLength (result.Row, dims[0]);
            for j := 0 to dims[0] - 1 do
                begin
                //result.Row := TSBWList.Create;
                result.Row[j].List := TSBWList.Create;
                lst := result.Row[j].List as TSBWList;
                Move (pData^, n, SizeofInteger);
                inc (pData, SizeOfInteger);
                for k := 0 to n - 1 do
                  case GetDataType (pData) of
                    dtInteger :
                       lst.Add (TSBWListItem.Create (ExtractInteger (pData)));
                    dtBoolean :
                       lst.Add (TSBWListItem.Create (ExtractBoolean (pData)));
                    dtDouble :
                       lst.Add (TSBWListItem.Create (ExtractDouble (pData)));
                    dtString :
                       lst.Add (TSBWListItem.Create (ExtractString (pData)));
                    //dtArray :
                    //   lst.Add (TSBWListItem.Create (ExtractArray (pData)));
                    dtList :
                       lst.Add (TSBWListItem.Create (ExtractList (pData)));
                  else
                     showmessage ('Unrecognised data item in list');
                  end;
                end;}
               //end;}
      //end;
//end;}


function ExtractList (var pData : PDataStream) : TSBWList;
var n, i : integer; str : AnsiString; ltitem : TSBWListItem;
begin
  result := TSBWList.Create;
  //inc (pData);
  Move (pData^, n, SizeofInteger);
  inc (pData, SizeOfInteger);
  for i := 0 to n - 1 do
     case GetDataType (pData) of
           dtInteger :
              result.Add (TSBWListItem.Create (ExtractInteger (pData)));
           dtBoolean :
              result.Add (TSBWListItem.Create (ExtractBoolean (pData)));
           dtDouble :
               result.Add (TSBWListItem.Create (ExtractDouble (pData)));
           dtComplex :
               result.Add (TSBWListItem.Create (ExtractComplex (pData)));
           dtString :
              begin
              str := ExtractString (pData);
              ltitem := TSBWListItem.Create;
              ltitem.DataType := dtString;
              ltItem.str := str;
              result.Add (ltItem);
               end;
           dtArray :
              result.Add (TSBWListItem.Create (ExtractArray (pData)));
           dtList :
              begin
              inc (pData);
              result.Add (TSBWListItem.Create (ExtractList (pData)));
              end;
           dtTerminator :
              begin end;
      else
         showmessage ('Unrecognised data item in list');
      end;
end;


function ExtractListData (var pData : PDataStream; n : integer) : TSBWList; // doesn't check for list type byte
var i : integer; str : AnsiString;  ltitem : TSBWListItem;
begin
  result := TSBWList.Create;
  Move (pData^, n, SizeofInteger);
  inc (pData, SizeOfInteger);

  for i := 0 to n - 1 do
     case getDataType (pData) of
           dtTerminator : exit;
           dtByte :
              result.Add (TSBWListItem.Create (ExtractByte(pData)));
           dtInteger :
              result.Add (TSBWListItem.Create (ExtractInteger (pData)));
           dtBoolean :
              result.Add (TSBWListItem.Create (ExtractBoolean (pData)));
           dtDouble :
               result.Add (TSBWListItem.Create (ExtractDouble (pData)));
           dtComplex :
               result.Add (TSBWListItem.Create (ExtractComplex (pData)));
           dtString :
              begin
              str := ExtractString (pData);
              ltitem := TSBWListItem.Create;
              ltitem.DataType := dtString;
              ltItem.str := str;
              //ltitem := TSBWListItem.Create (str);
              result.Add (ltItem);
              //result.Add (TSBWListItem.Create (ExtractString (pData)));
              end;
           dtArray :
              result.Add (TSBWListItem.Create (ExtractArray (pData)));
           dtList :
              begin
              inc (pData);
              result.Add (TSBWListItem.Create (ExtractList (pData)));
              end;
      else
         raise ESBWException.Create ('Unrecognised data item in list: [' + inttostr (getDataType (pData)) + ']');
      end;
end;


// ------------------------------------------------------------------------


function AddArray (var Data : TData; nDim : integer; dims : array of integer) : TData;
var len, i : integer;
begin
  len := 2 + 4 + 4*nDim;
  ReallocMem (Data.d, Data.len + len);
  result.d := Data.d;
  result.d[Data.Len] := dtArray;  inc (Data.Len);
  result.d[Data.Len] := dtString; inc (Data.Len);
  Move (nDim, result.d[Data.len], Sizeof (Integer)); inc (Data.Len, SizeOf (Integer));

  for i := 0 to High (dims) do
      begin
      Move (dims[i], result.d[Data.Len], 4);
      inc (Data.Len, 4);
      end;

  Move (Data.Len, Data.d^, 4);
  result.Len := Data.Len;
end;


function AddDoubleToArray (var Data : TData; value : double) : TData;
var len : integer;
begin
  len := sizeof (Double);
  ReAllocMem (Data.d, Data.Len + len + 4);
  Move (len, Data.d[Data.len], 4);
  inc (Data.len, 4);
  Move (value, Data.d[Data.len], len);
  Data.len := Data.Len + len;
  Move (Data.Len, Data.d^, 4);
  result := data;
end;


function AddComplexToArray (var Data : TData; value : TSBWComplex) : TData;
var len : integer;
begin
  len := sizeof (TSBWComplex);
  ReAllocMem (Data.d, Data.Len + len + 4);
  Move (len, Data.d[Data.len], 4);
  inc (Data.len, 4);
  Move (value, Data.d[Data.len], len);
  Data.len := Data.Len + len;
  Move (Data.Len, Data.d^, 4);
  result := data;
end;


function AddStringToArray (var Data : TData; name : AnsiString) : TData;
var len : integer;
begin
  len := length (name);
  ReAllocMem (Data.d, Data.Len + len + 4);
  Move (len, Data.d[Data.len], 4);
  inc (Data.len, 4);
  Move (name[1], Data.d[Data.len], len);
  Data.len := Data.Len + len;
  Move (Data.Len, Data.d^, 4);
  result := data;
end;


function AddBoolean (var Data : TData; value : boolean) : TData;
var len : integer;
begin
  len := SizeOf (Boolean) + 1;
  ReallocMem (Data.d, Data.Len + len);
  result.d := Data.d;
  result.d[Data.Len] := dtBoolean;

  inc (Data.Len);
  Move (value, result.d[Data.len], Sizeof (Boolean));
  inc (Data.Len, SizeOf (Boolean));
  Move (Data.Len, Data.d^, 4);
  result.Len := Data.Len;
end;


// Add an integer to the end of the TData type
function AddInteger (var Data : TData; value : integer) : TData;
var len : integer;
begin
  len := SizeOf (Integer) + 1;
  ReallocMem (Data.d, Data.len + len);
  result.d := Data.d;
  result.d[Data.Len] := dtInteger;
  inc (Data.Len);
  Move (value, result.d[Data.len], Sizeof (Integer));
  inc (Data.Len, SizeOf (Integer));
  Move (Data.Len, Data.d^, 4);
  result.Len := Data.Len;
end;


// Add an double to the end of the TData type
function AddDouble (var Data : TData; value : double) : TData;
var len : integer;
begin
  len := SizeOf (Double) + 1;
  ReallocMem (Data.d, Data.len + len);
  result.d := Data.d;
  result.d[Data.Len] := dtDouble;
  inc (Data.Len);
  Move (value, result.d[Data.len], Sizeof (double));
  inc (Data.Len, SizeOf (Double));
  Move (Data.Len, Data.d^, 4);
  result.Len := Data.Len;
end;


// Add a complex number to the end of the TData type
function AddComplex (var Data : TData; value : TSBWComplex) : TData;
var len : integer;
begin
  len := SizeOf (TSBWComplex) + 1;
  ReallocMem (Data.d, Data.len + len);
  result.d := Data.d;
  result.d[Data.Len] := dtComplex;
  inc (Data.Len);
  Move (value, result.d[Data.len], Sizeof (TSBWComplex));
  inc (Data.Len, SizeOf (TSBWCOmplex));
  Move (Data.Len, Data.d^, 4);
  result.Len := Data.Len;
end;


// Add a string to the end of the TData type
function AddString (var Data : TData; value : AnsiString) : TData;
var len, lstr : integer;
begin
  lstr := Length (value);
  // dtString, Length, Characters
  len := lstr + SizeOf (Integer) + 1;
  ReallocMem (Data.d, Data.len + len);
  result.d := Data.d;
  result.d[Data.Len] := dtString;
  inc (Data.Len);
  Move (lstr, result.d[Data.len], Sizeof (Integer));
  inc (Data.Len, SizeOf (Integer));
  if lstr > 0 then
     Move (value[1], result.d[Data.len], lstr);
  inc (Data.Len, lstr);

  Move (Data.Len, Data.d^, 4);
  result.len := Data.Len;
end;


function AddByte (var Data : TData; b : byte) : TData;
begin
  ReallocMem (Data.d, Data.len + 1);
  result.d := Data.d;
  result.d[Data.Len] := b;
  inc (Data.Len);
  result.Len := Data.Len;
end;


function AddList (var Data : TData; const value : variant) : TData;
var i, n, x : integer;
begin
  n := VarArrayHighBound (value, 1);
  Data := AddByte (Data, dtList);
  ReallocMem (Data.d, Data.len + SizeOfInteger);
  Move (n, Data.d[Data.len], Sizeof (integer));
  inc (Data.Len, SizeOf (integer));

  for i := 0 to n do
      begin
        x := VarType (value[i]);
        case x of
         varBoolean :
           AddBoolean (Data, value[i]);
         varInteger:
           AddInteger (Data, value[i]);
         varDouble,
         varCurrency:
           AddDouble (Data, value[i]);
         varOleStr:
           AddString (Data, value[i]);
         // SBW List type
         //vtVariant :
         //  begin
           //AddList (Data, VVariant^);
        //   end;
        end;
      end;
end;


function AddList (var Data : TData; const Value : TSBWList) : TData;
var len : integer; ds : TSBWDataStream; d : TData;
begin
  ds := Value.GetDataStream as TSBWDataStream;
  d := ds.Data;
  len := 1 + SizeOfInteger + d.Len;  // dtList, nItems, DataItems
  ReallocMem (Data.d, Data.len + len);
  result.d := Data.d;
  result.d[Data.Len] := dtList;
  inc (Data.Len, 1);
  Move (ds.nItems, result.d[Data.Len], SizeOfInteger);
  inc (Data.Len, SizeOfInteger);
  if d.Len > 0 then
     Move (d.d^, result.d[Data.len], d.Len);
  inc (Data.Len, d.Len);

  Move (Data.Len, Data.d^, 4);
  result.len := Data.Len;
  ds.Free;
end;


function AddTerminator (var Data : TData) : TData;
begin
  ReallocMem (Data.d, Data.len + 1);
  result.d := Data.d;
  result.d[Data.Len] := dtTerminator;
  inc (Data.Len);
  Move (Data.Len, Data.d^, 4);
  result.Len := Data.Len;
end;


function MergeData (D1, D2 : TData) : TData;
var tmp : PByteArray;
begin
  result.len := D1.Len + D2.Len;
  result.d := AllocMem (result.Len);
  Move (D1.d^, result.d^, D1.Len);
  tmp := result.d;
  Move (D2.d[0], tmp[D1.Len], D2.Len);

  Move (result.Len, result.d^, 4);
end;


function DataTypeToString (DataType : byte) : AnsiString;
begin
  case DataType of
       dtInteger : result := 'integer';
       dtDouble  : result := 'double';
       dtComplex : result := 'complex';
       dtBoolean : result := 'boolean';
       dtString  : result := 'string';
       dtArray   : result := 'array';
       dtList    : result := 'list';
       dtTerminator : result := 'null';

    else result := 'Unknown DataType';
  end;
end;


function BoolToStr (value : boolean) : AnsiString;
begin
  if value then
     result := 'True'
  else result := 'False';
end;


end.
