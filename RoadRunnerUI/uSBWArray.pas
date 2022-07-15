unit uSBWArray;

interface

Uses SysUtils, uSBWCommon, uSBWComplex;


type
  TElement = record
               getInteger : integer;
               getDouble : double;
               getScalar : double;
               getBoolean : boolean;
               getString : AnsiString;
               getComplex : TSBWComplex;
               getArray : TObject;
               getList : TObject;
  end;

  TSBW1DByteArray = array of byte;
  TSBW2DByteArray = array of TSBW1DByteArray;
  TSBW3DByteArray = array of array of TSBW1DByteArray;

  TSBW1DBooleanArray = array of Boolean;
  TSBW2DBooleanArray = array of TSBW1DBooleanArray;
  TSBW3DBooleanArray = array of array of TSBW1DBooleanArray;

  TSBW1DIntArray = array of integer;
  TSBW2DIntArray = array of TSBW1DIntArray;
  TSBW3DIntArray = array of array of TSBW1DIntArray;

  TSBW1DDoubleArray = array of double;
  TSBW2DDoubleArray = array of TSBW1DDoubleArray;
  TSBW3DDoubleArray = array of array of TSBW1DDoubleArray;

  TSBW1DComplexArray = array of TSBWComplex;
  TSBW2DComplexArray = array of TSBW1DComplexArray;
  TSBW3DComplexArray = array of array of TSBW1DComplexArray;

  TSBW1DStrArray = array of AnsiString;
  TSBW2DStrArray = array of TSBW1DStrArray;
  TSBW3DStrArray = array of array of array of TSBW1DStrArray;

  TSBW1DListArray = array of TObject;
  
  TSBWArray = class (TObject)
                dType : byte;
                nDimensions : integer;         // 1D, 2D, 3D etc
                nElements : array of integer;  // Number of elements in each dimension

                Byte1 : TSBW1DByteArray;
                Byte2 : TSBW2DByteArray;
                Byte3 : TSBW3DByteArray;

                Bool1 : TSBW1DBooleanArray;
                Bool2 : TSBW2DBooleanArray;
                Bool3 : TSBW3DBooleanArray;

                Int1 : TSBW1dIntArray;
                Int2 : TSBW2dIntArray;
                Int3 : TSBW3dIntArray;

                Double1 : TSBW1dDoubleArray;
                Double2 : TSBW2dDoubleArray;
                Double3 : TSBW3dDoubleArray;

                Complex1 : TSBW1DComplexArray;
                Complex2 : TSBW2DComplexArray;
                Complex3 : TSBW3DComplexArray;

                Str1 : TSBW1dStrArray;
                Str2 : TSBW2dStrArray;
                Str3 : TSBW3dStrArray;

                List1 : TSBW1dListArray;

                function getInt (i, j : integer) : integer; overload;
                function getInt (i : integer) : integer; overload;

                function getBoolean (i, j : integer) : boolean; overload;
                function getBoolean (i : integer) : boolean; overload;

                function getDouble (i, j : integer) : double; overload;
                function getDouble (i : integer) : double; overload;

                function getScalar (i : integer) : double; overload;
                function getScalar (i, j : integer) : double; overload;

                function getComplex (i : integer) : TSBWComplex; overload;
                function getComplex (i, j : integer) : TSBWComplex; overload;

                function getString (i, j : integer) : AnsiString; overload;
                function getString (i : integer) : AnsiString; overload;

                function getList (i : integer) : TObject; overload;

                function getLength : integer;
                function getNumRows : integer;
                function getNumCols : integer;

                constructor Create; overload;
                constructor Create (ar : TSBW1DIntArray); overload;
                constructor Create (ar : TSBW2DIntArray); overload;
                constructor Create (ar : TSBW1DDoubleArray); overload;
                constructor Create (ar : TSBW2DDoubleArray); overload;
                constructor Create (ar : TSBW1DComplexArray); overload;
                constructor Create (ar : TSBW2DComplexArray); overload;
                constructor Create (ar : TSBW1DStrArray); overload;
                constructor Create (ar : TSBW2DStrArray); overload;
                destructor  Destroy; override;
  end;


implementation

uses uSBWList;

constructor TSBWArray.Create;
begin
  inherited Create;
end;


constructor TSBWArray.Create (ar : TSBW1DIntArray);
begin
  Int1 := ar;
  dType := dtInteger;
  nDimensions := 1;
  setlength (nElements, 1);
  nElements[0] := length (ar);
end;


constructor TSBWArray.Create (ar : TSBW2DIntArray);
begin
  Int2 := ar;
  dType := dtInteger;
  nDimensions := 2;
  setlength (nElements, 2);
  nElements[0] := length (ar);
  if nElements[0] = 0 then
     nElements[1] := 0
  else nElements[1] := length (ar[0]);
end;


constructor TSBWArray.Create (ar : TSBW1DDoubleArray);
begin
  inherited Create;
  Double1 := ar;
  dType := dtDouble;
  nDimensions := 1;
  setlength (nElements, 1);
  nElements[0] := length (ar);
end;


constructor TSBWArray.Create (ar : TSBW2DDoubleArray);
begin
  Double2 := ar;
  dType := dtDouble;
  nDimensions := 2;
  setlength (nElements, 2);
  nElements[0] := length (ar);
  if nElements[0] = 0 then
     nElements[1] := 0
  else nElements[1] := length (ar[0]);
end;


constructor TSBWArray.Create (ar : TSBW1DComplexArray);
begin
  inherited Create;
  Complex1 := ar;
  dType := dtComplex;
  nDimensions := 1;
  setlength (nElements, 1);
  nElements[0] := length (ar);
end;


constructor TSBWArray.Create (ar : TSBW2DComplexArray);
begin
  Complex2 := ar;
  dType := dtComplex;
  nDimensions := 2;
  setlength (nElements, 2);
  nElements[0] := length (ar);
  if nElements[0] = 0 then
     nElements[1] := 0
  else nElements[1] := length (ar[0]);
end;




constructor TSBWArray.Create (ar : TSBW1DStrArray);
begin
  Str1 := ar;
  dType := dtString;
  nDimensions := 1;
  setlength (nElements, 1);
  nElements[0] := length (ar);
end;


constructor TSBWArray.Create (ar : TSBW2DStrArray);
begin
  Str2 := ar;
  dType := dtString;
  nDimensions := 2;
  setlength (nElements, 2);
  nElements[0] := length (ar);
  if nElements[0] = 0 then
     nElements[1] := 0
  else nElements[1] := length (ar[0]);
end;


// ------------------------------------------------------------------


destructor TSBWArray.Destroy;
var i : integer;
    lt : TSBWList;
begin
  setLength (nElements, 0);
  setLength (byte1, 0);
  setLength (bool1, 0);
  setLength (int1, 0);
  setLength (double1, 0);
  setLength (str1, 0);
  setLength (complex1, 0);
  for i := 0 to Length (List1) - 1 do
      begin
      lt := TSBWList (List1[i]);
      lt.Free;
      end;

  setLength (byte2, 0);
  setLength (bool2, 0);
  setLength (int2, 0);
  setLength (double2, 0);
  setLength (str2, 0);
  setLength (complex2, 0);

  setLength (byte3, 0);
  setLength (bool3, 0);
  setLength (int3, 0);
  setLength (double3, 0);
  setLength (str3, 0);
  setLength (complex3, 0);

  inherited Destroy;
end;


function TSBWArray.getLength : integer;
begin
  if nDimensions = 1 then
     begin
     case dType of
        dtByte    : result := length (Byte1);
        dtInteger : result := length (Int1);
        dtDouble  : result := length (Double1);
        dtBoolean : result := length (Bool1);
        dtString  : result := length (Str1);
        dtComplex : result := length (Complex1);
        dtList    : result := length (List1);
     else raise ESBWException.Create ('getLength: unsupported type: ' + inttostr (dType));
     end
     end
  else raise ESBWException.Create ('getLength only applies to 1D arrays');
end;


function TSBWArray.getNumRows : integer;
begin
  if nDimensions = 2 then
     begin
     case dType of
         dtInteger : result := length (Int2);
         dtDouble  : result := Length (Double2);
         dtString  : result := Length (Str2);
         dtBoolean : result := Length (Bool2);
     end;
     end
  else raise ESBWException.Create ('getNumRows only applies to 2D arrays');
end;


function TSBWArray.getNumCols : integer;
begin
  if nDimensions = 2 then
     begin
     case dType of
         dtInteger : result := length (Int2[0]);
         dtDouble  : result := Length (Double2[0]);
         dtString  : result := Length (Str2[0]);
         dtBoolean : result := Length (Bool2[0]);
     end;
     end
  else raise ESBWException.Create ('getNumCols only applies to 2D arrays');
end;


function TSBWArray.getInt (i, j : integer) : integer;
begin
  result := Int2[i,j];
end;


function TSBWArray.getInt (i : integer) : integer;
begin
  result := Int1[i];
end;

// ----

function TSBWArray.getBoolean (i, j : integer) : boolean;
begin
  result := Bool2[i,j];
end;


function TSBWArray.getBoolean (i : integer) : boolean;
begin
  result := Bool1[i];
end;


// ----


function TSBWArray.getDouble (i, j : integer) : double;
begin
  result := Double2[i,j];
end;


function TSBWArray.getDouble (i : integer) : double;
begin
  result := Double1[i];
end;

// ----


function TSBWArray.getScalar (i, j : integer) : double;
begin
  if dType = dtInteger then
     result := getInt (i, j)
  else result := getDouble (i, j);
end;


function TSBWArray.getScalar (i : integer) : double;
begin
  if dType = dtInteger then
     result := getInt (i)
  else result := getDouble (i);
end;

// ----

function TSBWArray.getComplex (i : integer) : TSBWComplex;
begin
  result := Complex1[i];
end;


function TSBWArray.getComplex (i, j : integer) : TSBWComplex;
begin
  result := Complex2[i,j];
end;


// ----

function TSBWArray.getString (i, j : integer) : AnsiString;
begin
  result := Str2[i,j];
end;


function TSBWArray.getString (i : integer) : AnsiString;
begin
  result := Str1[i];
end;


function TSBWArray.getList (i : integer) : TObject;
begin
  result := List1[i];
end;


end.
