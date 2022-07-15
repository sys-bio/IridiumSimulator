unit uNewMatrix;

interface

Uses Classes, SysUtils;

type
  T2DArray = array[0..100000] of double;
  P2Darray = ^T2Darray;

  TNMatrix = class
    private
     data : P2Darray;
     nRows, nCols : integer;

     function  getNumRows : integer;
     function  getNumCols : integer;
     function  getElement (i, j : integer) : double;
     procedure setElement (i, j : integer; value : double);

    public
     property r : integer read getNumRows;
     property c : integer read getNumCols;

     property  Values[i, j : integer] : double read getElement write setElement; default;
     class procedure test;
     constructor Create (r, c : integer);
  end;


implementation


constructor TNMatrix.Create(r, c: integer);
begin
  data := getMemory (r*c*sizeof (double));
  self.nRows := r;
  self.nCols := c;
end;


function TNMatrix.getElement(i, j: integer): double;
begin
  result := data[i*nRows + j];
end;


function TNMatrix.getNumCols: integer;
begin
  result := nCols;
end;


function TNMatrix.getNumRows: integer;
begin
  result := nRows;
end;


procedure TNMatrix.setElement(i, j: integer; value: double);
begin
  data[i*nCols + j] := value;
end;


class procedure TNMatrix.test;
var i, j : integer;
    m : TNMatrix;
begin
  m := TNMatrix.Create(4,5);
  for i := 0 to m.r - 1 do
      for j := 0 to m.c - 1 do
          m[i,j] := i*j;

  for i := 0 to m.r - 1 do
      for j := 0 to m.c - 1 do
          if m[i,j] <> i*j then
             raise Exception.Create('Test Failed');

  m.Free;
end;

end.
