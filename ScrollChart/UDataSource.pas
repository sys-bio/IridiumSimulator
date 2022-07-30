unit UDataSource;

interface
uses
  System.Classes, System.UITypes, System.UIConsts, System.Types,
  System.SysUtils, Skia, Skia.FMX, math;

const
    MAX_VALUE_AXIS_Y = 1E10;
    MIN_VALUE_AXIS_Y = -1E10;

type
  TFunctionTime = reference to function(t: double): double;
  TOnRedraw = procedure of object;

  PDataCol = ^TDataCol;
  TDataCol = record
      x: double;
      rows: TList;
  end;

  TDataSource = class (TObject)
      Data: PDataCol;
      Redraw: TOnRedraw;
      //PX: Pointer;
      currentX: double;
      cols: TList;
      minY, maxY: double;
      procedure add(x, y: double; serie: TObject);
      procedure addX(x: Double);
      procedure addY(y: double; serie: TObject);
      constructor Create;
      procedure deleteSerie(serie: TObject);
      procedure deleteAllSeries;
      procedure Delete(Q: PDataCol);
      procedure DeleteAll;
      destructor  Destroy; override;

      function isOut(wPlane: double): Boolean;
      procedure getNewLimits(var minX, maxX: double);
      procedure removeFirst;
      procedure init;
      function isEmpty: Boolean;
      procedure removeAll;
      procedure reset;
      procedure getYMinAndYMax;
      procedure updateYMinAndYMax(Q: Pointer);

  end;

  TDataSerie = class (TObject)

      functionTime: TFunctionTime;
      dataSource: TDataSource;
      color: TAlphaColor;
      lineWidth: real;
      LPath: ISkPathBuilder;
      name: String;
      FVisible: Boolean;
      count: Integer;
      procedure add(x, y: double);
      procedure addX(x: double);
      procedure addY(y: double);
      constructor Create;
      destructor  Destroy; override;
      procedure init;
      procedure SetVisible(val: Boolean);
      property visible: Boolean read FVisible write SetVisible;
  end;

  PData = ^TData;
  TData = record
        y: double;
    serie: TDataSerie;
  end;

implementation

uses
  UGlobalData;


procedure TDataSource.addX(x: Double);
begin
  if x > currentX then
    begin
      New(Data);
      Data^.x := x;
      Data^.rows := TList.Create;
      cols.Add(Data);
      currentX := x;
    end;
end;

procedure TDataSource.addY(y: double; serie: TObject);
var
  D: PData;
begin
  New(D);
  D^.y := y;
  D^.serie := serie as TDataSerie;
  D^.serie.count := D^.serie.count + 1;
  updateYMinAndYMax(D);
  Data^.rows.Add(D);
end;

procedure TDataSource.add(x, y: double; serie: TObject);
begin
  addX(x);
  addY(y, serie);
end;

procedure TDataSource.removeFirst;
var
  P: PDataCol;
begin
  P := cols[0];
  Delete(P);
  cols.Delete(0);
end;

procedure TDataSource.updateYMinAndYMax(Q: Pointer);
var
  D: PData;
begin
  D := Q;
  if D^.serie.visible then
    begin
      if D^.y < minY then minY := D^.y;
      if D^.y > maxY then maxY := D^.y;
    end;
end;

procedure TDataSource.getYMinAndYMax;
var
  i, j: Integer;
  col: PDataCol;
begin
  minY := MAX_VALUE_AXIS_Y;
  maxY := MIN_VALUE_AXIS_Y;

  for i := 0 to cols.Count - 1 do
    begin
      col := cols[i];
      for j := 0 to col^.rows.Count - 1 do updateYMinAndYMax(col^.rows[j]);
    end;

end;

function TDataSource.isEmpty: Boolean;
var
  i: Integer;
  col: PDataCol;
begin
  for i := cols.Count - 1 downto 0 do
    begin
      col := cols[i];
      if col^.rows.Count > 0 then
        begin
          Result := false;
          Exit;
        end;
    end;
  Result := true;
end;

procedure TDataSource.deleteAllSeries;
var
  i, j: Integer;
  col: PDataCol;
  row: PData;
begin
  for i := cols.Count - 1 downto 0 do
    begin
      col := cols[i];
      for j := col^.rows.Count - 1 downto 0 do
        begin
          row := col^.rows[j];
          Dispose(row);
          col^.rows.Delete(j);
        end;
    end;
  init;
end;

procedure TDataSource.deleteSerie(serie: TObject);
var
  i, j: Integer;
  col: PDataCol;
  row: PData;
begin
  for i := cols.Count - 1 downto 0 do
    begin
      col := cols[i];
      for j := col^.rows.Count - 1 downto 0 do
        begin
          row := col^.rows[j];
          if serie = row^.serie then
            begin
              Dispose(row);
              col^.rows.Delete(j);
            end;
        end;

    end;
    if isEmpty then init;
end;

procedure TDataSource.Delete(Q: PDataCol);
var
  j: Integer;
  yL: TList;
  D: PData;
begin
  yL := Q^.rows;
  for j := 0 to yL.Count - 1 do
    begin
      D := yL[j];
      D^.serie.count := D^.serie.count - 1;
      Dispose(D);
    end;
  yL.Free;
  Dispose(Q);
end;

procedure TDataSource.DeleteAll;
begin
  removeAll;
  cols.Free;
end;

procedure TDataSource.removeAll;
var
  i: Integer;
  P: PDataCol;
begin
  for i := cols.Count - 1 downto 0 do
    begin
      P := cols[i];
      Delete(P);
      cols.Delete(i);
    end;
end;

procedure TDataSource.reset;
begin
   removeAll;
   Init;
end;

constructor TDataSource.Create;
begin
  cols:= TList.Create;
  Init;
end;

procedure TDataSource.Init;
begin
   //x := Double.MinValue;
   //PX := Pointer(x);
   //PX := @x;
   minY := MAX_VALUE_AXIS_Y;
   maxY := MIN_VALUE_AXIS_Y;
   currentX := -1;
end;

procedure TDataSource.getNewLimits(var minX, maxX: double);
var
  P: PDataCol;
begin
  P := cols[0];
  minX := P^.x;
  P := cols[cols.Count - 1];
  maxX := P^.x;
end;

function TDataSource.isOut(wPlane: double): Boolean;
var
   P, Q: PDataCol;
begin
   if cols.Count >= 2 then
       begin
          P := cols[0];
          Q := cols[cols.Count - 1];
          if Q^.x - P^.x > wPlane then
            begin
              Result := true;
              Exit;
            end;
       end;
   Result:= false;
end;

destructor TDataSource.Destroy;
begin
   DeleteAll;
end;

constructor TDataSerie.Create;
begin
  inherited;
  color := TConst.DEFAULT_COLOR_SERIE;
  lineWidth:= TConst.DEFAULT_LINEWIDTH_SERIE;
  Fvisible := true;
  init;
end;

procedure TDataSerie.init;
begin
  count := 0;
  LPath := nil;
end;

destructor  TDataSerie.Destroy;
begin
  LPath := nil;
end;

procedure TDataSerie.addX(x: double);
begin
  dataSource.addX(x);
end;

procedure TDataSerie.addY(y: double);
begin
  dataSource.addY(y, self);
end;

procedure TDataSerie.add(x, y: double);
begin
  dataSource.add(x, y, self);
end;

procedure TDataSerie.SetVisible(val: Boolean);
begin
  if val <> FVisible then
    begin
      FVisible := val;
      dataSource.getYMinAndYMax;
      if Assigned(dataSource.Redraw) then dataSource.Redraw;
    end;
end;


end.

