unit uSBWList;

// Developer representation of a SBW List data type

interface

Uses SysUtils, Classes, uSBWCommon, uSBWComplex, uSBWArray;

type
  TSBWList = class;

  TSBWListItem = class (TObject)
            public
                DataType : Byte;
                b : Byte;
                bool : boolean;
                i : integer;
                d : double;
                c : TSBWComplex;
                str : AnsiString;
                Lt : TSBWList;
                Ar : TSBWArray;
                function getBoolean : Boolean;
                function getInteger : integer;
                function getDouble : double;
                function getScalar : double;
                function getString : AnsiString;
                function getComplex : TSBWComplex;

                function    getAsString : AnsiString;
                constructor Create (b : byte); overload;
                constructor Create (i : integer); overload;
                constructor Create (b : boolean); overload;
                constructor Create (d : double);  overload;
                constructor Create (c : TSBWComplex);  overload;
                constructor Create (str : AnsiString); overload;
                constructor Create (Lt : TSBWList); overload;
                constructor Create (Ar : TSBWArray); overload;

                destructor  Destroy; override;
  end;


  TSBWList = class (TList)
             protected
               function Get (Index : integer) : TSBWListItem;
               procedure Put (Index : integer; Item : TSBWListItem);
             public
               destructor Destroy; override;
               function  Add (Item : TSBWListItem) : integer;
               procedure Delete (Index : Integer);
               function  getDataStream : TObject;
               procedure copy (src : TSBWList);
               property  Items[Index : integer] : TSBWListItem read Get write Put; default;
  end;

implementation

Uses uSBWUtils;

constructor TSBWListItem.Create (i : integer);
begin
  inherited Create;
  DataType := dtInteger;
  Self.i := i;
end;


constructor TSBWListItem.Create (b : byte);
begin
  inherited Create;
  DataType := dtByte;
  Self.b := b;
end;



constructor TSBWListItem.Create (b : boolean);
begin
  inherited Create;
  DataType := dtBoolean;
  Self.bool := b;
end;


constructor TSBWListItem.Create (d : double);
begin
  inherited Create;
  DataType := dtDouble;
  Self.d := d;
end;


constructor TSBWListItem.Create (c : TSBWComplex);
begin
  inherited Create;
  DataType := dtComplex;
  Self.c := c;
end;


constructor TSBWListItem.Create (str : AnsiString);
begin
  inherited Create;
  DataType := dtString;
  Self.str := str;
end;


constructor TSBWListItem.Create (Lt : TSBWList);
begin
  inherited Create;
  DataType := dtList;
  Self.Lt := Lt;
end;


constructor TSBWListItem.Create (Ar : TSBWArray);
begin
  inherited Create;
  DataType := dtArray;
  Self.Ar := Ar;
end;


destructor TSBWListItem.Destroy;
begin
  case DataType of
     dtList : Lt.Free;
     dtArray : Ar.Free;
  end;
  inherited Destroy;
end;


// --------------------------------------------------------------------------

function TSBWListItem.getBoolean : Boolean;
begin
  if DataType <> dtBoolean then
     raise ESBWException.Create ('Boolean expected in List item');
  result := bool;
end;


function TSBWListItem.getInteger : integer;
begin
  if DataType <> dtInteger then
     raise ESBWException.Create ('Integer expected in List item');
  result := i;
end;


function TSBWListItem.getDouble : Double;
begin
  if DataType <> dtDouble then
     raise ESBWException.Create ('Double expected in List item');
  result := d;
end;


function TSBWListItem.getScalar : double;
begin
  case DataType of
    dtInteger : result := i;
    dtDouble  : result := d;
  else
     raise ESBWException.Create ('expecting double or integer in list item');
  end;
end;


function TSBWListItem.getComplex : TSBWComplex;
begin
  if DataType <> dtComplex then
     raise ESBWException.Create ('Complex expected in List item');
  result := c;
end;


function TSBWListItem.getString : AnsiString;
begin
  if DataType <> dtString then
     raise ESBWException.Create ('String expected in List item');
  result := str;
end;


function TSBWListItem.getAsString : AnsiString;
begin
  case DataType of
      dtInteger :
         result := inttostr (i);
      dtBoolean :
         if bool then
            result := 'True'
         else result := 'False';
      dtDouble :
         result := FloatToStr (d);
      dtComplex :
         result := complexToStr(c);
      dtString :
         result := str;
  else
     raise ESBWException.Create ('Unknown Datatype in GetAsString');
  end;
end;


// ----------------------------------------------------------------------


function TSBWList.Get (Index : integer) : TSBWListItem;
begin
  result := TSBWListItem(inherited Get(index));
end;


procedure TSBWList.Put (Index : integer; Item : TSBWListItem);
begin
  inherited Put (Index, Item);
end;


function TSBWList.Add (Item : TSBWListItem) : integer;
begin
  result := inherited Add (Item);
end;


procedure TSBWList.Delete (Index : Integer);
begin
  Items[Index].Free;
  Items[Index] := nil;
  inherited Delete (Index);
end;


destructor TSBWList.Destroy;
var i : integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Free;
  inherited Destroy;
end;


function TSBWList.getDataStream : TObject;
begin
  result := TSBWDataStream.Create;
  (result as TSBWDataStream).BuildFromDataItemList (Self);
end;


procedure TSBWList.copy (src: TSBWList);
var i : integer;
begin
  self.Clear;
  for i := 0 to src.count - 1 do
      begin
      case src[i].DataType of
          dtInteger : self.add (TSBWListItem.Create(src[i].i));
          dtDouble  : self.add (TSBWListItem.Create(src[i].d));
          dtComplex : self.add (TSBWListItem.Create(src[i].c));
          dtBoolean : self.add (TSBWListItem.Create(src[i].b));
          dtString  : self.add (TSBWListItem.Create(src[i].str));
          dtArray   : self.add (TSBWListItem.Create(src[i].Ar));
          dtList    : self.add (TSBWListItem.Create(src[i].Lt));
      else
          raise ESBWException.Create ('Unknown data type while copying SBWList');
      end;
      end;
end;


// ----------------------------------------------------------------------


end.
