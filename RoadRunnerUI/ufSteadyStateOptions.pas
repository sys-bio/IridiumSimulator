unit ufSteadyStateOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Memo, FMX.Layouts, FMX.ListBox, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Controls.Presentation;

type
  TfrmSteadyStateOptions = class(TForm)
    Layout1: TLayout;
    btnClose: TButton;
    lbl2: TLabel;
    lblSolverHint: TLabel;
    lstParameterNames: TListBox;
    lstSolverNames: TListBox;
    mmoSolverDescription: TMemo;
    mmoParameterDescription: TMemo;
    lbl1: TLabel;
    lbl3: TLabel;
    edtParameterName: TEdit;
    lblParameterNameToChange: TLabel;
    btn1: TButton;
    procedure lstSolverNamesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lstParameterNamesChange(Sender: TObject);
    procedure edtParameterNameExit(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    disableChange : Boolean;
    procedure displaySelectedSolver;
  end;

var
  frmSteadyStateOptions: TfrmSteadyStateOptions;

implementation

{$R *.fmx}

Uses uRoadRunner.API;

function IsInteger(S: String): Boolean;
begin
  try
    Result := True;
    StrToInt(S);
  except on E: EConvertError do
    Result := False;
  end;
end;


function IsFloat(S: String): Boolean;
begin
  try
    Result := True;
    StrToFloat(S);
  except on E: EConvertError do
    Result := False;
  end;
end;


procedure TfrmSteadyStateOptions.displaySelectedSolver;
var sl : TStringList; str : string;
begin
  uRoadRunner.API.setSteadyStateSolver (lstSolverNames.Items[lstSolverNames.ItemIndex]);
  mmoSolverDescription.Text := uRoadRunner.API.getSteadyStateSolverDescription;
  lblSolverHint.Text := uRoadRunner.API.getSteadyStateSolverHint;

  sl := uRoadRunner.API.getListOfSteadyStateSolverParameterNames;
  try
    lstParameterNames.Items.Assign (sl);
  finally
    sl.Free;
  end;
  mmoParameterDescription.Text := '';
end;


procedure TfrmSteadyStateOptions.btn1Click(Sender: TObject);
var i : Integer;
begin
  for i := 0 to 10 do
      lstSolverNames.Items.Add (uRoadRunner.API.solverTypeToString(i));
end;

procedure TfrmSteadyStateOptions.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSteadyStateOptions.edtParameterNameExit(Sender: TObject);
var str : string;
    t1, x : integer;
    h : Int64;
begin
  if disableChange then
     exit;

  str := lstParameterNames.Items[lstParameterNames.ItemIndex];
  t1 := uRoadRunner.API.getSteadyStateSolverParameterType (str);
  case t1 of
    0 :  uRoadRunner.API.setSteadyStateSolverParameterString (str, edtParameterName.Text); // String
    1 :  begin
         if (upperCase (edtParameterName.Text) = 'FALSE') or
            (upperCase (edtParameterName.Text) = 'TRUE')
            then
              uRoadRunner.API.setSteadyStateSolverParameterBoolean (str, strToBool (edtParameterName.Text)) // Bool
         else
            raise Exception.Create('Boolean value must be true or false');
         end;
    2 :  begin // int32
         if IsInteger (edtParameterName.Text) then
            uRoadRunner.API.setSteadyStateSolverParameterInt (str, strtoint (edtParameterName.Text))
         else
            raise Exception.Create('Parameter value must be an integer value');
         end;
    3 :  begin // uint32
         h := StrToInt64 (edtParameterName.Text);
         uRoadRunner.API.setSteadyStateSolverParameterUInt (str, h)
         end;

    7 :  begin // double
         if IsFloat (edtParameterName.Text) then
            uRoadRunner.API.setSteadyStateSolverParameterDouble (str, strtofloat (edtParameterName.Text))
         else
            raise Exception.Create('Parameter value must be an floating point value');
         end;
  else
     raise Exception.Create('Unknown parameter type: ' + inttostr (t1));

  end;
end;

procedure TfrmSteadyStateOptions.FormCreate(Sender: TObject);
begin
  disableChange := false;
end;

procedure TfrmSteadyStateOptions.lstParameterNamesChange(Sender: TObject);
var str : string;
    t1 : integer;
begin
  if disableChange then
     exit;

  str := lstParameterNames.Items[lstParameterNames.ItemIndex];
  mmoParameterDescription.Text := uRoadRunner.API.getSteadyStateSolverParameterDescription(str);

  lblParameterNameToChange.Text := str + ':';
  t1 := uRoadRunner.API.getSteadyStateSolverParameterType (str);
  case t1 of
    0 : edtParameterName.Text := uRoadRunner.API.getSteadyStateSolverParameterString (str);  // string
    1 : edtParameterName.Text := BoolToStr(uRoadRunner.API.getSteadyStateSolverParameterBoolean (str), True);  // Bool
    2 : edtParameterName.Text := IntToStr(uRoadRunner.API.getSteadyStateSolverParameterInt (str));  // int32
    3 : edtParameterName.Text := UIntToStr(uRoadRunner.API.getSteadyStateSolverParameterUInt (str));  // uint32
    7 : edtParameterName.Text := FloatToStr(uRoadRunner.API.getSteadyStateSolverParameterDouble (str));  // double
  else
     raise Exception.Create('Unknown parameter type: ' + inttostr (t1));
  end;
end;

procedure TfrmSteadyStateOptions.lstSolverNamesChange(Sender: TObject);
begin
  if disableChange then
     exit;
  displaySelectedSolver
end;

end.
