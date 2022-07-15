unit ufIntegratorOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Memo, FMX.Edit, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Controls.Presentation;

type
  TfrmIntegratorOptions = class(TForm)
    Layout1: TLayout;
    btnClose: TButton;
    lbl2: TLabel;
    lblIntegratorHint: TLabel;
    lstParameterNames: TListBox;
    lstSolverNames: TListBox;
    mmoIntegratorDescription: TMemo;
    mmoParameterDescription: TMemo;
    lbl1: TLabel;
    lbl3: TLabel;
    edtParameterName: TEdit;
    lblParameterNameToChange: TLabel;
    procedure lstParameterNamesChange(Sender: TObject);
    procedure lstSolverNamesChange(Sender: TObject);
    procedure edtParameterNameExit(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    disableChange : Boolean;
    procedure displaySelectedSolver;
  end;

var
  frmIntegratorOptions: TfrmIntegratorOptions;

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


procedure TfrmIntegratorOptions.displaySelectedSolver;
var sl : TStringList;
begin
  uRoadRunner.API.setIntegrator (lstSolverNames.Items[lstSolverNames.ItemIndex]);
  mmoIntegratorDescription.Text := uRoadRunner.API.getIntegratorDescription();
  lblIntegratorHint.Text := uRoadRunner.API.getIntegratorHint();

  sl := uRoadRunner.API.getListOfIntegratorParameterNames;
  try
    lstParameterNames.Items.Assign (sl);
  finally
    sl.Free;
  end;
  mmoParameterDescription.Text := '';
end;

procedure TfrmIntegratorOptions.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmIntegratorOptions.edtParameterNameExit(Sender: TObject);
var str : string;
    t1, x : integer;
    h : Int64;
begin
  if disableChange then
     exit;

  str := lstParameterNames.Items[lstParameterNames.ItemIndex];
  t1 := uRoadRunner.API.getIntegratorParameterType (str);
  case t1 of
    0 :  uRoadRunner.API.setIntegratorParameterString (str, edtParameterName.Text); // String
    1 :  begin
         if (upperCase (edtParameterName.Text) = 'FALSE') or
            (upperCase (edtParameterName.Text) = 'TRUE')
            then
              uRoadRunner.API.setIntegratorParameterBoolean (str, strToBool (edtParameterName.Text)) // Bool
         else
            raise Exception.Create('Boolean value must be true or false');
         end;
    2 :  begin // int32
         if IsInteger (edtParameterName.Text) then
            uRoadRunner.API.setIntegratorParameterInt (str, strtoint (edtParameterName.Text))
         else
            raise Exception.Create('Parameter value must be an integer value');
         end;
    3 : begin // uint32
        h := StrToInt64 (edtParameterName.Text);
        uRoadRunner.API.setIntegratorParameterUInt (str, h)
        end;

    7 :  begin // double
         if IsFloat (edtParameterName.Text) then
            uRoadRunner.API.setIntegratorParameterDouble (str, strtofloat (edtParameterName.Text))
         else
            raise Exception.Create('Parameter value must be an floating point value');
         end;
  else
     raise Exception.Create('Unknown parameter type: ' + inttostr (t1));

  end;
end;

procedure TfrmIntegratorOptions.FormCreate(Sender: TObject);
begin
  disableChange := false;
end;

procedure TfrmIntegratorOptions.lstParameterNamesChange(Sender: TObject);
var str : string;
    t1 : integer;
begin
  if disableChange then
     exit;

  str := lstParameterNames.Items[lstParameterNames.ItemIndex];
  mmoParameterDescription.Text := uRoadRunner.API.getIntegratorParameterDescription(str);

  lblParameterNameToChange.Text := str + ':';
  t1 := uRoadRunner.API.getIntegratorParameterType (str);
  case t1 of
    0 : edtParameterName.Text := uRoadRunner.API.getIntegratorParameterString (str);  // string
    1 : edtParameterName.Text := BoolToStr(uRoadRunner.API.getIntegratorParameterBoolean (str), True);  // Bool
    2 : edtParameterName.Text := IntToStr(uRoadRunner.API.getIntegratorParameterInt (str));  // int32
    3 : edtParameterName.Text := UIntToStr(uRoadRunner.API.getIntegratorParameterUInt (str));  // uint32
    7 : edtParameterName.Text := FloatToStr(uRoadRunner.API.getIntegratorParameterDouble (str));  // double
  else
     raise Exception.Create('Unknown parameter type: ' + inttostr (t1));
  end;
end;

procedure TfrmIntegratorOptions.lstSolverNamesChange(Sender: TObject);
begin
  if disableChange then
     exit;

  displaySelectedSolver;
end;

end.
