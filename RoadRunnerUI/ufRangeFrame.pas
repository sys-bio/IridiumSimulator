unit ufRangeFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation;

type
  TfrmRangeFrame = class(TFrame)
    lblLower: TLabel;
    edtLower: TEdit;
    lblUpper: TLabel;
    edtUpper: TEdit;
    lblParameterName: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
    sliderIndex: integer;
    procedure hide;
    procedure show;
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation

{$R *.fmx}


constructor TfrmRangeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  sliderIndex := -1;
end;


procedure TfrmRangeFrame.hide;
begin
  lblLower.visible := False;
  edtLower.visible := False;
  lblUpper.visible := False;
  edtUpper.visible := False;
  lblParameterName.visible := False;
end;


procedure TfrmRangeFrame.show;
begin
  lblLower.visible := True;
  edtLower.visible := True;
  lblUpper.visible := True;
  edtUpper.visible := True;
  lblParameterName.visible := True;
end;


end.
