unit ufAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, Skia.FMX, Skia;

type
  TfrmAbout = class(TForm)
    Image1: TImage;
    Layout1: TLayout;
    btnClose: TButton;
    StyleBook1: TStyleBook;
    Line1: TLine;
    lblRoadRunner: TLabel;
    Image2: TImage;
    lbllibSBML: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Line2: TLine;
    lbSkia: TLabel;
    lbVersion: TSkLabel;
    lblWho: TLabel;
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.fmx}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
