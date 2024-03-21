unit ufSplash;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Skia, FMX.skia, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TfrmSplash = class(TForm)
    StyleBook1: TStyleBook;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.fmx}

end.
