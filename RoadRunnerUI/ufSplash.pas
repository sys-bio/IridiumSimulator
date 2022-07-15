unit ufSplash;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Skia, Skia.FMX, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TfrmSplash = class(TForm)
    SkLabel1: TSkLabel;
    StyleBook1: TStyleBook;
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
