unit ufTextViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Platform, Rtti;

type
  TfrmTextViewer = class(TForm)
    Layout1: TLayout;
    btnClose: TButton;
    btnCopyToClipboard: TButton;
    textmemo: TMemo;
    StyleBook1: TStyleBook;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTextViewer: TfrmTextViewer;

implementation

{$R *.fmx}

procedure TfrmTextViewer.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTextViewer.btnCopyToClipboardClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
  Value: TValue;
  //itmap: TBitmap;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    begin
    Value := Svc.GetClipboard;
    value := textmemo.text;
    Svc.SetClipboard(Value);
    end;
end;

end.
