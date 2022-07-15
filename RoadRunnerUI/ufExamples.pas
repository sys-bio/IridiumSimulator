unit ufExamples;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects;

type
  TfrmExamples = class(TForm)
    Layout1: TLayout;
    btnClose: TButton;
    ltExamples: TListBox;
    Label1: TLabel;
    Rectangle1: TRectangle;
    procedure btnCloseClick(Sender: TObject);
    procedure ltExamplesItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExamples: TfrmExamples;

implementation

{$R *.fmx}

Uses ufMain;

procedure TfrmExamples.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmExamples.ltExamplesItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  frmMain.OnPickExample(ltExamples.ItemIndex);
end;

end.
