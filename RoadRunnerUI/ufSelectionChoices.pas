unit ufSelectionChoices;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.TreeView, FMX.Objects, FMX.Controls.Presentation;

type
  TfrmSelectionChoices = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    btnClose: TButton;
    Rectangle1: TRectangle;
    Layout3: TLayout;
    Layout5: TLayout;
    treeSelection: TTreeView;
    Layout6: TLayout;
    lstSelectedItems: TListBox;
    Layout7: TLayout;
    Layout4: TLayout;
    btnAdd: TButton;
    btnRemove: TButton;
    btnClear: TButton;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure treeSelectionClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure lstSelectedItemsDblClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSelectionChoices: TfrmSelectionChoices;

implementation

{$R *.fmx}

procedure TfrmSelectionChoices.btnClearClick(Sender: TObject);
begin
  lstSelectedItems.Clear;
end;

procedure TfrmSelectionChoices.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSelectionChoices.btnRemoveClick(Sender: TObject);
begin
  if lstSelectedItems.ItemIndex <> -1 then
     lstSelectedItems.Items.Delete(lstSelectedItems.ItemIndex);
end;

procedure TfrmSelectionChoices.lstSelectedItemsDblClick(Sender: TObject);
begin
  lstSelectedItems.Items.Delete(lstSelectedItems.ItemIndex);
end;

procedure TfrmSelectionChoices.treeSelectionClick(Sender: TObject);
var i : integer;
begin
  if lowerCase (treeSelection.Selected.Text) = 'time' then
     begin
     lstSelectedItems.Items.Add (treeSelection.Selected.Text);
     exit;
     end;

  if treeSelection.Selected.Level = 1 then
     begin
     for i := 0 to treeSelection.Selected.Count - 1 do
        lstSelectedItems.Items.Add (treeSelection.Selected.Items[i].Text);
     exit;
     end;
  lstSelectedItems.Items.Add (treeSelection.Selected.Text);
end;

end.
