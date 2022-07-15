unit ufPreferences;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.TabControl,
  FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.SpinBox,
  uConfiguration;

type
  TfrmPreferences = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    btnClose: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    sbFontSize: TSpinBox;
    btnSavePreferences: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure sbFontSizeChange(Sender: TObject);
    procedure btnSavePreferencesClick(Sender: TObject);
  private
    { Private declarations }
    fireEvent : boolean;
  public
    { Public declarations }
    //stylebook1 : TStyleBook;
  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.fmx}

procedure TfrmPreferences.btnCloseClick(Sender: TObject);
begin
  uConfiguration.configOpts.fontSize := trunc (sbFontSize.Value);

  Close;
end;

procedure TfrmPreferences.btnSavePreferencesClick(Sender: TObject);
begin
  saveConfigurationFile (CONFIG_FILE_NAME);
end;

procedure TfrmPreferences.FormCreate(Sender: TObject);
begin
  //stylebook := stylebook1;
  fireEvent := False;
  sbFontSize.Value := uConfiguration.configOpts.fontSize;
  fireEvent := True;
end;

procedure TfrmPreferences.sbFontSizeChange(Sender: TObject);
begin
  if fireEvent then
     uConfiguration.configOpts.fontSize := trunc (sbFontSize.Value);
end;

end.
