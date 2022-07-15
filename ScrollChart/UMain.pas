unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, Skia, Skia.FMX, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, UScrollingChart,
  FMX.Controls.Presentation, FMX.StdCtrls, System.UIConsts, UDataSource, uGlobalData,
  FMX.Edit, FMX.Objects, FMX.Colors, FMX.Layouts, FMX.ListBox, FMX.EditBox,
  FMX.SpinBox, FMX.NumberBox, FMX.DialogService;

type
  TMainForm = class(TForm)
    StyleBook1: TStyleBook;
    topPanel: TPanel;
    ClientPanel: TPanel;
    leftPanel: TPanel;
    RightPanel: TPanel;
    MyTimer: TTimer;
    GroupBox1: TGroupBox;
    EditTitle: TEdit;
    Button9: TButton;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    ColorComboBox1: TColorComboBox;
    MiddlePanel: TPanel;
    bottomPanel: TPanel;
    chartPanel: TPanel;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label2: TLabel;
    ColorComboBox2: TColorComboBox;
    Label3: TLabel;
    ColorComboBox3: TColorComboBox;
    ComboBox1: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    SpinBox1: TSpinBox;
    SpinBox2: TSpinBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ColorComboBox4: TColorComboBox;
    Label9: TLabel;
    SpinBox3: TSpinBox;
    Edit1: TEdit;
    Label10: TLabel;
    Button10: TButton;
    Label11: TLabel;
    Edit2: TEdit;
    Button11: TButton;
    ListBox1: TListBox;
    Button4: TButton;
    Button5: TButton;
    GroupBox6: TGroupBox;
    Button3: TButton;
    Label12: TLabel;
    ColorComboBox5: TColorComboBox;
    Label13: TLabel;
    SpinBox4: TSpinBox;
    GroupBox7: TGroupBox;
    Button6: TButton;
    Button8: TButton;
    GroupBox8: TGroupBox;
    Label14: TLabel;
    ColorComboBox6: TColorComboBox;
    Label15: TLabel;
    ColorComboBox7: TColorComboBox;
    Label16: TLabel;
    SpinBox5: TSpinBox;
    Panel1: TPanel;
    GroupBox9: TGroupBox;
    SkLabel1: TSkLabel;
    Button7: TButton;
    Button2: TButton;
    Button1: TButton;
    GroupBox10: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label19: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    ColorComboBox8: TColorComboBox;
    SaveTXT: TSaveDialog;
    SavePDF: TSaveDialog;
    chart: TScrollingChart;
    CheckBox3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MyTimerTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure EditTitleTyping(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ColorComboBox1Change(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure GroupBox9Resize(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure ColorComboBox2Change(Sender: TObject);
    procedure ColorComboBox3Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure SpinBox2Change(Sender: TObject);
    procedure ColorComboBox4Change(Sender: TObject);
    procedure SpinBox3Change(Sender: TObject);
    procedure Edit1Typing(Sender: TObject);
    procedure Edit2Typing(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure ColorComboBox8Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure ListBox1ChangeCheck(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure ColorComboBox5Change(Sender: TObject);
    procedure SpinBox4Change(Sender: TObject);
    procedure ColorComboBox6Change(Sender: TObject);
    procedure ColorComboBox7Change(Sender: TObject);
    procedure SpinBox5Change(Sender: TObject);
    procedure chartMouseWorldMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure CheckBox3Change(Sender: TObject);
  private
    { Private declarations }
  public
    time, dt: double;
    var FList: Array[0..3] of TFunctionTime;
    procedure initArrayOfSeries;
    procedure initComponents;
    procedure initListBox;
    procedure nothingSelect;
    procedure fillAddSerieForm;
    function addSerie(name: String; index: Integer; cl: TAlphaColor; lw: Single): TDataSerie;
    procedure resetEvents;
  end;

var
   MainForm: TMainForm;




implementation
uses
  Math, UAddSerie;

{$R *.fmx}

function func1(t: double): double;
var
  A, s, c, Tetha: Double;
begin
  A := 10;
  Tetha := (2*PI*60*t)/360;
  SinCos(Tetha, s, c);
  Result := A*s;
end;

function func2(t: double): double;
var
  A, s, c, Tetha: Double;
begin
  A := 2;
  Tetha := (2*PI*120*t)/360;
  SinCos(Tetha, s, c);
  Result := A*s;
end;

function sen(x: double): double;
var
  c: double;
begin
  SinCos(x, Result, c);
end;

function func3(t: double): double;
var
  A0, A1, A2, A3: Double;
  alpha, f: double;
begin
  f := 60;
  A0 := 3;
  A1 := A0/2;
  A2 := A0/3;
  A3 := A0/4;
  alpha := PI/4;
  Result := A0*sen(2*PI*f*t/360 + alpha) +
            A1*sen(2*PI*2*f*t/360 + alpha) +
            A2*sen(2*PI*3*f*t/360 + alpha) +
            A3*sen(2*PI*4*f*t/360 + alpha);

end;


function func4(t: double): double;
var
  A, s, c, Tetha: Double;
begin
  A := 8;
  Tetha := (2*PI*120*t)/360;
  SinCos(Tetha, s, c);
  Result := A*c;
end;

procedure TMainForm.Button10Click(Sender: TObject);
begin
  if Edit1.text <> '' then
    begin
      chart.xAxis.caption := Edit1.Text;
      Button10.Enabled := false;
    end
  else
    begin
      Edit1.Text := chart.xAxis.caption;
    end;
end;

procedure TMainForm.Button11Click(Sender: TObject);
begin
  if Edit2.text <> '' then
    begin
      chart.yAxis.caption := Edit2.Text;
      Button11.Enabled := false;
    end
  else
    begin
      Edit2.Text := chart.yAxis.caption;
    end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if Button1.Text = 'Pause' then
    begin
      Button1.Text := 'Resume';
      chart.pause;
    end
  else
    begin
      Button1.Text := 'Pause';
      chart.resume;
    end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  status: Boolean;
begin
  status := MyTimer.Enabled;
  MyTimer.Enabled := false;
  time := 0;
  chart.restart;
  MyTimer.Enabled := status;
end;

procedure TMainForm.fillAddSerieForm;
var
  I: Integer;
begin
  AddSerieForm.Edit1.Text := '';
  AddSerieForm.ColorComboBox1.Color := TConst.DEFAULT_COLOR_SERIE;
  AddSerieForm.SpinBox1.Value :=  TConst.DEFAULT_LINEWIDTH_SERIE;
  AddSerieForm.ComboBox1.Items.Clear;
  AddSerieForm.Button2.Enabled := false;
  for I := Low(FList) to High(FList) do
    AddSerieForm.ComboBox1.Items.Add('func' + IntToStr(I + 1));
end;

procedure TMainForm.Button4Click(Sender: TObject);
var
  serie: TDataSerie;
begin
  fillAddSerieForm;
  if AddSerieForm.showModal = mrOk then
    begin
      serie := AddSerie(AddSerieForm.Edit1.Text, AddSerieForm.ComboBox1.ItemIndex, AddSerieForm.ColorComboBox1.Color, AddSerieForm.SpinBox1.Value);
      if serie <> nil then
        begin
          resetEvents;
          ListBox1.Items.Add(serie.name);
          ListBox1.ListItems[ListBox1.Items.IndexOf(serie.name)].IsChecked := serie.visible;
        end;

    end;
end;

procedure TMainForm.resetEvents;
begin
  if not Assigned(ListBox1.OnChange) then
    begin
      ListBox1.OnChange := ListBox1Change;
      ColorComboBox5.OnChange := ColorComboBox5Change;
      SpinBox4.OnChange := SpinBox4Change;
      Button3.Enabled := true;
      ColorComboBox5.Enabled := true;
      SpinBox4.Enabled := true;
      Button5.Enabled := true;
    end;
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  if MessageDlg('Are you sure to delete all series?',
        TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], 0) = mrOk then
    begin
      ListBox1.OnChange := nil;
      ColorComboBox5.OnChange := nil;
      SpinBox4.OnChange := nil;
      Button3.Enabled := false;
      ListBox1.Items.Clear;
      chart.deleteSeries;
      nothingSelect;
    end;
end;

procedure TMainForm.Button6Click(Sender: TObject);
begin
  Button6.Enabled := false;
  if SaveTXT.Execute then chart.saveToFile(SaveTXT.FileName);
  Button6.Enabled := true;
end;

procedure TMainForm.Button8Click(Sender: TObject);
begin
  Button8.Enabled := false;
  if SavePDF.Execute then chart.saveToPDF(SavePDF.FileName);
  Button8.Enabled := true;
end;

procedure TMainForm.Button7Click(Sender: TObject);
begin
  if Assigned(chart.OnMouseWorldMove) then
    begin
      chart.OnMouseWorldMove := nil;
      Button7.Text := 'Enabled';
    end
  else
    begin
      chart.OnMouseWorldMove := chartMouseWorldMove;
      Button7.Text := 'Disabled';
    end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;

  InitComponents;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  FPS: Integer;
begin
   FPS := 60;  // Frames Per Sec
   chart.autoScaleDown := true;
   chart.setXAxisRange(0, 20);
   chart.setYAxisRange(-10, 10);
   time := 0;
   dt := chart.deltaX;
   initArrayOfSeries;
   MyTimer.Interval := Round(1000/FPS);
   MyTimer.Enabled := true;
end;

procedure TMainForm.GroupBox9Resize(Sender: TObject);
begin
  Button7.Position.X := GroupBox9.Width - 93;
end;

function TMainForm.AddSerie(name: String; index: Integer; cl: TAlphaColor; lw: Single): TDataSerie;
var
  serie: TDataSerie;
begin
  if name <> '' then
    serie := chart.addSerie(FList[index], name)
  else
    serie := chart.addSerie(FList[index]);
  if serie <> nil then
    begin
      serie.color := cl;
      serie.lineWidth := lw;
    end;
  Result := serie;
end;

procedure TMainForm.initArrayOfSeries;
begin
  FList[0] := func1;
  FList[1] := func2;
  FList[2] := func3;
  FList[3] := func4;

  AddSerie('', 0, claRed, 1.5);
  AddSerie('', 1, claGreen, 2);
end;

procedure TMainForm.MyTimerTimer(Sender: TObject);
begin
   chart.run(time);
   time := time + dt;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  Button2.Position.X := (Panel1.Width - (Button2.Width + 30 + Button1.Width))/2;
  Button1.Position.X := Button2.Position.X + Button2.Width + 30;
end;

procedure TMainForm.chartMouseWorldMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  SkLabel1.Text := Format('%s: %f, %s: %f', [chart.xAxis.caption, X, chart.yAxis.caption, Y]);
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  chart.autoScaleUp := CheckBox1.IsChecked;
end;

procedure TMainForm.CheckBox2Change(Sender: TObject);
begin
  chart.autoScaleDown := CheckBox2.IsChecked;
end;

procedure TMainForm.CheckBox3Change(Sender: TObject);
begin
  chart.legend.visible := CheckBox3.IsChecked;
end;

procedure TMainForm.ColorComboBox1Change(Sender: TObject);
begin
  chart.title.fontcolor := ColorComboBox1.Color;
end;

procedure TMainForm.ColorComboBox2Change(Sender: TObject);
begin
  chart.legend.fontColor := ColorComboBox2.Color;
end;

procedure TMainForm.ColorComboBox3Change(Sender: TObject);
begin
  chart.legend.backgroundColor := ColorComboBox3.Color;
end;

procedure TMainForm.ColorComboBox4Change(Sender: TObject);
begin
  chart.axisColor := ColorComboBox4.Color;
end;

procedure TMainForm.ColorComboBox8Change(Sender: TObject);
begin
  chart.backgroundColor := ColorComboBox8.Color;
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  chart.legend.Position := ComboBox1.ItemIndex;
  chart.legend.Margin := 10;
  SpinBox1.Value := chart.legend.x;
  SpinBox2.Value := chart.legend.Y;
end;

procedure TMainForm.InitComponents;
begin
  EditTitle.Text := chart.title.text;
  EditTitle.OnTyping := EditTitleTyping;
  ColorComboBox1.Color := chart.title.fontcolor;

  RadioButton1.IsChecked := chart.title.align = THAlign.left;
  RadioButton2.IsChecked := chart.title.align = THAlign.center;
  RadioButton3.IsChecked := chart.title.align = THAlign.right;

  ColorComboBox2.Color := chart.legend.fontColor;
  ColorComboBox3.Color := chart.legend.backgroundColor;

  ComboBox1.ItemIndex := chart.legend.Position;
  SpinBox1.Value := chart.legend.x;
  SpinBox2.Value := chart.legend.Y;

  ColorComboBox4.Color := chart.axisColor;
  SpinBox3.Value := chart.axisStrokeWidth;

  Edit1.Text := chart.xAxis.caption;
  Edit2.Text := chart.yAxis.caption;

  ColorComboBox8.Color := chart.backgroundColor;
  ColorComboBox6.Color := chart.plotPanelBackgroundColor;
  ColorComboBox7.Color := chart.gridColor;

  SpinBox5.Value := chart.gridStrokeWidth;


  CheckBox1.IsChecked := chart.autoScaleUp;
  CheckBox2.IsChecked := chart.autoScaleDown;
  CheckBox3.IsChecked := chart.legend.visible;


  SkLabel1.Text := Format('%s: %s, %s: %s', [chart.xAxis.caption, ' ', chart.yAxis.caption, '']);

  initListBox;
end;

procedure TMainForm.initListBox;
var
  I: Integer;
begin
  ListBox1.Items.Clear;
  for i := 0 to length(chart.series) - 1 do
    begin
      ListBox1.Items.Add(chart.series[i].name);
      ListBox1.ListItems[i].IsChecked := chart.series[i].visible;
    end;
end;

procedure TMainForm.ListBox1Change(Sender: TObject);
begin
  GroupBox6.Text := ListBox1.Selected.Text;
  ColorComboBox5.Color := chart.series[ListBox1.ItemIndex].color;
  SpinBox4.Value := chart.series[ListBox1.ItemIndex].lineWidth;
end;

procedure TMainForm.nothingSelect;
begin
  GroupBox6.Text := '';
  ColorComboBox5.Color := 0;
  SpinBox4.Value := 0;
  Button3.Enabled := false;
  ColorComboBox5.Enabled := false;
  SpinBox4.Enabled := false;
  Button5.Enabled := false;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  k: Integer;
begin
  if MessageDlg('Are you sure to delete ' + ListBox1.Selected.text + '?',
        TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], 0) = mrOk then
    begin
      ListBox1.OnChange := nil;
      ColorComboBox5.OnChange := nil;
      SpinBox4.OnChange := nil;
      Button3.Enabled := false;

      k := ListBox1.Selected.Index;
      ListBox1.Items.Delete(k);
      chart.deleteSerie(k);

      if ListBox1.Items.Count = 0 then
        nothingSelect
      else
        begin
          k := k - 1;
          if k = -1 then k := ListBox1.Items.Count - 1;

          ListBox1.OnChange := ListBox1Change;
          ColorComboBox5.OnChange := ColorComboBox5Change;
          SpinBox4.OnChange := SpinBox4Change;
          Button3.Enabled := true;

          ListBox1.ItemIndex := k;
        end;

    end;
end;


procedure TMainForm.ColorComboBox5Change(Sender: TObject);
begin
  chart.series[ListBox1.ItemIndex].color := ColorComboBox5.Color;
end;


procedure TMainForm.ColorComboBox6Change(Sender: TObject);
begin
  chart.plotPanelBackgroundColor := ColorComboBox6.Color;
end;

procedure TMainForm.ColorComboBox7Change(Sender: TObject);
begin
  chart.gridColor := ColorComboBox7.Color;
end;

procedure TMainForm.ListBox1ChangeCheck(Sender: TObject);
begin
  chart.series[ListBox1.ItemIndex].visible := ListBox1.Selected.IsChecked;
end;

procedure TMainForm.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.IsChecked then chart.title.align := THAlign.left;
  if RadioButton2.IsChecked then chart.title.align := THAlign.center;
  if RadioButton3.IsChecked then chart.title.align := THAlign.right;
end;

procedure TMainForm.SpinBox1Change(Sender: TObject);
begin
  chart.legend.x := SpinBox1.Value;
end;

procedure TMainForm.SpinBox2Change(Sender: TObject);
begin
  chart.legend.y := SpinBox2.Value;
end;

procedure TMainForm.SpinBox3Change(Sender: TObject);
begin
  chart.axisStrokeWidth := SpinBox3.Value;
end;

procedure TMainForm.SpinBox4Change(Sender: TObject);
begin
   chart.series[ListBox1.ItemIndex].lineWidth := SpinBox4.Value;
end;

procedure TMainForm.SpinBox5Change(Sender: TObject);
begin
  chart.gridStrokeWidth := SpinBox5.Value;
end;

procedure TMainForm.Edit1Typing(Sender: TObject);
begin
  Button10.Enabled := true;
end;

procedure TMainForm.Edit2Typing(Sender: TObject);
begin
  Button11.Enabled := true;
end;

procedure TMainForm.EditTitleTyping(Sender: TObject);
begin
  Button9.Enabled := true;
end;

procedure TMainForm.Button9Click(Sender: TObject);
begin
  chart.title.text := EditTitle.Text;
  Button9.Enabled := false;
end;

end.
