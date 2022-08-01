unit ufSliders;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, uSliderCommon, System.UIConsts, FMX.Edit,
  FMX.Controls.Presentation, FMX.Objects, ufRangeFrame,
  Generics.Collections;

type
  TSliderListener =  procedure (parameter : string; value : double) of object;

  TSliderInitialValue = class (TObject)
    lowRange, highRange : double;
    value : double;
    constructor Create (lowRange, highRange, value : double);
  end;

  TfrmSliders = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    btnClose: TButton;
    chkFixYAxis: TCheckBox;
    btnReset: TButton;
    Timer1: TTimer;
    btnClearAll: TButton;
    btnMakeAllSliders: TButton;
    VertScrollBox: TVertScrollBox;
    Layout3: TLayout;
    Label1: TLabel;
    rangeFrame: TfrmRangeFrame;
    Layout4: TLayout;
    lstParameters: TListBox;
    Rectangle1: TRectangle;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstParametersChange(Sender: TObject);
    procedure chkFixYAxisChange(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure rangeFrameedtLowerExit(Sender: TObject);
    procedure rangeFrameedtUpperExit(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnMakeAllSlidersClick(Sender: TObject);
  private
    { Private declarations }
    currentRowPosition : integer;
    sliderWidth : integer;
    closeButton : TSpeedButton;
    fireSlider : boolean;
    Observers : TList<TSliderListener>;
    procedure closeButtonClick (sender : TObject);
    procedure configButtonClick (sender : TObject);
    procedure deleteSlider (index : integer);
    procedure OnSliderTrack (Sender : TObject);
    procedure turnOffRangeSettings;
    procedure updateRangeLabel (index : integer);
    procedure updateCurrentValueLabel (index : integer);
   public
    { Public declarations }
    sliderList : TSliderList;
    OnNotifyChange : TSliderListener;
    procedure freeSliders;
    procedure AddSlider (parameter : string; lowRange, highRange : double; value : double);
  end;

var
  frmSliders: TfrmSliders;

implementation

{$R *.fmx}


Uses ufMain, ufConfigSlider, uCoyoteCommon;

const UPPER_ROW_POSITION = 12;
      SLIDER_VERTICAL_DISTANCE = 48;//26;
      LEFT_MARGIN = 24;
      GAP = 10;
      LB2_VERT_OFFSET = 16;
      LB2_HORIZ_OFFSET = 32;


constructor TSliderInitialValue.Create (lowRange, highRange, value : double);
begin
  self.value := value;
  self.lowRange := lowRange;
  self.highRange := highRange;
end;


function convertFloatToStr (value : double) : string;
var cOld : Char;
begin
  cOld := FormatSettings.DecimalSeparator; {save locale specified value}
  FormatSettings.DecimalSeparator := '.';
  try
    result := Format('%-8.4f', [value]);
  finally
    FormatSettings.DecimalSeparator := cOld;
  end;
end;


procedure TfrmSliders.btnClearAllClick(Sender: TObject);
begin
  freeSliders;
end;

procedure TfrmSliders.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSliders.btnMakeAllSlidersClick(Sender: TObject);
var si : TSliderInitialValue;
begin
  if lstParameters.Count > 150 then
     begin
     showmessage ('More than 15 sliders is a big excessive');
     exit;
     end;

  freeSliders;
  for var i := 0 to lstParameters.Count - 1 do
      begin
      si := lstParameters.Items.Objects[i] as TSliderInitialValue;
      addSlider (lstParameters.Items[i],
            si.lowRange,
            si.highRange,
            si.value);
      end;
end;

procedure TfrmSliders.updateRangeLabel (index : integer);
begin
  sliderList[index].lb2.Text := '[' + convertFloatToStr (sliderList[index].minValue) + ' -> ' + convertFloatToStr (sliderList[index].maxValue) + ']';
  updateCurrentValueLabel (index);
end;


procedure TfrmSliders.updateCurrentValueLabel (index : integer);
var value : double;
begin
  value := sliderList[index].minValue + (sliderList[index].slider.value/100) * (sliderList[index].maxValue - sliderList[index].minValue);
  sliderList[index].lb1.Text := sliderList[index].parameter + ' = ' + convertFloatToStr (value);
end;


procedure TfrmSliders.btnResetClick(Sender: TObject);
var i: integer;
begin
  fireSlider := False;
  for i := 0 to sliderList.Count - 1 do
      begin
      updateCurrentValueLabel (i);;
      updateRangeLabel (i);
      sliderList[i].slider.Value := 100 * (sliderList[i].initialLoadedValue / (sliderList[i].maxValue - sliderList[i].minValue));
      end;
  fireSlider := True;
end;


procedure TfrmSliders.deleteSlider (index : integer);
begin
  sliderList[index].lb1.Free;
  sliderList[index].lb2.Free;
  sliderList[index].slider.Free;
  sliderList.Delete(index);
end;


procedure TfrmSliders.freeSliders;
var i, j : integer;
    btn1, btn2 : TSpeedButton;
begin
  try
    for i := sliderList.Count - 1 downto 0 do
        begin
        btn1 := sliderList[i].closeButton;
        btn2 := sliderList[i].configButton;
        deleteSlider (i);

        currentRowPosition := UPPER_ROW_POSITION;
        for j := 0 to sliderList.Count - 1 do
            begin
            currentRowPosition := currentRowPosition + SLIDER_VERTICAL_DISTANCE;
            sliderList[j].closeButton.Position.Y := currentRowPosition - 2;
            sliderList[j].configButton.Position.Y := currentRowPosition - 2;
            sliderList[j].slider.Position.Y := currentRowPosition;
            sliderList[j].lb1.Position.Y := currentRowPosition - 2;
            sliderList[j].lb2.Position.Y := currentRowPosition - LB2_VERT_OFFSET;
            sliderList[j].closeButton.Tag := i;
            sliderList[j].configButton.Tag := i;
            sliderList[j].slider.Tag := i;
            sliderList[j].lb2.Tag := i;
            end;
        btn1.Free; btn2.Free;
        end;
  except
    on E: Exception do
       showmessage ('Internal error: ' + e.Message);
    end;
end;


procedure TfrmSliders.chkFixYAxisChange(Sender: TObject);
begin
  if chkFixYAxis.IsChecked then
     begin
     //frmMain.controller.ViewerSetProperty('UserScale_Ymax', frmMain.sg.properties.FWorldYmax);
     frmMain.controller.ViewerSetProperty('AutoYScaling', false)
     end
  else
     frmMain.controller.ViewerSetProperty('AutoYScaling', true)
end;


procedure TfrmSliders.turnOffRangeSettings;
begin
  btnClose.SetFocus;
  rangeFrame.hide;
end;


procedure TfrmSliders.closeButtonClick (sender : TObject);
var index : integer; i : integer;
    btn1, btn2 : TSpeedButton;
begin
  turnOffRangeSettings;
  try
    index := (sender as TSpeedButton).Tag;
    btn1 := sliderList[index].closeButton;
    btn2 := sliderList[index].configButton;
    deleteSlider (index);

    currentRowPosition := UPPER_ROW_POSITION;
    for i := 0 to sliderList.Count - 1 do
        begin
        sliderList[i].closeButton.Position.Y := currentRowPosition - 2;
        sliderList[i].configButton.Position.Y := currentRowPosition - 2;
        sliderList[i].slider.Position.Y := currentRowPosition;
        sliderList[i].lb1.Position.Y := currentRowPosition - 2;
        sliderList[i].lb2.Position.Y := currentRowPosition - LB2_VERT_OFFSET;
        sliderList[i].closeButton.Tag := i;
        sliderList[i].configButton.Tag := i;
        sliderList[i].slider.Tag := i;
        sliderList[i].lb2.Tag := i;
        currentRowPosition := currentRowPosition + SLIDER_VERTICAL_DISTANCE;
        end;
    btn2.Free;
    closeButton := btn1;
  except
    on E: Exception do
       showmessage ('Internal error: index = ' + inttostr (index));
    end;
  timer1.Enabled := true;
end;


// Inplements radio-like button behavior for the config button
procedure TfrmSliders.configButtonClick (sender : TObject);
var index : integer;
begin
  if Sender is TSpeedButton then
     index := (sender as TSpeedButton).Tag
  else
  if Sender is TLabel then
     index := (sender as TLabel).Tag;

  // If we clicked the same slider then turn off the edits boxes
  if index = rangeFrame.sliderIndex then
     begin
     rangeFrame.hide;
     btnClose.SetFocus;
     // Set the tag to -1 so that we don't recognize it next time round.
     rangeFrame.sliderIndex := -1;
     exit;
     end;

  // If not the same one, turn everythng off then
  // turn on the one we selected.
  rangeFrame.hide;
  rangeFrame.sliderIndex := index;

  btnClose.SetFocus;

  rangeFrame.show;

  rangeFrame.edtLower.Text := Format ('%8.5f', [sliderList[index].minValue]);
  rangeFrame.edtUpper.Text := Format ('%8.5f', [sliderList[index].maxValue]);

  rangeFrame.lblParameterName.text := sliderList[index].parameter;
  rangeFrame.lblParameterName.Visible := True;
  rangeFrame.edtLower.SetFocus;
end;


procedure TfrmSliders.OnSliderTrack (Sender : TObject);
var sliderInfo : TSliderInfo;
    value : double;
begin
  if not fireSlider then exit;

  sliderInfo := sliderList[(sender as TTrackBar).Tag];
  value := sliderInfo.minValue + ((sender as TTrackBar).value/100) * (sliderInfo.maxValue - sliderInfo.minValue);
  updateCurrentValueLabel ((sender as TTrackBar).Tag);
  updateRangeLabel ((sender as TTrackBar).Tag);

  if Assigned (OnNotifyChange) then
     OnNotifyChange (sliderInfo.parameter, value);
end;


procedure TfrmSliders.rangeFrameedtLowerExit(Sender: TObject);
var index : integer;
    value : double;
begin
  index := (TEdit(sender).parent as TfrmRangeFrame).sliderIndex;
  value := strtofloat (rangeFrame.edtLower.Text);
  if value > sliderList[index].maxValue then
     begin
     showmessage ('Lower value must be less than upper value');
     rangeFrame.edtLower.Text := convertFloatToStr(sliderList[index].maxValue - 1);
     exit;
     end;

  sliderList[index].minValue := value;
  updateRangeLabel(index);
end;


procedure TfrmSliders.rangeFrameedtUpperExit(Sender: TObject);
var index : integer;
    value : double;
begin
  index := (TEdit(sender).parent as TfrmRangeFrame).sliderIndex;
  value := strtofloat (rangeFrame.edtUpper.Text);
  if value < sliderList[index].minValue then
     begin
     showmessage ('Upper value must be greater than lower value');
     rangeFrame.edtUpper.Text := convertFloatToStr(sliderList[index].minValue + 1);
     exit;
     end;

  sliderList[index].maxValue := strtofloat (rangeFrame.edtUpper.Text);
  updateRangeLabel(index);
end;


procedure TfrmSliders.Timer1Timer(Sender: TObject);
begin
  closeButton.Free;
  timer1.Enabled := False;
end;


procedure TfrmSliders.addSlider (parameter : string; lowRange, highRange : double; value : double);
var slider : TTrackBar;
    sliderInfo : TSliderInfo;
    lb1, lb2 : TLabel;
    str : string;
    index : integer;
    closeButton, configButton : TSpeedButton;
    buttonWidth : integer;
begin
  buttonWidth := 20;
  closeButton := TSpeedButton.Create(Self);
  closeButton.Parent := VertScrollBox;
  closeButton.Width := buttonWidth;
  closeButton.Height := 20;
  closeButton.Text := 'X';
  closeButton.ShowHint := True;
  closeButton.Hint := 'Remove slider';
  closeButton.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
  closeButton.TextSettings.FontColor := claRed;
  closeButton.Position.X := LEFT_MARGIN;
  closeButton.Position.Y := currentRowPosition - 2;
  closeButton.OnClick := closeButtonClick;

  configButton := TSpeedButton.Create(Self);
  configButton.Parent := VertScrollBox;
  configButton.Width := buttonWidth;
  configButton.Height := 20;
  configButton.Text := 'C';
  configButton.ShowHint := True;
  configButton.Hint := 'Change slider range';
  configButton.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
  configButton.TextSettings.FontColor := claOrange;
  configButton.Position.X := LEFT_MARGIN + buttonWidth + GAP;
  configButton.Position.Y := currentRowPosition - 2;
  configButton.OnClick := configButtonClick;

  slider :=  TTrackBar.Create (Self);
  slider.Parent := VertScrollBox;
  slider.Position.X := configButton.Position.X + configButton.width + GAP;
  slider.Position.Y := currentRowPosition;

  slider.Width := (VertScrollBox.width - 136) - slider.Position.X;
  slider.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];

  lb1 := TLabel.Create (Self);
  lb1.Parent := VertScrollBox;
  lb1.Position.X := slider.position.X + slider.Width + GAP;
  lb1.Position.Y := currentRowPosition - 2;
  lb1.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight];

  lb2 := TLabel.Create (Self);
  lb2.Parent := VertScrollBox;
  lb2.Position.X := slider.position.X + LB2_HORIZ_OFFSET;
  lb2.Position.Y := currentRowPosition - LB2_VERT_OFFSET;
  lb2.Anchors := [TAnchorKind.akTop, TAnchorKind.akLeft];
  lb2.OnClick := configButtonClick;
  lb2.HitTest := True;

  sliderInfo := TSliderInfo.Create(parameter, 0, lowRange, highRange, value/10, lb1, lb2);
  sliderInfo.closeButton := closeButton;
  sliderInfo.configButton := configButton;
  sliderInfo.slider := slider;

  index := sliderList.Add(sliderInfo);
  sliderList[index].index := index;
  sliderList[index].closeButton.Tag := index;
  sliderList[index].configButton.Tag := index;
  sliderList[index].slider.Tag := index;
  sliderList[index].lb2.Tag := index;

  sliderList[index].initialLoadedValue := value;

  slider.Value := 100 * (value / (sliderList[index].maxValue - sliderList[index].minValue));
  slider.OnTracking := OnSliderTrack;

  updateCurrentValueLabel (index);
  updateRangeLabel (index);

  fireSlider := True;
  currentRowPosition := currentRowPosition + SLIDER_VERTICAL_DISTANCE;
end;


procedure TfrmSliders.FormCreate(Sender: TObject);
begin
  sliderList := TSliderList.Create;
  currentRowPosition := UPPER_ROW_POSITION;
  sliderWidth := 160;
  rangeFrame.hide;
  Observers := TList<TSliderListener>.Create;
end;


procedure TfrmSliders.lstParametersChange(Sender: TObject);
var i : integer;
   si : TSliderInitialValue;
begin
  // Check if the slider has already been added
  for i := 0 to sliderList.Count - 1 do
      if sliderList[i].parameter = lstParameters.Items[lstParameters.ItemIndex] then
         exit;

  si := lstParameters.Items.Objects[lstParameters.ItemIndex] as TSliderInitialValue;
  addSlider (lstParameters.Items[lstParameters.ItemIndex],
            si.lowRange,
            si.highRange,
            si.value);
end;

initialization
  frmSliders := nil;
end.
