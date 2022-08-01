unit ufScrollChart;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  System.UIConsts,
  Generics.Collections,
  UScrollingChart,
  UDataSource,
  FMX.ListBox,
  uController,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  uColorList,
  uViewer,
  uFormViewer,
  FMX.Objects, FMX.Colors, FMX.Edit, FMX.EditBox, FMX.NumberBox;

type
  TfrmScrollChart = class(TFormViewer)
    Layout1: TLayout;
    Layout2: TLayout;
    pnlchart: TLayout;
    Layout3: TLayout;
    lstSelectionBox: TListBox;
    Timer: TTimer;
    btnRealTimeSliders: TSpeedButton;
    Image10: TImage;
    btnReset: TButton;
    chkAutoScale: TCheckBox;
    nbNumberOfPoints: TNumberBox;
    trkSpeed: TTrackBar;
    lbl1: TLabel;
    btnPauseRun: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cboBackgroundColor: TColorComboBox;
    cboPlotBackgroundColor: TColorComboBox;
    chkShowLegend: TCheckBox;
    nbXAxisMax: TNumberBox;
    Label4: TLabel;
    Layout4: TLayout;
    Line1: TLine;
    Layout5: TLayout;
    Label5: TLabel;
    Com: TLayout;
    Label7: TLabel;
    Label6: TLabel;
    lblValue: TLabel;
    btnUpPulse: TSpeedButton;
    Image1: TImage;
    btnDownPulse: TSpeedButton;
    Image2: TImage;
    Line2: TLine;
    Label8: TLabel;
    nbPercentageChange: TNumberBox;
    cboParameterNames: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure btnPauseRunClick(Sender: TObject);
    procedure btnRealTimeSlidersClick(Sender: TObject);
    procedure lstSelectionBoxChangeCheck(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure chkAutoScaleChange(Sender: TObject);
    procedure nbNumberOfPointsExit(Sender: TObject);
    procedure trkSpeedChange(Sender: TObject);
    procedure cboBackgroundColorChange(Sender: TObject);
    procedure cboPlotBackgroundColorChange(Sender: TObject);
    procedure chkShowLegendChange(Sender: TObject);
    procedure nbXAxisMaxExit(Sender: TObject);
    procedure btnUpPulseClick(Sender: TObject);
    procedure btnDownPulseClick(Sender: TObject);
    procedure nbPercentageChangeChange(Sender: TObject);
  private
    { Private declarations }
    floatingSpeciesIds : TStringList;
    parameterIds : TStringList;
    numberOfPoints : integer;
    xMax : double;
    percentageChange : double;
    procedure addSeries;
  public
    { Public declarations }
    chart : TScrollingChart;
    controller : TController;
    s1, s2, s3 : TDataSerie;
    currentTime : double;
    stepSize : double;
    procedure   ViewerModelHasChanged (sender : TObject);
    procedure   OnRealTimeSliderNotify (parameter : string; value : double);
    constructor Create (Owner: TComponent; Controller : TController);
  end;

var
  frmScrollChart: TfrmScrollChart;

implementation

{$R *.fmx}

Uses StrUtils, ufSliders;

procedure TfrmScrollChart.OnRealTimeSliderNotify (parameter : string; value : double);
begin
  controller.simulator.roadrunner.setValue (parameter, value);
  Timer.Enabled := True;
end;


procedure TfrmScrollChart.btnUpPulseClick(Sender: TObject);
var refValue : double;
    name : string;
begin
  timer.Enabled := False;

  name := cboParameterNames.items[cboParameterNames.ItemIndex];
  refValue := controller.simulator.roadrunner.getValue(name);
  controller.simulator.roadrunner.setValue(name, refValue*(1+percentageChange/100));
  lblValue.Text := Format ('%6f', [refValue*(1+percentageChange/100)]);

  timer.Enabled := True;
end;

procedure TfrmScrollChart.btnDownPulseClick(Sender: TObject);
var refValue : double;
    name : string;
begin
  timer.Enabled := False;

  name := cboParameterNames.items[cboParameterNames.ItemIndex];
  refValue := controller.simulator.roadrunner.getValue(name);
  controller.simulator.roadrunner.setValue(name, refValue/(1+percentageChange/100));
  lblValue.Text := Format ('%6f', [refValue/(1+percentageChange/100)]);

  timer.Enabled := True;
end;

procedure TfrmScrollChart.ViewerModelHasChanged (sender : TObject);
var bids : TStringList;
begin
  Timer.Enabled := False;

  floatingSpeciesIds.Free;
  floatingSpeciesIds := controller.simulator.roadrunner.getFloatingSpeciesIds();
  lstSelectionBox.Assign(floatingSpeciesIds);
  for var i := 0 to lstSelectionBox.Count - 1 do
    (lstSelectionBox.listitems[i] as Tlistboxitem).IsChecked := true;

  cboParameterNames.BeginUpdate;
  cboParameterNames.Items.Assign(controller.simulator.roadrunner.getGlobalParameterIds());
  bids := controller.simulator.roadrunner.getBoundarySpeciesIds();
  for var i := 0 to bids.Count - 1 do
      cboParameterNames.Items.Insert(0, bids[i]);
  bids.Free;
  cboParameterNames.ItemIndex := 0;
  cboParameterNames.EndUpdate;

  chart.DeleteSeries;
  addSeries;

  controller.simulator.roadRunner.reset;
  currentTime := 0;
  //chart.restart;   // Causes chart to crash because stage = ni
end;




procedure TfrmScrollChart.btnRealTimeSlidersClick(Sender: TObject);
var list : TStringList;
    i : integer;
    value : double;
begin
  if not Assigned(frmSliders) then
     frmSliders := TfrmSliders.Create(nil);
  frmSliders.OnNotifyChange := OnRealTimeSliderNotify;
  frmSliders.StyleBook := StyleBook;

  list := controller.simulator.roadrunner.getGlobalParameterIds;
  try
    // We can't change total masses for time course
    // simulations, so let's remove any of those entries.
    for i := list.Count - 1 downto 0 do
        if StartsText('_CSUM', list[i]) then
           list.Delete(i);

    for i := frmSliders.lstParameters.Count - 1 downto 0 do
        if frmSliders.lstParameters.items.Objects[i] <> nil then
           frmSliders.lstParameters.items.Objects[i].Free;
    frmSliders.lstParameters.Clear;
    for i := 0 to list.Count - 1 do
        begin
        value := controller.simulator.roadrunner.getValue (list[i]);
        frmSliders.lstParameters.Items.AddObject(list[i], TSliderInitialValue.Create (value/10, value*10, value));
        end;

  finally
    list.Free;
  end;

  frmSliders.Show;
  //frmSliders.BringToFront;
end;


procedure TfrmScrollChart.btnResetClick(Sender: TObject);
var i : integer;
begin
  Timer.Enabled := False;
  controller.simulator.roadRunner.reset;
  currentTime := 0;
  chart.restart;
  btnPauseRun.Text := 'Run';
end;


procedure TfrmScrollChart.cboPlotBackgroundColorChange(Sender: TObject);
begin
  chart.plotPanelBackgroundColor := cboPlotBackgroundColor.Color;
  chart.Redraw;
end;

procedure TfrmScrollChart.chkAutoScaleChange(Sender: TObject);
begin
  if chkAutoScale.IsChecked then
     chart.autoScaleUp := True
  else
     chart.autoScaleUp := False;
end;


procedure TfrmScrollChart.chkShowLegendChange(Sender: TObject);
begin
  chart.legend.visible := chkShowLegend.IsChecked;
end;


procedure TfrmScrollChart.addSeries;
var colorList : TColorList;
    i, index : integer;
    series : TDataSerie;
begin
  colorList := TColorList.Create;
  try
    colorList.Restart;
    for i := 0 to floatingSpeciesIds.Count - 1 do
        begin
        series := chart.addSerieByName (floatingSpeciesIds[i]);
        series.color := colorList.NextColor;
        series.lineWidth := 2;
        end;
  finally
    colorList.Free;
  end;
end;


procedure TfrmScrollChart.cboBackgroundColorChange(Sender: TObject);
begin
  chart.BackgroundColor := cboBackgroundColor.Color;
  chart.Redraw;
end;


constructor TfrmScrollChart.Create (Owner : TComponent; Controller : TController);
begin
  inherited Create(Owner);

  numberOfPoints := 200;
  xMax := 20.0;

  self.controller := controller;

  chart := TScrollingChart.Create(self);
  chart.Parent := pnlchart;
  chart.align := TAlignLayout.Client;

  cboPlotBackgroundColor.Color := claWhite;
  cboBackgroundColor.Color := claAliceBlue;

  chart.xAxis.color := claBlack;
  chart.yAxis.color := claBlack;

  chart.setXAxisRange(0, xMax);
  chart.setYAxisRange(0, 4);

  chkShowLegend.IsChecked := True;
  PercentageChange := 50;

  stepSize := Xmax/numberOfPoints;
  chart.externalTimer := Timer;
  chart.deltaX := stepSize;
  currentTime := 0;

  floatingSpeciesIds := controller.simulator.roadRunner.getFloatingSpeciesIds();
  //chart.autoScale := true;

  OnViewerModelHasChanged := ViewerModelHasChanged;

  addSeries;
  controller.RegisterFormViewer(self);
 end;


procedure TfrmScrollChart.btnPauseRunClick(Sender: TObject);
begin
  if btnPauseRun.Text = 'Run' then
     begin
     timer.Enabled := True;
     btnPauseRun.Text := 'Pause';
     end
  else
     begin
     timer.Enabled := False;
     btnPauseRun.Text := 'Run';
     end;
end;


procedure TfrmScrollChart.FormDestroy(Sender: TObject);
begin
  chart.Destroy;
end;


procedure TfrmScrollChart.lstSelectionBoxChangeCheck(Sender: TObject);
begin
  for var i := 0 to lstSelectionBox.Items.Count - 1 do
      chart.series[i].visible := (lstSelectionBox.listitems[i] as Tlistboxitem).IsChecked;
end;


procedure TfrmScrollChart.nbNumberOfPointsExit(Sender: TObject);
begin
  timer.Enabled := False;
  numberOfPoints := trunc (nbNumberOfPoints.Value);
  Timer.Enabled := false;
  stepSize := xMax/numberOfPoints;
  chart.deltaX := stepSize;
  chart.Restart;
  currentTime := 0;
  controller.simulator.roadrunner.reset;
end;


procedure TfrmScrollChart.nbPercentageChangeChange(Sender: TObject);
begin
  percentageChange := nbPercentageChange.Value;
end;

procedure TfrmScrollChart.nbXAxisMaxExit(Sender: TObject);
begin
  timer.Enabled := False;
  xMax := nbXAxisMax.Value;
  chart.SetXAxisRange(0, xMax);
  stepSize := xMax/numberOfPoints;
  chart.deltaX := stepSize;
  chart.Restart;
  currentTime := 0;
  controller.simulator.roadrunner.reset;
end;


procedure TfrmScrollChart.TimerTimer(Sender: TObject);
begin
  try
    try
      chart.globaldata.dataSource.addX(currentTime);
      for var i := 0 to floatingSpeciesIds.Count - 1 do
          chart.series[i].addY(controller.simulator.roadrunner.getValue(floatingSpeciesIds[i]));
    finally

    end;

    chart.update;
    currentTime := controller.simulator.roadrunner.oneStep(currentTime, stepSize);
  except
   On E: exception do
      begin
      timer.Enabled := False;
      showmessage (e.Message);
      end;
  end;
end;

procedure TfrmScrollChart.trkSpeedChange(Sender: TObject);
begin
  timer.Interval := trunc (trkSpeed.Value);
end;

end.
