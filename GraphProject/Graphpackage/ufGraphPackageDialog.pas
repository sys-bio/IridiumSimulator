unit ufGraphPackageDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TabControl, FMX.Edit, FMX.Memo, uSubgraph, FMX.Colors,
  FMX.ListBox, FMX.Controls.Presentation, FMX.EditBox, FMX.SpinBox;

type
  TfrmGraphPackageDlg = class(TForm)
    tabGraph: TTabControl;
    tbGraph: TTabItem;
    tbAxes: TTabItem;
    Layout1: TLayout;
    btnCancel: TButton;
    tbSeries: TTabItem;
    tbLegend: TTabItem;
    lblMainTitle: TLabel;
    edtMainTitle: TEdit;
    btnAppy: TButton;
    TabControl2: TTabControl;
    tbXAxis: TTabItem;
    tbYAxis: TTabItem;
    Label1: TLabel;
    edtXAxisTitle: TEdit;
    btnOK: TButton;
    chkMainTitle: TCheckBox;
    chkGraphBorder: TCheckBox;
    chkXAxisTitle: TCheckBox;
    Label7: TLabel;
    edtYAxisTitle: TEdit;
    chkYAxisTitle: TCheckBox;
    StyleBook1: TStyleBook;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    cbSymbolFillColor: TComboColorBox;
    Layout2: TLayout;
    Layout3: TLayout;
    Label10: TLabel;
    lbSeries: TListBox;
    Layout4: TLayout;
    Layout5: TLayout;
    Label11: TLabel;
    lbYColumns: TListBox;
    Label12: TLabel;
    cbSymbolOutlineColor: TComboColorBox;
    GroupBox2: TGroupBox;
    Label13: TLabel;
    cboLineColor: TComboColorBox;
    tkSymbolRadius: TTrackBar;
    Label14: TLabel;
    btnClose: TButton;
    cboSymbols: TComboBox;
    Label15: TLabel;
    tkSymbolOutlineThickness: TTrackBar;
    chkGradient: TCheckBox;
    cbSymbolEndColor: TComboColorBox;
    lblStartColor: TLabel;
    lblEndColor: TLabel;
    chkAutoScaleBothAxes: TCheckBox;
    tkLineThickness: TTrackBar;
    Label16: TLabel;
    chkSymbolsVisible: TCheckBox;
    chkLinesVisible: TCheckBox;
    cboLineStyle: TComboBox;
    chkLogBothAxes: TCheckBox;
    chkLegendVisible: TCheckBox;
    GroupBox4: TGroupBox;
    chkLegendFrameVisible: TCheckBox;
    cbLegendFrameColor: TComboColorBox;
    Label19: TLabel;
    Label21: TLabel;
    tkLegendFrameThickness: TTrackBar;
    Label22: TLabel;
    tkFrameGap: TTrackBar;
    Label23: TLabel;
    tkLegendLineLength: TTrackBar;
    Label24: TLabel;
    cbolegendInteriorColor: TComboColorBox;
    cboBackgroundColor: TColorComboBox;
    Label20: TLabel;
    Label25: TLabel;
    cboGraphDrawingArea: TColorComboBox;
    Label26: TLabel;
    Label27: TLabel;
    cboGraphBorderColor: TColorComboBox;
    cboXTitleColor: TColorComboBox;
    Label30: TLabel;
    cboYTitleColor: TColorComboBox;
    Label31: TLabel;
    cboMainTitleColor: TColorComboBox;
    tbGrid: TTabItem;
    GroupBox3: TGroupBox;
    imgNoMinorGrid: TImageControl;
    imgXMinorGrid: TImageControl;
    imgYMinorGrid: TImageControl;
    imgXYMinorGrid: TImageControl;
    Label32: TLabel;
    GroupBox5: TGroupBox;
    ImageControl1: TImageControl;
    ImageControl2: TImageControl;
    ImageControl3: TImageControl;
    ImageControl8: TImageControl;
    Label28: TLabel;
    cboXMajorColor: TComboColorBox;
    Label34: TLabel;
    cboYMajorColor: TComboColorBox;
    Label33: TLabel;
    Label36: TLabel;
    Label35: TLabel;
    cboYMinorColor: TComboColorBox;
    cboXMinorColor: TComboColorBox;
    Label29: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    spinYMajorGridThickness: TSpinBox;
    SpinXMajorGridThickness: TSpinBox;
    SpinXMinorGridWidth: TSpinBox;
    SpinYMinorGridWidth: TSpinBox;
    SpinGraphBorderWidth: TSpinBox;
    Label37: TLabel;
    SpinNumberOfXMinorTicks: TSpinBox;
    Label38: TLabel;
    SpinNumberOfYMinorTicks: TSpinBox;
    Label39: TLabel;
    Label40: TLabel;
    SpinNumberOfXMajorTicks: TSpinBox;
    SpinNumberOfYMajorTicks: TSpinBox;
    GroupBox6: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label2: TLabel;
    cboXAxisColor: TComboColorBox;
    Label3: TLabel;
    edtXMinimum: TEdit;
    Label4: TLabel;
    edtXMaximum: TEdit;
    chkAutoXScaling: TCheckBox;
    chkLogXAxes: TCheckBox;
    GroupBox7: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label8: TLabel;
    cboYAxisColor: TComboColorBox;
    Label5: TLabel;
    edtYMinimum: TEdit;
    Label6: TLabel;
    edtYMaximum: TEdit;
    chkAutoYScaling: TCheckBox;
    chkLogYAxes: TCheckBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAppyClick(Sender: TObject);
    procedure lbSeriesChange(Sender: TObject);
    procedure lbYColumnsChange(Sender: TObject);
    procedure chkGraphBorderChange(Sender: TObject);
    procedure chkMainTitleChange(Sender: TObject);
    procedure cboLineColorChange(Sender: TObject);
    procedure cbSymbolFillColorChange(Sender: TObject);
    procedure cbSymbolOutlineColorChange(Sender: TObject);
    procedure chkAutoXScalingChange(Sender: TObject);
    procedure chkAutoYScalingChange(Sender: TObject);
    procedure tkSymbolRadiusChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure cboSymbolsChange(Sender: TObject);
    procedure tkSymbolOutlineThicknessChange(Sender: TObject);
    procedure chkXAxisTitleChange(Sender: TObject);
    procedure cboXAxisColorChange(Sender: TObject);
    procedure cboXMajorColorChange(Sender: TObject);
    procedure cboXMinorColorChange(Sender: TObject);
    procedure chkLogXAxesChange(Sender: TObject);
    procedure chkLogYAxesChange(Sender: TObject);
    procedure cboYMinorColorChange(Sender: TObject);
    procedure cboYMajorColorChange(Sender: TObject);
    procedure cboYAxisColorChange(Sender: TObject);
    procedure chkYAxisTitleChange(Sender: TObject);
    procedure edtXMaximumChange(Sender: TObject);
    procedure edtXAxisTitleChange(Sender: TObject);
    procedure edtYAxisTitleChange(Sender: TObject);
    procedure edtMainTitleChange(Sender: TObject);
    procedure chkGradientChange(Sender: TObject);
    procedure cbSymbolEndColorChange(Sender: TObject);
    procedure chkAutoScaleBothAxesChange(Sender: TObject);
    procedure tkLineThicknessChange(Sender: TObject);
    procedure chkLinesVisibleChange(Sender: TObject);
    procedure chkSymbolsVisibleChange(Sender: TObject);
    procedure cboLineStyleChange(Sender: TObject);
    procedure imgNoMajorGridClick(Sender: TObject);
    procedure imgXMajorGridClick(Sender: TObject);
    procedure imgYMajorGridClick(Sender: TObject);
    procedure imgXYMajorGridClick(Sender: TObject);
    procedure imgNoMinorGridClick(Sender: TObject);
    procedure imgXMinorGridClick(Sender: TObject);
    procedure imgYMinorGridClick(Sender: TObject);
    procedure imgXYMinorGridClick(Sender: TObject);
    procedure chkLogBothAxesChange(Sender: TObject);
    procedure edtXMinimumChange(Sender: TObject);
    procedure edtYMinimumChange(Sender: TObject);
    procedure edtYMaximumChange(Sender: TObject);
    procedure chkLegendVisibleChange(Sender: TObject);
    procedure chkLegendFrameVisibleChange(Sender: TObject);
    procedure cbLegendFrameColorChange(Sender: TObject);
    procedure cbLegendInteriorColorChange(Sender: TObject);
    procedure tkLegendFrameThicknessChange(Sender: TObject);
    procedure tkFrameGapChange(Sender: TObject);
    procedure tkLegendLineLengthChange(Sender: TObject);
    procedure cboBackgroundColorChange(Sender: TObject);
    procedure cboGraphDrawingAreaChange(Sender: TObject);
    procedure cboGraphBorderColorChange(Sender: TObject);
    procedure cboXTitleColorChange(Sender: TObject);
    procedure cboYTitleColorChange(Sender: TObject);
    procedure cboMainTitleColorChange(Sender: TObject);
    procedure trackGraphBorderWidthChange(Sender: TObject);
    procedure spinYMajorGridThicknessChange(Sender: TObject);
    procedure SpinXMajorGridThicknessChange(Sender: TObject);
    procedure SpinXMinorGridWidthChange(Sender: TObject);
    procedure SpinYMinorGridWidthChange(Sender: TObject);
    procedure SpinGraphBorderWidthChange(Sender: TObject);
    procedure SpinNumberOfXMinorTicksChange(Sender: TObject);
    procedure SpinNumberOfYMinorTicksChange(Sender: TObject);
    procedure SpinNumberOfXMajorTicksChange(Sender: TObject);
    procedure SpinNumberOfYMajorTicksChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    subgraph : TSubgraph;
    fireEvent : boolean;
    currentSubGraph : TSubGraphProperties;
    procedure copyPropertiesToDlg (gp : TSubGraphProperties);
    procedure copyPropertiesFromDlg (gp : TSubGraphProperties);
  end;

var
  frmGraphPackageDlg: TfrmGraphPackageDlg;

implementation

{$R *.fmx}

Uses uPlottingPanel, uRRDataSeries, uSymbolDetails, uLineDetails, uRRCommon;


procedure TfrmGraphPackageDlg.copyPropertiesToDlg (gp : TSubGraphProperties);
var i : integer;
begin
  fireEvent := false;
  currentSubGraph := gp;

  SpinXMajorGridThickness.Value := gp.XMajorGridThicknessInSkiaUnits;
  SpinYMajorGridThickness.Value := gp.YMajorGridThicknessInSkiaUnits;

  SpinYMinorGridWidth.Value := gp.YMinorGridThicknessInSkiaUnits;
  SpinYMinorGridWidth.Value := gp.YMinorGridThicknessInSkiaUnits;

  SpinNumberOfXMinorTicks.Value := gp.NumXMinorTicks;
  SpinNumberOfYMinorTicks.Value := gp.NumYMinorTicks;

  SpinNumberOfXMajorTicks.Value := gp.NumberOfXTicks;
  SpinNumberOfYMajorTicks.Value := gp.NumberOfYTicks;

  edtMainTitle.Text  := gp.mainTitleObject.textProperties.value;
  edtXAxisTitle.Text := gp.xaxisTitleObject.textProperties.value;
  edtYAxisTitle.Text := gp.yaxisTitleObject.textProperties.value;

  cboGraphDrawingArea.Color := gp.GraphBackgroundColor;
  cboGraphBorderColor.Color := gp.GraphBorderColor;
  SpinGraphBorderWidth.Value := gp.GraphBorderThicknessInSkiaUnits;

  cboXAxisColor.Color := gp.XAxisColor;
  cboYAXisColor.Color := gp.YAxisColor;

  cboXMajorColor.Color := gp.XMajorGridColor;
  cboYMajorColor.Color := gp.YMajorGridColor;
  cboXMinorColor.Color := gp.XMinorGridColor;
  cboYMinorColor.Color := gp.YMinorGridColor;

  chkGraphBorder.IsChecked := gp.GraphBorder;
  chkMainTitle.IsChecked := gp.bDrawMainTitle;
  chkXAxisTitle.IsChecked := gp.bDrawXAxisTitle;

  cboMainTitleColor.Color := gp.MainTitleObject.textProperties.fontColor;
  cboXTitleColor.Color := gp.XAxisTitleObject.textProperties.fontColor;
  cboYTitleColor.Color := gp.YAxisTitleObject.textProperties.fontColor;

  chkLogXAxes.IsChecked := gp.LogXAxis;
  chkLogYAxes.IsChecked := gp.LogYAxis;

  edtXMinimum.Text := floattostr (gp.UserScale_Xmin);
  edtXMaximum.Text := floattostr (gp.UserScale_Xmax);
  edtYMinimum.Text := floattostr (gp.UserScale_Ymin);
  edtYMaximum.Text := floattostr (gp.UserScale_Ymax);

  chkAutoXScaling.IsChecked := gp.AutoXScaling;
  chkAutoYScaling.IsChecked := gp.AutoYScaling;

  if gp.AutoXScaling and gp.AutoYScaling then
     chkAutoScaleBothAxes.IsChecked := true
  else
     chkAutoScaleBothAxes.IsChecked := false;

  chkLegendVisible.IsChecked := gp.legend.visible;
  chkLegendFrameVisible.IsChecked :=  gp.legend.frameVisible;
  cbLegendFrameColor.color := gp.legend.outlineColor;
  cboLegendInteriorColor.Color := gp.legend.interiorColor;
  tkLegendFrameThickness.Value := gp.legend.lineThicknessInCms*300;

  tkFrameGap.value := gp.legend.frameGapInCms*100;
  tkLegendLineLength.value := gp.legend.lineLengthInCms*50;
  tkLegendFrameThickness.Value := gp.legend.frameBorderThickness*10;

  lbSeries.Clear;
  lbSeries.Items.Add (gp.dataBlocks[0].name);
  lbYColumns.Clear;
  if gp.dataBlocks.count > 0 then
     for i := 0 to gp.dataBlocks[0].columns.Count - 1 do
        lbYColumns.Items.Add (gp.dataBlocks[0].columns[i].name);

  cboSymbols.Clear;
  cboSymbols.Items.Assign(getListOfSymbolNames());
  cboLineStyle.Items.Assign(getListOfLineStyles());

  lbSeries.ItemIndex := 0;
  lbYColumns.ItemIndex := 0;

  fireEvent := true;
end;


procedure TfrmGraphPackageDlg.edtMainTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.mainTitleObject.textProperties.value := edtMainTitle.Text;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.edtXAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.XAxisTitleObject.textProperties.value := edtXAxisTitle.Text;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.edtXMaximumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.UserScale_Xmax := strtofloat (edtXMaximum.Text);
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.edtXMinimumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.UserScale_Xmin := strtofloat (edtXMinimum.Text);
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.edtYAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.YAxisTitleObject.textProperties.value := edtYAxisTitle.Text;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.edtYMaximumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.UserScale_Ymax := strtofloat (edtYMaximum.Text);
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.edtYMinimumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.UserScale_Ymin := strtofloat (edtYMinimum.Text);
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.FormCreate(Sender: TObject);
begin
  tbGraph.AutoSize := true;
  tbAxes.AutoSize := true;
  tbSeries.AutoSize := true;
  tbLegend.AutoSize := true;
end;

procedure TfrmGraphPackageDlg.imgNoMajorGridClick(Sender: TObject);
begin
  currentSubGraph.XGridLines := false;
  currentSubGraph.YGridLines := false;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.imgNoMinorGridClick(Sender: TObject);
begin
  currentSubGraph.XMinorGridLines := false;
  currentSubGraph.YMinorGridLines := false;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.imgXMajorGridClick(Sender: TObject);
begin
  currentSubGraph.XGridLines := true;
  currentSubGraph.YGridLines := false;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.imgXMinorGridClick(Sender: TObject);
begin
  currentSubGraph.XMinorGridLines := true;
  currentSubGraph.YMinorGridLines := false;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.imgXYMajorGridClick(Sender: TObject);
begin
  currentSubGraph.XGridLines := true;
  currentSubGraph.YGridLines := true;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.imgXYMinorGridClick(Sender: TObject);
begin
  currentSubGraph.XMinorGridLines := true;
  currentSubGraph.YMinorGridLines := true;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.imgYMajorGridClick(Sender: TObject);
begin
  currentSubGraph.XGridLines := false;
  currentSubGraph.YGridLines := true;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.imgYMinorGridClick(Sender: TObject);
begin
  currentSubGraph.XMinorGridLines := false;
  currentSubGraph.YMinorGridLines := true;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.lbSeriesChange(Sender: TObject);
var index : integer;
    YColumnIndex : integer;
    i : integer;
begin
  if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  lbYColumns.Clear;
  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;
  for i := 0 to currentSubGraph.dataBlocks[index].columns.Count - 1 do
      lbYColumns.Items.Add (currentSubGraph.dataBlocks[index].columns[i].name);
  lbYColumns.ItemIndex := 0;
end;


procedure TfrmGraphPackageDlg.lbYColumnsChange(Sender: TObject);
var YColumnIndex : integer;
    index : integer;
begin
  if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  // We only have one data block, block zero
  //index:= currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  index := 0;
  YColumnIndex := lbYColumns.ItemIndex;

//  if currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].gradientType = gtNone then
//     cbSymbolFillColor.Color := currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].fillColor
//  else
//     begin
//     cbSymbolFillColor.Color := currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].fillColorStart;
//     cbSymbolEndColor.Color := currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].fillColorEnd;
//     end;

  cbSymbolOutlineColor.Color := currentSubGraph.dataBlocks[0].columns[YColumnIndex].symbol.outlineColor;

  cboLineColor.Color := currentSubGraph.dataBlocks[0].columns[YColumnIndex].lineDetails.color;

  tkSymbolRadius.Value := currentSubGraph.dataBlocks[0].columns[YColumnIndex].symbol.diameterInCms*300;
  tkSymbolOutlineThickness.Value := currentSubGraph.dataBlocks[0].columns[YColumnIndex].symbol.outlineInCms*300;

  tkLineThickness.value := currentSubGraph.dataBlocks[0].columns[YColumnIndex].lineDetails.ThicknessInSkiaUnits;

  cboSymbols.ItemIndex := ord (currentSubGraph.dataBlocks[0].columns[YColumnIndex].Symbol.symType);
  cboLineStyle.ItemIndex := ord (currentSubGraph.dataBlocks[0].columns[YColumnIndex].lineDetails.Style);

//  if currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].gradientType = gtNone then
//     chkGradient.IsChecked := false
//  else
//     chkGradient.IsChecked := true;

  chkLinesVisible.IsChecked := currentSubGraph.dataBlocks[0].columns[YColumnIndex].lineDetails.Visible;
  chkSymbolsVisible.IsChecked := currentSubGraph.dataBlocks[0].columns[YColumnIndex].symbol.Visible;
end;


procedure TfrmGraphPackageDlg.SpinGraphBorderWidthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  currentSubGraph.GraphBorderThicknessInSkiaUnits := SpinGraphBorderWidth.Value;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.SpinNumberOfXMajorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.NumberOfXTicks  := trunc (SpinNumberOfXMajorTicks.Value);
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.SpinNumberOfXMinorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.NumXMinorTicks := trunc (SpinNumberOfXMinorTicks.Value);
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.SpinNumberOfYMajorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.NumberOfYTicks  := trunc (SpinNumberOfYMajorTicks.Value);
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.SpinNumberOfYMinorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.NumYMinorTicks := trunc (SpinNumberOfYMinorTicks.Value);
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.SpinXMajorGridThicknessChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.XMajorGridThicknessInSkiaUnits := SpinXMajorGridThickness.Value;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.SpinXMinorGridWidthChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.XMinorGridThicknessInSkiaUnits := SpinXMinorGridWidth.Value;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.spinYMajorGridThicknessChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.YMajorGridThicknessInSkiaUnits := spinYMajorGridThickness.Value;
  currentSubGraph.YMajorGridThicknessInSkiaUnits := SpinYMajorGridThickness.Value;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.SpinYMinorGridWidthChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.YMinorGridThicknessInSkiaUnits := SpinYMinorGridWidth.Value;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.tkSymbolRadiusChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.diameterInCms := tkSymbolRadius.value/300;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.trackGraphBorderWidthChange(Sender: TObject);
begin
end;


// Converts a size in cms to pixels units according to the current device
function computePhysicalSize(sizeCm: double): single;
begin
  result := 1 * sizeCm / CmsInOneInch * CurrentXPixelsPerInch;
  // if result < 1.5 then
  // result := 1; // avoid antialiasing artifacts
end;


procedure TfrmGraphPackageDlg.tkFrameGapChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  currentSubGraph.legend.frameGapInCms := tkFrameGap.Value/150;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.tkLegendFrameThicknessChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  currentSubGraph.legend.frameBorderThickness := tkLegendFrameThickness.Value/10;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.tkLegendLineLengthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  currentSubGraph.legend.lineLengthInCms := tkLegendLineLength.Value/50;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.tkLineThicknessChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

   if lbSeries.ItemIndex = -1 then
      exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].lineDetails.ThicknessInSkiaUnits := tkLineThickness.value;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.tkSymbolOutlineThicknessChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.outlineInCms := tkSymbolOutlineThickness.value/300;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.btnOKClick(Sender: TObject);
begin
  Close;
end;


procedure TfrmGraphPackageDlg.cbSymbolEndColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  if chkGradient.IsChecked then
     currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.fillColorEnd := cbSymbolEndColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cbLegendFrameColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  currentSubGraph.legend.outlineColor := cbLegendFrameColor.color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cbLegendInteriorColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  currentSubGraph.legend.interiorColor := cboLegendInteriorColor.color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboBackgroundColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  (subgraph.parentGraph as TRRGraph).backgroundColor := cboBackgroundColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboGraphBorderColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  currentSubGraph.GraphBorderColor := cboGraphBorderColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboGraphDrawingAreaChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  currentSubGraph.GraphBackgroundColor := cboGraphDrawingArea.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboLineColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].lineDetails.Color := cboLineColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboLineStyleChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].lineDetails.Style := TLineStyle(cboLineStyle.ItemIndex);
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboMainTitleColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.mainTitleObject.textProperties.fontColor := cboMainTitleColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboSymbolsChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.symType := TSymbolType (cboSymbols.ItemIndex);
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboXAxisColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  currentSubGraph.XAxisColor := cboXAxisColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboXMajorColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  currentSubGraph.XMajorGridColor := cboXMajorColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboXMinorColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  currentSubGraph.XMinorGridColor := cboXMinorColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboXTitleColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.XAxisTitleObject.textProperties.fontColor := cboXTitleColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboYAxisColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  currentSubGraph.YAxisColor := cboYAxisColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboYTitleColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.YAxisTitleObject.textProperties.fontColor := cboYTitleColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboYMajorColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  currentSubGraph.YMajorGridColor := cboYMajorColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cboYMinorColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  currentSubGraph.YMinorGridColor := cboYMinorColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cbSymbolFillColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  if chkGradient.IsChecked then
     begin
     currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.fillColorStart := cbSymbolFillColor.Color;
     end
  else
     currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.fillColor := cbSymbolFillColor.Color;

  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.cbSymbolOutlineColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.outlineColor := cbSymbolOutlineColor.Color;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.chkYAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.bDrawYAxisTitle := chkYAxisTitle.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.chkAutoScaleBothAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.AutoXScaling := chkAutoScaleBothAxes.IsChecked;
  currentSubGraph.AutoYScaling := chkAutoScaleBothAxes.IsChecked;

  fireEvent := false;
  chkAutoXScaling.IsChecked := true;
  chkAutoYScaling.IsChecked := true;
  fireEvent := true;

  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkAutoXScalingChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.AutoXScaling := chkAutoXScaling.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkAutoYScalingChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.AutoYScaling := chkAutoYScaling.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkLegendFrameVisibleChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  currentSubGraph.legend.frameVisible := chkLegendFrameVisible.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkGradientChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
// HMS
//   if not fireEvent then
//     exit;
//
//  if lbSeries.ItemIndex = -1 then
//     exit;
//
//  index := currentSubGraph.data.find (lbSeries.items[lbSeries.ItemIndex]);
//  YColumnIndex := lbYColumns.ItemIndex;
//
//  if chkGradient.IsChecked then
//     begin
//     currentSubGraph.data[index].YSymbols[YColumnIndex].gradientType := gtVertLinear;
//     currentSubGraph.data[index].YSymbols[YColumnIndex].fillColorStart := cbSymbolFillColor.Color;
//     currentSubGraph.data[index].YSymbols[YColumnIndex].fillColorEnd := cbSymbolEndColor.Color;
//     end
//  else
//     begin
//     currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].gradientType := gtNone;
//     currentSubGraph.data[index].YSymbols[YColumnIndex].fillColor := cbSymbolFillColor.Color;
//     end;
//
//  if chkGradient.IsChecked then
//     begin
//     lblStartColor.Visible := true;
//     lblEndColor.Visible := true;
//     cbSymbolEndColor.Visible := true;
//     end
//  else
//     begin
//     lblStartColor.Visible := false;
//     lblEndColor.Visible := false;
//     cbSymbolEndColor.Visible := false;
//     end;
//  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkGraphBorderChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.GraphBorder := chkGraphBorder.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkLegendVisibleChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

   index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  currentSubGraph.legend.visible := chkLegendVisible.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkLinesVisibleChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].lineDetails.Visible := chkLinesVisible.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkLogBothAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  if (subgraph.properties.FWorldXmin <= 0) or
     (subgraph.properties.FWorldXmax <= 0) or
     (subgraph.properties.FWorldYmin <= 0) or
     (subgraph.properties.FWorldYmax <= 0) then
     begin
     showmessage ('One or more of the axes has zero or negative values');
     chkLogBothAxes.IsChecked := false;
     end
  else
     begin
     currentSubGraph.LogXAxis := chkLogBothAxes.IsChecked;
     currentSubGraph.LogYAxis := chkLogBothAxes.IsChecked;
     (subgraph.parentGraph as TRRGraph).redraw;
     end;
end;


procedure TfrmGraphPackageDlg.chkLogXAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  if (subgraph.properties.FWorldXmin <= 0) or
     (subgraph.properties.FWorldXmax <= 0) then
     begin
     showmessage ('To log the X axis its range must be greater than zero');
     chkLogXAxes.IsChecked := false;
     end
  else
     begin
     currentSubGraph.LogXAxis := chkLogXAxes.IsChecked;
     (subgraph.parentGraph as TRRGraph).redraw;
     end;
end;

procedure TfrmGraphPackageDlg.chkLogYAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  if (subgraph.properties.FWorldYmin <= 0) or
     (subgraph.properties.FWorldYmax <= 0) then
     begin
     showmessage ('To log the Y axis its range must be greater than zero');
     chkLogYAxes.IsChecked := false;
     end
  else
     begin
     currentSubGraph.LogYAxis := chkLogYAxes.IsChecked;
     (subgraph.parentGraph as TRRGraph).redraw;
     end;
end;


procedure TfrmGraphPackageDlg.chkMainTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.bDrawMainTitle := chkMainTitle.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TfrmGraphPackageDlg.chkSymbolsVisibleChange(Sender: TObject);
var index, YColumnIndex : integer;
    datacolumn : TDataColumn;
begin
   if not fireEvent then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  if lbSeries.ItemIndex = -1 then
     exit;

  datacolumn := currentSubGraph.dataBlocks[0].columns.find (lbSeries.items[lbSeries.ItemIndex], index);
  YColumnIndex := lbYColumns.ItemIndex;

  currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.Visible := chkSymbolsVisible.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.chkXAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  currentSubGraph.bDrawXAxisTitle := chkXAxisTitle.IsChecked;
  (subgraph.parentGraph as TRRGraph).redraw;
end;

procedure TfrmGraphPackageDlg.copyPropertiesFromDlg (gp : TSubGraphProperties);
var i : integer;
begin
  // HMS
  //gp.mainTitleObject.value := edtMainTitle.Text;
  //gp.xaxisTitleObject.value := edtXAxisTitle.Text;

  gp.XAxisColor := cboXAxisColor.Color;
  gp.YAxisColor := cboYAxisColor.Color;

  gp.XMajorGridColor := cboXMajorColor.Color;
  gp.XMinorGridColor := cboXMinorColor.Color;
  gp.YMajorGridColor := cboYMajorColor.Color;
  gp.YMinorGridColor := cboYMinorColor.Color;

  gp.GraphBorder := chkGraphBorder.IsChecked;
  gp.bDrawMainTitle := chkMainTitle.IsChecked;
  gp.bDrawXAxisTitle := chkXAxisTitle.IsChecked;

  gp.LogXAxis := chkLogXAxes.IsChecked;
  gp.LogYAxis := chkLogYAxes.IsChecked;

  gp.UserScale_Xmin := strtofloat (edtXMinimum.Text);
  gp.UserScale_Xmax := strtofloat (edtXMaximum.Text);
  gp.UserScale_Ymin := strtofloat (edtYMinimum.Text);
  gp.UserScale_Ymax := strtofloat (edtYMaximum.Text);

  gp.AutoXScaling := chkAutoXScaling.IsChecked;
  gp.AutoYScaling := chkAutoYScaling.IsChecked;

  if lbSeries.ItemIndex = -1 then
     exit;

  for i := 0 to gp.dataBlocks.Count - 1 do
      currentSubGraph.dataBlocks[i].columns[lbYColumns.ItemIndex].symbol.fillColor := cbSymbolFillColor.Color;
  for i := 0 to gp.dataBlocks.Count - 1 do
      currentSubGraph.dataBlocks[i].columns[lbYColumns.ItemIndex].symbol.outlineColor := cbSymbolOutlineColor.Color;
end;


procedure TfrmGraphPackageDlg.btnAppyClick(Sender: TObject);
begin
  if Assigned (subgraph) then
     begin
     copyPropertiesFromDlg (subgraph.properties);
     (subgraph.parentGraph as TRRGraph).redraw;
     end;
end;

procedure TfrmGraphPackageDlg.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmGraphPackageDlg.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

