unit ufPlotEditor;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Strutils,
  System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics,
  FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TabControl,
  FMX.Edit, FMX.Memo,
  FMX.Colors,
  FMX.ListBox,
  FMX.Controls.Presentation, FMX.EditBox, FMX.SpinBox, uLabelledTrackBar;

type
  TFrmPlotEditor = class(TForm)
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
    TabControlAxes: TTabControl;
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
    GroupBox1: TGroupBox;
    Label9: TLabel;
    cbSymbolFillColor: TComboColorBox;
    Layout4: TLayout;
    Layout5: TLayout;
    Label11: TLabel;
    lbYColumns: TListBox;
    Label12: TLabel;
    cbSymbolOutlineColor: TComboColorBox;
    GroupBox2: TGroupBox;
    Label13: TLabel;
    cboLineColor: TComboColorBox;
    Label14: TLabel;
    btnClose: TButton;
    cboSymbols: TComboBox;
    Label15: TLabel;
    chkGradient: TCheckBox;
    cbSymbolEndColor: TComboColorBox;
    lblStartColor: TLabel;
    lblEndColor: TLabel;
    chkAutoScaleBothAxes: TCheckBox;
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
    SpinNumberOfXMajorTicks: TSpinBox;
    GroupBox6: TGroupBox;
    chkShowXMajorTicks: TCheckBox;
    chkShowXMinorTicks: TCheckBox;
    Label2: TLabel;
    cboXAxisColor: TComboColorBox;
    Label3: TLabel;
    edtXMinimum: TEdit;
    Label4: TLabel;
    edtXMaximum: TEdit;
    chkAutoXScaling: TCheckBox;
    chkLogXAxes: TCheckBox;
    GroupBox7: TGroupBox;
    chkShowYMajorTicks: TCheckBox;
    chkShowYMinorTicks: TCheckBox;
    Label8: TLabel;
    cboYAxisColor: TComboColorBox;
    Label5: TLabel;
    edtYMinimum: TEdit;
    Label6: TLabel;
    edtYMaximum: TEdit;
    chkAutoYScaling: TCheckBox;
    chkLogYAxes: TCheckBox;
    Label43: TLabel;
    spinMainTitleFontSize: TSpinBox;
    Label44: TLabel;
    spinXAxisFontSize: TSpinBox;
    Label41: TLabel;
    spinXAxisLabelsFontSize: TSpinBox;
    Label42: TLabel;
    spinYAxisFontSize: TSpinBox;
    Label45: TLabel;
    spinYAxisLabelsFontSize: TSpinBox;
    GroupBox8: TGroupBox;
    rdoTopLeft: TRadioButton;
    rdoTopRight: TRadioButton;
    rdoBottomLeft: TRadioButton;
    rdoBottomRight: TRadioButton;
    tkLineThickness: TLabelledTrackBar;
    tkSymbolRadius: TLabelledTrackBar;
    tkSymbolOutlineThickness: TLabelledTrackBar;
    Label46: TLabel;
    chkSeriesVisible: TCheckBox;
    Label40: TLabel;
    SpinNumberOfYMajorTicks: TSpinBox;
    Label10: TLabel;
    SpinXMajorTickLength: TSpinBox;
    SpinXMinorTickLength: TSpinBox;
    SpinYMajorTickLength: TSpinBox;
    SpinYMinorTickLength: TSpinBox;
    Label47: TLabel;
    Label48: TLabel;
    cboYTickStyle: TComboBox;
    cboXTickStyle: TComboBox;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
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
    procedure spinXAxisLabelsFontSizeChange(Sender: TObject);
    procedure trackGraphBorderWidthChange(Sender: TObject);
    procedure spinYMajorGridThicknessChange(Sender: TObject);
    procedure SpinXMajorGridThicknessChange(Sender: TObject);
    procedure SpinXMinorGridWidthChange(Sender: TObject);
    procedure SpinYMinorGridWidthChange(Sender: TObject);
    procedure SpinGraphBorderWidthChange(Sender: TObject);
    procedure spinMainTitleFontSizeChange(Sender: TObject);
    procedure SpinNumberOfXMinorTicksChange(Sender: TObject);
    procedure SpinNumberOfYMinorTicksChange(Sender: TObject);
    procedure SpinNumberOfXMajorTicksChange(Sender: TObject);
    procedure SpinNumberOfYMajorTicksChange(Sender: TObject);
    procedure spinXAxisFontSizeChange(Sender: TObject);
    procedure spinYAxisFontSizeChange(Sender: TObject);
    procedure spinYAxisLabelsFontSizeChange(Sender: TObject);
    procedure rdoTopLeftChange(Sender: TObject);
    procedure chkSeriesVisibleChange(Sender: TObject);
    procedure chkShowXMajorTicksChange(Sender: TObject);
    procedure chkShowXMinorTicksChange(Sender: TObject);
    procedure chkShowYMajorTicksChange(Sender: TObject);
    procedure chkShowYMinorTicksChange(Sender: TObject);
    procedure SpinXMajorTickLengthChange(Sender: TObject);
    procedure SpinXMinorTickLengthChange(Sender: TObject);
    procedure SpinYMajorTickLengthChange(Sender: TObject);
    procedure SpinYMinorTickLengthChange(Sender: TObject);
    procedure cboXTickStyleChange(Sender: TObject);
    procedure cboYTickStyleChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fireEvent : boolean;
    procedure CopyPropertiesToEditor (OPlot : TObject);
    procedure CopyPropertiesFromEditor (OPLot : TObject);
  end;

var
  frmPlotEditor : TFrmPlotEditor;

implementation

{$R *.fmx}

Uses SkPlotPaintBox, uPlotSeries;

var Plot : TSkPlotPaintBox;

function Formatvalue (AValue : Single) : String;
begin
  result := FormatFloat('0.' + DupeString('0', 2), AValue);
end;


function ChangeColorAlpha(const AColor: TAlphaColor; const NewAlpha: Byte): TAlphaColor;
begin
  // Clears out the old alpha bits ($00FFFFFF) and shifts in the new alpha byte
  Result := (AColor and $00FFFFFF) or (TAlphaColor(NewAlpha) shl 24);
end;


procedure TFrmPlotEditor.CopyPropertiesToEditor (OPlot : TObject);
var i, j : integer;
    Marker : TMarkerShape;
    LineStyle : TLineStyle;
    TickStyle : TTickmarkDrawing;
    MarkerIndex : Integer;
begin
  fireEvent := false;
  Plot := TSkPlotPaintBox(OPlot);

  SpinXMajorGridThickness.Value := Plot.GridStyle.XMajorWidth;
  SpinYMajorGridThickness.Value := Plot.GridStyle.YMajorWidth;

  SpinYMinorGridWidth.Value := Plot.GridStyle.YMinorWidth;
  SpinYMinorGridWidth.Value := Plot.GridStyle.YMinorWidth;

  SpinNumberOfXMinorTicks.Value := Plot.GridStyle.XMinorDivisions;
  SpinNumberOfYMinorTicks.Value := Plot.GridStyle.YMinorDivisions;

  SpinNumberOfXMajorTicks.Value := Plot.GridStyle.XMajorDivisions;
  SpinNumberOfYMajorTicks.Value := Plot.GridStyle.YMajorDivisions;

  edtMainTitle.Text  := Plot.ChartTitle.Text;
  chkMainTitle.IsChecked := Plot.ChartTitle.Visible;
  edtXAxisTitle.Text := Plot.XAxisTitle.Text;
  chkXAxisTitle.IsChecked := Plot.XAxisTitle.Visible;
  edtYAxisTitle.Text := Plot.YAxisTitle.Text;
  chkYAxisTitle.IsChecked := Plot.YAxisTitle.Visible;

  cboGraphDrawingArea.Color := Plot.PlotAreaColor;
  cboGraphBorderColor.Color := Plot.PlotBorderColor;
  SpinGraphBorderWidth.Value := Plot.PlotBorderWidth;
  chkGraphBorder.IsChecked := Plot.PlotBorderVisible;
  cboBackgroundColor.Color := Plot.BackGroundColor;

//  cboXAxisColor.Color := gp.XAxisColor;
//  cboYAXisColor.Color := gp.YAxisColor;

  cboXMajorColor.Color := Plot.GridStyle.XMajorColor;
  cboYMajorColor.Color := Plot.GridStyle.YMajorColor;

  cboXMinorColor.Color := Plot.GridStyle.XMinorColor;
  cboYMinorColor.Color := Plot.GridStyle.YMinorColor;

  chkShowXMajorTicks.IsChecked := Plot.AxisStyle.XMajorTicksVisible;
  chkShowXMinorTicks.IsChecked := Plot.AxisStyle.XMinorTicksVisible;

  chkShowYMajorTicks.IsChecked := Plot.AxisStyle.YMajorTicksVisible;
  chkShowYMinorTicks.IsChecked := Plot.AxisStyle.YMinorTicksVisible;

  SpinXMajorTickLength.Value := Plot.AxisStyle.XMajorTickLength;
  SpinXMinorTickLength.Value := Plot.AxisStyle.XMinorTickLength;

  SpinYMajorTickLength.Value := Plot.AxisStyle.YMajorTickLength;
  SpinYMinorTickLength.Value := Plot.AxisStyle.YMinorTickLength;

  SpinXAxisFontSize.Value := Plot.XAxisTitle.FontSize;
  SpinYAxisFontSize.Value := Plot.YAxisTitle.FontSize;
  SpinMainTitleFontSize.Value := Plot.ChartTitle.FontSize;

  SpinXAxisLabelsFontSize.Value := Plot.XAxisFontSize;
  SpinYAxisLabelsFontSize.Value := Plot.YAxisFontSize;

  cboMainTitleColor.Color := Plot.ChartTitle.Color;
  cboXTitleColor.Color := Plot.XAxisTitle.Color;
  cboYTitleColor.Color := Plot.YAxisTitle.Color;

  chkLogXAxes.IsChecked := Plot.AxisStyle.LogX;
  chkLogYAxes.IsChecked := Plot.AxisStyle.LogY;

  edtXMinimum.Text := floattostr (Plot.AxisLimits.MinX);
  edtXMaximum.Text := floattostr (Plot.AxisLimits.MaxX);
  edtYMinimum.Text := floattostr (Plot.AxisLimits.MinY);
  edtYMaximum.Text := floattostr (Plot.AxisLimits.MaxY);

  chkAutoXScaling.IsChecked := Plot.AutoXScaling;
  chkAutoYScaling.IsChecked := Plot.AutoYScaling;

  if Plot.AutoXScaling and Plot.AutoYScaling then
     chkAutoScaleBothAxes.IsChecked := true
  else
     chkAutoScaleBothAxes.IsChecked := false;

  if Plot.Series.Count > 0 then
     begin
     chkLinesVisible.IsChecked := Plot.Series[0].LineVisible;
     chkSymbolsVisible.IsChecked := Plot.Series[0].MarkerVisible;
     chkSeriesVisible.IsChecked := Plot.Series[0].SeriesVisible;
     cbSymbolFillColor.Color := Plot.Series[0].MarkerFillColor;
     cbSymbolOutlineColor.Color := Plot.Series[0].MarkerStrokeColor;
     //lblSymbolRadius.Text := FormatValue (tkSymbolRadius.Value);
     tkSymbolRadius.Value := Plot.Series[0].MarkerSize;
     tkSymbolOutlineThickness.Value := Plot.Series[0].MarkerStrokeWidth;

     tkLineThickness.value := Plot.Series[0].LineWidth;
     //lblLineThicknessTrackbar.Text := Formatvalue(tkLineThickness.value);
     cboLineColor.Color := Plot.Series[0].LineColor;
     end;

  chkLegendVisible.IsChecked := Plot.LegendStyle.Visible;
  chkLegendFrameVisible.IsChecked :=  Plot.LegendStyle.BorderVisible;
  cbLegendFrameColor.color := Plot.LegendStyle.BorderColor;
  cboLegendInteriorColor.Color := Plot.LegendStyle.BackgroundColor;
  cbolegendInteriorColor.Color := ChangeColorAlpha(cbolegendInteriorColor.Color, Round (Plot.LegendStyle.BackgroundOpacity*255));
  tkLegendFrameThickness.Value := Plot.LegendStyle.BorderWidth;

  case Plot.LegendStyle.Location of
     llTopRight : rdoTopRight.IsChecked := True;
     llTopLeft : rdoTopLeft.IsChecked := True;
     llBottomRight : rdoBottomRight.IsChecked := True;
     llBottomLeft : rdoBottomleft.IsChecked := True;
  end;
  lbYColumns.Clear;
  // We only do block zero here as tha tis the defualt one that is visible at start up
  for i := 0 to Plot.Series.Count - 1 do
         lbYColumns.Items.Add (Plot.Series[i].Name);

  cboSymbols.Clear;
  for Marker := Low(TMarkerShape) to High(TMarkerShape) do
      cboSymbols.Items.Add (MarkerShapeNames[Marker]);
  if Plot.Series.Count > 0 then
     MarkerIndex := Ord (Plot.Series[0].MarkerShape)
  else
     MarkerIndex := 0;

  cboSymbols.ItemIndex := MarkerIndex;

  cboLineStyle.Clear;
  for LineStyle := Low(TLineStyle) to High(TLineStyle) do
      cboLineStyle.Items.Add (LineStyleNames[LineStyle]);
  cboLineStyle.ItemIndex := 0;

  cboXTickStyle.Clear;
  for TickStyle := Low(TTickmarkDrawing) to High(TTickmarkDrawing) do
      cboXTickStyle.Items.Add (TickDrawingNames[TickStyle]);
  cboXTickStyle.ItemIndex := Ord (Plot.AxisStyle.XTickDrawing);

  cboYTickStyle.Clear;
  for TickStyle := Low(TTickmarkDrawing) to High(TTickmarkDrawing) do
      cboYTickStyle.Items.Add (TickDrawingNames[TickStyle]);
  cboYTickStyle.ItemIndex := Ord (Plot.AxisStyle.YTickDrawing);

  lbYColumns.ItemIndex := 0;

  fireEvent := true;
end;



procedure TFrmPlotEditor.CopyPropertiesFromEditor (OPLot : TObject);
var i : integer;
    Marker : TMarkerShape;
    LineStyle : TLineStyle;
    SeriesIndex : Integer;
begin
  //Plot.ChartTitle.Text := edtMainTitle.Text;
  //Plot.XAxisTitle.Text := edtXAxisTitle.Text;

//  gp.XAxisColor := cboXAxisColor.Color;
//  gp.YAxisColor := cboYAxisColor.Color;
//
//  gp.XMajorGridColor := cboXMajorColor.Color;
//  gp.XMinorGridColor := cboXMinorColor.Color;
//  gp.YMajorGridColor := cboYMajorColor.Color;
//  gp.YMinorGridColor := cboYMinorColor.Color;
//
//  gp.GraphBorder := chkGraphBorder.IsChecked;
//  gp.bDrawMainTitle := chkMainTitle.IsChecked;
//  gp.bDrawXAxisTitle := chkXAxisTitle.IsChecked;
//
  Plot.AxisStyle.LogX := chkLogXAxes.IsChecked;
  Plot.AxisStyle.LogY := chkLogYAxes.IsChecked;

  Plot.AxisLimits.MinX := strtofloat (edtXMinimum.Text);
  Plot.AxisLimits.MaxX := strtofloat (edtXMaximum.Text);
  Plot.AxisLimits.MinY := strtofloat (edtYMinimum.Text);
  Plot.AxisLimits.MaxY := strtofloat (edtYMaximum.Text);

  Plot.AutoXScaling := chkAutoXScaling.IsChecked;
  Plot.AutoYScaling := chkAutoYScaling.IsChecked;

//  for i := 0 to gp.dataBlocks.Count - 1 do
//      currentSubGraph.dataBlocks[i].columns[lbYColumns.ItemIndex].symbol.fillColor := cbSymbolFillColor.Color;
//  for i := 0 to gp.dataBlocks.Count - 1 do
//      currentSubGraph.dataBlocks[i].columns[lbYColumns.ItemIndex].symbol.outlineColor := cbSymbolOutlineColor.Color;
end;


procedure TFrmPlotEditor.edtMainTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.ChartTitle.Text := edtMainTitle.Text;
  Plot.redraw;
end;


procedure TFrmPlotEditor.edtXAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.XAxisTitle.Text := edtXAxisTitle.Text;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.edtXMaximumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisLimits.MaxX := strtofloat (edtXMaximum.Text);
  Plot.redraw;
end;

procedure TFrmPlotEditor.edtXMinimumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisLimits.MinX := strtofloat (edtXMinimum.Text);
  Plot.Redraw;
end;

procedure TFrmPlotEditor.edtYAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.YAxisTitle.Text := edtYAxisTitle.Text;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.edtYMaximumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisLimits.MaxY := strtofloat (edtYMaximum.Text);
  Plot.redraw;
end;

procedure TFrmPlotEditor.edtYMinimumChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisLimits.MinY := strtofloat (edtYMinimum.Text);
  Plot.redraw;
end;

procedure TFrmPlotEditor.FormCreate(Sender: TObject);
begin
  tbGraph.AutoSize := true;
  tbAxes.AutoSize := true;
  tbSeries.AutoSize := true;
  tbLegend.AutoSize := true;
end;

procedure TFrmPlotEditor.imgNoMajorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMajorVisible := false;
  Plot.GridStyle.YMajorVisible := false;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.imgNoMinorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMinorVisible := false;
  Plot.GridStyle.YMinorVisible := false;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.imgXMajorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMajorVisible := true;
  Plot.GridStyle.YMajorVisible := false;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.imgXMinorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMinorVisible := true;
  Plot.GridStyle.YMinorVisible := false;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.imgXYMajorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMajorVisible := true;
  Plot.GridStyle.YMajorVisible := true;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.imgXYMinorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMinorVisible := true;
  Plot.GridStyle.YMinorVisible := true;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.imgYMajorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMajorVisible := false;
  Plot.GridStyle.YMajorVisible := true;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.imgYMinorGridClick(Sender: TObject);
begin
  Plot.GridStyle.XMinorVisible := false;
  Plot.GridStyle.YMinorVisible := true;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.lbSeriesChange(Sender: TObject);
var index : integer;
    YColumnIndex : integer;
    i : integer;
begin
  if not fireEvent then
     exit;

  lbYColumns.Clear;
  //index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);
  YColumnIndex := lbYColumns.ItemIndex;
  //for i := 0 to currentSubGraph.dataBlocks[index].columns.Count - 1 do
  //    lbYColumns.Items.Add (currentSubGraph.dataBlocks[index].columns[i].name);
  lbYColumns.ItemIndex := 0;
end;


procedure TFrmPlotEditor.lbYColumnsChange(Sender: TObject);
var YColumnIndex : integer;
    index : integer;
begin
  if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  index := 0;
  YColumnIndex := lbYColumns.ItemIndex;

//  if currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].gradientType = gtNone then
//     cbSymbolFillColor.Color := currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].fillColor
//  else
//     begin
//     cbSymbolFillColor.Color := currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].fillColorStart;
//     cbSymbolEndColor.Color := currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].fillColorEnd;
//     end;

  cbSymbolFillColor.Color := Plot.Series[YColumnIndex].MarkerFillColor;
  cbSymbolOutlineColor.Color := Plot.Series[YColumnIndex].MarkerStrokeColor;
  tkSymbolRadius.Value := Plot.Series[YColumnIndex].MarkerSize;
  tkSymbolOutlineThickness.Value := Plot.Series[YColumnIndex].MarkerStrokeWidth;

  tkLineThickness.value := Plot.Series[YColumnIndex].LineWidth;
  cboLineColor.Color := Plot.Series[YColumnIndex].LineColor;

  cboSymbols.ItemIndex := ord (Plot.Series[YColumnIndex].MarkerShape);
  cboLineStyle.ItemIndex := ord (Plot.Series[YColumnIndex].LineStyle);

//  if currentSubGraph.dataBlock[index].YSymbols[YColumnIndex].gradientType = gtNone then
//     chkGradient.IsChecked := false
//  else
//     chkGradient.IsChecked := true;

  chkLinesVisible.IsChecked := Plot.Series[YColumnIndex].LineVisible;
  chkSymbolsVisible.IsChecked := Plot.Series[YColumnIndex].MarkerVisible;
  chkSeriesVisible.IsChecked := Plot.Series[YColumnIndex].SeriesVisible;
end;


procedure TFrmPlotEditor.rdoTopLeftChange(Sender: TObject);
var
  SelectedButton: TRadioButton;
begin
   if not fireEvent then
     exit;

  // Ensure the Sender is actually a TRadioButton
  if Sender is TRadioButton then
  begin
    SelectedButton := TRadioButton(Sender);

    // FMX fires OnChange for BOTH the button being unchecked AND the one being checked.
    // We only want to execute code for the button that was JUST SELECTED.
    if SelectedButton.IsChecked then
    begin
      // Execute your logic here based on the selected button
      if SelectedButton = rdoTopLeft then
        Plot.LegendStyle.Location := TLegendLocation.llTopLeft
      else if SelectedButton = rdoTopRight then
        Plot.LegendStyle.Location := TLegendLocation.llTopRight
      else if SelectedButton = rdoBottomLeft then
        Plot.LegendStyle.Location := TLegendLocation.llBottomLeft
      else if SelectedButton = rdoBottomRight then
        Plot.LegendStyle.Location := TLegendLocation.llBottomRight
    end;
    Plot.Redraw;
  end;
end;

procedure TFrmPlotEditor.SpinGraphBorderWidthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.PlotBorderWidth := SpinGraphBorderWidth.Value;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.SpinNumberOfXMajorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.XMajorDivisions := trunc (SpinNumberOfXMajorTicks.Value);
  Plot.Redraw;
end;

procedure TFrmPlotEditor.SpinNumberOfXMinorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.XMinorDivisions := trunc (SpinNumberOfXMinorTicks.Value);
  Plot.Redraw;
end;


procedure TFrmPlotEditor.SpinNumberOfYMajorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.YMajorDivisions  := trunc (SpinNumberOfYMajorTicks.Value);
  Plot.Redraw;
end;

procedure TFrmPlotEditor.SpinNumberOfYMinorTicksChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.yMinorDivisions := trunc (SpinNumberOfYMinorTicks.Value);
  Plot.Redraw;
end;

procedure TFrmPlotEditor.SpinXMajorGridThicknessChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.XMajorWidth := SpinXMajorGridThickness.Value;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.SpinXMinorGridWidthChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.XMinorWidth := SpinXMinorGridWidth.Value;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.spinYMajorGridThicknessChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.YMajorWidth := spinYMajorGridThickness.Value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.SpinYMinorGridWidthChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.GridStyle.YMinorWidth := SpinYMinorGridWidth.Value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.SpinXMajorTickLengthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.AxisStyle.XMajorTickLength := SpinXMajorTickLength.Value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.SpinXMinorTickLengthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.AxisStyle.XMinorTickLength := SpinXMinorTickLength.Value;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.SpinYMajorTickLengthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.AxisStyle.YMajorTickLength := SpinYMajorTickLength.Value;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.SpinYMinorTickLengthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.AxisStyle.YMinorTickLength := SpinYMinorTickLength.Value;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.cboXTickStyleChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.AxisStyle.XTickDrawing := TTickmarkDrawing(cboXTickStyle.ItemIndex);
  Plot.Redraw;
end;

procedure TFrmPlotEditor.cboYTickStyleChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.AxisStyle.YTickDrawing := TTickmarkDrawing(cboYTickStyle.ItemIndex);
  Plot.Redraw;
end;

// -------------------------------------------------------------------------

procedure TFrmPlotEditor.tkSymbolRadiusChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  //lblSymbolRadius.Text := Formatvalue(tkSymbolRadius.value);

  Plot.Series[YColumnIndex].MarkerSize := tkSymbolRadius.value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.tkSymbolOutlineThicknessChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].MarkerStrokeWidth := tkSymbolOutlineThickness.value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboSymbolsChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].MarkerShape := TMarkerShape (cboSymbols.ItemIndex);
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cbSymbolFillColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  if chkGradient.IsChecked then
     begin
     //currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.fillColorStart := cbSymbolFillColor.Color;
     end
  else
     Plot.Series[YColumnIndex].MarkerFillColor := cbSymbolFillColor.Color;

  Plot.Redraw;
end;


procedure TFrmPlotEditor.cbSymbolOutlineColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].MarkerStrokeColor := cbSymbolOutlineColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cbSymbolEndColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  //if chkGradient.IsChecked then
  //   currentSubGraph.dataBlocks[index].columns[YColumnIndex].symbol.fillColorEnd := cbSymbolEndColor.Color;
  //(subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TFrmPlotEditor.trackGraphBorderWidthChange(Sender: TObject);
begin
end;


// Converts a size in cms to pixels units according to the current device
function computePhysicalSize(sizeCm: double): single;
begin
  //result := 1 * sizeCm / CmsInOneInch * CurrentXPixelsPerInch;
  // if result < 1.5 then
  // result := 1; // avoid antialiasing artifacts
end;


procedure TFrmPlotEditor.tkFrameGapChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  //currentSubGraph.legendObject.frameGapInCms := tkFrameGap.Value/150;
  //(subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TFrmPlotEditor.tkLegendFrameThicknessChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  if tkLegendFrameThickness.Value = 0 then
     Plot.LegendStyle.BorderWidth := 0   // ensure 1 pixel thick lines
  else
     Plot.LegendStyle.BorderWidth := tkLegendFrameThickness.Value/10;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cbLegendFrameColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  Plot.LegendStyle.BorderColor := cbLegendFrameColor.color;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.cbLegendInteriorColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  Plot.LegendStyle.BackgroundColor := cboLegendInteriorColor.color;
  Plot.LegendStyle.BackgroundOpacity := TAlphaColorRec(cbolegendInteriorColor.Color).A/255;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.tkLegendLineLengthChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  //currentSubGraph.legendObject.lineLengthInCms := tkLegendLineLength.Value/50;
  //(subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TFrmPlotEditor.chkLegendVisibleChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  Plot.LegendStyle.visible := chkLegendVisible.IsChecked;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.chkLegendFrameVisibleChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  Plot.LegendStyle.BorderVisible := chkLegendFrameVisible.IsChecked;
  Plot.Redraw;;
end;


procedure TFrmPlotEditor.tkLineThicknessChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
  if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  //lblLineThicknessTrackbar.Text := Formatvalue(tkLineThickness.value);

  Plot.Series[YColumnIndex].LineWidth := tkLineThickness.value;
  Plot.Redraw;
end;



procedure TFrmPlotEditor.btnOKClick(Sender: TObject);
begin
  Close;
end;


procedure TFrmPlotEditor.cboBackgroundColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.BackGroundColor := cboBackgroundColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboGraphBorderColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.PlotBorderColor := cboGraphBorderColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboGraphDrawingAreaChange(Sender: TObject);
begin
  if not fireEvent then
    exit;

  Plot.PlotAreaColor := cboGraphDrawingArea.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboLineColorChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].LineColor := cboLineColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboLineStyleChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].LineStyle := TLineStyle(cboLineStyle.ItemIndex);
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboMainTitleColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.ChartTitle.Color := cboMainTitleColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboXMajorColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.GridStyle.XMajorColor := cboXMajorColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboXMinorColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.GridStyle.XMinorColor := cboXMinorColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboXTitleColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.XAxisTitle.Color := cboXTitleColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboXAxisColorChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  //currentSubGraph.XAxisColor := cboXAxisColor.Color;
  //(subgraph.parentGraph as TRRGraph).redraw;
end;


procedure TFrmPlotEditor.cboYAxisColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  //index := currentSubGraph.dataBlocks.find (lbSeries.items[lbSeries.ItemIndex]);

  //currentSubGraph.YAxisColor := cboYAxisColor.Color;
  //(subgraph.parentGraph as TRRGraph).redraw;
end;



procedure TFrmPlotEditor.cboYTitleColorChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.YAxisTitle.Color := cboYTitleColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboYMajorColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;
  Plot.GridStyle.YMajorColor := cboYMajorColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.cboYMinorColorChange(Sender: TObject);
var index : integer;
begin
   if not fireEvent then
     exit;

  Plot.GridStyle.YMinorColor := cboYMinorColor.Color;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.chkYAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.YAxisTitle.Visible := chkYAxisTitle.IsChecked;
  Plot.Redraw;
end;



procedure TFrmPlotEditor.chkAutoScaleBothAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AutoXScaling := chkAutoScaleBothAxes.IsChecked;
  Plot.AutoYScaling := chkAutoScaleBothAxes.IsChecked;

  fireEvent := false;
  chkAutoXScaling.IsChecked := true;
  chkAutoYScaling.IsChecked := true;
  fireEvent := true;

  Plot.Redraw;
end;

procedure TFrmPlotEditor.chkAutoXScalingChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AutoXScaling := chkAutoXScaling.IsChecked;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.chkAutoYScalingChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AutoYScaling := chkAutoYScaling.IsChecked;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.chkGradientChange(Sender: TObject);
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

procedure TFrmPlotEditor.chkGraphBorderChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.PlotBorderVisible := chkGraphBorder.IsChecked;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.chkLinesVisibleChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].LineVisible := chkLinesVisible.IsChecked;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.chkLogBothAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  if (Plot.AxisLimits.MinX <= 0) or
     (Plot.AxisLimits.MaxX <= 0) or
     (Plot.AxisLimits.MinY <= 0) or
     (Plot.AxisLimits.MaxY <= 0) then
     begin
     showmessage ('One or more of the axes has zero or negative values');
     chkLogBothAxes.IsChecked := false;
     end
  else
     begin
     Plot.AxisStyle.LogX := chkLogBothAxes.IsChecked;
     Plot.AxisStyle.LogY := chkLogBothAxes.IsChecked;
     Plot.Redraw;
    end;
end;


procedure TFrmPlotEditor.chkLogXAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  if (Plot.AxisLimits.MinX <= 0) or
     (Plot.AxisLimits.MaxX <= 0) then
     begin
     if Plot.AxisLimits.MinX = 0 then
        begin
        Plot.AxisStyle.LogX := chkLogXAxes.IsChecked;
        Plot.AxisLimits.MinX := 0.1;
        edtXMinimum.Text := '0.1';
        Plot.Redraw;
        end
     else
        begin
        showmessage ('To log the X axis its range must be greater than zero');
        chkLogXAxes.IsChecked := false;
        end;
     end
  else
     begin
     Plot.AxisStyle.LogX := chkLogXAxes.IsChecked;
     Plot.Redraw;
     end;
end;

procedure TFrmPlotEditor.chkLogYAxesChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  if (Plot.AxisLimits.MinY <= 0) or
     (Plot.AxisLimits.MaxY <= 0) then
     begin
     if Plot.AxisLimits.MinY = 0 then
        begin
        Plot.AxisStyle.LogY := chkLogYAxes.IsChecked;
        Plot.AxisLimits.MinY := 0.1;
        edtYMinimum.Text := '0.1';
        Plot.Redraw;
        end
     else
        begin
        showmessage ('To log the Y axis its range must be greater than zero');
        chkLogYAxes.IsChecked := false;
        end;
     end
  else
     begin
     Plot.AxisStyle.LogY := chkLogYAxes.IsChecked;
     Plot.Redraw;
     end;
end;


procedure TFrmPlotEditor.chkMainTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.ChartTitle.Visible := chkMainTitle.IsChecked;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.chkSeriesVisibleChange(Sender: TObject);
var YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].SeriesVisible := chkSeriesVisible.IsChecked;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.chkShowXMajorTicksChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisStyle.XMajorTicksVisible := chkShowXMajorTicks.IsChecked;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.chkShowXMinorTicksChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisStyle.XMinorTicksVisible := chkShowXMinorTicks.IsChecked;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.chkShowYMajorTicksChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisStyle.YMajorTicksVisible := chkShowYMajorTicks.IsChecked;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.chkShowYMinorTicksChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.AxisStyle.YMinorTicksVisible := chkShowYMinorTicks.IsChecked;
  Plot.Redraw;
end;

procedure TFrmPlotEditor.chkSymbolsVisibleChange(Sender: TObject);
var index, YColumnIndex : integer;
begin
   if not fireEvent then
     exit;

  if Plot.Series.Count = 0 then
     exit;

  YColumnIndex := lbYColumns.ItemIndex;

  Plot.Series[YColumnIndex].MarkerVisible := chkSymbolsVisible.IsChecked;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.chkXAxisTitleChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  Plot.XAxisTitle.Visible := chkXAxisTitle.IsChecked;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.btnAppyClick(Sender: TObject);
begin
  //if Assigned (subgraph) then
     begin
     //copyPropertiesFromDlg (subgraph.properties);
     //(subgraph.parentGraph as TRRGraph).redraw;
     end;
end;

procedure TFrmPlotEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmPlotEditor.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmPlotEditor.spinMainTitleFontSizeChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.ChartTitle.FontSize := spinMainTitleFontSize.Value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.spinXAxisFontSizeChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.XAxisTitle.FontSize := spinXAxisFontSize.Value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.spinXAxisLabelsFontSizeChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.XAxisFontSize := spinXAxisLabelsFontSize.Value;
  Plot.Redraw;
end;


procedure TFrmPlotEditor.spinYAxisFontSizeChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.YAxisTitle.FontSize := spinYAxisFontSize.Value;
  Plot.redraw;
end;

procedure TFrmPlotEditor.spinYAxisLabelsFontSizeChange(Sender: TObject);
begin
   if not fireEvent then
     exit;

  Plot.YAxisFontSize := spinYAxisLabelsFontSize.Value;
  Plot.Redraw;
end;

end.

