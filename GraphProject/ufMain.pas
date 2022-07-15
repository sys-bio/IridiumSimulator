unit ufMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Math.Vectors, FMX.Layouts, FMX.Controls3D, FMX.Layers3D,
  FMX.Colors, FMX.ListBox, FMX.Edit, FMX.Ani, uRRDataSeries, uSubGraph,
  FMX.Menus, uCSVReader, FMX.Objects, uRRCommon, uPlottingPanel;

const
   VERSION = '0.7';

type
  TfmMain = class(TForm)
    Layout: TLayout;
    Layout1: TLayout;
    btnPlot: TButton;
    Layout2: TLayout;
    cboSymbolColor: TComboColorBox;
    Label1: TLabel;
    Label2: TLabel;
    cboSymbolOutlineColor: TComboColorBox;
    cboSymbolShape: TComboBox;
    Label3: TLabel;
    Label5: TLabel;
    cboLineColor: TComboColorBox;
    Label4: TLabel;
    edtSymbolOutlineWidth: TComboTrackBar;
    Label6: TLabel;
    edtLineWidth: TComboTrackBar;
    ColorAnimation1: TColorAnimation;
    lbl1: TLabel;
    lstDataBlocks: TListBox;
    lblScreenCoords: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnuLoadCSV: TMenuItem;
    MenuItem3: TMenuItem;
    mnuQuit: TMenuItem;
    OpenDialog1: TOpenDialog;
    btnPropertyEditor: TButton;
    lblWorldCoords: TLabel;
    mnuHelp: TMenuItem;
    mnuExamples: TMenuItem;
    mnuSinWave: TMenuItem;
    mnuSinCosPlot: TMenuItem;
    btnClear: TButton;
    Rectangle1: TRectangle;
    mnuAbout: TMenuItem;
    plt: TRRGraph;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure btnPlotClick(Sender: TObject);
    procedure btnSinClick(Sender: TObject);
    procedure cboSymbolColorChange(Sender: TObject);
    procedure cboSymbolOutlineColorChange(Sender: TObject);
    procedure cboSymbolShapeChange(Sender: TObject);
    procedure edtSymbolOutlineWidthChange(Sender: TObject);
    procedure cboLineColorChange(Sender: TObject);
    procedure edtLineWidthChange(Sender: TObject);
    procedure btnSinCosClick(Sender: TObject);
    procedure lstDataBlocksClick(Sender: TObject);
    procedure mnuLoadCSVClick(Sender: TObject);
    procedure btnPropertyEditorClick(Sender: TObject);
    procedure pltMainTitle(var subGraphId: Integer);
    procedure btnClearClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
  private
    { Private declarations }
    colorManager: TColorManager;
    procedure myOnReportCoordinates (mx, my, gx, gy : single);
  public
    { Public declarations }
    dataSeries : TDataSeries;
    csv : TCSV;
    sg : TSubGraph;
    procedure updateForm (index : integer);
    procedure setComboColorBox (chkBox : TComboColorBox; color : TAlphaColor);
    procedure setCheckBox (chkBox : TCheckBox; state : boolean);
    procedure setComboBox (chkBox : TComboBox; itemIndex : integer);
    procedure setComboTrackBar (chkBox : TComboTrackBar; value : single);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.fmx}

Uses uSymbolDetails, System.UIConsts, Math;

procedure TfmMain.setCheckBox (chkBox : TCheckBox; state : boolean);
var tmpOnChange : TNotifyEvent;
begin
  tmpOnChange := chkBox.OnChange;
  chkBox.OnChange := nil;
  chkBox.IsChecked := state;
  chkBox.OnChange := tmpOnChange;
end;


procedure TfmMain.setComboColorBox (chkBox : TComboColorBox; color : TAlphaColor);
var tmpOnChange : TNotifyEvent;
begin
  tmpOnChange := chkBox.OnChange;
  chkBox.OnChange := nil;
  chkBox.Color := color;
  chkBox.OnChange := tmpOnChange;
end;

procedure TfmMain.setComboTrackBar (chkBox : TComboTrackBar; value : single);
var tmpOnChange : TNotifyEvent;
begin
  tmpOnChange := chkBox.OnChange;
  chkBox.OnChange := nil;
  chkBox.value := value;
  chkBox.OnChange := tmpOnChange;
end;



procedure TfmMain.setComboBox (chkBox : TComboBox; itemIndex : integer);
var tmpOnChange : TNotifyEvent;
begin
  tmpOnChange := chkBox.OnChange;
  chkBox.OnChange := nil;
  chkBox.ItemIndex := itemIndex;
  chkBox.OnChange := tmpOnChange;
end;


procedure TfmMain.btnPropertyEditorClick(Sender: TObject);
begin
  plt.startPropertyEditor (self, 0);
end;


procedure TfmMain.btnSinClick(Sender: TObject);
var i : integer;
    startx, hstep : double;
    npoints, index, lstIndex : integer;
begin
  npoints := 20;
  startx := 0.0; hstep := 10/npoints;
  index := dataSeries.Add (TDataBlock.Create ('dsSin', npoints, 1));
  dataSeries[index].YData.names[0] := 'sin (x)';

  for i := 0 to dataSeries[0].nRows - 1 do
      begin
      dataSeries[index].XData[i].value := startx;
      dataSeries[index].YData.data[i,0].value := sin (startx);
      startx := startx + hstep;
      end;

  plt.subgraphs[0].subGraphProperties.dataSeries[index].YLines[0].Color := claBlue;
  plt.subgraphs[0].subGraphProperties.dataSeries[index].YLines[0].ThicknessInCms := 0.04;

  plt.subgraphs[0].subGraphProperties.dataSeries[index].YSymbols[0].Symbol := SolidSquare;
  plt.repaint;

  lstDataBlocks.Clear;
  for i := 0 to dataSeries.Count - 1 do
      lstIndex := lstDataBlocks.Items.Add (dataSeries[i].name);
  lstDataBlocks.ItemIndex := index;
  updateForm (Index);
end;

procedure TfmMain.btnSinCosClick(Sender: TObject);
var i : integer;
    startx, hstep : double;
    npoints, index, lstIndex : integer;
begin
  npoints := 20;
  startx := 0.0; hstep := 10/npoints;
  index := dataSeries.Add (TDataBlock.Create ('dsSinCos', npoints, 1));
  dataSeries[index].YData.names[0] := 'sin (x*0.8) + cos (x*1.6)';

  for i := 0 to dataSeries[0].nRows - 1 do
      begin
      dataSeries[index].XData[i].value := startx;
      dataSeries[index].YData.data[i,0].value := sin (startx*0.8) + cos (startx*1.6);
      startx := startx + hstep;
      end;

  plt.subgraphs[0].subGraphProperties.dataSeries := dataSeries;
  plt.subgraphs[0].subGraphProperties.dataSeries[index].YLines[0].Color := claRed;
  plt.subgraphs[0].subGraphProperties.dataSeries[index].YLines[0].ThicknessInCms := 0.04;

  plt.subgraphs[0].subGraphProperties.dataSeries[index].YSymbols[0].Symbol := SolidTriangle;
  plt.subgraphs[0].subGraphProperties.dataSeries[index].YSymbols[0].fillColor := claChartreuse;
  plt.repaint;

  lstDataBlocks.Clear;
  for i := 0 to dataSeries.Count - 1 do
      lstIndex := lstDataBlocks.Items.Add (dataSeries[i].name);
  lstDataBlocks.ItemIndex := index;
  updateForm (Index);
end;

procedure TfmMain.cboLineColorChange(Sender: TObject);
begin
  if plt.subgraphs[0].subGraphProperties.dataSeries.Count > 0 then
     begin
     plt.subgraphs[0].subGraphProperties.dataSeries[lstDataBlocks.ItemIndex].YLines[0].Color := cboLineColor.Color;
     plt.Repaint;
     end;
end;

procedure TfmMain.cboSymbolColorChange(Sender: TObject);
begin
  if plt.subgraphs[0].subGraphProperties.dataSeries.Count > 0 then
     begin
     plt.subgraphs[0].subGraphProperties.dataSeries[lstDataBlocks.ItemIndex].YSymbols[0].fillColor := cboSymbolColor.Color;
     plt.Repaint;
     end;
end;

procedure TfmMain.cboSymbolOutlineColorChange(Sender: TObject);
begin
  if plt.subgraphs[0].subGraphProperties.dataSeries.Count > 0 then
     begin
     plt.subgraphs[0].subGraphProperties.dataSeries[lstDataBlocks.ItemIndex].YSymbols[0].outlineColor := cboSymbolOutlineColor.Color;
     plt.Repaint;
     end;
end;

procedure TfmMain.cboSymbolShapeChange(Sender: TObject);
begin
  if plt.subgraphs[0].subGraphProperties.dataSeries.Count > 0 then
     begin
     plt.subgraphs[0].subGraphProperties.dataSeries[lstDataBlocks.ItemIndex].YSymbols[0].symbol := strToSymbolType (cboSymbolShape.Items[cboSymbolShape.ItemIndex]);
     plt.Repaint;
     end;
end;

procedure TfmMain.edtLineWidthChange(Sender: TObject);
begin
  if plt.subgraphs[0].subGraphProperties.dataSeries.Count > 0 then
     begin
      plt.subgraphs[0].subGraphProperties.dataSeries[lstDataBlocks.ItemIndex].YLines[0].ThicknessInCms := edtLineWidth.Value/10;
      plt.Repaint;
      end;
end;

procedure TfmMain.edtSymbolOutlineWidthChange(Sender: TObject);
begin
  if plt.subgraphs[0].subGraphProperties.dataSeries.Count > 0 then
     begin
     plt.subgraphs[0].subGraphProperties.dataSeries[lstDataBlocks.ItemIndex].YSymbols[0].outlineInCms := edtSymbolOutlineWidth.Value/10;
     plt.Repaint;
     end;
end;

// Plot a grid opf graph
procedure TfmMain.btnClearClick(Sender: TObject);
begin
  sg.subGraphProperties.dataSeries.Clear;
  lstDataBlocks.Clear;
  plt.repaint;
end;


procedure TfmMain.btnPlotClick(Sender: TObject);
var i, j : integer;
    dataSeries : TDataSeries;
    sg : TSubGraph;
    leftStart, bottomStart : double;
begin
  leftStart := 0.05;
  bottomStart := 0.1;

  for i := 0 to 2 do
      begin
      leftStart := 0.05;
      for j := 0 to 2 do
          begin
          sg := plt.new(leftStart, bottomStart, 0.25, 0.2);
          sg.Xmin := 0; sg.Xmax := 50;
          sg.Ymin := 0; sg.Ymax := 1;
          leftStart := leftStart + 0.3;
          end;
      bottomStart := bottomStart + 0.33;
      end;

  for i := 0 to plt.Subgraphs.Count - 1 do
      begin
      dataSeries := TDataSeries.Create;
      dataSeries.Add (TDataBlock.Create (20, 1));
      for j := 0 to dataSeries[0].nRows - 1 do
         begin
         dataSeries[0].XData[j].value := j*2.6;
         dataSeries[0].YData.data[j, 0].value := random;
         end;

      plt.subgraphs[i].subGraphProperties.dataSeries := dataSeries;
      plt.subgraphs[i].subGraphProperties.dataSeries[0].YSymbols[0].Symbol := strToSymbolType (cboSymbolShape.Items[cboSymbolShape.ItemIndex]);
      plt.subgraphs[i].subGraphProperties.dataSeries[0].YSymbols[0].radiusInCms := 0.075;
      end;
  plt.repaint;
end;

procedure TfmMain.myOnReportCoordinates (mx, my, gx, gy : single);
begin
  lblScreenCoords.Text := 'Screen: ' + floattostr (mx) + '; ' + floattostr (my);
  lblWorldCoords.Text := 'World: ' + Format ('%f', [gx]) + '; ' + Format ('%f', [gy]);
end;


procedure TfmMain.pltMainTitle(var subGraphId: Integer);
begin
 showmessage ('hi');
end;

procedure TfmMain.FormCreate(Sender: TObject);
var tmp : TNotifyEvent;
begin
  //fmMain.Quality:= TCanvasQuality.HighQuality;
  //fmMain.RecreateCanvas;
  //fmMain.Invalidate;

  plt.OnReportCoordinates := myOnReportCoordinates;
  cboSymbolShape.Items := plt.getListOfSymbolShapes;

  tmp := cboSymbolShape.OnChange;
  try
    cboSymbolShape.OnChange := nil;
    cboSymbolShape.ItemIndex := 0;
  finally
    cboSymbolShape.OnChange := tmp;
  end;

  colorManager := TColorManager.Create;

  if plt.subgraphs.Count = 0 then
     begin
     ShowMessage('Trying to access non-existent subgraph: subgraphs[0]');
     Application.Terminate;
     end;

  sg := plt.subgraphs[0];
  sg.Xmin := 0;     sg.Xmax := 10;
  sg.Ymin := -1.1;  sg.Ymax := 1.1;
  sg.subGraphProperties.AutoXScaling := true;
  sg.subGraphProperties.AutoYScaling := true;
  dataSeries := sg.subGraphProperties.dataSeries;
end;

procedure TfmMain.updateForm (index : integer);
begin
   setComboColorBox (cboLineColor, plt.subgraphs[0].subGraphProperties.dataSeries[Index].YLines[0].Color);
   setComboColorBox (cboSymbolColor, plt.subgraphs[0].subGraphProperties.dataSeries[Index].YSymbols[0].fillColor);
   setComboColorBox (cboSymbolOutlineColor, plt.subgraphs[0].subGraphProperties.dataSeries[Index].YSymbols[0].outlineColor);
   setComboBox (cboSymbolShape, ord (plt.subgraphs[0].subGraphProperties.dataSeries[Index].YSymbols[0].Symbol));
   setComboTrackBar (edtSymbolOutlineWidth, plt.subgraphs[0].subGraphProperties.dataSeries[Index].YSymbols[0].outlineInCms*10);
   setComboTrackBar (edtLineWidth, plt.subgraphs[0].subGraphProperties.dataSeries[Index].YLines[0].ThicknessInCms*10);
end;

procedure TfmMain.lstDataBlocksClick(Sender: TObject);
begin
  if lstDataBlocks.ItemIndex <> -1 then
     updateForm (lstDataBlocks.ItemIndex);
end;

procedure TfmMain.mnuAboutClick(Sender: TObject);
begin
  showmessage ('Simple Data Plotter: ' + VERSION);
end;

procedure TfmMain.mnuLoadCSVClick(Sender: TObject);
var err : TCSVErrorCode;
    i, j, index : integer;
    minv : double;
begin
  if OpenDialog1.Execute then
     begin
     dataSeries.Clear;

     csv := TCSV.Create (nil);
     csv.ReadCSV (openDialog1.FileName);
     index := dataSeries.Add (TDataBlock.Create (ExtractFileName (openDialog1.FileName), csv.rows, csv.cols-1));
     // Copy over the x column first
     for i := 0 to dataSeries[index].nRows - 1 do
         begin
         dataSeries[index].XData[i].status := dvDefined;
         dataSeries[index].XData[i].value := csv.data[i, 0].number;
         if csv.errorType[i, 0] = etSymmetric then
            begin
            dataSeries[index].XErrors[i].value := csv.symmetricError[i,0].value;
            dataSeries[index].XErrors[i].errorType := TErrorType (csv.errorType[i,0]);
            end;
         if csv.errorType[i, 0] = etASymmetric then
            begin
            dataSeries[index].XErrors[i].upper := csv.asymmetricError[i,0].upper;
            dataSeries[index].XErrors[i].lower := csv.asymmetricError[i,0].lower;
            dataSeries[index].XErrors[i].errorType := TErrorType (csv.errorType[i,0]);
            end;
         end;

     // Copy the the y columns next
     for i := 0 to dataSeries[index].nRows - 1 do
         for j := 0 to dataSeries[index].nYColumns - 1 do
             begin
             if csv.data[i,j+1].status = dsDefined then
                begin
                dataSeries[index].YData.data[i,j].value := csv.data[i,j+1].number;
                dataSeries[index].YData.data[i,j].status := TDataValueStatus.dvDefined;
                end
             else
                dataSeries[index].YData.data[i,j].status := TDataValueStatus.dvUndefined;
             if csv.errorType[i, j+1] = etSymmetric then
                begin
                dataSeries[index].YErrors[i,j].value := csv.symmetricError[i,j+1].value;
                dataSeries[index].YErrors[i,j].errorType := TErrorType (csv.errorType[i,j+1]);
                end;
             if csv.errorType[i, j+1] = etASymmetric then
                begin
                dataSeries[index].YErrors[i,j].upper := csv.asymmetricError[i,j+1].upper;
                dataSeries[index].YErrors[i,j].lower := csv.asymmetricError[i,j+1].lower;
                dataSeries[index].YErrors[i,j].errorType := TErrorType (csv.errorType[i,j+1]);
               end;
             end;
     for i := 0 to dataSeries[index].nYColumns - 1 do
         begin
         dataSeries[index].YSymbols[i].Symbol := uSymbolDetails.nextSymbol;
         dataSeries[index].YSymbols[i].fillColor := colorManager.nextColor;
         dataSeries[index].YLines[i].color := colorManager.nextColor;

         dataSeries[index].YData.names[i] := csv.header[i+1];
         end;

     if csv.rows > 80 then
        for i := 0 to dataSeries[index].nYColumns - 1 do
            dataSeries[index].YSymbols[i].Symbol := Empty;

     plt.subgraphs[0].MainTitle := ExtractFileName (openDialog1.FileName);
     plt.subgraphs[0].XAxisTitle := 'Hello';
     plt.subgraphs[0].Xmin := 0;
     plt.subgraphs[0].Xmax := 10.0;//MaxValue (dataSeries[index].XData);

     plt.subgraphs[0].Ymin := 0;
     minv := -1E12;
     for i := 0 to csv.rows - 1 do
         if dataSeries[index].YData.data[i,0].value > minv then
            minv := dataSeries[index].YData.data[i, 0].value;

     plt.subgraphs[0].Ymax := minv;

     lstDataBlocks.Clear;
     for i := 0 to dataSeries.Count - 1 do
         lstDataBlocks.Items.Add (dataSeries[i].name);
     lstDataBlocks.ItemIndex := index;
     updateForm (Index);

     plt.updatePropertyEditor (0);
     plt.repaint;
     end;
end;

end.
