unit ufMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ExtCtrls, System.Math.Vectors, FMX.Memo, FMX.TabControl,
  uPlottingPanel, uSubGraph, uRRDataSeries, uRRTypes, FMX.Platform,
  System.UIConsts, ufGraphPackageDialog, FMX.ListBox, FMX.Objects,
  System.Rtti, FMX.Grid, FMX.TreeView,
  FMX.Menus,
  FMX.Edit, uCoyoteCommon, strutils,
  FMX.Grid.Style, FMX.ScrollBox, FMX.Controls.Presentation, FMX.Memo.Types,
  Skia.FMX, Skia, FMX.Colors, uRRList, uSetup, ufStructuralAnalysis,
  uNewMatrix, uColorList, ufScrollChart, uController, uViewer,
  uPlotFrameViewer,
  uTableFrameViewer,
  uFrameFunctionPlotter,
  uFrameScanControl,
  uFrameSteadyStateControl,
  uConfiguration,
  ufMainConfig
{$IFDEF MSWINDOWS}
    , Winapi.Windows, Winapi.ShellAPI, ufFrameSplitPanel
{$ENDIF}
{$IFDEF POSIX}
    , Posix.Stdlib
{$ENDIF POSIX};

const
  VERSION = '0.952 Beta';

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    btnClose: TButton;
    pnlMain: TLayout;
    lblVersion: TLabel;
    OpenSBMLDialog: TOpenDialog;
    Rectangle1: TRectangle;
    FunctionTabControl: TTabControl;
    tabTimeCourse: TTabItem;
    pnlControls: TLayout;
    tabSteadyState: TTabItem;
    tbScan: TTabItem;
    btnTimeCourse: TSpeedButton;
    btnSteadyState: TSpeedButton;
    btnLoadAntimony: TSpeedButton;
    Rectangle2: TRectangle;
    rrMainMenu: TMainMenu;
    mnuMacFile: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuSeparator2: TMenuItem;
    mnuWinQuit: TMenuItem;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    btnScan: TSpeedButton;
    Image6: TImage;
    mnuOptions: TMenuItem;
    StyleBook1: TStyleBook;
    mnuSaveGraphasPdf: TMenuItem;
    MenuItem2: TMenuItem;
    SavePDFDialog: TSaveDialog;
    Button2: TButton;
    btnStructAnalysis: TSpeedButton;
    Image7: TImage;
    mnuLoadAntimonyModel: TMenuItem;
    OpenAntimonyDialog: TOpenDialog;
    mnuSaveAntFile: TMenuItem;
    mnuSaveAntimonyFileAs: TMenuItem;
    mnuNew: TMenuItem;
    SaveAntimonyFileDialog: TSaveDialog;
    mnuExportSBML: TMenuItem;
    mnuImportSBML: TMenuItem;
    MenuItem4: TMenuItem;
    btnNew: TSpeedButton;
    Image8: TImage;
    SpeedButton1: TSpeedButton;
    Image9: TImage;
    mnuMainExamples: TMenuItem;
    Rectangle5: TRectangle;
    btnRealTimeTool: TSpeedButton;
    Image4: TImage;
    mnuUseFloatingGraph: TMenuItem;
    mnuAnalysis: TMenuItem;
    mnuTimecourseSimulation: TMenuItem;
    mnuSteadyState: TMenuItem;
    mnuParameterScan: TMenuItem;
    mnuRealTimeSimulation: TMenuItem;
    mnuDivider1: TMenuItem;
    mnuStructuralAnalysis: TMenuItem;
    btnFunctionPlot: TSpeedButton;
    Image12: TImage;
    mnuPreferences: TMenuItem;
    mnuExamples: TMenuItem;
    mnuConfig: TMenuItem;
    mnuSpace1: TMenuItem;
    Button1: TButton;
    tbAntimony: TTabControl;
    tbModelDefinition: TTabItem;
    Layout4: TLayout;
    Panel2: TPanel;
    Layout8: TLayout;
    btnOpenGraph: TSpeedButton;
    Image5: TImage;
    btnOpenTabular: TSpeedButton;
    Image11: TImage;
    pnlOutputPanel: TLayout;
    pnlModelAndControlsPanel: TLayout;
    moModel: TMemo;
    Layout7: TLayout;
    Splitter1: TSplitter;
    frameSplitPanel: TframeSplitPanels;
    Label4: TLabel;
    Label5: TLabel;
    Rectangle3: TRectangle;
    Rectangle6: TRectangle;
    pnlBottomPanel: TLayout;
    GroupBox3: TGroupBox;
    btnSetTimeCourseSelection: TButton;
    Rectangle4: TRectangle;
    lstYAxis: TListBox;
    btnUnSelectAll: TButton;
    Layout5: TLayout;
    Label7: TLabel;
    btnConfigIntegrator: TButton;
    Layout6: TLayout;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtTimeStart: TEdit;
    Label2: TLabel;
    edtTimeEnd: TEdit;
    Label3: TLabel;
    edtNumberOfPoints: TEdit;
    Layout9: TLayout;
    btnSimulate: TButton;
    btnReset: TButton;
    btnTimeCourseSliders: TSpeedButton;
    Image10: TImage;
    Layout10: TLayout;
    GroupBox4: TGroupBox;
    chkAutoscaleX: TCheckBox;
    chkAutoScaleY: TCheckBox;
    Layout11: TLayout;
    GroupBox2: TGroupBox;
    cbXAxis: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbXAxisChange(Sender: TObject);
    procedure lstYAxisChange(Sender: TObject);
    procedure btnTimeCourseClick(Sender: TObject);
    procedure btnSteadyStateClick(Sender: TObject);
    procedure btnSetTimeCourseSelectionClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuMacQuitClick(Sender: TObject);
    procedure mnuWinQuitClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnConfigIntegratorClick(Sender: TObject);
    procedure mnuSaveGraphasPdfClick(Sender: TObject);
    procedure btnOpenGraphClick(Sender: TObject);
    procedure btnStructAnalysisClick(Sender: TObject);
    procedure mnuExamplesClick(Sender: TObject);
    procedure mnuLoadAntimonyModelClick(Sender: TObject);
    procedure mnuSaveAntFileClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuSaveAntimonyFileAsClick(Sender: TObject);
    procedure mnuImportSBMLClick(Sender: TObject);
    procedure mnuExportSBMLClick(Sender: TObject);
    procedure moModelImmediateChange(Sender: TObject);
    procedure chkAutoscaleXChange(Sender: TObject);
    procedure btnTimeCourseSlidersClick(Sender: TObject);
    procedure btnRealTimeToolClick(Sender: TObject);
    procedure mnuUseFloatingGraphClick(Sender: TObject);
    procedure edtNumberOfPointsChange(Sender: TObject);
    procedure edtTimeEndChange(Sender: TObject);
    procedure edtTimeStartChange(Sender: TObject);
    procedure chkAutoScaleYChange(Sender: TObject);
    procedure btnFunctionPlotClick(Sender: TObject);
    procedure btnUnSelectAllClick(Sender: TObject);
    procedure mnuPreferencesClick(Sender: TObject);
    procedure moModelChange(Sender: TObject);
    procedure mnuConfigClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure btnOpenTabularClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    currentAntimonyFileName: string;

    logFileName: AnsiString;

    launchPath: string;
    sbmlString: AnsiString;

    currentTime: double;

    procedure openGraphPanel;
    procedure closeGraphPanel;

    procedure openTabularPanel;
    procedure closeTabularPanel;

    procedure mnuHelpClick(Sender: TObject);
    procedure mnuHelpAntimonyClick(Sender: TObject);
    procedure collectModelSymbols;
    procedure bringUpSelectionForm (selectionList: TStringList; includeTime: boolean);
    procedure OnModelChange;
    procedure ClearAll;
    procedure loadModelIntoMemo(antStr: string);
    procedure loadBuiltInModel(name: string); overload;
    procedure loadBuiltInModel(index: integer); overload;
    procedure loadTemplateModel;
    procedure loadModelFromMemo;

    procedure ViewerModelHasChanged;
    procedure OnTimeCourseSliderNotify(parameter: string; value: double);
  public
    { Public declarations }

    config : TfrmMainConfig;

    antimonyLoaded: boolean;
    roadRunnerLoaded: boolean;
    errMsg: AnsiString;
    simulatedData: T2DMatrix;

    fireEvent: boolean;

    floatingSpeciesIds: TStringList;
    boundarySpeciesIds: TStringList;
    reactionRatesIds: TStringList;
    ratesOfChangeIds: TStringList;
    elasticityIds: TRRList; // Use RRList because its nested
    globalParameters : TStringList;

    selectedPalette: string;
    controller: TController;
    plotViewer : TPlotFrameViewer;
    tableViewer : TTableFrameViewer;

    procedure OnPickExample(index: integer);
    procedure selectIntegrator;
    procedure loadRoadRunnerLibrary;
    procedure doTimeCourseSimulationAndPlot;
    procedure populateXYSelectors;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

Uses uRoadRunner.API, Math, uSymbolDetails, uRRCommon, ufSelectionChoices,
  uBuiltInModels, ufSliders, uComplex,
  ufIntegratorOptions, ufSteadyStateOptions, FMX.Styles.Objects,
  ufMoreSteadyState, uParameterScan, ufFloatingPlotViewer,
  IOUtils, uRoadRunner, ufExamples, ufAbout, uSimulator, uViewerTypes,
  uScanArguments, ufPreferences;

const
   TIMECOURSE_FUNCTION = 0;

// ------------------------------------------------------------------------

// Not yet operational
procedure TfrmMain.selectIntegrator;
var
  i: integer;
  sl: TStringList;
  currentName: string;
begin
  raise exception.Create('Config panel not yet ready');
  if frmIntegratorOptions = nil then
    begin
      frmIntegratorOptions := TfrmIntegratorOptions.Create(nil);
      frmIntegratorOptions.StyleBook := StyleBook1;
    end;
  sl := uRoadRunner.API.getListOfRegisteredIntegrators();
  try
    frmIntegratorOptions.lstSolverNames.Items.Assign(sl);
    currentName := uRoadRunner.API.getCurrentIntegratorName;

    frmIntegratorOptions.disableChange := true;
    try
      for i := 0 to sl.Count - 1 do
        if sl[i] = currentName then
          begin
            frmIntegratorOptions.lstSolverNames.itemindex := i;
            break;
          end;
    finally
      frmIntegratorOptions.disableChange := false;
    end;
    frmIntegratorOptions.displaySelectedSolver;
    frmIntegratorOptions.Show;
  finally
    sl.Free;
  end;
end;


procedure TfrmMain.loadRoadRunnerLibrary;
begin
  if not loadRoadRunner(errMsg) then
     begin
     roadRunnerLoaded := false;
     end
  else
     roadRunnerLoaded := true;
end;


procedure TfrmMain.loadBuiltInModel(index: integer);
var
  model: TBuiltInModel;
  sbmlStr: AnsiString;
begin
  model := builtInModels[index];

  sbmlStr := controller.modelInputManager.getSBMLFromAntimony (model.modelStr);
  controller.loadSBMLModel(sbmlStr, true);

  loadModelIntoMemo(model.modelStr);

  controller.ViewerSetProperty('UserScale_Xmin', model.Xmin);
  controller.ViewerSetProperty('UserScale_Xmax', model.Xmax);
  controller.ViewerSetProperty('UserScale_Ymin', model.Ymin);
  controller.ViewerSetProperty('UserScale_Ymax', model.Ymax);

  edtTimeEnd.Text := model.timeEnd;
  edtNumberOfPoints.Text := inttostr(model.numberOfPoints);
  chkAutoscaleX.IsChecked := False;
  controller.setTimeEnd(strtofloat (model.timeEnd));
  controller.setNumberOfPoints(model.numberOfPoints);
end;


procedure TfrmMain.loadBuiltInModel(name: string);
var
  model: TBuiltInModel;
  sbmlStr: AnsiString;
begin
  OnModelChange;
  model := builtInModels.getBuiltInModel(name);

  sbmlStr := controller.modelInputManager.getSBMLFromAntimony (model.modelStr);
  controller.loadSBMLModel(sbmlStr, true);

  loadModelIntoMemo(model.modelStr);

  controller.ViewerSetProperty('UserScale_Xmin', model.Xmin);
  controller.ViewerSetProperty('UserScale_Xmax', model.Xmax);
  controller.ViewerSetProperty('UserScale_Ymin', model.Ymin);
  controller.ViewerSetProperty('UserScale_Ymax', model.Ymax);

  edtTimeEnd.Text := model.timeEnd;
  edtNumberOfPoints.Text := inttostr(model.numberOfPoints);
  chkAutoscaleX.IsChecked := False;
  controller.setTimeEnd(strtofloat (model.timeEnd));
  controller.setNumberOfPoints(model.numberOfPoints);
end;


procedure TfrmMain.lstYAxisChange(Sender: TObject);
var i : integer;
begin
  if fireEvent then
    begin
      for i := 0 to lstYAxis.Items.Count - 1 do
          begin
          controller.viewerPackage.YColumnChoice[i] := (lstYAxis.listitems[i] as Tlistboxitem).IsChecked;
          controller.viewerPackage.YColumnNames[i] := (lstYAxis.listitems[i] as Tlistboxitem).text;
          end;
      controller.updateViewers;
    end;
end;


procedure TfrmMain.mnuConfigClick(Sender: TObject);
begin
  saveConfigurationFile (CONFIG_FILE_NAME);
end;


procedure TfrmMain.mnuSaveAntFileClick(Sender: TObject);
begin
  if currentAntimonyFileName = '' then
    begin
      if SaveAntimonyFileDialog.Execute then
        begin
          currentAntimonyFileName := SaveAntimonyFileDialog.FileName;
          frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
          TFile.WriteAllText(currentAntimonyFileName, moModel.Text);
        end;
    end
  else
    TFile.WriteAllText(currentAntimonyFileName, moModel.Text);
end;


procedure TfrmMain.mnuSaveAntimonyFileAsClick(Sender: TObject);
begin
  if SaveAntimonyFileDialog.Execute then
    begin
      currentAntimonyFileName := SaveAntimonyFileDialog.FileName;
      frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
      TFile.WriteAllText(currentAntimonyFileName, moModel.Text);
    end;
end;


procedure TfrmMain.mnuSaveGraphasPdfClick(Sender: TObject);
begin
  if SavePDFDialog.Execute then
    plotViewer.plt.exportToPDF(SavePDFDialog.FileName);
end;


procedure TfrmMain.mnuUseFloatingGraphClick(Sender: TObject);
begin
  if not Assigned(frmFloatingPlotViewer) then
    frmFloatingPlotViewer := TfrmFloatingPlotViewer.Create(nil);
  frmFloatingPlotViewer.controller := controller;
  frmFloatingPlotViewer.StyleBook := StyleBook1;
  frmFloatingPlotViewer.Show;
end;


procedure TfrmMain.mnuHelpClick(Sender: TObject);
begin
  showmessage('There is no help at present: You''re on your own....'
        + sLineBreak + sLineBreak + 'Hint: Press the simulate button');
end;


procedure TfrmMain.mnuHelpAntimonyClick(Sender: TObject);
var
  myurl: string;
begin
  myurl := 'https://tellurium.readthedocs.io/en/latest/antimony.html';
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(myurl), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(myurl)));
{$ENDIF POSIX}
end;


procedure TfrmMain.mnuAboutClick(Sender: TObject);
var
  str: string;
begin
  str := controller.simulator.roadrunner.getVersionStr();
  // str := str + sLineBreak + uRoadRunner.getlibSBMLVersion();
  frmAbout := TfrmAbout.Create(nil);
  frmAbout.StyleBook := StyleBook1;
  frmAbout.lblRoadRunner.Text := 'Using libroadRunner version: ' + str;
  frmAbout.lbllibSBML.Text := 'Using libSBML vesion: ' + controller.simulator.roadrunner.getlibSBMLVersion();
  frmAbout.lbSkia.Text := 'Using skia: ' + Skia.SkVersion + ', Milestone: ' + inttostr(TSkVersion.LibraryMajor);

  frmAbout.lblWho.Text := 'Developed at the Sauro Lab, University of Washington, Seattle';
  frmAbout.lbVersion.Text := 'Iridium version: ' + VERSION;

  frmAbout.ShowModal;
  frmAbout.Free;
{$IFDEF MSWINDOWS}
  // Messagedlg('Version 1.0' + sLineBreak + sLineBreak + 'libRoadunner Version: '
  // + str, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
{$ELSE}
  // TDAMessageBox.MessageDialog('About roadRunner UI', 'libRoadunner Version: ' +
  // str, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
{$ENDIF}
end;


procedure TfrmMain.mnuExamplesClick(Sender: TObject);
begin
  if not Assigned(frmExamples) then
    begin
      frmExamples := TfrmExamples.Create(nil);
      for var i := 0 to builtInModels.Count - 1 do
        frmExamples.ltExamples.Items.Add(builtInModels[i].displayName);
    end;
  frmExamples.StyleBook := StyleBook1;

  if frmMain.Left > frmExamples.Width then
     begin
     frmExamples.Left := frmMain.Left - frmExamples.Width;
     frmExamples.Top := frmMain.Top;
     end;
  frmExamples.Show;
  frmExamples.BringToFront;
end;


procedure TfrmMain.mnuExportSBMLClick(Sender: TObject);
begin
  controller.modelInputManager.exportSBML;
end;


procedure TfrmMain.mnuImportSBMLClick(Sender: TObject);
var
  antStr: string;
  sbmlStr: string;
  path : string;
begin
  sbmlStr := controller.modelInputManager.importSBML (path);
  if sbmlStr = '' then
     exit;

  controller.loadSBMLModel(sbmlStr, true);

  antStr := controller.modelInputManager.getAntimonyFromSBML(sbmlStr);
  loadModelIntoMemo(antStr);

  currentAntimonyFileName := ChangeFileExt(path, '.ant');
  frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
end;


procedure TfrmMain.mnuLoadAntimonyModelClick(Sender: TObject);
var errMsg : string;
begin
  if OpenAntimonyDialog.Execute then
    begin
      if controller.modelInputManager.loadAntimonyFromFile(OpenAntimonyDialog.FileName, errMsg) then
         begin
         controller.simulator.loadSBMLFromString(controller.modelInputManager.antimonyStr);
         loadModelIntoMemo(controller.modelInputManager.antimonyStr);
         end
      else
        raise exception.Create('Error LoadAntimonyModel: ' + errMsg);

      currentAntimonyFileName := OpenAntimonyDialog.FileName;
      frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
    end;
  //controller.simulator.roadrunner.setComputeAndAssignConservationLaws(true);
end;


procedure TfrmMain.OnPickExample(index: integer);
begin
  loadBuiltInModel(index);
  loadModelFromMemo;
  populateXYSelectors;  // required to update viewerpackage in controller
end;


procedure TfrmMain.loadModelIntoMemo(antStr: string);
begin
  OnModelChange;
  moModel.Lines.Text := antStr; // getAntimonyFromSBML(sbmlStr);
  collectModelSymbols;
end;


procedure TfrmMain.mnuMacQuitClick(Sender: TObject);
begin
  saveConfigurationFile (CONFIG_FILE_NAME);
  if controller.outOfDate then
     begin
     if MessageDlg('Do you really want to quit, the model is not saved to disk', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = IDYes then
        Application.Terminate;
     end
  else
     Application.Terminate;
end;


procedure TfrmMain.ClearAll;
begin
  controller.clearViewers;
end;


procedure TfrmMain.loadTemplateModel;
begin
  moModel.Lines.LoadFromFile(launchPath + '\\template.txt');
end;


procedure TfrmMain.mnuNewClick(Sender: TObject);
begin
  currentAntimonyFileName := '';
  ClearAll;
  loadTemplateModel;
end;


procedure TfrmMain.mnuPreferencesClick(Sender: TObject);
begin
  frmPreferences := TfrmPreferences.Create(nil);
  try
    frmPreferences.StyleBook := StyleBook1;
    frmPreferences.ShowModal;
    controller.modelInputManager.modelMemo.Font.Size := configOpts.modelInputManagerConfig.fontSize;
  finally
    frmPreferences.Free;
  end;
end;


procedure TfrmMain.mnuWinQuitClick(Sender: TObject);
begin
  if controller.outOfDate then
     begin
     if MessageDlg('Do you really want to quit, the model is not saved to disk', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = IDYes then
        Application.Terminate;
     end
  else
     Application.Terminate;
end;


// Only fired after loosing focus from the model memo
procedure TfrmMain.moModelChange(Sender: TObject);
begin
  controller.modelChanged;
end;


// Only fired if the user changes the text on the model
// memo but without losoing focus.
procedure TfrmMain.moModelImmediateChange(Sender: TObject);
begin
  controller.outOfDate := True;
end;


procedure TfrmMain.btnConfigIntegratorClick(Sender: TObject);
begin
  selectIntegrator;
end;


procedure TfrmMain.btnFunctionPlotClick(Sender: TObject);
var t : TTabItem;
    frame : TframeFunctionPlotter;
    i : integer;
begin
  for i := 0 to FunctionTabControl.TabCount - 1 do
      if FunctionTabControl.Tabs[i].Text = 'Function Plotter' then
         begin
         FunctionTabControl.TabIndex := i;
         exit;
         end;

  t := FunctionTabControl.Add;
  t.Text := 'Function Plotter';
  frame := TframeFunctionPlotter.Create(FunctionTabControl);
  frame.stylebook1 := StyleBook1;
  frame.Parent := t;
  frame.Align := TAlignLayout.Client;
  frame.Visible := True;
  frame.controller := controller;
  FunctionTabControl.TabIndex := 3;
end;


procedure TfrmMain.populateXYSelectors;
var
  i: integer;
  currentSelectionList: TStringList;
begin
  fireEvent := false;

  currentSelectionList := controller.simulator.roadrunner.getTimeCourseSelectionList;
  try
    lstYAxis.Clear;
    cbXAxis.Clear;
    setlength (controller.viewerPackage.YColumnNames, currentSelectionList.Count);
    for i := 0 to currentSelectionList.Count - 1 do
      begin
        lstYAxis.Items.Add(currentSelectionList[i]);
        controller.viewerPackage.YColumnNames[i] := currentSelectionList[i];
        cbXAxis.Items.Add(currentSelectionList[i]);
      end;
    cbXAxis.itemindex := 0;
    controller.viewerPackage.XAxisTitle := currentSelectionList[0];
    controller.viewerPackage.XColumnIndex := 0;

    setLength(controller.viewerPackage.YColumnChoice, currentSelectionList.Count);
    if currentSelectionList.Count = 0 then
      exit;

    (lstYAxis.listitems[0] as Tlistboxitem).IsChecked := false;
    controller.viewerPackage.YColumnChoice[0] := false;
    for i := 1 to currentSelectionList.Count - 1 do
      begin
        (lstYAxis.listitems[i] as Tlistboxitem).IsChecked := true;
        controller.viewerPackage.YColumnChoice[i] := true;
      end;
    lstYAxis.itemindex := -1;
  finally
    fireEvent := true;
    currentSelectionList.Free;
  end;
end;


procedure TfrmMain.collectModelSymbols;
begin
  floatingSpeciesIds := controller.simulator.roadrunner.getFloatingSpeciesIds;
  boundarySpeciesIds := controller.simulator.roadrunner.getBoundarySpeciesIds;
  reactionRatesIds := controller.simulator.roadrunner.getReactionIds;
  ratesOfChangeIds := controller.simulator.roadrunner.getRatesOfChangeIds;
  elasticityIds := controller.simulator.roadrunner.getElasticityIds;
  globalParameters := controller.simulator.roadrunner.getGlobalParameterIds;
end;


procedure TfrmMain.openGraphPanel;
begin
  frameSplitPanel.toggleUpper;
end;


procedure TfrmMain.closeGraphPanel;
begin
  //pnlPlotBase.visible := false;
  //Splitter1.visible := false;
end;


procedure TfrmMain.openTabularPanel;
begin
  //Splitter1.visible := true;
  //Splitter1.position.x := plotViewer.plt.position.x;
  //pnlPlotBase.Align := TAlignLayout.Top;
  //pnlTabularBase.visible := true;
  //pnlTabularBase.Height := 200;
  //pnlOutputPanel.Width := configOpts.graphPanelWidth;
  //pnlPlotBase.Height := 300;
end;


procedure TfrmMain.closeTabularPanel;
begin
  //pnlPlotBase.height := pnlOutputPanel.Height;
  //pnlTabularBase.height := 25;
  //pnlTabularBase.Align := TAlignLayout.Bottom;
  //pnlPlotBase.Align := TAlignLayout.Client;
  //Splitter1.visible := false;
end;

procedure TfrmMain.btnOpenGraphClick(Sender: TObject);
begin
  configOpts.mainConfig.IsGraphPanelOpen := frameSplitPanel.toggleUpper;
end;


procedure TfrmMain.btnOpenTabularClick(Sender: TObject);
begin
  configOpts.mainConfig.IsTabularPanelOpen := frameSplitPanel.toggleLower;

  //if configOpts.IsTabularPanelOpen then
  //   closeTabularPanel
  //else
  //   openTabularPanel;
  //configOpts.IsTabularPanelOpen := not configOpts.IsTabularPanelOpen;
end;


procedure TfrmMain.btnRealTimeToolClick(Sender: TObject);
var
  list: TStringList;
  sbmlStr : string;
begin
  if frmScrollChart = nil then
    frmScrollChart := TfrmScrollChart.Create(nil, controller);

  frmScrollChart.StyleBook := StyleBook1;

  list := controller.simulator.roadrunner.getFloatingSpeciesIds();
  try
    sbmlStr := controller.modelInputManager.getSBMLFromAntimony(moModel.Lines.Text);
    controller.loadSBMLModel(sbmlStr, true);

    collectModelSymbols;
    ViewerModelHasChanged;

    frmScrollChart.Show;

  finally
    list.Free;
  end;
end;


procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  controller.simulator.roadrunner.reset();
end;


procedure TfrmMain.edtNumberOfPointsChange(Sender: TObject);
begin
  controller.setNumberOfPoints(strtoint (edtNumberOfPoints.text));
end;


procedure TfrmMain.edtTimeEndChange(Sender: TObject);
begin
  controller.setTimeEnd(strtofloat (edtTimeEnd.Text));
  chkAutoscaleX.IsChecked := False;
end;


procedure TfrmMain.edtTimeStartChange(Sender: TObject);
begin
  controller.setTimeStart(strtofloat (edtTimeStart.Text));
end;


procedure TfrmMain.doTimeCourseSimulationAndPlot;
begin
  controller.runTimeCourseSimulation;
  btnTimeCourseSliders.enabled := true;
end;


function compareTwoStringLists (s1, s2 : TStringList) : boolean;
begin
  if s1.Count <> s2.Count then
     exit (false);

  for var i := 0 to s1.Count - 1 do
      if s1[i] <> s2[i] then
         exit (false);

  exit (True);
end;


function compareModels (r1, r2 : TRoadRunner) : boolean;
var r1_floatingSpeciesIds : TStringList;
    r1_boundarySpeciesIds : TStringList;
    r1_reactionRatesIds : TStringList;
    r1_globalParameters : TStringList;

    r2_floatingSpeciesIds : TStringList;
    r2_boundarySpeciesIds : TStringList;
    r2_reactionRatesIds : TStringList;
    r2_globalParameters : TStringList;
begin
  r1_floatingSpeciesIds := r1.getFloatingSpeciesIds;   r1_floatingSpeciesIds.Sort;
  r1_boundarySpeciesIds := r1.getBoundarySpeciesIds;   r1_boundarySpeciesIds.Sort;
  r1_reactionRatesIds := r1.getReactionIds;            r1_reactionRatesIds.Sort;
  r1_globalParameters := r1.getGlobalParameterIds;     r1_globalParameters.Sort;

  r2_floatingSpeciesIds := r2.getFloatingSpeciesIds;   r2_floatingSpeciesIds.Sort;
  r2_boundarySpeciesIds := r2.getBoundarySpeciesIds;   r2_boundarySpeciesIds.Sort;
  r2_reactionRatesIds := r2.getReactionIds;            r2_reactionRatesIds.Sort;
  r2_globalParameters := r2.getGlobalParameterIds;     r2_globalParameters.Sort;

  if not compareTwoStringLists (r1_floatingSpeciesIds, r2_floatingSpeciesIds) then
     exit (False);

  if not compareTwoStringLists (r1_boundarySpeciesIds, r2_boundarySpeciesIds) then
     exit (False);

  if not compareTwoStringLists (r1_reactionRatesIds, r2_reactionRatesIds) then
     exit (False);

  if not compareTwoStringLists (r1_globalParameters, r2_globalParameters) then
     exit (False);

  // Structure appears the same
  exit (True);
end;


procedure TfrmMain.loadModelFromMemo;
var sbmlStr: string;
    r : TRoadRunner;
    structureIsTheSame : boolean;
begin
  if controller.outOfDate then
     begin
     sbmlStr := controller.modelInputManager.getSBMLFromAntimony(moModel.Lines.Text);
     r := TRoadRunner.Create;
     r.setComputeAndAssignConservationLaws(true);
     try
       r.loadSBMLFromString(sbmlStr);
       structureIsTheSame := compareModels (controller.simulator.roadrunner, r);

       controller.loadSBMLModel(sbmlStr, true);

       collectModelSymbols;
       ViewerModelHasChanged;
       controller.outOfDate := false;

       if not structureIsTheSame then
          populateXYSelectors;  // required to update viewerpackage in controller
     finally
       r.Free;
     end;
     end;
end;


procedure TfrmMain.btnSimulateClick(Sender: TObject);
begin
  loadModelFromMemo;

  controller.runTimeCourseSimulation;
end;


procedure TfrmMain.btnSteadyStateClick(Sender: TObject);
var t : TTabItem;
    frame : TFrameSteadyStateControl;
    i : integer;
begin
  for i := 0 to FunctionTabControl.TabCount - 1 do
      if FunctionTabControl.Tabs[i].Text = 'Steady-State Control' then
         begin
         FunctionTabControl.TabIndex := i;
         exit;
         end;

  t := FunctionTabControl.Add;
  t.Text := 'Steady-State Control';
  frame := TFrameSteadyStateControl.Create(FunctionTabControl);
  frame.stylebook1 := StyleBook1;
  frame.Parent := t;
  frame.Align := TAlignLayout.Client;
  frame.Visible := True;
  frame.controller := controller;
  FunctionTabControl.ActiveTab := t;

  loadModelFromMemo;
end;


procedure TfrmMain.btnStructAnalysisClick(Sender: TObject);
begin
  loadModelFromMemo;

  if not Assigned(frmStructuralAnalysis) then
    frmStructuralAnalysis := TfrmStructuralAnalysis.Create(frmMain);

  frmStructuralAnalysis.controller := controller;
  frmStructuralAnalysis.Show;
  frmStructuralAnalysis.BringToFront;
end;


procedure TfrmMain.btnTimeCourseClick(Sender: TObject);
begin
  FunctionTabControl.TabIndex := TIMECOURSE_FUNCTION;
end;


procedure TfrmMain.ViewerModelHasChanged;
begin
  controller.ViewerModelHasChanged (self);
end;


procedure TfrmMain.OnTimeCourseSliderNotify(parameter: string; value: double);
begin
  controller.simulator.roadrunner.setValue(parameter, value);
  controller.simulator.roadrunner.reset;
  controller.runTimeCourseSimulation;
end;

procedure TfrmMain.btnTimeCourseSlidersClick(Sender: TObject);
var
  list: TStringList;
  i: integer;
  value: double;
begin
  if not Assigned(frmSliders) then
    frmSliders := TfrmSliders.Create(nil);
  frmSliders.OnNotifyChange := OnTimeCourseSliderNotify;
  frmSliders.StyleBook := StyleBook1;

  list := controller.simulator.roadrunner.getGlobalParameterIds;
  // We can't change total masses for time course
  // simulations, so let's remove any of those entries.
  for i := list.Count - 1 downto 0 do
    if StartsText('_CSUM', list[i]) then
      list.Delete(i);

  for i := frmSliders.lstParameters.Count - 1 downto 0 do
    if frmSliders.lstParameters.Items.Objects[i] <> nil then
      frmSliders.lstParameters.Items.Objects[i].Free;
  frmSliders.lstParameters.Clear;
  for i := 0 to list.Count - 1 do
    begin
      value := controller.simulator.roadrunner.getValue(list[i]);
      frmSliders.lstParameters.Items.AddObject(list[i], TSliderInitialValue.Create(value));
    end;

  list.Free;

  frmSliders.Show;
  frmSliders.BringToFront;
end;

procedure TfrmMain.btnUnSelectAllClick(Sender: TObject);
begin
  fireEvent := false;
  for var i := 0 to lstYAxis.Items.Count - 1 do
      (lstYAxis.listitems[i] as TListboxitem).IsChecked := False;
  fireEvent := True;
  lstYAxisChange(Sender);
end;


procedure TfrmMain.Button1Click(Sender: TObject);
begin
  label4.Text :=  IntToHex(claCrimson, 8);
  label5.Text := inttostr ( StrToInt('$' + label4.text));
end;

// Toolbar/Menu selection of scanning frame
procedure TfrmMain.btnScanClick(Sender: TObject);
var t : TTabItem;
    frame : TFrameScanControl;
    i : integer;
begin
  for i := 0 to FunctionTabControl.TabCount - 1 do
      if FunctionTabControl.Tabs[i].Text = 'Scanning Control' then
         begin
         FunctionTabControl.TabIndex := i;
         exit;
         end;

  t := FunctionTabControl.Add;
  t.Text := 'Scanning Control';
  frame := TFrameScanControl.Create(FunctionTabControl);
  frame.stylebook1 := StyleBook1;
  frame.Parent := t;
  frame.Align := TAlignLayout.Client;
  frame.Visible := True;
  frame.controller := controller;
  frame.cboColorPalette.Items.Assign(getPaletteNames);
  frame.cboColorPalette.itemindex := 0;
  FunctionTabControl.ActiveTab := t;

  loadModelFromMemo;

  frame.initializeScanUserInterface;
end;



procedure TfrmMain.bringUpSelectionForm(selectionList: TStringList; includeTime: boolean);
var
  Item1, Item2, Item3: TTreeViewItem;
  i, j: integer;
begin
  frmSelectionChoices.Free;
  // This is to avoid visual corruption of calling form (reported to qc)
  frmSelectionChoices := TfrmSelectionChoices.Create(nil);
  // try
  for i := 0 to selectionList.Count - 1 do
    frmSelectionChoices.lstSelectedItems.Items.Add(selectionList[i]);

  // ---------------------------------------------------
  if includeTime then
    begin
      Item1 := TTreeViewItem.Create(self);
      Item1.Text := 'Time';
      Item1.Parent := frmSelectionChoices.treeSelection;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Floating Species';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to floatingSpeciesIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := floatingSpeciesIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Boundary Species';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to boundarySpeciesIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := boundarySpeciesIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Reaction Rates';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to reactionRatesIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := reactionRatesIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Rates of Change';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to ratesOfChangeIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := ratesOfChangeIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Elasticities';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to elasticityIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := elasticityIds[i].list[0].sValue;
      Item2.Parent := Item1;
      for j := 0 to elasticityIds[i].list[1].list.Count - 1 do
        begin
          Item3 := TTreeViewItem.Create(self);
          Item3.Text := elasticityIds[i].list[1].list[j].sValue;
          Item3.Parent := Item2;
        end;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Eigenvalues';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to controller.simulator.roadrunner.getEigenvalueIds.Count - 1 do
    begin
      // filter out eigen(X), only show eigenReal and eigenImag
      if not StartsText('eigen(', controller.simulator.roadrunner.getEigenvalueIds[i]) then
        begin
          Item2 := TTreeViewItem.Create(self);
          Item2.Text := controller.simulator.roadrunner.getEigenvalueIds[i];
          Item2.Parent := Item1;
        end;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Other';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to globalParameters.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := globalParameters[i];
      Item2.Parent := Item1;
    end;

  frmSelectionChoices.ShowModal;
end;


procedure TfrmMain.btnSetTimeCourseSelectionClick(Sender: TObject);
var
  i: integer;
  selectionList: TStringList;
begin
  loadModelFromMemo;

  // Include time as a choice
  bringUpSelectionForm(TStringList(lstYAxis.Items), true);

  selectionList := TStringList.Create;
  try
    for i := 0 to frmSelectionChoices.lstSelectedItems.Count - 1 do
        selectionList.Add(frmSelectionChoices.lstSelectedItems.Items[i]);
    controller.setSelectionList (selectionList);
  finally
    selectionList.Free;
  end;
  populateXYSelectors;
end;


procedure TfrmMain.cbXAxisChange(Sender: TObject);
begin
  if fireEvent then
    begin
      controller.setXAxisTitle (cbXAxis.Items[cbXAxis.ItemIndex]);
      controller.viewerPackage.XColumnIndex := cbXAxis.ItemIndex;
      controller.updateViewers;
    end;
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  configOpts.mainConfig.formLeft := frmMain.Left;
  configOpts.mainConfig.formWidth := frmMain.Width;
  configOpts.mainConfig.formHeight := frmMain.Height;
  configOpts.mainConfig.formTop := frmMain.Top;
  configOpts.mainConfig.outputPanelWidth := pnlOutputPanel.Width;
  configOpts.mainConfig.upperOutputPanelHeight := 100*frameSplitPanel.currentUpperHeight/frameSplitPanel.Height;

  configOpts.textFormViewer.dataMemoBackgroundColor := tableViewer.config.dataMemoBackgroundColor;
  configOpts.textFormViewer.dataMemoFontColor := tableViewer.config.dataMemoFontColor;

  saveConfigurationFile (CONFIG_FILE_NAME);

  if controller.outOfDate then
     begin
     if MessageDlg('Do you really want to quit? The model is not saved to disk.', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = IDYes then
        canClose := True
     else
        canClose := False;
     end
  else
     canClose := True;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
var
  mu, smu: TMenuItem;
  f : TextFile;
  configOk : boolean;
begin
  fireEvent := false;

//  AssignFile (f, '~/Library/Logs/iridium.log');
//  rewrite (f);
//  writeln (f, 'Start logging');
//  closefile (f);

  controller := TController.Create;
  controller.modelInputManager.setInputMemo(moModel);

  controller.setTimeEnd (strtofloat (edtTimeEnd.Text));
  controller.setTimeStart (strtofloat (edtTimeStart.Text));
  controller.setNumberOfPoints (strtoint (edtNumberOfPoints.Text));
  //controller.viewerPackage.autoXScale := False;
  //controller.viewerPackage.autoYScale := False;
  controller.viewerPackage.showLegend := True;

  //frameSplitPanel.setUp;

  configOk := readConfigurationFile (CONFIG_FILE_NAME);
  frmMain.Width := configOpts.mainConfig.formWidth;
  frmMain.Height := configOpts.mainConfig.formHeight;
  pnlOutputPanel.width := configOpts.mainConfig.outputPanelWidth;

  plotViewer := TPlotFrameViewer.Create(self);
  plotViewer.Parent := frameSplitPanel.pnlUpper;
  plotViewer.Align := TAlignLayout.Client;
  plotViewer.Setup(controller);

  tableViewer := TTableFrameViewer.Create (self);
  tableViewer.Parent := frameSplitPanel.pnlLower;
  tableViewer.Align := TAlignLayout.Client;
  tableViewer.Setup(controller, configOpts.textFormViewer);

  //chkAutoscaleX.IsChecked := True;

  plotViewer.plt.Width := configOpts.mainConfig.outputPanelWidth;
  controller.modelInputManager.modelMemo.Font.Size := configOpts.modelInputManagerConfig.fontSize;
  //controller.modelInputManager.config.fontSize := configOpts.modelInputManagerConfig.fontSize;

  tableViewer.config.dataMemoBackgroundColor := configOpts.textFormViewer.dataMemoBackgroundColor;
  tableViewer.config.dataMemoFontColor := configOpts.textFormViewer.dataMemoFontColor;

  currentAntimonyFileName := '';
  launchPath := ExtractFileDir(ParamStr(0));
  selectedPalette := 'Default';
  if TOSVersion.Platform <> pfMacOS then
    begin
      // Add windows help menu
      mu := TMenuItem.Create(frmMain);
      mu.Parent := rrMainMenu;
      mu.enabled := true;
      mu.Text := 'Help';
      mu.visible := true;
      rrMainMenu.AddObject(mu);

      smu := TMenuItem.Create(mu);
      smu.Text := 'Help....';
      smu.visible := true;
      smu.OnClick := mnuHelpClick;
      mu.AddObject(smu);

      smu := TMenuItem.Create(frmMain);
      smu.enabled := true;
      smu.Text := 'Help on Antimony';
      smu.visible := true;
      smu.OnClick := mnuHelpAntimonyClick;
      mu.AddObject(smu);

      smu := TMenuItem.Create(mu);
      smu.Text := '-';
      smu.visible := true;
      smu.OnClick := mnuAboutClick;
      mu.AddObject(smu);

      smu := TMenuItem.Create(mu);
      smu.Text := 'About Iridium';
      smu.visible := true;
      smu.OnClick := mnuAboutClick;
      mu.AddObject(smu);
    end
  else
    begin
      // Add special app menu for Mac
      mu := TMenuItem.Create(frmMain);
      mu.enabled := true;
      mu.Text := 'DummyText';
      mu.visible := true;
      rrMainMenu.InsertObject(0, mu);

      smu := TMenuItem.Create(mu);
      smu.Text := 'About Iridium';
      smu.visible := true;
      smu.OnClick := mnuAboutClick;
      mu.AddObject(smu);

      smu := TMenuItem.Create(frmMain);
      smu.enabled := true;
      smu.Text := 'Help on Antimony';
      smu.visible := true;
      smu.OnClick := mnuHelpAntimonyClick;
      mu.AddObject(smu);

      smu := TMenuItem.Create(mu);
      smu.Text := '-';
      smu.visible := true;
      mu.AddObject(smu);
    end;

  fireEvent := True;
  lblVersion.text := 'RoadRunner Version: ' + controller.simulator.roadrunner.getVersionStr;
end;


procedure TfrmMain.FormShow(Sender: TObject);
begin
  frameSplitPanel.setPercentageUpperHeight (configOpts.mainConfig.upperOutputPanelHeight);

  if configOpts.mainConfig.IsGraphPanelOpen then
     frameSplitPanel.setUpperVisible
  else
     frameSplitPanel.setUpperInVisible;

  if not configOpts.mainConfig.IsTabularPanelOpen then
     frameSplitPanel.setLowerInVisible;
end;


procedure TfrmMain.chkAutoscaleXChange(Sender: TObject);
begin
  //controller.viewerPackage.autoXScale := chkAutoscaleX.IsChecked;
end;


procedure TfrmMain.chkAutoScaleYChange(Sender: TObject);
begin
  //controller.viewerPackage.autoYScale := chkAutoscaleY.IsChecked;
end;


procedure TfrmMain.OnModelChange;
begin
  controller.outOfDate := true;
  if Assigned(frmSliders) then
    begin
      frmSliders.Free;
      frmSliders := nil;
    end;
  controller.clearViewers;
  controller.ViewerModelHasChanged(self);
end;


end.


