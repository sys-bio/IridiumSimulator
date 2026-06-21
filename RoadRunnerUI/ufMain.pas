unit ufMain;

interface

{DEFINE PYTHON}

uses
  System.SysUtils,
  System.Devices,
  System.Types,
  System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ExtCtrls, System.Math.Vectors, FMX.Memo, FMX.TabControl,
  uPlottingPanel, uSubGraph, uRRDataSeries, uRRTypes, FMX.Platform,
  System.UIConsts, ufGraphPackageDialog, FMX.ListBox, FMX.Objects,
  System.Rtti, FMX.Grid, FMX.TreeView,
  FMX.RichEdit.Style,
  FMX.Filter.Effects,
  FMX.Menus,
  FMX.EditBox,
  FMX.Edit,
  uCoyoteCommon,
  strutils,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.SpinBox,
  FMX.Controls.Presentation,
  FMX.Memo.Types,
  FMX.Skia,
  Skia,
  FMX.Colors,
  uRRList,
  uSetup,
  ufStructuralAnalysis,
  uNewMatrix,
  uRR2DSimpleMatrix,
  uColorList,
  ufScrollChart,
  uController,
  uViewer,
  uPlotFrameViewer,
  uTableFrameViewer,
  uFrameFunctionPlotter,
  uFrameScanControl,
  uFrameSteadyStateControl,
  uFrameForSliders,
  uPythonFrame,
  uPythonIOFrame,
  ufFrameSplitPanel,
  uConfiguration,
  uModelInputManager,
  ufFrameViewerBase,
  ufMainConfig,
  uAntimonyAPI,
  ufPythonIO,
  uModelState,
  SkPlotPaintBox,
  FrameMemoLineCount
{$IFDEF PYTHON}
    , WrapDelphi, PythonEngine, WrapDelphiClasses, FMX.PythonGUIInputOutput, uHostAPI, uPlotAPI
{$ENDIF}
{$IFDEF MSWINDOWS}
    , Winapi.Windows, Winapi.ShellAPI
{$ENDIF}
{$IFDEF POSIX}
    , Posix.Stdlib
{$ENDIF POSIX};

const
  VERSION = '0.910 Beta';

type
  TPatternElement = record
      Id : String;
      Selected: Boolean;
  end;

  TfrmMain = class(TForm)
    Layout2: TLayout;
    Layout3: TLayout;
    pnlMain: TLayout;
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
    MineShaft_Win_Style: TStyleBook;
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
    tbAntimony: TTabControl;
    tbModelDefinition: TTabItem;
    Layout4: TLayout;
    Panel2: TPanel;
    Layout8: TLayout;
    btnOpenGraph: TSpeedButton;
    Image5: TImage;
    btnOpenTabular: TSpeedButton;
    Image11: TImage;
    pnlOutputPanelBase: TLayout;
    pnlModelAndControlsPanel: TLayout;
    Layout7: TLayout;
    Splitter1: TSplitter;
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
    Layout11: TLayout;
    GroupBox2: TGroupBox;
    cbXAxis: TComboBox;
    Layout1: TLayout;
    btnClose: TButton;
    lblVersion: TLabel;
    Calypso_Win_Style: TStyleBook;
    cboStyleList: TComboBox;
    chkAlwaysReset: TCheckBox;
    lblFontSize: TLabel;
    MenuItem1: TMenuItem;
    mnuExportPython: TMenuItem;
    SaveCombineArchive: TSaveDialog;
    TabControl_Viewers: TTabControl;
    TabItem_PlotView: TTabItem;
    TabItem_TextView: TTabItem;
    pnlMiscPanel: TLayout;
    slitterMiscPanel: TSplitter;
    Rectangle3: TRectangle;
    muSpecialFunctions: TMenuItem;
    muRecordEndPoints: TMenuItem;
    mnuPython: TMenuItem;
    TabControl: TTabControl;
    pnlOutputPanel: TLayout;
    Splitter2: TSplitter;
    Rectangle7: TRectangle;
    TabOutputControl: TTabControl;
    MemoAntimony: TMemo;
    spFontSize: TSpinBox;
    chkShowLineNumbers: TCheckBox;
    StyleBookWinUI3: TStyleBook;
    StyleBookUI3: TStyleBook;
    btnSaveSelectionPattern: TButton;
    btnLoadSavedPattern: TButton;
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
    procedure btnStructAnalysisClick(Sender: TObject);
    procedure mnuExamplesClick(Sender: TObject);
    procedure mnuLoadAntimonyModelClick(Sender: TObject);
    procedure mnuSaveAntFileClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuSaveAntimonyFileAsClick(Sender: TObject);
    procedure mnuImportSBMLClick(Sender: TObject);
    procedure mnuExportSBMLClick(Sender: TObject);
    procedure moModelImmediateChange(Sender: TObject);
    procedure btnTimeCourseSlidersClick(Sender: TObject);
    procedure btnRealTimeToolClick(Sender: TObject);
    procedure mnuUseFloatingGraphClick(Sender: TObject);
    procedure edtNumberOfPointsChange(Sender: TObject);
    procedure edtTimeEndChange(Sender: TObject);
    procedure edtTimeStartChange(Sender: TObject);
    procedure btnFunctionPlotClick(Sender: TObject);
    procedure btnUnSelectAllClick(Sender: TObject);
    procedure mnuPreferencesClick(Sender: TObject);
    procedure moModel2Change(Sender: TObject);
    procedure mnuConfigClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cboStyleListChange(Sender: TObject);
    procedure chkShowLineNumbersChange(Sender: TObject);
    procedure MemoAntimonyChangeTracking(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure mnuExportPythonClick(Sender: TObject);
    procedure mnuPythonClick(Sender: TObject);
    procedure moModelmTxtChange(Sender: TObject);
    procedure moModelmTxtChangeTracking(Sender: TObject);
    procedure moModelmTxtPresentationNameChoosing(Sender: TObject; var
        PresenterName: string);
    procedure spFontSizeChange(Sender: TObject);
    procedure TabControl_ViewersChange(Sender: TObject);
    procedure btnSaveSelectionPatternClick(Sender: TObject);
    procedure btnLoadSavedPatternClick(Sender: TObject);
  private
    { Private declarations }
    currentAntimonyFileName: string;
    logFileName: AnsiString;
    launchPath: string;
    sbmlString: AnsiString;
    currentTime: double;
    SavedSelectionPattern : TArray<TPatternElement>;
    FrameScanControl: TFrameScanControl;
    procedure openGraphPanel;
    procedure closeGraphPanel;
    procedure openTabularPanel;
    procedure closeTabularPanel;
    procedure mnuHelpClick(Sender: TObject);
    procedure mnuHelpAntimonyClick(Sender: TObject);
    procedure collectModelSymbols;
    procedure bringUpSelectionForm(selectionList: TStringList; includeTime: Boolean);
    procedure OnModelChange;
    procedure ClearAll;
    procedure loadModelIntoMemo(antStr: string);
    function loadBuiltInModel(name: string): TModelErrorState; overload;
    function loadBuiltInModel(index: integer): TModelErrorState; overload;
    procedure loadTemplateModel;
    function loadModelFromMemo: TModelErrorState;
    procedure AppException(Sender: TObject; E: Exception);
    procedure ViewerModelHasChanged;
    procedure OnTimeCourseSliderNotify(parameter: string; value: double; runSimulation : boolean);
    procedure OnReRunSimulation(parameters: TArray<string>; values: TArray<double>);
    procedure OnRunSimulation;
    procedure updateConfigInformation;
    procedure freeSliderPanel;
    procedure freePythonFrame;
    procedure freePythonIOFrame;
    procedure InitPython;
  public
    { Public declarations }

{$IFDEF PYTHON}
    PythonEngine: TPythonEngine;
    PythonModule: TPythonModule;
    PythonGUIInputOutput: TPythonGUIInputOutput;
    DelphiWrapper: TPyDelphiWrapper;
    DelphiPlotWrapper : TPyDelphiWrapper;
    host : THostAPI;
    plot : TPlotAPI;
{$ENDIF}
    config: TfrmMainConfig;
    antimonyLoaded: Boolean;
    PythonSystemActive: Boolean;
    roadRunnerLoaded: Boolean;
    errMsg: AnsiString;
    fireEvent: Boolean;
    conservedMoietyAnalysis: Boolean;
    floatingSpeciesIds: TStringList;
    boundarySpeciesIds: TStringList;
    reactionRatesIds: TStringList;
    ratesOfChangeIds: TStringList;
    elasticityIds: TRRList; // Use RRList because its nested
    globalParameters: TStringList;
    selectedPalette: string;
    controller: TController;
    plotViewer: TPlotFrameViewer;
    tableViewer: TTableFrameViewer;
    sliderFrame: TframeForSliders;
    pythonFrame: TPythonFrame;
    pythonIOFrame : TPythonIOFrame;
    modelState : TModelState;
    previousModelState : TModelState;
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

Uses uRoadRunner.API,
  Math,
  FMX.BehaviorManager,
  uSymbolDetails,
  uRRCommon,
  ufSelectionChoices,
  uBuiltInModels,
  ufSliders,
  uComplex,
  ufIntegratorOptions,
  ufSteadyStateOptions,
  FMX.Styles.Objects,
  FMX.DialogService,
  ufMoreSteadyState,
  uParameterScan,
  ufFloatingPlotViewer,
  IOUtils,
  uRoadRunner,
  ufExamples,
  ufAbout,
  uSimulator,
  uViewerTypes,
  uScanArguments,
  uTimeCourseConfig,
  ufPreferences,
  uMakePython,
  uCombineArchive;

const
  TIMECOURSE_FUNCTION = 0;

//  // Create the host api package that plugins can import
//constructor TPyDelphi.Create(APythonType: TPythonType);
//begin
//  inherited;
//  // we need to set DelphiObject property
//  DelphiObject := THostAPI.Create;
//  Owned := True; // We own the objects we create
//end;
//
//
//constructor TPyDelphi.CreateWith(PythonType: TPythonType; args, kwds: PPyObject);
//begin
//  inherited;
//  with GetPythonEngine, DelphiObject as THostAPI do
//    begin
//      if PyArg_ParseTuple(args, ':Createapp') = 0 then
//        Exit;
//    end;
//end;
//
//class function TPyDelphi.DelphiObjectClass: TClass;
//begin
//  Result := THostAPI;
//end;


// ------------------------------------------------------------------------
// Not yet operational
procedure TfrmMain.selectIntegrator;
var
  i: integer;
  sl: TStringList;
  currentName: string;
begin
  TDialogService.ShowMessage('Configuration panel is not yet implemented');
  Exit;
  if frmIntegratorOptions = nil then
    begin
      frmIntegratorOptions := TfrmIntegratorOptions.Create(nil);
      // HMS frmIntegratorOptions.StyleBook := StyleBook1;
    end;
  sl := uRoadRunner.API.getListOfRegisteredIntegrators();
  try
    frmIntegratorOptions.lstSolverNames.Items.Assign(sl);
    currentName := uRoadRunner.API.getCurrentIntegratorName;
    frmIntegratorOptions.disableChange := True;
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
    roadRunnerLoaded := false
  else
    roadRunnerLoaded := True;
end;


function TfrmMain.loadBuiltInModel(index: integer): TModelErrorState;
var
  model: TBuiltInModel;
  errMsg: string;
  modelErrorState: TModelErrorState;
begin
  model := builtInModels[index];
  modelErrorState := controller.modelInputManager.getSBMLFromAntimony(model.modelStr);
  if modelErrorState.ok then
    begin
      controller.loadSBMLModelFromString(modelErrorState.sbmlStr, conservedMoietyAnalysis);
      loadModelIntoMemo(model.modelStr);
      controller.ViewerSetProperty('UserScale_Xmin', model.Xmin);
      controller.ViewerSetProperty('UserScale_Xmax', model.Xmax);
      controller.ViewerSetProperty('UserScale_Ymin', model.Ymin);
      controller.ViewerSetProperty('UserScale_Ymax', model.Ymax);
      edtTimeEnd.Text := model.timeEnd;
      edtNumberOfPoints.Text := inttostr(model.numberOfPoints);
      controller.setTimeEnd(strtofloat(model.timeEnd));
      controller.setNumberOfPoints(model.numberOfPoints);
    end;
  Result := modelErrorState;
end;


function TfrmMain.loadBuiltInModel(name: string): TModelErrorState;
var
  model: TBuiltInModel;
  sbmlStr: AnsiString;
  modelErrorState: TModelErrorState;
begin
  OnModelChange;
  model := builtInModels.getBuiltInModel(name);
  modelErrorState := controller.modelInputManager.getSBMLFromAntimony(model.modelStr);
  if modelErrorState.ok then
    begin
      controller.loadSBMLModelFromString(sbmlStr, conservedMoietyAnalysis);
      loadModelIntoMemo(model.modelStr);
      controller.ViewerSetProperty('UserScale_Xmin', model.Xmin);
      controller.ViewerSetProperty('UserScale_Xmax', model.Xmax);
      controller.ViewerSetProperty('UserScale_Ymin', model.Ymin);
      controller.ViewerSetProperty('UserScale_Ymax', model.Ymax);
      edtTimeEnd.Text := model.timeEnd;
      edtNumberOfPoints.Text := inttostr(model.numberOfPoints);
      controller.setTimeEnd(strtofloat(model.timeEnd));
      controller.setNumberOfPoints(model.numberOfPoints);
    end;
  Result := modelErrorState;
end;


procedure TfrmMain.lstYAxisChange(Sender: TObject);
var
  i: integer;
begin
  if fireEvent then
    begin
      for i := 0 to lstYAxis.Items.Count - 1 do
        begin
          controller.viewerPackage.YColumnChoice[i] := (lstYAxis.listitems[i] as Tlistboxitem).IsChecked;
          controller.viewerPackage.YColumnNames[i] := (lstYAxis.listitems[i] as Tlistboxitem).Text;
        end;
      controller.updateViewers;
    end;
end;


procedure TfrmMain.mnuConfigClick(Sender: TObject);
begin
  updateConfigInformation;
  saveConfigurationFile(CONFIG_FILE_NAME);
end;


procedure TfrmMain.mnuSaveAntFileClick(Sender: TObject);
begin
  if currentAntimonyFileName = '' then
    begin
      if SaveAntimonyFileDialog.Execute then
        begin
          currentAntimonyFileName := SaveAntimonyFileDialog.FileName;
          frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
          TFile.WriteAllText(currentAntimonyFileName, MemoAntimony.lines.Text);
        end;
    end
  else
    TFile.WriteAllText(currentAntimonyFileName, MemoAntimony.lines.Text);
end;


procedure TfrmMain.mnuSaveAntimonyFileAsClick(Sender: TObject);
begin
  if SaveAntimonyFileDialog.Execute then
    begin
      currentAntimonyFileName := SaveAntimonyFileDialog.FileName;
      frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
      TFile.WriteAllText(currentAntimonyFileName, MemoAntimony.lines.Text);
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
  frmFloatingPlotViewer.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
  frmFloatingPlotViewer.Show;
end;


procedure TfrmMain.mnuHelpClick(Sender: TObject);
begin
  ShowMessage('There is no help at present: You''re on your own....' + sLineBreak + sLineBreak +
    'Hint: Press the simulate button');
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
  frmAbout.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
  frmAbout.lblRoadRunner.Text := 'Using libroadRunner version: ' + str;
  frmAbout.lbllibSBML.Text := 'Using libSBML vesion: ' + controller.simulator.roadrunner.getlibSBMLVersion();
  frmAbout.lbSkia.Text := 'Using skia: ' + Skia.SkVersion + ', Milestone: ' + SkVersion;
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
  frmExamples.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
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
  path: string;
begin
  sbmlStr := controller.modelInputManager.importSBML(path);
  if sbmlStr = '' then
    Exit;
  controller.loadSBMLModelFromString(sbmlStr, conservedMoietyAnalysis);
  antStr := controller.modelInputManager.getAntimonyFromSBML(sbmlStr);
  loadModelIntoMemo(antStr);
  currentAntimonyFileName := ChangeFileExt(path, '.ant');
  frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
  if loadModelFromMemo.ok then
    populateXYSelectors; // required to update viewerpackage in controller
end;


procedure TfrmMain.mnuLoadAntimonyModelClick(Sender: TObject);
var
  errMsg: string;
  modelErrorState: TModelErrorState;
begin
  if OpenAntimonyDialog.Execute then
    begin
      if controller.modelInputManager.loadAntimonyFromFile(OpenAntimonyDialog.FileName, errMsg) then
        begin
          modelErrorState := uAntimonyAPI.getSBMLFromAntimony(controller.modelInputManager.antimonyStr);
          if not modelErrorState.ok then
            begin
              TDialogService.ShowMessage(modelErrorState.errMsg);
              Exit();
            end;

          freeSliderPanel;
          controller.loadSBMLModelFromString(modelErrorState.sbmlStr, conservedMoietyAnalysis);
          loadModelIntoMemo(controller.modelInputManager.antimonyStr);
          if loadModelFromMemo.ok then;
             populateXYSelectors;
        end
      else
        raise Exception.Create('Error LoadAntimonyModel: ' + errMsg);
      currentAntimonyFileName := OpenAntimonyDialog.FileName;
      frmMain.Caption := 'Iridium: ' + currentAntimonyFileName;
    end;
end;


procedure TfrmMain.OnPickExample(index: integer);
begin
  freeSliderPanel;
  loadBuiltInModel(index);
  if loadModelFromMemo.ok then
    populateXYSelectors; // required to update viewerpackage in controller
end;


procedure TfrmMain.loadModelIntoMemo(antStr: string);
begin
  OnModelChange;
  MemoAntimony.lines.Text := antStr; // getAntimonyFromSBML(sbmlStr);
  collectModelSymbols;
end;


procedure TfrmMain.mnuMacQuitClick(Sender: TObject);
begin
  updateConfigInformation;
  saveConfigurationFile(CONFIG_FILE_NAME);
  if controller.outOfDate then
    begin
      if MessageDlg('Do you really want to quit, the model is not saved to disk', TMsgDlgType.mtConfirmation,
        [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = IDYes then
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
  if FileExists(launchPath + '\\template.txt') then
    MemoAntimony.lines.LoadFromFile(launchPath + '\\template.txt');
  modelState.update(controller);
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
    frmPreferences.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
    frmPreferences.ShowModal;
    controller.modelInputManager.setMemoFontSize(configOpts.modelInputManagerConfig.fontSize);
  finally
    frmPreferences.Free;
  end;
end;


procedure TfrmMain.mnuWinQuitClick(Sender: TObject);
begin
  updateConfigInformation;
  saveConfigurationFile(CONFIG_FILE_NAME);

  if controller.outOfDate then
    begin
      if MessageDlg('Do you really want to quit, the model is not saved to disk', TMsgDlgType.mtConfirmation,
        [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = IDYes then
        Application.Terminate;
    end
  else
    Application.Terminate;
end;


// Only fired after loosing focus from the model memo
procedure TfrmMain.moModel2Change(Sender: TObject);
begin
  controller.modelChanged;
end;


// Only fired if the user changes the text on the
// model memo but without losoing focus.
procedure TfrmMain.moModelImmediateChange(Sender: TObject);
begin
  controller.outOfDate := True;
  controller.modelInputManager.antimonyStr := MemoAntimony.lines.Text;
end;


procedure TfrmMain.btnConfigIntegratorClick(Sender: TObject);
begin
  selectIntegrator;
end;


procedure TfrmMain.btnFunctionPlotClick(Sender: TObject);
var
  t: TTabItem;
  frame: TframeFunctionPlotter;
  i: integer;
begin
  for i := 0 to FunctionTabControl.TabCount - 1 do
    if FunctionTabControl.Tabs[i].Text = 'Function Plotter' then
      begin
        FunctionTabControl.TabIndex := i;
        Exit;
      end;
  t := FunctionTabControl.Add;
  t.Text := 'Function Plotter';
  frame := TframeFunctionPlotter.Create(FunctionTabControl);
  frame.stylebook1 := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
  frame.Parent := t;
  frame.Align := TAlignLayout.Client;
  frame.Visible := True;
  frame.controller := controller;
  FunctionTabControl.TabIndex := 3;
end;


procedure TfrmMain.btnLoadSavedPatternClick(Sender: TObject);
var i, j : Integer;
begin
  if length (SavedSelectionPattern) = 0 then exit;

  for i := 0 to lstYAxis.Count - 1 do
      begin
      for j := 0 to length (SavedSelectionPattern) - 1 do
          begin
          if (lstYAxis.listitems[i] as Tlistboxitem).Text = SavedSelectionPattern[j].Id then
              begin
              if SavedSelectionPattern[j].Selected then
                 begin
                 (lstYAxis.listitems[i] as Tlistboxitem).IsChecked := True;
                 controller.viewerPackage.YColumnChoice[i] := True;
                 end
              else
                 begin
                 (lstYAxis.listitems[j] as Tlistboxitem).IsChecked := False;
                 controller.viewerPackage.YColumnChoice[i] := False;
                 end;
              end;
          end;
      end;
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
    setlength(controller.viewerPackage.YColumnNames, currentSelectionList.Count);
    for i := 0 to currentSelectionList.Count - 1 do
      begin
        lstYAxis.Items.Add(currentSelectionList[i]);
        controller.viewerPackage.YColumnNames[i] := currentSelectionList[i];
        cbXAxis.Items.Add(currentSelectionList[i]);
      end;
    cbXAxis.itemindex := 0;
    controller.viewerPackage.XAxisTitle := currentSelectionList[0];
    controller.viewerPackage.XColumnIndex := 0;
    setlength(controller.viewerPackage.YColumnChoice, currentSelectionList.Count);
    if currentSelectionList.Count = 0 then
      Exit;
    (lstYAxis.listitems[0] as Tlistboxitem).IsChecked := false;
    controller.viewerPackage.YColumnChoice[0] := false;
    for i := 1 to currentSelectionList.Count - 1 do
      begin
        (lstYAxis.listitems[i] as Tlistboxitem).IsChecked := True;
        controller.viewerPackage.YColumnChoice[i] := True;
      end;
    lstYAxis.itemindex := -1;
  finally
    fireEvent := True;
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
end;


procedure TfrmMain.closeGraphPanel;
begin
end;


procedure TfrmMain.openTabularPanel;
begin
end;


procedure TfrmMain.closeTabularPanel;
begin
end;


procedure TfrmMain.btnRealTimeToolClick(Sender: TObject);
var
  list: TStringList;
  sbmlStr: string;
  modelErrorState: TModelErrorState;
begin
  try
    if frmScrollChart = nil then
      frmScrollChart := TfrmScrollChart.Create(nil, controller);
    frmScrollChart.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
    list := controller.simulator.roadrunner.getFloatingSpeciesIds();
    try
      modelErrorState := controller.modelInputManager.getSBMLFromAntimony(MemoAntimony.lines.Text);
      if modelErrorState.ok then
        begin
          controller.loadSBMLModelFromString(modelErrorState.sbmlStr, True);
          collectModelSymbols;
          ViewerModelHasChanged;
          frmScrollChart.Show;
        end;
    finally
      list.Free;
    end;
  except
    on E: Exception do
      TDialogService.ShowMessage(E.message);
  end;
end;


procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  controller.simulator.roadrunner.reset();
end;


procedure TfrmMain.edtNumberOfPointsChange(Sender: TObject);
var
  value: integer;
begin
  if TryStrToInt(edtNumberOfPoints.Text, value) then
    begin
      if value <= 0 then
        ShowMessage('The number of points must be a positive non-zero integer, try again')
      else
        begin
          if value < 500000 then
            controller.setNumberOfPoints(strtoint(edtNumberOfPoints.Text))
          else
            ShowMessage('Generating more than half a million points migth cause problems, try a smaller number');
        end;
    end
  else
    ShowMessage('The number of points must be an integer, try again');
end;


procedure TfrmMain.edtTimeEndChange(Sender: TObject);
var
  value: double;
begin
  if TryStrToFloat(edtTimeEnd.Text, value) then
    begin
      if value < controller.getTimeStart then
        ShowMessage('The time end cannnot be smaller than the time start, try again')
      else
        controller.setTimeEnd(value);
    end
  else
    ShowMessage('The simulation time end must be a floating point number, try again');
end;


procedure TfrmMain.edtTimeStartChange(Sender: TObject);
var
  value: double;
begin
  if TryStrToFloat(edtTimeStart.Text, value) then
    controller.setTimeStart(value)
  else
    ShowMessage('The simulation time start must be a floating point number, try again');
end;


procedure TfrmMain.doTimeCourseSimulationAndPlot;
begin
  controller.runTimeCourseSimulationAndPlot;
  btnTimeCourseSliders.enabled := True;
end;


function compareTwoStringLists(s1, s2: TStringList): Boolean;
begin
  if s1.Count <> s2.Count then
    Exit(false);
  for var i := 0 to s1.Count - 1 do
    if s1[i] <> s2[i] then
      Exit(false);
  Exit(True);
end;


function compareModels(r1, r2: TRoadRunner): Boolean;
var
  r1_floatingSpeciesIds: TStringList;
  r1_boundarySpeciesIds: TStringList;
  r1_reactionRatesIds: TStringList;
  r1_globalParameters: TStringList;
  r2_floatingSpeciesIds: TStringList;
  r2_boundarySpeciesIds: TStringList;
  r2_reactionRatesIds: TStringList;
  r2_globalParameters: TStringList;
begin
  r1_floatingSpeciesIds := r1.getFloatingSpeciesIds;
  r1_floatingSpeciesIds.Sort;
  r1_boundarySpeciesIds := r1.getBoundarySpeciesIds;
  r1_boundarySpeciesIds.Sort;
  r1_reactionRatesIds := r1.getReactionIds;
  r1_reactionRatesIds.Sort;
  r1_globalParameters := r1.getGlobalParameterIds;
  r1_globalParameters.Sort;
  r2_floatingSpeciesIds := r2.getFloatingSpeciesIds;
  r2_floatingSpeciesIds.Sort;
  r2_boundarySpeciesIds := r2.getBoundarySpeciesIds;
  r2_boundarySpeciesIds.Sort;
  r2_reactionRatesIds := r2.getReactionIds;
  r2_reactionRatesIds.Sort;
  r2_globalParameters := r2.getGlobalParameterIds;
  r2_globalParameters.Sort;
  if not compareTwoStringLists(r1_floatingSpeciesIds, r2_floatingSpeciesIds) then
    Exit(false);
  if not compareTwoStringLists(r1_boundarySpeciesIds, r2_boundarySpeciesIds) then
    Exit(false);
  if not compareTwoStringLists(r1_reactionRatesIds, r2_reactionRatesIds) then
    Exit(false);
  if not compareTwoStringLists(r1_globalParameters, r2_globalParameters) then
    Exit(false);
  // Structure appears the same
  Exit(True);
end;


function TfrmMain.loadModelFromMemo: TModelErrorState;
var
  r: TRoadRunner;
  structureIsTheSame: Boolean;
  modelErrorState: TModelErrorState;
begin
  if controller.outOfDate then
    begin
      try
        modelErrorState := controller.modelInputManager.getSBMLFromAntimony(MemoAntimony.lines.Text);
        if modelErrorState.ok then
          begin
            controller.modelInputManager.antimonyStr := MemoAntimony.lines.Text;
            modelState.update(controller);
            r := TRoadRunner.Create;
            try
              r.setComputeAndAssignConservationLaws(conservedMoietyAnalysis);
              r.loadSBMLFromString(modelErrorState.sbmlStr);
              structureIsTheSame := compareModels(controller.simulator.roadrunner, r);
              controller.loadSBMLModelFromString(modelErrorState.sbmlStr, conservedMoietyAnalysis);
              collectModelSymbols;
              ViewerModelHasChanged;
              controller.outOfDate := false;
              if not structureIsTheSame then
                populateXYSelectors; // required to update viewerpackage in controller
            finally
              r.Free;
            end;
          end
        else
          Exit(modelErrorState);
      except
        on E: Exception do
          begin
            ShowMessage(E.message);
            modelErrorState.ok := false;
            modelErrorState.errMsg := '';
            Exit(modelErrorState);
          end;
      end;
    end;
  modelErrorState.ok := True;
  Result := modelErrorState;
end;


procedure TfrmMain.btnSimulateClick(Sender: TObject);
var
  modelErrorState: TModelErrorState;
begin
  try
    modelErrorState := loadModelFromMemo();
    if modelErrorState.ok then
      begin
        if chkAlwaysReset.IsChecked then
          controller.simulator.roadrunner.reset();
        modelErrorState := controller.runTimeCourseSimulationAndPlot;
        if not modelErrorState.ok then
          begin
            ShowMessage(modelErrorState.errMsg);
            Exit;
          end;
      end
    else
      begin
        if modelErrorState.errMsg <> '' then
          ShowMessage(modelErrorState.errMsg);
      end;
  finally
    btnSimulate.SetFocus;
  end;
end;


procedure TfrmMain.btnSteadyStateClick(Sender: TObject);
var
  t: TTabItem;
  frame: TFrameSteadyStateControl;
  i: integer;
begin
  for i := 0 to FunctionTabControl.TabCount - 1 do
    if FunctionTabControl.Tabs[i].Text = 'Steady-State Control' then
      begin
        FunctionTabControl.TabIndex := i;
        Exit;
      end;
  t := FunctionTabControl.Add;
  t.Text := 'Steady-State Control';
  frame := TFrameSteadyStateControl.Create(FunctionTabControl);
  // So that any floating forms made by frame can be assigned a style
  frame.stylebook1 := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
  frame.Parent := t;
  frame.Align := TAlignLayout.Client;
  frame.Visible := True;
  frame.controller := controller;
  FunctionTabControl.ActiveTab := t;
  loadModelFromMemo;
end;


procedure TfrmMain.btnStructAnalysisClick(Sender: TObject);
var
  modelErrorState: TModelErrorState;
begin
  modelErrorState := loadModelFromMemo;
  if modelErrorState.ok then
    begin
      if not Assigned(frmStructuralAnalysis) then
        frmStructuralAnalysis := TfrmStructuralAnalysis.Create(frmMain);
      frmStructuralAnalysis.controller := controller;
      frmStructuralAnalysis.Show;
      frmStructuralAnalysis.BringToFront;
    end
  else
    ShowMessage(modelErrorState.errMsg);
end;


procedure TfrmMain.btnTimeCourseClick(Sender: TObject);
begin
  FunctionTabControl.TabIndex := TIMECOURSE_FUNCTION;
  Controller.OutOfDate := True;
  Controller.ClearViewers;
  populateXYSelectors;

  if Assigned (SliderFrame) then
     SliderFrame.SetFMXControlsEnabled(SliderFrame, True);
end;


procedure TfrmMain.ViewerModelHasChanged;
begin
  controller.ViewerModelHasChanged(self);
end;


procedure TfrmMain.OnTimeCourseSliderNotify(parameter: string; value: double; runSimulation : boolean);
begin
  controller.simulator.roadrunner.setValue(parameter, value);
  controller.simulator.roadrunner.reset;
  if runSimulation then
     controller.runTimeCourseSimulationAndPlot;
end;


procedure TfrmMain.OnRunSimulation;
begin
  controller.simulator.roadrunner.reset;
  controller.runTimeCourseSimulationAndPlot;
end;


procedure TfrmMain.OnReRunSimulation(parameters: TArray<string>; values: TArray<double>);
var
  i: integer;
begin
  for i := 0 to length(parameters) - 1 do
    controller.simulator.roadrunner.setValue(parameters[i], values[i]);
  controller.simulator.roadrunner.reset;
  controller.runTimeCourseSimulationAndPlot;
end;


procedure TfrmMain.freeSliderPanel;
var i : integer;
begin
  if Assigned(sliderFrame) then
    begin
      sliderFrame.shutDown;
      sliderFrame.Free;
      sliderFrame := nil;
      for i := 0 to TabControl.TabCount - 1 do
        if TabControl.Tabs[i].Text = 'Sliders' then
          begin
            TabControl.Delete(i);
            break;
          end;
      Exit;
    end;
end;


procedure TfrmMain.freePythonFrame;
var i : integer;
begin
  if Assigned(pythonFrame) or Assigned (pythonIOFrame) then
    begin
      pythonFrame.shutDown;
      pythonFrame.Free;
      pythonFrame := nil;

      freePythonIOFrame;

      for i := 0 to TabOutputControl.TabCount - 1 do
        if TabOutputControl.Tabs[i].Text = 'Python' then
          begin
            TabOutputControl.Delete(i);
            break;
          end;

      for i := 0 to TabControl.TabCount - 1 do
        if TabControl.Tabs[i].Text = 'Python' then
          begin
            TabControl.Delete(i);
            break;
          end;
       Exit;
    end;
end;


procedure TfrmMain.freePythonIOFrame;
begin
  if Assigned(pythonFrame) then
    begin
      pythonIOFrame.shutDown;
      pythonIOFrame.Free;
      pythonIOFrame := nil;
    end;
end;


procedure TfrmMain.btnTimeCourseSlidersClick(Sender: TObject);
var
  list: TStringList;
  i: integer;
  value: double;
  t: TTabItem;
begin
  if controller.OutOfDate then
     begin
     showmessage ('Run a simulation before selecting the slider option');
     exit;
     end;

  if Assigned(sliderFrame) then
    begin
      freeSliderPanel;
      for i := 0 to TabControl.TabCount - 1 do
        if TabControl.Tabs[i].Text = 'Sliders' then
          begin
            TabControl.Delete(i);
            break;
          end;

      Exit;
    end;

  t := TTabItem.Create(self);
  t.Parent := TabControl;
  t.Text := 'Sliders';

  sliderFrame := TframeForSliders.Create(self);
  sliderFrame.Parent := t; // pnlMiscPanel;
  sliderFrame.Align := TAlignLayout.Client;
  sliderFrame.Height := configOpts.miscFrameConfig.heigthOfFrame;

  sliderFrame.Setup(self, controller);
  sliderFrame.OnNotifyChange := OnTimeCourseSliderNotify;
  sliderFrame.OnNotifyReSimulation := OnReRunSimulation;
  sliderFrame.OnNotifyRunSimulation := OnRunSimulation;

  sliderFrame.stylebook1 := StyleBook;
  list := controller.simulator.roadrunner.getGlobalParameterIds;
  list.Sort;
  // We can't change total masses for time course
  // simulations, so let's remove any of those entries.
  for i := list.Count - 1 downto 0 do
    if StartsText('_CSUM', list[i]) then
      list.Delete(i);

  for i := sliderFrame.lstParameters.Count - 1 downto 0 do
    if sliderFrame.lstParameters.Items.Objects[i] <> nil then
      sliderFrame.lstParameters.Items.Objects[i].Free;
  sliderFrame.lstParameters.Clear;
  for i := 0 to list.Count - 1 do
    begin
      value := controller.simulator.roadrunner.getValue(list[i]);
      sliderFrame.lstParameters.Items.AddObject(list[i], TSliderInitialValue2.Create(value / 10, 5 * value, value));
    end;

  list.Free;

  Exit;

  // ------------------------------------------------------------

  if not Assigned(frmSliders) then
    frmSliders := TfrmSliders.Create(nil);
  frmSliders.OnNotifyChange := OnTimeCourseSliderNotify;
  frmSliders.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
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
      frmSliders.lstParameters.Items.AddObject(list[i], TSliderInitialValue.Create(value / 10, 5 * value, value));
    end;
  list.Free;
  // frmSliders.Show;
  // frmSliders.BringToFront;
end;


procedure TfrmMain.btnUnSelectAllClick(Sender: TObject);
begin
  fireEvent := false;
  for var i := 0 to lstYAxis.Items.Count - 1 do
    (lstYAxis.listitems[i] as Tlistboxitem).IsChecked := false;
  fireEvent := True;
  lstYAxisChange(Sender);
end;


procedure TfrmMain.Button3Click(Sender: TObject);
begin
end;


// Toolbar/Menu selection of scanning frame
procedure TfrmMain.btnSaveSelectionPatternClick(Sender: TObject);
var i  : Integer;
begin
  SetLength (SavedSelectionPattern,lstYAxis.Count);
  for i  := 0 to lstYAxis.Count - 1 do
      begin
      SavedSelectionPattern[i].Id := (lstYAxis.listitems[i] as Tlistboxitem).Text;
      if (lstYAxis.listitems[i] as Tlistboxitem).IsChecked then
         SavedSelectionPattern[i].Selected := True
      else
         SavedSelectionPattern[i].Selected := False;
      end;
end;


procedure TfrmMain.btnScanClick(Sender: TObject);
var
  TabItem: TTabItem;
  i: integer;
begin
   Controller.OutOfDate := True;
   Controller.ClearViewers;
   loadModelFromMemo();
   populateXYSelectors;

   if Assigned (SliderFrame) then
      SliderFrame.SetFMXControlsEnabled(SliderFrame, False);

   // Look for the scan tab, if found make it active.
   for i := 0 to FunctionTabControl.TabCount - 1 do
      if FunctionTabControl.Tabs[i].Text = 'Scanning Control' then
        begin
          FunctionTabControl.TabIndex := i;
          FrameScanControl.InitializeScanUserInterface;
          Exit;
        end;

  // If not found, create the TabItem
  TabItem := FunctionTabControl.Add;
  Tabitem.Text := 'Scanning Control';
  FrameScanControl := TFrameScanControl.Create(FunctionTabControl);
  FrameScanControl.stylebook1 := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
  FrameScanControl.Parent := TabItem;
  FrameScanControl.Align := TAlignLayout.Client;
  FrameScanControl.Visible := True;
  FrameScanControl.controller := controller;
  FrameScanControl.cboColorPalette.Items.Assign(getPaletteNames);
  FrameScanControl.cboColorPalette.itemindex := 0;
  FunctionTabControl.ActiveTab := TabItem;
  FrameScanControl.InitializeScanUserInterface;
end;


procedure TfrmMain.bringUpSelectionForm(selectionList: TStringList; includeTime: Boolean);
var
  Item1, Item2, Item3: TTreeViewItem;
  i, j: integer;
begin
  frmSelectionChoices.Free;
  // This is to avoid visual corruption of calling form (reported to qc)
  frmSelectionChoices := TfrmSelectionChoices.Create(nil);
  frmSelectionChoices.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;
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
  modelErrorState: TModelErrorState;
begin
  if controller.outOfDate then
    begin
      TDialogService.ShowMessage('Run the simulation before selecting outputs');
      Exit;
    end;
  modelErrorState := loadModelFromMemo;
  if modelErrorState.ok then
    begin
      // Include time as a choice
      bringUpSelectionForm(TStringList(lstYAxis.Items), True);
      selectionList := TStringList.Create;
      try
        for i := 0 to frmSelectionChoices.lstSelectedItems.Count - 1 do
          selectionList.Add(frmSelectionChoices.lstSelectedItems.Items[i]);
        controller.setSelectionList(selectionList);
      finally
        selectionList.Free;
      end;
      populateXYSelectors;
      controller.clearViewers;
      controller.simulator.InvalidateSimulationData;
    end
  else
    ShowMessage(modelErrorState.errMsg);
end;


procedure TfrmMain.cboStyleListChange(Sender: TObject);
begin
  StyleBook := TStyleBook(cboStyleList.listitems[cboStyleList.itemindex].Data);
  configOpts.UIStyle := StyleBook.name;
end;


procedure TfrmMain.cbXAxisChange(Sender: TObject);
begin
  if fireEvent then
    begin
      controller.setXAxisTitle(cbXAxis.Items[cbXAxis.itemindex]);
      controller.viewerPackage.XColumnIndex := cbXAxis.itemindex;
      controller.updateViewers;
    end;
end;


procedure TfrmMain.updateConfigInformation;
begin
  configOpts.mainConfig.formLeft := frmMain.Left;
  configOpts.mainConfig.formWidth := frmMain.Width;
  configOpts.mainConfig.formHeight := frmMain.Height;
  configOpts.mainConfig.formTop := frmMain.Top;
  configOpts.mainConfig.outputPanelWidth := pnlOutputPanelBase.Width;

  configOpts.miscFrameConfig.heigthOfFrame := pnlMiscPanel.Height;
  configOpts.mainConfig.outputPanelHeight := pnlOutputPanel.Height;

  configOpts.textFormViewer.dataMemoBackgroundColor := tableViewer.config.dataMemoBackgroundColor;
  configOpts.textFormViewer.dataMemoFontColor := tableViewer.config.dataMemoFontColor;
end;


procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  updateConfigInformation;
  saveConfigurationFile(CONFIG_FILE_NAME);

  if controller.outOfDate then
    begin
      if MessageDlg('Do you really want to quit? The model is not saved to disk.', TMsgDlgType.mtConfirmation,
        [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo) = IDYes then
        CanClose := True
      else
        CanClose := false;
    end
  else
    CanClose := True;
end;


// Outer exception handler
procedure TfrmMain.AppException(Sender: TObject; E: Exception);
begin
  Application.ShowException(E);
end;

procedure TfrmMain.chkShowLineNumbersChange(Sender: TObject);
begin
  TRichEditStyled(MemoAntimony.Presentation).ShowGutter := chkShowLineNumbers.IsChecked;
end;


procedure TfrmMain.InitPython;
begin
{$IFDEF PYTHON}
  PythonEngine := TPythonEngine.Create(nil);
  try
    PythonEngine.Name := 'PythonEngine';
    PythonEngine.DllPath := ExtractFileDir(ParamStr(0)) + '\\Python';
    PythonEngine.DllName := 'python311.dll';
    PythonEngine.AutoLoad := False;
    PythonEngine.FatalAbort := True;
    PythonEngine.FatalMsgDlg := True;
    PythonEngine.UseLastKnownVersion := False;
    PythonEngine.AutoFinalize := True;
    PythonEngine.PyFlags := [pfInteractive];

    //frmPythonIO := TfrmPythonIO.Create(nil);
   // frmPythonIO.StyleBook := cboStyleList.listitems[cboStyleList.itemindex].Data as TStyleBook;

    PythonGUIInputOutput := TPythonGUIInputOutput.Create(nil);
    //PythonGUIInputOutput.Output := frmPythonIO.moPython;
    PythonEngine.IO := PythonGUIInputOutput;

    PythonModule := TPythonModule.Create(nil);
    PythonModule.Name := 'DelphiModule';
    PythonModule.Engine := PythonEngine;
    PythonModule.ModuleName := 'app';

    DelphiWrapper := TPyDelphiWrapper.Create(nil);
    DelphiWrapper.Name := 'pyWrapper';
    DelphiWrapper.Engine := PythonEngine;
    DelphiWrapper.Module := PythonModule;

    DelphiPlotWrapper := TPyDelphiWrapper.Create(nil);
    DelphiPlotWrapper.Name := 'pyPlotWrapper';
    DelphiPlotWrapper.Engine := PythonEngine;
    DelphiPlotWrapper.Module := PythonModule;

    PythonEngine.LoadDll;
  finally
//
  end;
{$ENDIF}
end;


procedure TfrmMain.FormCreate(Sender: TObject);
var
  mu, smu: TMenuItem;
  f: TextFile;
  configOk: Boolean;
begin
  Application.OnException := AppException;
  PythonSystemActive := false;

  modelState := TModelState.Create;
  previousModelState := TModelState.Create;

  MemoAntimony.ScrollAnimation := TBehaviorBoolean.True;

  if MemoAntimony.Presentation is TRichEditStyled then
  begin
    TRichEditStyled(MemoAntimony.Presentation).SetCodeSyntaxName('ant', MemoAntimony.Font, MemoAntimony.FontColor);
  end;

  TRichEditStyled(MemoAntimony.Presentation).ShowGutter := True;

  try
    fireEvent := false;
    cboStyleList.Items.Add(MineShaft_Win_Style.StyleName);
    cboStyleList.listitems[0].Data := MineShaft_Win_Style;

    cboStyleList.Items.Add(Calypso_Win_Style.StyleName);
    cboStyleList.listitems[1].Data := Calypso_Win_Style;

    cboStyleList.Items.Add(StyleBookUI3.StyleName);
    cboStyleList.listitems[2].Data := StyleBookUI3;

    // pick up the UI style and apply it.
    var
    found := false;
    for var i := 0 to cboStyleList.Count - 1 do
      if TStyleBook(cboStyleList.listitems[i].Data).name = configOpts.UIStyle then
        begin
          cboStyleList.itemindex := i;
          found := True;
          break;
        end;
    if not found then
      cboStyleList.itemindex := 0;
  except
    on E: Exception do
      raise Exception.Create('Exception in FormCreate Setting up UI styling');
  end;

  try
    // AssignFile (f, '~/Library/Logs/iridium.log');
    // rewrite (f);
    // writeln (f, 'Start logging');
    // closefile (f);
    conservedMoietyAnalysis := false;
    controller := TController.Create;
    controller.modelInputManager.SetInputMemo(MemoAntimony);
    controller.setTimeEnd(strtofloat(edtTimeEnd.Text));
    controller.setTimeStart(strtofloat(edtTimeStart.Text));
    controller.setNumberOfPoints(strtoint(edtNumberOfPoints.Text));
    controller.viewerPackage.showLegend := True;
    configOk := readConfigurationFile(CONFIG_FILE_NAME);
    // These two line are used to prevent the isutation where a user
    // starts up the apps but htnimmeidately closes the app. Without these
    // lines the program will ask if they want to change the model even
    // though it hasn't changed.
    loadModelFromMemo(); // This is to ensure that at startup there is a model preloaded
    controller.outOfDate := false;
  except
    on E: Exception do
      raise Exception.Create('Exception in formcreate Stage A');
  end;

  try
    frmMain.Width := configOpts.mainConfig.formWidth;
    frmMain.Height := configOpts.mainConfig.formHeight;
    pnlOutputPanelBase.Width := configOpts.mainConfig.outputPanelWidth;
    chkAlwaysReset.IsChecked := configOpts.timeCourceConfig.alwaysResetAfterSimulation;
    pnlMiscPanel.Height := configOpts.miscFrameConfig.heigthOfFrame;
    pnlOutputPanel.Height := configOpts.mainConfig.outputPanelHeight;
  except
    on E: Exception do
      raise Exception.Create('Exception in FormCreate Stage B');
  end;

  try
    controller.modelInputManager.setMemoFontSize(configOpts.modelInputManagerConfig.fontSize);
  except
    on E: Exception do
      raise Exception.Create('Exception in FormCreate Stage D');
  end;

  try
    plotViewer := TPlotFrameViewer.Create(self);
    plotViewer.IsViewerVisible := false;
    plotViewer.Parent := TabControl_Viewers.Tabs[0];
    plotViewer.Align := TAlignLayout.Client;
    TabControl_Viewers.Tabs[0].TagObject := plotViewer;
    plotViewer.Setup(controller);
    controller.plotViewer := plotViewer;
  except
    on E: Exception do
      raise Exception.Create('Exception in FormCreate Stage E');
  end;

  try
    tableViewer := TTableFrameViewer.Create(self);
    tableViewer.IsViewerVisible := false;
    tableViewer.Parent := TabControl_Viewers.Tabs[1];
    tableViewer.Align := TAlignLayout.Client;
    // We need to know the viewer that is in the tab control
    TabControl_Viewers.Tabs[1].TagObject := tableViewer;
    tableViewer.Setup(controller, configOpts.textFormViewer);
  except
    on E: Exception do
      raise Exception.Create('Exception in formcreate Stage F');
  end;

  try
    plotViewer.APlot.Width := configOpts.mainConfig.outputPanelWidth;
    controller.modelInputManager.setMemoFontSize(configOpts.modelInputManagerConfig.fontSize);
    spFontSize.value := configOpts.modelInputManagerConfig.fontSize;
  except
    on E: Exception do
      raise Exception.Create('Exception in FormCreate PlotViewer');
  end;

  try
    // HMS tableViewer.config.dataMemoBackgroundColor := configOpts.textFormViewer.dataMemoBackgroundColor;
    // HMS tableViewer.config.dataMemoFontColor := configOpts.textFormViewer.dataMemoFontColor;
  except
    on E: Exception do
      raise Exception.Create('Exception in FormCreate Stage X');
  end;

  try
    currentAntimonyFileName := '';
    launchPath := ExtractFileDir(ParamStr(0));
    selectedPalette := 'Default';
    if TOSVersion.Platform <> pfMacOS then
      begin
        // Add windows help menu
        mu := TMenuItem.Create(frmMain);
        mu.Parent := rrMainMenu;
        mu.enabled := True;
        mu.Text := 'Help';
        mu.Visible := True;
        rrMainMenu.AddObject(mu);
        smu := TMenuItem.Create(mu);
        smu.Text := 'Help....';
        smu.Visible := True;
        smu.OnClick := mnuHelpClick;
        mu.AddObject(smu);
        smu := TMenuItem.Create(frmMain);
        smu.enabled := True;
        smu.Text := 'Help on Antimony';
        smu.Visible := True;
        smu.OnClick := mnuHelpAntimonyClick;
        mu.AddObject(smu);
        smu := TMenuItem.Create(mu);
        smu.Text := '-';
        smu.Visible := True;
        smu.OnClick := mnuAboutClick;
        mu.AddObject(smu);
        smu := TMenuItem.Create(mu);
        smu.Text := 'About Iridium';
        smu.Visible := True;
        smu.OnClick := mnuAboutClick;
        mu.AddObject(smu);
      end
    else
      begin
        // Add special app menu for Mac
        mu := TMenuItem.Create(frmMain);
        mu.enabled := True;
        mu.Text := 'DummyText';
        mu.Visible := True;
        rrMainMenu.InsertObject(0, mu);
        smu := TMenuItem.Create(mu);
        smu.Text := 'About Iridium';
        smu.Visible := True;
        smu.OnClick := mnuAboutClick;
        mu.AddObject(smu);
        smu := TMenuItem.Create(frmMain);
        smu.enabled := True;
        smu.Text := 'Help on Antimony';
        smu.Visible := True;
        smu.OnClick := mnuHelpAntimonyClick;
        mu.AddObject(smu);
        smu := TMenuItem.Create(mu);
        smu.Text := '-';
        smu.Visible := True;
        mu.AddObject(smu);
      end;
  except
    on E: Exception do
      raise Exception.Create('Exception in formcreate Stage I');
  end;
  //moModel.mTxtChangeTracking(Sender);
  fireEvent := True;
  lblVersion.Text := 'RoadRunner Version: ' + controller.simulator.roadrunner.getVersionStr;

{$IFDEF PYTHON}
   InitPython;
   host := THostAPI.Create (controller);
   DelphiWrapper.RegisterDelphiWrapper(TPyClassWrapper<THostAPI>).Initialize;
   DelphiWrapper.DefineVar('host', host);

   plot := TPlotAPI.Create (controller);
   DelphiPlotWrapper.RegisterDelphiWrapper(TPyClassWrapper<TPlotAPI>).Initialize;
   DelphiPlotWrapper.DefineVar('plt', plot);
{$ENDIF}
end;


procedure TfrmMain.FormShow(Sender: TObject);
begin
//
end;

procedure TfrmMain.MemoAntimonyChangeTracking(Sender: TObject);
begin
  //moModel.mTxtChangeTracking(Sender);
  if controller <> nil then
     begin
     controller.outOfDate := True;
     controller.modelInputManager.antimonyStr := MemoAntimony.lines.Text;

     previousModelState.copy (modelState);
     modelState.update(controller);

     //   modelStateStack.Push(modelState);

     case modelState.onlyParameterChanges (previousModelState) of
     wcModel :
        begin
        freeSliderPanel;
        controller.clearViewers;
        exit
        end;
     wcParameters :
        begin end;  // HMS
        //plotViewer.plt.dimGraph := True;

     wcNothing :
       begin

       end;
     end;
     end;
end;

procedure TfrmMain.MenuItem1Click(Sender: TObject);
begin
  if TOSVersion.Platform <> TOSVersion.TPlatform.pfMacOS then
    begin
      if SaveCombineArchive.Execute then
        createCombineArchive(SaveCombineArchive.FileName, controller);
    end
  else
    ShowMessage('This is not yet implemented on Mac OSX');
end;


procedure TfrmMain.mnuExportPythonClick(Sender: TObject);
var
  astr: string;
  clp: IFMXClipboardService;
begin
  astr := makePythonScript(controller);
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService) then
    begin
      clp := IFMXClipboardService(TPlatformServices.Current.GetPlatformService(IFMXClipboardService));
      clp.SetClipboard(astr);
    end;
  TDialogService.ShowMessage('Python copied to clipboard');
end;


procedure TfrmMain.mnuPythonClick(Sender: TObject);
var
  i : integer;
  t1, t2 : TTabItem;
begin
{$IFDEF PYTHON}
  if Assigned(pythonFrame) or Assigned (pythonIOFrame) then
    begin
      freePythonIOFrame;
      freePythonFrame;

      for i := 0 to TabOutputControl.TabCount - 1 do
        if TabOutputControl.Tabs[i].Text = 'Python' then
          begin
            TabOutputControl.Delete(i);
            break;
          end;

      for i := 0 to TabControl.TabCount - 1 do
        if TabControl.Tabs[i].Text = 'Python' then
          begin
            TabControl.Delete(i);
            break;
          end;
       Exit;
    end;

  // Prepare a tab sheet
  t1 := TTabItem.Create(self);
  t1.Parent := TabControl;
  t1.Text := 'Python';

  pythonFrame := TPythonFrame.Create(self);
  pythonFrame.Parent := t1;
  pythonFrame.Align := TAlignLayout.Client;
  pnlMiscPanel.Height := configOpts.miscFrameConfig.heigthOfFrame;
  pythonFrame.moPython.SetFocus;

  // Prepare a tab sheet
  t2 := TTabItem.Create(self);
  t2.Parent := TabOutputControl;
  t2.Text := 'Python';

  pythonIOFrame := TPythonIOFrame.Create(self);
  pythonIOFrame.Parent := t2;
  pythonIOFrame.Align := TAlignLayout.Client;
  PythonGUIInputOutput.Output := pythonIOFrame.moPython;
{$ELSE}
  showmessage ('Python support still in beta, not yet available');
{$ENDIF}
end;


procedure TfrmMain.moModelmTxtChange(Sender: TObject);
begin
  exit;
  controller.outOfDate := True;
  controller.modelInputManager.antimonyStr := MemoAntimony.lines.Text;
  freeSliderPanel;
  controller.clearViewers;
end;


procedure TfrmMain.moModelmTxtChangeTracking(Sender: TObject);
begin
  MemoAntimony.OnChangeTracking (Sender);
  if controller <> nil then
     begin
     controller.outOfDate := True;
     controller.modelInputManager.antimonyStr := MemoAntimony.lines.Text;

     previousModelState.copy (modelState);
     modelState.update(controller);

     //   modelStateStack.Push(modelState);

     case modelState.onlyParameterChanges (previousModelState) of
     wcModel :
        begin
        freeSliderPanel;
        controller.clearViewers;
        exit
        end;
     wcParameters :
        plotViewer.plt.dimGraph := True;

     wcNothing :
       begin

       end;
     end;
     end;
end;

procedure TfrmMain.moModelmTxtPresentationNameChoosing(Sender: TObject; var
    PresenterName: string);
begin
  // The choice of the presentation class by the control
  PresenterName := 'RichEditStyled';
end;


procedure TfrmMain.OnModelChange;
begin
  controller.outOfDate := True;
  // don't forget to empty modelstate stack and free state
  controller.modelInputManager.antimonyStr := MemoAntimony.lines.Text;
  freeSliderPanel;
  controller.clearViewers;
  controller.ViewerModelHasChanged(self);
end;


procedure TfrmMain.spFontSizeChange(Sender: TObject);
begin
  controller.modelInputManager.setMemoFontSize(trunc(spFontSize.value));
end;


procedure TfrmMain.TabControl_ViewersChange(Sender: TObject);
begin
  if TabControl_Viewers.ActiveTab.name = 'TabItem_TextView' then
    (TabControl_Viewers.ActiveTab.TagObject as TFrameViewerBase).IsViewerVisible := True
  else
    (TabControl_Viewers.ActiveTab.TagObject as TFrameViewerBase).IsViewerVisible := false;
  // if TabControl_Viewers.ActiveTab.Name = 'TabItem_PlotView' then
  // (TabControl_Viewers.ActiveTab.TagObject as TFrameViewerBase).IsViewerVisible := True
  // else
  // (TabControl_Viewers.ActiveTab.TagObject as TFrameViewerBase).IsViewerVisible := False;
end;

end.
