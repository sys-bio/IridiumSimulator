unit ufMain;

{ Application shell.

  Responsibilities:
    * Own the single TModelSession (the model state).
    * Own the single TFrameSliderContainer (the shared slider panel).
    * Own the plot.
    * Host the analysis frames in LayoutContainer; show one at a time.
    * Implement IAnalysisContext so frames can reach shared services
      without referring back to frmMain. }

interface

uses
  {$IFDEF MSWINDOWS}
      Winapi.Windows, Winapi.ShellAPI,
  {$ENDIF}
  {$IFDEF POSIX}
      Posix.Stdlib,
  {$ENDIF POSIX}

  System.SysUtils,
  FMX.Styles,
  System.Types,
  System.UIConsts, System.UITypes,
  System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Layouts,
  System.Skia, FMX.Skia,
  Generics.Collections,
  SkPlotPaintBox,
  uRR2DSimpleMatrix,
  uAnalysisTypes,
  uModelSession,
  uFrameSliderContainer,
  uFrameTimeCourse,
  uFrameSteadyState,
  uFrameParameterScan,
  ufAbout,
  uColorList,
  FMX.Menus,
  FMX.Edit,
  FMX.EditBox,
  FMX.BehaviorManager,
  FMX.RichEdit.Style,
  FMX.NumberBox,
  FMX.Objects,
  uBuiltInModels,
  FMX.SpinBox, FMX.TabControl,
  FMX.ListBox, System.Math.Vectors, FMX.Controls3D,
  FMX.Layers3D;

const
    VERSION = '0.96            ';

type
  TfrmMain = class(TForm, IAnalysisContext)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout6: TLayout;
    SliderContainer: TLayout;
    moAntimony: TMemo;
    LayoutContainer: TLayout;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    mnuSave: TMenuItem;
    mnuQuit: TMenuItem;
    OpenDialogAnt: TOpenDialog;
    SaveDialogAnt: TSaveDialog;
    SavePDFDialog: TSaveDialog;
    Splitter1: TSplitter;
    mnuNew: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuHelp: TMenuItem;
    mnuGeneralHelp: TMenuItem;
    mnuHelpAntimony: TMenuItem;
    MenuItem6: TMenuItem;
    mnuAbout: TMenuItem;
    btnNew: TSpeedButton;
    Image8: TImage;
    btnLoadAntimony: TSpeedButton;
    Image1: TImage;
    btnSave: TSpeedButton;
    Image9: TImage;
    Rectangle2: TRectangle;
    btnTimeCourse: TSpeedButton;
    Image2: TImage;
    btnSteadyState: TSpeedButton;
    Image3: TImage;
    btnScan: TSpeedButton;
    Image6: TImage;
    LayoutEditorPanel: TLayout;
    Layout7: TLayout;
    chkShowLineNumbers: TCheckBox;
    spFontSize: TSpinBox;
    lblFontSize: TLabel;
    mnuImportSBML: TMenuItem;
    mnuExportSBML: TMenuItem;
    MenuItem5: TMenuItem;
    SaveSBMLDialog: TSaveDialog;
    OpenSBMLDialog: TOpenDialog;
    SaveCSVDialog: TSaveDialog;
    StyleBook1: TStyleBook;
    Splitter2: TSplitter;
    TabControl1: TTabControl;
    tbPlot: TTabItem;
    tbTextView: TTabItem;
    Layout8: TLayout;
    Layout5: TLayout;
    chkAutoscaleX: TCheckBox;
    chkAutoScaleY: TCheckBox;
    lblXMin: TLabel;
    lblYMin: TLabel;
    lblYMax: TLabel;
    lblXMax: TLabel;
    chkShowLegend: TCheckBox;
    btnEditGraph: TButton;
    btnLoadCSV: TButton;
    btnClearData: TButton;
    btnExportToPDF: TButton;
    OpenDialog1: TOpenDialog;
    btnShowData: TButton;
    moTextView: TMemo;
    Layout9: TLayout;
    btnExportCSV: TButton;
    nubDecimalPlaces: TSpinBox;
    Label1: TLabel;
    btnCopyToClipBoard: TButton;
    btnRefresh: TButton;
    tbSteadyState: TTabItem;
    sbSteadyState: TScrollBox;
    edtXMin: TEdit;
    edtXMax: TEdit;
    edtYMin: TEdit;
    edtYMax: TEdit;
    Layout3D1: TLayout3D;
    pnlButtons: TLayout;
    Rectangle1: TRectangle;
    btnGeneratePython: TSpeedButton;
    Image4: TImage;
    pnlExampleModels: TLayout;
    cboExampleModels: TComboBox;
    Label2: TLabel;
    Plot: TSkPlotPaintBox;
    mnuGoToWedIridium: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTimeCourse1Click(Sender: TObject);
    procedure btnSteadyStateClick(Sender: TObject);
    procedure moAntimonyChangeTracking(Sender: TObject);
    procedure mnuLoadFileClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure chkAutoscaleXChange(Sender: TObject);
    procedure chkAutoScaleYChange(Sender: TObject);
    procedure chkShowLegendChange(Sender: TObject);
    procedure btnEditGraphClick(Sender: TObject);
    procedure btnExportToPDFClick(Sender: TObject);
    procedure btnLoadCSVClick(Sender: TObject);
    procedure btnClearDataClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuHelpAntimonyClick(Sender: TObject);
    procedure btnLoadAntimonyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure spFontSizeChange(Sender: TObject);
    procedure mnuExportSBMLClick(Sender: TObject);
    procedure mnuImportSBMLClick(Sender: TObject);
    procedure btnShowDataClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure btnExportCSVClick(Sender: TObject);
    procedure nubDecimalPlacesChange(Sender: TObject);
    procedure btnCopyToClipBoardClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure moAntimonyPresentationNameChoosing(Sender: TObject;
      var PresenterName: string);
    procedure chkShowLineNumbersChange(Sender: TObject);
    procedure cboExampleModelsChange(Sender: TObject);
    procedure edtXMinKeyDow(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtXMinExit(Sender: TObject);
    procedure edtXMaxExit(Sender: TObject);
    procedure edtXMaxKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtYMinKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtYMinExit(Sender: TObject);
    procedure edtYMaxKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure edtYMaxExit(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure btnGeneratePythonClick(Sender: TObject);
    procedure mnuGoToWedIridiumClick(Sender: TObject);
    procedure mnuGeneralHelpClick(Sender: TObject);
  private
    FSession:          TModelSession;
    FSliderFrame:      TFrameSliderContainer;
    FFrameTimeCourse:  TFrameTimeCourse;
    FFrameSteadyState: TFrameSteadyState;
    FFrameParameterScan: TFrameParameterScan;
    FActiveFrame:      TFrame;

    FCurrentFileName : String;
    FireEvent: Boolean;
    FIsModifiedSinceLastSave: Boolean;

    procedure CreateSession;
    procedure CreateSliderContainer;
    procedure CreateAnalysisFrames;

    procedure CheckNumberKeys (edt : TEdit; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

    function  GetAntimonyText: string;
    procedure SessionStateChanged(Sender: TObject);
    procedure ShowAnalysisFrame(ATarget: TFrame);

    procedure SessionModelReloaded(Sender: TObject;  AParameterSetChanged: Boolean);
    procedure AppendToAntimonySource(const ABlock: string);

    { IAnalysisContext }
    function  GetSession: TModelSession;
    function  GetSliderContainer: TFrameSliderContainer;
    function  GetSteadyStateHost: TScrollBox;
    procedure ShowSteadyStateTab;

    procedure PlotData(const AData: T2DMatrix;
                       const AXAxisName: string;
                       const AYAxisNames: TArray<string>);
    procedure PlotClearSimulationSeries;
    procedure PlotAddSeries(ASeries: TObject);
    procedure PlotRedraw;
    procedure PlotRecolorSimulationSeries(const ANextColor: TFunc<TAlphaColor>);
    function  PlotGetSimulationSeriesInfo: TArray<TPlotSeriesColorInfo>;
    function  PlotGetPlotInfo: TPlotInfo;

    procedure CopyTextToTextWindow (AString : String);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  IOUtils, uPlotSeries, uColorManager, uAntimonyAPI, uRoadRunner, ufPlotEditor;

const
  DEFAULT_SLIDER_HEIGHT = 322.0;

type
  TModelErrorState = record
    ok : boolean;
    errMsg : string;
    sbmlStr : string;
    runStr : string;
  end;


procedure TfrmMain.CheckNumberKeys (edt : TEdit; var Key: Word;  var KeyChar: WideChar; Shift: TShiftState);
var
  CurrentText: string;
begin
  CurrentText := edt.Text;

  // 1. Always allow control keys (Backspace, Delete, Arrow keys, Enter, Tab)
  if KeyChar = #0 then
    Exit;

  // 2. Reject any character not in the allowed scientific notation set
  if not CharInSet(KeyChar, ['0'..'9', '.', 'e', 'E', '+', '-']) then
  begin
    Key := 0;     // Discard the hardware key stroke
    KeyChar := #0; // Discard the character token
    Exit;
  end;

  // 3. Prevent duplicate decimal points
  if (KeyChar = '.') and CurrentText.Contains('.') then
  begin
    Key := 0;
    KeyChar := #0;
    Exit;
  end;

  // 4. Prevent duplicate 'e' or 'E' exponent indicators
  if (CharInSet(KeyChar, ['e', 'E'])) and (CurrentText.Contains('e') or CurrentText.Contains('E')) then
  begin
    Key := 0;
    KeyChar := #0;
    Exit;
  end;
end;
{ ── form lifecycle ───────────────────────────────────────────────────────── }

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if FIsModifiedSinceLastSave then
    CanClose := MessageDlg('The model has unsaved changes. Are you sure you want to quit?',
                           TMsgDlgType.mtConfirmation,
                           [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes;

end;



procedure TfrmMain.FormCreate(Sender: TObject);
var
  errMsgAnsi: AnsiString;
  errMsg:     string;
begin
  TabControl1.ActiveTab := tbPlot;

  FireEvent := False;

  // Setting the default syntax and fonts
  //if moAntimony.Presentation is TRichEditStyled then
  //begin
  //  TRichEditStyled(moAntimony.Presentation).SetCodeSyntaxName('pascal', moAntimony.Font, moAntimony.FontColor);
  //  TRichEditStyled(moAntimony.Presentation).ShowGutter := True;
  //end;

  //moAntimony.ScrollAnimation := TBehaviorBoolean.True;

  //if moAntimony.Presentation is TRichEditStyled then
  //begin
  //  TRichEditStyled(moAntimony.Presentation).SetCodeSyntaxName('pascal', moAntimony.Font, moAntimony.FontColor);
  //end;

 for var i := 0 to BuiltInModels.Count - 1 do
     cboExampleModels.Items.AddObject(BuiltInModels[i].DisplayName, BuiltInModels[i]);

  if not LoadAntimonyLibrary(errMsg) then
     begin
     Showmessage ('Unable to find the Antimony library. This is usually the result of a bad installation.');
     Halt;
     end;
  if not uRoadRunner.loadRoadRunner(errMsgAnsi) then
     begin
     Showmessage ('Unable to find the libRoadRunner library. This is usually the result of a bad installation.');
     Halt;
     end;

  CreateSession;
  CreateSliderContainer;
  CreateAnalysisFrames;

  Plot.AutoXScaling := True;
  Plot.AutoYScaling := True;
  Plot.LegendStyle.Visible := True;

  FIsModifiedSinceLastSave := False;

  chkAutoscaleX.IsChecked := Plot.AutoXScaling;
  chkAutoscaleY.IsChecked := Plot.AutoYScaling;
  chkShowLegend.IsChecked := Plot.LegendStyle.Visible;

  edtXMin.Text := '0';
  edtXMax.Text := '20';
  edtYMin.Text := '0';
  edtYMax.Text := '10';

  spFontSize.Value := 16;
  moAntimony.Font.Size := spFontSize.Value;

  ShowAnalysisFrame(FFrameTimeCourse);   { default view }

  FireEvent := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSession.Free;
  { Frames and slider frame are owned by Self (TComponent ownership) and
    will be freed automatically. }
end;

procedure TfrmMain.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
end;

{ ── construction helpers ─────────────────────────────────────────────────── }

procedure TfrmMain.CreateSession;
begin
  FSession := TModelSession.Create;
  FSession.OnNeedAntimonyText := GetAntimonyText;
  FSession.AddStateListener(SessionStateChanged);
  FSession.AddReloadedListener(SessionModelReloaded);
end;

procedure TfrmMain.CreateSliderContainer;
begin
  FSliderFrame := TFrameSliderContainer.Create(Self);
  FSliderFrame.Parent  := SliderContainer;
  FSliderFrame.Align   := TAlignLayout.Client;
  FSliderFrame.Visible := True;
end;

procedure TfrmMain.CreateAnalysisFrames;
begin
  FFrameTimeCourse := TFrameTimeCourse.Create(Self);
  FFrameTimeCourse.Parent  := LayoutContainer;
  FFrameTimeCourse.Align   := TAlignLayout.Client;
  FFrameTimeCourse.Visible := False;
  FFrameTimeCourse.SetContext(Self);

  FFrameSteadyState := TFrameSteadyState.Create(Self);
  FFrameSteadyState.Parent  := LayoutContainer;
  FFrameSteadyState.Align   := TAlignLayout.Client;
  FFrameSteadyState.Visible := False;
  FFrameSteadyState.SetContext(Self);

  FFrameParameterScan := TFrameParameterScan.Create(Self);
  FFrameParameterScan.Parent  := LayoutContainer;
  FFrameParameterScan.Align   := TAlignLayout.Client;
  FFrameParameterScan.Visible := False;
  FFrameParameterScan.SetContext(Self);
end;

{ ── memo / session bridge ────────────────────────────────────────────────── }

function TfrmMain.GetAntimonyText: string;
begin
  Result := moAntimony.Text;
end;


procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(nil);
  frmAbout.lblRoadRunner.Text := 'Using libroadRunner version: ' + TRoadRunner.getVersionStr();
  frmAbout.lbllibSBML.Text := TRoadrunner.getlibSBMLVersion();
  frmAbout.lbSkia.Text := 'Using skia: ' + SkVersion + ', Milestone: ' + SkVersion;
  frmAbout.lblWho.Text := 'Developed at the Sauro Lab, University of Washington, Seattle';
  frmAbout.lbVersion.Text := 'Iridium version: ' + VERSION;
  frmAbout.ShowModal;
  frmAbout.Free;
end;


procedure TfrmMain.mnuExportSBMLClick(Sender: TObject);
var sbmlStr : string;
    ModelErrorState : TModelErrorState;
begin
  if SaveSBMLDialog.Execute then
    begin
      uAntimonyAPI.getSBMLFromAntimony(moAntimony.Lines.Text);
      if modelErrorState.ok then
         TFile.WriteAllText(SaveSBMLDialog.FileName, modelErrorState.sbmlStr)
      else
         showmessage (modelErrorState.errMsg);
    end;
end;

procedure TfrmMain.mnuGeneralHelpClick(Sender: TObject);
var
  myurl: string;
begin
  myurl := 'https://github.com/sys-bio/IridiumSimulator';
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(myurl), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(myurl)));
{$ENDIF POSIX}
end;

procedure TfrmMain.mnuGoToWedIridiumClick(Sender: TObject);
var
  myurl: string;
begin
  myurl := 'https://sys-bio.github.io/WebIridium/';
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(myurl), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(myurl)));
{$ENDIF POSIX}
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

procedure TfrmMain.mnuImportSBMLClick(Sender: TObject);
var SBMLString: String;
begin
  if OpenSBMLDialog.Execute then
    begin
      SBMLString := TFile.ReadAllText(OpenSBMLDialog.FileName);
      if SBMLString = '' then exit;
      FSession.Unload;
      moAntimony.text := uAntimonyAPI.getAntimonyFromSBML(SBMLString);
      FSession.ClearDirty;
    end;
end;

procedure TfrmMain.mnuLoadFileClick(Sender: TObject);
begin
 if OpenDialogAnt.Execute then
    begin
    FSession.Unload;
    moAntimony.Text := TFile.ReadAllText(OpenDialogAnt.FileName);
    FCurrentFileName := ExtractFileName(OpenDialogAnt.FileName);
    Caption := 'Iridium II: ' + FCurrentFileName;
    FSession.ClearDirty;
    end;
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
begin
  moAntimony.Text := '';
  FSession.Unload;
  Plot.ClearSeries;
  FFrameTimeCourse.SetSimulationParameters(20, 200);
end;

procedure TfrmMain.mnuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuSaveClick(Sender: TObject);
begin
  if SaveDialogAnt.Execute then
     begin
     TFile.WriteAllText(SaveDialogAnt.FileName, moAntimony.Text);
     FIsModifiedSinceLastSave := False;
     end;
end;

procedure TfrmMain.moAntimonyChangeTracking(Sender: TObject);
begin
  FIsModifiedSinceLastSave := True;
  FSession.MarkDirty;
end;

procedure TfrmMain.moAntimonyPresentationNameChoosing(Sender: TObject;
  var PresenterName: string);
begin
  // The choice of the presentation class by the control
  //PresenterName := 'RichEditStyled';
end;

procedure TfrmMain.edtXMaxExit(Sender: TObject);
var
  Value: Double;
begin
  if not TryStrToFloat(edtXMax.Text.Trim, Value) then
     begin
     showmessage ('Number not entered correctly');
     edtXMax.SetFocus;
     end
  else
     begin
     Plot.AxisLimits.MaxX := Value;
     Plot.Redraw;
     end;
end;


procedure TfrmMain.edtXMaxKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtXmax, Key, KeyChar, Shift);
end;

procedure TfrmMain.edtXMinExit(Sender: TObject);
var
  Value: Double;
begin
  if not TryStrToFloat(edtXMin.Text.Trim, Value) then
     begin
     showmessage ('Number not entered correctly');
     edtXMin.SetFocus;
     end
  else
     begin
     Plot.AxisLimits.MinX := Value;
     Plot.Redraw;
     end;
end;


procedure TfrmMain.edtXMinKeyDow(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtXmin, Key, KeyChar, Shift);
end;

procedure TfrmMain.edtYMaxExit(Sender: TObject);
var
  Value: Double;
begin
  if not TryStrToFloat(edtYMax.Text.Trim, Value) then
     begin
     showmessage ('Number not entered correctly');
     edtYMax.SetFocus;
     end
  else
     begin
     Plot.AxisLimits.MaxY := Value;
     Plot.Redraw;
     end;
end;

procedure TfrmMain.edtYMaxKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtYMax, Key, KeyChar, Shift);
end;

procedure TfrmMain.edtYMinExit(Sender: TObject);
var
  Value: Double;
begin
  if not TryStrToFloat(edtYMin.Text.Trim, Value) then
     begin
     showmessage ('Number not entered correctly');
     edtYMin.SetFocus;
     end
  else
     begin
     Plot.AxisLimits.MinY := Value;
     Plot.Redraw;
     end;
end;

procedure TfrmMain.edtYMinKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  CheckNumberKeys(edtYMin, Key, KeyChar, Shift);
end;

procedure TfrmMain.nubDecimalPlacesChange(Sender: TObject);
begin
  moTextView.Text := '';
  moTextView.text := Plot.ExportCSVSeriesAsString(trunc (nubDecimalPlaces.Value), 14);
end;

procedure TfrmMain.SessionModelReloaded(Sender: TObject;
  AParameterSetChanged: Boolean);
begin
  if AParameterSetChanged then
  begin
    FSliderFrame.ClearSliders;
    FSliderFrame.LoadParams(FSession.GetParameterNames,    { <-- refresh catalogue }
                            FSession.GetParameterValues);
  end
  else
    FSliderFrame.RefreshValues(FSession.GetParameterNames,
                               FSession.GetParameterValues);
end;


procedure TfrmMain.SessionStateChanged(Sender: TObject);
begin
  { Model identity may have changed (dirty or unloaded). Sliders attached to
    the previous parameter set are no longer trustworthy -> clear them.
    Frames will rebuild on demand when the user clicks their slider button. }
  { Sliders are cleared only when the model goes unloaded entirely (e.g.
    after a failed parse). Becoming dirty no longer touches them — that's
    decided at reload time by SessionModelReloaded. }
  if not FSession.IsLoaded then
  begin
    FSliderFrame.ClearSliders;
    { Also hide the panel itself so it doesn't linger from the previous model. }
    if FSliderFrame.ParamPanelVisible then
      FSliderFrame.ToggleParamPanel;
  end;

  if FSession.IsLoaded and (not FSession.IsDirty) then
    Caption := 'Simulator — model loaded'
  else if FSession.IsLoaded and FSession.IsDirty then
    Caption := 'Simulator — model loaded (edits pending)'
  else
    Caption := 'Simulator — no model loaded';
end;

{ ── frame switching ──────────────────────────────────────────────────────── }

procedure TfrmMain.ShowAnalysisFrame(ATarget: TFrame);
begin
  FFrameTimeCourse.Visible    := False;
  FFrameSteadyState.Visible   := False;
  FFrameParameterScan.Visible := False;

  if FActiveFrame <> ATarget then
    FSliderFrame.ClearSliders;

  FActiveFrame := ATarget;

  { Re-bind shared slider container to the now-active frame. ClearSliders
    no longer touches the handler, so this is the single point where the
    binding tracks the active frame. Covers all paths into the slider UI,
    including "Add all" / listbox clicks that bypass each frame's own
    slider-button handler. }
  if ATarget = FFrameTimeCourse then
    FFrameTimeCourse.AttachToSliders
  else if ATarget = FFrameParameterScan then
    FFrameParameterScan.AttachToSliders
  else if ATarget = FFrameSteadyState then
    FFrameSteadyState.AttachToSliders;

  FSliderFrame.Visible := (ATarget = FFrameTimeCourse) or
                          (ATarget = FFrameParameterScan) or
                          (ATarget = FFrameSteadyState);

  if Assigned(ATarget) then
  begin
    ATarget.Visible := True;
    ATarget.BringToFront;
  end;

  if ATarget = FFrameParameterScan then
    FFrameParameterScan.UpdateScanParameterLock
  else
    FSliderFrame.SetLockedParam('');
end;


procedure TfrmMain.spFontSizeChange(Sender: TObject);
begin
  moAntimony.Font.Size := spFontSize.Value;
end;

procedure TfrmMain.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab = tbTextView then
     btnShowDataClick(Sender);
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
//
end;

procedure TfrmMain.btnScanClick(Sender: TObject);
begin
  ShowAnalysisFrame(FFrameParameterScan);
end;

procedure TfrmMain.btnTimeCourse1Click(Sender: TObject);
begin
  ShowAnalysisFrame(FFrameTimeCourse);
end;

procedure TfrmMain.btnClearDataClick(Sender: TObject);
begin
  for var i :=  Plot.Series.Count - 1 downto 0 do
      begin
      if Plot.Series[i].SeriesType = SERIES_TYPE_DATA then
         Plot.Series.Delete(i);
      end;
  Plot.Redraw;
end;

procedure TfrmMain.btnCopyToClipBoardClick(Sender: TObject);
begin
  moTextView.SelectAll;
  moTextView.CopyToClipboard;
end;

procedure TfrmMain.btnEditGraphClick(Sender: TObject);
begin
  if not Assigned (frmPlotEditor) then
     frmPlotEditor := TFrmPlotEditor.Create (nil);
  try
    frmPlotEditor.CopyPropertiesToEditor(Plot);
    frmPlotEditor.Show;
    //frmPlotEditor.CopyPropertiesFromEditor(Plot);
  finally
    //frmPlotEditor.Free;
  end;
end;


procedure TfrmMain.btnShowDataClick(Sender: TObject);
var astr : String;
begin
  astr := Plot.ExportCSVSeriesAsString(trunc (nubDecimalPlaces.Value), 14);
  moTextView.text := astr;
  TabControl1.ActiveTab := tbTextView;
end;

procedure TfrmMain.btnExportCSVClick(Sender: TObject);
begin
  if SaveCSVDialog.Execute then
     Plot.ExportCSV(SaveCSVDialog.FileName);
end;

procedure TfrmMain.btnExportToPDFClick(Sender: TObject);
begin
  if SavePDFDialog.Execute then
     begin
     Plot.ExportToPdf(SavePDFDialog.FileName);
     end;
end;

procedure TfrmMain.btnGeneratePythonClick(Sender: TObject);
var
  Exporter: IPythonScriptExporter;
  Script:   string;
begin
 if Supports(FActiveFrame, IPythonScriptExporter, Exporter) then
  begin
    Script := Exporter.GetPythonScript(moAntimony.Text);
    CopyTextToTextWindow (Script);
    { copy to clipboard / open save dialog / show in a memo / whatever }
  end
  else
  begin
    ShowMessage('This view doesn''t support Python export.');
  end;
end;

procedure TfrmMain.btnLoadAntimonyClick(Sender: TObject);
begin
  mnuLoadFileClick(Sender);
end;

procedure TfrmMain.btnLoadCSVClick(Sender: TObject);
var i : Integer;
    Series : TStringList;
begin
  if OpenDialog1.Execute then
     begin
     Series := Plot.LoadData(OpenDialog1.FileName, False, True, False);
     for i := 0 to Series.Count - 1 do
         begin
         TPlotSeries (Series.Objects[i]).SeriesType := SERIES_TYPE_DATA;
         TPlotSeries (Series.Objects[i]).LineVisible := False;
         //TPlotSeries (Series.Objects[i]).MarkerStrokeColor := TColorManager.NextColor;
         TPlotSeries (Series.Objects[i]).MarkerStrokeWidth := 1.5;
         TPlotSeries (Series.Objects[i]).MarkerFillColor := TPlotSeries (Series.Objects[i]).MarkerStrokeColor;
         TPlotSeries (Series.Objects[i]).MarkerSize := 4;
         end;
     Series.Free;
     Plot.Redraw;
     end;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  moTextView.text := Plot.ExportCSVSeriesAsString(trunc (nubDecimalPlaces.Value), 14);
end;

procedure TfrmMain.btnSteadyStateClick(Sender: TObject);
begin
  ShowAnalysisFrame(FFrameSteadyState);
end;

procedure TfrmMain.cboExampleModelsChange(Sender: TObject);
var Model : TBuiltInModel;
begin
  if not FireEvent then exit;

  Model := (cboExampleModels.Items.Objects[cboExampleModels.ItemIndex]) as TBuiltInModel;
  FSession.Unload;
  moAntimony.Text := Model.ModelStr;
  FCurrentFileName := 'untitled.txt';
  Caption := 'Iridium II: ' + FCurrentFileName;
  FSession.ClearDirty;
  FFrameTimeCourse.SetSimulationParameters(Model.timeEnd, Model.NumberOfPoints);
end;

procedure TfrmMain.chkAutoscaleXChange(Sender: TObject);
begin
  if not FireEvent then exit;

  if chkAutoscaleX.IsChecked then
     begin
     Plot.AutoXScaling := True;
     edtXMin.Enabled := False; edtXmax.Enabled := False;
     lblXMin.Enabled := False; lblXMax.Enabled := False;
     end
  else
     begin
     Plot.AutoXScaling := False;
     edtXMin.Enabled := True; edtXmax.Enabled := True;
     lblXMin.Enabled := True; lblXMax.Enabled := True;
     Plot.AxisLimits.MinX := strtofloat (edtXMin.Text);
     Plot.AxisLimits.MaxX := strtofloat (edtXMax.Text);
     end;

  Plot.Redraw;
end;

procedure TfrmMain.chkAutoScaleYChange(Sender: TObject);
begin
  if not FireEvent then exit;

  if chkAutoscaleY.IsChecked then
     begin
     Plot.AutoYScaling := True;
     edtYMin.Enabled := False; edtYmax.Enabled := False;
     lblYMin.Enabled := False; lblYMax.Enabled := False;
     end
  else
     begin
     Plot.AutoYScaling := False;
     edtYMin.Enabled := True;  edtYmax.Enabled := True;
     lblYMin.Enabled := True; lblYMax.Enabled := True;
     Plot.AxisLimits.MinY := strtofloat (edtYMin.Text);
     Plot.AxisLimits.MaxY := strtofloat (edtYMax.Text);
     end;

  Plot.Redraw;
end;

procedure TfrmMain.chkShowLegendChange(Sender: TObject);
begin
  if not FireEvent then exit;

  if chkShowLegend.IsChecked then
     begin
     Plot.LegendStyle.Visible := True;
     end
  else
     begin
     Plot.LegendStyle.Visible := False;
     end;
  Plot.Redraw;
end;

procedure TfrmMain.chkShowLineNumbersChange(Sender: TObject);
begin
 TRichEditStyled(moAntimony.Presentation).ShowGutter := chkShowLineNumbers.IsChecked;
end;

{ ── IAnalysisContext ─────────────────────────────────────────────────────── }

function TfrmMain.GetSession: TModelSession;
begin
  Result := FSession;
end;

function TfrmMain.GetSliderContainer: TFrameSliderContainer;
begin
  Result := FSliderFrame;
end;

function TfrmMain.GetSteadyStateHost: TScrollBox;
begin
  Result := sbSteadyState;
end;

procedure TfrmMain.ShowSteadyStateTab;
begin
  TabControl1.ActiveTab := tbSteadyState;
end;

procedure TfrmMain.PlotData(const AData: T2DMatrix;
  const AXAxisName: string;
  const AYAxisNames: TArray<string>);
var
  I, J, NumRows: Integer;
  XColIdx, YColIdx: Integer;
  Series:  TPlotSeries;
  XLabel:  string;

  function FindCol(const AName: string): Integer;
  var
    K: Integer;
  begin
    Result := -1;
    if AName = '' then Exit;
    for K := 0 to AData.c - 1 do
      if SameText(AData.columnHeader[K], AName) then
        Exit(K);
  end;

  procedure AddSeriesForColumn(AColIdx: Integer);
  var J  : Integer;
  begin
    Series := TPlotSeries.Create(AData.columnHeader[AColIdx], claBlue);
    Series.LineColor     := TColorManager.NextColor;
    Series.LineWidth     := 2.5;
    Series.MarkerVisible := False;
    for J := 0 to NumRows - 1 do
      Series.AddXY(AData[J, XColIdx], AData[J, AColIdx]);
    Plot.AddSeries(Series);
  end;

begin
  { Remove any previous simulation series. }
  for I := Plot.Series.Count - 1 downto 0 do
    if Plot.Series[I].SeriesType = SERIES_TYPE_SIMULATION then
      Plot.Series.Delete(I);

  NumRows := AData.r;

  { Resolve the X column. Empty / unrecognised name falls back to column 0. }
  XColIdx := FindCol(AXAxisName);
  if XColIdx < 0 then XColIdx := 0;
  XLabel := AData.columnHeader[XColIdx];

  TColorManager.ResetCycle;

  { Plot only the requested Y columns. An empty array yields an empty
    plot - this is intentional, and is what the live-update path needs
    when the user has unchecked every species. }
  for I := 0 to High(AYAxisNames) do
  begin
    YColIdx := FindCol(AYAxisNames[I]);
    if YColIdx < 0 then Continue;
    AddSeriesForColumn(YColIdx);
  end;

  Plot.XAxisTitle.Text := XLabel;
  Plot.Redraw;
  TabControl1.ActiveTab := tbPlot;
end;

procedure TfrmMain.PlotClearSimulationSeries;
var
  I: Integer;
begin
  for I := Plot.Series.Count - 1 downto 0 do
    if Plot.Series[I].SeriesType = SERIES_TYPE_SIMULATION then
      Plot.Series.Delete(I);
end;

procedure TfrmMain.PlotAddSeries(ASeries: TObject);
begin
  if ASeries is TPlotSeries then
    Plot.AddSeries(TPlotSeries(ASeries));
end;

procedure TfrmMain.PlotRedraw;
begin
  Plot.Redraw;
end;

procedure TfrmMain.PlotRecolorSimulationSeries(const ANextColor: TFunc<TAlphaColor>);
var
  I: Integer;
  NewColor: TAlphaColor;
begin
  if not Assigned(ANextColor) then Exit;
  for I := 0 to Plot.Series.Count - 1 do
    if Plot.Series[I].SeriesType = SERIES_TYPE_SIMULATION then
    begin
      NewColor := ANextColor();
      Plot.Series[I].LineColor         := NewColor;
      Plot.Series[I].MarkerStrokeColor := NewColor;
    end;
  Plot.Redraw;
end;

procedure TfrmMain.CopyTextToTextWindow (AString : String);
begin
  TabControl1.ActiveTab := tbTextView;
  moTextView.text := AString;
end;

procedure TfrmMain.AppendToAntimonySource(const ABlock: string);
const
  BLOCK_TAG = '// [SliderValues]';
var
  Src:    string;
  TagPos: Integer;
begin
  Src := moAntimony.Text;
  TagPos := Pos(BLOCK_TAG, Src);
  if TagPos > 0 then
    Src := Copy(Src, 1, TagPos - 1).TrimRight;
  moAntimony.Text := Src + sLinebreak + ABlock;
end;


function TfrmMain.PlotGetSimulationSeriesInfo: TArray<TPlotSeriesColorInfo>;
var
  I:   Integer;
  Src: TPlotSeries;
begin
  if (Plot = nil) or (Plot.Series = nil) then Exit(nil);

  SetLength(Result, Plot.Series.Count);
  for I := 0 to Plot.Series.Count - 1 do
  begin
    Src := Plot.Series[I];
    Result[I].Name      := Src.Name;
    Result[I].LineColor := Src.LineColor;
  end;
end;


function TfrmMain.PlotGetPlotInfo: TPlotInfo;
begin
  Result.LegendVisible := Plot.LegendStyle.Visible;
end;

end.
