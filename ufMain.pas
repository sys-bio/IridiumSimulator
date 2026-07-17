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
  System.UIConsts,
  System.UITypes,
  System.Classes,
  System.Variants,
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
  uCommonTypes,
  FMX.SpinBox, FMX.TabControl,
  FMX.ListBox,
  System.Math.Vectors, FMX.Controls3D,
  FMX.Layers3D, uSkiaCodeEditor;

const
    VERSION = '0.982            ';

type
  TfrmMain = class(TForm, IAnalysisContext)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout6: TLayout;
    SliderContainer: TLayout;
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
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cboLoadedFilename: TComboBox;
    Label4: TLabel;
    lblParameterName: TLabel;
    btnCopyToStorage: TButton;
    Label5: TLabel;
    chkOverlayData: TCheckBox;
    moAntimony: TSkiaCodeEditor;
    procedure FormCreate(Sender: TObject);
    procedure btnTimeCourse1Click(Sender: TObject);
    procedure btnSteadyStateClick(Sender: TObject);
    procedure moAntimony1ChangeTracking(Sender: TObject);
    procedure mnuLoadFileClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure chkAutoscaleXChange(Sender: TObject);
    procedure chkAutoScaleYChange(Sender: TObject);
    procedure chkShowLegendChange(Sender: TObject);
    procedure btnEditGraphClick(Sender: TObject);
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
    procedure moAntimony1PresentationNameChoosing(Sender: TObject;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure cboLoadedFilenameChange(Sender: TObject);
    procedure btnCopyToStorageClick(Sender: TObject);
    procedure Splitter2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Splitter2Moved(Sender: TObject);
    procedure moAntimony1ViewportPositionChange(Sender: TObject;
      const OldViewportPosition, NewViewportPosition: TPointF;
      const ContentSizeChanged: Boolean);
  private
    FSession:          TModelSession;
    FSliderFrame:      TFrameSliderContainer;
    FFrameTimeCourse:  TFrameTimeCourse;
    FFrameSteadyState: TFrameSteadyState;
    FFrameParameterScan: TFrameParameterScan;
    FActiveFrame:      TFrame;

    { When True, the next PlotBeginRebuild skips its styling snapshot. Set on
      every frame switch so the incoming frame's first rebuild does not capture
      the outgoing frame's leftover series under the incoming frame's key. }
    FSuppressPlotSnapshot: Boolean;

    FCurrentFileName : String;
    FireEvent: Boolean;
    FIsModifiedSinceLastSave: Boolean;

    FSavedViewport : TPointF;
    FDragging  : Boolean;

    FListOfLoadedDataFiles : TList<TLoadDataFile>;

    procedure CreateSession;
    procedure CreateSliderContainer;
    procedure CreateAnalysisFrames;

    { Settings-store key for the currently active analysis frame, or '' if none
      / unrecognised. Used to persist plot styling per analysis. }
    function ActiveAnalysisKey: string;

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
    procedure PlotBeginRebuild;
    procedure PlotEndRebuild;
    procedure PlotRecolorSimulationSeries(const ANextColor: TFunc<TAlphaColor>);
    function  PlotGetSimulationSeriesInfo: TArray<TPlotSeriesColorInfo>;
    function  PlotGetPlotInfo: TPlotInfo;

    procedure CopyTextToTextWindow (AString : String);
  public
    { Public declarations }
    procedure SplitterBeforeMove;
    procedure SplitterAfterMove;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  IOUtils, uPlotSeries, uColorManager, uAntimonyAPI, uRoadRunner, ufPlotEditor, uMySplitter, uLanguageKeywords;

const
  DEFAULT_SLIDER_HEIGHT = 322.0;

  DefaultModel = '''
      // Load a model from disk, type in a model,
      // or pick one of the example models from
      // the Examples menu

      // Note // is used to indicate a comment

      // eg

      A -> B; k1*A
      B -> C; k2*B
      k1 = 0.35; k2 = 0.2
      A = 10

      // If you're not sure what to do, just
      // click the simulate button to the left
  ''';

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

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    FSession.Free;
    // Frames and slider frame are owned by Self (TComponent ownership) and will be freed automatically.
    Action := TCloseAction.caFree;
  except
    on E: Exception do
     begin
     ShowMessage('An internal error occurred: ' + E.Message);
     Action := TCloseAction.caFree;
     end;
  end;
end;

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
  // This is part of a hack to avoid the tmemo snapping
  // to the top when a user moves the spitters. TMySplitter
  // allows us access the splitter movement before it tries
  // to redraw stuff.
  PPointer(Splitter1)^ := TMySplitter;
  PPointer(Splitter2)^ := TMySplitter;

  moAntimony.BackgroundColor := $FF1F1F1F;
  moAntimony.textColor := $FFFFFFFF;
  moAntimony.CaretColor := $FF808080;
  moAntimony.FontSize := 16;
  moAntimony.GutterColor := $FF272727;
  moAntimony.GutterTextColor := $FF808080;
  moAntimony.Highlighter.UseAntimony;
  moAntimony.Highlighter.AddKeywords(AntimonyKeywords);

  //moAntimony.SetText (DefaultModel);

  TabControl1.ActiveTab := tbPlot;

  FListOfLoadedDataFiles := TList<TLoadDataFile>.Create;

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
     Showmessage ('Unable to find the libRoadRunner library. This is usually the result of a bad installation: ' + errMsgAnsi + ')');
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

  edtXMin.Enabled := False; edtXmax.Enabled := False;
  lblXMin.Enabled := False; lblXMax.Enabled := False;

  edtYMin.Enabled := False; edtYmax.Enabled := False;
  lblYMin.Enabled := False; lblYMax.Enabled := False;

  edtXMin.Text := '0';
  edtXMax.Text := '20';
  edtYMin.Text := '0';
  edtYMax.Text := '10';

  spFontSize.Value := 16;
  //moAntimony.Font.Size := spFontSize.Value;
  moAntimony.FontSize := spFontSize.Value;

  ShowAnalysisFrame(FFrameTimeCourse);   { default view }

  FireEvent := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
   FListOfLoadedDataFiles.Free;
end;


procedure TfrmMain.SplitterBeforeMove;
begin
  FDragging := True;
end;

procedure TfrmMain.SplitterAfterMove;
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      FDragging := False;
    end);
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
  //Result := moAntimony.Text;
  Result := moAntimony.GetText;
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
      //uAntimonyAPI.getSBMLFromAntimony(moAntimony.Lines.Text);
      uAntimonyAPI.getSBMLFromAntimony(moAntimony.GetText);
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
      //moAntimony.text := uAntimonyAPI.getAntimonyFromSBML(SBMLString);
      moAntimony.SetText (uAntimonyAPI.getAntimonyFromSBML(SBMLString));
      FSession.ClearDirty;
    end;
end;

procedure TfrmMain.mnuLoadFileClick(Sender: TObject);
begin
 if OpenDialogAnt.Execute then
    begin
    FSession.Unload;
    //moAntimony.Text := TFile.ReadAllText(OpenDialogAnt.FileName);
    moAntimony.SetText (TFile.ReadAllText(OpenDialogAnt.FileName));

    FCurrentFileName := ExtractFileName(OpenDialogAnt.FileName);
    Caption := 'Iridium II: ' + FCurrentFileName;
    FSession.ClearDirty;
    end;
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
begin
  //moAntimony.Text := '';
  moAntimony.SetText('');
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
     TFile.WriteAllText(SaveDialogAnt.FileName, moAntimony.GetText);
     FIsModifiedSinceLastSave := False;
     end;
end;

procedure TfrmMain.moAntimony1ChangeTracking(Sender: TObject);
begin
  FIsModifiedSinceLastSave := True;
  FSession.MarkDirty;
end;

procedure TfrmMain.moAntimony1PresentationNameChoosing(Sender: TObject;
  var PresenterName: string);
begin
  // The choice of the presentation class by the control
  //PresenterName := 'RichEditStyled';
end;

procedure TfrmMain.moAntimony1ViewportPositionChange(Sender: TObject;
  const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
begin
  //if FDragging and ContentSizeChanged then
  //  moAntimony.ViewportPosition := FSavedViewport
  //else if not FDragging then
  //  FSavedViewport := NewViewportPosition;
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
    { A structurally different model means series names no longer match the
      stored per-analysis styling, so discard it rather than mis-applying it
      to unrelated series. A compatible in-place edit keeps the styling. }
    Plot.ClearAllSettings;
    FSliderFrame.ClearSliders;
    FSliderFrame.LoadParams(FSession.GetTunableNames,    { <-- refresh catalogue }
                            FSession.GetTunableValues);
  end
  else
    FSliderFrame.RefreshValues(FSession.GetTunableNames,
                               FSession.GetTunableValues);
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
    { Model gone -> stored per-analysis plot styling is meaningless. }
    Plot.ClearAllSettings;
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
  { Capture the outgoing frame's current plot styling before we switch away.
    Its series are still on the plot, so this snapshots any edits the user made
    since its last re-plot. On return the incoming frame's PlotEndRebuild will
    restore whichever styling it last saved. FSuppressPlotSnapshot then makes
    the incoming frame's first PlotBeginRebuild skip its snapshot, so it does
    not capture these leftover series under its own key. }
  if (ATarget <> FActiveFrame) and (ActiveAnalysisKey <> '') then
    Plot.SaveSettings(ActiveAnalysisKey);
  if ATarget <> FActiveFrame then
    FSuppressPlotSnapshot := True;

  FFrameTimeCourse.Visible    := False;
  FFrameSteadyState.Visible   := False;
  FFrameParameterScan.Visible := False;

  { Sliders persist across frame switches. The shared slider container is a
    single instance, and slider moves already write into the live model
    (OnSliderChanged -> Session.SetParameterValue), so the user's tuned
    values carry over to whichever analysis they switch to. We deliberately
    do NOT ClearSliders here — that is reserved for genuine model changes
    (SessionModelReloaded with a changed parameter set, or the model going
    unloaded in SessionStateChanged). }
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
  moAntimony.FontSize := spFontSize.Value;
end;

procedure TfrmMain.Splitter2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  moAntimony.Align := TAlignLayout.None;
  moAntimony.Visible := False;
  moAntimony.BeginUpdate;
  //HMS FSavedViewport := moAntimony.ViewportPosition;
end;

procedure TfrmMain.Splitter2Moved(Sender: TObject);
begin
  // Defer until after the layout pass finishes
  TThread.ForceQueue(nil,
    procedure
    begin
      //HMS moAntimony.ViewportPosition := FSavedViewport;
      moAntimony.Align := TAlignLayout.Client;
      moAntimony.EndUpdate;
      moAntimony.Visible := True;
    end);
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
  Plot.ClearSeriesKind(skData);
  FListOfLoadedDataFiles.Clear;
  cboLoadedFilename.Clear;
  lblParameterName.Text := 'None';
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

procedure TfrmMain.btnGeneratePythonClick(Sender: TObject);
var
  Exporter: IPythonScriptExporter;
  Script:   string;
begin
 if Supports(FActiveFrame, IPythonScriptExporter, Exporter) then
  begin
    Script := Exporter.GetPythonScript(moAntimony.GetText);
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
    Index : Integer;
    LoadedDataFile : TLoadDataFile;
    FileName : String;
    ClearSeries, ClearDataKind : Boolean;
begin
  if OpenDialog1.Execute then
     begin
     ClearSeries := False;
     if chkOverlayData.IsChecked then
        ClearDataKind := False
     else
        ClearDataKind := True;

     FileName := ExtractFileName(OpenDialog1.FileName);
     for i := 0 to FListOfLoadedDataFiles.Count - 1 do
         if FileName = FListOfLoadedDataFiles[i].FileName then
            begin
            showmessage ('This data file has already been loaded');
            exit;
            end;

     FireEvent := False;
     try
       Index := cboLoadedFilename.Items.Add(FileName);
       cboLoadedFilename.ItemIndex := Index;
     finally
       FireEvent := True;
     end;

     // Note TStringList Series don't own the Series, so its ok to free the stringlist.
     Series := Plot.LoadData(OpenDialog1.FileName, False, True, ClearSeries, ClearDataKind);
     for i := 0 to Series.Count - 1 do
         begin
         TPlotSeries (Series.Objects[i]).SeriesKind := skData;
         TPlotSeries (Series.Objects[i]).SeriesId := FileName + '_' + inttostr (i);
         TPlotSeries (Series.Objects[i]).LineVisible := False;
         TPlotSeries (Series.Objects[i]).MarkerStrokeWidth := 1.5;
         //TPlotSeries (Series.Objects[i]).MarkerFillColor := TPlotSeries (Series.Objects[i]).MarkerStrokeColor;
         TPlotSeries (Series.Objects[i]).MarkerSize := 4;
         end;

     lblParameterName.Text := TPlotSeries (Series.Objects[0]).XLabel;

     LoadedDataFile := TLoadDataFile.Create;
     LoadedDataFile.FileName := FileName;
     LoadedDataFile.ParameterName := lblParameterName.Text;
     for i := 0 to Series.Count - 1 do
         LoadedDataFile.Series.Add(TPlotSeries (Series.Objects[i]).Clone);
     FListOfLoadedDataFiles.Add(LoadedDataFile);

     Series.Free;
     Plot.Redraw;
     end;
end;

procedure TfrmMain.cboLoadedFilenameChange(Sender: TObject);
var i : integer;
    Index : Integer;
    Found : Boolean;
begin
  if not FireEvent then Exit;

  Found := False;
  for i := 0 to FListOfLoadedDataFiles.Count - 1 do
      if cboLoadedFilename.items[cboLoadedFilename.ItemIndex] = FListOfLoadedDataFiles[i].FileName then
         begin
         lblParameterName.Text := FListOfLoadedDataFiles[i].ParameterName;
         Found := True;
         Index:= i;
         break;
         end;
  if Found then
     begin
     Plot.ClearSeriesKind(skData);
     for i := 0 to FListOfLoadedDataFiles[Index].Series.Count - 1 do
         Plot.AddSeries(FListOfLoadedDataFiles[Index].Series[i].Clone);
     end;
  Plot.Redraw;
end;


procedure TfrmMain.btnCopyToStorageClick(Sender: TObject);
var i : integer;
    Index : Integer;
    StoredSeries : TPlotSeries;
    Found : Boolean;
begin
  Found := False;
  for i := 0 to FListOfLoadedDataFiles.Count - 1 do
      if cboLoadedFilename.items[cboLoadedFilename.ItemIndex] = FListOfLoadedDataFiles[i].FileName then
         begin
         lblParameterName.Text := FListOfLoadedDataFiles[i].ParameterName;
         Found := True;
         Index:= i;
         break;
         end;

  for i := 0 to Plot.Series.Count -1 do
      if Plot.Series[i].SeriesKind = skData then
         begin
         if Plot.Series[i].SeriesId = FListOfLoadedDataFiles[Index].Series[i].SeriesId then
            begin
            FListOfLoadedDataFiles[Index].Series[i].MarkerSize := Plot.Series[i].MarkerSize;
            FListOfLoadedDataFiles[Index].Series[i].MarkerFillColor := Plot.Series[i].MarkerFillColor;
            FListOfLoadedDataFiles[Index].Series[i].MarkerStrokeColor := Plot.Series[i].MarkerStrokeColor;
            end;
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
  moAntimony.SetText (Model.ModelStr);
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
 moAntimony.GutterVisible := chkShowLineNumbers.IsChecked;
 //TRichEditStyled(moAntimony.Presentation).ShowGutter := chkShowLineNumbers.IsChecked;
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

function TfrmMain.ActiveAnalysisKey: string;
begin
  if FActiveFrame = FFrameTimeCourse then
    Result := 'TimeCourse'
  else if FActiveFrame = FFrameParameterScan then
    Result := 'ParameterScan'
  else if FActiveFrame = FFrameSteadyState then
    Result := 'SteadyState'
  else
    Result := '';
end;

procedure TfrmMain.PlotBeginRebuild;
begin
  { Snapshot the plot's current styling under the active frame's key, unless a
    frame switch just occurred (in which case the plot still shows the previous
    frame's series and capturing them here would corrupt this frame's key). }
  if not FSuppressPlotSnapshot then
    if ActiveAnalysisKey <> '' then
      Plot.SaveSettings(ActiveAnalysisKey);
  FSuppressPlotSnapshot := False;
end;

procedure TfrmMain.PlotEndRebuild;
begin
  { Re-apply the active frame's saved styling to the just-rebuilt series
    (matched by series name) and redraw. No-op the first time a frame plots,
    before it has anything stored. }
  if (ActiveAnalysisKey <> '') and Plot.HasSettings(ActiveAnalysisKey) then
    Plot.RestoreSettings(ActiveAnalysisKey);
  Plot.Redraw;
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
    Series.YLabel        := AData.columnHeader[AColIdx];
    Series.LineColor     := TColorManager.NextColor;
    Series.LineWidth     := 2.5;
    Series.MarkerVisible := False;
    for J := 0 to NumRows - 1 do
      Series.AddXY(AData[J, XColIdx], AData[J, AColIdx]);
    Plot.AddSeries(Series);
  end;

begin
  { Remove any previous simulation series. }
  Plot.ClearSeriesKind(skSimulation);

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
begin
  Plot.ClearSeriesKind(skSimulation);
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
    if Plot.Series[I].SeriesKind = skSimulation then
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
  Src := moAntimony.GetText;
  TagPos := Pos(BLOCK_TAG, Src);
  if TagPos > 0 then
    Src := Copy(Src, 1, TagPos - 1).TrimRight;
  moAntimony.SetText (Src + sLinebreak + ABlock);
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
