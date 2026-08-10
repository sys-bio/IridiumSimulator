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
  System.JSON,
  Generics.Collections,
  SkPlotPaintBox,
  uRR2DSimpleMatrix,
  uAnalysisTypes,
  uModelSession,
  uFrameSliderContainer,
  uFrameTimeCourse,
  uFrameSteadyState,
  uFrameParameterScan,
  uFrameMetadata,
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
  Sim.Meta,
  Sim.Meta.Types,
  Sim.Meta.Model,
  Sim.Meta.Registry,
  Sim.Meta.Python,
  Sim.Meta.SedML.Export,
  uMetaExperiments,
  uMetaOutput,
  uMetaSymbolProvider,
  FMX.SpinBox, FMX.TabControl,
  FMX.ListBox,
  System.Math.Vectors, FMX.Controls3D,
  FMX.Layers3D, uSkiaCodeEditor, uPlotAnnotation, uRhoMarkdownViewer,
  System.Math, System.Threading, uBioModelsCache;

const
    VERSION = '0.985';

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
    tbHelp: TTabItem;
    HelpViewer: TRhoMarkdownViewer;
    Layout10: TLayout;
    btnAntimonyHelp: TButton;
    btnSimulationHelp: TButton;
    btnLighDark: TButton;
    mnuExportTelluriumScript: TMenuItem;
    mnuExportSEDMLFile: TMenuItem;
    mnuExportCOMBINEArchve: TMenuItem;
    MenuItem9: TMenuItem;
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
    procedure btnAntimonyHelpClick(Sender: TObject);
    procedure btnSimulationHelpClick(Sender: TObject);
    procedure btnLighDarkClick(Sender: TObject);
    procedure mnuExportTelluriumScriptClick(Sender: TObject);
    procedure mnuExportSEDMLFileClick(Sender: TObject);
    procedure mnuExportCOMBINEArchveClick(Sender: TObject);
  private
    FSession:          TModelSession;
    FSliderFrame:      TFrameSliderContainer;
    FFrameTimeCourse:  TFrameTimeCourse;
    FFrameSteadyState: TFrameSteadyState;
    FFrameParameterScan: TFrameParameterScan;
    { The metadata report. Not an analysis: it computes nothing and plots
      nothing, so ActiveAnalysisKey deliberately has no entry for it and
      the shell's plot-styling / loaded-data bookkeeping skips it. }
    FFrameMetadata:    TFrameMetadata;
    FActiveFrame:      TFrame;

    { When True, the next PlotBeginRebuild skips its styling snapshot. Set on
      every frame switch so the incoming frame's first rebuild does not capture
      the outgoing frame's leftover series under the incoming frame's key. }
    FSuppressPlotSnapshot: Boolean;

    { The X-axis title PlotData derived from the X column, and the value it
      held when the last snapshot was taken. A styling snapshot serialises
      the axis title TEXT along with its font and colour, so restoring it
      after a rebuild puts the previous X column's name back — which is why
      changing the X selection redrew the plot but left the label behind.
      Comparing the restored text against the auto value at snapshot time
      tells the two cases apart: unchanged means nobody had renamed the
      axis, so the fresh column name should win; different means the user
      (or an @plot xlabel) titled it deliberately and it must be kept. }
    FAutoXTitle:         string;
    FSnapshotAutoXTitle: string;

    { ── BioModels search ──────────────────────────────────────────────
      A search box in the top strip whose results drop down under it.
      Every control here is built in code: the .fmx is multi-megabyte and
      the dropdown has to be a child of the FORM rather than of the strip,
      so it can overhang the panels below.

      The cache object is touched only by the search task, which is
      serialised through FBioSearchRunning / FBioSearchAgain — one search
      at a time, with at most one more queued. Fetching a model uses its
      own short-lived cache object, so a click during a search cannot
      share the HTTP client with it. }
    FBioCache:         TBiomodelsCache;
    FBioSearchEdit:    TEdit;
    FBioSearchList:    TListBox;
    FBioSearchTimer:   TTimer;
    { The term the visible results describe, so a reply that arrives after
      the user has typed on can be recognised as stale and dropped. }
    FBioSearchTerm:    string;
    FBioSearchRunning: Boolean;
    FBioSearchAgain:   Boolean;
    FBioFetching:      Boolean;
    { OnChangeTracking does not distinguish typing from an assignment, so
      the "Loading..." message this code puts in the box would otherwise
      be searched for. }
    FBioSuppressSearch: Boolean;

    FCurrentFileName : String;
    FireEvent: Boolean;
    FIsModifiedSinceLastSave: Boolean;

    FSavedViewport : TPointF;
    FDragging  : Boolean;

    { Loaded CSV overlays, one catalogue per analysis panel, keyed by
      ActiveAnalysisKey. Owns its values. Reach it through CurrentPanelData
      rather than indexing directly — panels are created on first use. }
    FDataFilesByPanel : TObjectDictionary<string, TPanelDataFiles>;

    { False until the parameter-scan panel is first shown for the current model,
      at which point its observables are seeded from the time-course selection —
      once only. Set back to False on a structural model reload so the next
      model seeds afresh. }
    FScanObservablesSeeded : Boolean;

    { Stable colour per simulation observable. A series' colour is decided the
      first time that name is plotted and remembered here, so it never depends
      on the observable's position in the (alphabetically sorted) selection set.
      Without this, adding/removing one species re-indexed the palette for every
      other one, producing repeated colours and a curve changing colour when it
      was unchecked and rechecked. Reset on a structural model reload. }
    FSimColorByName : TDictionary<string, TAlphaColor>;

    { ── simulation metadata ────────────────────────────────────────────────
      The model's own @simulate / @plot / @scan block, parsed from the editor
      text. Both objects are rebuilt on every parse; nothing may hold a
      reference across one. See uMetaExperiments for the grouping rules. }
    FMeta:        TSimulationMetadata;
    FExperiments: TMetaExperimentSet;

    { Full path of the loaded model, or '' for untitled / example models.
      Metadata resolves a 'file' key against the directory holding the
      model, never the process working directory (spec 11.4), so the path
      matters and the bare FCurrentFileName will not do. }
    FCurrentFilePath: string;

    { The metadata as it stood when it was last applied to the panels.
      Compared on reload so an edited block can be OFFERED rather than
      applied behind the user's back. }
    FAppliedMetaSig: string;

    { Global opt-out. Off still parses, still reports diagnostics and still
      fills the Experiments list — it only stops the block touching the
      user's controls when a model is opened. }
    FApplyMetaOnOpen: Boolean;

    { The notice strip above the plot. A metadata block lives inside a
      comment and is otherwise invisible, so a model that carries one has
      to say so; without this the presets appear to arrive from nowhere. }
    { ── in-app help ────────────────────────────────────────────────────────
      The Help tab holds a markdown viewer beside the editor, so a document
      can be read while the model it describes is being written. The
      documents are ordinary .md files on disk, deliberately: a lab can
      annotate or extend them, which embedding them in the executable would
      prevent. }
    FHelpDocId:   string;    { document currently loaded, '' if none }
    FHelpScrollY: Single;    { kept current by OnScroll, so returning to
                               the tab resumes where the reader was }
    FHelpDark:    Boolean;

    FMetaBar:      TLayout;
    FMetaBarLabel: TLabel;
    FMetaBarApply: TButton;
    FMetaBarView:  TButton;
    FMetaBarWrite: TButton;
    FMetaBarClose: TButton;
    FMnuMetaApply: TMenuItem;
    { Metadata ▸ Run Experiment. Its children are rebuilt from the
      experiment set every time the Metadata menu is opened, so the list
      describes the block as the editor currently spells it. }
    FMnuMetaRun:   TMenuItem;

    { ── metadata export ────────────────────────────────────────────────────
      Tellurium, SED-ML and COMBINE, all driven from the parsed metadata
      rather than from the panels — so a file authored by hand exports the
      same as one built through the GUI. The existing Python button on the
      time-course panel is a different thing and stays: it exports what is
      on screen, for users who have not learned the metadata format. }
    { Drop every series on the plot — simulation output and loaded overlay data
      alike — plus the loaded-data bookkeeping. For use when the model itself
      changes and nothing on the old plot still describes it. }
    procedure ClearPlotAndLoadedData;
    procedure ClearLoadedDataFiles;
    procedure ClearAllLoadedDataFiles;

    { The loaded-data catalogue of the panel currently showing, created on
      first use. Never nil. }
    function  CurrentPanelData: TPanelDataFiles;

    { Frame-switch halves of the per-panel overlay handling: remember what the
      outgoing panel had on screen, then put the incoming panel's data back. }
    procedure CapturePanelDataState;
    procedure RestorePanelDataState;

    { Loaded data overlays belong to the panel they were loaded on. Styling the
      user edits on a plotted overlay is copied back into that panel's stored
      clones so it survives re-selection and panel switches. }
    procedure SyncOverlayStyleToStorage;

    { Drop the per-analysis plot-styling snapshots (but not DEFAULT_STYLE_KEY),
      for use when the model changes and the old panel styling no longer applies. }
    procedure ClearPanelStyleSnapshots;

    procedure CreateSession;
    procedure CreateSliderContainer;
    procedure CreateAnalysisFrames;

    { Locate a help document. Returns False, with APath naming the places
      searched, when it cannot be found. }
    function  FindHelpFile(const AFileName: string; out APath: string): Boolean;
    procedure ShowHelpDoc(const AId: string; const AAnchor: string = '');
    procedure HelpScrolled(Sender: TObject);
    procedure HelpLinkClicked(Sender: TObject; const AUrl: string);

    { The menu items are designer-created, so only their enabled state is
      managed here. The three routines below are the exports proper,
      called from the .fmx OnClick stubs — named for what they do rather
      than for a menu item, so a caption or a menu move breaks nothing. }
    procedure UpdateExportMenuState;
    procedure ExportTelluriumScript;
    procedure ExportSedML;
    function  SbmlRootNamespace(const ASbml: string): string;
    procedure ExportOmexArchive;
    { Somewhere to write to, defaulted from the model's own name. }
    function  AskExportPath(const ATitle, AFilter, ADefaultExt: string;
                            out APath: string): Boolean;
    { Convert the current model to SBML. SED-ML references quantities by
      XPath into SBML, so the archive cannot be built without it. }
    function  BuildSbml(out ASbml: string; out AError: string): Boolean;
    { The export report. Anything dropped or approximated must reach the
      user — someone depositing an archive for a journal needs to know
      what it does and does not capture. }
    procedure ShowExportReport(const AWhat, APath: string;
                               ADiags: TDiagnosticList);

    procedure CreateMetaBar;
    procedure CreateMetaMenu;

    { BioModels search — see the implementation section for how the pieces
      fit together. }
    procedure CreateBioModelsSearch;
    procedure BioSearchChanged(Sender: TObject);
    procedure BioSearchKeyDown(Sender: TObject; var Key: Word;
                               var KeyChar: WideChar; Shift: TShiftState);
    procedure BioSearchTimerTick(Sender: TObject);
    procedure StartBioSearch;
    procedure ShowBioResults(const AResults: TBiomodelArray;
                             const ATerm: string);
    procedure ShowBioMessage(const AText: string);
    procedure PositionBioList;
    procedure HideBioList;
    procedure BioResultClick(const Sender: TCustomListBox;
                             const Item: TListBoxItem);
    procedure LoadBiomodelSBML(const ASBML, AModelID, ATitle: string);
    procedure ShowMetaBar(const AText: string; AShowApply: Boolean);
    procedure HideMetaBar;
    procedure MetaBarApplyClick(Sender: TObject);
    procedure MetaBarCloseClick(Sender: TObject);
    procedure MetaBarViewClick(Sender: TObject);
    procedure MetaBarWriteClick(Sender: TObject);
    procedure OutputStateChanged;
    procedure MetaMenuApplyClick(Sender: TObject);
    procedure MetaMenuOpening(Sender: TObject);
    procedure RebuildRunExperimentMenu;
    procedure MetaRunExperimentClick(Sender: TObject);

    { Re-read the metadata block from the editor. AApplyToPanels = True only
      on the paths that OPEN a model. }
    procedure ParseMetadata(AApplyToPanels: Boolean);
    procedure ApplyMetadataToPanels;
    function  MetadataSignature: string;

    { Settings-store key for the currently active analysis frame, or '' if none
      / unrecognised. Used to persist plot styling per analysis. }
    function ActiveAnalysisKey: string;

    procedure CheckNumberKeys (edt : TEdit; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

    function  GetAntimonyText: string;
    procedure SessionStateChanged(Sender: TObject);
    procedure ShowAnalysisFrame(ATarget: TFrame);

    procedure SessionModelReloaded(Sender: TObject;  AParameterSetChanged: Boolean);
    procedure AppendToAntimonySource(const ABlock: string;
                                     AReplace: Boolean = False);

    { IAnalysisContext }
    function  GetSession: TModelSession;
    function  GetSliderContainer: TFrameSliderContainer;
    function  GetSteadyStateHost: TScrollBox;
    procedure ShowSteadyStateTab;
    function  GetMetadata: TSimulationMetadata;
    function  GetMetaExperiments: TMetaExperimentSet;
    procedure ShowHelp(const ATopic: string);

    procedure PlotData(const AData: T2DMatrix;
                       const AXAxisName: string;
                       const AYAxisNames: TArray<string>);
    procedure PlotSetXAxisTitle(const ATitle: string);
    procedure PlotClearSimulationSeries;
    procedure PlotAddSeries(ASeries: TObject);
    procedure PlotRedraw;
    procedure PlotBeginRebuild;
    procedure PlotEndRebuild;
    procedure PlotRecolorSimulationSeries(const ANextColor: TFunc<TAlphaColor>);
    procedure PlotApplyMetaStyle(ACmd: TPlotCommand);
    procedure PlotCaptureUserStyle;
    procedure PlotRestoreUserStyle;
    { Settings-store key for the active panel's pre-metadata styling, or ''
      if no analysis panel is active. }
    function  UserStyleKey: string;
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

  { Reserved settings-store key holding the plot's pristine styling, captured
    once at startup. Restored for any analysis panel that hasn't been visited
    yet, so a fresh panel starts from defaults (linear axes, autoscale) rather
    than inheriting the styling the previously used panel left on the shared
    plot. Never a real ActiveAnalysisKey, so it can't collide with a panel. }
  DEFAULT_STYLE_KEY = '__default__';

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

{ NOTE: a local TModelErrorState used to be declared here, shadowing the
  real one in uCommonTypes that uAntimonyAPI actually returns. Its only
  user was the SBML export, which declared a variable of the shadow type,
  never assigned it, and then read it — writing whatever happened to be on
  the stack. Removed rather than fixed in place, so the same mistake
  cannot be made again. }


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
  moAntimony.Highlighter.MetadataTagColor := claCoral;
  moAntimony.Highlighter.MetadataColor := claLightblue;

  moAntimony.SelectionColor := claCadetblue;//  claCornflowerblue;
  moAntimony.CaretColor := claWhite;

  //moAntimony.SetText (DefaultModel);

  TabControl1.ActiveTab := tbPlot;

  FDataFilesByPanel := TObjectDictionary<string, TPanelDataFiles>.Create([doOwnsValues]);
  FSimColorByName := TDictionary<string, TAlphaColor>.Create;

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

  { Declare what this build cannot realise, which is what makes "unknown to
    the format" and "known but not supported here" different messages
    (conformance C5) — the user's remedy differs. Kept out of the registry
    table so the shared library stays tool-neutral. }
  TMetaRegistry.SetUnsupported(['bifurcation', 'sensitivity', 'figure'], []);

  FMeta := TSimulationMetadata.Create;
  FMeta.ToolName    := 'Iridium';
  FMeta.ToolVersion := VERSION;
  { Leave SupportsContinuation False: without a continuation algorithm a
    scan over a steady state may converge to different branches at adjacent
    parameter values and miss multistability entirely, producing a curve
    that looks plausible and is wrong (spec 9.3). The library warns and
    skips it for us. }
  FApplyMetaOnOpen := True;
  CreateMetaBar;
  CreateMetaMenu;
  CreateBioModelsSearch;
  UpdateExportMenuState;
  OutputStateChanged;

  Plot.AutoXScaling := True;
  Plot.AutoYScaling := True;
  Plot.LegendStyle.Visible := True;

  { Snapshot the pristine plot styling now, before any user edits, as the
    baseline a never-visited panel restores. Must precede the first
    ShowAnalysisFrame below. }
  Plot.SaveSettings(DEFAULT_STYLE_KEY);

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

  { Edits to the source must mark the session dirty so the next EnsureLoaded
    re-parses instead of simulating the previously loaded model. Wired here
    rather than in the .fmx: TSkiaCodeEditor fires OnChange on text mutation
    only (not on SetText / load), which is exactly the dirty signal we want. }
  moAntimony.OnChange := moAntimony1ChangeTracking;

  { The three help buttons are wired in the .fmx. These two are not
    designer-visible events, so they are assigned here: OnScroll keeps the
    reading position current, OnLinkClick keeps cross-references inside
    the app instead of opening a browser. }
  HelpViewer.OnScroll    := HelpScrolled;
  HelpViewer.OnLinkClick := HelpLinkClicked;

  { The designed title is the auto title until PlotData derives one, so the
    first rebuild's comparison in PlotEndRebuild has something valid to
    measure against. }
  FAutoXTitle := Plot.XAxisTitle.Text;

  ShowAnalysisFrame(FFrameTimeCourse);   { default view }

  FireEvent := True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
   { doOwnsValues frees each TPanelDataFiles, which frees its datasets. }
   FDataFilesByPanel.Free;
   FSimColorByName.Free;
   { Experiments hold borrowed references into FMeta, so they go first. }
   FExperiments.Free;
   FMeta.Free;
   FBioCache.Free;
end;

{ The overlay catalogue of the panel showing now. Panels get one on first use,
  so every caller can treat the result as present. }
function TfrmMain.CurrentPanelData: TPanelDataFiles;
var
  Key: string;
begin
  Key := ActiveAnalysisKey;
  if not FDataFilesByPanel.TryGetValue(Key, Result) then
  begin
    Result := TPanelDataFiles.Create;
    FDataFilesByPanel.Add(Key, Result);
  end;
end;

{ Drop the loaded data of the panel showing now, and the UI that describes it.
  Other panels keep theirs. }
procedure TfrmMain.ClearLoadedDataFiles;
begin
  CurrentPanelData.ClearFiles;

  FireEvent := False;
  try
    cboLoadedFilename.Clear;
  finally
    FireEvent := True;
  end;
  lblParameterName.Text := 'None';
end;

{ Drop every panel's loaded data. For a model change, where no dataset on any
  panel still describes what is loaded. }
procedure TfrmMain.ClearAllLoadedDataFiles;
begin
  FDataFilesByPanel.Clear;   { doOwnsValues frees the panels and their datasets }

  FireEvent := False;
  try
    cboLoadedFilename.Clear;
  finally
    FireEvent := True;
  end;
  lblParameterName.Text := 'None';
end;

procedure TfrmMain.ClearPlotAndLoadedData;
begin
  Plot.ClearSeries;
  ClearAllLoadedDataFiles;
  Plot.Redraw;
end;

{ Drop the per-analysis plot-styling snapshots (but not DEFAULT_STYLE_KEY),
  for use when the model changes and the old panel styling no longer applies.
  Delete the three analysis keys individually rather than ClearAllSettings, so
  the pristine DEFAULT_STYLE_KEY baseline survives a model change and can still
  style a freshly-visited panel. DeleteSettings is a no-op for absent keys. }
procedure TfrmMain.ClearPanelStyleSnapshots;
begin
  Plot.DeleteSettings('TimeCourse');
  Plot.DeleteSettings('ParameterScan');
  Plot.DeleteSettings('SteadyState');
end;

{ Copy styling the user edited on the plotted data series back into the stored
  clones, so the dropdown's clear-and-re-add (cboLoadedFilenameChange) — and a
  panel switch — re-shows a dataset with the marker colour/shape the user last
  gave it rather than the as-loaded defaults. Matched by SeriesId; only styling
  is copied, the stored data points are left as they were. Only the showing
  panel's datasets can be on the plot, so only its catalogue is searched. }
procedure TfrmMain.SyncOverlayStyleToStorage;
var
  I, J, K: Integer;
  PS: TPlotSeries;
  Style: TJSONObject;
  Files: TList<TLoadDataFile>;
begin
  Files := CurrentPanelData.Files;

  for I := 0 to Plot.Series.Count - 1 do
  begin
    PS := Plot.Series[I];
    if PS.SeriesKind <> skData then Continue;

    for J := 0 to Files.Count - 1 do
      for K := 0 to Files[J].Series.Count - 1 do
        if Files[J].Series[K].SeriesId = PS.SeriesId then
        begin
          Style := PS.SaveStyleToJson;
          try
            Files[J].Series[K].LoadStyleFromJson(Style);
          finally
            Style.Free;
          end;
        end;
  end;
end;

{ Remember what the outgoing panel had on screen: the user's styling edits, the
  exact set of overlays drawn (several, with "overlay data" on) and the dropdown
  selection. Call while that panel is still the active one — the state is stored
  under ActiveAnalysisKey. }
procedure TfrmMain.CapturePanelDataState;
var
  Panel: TPanelDataFiles;
  I: Integer;
begin
  SyncOverlayStyleToStorage;

  Panel := CurrentPanelData;
  Panel.DisplayedIds.Clear;
  for I := 0 to Plot.Series.Count - 1 do
    if Plot.Series[I].SeriesKind = skData then
      Panel.DisplayedIds.Add(Plot.Series[I].SeriesId);

  Panel.SelectedIndex := cboLoadedFilename.ItemIndex;
end;

{ Put the incoming panel's overlays back: refill the filename dropdown from its
  catalogue and re-draw the datasets it was showing. Call after FActiveFrame has
  been switched. A panel with no loaded data ends up with a clear plot and an
  empty dropdown, which is the point — data does not leak between panels.

  The dropdown is refilled from Files in order, so SelectedIndex indexes both. }
procedure TfrmMain.RestorePanelDataState;
var
  Panel: TPanelDataFiles;
  I, J: Integer;
begin
  Plot.ClearSeriesKind(skData);
  Panel := CurrentPanelData;

  { Repopulating fires OnChange, which would re-plot and overwrite the stored
    selection — suppress it and set the plot up here instead. }
  FireEvent := False;
  try
    cboLoadedFilename.Clear;
    for I := 0 to Panel.Files.Count - 1 do
      cboLoadedFilename.Items.Add(Panel.Files[I].FileName);

    if (Panel.SelectedIndex >= 0) and
       (Panel.SelectedIndex < cboLoadedFilename.Items.Count) then
      cboLoadedFilename.ItemIndex := Panel.SelectedIndex
    else
      cboLoadedFilename.ItemIndex := -1;
  finally
    FireEvent := True;
  end;

  if cboLoadedFilename.ItemIndex >= 0 then
    lblParameterName.Text := Panel.Files[cboLoadedFilename.ItemIndex].ParameterName
  else
    lblParameterName.Text := 'None';

  for I := 0 to Panel.Files.Count - 1 do
    for J := 0 to Panel.Files[I].Series.Count - 1 do
      if Panel.DisplayedIds.IndexOf(Panel.Files[I].Series[J].SeriesId) >= 0 then
        Plot.AddSeries(Panel.Files[I].Series[J].Clone);
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

  FFrameMetadata := TFrameMetadata.Create(Self);
  FFrameMetadata.Parent  := LayoutContainer;
  FFrameMetadata.Align   := TAlignLayout.Client;
  FFrameMetadata.Visible := False;
  FFrameMetadata.SetContext(Self);
end;

{ ── metadata export ──────────────────────────────────────────────────────── }

{ The three export items live in the .fmx, created in the designer, which
  is why they sit above Quit where they belong — appending them in code
  put them after it. Only their enabled state is managed here. }

procedure TfrmMain.UpdateExportMenuState;
var
  Ready: Boolean;
begin
  { These exports describe the experiments the FILE defines, so without a
    metadata block there is nothing to export. Disabled rather than
    failing on click — an item that cannot work should look like it. }
  Ready := (FMeta <> nil) and FMeta.HasMetadata and
           (Length(FMeta.Runnable) > 0);

  mnuExportTelluriumScript.Enabled := Ready;
  mnuExportSEDMLFile.Enabled       := Ready;
  mnuExportCOMBINEArchve.Enabled   := Ready;
end;

function TfrmMain.AskExportPath(const ATitle, AFilter, ADefaultExt: string;
  out APath: string): Boolean;
var
  Dlg: TSaveDialog;
begin
  APath := '';
  Dlg := TSaveDialog.Create(nil);
  try
    Dlg.Title      := ATitle;
    Dlg.Filter     := AFilter;
    Dlg.DefaultExt := ADefaultExt;
    Dlg.Options    := Dlg.Options + [TOpenOption.ofOverwritePrompt];

    { Default to the model's own name and folder — an export almost always
      belongs beside the model it came from. }
    if FCurrentFilePath <> '' then
    begin
      Dlg.InitialDir := ExtractFilePath(FCurrentFilePath);
      Dlg.FileName   := TPath.GetFileNameWithoutExtension(FCurrentFilePath) +
                        '.' + ADefaultExt;
    end
    else
      Dlg.FileName := 'experiment.' + ADefaultExt;

    Result := Dlg.Execute;
    if Result then
      APath := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

function TfrmMain.BuildSbml(out ASbml: string; out AError: string): Boolean;
var
  Info: uCommonTypes.TModelErrorState;
begin
  ASbml  := '';
  AError := '';
  Info := getSBMLFromAntimony(GetAntimonyText);
  Result := Info.ok;
  if Result then
    ASbml := Info.sbmlStr
  else
    AError := Info.errMsg;
end;

procedure TfrmMain.ShowExportReport(const AWhat, APath: string;
  ADiags: TDiagnosticList);
var
  I: Integer;
  SB: TStringBuilder;
begin
  { A clean export says so briefly. A lossy one has to say what was lost,
    because the user cannot see it from the file and will not find out
    until something downstream is missing. }
  if (ADiags = nil) or (ADiags.Count = 0) then
  begin
    ShowMessage(AWhat + ' written to' + sLineBreak + APath);
    Exit;
  end;

  SB := TStringBuilder.Create;
  try
    SB.AppendLine(AWhat + ' written to');
    SB.AppendLine(APath);
    SB.AppendLine;
    SB.AppendLine(Format('%d item(s) could not be represented exactly:',
      [ADiags.Count]));
    SB.AppendLine;
    for I := 0 to ADiags.Count - 1 do
      SB.AppendLine('  ' + ADiags.Format(I));
    ShowMessage(SB.ToString);
  finally
    SB.Free;
  end;
end;

procedure TfrmMain.ExportTelluriumScript;
var
  Exporter: TPythonExporter;
  Path:     string;
  Symbols:  ISymbolProvider;
begin
  if not AskExportPath('Export Tellurium script',
                       'Python script (*.py)|*.py|All files (*.*)|*.*',
                       'py', Path) then Exit;

  { Symbols are optional here — with them the column selections match the
    specification's defaults, without them they narrow to what the file
    references and the report says so. Loading is not computing, so this
    does not breach the rule that results need an explicit click. }
  try
    if FSession.EnsureLoaded then
      Symbols := TRoadRunnerSymbolProvider.Create(FSession);
  except
    on E: Exception do
      Symbols := nil;   { export the script anyway; the report will note it }
  end;

  Exporter := TPythonExporter.Create(FMeta, GetAntimonyText, Symbols);
  try
    try
      Exporter.ExportToFile(Path);
    except
      on E: Exception do
      begin
        ShowMessage('Export failed: ' + E.Message);
        Exit;
      end;
    end;
    ShowExportReport('Tellurium script', Path, Exporter.Diagnostics);
  finally
    Exporter.Free;
  end;
end;

{ The default namespace declared on the <sbml> root of ASbml, which is what
  the SED-ML's XPath targets have to be resolved against. Empty when it
  cannot be found, in which case the exporter keeps its own default rather
  than writing something wrong. Deliberately a text scan: the level and
  version of what libantimony emits is not ours to assume, and pulling in
  an XML parser to read one attribute would be a poor trade. }
function TfrmMain.SbmlRootNamespace(const ASbml: string): string;
var
  Start, Stop, Tag: Integer;
  Head: string;
begin
  Result := '';

  Tag := Pos('<sbml', ASbml);
  if Tag = 0 then Exit;

  Stop := Pos('>', ASbml, Tag);
  if Stop = 0 then Exit;
  Head := Copy(ASbml, Tag, Stop - Tag + 1);

  { xmlns=, not xmlns:something= — the default namespace is the one the
    unprefixed element names are in. }
  Start := Pos('xmlns="', Head);
  if Start = 0 then Exit;
  Inc(Start, Length('xmlns="'));

  Stop := Pos('"', Head, Start);
  if Stop = 0 then Exit;

  Result := Copy(Head, Start, Stop - Start);
end;

procedure TfrmMain.ExportSedML;
var
  Exporter: TSedMLExporter;
  Path, Err: string;
  Sbml:     string;
  Symbols:  ISymbolProvider;
begin
  { SED-ML identifies quantities by XPath into SBML, so it cannot be
    written without a model that loads. Fail early and say why. }
  try
    if not FSession.EnsureLoaded then
    begin
      ShowMessage('The model must load before it can be exported: ' +
                  FSession.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('The model must load before it can be exported: ' +
                  E.Message);
      Exit;
    end;
  end;

  if not BuildSbml(Sbml, Err) then
  begin
    ShowMessage('Could not convert the model to SBML: ' + Err);
    Exit;
  end;

  if not AskExportPath('Export SED-ML',
                       'SED-ML (*.sedml)|*.sedml|XML (*.xml)|*.xml',
                       'sedml', Path) then Exit;

  Symbols  := TRoadRunnerSymbolProvider.Create(FSession);
  Exporter := TSedMLExporter.Create(FMeta, Symbols);
  try
    { So the sbml: prefix the targets use is declared as the namespace this
      model is actually in. }
    if SbmlRootNamespace(Sbml) <> '' then
      Exporter.SbmlNamespace := SbmlRootNamespace(Sbml);
    try
      Exporter.ExportToFile(Path);
    except
      on E: Exception do
      begin
        ShowMessage('Export failed: ' + E.Message);
        Exit;
      end;
    end;
    ShowExportReport('SED-ML', Path, Exporter.Diagnostics);
  finally
    Exporter.Free;
  end;
end;

procedure TfrmMain.mnuExportSEDMLFileClick(Sender: TObject);
begin
  ExportSedML;
end;

procedure TfrmMain.mnuExportTelluriumScriptClick(Sender: TObject);
begin
  ExportTelluriumScript;
end;

procedure TfrmMain.mnuExportCOMBINEArchveClick(Sender: TObject);
begin
  ExportOmexArchive;
end;

procedure TfrmMain.ExportOmexArchive;
var
  Exporter: TSedMLExporter;
  Path, Err: string;
  Sbml:     string;
  Symbols:  ISymbolProvider;
begin
  try
    if not FSession.EnsureLoaded then
    begin
      ShowMessage('The model must load before it can be exported: ' +
                  FSession.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('The model must load before it can be exported: ' +
                  E.Message);
      Exit;
    end;
  end;

  if not BuildSbml(Sbml, Err) then
  begin
    ShowMessage('Could not convert the model to SBML: ' + Err);
    Exit;
  end;

  if not AskExportPath('Export COMBINE archive',
                       'COMBINE archive (*.omex)|*.omex|All files (*.*)|*.*',
                       'omex', Path) then Exit;

  Symbols  := TRoadRunnerSymbolProvider.Create(FSession);
  Exporter := TSedMLExporter.Create(FMeta, Symbols);
  try
    if SbmlRootNamespace(Sbml) <> '' then
      Exporter.SbmlNamespace := SbmlRootNamespace(Sbml);
    try
      { The archive carries the SBML, the SED-ML, the original Antimony
        with its metadata block, and Dublin Core from @meta. }
      Exporter.ExportToOmex(Path, Sbml);
    except
      on E: Exception do
      begin
        ShowMessage('Export failed: ' + E.Message);
        Exit;
      end;
    end;
    ShowExportReport('COMBINE archive', Path, Exporter.Diagnostics);
  finally
    Exporter.Free;
  end;
end;

{ ── in-app help ──────────────────────────────────────────────────────────── }

type
  THelpDoc = record
    Id:       string;   { what ShowHelp names, and what deep links use }
    FileName: string;
    Title:    string;
  end;

const
  { Adding a document is an entry here plus a button; nothing else in the
    help code knows how many there are. }
  HELP_DOCS: array[0..1] of THelpDoc = (
    (Id: 'antimony'; FileName: 'ANTIMONY_MANUAL.md'; Title: 'The Antimony language'),
    (Id: 'metadata'; FileName: 'METADATA_MANUAL.md'; Title: 'Simulation metadata')
  );

function TfrmMain.FindHelpFile(const AFileName: string;
  out APath: string): Boolean;
var
  ExeDir: string;
  Tried:  TStringList;
  Candidate: string;
  I: Integer;
begin
  Result := False;
  APath  := '';
  ExeDir := ExtractFilePath(ParamStr(0));

  Tried := TStringList.Create;
  try
    { Beside the executable and in its Help folder are where a deployed
      build looks. The two relative paths let a development build read the
      documents straight out of the repository, so the manual can be
      edited and re-read without copying anything. }
    Tried.Add(TPath.Combine(ExeDir, AFileName));
    Tried.Add(TPath.Combine(TPath.Combine(ExeDir, 'Help'), AFileName));
    Tried.Add(TPath.Combine(TPath.Combine(ExeDir, '..\..\Help'), AFileName));
    Tried.Add(TPath.Combine(TPath.Combine(ExeDir, '..\..'), AFileName));

    for I := 0 to Tried.Count - 1 do
    begin
      Candidate := Tried[I];
      if TFile.Exists(Candidate) then
      begin
        APath := Candidate;
        Exit(True);
      end;
    end;

    { Report where we looked, so a missing document is something the user
      can fix rather than a blank panel. }
    APath := Tried.Text;
  finally
    Tried.Free;
  end;
end;

procedure TfrmMain.ShowHelpDoc(const AId: string; const AAnchor: string);
var
  I, Found: Integer;
  Path:     string;
begin
  Found := -1;
  for I := Low(HELP_DOCS) to High(HELP_DOCS) do
    if SameText(HELP_DOCS[I].Id, AId) then
    begin
      Found := I;
      Break;
    end;
  if Found < 0 then Exit;

  { Only reload when the document actually changes: re-reading would throw
    away the reader's position for no reason. }
  if not SameText(FHelpDocId, AId) then
  begin
    if FindHelpFile(HELP_DOCS[Found].FileName, Path) then
      HelpViewer.LoadFromFile(Path)
    else
      { Rendered as markdown, so it reads as a page rather than an error
        box, and names both the file and the places searched. }
      HelpViewer.MarkdownText :=
        '# ' + HELP_DOCS[Found].Title + sLineBreak + sLineBreak +
        'This help document could not be found.' + sLineBreak + sLineBreak +
        'Looking for **' + HELP_DOCS[Found].FileName + '** in:' +
        sLineBreak + sLineBreak +
        '```' + sLineBreak + Path + '```' + sLineBreak + sLineBreak +
        'Help documents are ordinary markdown files. Putting one at any ' +
        'of those locations will make it appear here, so they can be ' +
        'edited, extended or replaced to suit.';

    FHelpDocId   := AId;
    FHelpScrollY := 0;
    { LoadFromFile leaves the viewer light or dark as it was; re-apply so
      a document loaded after the theme was switched matches. }
    if FHelpDark then
      HelpViewer.ApplyTheme(rtDark);
  end;

  TabControl1.ActiveTab := tbHelp;

  if AAnchor <> '' then
    HelpViewer.ScrollToAnchor(AAnchor)
  else
    HelpViewer.SetScrollPos(FHelpScrollY);
end;

procedure TfrmMain.ShowHelp(const ATopic: string);
var
  P: Integer;
begin
  { 'metadata' or 'metadata#parameter-scans'. }
  P := Pos('#', ATopic);
  if P > 0 then
    ShowHelpDoc(Copy(ATopic, 1, P - 1), Copy(ATopic, P + 1, MaxInt))
  else
    ShowHelpDoc(ATopic);
end;

procedure TfrmMain.HelpScrolled(Sender: TObject);
begin
  { Track the position continuously rather than trying to catch the moment
    the tab is left — TabControl1.OnChange fires after the switch and does
    not say what was left behind. }
  if TabControl1.ActiveTab = tbHelp then
    FHelpScrollY := HelpViewer.ScrollY;
end;

procedure TfrmMain.HelpLinkClicked(Sender: TObject; const AUrl: string);
var
  I: Integer;
  Target: string;
begin
  { A bare '#anchor' is a jump within the current document. }
  if AUrl.StartsWith('#') then
  begin
    HelpViewer.ScrollToAnchor(AUrl);
    Exit;
  end;

  { A link to another help document, by file name or by id, keeps the
    reader inside the app. Without this every cross-reference would open
    a browser on a file:// URL, or nothing at all. }
  Target := AUrl;
  for I := Low(HELP_DOCS) to High(HELP_DOCS) do
    if SameText(Target, HELP_DOCS[I].FileName) or
       SameText(Target, HELP_DOCS[I].Id) then
    begin
      ShowHelpDoc(HELP_DOCS[I].Id);
      Exit;
    end;

  { Anything else is a real URL: hand it to the browser. }
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(AUrl), '', '', SW_SHOWNORMAL);
  {$ENDIF}
  {$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(AUrl)));
  {$ENDIF}
end;

{ ── simulation metadata ──────────────────────────────────────────────────── }

procedure TfrmMain.CreateMetaBar;

  function MakeButton(const AText: string; AWidth: Single;
                      AHandler: TNotifyEvent): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Parent    := FMetaBar;
    Result.Align     := TAlignLayout.Right;
    Result.Margins.Rect := RectF(4, 5, 4, 5);
    Result.Width     := AWidth;
    Result.Text      := AText;
    Result.OnClick   := AHandler;
  end;

begin
  { Top of the plot area rather than of an analysis panel: the notice
    concerns the model, not whichever panel happens to be showing, and the
    plot area is the one region visible in every mode. }
  FMetaBar := TLayout.Create(Self);
  FMetaBar.Parent  := Layout8;
  FMetaBar.Align   := TAlignLayout.Top;
  FMetaBar.Height  := 34;
  FMetaBar.Visible := False;

  FMetaBarClose := MakeButton('Dismiss', 80, MetaBarCloseClick);
  FMetaBarView  := MakeButton('View', 70, MetaBarViewClick);
  FMetaBarWrite := MakeButton('Write', 90, MetaBarWriteClick);
  FMetaBarWrite.Visible := False;
  FMetaBarApply := MakeButton('Reload settings', 130, MetaBarApplyClick);

  FMetaBarLabel := TLabel.Create(Self);
  FMetaBarLabel.Parent  := FMetaBar;
  FMetaBarLabel.Align   := TAlignLayout.Client;
  FMetaBarLabel.Margins.Rect := RectF(8, 0, 8, 0);
  FMetaBarLabel.VertTextAlign := TTextAlign.Center;
end;

{ ── BioModels search ─────────────────────────────────────────────────────
  Type in the box at the top right; matching models drop down beneath it;
  clicking one downloads its SBML, converts it to Antimony and loads it the
  same way Import SBML does.

  Both the search and the download run off the UI thread — the first search
  of a session downloads the whole cache document, which is far too slow to
  do between keystrokes on the main thread. }

procedure TfrmMain.CreateBioModelsSearch;
var
  Host: TLayout;
begin
  FBioCache := TBiomodelsCache.Create;

  Host := TLayout.Create(Self);
  Host.Parent := Layout4;
  Host.Align  := TAlignLayout.Right;
  Host.Width  := 320;

  FBioSearchEdit := TEdit.Create(Self);
  FBioSearchEdit.Parent      := Host;
  FBioSearchEdit.Align       := TAlignLayout.Client;
  FBioSearchEdit.Margins.Rect := RectF(6, 10, 10, 10);
  FBioSearchEdit.TextPrompt  := 'Search BioModels';
  FBioSearchEdit.Hint        := 'Type at least three characters; ' +
                                'click a result to load that model';
  FBioSearchEdit.ShowHint    := True;
  FBioSearchEdit.OnChangeTracking := BioSearchChanged;
  FBioSearchEdit.OnKeyDown        := BioSearchKeyDown;

  { A child of the FORM, not of the 50-pixel strip, which would clip it to
    its own height. }
  FBioSearchList := TListBox.Create(Self);
  FBioSearchList.Parent      := Self;
  FBioSearchList.Visible     := False;
  FBioSearchList.Height      := 300;
  FBioSearchList.OnItemClick := BioResultClick;

  { Debounce. Searching on every keystroke would queue a search per
    character typed; 350ms is long enough that ordinary typing produces
    one search at the end of a word. }
  FBioSearchTimer := TTimer.Create(Self);
  FBioSearchTimer.Enabled  := False;
  FBioSearchTimer.Interval := 350;
  FBioSearchTimer.OnTimer  := BioSearchTimerTick;
end;

procedure TfrmMain.BioSearchChanged(Sender: TObject);
begin
  if FBioSuppressSearch then Exit;

  FBioSearchTimer.Enabled := False;

  { Below three characters the term matches most of the repository, which
    is neither useful nor quick to render. }
  if Length(Trim(FBioSearchEdit.Text)) < 3 then
  begin
    HideBioList;
    Exit;
  end;

  FBioSearchTimer.Enabled := True;
end;

procedure TfrmMain.BioSearchKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkEscape then
  begin
    HideBioList;
    Key := 0;
  end
  else if Key = vkReturn then
  begin
    { Don't make the user wait out the debounce. }
    FBioSearchTimer.Enabled := False;
    StartBioSearch;
    Key := 0;
  end;
end;

procedure TfrmMain.BioSearchTimerTick(Sender: TObject);
begin
  FBioSearchTimer.Enabled := False;
  StartBioSearch;
end;

procedure TfrmMain.StartBioSearch;
var
  Term: string;
begin
  Term := Trim(FBioSearchEdit.Text);
  if Term = '' then
  begin
    HideBioList;
    Exit;
  end;

  FBioSearchTerm := Term;

  { One search at a time, with at most one queued behind it. The slow part
    is the cache download the first search does, and a newer term needs it
    just the same — so wait for it rather than starting a second. }
  if FBioSearchRunning then
  begin
    FBioSearchAgain := True;
    Exit;
  end;

  FBioSearchRunning := True;
  ShowBioMessage('Searching...');

  TTask.Run(
    procedure
    var
      Res:    TBiomodelArray;
      Err:    string;
      Wanted: string;
    begin
      Wanted := Term;
      Err    := '';
      try
        Res := FBioCache.Search(Wanted, 40);
      except
        on E: Exception do
        begin
          Res := nil;
          Err := E.Message;
        end;
      end;

      TThread.Queue(nil,
        procedure
        begin
          FBioSearchRunning := False;

          if Err <> '' then
            ShowBioMessage('Search failed: ' + Err)
          else if Wanted = FBioSearchTerm then
            { Stale otherwise: the user typed on while this was running,
              and the queued search below is about to replace it. }
            ShowBioResults(Res, Wanted);

          if FBioSearchAgain then
          begin
            FBioSearchAgain := False;
            StartBioSearch;
          end;
        end);
    end);
end;

procedure TfrmMain.ShowBioResults(const AResults: TBiomodelArray;
  const ATerm: string);
var
  I:    Integer;
  Item: TListBoxItem;
  Desc: string;
begin
  if Length(AResults) = 0 then
  begin
    ShowBioMessage('No models match "' + ATerm + '"');
    Exit;
  end;

  FBioSearchList.BeginUpdate;
  try
    FBioSearchList.Clear;
    for I := 0 to High(AResults) do
    begin
      Desc := AResults[I].Title;
      if Desc = '' then
        Desc := AResults[I].Name;

      Item := TListBoxItem.Create(FBioSearchList);
      Item.Parent := FBioSearchList;
      Item.Text   := AResults[I].ID + '   ' + Desc;
      { The id is what GetModel needs; the row text is for reading. }
      Item.TagString := AResults[I].ID;
      Item.Hint      := Trim(AResults[I].Authors + '  ' + AResults[I].Journal +
                             '  ' + AResults[I].Date);
      Item.ShowHint  := Item.Hint <> '';
    end;
  finally
    FBioSearchList.EndUpdate;
  end;

  PositionBioList;
end;

{ A one-row list saying what is going on. The row carries no id, so
  clicking it does nothing. }
procedure TfrmMain.ShowBioMessage(const AText: string);
var
  Item: TListBoxItem;
begin
  FBioSearchList.BeginUpdate;
  try
    FBioSearchList.Clear;
    Item := TListBoxItem.Create(FBioSearchList);
    Item.Parent    := FBioSearchList;
    Item.Text      := AText;
    Item.TagString := '';
    Item.Enabled   := False;
  finally
    FBioSearchList.EndUpdate;
  end;

  PositionBioList;
end;

procedure TfrmMain.PositionBioList;
var
  P: TPointF;
  W: Single;
begin
  W := 520;
  if W > ClientWidth - 16 then
    W := ClientWidth - 16;

  { Hang the list from the bottom-right corner of the box, so it grows
    leftwards and never runs off the right edge of the window. }
  P := FBioSearchEdit.LocalToAbsolute(
         PointF(FBioSearchEdit.Width, FBioSearchEdit.Height + 2));

  FBioSearchList.Width      := W;
  FBioSearchList.Position.X := Max(8, P.X - W);
  FBioSearchList.Position.Y := P.Y;
  FBioSearchList.Visible    := True;
  FBioSearchList.BringToFront;
end;

procedure TfrmMain.HideBioList;
begin
  if FBioSearchList = nil then Exit;
  FBioSearchList.Visible := False;
  FBioSearchList.Clear;
end;

procedure TfrmMain.BioResultClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  ModelID: string;
  Title:   string;
begin
  if Item = nil then Exit;

  ModelID := Item.TagString;
  if ModelID = '' then Exit;   { a status row, not a result }

  { One download at a time; the button-equivalent here is the row itself,
    and a second click while the first is in flight would race it. }
  if FBioFetching then Exit;

  Title := Item.Text;
  HideBioList;

  FBioFetching := True;
  FBioSearchEdit.Enabled := False;
  FBioSuppressSearch := True;
  try
    FBioSearchEdit.Text := 'Loading ' + ModelID + '...';
  finally
    FBioSuppressSearch := False;
  end;

  TTask.Run(
    procedure
    var
      SBML: string;
      Err:  string;
      Fetch: TBiomodelsCache;
    begin
      SBML := '';
      Err  := '';
      { Its own cache object: the shared one may be mid-search on the
        search task, and they would be sharing an HTTP client. }
      Fetch := TBiomodelsCache.Create;
      try
        try
          SBML := Fetch.GetModel(ModelID);
        except
          on E: Exception do
            Err := E.Message;
        end;
      finally
        Fetch.Free;
      end;

      TThread.Queue(nil,
        procedure
        begin
          FBioFetching := False;
          FBioSearchEdit.Enabled := True;
          FBioSuppressSearch := True;
          try
            FBioSearchEdit.Text := '';
          finally
            FBioSuppressSearch := False;
          end;

          if Err <> '' then
          begin
            ShowMessage('Could not download ' + ModelID + ': ' + Err);
            Exit;
          end;

          LoadBiomodelSBML(SBML, ModelID, Title);
        end);
    end);
end;

{ The same route Import SBML takes, so a downloaded model arrives in the
  same state as an opened one: nothing computed, no stale plot, and the
  metadata block (SBML has none, but the conversion may produce comments)
  parsed on the way in. }
procedure TfrmMain.LoadBiomodelSBML(const ASBML, AModelID, ATitle: string);
var
  Ant: string;
begin
  if Trim(ASBML) = '' then
  begin
    ShowMessage(AModelID + ' returned an empty document.');
    Exit;
  end;

  Ant := uAntimonyAPI.getAntimonyFromSBML(ASBML);
  if Trim(Ant) = '' then
  begin
    ShowMessage('Could not convert ' + AModelID + ' to Antimony. ' +
                'The model may use SBML features Antimony cannot express.');
    Exit;
  end;

  FSession.Unload;
  ClearPlotAndLoadedData;
  moAntimony.SetText(Ant);

  { Downloaded, not opened from disk: no path, so Save prompts for one. }
  FCurrentFileName := AModelID + '.ant';
  FCurrentFilePath := '';
  Caption := 'Iridium II: ' + FCurrentFileName;
  FSession.ClearDirty;
  FIsModifiedSinceLastSave := False;

  ParseMetadata(True);
end;

procedure TfrmMain.CreateMetaMenu;
var
  Top:  TMenuItem;
  Item: TMenuItem;
begin
  { Built in code so the .fmx — which is multi-megabyte — does not have to
    be edited for it. }
  Top := TMenuItem.Create(Self);
  MainMenu1.InsertObject(2, Top);
  Top.Text   := 'Metadata';
  { Opening the menu re-reads the block from the editor, so the run list
    describes what is typed there now rather than what was parsed at the
    last reload. Without this, editing a block and going straight to the
    menu would offer the previous set of labels. }
  Top.OnClick := MetaMenuOpening;

  Item := TMenuItem.Create(Self);
  Item.Parent  := Top;
  Item.Text    := 'Experiments and Diagnostics...';
  Item.OnClick := MetaBarViewClick;

  { Apply an experiment to its own panel and compute it. The one place in
    Iridium where metadata causes a computation — which is why it is
    named for what it does, and why nothing else in the feature does it. }
  FMnuMetaRun := TMenuItem.Create(Self);
  FMnuMetaRun.Parent := Top;
  FMnuMetaRun.Text   := 'Run Experiment';

  Item := TMenuItem.Create(Self);
  Item.Parent := Top;
  Item.Text   := '-';

  { The opt-out. Off still parses, still reports and still fills the
    selectors — it only stops the block writing into the user's controls
    when a model is opened. Deliberately global: a per-file "ignore this
    one" would have nowhere to live except the file itself, and writing a
    'please ignore me' marker into someone's model is worse than the
    problem it solves. }
  FMnuMetaApply := TMenuItem.Create(Self);
  FMnuMetaApply.Parent    := Top;
  FMnuMetaApply.Text      := 'Apply Settings When Opening a Model';
  FMnuMetaApply.IsChecked := FApplyMetaOnOpen;
  FMnuMetaApply.OnClick   := MetaMenuApplyClick;
end;

procedure TfrmMain.MetaMenuApplyClick(Sender: TObject);
begin
  FApplyMetaOnOpen := not FApplyMetaOnOpen;
  FMnuMetaApply.IsChecked := FApplyMetaOnOpen;
end;

procedure TfrmMain.MetaBarViewClick(Sender: TObject);
begin
  ShowAnalysisFrame(FFrameMetadata);
end;

procedure TfrmMain.MetaMenuOpening(Sender: TObject);
begin
  { A parse, never an apply: opening a menu must not write into the
    user's controls. This refreshes the experiment set and diagnostics
    from the current editor text, which is what the run list is built
    from. }
  ParseMetadata(False);   { rebuilds the run list on its way out }
end;

{ One row per experiment, labelled as the selector labels it. Unusable
  ones are listed and disabled, carrying their reason, for the same
  reason the selector lists them (C5): the user is looking here for the
  thing that is missing. }
procedure TfrmMain.RebuildRunExperimentMenu;
var
  I:    Integer;
  Exp:  TMetaExperiment;
  Item: TMenuItem;
begin
  if FMnuMetaRun = nil then Exit;

  while FMnuMetaRun.ItemsCount > 0 do
    FMnuMetaRun.Items[0].Free;

  if (FExperiments = nil) or (FExperiments.Count = 0) then
  begin
    { An always-empty submenu is a dead end. Say why it is empty. }
    Item := TMenuItem.Create(Self);
    Item.Parent  := FMnuMetaRun;
    Item.Text    := 'No experiments in this model';
    Item.Enabled := False;
    Exit;
  end;

  for I := 0 to FExperiments.Count - 1 do
  begin
    Exp := FExperiments[I];
    Item := TMenuItem.Create(Self);
    Item.Parent    := FMnuMetaRun;
    Item.Text      := Exp.DisplayText;
    { By label, never by index: the set is rebuilt wholesale on every
      re-parse, and this menu outlives the objects it was built from. }
    Item.TagString := Exp.LabelText;
    Item.Enabled   := Exp.Usable;
    Item.OnClick   := MetaRunExperimentClick;
  end;
end;

procedure TfrmMain.MetaRunExperimentClick(Sender: TObject);
var
  ALabel: string;
  Exp:    TMetaExperiment;
begin
  if not (Sender is TMenuItem) then Exit;
  ALabel := TMenuItem(Sender).TagString;

  if FExperiments = nil then Exit;
  Exp := FExperiments.FindByLabel(ALabel);
  if Exp = nil then
  begin
    { The block was edited between the menu being built and this click. }
    ShowMessage('"' + ALabel + '" is no longer defined in this model.');
    Exit;
  end;
  if not Exp.Usable then
  begin
    ShowMessage('This experiment cannot be used: ' + Exp.Reason);
    Exit;
  end;

  { Switch to the panel that owns the task kind first, so the run happens
    where the user can watch it, and so the frame's compute plots onto the
    panel's own styling. }
  case Exp.Kind of
    mekTimeCourse:
      begin
        ShowAnalysisFrame(FFrameTimeCourse);
        FFrameTimeCourse.RunExperiment(ALabel);
      end;
    mekScan:
      begin
        ShowAnalysisFrame(FFrameParameterScan);
        FFrameParameterScan.RunExperiment(ALabel);
      end;
    mekSteadyState:
      begin
        ShowAnalysisFrame(FFrameSteadyState);
        FFrameSteadyState.RunExperiment(ALabel);
      end;
  end;
end;

{ ── @output: writing the data files a block describes ────────────────────── }

procedure TfrmMain.OutputStateChanged;
var
  Provider: IMetaOutputProvider;
  Exp:      TMetaExperiment;
  Outs:     TArray<TOutputCommand>;
begin
  if FMetaBarWrite = nil then Exit;

  Exp := nil;
  if Supports(FActiveFrame, IMetaOutputProvider, Provider) then
    Exp := Provider.GetOutputExperiment;

  if Exp = nil then
  begin
    FMetaBarWrite.Visible := False;
    Exit;
  end;

  { Say what pressing it will do, so the user need not go back to the
    block to find out which file they are about to write. }
  Outs := Exp.Outputs;
  if Length(Outs) = 1 then
    if Outs[0].FileName <> '' then
      FMetaBarWrite.Text := 'Write ' + Outs[0].FileName
    else
      FMetaBarWrite.Text := 'Show data'
  else
    FMetaBarWrite.Text := Format('Write %d files', [Length(Outs)]);

  FMetaBarLabel.Text := 'Experiment ' + Exp.LabelText +
                        ' defines output for this result.';
  FMetaBarWrite.Visible := True;
  FMetaBarApply.Visible := False;
  { Results have just appeared and the block has something to do with
    them — worth re-showing the bar even if it was dismissed earlier,
    since this is a new and actionable state rather than the same notice
    repeated. }
  FMetaBar.Visible := True;
end;

procedure TfrmMain.MetaBarWriteClick(Sender: TObject);
var
  Provider: IMetaOutputProvider;
  Exp:      TMetaExperiment;
  Data:     T2DMatrix;
  Cmd:      TOutputCommand;
  Text, Err, Path: string;
  Written:  Integer;
  ToPanel:  string;
begin
  if not Supports(FActiveFrame, IMetaOutputProvider, Provider) then Exit;

  Exp  := Provider.GetOutputExperiment;
  Data := Provider.GetOutputData;
  if (Exp = nil) or (Data = nil) or (FMeta = nil) then Exit;

  Written := 0;
  ToPanel := '';

  for Cmd in Exp.Outputs do
  begin
    if (not Cmd.Supported) or (not Cmd.Valid) then Continue;

    { ProvenanceLines comes from the library, so this file and the one the
      exported Python script writes are byte-identical — the cheapest
      check that the two paths agree. }
    if not BuildOutputText(Cmd, Data, FMeta.ProvenanceLines(Cmd),
                           Text, Err) then
    begin
      ShowMessage('Cannot write ' + Cmd.DisplayName + ': ' + Err);
      Continue;
    end;

    { No 'file' key means the tool's own output panel (spec 10). }
    if Cmd.FileName = '' then
    begin
      ToPanel := ToPanel + Text;
      Continue;
    end;

    { Resolved against the directory holding the model, never the process
      working directory; the validator has already rejected absolute
      paths and '..' segments. }
    Path := Cmd.ResolvedFilePath(FMeta.ModelPath);

    if TFile.Exists(Path) then
      if MessageDlg('Overwrite ' + Path + '?', TMsgDlgType.mtConfirmation,
                    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
        Continue;

    try
      TFile.WriteAllText(Path, Text);
      Inc(Written);
    except
      on E: Exception do
        ShowMessage('Could not write ' + Path + ': ' + E.Message);
    end;
  end;

  if ToPanel <> '' then
    CopyTextToTextWindow(ToPanel);

  if Written > 0 then
    ShowMessage(Format('%d file(s) written to %s',
      [Written, ExtractFilePath(FMeta.ModelPath)]));
end;

procedure TfrmMain.ShowMetaBar(const AText: string; AShowApply: Boolean);
begin
  if FMetaBar = nil then Exit;
  FMetaBarLabel.Text   := AText;
  FMetaBarApply.Visible := AShowApply;
  FMetaBar.Visible     := True;
end;

procedure TfrmMain.HideMetaBar;
begin
  if FMetaBar <> nil then
    FMetaBar.Visible := False;
end;

procedure TfrmMain.MetaBarCloseClick(Sender: TObject);
begin
  HideMetaBar;
end;

procedure TfrmMain.MetaBarApplyClick(Sender: TObject);
begin
  { The user asking for the edited block, explicitly. This is the only way
    metadata reaches the controls outside of opening a model. }
  ApplyMetadataToPanels;
  HideMetaBar;
end;

{ What the panels were populated from, so an edit to the block can be
  detected on the next reload. Built from the parsed objects rather than
  the raw text so that reformatting or a comment change is not mistaken
  for a change of experiment. }
function TfrmMain.MetadataSignature: string;
var
  Cmd: TMetaCommandBase;
  SB:  TStringBuilder;
begin
  if FMeta = nil then Exit('');
  SB := TStringBuilder.Create;
  try
    for Cmd in FMeta.Commands do
      SB.Append(Cmd.Name).Append('|')
        .Append(Cmd.AutoLabel).Append('|')
        .Append(Cmd.SettingsSummary).Append(#10);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TfrmMain.ParseMetadata(AApplyToPanels: Boolean);
var
  Symbols: ISymbolProvider;
  Changed: Boolean;
  Count:   Integer;
begin
  if FMeta = nil then Exit;

  { Symbol checking needs a loaded model. Before the first Simulate there
    isn't one, and passing nil is the documented way to ask for a
    syntax-only check rather than a wrong one — every name would otherwise
    be reported as unknown. The parse is repeated with real symbols once
    the model loads, which upgrades the diagnostics in place. }
  if FSession.IsLoaded then
    Symbols := TRoadRunnerSymbolProvider.Create(FSession)
  else
    Symbols := nil;

  { False means ERRORS; warnings still return True. Either way the
    diagnostics are worth showing and the valid commands are worth using,
    so the result is deliberately not tested here. }
  FMeta.ParseSource(GetAntimonyText, FCurrentFilePath, Symbols);

  FreeAndNil(FExperiments);
  FExperiments := TMetaExperimentSet.Create(FMeta);

  if AApplyToPanels then
  begin
    if FApplyMetaOnOpen then
      ApplyMetadataToPanels
    else
      FAppliedMetaSig := MetadataSignature;   { treat as seen, not applied }

    Count := FExperiments.CountUsable;
    if Count = 1 then
      ShowMetaBar('This model defines an experiment in its metadata block. ' +
        'Its settings have been loaded into the panels; nothing has been run.',
        False)
    else if Count > 1 then
      ShowMetaBar(Format(
        'This model defines %d experiments in its metadata block. ' +
        'Their settings have been loaded into the panels; nothing has been run.',
        [Count]), False)
    else if FMeta.HasMetadata then
      ShowMetaBar('This model has a metadata block, but none of its ' +
                  'experiments can be used. See the diagnostics.', False)
    else
      HideMetaBar;
  end
  else
  begin
    { A reload, not an open. Re-parsing refreshes the diagnostics and the
      experiment list, but must not touch the controls: EnsureLoaded
      reloads after any edit, so applying here would overwrite the values
      the user had just typed. If the block itself changed, offer it. }
    Changed := (FAppliedMetaSig <> '') and
               (MetadataSignature <> FAppliedMetaSig);
    if Changed then
      ShowMetaBar('The experiment definitions in this model have changed.',
                  True);
  end;

  { The frames read their experiments back through the context, so they
    have to be told the set was replaced. Applying to the controls is a
    separate decision, made above. }
  if FFrameTimeCourse <> nil then
    FFrameTimeCourse.MetadataChanged(False);
  if FFrameParameterScan <> nil then
    FFrameParameterScan.MetadataChanged(False);
  if FFrameSteadyState <> nil then
    FFrameSteadyState.MetadataChanged(False);
  if FFrameMetadata <> nil then
    FFrameMetadata.Refresh;
  UpdateExportMenuState;
  RebuildRunExperimentMenu;
end;

procedure TfrmMain.ApplyMetadataToPanels;
begin
  if FExperiments = nil then Exit;

  { Each panel takes the first usable experiment of its own kind. No panel
    switching and no computation: the block fills controls, and the user
    presses the panel's own compute button when they want a result. }
  if FFrameTimeCourse <> nil then
    FFrameTimeCourse.MetadataChanged(True);
  if FFrameParameterScan <> nil then
    FFrameParameterScan.MetadataChanged(True);
  if FFrameSteadyState <> nil then
    FFrameSteadyState.MetadataChanged(True);

  FAppliedMetaSig := MetadataSignature;
end;

function TfrmMain.GetMetadata: TSimulationMetadata;
begin
  Result := FMeta;
end;

function TfrmMain.GetMetaExperiments: TMetaExperimentSet;
begin
  Result := FExperiments;
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
var
  Sbml, Err: string;
begin
  { Convert before asking where to put it: a model that will not convert
    should say so rather than send the user through a save dialog first. }
  if not BuildSbml(Sbml, Err) then
  begin
    ShowMessage('Could not convert the model to SBML: ' + Err);
    Exit;
  end;

  if SaveSBMLDialog.Execute then
    TFile.WriteAllText(SaveSBMLDialog.FileName, Sbml);
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
      ClearPlotAndLoadedData;
      //moAntimony.text := uAntimonyAPI.getAntimonyFromSBML(SBMLString);
      moAntimony.SetText (uAntimonyAPI.getAntimonyFromSBML(SBMLString));
      FCurrentFilePath := '';
      FSession.ClearDirty;
      { SBML carries no metadata block, so this normally just clears the
        previous model's experiments — which is the point. }
      ParseMetadata(True);
    end;
end;

procedure TfrmMain.mnuLoadFileClick(Sender: TObject);
begin
 if OpenDialogAnt.Execute then
    begin
    FSession.Unload;
    ClearPlotAndLoadedData;
    //moAntimony.Text := TFile.ReadAllText(OpenDialogAnt.FileName);
    moAntimony.SetText (TFile.ReadAllText(OpenDialogAnt.FileName));

    FCurrentFileName := ExtractFileName(OpenDialogAnt.FileName);
    FCurrentFilePath := OpenDialogAnt.FileName;
    Caption := 'Iridium II: ' + FCurrentFileName;
    FSession.ClearDirty;
    ParseMetadata(True);
    end;
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
var
  Msg: string;
begin
  { New discards the model text along with the plot and every loaded data
    overlay, and none of it can be recovered — so confirm before wiping.
    An empty editor has nothing to lose, so don't nag in that case. }
  if Trim(moAntimony.GetText) <> '' then
    begin
    if FIsModifiedSinceLastSave then
      Msg := 'The model has unsaved changes. Clear it and start a new model?'
    else
      Msg := 'Clear the current model and start a new model?';

    if MessageDlg(Msg, TMsgDlgType.mtConfirmation,
                  [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
      Exit;
    end;

  //moAntimony.Text := '';
  moAntimony.SetText('');
  FSession.Unload;
  ClearPlotAndLoadedData;
  FFrameTimeCourse.SetSimulationParameters(20, 200);

  { Back to the untitled state — the old file name no longer describes what is
    in the editor. Set after Unload: the session's state-changed listener
    rewrites Caption, so this has to be the last word. }
  FCurrentFileName := 'untitled.txt';
  FCurrentFilePath := '';
  Caption := 'Iridium II: ' + FCurrentFileName;
  { An empty editor has no experiments; this clears the previous model's. }
  ParseMetadata(True);
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
      to unrelated series. A compatible in-place edit keeps the styling.
      DEFAULT_STYLE_KEY is preserved — the pristine baseline is model-agnostic. }
    ClearPanelStyleSnapshots;
    { Observable colours belong to the old model's names; start the new model's
      palette fresh from the first colour. }
    FSimColorByName.Clear;
    TColorManager.ResetCycle;
    { New parameter set — let the scan panel re-seed its observables from the
      time-course selection when it is next shown. }
    FScanObservablesSeeded := False;
    FSliderFrame.ClearSliders;
    FSliderFrame.LoadParams(FSession.GetTunableNames,    { <-- refresh catalogue }
                            FSession.GetTunableValues);
  end
  else
    FSliderFrame.RefreshValues(FSession.GetTunableNames,
                               FSession.GetTunableValues);

  { Re-parse, but do NOT re-apply. Now that a model is loaded the parse can
    check symbols, which upgrades "unknown name" diagnostics that a
    syntax-only parse could not produce. Applying here would be wrong: this
    fires from EnsureLoaded, which runs after any edit, so it would
    overwrite whatever the user had just typed into the panel. }
  ParseMetadata(False);
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
    { Model gone -> stored per-analysis plot styling is meaningless.
      DEFAULT_STYLE_KEY is preserved as the model-agnostic baseline. }
    ClearPanelStyleSnapshots;
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
var
  Switching: Boolean;
begin
  Switching := ATarget <> FActiveFrame;

  { Loaded data belongs to the panel it was loaded on, so the outgoing panel's
    overlays come off the plot and into its own store. Runs before FActiveFrame
    moves, while ActiveAnalysisKey still names the outgoing panel. }
  if Switching and (ActiveAnalysisKey <> '') then
    CapturePanelDataState;

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
  FFrameMetadata.Visible      := False;

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

  { Pick up editor edits made while another panel was showing, so the scan
    parameter combo and observable lists describe the current model. Must run
    before UpdateScanParameterLock, which locks whatever the combo now holds. }
  if ATarget = FFrameParameterScan then
    FFrameParameterScan.RefreshFromModelIfStale;

  { The report describes the parse as it stands now — the block may have
    been edited since it was last rendered. }
  if ATarget = FFrameMetadata then
    FFrameMetadata.Refresh;

  { The first time the scan panel is shown for this model, seed its observables
    from the current time-course selection, so a scan starts from the variables
    the user was already plotting. Once only — thereafter their scan selection
    is left alone. Runs after RefreshFromModelIfStale so the lists are current. }
  if (ATarget = FFrameParameterScan) and (not FScanObservablesSeeded) then
  begin
    FFrameParameterScan.SetCheckedObservables(FFrameTimeCourse.GetSelectedYAxisNames);
    FScanObservablesSeeded := True;
  end;

  { Blank the simulation display on a mode switch — its results must be
    regenerated by the incoming panel. }
  Plot.ClearSeriesKind(skSimulation);

  { Bring in the incoming panel's own loaded data (and dropdown), replacing the
    outgoing panel's, which CapturePanelDataState stored above. }
  if Switching then
    RestorePanelDataState;

  { Restore this panel's chart/axis/legend styling now, not only on the next
    simulation rerun. Axis styling (log X/Y, manual limits, titles, legend
    position) lives on a single shared plot object, so without this the plot
    keeps whatever the previous panel left — e.g. a log axis set here reverts
    after visiting a panel that ran with linear axes. Data-series styling is
    excluded from these snapshots (see CaptureStylingJson), so restoring never
    reformats the loaded data.

    A panel that has never been visited has no saved styling, and would
    otherwise inherit the shared plot's current state (again, the previous
    panel's log axis). Restore the pristine default instead, so every panel's
    first appearance looks the same regardless of what was used before it. }
  if (ActiveAnalysisKey <> '') and Plot.HasSettings(ActiveAnalysisKey) then
    Plot.RestoreSettings(ActiveAnalysisKey)
  else if Plot.HasSettings(DEFAULT_STYLE_KEY) then
    Plot.RestoreSettings(DEFAULT_STYLE_KEY);

  Plot.Redraw;

  if ATarget = FFrameParameterScan then
    FFrameParameterScan.UpdateScanParameterLock
  else
    FSliderFrame.SetLockedParam('');

  { Whose results are showing has changed, so what can be written has too. }
  OutputStateChanged;
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

  { Returning to Help resumes where the reader was. Analysis actions
    switch away to the plot on purpose — help is for writing a model, not
    for watching it run — so coming back must not start again at the top. }
  if TabControl1.ActiveTab = tbHelp then
  begin
    if FHelpDocId = '' then
      ShowHelpDoc(HELP_DOCS[Low(HELP_DOCS)].Id)    { antimony: what a reader opening Help first needs }
    else
      HelpViewer.SetScrollPos(FHelpScrollY);
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  mnuSaveClick(Sender);
end;

procedure TfrmMain.btnScanClick(Sender: TObject);
begin
  ShowAnalysisFrame(FFrameParameterScan);
end;

procedure TfrmMain.btnTimeCourse1Click(Sender: TObject);
begin
  ShowAnalysisFrame(FFrameTimeCourse);
end;

procedure TfrmMain.btnAntimonyHelpClick(Sender: TObject);
begin
  ShowHelpDoc('antimony');
end;

procedure TfrmMain.btnClearDataClick(Sender: TObject);
begin
  { The user's explicit control over the loaded data: remove all of it from the
    panel they are looking at. Data loaded on the other panels is theirs to
    clear from there. (A model change clears every panel's, via
    ClearPlotAndLoadedData.) }
  Plot.ClearSeriesKind(skData);
  ClearLoadedDataFiles;
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

procedure TfrmMain.btnSimulationHelpClick(Sender: TObject);
begin
  ShowHelpDoc('metadata');
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

procedure TfrmMain.btnLighDarkClick(Sender: TObject);
begin
  { One button toggling between the two themes. ApplyTheme resets every
    colour surface, so a document loaded later has to have the current
    theme re-applied — see ShowHelpDoc. }
  FHelpDark := not FHelpDark;
  if FHelpDark then
    HelpViewer.ApplyTheme(rtDark)
  else
    HelpViewer.ApplyTheme(rtLight);
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
    Panel : TPanelDataFiles;
begin
  if OpenDialog1.Execute then
     begin
     ClearSeries := False;
     if chkOverlayData.IsChecked then
        ClearDataKind := False
     else
        ClearDataKind := True;

     { The data lands on the panel the user is looking at, and belongs to it. }
     Panel := CurrentPanelData;

     FileName := ExtractFileName(OpenDialog1.FileName);
     { A given file is loaded once per panel — the same measurements can
       legitimately be loaded again on another panel. }
     for i := 0 to Panel.Files.Count - 1 do
         if FileName = Panel.Files[i].FileName then
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
         { Markers are always solid, fill matching border. LoadData only paints
           both when the column name matches a simulation series; otherwise the
           fill keeps the component default (white), which reads as a hollow
           marker. The stroke already carries the next palette color in either
           case, so copying it across covers both. }
         TPlotSeries (Series.Objects[i]).MarkerFillColor := TPlotSeries (Series.Objects[i]).MarkerStrokeColor;
         TPlotSeries (Series.Objects[i]).MarkerSize := 4;
         end;

     lblParameterName.Text := TPlotSeries (Series.Objects[0]).XLabel;

     LoadedDataFile := TLoadDataFile.Create;
     LoadedDataFile.FileName := FileName;
     LoadedDataFile.ParameterName := lblParameterName.Text;
     for i := 0 to Series.Count - 1 do
         LoadedDataFile.Series.Add(TPlotSeries (Series.Objects[i]).Clone);
     Panel.Files.Add(LoadedDataFile);

     Series.Free;

     { Record what is now on screen (this dataset alone, or added to the ones
       already there when overlaying) so leaving and returning to this panel
       comes back to the same picture. }
     CapturePanelDataState;

     Plot.Redraw;
     end;
end;

procedure TfrmMain.cboLoadedFilenameChange(Sender: TObject);
var i : integer;
    Index : Integer;
    Found : Boolean;
    Panel : TPanelDataFiles;
begin
  if not FireEvent then Exit;

  { Clearing the combo (new model / clear data) fires OnChange with no
    selection — nothing to show, and indexing Items would raise. }
  if cboLoadedFilename.ItemIndex < 0 then Exit;

  { Preserve any styling the user edited on the dataset currently shown before
    we replace it, so re-selecting it later re-shows those edits. }
  SyncOverlayStyleToStorage;

  Panel := CurrentPanelData;

  Found := False;
  for i := 0 to Panel.Files.Count - 1 do
      if cboLoadedFilename.items[cboLoadedFilename.ItemIndex] = Panel.Files[i].FileName then
         begin
         lblParameterName.Text := Panel.Files[i].ParameterName;
         Found := True;
         Index:= i;
         break;
         end;
  if Found then
     begin
     { Show only the selected dataset: current data goes, selected comes in. }
     Plot.ClearSeriesKind(skData);
     for i := 0 to Panel.Files[Index].Series.Count - 1 do
         Plot.AddSeries(Panel.Files[Index].Series[i].Clone);

     { This panel now shows that one dataset — remember it for the next return. }
     Panel.DisplayedIds.Clear;
     for i := 0 to Panel.Files[Index].Series.Count - 1 do
         Panel.DisplayedIds.Add(Panel.Files[Index].Series[i].SeriesId);
     Panel.SelectedIndex := cboLoadedFilename.ItemIndex;
     end;
  Plot.Redraw;
end;


procedure TfrmMain.btnCopyToStorageClick(Sender: TObject);
var i : integer;
    Index : Integer;
    StoredSeries : TPlotSeries;
    Found : Boolean;
    Files : TList<TLoadDataFile>;
begin
  if cboLoadedFilename.ItemIndex < 0 then Exit;

  Files := CurrentPanelData.Files;

  Found := False;
  Index := -1;
  for i := 0 to Files.Count - 1 do
      if cboLoadedFilename.items[cboLoadedFilename.ItemIndex] = Files[i].FileName then
         begin
         lblParameterName.Text := Files[i].ParameterName;
         Found := True;
         Index:= i;
         break;
         end;
  if not Found then Exit;

  for i := 0 to Plot.Series.Count -1 do
      if Plot.Series[i].SeriesKind = skData then
         begin
         if Plot.Series[i].SeriesId = Files[Index].Series[i].SeriesId then
            begin
            Files[Index].Series[i].MarkerSize := Plot.Series[i].MarkerSize;
            Files[Index].Series[i].MarkerFillColor := Plot.Series[i].MarkerFillColor;
            Files[Index].Series[i].MarkerStrokeColor := Plot.Series[i].MarkerStrokeColor;
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
  ClearPlotAndLoadedData;
  moAntimony.SetText (Model.ModelStr);
  FCurrentFileName := 'untitled.txt';
  FCurrentFilePath := '';
  Caption := 'Iridium II: ' + FCurrentFileName;
  FSession.ClearDirty;
  FFrameTimeCourse.SetSimulationParameters(Model.timeEnd, Model.NumberOfPoints);
  { After the built-in model's own defaults, so a metadata block in the
    example wins over them. }
  ParseMetadata(True);
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
  { What the X title would be if nobody had renamed it — the yardstick
    PlotEndRebuild measures the restored title against. }
  FSnapshotAutoXTitle := FAutoXTitle;
end;

procedure TfrmMain.PlotEndRebuild;
begin
  { Re-apply the active frame's saved styling to the just-rebuilt series
    (matched by series name) and redraw. No-op the first time a frame plots,
    before it has anything stored. }
  if (ActiveAnalysisKey <> '') and Plot.HasSettings(ActiveAnalysisKey) then
    Plot.RestoreSettings(ActiveAnalysisKey);

  { The restore just reinstated the snapshot's X title. Where that title was
    the previous X column's name rather than something the user chose, the
    column PlotData has just plotted against is the right label. }
  if (FAutoXTitle <> '')
     and (Plot.XAxisTitle.Text = FSnapshotAutoXTitle)
     and (Plot.XAxisTitle.Text <> FAutoXTitle) then
    Plot.XAxisTitle.Text := FAutoXTitle;

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
  var
    J    : Integer;
    Name : string;
    C    : TAlphaColor;
  begin
    Name := AData.columnHeader[AColIdx];

    { Colour is keyed to the observable name, not its slot in the selection.
      First time we see a name it takes the next palette colour and keeps it;
      thereafter it is reused, so toggling other observables never recolours
      this one. }
    if not FSimColorByName.TryGetValue(Name, C) then
    begin
      C := TColorManager.NextColor;
      FSimColorByName.Add(Name, C);
    end;

    Series := TPlotSeries.Create(Name, claBlue);
    Series.YLabel        := Name;
    Series.LineColor     := C;
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
  FAutoXTitle := XLabel;
  Plot.Redraw;
  TabControl1.ActiveTab := tbPlot;
end;

procedure TfrmMain.PlotSetXAxisTitle(const ATitle: string);
begin
  Plot.XAxisTitle.Text := ATitle;
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

procedure TfrmMain.PlotApplyMetaStyle(ACmd: TPlotCommand);
var
  I:     Integer;
  Style: TSeriesStyle;
  S:     TPlotSeries;
begin
  if (ACmd = nil) or (Plot = nil) then Exit;

  { Chart-level appearance. Only keys the user actually wrote are applied:
    an absent 'title' must leave whatever the plot already shows rather
    than blanking it, so absence and empty-string stay distinguishable. }
  if ACmd.WasWritten('title') then
  begin
    Plot.ChartTitle.Text    := ACmd.Title;
    Plot.ChartTitle.Visible := ACmd.Title <> '';
  end;
  if ACmd.WasWritten('xlabel') then
    Plot.XAxisTitle.Text := ACmd.XLabel;
  if ACmd.WasWritten('ylabel') then
    Plot.YAxisTitle.Text := ACmd.YLabel;

  { grid: true sets both gridx and gridy, which the validator has already
    folded in, so only the two specific fields are read here. }
  if ACmd.WasWritten('grid') or ACmd.WasWritten('gridx') then
    Plot.GridStyle.XMajorVisible := ACmd.GridX;
  if ACmd.WasWritten('grid') or ACmd.WasWritten('gridy') then
    Plot.GridStyle.YMajorVisible := ACmd.GridY;

  if ACmd.WasWritten('logx') then Plot.AxisStyle.LogX := ACmd.LogX;
  if ACmd.WasWritten('logy') then Plot.AxisStyle.LogY := ACmd.LogY;

  { Per-series styling, matched by name against the series actually drawn.
    Only simulation series: a data overlay may share a name with its
    simulated counterpart, and restyling the overlay from a @plot would
    misrepresent the user's own data. }
  if (ACmd.Series.Count > 0) or ACmd.WasWritten('type') then
    for I := 0 to Plot.Series.Count - 1 do
    begin
      S := Plot.Series[I];
      if S.SeriesKind <> skSimulation then Continue;

      { The plot-wide 'type' first, so a per-series 'type' below can
        override it. }
      if ACmd.WasWritten('type') then
        case ACmd.PlotType of
          ptLine:       begin S.LineVisible := True;  S.MarkerVisible := False; end;
          ptScatter:    begin S.LineVisible := False; S.MarkerVisible := True;  end;
          ptLineMarker: begin S.LineVisible := True;  S.MarkerVisible := True;  end;
          ptBar:        ;
        end;

      { Series are named after result column headers, which use the
        model's own ids ('[A]'), while a series: block is keyed on the
        name the user wrote ('A'). Try both. }
      Style := ACmd.StyleOf(S.Name);
      if Style = nil then
        Style := ACmd.StyleOf(CanonicalModelName(S.Name));
      if Style = nil then Continue;

      { Colours arrive as a Cardinal in ARGB order — the library stores
        them that way so it need not pull in FMX for one field, and
        TAlphaColor is the same layout. HasColor rather than a sentinel:
        TAlphaColors.Null is itself a legal colour. }
      if Style.HasColor then
      begin
        S.LineColor         := TAlphaColor(Style.Color);
        S.MarkerStrokeColor := TAlphaColor(Style.Color);
        S.MarkerFillColor   := TAlphaColor(Style.Color);
        { Keep the palette in step, so a later re-plot of this observable
          reuses the colour the model asked for instead of reverting. }
        FSimColorByName.AddOrSetValue(S.Name, TAlphaColor(Style.Color));
      end;

      if Style.HasLineWidth then
        S.LineWidth := Style.LineWidth;

      if Style.HasLineStyle then
        case Style.LineStyle of
          Sim.Meta.Model.lsSolid:  S.LineStyle := ltSolid;
          Sim.Meta.Model.lsDashed: S.LineStyle := ltDashDash;
          Sim.Meta.Model.lsDotted: S.LineStyle := ltDotDot;
          { The plot has no dash-dot pattern. Dashed is the closest thing
            that still reads as "not a solid line". }
          Sim.Meta.Model.lsDashDot: S.LineStyle := ltDashDash;
        end;

      if Style.HasMarkerStyle then
      begin
        S.MarkerVisible := Style.MarkerStyle <> msNone;
        case Style.MarkerStyle of
          msCircle:   S.MarkerShape := symCircle;
          msSquare:   S.MarkerShape := symSquare;
          msTriangle: S.MarkerShape := symTriangle;
          msDiamond:  S.MarkerShape := symDiamond;
          msCross:    S.MarkerShape := symCross;
          msNone:     ;
        end;
      end;

      if Style.HasMarkerSize then
        S.MarkerSize := Style.MarkerSize;

      { An explicit per-series 'type' overrides the plot's. }
      if Style.HasType then
        case Style.PlotType of
          ptLine:       begin S.LineVisible := True;  S.MarkerVisible := False; end;
          ptScatter:    begin S.LineVisible := False; S.MarkerVisible := True;  end;
          ptLineMarker: begin S.LineVisible := True;  S.MarkerVisible := True;  end;
          ptBar:        ;   { no bar renderer here; reported by the caller }
        end;
    end;

  Plot.Redraw;
end;

function TfrmMain.UserStyleKey: string;
begin
  { Suffixed so it can never collide with the panel's own styling key,
    which PlotBeginRebuild / PlotEndRebuild own. }
  Result := ActiveAnalysisKey;
  if Result <> '' then
    Result := Result + '::user';
end;

procedure TfrmMain.PlotCaptureUserStyle;
var
  Key: string;
begin
  Key := UserStyleKey;
  if Key <> '' then
    Plot.SaveSettings(Key);
end;

procedure TfrmMain.PlotRestoreUserStyle;
var
  Key: string;
begin
  Key := UserStyleKey;
  if (Key <> '') and Plot.HasSettings(Key) then
    Plot.RestoreSettings(Key)
  { Nothing captured — the user never had settings of their own on this
    panel, so the honest answer is the pristine default rather than
    whatever the last preset happened to leave behind. }
  else if Plot.HasSettings(DEFAULT_STYLE_KEY) then
    Plot.RestoreSettings(DEFAULT_STYLE_KEY);
  Plot.Redraw;
end;

procedure TfrmMain.CopyTextToTextWindow (AString : String);
begin
  TabControl1.ActiveTab := tbTextView;
  moTextView.text := AString;
end;

procedure TfrmMain.AppendToAntimonySource(const ABlock: string;
  AReplace: Boolean);
const
  BLOCK_TAG = '// [SliderValues]';
var
  Src:    string;
  TagPos: Integer;
begin
  Src := moAntimony.GetText;
  { Replace mode: drop everything from the first tagged block onward, so only
    the new block remains. Append mode (default): leave existing blocks in
    place and add the new one at the end. }
  if AReplace then
  begin
    TagPos := Pos(BLOCK_TAG, Src);
    if TagPos > 0 then
      Src := Copy(Src, 1, TagPos - 1).TrimRight;
  end;
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
