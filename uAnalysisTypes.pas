unit uAnalysisTypes;

{ Shared types used across the analysis-frame architecture.

  IAnalysisContext is the small "services" interface that the main form
  implements. Each analysis frame receives one (via SetContext) and uses it
  to reach shared facilities: the model session, the slider container, and
  the plot. Frames never refer to frmMain directly. }

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  uRR2DSimpleMatrix,
  FMX.Layouts,
  uModelSession,
  uMetaExperiments,
  Sim.Meta,
  Sim.Meta.Model,
  uFrameSliderContainer;

type
  TPlotSeriesColorInfo = record
    Name:      string;
    LineColor: TAlphaColor;
  end;

  TPlotInfo = record
    LegendVisible:  Boolean;
  end;

  IPythonScriptExporter = interface
    ['{7059703F-8F7F-44DE-A4FC-1DBF3D26C1D9}']
    function GetPythonScript(const AntimonyText: string): string;
  end;

  { Implemented by an analysis frame whose results are not a set of plot
    series, and so cannot be rendered into the Text tab by exporting the
    plot. The shell asks the active frame first and falls back to the plot
    when it says nothing — so a panel that draws curves needs no code here,
    and a panel that fills grids can still answer.

    ADecimals is the panel's own decimal-places setting, passed in so the
    text matches what the user has on screen rather than a second,
    independent notion of precision. }
  ITextViewProvider = interface
    ['{2C1E4B77-6F3A-4E27-9B8D-6C3F4A50D2E1}']
    function GetTextView(ADecimals: Integer): string;

    { The user turned the decimals spin on the text output panel. A panel
      that formats its own results adopts the new precision here — its
      grids, its own decimals control and its text all move together.

      Deliberately separate from GetTextView, which must stay free of side
      effects: merely LOOKING at the Text tab calls that, and it must not
      silently re-render the panel behind it. This is called only when the
      user actually turns the dial. }
    procedure SetDisplayDecimals(ADecimals: Integer);
  end;

  { Implemented by an analysis frame whose results can satisfy an @output
    command. The shell asks the active frame for these when deciding
    whether to offer a Write button on the notice bar.

    The offer lives on the bar rather than on the panels because writing a
    file is not part of setting up a run, and the analysis panels are for
    setup. The bar already appears only for models carrying a metadata
    block, and it sits directly above the results being written. }
  IMetaOutputProvider = interface
    ['{2C9E5A41-7B3D-4E88-9F16-5A0D7C3B4E29}']
    { The experiment whose @output commands are currently satisfiable, or
      nil when there is nothing to write — no experiment selected, or no
      result yet. }
    function GetOutputExperiment: TMetaExperiment;
    { The result those commands would be written from. }
    function GetOutputData: T2DMatrix;
  end;


  { Implemented by an analysis panel that can describe its own settings as
    metadata commands — the return leg of the block, which until now only
    ever travelled from the file into the panels.

    The panel answers with the TASK it is set up to run. It does not build
    the '@plot': the plot belongs to the shell, and what is on screen is
    what the figure should be described from. APlotY is how the panel says
    what a plot over this task would draw, which the shell cannot read off
    the plot itself — a scan overlay names its series '[S1]  X0=0.5', after
    the observable and the parameter value together, and those are not
    model ids. An empty APlotY means "no figure": a steady state produces a
    table, so the format gives it '@output' rather than '@plot'.

    ATaskLabel is chosen by the shell so it cannot collide with a label
    already in the file; a panel emitting more than one command derives the
    rest from it by suffix.

    The caller owns the returned commands and frees them. }
  IMetaScriptProvider = interface
    ['{5E7A1C33-9D42-4B0E-8F16-2A7C4D3B9E51}']
    function GetMetaCommands(const ATaskLabel: string;
                             out APlotY: TArray<string>
                            ): TArray<TMetaCommandBase>;
  end;


  IAnalysisContext = interface
    ['{B3D8E2A4-5F1C-4B7E-9A2D-1E3F5A7B9C2D}']
    function  GetSession: TModelSession;
    function  GetSliderContainer: TFrameSliderContainer;

    { Plot a simulation result.
        AXAxisName   - name of the column to use as X. Empty string or an
                       unrecognised name falls back to column 0 (typically
                       time).
        AYAxisNames  - names of columns to plot as Y series. An empty
                       array yields an empty plot - callers wanting a
                       "plot everything" behaviour must enumerate the
                       column names explicitly. }
    procedure PlotData(const AData: T2DMatrix;
                       const AXAxisName: string;
                       const AYAxisNames: TArray<string>);

    { Low-level plot access for overlay-style results.
        PlotClearSimulationSeries removes all SERIES_TYPE_SIMULATION series.
        PlotAddSeries hands ownership of ASeries to the plot. }
    procedure PlotClearSimulationSeries;
    procedure PlotAddSeries(ASeries: TObject);
    procedure PlotRedraw;

    { Set the x-axis title. PlotData sets it from the X column automatically;
      the manual PlotAddSeries path (e.g. a parameter scan's time-course
      overlay) has no X column to infer from and must set it explicitly. }
    procedure PlotSetXAxisTitle(const ATitle: string);

    { Per-analysis plot-styling persistence. A frame must bracket every
      operation that clears and rebuilds its series (whether via PlotData or
      via PlotClearSimulationSeries + PlotAddSeries) between these two calls:

        PlotBeginRebuild - snapshot the plot's CURRENT styling under the
                           active analysis's key, so the user's latest edits
                           (colours, markers, axes, legend, limits, titles)
                           are captured before the series are destroyed.
        PlotEndRebuild   - re-apply that key's styling to the freshly rebuilt
                           series (matched by series name) and redraw.

      The shell owns the key (one per analysis frame) and the settings store,
      so frames never pass a key. Styling therefore survives both re-plots
      within a frame and switches away to another analysis and back.
      Right after a frame switch the first PlotBeginRebuild deliberately skips
      the snapshot so the outgoing frame's leftover series aren't captured
      under the incoming frame's key. }
    procedure PlotBeginRebuild;
    procedure PlotEndRebuild;

    { Reassign the LineColor (and MarkerStrokeColor) of every
      SERIES_TYPE_SIMULATION series currently in the plot, drawing
      successive colours from ANextColor, then redraw. The caller is
      responsible for resetting/cycling its own palette state before
      invoking. Used to apply a new colour palette to an existing scan
      without re-running the simulations. }
    procedure PlotRecolorSimulationSeries(const ANextColor: TFunc<TAlphaColor>);

    { Returns the name and line color of each currently-plotted simulation
      series, in the order they appear in the plot. Empty array if no series
      are plotted. Used by the script-export path to match on-screen colors. }
    function PlotGetSimulationSeriesInfo: TArray<TPlotSeriesColorInfo>;

    function PlotGetPlotInfo: TPlotInfo;

    procedure CopyTextToTextWindow (AString : String);

    { Add a tagged block to the Antimony editor. AReplace = False (default)
      appends the block, leaving any earlier tagged blocks in place. AReplace =
      True first removes the previous tagged block(s), so the editor keeps only
      the latest. }
    procedure AppendToAntimonySource(const ABlock: string;
                                     AReplace: Boolean = False);

    function  GetSteadyStateHost: TScrollBox;
    procedure ShowSteadyStateTab;

    { Re-render the Text tab from whatever the active panel currently
      shows. For a panel whose results can change while that tab is
      visible — a decimals setting, say — so the text does not go stale
      behind the user. A no-op when the Text tab is not showing. }
    procedure RefreshTextView;

    { Simulation metadata parsed from the model's own comment block.

      Both may be nil (no model, or a model with no metadata block), and
      both are replaced wholesale on every re-parse — so a frame must
      re-read them through the context rather than caching a reference,
      and must key anything it remembers on an experiment's LABEL, never
      on its index or its address.

      A metadata block is a library of presets: it populates a panel's
      controls and never causes a computation. Presets are applied when a
      model is OPENED, never on the reload EnsureLoaded performs after an
      edit — re-applying there would silently overwrite the values the
      user had just typed. }
    function  GetMetadata: TSimulationMetadata;
    function  GetMetaExperiments: TMetaExperimentSet;

    { Open the Help tab at a document, optionally at a heading within it:
      'metadata' or 'metadata#parameter-scans'. The anchor is a
      GitHub-style slug of the heading text.

      This is what makes help worth having from an analysis panel — a
      user who is stuck is far more likely to press a '?' beside the
      control that puzzles them than to go looking through a manual. }
    procedure ShowHelp(const ATopic: string);

    { Tell the shell that what this frame could write has changed — a run
      finished, or a different experiment was selected. The shell then
      re-asks through IMetaOutputProvider and shows or hides the Write
      button accordingly. Cheap; call it freely. }
    procedure OutputStateChanged;

    { Apply a @plot command's appearance — title, axis titles, log axes,
      grid, per-series colour and line/marker style — to the series
      currently on the plot. Call it inside the PlotBeginRebuild /
      PlotEndRebuild bracket, AFTER the series exist, since the per-series
      part matches by name against what is drawn.

      Metadata styling is applied on each compute; edits the user then
      makes in the plot editor stand until the next one. }
    procedure PlotApplyMetaStyle(ACmd: TPlotCommand);

    { The user's own plot appearance, held so that stepping off a metadata
      preset can put it back.

      A @plot may switch on a log axis, a grid or a title, and those
      outlive the series they were applied to — so a panel that returns to
      the user's own settings while still showing the file's log axis is
      only half back. These are the styling counterpart of the numbers a
      frame snapshots before applying its first preset: capture before the
      first preset (and again whenever the user has since been working
      without one), restore when they step off it.

      The shell owns the storage key, as it does for the per-analysis
      styling — a frame never names one. Restoring with nothing captured
      falls back to the plot's pristine defaults. }
    procedure PlotCaptureUserStyle;
    procedure PlotRestoreUserStyle;

    property Session:         TModelSession         read GetSession;
    property SliderContainer: TFrameSliderContainer read GetSliderContainer;
    property SteadyStateHost: TScrollBox read GetSteadyStateHost;
    property Metadata:        TSimulationMetadata   read GetMetadata;
    property MetaExperiments: TMetaExperimentSet    read GetMetaExperiments;
  end;

implementation

end.
