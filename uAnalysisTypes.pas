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
