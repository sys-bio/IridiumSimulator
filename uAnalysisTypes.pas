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

    procedure AppendToAntimonySource(const ABlock: string);

    function  GetSteadyStateHost: TScrollBox;
    procedure ShowSteadyStateTab;

    property Session:         TModelSession         read GetSession;
    property SliderContainer: TFrameSliderContainer read GetSliderContainer;
    property SteadyStateHost: TScrollBox read GetSteadyStateHost;
  end;

implementation

end.
