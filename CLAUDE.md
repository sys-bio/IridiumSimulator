# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Iridium is a desktop FireMonkey (FMX) application for systems-biology simulation,
built in Delphi (targeting Delphi 13 / Win64 and macOS ARM64). It loads
[Antimony](https://github.com/sys-bio/antimony) model descriptions, converts them
to SBML, and simulates them with the [libRoadRunner](https://github.com/sys-bio/roadrunner)
engine. Features: time-course simulation, steady-state, interactive slider-driven
simulation, parameter scans, and MCA-style sensitivity (control-coefficient) analysis.

## Build / Run

This is a Delphi project — there is no make/npm. Build with the RAD Studio command-line
compiler or the IDE.

- **Project file:** `IridiumSimulator.dproj` (main source `IridiumSimulator.dpr`).
- **Default config/platform:** `Debug` / `Win64`. Release and `OSXARM64` configs also exist.
- **Command-line build.** `rsvars.bat` sets up the compiler environment; it lives in the
  RAD Studio 37.0 `bin` directory. From the Bash tool (Git Bash) the working pattern is to
  shell out to `cmd` so the batch file's env vars survive into the `msbuild` call:
  ```
  cmd /c '"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" && msbuild IridiumSimulator.dproj /t:Build /p:Config=Debug /p:Platform=Win64'
  ```
  A successful compile ends with a line like `NNNNN lines, N.NN seconds, ... bytes code`.
  The external `uMatrix.pas` / `uJVector.pas` units emit many harmless H2164/W1036/W1029
  hints and warnings — these are expected, not build failures.
- **Output:** `Win64\Debug\` (DCUs under `Win64\Debug\dcu`).
- **No automated test suite exists.** Verification is manual: run the app, load a model
  (built-in models live in `uBuiltInModels.pas`, or `*.ant` files in the repo root such as
  `Lorenz.ant`, `Model1.ant`), and exercise the analysis tabs.

### Native DLL dependencies (runtime)

The app loads two native libraries at startup and will not function without them:
- `libantimony.dll` (`libantimony.dylib` on macOS) — loaded from the executable
  directory by `uAntimonyAPI.loadAntimonyLibrary`.
- `roadrunner_c_api.dll` — loaded by `uRoadRunner.loadRoadRunner`. On Windows the source
  references `libRoadRunner\bin\roadrunner_c_api.dll`.

`TfrmMain.FormCreate` (`ufMain.pas`) loads both; if either fails the app surfaces an error there.

## External source roots

The project pulls `.pas` units from sibling directories outside this repo via relative
paths in the `.dpr` — these are **not** in this repository:
- `..\..\CommonCode\libRoadRunner\` — the RoadRunner Pascal wrapper and numeric libs
  (`uRoadRunner.pas`, `uRoadRunner.API.pas`, `uMatrix.pas`, `uRR2DSimpleMatrix.pas`, etc.).
  This is registered as an additional working directory.
- `..\PlottingComponent\Source\` — the Skia-based plotting component
  (`SkPlotPaintBox`, `uPlotSeries`, `ufPlotEditor`, etc.).
- `..\T3DBarGraph-main\U3DBarGraph.pas` — the 3D bar-graph component (control-coefficient plots).

The in-repo `RichMemo\` folder is the syntax-highlighting Antimony code editor
(`Syntax.Code.Antimony.pas`, `FMX.RichEdit.Style.pas`, `SpellChecker.pas`).

## Architecture

The design centers on a **shell form + swappable analysis frames + a shared model session**,
deliberately decoupled so frames never reference the main form directly.

- **`ufMain.pas` (`TfrmMain`)** — application shell. Owns the single `TModelSession`, the
  shared slider panel, and the plot. Hosts the analysis frames inside `LayoutContainer`,
  showing one at a time. It **implements `IAnalysisContext`** and passes `Self` to each
  frame via `SetContext`.

- **`uModelSession.pas` (`TModelSession`)** — single source of model truth. Owns the one
  `TRoadRunner` instance and tracks `IsLoaded` / `IsDirty`. `EnsureLoaded` pulls Antimony
  text (via the `OnNeedAntimonyText` callback wired to the editor), converts it to SBML, and
  loads it into RoadRunner. Broadcasts two listener events to any number of subscribers:
  - **state-changed** (`TNotifyEvent`) — fires on `IsLoaded`/`IsDirty` transitions.
  - **model-reloaded** (`TModelReloadedEvent`) — fires after a successful reload;
    `AParameterSetChanged` tells frames whether the parameter set changed structurally
    (clear sliders) or is a compatible edit (refresh in place). The decision is made by
    comparing a sorted "tunable names" signature (globals + boundary species).

- **`uAnalysisTypes.pas` (`IAnalysisContext`)** — the narrow services interface the shell
  exposes to frames: `GetSession`, `GetSliderContainer`, plotting operations
  (`PlotData`, `PlotAddSeries`, `PlotClearSimulationSeries`, `PlotRecolorSimulationSeries`),
  steady-state host access, and Antimony-source append. **This is the contract every frame
  programs against — extend this interface rather than reaching into `TfrmMain`.**

- **Analysis frames** — each is a `TFrame` that receives an `IAnalysisContext` in
  `SetContext`, subscribes to the session's listeners there, and drives the shared
  RoadRunner/plot through the context:
  - `uFrameTimeCourse.pas` — time-course simulation.
  - `uFrameSteadyState.pas` — steady-state + control-coefficient (MCA) analysis.
  - `uFrameParameterScan.pas` — parameter scans.
  - `uFrameSliderContainer.pas` — the shared interactive-slider panel (`OnSliderChanged`).

- **`uAntimonyAPI.pas`** — thin `cdecl` FFI over `libantimony`. The key entry point used by
  the session is `getSBMLFromAntimony`, which returns a `TModelErrorState`
  (`uCommonTypes.pas`). Note: a non-empty libantimony error buffer after a successful load is
  treated as a **failure** (the resulting SBML won't simulate).

### Conventions

- Frames are decoupled: a frame must reach shared services only through its `IAnalysisContext`,
  never via the global `frmMain`. When a frame needs something new from the shell, add it to
  `IAnalysisContext` and implement it in `TfrmMain`.
- Listener registration is idempotent and dispatch is snapshot-based, so adding/removing
  listeners during a callback is safe.
- The app version string is `VERSION` in `ufMain.pas`.

### Notes when editing

- `.fmx` files are FireMonkey form designers paired with their `.pas`; `ufMain.fmx` is very
  large (multi-MB). Edit form structure through the matching `.pas` declarations where possible.
- `__history/` and `__recovery/` are IDE backup folders (git-ignored) — ignore them.
- Compiler binaries (`*.dcu`, `*.exe`, `*.dll`, `*.local`, `*.cfg`) are git-ignored.
