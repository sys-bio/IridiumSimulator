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
- `..\RhodyComponents\PlottingComponent\Source\` — the Skia-based plotting component
  (`SkPlotPaintBox`, `uPlotSeries`, `ufPlotEditor`, etc.). Shared with other
  applications, so a change here affects them too. `..\RhodyComponents\RhoEditor\Source\`
  resolves through `DCC_UnitSearchPath` rather than an explicit reference.

**Do not change public signatures in the libRoadRunner or RhodyComponents trees.** They
are compiled into several other applications, so any change there means retesting all of
them — a far bigger cost than most fixes are worth. Propose the change and let the user
decide; fix what you can on Iridium's side of the boundary instead. (A known example left
alone deliberately: `TRoadRunner.getSBML : AnsiString` mis-decodes non-ASCII SBML, but its
only caller reads ASCII element names out of the result, so it is latent.)
- `..\T3DBarGraph-main\U3DBarGraph.pas` — the 3D bar-graph component (control-coefficient plots).
- `..\..\Antimony_MetaData_Support\` — the `Sim.Meta.*` simulation-metadata library
  (see **Simulation metadata** below). Its own project, with its own console test
  harness; referenced rather than copied so a fix there is a fix here. **RTL-only by
  design** — nothing in it may reference FMX or libRoadRunner, which is what will let
  a future bifurcation tool reuse it. The core units, the writer and the exporters
  (`Sim.Meta.Python`, `Sim.Meta.SedML.Export`, `Sim.Meta.Omex`) are all wired in — see
  **Exporting** below. Unlike the trees above, this is the user's own project and a bug
  in it is fixed there rather than worked around here.

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
  steady-state host access, `RefreshTextView`, and Antimony-source append. **This is the
  contract every frame programs against — extend this interface rather than reaching into
  `TfrmMain`.** The same unit declares the interfaces pointing the other way, which a
  frame implements and the shell asks for: `IPythonScriptExporter`, `IMetaOutputProvider`
  and `ITextViewProvider`.

- **Analysis frames** — each is a `TFrame` that receives an `IAnalysisContext` in
  `SetContext`, subscribes to the session's listeners there, and drives the shared
  RoadRunner/plot through the context:
  - `uFrameTimeCourse.pas` — time-course simulation.
  - `uFrameSteadyState.pas` — steady-state + control-coefficient (MCA) analysis.
    Its **Observables** selector (Species / Boundary / Fluxes, with per-tab Select All
    and Unselect All) chooses what the first output grid reports. The group box, tab
    control and two buttons are in the `.fmx` so they can be laid out in the IDE; the
    tabs and their listboxes are built in code because their contents come from the
    model. The checklist is the single authority: it drives
    `setSteadyStateSelectionListEx` at Compute, and the grid is read back through
    `computeSteadyStateValues` so names and numbers cannot drift apart.
  - `uFrameParameterScan.pas` — parameter scans.
  - `uFrameMetadata.pas` — the simulation-metadata report (see below). Computes and
    plots nothing, so `ActiveAnalysisKey` deliberately has no entry for it and the
    shell's plot-styling / loaded-data bookkeeping correctly skips it.
  - `uFrameSliderContainer.pas` — the shared interactive-slider panel (`OnSliderChanged`).

- **`uAntimonyAPI.pas`** — thin `cdecl` FFI over `libantimony`. The key entry point used by
  the session is `getSBMLFromAntimony`, which returns a `TModelErrorState`
  (`uCommonTypes.pas`). Note: a non-empty libantimony error buffer after a successful load is
  treated as a **failure** (the resulting SBML won't simulate).

  **Everything crossing this boundary is UTF-8.** Every function takes and returns ordinary
  Delphi strings and converts at the edge — `UTF8String(...)` going out, `Utf8PtrToString`
  coming back. Never declare an `AnsiString` parameter or result for library text and never
  cast a call argument to `AnsiString`: that type carries the *system* codepage, so it
  transcodes UTF-8 to CP1252 in one direction and misreads it in the other. The failure is
  invisible on ASCII-only models — it showed up as libSBML rejecting a downloaded BioModel
  with "XML content is not well-formed" at the first line containing a character like `τ`.

- **`uMetaSetValues.pas`** — resolving, applying and undoing a task command's `set:`
  values, shared by the panels that compute. A species is written through **both** its
  `init(...)` and its plain selector: `init()` alone is not enough when the panel is not
  resetting before the run, and the plain form alone does not survive a reset. Everything
  else — a global parameter, a compartment size — has only the plain form.

- **`uBioModelsCache.pas`** — search and fetch over a GitHub-hosted mirror of BioModels (the
  repository's own services are too slow to type against). `Search` filters an in-memory
  snapshot; `EnsureLoaded` downloads and parses the cache document once per instance, because
  the search box calls `Search` on every keystroke. See **BioModels search** below.

### Conventions

- Frames are decoupled: a frame must reach shared services only through its `IAnalysisContext`,
  never via the global `frmMain`. When a frame needs something new from the shell, add it to
  `IAnalysisContext` and implement it in `TfrmMain`.
- Listener registration is idempotent and dispatch is snapshot-based, so adding/removing
  listeners during a callback is safe.
- **A compute button loads the model itself.** Handlers that need a model call
  `Session.EnsureLoaded` (and report `Session.LastError` on failure) rather than testing
  `Session.IsLoaded` and telling the user to go run something else first. `EnsureLoaded`
  also covers the dirty-source case, and its reloaded event repopulates the frame's
  selectors, so validation that follows finds sane defaults.
- The app version string is `VERSION` in `ufMain.pas`.

### Plot series kinds

Every `TPlotSeries` carries a `SeriesKind`: `skSimulation` (computed output, owned by
whichever analysis produced it) or `skData` (a CSV overlay the user loaded). The kind is
what makes overlays durable, so treat it as identity, not styling:

- `PlotData` / `PlotClearSimulationSeries` clear **only** `skSimulation`. Loaded data
  survives every re-simulation, including slider-driven ones, and goes away only via
  Clear Data, a model swap, or a switch to a panel that has its own data (see below).
- `TfrmMain.btnLoadCSVClick` is what stamps `skData` — `SkPlotPaintBox.LoadData` creates
  series with the `skSimulation` default and the host re-labels them afterwards.
- Styling snapshots (`PlotBeginRebuild` / `PlotEndRebuild` → `SaveSettings` /
  `RestoreSettings`) serialise `seriesKind` alongside the visual properties, and
  `LoadStyleFromJson` writes it back. Entries are therefore matched to live series by
  **name + kind**, each series consumed once. Matching on name alone is a trap: a data
  overlay and its simulated counterpart usually share a name, and applying the simulation
  entry to the data series silently re-labels it `skSimulation`, after which the next
  `ClearSeriesKind(skSimulation)` deletes the user's data.
- Swapping the model (File ▸ Load, Import SBML, New, Examples dropdown) clears the whole
  plot through `TfrmMain.ClearPlotAndLoadedData` — nothing on the old plot describes the
  new model.

**Loaded data is scoped to the analysis panel it was loaded on.** Data loaded on the
time-course plot describes a time course and means nothing on a parameter scan, so it
must not follow the user across a panel switch:

- `FDataFilesByPanel: TObjectDictionary<string, TPanelDataFiles>` in `ufMain.pas` holds one
  `TPanelDataFiles` per `ActiveAnalysisKey`. Reach it through `CurrentPanelData`, which
  creates the entry on first use and never returns nil.
- `TPanelDataFiles` (`uCommonTypes.pas`) holds `Files` (the panel's `TLoadDataFile`
  catalogue, which it owns, each owning its cloned series), `DisplayedIds` (the `SeriesId`s
  actually drawn — several when "overlay data" is on) and `SelectedIndex` (the filename
  dropdown). Empty it via `ClearFiles`, never a bare `Files.Clear`.
- `ShowAnalysisFrame` brackets the switch with `CapturePanelDataState` (before
  `FActiveFrame` moves, so it stores under the *outgoing* key) and `RestorePanelDataState`
  (after, refilling `cboLoadedFilename` with `FireEvent := False` and re-adding the
  incoming panel's overlays).
- `ClearLoadedDataFiles` clears the showing panel only (the Clear Data button);
  `ClearAllLoadedDataFiles` clears every panel and is what a model change uses.

### Simulation metadata (`@simulate`, `@plot`, `@scan`, `@steadystate`)

A model may describe the experiments run on it, in an Antimony block comment whose
first non-whitespace character is `@`. The format, its conformance rules and Iridium's
own documented behaviour are specified in `..\..\Antimony_MetaData_Support\`
(`simulation-metadata-spec.md`, `implementation.md`, `HANDOFF.md`) — read the spec
before changing behaviour, since §13 records Iridium as the reference implementation.

**A block is a library of presets, not a script.** It fills a panel's controls and
*never* causes a computation; the user still presses the panel's own compute button.
This is the rule everything else follows from.

The single exception is **Metadata ▸ Run Experiment**, a submenu of the block's
experiments by label: picking one switches to the panel owning that task kind, applies it
and computes. That is still the user asking for a result by clicking something that says
it computes — the rule being protected is that *applying* never computes, so nothing
happens behind the user's back. It exists because the alternative loop was Simulate →
Reload settings → Simulate: the notice bar's offer only appears once a reload has noticed
the block changed, making the first run pure overhead. **`RunExperiment` loads the model
BEFORE applying** (`EnsureLoaded`, then `ApplyLabel`, then compute) — applying first would
validate the preset against the outgoing model and let the reload rebuild the selectors
underneath it, which made an edited block need running twice. The submenu is rebuilt at
the end of every `ParseMetadata`, and the `Metadata` menu's own `OnClick` re-parses so the
labels describe what is in the editor now.

**Where the block asks for something Iridium can do, Iridium must do it.** The block is
authoritative and the export must not diverge from what the app draws — a file that
produces different figures in Iridium and Tellurium is worse than either behaviour alone.
This is why `time` is accepted on a `@plot`'s `y`: it belongs to no observable category,
so it was silently dropped from the selection while the SED-ML export honoured it. It is
now in `Known` (`ApplyPendingYSelection`), in `ValidIds` (the prune in
`PopulateAxisSelectors`) and has its own `Time` row in the Y list — all three, or the list
denies a selection the panel is honouring.

- **`uMetaSymbolProvider.pas`** — `ISymbolProvider` over the loaded model. This is the
  decoupling point: the library must not link to libRoadRunner, so the validator asks
  this interface whether a name exists. Passing `nil` disables symbol checking, which
  is what a syntax-only check before a model is loaded needs.
- **`uMetaExperiments.pas`** — groups the flat command list into *experiments*. Each
  task command (`@simulate` / `@scan` / `@steadystate`) opens one; the `@plot` and
  `@output` commands whose `source` resolves to it attach to it. Each experiment routes
  to the panel owning its task kind. Unusable commands are **kept**, with the reason
  attached, and shown in the selector — that is how conformance C5 is met.
- **`TfrmMain`** owns the `TSimulationMetadata` and the experiment set, both rebuilt
  wholesale on every parse. Nothing may cache either across one; key anything you
  remember on an experiment's **label**, never its index or address.

Rules that are easy to break:

- **Parse on every reload; apply only on model open.** `EnsureLoaded` reloads after any
  edit, so re-applying there silently overwrites what the user has just typed. Where a
  reload shows the block itself changed, the notice bar *offers* it.
- **RoadRunner spells a floating species `[A]`; the model file says `A`.** Translate at
  every boundary between metadata text and model identifiers — `CanonicalModelName` in
  `uMetaSymbolProvider`. Iridium's interior uses the RoadRunner form throughout (it is
  what selection lists, result `columnHeader`s and `TPlotSeries.Name` are keyed on), so
  translate at the edge and store the model's form. This failed silently in three
  separate places; a name-matching bug that reports "the names are all there but nothing
  matched" is almost always this.
- **A preset applied before the model loads must be replayed, not lost.** Iridium loads
  lazily, so between opening a file and the first compute there are no names to validate
  against and no lists to render into. The time-course frame holds a pending Y/X request;
  the scan frame holds a pending experiment label and re-applies on reload.
- **`—  (my own settings)` re-captures on the way out.** The snapshot behind that row is
  taken before the first preset *and again whenever the user leaves it*, because while
  it is selected the panel is their settings. Capturing once discards everything they do
  while on `—`.
- **`@plot` styling composes over the user's baseline, not over the previous `@plot`.**
  Each compute restores the captured user styling, then overlays only the keys this
  command wrote. Without the rebase, styling accumulates across experiments and no file
  fully describes its own figure. Apply it *after* `PlotEndRebuild`, which would
  otherwise undo it.
- **`@steadystate`'s solver keys have no panel controls** — they are engine settings,
  applied to RoadRunner at Compute, and the panel shows a generated summary of what that
  will do. Anything the block asked for and did not get is reported: a steady state solved
  without the requested pre-simulation is a different answer, not a near miss. Its
  `observables:` is the exception and is *not* applied to the engine there: it is a panel
  control now, so it fills the Observables checklist and the checklist drives the engine.
  Applying it from both ends would let the two disagree the moment a box was ticked.

- **While an experiment is selected, the engine holds its `set:` values.** They go in when
  it is applied (`ApplySelectedSetValues`) and come out when it is left — the `—` row,
  another experiment, an unload — *not* around each run. Applying per-run is the obvious
  design and is wrong twice over: the sliders go on showing the un-set model, and moving
  one has no visible effect because the next compute writes the file's value straight back
  over it. With the values resident, a slider explores the model **from** the experiment's
  conditions and the experiment stays selected. Boundaries: an unload **drops** the restore
  data without applying it (it describes an engine that no longer exists), and a reload
  re-applies from scratch (the rebuilt engine no longer has the values, and the old restore
  data is stale).

- **`uMetaSelector.pas`** — the dropdown itself, shared by all three analysis panels:
  the `—` row, label-keyed reselection across a re-parse, refusing to apply an unusable
  experiment while still showing it, and event suppression during a rebuild. Only what
  "apply" *means* differs per panel, which is why that is the `OnApply` callback and
  everything else lives in the helper. `OnApply`'s `AWasUnset` says the `—` row was
  selected immediately before — i.e. that the panel currently holds the user's own work
  and must be re-captured before the preset overwrites it. A panel needing more in the
  strip (steady state's summary line) adds to `Host` and raises its `Height`.

`MetadataDemo.ant` in the repo root exercises the lot, including the two deliberate
failure cases: an `@plot` inside a prose comment (which is *not* a metadata block, and
must warn) and an `@bifurcation` (known to the format, unsupported here).

An experiment needs a **task** command. A `@plot` with no `@simulate` / `@scan` /
`@steadystate` before it and no `source:` naming one has nothing to attach to, so no
experiment is created and the panel looks as though the block was never read. That case is
now recorded in `TMetaExperimentSet.Skipped` with the reason rather than dropped, per C5.

### Exporting (Python/Tellurium, SED-ML, COMBINE)

`ufMain.ExportSedML` / the OMEX equivalent build the SBML first (`BuildSbml`), then drive
`TSedMLExporter`. Two things about SED-ML that are easy to get wrong and that only an
external validator will tell you about:

- **An optional attribute must be absent, not empty.** `name=""` on a `<plot2D>` (an
  `@plot` with no `title:`) fails schema validation and makes the document unreadable to
  Tellurium. Emit whole attributes via `XmlOptAttr`, never `name="%s"`.
- **Variable targets are XPaths using an `sbml:` prefix**, so the document must declare
  that namespace *and it must match the SBML being exported* — otherwise the targets
  resolve to nothing. `TSedMLExporter.SbmlNamespace` carries it, and `TfrmMain` sets it
  from the real document via `SbmlRootNamespace`, which reads the `xmlns` off the `<sbml>`
  root rather than assuming a level and version.

### The Text tab

The output area's Text tab shows the active panel's results as text. `TfrmMain.BuildTextView`
is the only thing that fills `moTextView`, and **every path must go through it** — writing
`Plot.ExportCSVSeriesAsString` straight into the memo is what emptied it whenever the
steady-state panel was showing, since that panel has no plot series to export.

- A panel implements `ITextViewProvider` when its results are grids rather than curves;
  otherwise the plot export is the fallback and the panel needs no code at all.
- **`GetTextView` must stay free of side effects.** Merely selecting the tab calls it, so
  it must not re-render the panel behind it. Adopting a new precision is the separate
  `SetDisplayDecimals`, called only when the user actually turns the decimals spin — which
  keeps the shell's spin and a panel's own decimals control in agreement.
- A panel whose results can change while the tab is visible calls
  `IAnalysisContext.RefreshTextView`, which rebuilds only when that tab is showing.

### BioModels search

A search box at the top right of `Layout4` (the toolbar strip): type ≥3 characters, matching
models drop down beneath it, clicking one downloads its SBML, converts it to Antimony and
loads it through the same sequence as Import SBML (`Unload` → `ClearPlotAndLoadedData` →
`SetText` → `ClearDirty` → `ParseMetadata(True)`). Built entirely in code in
`TfrmMain.CreateBioModelsSearch`. Points worth knowing before changing it:

- **The dropdown is a child of the form, not of the strip** — a child of a 50-pixel-tall
  layout would be clipped to it. It is positioned under the box by `PositionBioList` and
  hangs from its right edge so it cannot run off the window.
- **Both the search and the download run off the UI thread** (`TTask.Run` +
  `TThread.Queue`). The first search downloads the whole cache document. Searches are
  serialised — one running, at most one queued (`FBioSearchRunning` / `FBioSearchAgain`) —
  and a reply whose term no longer matches what is typed is discarded. The download builds
  its own `TBiomodelsCache` so it never shares an HTTP client with a running search.
- **Typing is debounced** (350 ms) and `FBioSuppressSearch` brackets programmatic writes to
  the box: `OnChangeTracking` cannot tell an assignment from a keystroke, so the
  "Loading..." message would otherwise be searched for.
- Status rows ("Searching...", "No models match...") carry no id in `TagString`, which is
  what makes clicking them do nothing.

### Notes when editing

- `.fmx` files are FireMonkey form designers paired with their `.pas`; `ufMain.fmx` is very
  large (multi-MB). Edit form structure through the matching `.pas` declarations where possible.
- **A `TFrame` descendant must have a `.fmx`, even an empty one.** `TFrame.Create` calls
  `InitInheritedComponent` and raises `EResNotFound` without one — which surfaces as a
  startup access violation once `FormCreate` has bailed out half-constructed.
  `uFrameMetadata.fmx` is four lines and deliberately so; do not delete it.
- Controls added in code (the metadata selectors, the notice bar above the plot, the
  `Metadata` menu, the BioModels search box and its dropdown) will not appear in the IDE
  designer. That is deliberate — it avoids surgery on the multi-megabyte `ufMain.fmx` —
  but it is worth knowing before going to look for them there.
- **Analysis panels scroll.** `uFrameParameterScan` has a `TVertScrollBox` in its `.fmx`;
  the time-course and steady-state frames build the equivalent at construction
  (`InstallScrollBox`), re-parenting their designed content into it so a short form
  scrolls rather than clips. `UpdateContentHeight` keeps the content as tall as its
  Top-aligned children but never shorter than the viewport, and must be called again
  whenever a row appears or disappears — the metadata strip's visibility is toggled by
  `TMetaExperimentSelector.Rebuild`, so both frames call it after a rebuild.
- **A slider's four track properties move together.** `Min`, `Max`, `Frequency` and
  `Value` are only ever set through `SetTrackRange` (`uFrameSliderContainer.pas`);
  assigning any of them on its own is a bug. `Frequency` is the step the track snaps to,
  and the snap happens when `Value` is assigned — so `Value` goes last, after the grid it
  will be quantised against is in place. Setting it first rounds it onto the *previous*
  range's grid, which is how a value of 10 became 9.9 and 0.35 became 0.5024. The range is
  ×10 / ÷10 around the value (`RangeAround`), over **198** steps rather than 200 so the
  centre value lands exactly on a step: the span is 9.9 v, and v is 18 steps above `Min`.
  Refresh and reset both re-centre through this when a value falls outside the range,
  rather than clamping it — a clamped track is not written back to the engine, so the
  slider would show one number while the model ran at another.

- **A styling snapshot carries the axis title text.** `PlotEndRebuild`'s `RestoreSettings`
  therefore puts the previous X column's label back over the one `PlotData` just set.
  `TfrmMain` tracks the auto-derived title (`FAutoXTitle` / `FSnapshotAutoXTitle`) and
  reinstates it only when the restored text is still the old auto value — so a title the
  user typed, or one an `@plot xlabel:` set, is left alone.
- `__history/` and `__recovery/` are IDE backup folders (git-ignored) — ignore them.
- Compiler binaries (`*.dcu`, `*.exe`, `*.dll`, `*.local`, `*.cfg`) are git-ignored.
