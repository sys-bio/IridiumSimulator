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

- `..\..\libAntimonyAPI\` — the maintained libantimony wrapper (`uAntimonyAPI.pas`,
  `uAntimonyRaw.pas`, `uAntimonyTypes.pas`). **This replaced the old in-repo
  `uAntimonyAPI.pas`, which is gone.** `TModelErrorState` now lives in `uAntimonyTypes`
  and nowhere else. Two things to know: the raw unit binds ~135 entry points and
  **refuses to load if any is missing**, so an older `libantimony.dll` makes the app halt
  at start-up with "Unable to find the Antimony library" — the matching DLL is the one in
  `libAntimonyAPI\binary\lib\`. And `freeAll` must be called after a batch of queries,
  because the library leaks without it.
- `..\..\RateLawChecker\` — the rate law checker (`RateLaw.*.pas`), with its own console
  test harness. **RTL-only by design**: nothing in it may reference FMX, libantimony or
  libRoadRunner, which is what lets it be tested without a GUI or a DLL and reused later.
  Iridium contributes exactly two things — `uRateLawModelSource.pas`, an `IModelSource`
  over libantimony, and the UI in `ufRateLawOptions.pas`. See **Rate law checking** below.

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

### Rate law checking

`btnModelChecker` and the code-built **Check** menu run the checker and write a report to
the Text tab. The engine lives in `..\..\RateLawChecker\`; read
`specification_rate_law_checker_iridium.md` before changing behaviour.

The one idea everything follows from: **the set of rate laws checked is data, not code.**
A law is a JSON registry entry giving a canonical expression, the roles of its symbols, and
the behavioural invariants it guarantees. Adding a law must need no new checking code — if
a defect class ever has to ask *which* law it is looking at, that is a failure of the design
and should be logged as one.

Things that are easy to get wrong:

- **The adapter snapshots; it does not query lazily.** libantimony has one current model per
  process, and Iridium reloads it constantly — `EnsureLoaded` on every edit, SBML import,
  BioModels download. A source that called through on each method would answer about
  whatever was loaded most recently. `TAntimonyModelSource`'s constructor loads, copies
  everything out, calls `freeAll`, and never touches the library again.
- **The checker reads the editor, not the session.** A model with a malformed rate law is
  exactly what this is for, and such a model often will not load into RoadRunner at all.
- **Two trees are kept for every expression, pre- and post-canonicalisation.** A misplaced
  parenthesis is visible only before normalising (normalising is what erases it); a
  duplicated operand only after. Both are needed and neither is redundant.
- **Names bind last, not first.** Parameter roles are bound by *shape* — every permutation
  scored against the law — and the name only breaks ties. Binding `Km` to the `Km` role
  first cannot detect a transposition, because that binding is exactly what undoes it.
- **The behavioural half is opt-in** (`Check ▸ Rate Laws and Options...`). It samples each
  law over a grid, so it is orders of magnitude more work, and the report says which half
  ran — otherwise "no problems found" overstates what was looked at.
- **Adding a rate law is a JSON file, not code.** `..\..\RateLawChecker\RATE_LAW_MANUAL.md`
  is the authoring guide, reachable in the app from **Help ▸ Rate Law Help**: the schema, the ten invariant types, families such as mass
  action, annotations, and what each rejection code means. If a new law ever needs an
  engine change, that is a defect in the engine and worth recording as one.
#### Where it stands, and how to work on it

Milestones **M0–M14 are built**; `specification_rate_law_checker_iridium.md` §18 carries the
status table, the measured numbers, and — more useful — the deviations and findings, including
every place the design had to give. Read §18.3 and §18.4 before changing engine behaviour.

**M17 is done and its findings have been acted on.** The first run over the 1075 curated
BioModels had **33% of models reporting an error**, and since BioModels is curated those
were false positives. After the fixes it is **19%**, with silent models up from 35% to
53% and association up from 17% to 39%. §18.6 has the original numbers and the six causes;
**§18.7 has what fixing them cost and bought, and is the more useful of the two.**

The lesson §18.7 records, worth having before touching this code: **four of the six causes
were things every SBML model contains that no part of the engine had heard of** — the
compartment volume factor, `EmptySet`, `time`/`pi`, and a clamped species. Each was
therefore treated as an ordinary identifier and bound to a kinetic role. Before adding a
check, ask what it does with those four.

**Do not read the synthetic corpus as evidence the checker is ready.** It stayed green
through every one of these fixes, including the two passes that made the real numbers
*worse*. Its cases are expressions written as laws; real models are SBML, and the two
disagree about what a rate law looks like. Re-run the corpus after any engine change.

**One capability was deliberately given up.** Mutation detection is 57/62, not 61/61: the
subset admission that caught a dropped term by inference is gone, because it accused a
correct model every single time it fired (659 findings, none right). The case still fires
when the reaction is annotated. Do not restore that admission to make the number go back up.

**§18.8 said where the errors came from and §18.9 acted on it**: 52% were reactions whose
rate law depends on a **modifier** rather than its substrate. Two entries close it —
`catalytic_mass_action` and `modifier_proportional` — and the second is not optional: adding
the first alone moved errors the *wrong* way, 16.6% → 21.3%, because a reaction that is zero
order in the substrate it consumes then matched a law insisting on a substrate term.

**A law may declare `"association_floor"`.** Looseness is a property of the law, not of the
registry: "k times some species" sits near a great deal, and the two catalytic entries
declare 0.08 so they claim only near-exact matches. This is *not* the global floor §18.8
refused to lower — a ceiling on a law with no defect class of its own to catch is a
different thing. A declared ceiling gates the same-symbols admission too, which is how
`alpha1/(1+V^3)` was being claimed at **d = 1.000**.

**Antimony has almost no declared modifiers**, and this will bite anything that asks for
them. It records one only where the modeller drew an interaction arrow, and `sbmlToAntimony`
does not create arrows from SBML's `listOfModifiers`. Use `EffectiveModifiers` in
`RateLaw.Generative`, which infers them, and note it lives in the engine rather than in
Iridium's adapter on purpose: in the adapter, the fixture and real models would behave
differently.

**Do not try to normalise a lumped rate constant.** `IXa*VIIIa/r26_c` is mass action with
k = 1/r26_c, and collapsing constant factors so a role can bind to it was implemented and
removed: it hides the defects that consist of matching the wrong law. §18.8 has the three
variants tried and what each cost — the worst took detection from 57 to 35. Same wall as
the association floor, from the other side.

Still open: the rest of **M15** (a defect-code reference and a worked walkthrough; the
authoring manual itself is done), **M16** (Layer 3 simulation checks, `D101`–`D106`), and
the association floor question — `S011` (247 models) and `S006` (181) are what is left, and
no global threshold separates them from the founding defect at d=0.125.

Build and test, from `..\..\RateLawChecker\`:

**`-NUdcu` is not optional.** Without it `dcc64` writes its DCUs beside the
sources, and that directory is on Iridium's unit search path — so the next
Iridium build finds compiled units there and uses them instead of recompiling
the `.pas`. The symptom is the IDE silently ignoring changes to the checker:
edit a law, rebuild, and yesterday's registry is still what runs. If that
happens, delete `..\..\RateLawChecker\*.dcu` and
`Win64\Debug\dcu\*.dcu`, then rebuild.

```
dcc64 -B -NUdcu RateLawChecker_Project.dpr  (after sourcing rsvars.bat)
RateLawChecker_Project                      the whole suite
RateLawChecker_Project -coverage            the mutation matrix, per law
RateLawChecker_Project -laws                every registered law and whether it validates
RateLawChecker_Project -check <case>        one corpus case, in full
RateLawChecker_Project -bind <case>         which law each candidate binds to, and how far off
RateLawChecker_Project -expr "<expr>"       parse one expression, both trees
```

and from `Win64\Debug`, against real models:

```
CheckAntFile *.ant                      one line per file
CheckAntFile -report <model.ant>        exactly what the Text tab shows
CheckAntFile -laws <folder> <model>     with a folder of your own .json laws
```

**Baseline as of 2026-08-28, after the M17 fixes and the Hill/catalytic entries — a change that moves these down is a
regression, with the one documented exception noted in the table:**

| | |
| :---- | :---- |
| corpus cases | 51/51 |
| role-binding cases | 8/8 |
| malformed registry entries rejected | 11/11 |
| registered laws, all self-validating | 18 |
| mutation coverage: correct forms left clean | 18/18 |
| mutation coverage: detected at all | 69/83 — the shortfall is deliberate, see §18.7 |
| mutation coverage: classified exactly right | 57/83 |
| Iridium's own `.ant` models | 20/23 associated, 0 errors |
| BioModels: models reporting an error | 159/1013 (15.7%), from 33.1% — see §18.7, §18.9 |
| BioModels: models reporting anything | 339/1013 (33.5%), from 64.9% |
| BioModels: models entirely silent | 674/1013 (66.5%), from 35.1% |
| BioModels: reactions associated | 18271/45319 (40.3%), from 17.1% |

The first and fifth rows matter most. **A correct model reporting anything is worse than a
defect being missed** — that is the failure that gets a checker switched off — and the three
unassociated reactions are `Lorenz.ant`, which is not kinetics and correctly matches nothing.
The exact-classification rate falls as laws are added and will keep falling; most misses are
`S002` between laws that differ in one place, where refusing to guess is right.

- `CheckAntFile.dpr` builds a console tool into `Win64\Debug` that runs the checker over
  `.ant` files (`CheckAntFile *.ant`). Not part of Iridium; it is how the false-positive
  rate on real models is measured. It also reads **SBML** (`.xml`), converting with
  `sbmlToAntimony` first, and `-csv <prefix>` writes three machine-readable tables
  (files, diagnostics, associations) instead of prose.
  `-md` prints the markdown rendering the GUI's report panel shows, so it can be
  eyeballed and diffed against `-report` without driving the GUI.
- `corpus/` drives the BioModels evaluation (§18.6): `fetch.sh` downloads the 1075-model
  mirror, `run.sh` runs `CheckAntFile` over it and merges the tables, `report.py` prints the
  figures. The corpus itself is ~171 MB and is **not** checked in — fetch it to a scratch
  directory. Two things `run.sh` does deliberately: it chunks the corpus, because
  libantimony is a C++ library behind an FFI and a model that takes it down should cost one
  chunk rather than the run; and it gives each chunk its own CSV prefix, because `-csv`
  appends and parallel writers to one file interleave rows mid-line.

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
- **A new form gets a designed `.fmx`, always.** Not `TForm.CreateNew` with the controls
  assembled in code: that form cannot be opened in the IDE, so it cannot be laid out by
  eye or even looked at without running the app. Saving a file is a maintainer's
  convenience bought at the owner's expense. Register it in the `.dpr` as
  `unit in 'unit.pas' {frmName}` and in the `.dproj` with `<Form>` and
  `<FormType>fmx</FormType>`, or the IDE will not list it. Give buttons real
  `Position.X`/`Y` rather than `Align`, so they can be dragged. Only what genuinely
  cannot be designed — list contents driven by data — stays in code.
  `ufRateLawOptions` is the worked example. **Menus are the exception**: those are still
  built in code, because editing the multi-megabyte `ufMain.fmx` is its own hazard.
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
- **Help documents master in their own project and are COPIED into `Win64\Debug\Help\`.**
  `METADATA_MANUAL.md` lives in `..\..\Antimony_MetaData_Support\` and
  `RATE_LAW_MANUAL.md` in `..\..\RateLawChecker\`; the copies under `Win64\` are a
  deploy target, not a source, and are git-ignored. Edit the master and re-copy — a fix
  made to the deployed copy is lost at the next clean build. **Copy into BOTH
  `Win64\Debug\Help\` and `Win64\Release\Help\`**: the viewer resolves everything
  relative to the running executable, so a document deployed to only one configuration
  is missing from the other and reports itself as not found. Adding a document is an
  entry in `HELP_DOCS` plus a button on the Help tab, and nothing else in the help code
  needs to know.
- `__history/` and `__recovery/` are IDE backup folders (git-ignored) — ignore them.
- Compiler binaries (`*.dcu`, `*.exe`, `*.dll`, `*.local`, `*.cfg`) are git-ignored.
