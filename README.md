# Iridium

<table style="width:100%">
  <tr>
    <td><img alt="Licence" src="https://img.shields.io/badge/License-MIT-yellowgreen"></td>
    <td><img alt="Funding" src="https://img.shields.io/badge/Funding-NIH/NIBIB%20(P41EB023912)-blue"></td>
    <td><img alt="Language" src="https://img.shields.io/badge/Delphi-13-blue.svg"></td>
    <td><img alt="Platforms" src="https://img.shields.io/badge/Platforms-Windows%20%7C%20macOS-lightgrey"></td>
    <td><img alt="GitHub all releases" src="https://img.shields.io/github/downloads/sys-bio/IridiumSimulator/total?color=red&style=plastic"></td>
  </tr>
</table>

**An interactive desktop simulator for systems biology.** Write a model in
[Antimony](https://github.com/sys-bio/antimony), press Simulate, and drag a slider to watch
the answer move. Iridium is built on the [libRoadRunner](https://github.com/sys-bio/roadrunner)
simulation engine and is fully [SBML](https://github.com/sbmlteam/libsbml) compliant.

> This is a new rewrite of the original Iridium Simulator platform.
> Developed at the Sauro Lab, University of Washington, Seattle.

<img src="/Images/iridium1.png" width="80%"></img>

---

## Acknowledgements

I would like to thank Guillermo Canedo Ramirez who did an excellent job in making the 3D bar plotting compoment (https://github.com/gcanedo/T3DBarGraph)

The web version which runs inside the browser can be found at

Source code: https://github.com/sys-bio/WebIridium

GitHub page: https://sys-bio.github.io/WebIridium/

This work is supported by NIH/NIBIB center grant P41EB023912

## Why you might want it

Most simulation work happens in a notebook: a model file over here, a script that simulates
it over there, and a figure that came from some combination of the two that you can no longer
reconstruct. Iridium takes a different position.

- **The model is the document.** One `.ant` file, opened and edited in the app.
- **The experiment lives in the model too.** Iridium can record the runs you did — the time
  course, the scan, the figure — *inside the model file*, in a comment, so it travels with
  the model and no other tool is disturbed by it. See
  [Recording your experiments](#recording-your-experiments-in-the-model) below.
- **Results are never more than a click old.** Nothing recomputes behind your back. You press
  a button and get an answer for the settings you can see on screen.
- **Everything you get on screen, you can take with you** — as a Python/Tellurium script,
  SED-ML, a COMBINE archive, SBML, CSV, or a PDF report.

No installation of Python, no environment to manage, no notebook kernel. It is a desktop
application that starts in a second.

---

## What you can do with it

### Time course simulation

Integrate the model over time and plot any combination of floating species, boundary species,
reaction rates, rates of change, assignment rules, global parameters, compartments,
eigenvalues and scaled elasticities. Set the start, end and number of output points; choose
the observables from a filtered list rather than typing selectors.

### Interactive sliders

Every global parameter and boundary species gets a slider. Drag one and the simulation
re-runs live. The track auto-ranges around the current value (×10 / ÷10), so a parameter is
explorable without you having to decide its bounds in advance, and it re-centres rather than
clamping when a value drifts out of range.

This is the feature that makes a model *feel* like something you can reason about, and it is
the reason the application exists.

### Steady state and metabolic control analysis

Solve for the steady state and report exactly the quantities you ask for — an **Observables**
checklist over species, boundary species and fluxes, so the table shows what you care about
and not a wall of everything.

From there, [metabolic control analysis](https://en.wikipedia.org/wiki/Metabolic_control_analysis):
flux and concentration control coefficients, presented both as a matrix and as an interactive
3D bar chart.

<img src="/Images/iridium3.png" width="80%"></img>

<img src="/Images/iridium5.png" width="80%"></img>

### Parameter scans

Sweep one parameter (or a species' initial value) over a linear range, a log range, or an
explicit list of values, and choose what each point of the sweep contributes:

| Measure | What you get |
|---|---|
| **Time course overlay** | one full trajectory per scan point — the familiar fan of curves |
| **Sample at *t*** | the value at a chosen time, plotted against the scanned parameter |
| **Peak value** | the maximum reached |
| **Time to peak** | when that maximum occurred |

The last three give you a dose–response curve rather than a pile of trajectories.

<img src="/Images/iridium2.png" width="80%"></img>

### Checking your rate laws

Models acquire typos. A `Km` and a `Vmax` transposed, a parenthesis in the wrong place, a
term quietly dropped — the model still simulates, it just simulates something else.

**Check ▸ Check Rate Laws** reads the kinetics in your model and reports what looks wrong.
It works in two halves:

- a **structural** pass, which compares each rate law against a registry of known kinetic
  forms — mass action, Michaelis–Menten, reversible MM, Hill activation and repression,
  competitive / uncompetitive / non-competitive inhibition, ordered bi-bi, and others
  (18 forms ship built in);
- an optional **behavioural** pass, which samples each law numerically and checks the
  invariants that form is supposed to guarantee. It is much slower, so it is opt-in — and
  the report always says which halves ran, because "no problems found" means different things
  in each case.

**Adding a kinetic form needs no code.** A rate law is a JSON file. *Check ▸ Rate Laws and
Options… ▸ Copy Built-Ins* writes the shipped laws into a folder in your home directory;
copy one, edit it, and it is picked up on the next check without restarting. The in-app
**Help ▸ Rate Law Help** manual walks through it.

The report lands in the **Reports** tab and can be copied or saved as PDF.

### Finding models

A search box in the toolbar searches [BioModels](https://www.ebi.ac.uk/biomodels/) as you
type. Click a result and it is downloaded, converted from SBML to Antimony, and loaded —
ready to simulate. There are also seventeen built-in example models in the *Example Models*
dropdown, from a three-step pathway to a feedback oscillator, a bistable switch and the
Lorenz attractor.

### Comparing against data

Load a CSV and it is drawn on the same axes as your simulation. Data overlays are durable:
re-simulating, dragging a slider, and re-styling all leave them alone, and each analysis
panel keeps its own overlays — data that describes a time course does not follow you onto a
parameter scan, where it would mean nothing.

### Getting things out

| Export | What for |
|---|---|
| **Python / Tellurium script** | reproduce the run in a notebook |
| **SED-ML** | the standard interchange format for simulation experiments |
| **COMBINE archive (.omex)** | model + experiment + everything, in one file for a journal |
| **SBML** | the model itself |
| **CSV** | the numbers, from the plot or the grids — including files an `@output` command names |
| **PDF** | the rate law report |

The exports are held to the standard, not just to what round-trips through Iridium — SED-ML
output is written so that Tellurium and external validators accept it.

---

## Recording your experiments in the model

This is the part of Iridium with no real equivalent elsewhere, so it is worth a section.

An Antimony file describes a model. What it does not describe is *what you did with it*.
Six months later you open the model and cannot remember whether Figure 3 used `Vmax = 1.0`
or `Vmax = 2.5`. Send it to a colleague and they get the model but not the experiment.

Iridium lets you write the experiment into the model file itself, in an ordinary comment:

```
/*
@simulate wt: { timestart: 0, timeend: 20, points: 500 }

@scan doseresponse: {
    source: wt,
    parameter: X0,
    start: 0.1, end: 10, points: 50,
    observables: [S1, S2],
    measure: { sampleat: 1.0 }
}

@plot: {
    source: doseresponse,
    y: [S1, S2],
    title: "Dose response at t = 1.0",
    xlabel: "X0",
    ylabel: "concentration",
    legendposition: topright
}
*/
```

Because it lives in a comment, the file is still an ordinary Antimony model and every other
tool ignores it.

Open that model in Iridium and the experiments appear in a dropdown on each analysis panel.
Pick one and it **fills in the controls** — the scan range, the observables, the time
settings, the figure's title and axis labels. It does not run anything: a metadata block is
a library of presets, not a script, so you still press the panel's own compute button and
nothing ever happens behind your back. (The one exception is **Metadata ▸ Run Experiment**,
which is you explicitly asking for a result.)

It works in the other direction too. Set a panel up by hand, press the **`@sim`** button, and
Iridium writes the block describing what you just did — including the plot title, axis
labels, log axes, font sizes, legend position and the series colours currently on screen —
and appends it to your model.

The notation is documented in full in the app under **Help ▸ Simulation Help**, and
`MetadataDemo.ant` in this repository exercises the whole format, including its deliberate
failure cases.

---

## Getting started

1. Download a release for Windows or macOS, or build from source (see below).
2. Start Iridium. It opens with a small example model already in the editor.
3. Press **Simulate**.
4. Drag a slider.

From there: try *Example Models* for something more interesting, or type three characters
into the BioModels search box and pull down a published model.

The editor has Antimony syntax highlighting, line numbers and an adjustable font, and
highlights metadata blocks distinctly from ordinary comments. Three manuals ship with the
application and are readable inside it — **Help ▸ Antimony Help**, **Simulation Help** and
**Rate Law Help**.

Iridium remembers your window size and position, your panel layout, and your recent files
between sessions (**File ▸ Load Recent Files**). Preferences live in a small JSON file:
`%APPDATA%\Iridium\preferences.json` on Windows,
`~/Library/Application Support/Iridium/preferences.json` on macOS.

---

## For developers

Iridium is a [FireMonkey](https://www.embarcadero.com/products/rad-studio/fm-application-platform)
(FMX) desktop application written in Delphi, targeting **Delphi 13 / RAD Studio 37.0**, for
**Win64** and **macOS ARM64**. There is no `make` or `npm`; build with the RAD Studio IDE or
the command-line compiler.

### Step 1 — install the components

**Iridium will not open or compile until these three components are installed in the IDE.**
Each lives in its own repository and carries its own installation instructions; follow those
first, then come back here.

| Component | Class | Used for |
|---|---|---|
| [Plotting component](https://github.com/hsauro/Plot2DComponent) | `TSkPlotPaintBox` | every chart in the application |
| [Markdown viewer](https://github.com/hsauro/RhoMarkdownViewer) | `TRhoMarkdownViewer` | the Help tab and the Reports tab |
| [Code editor](https://github.com/hsauro/RhoSkiaCodeEditor) | `TSkiaCodeEditor` | the Antimony editor |

These are all design-time packages: once installed they appear on the component palette, which is
what lets `ufMain.fmx` and the analysis frames open in the form designer. If you open the
project without them, the IDE will report unknown component classes and offer to drop the
controls from the forms — **say no**, and install the components instead.

Nothing else needs installing. The application also uses Skia, which RAD Studio 13 ships
with, and the 3D bar graph in Step 2 is created at runtime rather than dropped on a form —
so neither costs you an extra package.

### Step 2 — check out the sibling projects

The rest of the dependencies are **source**, not installed packages. They are referenced by
relative path from `IridiumSimulator.dpr`, so they must sit beside this repository in the
expected layout rather than being copied in — a fix in one is then a fix everywhere it is used.

Every one of them must sit **beside** this repository, in the same parent folder, under
exactly these names — the paths in the `.dpr` are a single `..\`, so a differently named or
differently placed checkout will not build.

| Folder | What it provides |
|---|---|
| [`libRoadRunner_Delphi_Bindings`](https://github.com/sys-bio/libRoadRunner_Delphi_Bindings) | the RoadRunner Pascal wrapper |
| [`libAntimony_Delphi_Bindings`](https://github.com/sys-bio/libAntimony_Delphi_Bindings) | the libantimony wrapper |
| [`ModelCheckerLib`](https://github.com/sys-bio/ModelCheckerLib) | the model checking engine (currently rate laws) |
| `Antimony_MetaData_Support` | the simulation-metadata parser, writer and exporters |
| `RhodyComponents` | the source of the Step 1 components — the `.dpr` compiles ~12 units from `RhodyComponents\PlottingComponent\Source\` directly, so the tree must be present even though the components are also installed |
| `T3DBarGraph-main` | the 3D bar graph for control coefficients |

Note the `-main` suffix on `T3DBarGraph-main`: that is the folder name a GitHub zip download
produces, and it is what the `.dpr` expects.

`T3DBarGraph-main` is a source dependency rather than an installed component: `ufBar3DWindow`
constructs the bar graph in code and docks it, deliberately, so that no `.fmx` references the
unit and the IDE never has to load its package.

Three parts of the codebase — the metadata library, the model checker, and the preferences
unit — are **RTL-only by design**: nothing in them may reference FMX or libRoadRunner. That is
what lets them be tested from a console harness without a GUI or a DLL, and reused by other
tools later.

### Step 3 — build

```
"C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat" && ^
  msbuild IridiumSimulator.dproj /t:Build /p:Config=Debug /p:Platform=Win64
```

`rsvars.bat` sets up the compiler environment and must be sourced first. Output lands in
`Win64\Debug\`. A successful compile ends with a line like
`NNNNN lines, N.NN seconds, ... bytes code`. Some external numeric units emit a large number
of harmless hints; these are expected and are not build failures.

### Runtime dependencies

Two native libraries are loaded at startup and the application will not function without
them:

- `libantimony.dll` / `libantimony.dylib` — the Antimony parser.
- `roadrunner_c_api.dll` — the simulation engine.

The Antimony wrapper binds ~135 entry points and refuses to load if any is missing, so an
older `libantimony.dll` will halt the application at startup with *"Unable to find the
Antimony library"*. Use the matching DLL that ships with the wrapper project.

### Source layout

This repository holds the application itself: the shell form (`ufMain.pas`), the analysis
frames, the model session, and the metadata, preferences and rate-law-checker glue.
Everything else comes from the components in Step 1 and the sibling projects in Step 2.

### Architecture in one paragraph

A **shell form**, a set of **swappable analysis frames**, and one **shared model session**.
The shell owns the single `TRoadRunner` instance, the plot, and the slider panel; each
analysis panel is a `TFrame` that reaches shared services only through a narrow
`IAnalysisContext` interface and never refers to the main form directly. The session
broadcasts load and reload events, so panels repopulate themselves when the model changes.
When a panel needs something new from the shell, the interface is extended rather than the
boundary crossed.

`CLAUDE.md` in this repository is a detailed architecture and conventions guide — the
invariants that are easy to break, and why they exist. Read it before making non-trivial
changes.

### Testing

There is no automated test suite for the application itself; verification is manual — load a
model and exercise the analysis panels. The RTL-only sibling projects (metadata support and
`ModelCheckerLib`) *do* have console test harnesses, and so do the two wrapper projects.

The model checker is additionally measured against the **1013-model curated BioModels
corpus**. As of the 2026-08-28 baseline: 15.7% of models report an error (down from 33.1%),
66.5% are entirely silent (up from 35.1%), and 40.3% of the 45,319 reactions are associated
with a known rate law (up from 17.1%). Because BioModels is curated, a reported error is
usually a false positive, so the error rate is the number to drive down. `CheckAntFile.dpr`
in this repository is the console harness used for those runs.

---

## Licence

MIT. See [LICENSE](LICENSE).
