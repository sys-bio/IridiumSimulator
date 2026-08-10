# Recording simulation experiments in an Antimony file

**A user's guide**
Version 1.0 · 9 August 2026

---

## 1. What this is for

An antimony script is used to describe a model, for example:

```
J1: -> S1; Vmax * Km / (Km + S1)
 S1 -> S2; 0.3 * S1
 S2 ->   ; 0.2 * S2

 S1 = 1.0;  S2 = 0.5
 Vmax = 1.0;  Km = 0.5
```
What is doens't describe is what you *did* with the model. The purpose of the metadata extension is 
to let you describe the kinds of computational experiments you did with the model.

The time course you ran, the parameter you scanned, the figure you made for
the paper, these normally live in a separate script, or a notebook. Six months later you open the model and cannot remember
whether Figure 3 used `Vmax = 1.0` or `Vmax = 2.5`. Send the model to a
colleague and they get the model but not the experiment.

This notation lets you write the experiment **into the model file itself**. Because the antimony script reader
doesn't understand this new format, the experiment descrption must be put into an Antimony comment, such as:
in a comment:

```
/*
@simulate: { timeend: 50, points: 500 }
@plot: { y: [S1, S2] }
*/
```

Because it lives in a comment, the file is still an ordinary Antimony model.
Every existing tool will ignore it. This addition doesn't break other tools that use Antimony.

For who you are familair or know of SED-ML, the experiment descrptions can also be exported as SED-ML, this
means you can repeast the sme expeiment in tools like Telluirum, Copasi, or VCell.

### What you get from it

- The model and the experiment travel together, as one file.
- A data file you export can carry its own provenance, so a CSV found in
  two years still says which model and which settings produced it.
- The experiment can be turned into a Python script, or into SED-ML and a
  COMBINE archive for a journal deposit.

### What it is not

It is **not a programming language**. There are no variables, no loops, no
arithmetic. If something needs computing, it belongs in the model as an
assignment rule, or in a real script. §10 says what to do when you hit that
wall.

---

## 2. Five minutes to your first experiment

Here is a complete model file. The top half is ordinary Antimony; the
bottom half is the experiment.

```
J1: -> S1; Vmax * Km / (Km + S1)
 S1 -> S2; 0.3 * S1
 S2 ->   ; 0.2 * S2

 S1 = 1.0;  S2 = 0.5
 Vmax = 1.0;  Km = 0.5

/*
@simulate: { timeend: 50, points: 500 }

@plot: {
  y: [S1, S2],
  title: "Wild type",
}
*/
```

That is the whole thing. Run a time course from 0 to 50, then draw `S1` and
`S2` against time.

Three rules will get you most of the way:

1. The block starts with `/*` and the **first thing inside it must be `@`**.
2. Every setting is `name: value`, and settings are separated by **commas**.
3. Command names start with `@`.

---

## 3. Writing it down

### 3.1 Where the block goes

Anywhere in the file. Before the model, after it, or split across several
blocks — they are read in the order they appear and behave as though joined
together.

The **only** rule is that the first non-blank character inside `/*` is `@`:

```
/*
@simulate: { timeend: 10 }
*/
```

This does **not** work, and is the single most common mistake:

```
/*
Model of yeast glycolysis, reduced form.
@plot: { y: [S1] }          <-- ignored: the block starts with 'Model'
*/
```

A block that begins with prose is just a comment. Put your description in
`@meta` (§5), or keep the prose in a separate comment block.

Good tools will warn you when they spot this, because otherwise it fails
completely silently.

### 3.2 Commas, colons, quotes

- **Commas separate things.** They are required between settings and
  between list items.
- **A trailing comma is fine** before a closing `}` or `]`. This is
  deliberate — it means adding, removing and reordering lines costs one
  line each, with no punctuation to fix up.
- **The separator is always `:`**. `=` is not accepted anywhere.
- **Text goes in quotes**, single or double: `"Wild type"` or `'Wild type'`.

```
@plot: {
  y: [S1, S2],
  title: "Wild type",       // trailing comma: perfectly legal
}
```

### 3.3 Comments inside the block

Use `//` to the end of the line:

```
@plot: {
  y: [S1, S2],   // S3 left out, it swamps the others
}
```

You cannot nest `/* */` inside the block.

### 3.4 Numbers

All of these work: `0`, `50`, `0.5`, `.5`, `5.`, `-2`, `1e-9`, `1.0E+3`.

Settings that count things — `points`, `steps`, `precision`, `maxiter` —
need whole numbers. Writing `points: 10.5` is an error rather than being
quietly rounded, because a silently altered count is worse than a message.

### 3.5 Names

Species, parameters and reaction names are written plainly, with no quotes:
`y: [S1, S2]`, `parameter: Vmax`.

**Words used by this notation are not reserved.** If your model has a
species called `time`, or `cross`, or `log`, it still works — the model
always wins. Where that could surprise you, a tool will say so:

```
warning: 'time' in @plot x: refers to the species 'time' declared in the
  model, not to simulation time. This will produce a phase portrait.
```

Names **are** case-sensitive, here as in Antimony. `logy` is a setting;
`logY` is not.

---

## 4. Connecting commands together

Most files have one time course and one plot, and you can leave everything
implicit — a `@plot` with no `source:` uses the task just above it.

When there is more than one task, give them **labels** and refer to them.
A label goes between the command name and the colon:

```
@simulate wt: { timeend: 50, points: 500 }
@simulate mutant: { timeend: 50, points: 500 }

@plot: { source: wt, y: [S1] }
```

Rules:

- Labels are ordinary names and must be unique in the file.
- `source:` can take a list, to overlay results: `source: [wt, mutant]`.
- If you leave `source:` out and there is exactly one task before, that one
  is used.
- If you leave it out and there is more than one, that is an error — the
  message lists the labels available, so you can pick.

Labels are also how the exported Python script names its variables, so
short meaningful ones (`wt`, `sweep`, `ss`) pay off twice.

---

## 5. `@meta` — describing the work

Optional, and worth the thirty seconds. These fields are what make an
exported data file self-describing later, and they become the Dublin Core
metadata in a COMBINE archive.

| Setting | Type |
|---|---|
| `title` | text |
| `author` | text |
| `description` | text |

```
@meta: {
  title: "Glycolytic oscillations in yeast",
  author: "H. Sauro",
  description: "Reduced model, parameters from Teusink 2000.",
}
```

---

## 6. `@simulate` — running a time course

| Setting | Type | Default | Notes |
|---|---|---|---|
| `timestart` | number | `0` | |
| `timeend` | number | **required** | must be greater than `timestart` |
| `points` | whole number | `100` | not with `steps` |
| `steps` | whole number | — | not with `points` |
| `solver` | name | tool's choice | e.g. `cvode`, `gillespie` |

```
@simulate wt: {
  timestart: 0,
  timeend: 50,
  points: 500,
  solver: cvode,
}
```

### `points` or `steps` — pick either

Both terms are in common use and both are supported. They differ by one:

```
points = steps + 1
```

```
@simulate: { timestart: 0, timeend: 10, points: 101 }   // 101 rows, 0.0 … 10.0
@simulate: { timestart: 0, timeend: 10, steps:  100 }   // exactly the same
```

Write whichever you already think in. Using **both in one command** is an
error, because they would contradict each other.

Both endpoints are always included in the output.

> `timestart` and `timeend` are spelled out in full because `start` and
> `end` mean something different in `@scan` (§9). The longer names let each
> command be read on its own.

---

## 7. `@plot` — drawing it

| Setting | Type | Default | Notes |
|---|---|---|---|
| `source` | label(s) | the task above | |
| `x` | name | `time` | |
| `y` | name or list | **required** | |
| `type` | `line` `scatter` `line+marker` `bar` | `line` | |
| `title` | text | — | |
| `xlabel`, `ylabel` | text | — | |
| `grid` | true/false | `false` | sets both axes |
| `gridx`, `gridy` | true/false | `false` | |
| `logx`, `logy` | true/false | `false` | |
| `series` | object | — | §7.2 |
| `file` | text | — | §7.3 |

`y` accepts a single name or a list, so both of these are fine:

```
@plot: { y: S1 }
@plot: { y: [S1, S2] }
```

### 7.1 Phase portraits come free

`x` defaults to `time`. Set it to a species and you get a phase portrait:

```
@plot: { x: S1, y: S2, title: "Limit cycle" }
```

### 7.2 Styling individual curves

`series` maps a plotted quantity to how it should look:

```
@plot: {
  y: [S1, S2],
  series: {
    S1: { color: #1f77b4, line_width: 2 },
    S2: { color: #d62728, line_style: dashed, marker_style: circle },
  },
}
```

| Style setting | Values |
|---|---|
| `color` | `#rrggbb`, `#rrggbbaa`, or a colour name |
| `line_style` | `solid` `dashed` `dotted` `dashdot` |
| `line_width` | number |
| `marker_style` | `none` `circle` `square` `triangle` `diamond` `cross` |
| `marker_size` | number |
| `type` | overrides `type` for this one curve |

Hex colours always work. Names like `red`, `blue`, `orange` are also
accepted, as are the matplotlib defaults (`tab_blue`, `tab_orange` and so
on) — but hex is the portable choice.

Note that `line+marker` is written with no spaces around the `+`.

### 7.3 Saving the figure

```
@plot: { y: [S1, S2], file: "figure1.pdf" }
```

The format comes from the extension: `.png`, `.pdf` or `.svg`. There is no
separate format setting, because everyone already understands extensions.

Saving **adds to** display rather than replacing it — a tool with a screen
draws the plot *and* writes the file. Adding `file:` to capture a figure
should never stop you seeing it.

If a tool cannot write the format you asked for, it may substitute a close
one, but it must tell you both what you asked for and what it produced, and
the file name will always match what is actually in the file.

---

## 8. `@output` — writing the numbers out

| Setting | Type | Default | Notes |
|---|---|---|---|
| `source` | label(s) | the task above | |
| `file` | text | — | omit to write to the tool's panel |
| `format` | `csv` `tsv` `txt` | from the extension | |
| `columns` | list | time + all species | |
| `precision` | whole number | `6` | **significant figures** |
| `header` | true/false | `true` | |
| `comments` | true/false | see below | |
| `notes` | text | — | your own commentary |

```
@output: {
  source: wt,
  file: "wildtype.csv",
  columns: [time, S1, S2],
  precision: 8,
}
```

`precision` is in **significant figures**, not decimal places, because
concentrations routinely span several orders of magnitude.

### 8.1 The `comments` setting, and Excel

When comments are on, the file gets a `#`-prefixed header describing where
it came from: tool and version, timestamp, model file, the task and its
settings, your `@meta` fields, then your `notes`.

```
# Generated by Iridium 1.0, 2026-08-09T14:22:31
# Model: glycolysis.ant
# Title: Glycolytic oscillations in yeast
# Author: H. Sauro
# Task: wt (timestart=0, timeend=50, points=500, solver=cvode)
#
# Wild-type baseline. Vmax from Teusink 2000, Table 3.
#
time,S1,S2
0,1.0,0.5
```

This is genuinely useful — `pandas.read_csv(comment='#')` and
`numpy.loadtxt` both skip those lines by default.

**But Excel does not.** It shows them as data rows and makes a mess. So:

| Format | `comments` default | Why |
|---|---|---|
| `txt` | `true` | nothing parses it strictly |
| `csv`, `tsv` | `false` | Excel would show the comments as data |

If you want commentary in a CSV, set `comments: true` deliberately and
accept the consequence for Excel. One switch, no surprises.

Because `comments` governs *all* comment output, `notes` will not appear in
a CSV unless you also turn comments on. A good tool warns when you write
`notes` that would be invisible.

---

## 9. `@scan` — sweeping a parameter

Repeat a task across a range of values of one parameter.

| Setting | Type | Default | Notes |
|---|---|---|---|
| `source` | label | the task above | the task to repeat |
| `parameter` | name | **required** | parameter, or a species' initial value |
| `start`, `end`, `points` | numbers | — | one way to give the range |
| `spacing` | `linear` or `log` | `linear` | |
| `values` | list of numbers | — | the other way |
| `observables` | list | **required** | what to record |
| `measure` | see below | `timecourse` | what to take from each run |

Give **either** `start`/`end`/`points` **or** `values`, never both:

```
parameter: Vmax, start: 0.1, end: 10, points: 50, spacing: log
parameter: Vmax, values: [0.1, 0.5, 1, 2, 10]
```

`spacing: log` needs a positive range. `parameter` may name a species, in
which case its **initial value** is scanned.

**Every scan point starts fresh** from the model's initial conditions, with
only the scanned parameter changed. Nothing carries over from the previous
point. (Sweeps that deliberately carry state forward — to chase hysteresis
— need a continuation tool and are not part of this notation.)

### 9.1 `measure` — what each run contributes

| `measure` | What you get per scan point |
|---|---|
| `timecourse` | the whole trajectory *(default)* |
| `peakvalue` | the largest value reached |
| `timetopeak` | the time at which that maximum occurred |
| `{ sampleat: t }` | the value at time `t`, interpolated |

`timecourse` gives you *N* trajectories — the familiar overlay plot, and
the most common use of `@scan`. The other three give a single number per
point, so you get a curve of that quantity against the scanned parameter.

```
measure: timecourse
measure: peakvalue
measure: timetopeak
measure: { sampleat: 35 }
```

`sampleat` is the only one that takes a value, which is why it alone is
written in braces. Its time must lie inside the simulated range.

> **A gotcha worth knowing:** `peakvalue` on a curve that only ever rises
> returns the final value. That is arithmetically correct — a saturating
> response does peak at the end — but it may not be what you had in mind.

### 9.2 Scanning a steady state

You can point `source:` at an `@steadystate`. Be aware that doing this
*properly* needs a continuation algorithm, which most tools do not have.
Without one, re-solving at each parameter value can jump between branches,
fail near folds, and miss multistability entirely — producing a curve that
looks completely plausible and is wrong.

Tools without continuation are expected to warn and skip rather than guess.
If yours skips this, that is the reason, and it is the right call.

---

## 10. `@steadystate` — solving for the resting state

| Setting | Type | Notes |
|---|---|---|
| `solver` | name | e.g. `newton`, `nleq2` |
| `tolerance` | number | |
| `maxiter` | whole number | |
| `presimulate` | number | run this long first, §10.1 |
| `initial` | object | starting values, `{ S1: 0.5, S2: 2 }` |
| `observables` | list | defaults to all floating species |

```
@steadystate ss: {
  solver: newton,
  presimulate: 200,
  observables: [S1, S2, J1],
}
```

### 10.1 `presimulate`

Newton-type solvers need a decent starting guess. `presimulate` runs a time
course of the given length first and hands the result to the solver — which
is exactly what people do by hand, so it is worth being able to say
directly.

`initial` and `presimulate` combine in the obvious order: initial values
are applied, the pre-simulation runs from those, then the solver runs.

### 10.2 A steady state is a table, not a curve

It produces one set of numbers, so there is nothing for `@plot` to draw.
Use `@output` to report it:

```
@steadystate ss: { solver: newton, presimulate: 200 }

@output: {
  source: ss,
  file: "steadystate.txt",
  notes: "At the default parameter set.",
}
```

---

## 11. Recipes

### Two conditions on one plot

```
@simulate wt:  { timeend: 50, points: 500 }
@simulate high: { timeend: 50, points: 500 }

@plot: {
  source: [wt, high],
  y: [S1],
  title: "Wild type vs raised Vmax",
}
```

### How does the peak depend on a parameter?

```
@simulate wt: { timeend: 50, points: 500 }

@scan sweep: {
  source: wt,
  parameter: Vmax,
  start: 0.1,
  end: 10,
  points: 50,
  spacing: log,
  observables: [S1],
  measure: peakvalue,
}

@plot: {
  source: sweep,
  logx: true,
  y: [S1],
  title: "Peak S1 vs Vmax",
  xlabel: "Vmax",
}
```

### A family of trajectories

Leave `measure` at its default and you get one curve per scan point:

```
@simulate base: { timeend: 50, points: 500 }

@scan family: {
  source: base,
  parameter: Km,
  values: [0.1, 0.25, 0.5, 1, 2],
  observables: [S1],
}

@plot: { source: family, y: [S1], title: "Effect of Km" }
```

### A publication figure

```
@plot: {
  source: wt,
  y: [S1, S2],
  title: "Wild type",
  xlabel: "time (min)",
  ylabel: "concentration (mM)",
  gridy: true,
  series: {
    S1: { color: #1f77b4, line_width: 2 },
    S2: { color: #d62728, line_style: dashed },
  },
  file: "figure1.pdf",
}
```

### Data with its provenance attached

```
@output: {
  source: wt,
  file: "wildtype.csv",
  columns: [time, S1, S2],
  precision: 8,
  comments: true,
  notes: "Wild-type baseline. Vmax from Teusink 2000, Table 3.",
}
```

### Everything together

```
/*
@meta: {
  title: "Glycolytic oscillations",
  author: "H. Sauro",
  description: "Reduced model; parameters from Teusink 2000.",
}

// baseline time course
@simulate wt: {
  timestart: 0,
  timeend: 50,
  points: 500,
  solver: cvode,
}

@plot: {
  source: wt,
  y: [S1, S2],
  title: "Wild type",
  xlabel: "time (min)",
  ylabel: "concentration (mM)",
  gridy: true,
  file: "figure1.pdf",
}

@output: {
  source: wt,
  file: "wildtype.csv",
  columns: [time, S1, S2],
  precision: 8,
}

// how does amplitude depend on Vmax?
@scan sweep: {
  source: wt,
  parameter: Vmax,
  start: 0.1,
  end: 10,
  points: 50,
  spacing: log,
  observables: [S1],
  measure: peakvalue,
}

@plot: {
  source: sweep,
  logx: true,
  y: [S1],
  title: "Peak S1 vs Vmax",
  xlabel: "Vmax",
}

// steady state, for the record
@steadystate ss: {
  solver: newton,
  presimulate: 200,
  observables: [S1, S2, J1],
}

@output: {
  source: ss,
  file: "steadystate.txt",
  notes: "Reported at the default parameter set.",
}
*/
```

---

## 12. Where files are written

Paths in `file:` are relative to **the folder containing the model**, not
wherever the tool happened to be launched from. Otherwise the same file
would behave differently depending on how you started the program.

For safety, a model you downloaded cannot write outside its own folder:

- absolute paths are refused (`C:\temp\out.csv`, `/home/me/out.csv`)
- `..` is refused (`../../results/out.csv`)

Sub-folders below the model are fine: `results/figure1.pdf`.

---

## 13. When something is wrong

Tools report problems as **errors** (something is genuinely wrong and that
command cannot run) or **warnings** (it will carry on, but you should
know). One bad command never stops the others from running.

Every message carries a code like `META0301`, which is stable and worth
quoting if you need to ask about one.

The ones people actually hit:

| What you see | What happened |
|---|---|
| `'@plot' found in a comment that is not a metadata block` | the block starts with prose — see §3.1 |
| `unknown key 'logY'; keys are case-sensitive` | `logY` should be `logy` |
| `'points' and 'steps' cannot both appear` | pick one; they differ by one |
| `'@plot' needs a 'source': 3 tasks precede it` | say which task you meant; the message lists them |
| `'timeend' must be greater than 'timestart'` | the range runs backwards |
| `'S4' is not declared in the model` | typo, or the name really is missing |
| `'spacing: log' needs a positive range` | a log axis cannot start at or below zero |
| `may not contain '..'` | see §12 |
| `'notes' will not appear: comments are off` | set `comments: true` — see §8.1 |
| `'@bifurcation' is a deferred feature` | not part of this version of the notation |

If a tool does not understand a command at all, it says so, skips it, and
carries on with everything else. Your file does not become invalid because
one tool is older than another.

---

## 14. What it deliberately does not do

This notation is small on purpose. When you hit its edges, that is usually
the signal to move to a real script — and every tool should be able to hand
you one (§15).

**No arithmetic in `y:` or anywhere else.** You cannot write
`y: [S1, S2, S1+S2]`. Put the sum in the model instead, where it belongs:

```
total := S1 + S2;        // in the Antimony model
```
```
@plot: { y: [total] }    // then just name it
```

That is one line, it is reusable, and it keeps the experiment description
free of a whole expression language.

**No conditionals, loops or variables.** `@scan` covers the one repetition
people genuinely need.

**Not yet included, but recognised as wanted:** bifurcation analysis,
sensitivity coefficients, frequency response, multi-panel figures, axis
limits, legend control, stochastic ensembles. These are deliberately
deferred rather than forgotten. A tool meeting one of them says so plainly
rather than guessing.

---

## 15. Getting out again

The experiment can be exported:

- **A Python / Tellurium script.** This is the everyday one: a single
  self-contained file you can send to a collaborator, attach to a paper, or
  edit when you need something the notation cannot express. Use this as
  your escape hatch.
- **SED-ML, and a COMBINE archive.** The archival route, for journal and
  BioModels deposits. The archive carries the SBML, the SED-ML, the
  original Antimony file, and the metadata.

Not everything survives the trip to SED-ML — it has no way to say
"integrate first, then solve", and no equivalent of `timetopeak`. Whatever
is dropped is listed in an **export report** when you export, so you always
know exactly what an archive does and does not capture. The Python script
expresses all of it.

---

## Appendix: everything at a glance

**Commands**

| | Purpose | Can be a `source` |
|---|---|---|
| `@meta` | title, author, description | no |
| `@simulate` | a time course | yes |
| `@steadystate` | solve for the resting state | yes |
| `@scan` | repeat a task over a parameter | yes |
| `@plot` | draw a result | no |
| `@output` | write a result to a file | no |

**Punctuation**

| | |
|---|---|
| block | `/*` … `*/`, first character inside must be `@` |
| label | `@simulate wt: { … }` |
| setting | `name: value`, never `=` |
| separator | `,` — required; trailing one allowed |
| list | `[a, b, c]`; a single item needs no brackets |
| comment | `//` to end of line |
| text | `"quoted"` or `'quoted'` |
| colour | `#rrggbb`, `#rrggbbaa`, or a name |

**Fixed choices**

| Setting | Values |
|---|---|
| `type` | `line` `scatter` `line+marker` `bar` |
| `line_style` | `solid` `dashed` `dotted` `dashdot` |
| `marker_style` | `none` `circle` `square` `triangle` `diamond` `cross` |
| `spacing` | `linear` `log` |
| `measure` | `timecourse` `peakvalue` `timetopeak` `{ sampleat: t }` |
| `format` | `csv` `tsv` `txt` |
| plot `file` | `.png` `.pdf` `.svg` |
