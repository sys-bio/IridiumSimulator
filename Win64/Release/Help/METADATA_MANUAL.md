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

1. Commands go inside a `/* */` comment, one per line, each starting with `@`.
2. Every setting is `name: value`, and settings are separated by **commas**.
3. Anything in the comment that is not a command is just a description.

---

## 3. Writing it down

### 3.1 Where the block goes

Anywhere in the file. Before the model, after it, or split across several
blocks — they are read in the order they appear and behave as though joined
together.

A command is a line that starts with `@`:

```
/*
@simulate: { timeend: 10 }
*/
```

You can write notes to yourself in the same comment. Anything that is not a
command is ignored, so this works exactly as it looks:

```
/*
Model of yeast glycolysis, reduced form.
Parameters from Teusink 2000.

@simulate: { timeend: 10 }

S3 is left out below -- on a linear axis it swamps the other two.

@plot: { y: [S1, S2] }
*/
```

Notes like this are for you. If you want the description to travel with the
model — into an exported SED-ML archive, say — put it in `@meta` (§5) instead.

The one thing to watch: a note written *inside* a command's `{ }` is not a
note, it is a syntax error. Close the `}` first.

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
| `set` | settings | — | values to assign before running |

```
@simulate wt: {
  timestart: 0,
  timeend: 50,
  points: 500,
  solver: cvode,
}
```

### Changing a value before the run

`set` assigns anything the model can set — a parameter, a species starting
value, a compartment size — for that task only:

```
@simulate wt:      { timeend: 50 }
@simulate mutant:  { timeend: 50, set: { k1: 0.1 } }

@plot: { source: [wt, mutant], y: [S1] }
```

That is the whole recipe for a knockdown comparison: two tasks, one of which
changes a value. `wt` still sees the model exactly as written.

Note the nesting. It is `set: { k1: 0.1 }`, not `k1: 0.1` directly inside
the command — otherwise the tool could not tell `timend: 50` from a request
to set a parameter called `timend`, and you would lose the *did you mean
`timeend`?* message.

If you name something the model does not have, you get a warning rather than
an error, and the line is kept. That is deliberate: you may be writing the
metadata before the model catches up.

`set` works on `@steadystate` (§10) and `@scan` (§9) too.

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
| `titlefontsize` | number | the tool's own | points, §7.4 |
| `axesfontsize` | number | the tool's own | points, §7.4 |
| `legendposition` | `topleft` `topright` `bottomleft` `bottomright` | the tool's own | §7.4 |

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

### 7.4 Text size and the legend

Two settings cover the usual complaints — the title is too small for a slide,
the axes are too small for print:

```
@plot: {
  y: [S1, S2],
  title: "Dose response",
  titlefontsize: 16,
  axesfontsize: 12,
  legendposition: topright,
}
```

Sizes are in points and must be greater than 0. `axesfontsize` sizes the axis
labels *and* the numbers along the axes, since wanting one larger almost
always means wanting the other.

`legendposition` takes one of four corners: `topleft`, `topright`,
`bottomleft`, `bottomright`. Leave it out and the tool places the legend
itself, which is usually what you want — set it only when the automatic
choice lands on top of your data.

Leaving a font size out is not the same as setting it to a default number:
the tool keeps whatever size it would normally use, which will suit its own
figure size better than a number written here.

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
| `set` | settings | — | values held fixed across the sweep, §6 |
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
| `set` | settings | values to assign before solving, §6 |
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

`set` and `presimulate` combine in the obvious order: the assignments
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

### A dose response

A dose response asks how the system answers a *held* input, rather than how it
evolves from one. Here the input is `X0`, the readouts are `S1` and `S2`, and
each dose is read off at a fixed time.

This is a complete file — model and metadata together:

```
// A simple three step pathway using Michaelis-Menten kinetics.

J0:  $X0 -> S1; (J0_Vmax/J0_Km1)*(X0 - S1/J0_Keq)/(1 + X0/J0_Km1 + S1/J0_Km2);
J1:  S1 -> S2;  (J1_Vmax/J1_Km1)*(S1 - S2/J1_Keq)/(1 + S1/J1_Km1 + S2/J1_Km2);
J2:  S2 -> $X1; (J2_Vmax/J2_Km1)*(S2 - X1/J2_Keq)/(1 + S2/J2_Km1 + X1/J2_Km2);

// Species initializations:
S1 = 0;
S2 = 0;
X0 = 10;
X1 = 0;

// Variable initializations:
J0_Vmax = 1;  J0_Km1 = 0.4;    J0_Keq = 1;    J0_Km2 = 1;
J1_Vmax = 1;  J1_Km1 = 0.8;    J1_Keq = 1.5;  J1_Km2 = 1;
J2_Vmax = 1;  J2_Km1 = 0.232;  J2_Keq = 1.6;  J2_Km2 = 1;

/*
@meta: {
  title: "Dose response of a three-step pathway",
  author: "H. Sauro",
  description: "S1 and S2 read at t = 1, across a range of the input X0.",
}

// The run that is repeated at every dose.  Nothing here mentions the
// dose: this is simply how one experiment is performed.
@simulate wt: {
  timeend: 20,
  points: 500,
}

@scan doseresponse: {
  source: wt,
  parameter: X0,
  start: 0.1,
  end: 10,
  points: 50,
  spacing: log,
  observables: [S1, S2],
  measure: { sampleat: 1.0 },
}

@plot: {
  source: doseresponse,
  logx: true,
  y: [S1, S2],
  title: "Dose response at t = 1",
  xlabel: "X0",
  ylabel: "concentration",
  gridx: true,
  gridy: true,
  titlefontsize: 16,
  axesfontsize: 12,
  legendposition: topleft,
  series: {
    S1: { color: #1f77b4, line_width: 2, marker_style: circle },
    S2: { color: #d62728, line_style: dashed, marker_style: square },
  },
  file: "doseresponse.pdf",
}

@output: {
  source: doseresponse,
  file: "doseresponse.csv",
  precision: 8,
  comments: true,
  notes: "S1 and S2 sampled at t = 1; X0 swept 0.1 to 10 on a log scale.",
}
*/
```

Five things in that file are worth pointing at, because each is a decision
rather than boilerplate:

- **`X0` is a boundary species** — that is what the `$` in `$X0` means. A
  boundary species is held at its value instead of being consumed, so
  scanning it really does hold the dose steady for the whole run. Scanning a
  *floating* species sets only its starting value, which then drains away as
  the reactions proceed; that is a different experiment, and usually not the
  one you meant by "dose".

- **`measure: { sampleat: 1.0 }`** is what makes it a dose response rather
  than a pile of time courses. Each run is reduced to one number, so the
  result is 50 rows of `X0, S1, S2` — a curve of response against dose. Leave
  `measure` out and you would get 50 overlaid trajectories instead.

- **`timeend: 20` outruns the sample at `t = 1`** on purpose. Only `t = 1` is
  read out, so the extra time costs something; keeping it means the same `wt`
  task can be reused later for a plot of the full trajectory without becoming
  a second, slightly different definition of "the experiment". Set
  `timeend: 1` instead if you want the scan to run as fast as it can.

- **`spacing: log`** goes with `logx: true` on the plot. The range spans two
  decades, and linear spacing would put most of the 50 points above `X0 = 1`,
  leaving the interesting low-dose end of the curve nearly empty.

- **`legendposition: topleft`** because a dose response usually rises to the
  right, so the top-right corner is where the curve ends up.

### A publication figure

```
@plot: {
  source: wt,
  y: [S1, S2],
  title: "Wild type",
  xlabel: "time (min)",
  ylabel: "concentration (mM)",
  gridy: true,
  titlefontsize: 14,
  axesfontsize: 11,
  legendposition: topright,
  series: {
    S1: { color: #1f77b4, line_width: 2 },
    S2: { color: #d62728, line_style: dashed },
  },
  file: "figure1.pdf",
}
```

A journal column is narrow, so the figure is reduced on the page and the
default text comes out small. Setting the two sizes here means the file
records the sizes the figure was *designed* at, rather than leaving them to
whichever tool happens to open it.

`legendposition: topright` is worth setting explicitly for a figure you are
submitting: automatic placement moves as the data changes, so a plot you
re-run after editing the model can come back with the legend somewhere else.
Pinning it keeps the figure stable across re-runs.

### The same figure, sized for a talk

The only difference from the version above is the numbers. Slides are viewed
from across a room, so everything grows, and the legend moves out of the way
of a curve that rises to the right:

```
@plot: {
  source: wt,
  y: [S1, S2],
  title: "Wild type",
  xlabel: "time (min)",
  ylabel: "concentration (mM)",
  gridy: true,
  titlefontsize: 22,
  axesfontsize: 18,
  legendposition: bottomright,
  series: {
    S1: { color: #1f77b4, line_width: 3 },
    S2: { color: #d62728, line_style: dashed, line_width: 3 },
  },
  file: "talk_figure.png",
}
```

Keeping both as separate `@plot` commands beats editing one back and forth:
they draw from the same `wt` task, so the figures cannot drift apart, and the
file records that both were wanted.

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
| `unclosed '{' opened at line 12` | a `}` is missing; if a note follows the command, close the `}` before it — see §3.1 |
| `unknown key 'logY'; keys are case-sensitive` | `logY` should be `logy` |
| `'points' and 'steps' cannot both appear` | pick one; they differ by one |
| `'@plot' needs a 'source': 3 tasks precede it` | say which task you meant; the message lists them |
| `'timeend' must be greater than 'timestart'` | the range runs backwards |
| `'S4' is not declared in the model` | typo, or the name really is missing |
| `'spacing: log' needs a positive range` | a log axis cannot start at or below zero |
| `may not contain '..'` | see §12 |
| `'notes' will not appear: comments are off` | set `comments: true` — see §8.1 |
| `'upperright' is not a valid value for 'legendposition'` | the corners are `topleft`, `topright`, `bottomleft`, `bottomright` — see §7.4 |
| `'titlefontsize' is a size in points and must be greater than 0` | omit the setting to keep the tool's own size; `0` is not a way to say that |
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
| block | `/*` … `*/`; commands start with `@`, anything else is a note |
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
| `legendposition` | `topleft` `topright` `bottomleft` `bottomright` |
| `spacing` | `linear` `log` |
| `measure` | `timecourse` `peakvalue` `timetopeak` `{ sampleat: t }` |
| `format` | `csv` `tsv` `txt` |
| plot `file` | `.png` `.pdf` `.svg` |
