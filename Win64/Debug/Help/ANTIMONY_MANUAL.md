# Writing models in Antimony

Antimony is a plain-text language for describing biochemical models: you write
the reactions and the numbers, and Iridium turns that into something it can
simulate. Because a model is just text, it reads like the chemistry it
describes, and it can be searched, copied, compared and kept under version
control like any other document.

---

## Your first model

Two species and one reaction:

```antimony
J1: S1 -> S2; k1*S1;

S1 = 10;
S2 = 0;
k1 = 0.3;
```

That is a complete model. The first line says a reaction called `J1` turns
`S1` into `S2` at a rate of `k1*S1`. The rest give the starting amounts and the
rate constant. Run it and `S1` decays into `S2`.

Two things are worth noticing straight away. You never declared `S1`, `S2` or
`k1` — Antimony worked out that the first two are species because they appear
in a reaction, and that `k1` is a parameter because it does not. And the rate
law is written out in full: Antimony does not assume mass action, or anything
else, so what you write is what is simulated.

### Adding a compartment

Real models live in a volume:

```antimony
compartment cell = 1.5;
species S1 in cell, S2 in cell;

J1: S1 -> S2; k1*S1*cell;

S1 = 10;
S2 = 0;
k1 = 0.3;
```

`in cell` puts each species in the compartment, and the compartment's size is
now available as a number you can use in rate laws.

### A species held constant

A `$` in front of a species means "something else maintains this" — it takes
part in reactions but is never changed by them:

```antimony
compartment cell = 1;
species $Xo in cell, S1 in cell, $X1 in cell;

J1: $Xo -> S1;  k1*Xo;
J2: S1 -> $X1;  k2*S1;

Xo = 10;
X1 = 0;
S1 = 0;
k1 = 0.4;
k2 = 0.2;
```

`Xo` is a fixed supply and `X1` is a sink, so `S1` settles at a steady state
instead of everything draining away. This is the usual way to model a pathway
that sits inside something larger.

---

## The pieces

### Species, compartments and parameters

You can declare things explicitly:

```antimony
compartment cell = 1;
species S1 in cell, S2 in cell;
```

or let Antimony infer them from use, as in the first example. Explicit
declarations are worth it once a model grows: they document intent, and they
catch typos, because a name you meant to reuse but misspelled shows up as a
new parameter rather than silently doing nothing.

### Initial values

A value can be a number or an expression over other values:

```antimony
Vmax = 10;
Km = 0.5;
ratio = Vmax/Km;
S1 = 2*ratio;
```

Expressions are worked out once, at the start. If you want a quantity that
keeps up with its inputs as the simulation runs, you want an assignment rule —
see below.

### Reactions

The arrow says whether the reaction is reversible:

```antimony
J1: S1 -> S2;  k1*S1 - k2*S2;    // reversible
J2: S2 => S3;  k3*S2;            // irreversible
```

`->` is reversible and `=>` is irreversible. Either way the rate law is yours
to write; a reversible reaction usually has a forward and a reverse term, as
above.

Several reactants and products are joined with `+`, and a number in front is a
stoichiometric coefficient:

```antimony
J1: 2 A + B -> C;  k1*A^2*B;
```

A reaction can also have nothing on one side, which is how you write something
entering or leaving the system:

```antimony
J1:  -> S1;  v0;        // production from nowhere
J2: S1 -> ;  k1*S1;     // degradation
```

### Naming a stoichiometry

If a coefficient is itself a quantity you want to refer to, give it a name:

```antimony
sr = 2;
J1: sr S1 => S2;  k1*S1;
```

### Constants and variables

`const` marks something that never changes; `var` marks something that does:

```antimony
const k1, k2;
var S1;
```

Parameters are constant unless something changes them, so `const` is mostly
documentation — useful documentation, though, because it says the value is
meant to be fixed rather than merely not varying yet.

### Assignment rules

`:=` defines a quantity that is recomputed continuously from other quantities:

```antimony
Stotal := S1 + S2;
```

`Stotal` is not a separate pool with its own dynamics; it is a running total,
correct at every point in the simulation. Use this for derived quantities you
want to plot.

### Rate rules

An apostrophe defines a quantity by its rate of change rather than by a
reaction:

```antimony
P' = k1*S1 - k2*P;

P = 0;
```

This is an ODE written directly. It needs a starting value, exactly as a
species does. Rate rules are useful for things that are not really chemistry —
a signal, a growth rate, an accumulating dose.

### Events

An event changes something the moment a condition becomes true:

```antimony
E1: at (time > 10): S1 = S1 + 5;
```

The condition can be any expression, and several assignments are separated by
commas:

```antimony
E1: at (S1 < 0.1): S1 = 10, S2 = 0;
```

An event can also fire after a delay. The delay is written *before* the
trigger, separated by `after`:

```antimony
E1: at 2 after (S1 < 0.1): S1 = 10;
```

That reads "two time units after `S1` drops below 0.1". Events are the way to
model a dose, a wash, a switch being thrown, or anything else imposed on the
system from outside.

### Function definitions

If the same rate law appears repeatedly, you can name it:

```antimony
function MM(S, Vm, Km)
  Vm*S/(Km + S)
end

J1: S1 -> S2;  MM(S1, Vm1, Km1);
J2: S2 -> S3;  MM(S2, Vm2, Km2);
```

Functions take their arguments by position and have no side effects — they are
shorthand for an expression, nothing more. They are entirely optional, and most
models are perfectly readable without them.

### Amounts and concentrations

By default a species value is a concentration. Mark it `substanceOnly` if the
number you are giving is an amount:

```antimony
compartment cell = 2;
species S1 in cell;
substanceOnly species S2 in cell;

S1 = 5;    // concentration
S2 = 5;    // amount
```

### Units

You can declare units and attach them to quantities:

```antimony
unit substance = mole;
unit time_unit = second;
unit conc = mole/litre;

compartment cell = 1;
species S1 in cell;
S1 = 2;
S1 has conc;

k1 = 0.3;
k1 has time_unit;
```

Units are documentation that travels with the model. Iridium does not use them
to convert anything for you, so the numbers still have to be consistent — but
recording them makes it much easier to spot when they are not.

### Display names

An identifier has to be a single word, which is often not what you want to see
on a plot. `is` gives a quantity a human-readable name:

```antimony
S1 is "Glucose 6-phosphate";
J1 is "Hexokinase";
```

### Comments

```antimony
// A single-line comment.

/* A comment that runs
   over several lines. */

S1 = 10;   // ...or at the end of a line
```

Comments survive a load-and-save cycle, so notes to yourself stay where you
put them.

### Notes and annotations

`notes` attaches free text to the model or to any quantity in it:

```antimony
model notes "Fitted to the 2019 dataset; k2 is a rough estimate.";
S1 notes "Measured, not fitted.";
```

For longer notes, use triple backticks:

```antimony
model notes ```
This model reproduces figure 3 of the paper.
Parameters were fitted by hand.
```
```

You can also record who made the model and when, and link quantities to public
database entries so other tools know what they refer to:

```antimony
model created "2026-03-14T09:00:00Z";
model creator1.givenName "Ada";
model creator1.familyName "Lovelace";

S1 identity "http://identifiers.org/chebi/CHEBI:17234";
S1 identity "http://identifiers.org/kegg.compound/C00293",
            "http://identifiers.org/chebi/CHEBI:4167";
```

A quantity can carry several identifiers at once, separated by commas as above.
This is optional, and models simulate perfectly well without any of it, but it
is what makes a model useful to someone who did not write it.

### Numbers and names

Numbers can be written plainly or in scientific notation, and `inf` and `nan`
are available:

```antimony
k1 = 0.0003;
k2 = 3e-4;      // the same number
big = inf;
```

Names may contain letters, digits and underscores, and must not start with a
digit. There are no reserved words: `compartment`, `time`, `is` and `at` are
all perfectly good names for your own quantities, and Antimony works out from
position which meaning is intended.

---

## Worked examples

Each of these is complete. Paste one in and run it.

### Michaelis–Menten kinetics

The classic saturating rate law, written directly:

```antimony
// Irreversible Michaelis-Menten conversion of S to P.

compartment cell = 1;
species S in cell, P in cell;

J1: S -> P;  Vmax*S/(Km + S);

S = 10;
P = 0;
Vmax = 1.5;
Km = 2;
```

`S` falls quickly while it is well above `Km`, then more slowly as the enzyme
saturates. The same model with the rate law named:

```antimony
// The same model, with the rate law as a reusable function.

function MM(substrate, Vm, Km)
  Vm*substrate/(Km + substrate)
end

compartment cell = 1;
species S in cell, P in cell;

J1: S -> P;  MM(S, Vmax, Km);

S = 10;
P = 0;
Vmax = 1.5;
Km = 2;
```

The two behave identically. The function form pays off once the same law
appears three or four times; below that it mostly adds a layer to read
through.

### A pathway with fixed ends

A three-step pathway fed by a fixed source and draining to a fixed sink, with
one reversible step in the middle:

```antimony
// A linear pathway held between a fixed source and sink.

compartment cell = 1;
species $Xo in cell, S1 in cell, S2 in cell, $X1 in cell;

J1: $Xo -> S1;  k1*Xo;
J2: S1 -> S2;   k2*S1 - k3*S2;      // reversible
J3: S2 -> $X1;  k4*S2;

// The boundary species are held constant.
Xo = 10;
X1 = 0;

S1 = 0;
S2 = 0;

k1 = 0.3;
k2 = 0.6;
k3 = 0.2;
k4 = 0.4;

// Handy to plot alongside the species themselves.
total := S1 + S2;
```

Both intermediates rise from zero and settle at a steady state, because the
source never runs down and the sink never fills up.

### A dose, repeated

Events driving a system from outside — a substance is added at intervals and
cleared continuously:

```antimony
// Repeated dosing with first-order clearance.

compartment body = 1;
species D in body;

J1: D -> ;  ke*D;         // clearance

D = 0;
ke = 0.25;
dose = 8;

// Three doses, at t = 1, 10 and 20.
E1: at (time > 1):  D = D + dose;
E2: at (time > 10): D = D + dose;
E3: at (time > 20): D = D + dose;

D is "Drug";
```

Each event steps `D` up, and between doses it decays. Run this to about t = 40
to see the sawtooth.

### An annotated model

Everything above, plus the record of where the model came from:

````antimony
// A small annotated model, of the shape you would publish.

model *Glycolysis_fragment()

  compartment cytosol = 1;
  species Glc in cytosol, G6P in cytosol, $ATP in cytosol;

  J1: Glc + $ATP -> G6P;  Vmax*Glc/(Km + Glc);

  Glc = 5;
  G6P = 0;
  ATP = 2;

  Vmax = 1.2;
  Km = 0.8;

  // Readable names for plots.
  Glc is "Glucose";
  G6P is "Glucose 6-phosphate";
  J1  is "Hexokinase";

  // What the quantities actually are.
  Glc identity "http://identifiers.org/chebi/CHEBI:17234";
  G6P identity "http://identifiers.org/chebi/CHEBI:4170";
  J1  identity "http://identifiers.org/ec-code/2.7.1.1";

  // Where the model came from.
  model created "2026-03-14T09:00:00Z";
  model creator1.givenName "Ada";
  model creator1.familyName "Lovelace";
  model creator1.organization "Example University";

  model notes ```
A fragment of glycolysis, used here only to show what an annotated
model looks like. The rate law is illustrative rather than fitted.
```

end
````

The `model … end` wrapper gives the model a name. It is optional — every
earlier example left it out — but once a model is worth annotating it is
usually worth naming too.

---

## When something is wrong

Iridium reports problems with a line number and an explanation. The ones you
are most likely to meet:

**"… was never declared; it has been assumed to be a parameter. Check for a
typo"**
You used a name once and nowhere else. Nearly always a misspelling: `k1` in one
place and `kl` in another. Antimony cannot tell the difference between a typo
and a new parameter, so it warns rather than guesses.

**"… has no value, assignment rule or rate rule"**
Something in the model never got a starting value. Harmless if you meant it —
it will start at zero — but worth checking.

**"Cannot use X as a species; it was already established as a reaction"**
A name is being used for two different things. Rename one of them.

**"Syntax error"**
Usually a missing semicolon, or a rate law that ran off the end of the line.
The line number is where the parser gave up, which is occasionally the line
*after* the mistake.

**Negative stoichiometry**
`-1 S1` is rejected. Write the reaction the other way round, or as a separate
reaction; a negative coefficient means something different from what it looks
like.

---

## Quick reference

| | |
|---|---|
| `compartment c = 1;` | a compartment with a size |
| `species S1 in c;` | a species in a compartment |
| `substanceOnly species S2 in c;` | its value is an amount, not a concentration |
| `$S1` | held constant by something outside the model |
| `const k1;` / `var S1;` | fixed / changing |
| `J1: A -> B; rate;` | reversible reaction |
| `J1: A => B; rate;` | irreversible reaction |
| `J1: 2 A + B -> C; rate;` | stoichiometry |
| `J1: -> S1; rate;` | production; `S1 -> ;` for degradation |
| `S1 = 10;` | starting value |
| `x := S1 + S2;` | assignment rule, recomputed continuously |
| `P' = k*S;` | rate rule, an ODE |
| `E1: at (time > 5): S1 = 1;` | event |
| `E1: at 2 after (S1 < 1): S1 = 10;` | event with a delay |
| `function f(x) x*2 end` | reusable expression |
| `unit u = mole/litre;` / `S1 has u;` | units |
| `S1 is "Glucose";` | display name |
| `S1 notes "...";` | free text |
| `S1 identity "http://...";` | link to a database entry |
| `// ...` , `/* ... */` | comments |

---

## A note on SBML

SBML is the standard file format that most systems-biology software reads and
writes. Iridium converts your Antimony to SBML whenever it needs to hand the
model to another tool, and the annotations, units and display names above are
carried across. You do not have to know anything about SBML to use Antimony —
this is only here so the word is not a surprise when you meet it.
