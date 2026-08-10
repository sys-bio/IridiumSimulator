
## Antimony Simulation Metadata Specification (ASMS v1.0)

## 1. Overview
The Antimony Simulation Metadata Specification (ASMS) defines a lightweight, human-readable data format designed to embed simulation and visualization configurations directly inside Antimony biochemical model files. By defining the multi-line comment markers (/* and */) as the definitive boundaries, the format completely eliminates the need for outer brackets or meta-tags. The root level functions as a braceless dictionary, maximizing human readability while remaining entirely whitespace-insensitive with relaxed punctuation rules.

## 2. Syntax & Structure

## 2.1 The Single Comment Wrapper (Braceless Root)

All configuration options must live within exactly one native Antimony multi-line comment block. The comment markers themselves act as the implicit container for the top-level parameters:

* The block begins with: /*
* The block ends with: */

## 2.2 Core Directives (Keys)

At the root level of the comment block, configurations are split into distinct functional fields:

* simulate: An inner object block configuring data-generation parameters (e.g., time limits, steps, solvers).
* plot: An inner object block configuring downstream data visualization mapping (e.g., axes, column colors, plot types).

## 2.3 Total Whitespace Insensitivity

All whitespace characters (spaces, tabs, newlines, carriage returns) are completely ignored at the tokenizer level, except when they occur inside an explicitly quoted text string.

## 2.4 Relaxed Punctuation Rules

Commas (,) act as the absolute boundary between sequential primitive values (like numbers or strings). However, commas are completely optional immediately following a closing object brace (}) or closing list bracket (]). This allows sub-blocks at the root level to be neatly stacked vertically without forcing comma separation.

## 3. Data Types

* Numbers: Standard integer, floating-point, or scientific notation configurations (e.g., 0, 10, 1e-5).
* Booleans: Case-insensitive literal values true or false.
* Strings: Text wrapped in double or single quotes (e.g., "cvode", 'Time vs Species').
* Lists: An ordered sequence of values wrapped in square brackets [ ... ]. Items inside the list must be separated by commas.
* Objects: A nested map of key-value pairs wrapped in curly braces { ... }.
* Colors & Named Constants: Unquoted alphanumeric strings or hash-prefixed codes.
* Hex Colors: Must begin with # followed by 6, or 8 hexadecimal characters (e.g., #FF5733, #34673AFF).  8 digits are used where the last two are the opacity value. By default opacity is FF
   * Identifiers: Standard text names (e.g., blue, darkred, k1, time) used without quotes for a clean look.

## 4. Formal Grammar (EBNF)


```ebnf
(* ==========================================
   LEXER RULES (Tokens and Ignored Channels)
   ========================================== *)
HexColor    ::= '#' ([a-fA-F0-9]{3,4} | [a-fA-F0-9]{6} | [a-fA-F0-9]{8})
Identifier  ::= [a-zA-Z_][a-zA-Z0-9_]*
String      ::= '"' [^"\\]* '"' | "'" [^'\\]* "'"
Number      ::= '-'? [0-9]+ ('.' [0-9]+)? ([eE] ['+','-']? [0-9]+)?
Boolean     ::= 'true' | 'false'

(* The Lexer silently drops all whitespace tokens before parsing *)
_WS         ::= [ \t\n\r]+ -> SKIP 
```


```ebnf
(* ==========================================
   PARSER RULES (Structural Configuration)
   ========================================== *)
MetadataBlock ::= '/*' CommandList '*/'
CommandList   ::= Command ( Separator Command )* [ Separator ]

Command       ::= '@' 'simulate' SimulateObject
                | '@' 'plot' PlotObject

(* Strict simulation dictionary parser *)
SimulateObject   ::= '{' [ SimPropertyList ] '}'
SimPropertyList  ::= SimProperty ( Separator SimProperty )* [ Separator ]
SimProperty      ::= SimKey '=' Value
SimKey           ::= 'timestart' | 'timeend' | 'points' | 'steps' | 'solver' | atol | rtol

(* Strict visualization dictionary parser *)
PlotObject       ::= '{' [ PlotPropertyList ] '}'
PlotPropertyList ::= PlotProperty ( Separator PlotProperty )* [ Separator ]
PlotProperty     ::= 'x' '=' Value
                   | 'y' '=' Value
                   | 'type' '=' Value
                   | 'title' '=' Value
                   | 'grid' '= Value
                   | 'gridx' '=' Value
                   | 'gridy' '=' Value
                   | 'logx' '=' Value 
                   | 'logy' '=' Value                  
                   | 'series' '=' SeriesDictionary

(* Dynamic series styling mapper mapping variables to explicit look styles *)
SeriesDictionary ::= '{' [ SeriesMapList ] '}'
SeriesMapList    ::= SeriesMapItem ( Separator SeriesMapItem )* [ Separator ]
SeriesMapItem    ::= Identifier '=' StyleObject

StyleObject      ::= '{' [ StylePropertyList ] '}'
StylePropertyList::= StyleProperty ( Separator StyleProperty )* [ Separator ]
StyleProperty    ::= StyleKey '=' Value
StyleKey         ::= 'color' | 'line_style' | 'line_width' | 'marker_style' | 'marker_size'

(* Fallback generic containers *)
Object        ::= '{' [ PropertyList ] '}'
PropertyList  ::= Property ( Separator Property )* [ Separator ]
Property      ::= Key '=' Value
Key           ::= Identifier

Value         ::= Number | Boolean | String | List | Object | ColorOrIdentifier
List          ::= '[' [ ValueList ] ']'
ValueList     ::= Value ( ',' Value )* [ ',' ]
ColorOrIdentifier ::= HexColor | Identifier

Separator     ::= ',' | /* empty */

```

## 5. Small Production Example

```Antimony
// Biochemical Model Definitions
model pathway()
  J0: S1 -> S2; k1*S1;
  S1 = 50; S2 = 0; k1 = 0.23;
end

/* 
   @simulate = { 
     timestart = 0, 
     timeend = 15, 
     points = 500
   }

   @plot = { 
     title = "Simulation Results",
     x = time, 
     y = [S1, S2], 
   }
*/
```

The following also acceptable:

```Antimony
/* 
   @simulate = { timestart = 0, timeend = 15, points = 500, solver = "cvode", atol = 1e-6, rtol = 1e-6 }

*/
```

## Plotting Command


For a biochemical simulation visualization engine, @plot properties should be categorized into data mapping, styling, and layout rules. This ensures a clean separation between what is being plotted and how it looks.
Here is an enumeration of the standard properties commonly used in a @plot command:

## 1. Data Mapping Properties (The "What")

These keys map the generated simulation columns directly onto chart axes.

* x (Identifier): The column name for the horizontal axis (almost always time).
* y (Identifier or List of Identifiers): The columns to plot on the vertical axis (e.g., S1, [S1, S2, Substrate]).

## 2. Styling Properties (The "How")

These keys alter the look of lines, markers, and shapes.

* colors (List of Colors/Identifiers): An ordered list of color choices mapping sequentially to the elements in your y list (e.g., [blue, #FF573380]).
* style (String or Identifier): The chart representation format, such as line, scatter, bar, or staircase.
* line_width (Number): The stroke thickness for your plotted lines (e.g., 2 or 1.5).
* markers (Boolean): Toggles individual data point symbols on or off (true/false). [1] 

## 3. Layout & Canvas Properties (The "Aesthetics")

These keys manage the overall environment of the output canvas.

* title (String): A global descriptive heading printed at the top of the chart.
* xlabel / ylabel (String): Explicit text overrides for the axis titles if you don't want to use the raw variable identifiers.
* gridx, gridy (Boolean): Toggles background layout gridlines (true/false).
* legend (Boolean): Controls the visibility of the species label key.
* xscale / yscale (Identifier or String): Changes the axes scaling behavior, natively supporting linear or log.

## Example Integration

```Antimony
// Biochemical Model Definitions
model pathway()
  J0: S1 -> S2; k1*S1;
  S1 = 50; S2 = 0; k1 = 0.23;
end

/* 
   @simulate { 
     timestart = 0, 
     timeend = 15, 
     points = 500, 
     solver = cvode,
     atol = 1e-6
     rtol = 1e-12
   }

   @plot { 
     title = "Simulation Results",
     x = time, 
     y = [S1, S2, S3], 
     type = line+marker,
     gridx = false,     (* Keep horizontal clean *)
     gridy = true,      (* Enable vertical gridlines *)
     logx = false,      (* Linear time scale *)
     logy = true,       (* Logarithmic concentration scale *)
     
     series = {
       S1 = { color = blue, line_style = solid, marker_style = circle, type = line+marker, }
       S2 = { color = #FF573380, line_style = dashed, marker_size = 8, type = marker, }
       // S3 is omitted entirely and will gracefully render using theme engine defaults!
     }
   }
*/
```
