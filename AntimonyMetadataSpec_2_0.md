## Antimony Simulation Metadata Specification (ASMS v1.0)## 1. Overview

The Antimony Simulation Metadata Specification (ASMS) defines a lightweight, human-readable data format designed to embed simulation and visualization configurations directly inside Antimony biochemical model files. By defining the multi-line comment markers (/* and */) as the definitive boundaries, the format completely eliminates the need for outer brackets or meta-tags. The root level functions as a braceless dictionary that is entirely whitespace-insensitive with relaxed punctuation rules.
## 2. Syntax & Structure

## 2.1 The Single Comment Wrapper (Braceless Root)

All configuration options must live within exactly one native Antimony multi-line comment block. The comment markers themselves act as the implicit container for the top-level parameters:

* The block begins with: /*
* The block ends with: */ [1] 

## 2.2 Directives & Explicit Commands

Top-level structural configurations inside the comment block begin with an @ character followed immediately by a strict command name and an opening curly brace: [2] 

* @simulate: { ... } Configures data-generation variables.
* @plot: { ... } Configures downstream canvas layout and visualization mappings.

## 2.3 Total Whitespace Insensitivity

All whitespace characters (spaces, tabs, newlines, carriage returns) are completely ignored at the tokenizer level, except when they occur inside an explicitly quoted text string. Users have absolute layout freedom. [3, 4, 5, 6] 

## 2.4 Relaxed Punctuation Rules

Commas (,) act as the absolute boundary between sequential primitive values (like numbers or strings). However, commas are completely optional immediately following a closing object brace (}) or closing list bracket (]). This allows sub-blocks to be neatly stacked vertically without forcing comma separation.

## 3. Data Types & Property Enumeration

## 3.1 Primitive Types

* Numbers: Standard integer, floating-point, or scientific notation configurations (e.g., 0, 15, 1e-5). [7] 
* Booleans: Case-insensitive literal values true or false. [8, 9, 10] 
* Strings: Text wrapped in double or single quotes (e.g., "cvode").
* Lists: Ordered values wrapped in square brackets [ ... ] and separated by commas. [11] 
* Objects / Dictionaries: A nested map of key-value pairs wrapped in curly braces { ... }.
* Colors & Named Constants: Unquoted alphanumeric strings or hash-prefixed codes.
* Hex Colors: # followed by 6, or 8 hexadecimal characters (e.g., #FF5733, #00ff0080). Suffixes handle transparency (80 for 50% opacity). 
   * Identifiers: Standard alphanumeric text strings (e.g., blue, time, S1) used without quotes.

## 3.2 The @simulate Block Keys

The @simulate block enforces a strict validation schema containing only the following parameters:

The parser will allow names witho or witout underscaoe, eg time_start and timestart are treated the same.

* time_start (Double) : The initial tracking time slice.
* time_end (Number): The maximum time threshold for the execution run.
* points (Number): The exact total number of data points to generate and plot for the time course. 
* steps (Number): Alternative that specifies the  number of intervals (intervals = points + 1)
* solver (String/Identifier): Integration algorithm choice (e.g., cvode, gillespie).
* atol (double): Absolute tolerance for the solver
* rtol (double): Relative tolerance for the solver

## 3.3 The @plot Block Keys

The @plot block manages global properties while allowing granular track overrides:

* title (String): Canvas heading text label.
* grid (Boolean): Global gridlines toggle flag (true/false).
* gridx (Boolean): Independent horizontal axis grid override.
* gridy (Boolean): Independent vertical axis grid override.
* logx (Boolean): Configures the horizontal axis to use a logarithmic scale.
* logy (Boolean): Configures the vertical axis to use a logarithmic scale.
* x (Identifier): Axis column mapper variable (e.g., time).
* y (Identifier/List): Variable column name or ordered collection array to display on the chart.
* type (Identifier): Fallback global chart plot variant (line, scatter, line+marker, bar).
* series (Object, Optional): A dictionary mapping explicit identifiers found in y directly to inner styling objects. Each value object accepts the following optional parameters:
* type (Identifier): Series-specific type override allowing mixed plot compositions. Valid options: line, scatter, line+marker, bar.
   * color (Color/Identifier): Explicit color or transparency hash assignment.
   * line_style (Identifier): Path stroke pattern (solid, dashed, dotted, dashdot).
   * line_width (Number): Path line thickness value.
   * marker_style (Identifier): Point node shape (circle, square, diamond, triangle, cross).
   * marker_size (Number): Visual node dimension diameter.

## 4. Formal Grammar (EBNF)

```ebnf
(* ==========================================
   LEXER RULES (Tokens and Ignored Channels)
   ========================================== *)
HexColor    ::= '#' [a-fA-F0-9]{6} | '#' [a-fA-F0-9]{8}
Identifier  ::= [a-zA-Z_][a-zA-Z0-9_]*
String      ::= '"' [^"\\]* '"' | "'" [^'\\]* "'"
Integer     ::= '-'? [0-9]+
Double      ::= '-'? [0-9]+ '.' [0-9]+ ([eE] ['+' '-']? [0-9]+)?
            | '-'? [0-9]+ [eE] ['+' '-']? [0-9]+
Boolean     ::= 'true' | 'false'

(* The Lexer silently drops all whitespace tokens before parsing *)
_WS         ::= [ \t\n\r]+ -> SKIP 
```

```ebnf
(* ==========================================
   PARSER RULES (Structural Configuration)
   ========================================== *)
MetadataBlock ::= '/*' CommandList '*/'

CommandList ::= Command ( Command )*
Command ::= '@' 'simulate' ':' SimulateObject
          | '@' 'plot' ':' PlotObject

SimulateObject   ::= '{' [ SimPropertyList ] '}'
SimPropertyList  ::= SimProperty ( ',' SimProperty )* [ ',' ]
SimProperty      ::= SimKey ':' Value
SimKey           ::= 'timestart' | 'timeend' | 'points' | 'steps' | 'solver'

PlotObject       ::= '{' [ PlotPropertyList ] '}'
PlotPropertyList ::= PlotProperty ( ',' PlotProperty )* [ ',' ]

PlotProperty ::= 'x' ':' Identifier
               | 'y' ':' ( Identifier | List )
               | 'type' ':' PlotType
               | 'title' ':' String
               | 'grid' ':' Boolean
               | 'gridx' ':' Boolean
               | 'gridy' ':' Boolean
               | 'logx' ':' Boolean
               | 'logy' ':' Boolean
               | 'series' ':' SeriesDictionary

PlotType   ::= 'line' | 'scatter' | 'line+marker' | 'bar'
LineStyle  ::= 'solid' | 'dashed' | 'dotted' | 'dashdot'
MarkerStyle ::= 'none' | 'circle' | 'square' | 'triangle'
              | 'diamond' | 'cross'

SeriesDictionary ::= '{' [ SeriesMapList ] '}'
SeriesMapList    ::= SeriesMapItem ( ',' SeriesMapItem )* [ ',' ]
SeriesMapItem    ::= Identifier ':' StyleObject

StyleObject       ::= '{' [ StylePropertyList ] '}'
StylePropertyList ::= StyleProperty ( ',' StyleProperty )* [ ',' ]

StyleProperty ::= 'type' ':' PlotType
                | 'color' ':' ColorOrIdentifier
                | 'line_style' ':' LineStyle
                | 'line_width' ':' ( Integer | Double )
                | 'marker_style' ':' MarkerStyle
                | 'marker_size' ':' ( Integer | Double )

Object         ::= '{' [ PropertyList ] '}'
PropertyList   ::= Property ( ',' Property )* [ ',' ]
Property       ::= Key ':' Value
Key            ::= Identifier

Value ::= Integer | Double | Boolean | String | List | Object | ColorOrIdentifier

List      ::= '[' [ ValueList ] ']'
ValueList ::= Value ( ',' Value )* [ ',' ]

ColorOrIdentifier ::= HexColor | Identifier
```

## 5. Production Example

```Antimony
// Biochemical Model Definitions
model cascade_pathway()
  J0: S1 -> S2; k1*S1;
  J1: S2 -> S3; k2*S2;
  S1 = 100; S2 = 0; S3 = 0; k1 = 0.15; k2 = 0.05;
end

/* 
   @simulate: { 
     timestart = 0,
     timeend = 50,
     points = 1000,
     solver = cvode
   }

   @plot: { 
     title = "Multi-Type Kinetic Composition",
     x = time,
     y = [S1, S2, S3],
     type = line,
     gridx = false,
     gridy = true,
     logy = false,
     
     series: {
       S1: { color = blue, line_style = solid },
       
       S2: { 
         type = line+marker,
         color = #573380, 
         line_style = dashed, 
         marker_style = diamond,
         marker_size = 6
       },
       
       S3: { 
         type = scatter,
         color = darkgreen, 
         marker_style = circle 
       }
     }
   }
*/
```
