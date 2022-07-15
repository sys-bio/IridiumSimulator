{ ******************************************************** }
{                                                          }
{ Copyright (c) H M Sauro 1996                             }
{ ******************************************************** }


unit uExpressionEvaluator;

interface

uses SysUtils, Classes, uSymbolTable;

const
  EOF_CHAR = $7F;
  MAX_DIGIT_COUNT  = 2;
  DEFAULT_MAX_SIZE = 400;   { Initial max size of rpn expression space, can grow dynamically }
  GROWTH           = 100;   { This is how much the rpn expression grows by if space runs out }
  MAX_BUILTINS     = 24;    { Maximum number of built-in functions }
  DEFAULT_STACK_SIZE = 100; { default depth of evaluation stack, can be set dynamically at runtime }


type
  { Changes the way the parser interprets new symbols. eInstallSymbols means
    add new symbols to the symbol table without complaint, initialised to zero.
    eReportNewSymbols means cause n exception when new symbol encountered }

  TAssignBehaviour = (eReportNewSymbols, eInstallAllSymbols, eInstallLeftSymbol, eInstallRightSymbols);
  TEventsStatus    = (eEventsOff, eEventsOn);

  TAlgExprError    = (eAlgOK, eAlgNoExpression, eAlgUnknownIdentifier, eAlgSyntaxError,
                      eAlgMissingLParen, eAlgMissingRParen, eAlgUserSpecified);

  TParser = class;
  TEvaluator = class;

  { This is the Object that is accessible from the Component palette }

  TAlgExpression =
        class (TComponent)
          private
            FOnChange     : TNotifyEvent;
            FExpression   : string;
            FResult       : Extended;
            FParser       : TParser;  { This owns the rpn expression }
            FSymbolTable  : TBinarySearchTree; { Its own private symbol table ! }
            FEvaluator    : TEvaluator;
            FEventsStatus : TEventsStatus;
            FClearRPN     : boolean;
            nConstants    : integer;  { The # of built-in constants stored in the symbol table }
           public
            constructor create (AOwner : TComponent); override;
            destructor  destroy; override;

            function  EvalThis (s : string) : Extended;
            function  Eval : Extended;
            procedure Compile (s : string);
            procedure CatCompile (s : string);
            function  Check (s : string) : TAlgExprError;
            function  CheckForVariable (s : string) : boolean;
            procedure SetExp (s: string);
            function  GetExp : string;
            procedure SetRunTimeStack (s : integer);
            function  GetRunTimeStack : integer;
            procedure Clear;
            procedure SetVar (s : string; d : Extended);
            function  GetVar (s : string) : Extended;
            function  GetnthSymbol (n : integer) : string;
            procedure SetnthSymbol (n : integer; d : double);
            function  rpn : string;
            procedure SetAssignBehaviour (value : TAssignBehaviour);
            function  GetAssignBehaviour : TAssignBehaviour;
            procedure SetEventsStatus (value : TEventsStatus);
            function  GetnSymbols : integer;
            procedure EventsOff;
            procedure EventsOn;
            property  answer : Extended read FResult; { don't use result, it might conflict with function result }
            property  nSymbols : integer read GetnSymbols; { number of symbols in symbol table }
          published
            property  str: string read GetExp write SetExp;
            property  AssignBehaviour: TAssignBehaviour read GetAssignBehaviour write SetAssignBehaviour;
            property  RunTimeStack: integer read GetRunTimeStack write SetRunTimeStack;
            property  EventsStatus: TEventsStatus read FEventsStatus write SetEventsStatus;
            property  OnChange : TNotifyEvent read FOnChange write FOnChange;
         end;


  { ********************************************************************** }
  { Declarations for the Parser }

  ECompileException  = class (Exception);
  EMissingLParen     = class (ECompileException);
  EMissingRParen     = class (ECompileException);
  ESyntaxError       = class (ECompileException);
  EUnknownIdentifier = class (ECompileException);

  { ********************* Lexical scanner types etc *********************** }

  TcharCode   = (cLETTER, cDIGIT, cPOINT, cSPECIAL, cETX);
  TTokenCodes = (tEMPTY, tIDENTIFIER, tNUMBER, tPLUS, tMINUS, tMULT,
                 tPOWER, tRPAREN, tLPAREN, tSLASH, tERROR, tEQUALS,
                 tAPOSTROPHY, tSEMICOLON, tCOMMA, End_of_string);

  TFunctionId = record
                 name : string;
                 opcode : integer;
                 nArgs : integer;
                end;

  { ************* Reverse polish data structures and types ****************** }

  TRpnEntry = (V, O, C);

  { The instruction stream is composed of RpnElement types. These are
    made from a union of three data types:
    1. value RP_CONSTANT followed by Extended value, used for storing constants;
    2. value RP_OPERAND followed by a pointer into the symbol table for fast access;
    3. value < 0, an integer, always less than zero and is a code for
    representing an operation, eg -1 means ADDITION.

    The Union structure was chosen initially to converse memory but later it
    was discovered that a Union also speeded up processing }

  TRpnElement = record
                  case TRpnEntry of
                    V : (value : integer);
                    O : (operand  : pTSymTabNode); { operands and operators }
                    C : (constant : Extended);       { constants, eg 1.234 }
                  end;

  { for the benefit of dynamic rpn expressions }
  TRpnArrayType = array[1..1000] of TRpnElement; { doesn't allocate space! }
  pRpnArrayType = ^TRpnArrayType;

  { Because the rpn expression is a dynamic structure it has two lengths,
    a MaxLength which is the actual space allocated on the heap; and LengthUsed
    which is the space actually used in the array. LengthUsed <= MaxLength }
  Trpnexpr = record
               MaxLength, LengthUsed : integer;
               expr   : pRpnArrayType;
             end;

  { ... and finally }

  pTrpn = ^Trpnexpr;

  { *********************** Start of TParser OBJECT ************************* }


  TParser = class (TObject)
             private
               FRpnExpr : pTrpn;  { Note the parser owns the rpn expression }
               FSymTab  : TBinarySearchTree;  { Only a pointer to the copy held by TAlgExpression }
               FToken, Fprevious_token  : TTokenCodes;
               Fvalue, Fprevious_value  : Extended;
               Fname,  Fprevious_name   : string;
               Fexpression : string; Flen_expr : integer;
               Fch : char; Fposition : integer;
               Fchar_table  : array[0..255] of TcharCode;
               Ffunction_id : array[1..MAX_BUILTINS] of TFunctionId; { built-in functions }
               FBehaviour : TAssignBehaviour;
               procedure initialise_scanner;
               procedure init_funcTable;
               procedure SetFunc (i : integer; f : string; op, nargs : integer);
               function  search_functionId (name : string; var index : integer) : boolean;
               procedure SetAssignBehaviour (behave : TAssignBehaviour);
               function  GetAssignBehaviour  : TAssignBehaviour;
               function  ReturnRpnString : string;
               procedure AllocRpnExpression  (size : integer);
               procedure nextchar;
               procedure skip_blanks;
               procedure get_token;
               procedure unget_token;
               procedure get_word;
               procedure get_number;
               procedure get_special;
               procedure ParseFunction (index : integer);
               procedure factor;
               procedure super_term;
               procedure term;
               procedure doexpression;
               procedure doassignment (lefthand_name : string);
               procedure dostatement;
               procedure CheckEnoughSpace;
               procedure StoreInst (inst : integer);
               procedure StoreSymbol (symb : pTSymTabNode);
               procedure StoreConstant (c : Extended);
             public
               constructor create;
               destructor  destroy; override;

               procedure ParseExp (expr : string; SymTab : TBinarySearchTree; ClearRPN : boolean);
               procedure FreeRpnExpr;
            end;


  { ********************************************************************** }
  { Declarations for the Evaluator }

  { Evaluator exceptions }
  EEvaluateException = class (Exception);
  ERTStackOverFlow   = class (EEvaluateException);  { Only applicable to xD routines }
  ERTStackUnderFlow  = class (EEvaluateException);
  EMathPower         = class (EEvaluateException);
  EEvaluationError   = class (EEvaluateException);


  { Record returned after an evaluation }
  TEvalType = record
              value : Extended;
              SymTabPtr : pTSymTabNode;
              end;

  TStack = array[0..1023] of Extended;
  pTStack = ^TStack;

  TEvaluator = class (TObject)
                private
                  { declare runtime stack used during evaluations }
                  FStack : pTStack;
                  FStack_top : integer;
                  FStackSize : integer;
                  FStackSize_Minus_1 : integer;   { StackSize - 1 }
                  TrigF : Extended;  { set to Pi/180 if uses decides to use degrees else = 1.0 }
                  FUseRadians : boolean;
                  procedure FreeStack;
                  function  GetStack : integer;
                  procedure SetStack (size : integer);

                public
                  procedure   Use_radians;
                  procedure   Use_degrees;
                  function    run (rp_expr : pTrpn; SymTab : TBinarySearchTree) : TEvalType;
                  constructor create;
                  destructor  destroy; override;
               end;

{ -------------------------------------------------------------------------- }

procedure Register;

implementation


{ -------------------------------------------------------------------------- }
{ -------------------------------------------------------------------------- }


{ Further Declarations for the Evaluator }

const

  { Old values but which turn out to be speed penalty, see new values }

  {RP_CONSTANT  = 5000;  { Push constant value which follows on to the stack }
  {RP_OPERAND   = 1000;  { Push value pointed to by following ptr onto stack }
  {RP_ADD       = 1;     { Pop two args off stack, add them and push result onto stack }
  {RP_SUBTRACT  = 2;
  RP_MULTIPLY  = 3;
  RP_DIVIDE    = 4;
  RP_POWER     = 5;

  RP_SIN       = 101;
  RP_COS       = 102;
  RP_TAN       = 103;
  RP_EXP       = 104;
  RP_LOG       = 105;    { log to the base 10 }
  {RP_LN        = 106;    { log to the base e }
  {RP_SQR       = 107;    { square function }
  {RP_SQRT      = 108;    { square root function }
  {RP_FACT      = 109;    { factorial, x! }
  {RP_ARCSIN    = 110;
  RP_ARCCOS    = 111;
  RP_ARCTAN    = 112;
  RP_SINH      = 113;
  RP_COSH      = 114;
  RP_TANH      = 115;
  RP_ARCSINH   = 116;
  RP_ARCCOSH   = 117;
  RP_ARCTANH   = 118;

  RP_UNARYMINUS = 200;

  RP_EQUALS     = 500;   { Assignment operator }

  {RP_STOP     = 0;}


  { These values are set to optimise the case statement in the evaluator }

  RP_CONSTANT  = 1;  { Push constant value which follows on to the stack }
  RP_OPERAND   = 2;  { Push value pointed to by following ptr onto stack }
  RP_ADD       = 3;     { Pop two args off stack, add them and push result onto stack }
  RP_SUBTRACT  = 4;
  RP_MULTIPLY  = 5;
  RP_DIVIDE    = 6;
  RP_POWER     = 7;

  RP_SIN       = 8;
  RP_COS       = 9;
  RP_TAN       = 10;
  RP_EXP       = 11;
  RP_LOG       = 12;    { log to the base 10 }
  RP_LN        = 13;    { log to the base e }
  RP_SQR       = 14;    { square function }
  RP_SQRT      = 15;    { square root function }
  RP_FACT      = 16;    { factorial, x! }
  RP_ARCSIN    = 17;
  RP_ARCCOS    = 18;
  RP_ARCTAN    = 19;
  RP_SINH      = 20;
  RP_COSH      = 21;
  RP_TANH      = 22;
  RP_ARCSINH   = 23;
  RP_ARCCOSH   = 24;
  RP_ARCTANH   = 25;

  RP_UNARYMINUS = 26;

  RP_EQUALS     = 27;   { Assignment operator }

  RP_STOP     = 0;

{ The instruction stream, anything entry value greater than zero is either
a constant of an operand instruction. In either case the argument follows in
the next isntruction. In the case of a constant, the following value is the
constant, in the case of an operand, the following value is a pointer to the
symbol table. Anything else is an operator and have values all less than zero,
except the STOP instruction which has a value of zero. See constants above for
types of operator available }

{ --------------------------------------------------------------------------- }


function power (x : Extended; n : Extended) : Extended; forward;


constructor TAlgExpression.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  FExpression   := '';
  FResult       := 0.0;
  FClearRpn     := true;
  FEventsStatus := eEventsOn;
  FParser       := TParser.Create;
  FSymbolTable  := TBinarySearchTree.Create;
  FEvaluator    := TEvaluator.Create;

  { Install built-in constants. TExp owns the symbol table }
  FSymbolTable.insert ('Pi', 3.141592653589793);
  FSymbolTable.insert ('e',  2.718281828459045);
  FSymbolTable.insert ('Degrees', 0.0174532925199433);
  nConstants := 3; { Very important, you must update this if you add more constants }
end;


destructor TAlgExpression.Destroy;
begin
  FSymbolTable.free;
  FParser.free;
  FEvaluator.free;
  inherited Destroy;
end;


procedure TAlgExpression.SetAssignBehaviour (value : TAssignBehaviour);
begin
  if Fparser.GetAssignBehaviour <> value then
     FParser.SetAssignBehaviour (value);
end;


function TAlgExpression.GetAssignBehaviour : TAssignBehaviour;
begin
  result := FParser.GetAssignBehaviour;
end;


{ Access via properties }
procedure TAlgExpression.SetExp (s: string);
begin
  if s = '' then begin FParser.FreeRpnExpr; exit; end;
  if s <> FExpression then
     begin
     FExpression := s; FResult := 0.0;
     { FClearRPN is set depending on whether Compile or CatCompile was last called }
     FParser.ParseExp (FExpression, FSymbolTable, FClearRPN);  { note ParseExp frees any previously allocated space }
     FResult := FEvaluator.run(FParser.FRpnExpr, FSymbolTable).value;
     if (FEventsStatus = eEventsOn) and Assigned (FOnChange) then FOnChange (Self);
     end;
end;


{ Access via properties }
function TAlgExpression.GetExp : string;
begin
  result := FExpression;
end;


{ Allow user of object to resize the runtime stack used by the evaluator, note
the existing stack is destroyed during this operation }
procedure TAlgExpression.SetRunTimeStack (s : integer);
begin
  if s > 0 then
     FEvaluator.SetStack (s)
  else
     raise Exception.Create ('TAlgExpression Error: Stack size must be greater than zero!');
end;


{ Get the current size of the stack }
function TAlgExpression.GetRunTimeStack : integer;
begin
  result := FEvaluator.GetStack;
end;


{ Evaulate expression s by compiling and evaulating, slow but simple for quick hacks }
{ Use: x := e.evalThis ('cos(1.2) + sin(4.5)'); }
function TAlgExpression.EvalThis (s : string) : Extended;
begin
  if s <> '' then
     begin
     { Only compile if its different from the last string processed }
     if FExpression <> s then
        begin
        FExpression := s;
        FParser.ParseExp (FExpression, FSymbolTable, True);
        end;
     result := FEvaluator.run(Fparser.FRpnExpr, FSymbolTable).value;
     FResult := result;
     if (FEventsStatus = eEventsOn) and Assigned (FOnChange) then FOnChange (Self);
     end
  else
     raise Exception.Create ('TAlgExpression Error: no expression to evaluate');
end;


{ Fast version of the above, less error checking code but much faster }
function TAlgExpression.Eval : Extended;
begin
  if FExpression <> '' then
     begin
     if Fparser.FRpnExpr = Nil then FParser.ParseExp (FExpression, FSymbolTable, True);
     result := FEvaluator.run (Fparser.FRpnExpr, FSymbolTable).value;
     FResult := result;
     end
  else
     raise Exception.Create ('TAlgExpression Error: no expression to evaluate');
end;


function TAlgExpression.Check (s : string) : TAlgExprError;
begin
  result := eAlgOK;
  if s <> '' then
     try
       FParser.ParseExp (s, FSymbolTable, True)
     except
       on ESyntaxError do result := eAlgSyntaxError;
       on EUnknownIdentifier do result := eAlgUnknownIdentifier;
       on EMissingLParen do result := eAlgMissingLParen;
       on EMissingRParen do result := eAlgMissingRParen;
     end
  else
     result := eAlgNoExpression;
end;


{ Check for the presence of a specific variable in the symbol table.
  Retuns true if variable found, false if not }
function TAlgExpression.CheckForVariable (s : string) : boolean;
begin
  if FSymbolTable.find_symbol (s) = Nil then
     result := false
  else
     result := true;
end;


{ Convert string expression into internal rpn format }
{ Usage: e.Compile ('cos(1.2)'); Subsequent calls to e.eval would evaluate
  cos(1.2) much quicker }
procedure TAlgExpression.Compile (s : string);
begin
  if s <> '' then
     begin
     { Only compile if the new expression is different }
     if FExpression <> s then
        begin
        FExpression := s; FClearRPN := True;
        FParser.ParseExp (FExpression, FSymbolTable, True);
        end;
     end
  else
     raise Exception.Create ('TAlgExpression Error: no expression to evaluate');
end;


{ Concatenate next compiled expression onto existing one }
procedure TAlgExpression.CatCompile (s : string);
begin
  if s <> '' then
     begin
     { Only compile if the new expression is different }
     if FExpression <> s then
        begin
        FExpression := s; FClearRPN := False;
        FParser.ParseExp (FExpression, FSymbolTable, False);
        end;
     end
  else
     raise Exception.Create ('TAlgExpression Error: no expression to evaluate');

end;


{ Declare and initialise a string variable in the symbol table }
{ Usage: e.SetVar ('a', 1.2); e.SetVar ('Pi', 3.1415);
         e.EvalThis ('a*Pi');
}
procedure TAlgExpression.SetVar (s : string; d : Extended);
var np : pTSymTabNode;
begin
  np := FSymbolTable.find_symbol (s);
  if np <> Nil then FSymbolTable.SetSymTabValue (np, d) else FSymbolTable.insert (s, d);
  if (FEventsStatus = eEventsOn) and Assigned (FOnChange) then
     begin
     eval;
     FOnChange (Self);
     end;
end;


{ Fetch the value of a declared variable from the symbol table }
{ Usage:  e.SetVar ('a', 1.2); x := e.GetVar ('a'). x then equals 1.2 }
function TAlgExpression.GetVar (s : string) : Extended;
var np : pTSymTabNode;
begin
  np := FSymbolTable.find_symbol (s);
  if np <> Nil then
     result := FSymbolTable.GetSymTabValue (np)
  else
     raise EUnknownIdentifier.Create ('TAlgExpression Error (GetVar): Symbol <' + s + '> not declared');
end;


{ Clear all declared variables from the symbol table }
procedure TAlgExpression.Clear;
begin
  { Free everything, because RpnExpr may contain references to SymbolTable }
  FParser.FreeRpnExpr;
  FSymbolTable.Clear;
  FExpression := ''; FResult := 0.0;
  { ReInstall built-in constants }
  FSymbolTable.insert ('Pi', 3.141592653589793);
  FSymbolTable.insert ('e',  2.718281828459045);
  FSymbolTable.insert ('Degrees', 0.0174532925199433);
  nConstants := 3; { Very important, you must update this if you add more constants }
end;


function TAlgExpression.rpn : string;
begin
  if Fparser.FRpnExpr <> Nil then
     result := FParser.ReturnRpnString;
end;


procedure TAlgExpression.SetEventsStatus (value : TEventsStatus);
begin
  if value <> FEventsStatus then
     FEventsStatus := value;
end;


procedure TAlgExpression.EventsOff;
begin
  FEventsStatus := eEventsOff;
end;

procedure TAlgExpression.EventsOn;
begin
  FEventsStatus := eEventsOn;
end;


{ Get the number of symbols (not including built-in constants stored in the symbol table }
function TAlgExpression.GetnSymbols : integer;
begin
  result := FSymbolTable.GetnSymbols - nConstants;  { Subtract  to adjust for the built-in constants }
end;


function TAlgExpression.GetnthSymbol (n : integer) : string;
begin
  result := FSymbolTable.GetnthSymbol (n+nConstants)^.name;  { Don't count built-in symbols, hence +nConstants }
end;


procedure TAlgExpression.SetnthSymbol (n : integer; d : double);
begin
  FSymbolTable.SetnthSymbol (n + nConstants, d);
end;


procedure Register;
begin
  RegisterComponents('FSS', [TAlgExpression]);
end;


{ ------------------------------------------------------------------------- }
{ -------              PARSER ROUTINES FOLLOW                               }
{ ------------------------------------------------------------------------- }


{ ----------------------------------------------------------------------- }


constructor TParser.Create;
begin
  inherited Create;
  FRpnExpr := Nil;
  initialise_scanner;
end;


destructor TParser.Destroy;
begin
  FreeRpnExpr;
  inherited Destroy;
end;


procedure TParser.FreeRpnExpr;
begin
  { Check it hasn't been destroyed already ! }
  if FRpnExpr = Nil then exit;

  if FRpnExpr^.expr <> Nil then
     FreeMem (FRpnExpr^.expr, (FRpnExpr^.MaxLength+1)*Sizeof (TrpnElement));
  if FRpnExpr <> Nil then
     FreeMem (FRpnExpr, sizeof (TrpnExpr));
  FRpnExpr := Nil;
end;



procedure TParser.SetAssignBehaviour (behave : TAssignBehaviour);
begin
  FBehaviour := behave;
end;


function TParser.GetAssignBehaviour  : TAssignBehaviour;
begin
  result := FBehaviour;
end;


procedure TParser.SetFunc (i : integer; f : string; op, nargs : integer);
begin
  Ffunction_id[i].name := f;
  Ffunction_id[i].opcode := op;
  Ffunction_id[i].nArgs := nargs;
end;


procedure TParser.init_funcTable;
begin
  { all function names must be in LOWER case }
  SetFunc (1,  'sin', RP_SIN, 1);
  SetFunc (2,  'cos', RP_COS, 1);
  SetFunc (3,  'tan', RP_TAN, 1);
  SetFunc (4,  'exp', RP_EXP, 1);
  SetFunc (5,  'log', RP_LOG, 1);
  SetFunc (6,  'ln',  RP_LN, 1);
  SetFunc (7,  'sqr', RP_SQR, 1);
  SetFunc (8,  'sqrt', RP_SQRT, 1);
  SetFunc (9,  'fact', RP_FACT, 1);
  SetFunc (10, 'arcsin', RP_ARCSIN, 1);
  SetFunc (11, 'arccos', RP_ARCCOS, 1);
  SetFunc (12, 'arctan', RP_ARCTAN, 1);
  SetFunc (13, 'sinh', RP_SINH, 1);
  SetFunc (14, 'cosh', RP_COSH, 1);
  SetFunc (15, 'tanh', RP_TANH, 1);
  SetFunc (16, 'arcsinh', RP_ARCSINH, 1);
  SetFunc (17, 'arccosh', RP_ARCCOSH, 1);
  SetFunc (18, 'arctanh', RP_ARCTANH, 1);

  { use RP_STOP from RpnTypes to indicate end of function list }
  Ffunction_id[19].opcode := RP_STOP;
end;



procedure TParser.initialise_scanner;
var ch : char;
begin
  for ch := chr(0) to chr(255) do Fchar_table[ord(ch)] := cSPECIAL;
  for ch := '0' to '9' do Fchar_table[ord(ch)] := cDIGIT;
  for ch := 'A' to 'Z' do Fchar_table[ord(ch)] := cLETTER;
  for ch := 'a' to 'z' do Fchar_table[ord(ch)] := cLETTER;
  Fchar_table[ord('.')] := cPOINT;
  Fchar_table[EOF_CHAR] := cETX;

  init_funcTable;
  FBehaviour :=  eInstallLeftSymbol;
end;


{ return true if it finds name in the built-in function
table and index then holds the op code }
function TParser.search_functionId (name : string; var index : integer) : boolean;
var i : integer;
begin
  name := LowerCase (name);  { drop the case just in case the user has used upper case }
  i := 1;
  repeat
     if name = Ffunction_id[i].name then
        begin
        index := i;{Ffunction_id[i].opcode;}
        result := true;
        exit;
        end;
     inc (i);
  until Ffunction_id[i].opcode = RP_STOP;
  index := 0;
  result := false;
end;


{ Update ch to next character in input stream }
procedure TParser.nextchar;
begin
  if Fposition > Flen_expr then
     Fch := char(EOF_CHAR)
  else
     begin
     Fch := Fexpression [Fposition];
     inc (Fposition);
     end;
end;


procedure TParser.skip_blanks;
begin
  while Fch = ' ' do nextchar;
end;


{ Scan in a number token }
procedure TParser.get_number;
var single_digit, scale, evalue : Extended; exponent_sign, digit_count : integer;
begin
  Fvalue := 0.0; evalue := 0.0; exponent_sign := 1;
  { check for decimal point just in case user has typed some thing like .5 }
  if Fch <> '.' then
     repeat
       single_digit := ord (Fch) - ord ('0');
       Fvalue := 10*Fvalue + single_digit;
       nextchar;
     until Fchar_table[ord(Fch)] <> cDIGIT;

  scale := 1;
  if Fch = '.' then
     begin
     { start collecting fractional part }
     nextchar;
     if Fchar_table[ord(Fch)] <> cDIGIT then
        begin
        FreeRpnExpr;
        raise ESyntaxError.Create ('Syntax error: expecting number after decimal point');
        end;

     while Fchar_table[ord(Fch)] = cDIGIT do
       begin
       scale := scale * 0.1;
       single_digit := ord (Fch) - ord ('0');
       Fvalue := Fvalue + (single_digit * scale);
       nextchar;
       end;
     {until char_table[ord(ch)] <> DIGIT;}
     end;
  { next check for scientific notation }
  if (Fch = 'e') or (Fch = 'E') then
     begin
     nextchar;
     if (Fch = '-') or (Fch = '+') then
        begin
        if Fch = '-' then exponent_sign := -1;
        nextchar;
        end;
     { accumulate exponent, check that firat ch is a digit }
     if Fchar_table[ord(Fch)] <> cDIGIT then
        begin
        FreeRpnExpr;
        raise ESyntaxError.Create ('Syntax error: number expected in exponent');
        end;

     digit_count := 0;
     repeat
       inc (digit_count);
       single_digit := ord (Fch) - ord ('0');
       evalue := 10*evalue + single_digit;
       nextchar;
     until (Fchar_table[ord(Fch)] <> cDIGIT) or (digit_count > MAX_DIGIT_COUNT);

     if digit_count > MAX_DIGIT_COUNT then
        begin
        FreeRpnExpr;
        raise ESyntaxError.Create ('Syntax error: too many digits in exponent');
        end;

     evalue := evalue * exponent_sign;
     evalue := power (10.0, evalue);
     Fvalue := Fvalue * evalue;
     end;
  Ftoken := tNUMBER;
end;


{ Scan in an identifier token }
procedure TParser.get_word;
begin
  Fname := '';
  while (Fchar_table [ord(Fch)] = cLETTER) or (Fchar_table[ord(Fch)] = cDIGIT) do
        begin
          Fname := Fname + Fch;
          nextchar;
        end;
  FToken := tIdentifier;
end;



{ Get special tokens }
procedure TParser.get_special;
begin
  case Fch of
     '+'  : Ftoken := tPlus;
     '-'  : Ftoken := tMinus;
     '*'  : Ftoken := tMult;
     '/'  : Ftoken := tSlash;
     '^'  : Ftoken := tPower;
     '('  : Ftoken := tLparen;
     ')'  : Ftoken := tRparen;
     '='  : Ftoken := tEquals;
     ';'  : Ftoken := tSEMICOLON;
     '''' : Ftoken := tApostrophy;
  else
     Ftoken := tError;
  end;
  nextchar;
end;


{ Get the next token }
procedure TParser.get_token;
  var i : integer;
begin
  { check if a token has been pushed back into the token stream, if so use it first }
  if Fprevious_token <> tEMPTY then
     begin
     Ftoken := Fprevious_token;
     Fname  := Fprevious_name;
     Fvalue := Fprevious_value;
     Fprevious_token := tEMPTY;
     exit;
     end;

  skip_blanks;

  case Fchar_table[ord(Fch)] of

     cLETTER :  begin get_word; end;
     cPOINT  :  get_number; { just in case user has entered .5 etc }
     cDIGIT  :  get_number;
     cETX    :  Ftoken := End_of_string;
   else
     get_special;
  end;
end;


{ push token back into token stream }
procedure TParser.unget_token;
begin
  Fprevious_token := Ftoken;
  Fprevious_name  := Fname;
  Fprevious_value := Fvalue;
end;


{ -------------------------------------------------------------------- }
{ Some debugging routines for internal use }

{ Debugging routines, not for general use }
{procedure show_token;
begin
    case token of
       EMPTY      : writeln ('<No token>');
       Identifier : writeln ('<Word>');
       Number     : writeln ('<Number:', value:8, '>');
       Plus       : writeln ('<Plus>');
       Minus      : writeln ('<Minus>');
       Mult       : writeln ('<Mult>');
       Slash      : writeln ('<Divide>');
       cPower     : writeln ('<Power>');
       Error      : writeln ('<Error!!!>');
       End_of_string : writeln ('<Reached end of string>');
    end;
end;}

{ For debugging only }
{procedure display_rpn_expr (rpn_array : pRpnArrayType);
var i : integer;
begin
  i := 1;
  repeat
    if rpn_array^[i].value = RP_CONSTANT then
       begin
       write ('<CONSTANT:');
       inc (i); writeln (rpn_array^[i].constant);
       end
    else
       case rpn_array^[i].value of
          -1 : writeln ('<+>');
          -2 : writeln ('<->');
          -3 : writeln ('<*>');
          -4 : writeln ('</>');
          -5 : writeln ('<^>');
       else
          writeln (rpn_array^[i].value);
       end;
    inc (i);
  until rpn_array^[i].value = RP_STOP;
  writeln ('<RP_STOP>');
end;}


function TParser.ReturnRpnString : string;
var p : pRpnArrayType; i : integer;
begin
  i := 1; p := FRpnExpr^.expr; result := '';
  repeat
    if p^[i].value = RP_OPERAND then
       begin
       inc (i); result := result + (p^[i].operand)^.name + ' ';
       end
    else if p^[i].value = RP_CONSTANT then
       begin
       inc (i);
       result := result + floattostr (p^[i].constant) + ' ';
       end
    else
       case p^[i].value of
            RP_EQUALS   : begin
                          result := result + '= '; inc (i);
                          result := result + (p^[i].operand)^.name + ' ';
                          end;
            RP_ADD      : result := result + '+ ';
            RP_SUBTRACT : result := result + '- ';
            RP_MULTIPLY : result := result + '* ';
            RP_DIVIDE   : result := result + '/ ';
            RP_POWER    : result := result + '^ ';
            RP_SIN      : result := result + 'sin ';
            RP_COS      : result := result + 'cos ';
            RP_TAN      : result := result + 'tan ';
            RP_ARCSIN   : result := result + 'arcsin ';
            RP_ARCCOS   : result := result + 'arccos ';
            RP_ARCTAN   : result := result + 'arctan ';

            RP_SINH     : result := result + 'sinh ';
            RP_COSH     : result := result + 'cosh ';
            RP_TANH     : result := result + 'tanh ';

            RP_ARCSINH  : result := result + 'arcsinh ';
            RP_ARCCOSH  : result := result + 'arccosh ';
            RP_ARCTANH  : result := result + 'arctanh ';

            RP_EXP      : result := result + 'exp ';
            RP_LOG      : result := result + 'log ';
            RP_LN       : result := result + 'ln ';
            RP_SQR      : result := result + 'sqr ';
            RP_SQRT     : result := result + 'sqrt ';
            RP_FACT     : result := result + 'fact ';
            RP_UNARYMINUS : result := result + 'UMINUS ';
       end;
    inc (i);
  until p^[i].value = RP_STOP;
end;


{ ------------------------------------------------------------------------ }

{ Checks that there is enough space to store next instruction, if not then
it will grow the array space autmatically }
procedure TParser.CheckEnoughSpace;
begin
  if FRpnExpr^.LengthUsed+1 > FRpnExpr^.MaxLength then
     begin
     try
       { Not enough space, so reallocate heap space with GROWTH extra spaces }
       ReAllocMem(FRpnExpr^.expr, (FRpnExpr^.MaxLength+GROWTH)*Sizeof (TrpnElement));
       FRpnExpr^.MaxLength := FRpnExpr^.MaxLength + GROWTH;
     except
       on EOutofMemory do
          begin
          { Release what I know for certain to exist and pass the exception on }
          FRpnExpr^.expr := Nil;
          FreeRpnExpr;
          raise;
          end;
     end;
     end;
  inc (FRpnExpr^.LengthUsed);
end;


procedure TParser.StoreInst (inst : integer);
begin
  CheckEnoughSpace;   { incs LengthUsed and allocates more heap if necessary }
  FRpnExpr^.expr^[FRpnExpr^.LengthUsed].value := inst;
end;


procedure TParser.StoreSymbol (symb : pTSymTabNode);
begin
  CheckEnoughSpace;
  FRpnExpr^.expr^[FRpnExpr^.LengthUsed].operand := symb;
end;


procedure TParser.StoreConstant (c : Extended);
begin
  CheckEnoughSpace;
  FRpnExpr^.expr^[FRpnExpr^.LengthUsed].constant := c;
end;


{ Parse function calls of the sort: f (x, y, .... ) }
procedure TParser.ParseFunction (index : integer);
var arg_count : integer;
begin
  get_token;
  if Ftoken <> tLparen then
     begin
     FreeRpnExpr;
     raise EMissingLParen.Create ('Syntax error: missing left bracket');
     end;
  get_token;
  doexpression; arg_count := 1;  { stating counting the number of arguments }
  while Ftoken = tCOMMA do
        begin
        inc (arg_count);
        get_token;
        doexpression;
        end;

  if Ftoken <> tRparen then
     begin
     FreeRpnExpr;
     raise EMissingRParen.Create ('Syntax error: missing right bracket');
     end;
  { Check that the number of arguments found matches the expected number }
  if arg_count = Ffunction_id[index].nArgs then
     { enter function id into instruction stream, not an Instruction as it appears }
     StoreInst (Ffunction_id[index].opcode)
  else
     begin
     FreeRpnExpr;
     raise ESyntaxError.Create ('Syntax error: incorrect number of arguments in function call <'
                                 + Ffunction_id[index].name + '>');
     end;
end;



{ Parse for constants, identifier, functions or '(' expressions ')' }
procedure TParser.factor;
var index : integer; np : pTSymTabNode;
begin
  case Ftoken of
     tNumber :
         begin
         StoreInst (RP_CONSTANT);
         StoreConstant (Fvalue);
         get_token;
         end;

     tIdentifier:
         begin
         { Check to see whether the name is a function identifier }
         if search_functionId (Fname, index) then
            ParseFunction (index)
         else
            begin
            { identifier wasn't a function name, so check in symbol table..}
            { Search the symbol table. Nil if the name cannot be found }
            np := FSymTab.find_symbol (Fname);
            if np = Nil then
               begin
               { Check behaviour setting }
               if (FBehaviour = eInstallRightSymbols) or (FBehaviour = eInstallAllSymbols) then
                  { i.e user wishes to accept undeclared identifiers, add to symbol table, init to 0.0 }
                  np := FSymTab.insert (Fname, 0.0)
               else
                   begin
                   { User requests reporting of undeclared lefthand variables as an error }
                   FreeRpnExpr;
                   raise EUnknownIdentifier.CreateFmt ('Syntax error: Unrecognised identifier <%s> in expression', [Fname]);
                   end;
               end;
            { add operand to rpn expression }
            StoreInst (RP_OPERAND);
            StoreSymbol (np);
            end;

         get_token;
         end;
     tLparen :
         begin
         get_token;
         doexpression;
         if (Ftoken = tRparen) then
            get_token
         else
            begin
            FreeRpnExpr;
            raise EMissingRParen.Create ('Syntax error: missing right bracket');
            end
         end;
     else
         begin
         FreeRpnExpr;
         raise ESyntaxError.Create ('Syntax error: expecting number, identifier or left bracket');
         end;
  end;
end;


{ Allows one to deal with power operators '^', sym ^ sym }
procedure TParser.super_term;
var op : TtokenCodes;
begin
  factor;
  while (Ftoken = tPower) do
        begin
        op := Ftoken;
        get_token;
        factor;
        StoreInst (RP_POWER);
        end;
end;


{ Parse things like sym * sym or sym / sym }
procedure TParser.term;
var op : TtokenCodes;
begin
  super_term;
  while (Ftoken = tMULT) or (Ftoken = tSLASH) do
     begin
       op := Ftoken;
       get_token;
       super_term;

       case op of
          tMULT :  StoreInst (RP_MULTIPLY);
          tSLASH : StoreInst (RP_DIVIDE);
       end;
     end;
end;


{ Parse things like sym + sym or sym - sym, checking also fo runary operators }
procedure TParser.doexpression;
var op, unary_op : TtokenCodes;
begin
  { remember unary + or - if there is one }
  unary_op := tPlus;
  if (Ftoken = tPlus) or (Ftoken = tMinus) then
     begin
     unary_op := Ftoken;
     get_token;
     end;

  term;
  if (unary_op = tMinus) then StoreInst (RP_UNARYMINUS);

  while (Ftoken = tPLUS) or (Ftoken = tMINUS) do
     begin
       op := Ftoken;  { remember the token }
       get_token;
       term;

       case op of
          tPLUS  : StoreInst (RP_ADD);
          tMINUS : StoreInst (RP_SUBTRACT);
       end;
     end;
end;


{ Process expression of type, y=f(x). y= has already been parsed and y string
is stored in variable lefthand_name }
procedure TParser.doassignment (lefthand_name : string);
var index : integer; np : pTSymTabNode;
begin
  { Search for identifier 'y', Nil if not found }
  np := FSymTab.find_symbol (Lefthand_name);
  if np = Nil then
     begin
     { Check behaviour setting }
     if (FBehaviour = eInstallLeftSymbol) or (FBehaviour = eInstallAllSymbols) then
        begin
        { i.e user wishes to accept undeclared assigned identifiers, add to symbol table, init to 0.0 }
        np := FSymTab.insert (Lefthand_name, 0.0);
        end
     else
        begin
        { User requests reporting of undeclared lefthand variables as an error }
        FreeRpnExpr;
        raise ESyntaxError.CreateFmt ('Syntax error: Unrecognised identifier <%s> on left of expression', [Lefthand_name]);
        end;
     end;

  get_token;
  doexpression;
  StoreInst (RP_EQUALS);
  StoreSymbol (np);
end;




procedure TParser.dostatement;
var savedname : string;
begin
  if Ftoken = tIdentifier then
     begin
     savedname := Fname;
     get_token;    { look ahead for '=' token }
     if Ftoken = tEQUALS then doassignment (savedname) else
        begin
        { Not an assignment, therefore Unwind back to start before processing expression}
        unget_token;
        Ftoken := tIdentifier;
        Fname  := savedname;
        doexpression;
        end;
     end
  else
     doexpression;
end;


{ Allocate some initial space for the reverse polish notation expression }
procedure TParser.AllocRpnExpression (size : integer);
begin
  try
    FRpnExpr := AllocMem (sizeof (TrpnExpr));
    FRpnExpr^.MaxLength  := size;
    FRpnExpr^.LengthUsed := 0;
    FRpnExpr^.expr       := Nil;
    try
      { Add one to the allocation so that we start to index from ONE }
      FRpnExpr^.expr := AllocMem ((size+1)*Sizeof (TrpnElement));
    except
      on EOutofMemory do
         begin
         FRpnExpr^.expr := Nil;
         FreeRpnExpr;
         raise;
         end;
    end;
  except
      on EOutofMemory do
         begin
         FRpnExpr^.expr := Nil;
         FreeRpnExpr;
         raise;
         end;
  end;
end;


{ compiles an expression into an internal static array called rpn_array }
procedure TParser.ParseExp (expr : string; SymTab : TBinarySearchTree; ClearRPN : boolean);
var i, size : integer; OldRpn : pTrpn;
begin
  { make a local copy of the symbol table pointer so that it is
    accessible throughout the unit }
  FSymTab := SymTab;

  { make a local copy of the expressiob string so that it is
    accessible through out the unit }
  Fexpression := expr; Flen_expr := length (Fexpression);
  Fposition := 1; Fprevious_token := tEMPTY;

  if ClearRPN or (FRpnExpr = Nil) then
     begin
     { Free any previously allocated rpn space}
     FreeRpnExpr;
     { Create space for the rpn expression }
     AllocRpnExpression (DEFAULT_MAX_SIZE);
     end
  else
     begin
     OldRpn := FRpnExpr;
     AllocRpnExpression (DEFAULT_MAX_SIZE + OldRpn^.LengthUsed);
     { Copy OldRpn into new space and set LengthUsed }
     size := sizeof (OldRpn^.expr^[1]);
     { Note the -1! Don't copy over the STOP instruction }
     for i := 1 to OldRpn^.LengthUsed-1 do
         Move (OldRpn^.expr^[i], FRpnExpr^.expr^[i], size);
     FRpnExpr^.LengthUsed := OldRpn^.LengthUsed-1;

     { Now destroy the old copy, just in case make sure it hasn't been destroyed already ! }
     if OldRpn <> Nil then
        begin
        if OldRpn^.expr <> Nil then
           FreeMem (OldRpn^.expr, (OldRpn^.MaxLength+1)*Sizeof (TrpnElement));
        if OldRpn <> Nil then
           FreeMem (OldRpn, sizeof (TrpnExpr));
        OldRpn := Nil;
        end;
     end;

  { start scan by fetching first character }
  nextchar;
  get_token;
  dostatement;   { Note statments can be spearated by ';' }
  while (Ftoken = tSEMICOLON) and (Ftoken <> End_of_String) do
        begin
        get_token;
        if Ftoken = End_of_String then break;
        dostatement;
        end;

  { At this point we should have reached end of string, if we havn't
  then it's a syntax error }
  if Ftoken <> End_of_String then
     begin
     FreeRpnExpr;
     raise ESyntaxError.Create ('Syntax error: reached end of equation too soon, something missing?');
     end;
  { ...and don't forget the terminator instruction }
  StoreInst (RP_STOP);
  { --------------------------- END PARSING ----------------------------- }

  { Reduce size of memory used to what's required }
  If FRpnExpr^.LengthUsed < FRpnExpr^.MaxLength then
     begin
     try
       ReAllocMem(FRpnExpr^.expr, (FRpnExpr^.LengthUsed+1)*Sizeof (TrpnElement));
       FRpnExpr^.MaxLength := FRpnExpr^.LengthUsed;
     except
       on EOutofMemory do
          begin
          FRpnExpr^.expr := Nil;
          FreeRpnExpr;
          raise;
          end;
     end;
     end;
end;


{ ********************* END OF PARSER ROUTINES **************************** }


{ ------------------------------------------------------------------------- }
{ -------              EVALUATOR ROUTINES FOLLOW                            }
{ ------------------------------------------------------------------------- }



{ public routines for changing behaviour of trifg functions, i.e whether the
  trig functions accept radians or degrees }


constructor TEvaluator.Create;
begin
  inherited Create;
  { Initialise Constants and Evaluator runtime stack }
  {Log_e := 0.4342944819032518;     { Log10 of e }
  FStack := Nil;     { Holds the stack pointer }
  SetStack (DEFAULT_STACK_SIZE);
  //Use_Radians;
  Use_Degrees;
end;


destructor TEvaluator.Destroy;
begin
  FreeStack;
  inherited Destroy;
  { Do not destroy rpn expression since this is owned by the Parser ! }
end;


procedure TEvaluator.Use_radians;
begin
  FUseRadians := true;
end;


procedure TEvaluator.Use_degrees;
begin
  FUseRadians := false;
end;


{ Set size of dynamic stack }
procedure TEvaluator.SetStack (size : integer);
begin
  { Destroy old stack if necessary, could use ReAllocMem I suppose }
  if FStack <> Nil then FreeMem (FStack, sizeof (Extended) * FStackSize);
  FStackSize := size;
  FStackSize_Minus_1 := size - 1;
  try
    FStack := AllocMem (sizeof (Extended) * size);
  except
    on EOutofMemory do
       begin
       FStack := Nil;
       raise;  { Just pass to user and hope they've got a finally to release rpn expr }
       end;
  end;
end;


{ Returns current size of runtime stack }
function TEvaluator.GetStack : integer;
begin
  result := FStackSize;
end;


{ Destroy space allocated for stack }
procedure TEvaluator.FreeStack;
begin
  if FStack <> Nil then FreeMem (FStack, sizeof (Extended) * FStackSize);
  FStack := Nil;
end;



{ ---------------------------------------------------------------------- }


{ Something missing from Pascal!
  Compute x^n where n may be a fractional value, eg 1.2^3.4 }
function power (x : Extended; n : Extended) : Extended;
var int_of_n : Extended; intn : integer;
begin
  if n = 0.0 then begin result := 1.0; exit; end; { save going through exp etc }
  if x > 0 then
     result := exp (n*ln(x))
  else if x = 0 then
       result := 0.0
  else if x < 0 then
     begin
     { check if n is an integer }
     int_of_n := int (n);
     intn := round (int_of_n);
     if int_of_n = n then    { if it's an integer }
        begin
        result := exp (n*ln(abs(x)));
        { now work out the sign }
        if (odd (intn)) and (int_of_n <> 0) then result := result * (-1);
        end
     else
        begin
        { Perhaps this is what the user wanted ?? }
        result := exp (n*ln(abs(x)));
        result := -result;
        {raise EMathPower.Create ('Error calculating root of negative number in power');}
        end;
     end;
end;

{ --------------------------------------------------------------------------- }
{                         Much Optimised version of run                       }
{ --------------------------------------------------------------------------- }


{ evaluate reverse polish expression, fully optimiased for speed }
function TEvaluator.run (rp_expr : pTrpn; SymTab : TBinarySearchTree) : TEvalType;
var i, k : integer; symIndex : pTSymTabNode;
    p : pRpnArrayType; x : Extended; xi : integer;
begin
  { scan reverse polish expression until we come across the stop code }

  { If the expression is an assignment, then the routine returns a TEvalType
  record which holds the value of the right-hand side and a pointer to the
  variables which was assigned }

  symIndex := Nil;  { in case expression is not an assignment function }
  if FUseRadians then TrigF := 1.0 else TrigF := Pi/180;
  { Initialise runtime stack }
  Fstack_top := -1; i := 1; p := rp_expr^.expr;
  repeat    { infinite loop, get out of case statement when RS_STOP instruction encountered }

  { Due the numbering of the RP_ constants, this case statement is compiled
    by delphi into a fast jump table }
  case p^[i].value of

     RP_STOP       : begin  { finally pop off the answer and return to caller }
                     result.value := Fstack^[Fstack_top];
                     result.SymTabPtr := symIndex;  { set to nil if expression not an assignment }
                     exit;
                     end;

     RP_CONSTANT   : begin inc (i);
                     if Fstack_top = FStackSize_MINUS_1 then
                        raise ERTStackOverFlow.Create ('Evaluation Stack overflow during Constant push');
                     inc (Fstack_top); Fstack^[Fstack_top] := p^[i].constant;
                     end;

     RP_OPERAND    : begin inc (i);
                     if Fstack_top = FStackSize_MINUS_1 then
                        raise ERTStackOverFlow.Create ('Evaluation Stack overflow during Operand push');

                     inc (Fstack_top);
                     { Use the official calls to get to symbol table, best not
                     go direct to the symbol table , a comprimise }
                     Fstack^[Fstack_top] := SymTab.GetSymTabValue (p^[i].operand);
                     {Direct method: stack[stack_top] := (p^[i].operand)^.value;}
                     end;

     RP_ADD        : begin
                     dec(Fstack_top);
                     Fstack^[Fstack_top] := Fstack^[Fstack_top] + Fstack^[Fstack_top+1];
                     end;

     RP_SUBTRACT   : begin
                     dec(Fstack_top);
                     Fstack^[Fstack_top] := Fstack^[Fstack_top] - Fstack^[Fstack_top+1];
                     end;

     RP_MULTIPLY   : begin
                     dec(Fstack_top);
                     Fstack^[Fstack_top] := Fstack^[Fstack_top] * Fstack^[Fstack_top+1];
                     end;

     RP_DIVIDE     : begin
                     dec(Fstack_top);
                     { Let the user catch divide by zero errors ! }
                     Fstack^[Fstack_top] := Fstack^[Fstack_top] / Fstack^[Fstack_top+1];
                     end;

     RP_POWER      : begin
                     dec(Fstack_top);
                     Fstack^[Fstack_top] := power (Fstack^[Fstack_top], Fstack^[Fstack_top+1]);
                     end;
     RP_SIN        : Fstack^[Fstack_top] := sin (Fstack^[Fstack_top]*TrigF);
     RP_COS        : Fstack^[Fstack_top] := cos (Fstack^[Fstack_top]*TrigF);
     RP_TAN        : begin x := Fstack^[Fstack_top]*TrigF;
                            Fstack^[Fstack_top] := sin(x)/cos(x) end;

     RP_EXP        : Fstack^[Fstack_top] := exp(Fstack^[Fstack_top]);
     RP_LOG        : Fstack^[Fstack_top] := ln(Fstack^[Fstack_top]) * 0.4342944819032518;{ Log10 of e }
     RP_LN         : Fstack^[Fstack_top] := ln(Fstack^[Fstack_top]);
     RP_SQR        : Fstack^[Fstack_top] := sqr(Fstack^[Fstack_top]);
     RP_SQRT       : Fstack^[Fstack_top] := sqrt(Fstack^[Fstack_top]);

     RP_FACT       : begin
                     xi := round (Fstack^[Fstack_top]); x := 1.0;
                     for k := 1 to xi do x := x * k;
                     Fstack^[Fstack_top] := x;
                     end;

     RP_ARCSIN     : begin x := Fstack^[Fstack_top];
                           Fstack^[Fstack_top] := (ArcTan (x/(sqrt(1-sqr(x)))))/TrigF; end;
     RP_ARCCOS     : begin x := Fstack^[Fstack_top];
                           Fstack^[Fstack_top] := (ArcTan ((sqrt(1-sqr(x))/x)))/TrigF; end;
     RP_ARCTAN     : Fstack^[Fstack_top] := (ArcTan (Fstack^[Fstack_top]))/TrigF;

     RP_SINH       : begin x := exp (Fstack^[Fstack_top]); Fstack^[Fstack_top] := (x - (1/x))/2; end;
     RP_COSH       : begin x := exp (Fstack^[Fstack_top]); Fstack^[Fstack_top] := (x + (1/x))/2; end;
     RP_TANH       : begin x := exp (2*Fstack^[Fstack_top]); Fstack^[Fstack_top] := (x-1)/(x+1); end;

     RP_ARCSINH    : Fstack^[Fstack_top] := ln(Fstack^[Fstack_top] + sqrt (sqr (Fstack^[Fstack_top]) + 1));
     RP_ARCCOSH    : Fstack^[Fstack_top] := ln(Fstack^[Fstack_top] + sqrt (sqr (Fstack^[Fstack_top]) - 1));
     RP_ARCTANH    : Fstack^[Fstack_top] := ln((1 + Fstack^[Fstack_top])/(1 - Fstack^[Fstack_top]))/2;

     RP_UNARYMINUS : Fstack^[Fstack_top] := -Fstack^[Fstack_top];

     RP_EQUALS     : begin
                     { assignment operation, result already on stack and
                     right-hand side to follow in instruction stream as an
                     operand pointer into symbol table }
                     inc (i);
                     SymTab.SetSymTabValue (p^[i].operand, Fstack^[Fstack_top]);
                     { Direct method: (p^[i].operand)^.value := stack[stack_top];}
                     symIndex := p^[i].operand;
                     end;
    else
       raise EEvaluationError.Create ('Internal Evaluation Error: unrecognised operator');
  end;
  inc (i);
  until false;
end;



end.

