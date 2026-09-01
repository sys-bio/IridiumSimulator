unit uRateLawModelSource;

{ Iridium's implementation of IModelSource, over libantimony.

  This is the only place the rate law checker touches anything native. The
  engine itself is RTL-only by design and sees a model exclusively through
  IModelSource, which is what lets the same engine run under the console test
  harness with no DLL at all.

  IT SNAPSHOTS, IT DOES NOT QUERY LAZILY.

  libantimony is global mutable state: there is one "current model" per
  process, and loading anything replaces it. Iridium loads models into it all
  the time -- TModelSession.EnsureLoaded does it on every reload, the SBML
  import does it, the BioModels download does it. An IModelSource that called
  through to libantimony on each method would therefore answer questions about
  whatever was loaded most recently rather than about the model it was created
  for, and the failure would be silent and intermittent: a check that was
  correct when run alone and wrong when run after a reload.

  So the constructor loads the text, copies everything out, and never talks to
  the library again. The object is inert afterwards and safe to hold across
  anything the rest of the application does.

  It also means freeAll can be called immediately -- libantimony leaks
  otherwise, and the checker asks it a lot of questions. }

interface

uses
  System.SysUtils, System.Classes, System.Math, System.Generics.Collections,
  uAntimonyAPI, uAntimonyTypes,
  RateLaw.Types;

type
  TAntimonyModelSource = class(TInterfacedObject, IModelSource)
  private
    type
      TRx = record
        Id:        string;
        RateLaw:   string;
        Reactants: TSpeciesRefs;
        Products:  TSpeciesRefs;
        Modifiers: TModifierRefs;
        Annotation: string;
        Line:      Integer;
      end;
      TSym = record
        Kind:       TSymbolKind;
        Constant:   Boolean;
        HasVal:     Boolean;
        Value:      Double;
        Assignment: string;
      end;
      TFn = record
        Args: TArray<string>;
        Body: string;
      end;
  private
    FReactions: TList<TRx>;
    FSymbols:   TDictionary<string, TSym>;
    FFunctions: TDictionary<string, TFn>;
    FOk:        Boolean;
    FLastError: string;
    procedure Snapshot(const AAntimonyText: string);
    procedure SnapshotSymbols(ARType: TReturnType; AKind: TSymbolKind;
                              AConstant: Boolean = False);
    procedure SnapshotModifiers;
    function  InRange(AIndex: Integer): Boolean;
  public
    { Loads AAntimonyText into libantimony and copies everything out. Never
      raises: a model that will not parse is the normal case for a checker,
      and is reported through Ok/LastError with zero reactions. }
    constructor Create(const AAntimonyText: string);
    destructor  Destroy; override;

    { False when the text would not load. The checker can still be run -- it
      simply has nothing to look at -- so callers should say so rather than
      showing an empty report. }
    property Ok: Boolean read FOk;
    property LastError: string read FLastError;

    { IModelSource }
    function ReactionCount: Integer;
    function ReactionId   (AIndex: Integer): string;
    function RateLawText  (AIndex: Integer): string;
    function Reactants    (AIndex: Integer): TSpeciesRefs;
    function Products     (AIndex: Integer): TSpeciesRefs;
    function Modifiers    (AIndex: Integer): TModifierRefs;
    function SymbolKind   (const AName: string): TSymbolKind;
    function HasValue     (const AName: string): Boolean;
    function ValueOf      (const AName: string): Double;
    function AssignmentRule(const AName: string): string;
    function IsConstant   (const AName: string): Boolean;
    function UserFunction (const AName: string; out AArgs: TArray<string>;
                           out ABody: string): Boolean;
    function KnowsSymbolKinds: Boolean;
    function AnnotatedLaw (AIndex: Integer): string;
    function SourceLineOf (AIndex: Integer): Integer;
  end;

implementation

var
  GInv: TFormatSettings;

{ ------------------------------------------------------------------ helpers }

{ Antimony gives no source positions, so the line is recovered by looking for
  the reaction's own label in the text. Purely for the UI's benefit -- nothing
  in the engine reads it -- so an approximate answer beats no answer, and -1
  is returned rather than a guess when the label cannot be found. }
function FindReactionLine(ALines: TStringList; const AId: string;
  AOrdinal: Integer): Integer;
var
  I, P, Seen: Integer;
  L, T: string;
begin
  Result := -1;
  if AId <> '' then
    for I := 0 to ALines.Count - 1 do
    begin
      L := ALines[I];
      P := Pos(AId, L);
      if P = 0 then Continue;
      { The label form 'J1:' -- and only at a token boundary, or a reaction
        called J1 would match inside J12. }
      if (P + Length(AId) <= Length(L)) and (L[P + Length(AId)] = ':') then
        if (P = 1) or not CharInSet(L[P - 1], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
          Exit(I + 1);
    end;

  { Antimony auto-names an unlabelled reaction '_J1', '_J2' and so on, and
    that name appears nowhere in the user's file -- most real models are
    written without labels, so the search above finds nothing for any of them.
    Falling back to the Nth line that looks like a reaction makes the report
    navigable for exactly those models. }
  if AOrdinal < 0 then Exit;
  Seen := 0;
  for I := 0 to ALines.Count - 1 do
  begin
    T := Trim(ALines[I]);
    if T.StartsWith('//') or T.StartsWith('#') then Continue;
    if (Pos('->', T) = 0) and (Pos('=>', T) = 0) then Continue;
    if Seen = AOrdinal then Exit(I + 1);
    Inc(Seen);
  end;
end;

{ The law a reaction was annotated with.

    # @ratelaw michaelis_menten_irrev
    J1: S -> P; Vm*S/(Km + S);

  A standalone comment rather than a command inside the simulation-metadata
  block. The block is specified in Antimony_MetaData_Support and names Iridium
  as its reference implementation, so adding a command there is a change to
  THAT specification; and a metadata command is per-model where this has to be
  per-reaction. Both '#' and '//' are accepted because Antimony accepts both.

  The annotation attaches to the next reaction line beneath it, or may sit at
  the end of the reaction's own line. }
function ScanAnnotations(ALines: TStringList): TArray<string>;
const
  Tag = '@ratelaw';
var
  I, P, Q: Integer;
  T, Pending, Id: string;
  IsComment, LooksLikeReaction: Boolean;
begin
  Result := nil;
  Pending := '';
  for I := 0 to ALines.Count - 1 do
  begin
    T := Trim(ALines[I]);
    if T = '' then Continue;

    IsComment := T.StartsWith('//') or T.StartsWith('#');
    P := Pos(Tag, T);

    if P > 0 then
    begin
      { Only inside a comment: '@ratelaw' in running model text is not an
        annotation and must not be read as one. }
      Q := Pos('//', T);
      if Q = 0 then Q := Pos('#', T);
      if (Q > 0) and (Q < P) then
      begin
        Id := Trim(Copy(T, P + Length(Tag), MaxInt));
        { Stop at the first whitespace: trailing prose after the id is fine. }
        Q := 1;
        while (Q <= Length(Id)) and not CharInSet(Id[Q], [' ', #9]) do Inc(Q);
        Id := Copy(Id, 1, Q - 1);
        if IsComment then Pending := Id;   { applies to the next reaction }
      end;
    end;

    if IsComment then Continue;

    LooksLikeReaction := (Pos('->', T) > 0) or (Pos('=>', T) > 0);
    if not LooksLikeReaction then Continue;

    { A trailing annotation on the reaction's own line wins over a pending
      one above it: it is the more specific statement. }
    if (P > 0) and (Id <> '') then
    begin
      Result := Result + [Id];
      Pending := '';
    end
    else
    begin
      Result := Result + [Pending];
      Pending := '';
    end;
    Id := '';
  end;
end;

function RefsFrom(const ANames: TArray<string>;
                  const AStoich: TArray<Double>;
                  ARxn: Integer; AReactantSide: Boolean): TSpeciesRefs;
var
  I: Integer;
  V: Double;
  Txt: string;
begin
  SetLength(Result, Length(ANames));
  for I := 0 to High(ANames) do
  begin
    if I <= High(AStoich) then V := AStoich[I] else V := 1;

    { A stoichiometry written as a symbol ('S1 + n S2 => S3') comes back as
      NaN from the numeric accessor; the string accessor has the symbol's
      name. Generative laws instantiate their exponents from this, so losing
      the symbolic form would turn n into 'not a number'. }
    Txt := '';
    if IsNan(V) then
      try
        if AReactantSide then
          Txt := getNthReactionMthReactantStoichiometryString(ARxn, I)
        else
          Txt := getNthReactionMthProductStoichiometryString(ARxn, I);
      except
        Txt := '';
      end;

    Result[I] := TSpeciesRef.Make(ANames[I], V, Txt);
  end;
end;

{ ------------------------------------------------------- TAntimonyModelSource }

constructor TAntimonyModelSource.Create(const AAntimonyText: string);
begin
  inherited Create;
  FReactions := TList<TRx>.Create;
  FSymbols   := TDictionary<string, TSym>.Create;
  FFunctions := TDictionary<string, TFn>.Create;
  FOk        := False;
  Snapshot(AAntimonyText);
end;

destructor TAntimonyModelSource.Destroy;
begin
  FFunctions.Free;
  FSymbols.Free;
  FReactions.Free;
  inherited;
end;

procedure TAntimonyModelSource.SnapshotSymbols(ARType: TReturnType;
  AKind: TSymbolKind; AConstant: Boolean);
var
  Names, Eqns, Rules: TArray<string>;
  I: Integer;
  S: TSym;
  V: Double;
begin
  Names := getSymbolNamesOfType(ARType);
  Eqns  := getSymbolEquationsOfType(ARType);
  Rules := getSymbolAssignmentRulesOfType(ARType);

  for I := 0 to High(Names) do
  begin
    S := Default(TSym);
    S.Kind := AKind;
    S.Constant := AConstant;

    { getSymbolHasValue is the only thing that separates 'declared with no
      value' from 'declared as zero' -- both leave the equation empty -- and
      that distinction is the whole of defect S014. }
    try
      S.HasVal := getSymbolHasValue(Names[I]);
    except
      S.HasVal := False;
    end;

    S.Value := NaN;
    if (I <= High(Eqns)) and (Eqns[I] <> '') then
      if TryStrToFloat(Eqns[I], V, GInv) then S.Value := V;

    if I <= High(Rules) then S.Assignment := Rules[I];

    { Later types do not overwrite earlier ones: a symbol that appears in two
      overlapping return_types keeps the more specific classification it was
      given first. }
    if not FSymbols.ContainsKey(Names[I]) then
      FSymbols.Add(Names[I], S);
  end;
end;

procedure TAntimonyModelSource.SnapshotModifiers;
var
  N, I, J, K: Integer;
  Interactors, Interactees: TArray<string>;
  Divider: TReactionDivider;
  Role: TModifierRole;
  Rx: TRx;
begin
  { Antimony records a modifier only where the modeller used an interaction
    arrow -- -o activates, -| inhibits, -( is generic -- so the role is stated
    rather than inferred. Where no arrow was used there is no interaction and
    the species simply appears in the rate law; the engine treats that as an
    unspecified modifier, which is a weaker but honest signal. }
  N := getNumInteractions;
  for I := 0 to N - 1 do
  begin
    try
      Interactors := getNthInteractionInteractorNames(I);
      Interactees := getNthInteractionInteracteeNames(I);
      Divider     := getNthInteractionDivider(I);
    except
      Continue;
    end;

    case Divider of
      rdActivates:  Role := mrActivator;
      rdInhibits:   Role := mrInhibitor;
      rdInfluences: Role := mrGeneric;
    else            Role := mrUnspecified;
    end;

    { An interactee is the reaction the interaction acts on. }
    for J := 0 to High(Interactees) do
      for K := 0 to FReactions.Count - 1 do
        if FReactions[K].Id = Interactees[J] then
        begin
          Rx := FReactions[K];
          for var M := 0 to High(Interactors) do
            Rx.Modifiers := Rx.Modifiers
              + [TModifierRef.Make(Interactors[M], Role)];
          FReactions[K] := Rx;
        end;
  end;
end;

procedure TAntimonyModelSource.Snapshot(const AAntimonyText: string);
var
  I, N: Integer;
  Rx: TRx;
  Lines: TStringList;
  Fn: TFn;
  FnName: string;
  Annots: TArray<string>;
begin
  FLastError := '';
  if Trim(AAntimonyText) = '' then
  begin
    FLastError := 'The model is empty.';
    Exit;
  end;

  try
    loadAntimonyString(AAntimonyText);
  except
    on E: Exception do
    begin
      { Not an error worth raising: a model that will not parse is exactly the
        model a checker is most needed for, and the caller has better things
        to say about it than an exception dialog. }
      FLastError := E.Message;
      Exit;
    end;
  end;

  Lines := TStringList.Create;
  try
    Lines.Text := AAntimonyText;
    { Scanned from the text, because libantimony discards comments. The
      ordering assumption -- the Nth reaction-looking line is the Nth reaction
      -- is the same one SourceLineOf already relies on. }
    Annots := ScanAnnotations(Lines);

    try
      { --- reactions --- }
      N := getNumReactions;
      for I := 0 to N - 1 do
      begin
        Rx := Default(TRx);
        Rx.Id      := getNthReactionName(I);
        Rx.RateLaw := getNthReactionRate(I);
        Rx.Reactants := RefsFrom(getNthReactionReactantNames(I),
                                 getNthReactionReactantStoichiometries(I),
                                 I, True);
        Rx.Products  := RefsFrom(getNthReactionProductNames(I),
                                 getNthReactionProductStoichiometries(I),
                                 I, False);
        Rx.Line := FindReactionLine(Lines, Rx.Id, I);
        if I <= High(Annots) then Rx.Annotation := Annots[I];
        FReactions.Add(Rx);
      end;

      SnapshotModifiers;

      { --- symbols ---
        Most specific first, because SnapshotSymbols does not overwrite: a
        boundary species must be classified as a species, not as whatever
        broader category also contains it. }
      SnapshotSymbols(rtVarSpecies,       skSpecies);
      SnapshotSymbols(rtConstSpecies,     skSpecies,     True);
      SnapshotSymbols(rtAllSpecies,       skSpecies);
      SnapshotSymbols(rtVarCompartments,  skCompartment);
      SnapshotSymbols(rtConstCompartments, skCompartment, True);
      SnapshotSymbols(rtAllCompartments,  skCompartment);
      SnapshotSymbols(rtVarFormulas,      skParameter);
      SnapshotSymbols(rtConstFormulas,    skParameter,   True);
      SnapshotSymbols(rtAllFormulas,      skParameter);

      { --- user-defined functions --- }
      N := getNumUserFunctions;
      for I := 0 to N - 1 do
      begin
        FnName   := getNthUserFunctionName(I);
        Fn.Args  := getNthUserFunctionArguments(I);
        Fn.Body  := getNthUserFunctionBody(I);
        if (FnName <> '') and not FFunctions.ContainsKey(FnName) then
          FFunctions.Add(FnName, Fn);
      end;

      FOk := True;
    except
      on E: Exception do
      begin
        FLastError := 'Reading the model failed: ' + E.Message;
        FOk := FReactions.Count > 0;   { partial is better than nothing }
      end;
    end;
  finally
    Lines.Free;
    { Every pointer libantimony handed back has been copied into a Delphi
      string or array by the wrapper, so this is safe here -- and necessary,
      because the library leaks without it and a check asks it a great many
      questions. }
    try
      freeAll;
    except
      { A failure to free is not worth losing the snapshot over. }
    end;
  end;
end;

function TAntimonyModelSource.InRange(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FReactions.Count);
end;

function TAntimonyModelSource.ReactionCount: Integer;
begin
  Result := FReactions.Count;
end;

function TAntimonyModelSource.ReactionId(AIndex: Integer): string;
begin
  if InRange(AIndex) then Result := FReactions[AIndex].Id else Result := '';
end;

function TAntimonyModelSource.RateLawText(AIndex: Integer): string;
begin
  if InRange(AIndex) then Result := FReactions[AIndex].RateLaw else Result := '';
end;

function TAntimonyModelSource.Reactants(AIndex: Integer): TSpeciesRefs;
begin
  if InRange(AIndex) then Result := FReactions[AIndex].Reactants else Result := nil;
end;

function TAntimonyModelSource.Products(AIndex: Integer): TSpeciesRefs;
begin
  if InRange(AIndex) then Result := FReactions[AIndex].Products else Result := nil;
end;

function TAntimonyModelSource.Modifiers(AIndex: Integer): TModifierRefs;
begin
  if InRange(AIndex) then Result := FReactions[AIndex].Modifiers else Result := nil;
end;

function TAntimonyModelSource.SymbolKind(const AName: string): TSymbolKind;
var
  S: TSym;
begin
  if FSymbols.TryGetValue(AName, S) then Result := S.Kind
  else Result := skUnknown;
end;

function TAntimonyModelSource.HasValue(const AName: string): Boolean;
var
  S: TSym;
begin
  Result := FSymbols.TryGetValue(AName, S) and S.HasVal;
end;

function TAntimonyModelSource.ValueOf(const AName: string): Double;
var
  S: TSym;
begin
  if FSymbols.TryGetValue(AName, S) then Result := S.Value else Result := NaN;
end;

function TAntimonyModelSource.AssignmentRule(const AName: string): string;
var
  S: TSym;
begin
  if FSymbols.TryGetValue(AName, S) then Result := S.Assignment else Result := '';
end;

{ Antimony's const* symbol types are the source of truth: a species listed
  under rtConstSpecies is SBML's boundaryCondition, which is exactly the
  clamped-species case S017 must not report. }
function TAntimonyModelSource.IsConstant(const AName: string): Boolean;
var
  S: TSym;
begin
  Result := FSymbols.TryGetValue(AName, S) and S.Constant;
end;

function TAntimonyModelSource.UserFunction(const AName: string;
  out AArgs: TArray<string>; out ABody: string): Boolean;
var
  F: TFn;
begin
  AArgs := nil;
  ABody := '';
  Result := FFunctions.TryGetValue(AName, F);
  if Result then
  begin
    AArgs := F.Args;
    ABody := F.Body;
  end;
end;

function TAntimonyModelSource.KnowsSymbolKinds: Boolean;
begin
  { Only when the snapshot actually succeeded. A failed load leaves an empty
    symbol table, and claiming to know kinds then would make the engine report
    every identifier in every rate law as undefined. }
  Result := FOk and (FSymbols.Count > 0);
end;

function TAntimonyModelSource.AnnotatedLaw(AIndex: Integer): string;
begin
  if InRange(AIndex) then Result := FReactions[AIndex].Annotation
  else Result := '';
end;

function TAntimonyModelSource.SourceLineOf(AIndex: Integer): Integer;
begin
  if InRange(AIndex) then Result := FReactions[AIndex].Line else Result := -1;
end;

initialization
  GInv := TFormatSettings.Invariant;
  GInv.DecimalSeparator := '.';

end.
