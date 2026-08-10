unit uMetaSymbolProvider;

{ ISymbolProvider over the loaded RoadRunner model.

  This is the decoupling point between the simulation-metadata library and
  Iridium: Sim.Meta must not link to libRoadRunner, so the validator asks
  this interface whether a name exists in the model. Without it the
  validator cannot check that y:, observables:, columns: and parameter:
  name real quantities, and it cannot implement the shadowing rule
  (spec 3.5) — a model with a species called 'time' or 'cross'.

  Report what the LOADED MODEL says, not what the Antimony text looks
  like. A species that appears in no reaction comes back from libAntimony
  as a parameter, and an assignment-rule target (total := S1 + S2) is a
  global parameter too. Those are the facts the simulation will run
  against, so they are the facts the validator should check against.

  A snapshot, not a live view: the dictionary is built once at
  construction. Create one per parse and let it die — it is refcounted,
  and holding one across a model reload would answer with stale names. }

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Sim.Meta.Types,
  uModelSession;

{ libRoadRunner reports a floating species as '[A]' — its notation for a
  concentration — while SBML, Antimony and therefore a metadata block all
  call it 'A'. The two must be translated at every boundary between them,
  or names that plainly correspond silently fail to match: a @plot asking
  for y: [A, B, C] finds nothing, and the validator reports three
  perfectly good species as undeclared.

  Iridium's internals use the RoadRunner form throughout (it is what
  selection lists and result column headers are keyed on), so translate
  at the edge and leave the interior alone. }
function CanonicalModelName(const AId: string): string;

type
  TRoadRunnerSymbolProvider = class(TInterfacedObject, ISymbolProvider)
  private
    FKinds:    TDictionary<string, TSymbolKind>;
    FOrder:    TStringList;          { every symbol, in category order }
    FFloating: TStringList;
    procedure AddIds(AIds: TStringList; AKind: TSymbolKind);
  public
    { ASession may be nil or unloaded: the result then answers 'nothing
      exists', which is not the same as passing nil to the parser. Pass nil
      to the parser to disable symbol checking altogether — that is what an
      editor wants for a syntax-only check before a model has been
      loaded. }
    constructor Create(ASession: TModelSession);
    destructor  Destroy; override;

    { ISymbolProvider }
    function KindOf(const AName: string): TSymbolKind;
    function FloatingSpecies: TArray<string>;
    function AllSymbols: TArray<string>;
  end;

implementation

uses
  uRoadRunner;

function CanonicalModelName(const AId: string): string;
begin
  Result := AId;
  if (Length(Result) > 2) and (Result[1] = '[') and
     (Result[Length(Result)] = ']') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

constructor TRoadRunnerSymbolProvider.Create(ASession: TModelSession);
var
  RR: TRoadRunner;
begin
  inherited Create;

  { CaseSensitive throughout: SBML identifiers are case-sensitive, and the
    library reports a case-only mismatch as its own diagnostic. Folding
    case here would silently accept 's1' for 'S1'. }
  FKinds    := TDictionary<string, TSymbolKind>.Create;
  FOrder    := TStringList.Create;
  FFloating := TStringList.Create;
  FOrder.CaseSensitive    := True;
  FFloating.CaseSensitive := True;

  if (ASession = nil) or (not ASession.IsLoaded) then Exit;

  RR := ASession.RoadRunner;

  { Order matters where a name appears in more than one category: the
    first kind added wins (AddIds does not overwrite). Species first, so a
    quantity a user thinks of as a species is reported as one. }
  AddIds(RR.getFloatingSpeciesIds, skFloatingSpecies);
  AddIds(RR.getBoundarySpeciesIds, skBoundarySpecies);
  AddIds(RR.getGlobalParameterIds, skParameter);
  AddIds(RR.getCompartmentIds,     skCompartment);
  AddIds(RR.getReactionIds,        skReaction);
end;

destructor TRoadRunnerSymbolProvider.Destroy;
begin
  FFloating.Free;
  FOrder.Free;
  FKinds.Free;
  inherited;
end;

{ Takes ownership of AIds — every RoadRunner getXxxIds returns a list the
  caller must free. }
procedure TRoadRunnerSymbolProvider.AddIds(AIds: TStringList;
  AKind: TSymbolKind);
var
  I:    Integer;
  Name: string;
begin
  if AIds = nil then Exit;
  try
    for I := 0 to AIds.Count - 1 do
    begin
      { Report the name as the metadata block spells it — 'A', not the
        '[A]' concentration form RoadRunner uses. Everything this
        interface answers is compared against text the user wrote in the
        model file. }
      Name := CanonicalModelName(AIds[I]);
      if Name = '' then Continue;
      if FKinds.ContainsKey(Name) then Continue;

      FKinds.Add(Name, AKind);
      FOrder.Add(Name);
      if AKind = skFloatingSpecies then
        FFloating.Add(Name);
    end;
  finally
    AIds.Free;
  end;
end;

function TRoadRunnerSymbolProvider.KindOf(const AName: string): TSymbolKind;
begin
  if not FKinds.TryGetValue(AName, Result) then
    Result := skUnknown;
end;

function TRoadRunnerSymbolProvider.FloatingSpecies: TArray<string>;
begin
  Result := FFloating.ToStringArray;
end;

function TRoadRunnerSymbolProvider.AllSymbols: TArray<string>;
begin
  Result := FOrder.ToStringArray;
end;

end.
