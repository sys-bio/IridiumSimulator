unit uModelSession;

{ TModelSession owns the single TRoadRunner instance and tracks whether the
  model is loaded and whether the Antimony source has been edited since the
  last load.

  Two broadcast events, each supporting any number of subscribers via
  Add/Remove listener methods:

    State-changed listeners (TNotifyEvent)
      Fire on transitions of IsLoaded or IsDirty. Used for UI affordances
      like the caption, a "model loaded" status indicator, or a frame
      enabling / disabling compute buttons based on model state.

    Model-reloaded listeners (TModelReloadedEvent)
      Fire after EnsureLoaded successfully parses and loads the model.
      AParameterSetChanged is True when the sorted set of global parameter
      names differs from the previous load (or this is the first load).
      Used to decide whether sliders attached to the previous parameter set
      need to be cleared (structural change) or merely refreshed in place
      (compatible edit). Any frame that caches model-derived data and
      wants to invalidate on reload subscribes here.

  Listener semantics:
    * Add ignores duplicate registrations (the same method pointer can be
      added only once).
    * Remove does nothing if the handler isn't registered.
    * Dispatch iterates a snapshot, so a listener may safely add or remove
      listeners during firing without disturbing the current pass. }

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  uRoadRunner;

type
  TGetTextEvent = function: string of object;

  TModelReloadedEvent = procedure(Sender: TObject;
                                  AParameterSetChanged: Boolean) of object;

  TModelSession = class
  private
    FRoadRunner:         TRoadRunner;
    FIsLoaded:           Boolean;
    FIsDirty:            Boolean;
    FLastError:          string;
    FLastParameterSig:   string;

    FStateListeners:     TList<TNotifyEvent>;
    FReloadedListeners:  TList<TModelReloadedEvent>;
    FOnNeedAntimonyText: TGetTextEvent;

    procedure SetIsLoaded(AValue: Boolean);
    procedure SetIsDirty(AValue: Boolean);
    procedure DoStateChanged;
    procedure DoModelReloaded(AParameterSetChanged: Boolean);
    function  ComputeParameterSignature: string;

    function  FindStateListener(const AHandler: TNotifyEvent): Integer;
    function  FindReloadedListener(const AHandler: TModelReloadedEvent): Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure MarkDirty;
    procedure ClearDirty;
    function  EnsureLoaded: Boolean;
    procedure Unload;

    function  GetParameterNames:  TArray<string>;
    function  GetParameterValues: TArray<Double>;
    procedure SetParameterValue(const AName: string; AValue: Double);

    function  GetTunableNames:  TArray<string>;   { globals + boundary species }
    function  GetTunableValues: TArray<Double>;

    procedure AddStateListener(const AHandler: TNotifyEvent);
    procedure RemoveStateListener(const AHandler: TNotifyEvent);

    procedure AddReloadedListener(const AHandler: TModelReloadedEvent);
    procedure RemoveReloadedListener(const AHandler: TModelReloadedEvent);

    function GetCurrentAntimonyText: string;

    property RoadRunner: TRoadRunner read FRoadRunner;
    property IsLoaded:   Boolean     read FIsLoaded;
    property IsDirty:    Boolean     read FIsDirty;
    property LastError:  string      read FLastError;

    property OnNeedAntimonyText: TGetTextEvent
      read FOnNeedAntimonyText write FOnNeedAntimonyText;
  end;

implementation

uses
  uAntimonyAPI, uCommonTypes, IOUtils;

{ ── method-pointer equality ──────────────────────────────────────────────── }

function MethodsEqual(const A, B: TMethod): Boolean; inline;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;

{ ── construction ─────────────────────────────────────────────────────────── }

constructor TModelSession.Create;
begin
  inherited Create;
  FRoadRunner        := TRoadRunner.Create;
  FIsLoaded          := False;
  FIsDirty           := False;
  FLastParameterSig  := '';
  FStateListeners    := TList<TNotifyEvent>.Create;
  FReloadedListeners := TList<TModelReloadedEvent>.Create;
end;

destructor TModelSession.Destroy;
begin
  FStateListeners.Free;
  FReloadedListeners.Free;
  FRoadRunner.Free;
  inherited;
end;

{ ── listener registration ────────────────────────────────────────────────── }

function TModelSession.FindStateListener(const AHandler: TNotifyEvent): Integer;
var
  I: Integer;
begin
  for I := 0 to FStateListeners.Count - 1 do
    if MethodsEqual(TMethod(FStateListeners[I]), TMethod(AHandler)) then
      Exit(I);
  Result := -1;
end;

function TModelSession.FindReloadedListener(
  const AHandler: TModelReloadedEvent): Integer;
var
  I: Integer;
begin
  for I := 0 to FReloadedListeners.Count - 1 do
    if MethodsEqual(TMethod(FReloadedListeners[I]), TMethod(AHandler)) then
      Exit(I);
  Result := -1;
end;

procedure TModelSession.AddStateListener(const AHandler: TNotifyEvent);
begin
  if not Assigned(AHandler) then Exit;
  if FindStateListener(AHandler) < 0 then
    FStateListeners.Add(AHandler);
end;

procedure TModelSession.RemoveStateListener(const AHandler: TNotifyEvent);
var
  Idx: Integer;
begin
  Idx := FindStateListener(AHandler);
  if Idx >= 0 then
    FStateListeners.Delete(Idx);
end;

procedure TModelSession.AddReloadedListener(
  const AHandler: TModelReloadedEvent);
begin
  if not Assigned(AHandler) then Exit;
  if FindReloadedListener(AHandler) < 0 then
    FReloadedListeners.Add(AHandler);
end;

procedure TModelSession.RemoveReloadedListener(
  const AHandler: TModelReloadedEvent);
var
  Idx: Integer;
begin
  Idx := FindReloadedListener(AHandler);
  if Idx >= 0 then
    FReloadedListeners.Delete(Idx);
end;

{ ── dispatch (snapshot-based, safe under concurrent add/remove) ──────────── }

procedure TModelSession.DoStateChanged;
var
  Snapshot: TArray<TNotifyEvent>;
  I:        Integer;
begin
  Snapshot := FStateListeners.ToArray;
  for I := 0 to High(Snapshot) do
    Snapshot[I](Self);
end;

procedure TModelSession.DoModelReloaded(AParameterSetChanged: Boolean);
var
  Snapshot: TArray<TModelReloadedEvent>;
  I:        Integer;
begin
  Snapshot := FReloadedListeners.ToArray;
  for I := 0 to High(Snapshot) do
    Snapshot[I](Self, AParameterSetChanged);
end;

{ ── state plumbing ───────────────────────────────────────────────────────── }

procedure TModelSession.SetIsLoaded(AValue: Boolean);
begin
  if FIsLoaded = AValue then Exit;
  FIsLoaded := AValue;
  DoStateChanged;
end;

procedure TModelSession.SetIsDirty(AValue: Boolean);
begin
  if FIsDirty = AValue then Exit;
  FIsDirty := AValue;
  DoStateChanged;
end;

procedure TModelSession.MarkDirty;
begin
  SetIsDirty(True);
end;

procedure TModelSession.ClearDirty;
begin
  SetIsDirty(False);
end;

{ ── parameter-set signature ──────────────────────────────────────────────── }

function TModelSession.ComputeParameterSignature: string;
var
  Names: TArray<string>;
  Lst:   TStringList;
  I:     Integer;
begin
  if not FIsLoaded then Exit('');

  Names := GetTunableNames;
  Lst := TStringList.Create;
  try
    Lst.Sorted := True;
    for I := 0 to High(Names) do
      Lst.Add(Names[I]);
    Result := Lst.CommaText;
  finally
    Lst.Free;
  end;
end;

{ ── load / unload ────────────────────────────────────────────────────────── }

function TModelSession.EnsureLoaded: Boolean;
var
  AntText:         string;
  SbmlInfo:        TModelErrorState;
  NewSig:          string;
  ParamSetChanged: Boolean;
  FirstLoad:       Boolean;
begin
  if FIsLoaded and (not FIsDirty) then Exit(True);

  if not Assigned(FOnNeedAntimonyText) then
  begin
    FLastError := 'No text source wired to session.';
    Exit(False);
  end;

  AntText := FOnNeedAntimonyText;
  if Trim(AntText) = '' then
  begin
    FLastError := 'Antimony source is empty.';
    Exit(False);
  end;



  try
    SbmlInfo := getSBMLFromAntimony(AnsiString(AntText));
    if not SbmlInfo.ok then
      raise Exception.Create('Antimony parse failed: ' + SbmlInfo.errMsg);

    if not FRoadRunner.loadSBMLFromString(SbmlInfo.sbmlStr) then
       raise Exception.Create('RoadRunner: ' + FRoadRunner.getLastError);

    FLastError := '';
    FIsDirty   := False;
    FIsLoaded  := True;
    DoStateChanged;

    { Compare parameter-set signatures to classify the reload. The first
      successful load is treated as "parameter set changed" since there
      were no prior sliders to preserve anyway. }
    NewSig          := ComputeParameterSignature;
    FirstLoad       := (FLastParameterSig = '');
    ParamSetChanged := FirstLoad or (NewSig <> FLastParameterSig);
    FLastParameterSig := NewSig;

    DoModelReloaded(ParamSetChanged);

    Result := True;
  except
    on E: Exception do
    begin
      FLastError := E.Message;
      FLastParameterSig := '';
      SetIsLoaded(False);
      raise;
    end;
  end;
end;

procedure TModelSession.Unload;
begin
  FLastParameterSig := '';
  SetIsLoaded(False);
end;

{ ── parameter helpers ────────────────────────────────────────────────────── }

function TModelSession.GetParameterNames: TArray<string>;
var
  Ids: TStringList;
  I:   Integer;
begin
  if not FIsLoaded then Exit(nil);

  Ids := FRoadRunner.getGlobalParameterIds;
  try
    SetLength(Result, Ids.Count);
    for I := 0 to Ids.Count - 1 do
      Result[I] := Ids[I];
  finally
    Ids.Free;
  end;
end;

function TModelSession.GetParameterValues: TArray<Double>;
var
  N, I: Integer;
begin
  if not FIsLoaded then Exit(nil);

  N := FRoadRunner.GetNumberOfGlobalParameters;
  SetLength(Result, N);
  for I := 0 to N - 1 do
    Result[I] := FRoadRunner.getGlobalParameterByIndex(I);
end;

procedure TModelSession.SetParameterValue(const AName: string; AValue: Double);
begin
  if FIsLoaded then
    FRoadRunner.SetValue(AName, AValue);
end;

function TModelSession.GetCurrentAntimonyText: string;
begin
  if Assigned(FOnNeedAntimonyText) then
    Result := FOnNeedAntimonyText
  else
    Result := '';
end;

function TModelSession.GetTunableNames: TArray<string>;
var
  Ids: TStringList;
  P:   TArray<string>;
  I, N: Integer;
begin
  if not FIsLoaded then Exit(nil);

  P := GetParameterNames;
  Ids := FRoadRunner.getBoundarySpeciesIds;
  try
    SetLength(Result, Length(P) + Ids.Count);
    for I := 0 to High(P) do
      Result[I] := P[I];
    N := Length(P);
    for I := 0 to Ids.Count - 1 do
      Result[N + I] := Ids[I];
  finally
    Ids.Free;
  end;
end;

function TModelSession.GetTunableValues: TArray<Double>;
var
  V:    TArray<Double>;
  I, N: Integer;
  NB:   Integer;
begin
  if not FIsLoaded then Exit(nil);

  V  := GetParameterValues;
  NB := FRoadRunner.GetNumberOfBoundarySpecies;
  SetLength(Result, Length(V) + NB);
  for I := 0 to High(V) do
    Result[I] := V[I];
  N := Length(V);
  for I := 0 to NB - 1 do
    Result[N + I] := FRoadRunner.getBoundarySpeciesByIndex(I);
end;

end.
