unit uMetaSetValues;

{ Applying a metadata block's 'set:' assignments to the engine, and undoing
  them afterwards.

  A task command may carry a 'set:' section — k1: 0.1, S1: 0.3 and so on
  (spec 7.2) — meaning "run this experiment with these values". Note for
  editors: no braces in this comment, since a brace comment ends at the
  first one. Every panel that computes needs
  the same three things, which is why they live here rather than in one
  frame: resolve the name the file used to the id RoadRunner uses, write it
  in the form that quantity actually has, and put back what was there.

  **A preset must not leak.** The values are applied for the run and undone
  after it, so selecting one experiment, computing, and then going back to
  '— (my own settings)' leaves the engine exactly as it was. Without the
  restore, a 'set:' would silently persist into every later run, including
  the user's own — a scan whose baseline had quietly moved would be very
  hard to explain.

  A species is written through BOTH its initial-value and its current-value
  selector. init() alone is not enough when the panel is not resetting
  before the run (the current concentration would still be the old one),
  and the current value alone does not survive a reset. Everything else —
  a global parameter, a compartment size — has only the plain form. }

interface

uses
  System.SysUtils, System.Classes,
  uRoadRunner, Sim.Meta.Model;

type
  { One engine selector and the value it held before a 'set:' overwrote it. }
  TSetValueRestore = record
    Selector: string;
    Value:    Double;
  end;
  TSetValueRestoreArray = TArray<TSetValueRestore>;

{ The id RoadRunner uses for the name the metadata block spelled, or '' if
  this model has no such quantity. Accepts either spelling: the file says
  'S1', RoadRunner may call it '[S1]'. }
function ResolveEngineId(ARR: TRoadRunner; const AName: string): string;

{ Apply ACmd's SetValues to ARR, returning what RestoreSetValues needs to
  undo them. Names that cannot be resolved or written are described in
  AUnmet (may be nil) — never silently skipped: a run at the wrong
  parameter value is a different experiment, not a near miss. }
function ApplySetValues(ARR: TRoadRunner; ACmd: TTaskCommand;
                        AUnmet: TStrings): TSetValueRestoreArray;

{ Put back everything ApplySetValues overwrote. }
procedure RestoreSetValues(ARR: TRoadRunner;
                           const ASaved: TSetValueRestoreArray);

{ True when ACmd's 'set:' section names AName, in either spelling. A slider
  moving such a quantity is the user overriding the file, and the panel has
  to stop re-applying it — otherwise every run puts the file's value back
  and the slider appears to do nothing. }
function SetValuesName(ACmd: TTaskCommand; const AName: string): Boolean;

implementation

function FindIn(AList: TStringList; const AName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AList.Count - 1 do
    if (AList[I] = AName) or (AList[I] = '[' + AName + ']') then
      Exit(AList[I]);
end;

function ResolveEngineId(ARR: TRoadRunner; const AName: string): string;
var
  Ids: TStringList;
begin
  Result := '';
  if (ARR = nil) or (AName = '') then Exit;

  Ids := ARR.getFloatingSpeciesIds;
  try Result := FindIn(Ids, AName); finally Ids.Free; end;
  if Result <> '' then Exit;

  Ids := ARR.getBoundarySpeciesIds;
  try Result := FindIn(Ids, AName); finally Ids.Free; end;
  if Result <> '' then Exit;

  Ids := ARR.getGlobalParameterIds;
  try Result := FindIn(Ids, AName); finally Ids.Free; end;
  if Result <> '' then Exit;

  Ids := ARR.getCompartmentIds;
  try Result := FindIn(Ids, AName); finally Ids.Free; end;
end;

{ True when AId names a species, which is what decides whether there is an
  init() form to write as well. }
function IsSpecies(ARR: TRoadRunner; const AId: string): Boolean;
var
  Ids: TStringList;
begin
  Ids := ARR.getFloatingSpeciesIds;
  try Result := FindIn(Ids, AId) <> ''; finally Ids.Free; end;
  if Result then Exit;

  Ids := ARR.getBoundarySpeciesIds;
  try Result := FindIn(Ids, AId) <> ''; finally Ids.Free; end;
end;

function ApplySetValues(ARR: TRoadRunner; ACmd: TTaskCommand;
  AUnmet: TStrings): TSetValueRestoreArray;
var
  IV:        TSetValue;
  Id:        string;
  Selectors: TArray<string>;
  Sel:       string;
  Saved:     TSetValueRestore;
  Wrote:     Boolean;
begin
  Result := nil;
  if (ARR = nil) or (ACmd = nil) then Exit;

  for IV in ACmd.SetValues do
  begin
    Id := ResolveEngineId(ARR, IV.Name);
    if Id = '' then
    begin
      if AUnmet <> nil then
        AUnmet.Add('set ' + IV.Name + ': no such quantity in the model');
      Continue;
    end;

    if IsSpecies(ARR, Id) then
      Selectors := ['init(' + Id + ')', Id]
    else
      Selectors := [Id];

    Wrote := False;
    for Sel in Selectors do
    begin
      { Read before writing — this is the only moment the old value still
        exists. getValue cannot report failure, so a selector the engine
        does not know reads as 0; that costs nothing, because the write
        below is what decides whether it is remembered at all. }
      Saved.Selector := Sel;
      Saved.Value    := ARR.getValue(AnsiString(Sel));

      if ARR.setValue(AnsiString(Sel), IV.Value) then
      begin
        Result := Result + [Saved];
        Wrote  := True;
      end;
    end;

    if (not Wrote) and (AUnmet <> nil) then
      AUnmet.Add('set ' + IV.Name + ': could not be set');
  end;
end;

function SetValuesName(ACmd: TTaskCommand; const AName: string): Boolean;
var
  IV:    TSetValue;
  Bare:  string;
begin
  Result := False;
  if (ACmd = nil) or (AName = '') then Exit;

  { Sliders carry the model's spelling ('[S1]'); the file carries the bare
    name. Compare both ways rather than depending on which side is which. }
  Bare := AName;
  if (Length(Bare) > 2) and (Bare[1] = '[') and (Bare[Length(Bare)] = ']') then
    Bare := Copy(Bare, 2, Length(Bare) - 2);

  for IV in ACmd.SetValues do
    if SameText(IV.Name, AName) or SameText(IV.Name, Bare) then
      Exit(True);
end;

procedure RestoreSetValues(ARR: TRoadRunner;
  const ASaved: TSetValueRestoreArray);
var
  I: Integer;
begin
  if ARR = nil then Exit;
  { Backwards, so a species' current value is put back after its initial
    value, matching the order they were written in. }
  for I := High(ASaved) downto 0 do
    ARR.setValue(AnsiString(ASaved[I].Selector), ASaved[I].Value);
end;

end.
