unit uMetaOutput;

{ Rendering an @output command's data file.

  Separate from the frames because every analysis panel that can produce
  rows will eventually need it, and because the format of the file is the
  specification's business rather than any one panel's.

  Writing the file is NOT done here: the caller decides where the text
  goes, so that an @output with no 'file' key can be sent to the text
  panel instead (spec 10 — omit the file name and the result goes to the
  tool's output panel, which Iridium has). Keeping the decision out of
  this unit also keeps the overwrite prompt with the UI. }

interface

uses
  System.SysUtils, System.Classes,
  uRR2DSimpleMatrix,
  Sim.Meta.Model;

type
  { A column that will be written: its heading as the file should show it,
    and the index into the result matrix it comes from. }
  TOutputColumn = record
    Header: string;
    Index:  Integer;
  end;

{ The complete text of the file ACmd describes, given the matrix its
  source task produced.

  AProvenance is TSimulationMetadata.ProvenanceLines for this command,
  already '#'-prefixed. It is passed in rather than generated here so that
  the file Iridium writes is byte-identical to the one the exported Python
  script writes — that identity is the cheapest possible check that the
  two paths agree, which is why the library generates those lines rather
  than each caller formatting its own.

  Returns False with AError set when the command names columns the result
  does not contain. }
function BuildOutputText(ACmd: TOutputCommand;
                         const AData: T2DMatrix;
                         const AProvenance: TArray<string>;
                         out AText: string;
                         out AError: string): Boolean;

implementation

uses
  System.Math;

{ Result columns carry RoadRunner's names, so a floating species appears
  as '[A]' while the model file calls it 'A'. Accept either spelling. }
function FindColumn(const AData: T2DMatrix; const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to AData.c - 1 do
    if SameText(AData.columnHeader[I], AName) or
       SameText(AData.columnHeader[I], '[' + AName + ']') then
      Exit(I);
  Result := -1;
end;

function ResolveColumns(ACmd: TOutputCommand; const AData: T2DMatrix;
  out ACols: TArray<TOutputColumn>; out AError: string): Boolean;
var
  Name: string;
  Idx, N: Integer;
begin
  Result := True;
  AError := '';
  ACols  := [];

  if Length(ACmd.Columns) = 0 then
  begin
    { No 'columns' key: write what was computed, in the order the result
      already has. The specification's default is time plus the floating
      species, which is what the selection list produced anyway — and
      writing the actual result is more honest than writing a default
      that might not match it. }
    SetLength(ACols, AData.c);
    for N := 0 to AData.c - 1 do
    begin
      ACols[N].Header := AData.columnHeader[N];
      ACols[N].Index  := N;
    end;
    Exit;
  end;

  for Name in ACmd.Columns do
  begin
    Idx := FindColumn(AData, Name);
    if Idx < 0 then
    begin
      { Naming a column the run did not produce is worth refusing over:
        writing the file without it would quietly produce data that does
        not match what the file asked for. }
      AError := Format('the result has no column named ''%s''', [Name]);
      Exit(False);
    end;
    N := Length(ACols);
    SetLength(ACols, N + 1);
    { Header as the user wrote it: they asked for 'A', not '[A]'. }
    ACols[N].Header := Name;
    ACols[N].Index  := Idx;
  end;
end;

function BuildOutputText(ACmd: TOutputCommand; const AData: T2DMatrix;
  const AProvenance: TArray<string>; out AText: string;
  out AError: string): Boolean;
var
  Cols: TArray<TOutputColumn>;
  SB:   TStringBuilder;
  Line: string;
  Fmt:  TFormatSettings;
  Delim: string;
  I, J: Integer;
begin
  AText  := '';
  AError := '';

  if AData = nil then
  begin
    AError := 'there is no result to write';
    Exit(False);
  end;

  if not ResolveColumns(ACmd, AData, Cols, AError) then
    Exit(False);

  Fmt   := TFormatSettings.Invariant;
  Delim := ACmd.Delimiter;

  SB := TStringBuilder.Create;
  try
    { Comments carry the provenance block and the user's notes. The
      default is False for csv and tsv because Excel does not skip
      comment lines and shows them as data rows; the validator has
      already resolved that from the format, so it is simply obeyed
      here. }
    if ACmd.Comments then
      for Line in AProvenance do
        SB.AppendLine(Line);

    if ACmd.Header then
    begin
      Line := '';
      for I := 0 to High(Cols) do
      begin
        if I > 0 then Line := Line + Delim;
        Line := Line + Cols[I].Header;
      end;
      SB.AppendLine(Line);
    end;

    for I := 0 to AData.r - 1 do
    begin
      Line := '';
      for J := 0 to High(Cols) do
      begin
        if J > 0 then Line := Line + Delim;
        { Precision is in SIGNIFICANT FIGURES, not decimal places, because
          concentrations routinely span several orders of magnitude —
          which is what %g takes. }
        Line := Line + Format('%.*g',
                  [Max(1, ACmd.Precision), AData[I, Cols[J].Index]], Fmt);
      end;
      SB.AppendLine(Line);
    end;

    AText := SB.ToString;
    Result := True;
  finally
    SB.Free;
  end;
end;

end.
