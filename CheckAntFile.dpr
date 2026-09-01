program CheckAntFile;

{ Runs the rate law checker over .ant files from the command line.

  Not part of Iridium -- it exists so the adapter can be measured against real
  models without driving the GUI, which is the only way to find out the number
  that actually decides whether this feature is usable: how often it flags a
  model that is perfectly correct.

  Usage:  CheckAntFile <file.ant> [more.ant ...]
          CheckAntFile *.ant
          CheckAntFile -csv <prefix> <file.xml> ...

  .xml input is SBML and is converted with sbmlToAntimony first, which is what
  lets the BioModels corpus (M17) be run without hand-converting a thousand
  files. -csv writes machine-readable rows instead of prose, appending, so a
  corpus run can be driven one file per process and a model that takes the
  library down with it costs one row rather than the whole run. }

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  uAntimonyAPI in '..\libAntimony_Delphi_Bindings\uAntimonyAPI.pas',
  uAntimonyRaw in '..\libAntimony_Delphi_Bindings\uAntimonyRaw.pas',
  uAntimonyTypes in '..\libAntimony_Delphi_Bindings\uAntimonyTypes.pas',
  RateLaw.Types in '..\ModelCheckerLib\RateLaw.Types.pas',
  RateLaw.Ast in '..\ModelCheckerLib\RateLaw.Ast.pas',
  RateLaw.Parser in '..\ModelCheckerLib\RateLaw.Parser.pas',
  RateLaw.Canonical in '..\ModelCheckerLib\RateLaw.Canonical.pas',
  RateLaw.BuiltInLaws in '..\ModelCheckerLib\RateLaw.BuiltInLaws.pas',
  RateLaw.Diff in '..\ModelCheckerLib\RateLaw.Diff.pas',
  RateLaw.Registry in '..\ModelCheckerLib\RateLaw.Registry.pas',
  RateLaw.Generative in '..\ModelCheckerLib\RateLaw.Generative.pas',
  RateLaw.Bind in '..\ModelCheckerLib\RateLaw.Bind.pas',
  RateLaw.Associate in '..\ModelCheckerLib\RateLaw.Associate.pas',
  RateLaw.Eval in '..\ModelCheckerLib\RateLaw.Eval.pas',
  RateLaw.Dynamic in '..\ModelCheckerLib\RateLaw.Dynamic.pas',
  RateLaw.Static in '..\ModelCheckerLib\RateLaw.Static.pas',
  RateLaw.Report in '..\ModelCheckerLib\RateLaw.Report.pas',
  uRateLawModelSource in 'uRateLawModelSource.pas';

var
  GErrors, GWarnings, GReactions, GAssociated, GFiles: Integer;
  GCsvPrefix: string;
  GQuiet: Boolean;

{ ---------------------------------------------------------------------------
  CSV output for the corpus run (M17)

  Written as three flat tables rather than one wide row, because the question
  the corpus is asked -- "which defect codes fire on models that are known to
  be correct, and why" -- is a group-by over diagnostics, not over files.
  Appended and flushed per file so an interrupted or crashed run keeps
  everything it had already established.
  --------------------------------------------------------------------------- }

function CsvQuote(const S: string): string;
begin
  Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"';
  Result := StringReplace(Result, #13, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, ' ', [rfReplaceAll]);
end;

procedure CsvAppend(const ASuffix, AHeader, ALine: string);
var
  Path: string;
  F: TextFile;
begin
  if GCsvPrefix = '' then Exit;
  Path := GCsvPrefix + ASuffix;
  AssignFile(F, Path);
  try
    if TFile.Exists(Path) then
      Append(F)
    else
    begin
      Rewrite(F);
      Writeln(F, AHeader);
    end;
    Writeln(F, ALine);
  finally
    CloseFile(F);
  end;
end;

procedure CsvFile(const AName: string; AOk: Boolean; const AError: string;
                  AReactions, AAssociated, AErrors, AWarnings: Integer;
                  AMillis: Int64);
begin
  CsvAppend('_files.csv',
    'file,ok,error,reactions,associated,errors,warnings,ms',
    Format('%s,%d,%s,%d,%d,%d,%d,%d',
      [CsvQuote(AName), Ord(AOk), CsvQuote(AError),
       AReactions, AAssociated, AErrors, AWarnings, AMillis]));
end;

procedure CsvDiag(const AName: string; const D: TRateLawDiagnostic);
begin
  CsvAppend('_diags.csv',
    'file,code,severity,law,reaction,line,message,found,expected',
    Format('%s,%s,%s,%s,%s,%d,%s,%s,%s',
      [CsvQuote(AName), CsvQuote(D.Code), CsvQuote(SeverityName(D.Severity)),
       CsvQuote(D.LawId), CsvQuote(D.ReactionId), D.SourceLine,
       CsvQuote(D.Message), CsvQuote(D.Found), CsvQuote(D.Expected)]));
end;

procedure CsvAssoc(const AName: string; const A: TAssociation;
                   const ARateLaw: string);
begin
  CsvAppend('_assoc.csv', 'file,reaction,law,detail,ratelaw',
    Format('%s,%s,%s,%s,%s',
      [CsvQuote(AName), CsvQuote(A.ReactionId), CsvQuote(A.LawId),
       CsvQuote(A.Detail), CsvQuote(ARateLaw)]));
end;

{ Reads a model file as Antimony. SBML (.xml) is converted first -- the whole
  BioModels corpus is SBML, and hand-converting it would put a manual step
  between the checker and the only number that says whether it is usable.
  Returns False with a reason rather than raising: a model libantimony cannot
  read is data about the corpus, not an error in the run. }
function ReadModelText(const APath: string; out AText, AError: string): Boolean;
var
  Res: TModelErrorState;
begin
  AText := '';
  AError := '';
  if SameText(ExtractFileExt(APath), '.xml') or
     SameText(ExtractFileExt(APath), '.sbml') then
  begin
    Res := sbmlToAntimony(TFile.ReadAllText(APath, TEncoding.UTF8));
    if not Res.ok then
    begin
      AError := 'SBML will not convert: ' + Res.errMsg;
      Exit(False);
    end;
    AText := Res.sbmlStr;
  end
  else
    AText := TFile.ReadAllText(APath);
  Result := True;
end;

procedure CheckOne(const APath: string; ARegistry: TRateLawRegistry;
                   AVerbose, AReport, AMarkdown, ADynamic: Boolean);
var
  Source: TAntimonyModelSource;
  Src: IModelSource;
  Res: TCheckResult;
  D: TRateLawDiagnostic;
  A: TAssociation;
  I, Assoc: Integer;
  Name, Text, Error: string;
  Started: TDateTime;
  Millis: Int64;
begin
  { Not ExtractFileName: a list file written by a POSIX shell carries
    forward slashes, and ExtractFileName splits a Windows path on '\'
    and ':' only -- so "C:/models/x.xml" came back as "/models/x.xml"
    and every CSV row carried most of its own path. }
  Name := ExtractFileName(StringReplace(APath, '/', PathDelim, [rfReplaceAll]));
  Started := Now;
  if not GQuiet then
  begin
    Writeln;
    Writeln('=== ', Name, ' ===');
  end;

  if not ReadModelText(APath, Text, Error) then
  begin
    if not GQuiet then Writeln('  ', Error);
    CsvFile(Name, False, Error, 0, 0, 0, 0, MilliSecondsBetween(Now, Started));
    Exit;
  end;

  Source := TAntimonyModelSource.Create(Text);
  Src := Source;
  if not Source.Ok then
  begin
    if not GQuiet then Writeln('  will not load: ', Source.LastError);
    CsvFile(Name, False, Source.LastError, 0, 0, 0, 0,
      MilliSecondsBetween(Now, Started));
    Exit;
  end;

  Inc(GFiles);
  Res := CheckModel(ARegistry, Src, ADynamic);
  try
    Assoc := 0;
    for A in Res.Associations do
      if A.LawId <> '' then Inc(Assoc);

    Millis := MilliSecondsBetween(Now, Started);
    if GCsvPrefix <> '' then
    begin
      CsvFile(Name, True, '', Src.ReactionCount, Assoc,
        Res.ErrorCount, Res.WarningCount, Millis);
      for I := 0 to Res.Diagnostics.Count - 1 do
        if Res.Diagnostics[I].Severity <> sevInfo then
          CsvDiag(Name, Res.Diagnostics[I]);
      for I := 0 to Res.Associations.Count - 1 do
        if Res.Associations[I].LawId <> '' then
        begin
          A := Res.Associations[I];
          { The rate law text goes with the association so a suspicious
            finding can be read back without re-running anything. }
          Text := '';
          for var J := 0 to Src.ReactionCount - 1 do
            if Src.ReactionId(J) = A.ReactionId then
            begin
              Text := Src.RateLawText(J);
              Break;
            end;
          CsvAssoc(Name, A, Text);
        end;
    end;

    Inc(GReactions, Src.ReactionCount);
    Inc(GAssociated, Assoc);
    Inc(GErrors, Res.ErrorCount);
    Inc(GWarnings, Res.WarningCount);

    if AReport then
    begin
      var Opts := TReportOptions.Default;
      Opts.DynamicWasRun := ADynamic;
      if AMarkdown then
        Writeln(RateLaw.Report.AsMarkdown(Res, ARegistry, Src.ReactionCount, Opts))
      else
        Writeln(RateLaw.Report.AsText(Res, ARegistry, Src.ReactionCount, Opts));
      Exit;
    end;

    if GQuiet then
    begin
      Writeln(Format('%-28s %4d rx, %4d assoc, %3d err, %3d warn',
        [Name, Src.ReactionCount, Assoc, Res.ErrorCount, Res.WarningCount]));
      Exit;
    end;

    Writeln(Format('  %d reaction(s), %d associated, %d error(s), %d warning(s)',
      [Src.ReactionCount, Assoc, Res.ErrorCount, Res.WarningCount]));

    if AVerbose then
      for I := 0 to Src.ReactionCount - 1 do
        Writeln(Format('    %-12s %s', [Src.ReactionId(I), Src.RateLawText(I)]));

    for A in Res.Associations do
      if A.LawId <> '' then
        Writeln(Format('    %-12s -> %s  (%s)',
          [A.ReactionId, A.LawId, A.Detail]));

    for I := 0 to Res.Diagnostics.Count - 1 do
    begin
      D := Res.Diagnostics[I];
      if D.Severity = sevInfo then Continue;
      Writeln('    ', D.ToString);
    end;
  finally
    Res.Free;
  end;
end;

var
  Registry: TRateLawRegistry;
  Err, Pattern, Dir: string;
  Files: TArray<string>;
  F: string;
  I: Integer;
  Verbose, Report, Markdown, Dynamic, SkipNext: Boolean;
  LawDir: string;
begin
  try
    if not loadAntimonyLibrary(Err) then
    begin
      Writeln('Cannot load libantimony: ', Err);
      ExitCode := 1;
      Exit;
    end;

    Verbose := False;
    Report := False;
    Markdown := False;
    Dynamic := False;
    GQuiet := False;
    GCsvPrefix := '';
    SkipNext := False;
    LawDir := '';
    Files := nil;
    for I := 1 to ParamCount do
    begin
      if SameText(ParamStr(I), '-v') then
      begin
        Verbose := True;
        Continue;
      end;
      if SameText(ParamStr(I), '-quiet') then
      begin
        GQuiet := True;
        Continue;
      end;
      if SameText(ParamStr(I), '-csv') and (I < ParamCount) then
      begin
        { A prefix, not a filename: three tables are written. Implies -quiet,
          since a corpus run's console output is not the product. }
        GCsvPrefix := ParamStr(I + 1);
        GQuiet := True;
        SkipNext := True;
        Continue;
      end;
      if SameText(ParamStr(I), '-list') and (I < ParamCount) then
      begin
        { One path per line. Command lines do not stretch to a thousand
          models, and a list file is also what makes a run resumable. }
        for F in TFile.ReadAllLines(ParamStr(I + 1)) do
          if Trim(F) <> '' then
            Files := Files + [Trim(F)];
        SkipNext := True;
        Continue;
      end;
      if SameText(ParamStr(I), '-laws') and (I < ParamCount) then
      begin
        { A folder of your own .json laws, so one can be validated before it
          is trusted in the GUI. }
        LawDir := ParamStr(I + 1);
        SkipNext := True;
        Continue;
      end;
      if SkipNext then
      begin
        SkipNext := False;
        Continue;
      end;
      if SameText(ParamStr(I), '-dynamic') then
      begin
        { The behavioural half, which the GUI puts behind "Also check
          behaviour". Orders of magnitude more work than the structural
          checks, so it is opt-in in both places -- and testable here, which
          is how one finds out whether that checkbox does anything. }
        Dynamic := True;
        Continue;
      end;
      if SameText(ParamStr(I), '-md') then
      begin
        { The markdown rendering, which is what the GUI's report panel shows.
          Here so it can be eyeballed -- and diffed against -report -- without
          driving the GUI: the two renderings are supposed to differ in
          presentation only. }
        Markdown := True;
        Report   := True;
        Continue;
      end;
      if SameText(ParamStr(I), '-report') then
      begin
        { Prints exactly what Iridium puts in the Text tab, which is how that
          output gets looked at without driving the GUI. }
        Report := True;
        Continue;
      end;
      Pattern := ParamStr(I);
      if Pos('*', Pattern) > 0 then
      begin
        Dir := ExtractFilePath(Pattern);
        if Dir = '' then Dir := GetCurrentDir;
        for F in TDirectory.GetFiles(Dir, ExtractFileName(Pattern)) do
          Files := Files + [F];
      end
      else
        Files := Files + [Pattern];
    end;

    if Length(Files) = 0 then
    begin
      Writeln('Usage: CheckAntFile [-v] [-report] [-md] [-quiet] [-dynamic]');
      Writeln('                   [-laws <dir>]');
      Writeln('                   [-csv <prefix>] [-list <paths.txt>] <file.ant|file.xml> ...');
      Exit;
    end;

    Registry := TRateLawRegistry.Create;
    try
      Registry.LoadDefaults(LawDir, '');
      for var D in Registry.LoadDiagnostics do
        if D.Severity <> sevInfo then
          Writeln(Format('  registry %s %s: %s', [D.Code, D.LawId, D.Message]));
      if not GQuiet then
        Writeln(Format('%d rate law(s) active.', [Registry.ActiveCount]));

      for F in Files do
        try
          CheckOne(F, Registry, Verbose, Report, Markdown, Dynamic);
        except
          on E: Exception do
            Writeln('  FAILED: ', E.ClassName, ': ', E.Message);
        end;

      if not GQuiet then
      begin
        Writeln;
        Writeln('---------------------------------------------');
      end;
      Writeln(Format('%d file(s), %d reaction(s), %d associated',
        [GFiles, GReactions, GAssociated]));
      Writeln(Format('%d error(s), %d warning(s)', [GErrors, GWarnings]));
    finally
      Registry.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
