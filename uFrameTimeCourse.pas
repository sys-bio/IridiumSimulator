    unit uFrameTimeCourse;

{ Time course analysis frame.

  Y-axis selection (multi-category):
    The Y-axis list groups every plottable quantity by category, using
    TListBoxGroupHeader rows as non-checkable section dividers:

       Floating Species
       Boundary Species
       Reactions          (reaction rates / fluxes)
       Rates of Change    (dX/dt)
       Global Parameters  (covers assignment-rule targets like x := sin(time))
       Compartments

    A "Show:" filter combo above the list narrows the view to a single
    category (or "All"). The combo is purely a visual filter — items hidden
    by it remain in FSelectedYNames and continue to be plotted/simulated.
    Select All / Unselect All operate on the currently visible items only,
    which gives the buttons a useful scope when the filter is set.

    FSelectedYNames is the canonical Y selection set. The visible listbox
    is just a view onto it: OnChangeCheck syncs visible items in,
    RepopulateYList renders FSelectedYNames out. Hidden categories don't
    lose their checks when the filter changes.

  X-axis selection:
    cbXAxis contains time + state-like quantities (floating species,
    boundary species, global parameters, compartments). Fluxes and rates
    are intentionally excluded — phase plots against a rate are unusual
    and bloat the combo for large models.

  Live plot updates:
    Toggling a checkbox in lstYAxis or changing cbXAxis re-plots immediately
    from the cached simulation matrix - no re-run. The cache is invalidated
    when the session goes dirty (memo edited) or a model reload happens, so
    live updates never paint from stale data. Until the next Simulate
    succeeds, toggling checkboxes simply does nothing.

  Selection persistence across reloads:
    Prior selections survive compatible model reloads. Names that no
    longer exist after a reload are pruned from FSelectedYNames; new names
    arrive unchecked. The very first population checks all floating species
    so the user sees a sensible plot on first Simulate.

  This frame does NOT own a TRoadRunner. It receives an IAnalysisContext
  and uses the shared TModelSession for everything model-related. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.RegularExpressions, System.Math, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  FMX.Objects, FMX.Layouts, FMX.ScrollBox, FMX.ListBox,
  uRR2DSimpleMatrix,
  uAnalysisTypes, uMetaExperiments, uMetaSelector, uMetaOutput,
  Sim.Meta, Sim.Meta.Model,
  System.Skia, FMX.Skia;

type
  { Observable categories surfaced in the Y-axis list. Enum order is the
    display order; filter combo items 1..N follow this enum 1:1, with
    combo index 0 reserved for 'All'.

    ocAssignmentRules is a pseudo-category split out of ocGlobalParameters
    at refresh time by parsing the SBML for <assignmentRule> elements.
    libroadrunner's C API does not expose rule introspection directly, so
    SBML is the source of truth. }
  TObservableCategory = (
    ocFloating,
    ocBoundary,
    ocReactions,
    ocRatesOfChange,
    ocAssignmentRules,
    ocGlobalParameters,
    ocCompartments,
    ocEigenvalues,
    ocElasticities
  );
  TObservableCategorySet = set of TObservableCategory;

  TFrameTimeCourse = class(TFrame, IPythonScriptExporter, IMetaOutputProvider)
    btnSimulate: TButton;
    btnReset: TButton;
    Layout5: TLayout;
    Label7: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtTimeStart: TNumberBox;
    Label2: TLabel;
    Label3: TLabel;
    edtTimeEnd: TNumberBox;
    edtNumberofPoints: TNumberBox;
    Layout1: TLayout;
    Layout11: TLayout;
    GroupBox2: TGroupBox;
    cbXAxis: TComboBox;
    GroupBox3: TGroupBox;
    btnSetTimeCourseSelection: TButton;
    Rectangle4: TRectangle;
    lstYAxis: TListBox;
    btnUnSelectAll: TButton;
    btnTimeCourseSliders: TSpeedButton;
    Image1: TImage;
    chkAlwaysReset: TCheckBox;
    Layout2: TLayout;
    btnCopySliderValuesToModel: TButton;
    lblFilter: TLabel;
    cmbFilter: TComboBox;
    btnConfigIntegrator: TSpeedButton;
    SkSvgConfig: TSkSvg;
    chkReplaceValues: TCheckBox;
    procedure btnSimulateClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSliders1Click(Sender: TObject);
    procedure btnSetTimeCourseSelectionClick(Sender: TObject);
    procedure btnUnSelectAllClick(Sender: TObject);
    procedure btnTimeCourseSlidersClick(Sender: TObject);
    procedure btnCopySliderValuesToModelClick(Sender: TObject);
    procedure btnSimulateMouseLeave(Sender: TObject);
    procedure cmbFilterChange(Sender: TObject);
    procedure btnConfigIntegratorClick(Sender: TObject);
    procedure chkReplaceValuesChange(Sender: TObject);
  private
    FContext:            IAnalysisContext;
    FLastData:           T2DMatrix;
    FHasData:            Boolean;
    FSuppressPlotUpdate: Boolean;

    { Scrolling. The designed panel is taller than a short form, so the
      whole of the designer's content rectangle is re-parented into a
      TVertScrollBox at construction (the parameter-scan frame has the
      equivalent box in its .fmx). FContent is Rectangle1 — it has no
      published field, so it is reached through GroupBox1.Parent. }
    FScroll:             TVertScrollBox;
    FContent:            TControl;

    { Canonical Y-axis selection — survives filter changes. Sorted, case-
      sensitive, no duplicates. The visible listbox is a view onto this. }
    FSelectedYNames:     TStringList;

    { Per-category ID caches, refreshed from RoadRunner on every reload. }
    FCategoryIds:        array[TObservableCategory] of TStringList;

    { True until the first successful population — used to seed defaults
      (all floating species checked) on the very first model load. }
    FFirstPopulation:    Boolean;

    { The list is showing the file's requested selection rather than the
      model's own names, because no model has been loaded yet. Those rows
      are a preview: they must not be read back as if the user had ticked
      them. }
    FShowingUnverified:  Boolean;

    { ── metadata presets ───────────────────────────────────────────────
      The @simulate experiments this model defines. The block fills these
      controls and never runs anything: the user still presses Simulate.
      The dropdown itself, and everything generic about it, lives in
      uMetaSelector. }
    FSelector: TMetaExperimentSelector;

    { What the preset put in the boxes, so an edit to any of them can be
      recognised as the user diverging from it. }
    FPresetTimeStart: Double;
    FPresetTimeEnd:   Double;
    FPresetPoints:    Integer;

    { The user's own settings, captured once before the first preset is
      applied, so selecting '—' can genuinely put them back. }
    FUserTimeStart:   Double;
    FUserTimeEnd:     Double;
    FUserPoints:      Integer;
    FUserYNames:      TStringList;
    FHasUserSettings: Boolean;

    { The Y selection (and X axis) a preset asked for, held until there is
      a model to validate it against.

      A preset is applied when the model is OPENED, which is before it has
      been loaded into RoadRunner — so at that moment there are no names
      to check against and no list to check them in. Writing straight into
      FSelectedYNames looked like it worked and did not: that list is the
      live selection, and the session clears it whenever the model is
      unloaded, which is exactly the state an unopened model is in. Held
      separately, the request survives until PopulateAxisSelectors can
      honour it. }
    FPendingYNames:   TArray<string>;
    FPendingX:        string;
    FHasPendingY:     Boolean;

    { Experiment whose extra @plot commands have already been reported, so
      the warning appears once rather than on every Simulate. }
    FWarnedMultiPlot: string;

    { IMetaOutputProvider — what this panel could satisfy an @output from.
      The Write button itself lives on the shell's notice bar: writing a
      file is not part of setting up a run, and these panels are for
      setup. }
    function GetOutputExperiment: TMetaExperiment;
    function GetOutputData: T2DMatrix;

    procedure ApplyExperiment(AExp: TMetaExperiment; AWasUnset: Boolean);
    procedure RestoreUserSettings(Sender: TObject);
    procedure SnapshotUserSettings;
    { Move a preset's Y/X request into the live selection, filtered to
      names the loaded model actually has. False if nothing matched, so
      the caller can fall back to the usual defaults rather than leave the
      user with an empty selection and no way to plot. }
    function  ApplyPendingYSelection: Boolean;
    procedure DoTimeSettingChanged(Sender: TObject);
    function  DivergedFromPreset: Boolean;
    function  GetMetaExperiments: TMetaExperimentSet;
    { Style the freshly drawn series from the selected experiment's @plot,
      and warn about any it could not draw (conformance C6). }
    procedure ApplyPlotMetadata;

    function  RunSimulation: Boolean;
    procedure OnSliderChanged(Sender: TObject;
                              const ASliderString: string;
                              const AValue: Single);

    procedure SessionStateChanged(Sender: TObject);
    procedure SessionModelReloaded(Sender: TObject;
                                   AParameterSetChanged: Boolean);

    procedure PopulateAxisSelectors;
    procedure RefreshAllIds;
    procedure PartitionGlobalsByRules;
    procedure RepopulateYList;
    function  CurrentFilterCategories: TObservableCategorySet;
    procedure AddCategoryToXAxis(ACat: TObservableCategory);

    procedure SyncSelectionFromVisible;
    procedure SetAllVisibleChecked(AChecked: Boolean);

    procedure DoPlotSelectionChanged(Sender: TObject);
    procedure DoYListCheckChanged(Sender: TObject);
    function  SelectionNeedsRecompute: Boolean;

    function GetPythonScript(const AntimonyText: string): string;

    function  GetSelectedXAxisName:  string;

    procedure InstallScrollBox;
    procedure UpdateContentHeight;
    procedure DoScrollResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SetContext(const AContext: IAnalysisContext);
    procedure SetSimulationParameters(ATimeEnd: Double; ANumPoints: Integer);

    { The shell re-parsed the model's metadata block. AApply = True only
      when a model was OPENED: the first usable @simulate experiment is
      loaded into the controls. AApply = False merely refreshes the
      selector — a re-parse happens on every model reload, and applying
      there would overwrite what the user had just typed. }
    procedure MetadataChanged(AApply: Boolean);

    { Apply the named experiment to this panel and compute it — what
      Metadata ▸ Run Experiment dispatches here. The compute is the
      panel's own, so it loads the model itself and reports its own
      errors. }
    procedure RunExperiment(const ALabel: string);
    procedure AttachToSliders;

    { The canonical Y-axis selection, for the shell to seed the parameter scan's
      observables from on that panel's first appearance. }
    function  GetSelectedYAxisNames: TArray<string>;
  end;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  uRoadRunner, uRRList, uAntimonyAPI, uCommonTypes, ufConfigureCVODE,
  uMetaSymbolProvider;   { CanonicalModelName: '[A]' <-> 'A' }

const
  TIME_COLUMN_LABEL = 'time';

  CATEGORY_LABELS: array[TObservableCategory] of string = (
    'Floating Species',
    'Boundary Species',
    'Reactions',
    'Rates of Change',
    'Assignment Rules',
    'Global Parameters',
    'Compartments',
    'Eigenvalues',
    'Scaled Elasticities'
  );

  { Range form so the set picks up any future enum additions automatically. }
  ALL_CATEGORIES: TObservableCategorySet =
    [Low(TObservableCategory)..High(TObservableCategory)];

  { Regex for SBML <assignmentRule variable="..."> elements. Tolerates:
      - optional namespace prefix (e.g. <sbml:assignmentRule>)
      - any attribute order (variable= may not be first)
      - whitespace around =
      - either quote style
    libsbml output is well-formed and single-line per element so we don't
    need full XML parsing; regex is two orders of magnitude faster on the
    50-60 species models that matter most. }
  ASSIGNMENT_RULE_PATTERN =
    '<(?:\w+:)?assignmentRule\b[^>]*?\bvariable\s*=\s*' +
    '(?:"([^"]+)"|''([^'']+)'')';

{ ── construction / destruction ─────────────────────────────────────────── }

constructor TFrameTimeCourse.Create(AOwner: TComponent);
var
  Cat: TObservableCategory;
begin
  inherited;

  InstallScrollBox;

  FSelector := TMetaExperimentSelector.Create(Self, mekTimeCourse,
                                              GetMetaExperiments);
  FSelector.OnApply   := ApplyExperiment;
  FSelector.OnRestore := RestoreUserSettings;
  { Immediately above the time-settings group, where the values it fills
    in are. GroupBox1.Parent rather than a named container: the frame's
    background rectangle has no published field to reach it by. }
  FSelector.Place(GroupBox1.Parent, GroupBox1);


  FUserYNames := TStringList.Create;
  FUserYNames.CaseSensitive := True;
  FUserYNames.Sorted        := True;
  FUserYNames.Duplicates    := dupIgnore;

  FSelectedYNames := TStringList.Create;
  FSelectedYNames.CaseSensitive := True;
  FSelectedYNames.Sorted        := True;
  FSelectedYNames.Duplicates    := dupIgnore;

  for Cat := Low(TObservableCategory) to High(TObservableCategory) do
  begin
    FCategoryIds[Cat] := TStringList.Create;
    FCategoryIds[Cat].CaseSensitive := True;
  end;

  cmbFilter.ItemIndex := 1;
  FFirstPopulation := True;

  { After the selector has been placed, so its row counts towards the height. }
  UpdateContentHeight;
end;

{ Re-parent the designed content into a vertical scroll box so the panel
  scrolls rather than clips when the form is shorter than the design height. }
procedure TFrameTimeCourse.InstallScrollBox;
begin
  if not (GroupBox1.Parent is TControl) then
    Exit;

  FContent := TControl(GroupBox1.Parent);
  if FContent = Self then   { already flat — nothing to wrap }
    Exit;

  FScroll := TVertScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.Align  := TAlignLayout.Client;

  FContent.Parent := FScroll;
  FContent.Align  := TAlignLayout.Top;

  FScroll.OnResize := DoScrollResize;
end;

{ The content rectangle must be as tall as its Top-aligned children to make
  the scroll box scroll, but never shorter than the viewport, or its border
  would stop part-way down a tall form. }
procedure TFrameTimeCourse.UpdateContentHeight;
var
  I: Integer;
  C: TControl;
  H: Single;
begin
  if (FScroll = nil) or (FContent = nil) then
    Exit;

  H := FContent.Padding.Top + FContent.Padding.Bottom;
  for I := 0 to FContent.ControlsCount - 1 do
  begin
    C := FContent.Controls[I];
    if C.Visible and (C.Align = TAlignLayout.Top) then
      H := H + C.Height + C.Margins.Top + C.Margins.Bottom;
  end;

  if H < FScroll.Height then
    H := FScroll.Height;

  if FContent.Height <> H then
    FContent.Height := H;
end;

procedure TFrameTimeCourse.DoScrollResize(Sender: TObject);
begin
  UpdateContentHeight;
end;

destructor TFrameTimeCourse.Destroy;
var
  Cat: TObservableCategory;
begin
  FSelectedYNames.Free;
  FSelector.Free;
  FUserYNames.Free;
  for Cat := Low(TObservableCategory) to High(TObservableCategory) do
    FCategoryIds[Cat].Free;
  inherited;
end;

{ ── metadata presets ───────────────────────────────────────────────────── }

function TFrameTimeCourse.GetMetaExperiments: TMetaExperimentSet;
begin
  { A function, not a stored reference: the set is rebuilt wholesale on
    every re-parse. }
  if FContext = nil then
    Result := nil
  else
    Result := FContext.MetaExperiments;
end;

procedure TFrameTimeCourse.MetadataChanged(AApply: Boolean);
begin
  if (FContext = nil) or (FSelector = nil) then Exit;

  { A fresh parse may have changed how many plots an experiment has, so
    the "only drew the first" warning is due again. }
  FWarnedMultiPlot := '';

  FSelector.Rebuild(FContext.MetaExperiments);
  { Rebuild shows or hides the strip, changing how tall the panel is. }
  UpdateContentHeight;
  if FContext <> nil then FContext.OutputStateChanged;

  { A model was opened. The first usable @simulate experiment is applied;
    an unusable one is listed but never applied — its fields are not
    trustworthy. }
  if AApply then
    FSelector.ApplyFirstUsable(FContext.MetaExperiments);
end;

procedure TFrameTimeCourse.RunExperiment(const ALabel: string);
begin
  if (FContext = nil) or (FSelector = nil) then Exit;

  { Load BEFORE applying, which is the whole reason this is not simply
    "apply then press Simulate". The block has just been edited, so the
    source is dirty: applying first would validate the @plot's names
    against the OUTGOING model, and the reload that the compute then
    triggers would rebuild the selectors underneath the preset. That is
    what made a changed block need running twice — the first run applied
    to the old model and the second to the new one. }
  try
    if not FContext.Session.EnsureLoaded then
    begin
      ShowMessage('Cannot load model: ' + FContext.Session.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Model load failed: ' + E.Message);
      Exit;
    end;
  end;

  { The reload above re-parsed the block, so this is the fresh set. }
  FSelector.ApplyLabel(FContext.MetaExperiments, ALabel);
  RunSimulation;
end;

procedure TFrameTimeCourse.SnapshotUserSettings;
begin
  { Once only. A second preset must not overwrite the snapshot with the
    first preset's values — '—' means "what I had", not "the last thing
    the file did". }
  if FHasUserSettings then Exit;
  FUserTimeStart   := edtTimeStart.Value;
  FUserTimeEnd     := edtTimeEnd.Value;
  FUserPoints      := Trunc(edtNumberofPoints.Value);
  FUserYNames.Assign(FSelectedYNames);
  { The plot's appearance too. A @plot switches on log axes, grids and
    titles, and those outlive the series they were applied to — so
    without this, stepping back to '—' returns the numbers but leaves the
    file's log axis in place. }
  if FContext <> nil then
    FContext.PlotCaptureUserStyle;
  FHasUserSettings := True;
end;

procedure TFrameTimeCourse.RestoreUserSettings(Sender: TObject);
begin
  if not FHasUserSettings then Exit;

  { A preset that never got applied must not be applied later just
    because the user asked for their own settings back. }
  FHasPendingY := False;
  FPendingX    := '';

  FSuppressPlotUpdate := True;
  try
    edtTimeStart.Value      := FUserTimeStart;
    edtTimeEnd.Value        := FUserTimeEnd;
    edtNumberofPoints.Value := FUserPoints;

    if FUserYNames.Count > 0 then
      FSelectedYNames.Assign(FUserYNames)
    else
      { The snapshot was taken on a model that had only just been opened,
        so there was no selection to remember. Handing back an empty one
        would leave Simulate unable to do anything — which is not what
        "my own settings" should mean. Fall back to Iridium's own default
        of every floating species, the same thing a model with no
        metadata block would have given. }
      FSelectedYNames.Assign(FCategoryIds[ocFloating]);
  finally
    FSuppressPlotUpdate := False;
  end;

  RepopulateYList;

  { Put the plot's appearance back as well — a log axis or a title the
    preset switched on is as much part of "the file's settings" as the
    numbers were. }
  if FContext <> nil then
    FContext.PlotRestoreUserStyle;

  { No named experiment means no @output to honour. }
  if FContext <> nil then FContext.OutputStateChanged;

  { Results on screen were computed from other settings. }
  FHasData := False;
end;

procedure TFrameTimeCourse.ApplyExperiment(AExp: TMetaExperiment;
  AWasUnset: Boolean);
var
  Sim:  TSimulateCommand;
  Plot: TPlotCommand;
  Idx:  Integer;
begin
  if (AExp = nil) or (not AExp.Usable) then Exit;
  if not (AExp.Task is TSimulateCommand) then Exit;
  Sim := TSimulateCommand(AExp.Task);

  SnapshotUserSettings;

  { Coming off '—', everything on the panel right now is the user's own
    work — the numbers, the Y selection and the plot's appearance alike —
    so re-capture all of it before the preset overwrites it. That makes
    '—' mean "the last settings I made myself" rather than only "the ones
    I had before the very first preset", which silently discarded
    everything the user did while '—' was selected. }
  if AWasUnset and FHasUserSettings then
  begin
    FUserTimeStart := edtTimeStart.Value;
    FUserTimeEnd   := edtTimeEnd.Value;
    FUserPoints    := Trunc(edtNumberofPoints.Value);
    FUserYNames.Assign(FSelectedYNames);
    if FContext <> nil then
      FContext.PlotCaptureUserStyle;
  end;

  FSelector.Suppressed := True;
  FSuppressPlotUpdate  := True;
  try
    { 'steps' has already been folded into Points by the validator
      (points = steps + 1), so there is no arithmetic to repeat here. }
    edtTimeStart.Value      := Sim.TimeStart;
    edtTimeEnd.Value        := Sim.TimeEnd;
    edtNumberofPoints.Value := Sim.Points;

    FPresetTimeStart := Sim.TimeStart;
    FPresetTimeEnd   := Sim.TimeEnd;
    FPresetPoints    := Sim.Points;

    { The experiment's @plot says which quantities the user wanted drawn,
      which is the Y selection. Recorded as a request rather than applied
      here: on the path that matters — opening a model — there is no
      loaded model yet, so there are no names to validate against and no
      list to show them in. PopulateAxisSelectors honours it as soon as
      there is. A task with no plot leaves the selection alone. }
    Plot := AExp.FirstPlot;
    if (Plot <> nil) and (Length(Plot.Y) > 0) then
    begin
      FPendingYNames := Copy(Plot.Y);
      FPendingX      := Plot.X;
      FHasPendingY   := True;

      if (FContext <> nil) and FContext.Session.IsLoaded then
        { A model is loaded, so the request can be validated and shown at
          once — there is no reason to make the user press Simulate to
          see the selection change. }
        ApplyPendingYSelection
      else
      begin
        { No model loaded yet. Iridium loads lazily, so this is the normal
          state between opening a file and the first compute, and the
          observable lists do not exist to validate against or render
          into.

          Take the request at face value anyway. The alternative — leave
          the live selection alone until the lists appear — is what made
          switching experiments look like it did nothing: the panel went
          on showing the PREVIOUS experiment's selection, and Simulate
          either plotted the wrong variables or refused for want of any.
          Anything the model turns out not to have is dropped by the
          prune in PopulateAxisSelectors, and the pending request is
          still honoured there, so this is only ever an early view of the
          same answer. }
        FSuppressPlotUpdate := True;
        try
          FSelectedYNames.Clear;
          for Idx := 0 to High(FPendingYNames) do
            FSelectedYNames.Add(FPendingYNames[Idx]);
        finally
          FSuppressPlotUpdate := False;
        end;
        RepopulateYList;
      end;
    end;

    { The selector already holds this label — it is what dispatched here. }
  finally
    FSuppressPlotUpdate  := False;
    FSelector.Suppressed := False;
  end;

  { Settings changed, so anything on screen is stale. Nothing is
    recomputed: the user presses Simulate when they want a result. }
  FHasData := False;
  if FContext <> nil then FContext.OutputStateChanged;
end;

function TFrameTimeCourse.ApplyPendingYSelection: Boolean;
var
  Cat:    TObservableCategory;
  Known:  TDictionary<string, string>;
  Prev:   TStringList;
  Name:   string;
  ModelId: string;
  I, Idx: Integer;
begin
  Result := False;
  if not FHasPendingY then Exit;

  { Known: every name the model actually has, across all categories — a
    @plot may legitimately name a flux or a boundary species, not only a
    floater. Prev: what was selected before, so a request naming nothing
    this model has can fall back to something usable instead of leaving
    an empty selection that Simulate can only refuse. }
  { Keyed on the name as the METADATA spells it, valued with the id the
    MODEL uses — 'A' -> '[A]' for a floating species, 'k1' -> 'k1' for a
    parameter. Iridium's selection lists and result column headers are
    keyed on the model's form, so the translation has to survive into
    what gets stored, not just into the comparison. }
  Known := TDictionary<string, string>.Create;
  Prev  := TStringList.Create;
  try
    for Cat := Low(TObservableCategory) to High(TObservableCategory) do
      for I := 0 to FCategoryIds[Cat].Count - 1 do
      begin
        ModelId := FCategoryIds[Cat][I];
        { First writer wins, matching the category order the lists are
          built in, so a name appearing twice keeps its primary kind. }
        if not Known.ContainsKey(CanonicalModelName(ModelId)) then
          Known.Add(CanonicalModelName(ModelId), ModelId);
      end;

    { No model loaded yet, so nothing can be validated. Leave the request
      standing — this is the ordinary case when a model has just been
      opened, and consuming it here is exactly the bug this whole
      mechanism exists to avoid. }
    if Known.Count = 0 then Exit;

    Prev.Assign(FSelectedYNames);

    FSuppressPlotUpdate := True;
    try
      FSelectedYNames.Clear;
      for Name in FPendingYNames do
        { Accept the model's own spelling too, so a block written against
          RoadRunner's '[A]' form works as well as the plain 'A' the
          format calls for. }
        if Known.TryGetValue(Name, ModelId) or
           Known.TryGetValue(CanonicalModelName(Name), ModelId) then
        begin
          FSelectedYNames.Add(ModelId);
          Result := True;
        end;

      if not Result then
      begin
        if Prev.Count > 0 then
          FSelectedYNames.Assign(Prev)
        else
          FSelectedYNames.Assign(FCategoryIds[ocFloating]);
      end;

      { FPendingX is deliberately NOT applied here. When this runs from
        PopulateAxisSelectors the X combo has not been rebuilt yet, so a
        lookup would search the previous model's names. It is applied
        where the combo is rebuilt, and cleared there. }
      if Result and (FPendingX <> '') and (cbXAxis.Items.Count > 0) then
      begin
        Idx := cbXAxis.Items.IndexOf(FPendingX);
        { 'time' is itself, but a phase portrait's x: names a species and
          needs the same translation as the y: entries. }
        if (Idx < 0) and Known.TryGetValue(FPendingX, ModelId) then
          Idx := cbXAxis.Items.IndexOf(ModelId);
        if Idx >= 0 then
        begin
          cbXAxis.ItemIndex := Idx;
          FPendingX := '';
        end;
      end;
    finally
      FSuppressPlotUpdate := False;
    end;

    { Consumed: satisfied or not, a request must not go on overriding the
      user at every later reload. }
    FHasPendingY := False;

    { Always, not only when something matched. The listbox is a view onto
      FSelectedYNames, and this function has just rewritten it — skipping
      the render leaves ticks on screen that no longer mean anything,
      which is worse than a wrong selection because it cannot be seen to
      be wrong. }
    RepopulateYList;
  finally
    Prev.Free;
    Known.Free;
  end;
end;

function TFrameTimeCourse.DivergedFromPreset: Boolean;
begin
  Result := (not SameValue(edtTimeStart.Value, FPresetTimeStart)) or
            (not SameValue(edtTimeEnd.Value,   FPresetTimeEnd))   or
            (Trunc(edtNumberofPoints.Value) <> FPresetPoints);
end;

procedure TFrameTimeCourse.DoTimeSettingChanged(Sender: TObject);
begin
  if (FSelector = nil) or FSelector.Suppressed or FSuppressPlotUpdate then Exit;
  if FSelector.ActiveLabel = '' then Exit;
  if not DivergedFromPreset then Exit;

  { The controls no longer describe the named experiment, so the selector
    must stop claiming they do. This is the '—' state, and it arises on
    its own rather than being something the user has to think to choose —
    which is why it must not restore anything. }
  FSelector.MarkDiverged;
end;

function TFrameTimeCourse.GetOutputExperiment: TMetaExperiment;
begin
  { Nil unless there is genuinely something to write: an experiment
    selected, its @output commands present, and a result to write from.
    The shell shows or hides the Write button on that answer alone, so
    the offer can never appear with nothing behind it. }
  Result := nil;
  if (FSelector = nil) or (not FHasData) then Exit;

  Result := FSelector.ActiveExperiment;
  if Result = nil then Exit;
  if (not Result.Usable) or (Length(Result.Outputs) = 0) then
    Result := nil;
end;

function TFrameTimeCourse.GetOutputData: T2DMatrix;
begin
  if FHasData then
    Result := FLastData
  else
    Result := nil;
end;

procedure TFrameTimeCourse.ApplyPlotMetadata;
var
  Exp:     TMetaExperiment;
  Skipped: TArray<TPlotCommand>;
  P:       TPlotCommand;
  Names:   string;
begin
  Exp := FSelector.ActiveExperiment;
  if (Exp = nil) or (not Exp.Usable) then Exit;

  P := Exp.FirstPlot;
  if P = nil then Exit;

  { Reset to the user's own appearance FIRST, then overlay only what this
    @plot actually specifies.

    A @plot is applied key by key — absent keys are left alone, so that a
    command saying nothing about the grid does not switch the grid off.
    But "left alone" has to mean left at the user's baseline, not left at
    whatever the PREVIOUS experiment's @plot happened to set. Without the
    reset, styling accumulates across experiments: switch from one that
    dashes B to one that says nothing about series at all, and B stays
    dashed for no reason the file can explain. Each experiment should
    look the way its own command describes it. }
  FContext.PlotRestoreUserStyle;
  FContext.PlotApplyMetaStyle(P);

  { Iridium has one plot surface. Where an experiment defines several
    @plot commands it draws the first and must warn naming the others
    (conformance C6, and spec 13 records this as Iridium's documented
    behaviour). The warning belongs here, at the moment a plot was
    actually declined — not at load, where it would be one more line in a
    wall of text the user learns to dismiss. }
  Skipped := Exp.SkippedPlots;
  if Length(Skipped) = 0 then Exit;

  { Once per experiment, not once per Simulate: a dialog on every run
    would be punishment, and the user cannot act on it any faster the
    fifth time. Reset when the metadata is re-parsed. }
  if FWarnedMultiPlot = Exp.LabelText then Exit;
  FWarnedMultiPlot := Exp.LabelText;

  Names := '';
  for P in Skipped do
  begin
    if Names <> '' then Names := Names + ', ';
    Names := Names + P.DisplayName;
  end;
  ShowMessage(
    'Experiment ' + Exp.LabelText + ' defines more than one plot. ' +
    'Iridium has a single plot surface, so it drew the first and did not ' +
    'draw: ' + Names + '.');
end;

{ ── simulation parameters from main form ───────────────────────────────── }

procedure TFrameTimeCourse.SetSimulationParameters(ATimeEnd: Double;
                                                    ANumPoints: Integer);
begin
  edtTimeEnd.Text   := FloatToStr(ATimeEnd);
  edtNumberofPoints.Text := IntToStr(ANumPoints);
  { Invalidate any cached results -- the old simulation no longer
    matches the displayed parameters. }
  FHasData := False;
end;

procedure TFrameTimeCourse.AttachToSliders;
begin
  if FContext = nil then Exit;
  FContext.SliderContainer.OnSliderChanged := OnSliderChanged;
  FContext.SliderContainer.ReleaseOnlyMode := False;  { continuous updates }
end;

{ ── context wiring ─────────────────────────────────────────────────────── }

procedure TFrameTimeCourse.SetContext(const AContext: IAnalysisContext);
begin
  FContext := AContext;
  if FContext <> nil then
  begin
    FContext.Session.AddStateListener(SessionStateChanged);
    FContext.Session.AddReloadedListener(SessionModelReloaded);
  end;

  { Y-axis check changes go through DoYListCheckChanged so we can update
    the canonical FSelectedYNames before re-plotting. X-axis is a simple
    re-plot. }
  lstYAxis.OnChangeCheck := DoYListCheckChanged;
  cbXAxis.OnChange       := DoPlotSelectionChanged;

  { Editing any of the three time settings means the controls no longer
    describe the selected experiment. Wired here rather than in the .fmx
    because nothing else needs these events. }
  edtTimeStart.OnChange      := DoTimeSettingChanged;
  edtTimeEnd.OnChange        := DoTimeSettingChanged;
  edtNumberofPoints.OnChange := DoTimeSettingChanged;
end;

{ ── session callbacks ──────────────────────────────────────────────────── }

procedure TFrameTimeCourse.SessionStateChanged(Sender: TObject);
var
  Cat: TObservableCategory;
begin
  { Memo edited — cached matrix is stale; disable live updates. }
  if FContext.Session.IsDirty then
    FHasData := False;

  { Model unloaded (new file, New, or failed parse) — clear everything
    so stale names from the previous model can't leak into the next one.
    Re-arm FFirstPopulation so the next successful load gets fresh defaults
    (all floaters checked). }
  if not FContext.Session.IsLoaded then
  begin
    FHasData := False;
    cbXAxis.Clear;
    lstYAxis.Clear;
    FSelectedYNames.Clear;
    FFirstPopulation := True;
    { The per-category id caches too, and not merely for tidiness: they are
      how RepopulateYList decides whether the model's names are known yet
      (NoModelNamesYet). Left holding the previous model's ids, the next
      model's @plot preview is rendered from THAT model's list instead —
      the new panel looks unchanged, and only after loading one model does
      opening a second go wrong. }
    for Cat := Low(TObservableCategory) to High(TObservableCategory) do
      FCategoryIds[Cat].Clear;
  end;
end;

procedure TFrameTimeCourse.SessionModelReloaded(Sender: TObject;
  AParameterSetChanged: Boolean);
begin
  { Any cached data is from a pre-reload run; treat as invalid. }
  FHasData := False;
  PopulateAxisSelectors;
end;

{ ── axis selector population ───────────────────────────────────────────── }

procedure TFrameTimeCourse.RefreshAllIds;
var
  RR:  TRoadRunner;
  Tmp: TStringList;

  procedure CopyInto(ACat: TObservableCategory; ASource: TStringList);
  begin
    try
      FCategoryIds[ACat].Assign(ASource);
    finally
      ASource.Free;
    end;
  end;

  procedure FetchEigenvalues;
  var
    Src: TStringList;
    I:   Integer;
    Id:  string;
  begin
    FCategoryIds[ocEigenvalues].Clear;
    Src := RR.getEigenvalueIds;
    try
      { getEigenvalueIds returns triplets per dependent species --
        'eigen(X)', 'eigenReal(X)', 'eigenImag(X)'. The bare 'eigen(X)'
        form is complex-valued and the result matrix can't hold a complex
        scalar, so we drop them and keep only the Real and Imag projections.
        Filter is a literal 6-char prefix compare; case-sensitive on purpose
        since SBML/RR identifiers are. }
      for I := 0 to Src.Count - 1 do
      begin
        Id := Src[I];
        if Copy(Id, 1, 6) <> 'eigen(' then
          FCategoryIds[ocEigenvalues].Add(Id);
      end;
    finally
      Src.Free;
    end;
  end;

  procedure FetchElasticities;
  var
    Outer, Inner, Ids: TRRList;
    I, J:              Integer;
  begin
    FCategoryIds[ocElasticities].Clear;
    Outer := nil;
    try
      Outer := RR.getElasticityIds;
      if Outer = nil then Exit;

      { Nesting per the libroadrunner convention: outer list is one entry
        per reaction; each entry is a 2-element sub-list where [0] is the
        reaction name and [1] is a sub-list of the elasticity ID strings
        for that reaction. We only need the flat list of IDs. Each access
        is guarded so a malformed structure (older / future binding shape)
        degrades to "no elasticities" rather than crashing. }
      for I := 0 to Outer.Count - 1 do
      begin
        if (Outer[I] = nil) or (Outer[I].list = nil) then Continue;
        Inner := Outer[I].list;
        if Inner.Count < 2 then Continue;
        if (Inner[1] = nil) or (Inner[1].list = nil) then Continue;

        Ids := Inner[1].list;
        for J := 0 to Ids.Count - 1 do
          if Ids[J] <> nil then
            { libroadrunner inconsistency: getElasticityIds returns IDs
              with no whitespace ('ec(J2,S1)'), but simulate()'s default
              columnHeader formats elasticity columns with a space after
              the comma ('ec(J2, S1)'). When the selection list is set
              explicitly, simulate() echoes our input as the columnHeader,
              so we standardize on the no-space form here and the round
              trip through RR matches at FindCol time. Strip spaces at
              the capture point so the canonical name is consistent
              everywhere downstream — selection list, listbox, plot
              lookup. Remove once the libroadrunner team aligns the
              two APIs. }
            FCategoryIds[ocElasticities].Add(
              StringReplace(string(Ids[J].sValue), ' ', '',
                            [rfReplaceAll]));
      end;
    finally
      Outer.Free;
    end;
  end;

begin
  RR := FContext.Session.RoadRunner;

  Tmp := RR.getFloatingSpeciesIds;   CopyInto(ocFloating,         Tmp);
  Tmp := RR.getBoundarySpeciesIds;   CopyInto(ocBoundary,         Tmp);
  Tmp := RR.getReactionIds;          CopyInto(ocReactions,        Tmp);
  Tmp := RR.getRatesOfChangeIds;     CopyInto(ocRatesOfChange,    Tmp);
  Tmp := RR.getGlobalParameterIds;   CopyInto(ocGlobalParameters, Tmp);
  Tmp := RR.getCompartmentIds;       CopyInto(ocCompartments,     Tmp);

  FetchEigenvalues;
  FetchElasticities;

  { ocAssignmentRules is derived, not pulled from RR. Must run after
    ocGlobalParameters is populated since it moves entries out of it. }
  PartitionGlobalsByRules;
end;

procedure TFrameTimeCourse.PartitionGlobalsByRules;
var
  SBML:    string;
  Matches: TMatchCollection;
  Match:   TMatch;
  Target:  string;
  RuleSet: TStringList;
  I, Idx:  Integer;
begin
  FCategoryIds[ocAssignmentRules].Clear;
  if FCategoryIds[ocGlobalParameters].Count = 0 then Exit;

  { getSBML returns AnsiString; cast widens but keeps the bytes since
    libsbml emits UTF-8 ASCII for tag/attribute syntax. }
  SBML := string(FContext.Session.RoadRunner.getSBML);
  if SBML = '' then Exit;

  RuleSet := TStringList.Create;
  try
    RuleSet.CaseSensitive := True;
    RuleSet.Sorted        := True;
    RuleSet.Duplicates    := dupIgnore;

    { Build the set of every <assignmentRule variable="X"> target in the
      model, regardless of underlying entity type. Group 1 captures the
      double-quoted form, group 2 the single-quoted form — exactly one
      will be populated per match. }
    Matches := TRegEx.Matches(SBML, ASSIGNMENT_RULE_PATTERN);
    for Match in Matches do
    begin
      Target := Match.Groups[1].Value;
      if Target = '' then
        Target := Match.Groups[2].Value;
      if Target <> '' then
        RuleSet.Add(Target);
    end;

    if RuleSet.Count = 0 then Exit;

    { Move every rule-target that's also a global parameter from
      ocGlobalParameters to ocAssignmentRules. Species and compartments
      under assignment rules stay in their entity-type categories on
      purpose -- a user looking for S1 expects to find it under Floating
      Species, not under a rule-based category. Iterate backwards because
      we delete during the walk. }
    for I := FCategoryIds[ocGlobalParameters].Count - 1 downto 0 do
    begin
      Idx := RuleSet.IndexOf(FCategoryIds[ocGlobalParameters][I]);
      if Idx >= 0 then
      begin
        FCategoryIds[ocAssignmentRules].Add(
          FCategoryIds[ocGlobalParameters][I]);
        FCategoryIds[ocGlobalParameters].Delete(I);
      end;
    end;

    { ocAssignmentRules ends up reverse-iteration-order; sort for stable
      display. ocGlobalParameters retains its RR-provided order minus the
      moved-out entries. }
    FCategoryIds[ocAssignmentRules].Sort;
  finally
    RuleSet.Free;
  end;
end;

procedure TFrameTimeCourse.AddCategoryToXAxis(ACat: TObservableCategory);
var
  I: Integer;
begin
  for I := 0 to FCategoryIds[ACat].Count - 1 do
    cbXAxis.Items.Add(FCategoryIds[ACat][I]);
end;

function TFrameTimeCourse.CurrentFilterCategories: TObservableCategorySet;
var
  Idx: Integer;
begin
  Idx := cmbFilter.ItemIndex;
  { Combo layout: 0 = 'All', then 1..N = one entry per TObservableCategory
    in enum order. Negative or zero -> show everything. }
  if Idx <= 0 then
    Result := ALL_CATEGORIES
  else
    Result := [TObservableCategory(Idx - 1)];
end;

procedure TFrameTimeCourse.RepopulateYList;
var
  Cat:     TObservableCategory;
  Visible: TObservableCategorySet;
  I:       Integer;
  Header:  TListBoxGroupHeader;
  Item:    TListBoxItem;
  Name:    string;

  { True before the first successful load: Iridium loads lazily, so a
    model that has only been opened has no names yet. }
  function NoModelNamesYet: Boolean;
  var
    C: TObservableCategory;
  begin
    for C := Low(TObservableCategory) to High(TObservableCategory) do
      if FCategoryIds[C].Count > 0 then
        Exit(False);
    Result := True;
  end;

begin
  Visible := CurrentFilterCategories;

  FSuppressPlotUpdate := True;
  try
    lstYAxis.BeginUpdate;
    try
      lstYAxis.Clear;

      { Nothing to list from the model, but the file has said what it
        wants plotted — so show that rather than an empty box. An empty
        list and "nothing is selected" look identical, and the difference
        matters: one means the model has not loaded, the other means the
        user must pick something.

        Shown disabled, because these names have not been checked against
        anything yet. Pressing Simulate loads the model and replaces them
        with the real list, at which point any name the model does not
        have quietly drops out. }
      FShowingUnverified := NoModelNamesYet and (FSelectedYNames.Count > 0);
      if FShowingUnverified then
      begin
        Header := TListBoxGroupHeader.Create(lstYAxis);
        Header.Parent     := lstYAxis;
        Header.Text       := 'From the model file — press Simulate to load';
        Header.Selectable := False;

        for I := 0 to FSelectedYNames.Count - 1 do
        begin
          Name := FSelectedYNames[I];
          Item := TListBoxItem.Create(lstYAxis);
          Item.Parent    := lstYAxis;
          Item.Text      := Name;
          Item.TagString := Name;
          Item.IsChecked := True;
          Item.Enabled   := False;
        end;
        Exit;
      end;

      for Cat := Low(TObservableCategory) to High(TObservableCategory) do
      begin
        if not (Cat in Visible) then Continue;
        if FCategoryIds[Cat].Count = 0 then Continue;

        Header := TListBoxGroupHeader.Create(lstYAxis);
        Header.Parent     := lstYAxis;
        Header.Text       := CATEGORY_LABELS[Cat];
        Header.Selectable := False;

        for I := 0 to FCategoryIds[Cat].Count - 1 do
        begin
          Name := FCategoryIds[Cat][I];
          Item := TListBoxItem.Create(lstYAxis);
          Item.Parent    := lstYAxis;
          Item.Text      := Name;
          { Stash the canonical RR identifier on TagString. Item.Text goes
            through FMX styled-text plumbing that has been observed to
            normalize whitespace (e.g. the space in 'ec(J0, J0_Keq)'),
            and we don't want that touching the string we'll later match
            against the simulation result's columnHeader. TagString is a
            plain string field nothing renders or normalizes. }
          Item.TagString := Name;
          Item.IsChecked := FSelectedYNames.IndexOf(Name) >= 0;
        end;
      end;
    finally
      lstYAxis.EndUpdate;
    end;
  finally
    FSuppressPlotUpdate := False;
  end;
end;

procedure TFrameTimeCourse.PopulateAxisSelectors;
var
  Cat:        TObservableCategory;
  ValidIds:   TStringList;
  I:          Integer;
  IsFirstPop: Boolean;
  PrevX:      string;
  Id:         string;
  Idx:        Integer;
begin
  if (FContext = nil) or (not FContext.Session.IsLoaded) then Exit;

  IsFirstPop := FFirstPopulation;

  { Pull every category's IDs fresh. }
  RefreshAllIds;

  { Prune FSelectedYNames: drop any name that doesn't exist in the
    current model. Build the valid-set as the union of all categories. }
  ValidIds := TStringList.Create;
  try
    ValidIds.CaseSensitive := True;
    ValidIds.Sorted        := True;
    ValidIds.Duplicates    := dupIgnore;

    for Cat := Low(TObservableCategory) to High(TObservableCategory) do
      for I := 0 to FCategoryIds[Cat].Count - 1 do
        ValidIds.Add(FCategoryIds[Cat][I]);

    for I := FSelectedYNames.Count - 1 downto 0 do
      if ValidIds.IndexOf(FSelectedYNames[I]) < 0 then
        FSelectedYNames.Delete(I);
  finally
    ValidIds.Free;
  end;

  { A preset's Y selection, if one is outstanding. This is the first point
    at which the model's names are known, which is why it happens here
    rather than where the preset was applied. It wins over the
    first-population default — the file said what to plot, so burying it
    under "every floating species" would be ignoring the model's own
    instructions. }
  if FHasPendingY and ApplyPendingYSelection then
  begin
    IsFirstPop       := False;
    FFirstPopulation := False;
  end;

  { First-population defaults: check every floating species. Subsequent
    populations preserve whatever the user has. }
  if IsFirstPop then
  begin
    for I := 0 to FCategoryIds[ocFloating].Count - 1 do
    begin
      Id := FCategoryIds[ocFloating][I];
      if FSelectedYNames.IndexOf(Id) < 0 then
        FSelectedYNames.Add(Id);
    end;
    FFirstPopulation := False;
  end;

  { Rebuild X-axis: time + state-like quantities, preserve prior choice. }
  FSuppressPlotUpdate := True;
  try
    PrevX := '';
    if cbXAxis.ItemIndex >= 0 then
      PrevX := cbXAxis.Items[cbXAxis.ItemIndex];

    cbXAxis.BeginUpdate;
    try
      cbXAxis.Clear;
      cbXAxis.Items.Add(TIME_COLUMN_LABEL);
      AddCategoryToXAxis(ocFloating);
      //AddCategoryToXAxis(ocBoundary);
      AddCategoryToXAxis(ocAssignmentRules);
      //AddCategoryToXAxis(ocGlobalParameters);
      //AddCategoryToXAxis(ocCompartments);

      if PrevX <> '' then
        cbXAxis.ItemIndex := cbXAxis.Items.IndexOf(PrevX);

      { A preset's x: overrides the remembered choice — the file is
        describing this experiment, and 'x' is how a phase portrait is
        asked for. Cleared once honoured; a name this model does not have
        falls through to the remembered choice. }
      if FPendingX <> '' then
      begin
        Idx := cbXAxis.Items.IndexOf(FPendingX);
        { The combo holds the model's own ids, so a species named in the
          block has to be looked up in RoadRunner's concentration form
          as well. 'time' matches directly. }
        if Idx < 0 then
          Idx := cbXAxis.Items.IndexOf('[' + FPendingX + ']');
        if Idx >= 0 then
          cbXAxis.ItemIndex := Idx;
        FPendingX := '';
      end;

      if cbXAxis.ItemIndex < 0 then
        cbXAxis.ItemIndex := 0;
    finally
      cbXAxis.EndUpdate;
    end;
  finally
    FSuppressPlotUpdate := False;
  end;

  RepopulateYList;
end;

{ ── visible-list / canonical-selection sync ────────────────────────────── }

procedure TFrameTimeCourse.SyncSelectionFromVisible;
var
  I, Idx: Integer;
  Item:   TListBoxItem;
  Id:     string;
begin
  { An empty list is not evidence that nothing is selected — it is what a
    list looks like mid-rebuild, or before the model has been loaded.
    Syncing from it would wipe the canonical selection and leave Simulate
    with nothing to plot, so treat it as "no information" and return. }
  if lstYAxis.Count = 0 then Exit;

  { Likewise when the rows are the file's unverified preview: they were
    rendered FROM the selection, so reading them back would be circular
    at best, and would drop the RoadRunner spelling of any name once the
    model does load. }
  if FShowingUnverified then Exit;

  { For every visible (non-header) row, mirror its IsChecked state into
    FSelectedYNames. Items hidden by the filter are NOT touched here —
    they keep whatever state they had.

    Read the canonical id from TagString, not Text. Text is for display
    and FMX is allowed to mess with whitespace there; TagString is the
    untouched RR identifier we stored at populate time. }
  for I := 0 to lstYAxis.Count - 1 do
  begin
    Item := lstYAxis.ListItems[I];
    if Item is TListBoxGroupHeader then Continue;

    Id  := Item.TagString;
    Idx := FSelectedYNames.IndexOf(Id);
    if Item.IsChecked then
    begin
      if Idx < 0 then FSelectedYNames.Add(Id);
    end
    else
    begin
      if Idx >= 0 then FSelectedYNames.Delete(Idx);
    end;
  end;
end;

procedure TFrameTimeCourse.DoYListCheckChanged(Sender: TObject);
begin
  if FSuppressPlotUpdate then Exit;
  SyncSelectionFromVisible;
  DoPlotSelectionChanged(Sender);
end;

procedure TFrameTimeCourse.SetAllVisibleChecked(AChecked: Boolean);
var
  I:    Integer;
  Item: TListBoxItem;
begin
  { The preview rows are disabled, but Enabled only stops the user clicking
    them — this writes IsChecked directly. Without the guard, Deselect All
    would visibly clear the file's requested names while SyncSelectionFromVisible
    (which correctly ignores the preview) left them selected, so Simulate would
    plot what the list said was unchecked. }
  if FShowingUnverified then Exit;

  FSuppressPlotUpdate := True;
  try
    lstYAxis.BeginUpdate;
    try
      for I := 0 to lstYAxis.Count - 1 do
      begin
        Item := lstYAxis.ListItems[I];
        if Item is TListBoxGroupHeader then Continue;
        Item.IsChecked := AChecked;
      end;
    finally
      lstYAxis.EndUpdate;
    end;
    SyncSelectionFromVisible;
  finally
    FSuppressPlotUpdate := False;
  end;

  { Apply the bulk change to the plot once, instead of N times. }
  DoPlotSelectionChanged(lstYAxis);
end;

{ ── plot / event glue ──────────────────────────────────────────────────── }

procedure TFrameTimeCourse.DoPlotSelectionChanged(Sender: TObject);
begin
  if FSuppressPlotUpdate then Exit;
  if (FContext = nil) or (not FHasData) then Exit;

  { If the user added an observable that wasn't in the last simulation's
    selection list, the cached matrix has no column for it. Re-run rather
    than silently dropping it from the plot. Otherwise serve from cache. }
  if SelectionNeedsRecompute then
    RunSimulation
  else
  begin
    FContext.PlotBeginRebuild;
    FContext.PlotData(FLastData,
                      GetSelectedXAxisName,
                      GetSelectedYAxisNames);
    FContext.PlotEndRebuild;
    ApplyPlotMetadata;
  end;
end;

function TFrameTimeCourse.SelectionNeedsRecompute: Boolean;
var
  I:    Integer;
  Name: string;
begin
  { Cheap pre-checks. }
  Result := True;
  if (not FHasData) or (FLastData = nil) then Exit;

  { X column must be present. }
  Name := GetSelectedXAxisName;
  if FLastData.columnHeader.IndexOf(Name) < 0 then Exit;

  { Every Y column must be present. }
  for I := 0 to FSelectedYNames.Count - 1 do
    if FLastData.columnHeader.IndexOf(FSelectedYNames[I]) < 0 then Exit;

  Result := False;
end;

procedure TFrameTimeCourse.chkReplaceValuesChange(Sender: TObject);
begin
  { No action needed here: btnCopySliderValuesToModelClick reads
    chkReplaceValues.IsChecked at click time. Off (default) = append a new
    block; on = replace the previous block. }
end;

procedure TFrameTimeCourse.cmbFilterChange(Sender: TObject);
begin
  if FContext = nil then Exit;
  { Filter changes the view, not the selection set. RepopulateYList
    re-renders from FSelectedYNames so hidden checks aren't lost, and
    the plot doesn't need updating. }
  RepopulateYList;
end;

function TFrameTimeCourse.GetSelectedXAxisName: string;
begin
  if (cbXAxis.ItemIndex < 0) or (cbXAxis.Selected = nil) then
    Result := TIME_COLUMN_LABEL
  else
    Result := cbXAxis.Items[cbXAxis.ItemIndex];
end;

function TFrameTimeCourse.GetSelectedYAxisNames: TArray<string>;
var
  I: Integer;
begin
  { Read from the canonical set, NOT from the visible listbox — entries
    hidden by the filter still count as selected. }
  SetLength(Result, FSelectedYNames.Count);
  for I := 0 to FSelectedYNames.Count - 1 do
    Result[I] := FSelectedYNames[I];
end;

procedure TFrameTimeCourse.btnSetTimeCourseSelectionClick(Sender: TObject);
begin
  SetAllVisibleChecked(True);
end;

procedure TFrameTimeCourse.btnUnSelectAllClick(Sender: TObject);
begin
  SetAllVisibleChecked(False);
end;

{ ── simulate ───────────────────────────────────────────────────────────── }

function TFrameTimeCourse.RunSimulation: Boolean;
var
  TimeStart, TimeEnd: Double;
  NPoints:   Integer;
  Data:      T2DMatrix;
  XName:     string;
  YNames:    TArray<string>;
  Selection: TStringList;
  I:         Integer;
  Name:      string;
begin
  try
    Result := False;
    if FContext = nil then Exit;

    try
      if not FContext.Session.EnsureLoaded then
      begin
        ShowMessage('Cannot load model: ' + FContext.Session.LastError);
        Exit;
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Model load failed: ' + E.Message);
        Exit;
      end;
    end;

    { Resolve axis choices early so we can bail before any RR work if
      there's nothing to plot. }
    XName  := GetSelectedXAxisName;
    YNames := GetSelectedYAxisNames;

    if Length(YNames) = 0 then
    begin
      ShowMessage('Select at least one variable for the Y axis.');
      Exit;
    end;

    TimeStart := edtTimeStart.Value;
    TimeEnd   := edtTimeEnd.Value;
    NPoints   := Trunc(edtNumberofPoints.Value);

    FContext.Session.RoadRunner.SetTimeStart(TimeStart);
    FContext.Session.RoadRunner.SetTimeEnd(TimeEnd);
    FContext.Session.RoadRunner.SetNumberOfPoints(NPoints);

    { Tell RoadRunner which columns to compute and return. Without this
      it defaults to time + floating species, silently dropping every
      other category (boundary, fluxes, rates, globals, compartments)
      from the result matrix. Order: time first, then X (if not time),
      then every Y. Deduplicated because X is often also a Y. }
    Selection := TStringList.Create;
    try
      Selection.CaseSensitive := True;  { SBML IDs are case-sensitive }
      Selection.Add(TIME_COLUMN_LABEL);

      if not SameText(XName, TIME_COLUMN_LABEL) and
         (Selection.IndexOf(XName) < 0) then
        Selection.Add(XName);

      for I := 0 to High(YNames) do
      begin
        Name := YNames[I];
        if Selection.IndexOf(Name) < 0 then
          Selection.Add(Name);
      end;

      FContext.Session.RoadRunner.setTimeCourseSelectionListEx(Selection);
    finally
      Selection.Free;
    end;

    if chkAlwaysReset.IsChecked then
      FContext.Session.RoadRunner.reset();

    Data := FContext.Session.RoadRunner.Simulate;

    { Cache for live updates triggered by subsequent selection changes. }
    FLastData := Data;
    FHasData  := True;

    FContext.PlotBeginRebuild;
    FContext.PlotData(Data, XName, YNames);
    FContext.PlotEndRebuild;
    { After the bracket, not inside it: PlotEndRebuild re-applies this
      panel's saved styling, which would otherwise undo everything the
      @plot command just asked for. Applying last also gives the intended
      precedence — the file's styling wins on each compute, and the user's
      later edits in the plot editor stand until the next one (the next
      PlotBeginRebuild snapshots them). }
    ApplyPlotMetadata;
    if FContext <> nil then FContext.OutputStateChanged;
    Result := True;
  except
    on E: Exception do
      ShowMessage('Error in Run Simulation: ' + E.Message);
  end;
end;

procedure TFrameTimeCourse.btnSimulateClick(Sender: TObject);
begin
  RunSimulation;
end;

procedure TFrameTimeCourse.btnSimulateMouseLeave(Sender: TObject);
begin
  TButton(Sender).Enabled := False;
  TButton(Sender).Enabled := True;
end;

procedure TFrameTimeCourse.btnConfigIntegratorClick(Sender: TObject);
begin
  frmConfigCVODE := TfrmConfigCVODE.Create(nil);
  try
    frmConfigCVODE.SetContext(FContext);
    frmConfigCVODE.ShowModal;
  finally
    frmConfigCVODE.Free;
  end;
end;

procedure TFrameTimeCourse.btnCopySliderValuesToModelClick(Sender: TObject);
const
  BLOCK_TAG = '// [SliderValues]';
var
  Names:  TArray<string>;
  Values: TArray<Double>;
  I:      Integer;
  Block:  string;
begin
  FContext.SliderContainer.GetSliderValues(Names, Values);
  if Length(Names) = 0 then Exit;

  Block := sLineBreak + BLOCK_TAG + ' ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + sLineBreak;
  for I := 0 to High(Names) do
    Block := Block + Names[I] + ' = ' + FloatToStr(Values[I]) + sLineBreak;

  { chkReplaceValues off (default) appends a new block each click; on replaces
    the previously copied block with this one. }
  FContext.AppendToAntimonySource(Block, chkReplaceValues.IsChecked);
end;


procedure TFrameTimeCourse.btnResetClick(Sender: TObject);
begin
  if (FContext <> nil) and FContext.Session.IsLoaded then
    FContext.Session.RoadRunner.Reset;
end;

{ ── sliders ────────────────────────────────────────────────────────────── }

procedure TFrameTimeCourse.btnSliders1Click(Sender: TObject);
var
  Names:  TArray<string>;
  Values: TArray<Double>;
begin
  if FContext = nil then Exit;

  try
    if not FContext.Session.EnsureLoaded then
    begin
      ShowMessage('Cannot load model: ' + FContext.Session.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Model load failed: ' + E.Message);
      Exit;
    end;
  end;

  if not FContext.SliderContainer.ParamPanelVisible then
    begin
    Names  := FContext.Session.GetTunableNames;
    Values := FContext.Session.GetTunableValues;
    FContext.SliderContainer.LoadParams(Names, Values);
    end;

  FContext.SliderContainer.ToggleParamPanel;
end;

procedure TFrameTimeCourse.btnTimeCourseSlidersClick(Sender: TObject);
var
  Names:  TArray<string>;
  Values: TArray<Double>;
begin
  if FContext = nil then Exit;

  try
    if not FContext.Session.EnsureLoaded then
    begin
      ShowMessage('Cannot load model: ' + FContext.Session.LastError);
      Exit;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Model load failed: ' + E.Message);
      Exit;
    end;
  end;

  if not FContext.SliderContainer.ParamPanelVisible then
    begin
    Names  := FContext.Session.GetTunableNames;
    Values := FContext.Session.GetTunableValues;
    FContext.SliderContainer.LoadParams(Names, Values);
    end;

  FContext.SliderContainer.ToggleParamPanel;
end;

procedure TFrameTimeCourse.OnSliderChanged(Sender: TObject;
  const ASliderString: string; const AValue: Single);
begin
  if FContext = nil then Exit;

  FContext.Session.SetParameterValue(ASliderString, AValue);
  RunSimulation;
  if FContext.Session.IsLoaded then
    FContext.Session.RoadRunner.Reset;
end;


function TFrameTimeCourse.GetPythonScript(const AntimonyText: string): string;
const
  IND = '    ';
var
  SB:         TStringBuilder;
  Fmt:        TFormatSettings;
  XName:      string;
  YNames:     TArray<string>;
  Selection:  TArray<string>;
  SeriesInfo: TArray<TPlotSeriesColorInfo>;
  I:          Integer;

  function FStr(V: Double): string;
  begin
    Result := FloatToStr(V, Fmt);
  end;

  function ColorToPyHex(C: TAlphaColor): string;
  begin
    Result := Format('''#%.2x%.2x%.2x''',
      [TAlphaColorRec(C).R, TAlphaColorRec(C).G, TAlphaColorRec(C).B]);
  end;

  { Emit an SBML/RR identifier as a Python string literal. Double quotes
    because rate-of-change IDs end with an apostrophe (e.g. S1') which
    would terminate a single-quoted literal. SBML IDs never contain
    double quotes, so this is always safe. }
  function PyStr(const S: string): string;
  begin
    Result := '"' + S + '"';
  end;

  procedure AddToSelection(const AName: string);
  var
    J:     Integer;
    Found: Boolean;
  begin
    Found := False;
    for J := 0 to High(Selection) do
      if SameText(Selection[J], AName) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      SetLength(Selection, Length(Selection) + 1);
      Selection[High(Selection)] := AName;
    end;
  end;

begin
  Fmt := TFormatSettings.Invariant;

  if FContext = nil then
    Exit('# Iridium: no analysis context.');
  if not FContext.Session.IsLoaded then
    Exit('# Iridium: no model loaded.');

  YNames := GetSelectedYAxisNames;
  if Length(YNames) = 0 then
    Exit('# Iridium: no Y-axis observables selected.');

  XName := GetSelectedXAxisName;

  { Build deduplicated selection: time first, then X (if not time), then Ys.
    Tellurium needs every referenced column present in selection exactly once. }
  SetLength(Selection, 1);
  Selection[0] := 'time';
  if not SameText(XName, 'time') then
    AddToSelection(XName);
  for I := 0 to High(YNames) do
    AddToSelection(YNames[I]);

  SeriesInfo := FContext.PlotGetSimulationSeriesInfo;

  SB := TStringBuilder.Create;
  try
    SB.AppendLine('# Python script generated by Iridium time course.');
    SB.AppendLine('# Reproduces the simulation using Tellurium.');
    SB.AppendLine;
    SB.AppendLine('import tellurium as te');
    SB.AppendLine('import matplotlib.pyplot as plt');
    SB.AppendLine;

    { ── Model ──────────────────────────────────────────────────────────── }
    SB.AppendLine('# ── Model ─────────────────────────────────────────────');
    SB.AppendLine('r = te.loada(r"""');
    SB.AppendLine(AntimonyText.TrimRight);
    SB.AppendLine('""")');
    SB.AppendLine;

    { ── Time settings ─────────────────────────────────────────────────── }
    SB.AppendLine('# ── Time course settings ──────────────────────────────');
    SB.AppendLine('time_start = ' + FStr(edtTimeStart.Value));
    SB.AppendLine('time_end   = ' + FStr(edtTimeEnd.Value));
    SB.AppendLine('num_points = ' + IntToStr(Round(edtNumberofPoints.Value)));
    SB.AppendLine;

    { ── Selection ────────────────────────────────────────────────────── }
    SB.AppendLine('# ── Selection (time + X axis + Y observables) ────────');
    SB.Append('selection = [');
    for I := 0 to High(Selection) do
    begin
      if I > 0 then SB.Append(', ');
      SB.Append(PyStr(Selection[I]));
    end;
    SB.AppendLine(']');
    SB.AppendLine;

    { ── Simulate ──────────────────────────────────────────────────────── }
    SB.AppendLine('# ── Simulate ──────────────────────────────────────────');
    SB.AppendLine('r.reset()');
    SB.AppendLine('m = r.simulate(time_start, time_end, num_points, selection)');
    SB.AppendLine;

    { ── X column ──────────────────────────────────────────────────────── }
    SB.AppendLine('# X-axis column (may be time, or a species for phase plots).');
    SB.AppendLine('x_name = ' + PyStr(XName));
    SB.AppendLine('x_col  = m[:, selection.index(x_name)]');
    SB.AppendLine;

    { ── Colors ────────────────────────────────────────────────────────── }
    SB.AppendLine('# ── Colors (read from on-screen plot) ─────────────────');
    SB.AppendLine('# Match what the Iridium time course renders. Edit to restyle.');
    if Length(SeriesInfo) = 0 then
      SB.AppendLine('colors = None  # no prior simulation; matplotlib defaults')
    else
    begin
      SB.AppendLine('colors = {');
      for I := 0 to High(SeriesInfo) do
      begin
        SB.Append(IND);
        SB.Append(PyStr(SeriesInfo[I].Name)).Append(': ');
        SB.Append(ColorToPyHex(SeriesInfo[I].LineColor));
        if I < High(SeriesInfo) then SB.Append(',');
        SB.AppendLine;
      end;
      SB.AppendLine('}');
    end;
    SB.AppendLine;

    { ── Plot ──────────────────────────────────────────────────────────── }
    SB.AppendLine('# ── Plot ──────────────────────────────────────────────');
    SB.Append('y_names = [');
    for I := 0 to High(YNames) do
    begin
      if I > 0 then SB.Append(', ');
      SB.Append(PyStr(YNames[I]));
    end;
    SB.AppendLine(']');
    SB.AppendLine;
    SB.AppendLine('for y in y_names:');
    SB.AppendLine(IND + 'c = colors[y] if colors else None');
    SB.AppendLine(IND + 'plt.plot(x_col, m[:, selection.index(y)], color=c, label=y)');
    SB.AppendLine;
    SB.AppendLine('plt.xlabel(x_name)');
    if FContext.PlotGetPlotInfo.LegendVisible then
       SB.AppendLine('plt.legend()');
    SB.AppendLine('plt.show()');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;


end.
