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
  System.RegularExpressions,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  FMX.Objects, FMX.Layouts, FMX.ListBox,
  uRR2DSimpleMatrix,
  uAnalysisTypes;

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

  TFrameTimeCourse = class(TFrame, IPythonScriptExporter)
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
    procedure btnSimulateClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSliders1Click(Sender: TObject);
    procedure btnSetTimeCourseSelectionClick(Sender: TObject);
    procedure btnUnSelectAllClick(Sender: TObject);
    procedure btnTimeCourseSlidersClick(Sender: TObject);
    procedure btnCopySliderValuesToModelClick(Sender: TObject);
    procedure btnSimulateMouseLeave(Sender: TObject);
    procedure cmbFilterChange(Sender: TObject);
  private
    FContext:            IAnalysisContext;
    FLastData:           T2DMatrix;
    FHasData:            Boolean;
    FSuppressPlotUpdate: Boolean;

    { Canonical Y-axis selection — survives filter changes. Sorted, case-
      sensitive, no duplicates. The visible listbox is a view onto this. }
    FSelectedYNames:     TStringList;

    { Per-category ID caches, refreshed from RoadRunner on every reload. }
    FCategoryIds:        array[TObservableCategory] of TStringList;

    { True until the first successful population — used to seed defaults
      (all floating species checked) on the very first model load. }
    FFirstPopulation:    Boolean;

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
    function  GetSelectedYAxisNames: TArray<string>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure SetContext(const AContext: IAnalysisContext);
    procedure SetSimulationParameters(ATimeEnd: Double; ANumPoints: Integer);
    procedure AttachToSliders;
  end;

implementation

{$R *.fmx}

uses
  uRoadRunner, uRRList, uAntimonyAPI, uCommonTypes;

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
end;

destructor TFrameTimeCourse.Destroy;
var
  Cat: TObservableCategory;
begin
  FSelectedYNames.Free;
  for Cat := Low(TObservableCategory) to High(TObservableCategory) do
    FCategoryIds[Cat].Free;
  inherited;
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
end;

{ ── session callbacks ──────────────────────────────────────────────────── }

procedure TFrameTimeCourse.SessionStateChanged(Sender: TObject);
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
begin
  Visible := CurrentFilterCategories;

  FSuppressPlotUpdate := True;
  try
    lstYAxis.BeginUpdate;
    try
      lstYAxis.Clear;
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
    FContext.PlotData(FLastData,
                      GetSelectedXAxisName,
                      GetSelectedYAxisNames);
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

    FContext.PlotData(Data, XName, YNames);
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

  FContext.AppendToAntimonySource(Block);
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
    Names  := FContext.Session.GetParameterNames;
    Values := FContext.Session.GetParameterValues;
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
    Names  := FContext.Session.GetParameterNames;
    Values := FContext.Session.GetParameterValues;
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
