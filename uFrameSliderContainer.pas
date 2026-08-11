    unit uFrameSliderContainer;

{ Slider container frame with a parameter-picker panel on the left.
  Left panel:  TEdit (EditFilter) + TListBox (ListBoxParams).
  Right panel: TVertScrollBox (VertScrollBox1) containing one row per
               active slider.

  Workflow
  --------
  1. Host calls LoadParams(names, values) once after a model is loaded.
     This populates the listbox and stores the initial values; no sliders
     are created yet.
  2. User types in EditFilter to narrow the list, then clicks a name to
     add that slider.  The parameter is NOT removed from the listbox so
     the full catalogue is always visible; a second click on an already-
     active parameter is silently ignored.
  3. Each slider row has a small "x" button on its left edge.  Clicking it
     removes just that row.
  4. Right-clicking the parameter label still opens the range-edit dialog.
  5. The host may still call BuildSliders / ClearSliders / RefreshValues
     as before.  BuildSliders now also reloads the listbox from the names
     supplied, replacing any previous catalogue.

  Continuous vs release-only updates
  ----------------------------------
  The OnSliderChanged event fires while the user moves a slider. By
  default this is continuous (every thumb position change), which suits
  time course and parameter scan frames. Setting ReleaseOnlyMode := True
  switches new and existing rows to fire OnSliderChanged only once, on
  release -- the underlying mechanism is FMX TTrackBar.Tracking. The
  steady state frame uses this mode because steady-state solves are less
  robust than time-course integration and we don't want to hammer them
  continuously. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.ListBox, FMX.Objects, System.Generics.Defaults;

type
  TSliderChangedEvent = procedure(Sender: TObject; const ASliderString: string;
                                  const AValue: Single) of object;

  TSliderRow = record
    Layout:    TLayout;
    RemoveBtn: TButton;
    Resetbtn:  TButton;
    Lbl:       TLabel;
    Track:     TTrackBar;
    ParamName: string;
  end;

  TScrollBoxCracker = class(TVertScrollBox);

  TFrameSliderContainer = class(TFrame)
    VertScrollBox1: TVertScrollBox;
    LayoutLeft:     TLayout;
    Splitter1:      TSplitter;
    EditFilter:     TEdit;
    ListBoxParams:  TListBox;
    Layout1: TLayout;
    btnResetAllParameters: TButton;
    btnAddAllParameters: TButton;
    chkPosition: TCheckBox;
    btnDeleteAll: TButton;
    procedure btnDeleteAllClick(Sender: TObject);
    procedure chkPositionChange(Sender: TObject);
  private
    FRows:            TArray<TSliderRow>;
    FOwned:           TObjectList<TComponent>;
    FOnSliderChanged: TSliderChangedEvent;
    FReleaseOnlyMode: Boolean;

    { Full catalogue: parallel arrays kept in sync }
    { Full catalogue: parallel arrays kept in sync. FAllParamNames[i] and
     FAllParamValues[i] MUST refer to the same parameter — any mutation
     that reorders one must reorder the other identically. }
    FAllParamNames:  TArray<string>;
    FAllParamValues: TArray<Double>;

      { Optional "locked" parameter -- shown in the listbox but greyed and
      not clickable, and if it has an active slider that slider's track
      is disabled. Used by the parameter-scan frame to lock the
      currently-scanning parameter. Empty string = no lock. }
    FLockedParam:    string;

    { -- internal helpers -- }
    procedure RebuildListBox(const AFilter: string);
    { The x10 / /10 range a value gets, so a row built now and a row
      re-centred by RefreshValues agree. }
    procedure RangeAround(const AValue: Double; out AMin, AMax: Single);

    { AAtTop puts the new row above the existing ones and scrolls it into
      view — for rows the user just picked, which they mean to use straight
      away. Bulk builds leave it False so the rows keep model order. }
    procedure AddSliderRow(const AName: string; const AInitValue: Double;
                           AAtTop: Boolean = False);
    function  SeedRowY(AAtTop: Boolean): Single;
    procedure RemoveSliderRow(const AParamName: string);
    function  RowIndexOf(const AParamName: string): Integer;
    function  InitialValueOf(const AName: string): Double;
    function  IsActive(const AName: string): Boolean;

    function  GetParamPanelVisible: Boolean;
    procedure SetReleaseOnlyMode(Value: Boolean);

    { -- widget callbacks -- }
    procedure DoTrackBarChange(Sender: TObject);
    procedure DoLabelMouseDown(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Single);
    procedure DoRemoveBtnClick(Sender: TObject);
    procedure DoListBoxItemClick(const Sender: TCustomListBox;
                                 const Item: TListBoxItem);
    procedure DoFilterChange(Sender: TObject);
    procedure DoBtnAddAllClick(Sender: TObject);
    procedure DoBtnResetClick(Sender: TObject);

    procedure DoResetSingleSlider (Sender : TObject);

    function  FormatLabelText(const AName: string;
                              const AValue: Single): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { -- primary API -- }

    { Load (or replace) the full parameter catalogue. Populates the
      listbox; does NOT create any sliders -- the user picks them. }
    procedure LoadParams(const AParamNames:  TArray<string>;
                         const AInitValues:  TArray<Double>);

    { Legacy bulk-build: creates one slider for every parameter, just as
      before. Also refreshes the listbox catalogue. }
    procedure BuildSliders(const AParamNames:   TArray<string>;
                           const AInitialValues: TArray<Double>;
                           const AMaxValue:      Single = 1.0);

    procedure ClearSliders;

    procedure ToggleParamPanel;

    { Update slider thumb positions without firing OnSliderChanged. }
    procedure RefreshValues(const ANames:  TArray<string>;
                            const AValues: TArray<Double>);

    procedure GetSliderValues(out ANames: TArray<string>; out AValues: TArray<Double>);

    { Lock (disable) the slider/listbox entry for a single parameter, and
      simultaneously release any previous lock. Pass '' to clear.
      Safe to call whether the parameter has an active slider row yet
      or not -- if it doesn't, the lock simply takes effect in the
      listbox (greyed, non-clickable) and on any future row created for
      that name. }
    procedure SetLockedParam(const AParamName: string);

    property OnSliderChanged: TSliderChangedEvent read  FOnSliderChanged
                                                  write FOnSliderChanged;

    { When False (default): OnSliderChanged fires continuously while the
      user drags a slider. When True: OnSliderChanged fires only once,
      when the user releases. Setting this property updates both newly-
      created rows and any existing ones. }
    property ReleaseOnlyMode: Boolean read FReleaseOnlyMode
                                      write SetReleaseOnlyMode;

    property ParamPanelVisible: Boolean read GetParamPanelVisible;
    property LockedParam: string read FLockedParam;
  end;

implementation

{$R *.fmx}

const
  ROW_H   = 44;
  BTN_W   = 28;   { width of the left-side button column }
  BTN_H   = 18;   { visual height of the button itself   }

{ Min, Max, Frequency and Value always move together — that is the whole
  point of this routine, and assigning any of them on its own elsewhere is a
  bug.

  ORDER MATTERS. Frequency is the step a TTrackBar snaps its value to, and
  the snap is applied when Value is assigned — so Value must be written
  LAST, after the grid it will be quantised against is in place. Written
  before, it is rounded onto the PREVIOUS range's grid: re-centring a row
  from 0.035..3.5 onto 1..100 and then setting 10 produced 9.9, and coming
  back produced 0.5024 instead of 0.35.

  198 steps, not 200, so that the value a row is centred on falls exactly on
  a step. RangeAround gives Min = v/10 and Max = 10v, a span of 9.9v; over
  198 steps that is 0.05v each, and v itself is exactly 18 steps above Min.
  With 200 it is 18.18 steps — off-grid, so the track would round the very
  value it was being centred on. The zero and negative cases work out
  exactly too. }
procedure SetTrackRange(ATrack: TTrackBar; AMin, AMax, AValue: Single);
const
  STEPS_ACROSS_RANGE = 198;
begin
  ATrack.BeginUpdate;
  try
    ATrack.Min       := AMin;
    ATrack.Max       := AMax;
    ATrack.Frequency := (AMax - AMin) / STEPS_ACROSS_RANGE;
    ATrack.Value     := EnsureRange(AValue, AMin, AMax);
  finally
    ATrack.EndUpdate;
  end;
end;

{ -- modal range-edit dialog --------------------------------------------- }

function PromptForRange(const ATitle: string;
                        var AMin, AMax: Single): Boolean;
var
  F:            TForm;
  LblMin:       TLabel;
  LblMax:       TLabel;
  EdMin:        TEdit;
  EdMax:        TEdit;
  BtnOK:        TButton;
  BtnCancel:    TButton;
  V1, V2:       Single;
begin
  Result := False;
  F := TForm.CreateNew(nil);
  try
    F.Caption      := ATitle;
    F.BorderStyle  := TFmxFormBorderStyle.None;
    F.BorderIcons  := [TBorderIcon.biSystemMenu];
    F.Position     := TFormPosition.MainFormCenter;
    F.ClientWidth  := 260;
    F.ClientHeight := 90;

    LblMin             := TLabel.Create(F);
    LblMin.Parent      := F;
    LblMin.Position.X  := 16;
    LblMin.Position.Y  := 16;
    LblMin.Width       := 40;
    LblMin.Height      := 18;
    LblMin.Text        := 'Min:';

    EdMin             := TEdit.Create(F);
    EdMin.Parent      := F;
    EdMin.Position.X  := 60;
    EdMin.Position.Y  := 12;
    EdMin.Width       := 180;
    EdMin.Height      := 26;
    EdMin.Text        := FloatToStr(AMin);

    LblMax             := TLabel.Create(F);
    LblMax.Parent      := F;
    LblMax.Position.X  := 16;
    LblMax.Position.Y  := 50;
    LblMax.Width       := 40;
    LblMax.Height      := 18;
    LblMax.Text        := 'Max:';

    EdMax             := TEdit.Create(F);
    EdMax.Parent      := F;
    EdMax.Position.X  := 60;
    EdMax.Position.Y  := 46;
    EdMax.Width       := 180;
    EdMax.Height      := 26;
    EdMax.Text        := FloatToStr(AMax);

    BtnCancel             := TButton.Create(F);
    BtnCancel.Parent      := F;
    BtnCancel.Position.X  := 80;
    BtnCancel.Position.Y  := 88;
    BtnCancel.Width       := 75;
    BtnCancel.Height      := 26;
    BtnCancel.Text        := 'Cancel';
    BtnCancel.ModalResult := mrCancel;
    BtnCancel.Cancel      := True;

    BtnOK             := TButton.Create(F);
    BtnOK.Parent      := F;
    BtnOK.Position.X  := 165;
    BtnOK.Position.Y  := 88;
    BtnOK.Width       := 75;
    BtnOK.Height      := 26;
    BtnOK.Text        := 'OK';
    BtnOK.ModalResult := mrOk;
    BtnOK.Default     := True;

    if F.ShowModal <> mrOk then Exit;

    if not TryStrToFloat(EdMin.Text, V1) or
       not TryStrToFloat(EdMax.Text, V2) then
    begin
      ShowMessage('Please enter valid numbers for Min and Max.');
      Exit;
    end;

    if V1 >= V2 then
    begin
      ShowMessage('Min must be less than Max.');
      Exit;
    end;

    AMin   := V1;
    AMax   := V2;
    Result := True;
  finally
    F.Free;
  end;
end;

function TFrameSliderContainer.GetParamPanelVisible: Boolean;
begin
  Result := (ControlsCount > 0) and Controls[0].Visible;
end;

procedure TFrameSliderContainer.ToggleParamPanel;
begin
  for var i := 0 to Self.ControlsCount - 1 do
  begin
    Self.Controls[i].Visible := not Self.Controls[i].Visible;
  end;
end;

procedure TFrameSliderContainer.SetReleaseOnlyMode(Value: Boolean);
var
  I: Integer;
begin
  if FReleaseOnlyMode = Value then Exit;
  FReleaseOnlyMode := Value;
  { Reconfigure any existing rows so the mode applies consistently. }
  for I := 0 to High(FRows) do
    if FRows[I].Track <> nil then
      FRows[I].Track.Tracking := not Value;
end;

{ -- TFrameSliderContainer ----------------------------------------------- }

constructor TFrameSliderContainer.Create(AOwner: TComponent);
begin
  inherited;
  { Panel starts hidden }
  for var i := 0 to ControlsCount - 1 do
    Controls[i].Visible := False;

  FOwned := TObjectList<TComponent>.Create(True);

  EditFilter.TextPrompt  := 'Filter parameters...';
  EditFilter.OnChangeTracking := DoFilterChange;

  ListBoxParams.OnItemClick := DoListBoxItemClick;

  btnAddAllParameters.OnClick := DoBtnAddAllClick;
  btnResetAllParameters.OnClick  := DoBtnResetClick;

  VertScrollBox1.ShowScrollBars := True;
  VertScrollBox1.AniCalculations.AutoShowing := False;

  if TScrollBoxCracker(VertScrollBox1).VScrollBar <> nil then
    begin
    TScrollBoxCracker(VertScrollBox1).AutoHide := False;
    TScrollBoxCracker(VertScrollBox1).VScrollBar.Visible := True;
    end;
end;

destructor TFrameSliderContainer.Destroy;
begin
  FOwned.Free;
  inherited;
end;

{ -- formatting ---------------------------------------------------------- }

function TFrameSliderContainer.FormatLabelText(const AName: string;
  const AValue: Single): string;
begin
  Result := Format('%s = %.4g', [AName, AValue]);
end;

{ -- catalogue helpers --------------------------------------------------- }

function TFrameSliderContainer.InitialValueOf(const AName: string): Double;
var
  I: Integer;
begin
  Result := 0.0;
  for I := 0 to High(FAllParamNames) do
    if FAllParamNames[I] = AName then
    begin
      if I < Length(FAllParamValues) then
        Result := FAllParamValues[I];
      Exit;
    end;
end;

function TFrameSliderContainer.IsActive(const AName: string): Boolean;
begin
  Result := RowIndexOf(AName) >= 0;
end;

function TFrameSliderContainer.RowIndexOf(const AParamName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FRows) do
    if FRows[I].ParamName = AParamName then
    begin
      Result := I;
      Exit;
    end;
end;

{ -- listbox population -------------------------------------------------- }

procedure TFrameSliderContainer.RebuildListBox(const AFilter: string);
var
  I:    Integer;
  Name: string;
  Lo:   string;
  Item: TListBoxItem;
begin
  Lo := AFilter.ToLower;

  ListBoxParams.BeginUpdate;
  try
    ListBoxParams.Clear;
    for I := 0 to High(FAllParamNames) do
    begin
      Name := FAllParamNames[I];
      if (Lo <> '') and (Pos(Lo, Name.ToLower) = 0) then
        Continue;

      Item        := TListBoxItem.Create(ListBoxParams);
      Item.Text   := Name;
      Item.Parent := ListBoxParams;

      { Grey out names that already have an active slider, or that are
        the currently-locked parameter. }
      if IsActive(Name) or
         ((FLockedParam <> '') and SameText(Name, FLockedParam)) then
        Item.TextSettings.FontColor := TAlphaColorRec.Gray
      else
        Item.TextSettings.FontColor := TAlphaColorRec.Null;
    end;
  finally
    ListBoxParams.EndUpdate;
  end;
end;

{ -- slider row construction --------------------------------------------- }

{ Y coordinate that will sort a brand-new row above (AAtTop) or below every
  existing row when the scrollbox realigns its top-aligned children. Only the
  ordering matters — the realign overwrites the value with the real position.

  During a bulk build the rows sit inside BeginUpdate/EndUpdate and have not
  been arranged yet, so their Y values are the seeds handed out here; walking
  the live layouts (rather than assuming a fixed row pitch) keeps the sequence
  strictly increasing in that case too. }
function TFrameSliderContainer.SeedRowY(AAtTop: Boolean): Single;
var
  I: Integer;
begin
  if Length(FRows) = 0 then
    Exit(0);

  Result := FRows[0].Layout.Position.Y;
  if AAtTop then
  begin
    for I := 1 to High(FRows) do
      Result := Min(Result, FRows[I].Layout.Position.Y);
    Result := Result - 1;
  end
  else
  begin
    Result := Result + FRows[0].Layout.Height;
    for I := 1 to High(FRows) do
      Result := Max(Result,
                    FRows[I].Layout.Position.Y + FRows[I].Layout.Height);
    Result := Result + 1;
  end;
end;

{ The range a slider gets for a value: x10 / /10 around it, so the handle
  sits in the middle and there is a decade of travel either way. Zero has no
  decade, so it falls back to [0, 1]. Shared by row creation and by
  RefreshValues, which re-centres — the two must not drift apart, or a
  refreshed row would sit differently from a freshly built one. }
procedure TFrameSliderContainer.RangeAround(const AValue: Double;
  out AMin, AMax: Single);
begin
  if AValue = 0.0 then
  begin
    AMin := 0.0;
    AMax := 1.0;
  end
  else if AValue > 0.0 then
  begin
    AMin := AValue / 10.0;
    AMax := AValue * 10.0;
  end
  else
  begin
    AMin := AValue * 10.0;
    AMax := AValue / 10.0;
  end;
end;

procedure TFrameSliderContainer.AddSliderRow(const AName: string;
  const AInitValue: Double; AAtTop: Boolean);
var
  Row:      TSliderRow;
  RangeMax: Single;
  RangeMin: Single;
const
  ROW_H   = 44;
  BTN_W   = 24;
begin
  if IsActive(AName) then Exit;

  RangeAround(AInitValue, RangeMin, RangeMax);

  Row.ParamName := AName;

  { -- outer layout --
    Y is seeded before the layout is parented: parenting is what triggers the
    scrollbox's realign, and that realign is what reads Y to decide the row's
    place in the stack (see the note further down). }
  Row.Layout                := TLayout.Create(Self);
  Row.Layout.Position.Y     := SeedRowY(AAtTop);
  Row.Layout.Parent         := VertScrollBox1;
  Row.Layout.Align          := TAlignLayout.Top;
  Row.Layout.Height         := ROW_H;
  Row.Layout.Margins.Top    := 4;
  Row.Layout.Margins.Left   := 4;
  Row.Layout.Margins.Right  := 8;

  var BtnLayout := TLayout.Create(Self);
  BtnLayout.Parent        := Row.Layout;
  BtnLayout.Align         := TAlignLayout.Left;
  BtnLayout.Width         := BTN_W*2;
  BtnLayout.Margins.Right := 4;

  Row.RemoveBtn            := TButton.Create(Self);
  Row.RemoveBtn.Parent     := BtnLayout;
  Row.RemoveBtn.Align      := TAlignLayout.None;
  Row.RemoveBtn.Width      := BTN_W - 4;
  Row.RemoveBtn.Height     := BTN_H;
  Row.RemoveBtn.Position.X := 0;
  Row.RemoveBtn.Position.Y := (Row.Layout.Height - BTN_H) - 27;
  Row.RemoveBtn.Text       := 'X';
  Row.RemoveBtn.TagString  := AName;
  Row.RemoveBtn.Hint       := 'Delete slider';
  Row.RemoveBtn.OnClick    := DoRemoveBtnClick;

  Row.Resetbtn             := TButton.Create(Self);
  Row.ResetBtn.Parent     := BtnLayout;
  Row.ResetBtn.Align      := TAlignLayout.None;
  Row.ResetBtn.Width      := BTN_W - 4;
  Row.ResetBtn.Height     := BTN_H;
  Row.ResetBtn.Position.X := 26;
  Row.ResetBtn.Position.Y := (Row.Layout.Height - BTN_H) - 27;
  Row.ResetBtn.Text       := 'R';
  Row.ResetBtn.TagString  := AName;
  Row.ResetBtn.Hint       := 'Reset slider';
  Row.ResetBtn.OnClick    := DoResetSingleSlider;

  { -- inner layout for label + trackbar -- }
  Row.Lbl             := TLabel.Create(Self);
  Row.Lbl.Parent      := Row.Layout;
  Row.Lbl.Align       := TAlignLayout.Top;
  Row.Lbl.Height      := 20;
  Row.Lbl.HitTest     := True;
  Row.Lbl.Cursor      := crHandPoint;
  Row.Lbl.Hint        := 'Right-click to edit min/max';
  Row.Lbl.ShowHint    := True;
  Row.Lbl.TextSettings.HorzAlign := TTextAlign.Trailing;
  Row.Lbl.Text        := FormatLabelText(AName, AInitValue);
  Row.Lbl.TagString   := AName;
  Row.Lbl.OnMouseDown := DoLabelMouseDown;

  Row.Track             := TTrackBar.Create(Self);
  Row.Track.Parent      := Row.Layout;
  Row.Track.Align       := TAlignLayout.Client;
  Row.Track.Margins.Top := -8;
  SetTrackRange(Row.Track, RangeMin, RangeMax, AInitValue);
  Row.Track.Tracking    := not FReleaseOnlyMode;
  Row.Track.OnChange    := DoTrackBarChange;

  { cross-link label <-> trackbar for event handlers }
  Row.Track.TagObject := Row.Lbl;
  Row.Lbl.TagObject   := Row.Track;

  { FMX stacks top-aligned siblings by their *current* Position.Y, not by
    child index (AlignObjects/InsertBefore in FMX.Types sorts on Top and only
    falls back to child order for exact ties). A freshly created layout sits at
    Y = 0, which ties with the topmost existing row and lands the new slider
    second from the top — never at the bottom. So seed Y just outside the
    range the existing rows occupy (SeedRowY, above) and let the realign snap
    it into place. FRows keeps insertion order regardless — nothing depends on
    it matching the visual order. }
  if AAtTop then
  begin
    Row.Layout.Index := 0;   { tie-break, in case every row still sits at Y = 0 }
    VertScrollBox1.ViewportPosition := PointF(0, 0);
  end;

  FOwned.Add(Row.Layout);

  SetLength(FRows, Length(FRows) + 1);
  FRows[High(FRows)] := Row;

  { If this row corresponds to the currently-locked parameter, disable
    its trackbar and reset button so the user can see (and not change)
    the value. }
  if (FLockedParam <> '') and SameText(AName, FLockedParam) then
  begin
    Row.Track.Enabled    := False;
    Row.ResetBtn.Enabled := False;
  end;
end;

{ -- slider row removal -------------------------------------------------- }

procedure TFrameSliderContainer.RemoveSliderRow(const AParamName: string);
var
  Idx: Integer;
  I:   Integer;
begin
  Idx := RowIndexOf(AParamName);
  if Idx < 0 then Exit;

  { Shift focus to the scrollbox before destroying anything.
    Without this, FMX's focus chain is left pointing at a dead control
    and the app appears to freeze until the window is refreshed. }
  VertScrollBox1.SetFocus;

  FOwned.Remove(FRows[Idx].Layout);

  for I := Idx to High(FRows) - 1 do
    FRows[I] := FRows[I + 1];
  SetLength(FRows, Length(FRows) - 1);

  RebuildListBox(EditFilter.Text);
end;

{ -- public API ---------------------------------------------------------- }

procedure TFrameSliderContainer.LoadParams(const AParamNames: TArray<string>;
                                           const AInitValues: TArray<Double>);
var
  Idx: TArray<Integer>;
  I, N: Integer;
begin
  N := Length(AParamNames);
  SetLength(FAllParamNames,  N);
  SetLength(FAllParamValues, N);

  { Sort an index array by name, then permute both source arrays through it.
    Keeps FAllParamNames[i] and FAllParamValues[i] aligned after sorting. }
  SetLength(Idx, N);
  for I := 0 to N - 1 do Idx[I] := I;

  TArray.Sort<Integer>(Idx,
    TComparer<Integer>.Construct(
      function(const L, R: Integer): Integer
      begin
        Result := CompareStr(AParamNames[L], AParamNames[R]);
      end));

  for I := 0 to N - 1 do
  begin
    FAllParamNames[I] := AParamNames[Idx[I]];
    if Idx[I] < Length(AInitValues) then
      FAllParamValues[I] := AInitValues[Idx[I]]
    else
      FAllParamValues[I] := 0.0;
  end;

  RebuildListBox(EditFilter.Text);
end;

{ Removes slider rows only. The catalogue (FAllParamNames/FAllParamValues)
  is not touched — call LoadParams from the orchestrator if a model reload
  may have changed the parameter set. }
procedure TFrameSliderContainer.ClearSliders;
begin
  { NOTE: handler lifecycle (FOnSliderChanged, FReleaseOnlyMode) is
    owner-managed — the host orchestrating tab switches re-binds via
    each frame's AttachToSliders after calling ClearSliders. Clearing
    them here would orphan rows created by the user via "Add all" or
    the listbox after a tab switch, since those paths don't re-bind. }
  VertScrollBox1.BeginUpdate;
  try
    FOwned.Clear;
    FRows := [];
  finally
    VertScrollBox1.EndUpdate;
  end;
  FLockedParam := '';
  RebuildListBox(EditFilter.Text);
end;


procedure TFrameSliderContainer.btnDeleteAllClick(Sender: TObject);
begin
  { Quick way to clear the whole set so the user can pick a fresh one.
    ClearSliders removes every row (and any lock) but leaves OnSliderChanged
    bound, so adding new sliders afterwards works without re-attaching. }
  ClearSliders;
end;

procedure TFrameSliderContainer.BuildSliders(
  const AParamNames:    TArray<string>;
  const AInitialValues: TArray<Double>;
  const AMaxValue:      Single);
var
  I:    Integer;
  Init: Double;
begin
  ClearSliders;
  if Length(AParamNames) = 0 then Exit;

  FAllParamNames  := Copy(AParamNames);
  FAllParamValues := Copy(AInitialValues);
  TArray.Sort<string>(FAllParamNames);

  VertScrollBox1.BeginUpdate;
  try
    for I := 0 to High(AParamNames) do
    begin
      Init := 0.0;
      if I < Length(AInitialValues) then Init := AInitialValues[I];
      AddSliderRow(AParamNames[I], Init);
    end;
  finally
    VertScrollBox1.EndUpdate;
  end;

  RebuildListBox(EditFilter.Text);
end;

procedure TFrameSliderContainer.chkPositionChange(Sender: TObject);
begin
  { No action needed: DoListBoxItemClick reads chkPosition.IsChecked at the
    moment a slider is added. Off (default) = add at bottom, on = add at top.
    Existing rows are left where they are. }
end;

procedure TFrameSliderContainer.RefreshValues(const ANames:  TArray<string>;
                                              const AValues: TArray<Double>);
var
  I, J:            Integer;
  SavedHandler:    TSliderChangedEvent;
  NewMin, NewMax:  Single;
begin
  if Length(FRows) = 0 then Exit;

  SavedHandler     := FOnSliderChanged;
  FOnSliderChanged := nil;
  try
    for I := 0 to High(ANames) do
    begin
      if I >= Length(AValues) then Break;
      J := RowIndexOf(ANames[I]);
      if J >= 0 then
      begin
        { A value from outside the row's range is entirely ordinary — the
          range is x10 / /10 around whatever the value was when the row was
          built, so a preset holding k1 at 10 gives 1..100 and the model's
          own 0.35 falls below it. Re-centre on the new value rather than
          clamping to the old range.

          Clamping was the original bug and the worse half of it: the track
          is not written back to the engine here, so the slider would have
          said 1.0 while the model ran at 0.35, with nothing on screen
          admitting the two had parted company.

          Only when the value does not fit. A value still inside the range
          leaves it alone, so a range the user has widened themselves
          survives an ordinary refresh. }
        if (AValues[I] < FRows[J].Track.Min) or
           (AValues[I] > FRows[J].Track.Max) then
        begin
          { Through SetTrackRange, never by assigning Min and Max directly:
            it also recomputes Frequency, which is the step the track snaps
            to. Leave Frequency describing the OLD range and the new value
            is quantised against a grid that no longer belongs to it — the
            slider lands on a neighbouring step instead of the value it was
            given, and the steps are visibly the wrong size. }
          RangeAround(AValues[I], NewMin, NewMax);
          SetTrackRange(FRows[J].Track, NewMin, NewMax, AValues[I]);
        end
        else
          FRows[J].Track.Value := AValues[I];
      end;
    end;
  finally
    FOnSliderChanged := SavedHandler;
  end;
end;

{ -- widget event handlers ----------------------------------------------- }

procedure TFrameSliderContainer.DoFilterChange(Sender: TObject);
begin
  RebuildListBox(EditFilter.Text);
end;

procedure TFrameSliderContainer.DoListBoxItemClick(
  const Sender: TCustomListBox; const Item: TListBoxItem);
var
  Name:   string;
  Filter: string;
  AtTop:  Boolean;
begin
  Name := Item.Text;
  if IsActive(Name) then Exit;
  if (FLockedParam <> '') and SameText(Name, FLockedParam) then
    Exit;

  { chkPosition off (the default) adds at the bottom; on adds at the top.
    AddSliderRow scrolls a top-added row into view itself; for a bottom-added
    row we scroll to the end below, so a freshly picked slider is never left
    off-screen either way. }
  AtTop := chkPosition.IsChecked;
  AddSliderRow(Name, InitialValueOf(Name), AtTop);

  { Defer the listbox rebuild — Clear() would free the TListBoxItem still
    referenced by the FMX MouseUp handler that's about to run, crashing
    on macOS where the allocator is stricter than Windows'. Same pattern
    as DoRemoveBtnClick. The scroll-to-end is deferred too, so it runs after
    the new row's layout has settled and the content height is up to date. }
  Filter := EditFilter.Text;
  TThread.ForceQueue(nil, procedure
  begin
    RebuildListBox(Filter);
    if not AtTop then
      VertScrollBox1.ViewportPosition := PointF(0, VertScrollBox1.ContentBounds.Height);
  end);
end;

//procedure TFrameSliderContainer.DoListBoxItemClick(
//  const Sender: TCustomListBox; const Item: TListBoxItem);
//var
//  Name: string;
//begin
//  Name := Item.Text;
//  if IsActive(Name) then Exit;
//  if (FLockedParam <> '') and SameText(Name, FLockedParam) then
//    Exit;
//  AddSliderRow(Name, InitialValueOf(Name));
//  RebuildListBox(EditFilter.Text);
//end;

procedure TFrameSliderContainer.DoRemoveBtnClick(Sender: TObject);
var
  Name: string;
begin
  if not (Sender is TButton) then Exit;
  Name := TButton(Sender).TagString;
  TThread.ForceQueue(nil, procedure
  begin
    RemoveSliderRow(Name);
  end);
end;

procedure TFrameSliderContainer.DoTrackBarChange(Sender: TObject);
var
  Track: TTrackBar;
  Lbl:   TLabel;
begin
  if not (Sender is TTrackBar) then Exit;
  Track := TTrackBar(Sender);

  if not (Assigned(Track.TagObject) and (Track.TagObject is TLabel)) then Exit;
  Lbl := TLabel(Track.TagObject);

  Lbl.Text := FormatLabelText(Lbl.TagString, Track.Value);

  if Assigned(FOnSliderChanged) then
    FOnSliderChanged(Self, Lbl.TagString, Track.Value);
end;

{ -- "Add all" button ---------------------------------------------------- }

procedure TFrameSliderContainer.DoBtnAddAllClick(Sender: TObject);
var
  I: Integer;
begin
  if Length(FAllParamNames) = 0 then Exit;

  VertScrollBox1.BeginUpdate;
  try
    for I := 0 to High(FAllParamNames) do
      if not IsActive(FAllParamNames[I]) then
        AddSliderRow(FAllParamNames[I], InitialValueOf(FAllParamNames[I]));
  finally
    VertScrollBox1.EndUpdate;
  end;

  RebuildListBox(EditFilter.Text);
end;

{ -- "Reset" buttons ----------------------------------------------------- }

procedure TFrameSliderContainer.DoResetSingleSlider (Sender : TObject);
var
  Idx:            Integer;
  InitVal:        Double;
  Row:            TSliderRow;
  NewMin, NewMax: Single;
begin
  { Reset the slider whose own "R" button was clicked. The button carries
    its parameter name in TagString (set in AddSliderRow), so we look the
    row up by name -- resetting strictly that row, regardless of which
    slider the user last dragged. }
  if not (Sender is TButton) then Exit;

  Idx := RowIndexOf(TButton(Sender).TagString);
  if Idx < 0 then Exit;

  Row     := FRows[Idx];
  InitVal := InitialValueOf(Row.ParamName);

  Row.Track.OnChange := nil;
  try
    { Re-centre through SetTrackRange when the value falls outside, so the
      track's Frequency is recomputed with it — assigning Min and Max on
      their own leaves the snap grid describing the previous range. }
    if (InitVal < Row.Track.Min) or (InitVal > Row.Track.Max) then
    begin
      RangeAround(InitVal, NewMin, NewMax);
      SetTrackRange(Row.Track, NewMin, NewMax, InitVal);
    end
    else
      Row.Track.Value := InitVal;
  finally
    Row.Track.OnChange := DoTrackBarChange;
  end;

  Row.Lbl.Text := FormatLabelText(Row.ParamName, InitVal);

  if Assigned(FOnSliderChanged) then
     FOnSliderChanged(Self, Row.ParamName, InitVal);
end;

procedure TFrameSliderContainer.DoBtnResetClick(Sender: TObject);
var
  I:              Integer;
  InitVal:        Double;
  Row:            TSliderRow;
  NewMin, NewMax: Single;
begin
  if Length(FRows) = 0 then Exit;

  for I := 0 to High(FRows) do
  begin
    Row     := FRows[I];
    InitVal := InitialValueOf(Row.ParamName);

    Row.Track.OnChange := nil;
    try
      { See the note in DoBtnRowResetClick: Min/Max and Frequency go
        together. }
      if (InitVal < Row.Track.Min) or (InitVal > Row.Track.Max) then
      begin
        RangeAround(InitVal, NewMin, NewMax);
        SetTrackRange(Row.Track, NewMin, NewMax, InitVal);
      end
      else
        Row.Track.Value := InitVal;
    finally
      Row.Track.OnChange := DoTrackBarChange;
    end;

    Row.Lbl.Text := FormatLabelText(Row.ParamName, InitVal);

    if Assigned(FOnSliderChanged) then
      FOnSliderChanged(Self, Row.ParamName, InitVal);
  end;
end;

procedure TFrameSliderContainer.DoLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Lbl:           TLabel;
  Track:         TTrackBar;
  NewMin, NewMax: Single;
begin
  // 1. Check if it is a standard right-click
  // 2. OR check if it is a Mac trackpad context click (Left click + Control key)
  if (Button = TMouseButton.mbRight) or
     ((Button = TMouseButton.mbLeft) and (ssCtrl in Shift)) then
     begin
     if not (Sender is TLabel) then Exit;
     Lbl := TLabel(Sender);
     if not (Assigned(Lbl.TagObject) and (Lbl.TagObject is TTrackBar)) then Exit;
     Track := TTrackBar(Lbl.TagObject);

     NewMin := Track.Min;
     NewMax := Track.Max;

     if PromptForRange('Edit range -- ' + Lbl.TagString, NewMin, NewMax) then
      begin
        Track.BeginUpdate;
        try
          SetTrackRange(Track, NewMin, NewMax, Track.Value);
        finally
          Track.EndUpdate;
        end;
        Lbl.Text := FormatLabelText(Lbl.TagString, Track.Value);
      end;
     end;
end;

procedure TFrameSliderContainer.GetSliderValues(out ANames: TArray<string>;
                                                out AValues: TArray<Double>);
var
  I: Integer;
begin
  SetLength(ANames,  Length(FRows));
  SetLength(AValues, Length(FRows));
  for I := 0 to High(FRows) do
  begin
    ANames[I]  := FRows[I].ParamName;
    AValues[I] := FRows[I].Track.Value;
  end;
end;

{ -- locked-parameter support -------------------------------------------- }

procedure TFrameSliderContainer.SetLockedParam(const AParamName: string);
var
  OldIdx, NewIdx: Integer;
begin
  if SameText(AParamName, FLockedParam) then Exit;

  if FLockedParam <> '' then
  begin
    OldIdx := RowIndexOf(FLockedParam);
    if OldIdx >= 0 then
    begin
      FRows[OldIdx].Track.Enabled    := True;
      FRows[OldIdx].ResetBtn.Enabled := True;
    end;
  end;

  FLockedParam := AParamName;

  if FLockedParam <> '' then
  begin
    NewIdx := RowIndexOf(FLockedParam);
    if NewIdx >= 0 then
    begin
      FRows[NewIdx].Track.Enabled    := False;
      FRows[NewIdx].ResetBtn.Enabled := False;
    end;
  end;

  RebuildListBox(EditFilter.Text);
end;

end.
