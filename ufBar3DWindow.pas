unit ufBar3DWindow;

{ TfrmBar3D -- non-modal floating window that renders one MCA matrix
  (elasticities, flux control coefficients, or concentration control
  coefficients) as a 3D bar chart using TBarGraph (U3DBarGraph).

  Lifetime model
  --------------
  Owned by the Application; freed on close (Action := caFree). The host
  frame (TFrameSteadyState) keeps a dictionary keyed by TBar3DMatrixKind
  so the same window is reused for repeat clicks. The window fires
  OnFormClosed before tearing down so the host can drop its dictionary
  entry. The frame, in turn, calls LoadMatrix again on each Recompute
  to keep open windows live as sliders move and steady states resolve.

  Data model
  ----------
  LoadMatrix is the only data-entry point. It Resets the bar graph,
  computes MaxAbs for intensity normalization, applies per-bar colors
  matching the grid coloring convention (positive -> green family,
  negative -> red family, intensity -> magnitude / MaxAbs), and feeds
  row/column labels straight from the SpeciesIds / ReactionIds the
  caller already has on hand.

  Why per-bar color instead of TBarGraph.BarColor
  -----------------------------------------------
  Control coefficients routinely go negative; a single global bar color
  destroys sign information. The Add() overload takes a TAlphaColor
  per bar, which is what we use. The component's built-in negative
  plane visibility (ViewNegativePlane / ViewPositivePlane) is left at
  its defaults -- both half-planes visible -- so negative bars hang
  down through the XY plane as the user would expect. }

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.Math,
  System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts,
  FMX.Controls.Presentation, FMX.Objects,
  U3DBarGraph,
  uRR2DSimpleMatrix, FMX.ListBox, FMX.Colors;

type
  { Tag identifying which steady-state matrix a window is showing.
    The frame's dictionary is keyed by this so each matrix type has
    at most one open window at a time. Add new kinds here if we later
    extend 3D view to the Jacobian or other matrices. }
  TBar3DMatrixKind = (mkElasticities, mkFluxCC, mkConcCC);

  TfrmBar3D = class(TForm)
    LayoutToolbar: TLayout;
    lblTitle: TLabel;
    lblScaling: TLabel;
    LayoutHost: TLayout;
    Layout1: TLayout;
    cboColorGrid: TColorComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cboColorXYandYXPlaneColor: TColorComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cboColorGridChange(Sender: TObject);
    procedure cboColorXYandYXPlaneColorChange(Sender: TObject);
  private
    FBarGraph: TBarGraph;
    FKind: TBar3DMatrixKind;
    FOnFormClosed: TNotifyEvent;

    FGridColor: TAlphaColor;
    FXYAndYZColor: TAlphaColor;

    function ColorForValue(V, MaxAbs: Double): TAlphaColor;
  public
    { Replace the window's data with the given matrix. Safe to call
      repeatedly; reuses the same TBarGraph instance. RowLabels apply to
      the Y axis (rows of M), ColLabels to the X axis (columns of M).
      AZLabel is the Z-axis label, typically a math symbol like
      'epsilon', 'C^J', or 'C^S' plus the scaling state. }
    procedure LoadMatrix(AKind: TBar3DMatrixKind;
                         M: T2DMatrix;
                         const ARowLabels, AColLabels: TArray<string>;
                         const AZLabel: string;
                         AScaled: Boolean);

    { The matrix kind this window is currently displaying. Used by the
      host frame to look the window up in its dictionary. }
    property Kind: TBar3DMatrixKind read FKind;

    { Fired just before the form is destroyed (during FormClose).
      The host uses this to drop its dictionary entry so the next
      button click creates a fresh window. }
    property OnFormClosed: TNotifyEvent read FOnFormClosed write FOnFormClosed;
  end;

  { Convenience strings for callers building axis labels. Kept here so
    the form unit owns its own conventions; callers pass the final
    string to LoadMatrix and need not import the bar graph unit. }
function Bar3DKindCaption(AKind: TBar3DMatrixKind; AScaled: Boolean): string;
function Bar3DKindZLabel (AKind: TBar3DMatrixKind; AScaled: Boolean): string;

implementation

{$R *.fmx}

const
  { Coloring convention matches the 2D grid: positive -> Yellowgreen,
    negative -> Tomato, white at zero, full color at MaxAbs. Kept as
    constants so the convention is changed in one place. }
  COLOR_POSITIVE: TAlphaColor = TAlphaColorRec.Yellowgreen;
  COLOR_NEGATIVE: TAlphaColor = TAlphaColorRec.Tomato;
  COLOR_ZERO    : TAlphaColor = TAlphaColorRec.White;

function Bar3DKindCaption(AKind: TBar3DMatrixKind; AScaled: Boolean): string;
const
  ScalingTag: array [Boolean] of string = (' (unscaled)', ' (scaled)');
begin
  case AKind of
    mkElasticities: Result := 'Elasticities';
    mkFluxCC:       Result := 'Flux Control Coefficients';
    mkConcCC:       Result := 'Concentration Control Coefficients';
  else
    Result := '';
  end;
  Result := Result + ScalingTag[AScaled];
end;

function Bar3DKindZLabel(AKind: TBar3DMatrixKind; AScaled: Boolean): string;
begin
  case AKind of
    mkElasticities: if AScaled then Result := 'eps'  else Result := 'dv/dS';
    mkFluxCC:       if AScaled then Result := 'C^J'  else Result := 'dJ/dE';
    mkConcCC:       if AScaled then Result := 'C^S'  else Result := 'dS/dE';
  else
    Result := '';
  end;
end;

{ -- color helpers ------------------------------------------------------- }

function LerpChannel(A, B: Byte; T: Single): Byte; inline;
begin
  Result := Byte(Round(A + (B - A) * T));
end;

{ Blend two TAlphaColors linearly. T = 0 gives ACol0, T = 1 gives ACol1. }
function BlendAlphaColor(ACol0, ACol1: TAlphaColor; T: Single): TAlphaColor;
var
  R0, G0, B0, A0: Byte;
  R1, G1, B1, A1: Byte;
begin
  T := EnsureRange(T, 0.0, 1.0);

  A0 := TAlphaColorRec(ACol0).A;
  R0 := TAlphaColorRec(ACol0).R;
  G0 := TAlphaColorRec(ACol0).G;
  B0 := TAlphaColorRec(ACol0).B;

  A1 := TAlphaColorRec(ACol1).A;
  R1 := TAlphaColorRec(ACol1).R;
  G1 := TAlphaColorRec(ACol1).G;
  B1 := TAlphaColorRec(ACol1).B;

  TAlphaColorRec(Result).A := LerpChannel(A0, A1, T);
  TAlphaColorRec(Result).R := LerpChannel(R0, R1, T);
  TAlphaColorRec(Result).G := LerpChannel(G0, G1, T);
  TAlphaColorRec(Result).B := LerpChannel(B0, B1, T);
end;

procedure TfrmBar3D.cboColorGridChange(Sender: TObject);
begin
  FBarGraph.GridColor := cboColorGrid.Color;
end;

procedure TfrmBar3D.cboColorXYandYXPlaneColorChange(Sender: TObject);
begin
  FBarGraph.XZandYZPlaneColor := cboColorXYandYXPlaneColor.Color;
end;

function TfrmBar3D.ColorForValue(V, MaxAbs: Double): TAlphaColor;
var
  T: Single;
begin
  { Degenerate matrix (all zeros) -> show every bar in neutral. The bars
    will be flat anyway, so this is purely defensive. }
  if (MaxAbs <= 0) or IsNan(V) or IsInfinite(V) then
    Exit(COLOR_ZERO);

  T := EnsureRange(Abs(V) / MaxAbs, 0.0, 1.0);
  if V >= 0 then
    Result := BlendAlphaColor(COLOR_ZERO, COLOR_POSITIVE, T)
  else
    Result := BlendAlphaColor(COLOR_ZERO, COLOR_NEGATIVE, T);
end;

{ -- form lifecycle ------------------------------------------------------ }

procedure TfrmBar3D.FormCreate(Sender: TObject);
begin
  FXYAndYZColor := claLightGrey;
  FGridColor := claLightGrey;

  { The bar graph is created in code rather than placed at design time
    so the unit can be compiled and dropped in without an IDE-edited
    .fmx referencing U3DBarGraph (which would force the IDE to load
    the bar graph package). LayoutHost is the docking target. }
  FBarGraph := TBarGraph.Create(Self);
  FBarGraph.Parent := LayoutHost;
  FBarGraph.Align := TAlignLayout.Client;
  FBarGraph.XZandYZPlaneColor := FGridColor;
  FBarGraph.GridColor := FXYAndYZColor;

  { Sensible defaults; the user can drag/zoom/rotate from here. The
    component handles its own camera/mouse/keyboard. }
  FBarGraph.AutoScale := True;
  FBarGraph.NumTicks  := 10;
end;

procedure TfrmBar3D.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { Free on close -- 3D viewports are heavyweight, and the user's next
    click on the same matrix's 3D button will rebuild from scratch.
    Notify the host first so it can drop its dictionary entry. }
  if Assigned(FOnFormClosed) then
    FOnFormClosed(Self);
  Action := TCloseAction.caFree;
end;

{ -- data loading -------------------------------------------------------- }

procedure TfrmBar3D.LoadMatrix(AKind: TBar3DMatrixKind;
                               M: T2DMatrix;
                               const ARowLabels, AColLabels: TArray<string>;
                               const AZLabel: string;
                               AScaled: Boolean);
var
  R, C: Integer;
  V, MaxAbs: Double;
begin
  FKind := AKind;
  Caption := Bar3DKindCaption(AKind, AScaled);
  lblTitle.Text := Caption;
  lblScaling.Text := 'rows: ' + IntToStr(Length(ARowLabels)) +
                    '   cols: ' + IntToStr(Length(AColLabels));

  if M = nil then Exit;

  { Compute the normalization constant once. Same convention as the
    2D grid: scale intensity by max absolute value across the matrix. }
  MaxAbs := 0.0;
  for R := 0 to M.r - 1 do
    for C := 0 to M.c - 1 do
    begin
      V := Abs(M[R, C]);
      if V > MaxAbs then MaxAbs := V;
    end;

  FBarGraph.BeginDataUpdate;
  try
    FBarGraph.Reset;

    { Axis labels first -- TBarGraph stores them indexed by row/col,
      so it's fine to feed them in any order before the data points. }
    for R := 0 to High(ARowLabels) do
      FBarGraph.AddYLabel(R, ARowLabels[R]);
    for C := 0 to High(AColLabels) do
      FBarGraph.AddXLabel(C, AColLabels[C]);

    FBarGraph.ZLabel := AZLabel;

    for R := 0 to M.r - 1 do
      for C := 0 to M.c - 1 do
      begin
        V := M[R, C];
        if IsNan(V) or IsInfinite(V) then Continue;
        FBarGraph.Add(R, C, V, ColorForValue(V, MaxAbs));
      end;
  finally
    FBarGraph.EndDataUpdate;
  end;
end;

end.
