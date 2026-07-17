unit ufConfigureSteadyState;

{ Steady-state solver configuration dialog.

  Sibling of ufConfigureCVODE - same design, but drives the libroadrunner
  steady-state solver API (NLEQ1/NLEQ2/etc.) instead of the integrator API.
  Presents the parameters of the currently selected solver as a scrolling list
  of typed editors, letting the user inspect and change them. The solver combo
  at the top switches which solver is current.

  Model of operation:
    - Solver selection (the combo) is applied LIVE, because the C API can only
      report parameters for the *current* solver - to show another solver's
      parameters we must make it current first.
    - Parameter value edits are deferred: they are written to the engine only
      when the user clicks Apply or OK. Cancel writes nothing and restores the
      solver that was current when the dialog opened.

  All solver queries/setters are unit-level functions in uRoadRunner that act
  on the global internalRRHandle, which TRoadRunner.Create points at the
  session's instance. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.StdCtrls, FMX.Edit, FMX.ListBox,
  FMX.Objects,
  uRoadRunner, uAnalysisTypes;

type
  { One editable solver parameter and the control bound to it. }
  TSolverParamRow = record
    Name  : string;
    PType : Integer;      // libroadrunner parameter-type code (see PARAM_TYPE_*)
    Editor: TControl;     // TCheckBox for bool, otherwise TEdit
    Row   : TControl;     // owning row layout (freed to remove the row)
  end;

  TfrmConfigSteadyState = class(TForm)
    pnlTop: TLayout;
    lblSolver: TLabel;
    cboSolver: TComboBox;
    lblDescription: TLabel;
    sbParams: TVertScrollBox;
    pnlBottom: TLayout;
    btnOK: TButton;
    btnCancel: TButton;
    btnReset: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cboSolverChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    { Private declarations }
    FContext:       IAnalysisContext;
    FRows:          TList<TSolverParamRow>;
    FFmt:           TFormatSettings;   // invariant '.' decimals for doubles
    FOriginalSolver: string;
    FLoading:       Boolean;           // suppresses combo OnChange during setup

    function ReadParamAsText (const AName: string; AType: Integer;
                          const AFmt: TFormatSettings): string;
    procedure BuildSolverList;
    procedure BuildParamEditors;
    procedure ClearParamEditors;
    procedure AddParamRow(const AName, ADisplayName: string; AType: Integer);
    procedure RefreshEditorsFromEngine;
    procedure UpdateSolverDescription;
    function  ApplyChanges: Boolean;
    procedure SetUIEnabled(AEnabled: Boolean);
  public
    { Public declarations }
    RR : TRoadRunner;
    destructor  Destroy; override;
    procedure SetContext (const AContext : IAnalysisContext);
  end;

var
  frmConfigSteadyState: TfrmConfigSteadyState;

implementation

{$R *.fmx}

const
  { libroadrunner parameter-type codes. These are the index into the
    std::variant that backs rr::Setting (NOT the older TypeId enum). Order:
      monostate, string, bool, int32, uint32, int64, uint64, float, double,
      char, uchar, vector<double> }
  PARAM_TYPE_EMPTY  = 0;   // std::monostate
  PARAM_TYPE_STRING = 1;
  PARAM_TYPE_BOOL   = 2;
  PARAM_TYPE_INT32  = 3;
  PARAM_TYPE_UINT32 = 4;
  PARAM_TYPE_INT64  = 5;
  PARAM_TYPE_UINT64 = 6;
  PARAM_TYPE_FLOAT  = 7;
  PARAM_TYPE_DOUBLE = 8;
  PARAM_TYPE_CHAR   = 9;
  PARAM_TYPE_UCHAR  = 10;
  PARAM_TYPE_DBLVEC = 11;  // std::vector<double> - not editable here

  DESC_COLOR = $FFFFFFFF; // white - legible against the dark form style

function TypeName (AType : Integer) : string;
begin
  case AType of
    PARAM_TYPE_EMPTY:  Result := 'empty';
    PARAM_TYPE_STRING: Result := 'string';
    PARAM_TYPE_BOOL:   Result := 'bool';
    PARAM_TYPE_INT32:  Result := 'int';
    PARAM_TYPE_UINT32: Result := 'uint';
    PARAM_TYPE_INT64:  Result := 'int64';
    PARAM_TYPE_UINT64: Result := 'uint64';
    PARAM_TYPE_FLOAT:  Result := 'float';
    PARAM_TYPE_DOUBLE: Result := 'double';
    PARAM_TYPE_CHAR:   Result := 'char';
    PARAM_TYPE_UCHAR:  Result := 'uchar';
    PARAM_TYPE_DBLVEC: Result := 'double[]';
  else                 Result := 'type ' + IntToStr (AType);
  end;
end;

{ Current engine value of a parameter rendered as display text, dispatched on
  its type. Doubles use invariant formatting so scientific tolerances round
  trip cleanly. }
function TfrmConfigSteadyState.ReadParamAsText (const AName: string; AType: Integer;
                          const AFmt: TFormatSettings): string;
begin
  case AType of
    PARAM_TYPE_INT32,
    PARAM_TYPE_INT64:  Result := IntToStr (RR.getSteadyStateSolverParameterInt (AName));
    PARAM_TYPE_UINT32,
    PARAM_TYPE_UINT64: Result := UIntToStr (RR.getSteadyStateSolverParameterUInt (AName));
    PARAM_TYPE_FLOAT,
    PARAM_TYPE_DOUBLE: Result := FloatToStr (RR.getSteadyStateSolverParameterDouble (AName), AFmt);
    PARAM_TYPE_DBLVEC: Result := '(double vector - edit not supported)';
    PARAM_TYPE_EMPTY:  Result := '';
  else                 Result := RR.getSteadyStateSolverParameterString (AName);
  end;
end;

procedure TfrmConfigSteadyState.FormCreate(Sender: TObject);
begin
  FRows := TList<TSolverParamRow>.Create;
  FFmt  := TFormatSettings.Invariant;
end;

destructor TfrmConfigSteadyState.Destroy;
begin
  FRows.Free;
  inherited;
end;

procedure TfrmConfigSteadyState.SetContext (const AContext : IAnalysisContext);
begin
  FContext := AContext;
  { Safe to resolve RR now that we have a context. FormCreate runs during
    Create(nil), before the caller can set the context, so RR cannot be
    assigned there. }
  RR := FContext.Session.RoadRunner;

  if (FContext = nil) or (not FContext.Session.IsLoaded) then
    begin
    lblDescription.Text := 'No model loaded - load and run a model first.';
    SetUIEnabled (False);
    Exit;
    end;

  FOriginalSolver := RR.getCurrentSteadyStateSolverName;
  BuildSolverList;
  BuildParamEditors;
end;

procedure TfrmConfigSteadyState.SetUIEnabled(AEnabled: Boolean);
begin
  cboSolver.Enabled := AEnabled;
  sbParams.Enabled  := AEnabled;
  btnOK.Enabled     := AEnabled;
  btnReset.Enabled  := AEnabled;
end;

{ Populate the solver combo from the registered list and select the one that
  is currently active. }
procedure TfrmConfigSteadyState.BuildSolverList;
var
  Names: TStringList;
begin
  FLoading := True;
  try
    cboSolver.Items.Clear;
    Names := getListOfRegisteredSteadyStateSolvers;
    try
      cboSolver.Items.AddStrings (Names);
    finally
      Names.Free;
    end;
    cboSolver.ItemIndex := cboSolver.Items.IndexOf (RR.getCurrentSteadyStateSolverName);
  finally
    FLoading := False;
  end;
end;

procedure TfrmConfigSteadyState.UpdateSolverDescription;
begin
  lblDescription.Text := RR.getCurrentSteadyStateSolverName + ' - ' + RR.getSteadyStateSolverDescription;
end;

{ Rebuild the editor rows for whatever solver is currently active. Iterate by
  index (not the names list) so we can pull each parameter's friendly display
  name, which is only available via the Nth-parameter API. }
procedure TfrmConfigSteadyState.BuildParamEditors;
var
  I, Count: Integer;
  Name:     string;
begin
  ClearParamEditors;
  UpdateSolverDescription;

  Count := RR.getNumberOfSteadyStateSolverParameters;
  for I := 0 to Count - 1 do
    begin
    Name := RR.getCurrentSteadyStateNthParameterName (I);
    AddParamRow (Name,
                 RR.getCurrentSteadyStateNthParameterDisplayName (I),
                 RR.getSteadyStateSolverParameterType (Name));
    end;
end;

procedure TfrmConfigSteadyState.ClearParamEditors;
var
  I: Integer;
begin
  for I := 0 to FRows.Count - 1 do
    FRows[I].Row.Free;      // frees the row layout and its child controls
  FRows.Clear;
end;

{ Create one row: a top line with the parameter name (left) and its editor
  (right), and a wrapped description underneath. The current engine value seeds
  the editor. Guarded so a single unreadable parameter is skipped rather than
  aborting the whole build. }
procedure TfrmConfigSteadyState.AddParamRow(const AName, ADisplayName: string; AType: Integer);
var
  Row:     TLayout;
  TopLine: TLayout;
  NameLbl: TLabel;
  DescLbl: TLabel;
  Sep:     TRectangle;
  Chk:     TCheckBox;
  Edt:     TEdit;
  Editor:  TControl;
  Desc:    string;
  Caption: string;
  RowRec:  TSolverParamRow;
begin
  try
    Row := TLayout.Create (Self);
    Row.Parent := sbParams;
    Row.Align  := TAlignLayout.Top;
    Row.Height := 62;
    Row.Margins.Left  := 12;
    Row.Margins.Right := 12;
    Row.Margins.Top   := 6;

    TopLine := TLayout.Create (Self);
    TopLine.Parent := Row;
    TopLine.Align  := TAlignLayout.Top;
    TopLine.Height := 30;

    if AType = PARAM_TYPE_BOOL then
      begin
      Chk := TCheckBox.Create (Self);
      Chk.Parent    := TopLine;
      Chk.Align     := TAlignLayout.Right;
      Chk.Width     := 220;
      Chk.Text      := '';
      Chk.IsChecked := RR.getSteadyStateSolverParameterBoolean (AName);
      Editor := Chk;
      end
    else
      begin
      Edt := TEdit.Create (Self);
      Edt.Parent := TopLine;
      Edt.Align  := TAlignLayout.Right;
      Edt.Width  := 220;
      Edt.Text   := ReadParamAsText (AName, AType, FFmt);
      { Types we can read but not round-trip through the scalar setters are
        shown read-only rather than pretending they're editable. }
      Edt.ReadOnly := AType in [PARAM_TYPE_DBLVEC, PARAM_TYPE_EMPTY];
      Editor := Edt;
      end;

    { Prefer the friendly display name; fall back to the raw id. }
    Caption := ADisplayName;
    if Trim (Caption) = '' then
      Caption := AName;

    NameLbl := TLabel.Create (Self);
    NameLbl.Parent := TopLine;
    NameLbl.Align  := TAlignLayout.Client;
    NameLbl.Margins.Right := 8;
    NameLbl.StyledSettings := NameLbl.StyledSettings - [TStyledSetting.Style];
    NameLbl.TextSettings.Font.Style := [TFontStyle.fsBold];
    NameLbl.Text := Caption + '   (' + TypeName (AType) + ')';

    Desc := RR.getSteadyStateSolverParameterDescription (AName);
    if Trim (Desc) = '' then
      Desc := RR.getSteadyStateSolverParameterHint (AName);

    DescLbl := TLabel.Create (Self);
    DescLbl.Parent := Row;
    DescLbl.Align  := TAlignLayout.Client;
    DescLbl.StyledSettings := DescLbl.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
    DescLbl.TextSettings.Font.Size := 12;
    DescLbl.TextSettings.FontColor := DESC_COLOR;
    DescLbl.TextSettings.WordWrap  := True;
    DescLbl.Text := Desc;

    Sep := TRectangle.Create (Self);
    Sep.Parent      := Row;
    Sep.Align       := TAlignLayout.Bottom;
    Sep.Height      := 1;
    Sep.Stroke.Kind := TBrushKind.None;
    Sep.Fill.Color  := $FFE6E6E6;

    RowRec.Name   := AName;
    RowRec.PType  := AType;
    RowRec.Editor := Editor;
    RowRec.Row    := Row;
    FRows.Add (RowRec);
  except
    on E: Exception do
      { Skip a parameter we can't render; keep going with the rest. }
      ;
  end;
end;

{ Re-read every parameter from the engine into its editor. Called after Apply
  so the user sees any values the solver clamped or normalised. }
procedure TfrmConfigSteadyState.RefreshEditorsFromEngine;
var
  I:   Integer;
  Rec: TSolverParamRow;
begin
  for I := 0 to FRows.Count - 1 do
    begin
    Rec := FRows[I];
    try
      if Rec.PType = PARAM_TYPE_BOOL then
        TCheckBox(Rec.Editor).IsChecked := RR.getSteadyStateSolverParameterBoolean (Rec.Name)
      else
        TEdit(Rec.Editor).Text := ReadParamAsText (Rec.Name, Rec.PType, FFmt);
    except
      on E: Exception do ;
    end;
    end;
end;

{ Write every editor value back to the engine. Returns True if all succeeded;
  otherwise reports the first failures in the status label and returns False. }
function TfrmConfigSteadyState.ApplyChanges: Boolean;
var
  I:      Integer;
  Rec:    TSolverParamRow;
  Iv:     Integer;
  Dv:     Double;
  Errors: TStringList;
begin
  Errors := TStringList.Create;
  try
    for I := 0 to FRows.Count - 1 do
      begin
      Rec := FRows[I];
      try
        case Rec.PType of
          PARAM_TYPE_BOOL:
            RR.setSteadyStateSolverParameterBoolean (Rec.Name, TCheckBox(Rec.Editor).IsChecked);
          PARAM_TYPE_INT32, PARAM_TYPE_INT64:
            begin
              Iv := StrToInt (Trim (TEdit(Rec.Editor).Text));
              if Iv < 0 then raise Exception.Create ('must be a non-negative number');
              RR.setSteadyStateSolverParameterInt (Rec.Name, Iv);
            end;
          PARAM_TYPE_UINT32, PARAM_TYPE_UINT64:
            { StrToUInt already rejects negatives. }
            RR.setSteadyStateSolverParameterUInt (Rec.Name, StrToUInt (Trim (TEdit(Rec.Editor).Text)));
          PARAM_TYPE_FLOAT, PARAM_TYPE_DOUBLE:
            begin
              Dv := StrToFloat (Trim (TEdit(Rec.Editor).Text), FFmt);
              if Dv < 0 then raise Exception.Create ('must be a non-negative number');
              RR.setSteadyStateSolverParameterDouble (Rec.Name, Dv);
            end;
          PARAM_TYPE_STRING:
            RR.setSteadyStateSolverParameterString (Rec.Name, TEdit(Rec.Editor).Text);
        else
          { empty / char / uchar / double-vector: shown read-only, not written. }
          Continue;
        end;
      except
        on E: Exception do
          Errors.Add (Rec.Name + ': ' + E.Message);
      end;
      end;

    Result := Errors.Count = 0;
    if Result then
      lblStatus.Text := 'Applied.'
    else
      lblStatus.Text := IntToStr (Errors.Count) + ' error(s): ' + Errors[0];
  finally
    Errors.Free;
  end;

  { Reflect whatever the engine actually stored. }
  RefreshEditorsFromEngine;
end;

procedure TfrmConfigSteadyState.cboSolverChange(Sender: TObject);
begin
  if FLoading then Exit;
  if cboSolver.ItemIndex < 0 then Exit;

  { Switching solver is applied live - the C API only exposes parameters for
    the current solver. }
  RR.setSteadyStateSolver (cboSolver.Items[cboSolver.ItemIndex]);
  BuildParamEditors;
  lblStatus.Text := '';
end;

procedure TfrmConfigSteadyState.btnResetClick(Sender: TObject);
begin
  { Reset the live solver to its factory defaults, then re-read every value
    into the editors so the user sees what changed. Note: this writes to the
    engine immediately (like the solver switch), so Cancel won't undo it. }
  try
    RR.resetSteadyStateSolverParameters;
    RefreshEditorsFromEngine;
    lblStatus.Text := 'Restored defaults.';
  except
    on E: Exception do
      lblStatus.Text := 'Reset failed: ' + E.Message;
  end;
end;

procedure TfrmConfigSteadyState.btnOKClick(Sender: TObject);
begin
  if ApplyChanges then
    ModalResult := mrOk;
  { On failure we stay open so the user can fix the offending value. }
end;

procedure TfrmConfigSteadyState.btnCancelClick(Sender: TObject);
begin
  { Parameter edits were never written, so nothing to undo there. Only the
    solver selection is applied live, so restore it if the user changed it. }
  try
    if (FOriginalSolver <> '') and
       (RR.getCurrentSteadyStateSolverName <> FOriginalSolver) then
      RR.setSteadyStateSolver (FOriginalSolver);
  except
    on E: Exception do ;
  end;
  ModalResult := mrCancel;
end;

end.
