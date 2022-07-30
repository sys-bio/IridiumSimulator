unit uFrameFunctionPlotter;

interface

uses
  System.SysUtils, System.Types,
  System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
  uRRTypes,
  uController,
  uExpressionEvaluator,
  FMX.EditBox,
  FMX.NumberBox, FMX.Objects;

type
  TframeFunctionPlotter = class(TFrame)
    edtExpression: TEdit;
    nbStart: TNumberBox;
    nbEnd: TNumberBox;
    btnRun: TButton;
    Label8: TLabel;
    btnFunctionSliders: TSpeedButton;
    Image11: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    nbNumPoints: TNumberBox;
    chkAutoYScale: TCheckBox;
    chkShowLegend: TCheckBox;
    procedure btnRunClick(Sender: TObject);
    procedure btnFunctionSlidersClick(Sender: TObject);
    procedure chkAutoYScaleCanFocus(Sender: TObject; var ACanFocus: Boolean);
    procedure chkShowLegendChange(Sender: TObject);
  private
    { Private declarations }
    expr : TAlgExpression;
    autoYScale : boolean;
    showLegend : boolean;
    procedure evalFunction;
  public
    { Public declarations }
    stylebook1 : TStyleBook;
    controller : TController;
    procedure   OnFunctionSliderNotify(parameter: string; value: double);
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation

{$R *.fmx}

Uses ufSliders;

procedure TframeFunctionPlotter.OnFunctionSliderNotify(parameter: string; value: double);
begin
  expr.SetVar(parameter, value);
  evalFunction;
end;


procedure TframeFunctionPlotter.evalFunction;
var i : integer;
    x, delta : double;
    data : T2DMatrix;
begin
  if nbStart.Value >= nbEnd.Value then
     raise Exception.Create('Start value for x must be less than end value for x');

  data := T2DMatrix.Create(trunc (nbNumPoints.Value), 2);
  x := nbStart.Value;
  delta := (nbEnd.value - nbStart.value)/trunc (nbNumPoints.Value);
  for i := 0 to trunc (nbNumPoints.Value) - 1 do
      begin
      expr.SetVar('x', x);
      data[i,0] := x;
      data[i,1] := expr.Eval;
      x := x + delta;
      end;
  controller.viewerPackage.XColumnIndex := 0;
  setlength (controller.viewerPackage.YColumnChoice, 2);
  controller.viewerPackage.XAxisTitle := 'X Value';
  controller.viewerPackage.YColumnChoice[0] := False;
  controller.viewerPackage.YColumnChoice[1] := True;
  //controller.viewerPackage.autoYScale := chkAutoYScale.IsChecked;
  //controller.viewerPackage.showLegend := showLegend;
  setLength (controller.viewerPackage.YColumnNames, 2);
  controller.viewerPackage.YColumnNames[0] := 'x';
  controller.viewerPackage.YColumnNames[1] := 'y';
  controller.simulator.simulationData := data;
  controller.updateViewers;
end;


procedure TframeFunctionPlotter.btnRunClick(Sender: TObject);
var i : integer;
begin
  expr.Compile(edtExpression.Text);
  for i := 1 to expr.nSymbols do
      if expr.GetnthSymbol(i) <> 'x' then
         begin
         expr.SetnthSymbol(i, 1.0);
         end;
  evalFunction;
end;


procedure TframeFunctionPlotter.chkAutoYScaleCanFocus(Sender: TObject; var ACanFocus: Boolean);
begin
  chkAutoYScale.IsChecked := autoYScale;
end;


procedure TframeFunctionPlotter.chkShowLegendChange(Sender: TObject);
begin
  showLegend := chkShowLegend.IsChecked;
  controller.viewerPackage.showLegend := showLegend;
  controller.updateViewers;
end;


procedure TframeFunctionPlotter.btnFunctionSlidersClick(Sender: TObject);
var
  i: integer;
  alist: TStringList;
  value: double;
begin
  if not Assigned(frmSliders) then
    frmSliders := TfrmSliders.Create(nil);
  frmSliders.OnNotifyChange := OnFunctionSliderNotify;
  frmSliders.StyleBook := StyleBook1;
  for i := frmSliders.lstParameters.Count - 1 downto 0 do
    if frmSliders.lstParameters.Items.Objects[i] <> nil then
      frmSliders.lstParameters.Items.Objects[i].Free;

  frmSliders.lstParameters.Clear;
  alist := TStringList.Create;
  for i := 1 to expr.nSymbols do
      if expr.GetnthSymbol(i) <> 'x' then
         begin
         expr.SetnthSymbol(i, 1.0);
         alist.Add(expr.GetnthSymbol(i));
         end;

  for i := 0 to alist.Count - 1 do
      frmSliders.lstParameters.Items.AddObject(alist[i], TSliderInitialValue.Create(1.0));

  frmSliders.Show;
  frmSliders.BringToFront;
end;

constructor TframeFunctionPlotter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  expr := TAlgExpression.Create(nil);
  expr.AssignBehaviour := TAssignBehaviour.eInstallRightSymbols;
  chkAutoYScale.IsChecked := True;
  chkShowLegend.IsChecked := False;
end;

end.
