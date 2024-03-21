unit uFrameSteadyStateControl;

interface

uses
  System.SysUtils, System.Types,
  System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics,
  FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style,
  FMX.Grid, FMX.ScrollBox,
  FMX.Objects,
  uRRList,
  uController,
  uModelInputManager,
  FMX.Layouts;

type
  TFrameSteadyStateControl = class(TFrame)
    Layout1: TLayout;
    Label8: TLabel;
    btnConfigureSteadyStateSolver: TButton;
    Layout2: TLayout;
    btnGetSteadyState: TButton;
    lblSteadyState: TLabel;
    btnSteadyStateSliders: TSpeedButton;
    Image11: TImage;
    Layout3: TLayout;
    SteadyStateGrid: TStringGrid;
    gridSymbolColumn: TStringColumn;
    gridValueColumn: TStringColumn;
    Splitter1: TSplitter;
    Layout4: TLayout;
    Label4: TLabel;
    gridEigenValues: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    Layout5: TLayout;
    btnMoreSteadyState: TButton;
    Splitter2: TSplitter;
    procedure btnGetSteadyStateClick(Sender: TObject);
    procedure btnMoreSteadyStateClick(Sender: TObject);
    procedure btnSteadyStateSlidersClick(Sender: TObject);
    procedure btnConfigureSteadyStateSolverClick(Sender: TObject);
  private
    { Private declarations }
    floatingSpeciesIds: TStringList;
    boundarySpeciesIds: TStringList;
    reactionRatesIds: TStringList;
    ratesOfChangeIds: TStringList;
    elasticityIds: TRRList; // Use RRList because its a nested list
    globalParameters : TStringList;

    procedure updateSteadyStateDisplay;
    function  loadModelFromMemo : TModelErrorState;
    procedure collectModelSymbols;
    procedure selectSteadyStateSolver;

    procedure OnSteadyStateSliderNotify(parameter: string; value: double);
  public
    { Public declarations }
    stylebook1 : TStyleBook;
    controller : TController;
  end;

implementation

{$R *.fmx}

Uses ufFloatingPlotViewer, uRRTypes, ufMoreSteadyState, ufSliders;

// Not yet operational
procedure TFrameSteadyStateControl.selectSteadyStateSolver;
var
  i: integer;
  sl: TStringList;
  currentName: string;
begin
  raise exception.Create('Steady state solver configuration options not yet implemented');
//  if frmSteadyStateOptions = nil then
//    begin
//      frmSteadyStateOptions := TfrmSteadyStateOptions.Create(nil);
//      frmSteadyStateOptions.StyleBook := StyleBook1;
//    end;
//  sl := uRoadRunner.API.getListOfRegisteredSteadyStateSolvers();
//  try
//    frmSteadyStateOptions.lstSolverNames.Items.Assign(sl);
//    currentName := uRoadRunner.API.getCurrentSteadyStateSolverName;
//
//    frmSteadyStateOptions.disableChange := true;
//    for i := 0 to sl.Count - 1 do
//      if sl[i] = currentName then
//        begin
//          frmSteadyStateOptions.lstSolverNames.itemindex := i;
//          break;
//        end;
//    frmSteadyStateOptions.disableChange := false;
//    frmSteadyStateOptions.displaySelectedSolver;
//    frmSteadyStateOptions.Show;
//  finally
//    sl.Free;
//  end;
end;


function TFrameSteadyStateControl.loadModelFromMemo : TModelErrorState;
var
  sbmlStr: string;
  modelErrorState : TModelErrorState;
begin
  modelErrorState := controller.modelInputManager.getSBMLFromAntimony(controller.modelInputManager.modelMemo.Lines.Text);
  if modelErrorState.ok then
     begin
     controller.loadSBMLModel(sbmlStr, true);

     collectModelSymbols;
     controller.outOfDate := false;
     end;
  result := modelErrorState;
end;



procedure TFrameSteadyStateControl.btnMoreSteadyStateClick(Sender: TObject);
begin
  if not Assigned (frmMoreSteadyState) then
     begin
     frmMoreSteadyState := TfrmMoreSteadyState.Create(nil);
     frmMoreSteadyState.controller := controller;
     end;
  frmMoreSteadyState.Show;
end;


// This gets called when the slider is moved
procedure TFrameSteadyStateControl.OnSteadyStateSliderNotify(parameter: string; value: double);
begin
  controller.simulator.roadrunner.setValue(parameter, value);
  controller.simulator.roadrunner.steadyState;
  updateSteadyStateDisplay;
end;


procedure TFrameSteadyStateControl.btnSteadyStateSlidersClick(Sender: TObject);
var
  i: integer;
  alist: TStringList;
  value: double;
begin
  if not Assigned(frmSliders) then
    frmSliders := TfrmSliders.Create(nil);
  frmSliders.OnNotifyChange := OnSteadyStateSliderNotify;
  frmSliders.StyleBook := StyleBook1;
  for i := frmSliders.lstParameters.Count - 1 downto 0 do
    if frmSliders.lstParameters.Items.Objects[i] <> nil then
      frmSliders.lstParameters.Items.Objects[i].Free;

  frmSliders.lstParameters.Clear;
  alist := controller.simulator.roadrunner.getGlobalParameterIds;
  for i := 0 to alist.Count - 1 do
    begin
      value := controller.simulator.roadrunner.getValue(alist[i]);
      frmSliders.lstParameters.Items.AddObject(alist[i], TSliderInitialValue.Create(value/10, value*5, value));
    end;

  frmSliders.Show;
  frmSliders.BringToFront;
end;

procedure TFrameSteadyStateControl.collectModelSymbols;
begin
  floatingSpeciesIds := controller.simulator.roadrunner.getFloatingSpeciesIds;
  boundarySpeciesIds := controller.simulator.roadrunner.getBoundarySpeciesIds;
  reactionRatesIds := controller.simulator.roadrunner.getReactionIds;
  ratesOfChangeIds := controller.simulator.roadrunner.getRatesOfChangeIds;
  elasticityIds := controller.simulator.roadrunner.getElasticityIds;
  globalParameters := controller.simulator.roadrunner.getGlobalParameterIds;
end;


procedure TFrameSteadyStateControl.btnConfigureSteadyStateSolverClick(Sender: TObject);
begin
  selectSteadyStateSolver;
end;

procedure TFrameSteadyStateControl.btnGetSteadyStateClick(Sender: TObject);
var
  strList: TStringList;
  i, j: integer;
begin
  if controller.outOfDate then
    loadModelFromMemo;

  updateSteadyStateDisplay;
end;


procedure TFrameSteadyStateControl.updateSteadyStateDisplay;
var
  i: integer;
  strList: TStringList;
  eigen: T2DMatrix;
  floatValues: TArray<double>;
begin
  lblSteadyState.Text := floattostr(controller.simulator.roadrunner.steadyState);
  strList := controller.simulator.roadrunner.getFloatingSpeciesIds;
  try
    for i := 0 to strList.Count - 1 do
      SteadyStateGrid.Cells[0, i] := strList[i];
    floatValues := controller.simulator.roadrunner.getFloatingSpeciesConcentrations;
    for i := 0 to strList.Count - 1 do
      SteadyStateGrid.Cells[1, i] := Format('%6.8f', [floatValues[i]]);
    SteadyStateGrid.Repaint;
  finally
    strList.Free;
  end;
  eigen := controller.simulator.roadrunner.getEigenvalues;
  try
    for i := 0 to eigen.r - 1 do
      begin
        gridEigenValues.Cells[0, i] := Format('%8.6f', [eigen[i, 0]]);
        gridEigenValues.Cells[1, i] := Format('%8.6f', [eigen[i, 1]]);
      end;
    gridEigenValues.Repaint;
  finally
    eigen.Free;
  end;

  if Assigned (frmMoreSteadyState) then
     begin
     if frmMoreSteadyState.visible then
        frmMoreSteadyState.btnSolveClick(nil);
     end;
end;


end.
