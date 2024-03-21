unit uController;

interface

Uses SysUtils,
     Classes,
     Rtti,
     uModel,
     uSimulator,
     uScanArguments,
     uViewer,
     uFormViewer,
     uViewerTypes,
     uModelInputManager,
     Generics.Collections,
     ufFrameViewerBase;

type
  TListOfViewers = TList<TFrameViewerBase>;
  TListOfFormViewers = TList<TFormViewer>;

  TController = class (TObject)
      outOfDate : boolean;
      simulator : TSimulator;
      modelInputManager : TModelInputManager;

      listOfViewers : TListOfViewers;          // Viewers that have their own form
      listOfFormViewers : TListOfFormViewers;  // viewer embedded in the main form

      viewerPackage : TViewerPackage;          // Gets passed to viewers

      function  getTimeStart : double;
      procedure setTimeStart (timeStart : double);
      procedure setTimeEnd (timeEnd : double);
      procedure setNumberOfPoints (numberOfPoints : integer);
      procedure setSelectionList (selectionList : TStringList);

      procedure setXAxisTitle (atitle : string);
      procedure setPalette (palette : string);
      procedure setShowLegend (value : boolean);

      procedure loadSBMLModel (sbmlStr : string; conservedMoietyAnalysis : boolean);

      procedure RegisterViewer (viewer: TFrameViewerBase);
      procedure RegisterFormViewer (viewer: TFormViewer);

      procedure ViewerModelHasChanged (sender: TObject);
      procedure updateViewers;
      procedure clearViewers;
      procedure ViewerSetProperty (name : string; value : TValue);

      function  runTimeCourseSimulation : TModelErrorState;
      procedure runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
      procedure runSteadyStateScan (selectionList : TStringList; scanArguments : TScanArguments);
      procedure modelChanged;
      constructor Create;
      destructor  Destroy; override;
 end;

implementation

constructor TController.Create;
begin
  inherited;
  simulator := TSimulator.Create;
  modelInputManager := TModelInputManager.Create;
  outOfDate := True;

  viewerPackage := TViewerPackage.Create;

  listOfViewers := TListOfViewers.Create;
  listOfFormViewers := TListOfFormViewers.Create;
end;


destructor TController.Destroy;
begin
  viewerPackage.Free;
  listOfViewers.Free;
  listOfFormViewers.Free;
  modelInputManager.Free;
  simulator.Free;
  inherited;
end;

procedure TController.RegisterViewer (viewer: TFrameViewerBase);
begin
  listOfViewers.Add(viewer);
end;


procedure TController.RegisterFormViewer (viewer: TFormViewer);
begin
  listOfFormViewers.Add(viewer);
end;


function TController.getTimeStart : double;
begin
  result := simulator.timeStart;
end;


procedure TController.setTimeStart (timeStart : double);
begin
  simulator.timeStart := timeStart;
end;


procedure TController.setTimeEnd (timeEnd : double);
begin
  simulator.timeEnd := timeEnd;
  viewerPackage.timeEnd := timeEnd;
end;


procedure TController.setNumberOfPoints (numberOfPoints : integer);
begin
  simulator.numberOfPoints := numberOfPoints;
end;


procedure TController.setXAxisTitle (atitle : string);
begin
  viewerPackage.XAxisTitle := atitle;
end;


procedure TController.setPalette (palette : string);
begin
  viewerPackage.palette := palette;
end;

procedure TController.setShowLegend (value : boolean);
begin
  //controller.
  //viewerPackage.showLegend := value;
end;

procedure TController.setSelectionList (selectionList : TStringList);
begin
  simulator.setSelectionList (selectionList);
end;


procedure TController.ViewerModelHasChanged (sender: TObject);
begin
  for var i := 0 to listOfViewers.Count - 1 do
      if Assigned (listOfViewers[i].OnViewerModelHasChanged) then
         listOfViewers[i].OnViewerModelHasChanged (self);
  for var i := 0 to listOfFormViewers.Count - 1 do
      if Assigned (listOfFormViewers[i].OnViewerModelHasChanged) then
         listOfFormViewers[i].OnViewerModelHasChanged (self);
end;


procedure TController.updateViewers;
var i : integer;
begin
  for i := 0 to listOfViewers.Count - 1 do
      if Assigned (listOfViewers[i].OnViewerChange) then
         listOfViewers[i].OnViewerChange (self, viewerPackage);
  for i := 0 to listOfFormViewers.Count - 1 do
      if Assigned (listOfFormViewers[i].OnViewerChange) then
         listOfFormViewers[i].OnViewerChange (self, viewerPackage);
end;

procedure TController.clearViewers;
var i : integer;
begin
  for i := 0 to listOfViewers.Count - 1 do
      if Assigned (listOfViewers[i].OnViewerClear) then
         listOfViewers[i].OnViewerClear (self);
  for i := 0 to listOfFormViewers.Count - 1 do
      if Assigned (listOfFormViewers[i].OnViewerClear) then
         listOfFormViewers[i].OnViewerClear (self);
end;


procedure TController.ViewerSetProperty (name : string; value : TValue);
var i : integer;
begin
  for i := 0 to listOfViewers.Count - 1 do
      begin
      if Assigned (listOfViewers[i].OnViewerSetProperty) then
        listOfViewers[i].OnViewerSetProperty (name, value);
      end;
  for i := 0 to listOfFormViewers.Count - 1 do
      begin
      if Assigned ( listOfFormViewers[i].OnViewerSetProperty) then
         listOfFormViewers[i].OnViewerSetProperty (name, value);
      end;
end;


procedure TController.loadSBMLModel (sbmlStr : string; conservedMoietyAnalysis : boolean);
begin
  simulator.roadrunner.setComputeAndAssignConservationLaws(conservedMoietyAnalysis);
  if not simulator.roadrunner.loadSBMLFromString(sbmlStr) then
      raise exception.Create(simulator.roadrunner.getLastError());
end;


function TController.runTimeCourseSimulation : TModelErrorState;
begin
  try
    simulator.simulate();
  except
    on e: exception do
       begin
       result.ok := False;
       result.errMsg := e.Message;
       exit;
       end;
  end;
  updateViewers;
end;


procedure TController.runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
var i : integer;
begin
  simulator.runTimeCourseScan (selectionList, scanArguments);

  viewerPackage.XAxisTitle := 'Time';
  viewerPackage.XColumnIndex := 0;
  viewerPackage.timeStart := scanArguments.FTimeStart;
  viewerPackage.timeEnd := scanArguments.FTimeEnd;
  //viewerPackage.autoXScale := True;
  //viewerPackage.autoYScale := True;
//
  setLength (viewerPackage.YColumnNames, selectionList.Count);
  for i := 0 to selectionList.Count - 1 do
      viewerPackage.YColumnNames[i] := selectionList[i];

  setLength (viewerPackage.YColumnChoice, selectionList.Count);
  for i := 0 to selectionList.Count - 1 do
      viewerPackage.YColumnChoice[i] := True;
  viewerPackage.YColumnChoice[0] := False;

  updateViewers;
end;



procedure TController.runSteadyStateScan (selectionList : TStringList; scanArguments : TScanArguments);
var i : integer;
begin
  simulator.runSteadyStateScan(selectionList, scanArguments);

  viewerPackage.XAxisTitle := scanArguments.FParameterId;
  viewerPackage.XColumnIndex := 0;
  // HMS viewerPackage.autoXScale := True;

  setLength (viewerPackage.YColumnNames, selectionList.Count);
  for i := 0 to selectionList.Count - 1 do
      viewerPackage.YColumnNames[i] := selectionList[i];

  setLength (viewerPackage.YColumnChoice, selectionList.Count);
  for i := 0 to selectionList.Count - 1 do
      viewerPackage.YColumnChoice[i] := True;
  viewerPackage.YColumnChoice[0] := False;

  updateViewers;
end;


procedure TController.modelChanged;
begin
  outOfDate := True;
  ViewerModelHasChanged (self);
end;


end.
