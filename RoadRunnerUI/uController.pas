unit uController;

interface

Uses SysUtils,
     Classes,
     Rtti,
     FMX.DialogService,
     uModel,
     uSimulator,
     uScanArguments,
     uViewer,
     uFormViewer,
     uViewerTypes,
     uModelInputManager,
     Generics.Collections,
     ufFrameViewerBase,
     uForth,
     uFStack,
     uRRTypes,
     uRR2DSimpleMatrix;

type
  TListOfViewers = TList<TFrameViewerBase>;
  TListOfFormViewers = TList<TFormViewer>;

  TDataStack = class (TStack<T2DMatrix>)
  end;

  TController = class (TObject)
      outOfDate : boolean;
      simulator : TSimulator;
      modelInputManager : TModelInputManager;

      listOfViewers : TListOfViewers;          // Viewers that have their own form
      listOfFormViewers : TListOfFormViewers;  // viewer embedded in the main form

      viewerPackage : TViewerPackage;          // Gets passed to viewers
      dataStack : TDataStack;

      sed2 : TForth;

      plotViewer : TObject;  // TPlotFrameViewer

      procedure simulateCallBack (stack : TFStack; timeStart, timeEnd : double; numPoints : integer);
      procedure resetCallBack;
      procedure plotCallBack;
      procedure setSymbolCallBack;
      procedure getSymbolCallBack;

      function  getTimeStart : double;
      procedure setTimeStart (timeStart : double);
      procedure setTimeEnd (timeEnd : double);
      procedure setNumberOfPoints (numberOfPoints : integer);
      procedure setSelectionList (selectionList : TStringList);

      procedure setXAxisTitle (atitle : string);
      procedure setPalette (palette : string);
      procedure setShowLegend (value : boolean);

      procedure loadSBMLModelFromString (sbmlStr : string; conservedMoietyAnalysis : boolean);
      function  getAntimony : string;
      function  getSBML : string;

      procedure RegisterViewer (viewer: TFrameViewerBase);
      procedure RegisterFormViewer (viewer: TFormViewer);

      procedure viewerModelHasChanged (sender: TObject);
      procedure updateViewers;
      procedure clearViewers;
      procedure viewerSetProperty (name : string; value : TValue);

      procedure plot (m : T2DMatrix);
      procedure addPlot (m : T2DMatrix);
      procedure clearPlot;

      function  runTimeCourseSimulationAndPlot : TModelErrorState;
      function  runTimeCourseSimulationNoPlot : TModelErrorState;
      procedure runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
      procedure runSteadyStateScan (selectionList : TStringList; scanArguments : TScanArguments);
      procedure modelChanged;
      constructor Create;
      destructor  Destroy; override;
 end;

implementation

Uses FMX.Dialogs, uAntimonyAPI, uPlotFrameViewer;


constructor TController.Create;
begin
  inherited;
  simulator := TSimulator.Create;
  modelInputManager := TModelInputManager.Create;
  outOfDate := True;

  viewerPackage := TViewerPackage.Create;
  dataStack := TDataStack.Create;

  listOfViewers := TListOfViewers.Create;
  listOfFormViewers := TListOfFormViewers.Create;

  sed2 := TForth.Create;
  sed2.simulateCallBack := simulateCallBack;
  sed2.resetCallBack := resetCallBack;
  sed2.plotCallBack := plotCallBack;
  sed2.setSymbol := setSymbolCallBack;
  sed2.getSymbol := getSymbolCallBack;
end;


destructor TController.Destroy;
begin
  viewerPackage.Free;
  listOfViewers.Free;
  listOfFormViewers.Free;
  modelInputManager.Free;
  simulator.Free;
  dataStack.free;
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

// ------------------------------------------------------------------------------------


procedure TController.simulateCallBack (stack : TFStack; timeStart, timeEnd : double; numPoints : integer);
var modelErrorState : TModelErrorState;
    st : TStackElement2;
begin
  setTimeStart (timeStart);
  setTimeEnd (timeEnd);
  setNumberOfPoints (numPoints);
  modelErrorState := runTimeCourseSimulationNoPlot;  // This does push result on to stack, we we must here

  st.elementType := etMatrix;
  st.mValue := simulator.simulationData;
  stack.push (st);
end;


procedure TController.resetCallBack;
begin
  simulator.roadrunner.reset();
end;


procedure TController.plotCallBack;
begin
  updateViewers;
end;


procedure TController.setSymbolCallBack;
var symbolName : string;
    value : TStackElement2;
    symbolValue : double;
begin
  symbolName := sed2.popValue.sValue;
  value := sed2.popValue;
  symbolValue := sed2.getScalar (value);
  simulator.roadrunner.setValue(symbolName, symbolValue);
end;


procedure TController.getSymbolCallBack;
var symbolName : string;
    value : TStackElement2;
begin
  symbolName := sed2.popValue.sValue;
  value.dValue := simulator.roadrunner.getValue(symbolName);
  value.elementType := etDouble;
  sed2.stack.push(value);
end;


// ------------------------------------------------------------------------------------

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
  // HMS
  //if (plotViewer as TPlotFrameViewer).plt.dimGraph then
  //   (plotViewer as TPlotFrameViewer).plt.dimGraph := False;

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


procedure TController.loadSBMLModelFromString (sbmlStr : string; conservedMoietyAnalysis : boolean);
begin
  simulator.roadrunner.setComputeAndAssignConservationLaws(conservedMoietyAnalysis);
  if not simulator.loadSBMLFromString(sbmlStr) then
      raise exception.Create(simulator.roadrunner.getLastError());
end;


function TController.getSBML : string;
var  modelErrorState: TModelErrorState;
begin
   modelErrorState := uAntimonyAPI.getSBMLFromAntimony(modelInputManager.antimonyStr);
   if not modelErrorState.ok then
      begin
      TDialogService.ShowMessage(modelErrorState.errMsg);
      Exit();
      end;
   result := modelErrorState.sbmlStr;
end;


function TController.getAntimony : string;
begin
  result:= modelInputManager.antimonyStr;
end;



function TController.runTimeCourseSimulationAndPlot : TModelErrorState;
var st : TStackElement2;
begin
  try
    simulator.simulate();
    st.elementType := etMatrix;
    st.mValue := simulator.simulationData;
    sed2.stack.push (st);
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


function TController.runTimeCourseSimulationNoPlot : TModelErrorState;
begin
  try
    simulator.simulate();
    // No need to push results on to stack, the caller should do this.
  except
    on e: exception do
       begin
       result.ok := False;
       result.errMsg := e.Message;
       exit;
       end;
  end;
end;


procedure TController.runTimeCourseScan (selectionList : TStringList; scanArguments : TScanArguments);
var i : integer;
    st : TStackelement2;
begin
  simulator.runTimeCourseScan (selectionList, scanArguments);
  st.elementType := etMatrix;
  st.mValue := simulator.simulationData;
  sed2.stack.Push(st);

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
    st : TStackelement2;
begin
  simulator.runSteadyStateScan(selectionList, scanArguments);

  st.elementType := etMatrix;
  st.mValue := simulator.simulationData;
  sed2.stack.Push(st);

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
  viewerModelHasChanged (self);
end;


procedure TController.plot (m : T2DMatrix);
//var i : integer;
begin
  (plotViewer as TPlotFrameViewer).plot(self, m, viewerPackage);
  //for i := 0 to listOfViewers.Count - 1 do
  //    if listOfViewers[i].viewerType = vtPlot then
   //      listOfViewers[0].OnViewerViewData (self, m, viewerPackage);
end;


procedure TController.addPlot (m : T2DMatrix);
//var i : integer;
begin
  (plotViewer as TPlotFrameViewer).addPlot(self, m, viewerPackage);
  //for i := 0 to listOfViewers.Count - 1 do
  //    if listOfViewers[i].viewerType = vtPlot then
   //      listOfViewers[0].OnViewerViewData (self, m, viewerPackage);
end;


procedure TController.clearPlot;
begin
  (plotViewer as TPlotFrameViewer).clearPlot;
end;

end.
