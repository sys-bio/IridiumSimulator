unit uFrameScanControl;

// Handles the parameter scanning interface

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  uController,
  FMX.Controls.Presentation,
  FMX.DialogService,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Edit,
  FMX.TreeView,
  FMX.Objects,
  URRList;

type
  TFrameScanControl = class(TFrame)
    pnlScanUpper: TLayout;
    Label5: TLabel;
    pnlScanLayout: TLayout;
    Rectangle3: TRectangle;
    btnWhatToPlot: TButton;
    btnUpdate: TButton;
    lstScanSelectionList: TListBox;
    btnStopScan: TButton;
    btnStartScan: TButton;
    GroupBox5: TGroupBox;
    rdoScanTimeCourse: TRadioButton;
    rdoScanSteadyState: TRadioButton;
    lblScanTimeStart: TLabel;
    lblScanTimeEnd: TLabel;
    lblScanNumPoints: TLabel;
    edtTimeStartScan: TEdit;
    edtTimeEndScan: TEdit;
    edtTimeCouseNumOfPointsScan: TEdit;
    GroupBox6: TGroupBox;
    Label6: TLabel;
    cboScan1: TComboBox;
    lblScanMin1: TLabel;
    edtMin1: TEdit;
    edtMax1: TEdit;
    lblScanMax1: TLabel;
    lblScanNumPoints1: TLabel;
    edtNumScanValues: TEdit;
    chkLogScan1: TCheckBox;
    chkUseAList: TCheckBox;
    edtList1: TEdit;
    chkEnableTwoParameterScan: TCheckBox;
    chkShowLegend: TCheckBox;
    GroupBox7: TGroupBox;
    chkScanGraphOutput: TCheckBox;
    chkScanTableOutput: TCheckBox;
    cboColorPalette: TComboBox;
    Label11: TLabel;
    btnScanPython: TButton;
    procedure btnWhatToPlotClick(Sender: TObject);
    procedure btnStartScanClick(Sender: TObject);
    procedure lstScanSelectionListChange(Sender: TObject);
    procedure btnStopScanClick(Sender: TObject);
    procedure rdoScanTimeCourseChange(Sender: TObject);
    procedure rdoScanSteadyStateChange(Sender: TObject);
    procedure chkUseAListChange(Sender: TObject);
    procedure chkShowLegendChange(Sender: TObject);
    procedure cboColorPaletteChange(Sender: TObject);
    procedure btnScanPythonClick(Sender: TObject);
  private
    { Private declarations }
    floatingSpeciesIds: TStringList;
    boundarySpeciesIds: TStringList;
    reactionRatesIds: TStringList;
    ratesOfChangeIds: TStringList;
    elasticityIds: TRRList; // Use RRList because its a nested list
    globalParameters : TStringList;
    stopScan: boolean;
    selectedPalette: string;

    procedure scanTimeCourse;
    procedure scanSteadyState;
    procedure loadModelFromMemo;
    procedure collectModelSymbols;
    procedure bringUpSelectionForm(selectionList: TStringList; includeTime: boolean);
  public
    { Public declarations }
    stylebook1 : TStyleBook;
    controller : TController;
    procedure initializeScanUserInterface;
  end;


implementation

{$R *.fmx}

uses StrUtils,
     IOUtils,
     uRRTypes,
     uScanArguments,
     ufSelectionChoices,
     ufFloatingPlotViewer,
     ufTextViewer;

procedure TFrameScanControl.loadModelFromMemo;
var
  sbmlStr: string;
begin
  sbmlStr := controller.modelInputManager.getSBMLFromAntimony(controller.modelInputManager.modelMemo.Lines.Text);
  controller.loadSBMLModel(sbmlStr);

  controller.simulator.roadrunner.setComputeAndAssignConservationLaws(true);
  collectModelSymbols;
  //if Assigned(frmFloatingPlotViewer) then
  //   frmFloatingPlotViewer.initializeFloatingGraph;
  controller.outOfDate := false;
end;


procedure TFrameScanControl.lstScanSelectionListChange(Sender: TObject);
begin
  lstScanSelectionList.Items.Delete(lstScanSelectionList.itemindex);
end;

procedure TFrameScanControl.rdoScanSteadyStateChange(Sender: TObject);
begin
  edtTimeStartScan.enabled := false;
  edtTimeEndScan.enabled := false;
  edtTimeCouseNumOfPointsScan.enabled := false;
  lblScanTimeStart.enabled := false;
  lblScanTimeEnd.enabled := false;
  lblScanNumPoints.enabled := false;
  chkEnableTwoParameterScan.enabled := true;
end;

procedure TFrameScanControl.rdoScanTimeCourseChange(Sender: TObject);
begin
  edtTimeStartScan.enabled := true;
  edtTimeEndScan.enabled := true;
  edtTimeCouseNumOfPointsScan.enabled := true;
  lblScanTimeStart.enabled := true;
  lblScanTimeEnd.enabled := true;
  lblScanNumPoints.enabled := true;
  chkEnableTwoParameterScan.enabled := false;
end;

procedure TFrameScanControl.initializeScanUserInterface;
var
  list: TStringList;
  i: integer;
  str: AnsiString;
begin
  collectModelSymbols;
  lstScanSelectionList.Clear;
  // masterColorList.restart;
  list := controller.simulator.roadrunner.getFloatingSpeciesIds;
  try
    lstScanSelectionList.Items.Assign(list);
  finally
    list.Free;
  end;
  cboScan1.Clear;
  list := controller.simulator.roadrunner.getGlobalParameterIds;
  try
    cboScan1.Items.Assign(list);
    // cboScan2.Items.Add (AnsiString (HostAPI.strArray.getString (list, i)));
  finally
    list.Free;
  end;

  list := controller.simulator.roadrunner.getBoundarySpeciesIds;
  try
    for i := 0 to list.Count - 1 do
      begin
        cboScan1.Items.Add(AnsiString(list[i]));
        // cboScan2.Items.Add (AnsiString (HostAPI.strArray.getString (list, i)));
      end;
  finally
    list.Free;
  end;

  list := controller.simulator.roadrunner.getFloatingSpeciesInitialConditionIds;
  try
    for i := 0 to list.Count - 1 do
      begin
        cboScan1.Items.Add(AnsiString(list[i]));
        // cboScan2.Items.Add (AnsiString (HostAPI.strArray.getString (list, i)));
      end;
  finally
    list.Free;
  end;

  cboScan1.itemindex := 0;
  // cboScan2.itemIndex := 0;
end;


procedure TFrameScanControl.btnScanPythonClick(Sender: TObject);
var astr, tmp : string;
    i : integer;
begin
  astr := 'import tellurium as te' + sLineBreak +
          'import matplotlib.pyplot as plt' + sLineBreak +
          'import math' + sLineBreak + sLineBreak +

          'r = te.loada (''''''' + sLineBreak +

          controller.modelInputManager.modelMemo.Text + sLineBreak +

  ''''''')' + sLineBreak +

  'showLegend = False' + sLineBreak +
  'k = ' + edtMin1.Text + sLineBreak +
  'numScans = ' + edtNumScanValues.text + sLineBreak +
  'stepSize = (' + edtMax1.Text + '-' + edtMin1.Text + ')/numScans' + sLineBreak +

  'timeEnd = 20' + sLineBreak +

  'selection = [''time''';
  if lstScanSelectionList.Count = 0 then
     exit;

  for i := 0 to lstScanSelectionList.Items.Count - 1 do
      tmp := tmp + ', ' + '''' + lstScanSelectionList.Items[i] + '''';

  tmp := tmp + ']';
  astr := astr + tmp + sLineBreak;

  astr := astr +
  'for i in range (numScans):' + sLineBreak;
  astr := astr +
       '  r.setValue (''' + cboScan1.items[cboScan1.ItemIndex]  + ''', k)' + sLineBreak;
  astr := astr +
       '  r.reset()' + sLineBreak;
  astr := astr +
       '  m = r.simulate (0, timeEnd, 100, selection)' + sLineBreak;
  astr := astr +
       '  for j in range (len (selection) - 1):' + sLineBreak;
  astr := astr +
       '      plt.plot (m[selection[0]], m[selection[j+1]], label=''' + 'k=' + '''+str(math.trunc (k*1000)/1000))' + sLineBreak;
  astr := astr +
       '      k = k + stepSize' + sLineBreak;
  astr := astr +
       '      plt.xlabel(selection[0])' + sLineBreak;
  astr := astr + 'if showLegend:' + sLineBreak;
  astr := astr + '   plt.legend()' + sLineBreak;
  frmTextViewer := TfrmTextViewer.Create(nil);
  try
    frmTextViewer.StyleBook := stylebook1;
    frmTextViewer.textmemo.Text := astr;
    frmTextViewer.ShowModal;
  finally
    frmTextViewer.free;
  end;
  //TFile.WriteAllText('c:\\tmp\\astr.py', astr);
end;

procedure TFrameScanControl.btnStartScanClick(Sender: TObject);
begin
  if controller.outOfDate then
    begin
      loadModelFromMemo;
      initializeScanUserInterface;
    end;

  if rdoScanSteadyState.IsChecked then
    scanSteadyState
  else
    scanTimeCourse;
end;


procedure TFrameScanControl.btnStopScanClick(Sender: TObject);
begin
  stopScan := true;
end;


procedure MyMessageDialog (msg : string);
begin
TDialogService.MessageDialog(msg,
                TMsgDlgType.mtInformation,
                FMX.Dialogs.mbOKCancel,
                TMsgDlgBtn.mbNo,
                0,
      procedure(const AResult: System.UITypes.TModalResult)
      begin
        //if AResult = mrOK Then
        //  ShowMessage('Yes was selected')
        //else if AResult = mrNo Then
        //  ShowMessage('No was selected')
        //else if AResult = mrCancel Then
        //  ShowMessage('Cancel was selected');

      end);
end;




procedure TFrameScanControl.scanTimeCourse;
var
  selectionList: TStringList;
  i: integer;
  scanValues: TDoubleArray;
  scanArguments : TScanArguments;
begin
  if strtoint(edtNumScanValues.Text) <= 0 then
    begin
      MyMessageDialog('Number of points cannnot be zero or negative');
      exit;
    end;

  try
    selectionList := TStringList.Create;
    selectionList.Add('Time');
    for i := 0 to lstScanSelectionList.Count - 1 do
      selectionList.Add(lstScanSelectionList.Items[i]);

    if not chkEnableTwoParameterScan.IsChecked then
      begin
        // Single scan
        scanArguments := TScanArguments.Create;
        scanArguments.FParameterId := cboScan1.Items[cboScan1.itemindex];
        scanArguments.FTimeStart := strtofloat(edtTimeStartScan.Text);
        scanArguments.FTimeEnd := strtofloat(edtTimeEndScan.Text);
        scanArguments.FNumberOfPoints := strtoint(edtTimeCouseNumOfPointsScan.Text);
        scanArguments.FLogScan := chkLogScan1.IsChecked;
        scanArguments.minValue := strtofloat(edtMin1.Text);
        scanArguments.maxValue := strtofloat(edtMax1.Text);
        scanArguments.FNumberOfScans := strtoint(edtNumScanValues.Text);
        if chkUseAList.IsChecked then
          begin
            scanArguments.FScanValuesType := TScanType.stList;
            scanArguments.listOfScanValues := edtList1.Text;
          end
        else
          scanArguments.FScanValuesType := TScanType.stRange;

        controller.viewerPackage.showLegend := chkShowLegend.IsChecked;
        controller.runTimeCourseScan(selectionList, scanArguments);
      end
    else
      begin
        // doTimeCourseDoubleParameterScan (cboScan1.Items[cboScan1.ItemIndex], cboScan2.Items[cboScan2.ItemIndex]);
      end;
  finally
    selectionList.Free;
    scanArguments.Free;
  end;
end;


procedure TFrameScanControl.scanSteadyState;
var
  selectionList: TStringList;
  i: integer;
  scanValues1, scanValues2: TDoubleArray;
  m, scanResult: T2DMatrix;
  parameterStr: string;
  scan: TScanArguments;
begin
  selectionList := TStringList.Create;
  parameterStr := cboScan1.Items[cboScan1.itemindex];
  scan := TScanArguments.Create;
  try

    // Single scan
    selectionList.Add(parameterStr);
    for i := 0 to lstScanSelectionList.Count - 1 do
        selectionList.Add(lstScanSelectionList.Items[i]);
    scan.FParameterId := parameterStr;
    scan.FNumberOfScans := strtoint(edtNumScanValues.Text);
    scan.FLogScan := chkLogScan1.IsChecked;
    scan.minValue := strtofloat(edtMin1.Text);
    scan.maxValue := strtofloat(edtMax1.Text);
    if chkUseAList.IsChecked then
      begin
        scan.FScanValuesType := TScanType.stList;
        scan.listOfScanValues := edtList1.Text;
      end
    else
      scan.FScanValuesType := TScanType.stRange;

    controller.runSteadyStateScan (selectionList, scan);

    // else
    // begin
    // // scanValues2 := getScanValues(strtofloat (edtMin2.text), strtofloat (edtMax2.text), strtoint (edtNumScanPoints2.Text),
    // // chkLogScan2.checked, edtManualValues2.Text, chkScanManual2.checked);
    // // for i := 0 to lstOutputScan.Count - 1 do
    // // selectionList.Add (scanSelectionList[i]);
    // //
    // // m := doSteadyStateDoubleParameterScan (cboScan1.Items[cboScan1.ItemIndex], cboScan2.Items[cboScan2.ItemIndex], selectionList, scanValues1, scanValues2);
    // // selectionList.Insert (0, parameterStr);
    // //
    // // tmp := TCMatrix.Create (length (scanValues1), 1);
    // // tmp.setColumnName(1, parameterStr);
    // // for i := 0 to length (scanValues1) - 1 do
    // // tmp[i+1,1] := complex (scanValues1[i], 0);
    // //
    // // scanResult := TCMatrix.Create;
    // // scanResult.augment (tmp, m);
    // //
    // // buildScanChart (selectionList);
    // // UpdateScanViewer (scanResult, 1, selectionList, true);
    // end;

  finally
    selectionList.Free;
    scan.Free;
  end;
end;


procedure TFrameScanControl.btnWhatToPlotClick(Sender: TObject);
var
  i: integer;
begin
  if controller.outOfDate then
    begin
      loadModelFromMemo;
      initializeScanUserInterface;
    end;

  // false = no time as a choice
  bringUpSelectionForm(TStringList(lstScanSelectionList.Items), false);

  lstScanSelectionList.Clear;
  for i := 0 to frmSelectionChoices.lstSelectedItems.Count - 1 do
    lstScanSelectionList.Items.Add(frmSelectionChoices.lstSelectedItems.Items[i]);
  controller.simulator.roadrunner.setTimeCourseSelectionListEx(TStringList(lstScanSelectionList.Items));
end;


procedure TFrameScanControl.cboColorPaletteChange(Sender: TObject);
begin
  selectedPalette := cboColorPalette.Items[cboColorPalette.itemindex];
  controller.setPalette (selectedPalette);
end;


procedure TFrameScanControl.chkShowLegendChange(Sender: TObject);
begin
  if chkShowLegend.IsChecked then
     controller.ViewerSetProperty('legend', true)
  else
     controller.ViewerSetProperty('legend', false)
end;


procedure TFrameScanControl.chkUseAListChange(Sender: TObject);
begin
  if chkUseAList.IsChecked then
    begin
      edtList1.visible := true;
      edtMin1.visible := false;
      edtMax1.visible := false;
      edtNumScanValues.visible := false;
      lblScanMin1.Text := 'Values, space separated';
      lblScanMax1.Text := '';
      lblScanNumPoints1.Text := '';
    end
  else
    begin
      edtList1.visible := false;
      edtMin1.visible := true;
      edtMax1.visible := true;
      edtNumScanValues.visible := true;
      lblScanMin1.Text := 'Min:';
      lblScanMax1.Text := 'Max:';
      lblScanNumPoints1.Text := 'Num of Points:';
    end;
end;


procedure TFrameScanControl.collectModelSymbols;
begin
  floatingSpeciesIds := controller.simulator.roadrunner.getFloatingSpeciesIds;
  boundarySpeciesIds := controller.simulator.roadrunner.getBoundarySpeciesIds;
  reactionRatesIds := controller.simulator.roadrunner.getReactionIds;
  ratesOfChangeIds := controller.simulator.roadrunner.getRatesOfChangeIds;
  elasticityIds := controller.simulator.roadrunner.getElasticityIds;
  globalParameters := controller.simulator.roadrunner.getGlobalParameterIds;
end;


procedure TFrameScanControl.bringUpSelectionForm(selectionList: TStringList; includeTime: boolean);
var
  Item1, Item2, Item3: TTreeViewItem;
  i, j: integer;
begin
  frmSelectionChoices.Free;
  // This is to avoid visual corruption of calling form (reported to qc)
  frmSelectionChoices := TfrmSelectionChoices.Create(nil);
  // try
  for i := 0 to selectionList.Count - 1 do
    frmSelectionChoices.lstSelectedItems.Items.Add(selectionList[i]);

  // ---------------------------------------------------
  if includeTime then
    begin
      Item1 := TTreeViewItem.Create(self);
      Item1.Text := 'Time';
      Item1.Parent := frmSelectionChoices.treeSelection;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Floating Species';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to floatingSpeciesIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := floatingSpeciesIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Boundary Species';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to boundarySpeciesIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := boundarySpeciesIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Reaction Rates';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to reactionRatesIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := reactionRatesIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Rates of Change';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to ratesOfChangeIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := ratesOfChangeIds[i];
      Item2.Parent := Item1;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Elasticities';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to elasticityIds.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := elasticityIds[i].list[0].sValue;
      Item2.Parent := Item1;
      for j := 0 to elasticityIds[i].list[1].list.Count - 1 do
        begin
          Item3 := TTreeViewItem.Create(self);
          Item3.Text := elasticityIds[i].list[1].list[j].sValue;
          Item3.Parent := Item2;
        end;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Eigenvalues';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to controller.simulator.roadrunner.getEigenvalueIds.Count - 1 do
    begin
      // filter out eigen(X), only show eigenReal and eigenImag
      if not StartsText('eigen(', controller.simulator.roadrunner.getEigenvalueIds[i]) then
        begin
          Item2 := TTreeViewItem.Create(self);
          Item2.Text := controller.simulator.roadrunner.getEigenvalueIds[i];
          Item2.Parent := Item1;
        end;
    end;

  // ---------------------------------------------------
  Item1 := TTreeViewItem.Create(self);
  Item1.Text := 'Other';
  Item1.Parent := frmSelectionChoices.treeSelection;

  for i := 0 to globalParameters.Count - 1 do
    begin
      Item2 := TTreeViewItem.Create(self);
      Item2.Text := globalParameters[i];
      Item2.Parent := Item1;
    end;

  frmSelectionChoices.ShowModal;
end;



end.
