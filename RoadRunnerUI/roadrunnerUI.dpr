program roadrunnerUI;

uses
  skia,
  skia.FMX,
  FMX.Forms,
  FMX.Types,
  FMX.Dialogs,
  Classes,
  SysUtils,
  ufMain in 'ufMain.pas' {frmMain},
  uComplex in 'uComplex.pas',
  uJVector in 'uJVector.pas',
  uMatrix in 'uMatrix.pas',
  uEigenvalues in 'uEigenvalues.pas',
  uIMSLLib in 'uIMSLLib.pas',
  ufSelectionChoices in 'ufSelectionChoices.pas' {frmSelectionChoices},
  uPlottingPanel in '..\GraphProject\uPlottingPanel.pas',
  uSubgraph in '..\GraphProject\uSubgraph.pas',
  uRRCommon in '..\GraphProject\uRRCommon.pas',
  ufGraphPackageDialog in '..\GraphProject\Graphpackage\ufGraphPackageDialog.pas' {frmGraphPackageDlg},
  uRRDataSeries in '..\GraphProject\uRRDataSeries.pas',
  uSymbolDetails in '..\GraphProject\uSymbolDetails.pas',
  uLineDetails in '..\GraphProject\uLineDetails.pas',
  uGObject in '..\GraphProject\uGObject.pas',
  uSubGraphCollectionEditor in '..\GraphProject\uSubGraphCollectionEditor.pas',
  uRRUtilities in '..\GraphProject\uRRUtilities.pas',
  uClipping in '..\GraphProject\uClipping.PAS',
  uErrorBars in '..\GraphProject\uErrorBars.pas',
  uBuiltInModels in 'uBuiltInModels.pas',
  ufSliders in 'ufSliders.pas' {frmSliders},
  uSliderCommon in 'uSliderCommon.pas',
  ufConfigSlider in 'ufConfigSlider.pas' {frmConfigSlider},
  uCoyoteCommon in 'uCoyoteCommon.pas',
  uAntimonyAPI in 'uAntimonyAPI.pas',
  FMXDirect2DFix in 'FMXDirect2DFix.pas',
  ufIntegratorOptions in 'ufIntegratorOptions.pas' {frmIntegratorOptions},
  ufSteadyStateOptions in 'ufSteadyStateOptions.pas' {frmSteadyStateOptions},
  ufMoreSteadyState in 'ufMoreSteadyState.pas' {frmMoreSteadyState},
  uColorList in 'uColorList.pas',
  uRRTypes in '..\..\..\CommonCode\libRoadRunner\uRRTypes.pas',
  uRRList in '..\..\..\CommonCode\libRoadRunner\uRRList.pas',
  uRoadRunner.API in '..\..\..\CommonCode\libRoadRunner\uRoadRunner.API.pas',
  uRoadRunner in '..\..\..\CommonCode\libRoadRunner\uRoadRunner.pas',
  uSetup in 'uSetup.pas',
  ufStructuralAnalysis in 'ufStructuralAnalysis.pas' {frmStructuralAnalysis},
  ufExamples in 'ufExamples.pas' {frmExamples},
  ufAbout in 'ufAbout.pas' {frmAbout},
  uParameterScan in 'uParameterScan.pas',
  uNewMatrix in 'uNewMatrix.pas',
  ufRangeFrame in 'ufRangeFrame.pas' {frmRangeFrame: TFrame},
  uModel in 'uModel.pas',
  UComps in '..\ScrollChart\UComps.pas',
  UContainer in '..\ScrollChart\UContainer.pas',
  UDataSource in '..\ScrollChart\UDataSource.pas',
  UGlobalData in '..\ScrollChart\UGlobalData.pas',
  UScrollingChart in '..\ScrollChart\UScrollingChart.pas',
  UStage in '..\ScrollChart\UStage.pas',
  ufScrollChart in 'ufScrollChart.pas' {frmScrollChart},
  uController in 'uController.pas',
  ufFloatingPlotViewer in 'ufFloatingPlotViewer.pas' {frmFloatingPlotViewer},
  uSimulator in 'uSimulator.pas',
  uInputs in 'uInputs.pas',
  uViewer in 'uViewer.pas',
  uFormViewer in 'uFormViewer.pas',
  uPlotFormViewerOld in 'uPlotFormViewerOld.pas',
  uViewerTypes in 'uViewerTypes.pas',
  uTableFormViewerOld in 'uTableFormViewerOld.pas',
  uCommonTypes in 'uCommonTypes.pas',
  uScanArguments in 'uScanArguments.pas',
  uModelInputManager in 'uModelInputManager.pas',
  uFrameFunctionPlotter in 'uFrameFunctionPlotter.pas' {frameFunctionPlotter: TFrame},
  uExpressionEvaluator in 'uExpressionEvaluator.pas',
  uFrameScanControl in 'uFrameScanControl.pas' {FrameScanControl: TFrame},
  uFrameSteadyStateControl in 'uFrameSteadyStateControl.pas' {FrameSteadyStateControl: TFrame},
  ufPreferences in 'ufPreferences.pas' {frmPreferences},
  uConfiguration in 'uConfiguration.pas',
  ufSplash in 'ufSplash.pas' {frmSplash},
  ufTextViewer in 'ufTextViewer.pas' {frmTextViewer},
  ufFrameSplitPanel in 'ufFrameSplitPanel.pas' {frameSplitPanels: TFrame},
  ufMainConfig in 'ufMainConfig.pas',
  ufFrameViewerBase in 'ufFrameViewerBase.pas' {FrameViewerBase: TFrame},
  uTableFrameViewer in 'uTableFrameViewer.pas' {TableFrameViewer: TFrame},
  uPlotFrameViewer in 'uPlotFrameViewer.pas' {PlotFrameViewer: TFrame};

{$R *.res}

var f: TextFile;

begin
  GlobalUseSkia := True;

  //AssignFile (f, '/tmp/iridium1.log');
  //rewrite (f);
  //writeln (f, 'Start logging');
  //closefile (f);

  Application.Initialize;

  frmSplash := TfrmSplash.Create(nil);
  frmSplash.Show;
  Application.ProcessMessages;

  GlobalUseSkia := True;

  try
    TRoadRunnerAPI.Initialize;

  except
    on e: Exception do
       begin
       showmessage (e.Message);
       Halt;
       end;
  end;

  sleep(1000);
  frmSplash.Close;
  frmSplash.Free;

  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmGraphPackageDlg, frmGraphPackageDlg);
  Application.Run;
end.
