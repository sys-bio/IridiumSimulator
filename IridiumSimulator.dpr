program IridiumSimulator;

uses
  System.StartUpCopy,
  FMX.Skia,
  FMX.Types,
  FMX.Forms,
  FMX.Styles,
  ufMain in 'ufMain.pas' {frmMain},
  uComplex in '..\..\CommonCode\libRoadRunner\uComplex.pas',
  uEigenvalues in '..\..\CommonCode\libRoadRunner\uEigenvalues.pas',
  uIMSLLib in '..\..\CommonCode\libRoadRunner\uIMSLLib.pas',
  uJVector in '..\..\CommonCode\libRoadRunner\uJVector.pas',
  uMatrix in '..\..\CommonCode\libRoadRunner\uMatrix.pas',
  uRoadRunner.API in '..\..\CommonCode\libRoadRunner\uRoadRunner.API.pas',
  uRoadRunner in '..\..\CommonCode\libRoadRunner\uRoadRunner.pas',
  uRR2DSimpleMatrix in '..\..\CommonCode\libRoadRunner\uRR2DSimpleMatrix.pas',
  uRRList in '..\..\CommonCode\libRoadRunner\uRRList.pas',
  uRRTypes in '..\..\CommonCode\libRoadRunner\uRRTypes.pas',
  uAntimonyAPI in 'uAntimonyAPI.pas',
  uCommonTypes in 'uCommonTypes.pas',
  uFrameTimeCourse in 'uFrameTimeCourse.pas' {FrameTimeCourse: TFrame},
  uFrameSteadyState in 'uFrameSteadyState.pas' {FrameSteadyState: TFrame},
  uFrameSliderContainer in 'uFrameSliderContainer.pas' {FrameSliderContainer: TFrame},
  uModelSession in 'uModelSession.pas',
  uAnalysisTypes in 'uAnalysisTypes.pas',
  uColorList in 'uColorList.pas',
  uFrameParameterScan in 'uFrameParameterScan.pas' {FrameParameterScan: TFrame},
  uFontHandling in 'uFontHandling.pas',
  FMX.RichEdit.Style in 'RichMemo\FMX.RichEdit.Style.pas',
  FMX.StyledContextMenu in 'RichMemo\FMX.StyledContextMenu.pas',
  SpellChecker in 'RichMemo\SpellChecker.pas',
  Syntax.Code.Antimony in 'RichMemo\Syntax.Code.Antimony.pas',
  Syntax.Code in 'RichMemo\Syntax.Code.pas',
  uBuiltInModels in 'uBuiltInModels.pas',
  Syntax.Code.Pascal in 'RichMemo\Syntax.Code.Pascal.pas',
  ufBar3DWindow in 'ufBar3DWindow.pas' {frmBar3D},
  U3DBarGraph in '..\T3DBarGraph-main\U3DBarGraph.pas',
  ufAbout in 'ufAbout.pas' {frmAbout},
  SkPlotPaintBox in '..\PlottingComponent\Source\SkPlotPaintBox.pas',
  SkPlotPaintBoxRegister in '..\PlottingComponent\Source\SkPlotPaintBoxRegister.pas',
  uColorManager in '..\PlottingComponent\Source\uColorManager.pas',
  uCSVReaderForPlotter in '..\PlottingComponent\Source\uCSVReaderForPlotter.pas',
  uLabelledTrackBar in '..\PlottingComponent\Source\uLabelledTrackBar.pas',
  uMathParser in '..\PlottingComponent\Source\uMathParser.pas',
  uPlotDefaults in '..\PlottingComponent\Source\uPlotDefaults.pas',
  uPlotJsonUtils in '..\PlottingComponent\Source\uPlotJsonUtils.pas',
  uPlotMapper in '..\PlottingComponent\Source\uPlotMapper.pas',
  uPlotSeries in '..\PlottingComponent\Source\uPlotSeries.pas',
  uMySplitter in 'uMySplitter.pas',
  ufConfigureCVODE in 'ufConfigureCVODE.pas' {frmConfigCVODE},
  ufConfigureSteadyState in 'ufConfigureSteadyState.pas' {frmConfigSteadyState};

{$R *.res}

begin
  {$IFDEF MACOS}
  //FMX.Types.GlobalUseMetal := False;
  //FMX.Types.GlobalEventDrivenDisplayUpdates := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
