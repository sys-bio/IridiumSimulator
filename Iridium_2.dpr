program Iridium_2;

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
  uPlotSeries in 'ChartComponent\uPlotSeries.pas',
  uPlotMapper in 'ChartComponent\uPlotMapper.pas',
  SkPlotPaintBox in 'ChartComponent\SkPlotPaintBox.pas',
  uColorManager in 'ChartComponent\uColorManager.pas',
  uCSVReaderForPlotter in 'ChartComponent\uCSVReaderForPlotter.pas',
  uPlotDefaults in 'ChartComponent\uPlotDefaults.pas',
  uFrameTimeCourse in 'uFrameTimeCourse.pas' {FrameTimeCourse: TFrame},
  uFrameSteadyState in 'uFrameSteadyState.pas' {FrameSteadyState: TFrame},
  uFrameSliderContainer in 'uFrameSliderContainer.pas' {FrameSliderContainer: TFrame},
  uModelSession in 'uModelSession.pas',
  uAnalysisTypes in 'uAnalysisTypes.pas',
  ufPlotEditor in 'ChartComponent\ufPlotEditor.pas' {FrmPlotEditor},
  uLabelledTrackBar in 'ChartComponent\uLabelledTrackBar.pas',
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
  ufAbout in 'ufAbout.pas' {frmAbout};

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
