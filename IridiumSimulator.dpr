program IridiumSimulator;

uses
  System.StartUpCopy,
  FMX.Skia,
  FMX.Types,
  FMX.Forms,
  FMX.Styles,
  ufMain in 'ufMain.pas' {frmMain},
  uRoadRunner.API in '..\libRoadRunner_Delphi_Bindings\uRoadRunner.API.pas',
  uRoadRunner in '..\libRoadRunner_Delphi_Bindings\uRoadRunner.pas',
  uRR2DSimpleMatrix in '..\libRoadRunner_Delphi_Bindings\uRR2DSimpleMatrix.pas',
  uRRList in '..\libRoadRunner_Delphi_Bindings\uRRList.pas',
  uRRTypes in '..\libRoadRunner_Delphi_Bindings\uRRTypes.pas',
  uAntimonyTypes in '..\libAntimony_Delphi_Bindings\uAntimonyTypes.pas',
  uAntimonyRaw in '..\libAntimony_Delphi_Bindings\uAntimonyRaw.pas',
  uAntimonyAPI in '..\libAntimony_Delphi_Bindings\uAntimonyAPI.pas',
  RateLaw.Types in '..\ModelCheckerLib\RateLaw.Types.pas',
  RateLaw.Ast in '..\ModelCheckerLib\RateLaw.Ast.pas',
  RateLaw.Parser in '..\ModelCheckerLib\RateLaw.Parser.pas',
  RateLaw.Canonical in '..\ModelCheckerLib\RateLaw.Canonical.pas',
  RateLaw.BuiltInLaws in '..\ModelCheckerLib\RateLaw.BuiltInLaws.pas',
  RateLaw.Diff in '..\ModelCheckerLib\RateLaw.Diff.pas',
  RateLaw.Registry in '..\ModelCheckerLib\RateLaw.Registry.pas',
  RateLaw.Generative in '..\ModelCheckerLib\RateLaw.Generative.pas',
  RateLaw.Bind in '..\ModelCheckerLib\RateLaw.Bind.pas',
  RateLaw.Associate in '..\ModelCheckerLib\RateLaw.Associate.pas',
  RateLaw.Eval in '..\ModelCheckerLib\RateLaw.Eval.pas',
  RateLaw.Dynamic in '..\ModelCheckerLib\RateLaw.Dynamic.pas',
  RateLaw.Report in '..\ModelCheckerLib\RateLaw.Report.pas',
  RateLaw.Static in '..\ModelCheckerLib\RateLaw.Static.pas',
  ufRateLawOptions in 'ufRateLawOptions.pas' {frmRateLawOptions},
  uRateLawModelSource in 'uRateLawModelSource.pas',
  uCommonTypes in 'uCommonTypes.pas',
  uFrameTimeCourse in 'uFrameTimeCourse.pas' {FrameTimeCourse: TFrame},
  uFrameSteadyState in 'uFrameSteadyState.pas' {FrameSteadyState: TFrame},
  uFrameSliderContainer in 'uFrameSliderContainer.pas' {FrameSliderContainer: TFrame},
  uModelSession in 'uModelSession.pas',
  uAnalysisTypes in 'uAnalysisTypes.pas',
  uColorList in 'uColorList.pas',
  uFrameParameterScan in 'uFrameParameterScan.pas' {FrameParameterScan: TFrame},
  uFontHandling in 'uFontHandling.pas',
  uBuiltInModels in 'uBuiltInModels.pas',
  uBioModelsCache in 'uBioModelsCache.pas',
  ufBar3DWindow in 'ufBar3DWindow.pas' {frmBar3D},
  U3DBarGraph in '..\T3DBarGraph-main\U3DBarGraph.pas',
  ufAbout in 'ufAbout.pas' {frmAbout},
  SkPlotPaintBox in '..\RhodyComponents\PlottingComponent\Source\SkPlotPaintBox.pas',
  SkPlotPaintBoxRegister in '..\RhodyComponents\PlottingComponent\Source\SkPlotPaintBoxRegister.pas',
  uColorManager in '..\RhodyComponents\PlottingComponent\Source\uColorManager.pas',
  uCSVReaderForPlotter in '..\RhodyComponents\PlottingComponent\Source\uCSVReaderForPlotter.pas',
  uLabelledTrackBar in '..\RhodyComponents\PlottingComponent\Source\uLabelledTrackBar.pas',
  uMathParser in '..\RhodyComponents\PlottingComponent\Source\uMathParser.pas',
  uPlotDefaults in '..\RhodyComponents\PlottingComponent\Source\uPlotDefaults.pas',
  uPlotJsonUtils in '..\RhodyComponents\PlottingComponent\Source\uPlotJsonUtils.pas',
  uPlotMapper in '..\RhodyComponents\PlottingComponent\Source\uPlotMapper.pas',
  uPlotSeries in '..\RhodyComponents\PlottingComponent\Source\uPlotSeries.pas',
  uMetaSymbolProvider in 'uMetaSymbolProvider.pas',
  uMetaScriptGen in 'uMetaScriptGen.pas',
  uPreferences in 'uPreferences.pas',
  uMetaExperiments in 'uMetaExperiments.pas',
  uMetaSelector in 'uMetaSelector.pas',
  uMetaSetValues in 'uMetaSetValues.pas',
  uMetaOutput in 'uMetaOutput.pas',
  uFrameMetadata in 'uFrameMetadata.pas' {FrameMetadata: TFrame},
  uMySplitter in 'uMySplitter.pas',
  ufConfigureCVODE in 'ufConfigureCVODE.pas' {frmConfigCVODE},
  ufConfigureSteadyState in 'ufConfigureSteadyState.pas' {frmConfigSteadyState},
  ufPlotEditor in '..\RhodyComponents\PlottingComponent\Source\ufPlotEditor.pas' {FrmPlotEditor},
  Sim.Meta.Ast in '..\Antimony_MetaData_Library\Sim.Meta.Ast.pas',
  Sim.Meta.Lexer in '..\Antimony_MetaData_Library\Sim.Meta.Lexer.pas',
  Sim.Meta.Model in '..\Antimony_MetaData_Library\Sim.Meta.Model.pas',
  Sim.Meta.Omex in '..\Antimony_MetaData_Library\Sim.Meta.Omex.pas',
  Sim.Meta.Parser in '..\Antimony_MetaData_Library\Sim.Meta.Parser.pas',
  Sim.Meta in '..\Antimony_MetaData_Library\Sim.Meta.pas',
  Sim.Meta.Python in '..\Antimony_MetaData_Library\Sim.Meta.Python.pas',
  Sim.Meta.Registry in '..\Antimony_MetaData_Library\Sim.Meta.Registry.pas',
  Sim.Meta.SedML.Export in '..\Antimony_MetaData_Library\Sim.Meta.SedML.Export.pas',
  Sim.Meta.SedML.Types in '..\Antimony_MetaData_Library\Sim.Meta.SedML.Types.pas',
  Sim.Meta.TestCorpus in '..\Antimony_MetaData_Library\Sim.Meta.TestCorpus.pas',
  Sim.Meta.Types in '..\Antimony_MetaData_Library\Sim.Meta.Types.pas',
  Sim.Meta.Validate in '..\Antimony_MetaData_Library\Sim.Meta.Validate.pas',
  Sim.Meta.Writer in '..\Antimony_MetaData_Library\Sim.Meta.Writer.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  {$IFDEF MACOS}
  //FMX.Types.GlobalUseMetal := False;
  //FMX.Types.GlobalEventDrivenDisplayUpdates := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TFrmPlotEditor, FrmPlotEditor);
  Application.Run;
end.
