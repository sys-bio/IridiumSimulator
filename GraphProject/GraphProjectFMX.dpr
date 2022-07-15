program GraphProjectFMX;

uses
  FMX.Forms,
  ufMain in 'ufMain.pas' {fmMain},
  uSubgraph in 'uSubgraph.pas',
  uSymbolDetails in 'uSymbolDetails.pas',
  uClipping in 'uClipping.PAS',
  uPlottingPanel in 'uPlottingPanel.pas',
  uGObject in 'uGObject.pas',
  uLineDetails in 'uLineDetails.pas',
  uSubGraphCollectionEditor in 'uSubGraphCollectionEditor.pas',
  FMX.Types,
  uCSVReader in 'uCSVReader.pas',
  ufGraphPackageDialog in 'Graphpackage\ufGraphPackageDialog.pas' {frmGraphPackageDlg},
  uRRDataSeries in 'uRRDataSeries.pas',
  uRRCommon in 'uRRCommon.pas',
  uErrorBars in 'uErrorBars.pas',
  uRRUtilities in 'uRRUtilities.pas';

{$R *.res}

begin
  GlobalUseDirect2D := false;
  GlobalUseDX10 := false;

  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfrmGraphPackageDlg, frmGraphPackageDlg);
  Application.Run;
end.
