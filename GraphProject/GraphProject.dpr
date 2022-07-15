program GraphProject;

uses
  Forms,
  ufMain in 'ufMain.pas' {frmMain},
  uPlottingPanel in 'uPlottingPanel.pas',
  uSubgraph in 'uSubgraph.pas',
  uCommon in 'uCommon.pas',
  uDataSeries in 'uDataSeries.pas',
  uClipping in 'uClipping.PAS',
  uSymbolDetails in 'uSymbolDetails.pas',
  uLineDetails in 'uLineDetails.pas',
  uGObject in 'uGObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
