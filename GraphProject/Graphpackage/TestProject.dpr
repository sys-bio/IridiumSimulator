program TestProject;

uses
  FMX.Forms,
  FMX.Types,
  ufMainTest in 'ufMainTest.pas' {frmMain},
  uCSVUtils in '..\uCSVUtils.pas';

{$R *.res}

begin
  GlobalUseDX10 := false;
  GlobalUseDX10Software := false;
  GlobalUseDirect2D := false;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
