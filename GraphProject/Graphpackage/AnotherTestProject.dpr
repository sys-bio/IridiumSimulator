program AnotherTestProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufAnotherMainTest in 'ufAnotherMainTest.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
