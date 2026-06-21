unit SkPlotPaintBoxRegister;

interface

Uses Classes, SkPlotPaintBox;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ComponentLibrary', [TSkPlotPaintBox]);
end;


end.
