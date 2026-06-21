unit ufMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes,
  System.UIConsts,
  System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, SkPlotPaintBox,
  Skia, FMX.Skia,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  uPlotSeries;

type
  TfrmMain = class(TForm)
    btnPlotData: TButton;
    btnLoaddata: TButton;
    OpenDialog1: TOpenDialog;
    Plot: TSkPlotPaintBox;
    procedure btnLoaddataClick(Sender: TObject);
    procedure btnPlotDataClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.btnLoaddataClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
     begin
     Plot.LoadData(OpenDialog1.FileName);
     end;
end;

procedure TfrmMain.btnPlotDataClick(Sender: TObject);
var Series : TPlotSeries;
    i, N : Integer;
begin
  N := 60;
  Series := TPlotSeries.Create('y1', claRed);

  for i := 0 to N - 1 do
      Series.AddXY (i/5, sin (i/5));
  Plot.AddSeries(Series);

  N := 24;
  Series := TPlotSeries.Create('y2', claBlue);
  for i := 0 to N - 1 do
      Series.AddXY (i/2, cos (i/2));
  Plot.AddSeries(Series);

  Plot.Redraw;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  //Plot := TSkPlotPaintBox.Create (Self);
  //Plot.Parent := Self;
  //Plot.Width := 350;
  //Plot.Height := 300;
end;

end.
