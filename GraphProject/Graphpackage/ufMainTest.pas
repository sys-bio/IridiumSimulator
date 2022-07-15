unit ufMainTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uPlottingPanel, ufGraphPackageDialog, System.Math.Vectors, FMX.Layouts,
  FMX.Controls3D, FMX.Layers3D, FMX.Colors, FMX.Controls.Presentation, uSubGraph;

type
  TfrmMain = class(TForm)
    Label1: TLabel;
    Layout3D1: TLayout3D;
    Layout1: TLayout;
    btnGridLines: TButton;
    btnStartEditor: TButton;
    ComboColorBox: TComboColorBox;
    btnPlot: TButton;
    btnPlotTwoGraphs: TButton;
    Label2: TLabel;
    btnSimPlot: TButton;
    btnSaveBitmap: TButton;
    StyleBook1: TStyleBook;
    Button1: TButton;
    procedure btnGridLinesClick(Sender: TObject);
    procedure RRGraphReportCordinates(mx, my, gx, gy: Single);
    procedure ComboColorBoxChange(Sender: TObject);
    procedure btnStartEditorClick(Sender: TObject);
    procedure btnPlotClick(Sender: TObject);
    procedure btnPlotTwoGraphsClick(Sender: TObject);
    procedure RRGraphReportCoordinates(mx, my, gx, gy: Single);
    procedure btnSimPlotClick(Sender: TObject);
    procedure btnSaveBitmapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    rrGraph : TRRGraph;
    gd : TfrmGraphPackageDlg;
    fireEvent : boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

Uses System.UIConsts, uSymbolDetails, uRRDataSeries, uRRCommon, Generics.Collections;

const
  mydata : Array[0..199,0..1] of double =
((0, 0),
(0.100502512562814, 0.654858689497561),
(0.201005025125628, 1.10602442393467),
(0.301507537688442, 1.47549140186115),
(0.402010050251256, 1.79493977951993),
(0.50251256281407, 2.07890222122281),
(0.603015075376884, 2.33572860670509),
(0.703517587939699, 2.57073061454102),
(0.804020100502513, 2.78706975780874),
(0.904522613065327, 2.98500705844914),
(1.00502512562814, 3.15877057707501),
(1.10552763819095, 3.29052163229521),
(1.20603015075377, 3.34609535198072),
(1.30653266331658, 3.28821259990787),
(1.4070351758794, 3.10941290476299),
(1.50753768844221, 2.84318721844308),
(1.60804020100503, 2.53584212446232),
(1.70854271356784, 2.22234472092164),
(1.80904522613065, 1.92282311928245),
(1.90954773869347, 1.64719991811661),
(2.01005025125628, 1.39964534288251),
(2.1105527638191, 1.18119866399713),
(2.21105527638191, 0.99119354775577),
(2.31155778894472, 0.827993942949151),
(2.41206030150754, 0.689410701934074),
(2.51256281407035, 0.572922533424135),
(2.61306532663317, 0.475884783684774),
(2.71356783919598, 0.395631503163664),
(2.81407035175879, 0.329627886787054),
(2.91457286432161, 0.275536207551304),
(3.01507537688442, 0.231266041464353),
(3.11557788944724, 0.195029316471744),
(3.21608040201005, 0.165333948630253),
(3.31658291457286, 0.140956565753084),
(3.41708542713568, 0.120922932568675),
(3.51758793969849, 0.104472671641926),
(3.61809045226131, 0.0910229986120881),
(3.71859296482412, 0.0801630524539113),
(3.81909547738693, 0.07167026796559),
(3.91959798994975, 0.0655560136840936),
(4.02010050251256, 0.0622133173827576),
(4.12060301507538, 0.062728106758297),
(4.22110552763819, 0.0695516673607884),
(4.32160804020101, 0.0878701348767442),
(4.42211055276382, 0.127981783676471),
(4.52261306532663, 0.207559999103403),
(4.62311557788945, 0.347685495644764),
(4.72361809045226, 0.555465647848115),
(4.82412060301508, 0.810046031615859),
(4.92462311557789, 1.07685313192678),
(5.0251256281407, 1.32811584057967),
(5.12562814070352, 1.54618425422905),
(5.22613065326633, 1.71824643570864),
(5.32663316582915, 1.83250186414181),
(5.42713567839196, 1.87848345326146),
(5.52763819095477, 1.8511774107122),
(5.62814070351759, 1.75572129730582),
(5.7286432160804, 1.60788072159472),
(5.82914572864322, 1.42890897272685),
(5.92964824120603, 1.23903745386039),
(6.03015075376884, 1.05361777803466),
(6.13065326633166, 0.882434849700405),
(6.23115577889447, 0.730660862017173),
(6.33165829145729, 0.600153179545211),
(6.4321608040201, 0.490610177153449),
(6.53266331658291, 0.400423118442916),
(6.63316582914573, 0.327308281602026),
(6.73366834170854, 0.268752683893519),
(6.83417085427136, 0.222325940732369),
(6.93467336683417, 0.185873087769538),
(7.03517587939699, 0.157629794886702),
(7.1356783919598, 0.136293299190031),
(7.23618090452261, 0.12107139348793),
(7.33668341708543, 0.111782262216183),
(7.43718592964824, 0.109073393721281),
(7.53768844221106, 0.114871339033727),
(7.63819095477387, 0.13317295693961),
(7.73869346733668, 0.171150427384906),
(7.8391959798995, 0.239736535794313),
(7.93969849246231, 0.351162116632875),
(8.04020100502513, 0.511043061204537),
(8.14070351758794, 0.710259587366717),
(8.24120603015075, 0.927421742171926),
(8.34170854271357, 1.13928502783194),
(8.44221105527638, 1.32703692573939),
(8.5427135678392, 1.4762649966721),
(8.64321608040201, 1.57542425986855),
(8.74371859296483, 1.61604643518075),
(8.84422110552764, 1.59504554149099),
(8.94472361809045, 1.51717139476288),
(9.04522613065327, 1.39491339243933),
(9.14572864321608, 1.24503369533881),
(9.2462311557789, 1.08406910953812),
(9.34673366834171, 0.925245640241145),
(9.44723618090452, 0.777500121852547),
(9.54773869346734, 0.645897313191479),
(9.64824120603015, 0.532498633917846),
(9.74874371859297, 0.43732813541529),
(9.84924623115578, 0.359133700135415),
(9.94974874371859, 0.295999751062115),
(10.0502512562814, 0.245802267689826),
(10.1507537688442, 0.206505973495009),
(10.251256281407, 0.176386344636118),
(10.3517587939699, 0.154170842181323),
(10.4522613065327, 0.139149064366728),
(10.5527638190955, 0.131342021715833),
(10.6532663316583, 0.131776476868947),
(10.7537688442211, 0.142996778493602),
(10.8542713567839, 0.169785901466936),
(10.9547738693467, 0.219804708812103),
(11.0552763819095, 0.302892724142838),
(11.1557788944724, 0.426985699079516),
(11.2562814070352, 0.5911852479611),
(11.356783919598, 0.782509915642953),
(11.4572864321608, 0.980922857314301),
(11.5577889447236, 1.16677809825024),
(11.6582914572864, 1.32402990634824),
(11.7587939698492, 1.4400207333425),
(11.8592964824121, 1.50512357874401),
(11.9597989949749, 1.51394490601268),
(12.0603015075377, 1.46736579239496),
(12.1608040201005, 1.3735217913741),
(12.2613065326633, 1.24604114631021),
(12.3618090452261, 1.10041105371646),
(12.4623115577889, 0.950486345837331),
(12.5628140703518, 0.80665358372032),
(12.6633165829146, 0.675517864085145),
(12.7638190954774, 0.560496367607664),
(12.8643216080402, 0.462624446749913),
(12.964824120603, 0.381357955584753),
(13.0653266331658, 0.315233773397589),
(13.1658291457286, 0.262386386323652),
(13.2663316582915, 0.220905815813076),
(13.3668341708543, 0.189111376943569),
(13.4673366834171, 0.165727726557709),
(13.5678391959799, 0.150034009084384),
(13.6683417085427, 0.14204614810516),
(13.7688442211055, 0.142800850049625),
(13.8693467336683, 0.154820932019078),
(13.9698492462312, 0.182749040438859),
(14.070351758794, 0.233786890375537),
(14.1708542713568, 0.316791145019438),
(14.2713567839196, 0.43838789864209),
(14.3718592964824, 0.596970372860479),
(14.4723618090452, 0.780188533124032),
(14.572864321608, 0.969366966703477),
(14.6733668341709, 1.14593034620409),
(14.7738693467337, 1.29441328634901),
(14.8743718592965, 1.40257151408686),
(14.9748743718593, 1.46130728527751),
(15.0753768844221, 1.46591584305129),
(15.1758793969849, 1.41782112548258),
(15.2763819095477, 1.32521043054385),
(15.3768844221106, 1.20122672989777),
(15.4773869346734, 1.06054115699149),
(15.5778894472362, 0.916210197385777),
(15.678391959799, 0.778010062031072),
(15.7788944723618, 0.652173977680231),
(15.8793969849246, 0.541907967295863),
(15.9798994974874, 0.448169822609977),
(16.0804020100503, 0.370421387380308),
(16.1809045226131, 0.307257945409622),
(16.2814070351759, 0.256901893113946),
(16.3819095477387, 0.217563282684651),
(16.4824120603015, 0.187698174577878),
(16.5829145728643, 0.166192959742004),
(16.6834170854271, 0.152529711481394),
(16.78391959799, 0.146998710015996),
(16.8844221105528, 0.151022825520138),
(16.9849246231156, 0.167657356987947),
(17.0854271356784, 0.202170487378454),
(17.1859296482412, 0.262139000942925),
(17.286432160804, 0.355745430276814),
(17.3869346733668, 0.487170331841244),
(17.4874371859297, 0.651411090395578),
(17.5879396984925, 0.834005452181879),
(17.6884422110553, 1.01640712123257),
(17.7889447236181, 1.18131080250138),
(17.8894472361809, 1.31452751420117),
(17.9899497487437, 1.40495078862609),
(18.0904522613065, 1.44494415367524),
(18.1909547738693, 1.43175236952575),
(18.2914572864322, 1.36906811434032),
(18.391959798995, 1.26675107346329),
(18.4924623115578, 1.13844106311457),
(18.5929648241206, 0.998177454787668),
(18.6934673366834, 0.857756432559233),
(18.7939698492462, 0.725608238161276),
(18.894472361809, 0.606832619160859),
(18.9949748743719, 0.503806014489472),
(19.0954773869347, 0.416943718033355),
(19.1959798994975, 0.345406044483727),
(19.2964824120603, 0.287671945204177),
(19.3969849246231, 0.241984776199526),
(19.4974874371859, 0.206672066694603),
(19.5979899497487, 0.180379796092634),
(19.6984924623116, 0.162246774539734),
(19.7989949748744, 0.152095153258899),
(19.8994974874372, 0.150674612302422),
(20, 0.160065451980172));


procedure TfrmMain.btnPlotClick(Sender: TObject);
var npoints, i, index : integer;
    startx, hstep : double;
    ds : TData;
begin
  ds := RRgraph.subgraphs[0].properties.dataSeries;

  ds.clear;

  //RRGraph.subgraphs[0].MajorXTicksStyle := tsOut;
  //RRGraph.subgraphs[0].MajorYTicksStyle := tsOut;

  //RRGraph.subgraphs[0].MinorXTicksStyle := tsOut;
  //RRGraph.subgraphs[0].MinorYTicksStyle := tsOut;

  RRGraph.subgraphs[0].graphBorder := false;

  npoints := 40;
  startx := 0.0; hstep := 20/npoints;
  ds[0].createDataColumn('x', npoints);
  ds[0].createDataColumn('y', npoints);

//  ds[0].YData.names[0] := 'Y Data';
  for i := 0 to npoints - 1 do
      begin
      ds[0].columns[0].data[i] := startx;
      ds[0].columns[1].data[i] := sin (startx);
      startx := startx + hstep;
      end;

  rrGraph.subgraphs[0].setWorldXMax(20);
  rrGraph.subgraphs[0].setWorldYMin(-2);
  rrGraph.subgraphs[0].setWorldYMax(2);

//  ds[index].YLines[0].Color := claBlue;
//  ds[index].YLines[0].ThicknessInCms := 0.04;
//
//  ds[index].YSymbols[0].Symbol := SolidSquare;
  RRgraph.Redraw;
end;

procedure TfrmMain.btnPlotTwoGraphsClick(Sender: TObject);
var  ds : TData;
     i : integer;
     index: integer;
begin
  ds := RRgraph.subgraphs[0].properties.dataSeries;

  ds.Clear;

  RRGraph.subgraphs[0].MajorXTicksStyle := tsOut;
  RRGraph.subgraphs[0].MajorYTicksStyle := tsOut;

  RRGraph.subgraphs[0].MinorXTicksStyle := tsOut;
  RRGraph.subgraphs[0].MinorYTicksStyle := tsOut;

  RRGraph.subgraphs[0].graphBorder := false;

//  index := ds.Add (TDataBlock.Create ('dsData', 6, 2));
//  ds[index].YData.names[0] := 'Y1 Data';
//  for i := 0 to ds[index].nRows - 1 do
//      begin
//      ds[index].XData[i].value := i+1;
//      ds[index].YData.data[i,0].value := 1 + Random(10);
//      end;
//  ds[index].YLines[0].Color := claRed;
//  ds[index].YLines[0].ThicknessInCms := 0.04;
//  ds[index].YSymbols[0].Symbol := SolidCircle;
//
//  ds[index].YData.names[1] := 'Y2 Data';
//  for i := 0 to ds[0].nRows - 1 do
//      begin
//      ds[index].YData.data[i,1].value := 1 + Random(20);
//      end;
//  ds[index].YLines[1].Color := claBlue;
//  ds[index].YLines[1].ThicknessInCms := 0.04;
//  ds[index].YSymbols[1].Symbol := SolidSquare;

  RRgraph.redraw;
end;

procedure TfrmMain.btnSaveBitmapClick(Sender: TObject);
begin
  RRGraph.saveOffScreenBitmap;
end;

procedure TfrmMain.btnSimPlotClick(Sender: TObject);
var  ds : TData;
     i : integer;
     index: integer;
begin
  ds := RRgraph.subgraphs[0].properties.dataSeries;
  ds.Clear;

  index := ds.Add (TDataBlock.Create ('dsSimData', 200, 1));
//  ds[index].YData.names[0] := 'Y1 Data';
//
//  RRGraph.subgraphs[0].properties.UserScale_Ymax := 3.5;
//  RRGraph.subgraphs[0].properties.UserScale_Xmax := 20;
//  for i := 0 to 199 do
//      begin
//      ds[index].XData[i].value := mydata[i, 0];
//      ds[index].YData.data[i,0].value := mydata[i, 1];
//      end;
//  ds[index].YSymbols[0].Symbol := TSymbolType.Empty;
//  ds[index].YLines[0].Color := claBlue;
//  ds[index].YLines[0].ThicknessInCms := 0.076;

  RRgraph.redraw;
end;

procedure TfrmMain.btnGridLinesClick(Sender: TObject);
begin
  RRgraph.subgraphs[0].properties.XGridLines := true;
  RRgraph.subgraphs[0].properties.YGridLines := true;

  RRgraph.subgraphs[0].properties.XMinorGridLines := true;
  RRgraph.subgraphs[0].properties.YMinorGridLines := true;

  fireEvent := false;
  ComboColorBox.Color := RRGraph.subgraphs[0].properties.XMinorGridColor;
  fireEvent := true;

  RRgraph.redraw;
end;

procedure TfrmMain.btnStartEditorClick(Sender: TObject);
begin
  RRGraph.startPropertyEditor (self, 0);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var i : integer;
    startx, hstep : double;
    npoints, index, lstIndex : integer;
    sg : TSubGraph;
    data0, data1 : TDataColumn;
    subgraph : TSubgraph;
    ds : TDataBlock;
begin
  RRGraph.subgraphs.Clear;

  sg := RRGraph.addDefaultSubgraph;
  sg.YMin := -3;
  sg.YMax := 3;
  sg.XMin := 0;
  sg.XMax := 10;

  npoints := 40;
  startx := 0.0; hstep := 20/npoints;

  subgraph  := RRGraph.getSubgraph(0);

  subgraph.properties.LogXAxis := False;
  subgraph.properties.LogYAxis := False;

  subgraph.properties.XGridLines := True;
  subgraph.properties.YGridLines := True;

  subgraph.properties.XMinorGridLines := True;
  subgraph.properties.YMinorGridLines := True;

  RRGraph.setNameOfDataSet (0, 0, 'dsSin');

  data0 := RRGraph.createDataColumn (0, 'x', npoints);
  data1 := RRGraph.createDataColumn (0, 'sin (x)', npoints);

  for i := 0 to length (RRGraph.subgraphs[0].properties.dataSeries[0].columns[0].data) - 1 do
      begin
      data0.setDataValue(i, startx);
      data1.setDataValue(i, sin (startx));
      startx := startx + hstep;
      end;

  ds := subgraph.properties.dataSeries[0];
  ds.columns[1].lineDetails.Color := claBlue;
  ds.columns[1].lineDetails.ThicknessInCms := 0.04;
  ds.columns[1].lineDetails.Visible := True;

  ds.columns[1].symbol.symType := SolidCircle;
  ds.columns[1].symbol.outlineInCms := ds.columns[1].symbol.outlineInCms*1.4;

  //lstDataBlocks.Clear;
  //for i := 0 to RRGraph.subgraphs[0].properties.dataSeries[0].columns.Count - 1 do
  //    lstIndex := lstDataBlocks.Items.Add (plt.subgraphs[0].properties.dataSeries[0].columns[i].name);
  //lstDataBlocks.ItemIndex := 1;
  //updateForm (1);
end;

procedure TfrmMain.ComboColorBoxChange(Sender: TObject);
begin
  if not fireEvent then
     exit;

  RRgraph.subgraphs[0].properties.YMinorGridColor := ComboColorBox.Color;
  RRgraph.subgraphs[0].properties.XMinorGridColor := ComboColorBox.Color;
  RRGraph.subgraphs[0].GridLineStyle := TStrokeDash.Dash;
  RRGraph.subgraphs[0].MinorGridThicknessInCms := 0.005;

  RRgraph.redraw;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  rrGraph := TRRGraph.Create(self);
  rrGraph.Parent := frmMain;
  rrGraph.new(0.2, 0.8, 0.7, 0.6);
  rrGraph.Align := TAlignLayout.Client;
end;

procedure TfrmMain.RRGraphReportCoordinates(mx, my, gx, gy: Single);
begin
  label2.Text := floattostr (mx) + '; ' + floattostr (my);
end;

procedure TfrmMain.RRGraphReportCordinates(mx, my, gx, gy: Single);
begin
  label1.Text := floattostr (mx) + ', ' + floattostr (my);
  RRgraph.redraw;
end;

end.
