unit uPlottingPanel;

interface

Uses FMX.Dialogs, Classes, SysUtils, FMX.Types, FMX.Graphics, FMX.Controls,
     Generics.Collections, uSubgraph, uRRCommon, FMX.StdCtrls, FMX.ExtCtrls,
     IOUtils, uRRDataSeries, System.UITypes, System.Types, ufGraphPackageDialog,
     uGObject, Json, skia, skia.FMX;

const
   //defaultWidth = 400;
   //defaultHeight = 300;

    SIGNATURE = 'GP146';
    VERSION = '1.0';

type
  TRRGraph = class;

  TRRPanelSelected = (psNone, psSubGraph);

  TOnReportCoordinates = procedure(mx, my, gx, gy : single) of object;
  TOnMainTitle = procedure(var subGraphId : integer) of object;
  TOnXAxisTitle = procedure(var subGraphId : integer) of object;
  TOnYAxisTitle = procedure(var subGraphId : integer) of object;

  TSubgraphCollectionItem = class (TCollectionItem)
    private
      FSubgraph : TSubgraph;
      procedure setSubgraph (subgraph : TSubgraph);
    public
      constructor Create (collection : TCollection); override;
    published
      property subgraph: TSubgraph read FSubgraph write setSubgraph;
    end;

  TSubgraphCollection = class (TCollection)
    private
      FOwner: TRRGraph;
      procedure  setItem (index: integer; value : TSubgraph);
      function   getItem (index : integer) : TSubgraph;
    public
      function    getPanel : TRRGraph;
      constructor Create (CollectionOwner: TRRGraph);
      function    GetOwner: TPersistent; override;
      function    Add : TSubgraphCollectionItem;
      property    Items[Index : integer] : TSubgraph read GetItem write SetItem; default;
    end;

   TRRGraph = class (TSkCustomControl)

     private
       FOnReportCoordinates : TOnReportCoordinates;
       FOnMainTitle : TOnMainTitle;
       FOnXAxisTitle : TOnXAxisTitle;
       FOnYAxisTitle : TOnYAxisTitle;

       Orig_x, Orig_y : single;
       oldCorner_x, oldCorner_y : single;
       moveObject : boolean;
       ReSizingObject : boolean;
       dx, dy : single;
       currentlySelectedObject : TGraphBase;
       currentlySelectedObjectType : TSubGraphSelectedObjectType;

       myOwner : TComponent;
       offScreenBitmap : TBitmap;
       magnification : double;

       mouseDownState : boolean;
       bolDblClick : boolean;
       addGraphState : boolean;
       bolMovingLasso : boolean;
       originalX, originalY : single; // MouseDown coordinates
       startX, startY : single;
       fstart, fend : TPointF;
       currentSubGraphId : integer;
       rectStart : TRectF;
       FSubgraphs : TSubgraphCollection;
       dragDirection : TDirection;

       FOnKeyDown : TKeyEvent;

       LCanvas : ISkCanvas;

       procedure   draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
       procedure   drawToCanvas(ACanvas : ISkCanvas);

       procedure DblClick; override;
       procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
       procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
       procedure MouseMove(Shift: TShiftState; X, Y: single); override;
       procedure Resize; override;

       procedure drawMoveRectangle (canvas : TCanvas);
       procedure drawFinalRectangle (canvas : TCanvas);
       procedure makeSubGraph (rx, ry, rxend, ryend : double);
       function  IsOnMainTitle (canvas : TCanvas; x, y : integer) : boolean;
       function  IsOnXAxisTitle (canvas : TCanvas; x, y : integer) : boolean;
       function  IsOnXAxis (canvas: TCanvas;x, y : integer) : boolean;
       function  IsOnYAxisTitle (canvas: TCanvas;x, y : integer) : boolean;
       function  IsOnLegend (canvas: TCanvas;x, y : integer) : boolean;

       function  IsOnObject (x, y : single; var currentSubGraph : integer; var graphObject : TGraphBase) : boolean;

     public
      CurrentlyPrinting : boolean;
      HScrollBar, VScrollBar : TScrollBar;
      Origin : TPoint;
      panelSelected : TRRPanelSelected;
      divideByZeroError : boolean;
      designerForm : TfrmGraphPackageDlg;
      backgroundColor : TAlphaColor;

      procedure   saveOffScreenBitmap;

      constructor Create (AOwner : TComponent); override;
      destructor  Destroy; override;

      procedure   clear;

      function    addDefaultSubgraph : TSubGraph;
      function    new  (rxmin, rymin, rwidth, rheight : double) : TSubGraph;
      function    onSubGraph (ACanvas : ISkCanvas; x, y : single; var graphObject : TGraphBase; var Id: String) : boolean; overload;
      function    onSubGraph (ACanvas : ISkCanvas; x, y : single; var graphObject : TGraphBase; var index: integer) : boolean; overload;
      //function    onSubGraph (x, y : integer; var index: integer) : boolean; overload;
      procedure   unselectAllSubGraphs;
      procedure   selectObject (canvas : TCanvas; currentlySelectedObject : TSubGraphSelectedObjectType);
      procedure   unSelectAllObjects;

      function    getListOfSymbolShapes : TStringList;

      procedure   saveToJson (const fileName : string);
      procedure   readFromJson  (const fileName : string);

      //procedure   exportToPng (fileName : string);
      procedure   exportToPDF (filename : string);

      procedure   startAddInteractiveGraph;
      procedure   startPropertyEditor (AOwner : TComponent; subGraphId : integer);
      procedure   updatePropertyEditor (subGraphId : integer);

      function    getSubgraph (index :integer) : TSubGraph;

      procedure   setLogXAxis (subgraph : integer; value : boolean);
      procedure   setLogYAxis (subgraph : integer; value : boolean);

      procedure   setXAxisGridLines (subgraph : integer; value : boolean);
      procedure   setYAxisGridLines (subgraph : integer; value : boolean);

      procedure   setXAxisMinorGridLines (subgraph : integer; value : boolean);
      procedure   setYAxisMinorGridLines (subgraph : integer; value : boolean);

      function    createDataColumn (subgraph : integer; name : string; npoints : integer) : TDataColumn;

      procedure   setNameOfDataSet (subgraph, datablock : integer; name : string);

      procedure   ScrollBarOnScroll (Sender : TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
      procedure   KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;

     published
      property    OnReportCoordinates : TOnReportCoordinates read FOnReportCoordinates write FOnReportCoordinates;
      property    OnMainTitle : TOnMainTitle read FOnMainTitle write FOnMainTitle;
      property    OnXAxisTitle : TOnXAxisTitle read FOnXAxisTitle write FOnXAxisTitle;
      property    OnYAxisTitle : TOnYAxisTitle read FOnYAxisTitle write FOnYAxisTitle;
      property    subgraphs : TSubgraphCollection read FSubgraphs write FSubgraphs;

      property    OnMouseDown;
      property    OnMouseUp;
      property    OnMouseMove;

      property    Align;
      property    OnExit;
      property    OnEnter;
      property    OnDblClick;
      property    OnResize;
      property    ShowHint;
      property    PopupMenu;
      property    Visible;
      property    Position;
      property    Width;
      property    Height;
   end;

procedure register;

implementation

Uses FMX.Forms, uSubGraphCollectionEditor, uSymbolDetails, System.UIConsts, uRRUtilities;

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

// -------------------------------------------------------------------------


constructor TSubgraphCollection.Create (CollectionOwner: TRRGraph);
begin
  inherited Create (TSubgraphCollectionItem);
  FOwner := CollectionOwner;
end;


function TSubgraphCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


function TSubgraphCollection.getPanel : TRRGraph;
begin
  result := FOwner;
end;


procedure TSubgraphCollection.setItem (index: integer; value : TSubgraph);
begin
  inherited setItem (Index, TCollectionItem (value));
end;


function TSubgraphCollection.getItem (index : integer) : TSubgraph;
var value : TSubgraphCollectionItem;
begin
  value := TSubgraphCollectionItem (inherited getItem (index));
  result := value.subgraph;
end;


function TSubgraphCollection.Add : TSubgraphCollectionItem;
begin
  result := TSubgraphCollectionItem (inherited Add);
end;


// ----------------------------------------------------------------------


constructor TSubgraphCollectionItem.Create (collection : TCollection);
var panel : TRRGraph;
begin
  inherited Create (collection);
  panel := (collection as  TSubgraphCollection).getPanel;
  FSubgraph := TSubgraph.Create (panel);

  panel.designerForm.subgraph := FSubgraph;
  panel.designerForm.copyPropertiesToDlg (FSubgraph.properties);

  (FSubGraph.parentGraph as TRRGraph).Repaint;
end;


procedure TSubgraphCollectionItem.setSubgraph (subgraph : TSubgraph);
begin
  FSubgraph := subgraph;
end;


// -------------------------------------------------------------------------


constructor TRRGraph.Create (AOwner : TComponent);
begin
  inherited;

  offScreenBitmap := TBitmap.Create(trunc (width), trunc (height));
  myOwner := AOwner;
  CurrentlyPrinting := false;
  magnification := 1.0;
  Origin := point (0, 0);
  panelSelected := psNone;
  bolDblClick := false;
  currentSubGraphId := -1;
  HitTest := True;

  designerForm := TfrmGraphPackageDlg.Create(nil);

  FSubgraphs := TSubgraphCollection.Create (self);
  getCurrentPixelsPerInch(canvas, false);

  mouseDownState := false;
  addGraphState := false;
  bolMovingLasso := false;

  backgroundColor := claWhite;
  designerForm.cboBackgroundColor.Color := backgroundColor;

//  HScrollBar := TScrollBar.Create (Self);
//  HScrollBar.Parent := Self;
//  HScrollBar.visible := true;
//  HScrollBar.Align :=  TAlignLayout.Bottom;
  //HScrollBar.OnScroll := ScrollBarOnScroll;
  //HScrollBar.Max := 100;
  //HScrollBar.SmallChange := 6;
  //HScrollBar.LargeChange := 20;
 //HScrollBar.DoubleBuffered := not IsVista;

//  vScrollBar := TScrollBar.Create (Self);
//  vScrollBar.Parent := Self;
//  VScrollBar.visible := true;
  //vScrollBar.Kind := sbVertical;
  //vScrollBar.Align := TAlignLayout.Right;
  //vScrollBar.OnScroll := ScrollBarOnScroll;
  //vScrollBar.Max := 100;
  //vScrollBar.SmallChange := 6;
  //vScrollBar.LargeChange := 20;
  //HScrollBar.DoubleBuffered := not IsVista;
end;


destructor TRRGraph.Destroy;
begin
  FSubgraphs.Free;
  inherited;
end;


procedure TRRGraph.clear;
begin
  FSubgraphs.Clear;
end;


procedure TRRGraph.Resize;
begin
  inherited;
  offScreenBitmap.SetSize(trunc(Width), trunc(Height));
  repaint;
end;


procedure TRRGraph.ScrollBarOnScroll (Sender : TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  //Origin := point (hScrollBar.Position, vScrollBar.Position);
  RePaint;
end;


procedure TRRGraph.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if Assigned (FOnKeyDown) then
     FOnKeyDown (Self, Key, KeyChar, Shift);
end;


procedure TRRGraph.drawMoveRectangle (canvas : TCanvas);
begin
  //canvas.pen.Mode := pmNotXor;
  //canvas.pen.Style := psDot;
  //canvas.pen.color := clBlack;
  //canvas.Rectangle (FStart.X, FStart.Y, FEnd.X, FEnd.Y);
end;


procedure TRRGraph.drawFinalRectangle (canvas : TCanvas);
begin
  //canvas.pen.Mode := pmCopy;
 // canvas.pen.Style := psSolid;
 //canvas.rectangle (FStart.X, FStart.Y, FEnd.X, FEnd.Y);
end;


function TRRGraph.IsOnMainTitle (canvas : TCanvas; x, y : integer) : boolean;
begin
  //result := FSubgraphs[currentSubGraphId].IsOnMainTitle (ACanvas, x, y);
end;


function TRRGraph.IsOnXAxisTitle (canvas : TCanvas; x, y : integer) : boolean;
begin
 // HMS
 // result := FSubgraphs[currentSubGraphId].IsOnXAxisTitle (canvas, x, y);
end;


function TRRGraph.IsOnXAxis (canvas: TCanvas; x, y : integer) : boolean;
begin
  // HMS result := FSubgraphs[currentSubGraphId].IsOnXAxis (canvas, x, y);
end;

function TRRGraph.IsOnYAxisTitle (canvas: TCanvas; x, y : integer) : boolean;
begin
  // HMS result := FSubgraphs[currentSubGraphId].IsOnYAxisTitle (canvas, x, y);
end;


function TRRGraph.IsOnLegend (canvas: TCanvas;x, y : integer) : boolean;
begin
  // HMS result := FSubgraphs[currentSubGraphId].IsOnLegend (canvas, x, y);
end;


procedure TRRGraph.unselectAllSubGraphs;
var i : integer;
begin
  for i := 0 to FSubgraphs.Count - 1 do
      FSubgraphs[i].selectedObjectType := coNone;
end;


procedure TRRGraph.DblClick;
begin
if currentSubGraphId = -1 then
   exit;

  case FSubgraphs[currentSubGraphId].SelectedObjectType of
     coNone : exit;
     coMainTitle  : startPropertyEditor (self, 0);
     coXAxisTitle : startPropertyEditor (self, 0);
     coYAxisTitle : showmessage ('Y Axis title');
     coXAxis      : showmessage ('X Axis');
     coLegend     : showmessage ('Legend');
     coGraphingArea : startPropertyEditor (self, 0);
  end;
  bolDblClick := true;
end;


function TRRGraph.IsOnObject (x, y : single; var currentSubGraph : integer; var graphObject : TGraphBase) : boolean;
var box : TBox;
begin
  result := false;
  panelSelected := psSubGraph;

  originalX := x; originalY := y;
  startX := x; startY := y;
  if onSubGraph (LCanvas, x, y, graphObject, currentSubGraph) then
     case graphObject.objType of
         coGraphingArea :
           begin
           subgraphs[currentSubGraph].SelectedObjectType := coGraphingArea;
           rectStart := FSubgraphs[currentSubGraph].getGraphDeviceDrawingArea;
           result := true;
           end;

         coXAxisTitle :
           begin
           FSubgraphs[currentSubGraph].SelectedObjectType := coXAxisTitle;
           result := true;
           end;

         coYAxisTitle :
           begin
           FSubgraphs[currentSubGraph].SelectedObjectType := coYAxisTitle;
           result := true;
           end;

         coMainTitle :
           begin
           FSubgraphs[currentSubGraph].SelectedObjectType := coMainTitle;
           box := FSubgraphs[currentSubGraph].properties.mainTitleObject.textProperties.box;  //graphObjects[mainTitleId].textProperties.box;
           rectStart := boxToRectF (box);
           if Assigned(FOnMainTitle) then
              FOnMainTitle (currentSubGraph);
           exit (true);
           end;

         coXAxis :
           begin
           FSubgraphs[currentSubGraph].SelectedObjectType := coXAxis;
           box := FSubgraphs[currentSubGraph].properties.xaxisTitleObject.textProperties.box;  //graphObjects[mainTitleId].textProperties.box;
           rectStart := boxToRectF (box);
           exit (true);
           end;

         coLegend :
           begin
           FSubgraphs[currentSubGraph].SelectedObjectType := coLegend;
           result := true;
           end;
     else
           begin
           result := false;
           panelSelected := psSubGraph;
           end;
     end;
end;

procedure TRRGraph.selectObject (canvas : TCanvas; currentlySelectedObject : TSubGraphSelectedObjectType);
begin
  repaint;
end;


procedure TRRGraph.unSelectAllObjects;
begin
  if currentSubGraphId <> -1 then
     begin
     subgraphs[currentSubGraphId].unselectAllObjects;
     currentlySelectedObject := nil;
     repaint;
     end;
end;


procedure TRRGraph.MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: single);
var Id : AnsiString;
    aBox : TBox;
    tmpSelectedObject : TGraphBase;
    foundObject : boolean;
begin
  inherited;

  { Where is the mouse ?
    1. Over a resizing handle, then get ready to resize
    2. Over an unselected rectangle, then select the rectangle
    3. Over a selected rectangle, get ready to move the rectangle
    4. Over empty space, unselected any selected rectangles }

  moveObject := false; ReSizingObject := false;
  Orig_x := x; Orig_y := y;

  // If we're in empty space just deselect all objects
  foundObject := IsOnObject (x, y, currentSubGraphId, tmpSelectedObject) and (not bolDblClick);
  if not foundObject then
     begin
     unSelectAllObjects;
     exit;
     end;

  if foundObject then
     begin
     currentlySelectedObject := tmpSelectedObject;
     currentlySelectedObject.selected := true;

     // HMS
     //if currentlySelectedObject.ObjType = coLegend then
     //   aBox := subgraphs[currentSubGraphId].relativeToDevice2 (subgraphs[currentSubGraphId].properties.legend)
     //else
        aBox := subgraphs[currentSubGraphId].relativeToDevice2(currentlySelectedObject);
     currentlySelectedObject.dBox := aBox;
     if currentlySelectedObject.IsReSizable and PtOverHandle (aBox , x, y, dragDirection) then
        begin
        Cursor := crCross;
        // record the coords of the handle that is going to move
        case dragDirection of
             NW : begin
                  oldCorner_x := aBox.left;  oldCorner_y := aBox.top;
                  end;
             NE : begin
                  oldCorner_x := aBox.left + aBox.w; oldCorner_y := aBox.top;
                  end;
             SE : begin
                  oldCorner_x := aBox.left + aBox.w; oldCorner_y := aBox.top + aBox.h;
                  end;
             SW : begin
                  oldCorner_x := aBox.left;  oldCorner_y := aBox.top + aBox.h;
                  end;
          end;
        ReSizingObject := true; MoveObject := false;
        end
     else
        begin
        moveObject := true; ReSizingObject := false;
        end;
     end;

  if addGraphState then
     begin
     fstart.x := x; fstart.y := y;
     fend := fstart;
     bolMovingLasso := true;
     exit;
     end;
end;


procedure TRRGraph.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single);
var deltaX, deltaY, rx, rw, rxend, ry, ryend : double;
    index : integer;
begin
  inherited;
  moveObject := false;
  ReSizingObject := false;
  if mouseDownState then
     begin
     mouseDownState := false;
     if currentlySelectedObjectType = coGraphingArea then
        begin
        // Don't transform unless we've actually moved
        if originalX <> x then
           FSubGraphs[currentSubGraphId].rxmin := rectStart.left/Width;
        if originalY <> y then
           FSubGraphs[currentSubGraphId].rymin := 1 - rectStart.bottom/Height;
        end;
     end;

  // Not yet implemented
  if bolMovingLasso then
     begin
     fend := pointf (x, y);
     drawFinalRectangle (canvas);

     rx := fstart.x/Width;
     ry := 1 - fstart.y/Height;
     rxend := X/Width;
     ryend := 1 - Y/Height;

     makeSubGraph (rx, ryend, rxend, ry);
     bolMovingLasso := false;
     addGraphState := false;
     end;
  bolDblClick := false;
end;


procedure TRRGraph.MouseMove(Shift: TShiftState; X, Y: single);
var index : integer;
    rx, ry : double;
    r : TRectF; a, b : integer;
    deltaX, deltaY : single;
    box : TBox;
    rBoxGraphingArea : TLogicalBox;
    p : TPointF;
    tmpSelectedObject : TGraphBase;
    aBox : TBox;
    aleft, aright, atop, abottom : single;
    tmph : single;
begin
  inherited;
  if IsOnObject (x, y, currentSubGraphId, tmpSelectedObject) and (not bolDblClick) then
     Cursor := crHandPoint
  else
     Cursor:= crDefault;

  if currentSubGraphId <> -1 then
     begin
     aBox := subgraphs[currentSubGraphId].relativeToDevice2(subgraphs[currentSubGraphId].properties.graphObjects[graphingAreaId]);
     if PtOverHandle (aBox , x, y, dragDirection) then
        Cursor := crCross;
     end;

  if ReSizingObject then
     begin
     canvas.BeginScene;
     aBox := subgraphs[currentSubGraphId].relativeToDevice2(currentlySelectedObject);
     aleft := aBox.left; atop := aBox.top;
     aright := aBox.left + aBox.w; abottom := aBox.top + aBox.h;
     case dragDirection of
          NW : begin
               { Don't allow user to drag below rectangle }
               if y < abottom-4 then
                  begin
                  dy := y - oldCorner_y;
                  currentlySelectedObject.dBox.top := currentlySelectedObject.dBox.top + dy; // y is inverted
                  currentlySelectedObject.dBox.h := currentlySelectedObject.dBox.h - dy;

                  tmph := Height - currentlySelectedObject.dBox.top;
                  currentlySelectedObject.logicalBox.top := tmph/Height;
                  currentlySelectedObject.logicalBox.h := currentlySelectedObject.dBox.h / Height;

                  oldCorner_x := x; oldCorner_y := y; { ! This line can't be moved from here ! }
                  end;
               end;
          NE : begin
               { Don't allow user to drag below rectangle }
               if (y < abottom-3) and (x > aleft+3) then
                  begin
                  dx := oldCorner_x - x; dy := y - oldCorner_y;
                  currentlySelectedObject.dBox.top := currentlySelectedObject.dBox.top + dy; // y is inverted
                  currentlySelectedObject.dBox.h := currentlySelectedObject.dBox.h - dy;

                  currentlySelectedObject.dBox.w := currentlySelectedObject.dBox.w - dx;

                  tmph := Height - currentlySelectedObject.dBox.top;
                  currentlySelectedObject.logicalBox.top := tmph/Height;
                  currentlySelectedObject.logicalBox.h := currentlySelectedObject.dBox.h / Height;
                  currentlySelectedObject.logicalBox.w := currentlySelectedObject.dBox.w/Width;

                  oldCorner_x := x; oldCorner_y := y;
                  end;
               end;
          SE : begin
               { Don't allow user to drag below rectangle }
               if x > aleft+3 then
                  begin
                  dx := oldCorner_x - x; dy := y - oldCorner_y;
                  currentlySelectedObject.dBox.w := currentlySelectedObject.dBox.w - dx;
                  currentlySelectedObject.logicalBox.w := currentlySelectedObject.dBox.w/Width;

                  oldCorner_x := x; oldCorner_y := y;
                  end;
               end;
          SW : begin
               { Don't allow user to drag below rectangle }
               if (y > atop+3) and (x < aright-3) then
                  begin
                  dx := x - oldCorner_x; dy := y - oldCorner_y;
                  currentlySelectedObject.dBox.left := currentlySelectedObject.dBox.left + dx;
                  currentlySelectedObject.dBox.w := currentlySelectedObject.dBox.w - dx;

                  currentlySelectedObject.logicalBox.left := currentlySelectedObject.dBox.left/Width;
                  currentlySelectedObject.logicalBox.w := currentlySelectedObject.dBox.w / Width;

                  oldCorner_x := x; oldCorner_y := y;
                  end;
               end;
     end;
     Canvas.EndScene;
     end;

  if moveObject then
     begin
     dx := x - orig_x;
     dy := y - orig_y;

     rBoxGraphingArea := FSubgraphs[currentSubGraphId].getLogicalBoundingBox (graphingAreaId);

     if currentlySelectedObject.ObjType <> coGraphingArea then
        begin
        // Coordinates are fractional, therefore we move them by fractional amounts
        currentlySelectedObject.logicalBox.left := currentlySelectedObject.logicalBox.left + dx/(rBoxGraphingArea.w * Width);
        currentlySelectedObject.logicalBox.top := currentlySelectedObject.logicalBox.top + dy/(rBoxGraphingArea.h * Height);
       end
     else
       begin
       currentlySelectedObject.logicalBox.left := currentlySelectedObject.logicalBox.left + dx/Width;
       currentlySelectedObject.logicalBox.top  := currentlySelectedObject.logicalBox.top - dy/Height;
       end;
     orig_x := x; orig_y := y;
     end;

//  if mouseDownState then
//     begin
//     // Move dotted rectangle.
//     //Canvas.Pen.Mode := pmNotXor;
//     //Canvas.Pen.Style := psDot;
//     //Canvas.Rectangle(rectStart);
//     deltaX := x - startX;
//     deltaY := y - startY;
//
//     r.left := rectStart.left + deltaX;
//     r.top  := rectStart.top + deltaY;
//
//     r.right  := rectStart.right + deltaX;
//     r.bottom := rectStart.bottom + deltaY;
//
//     //Canvas.Rectangle(r);
//
//     startX := X;
//     startY := Y;
//     rectStart := r;
//     //canvas.Pen.Mode := pmCopy;
//     end;

  // Not yet implemented
//  if bolMovingLasso then
//     begin
//     drawMoveRectangle (canvas);
//     fend := pointf (x, y);
//     drawMoveRectangle (canvas);
//     //canvas.Pen.Mode := pmCopy;
//     end;
  if Assigned(FOnReportCoordinates) then
     begin
     p.X := 0; p.Y := 0;
     if IsOnObject (x, y, currentSubGraphId, tmpSelectedObject) and (not bolDblClick) then
        begin
        p := FSubgraphs[currentSubGraphId].deviceToWorld(x, y);
        end;
     FOnReportCoordinates (x, y, p.x, p.y);
     end;
  redraw;
end;


procedure TRRGraph.makeSubGraph (rx, ry, rxend, ryend : double);
var sg : TSubGraph;
begin
   sg := new(rx, ry, rxend, ryend);
   sg.Xmin := 0;    sg.Xmax := 10;
   sg.Ymin := -1.1; sg.Ymax := 1.1;
   sg.properties.LogXAxis := false;
end;


function TRRGraph.onSubGraph (ACanvas : ISkCanvas; x, y : single; var graphObject : TGraphBase; var Id: String) : boolean;
var i : integer;
begin
  result := false;
  for i := 0 to FSubgraphs.Count - 1 do
      if FSubgraphs[i].onSubGraph (ACanvas, x, y, graphObject) then
         begin
         result := true;
         Id := FSubgraphs[i].Id;
         exit;
         end;
end;


function TRRGraph.onSubGraph (ACanvas : ISkCanvas; x, y : single; var graphObject : TGraphBase; var index: integer) : boolean;
var i : integer;
begin
  result := false;
  for i := 0 to FSubgraphs.Count - 1 do
      if FSubgraphs[i].onSubGraph (ACanvas, x, y, graphObject) then
         begin
         result := true;
         index := i;
         exit;
         end;
end;


procedure TRRGraph.updatePropertyEditor (subGraphId : integer);
begin
  if assigned (designerForm) then
     begin
     designerForm.subgraph := subgraphs[subGraphId];
     designerForm.copyPropertiesToDlg (subgraphs[subGraphId].properties);
     end;
end;


procedure TRRGraph.startPropertyEditor (AOwner : TComponent; subGraphId : integer);
begin
  if designerForm = nil then
     begin
     if AOwner = nil then
        designerForm := TfrmGraphPackageDlg.Create(Application)
     else
        designerForm := TfrmGraphPackageDlg.Create (AOwner);
     end;

  try
    designerForm.subgraph := subgraphs[subGraphId];
    designerForm.copyPropertiesToDlg (subgraphs[subGraphId].properties);
    designerForm.Show;
  finally
    // Messes up the visuals on the host form is we try to free this one
    //DesignerForm.Free;
  end;
  Repaint;
end;


function TRRGraph.getSubgraph (index :integer) : TSubGraph;
begin
  if index >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (index));

  result := subgraphs[index];
end;

procedure TRRGraph.setLogXAxis (subgraph : integer; value : boolean);
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  subgraphs[subgraph].properties.LogXAxis := value;
end;


procedure TRRGraph.setLogYAxis (subgraph : integer; value : boolean);
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  subgraphs[subgraph].properties.LogYAxis := value;
end;


procedure TRRGraph.setXAxisGridLines (subgraph : integer; value : boolean);
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  subgraphs[subgraph].properties.XGridLines := value;
end;


procedure TRRGraph.setYAxisGridLines (subgraph : integer; value : boolean);
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  subgraphs[subgraph].properties.YGridLines := value;
end;


procedure TRRGraph.setXAxisMinorGridLines (subgraph : integer; value : boolean);
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  subgraphs[subgraph].properties.XMinorGridLines := value;
end;


procedure TRRGraph.setYAxisMinorGridLines (subgraph : integer; value : boolean);
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  subgraphs[subgraph].properties.YMinorGridLines := value;
end;


function TRRGraph.createDataColumn (subgraph : integer; name : string; npoints : integer) : TDataColumn;
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  result := TDataColumn.Create (name, npoints);
  subgraphs[subgraph].properties.dataBlocks[0].columns.Add(result);
end;


procedure TRRGraph.setNameOfDataSet (subgraph, datablock : integer; name : string);
begin
  if subgraph >= subgraphs.Count then
     raise Exception.Create('Specified subgraph does not exist: ' + inttostr (subgraph));

  if dataBlock >= subgraphs[subgraph].properties.dataBlocks.Count then
     raise Exception.Create('The data block you specified does not exist: ' + inttostr (datablock));
end;


function TRRGraph.addDefaultSubgraph : TSubGraph;
begin
  result := new (0.15, 0.8, 0.75, 0.6);
end;

// Create a new subgraph. The dimensions are normalized coordinates
function TRRGraph.new (rxmin, rymin, rwidth, rheight : double) : TSubGraph;
var w, h, l, t : integer;
begin
  result := FSubgraphs.Add.FSubgraph;
  result.rxmin := rxmin; result.rwidth  := rwidth;
  result.rymin := rymin; result.rheight := rheight;

  result.parentGraph := self;
end;


procedure TRRGraph.saveOffScreenBitmap;
begin
  offScreenBitmap.SaveToFile ('c:\\tmp\\bitmap.bmp');
end;



procedure TRRGraph.draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var rec : TRectF;
begin
  ACanvas.Save;
  try
    LCanvas := ACanvas;
    ACanvas.ClipRect(ADest);
    ACanvas.Clear (backgroundColor);
    drawToCanvas(ACanvas);
  finally
    ACanvas.Restore;
  end;
end;



procedure TRRGraph.drawToCanvas(ACanvas : ISkCanvas);
var i : integer;
  vBitMapData  : TBitmapData;
  vPixelColor  : TAlphaColor;
begin
  divideByZeroError := false;
  try
  for i := 0 to FSubgraphs.Count - 1 do
      FSubgraphs[i].paint(ACanvas);
  except
    on E: EZeroDivide do
       divideByZeroError := true;
    on E: Exception do
       divideByZeroError := true;
  end;
end;


function TRRGraph.getListOfSymbolShapes : TStringList;
begin
  result := uSymbolDetails.getListOfSymbolNames;
end;


procedure TRRGraph.startAddInteractiveGraph;
begin
  addGraphState := true;
end;


procedure TRRGraph.saveToJson (const fileName : string);
var saveJsonFileName : string;
    JSONObj : TJSONObject;
    ar, dataArray: TJSONArray;
    i : integer;
begin
  if ExtractFileExt (fileName) <> '.json' then
     saveJsonFileName := filename + '.json'
  else
     saveJsonFileName := fileName;

  JSONObj := TJSONObject.Create;
  try
   JsonObj.AddPair(SIGNATURE, VERSION);
   ar := TJSONArray.Create();
   JsonObj.AddPair('subgraphs', ar);
   //for i := 0 to  FSubgraphs.Count - 1 do
   //    ar.Add (FSubgraphs[i].saveToJson());

   TFile.WriteAllText(saveJsonFileName, JsonObj.ToString);
  finally
    JsonObj.Free;
  end;
end;


procedure TRRGraph.readFromJson (const fileName : string);
var saveJsonFileName : string;
    obj, textObj, layoutObj : TJSONObject;
    sgObj : TJSONObject;
    astr : string;
    version : string;
    columns : TJSONArray;
    i, j : integer;
    column : TDataColumn;
    db : TDataBlock;
    ar : TJSONArray;
    index : integer;
    sgs : TJSONArray;
    sg : TSubgraph;
begin
  if ExtractFileExt (fileName) <> '.json' then
     saveJsonFileName := filename + '.json'
  else
     saveJsonFileName := fileName;

  astr := TFile.ReadAllText(saveJsonFileName);
  obj := TJSONObject.ParseJSONValue(astr) as TJSONObject;
  if not Obj.TryGetValue (SIGNATURE, version) then
     raise Exception.Create('This is not a valid json file for this application');
  clear;

  sgs := Obj.Get('subgraphs').JsonValue as TJSONArray;
  for i := 0 to sgs.Count - 1 do
      begin
      sg := FSubgraphs.Add.FSubgraph;
      sgobj := sgs[i] as TJSONObject;
      //sg.readFromJson(sgObj);
      end;

  Repaint;
end;


//procedure TRRGraph.exportToPng (fileName : string);
//var
//  LImage: ISkImage;
//  LSurface: ISkSurface;
//begin
//  LSurface := TSkSurface.MakeRaster(trunc (Width),trunc (Height));
//  FSubgraphs[0].drawToCanvas (LSurface.Canvas);
//  LImage := LSurface.MakeImageSnapshot;
//  TFile.WriteAllBytes(fileName, LImage.Encode(TSkEncodedImageFormat.PNG));
//end;


procedure TRRGraph.exportToPDF (filename : string);
var LPDFStream: TStream;
    LDocument: ISkDocument;
    LCanvas : ISkCanvas;
    i : integer;
begin
  LPDFStream := TFileStream.Create(fileName, fmCreate);
  try
    LDocument := TSkDocument.MakePDF(LPDFStream);
    try
      LCanvas := LDocument.BeginPage(Width, Height);
      try
       LCanvas.Clear(TAlphaColors.White);
       drawToCanvas(LCanvas);
      finally
        LDocument.EndPage;
      end;
    finally
      LDocument.Close;
    end;
  finally
    LPDFStream.Free;
  end;
end;


procedure Register;
begin
  RegisterComponents('ComponentLibrary', [TRRGraph]);
  RegisterClass(TSubgraphCollection);
  RegisterClass(TSubgraphCollectionItem);
end;

end.

