unit uReadPlotFile;

interface

Uses SysUtils,
     Classes,
     System.UIConsts,
     Generics.Collections,
     uCSVReader,
     uHCL_Lite,
     uPlotConfig,
     uMathParser,
     uPlotSeries;

procedure ReadPlotFile (PlotStr : String; PlotConfig : TPlotCOnfig);

implementation

function ReadSource (Node : TConfigNode) : TDataSource;
var Id : String;
    Src : String;
    FileName : String;
    domain : TObjectList<TConfigNode>;
begin
  Result := TDataSource.Create;
  Id := Node.LabelName;
  if Node.GetString ('file') <> '' then
     begin
     FileName := Node.GetString ('file');
     Result.SourceId := Id;
     Result.srcType := TSourceType.stCSVFile;
     if FileExists(FileName) then
        begin
        Result.csv := TCSV.Create (nil);
        Result.csv.ReadCSV (FileName);
        end
     else
       raise Exception.Create('Unable to locate file: ' + FileName);
     end
  else
     if Node.GetString('function') <> '' then
        begin
        Result.funcString := Node.GetString('function');
        domain := Node.GetList('domain');
        Result.SourceId := Id;
        Result.srcType := TSourceType.stFunction;
        Result.MathFunction := TMathParser.Create;
        Result.StartPt := domain[0].FloatVal;
        Result.EndPt := domain[1].FloatVal;
        Result.NumberOfPts := Trunc (domain[2].FloatVal);
        end
     else
        raise Exception.Create('Unknown source type');
end;


procedure ReadPlotFile (PlotStr : String; PlotConfig : TPlotCOnfig);
var Root, SourceNode, PlotNode, Node  : TConfigNode;
    csv : TCSV;
    NewSeries: TPlotSeries;
    src, Id, XId, YId : String;
    domain : TObjectList<TConfigNode>;
    Line, Symbol : TConfigNode;
    i : Integer;
    MarkerStr : String;
    Plot : TPlot;
begin
  Root := THclParser.Create.Parse(PlotStr);
  try
    PlotConfig.ChartTitle := Root.GetString('title');

    // Look for plots
    for Node in Root.Children do
        begin
        if SameText(Node.Name, 'plot') then
           begin
           Plot := TPlot.Create;

           Plot.XId := Node.GetString ('x');
           Plot.YId := Node.GetString ('y');

           Src := Node.GetString('source');
           Line := Node.GetBlock('line');
           if Assigned (Line) then
              begin
              Plot.LineVisible := Line.GetBool('visible', True);
              Plot.LineColor := Line.GetAlphaColor('color', claBlack);
              Plot.LineWidth := Line.GetFloat('thickness', 2);
              end;

           Symbol := Node.GetBlock ('symbol');
           if Assigned (Symbol) then
              begin
              Plot.MarkerFillColor := Symbol.GetAlphaColor('fillcolor', claWhite);
              Plot.MarkerBorderColor := Symbol.GetAlphaColor('bordercolor', claBlack);
              Plot.MarkerSize := Symbol.GetFloat('size', 5);
              Plot.MarkerBorderThickness := Symbol.GetFloat('thickness', 2);
              Plot.MarkerVisible := Symbol.GetBool('visible', True);
              MarkerStr := Symbol.GetString('shape');
              Plot.MarkerShape := MarkerStrToMarkerShape (MarkerStr);
              end;

           PlotConfig.Plots.Add (Plot);

           // Search for source
           for SourceNode in Root.Children do
               begin
               if SameText(SourceNode.Name, 'define_source') then
                  begin
                  if src = SourceNode.LabelName then
                     begin
                     // Found the source for the plot
                     Plot.Source := ReadSource (SourceNode);
                     end;

                  end;
               end;
           end;
        end;



//    for Node in Root.Children do
//        begin
//        if SameText(Node.Name, 'define_source') then
//           begin
//           SourceNode := Root.GetBlock ('define_source');
//           Id := SourceNode.LabelName;
//           if SourceNode.GetString ('file') <> '' then
//              begin
//              src := SourceNode.GetString ('file');
//              PlotConfig.AddSource(Id, src);
//              end
//           else
//            if SourceNode.GetString('function') <> '' then
//              begin
//              src := SourceNode.GetString('function');
//              domain := SourceNode.GetList('domain');
//              PlotConfig.AddSource(Id, src, domain[0].FloatVal, domain[1].FloatVal, Trunc (domain[2].FloatVal));
//
//              end
//           else
//              raise Exception.Create('Unknown source type');
//           end;
//        end;
//    // Collect the plot info
//    for Node in Root.Children do
//        begin
//        if SameText(Node.Name, 'plot') then
//           begin
//           XId := Node.GetString ('x');
//           YId := Node.GetString ('y');
//           Src := Node.GetString('source');
//           // Search for source
//           for i := 0 to length (PlotConfig.Sources) -1 do
//               begin
//               if src = PlotConfig.Sources[i].SourceId then
//                  begin
//                  var l := length (PlotConfig.plots);
//                  SetLength (PlotConfig.Sources[i].plots, l + 1);
//                  PlotConfig.Sources[i].plots[l].XId := XId;
//                  PlotConfig.Sources[i].plots[l].YId := YId;
//                  end;
//
//               end;
//
//           end;
//        end;

  finally
    Root.Free;
  end;
end;


//procedure ReadPlotFile (PlotStr : String; PlotConfig : TPlotCOnfig);
//var Root, SourceNode, PlotNode, Node  : TConfigNode;
//    csv : TCSV;
//    NewSeries: TPlotSeries;
//    src, Id, XId, YId : String;
//    domain : TObjectList<TConfigNode>;
//    i : Integer;
//begin
//  Root := THclParser.Create.Parse(PlotStr);
//  try
//    PlotConfig.ChartTitle := Root.GetString('title');
//
//    // Look for sources
//    for Node in Root.Children do
//        begin
//        if SameText(Node.Name, 'define_source') then
//           begin
//           SourceNode := Root.GetBlock ('define_source');
//           Id := SourceNode.LabelName;
//           if SourceNode.GetString ('file') <> '' then
//              begin
//              src := SourceNode.GetString ('file');
//              PlotConfig.AddSource(Id, src);
//              end
//           else
//            if SourceNode.GetString('function') <> '' then
//              begin
//              src := SourceNode.GetString('function');
//              domain := SourceNode.GetList('domain');
//              PlotConfig.AddSource(Id, src, domain[0].FloatVal, domain[1].FloatVal, Trunc (domain[2].FloatVal));
//
//              end
//           else
//              raise Exception.Create('Unknown source type');
//           end;
//        end;
//    // Collect the plot info
//    for Node in Root.Children do
//        begin
//        if SameText(Node.Name, 'plot') then
//           begin
//           XId := Node.GetString ('x');
//           YId := Node.GetString ('y');
//           Src := Node.GetString('source');
//           // Search for source
//           for i := 0 to length (PlotConfig.Sources) -1 do
//               begin
//               if src = PlotConfig.Sources[i].SourceId then
//                  begin
//                  var l := length (PlotConfig.plots);
//                  SetLength (PlotConfig.Sources[i].plots, l + 1);
//                  PlotConfig.Sources[i].plots[l].XId := XId;
//                  PlotConfig.Sources[i].plots[l].YId := YId;
//                  end;
//
//               end;
//
//           end;
//        end;
//
//  finally
//    Root.Free;
//  end;
//end;

end.
