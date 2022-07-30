unit uConfiguration;

interface

Uses SysUtils, REST.Json, FMX.Dialogs, uTableFrameViewer, ufMainConfig, uModelInputManager;

const
   CONFIG_FILE_NAME = 'iridium.json';

type
  TConfigOptions = class (TObject)
    private
     FMainConfig : TfrmMainConfig;
     FModelInputManagerConfig : TModelInputManagerConfig;
     FTextFormViewer : TTableFormViewerConfig;
    published
      property mainConfig : TfrmMainConfig read FMainConfig write FMainConfig;
      property modelInputManagerConfig : TModelInputManagerConfig read FModelInputManagerConfig write FModelInputManagerConfig;
      property textFormViewer : TTableFormViewerConfig read FTextFormViewer write FTextFormViewer;
    public
      procedure saveToJson (const fileName : string);
      procedure readFromJson (const fileName : string);
      constructor Create;
  end;

  procedure saveConfigurationFile (fileName : string);
  function  readConfigurationFile (fileName : string) : boolean;
  function  getConfigPath : string;

  var configOpts : TConfigOptions;

implementation

Uses IOUtils, Json, ufMain;

const
  SIGNATURE = 'IND01';

constructor TConfigOptions.create;
begin
  inherited;
end;


function getConfigPath : string;
begin
{$IFDEF OSX}
  result := TPath.Combine(TPath.GetLibraryPath, 'Application Support') + TPath.DirectorySeparatorChar;
{$ELSE}
  result := TPath.GetHomePath + TPath.DirectorySeparatorChar
{$ENDIF}
end;

function readConfigurationFile (fileName : string): boolean;
var cjson : string;
    path : string;
begin
  result := true;
  try
    path := getConfigPath + fileName;
    if FileExists (path) then
       begin
       configOpts.readFromJson(getConfigPath + fileName);
       //cJson := TFile.ReadAllText(path);
       end;
  except
    on e: Exception do
       begin
       showmessage ('Configuration loading error: ' + e.Message);
       exit (false);
       end;
  end;
end;

procedure TConfigOptions.saveToJson (const fileName : string);
var saveJsonFileName : string;
    mainObj, topObj : TJSONObject;
    ar, dataArray: TJSONArray;
    i : integer;
begin
  topObj := TJSONObject.Create;
  mainObj := TJSONObject.Create;
  try
   topObj.AddPair(SIGNATURE, VERSION);
   topObj.AddPair ('config', mainObj);
   mainObj.AddPair ('mainForm', FMainConfig.saveToJson);
   mainObj.AddPair ('modelInputManager', FModelInputManagerConfig.saveToJson);
   mainObj.AddPair ('textFormViewer', FTextFormViewer.saveToJson);

  // ar := TJSONArray.Create();
   //JsonObj.AddPair('subgraphs', ar);
   //for i := 0 to  FSubgraphs.Count - 1 do
   //    ar.Add (FSubgraphs[i].saveToJson());

   TFile.WriteAllText(fileName, topObj.ToString);
  finally
    topObj.Free;
  end;
end;

procedure TConfigOptions.readFromJson (const fileName : string);
var obj,
    configObj,
    mainObj,
    modelManagerObj,
    tableViewerObj : TJSONObject;
    astr : string;
    version : string;
begin
  astr := TFile.ReadAllText(fileName);
  obj := TJSONObject.ParseJSONValue(astr) as TJSONObject;
  if not Obj.TryGetValue (SIGNATURE, version) then
     begin
     FMainConfig := TfrmMainConfig.CreateDefault;
     FModelInputManagerConfig := TModelInputManagerConfig.CreateDefault;
     FTextFormViewer := TTableFormViewerConfig.CreateDefault;
     raise Exception.Create('There is no valid configuration file for this application');
     end;

  Obj.TryGetValue ('config', configObj);
  mainObj := configObj.Get('mainForm').JsonValue as TJSONObject;
  FMainConfig := TfrmMainConfig.readFromJson(mainObj);

  modelManagerObj := configObj.Get('modelInputManager').JsonValue as TJSONObject;
  FModelInputManagerConfig := TModelInputManagerConfig.readFromJson(modelManagerObj);

  tableViewerObj := configObj.Get('textFormViewer').JsonValue as TJSONObject;
  FTextFormViewer := TTableFormViewerConfig.readFromJson(tableViewerObj);
end;


procedure saveConfigurationFile (fileName : string);
var path : string;
begin
  path := getConfigPath;
  if DirectoryExists(path) then
     begin
     //TFile.WriteAllText(path + fileName, TJson.ObjectToJsonString(configOpts));
     configOpts.saveToJson(path + fileName);
     end
  else
     TDirectory.CreateDirectory(path);
end;


initialization
  configOpts := TConfigOptions.Create;
//  configOpts.fontSize := 16;
//  configOpts.outputPanelWidth := 480;
//  configOpts.upperOutputPanelHeight := 50.0;  // 50 %
//  configOpts.IsGraphPanelOpen := True;
//  configOpts.IsTabularPanelOpen := True;
//  configOpts.formTop := 245;
//  configOpts.formLeft := 452;
//  configOpts.formWidth := 1290;
//  configOpts.formHeight := 840;
//  configOpts.FTextFormViewer := TTableFormViewerConfig.Create;
end.
