unit uConfiguration;

interface

Uses SysUtils, REST.Json, FMX.Dialogs,
     uTableFrameViewer,
     ufMainConfig,
     uTimeCourseConfig,
     uModelInputManager;

const
   CONFIG_FILE_NAME = 'iridium.json';

type
  TConfigOptions = class (TObject)
    private
     FMainConfig : TfrmMainConfig;
     FModelInputManagerConfig : TModelInputManagerConfig;
     FTimeCourseConfig : TTimeCourseConfig;
     FTextFormViewer : TTableFormViewerConfig;
     FUIStyle : string;
    published
      property UIStyle : string read FUIStyle write FUIStyle;
      property mainConfig : TfrmMainConfig read FMainConfig write FMainConfig;
      property timeCourceConfig : TTimeCourseConfig read FTimeCourseConfig write FTimeCourseConfig;
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
       end
    else
       begin
       configOpts.FMainConfig := TfrmMainConfig.CreateDefault;
       configOpts.FModelInputManagerConfig := TModelInputManagerConfig.CreateDefault;
       configOpts.FTextFormViewer := TTableFormViewerConfig.CreateDefault;
       configOpts.FTimeCourseConfig := TTimeCourseConfig.CreateDefault;
       configOpts.UIStyle := 'MineShaft_Win_Style';
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
   mainObj.AddPair ('style', FUIStyle);
   mainObj.AddPair (TfrmMainConfig.configName, FMainConfig.saveToJson);
   mainObj.AddPair (TTimeCourseConfig.configName, FTimeCourseConfig.saveToJson);
   mainObj.AddPair (TModelInputManagerConfig.configName, FModelInputManagerConfig.saveToJson);
   mainObj.AddPair (TTableFormViewerConfig.configName, FTextFormViewer.saveToJson);

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
    tmp,
    configObj,
    mainFormObj,
    modelManagerObj,
    timeCourseObj,
    tableViewerObj : TJSONObject;
    astr : string;
    version : string;
    pair : TJSONPair;
begin
  astr := TFile.ReadAllText(fileName);
  obj := TJSONObject.ParseJSONValue(astr) as TJSONObject;
  if not Obj.TryGetValue (SIGNATURE, version) then
     begin
     FMainConfig := TfrmMainConfig.CreateDefault;
     FModelInputManagerConfig := TModelInputManagerConfig.CreateDefault;
     FTextFormViewer := TTableFormViewerConfig.CreateDefault;
     FTimeCourseConfig := TTimeCourseConfig.createDefault;
     UIStyle := 'MineShaft_Win_Style';
     exit;
     end;

  Obj.TryGetValue ('config', configObj);
  UIStyle := configObj.GetValue<string>('style', 'MineShaft_Win_Style');
  if UIStyle = '' then
     UIStyle := 'MineShaft_Win_Style';

  FMainConfig := TfrmMainConfig.readSection (configObj);
  FTimeCourseConfig := TTimeCourseConfig.readSection (configObj);
  FModelInputManagerConfig := TModelInputManagerConfig.readSection (configObj);
  FTextFormViewer := TTableFormViewerConfig.readSection (configObj);
end;


procedure saveConfigurationFile (fileName : string);
var path : string;
begin
  path := getConfigPath;
  if DirectoryExists(path) then
     begin
     configOpts.saveToJson(path + fileName);
     end
  else
     TDirectory.CreateDirectory(path);
end;


initialization
  configOpts := TConfigOptions.Create;
end.
