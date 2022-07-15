unit uConfiguration;

interface

Uses SysUtils, REST.Json, FMX.Dialogs;

const
   CONFIG_FILE_NAME = 'iridium.json';

type
  TConfigOptions = class (TObject)
    private
     FFontSize : integer;
     FGraphPanelWidth : single;
     FIsGraphPanelOpen : boolean;
     FFormWidth : integer;
     FFormHeight : integer;
     FFormLeft : integer;
     FFormTop : integer;
    published
      property fontSize : integer read FFontSize write FFontSize;
      property graphPanelWidth : single read FGraphPanelWidth write FGraphPanelWidth;
      property IsGraphPanelOpen : boolean read FIsGraphPanelOpen write FIsGraphPanelOpen;
      property formLeft : integer read FFormLeft write FFormLeft;
      property formTop : integer read FFormTop write FFormTop;
      property formWidth : integer read FFormWidth write FFormWidth;
      property formHeight : integer read FFormHeight write FFormHeight;
  end;

  procedure saveConfigurationFile (fileName : string);
  procedure readConfigurationFile (fileName : string);
  function  getConfigPath : string;

  var configOpts : TConfigOptions;

implementation

Uses IOUtils;

function getConfigPath : string;
begin
{$IFDEF OSX}
  result := TPath.Combine(TPath.GetLibraryPath, 'Application Support') + TPath.DirectorySeparatorChar;
{$ELSE}
  result := TPath.GetHomePath + TPath.DirectorySeparatorChar
{$ENDIF}
end;

procedure readConfigurationFile (fileName : string);
var cjson : string;
    path : string;
begin
  path := getConfigPath + fileName;
  if FileExists (path) then
     begin
     cJson := TFile.ReadAllText(path);
     configOpts := TJson.JsonToObject<TConfigOptions>(cJson);
     if configOpts.FGraphPanelWidth = 0 then
        configOpts.FGraphPanelWidth := 480;
     if configOpts.formWidth = 0 then
        configOpts.formWidth := 1290;
     if configOpts.formHeight = 0 then
        configOpts.formHeight := 840;

     end;
end;


procedure saveConfigurationFile (fileName : string);
var path : string;
begin
  path := getConfigPath;
  if DirectoryExists(path) then
     TFile.WriteAllText(path + fileName, TJson.ObjectToJsonString(configOpts))
  else
     TDirectory.CreateDirectory(path);
end;


initialization
  configOpts := TConfigOptions.Create;
  configOpts.fontSize := 16;
  configOpts.graphPanelWidth := 480;
  configOpts.IsGraphPanelOpen := True;
  configOpts.formTop := 245;
  configOpts.formLeft := 452;
  configOpts.formWidth := 1290;
  configOpts.formHeight := 840;
end.
