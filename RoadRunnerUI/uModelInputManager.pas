unit uModelInputManager;

interface

Uses SysUtils,
     Classes,
     FMX.Dialogs,
     FMX.Memo,
     StrUtils,
     FrameMemoLineCount,
     Json;//, uController;

type
  TModelInputManagerConfig = class (TObject)
     private
       FFontSize : integer;
     public
       class var configName : string;
       class function readSection (obj : TJSONObject) : TModelInputManagerConfig;
       function saveToJson : TJSONObject;
       constructor readFromJson (obj : TJSONObject);
       constructor Create;
       constructor CreateDefault;
     published
       property fontSize : integer read FFontSize write FFontSize;
  end;


  TModelErrorState = record
    ok : boolean;
    errMsg : string;
    sbmlStr : string;
    runStr : string;
  end;

  TModelInputManager = class (TObject)
      saveSBMLDialog : TSaveDialog;
      openSBMLDialog : TOpenDialog;

      antimonyStr : string;

      modelMemo : TMemo;  // reference to a TMemo control
      antimonyLoaded : boolean;
      currentAntimonyFileName : string;
      function  getSBMLFromAntimony (antStr : string) : TModelErrorState;
      function  getAntimonyFromSBML (sbmlStr : string) : string;

      function loadAntimonyFromFile (fileName : string; var errMsg : string) : boolean;

      function checkForValidAntimony : boolean;
      procedure loadAntimonyLibrary;
      procedure SetInputMemo (memo : TMemo);
      procedure SetMemoFontSize (fontSize : integer);
      procedure exportSBML;
      function  importSBML (var path : string) : string;
      constructor Create;
  end;


implementation

Uses IOUtils, uConfiguration, uAntimonyAPI;

class function TModelInputManagerConfig.readSection (obj : TJSONObject) : TModelInputManagerConfig;
var mainObj : TJSONObject;
begin
  if obj.TryGetValue(TModelInputManagerConfig.configName, mainObj) then
     result := TModelInputManagerConfig.readFromJson(mainObj)
  else
     result := TModelInputManagerConfig.CreateDefault;
end;


constructor TModelInputManagerConfig.Create;
begin
  inherited;
  fontSize := 16;
end;

constructor TModelInputManagerConfig.CreateDefault;
begin
  Create;
end;


constructor TModelInputManagerConfig.readFromJson (obj : TJSONObject);
begin
  fontSize := obj.GetValue<integer>('fontSize', 16);
end;


function TModelInputManagerConfig.saveToJson : TJSONObject;
var obj : TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair ('fontSize', fontSize);
  if fontSize = 0 then
     fontSize := 16;
  result := obj;
end;


constructor TModelInputManager.Create;
begin
  antimonyLoaded := False;
  currentAntimonyFileName := '';

  loadAntimonyLibrary;
  saveSBMLDialog := TSaveDialog.Create (nil);
  saveSBMLDialog.DefaultExt := '.xml';
  saveSBMLDialog.Filter := 'SBML File|*.xml|Any files|*.*';

  openSBMLDialog := TOpenDialog.Create (nil);
  openSBMLDialog.DefaultExt := '.xml';
  openSBMLDialog.Filter := 'SBML File|*.xml|Any files|*.*';

//  if not antimonyLoaded then
//    begin
//      showmessage(errMsg);
//      lblVersion.Text := errMsg;
//    end
//  else
//    begin
//      lblVersion.Text := 'libAntimony Version: ' + 'Antimony does not support version numbers ????';
//    end;
end;


procedure TModelInputManager.setInputMemo (Memo : TMemo);
begin
  modelMemo := Memo;
end;


procedure TModelInputManager.SetMemoFontSize (FontSize : integer);
begin
  ModelMemo.TextSettings.Font.Size := FontSize;

  configOpts.modelInputManagerConfig.FFontSize := fontSize;
end;


function TModelInputManager.getSBMLFromAntimony (antStr : string) : TModelErrorState;
var offset : integer;
    runStr : string;
    r : string;
begin
  r := '#RUN';
  // Check for #run section
  offset := Pos (r.ToLower, antStr);
  if offset <> 0 then
     begin
     runStr := copy (antStr, offset + length ('#RUN'), length (antStr));
     runStr := ReplaceStr(runStr, sLineBreak, ' ');
     antStr := copy (antStr, 0, offset);
     end;

  result := uAntimonyAPI.getSBMLFromAntimony(antstr);
  if runStr <> '' then
     result.runStr := runStr
  else
     result.runStr := '';
end;


function TModelInputManager.getAntimonyFromSBML (sbmlStr : string) : string;
begin
  result := uAntimonyAPI.getAntimonyFromSBML(sbmlStr);
end;


function TModelInputManager.loadAntimonyFromFile (fileName : string; var errMsg : string) : boolean;
begin
   result := True;
   try
     antimonyStr := TFile.ReadAllText(fileName);
   except
     on E: Exception do
        begin
        errMsg := e.Message;
        result := False;
        end;
   end;
end;


function TModelInputManager.checkForValidAntimony : boolean;
begin
  if uAntimonyAPI.ant_loadAntimonyStringWithException(antimonyStr) <> -1 then
     result := False
  else
     result := True;
end;


procedure TModelInputManager.loadAntimonyLibrary;
var
  errMsg : string;
begin
  if not uAntimonyAPI.loadAntimonyLibrary (errMsg) then
    begin
      antimonyLoaded := false;
      showmessage(errMsg);
    end
  else
    antimonyLoaded := true;
end;


procedure TModelInputManager.exportSBML;
var sbmlStr : string;
    modelErrorState : TModelErrorState;
begin
  if SaveSBMLDialog.Execute then
    begin
      modelErrorState := uAntimonyAPI.getSBMLFromAntimony(ModelMemo.Lines.Text);
      if modelErrorState.ok then
         TFile.WriteAllText(SaveSBMLDialog.FileName, modelErrorState.sbmlStr)
      else
         showmessage (modelErrorState.errMsg);
    end;
end;


function TModelInputManager.importSBML (var path : string) : string;
begin
  result := '';
  if OpenSBMLDialog.Execute then
    begin
      result := TFile.ReadAllText(OpenSBMLDialog.FileName);
      path := OpenSBMLDialog.FileName;
    end;
end;


initialization
  TModelInputManagerConfig.configName := 'modelInputManager';
end.
