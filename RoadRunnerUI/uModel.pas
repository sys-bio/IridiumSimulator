unit uModel;

interface

Uses SysUtils, Classes, uRoadRunner, uAntimonyAPI;

type
  TModel = class (TObject)

     //roadRunner : TRoadRunner;

     antStr : string;
     sbmlStr : string;
     errMsg : string;

     //function loadSBMLFromString (sbmlStr : string) : boolean;
     //function loadAntModelFromFile (fileName : string) : boolean;
     constructor Create;

  end;


implementation

Uses IOUtils, FMX.Dialogs;


constructor TModel.Create;
begin
  //roadRunner := TRoadRunner.Create;
end;


//function TModel.loadSBMLFromString (sbmlStr : string) : boolean;
//begin
//  result := roadrunner.loadSBMLFromString(sbmlStr);
//end;
//
//
//function TModel.loadAntModelFromFile (fileName : string) : boolean;
//begin
//  antStr := TFile.ReadAllText(fileName);
//  sbmlStr := uAntimonyAPI.getSBMLFromAntimony(antStr);
//  if not roadrunner.loadSBMLFromString(sbmlStr) then
//     begin
//     errMsg := roadrunner.getLastError();
//     exit (False);
//     end
//  else
//    exit (True);
//end;


end.
