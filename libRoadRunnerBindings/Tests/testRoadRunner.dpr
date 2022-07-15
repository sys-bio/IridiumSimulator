program testRoadRunner;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Classes,
  uRRList in '..\uRRList.pas',
  uRRTypes in '..\uRRTypes.pas',
  uComplex in '..\uComplex.pas',
  uEigenvalues in '..\uEigenvalues.pas',
  uIMSLLib in '..\uIMSLLib.pas',
  uJVector in '..\uJVector.pas',
  uMatrix in '..\uMatrix.pas',
  uRoadRunner.API in '..\uRoadRunner.API.pas',
  uRoadRunner in '..\uRoadRunner.pas';

var roadRunnerLoaded : boolean;
    errMsg : AnsiString;
    data : T2DDoubleArray;
    i, j, k : integer;
    elasticities : TRRList;
    handle : pointer;
    count : integer;

    listItem, sublistItem, subsublistItem : PRRListItemRecord;
    list : PRRListRecordHandle;
    sublist, subsublist : PRRListRecordHandle;

procedure loadRoadRunnerAPI;
var methodList : TStringList;
begin
  methodList := TStringList.Create;
  if not loadRoadRunner (errMsg, methodList) then
     begin
     roadRunnerLoaded := false;
     end
  else
     roadRunnerLoaded := true;
end;


begin
  try
    loadRoadRunnerAPI;
    if not roadRunnerLoaded then
       begin
       writeln ('Failed to roadrunner: ' + errMsg);
       readln;
       exit;
       end;

    handle := TRoadRunnerAPI.libCreateRRInstance();
    writeln ('RoadRunner Version: ' + inttostr (TRoadRunnerAPI.libGetVersionInt()));

    TRoadRunnerAPI.libLoadSBMLFromString(handle, PAnsiChar ('.\models\model1.xml'));

    list := TRoadRunnerAPI.libGetElasticityIds (handle);
    count := TRoadRunnerAPI.libGetListLength (list);
    for i := 0 to Count - 1 do
        begin
        listItem := TRoadRunnerAPI.libGetListItem (list, i);
        if TRoadRunnerAPI.libIsListItemString (listItem).ToBoolean   then
           writeln ('String: ', TRoadRunnerAPI.libGetStringListItem (listItem));
        if TRoadRunnerAPI.libIsListItemList (listItem).ToBoolean   then
           begin
           writeln ('List');
           sublist := TRoadRunnerAPI.libGetList (listItem);
           for j := 0 to TRoadRunnerAPI.libGetListLength (sublist) - 1 do
               begin
               sublistItem := TRoadRunnerAPI.libGetListItem (sublist, j);
               if TRoadRunnerAPI.libIsListItemString (sublistItem).ToBoolean   then
                  writeln ('  String: ', TRoadRunnerAPI.libGetStringListItem (sublistItem));
               if TRoadRunnerAPI.libIsListItemList (sublistItem).ToBoolean   then
                  begin
                  writeln ('  List');
                  subsublist := TRoadRunnerAPI.libGetList (sublistItem);
                  for k := 0 to TRoadRunnerAPI.libGetListLength (subsublist) - 1 do
                      begin
                      subsublistItem := TRoadRunnerAPI.libGetListItem (subsublist, k);
                      if TRoadRunnerAPI.libIsListItemString (subsublistItem).ToBoolean   then
                         writeln ('  String: ', TRoadRunnerAPI.libGetStringListItem (subsublistItem));
                      end;
                  end;
               end;
           end;
        end;

    readln;
  except
    on E: Exception do
      begin
      Writeln(E.ClassName, ': ', E.Message);
      readln;
      end;
  end;
end.
