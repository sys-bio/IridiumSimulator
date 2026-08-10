unit uBioModelsCache;

interface

uses
  System.SysUtils,
  System.JSON,
  System.StrUtils,
  System.Net.HttpClient,
  System.Net.URLClient;

type
  TBiomodel = record
    ID: string;
    Name: string;
    Title: string;
    Synopsis: string;
    Authors: string;
    Journal: string;
    Date: string;
  end;

  TBiomodelArray = TArray<TBiomodel>;

  TBiomodelsCache = class
  private
    FHTTPClient: THTTPClient;

    { The whole cache, downloaded and parsed once. Search is called on
      every keystroke, and re-fetching a multi-megabyte JSON document each
      time would make typing unusable — and would hammer GitHub. }
    FModels: TBiomodelArray;
    FLoaded: Boolean;
    procedure EnsureLoaded;

    function GetURL(const URL: string): string;
    function GetString(const Obj: TJSONObject;
      const Name: string): string;
    function GetAuthors(const Obj: TJSONObject): string;
    function NormalizeModelID(const ID: string): string;
    function GetStoreFileURL(const ModelID: string;
      out FileName: string): string;

  public
    constructor Create;
    destructor Destroy; override;

    function Search(const Term: string;
      Limit: Integer = 20): TBiomodelArray;

    { Drop the downloaded cache so the next Search fetches it again. }
    procedure Refresh;

    function GetModel(const ModelID: string): string;
  end;

implementation

const
  CACHE_JSON_URL =
    'https://raw.githubusercontent.com/sys-bio/biomodels_cache/main/' +
    'cache/biomodels_cache.json';

  GITHUB_CONTENTS_URL =
    'https://api.github.com/repos/sys-bio/BiomodelsStore/' +
    'contents/biomodels/%s';

{ TBiomodelsCache }

constructor TBiomodelsCache.Create;
begin
  inherited Create;

  FHTTPClient := THTTPClient.Create;

  FHTTPClient.ConnectionTimeout := 30000;
  FHTTPClient.ResponseTimeout := 120000;
  FHTTPClient.UserAgent := 'BioModelsCache/1.0';
end;

destructor TBiomodelsCache.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TBiomodelsCache.GetURL(const URL: string): string;
var
  Response: IHTTPResponse;
begin
  Response := FHTTPClient.Get(URL);

  if (Response.StatusCode < 200) or
     (Response.StatusCode >= 300) then
  begin
    raise Exception.CreateFmt(
      'HTTP error %d: %s',
      [Response.StatusCode, Response.StatusText]);
  end;

  Result := Response.ContentAsString(TEncoding.UTF8);
end;

function TBiomodelsCache.GetString(
  const Obj: TJSONObject;
  const Name: string): string;
var
  Value: TJSONValue;
begin
  Result := '';

  Value := Obj.GetValue(Name);

  if Value <> nil then
    Result := Value.Value;
end;

function TBiomodelsCache.GetAuthors(
  const Obj: TJSONObject): string;
var
  Value: TJSONValue;
  Authors: TJSONArray;
  I: Integer;
begin
  Result := '';

  Value := Obj.GetValue('authors');

  if not (Value is TJSONArray) then
    Exit;

  Authors := Value as TJSONArray;

  for I := 0 to Authors.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + '; ';

    Result := Result + Authors.Items[I].Value;
  end;
end;

function TBiomodelsCache.NormalizeModelID(
  const ID: string): string;
var
  N: Int64;
begin
  Result := Trim(ID);

  if Result = '' then
    Exit;

  if TryStrToInt64(Result, N) then
    Result := Format('BIOMD%.10d', [N]);
end;

procedure TBiomodelsCache.EnsureLoaded;
var
  Text: string;
  Root: TJSONObject;
  Pair: TJSONPair;
  Model: TJSONObject;
  Count: Integer;
  Biomodel: TBiomodel;
begin
  if FLoaded then
    Exit;

  Text := GetURL(CACHE_JSON_URL);

  Root := TJSONObject.ParseJSONValue(Text) as TJSONObject;

  if Root = nil then
    raise Exception.Create(
      'The BioModels cache JSON could not be parsed.');

  try
    Count := 0;
    SetLength(FModels, Root.Count);

    for Pair in Root do
    begin
      if not (Pair.JsonValue is TJSONObject) then
        Continue;

      Model := Pair.JsonValue as TJSONObject;

      Biomodel.ID := Pair.JsonString.Value;
      Biomodel.Name := GetString(Model, 'name');
      Biomodel.Title := GetString(Model, 'title');
      Biomodel.Synopsis := GetString(Model, 'synopsis');
      Biomodel.Authors := GetAuthors(Model);
      Biomodel.Journal := GetString(Model, 'journal');
      Biomodel.Date := GetString(Model, 'date');

      FModels[Count] := Biomodel;
      Inc(Count);
    end;

    SetLength(FModels, Count);
    FLoaded := True;

  finally
    Root.Free;
  end;
end;

procedure TBiomodelsCache.Refresh;
begin
  FModels := nil;
  FLoaded := False;
end;


function TBiomodelsCache.Search(const Term: string; Limit: Integer): TBiomodelArray;
var
  LowerTerm: string;
  Count: Integer;
  I: Integer;
  Biomodel: TBiomodel;
begin
  if Limit < 1 then
    raise EArgumentOutOfRangeException.Create(
      'Limit must be greater than zero.');

  Result := nil;

  { Pos('', S) is 0, so an empty term would match nothing anyway; return
    early rather than download the cache to discover that. }
  if Term = '' then
    Exit;

  EnsureLoaded;

  LowerTerm := LowerCase(Term);
  Count := 0;

  for I := 0 to High(FModels) do
  begin
    Biomodel := FModels[I];

    if (Pos(LowerTerm, LowerCase(Biomodel.ID)) > 0) or
       (Pos(LowerTerm, LowerCase(Biomodel.Name)) > 0) or
       (Pos(LowerTerm, LowerCase(Biomodel.Title)) > 0) or
       (Pos(LowerTerm, LowerCase(Biomodel.Synopsis)) > 0) or
       (Pos(LowerTerm, LowerCase(Biomodel.Authors)) > 0) or
       (Pos(LowerTerm, LowerCase(Biomodel.Journal)) > 0) then
    begin
      Inc(Count);

      SetLength(Result, Count);
      Result[Count - 1] := Biomodel;

      if Count >= Limit then
        Break;
    end;
  end;
end;

function TBiomodelsCache.GetStoreFileURL(const ModelID: string; out FileName: string): string;
var
  Text: string;
  Contents: TJSONArray;
  Item: TJSONValue;
  Obj: TJSONObject;
  Name: string;
  DownloadURL: string;
begin
  Result := '';
  FileName := '';

  Text := GetURL(
    Format(GITHUB_CONTENTS_URL, [ModelID]));

  Contents := TJSONObject.ParseJSONValue(Text) as TJSONArray;

  if Contents = nil then
    raise Exception.Create(
      'GitHub did not return a model directory listing.');

  try
    for Item in Contents do
    begin
      if not (Item is TJSONObject) then
        Continue;

      Obj := Item as TJSONObject;

      if GetString(Obj, 'type') <> 'file' then
        Continue;

      Name := GetString(Obj, 'name');

      if not EndsText('.xml', Name) then
        Continue;

      DownloadURL := GetString(Obj, 'download_url');

      if DownloadURL = '' then
        raise Exception.CreateFmt(
          'GitHub returned no download URL for %s.',
          [Name]);

      FileName := Name;
      Result := DownloadURL;
      Exit;
    end;

  finally
    Contents.Free;
  end;

  raise Exception.CreateFmt(
    'No XML model file found for %s.',
    [ModelID]);
end;

function TBiomodelsCache.GetModel(const ModelID: string): string;
var
  NormalizedID: string;
  FileName: string;
  URL: string;
begin
  NormalizedID := NormalizeModelID(ModelID);

  if not StartsText(
    'BIOMD',
    UpperCase(NormalizedID)) then
  begin
    raise EArgumentException.Create(
      'Model ID must be a BIOMD identifier or numeric ID.');
  end;

  URL := GetStoreFileURL(
    NormalizedID,
    FileName);

  Result := GetURL(URL);
end;

end.
