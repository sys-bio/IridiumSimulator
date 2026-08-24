unit uPreferences;

{ User preferences, persisted as JSON.

  What survives between sessions and nothing else: the recent-files list,
  the main window's bounds, and the two splitter positions. Model content
  never comes near this file — a preference is something the user set up
  once and should not have to set up again, not a document.

  RTL-only by design: System.JSON and System.IOUtils, no FMX. The caller
  reads and writes plain numbers, so a form knows how to place itself and
  this unit does not have to know what a form is.

  **Nothing here may prevent the application starting.** A preferences file
  that is missing, truncated, hand-edited into nonsense or written by a
  later version must leave the app running on its defaults. Every read is
  therefore defaulted and every failure swallowed — the one place where
  quietly carrying on beats reporting. }

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.JSON;

const
  { Ten is the length at which the list is still scannable as a menu. }
  MAX_RECENT_FILES = 10;

type
  TIridiumPreferences = class
  private
    FRecentFiles: TStringList;
    function  GetRecentCount: Integer;
    function  GetRecentFile(AIndex: Integer): string;
    procedure ReadJson(const AJson: TJSONObject);
    function  WriteJson: TJSONObject;
  public
    { Main window bounds, in pixels. HasWindowBounds is False until a
      session has actually saved some — the difference between "the user
      likes this size" and "no one has ever said", which is what stops a
      first run from being placed at 0,0 with zero size. }
    HasWindowBounds: Boolean;
    WindowLeft:      Integer;
    WindowTop:       Integer;
    WindowWidth:     Integer;
    WindowHeight:    Integer;

    { Splitter positions, stored as the size of the panel each splitter
      resizes rather than as the splitter's own coordinate. A coordinate is
      meaningless at a different window size; a panel size is not.

        SliderPanelHeight  Splitter1, between the editor and the sliders.
        OutputPanelWidth   Splitter2, between the editor and the output.

      Zero means "never saved", so the designed layout stands. }
    SliderPanelHeight: Double;
    OutputPanelWidth:  Double;

    constructor Create;
    destructor  Destroy; override;

    { Where the file lives. Per-user, and per-platform by convention:
      %APPDATA%\Iridium on Windows, ~/Library/Application Support/Iridium
      on macOS. }
    class function FolderPath: string;
    class function FilePath: string;

    { Neither raises. Load leaves defaults in place when there is nothing
      readable; Save answers False if it could not write, which a caller
      may report or ignore — closing down is not a good moment to argue. }
    procedure Load;
    function  Save: Boolean;

    { Newest first, de-duplicated case-insensitively (Windows paths differ
      only in case for the same file), capped at MAX_RECENT_FILES. }
    procedure AddRecentFile(const APath: string);
    procedure RemoveRecentFile(const APath: string);

    property RecentCount: Integer read GetRecentCount;
    property RecentFiles[AIndex: Integer]: string read GetRecentFile;
  end;

implementation

{ -- construction --------------------------------------------------------- }

constructor TIridiumPreferences.Create;
begin
  inherited Create;
  FRecentFiles := TStringList.Create;
  FRecentFiles.CaseSensitive := False;
end;

destructor TIridiumPreferences.Destroy;
begin
  FRecentFiles.Free;
  inherited;
end;

{ -- location ------------------------------------------------------------- }

class function TIridiumPreferences.FolderPath: string;
begin
  {$IFDEF MACOS}
  { GetHomePath is /Users/<name> here, and application data belongs under
    Library/Application Support by macOS convention. }
  Result := TPath.Combine(TPath.Combine(TPath.GetHomePath, 'Library'),
                          'Application Support');
  {$ELSE}
  { On Windows GetHomePath is already %APPDATA% (...\AppData\Roaming),
    which is where per-user settings belong. }
  Result := TPath.GetHomePath;
  {$ENDIF}
  Result := TPath.Combine(Result, 'Iridium');
end;

class function TIridiumPreferences.FilePath: string;
begin
  Result := TPath.Combine(FolderPath, 'preferences.json');
end;

{ -- recent files --------------------------------------------------------- }

function TIridiumPreferences.GetRecentCount: Integer;
begin
  Result := FRecentFiles.Count;
end;

function TIridiumPreferences.GetRecentFile(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < FRecentFiles.Count) then
    Result := FRecentFiles[AIndex]
  else
    Result := '';
end;

procedure TIridiumPreferences.AddRecentFile(const APath: string);
var
  Full: string;
begin
  if Trim(APath) = '' then Exit;

  { The full path, so two models with the same file name in different
    folders are two entries and open the right one. }
  try
    Full := TPath.GetFullPath(APath);
  except
    Full := APath;
  end;

  { Re-opening a file moves it to the top rather than adding it twice. }
  RemoveRecentFile(Full);
  FRecentFiles.Insert(0, Full);

  while FRecentFiles.Count > MAX_RECENT_FILES do
    FRecentFiles.Delete(FRecentFiles.Count - 1);
end;

procedure TIridiumPreferences.RemoveRecentFile(const APath: string);
var
  I: Integer;
begin
  for I := FRecentFiles.Count - 1 downto 0 do
    if SameText(FRecentFiles[I], APath) then
      FRecentFiles.Delete(I);
end;

{ -- JSON ----------------------------------------------------------------- }

procedure TIridiumPreferences.ReadJson(const AJson: TJSONObject);
var
  Arr:  TJSONArray;
  Win:  TJSONObject;
  Lay:  TJSONObject;
  V:    TJSONValue;
  I:    Integer;
  S:    string;
begin
  FRecentFiles.Clear;
  if AJson.TryGetValue<TJSONArray>('recentFiles', Arr) then
    for I := 0 to Arr.Count - 1 do
    begin
      V := Arr.Items[I];
      if (V is TJSONString) and (FRecentFiles.Count < MAX_RECENT_FILES) then
      begin
        S := V.Value;
        if Trim(S) <> '' then
          FRecentFiles.Add(S);
      end;
    end;

  if AJson.TryGetValue<TJSONObject>('window', Win) then
  begin
    { All four or none: three of four leaves the window half-placed, which
      looks like a bug rather than a preference. }
    HasWindowBounds :=
      Win.TryGetValue<Integer>('left',   WindowLeft) and
      Win.TryGetValue<Integer>('top',    WindowTop) and
      Win.TryGetValue<Integer>('width',  WindowWidth) and
      Win.TryGetValue<Integer>('height', WindowHeight);
  end;

  if AJson.TryGetValue<TJSONObject>('layout', Lay) then
  begin
    if not Lay.TryGetValue<Double>('sliderPanelHeight', SliderPanelHeight) then
      SliderPanelHeight := 0;
    if not Lay.TryGetValue<Double>('outputPanelWidth', OutputPanelWidth) then
      OutputPanelWidth := 0;
  end;
end;

function TIridiumPreferences.WriteJson: TJSONObject;
var
  Arr: TJSONArray;
  Win: TJSONObject;
  Lay: TJSONObject;
  I:   Integer;
begin
  Result := TJSONObject.Create;

  Arr := TJSONArray.Create;
  for I := 0 to FRecentFiles.Count - 1 do
    Arr.Add(FRecentFiles[I]);
  Result.AddPair('recentFiles', Arr);

  if HasWindowBounds then
  begin
    Win := TJSONObject.Create;
    Win.AddPair('left',   TJSONNumber.Create(WindowLeft));
    Win.AddPair('top',    TJSONNumber.Create(WindowTop));
    Win.AddPair('width',  TJSONNumber.Create(WindowWidth));
    Win.AddPair('height', TJSONNumber.Create(WindowHeight));
    Result.AddPair('window', Win);
  end;

  Lay := TJSONObject.Create;
  Lay.AddPair('sliderPanelHeight', TJSONNumber.Create(SliderPanelHeight));
  Lay.AddPair('outputPanelWidth',  TJSONNumber.Create(OutputPanelWidth));
  Result.AddPair('layout', Lay);
end;

{ -- load / save ---------------------------------------------------------- }

procedure TIridiumPreferences.Load;
var
  Text: string;
  Json: TJSONValue;
begin
  HasWindowBounds   := False;
  SliderPanelHeight := 0;
  OutputPanelWidth  := 0;
  FRecentFiles.Clear;

  try
    if not TFile.Exists(FilePath) then Exit;
    Text := TFile.ReadAllText(FilePath, TEncoding.UTF8);
  except
    Exit;
  end;

  Json := nil;
  try
    Json := TJSONObject.ParseJSONValue(Text);
    if Json is TJSONObject then
      ReadJson(TJSONObject(Json));
  except
    { A hand-edited or truncated file must not stop the app starting. }
  end;
  Json.Free;
end;

function TIridiumPreferences.Save: Boolean;
var
  Json: TJSONObject;
begin
  Result := False;
  Json := WriteJson;
  try
    try
      if not TDirectory.Exists(FolderPath) then
        TDirectory.CreateDirectory(FolderPath);
      { Formatted, because a preferences file is one a user may well open
        to see what the application is remembering about them. }
      TFile.WriteAllText(FilePath, Json.Format(2), TEncoding.UTF8);
      Result := True;
    except
      { A read-only profile or a full disk costs the user their window
        position, and nothing else. Not worth a dialog at shutdown. }
    end;
  finally
    Json.Free;
  end;
end;

end.
