unit uCommonTypes;

interface

Uses System.Classes,
     Generics.Collections,
     uPlotSeries;

type
  TModelErrorState = record
      errMsg : String;
      sbmlStr : String;
      ok : Boolean;
  end;

  TLoadDataFile = class
      FileName : string;
      ParameterName : string; // The x column
      Series : TList<TPlotSeries>;
      constructor Create;
      destructor Destroy; override;
  end;

  { The CSV overlays belonging to one analysis panel.

    Loaded data is scoped to the panel it was loaded on: data loaded on the
    time-course plot describes a time course and has no meaning on a parameter
    scan, so each panel keeps its own catalogue, its own dropdown selection and
    its own record of what was on screen when the user switched away.

      Files         - every dataset loaded on this panel; owns its entries.
      DisplayedIds  - SeriesId of each overlay drawn when the panel was left,
                      so returning to it re-draws exactly that set (which may
                      be several datasets, when "overlay data" is on).
      SelectedIndex - the filename dropdown's selection, an index into Files. }
  TPanelDataFiles = class
      Files         : TList<TLoadDataFile>;
      DisplayedIds  : TStringList;
      SelectedIndex : Integer;
      constructor Create;
      destructor Destroy; override;
      procedure ClearFiles;
  end;

implementation

constructor TLoadDataFile.Create;
begin
  inherited;
  Series := TList<TPlotSeries>.Create;
end;

destructor TLoadDataFile.Destroy;
var
  I: Integer;
begin
  { The clones handed to Series belong to this object — the plot keeps its own
    copies — so free them along with the list. }
  for I := 0 to Series.Count - 1 do
    Series[I].Free;
  Series.Free;
  inherited;
end;

constructor TPanelDataFiles.Create;
begin
  inherited;
  Files         := TList<TLoadDataFile>.Create;
  DisplayedIds  := TStringList.Create;
  SelectedIndex := -1;
end;

destructor TPanelDataFiles.Destroy;
begin
  ClearFiles;
  Files.Free;
  DisplayedIds.Free;
  inherited;
end;

{ Files owns its TLoadDataFile entries (which own their cloned series), so
  emptying the list means freeing them. }
procedure TPanelDataFiles.ClearFiles;
var
  I: Integer;
begin
  for I := 0 to Files.Count - 1 do
    Files[I].Free;
  Files.Clear;
  DisplayedIds.Clear;
  SelectedIndex := -1;
end;

end.
