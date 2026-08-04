unit uCommonTypes;

interface

Uses Generics.Collections,
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

end.
