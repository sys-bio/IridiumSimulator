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
begin
  Series.Free;
  inherited;
end;

end.
