unit uSliderCommon;

interface

Uses SysUtils, FMX.StdCtrls, Generics.Collections;

type
  TSliderInfo = class (TObject)
        parameter : string;
        initialLoadedValue : double;
        index : integer;
        minValue : double;
        maxValue : double;
        stepValue : double;
        slider : TTrackBar;
        lb1, lb2 : TLabel;
        closeButton : TSpeedButton;
        configButton : TSpeedButton;
        constructor Create (name : string; index : integer; minValue, maxValue, stepValue : double; lb1, lb2 : TLabel);
  end;

  TSliderList = TObjectList<TSliderInfo>;

implementation

constructor TSliderInfo.Create (name : string; index : integer; minValue, maxValue, stepValue : double; lb1, lb2 : TLabel);
begin
  Self.parameter := name;
  Self.Index := Index;
  Self.minValue := minValue;
  Self.maxValue := maxValue;
  Self.stepValue := stepValue;
  Self.lb1 := lb1;
  Self.lb2 := lb2;
end;


end.
