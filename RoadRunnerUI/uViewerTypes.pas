unit uViewerTypes;

interface

// When a viewer registers with the controller, the control will pass this
// record to the viewer when the viewer needs to be updated.

Uses Rtti;

type
  TViewerPackage = class (TObject)
      XAxisTitle : string;
      XColumnIndex : integer;
      YColumnChoice: array of boolean;
      YColumnNames : array of string;
      showLegend : boolean;
      timeStart : double;   // if autoXScale is false you can use these
      timeEnd : double;     // to specify the x-axis limits
      palette : string;     // color palette name

      constructor Create;
  end;

  // The sender will normally be a reference to the controller
  TNotifySimulateEvent = procedure(Sender: TObject; viewPackage : TViewerPackage) of object;
  TNotifySetPropertyEvent = procedure(name : string; value : TValue) of object;


implementation

constructor TViewerPackage.Create;
begin
  inherited;
end;


end.
