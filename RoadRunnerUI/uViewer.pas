unit uViewer;

interface

Uses SysUtils, Classes, uViewerTypes;

type
  TViewer = class (TObject)
    private
        FViewerEnabled: Boolean;
        FOnViewerChange: TNotifySimulateEvent;
        FOnViewerClear : TNotifyEvent;
        FOnViewerSetProperty : TNotifySetPropertyEvent;
        FOnViewerModelHasChanged : TNotifyEvent;
    public
        property ViewerEnabled: Boolean read FViewerEnabled write FViewerEnabled;
        property OnViewerChange: TNotifySimulateEvent read FOnViewerChange write FOnViewerChange;
        property OnViewerClear : TNotifyEvent read FOnViewerClear write FOnViewerClear;
        property OnViewerSetProperty : TNotifySetPropertyEvent read FOnViewerSetProperty write FOnViewerSetProperty;
        property OnViewerModelHasChanged : TNotifyEvent read FOnViewerModelHasChanged write FOnViewerModelHasChanged;
  end;

implementation

end.
