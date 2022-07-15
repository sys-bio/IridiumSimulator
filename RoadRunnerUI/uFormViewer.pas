unit uFormViewer;

interface

Uses SysUtils, Classes, FMX.Forms, uViewerTypes;

type
  TFormViewer = class (TForm)
   private
        FViewerEnabled: Boolean;
        FOnViewerChange: TNotifySimulateEvent;
        FOnViewerClear : TNotifyEvent;
        FOnViewerSetProperty : TNotifySetPropertyEvent;
        FOnViewerModelHasChanged : TNotifyEvent;
    public
        property ViewerEnabled: Boolean read FViewerEnabled write FViewerEnabled;
        property OnViewerChange: TNotifySimulateEvent read FOnViewerChange write FOnViewerChange;
        property OnViewerClear: TNotifyEvent read FOnViewerClear write FOnViewerClear;
        property OnViewerSetProperty : TNotifySetPropertyEvent read FOnViewerSetProperty write FOnViewerSetProperty;
        property OnViewerModelHasChanged : TNotifyEvent read FOnViewerModelHasChanged write FOnViewerModelHasChanged;

       constructor Create(AOwner: TComponent); override;
       destructor  Destroy; override;
  end;

implementation

constructor TFormViewer.Create(AOwner: TComponent);
begin
  inherited;
  FViewerEnabled := True;
end;

destructor TFormViewer.Destroy;
begin
  inherited;
end;


end.
