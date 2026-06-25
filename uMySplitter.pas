unit uMySplitter;

// This is hack to allow us to access splitter mousedown events before the
// splitter has a chance to redraw the panels on either side. Fized the tmemo
// from snapping back to the top line when the splitte is moved.
interface

Uses
  System.Classes, System.UITypes, FMX.Controls, FMX.StdCtrls;

type
  TMySplitter = class(TSplitter)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
  end;

implementation

Uses ufMain;

procedure TMySplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if Owner is TfrmMain then
    TfrmMain(Owner).SplitterBeforeMove;
  inherited;
end;

procedure TMySplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if Owner is TfrmMain then
    TfrmMain(Owner).SplitterAfterMove;
end;


end.
