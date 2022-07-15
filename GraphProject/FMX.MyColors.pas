{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2014 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit FMX.MyColors;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.Rtti, System.UITypes, FMX.Objects, FMX.Types, FMX.StdCtrls,
  FMX.Edit, FMX.ListBox, FMX.Controls, FMX.Pickers, FMX.Graphics;

const
  ColorPickSize = 10;

type

{ THueTrackBar }

  TMyHueTrackBar = class(TBitmapTrackBar)
  private
    function IsValueStored: Boolean;
  protected
    procedure FillBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value stored IsValueStored;
  end;

{ TAlphaTrackBar }

  TMyAlphaTrackBar = class(TBitmapTrackBar)
  private
    function IsValueStored: Boolean;
  protected
    procedure FillBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value stored IsValueStored;
  end;

{ TBWTrackBar }

  TMyBWTrackBar = class(TBitmapTrackBar)
  private
    function IsValueStored: Boolean;
  protected
    procedure FillBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value stored IsValueStored;
  end;

{ TColorBox }

  TMyColorBox = class(TControl)
  private
    FColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Color: TAlphaColor read FColor write SetColor;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TColorQuad }

  TMyColorQuad = class(TControl)
  private
    FColorBox: TMyColorBox;
    FColorBitmap: TBitmap;
    FHue: Single;
    FSat: Single;
    FLum: Single;
    FOnChange: TNotifyEvent;
    FAlpha: Single;
    FPendingChanges: Boolean;
    procedure SetHue(const Value: Single);
    procedure SetLum(const Value: Single);
    procedure SetSat(const Value: Single);
    procedure SetAlpha(const Value: Single);
    procedure SetColorBox(const Value: TMyColorBox);
    procedure PreviewColor(const ValHue, ValLum, ValSat, ValAlpha: Single);
    procedure SetColor(const ValHue, ValLum, ValSat, ValAlpha: Single);
    function GetIsTracking: Boolean;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function GetAbsoluteRect: TRectF; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    function PointInObjectLocal(X, Y: Single): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Alpha: Single read FAlpha write SetAlpha;
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property ColorBox: TMyColorBox read FColorBox write SetColorBox;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Hue: Single read FHue write SetHue;
    property Lum: Single read FLum write SetLum;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Sat: Single read FSat write SetSat;
    property Scale;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TColorPicker }

  TMyColorPicker = class(TControl)
  private
    FHueBitmap: TBitmap;
    FHue: Single;
    FColorQuad: TMyColorQuad;
    procedure SetHue(const Value: Single);
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function GetAbsoluteRect: TRectF; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    function PointInObjectLocal(X, Y: Single): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Color: TAlphaColor read GetColor write SetColor;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property ColorQuad: TMyColorQuad read FColorQuad write FColorQuad;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Hue: Single read FHue write SetHue;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TGradientEdit }

  TMyGradientEdit = class(TControl)
  private
    FBitmap: TBitmap;
    FGradient: TGradient;
    FCurrentPoint: Integer;
    FCurrentPointInvisible: Boolean;
    FMoving: Boolean;
    FOnChange: TNotifyEvent;
    FOnSelectPoint: TNotifyEvent;
    FColorPicker: TMyColorPicker;
    procedure SetGradient(const Value: TGradient);
    function GetPointRect(const Point: Integer): TRectF;
    procedure DoChanged(Sender: TObject);
    procedure SetCurrentPoint(const Value: Integer);
    procedure SetColorPicker(const Value: TMyColorPicker);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateGradient;
    property Gradient: TGradient read FGradient write SetGradient;
    property CurrentPoint: Integer read FCurrentPoint write SetCurrentPoint;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property ColorPicker: TMyColorPicker read FColorPicker write SetColorPicker;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    {events}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnSelectPoint: TNotifyEvent read FOnSelectPoint write FOnSelectPoint;
  end;

{ TColorPanel }

  TMyColorPanel = class(TControl)
  private
    FOnChange: TNotifyEvent;
    FColorQuad: TMyColorQuad;
    FAlphaTrack: TMyAlphaTrackBar;
    FHueTrack: TMyHueTrackBar;
    FColorBox: TMyColorBox;
    FUseAlpha: Boolean;
    FOwnerObserves: Boolean;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetColorBox(const Value: TMyColorBox);
    procedure SetUseAlpha(const Value: Boolean);
  protected
    function GetObservers: TObservers; override;
    function CanObserve(const ID: Integer): Boolean; override;
    function GetData: TValue; override;
    procedure DoAlphaChange(Sender: TObject);
    procedure DoHueChange(Sender: TObject);
    procedure DoQuadChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OwnerObserves;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Color: TAlphaColor read GetColor write SetColor;
    property ColorBox: TMyColorBox read FColorBox write SetColorBox;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property UseAlpha: Boolean read FUseAlpha write SetUseAlpha default True;
    property Visible default True;
    property Width;
    {events}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TComboColorBox }

  TMyComboColorBox = class(TStyledControl)
  private
    FPopup: TPopup;
    FColorPanel: TMyColorPanel;
    FColorBox: TMyColorBox;
    FColorText: TEdit;
    FPlacement: TPlacement;
    FOnChange: TNotifyEvent;
    function GetValue: TAlphaColor;
    procedure SetValue(const Value: TAlphaColor);
    function GetUseAlpha: Boolean;
    procedure SetUseAlpha(const Value: Boolean);
    procedure CalcSize;
  protected
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure ApplyStyle; override;
    function CanObserve(const ID: Integer): Boolean; override;
    function GetDefaultStyleLookupName: string; override;
    procedure DoContentPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChangeParent; override;
    procedure DoColorChange(Sender: TObject); virtual;
    procedure DoTextChange(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DropDown;
  published
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Color: TAlphaColor read GetValue write SetValue;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property UseAlpha: Boolean read GetUseAlpha write SetUseAlpha default True;
    property Visible default True;
    property Width;

    {events}
    property OnApplyStyleLookup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TColorButton }

  TMyColorButton = class(TCustomButton)
  private
    FFill: TShape;
    FColor: TAlphaColor;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TAlphaColor);
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property AutoTranslate default False;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Color: TAlphaColor read FColor write SetColor;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property DisableFocusEffect;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest default True;
    {trigger}
    property StaysPressed default False;
    property IsPressed;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;

    {events}
    property OnApplyStyleLookup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TColorListBox }

  TMyColorListBox = class(TCustomListBox)
  private
    procedure SetColor(const Value: TAlphaColor);
    function GetColor: TAlphaColor;
    procedure DoItemApplyStyleLookup(Sender: TObject);
  protected
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure RebuildList;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color: TAlphaColor read GetColor write SetColor;
    property Align;
    property AllowDrag;
    property AlternatingRowBackground;
    property Anchors;
    property CanFocus;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DisableFocusEffect default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property ItemIndex;
    property ItemHeight;
    property ItemWidth;
    property DefaultItemStyles;
    property GroupingKind;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property Visible default True;
    property Width;

    {events}
    property OnApplyStyleLookup;
    property OnChange;
    {Drag and Drop events}
    property OnDragChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnItemClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

{ TColorComboBox }

  TMyColorComboBox = class(TComboBox)
  private
    procedure SetColor(const Value: TAlphaColor);
    function GetColor: TAlphaColor;
    procedure DoItemApplyStyleLookup(Sender: TObject);
  protected
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure RebuildList;
    function GetDefaultStyleLookupName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DropDownKind nodefault;
    property Color: TAlphaColor read GetColor write SetColor;
  end;

procedure MakeChessBoardBrush(ABrushBitmap: TBrushBitmap; const AChessStep: Single);

implementation

uses
  System.UIConsts, System.Variants, System.SysUtils, System.Math, FMX.Platform;

type
  TOpenEdit = class (TCustomEdit)
  end;

procedure MakeChessBoardBrush(ABrushBitmap: TBrushBitmap; const AChessStep: Single);
var
  BitmapTmp: TBitmap;
begin
  BitmapTmp := ABrushBitmap.Bitmap;
  BitmapTmp.SetSize(Trunc(2 * AChessStep), Trunc(2 * AChessStep));
  BitmapTmp.Clear(TAlphaColorRec.White);
  BitmapTmp.ClearRect(RectF(0, 0, AChessStep, AChessStep), TAlphaColorRec.Lightgray);
  BitmapTmp.ClearRect(RectF(AChessStep, AChessStep, 2 * AChessStep, 2 * AChessStep), TAlphaColorRec.Lightgray);
  ABrushBitmap.WrapMode := TWrapMode.Tile;
end;

{ THueTrackBar }

constructor TMyHueTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Max := 1;
  Value := 0.5;
end;

procedure TMyHueTrackBar.FillBitmap;
var
  I, J: Integer;
  M: TBitmapData;
begin
  if FBitmap.Map(TMapAccess.Write, M) then
  try
    for J := 0 to FBitmap.Height - 1 do
      for I := 0 to FBitmap.Width - 1 do
        if Orientation = TOrientation.Horizontal then
          M.SetPixel(I, J, (HSLtoRGB(I / FBitmap.Width, 0.9, 0.5)))
        else
          M.SetPixel(I, J, (HSLtoRGB(J / FBitmap.Height, 0.9, 0.5)));
  finally
    FBitmap.Unmap(M);
  end;
end;

function TMyHueTrackBar.IsValueStored: Boolean;
begin
  Result := Value <> 0.5;
end;

{ TAlphaTrackBar }

constructor TMyAlphaTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Max := 1;
  Value := 1;
end;

procedure TMyAlphaTrackBar.FillBitmap;
var
  I, J: Integer;
  M: TBitmapData;
begin
  if FBitmap.Map(TMapAccess.Write, M) then
  try
    for J := 0 to FBitmap.Height - 1 do
    begin
      for I := 0 to FBitmap.Width - 1 do
      begin
        if odd(I div 3) and not odd(J div 3) then
          M.SetPixel(I, J, $FFA0A0A0)
        else if not odd(I div 3) and odd(J div 3) then
          M.SetPixel(I, J, $FFA0A0A0)
        else
          M.SetPixel(I, J, $FFFFFFFF)
      end;
    end;
  finally
    FBitmap.Unmap(M);
  end;
  if FBitmap.Canvas.BeginScene then
  try
    FBitmap.Canvas.Fill.Kind := TBrushKind.Gradient;
    FBitmap.Canvas.Fill.Gradient.Points[0].Color := $00FFFFFF;
    FBitmap.Canvas.Fill.Gradient.Points[1].Color := $FFFFFFFF;
    if Orientation = TOrientation.Horizontal then
      FBitmap.Canvas.Fill.Gradient.StopPosition.Point := PointF(1, 0)
    else
      FBitmap.Canvas.Fill.Gradient.StopPosition.Point := PointF(0, 1);
    FBitmap.Canvas.FillRect(RectF(0, 0, FBitmap.Width, FBitmap.Height), 0, 0, [], 1);
  finally
    FBitmap.Canvas.EndScene;
  end;
end;

function TMyAlphaTrackBar.IsValueStored: Boolean;
begin
  Result := Value <> 1;
end;

{ TBWTrackBar }

constructor TMyBWTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Max := 1;
  Value := 0.5;
end;

procedure TMyBWTrackBar.FillBitmap;
var
  I, J: Integer;
  A: Byte;
  M: TBitmapData;
begin
  if FBitmap.Map(TMapAccess.Write, M) then
  try
    for J := 0 to FBitmap.Height - 1 do
    begin
      for I := 0 to FBitmap.Width - 1 do
      begin
        if Orientation = TOrientation.Horizontal then
          A := Round((I / FBitmap.Width) * $FF)
        else
          A := Round((J / FBitmap.Height) * $FF);
        M.SetPixel(I, J, MakeColor(A, A, A))
      end;
    end;
  finally
    FBitmap.Unmap(M);
  end;
end;

function TMyBWTrackBar.IsValueStored: Boolean;
begin
  Result := Value <> 0.5;
end;

{ TColorBox }

constructor TMyColorBox.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
end;

procedure TMyColorBox.Paint;
var
  State: TCanvasSaveState;
begin
  State := Canvas.SaveState;
  try
    MakeChessBoardBrush(Canvas.Fill.Bitmap, 5);
    Canvas.Fill.Kind := TBrushKind.Bitmap;
    Canvas.FillRect(LocalRect, 0, 0, AllCorners, AbsoluteOpacity);

    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := FColor;
    Canvas.FillRect(LocalRect, 0, 0, AllCorners, AbsoluteOpacity);
  finally
    Canvas.RestoreState(State);
  end;
end;

procedure TMyColorBox.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Repaint;
  end;
end;

{ TColorQuad }

constructor TMyColorQuad.Create(AOwner: TComponent);
begin
  inherited;
  FAlpha := 1;
  AutoCapture := True;
  SetAcceptsControls(False);
  FPendingChanges := false;
end;

destructor TMyColorQuad.Destroy;
begin
  if Assigned(FColorBitmap) then
    FreeAndNil(FColorBitmap);
  inherited;
end;

function TMyColorQuad.GetAbsoluteRect: TRectF;
begin
  Result := inherited GetAbsoluteRect;
  InflateRect(Result, ColorPickSize + 1, ColorPickSize + 1);
end;

function TMyColorQuad.GetIsTracking: Boolean;
begin
  Result := FPressed;
end;

function TMyColorQuad.PointInObjectLocal(X, Y: Single): Boolean;
begin
  Result := False;
  if (X > -ColorPickSize / 2) and (X < Width + ColorPickSize / 2) and
    (Y > -ColorPickSize / 2) and (Y < Height + ColorPickSize / 2) then
  begin
    Result := True;
  end;
end;

procedure TMyColorQuad.PreviewColor(const ValHue, ValLum, ValSat, ValAlpha: Single);
var
  LChanged : Boolean;
begin
  LChanged := false;

  if FHue <> ValHue then
  begin
    FHue := ValHue;
    if FHue < 0 then
      FHue := 0;
    if FHue > 1 then
      FHue := 1;

    LChanged := true;
  end;

  if FLum <> ValLum then
  begin
    FLum := ValLum;
    if FLum < 0 then
      FLum := 0;
    if FLum > 1 then
      FLum := 1;

    LChanged := true;
  end;

  if FSat <> ValSat then
  begin
    FSat := ValSat;
    if FSat < 0 then
      FSat := 0;
    if FSat > 1 then
      FSat := 1;

    LChanged := true;
  end;

  if FAlpha <> ValAlpha then
  begin
    FAlpha := ValAlpha;
    if FAlpha < 0 then
      FAlpha := 0;
    if FAlpha > 1 then
      FAlpha := 1;

    LChanged := true;
  end;

  if LChanged then
  begin
    if Assigned(FColorBitmap) then
      FreeAndNil(FColorBitmap);
    if Assigned(FColorBox) then
      FColorBox.Color := HSLtoRGB(FHue, FSat, FLum) and $FFFFFF or (Round(FAlpha * $FF) shl 24);
    Repaint;
  end;
end;

procedure TMyColorQuad.MouseMove(Shift: TShiftState; X, Y: Single);
var
  LLum, LSat: Single;
begin
  inherited;
  if FPressed then
  begin
    LLum := Lum;
    LSat := Sat;
    if Height <> 0 then
      LLum := 1 - ((Y) / (Height));
    if Width <> 0 then
      LSat := ((X) / (Width));

    if GetIsTracking then
      SetColor(Hue, LLum, LSat, Alpha)
    else
      // will not fire OnChange event. MouseUp though will change the value when gets fired
      PreviewColor(Hue, LLum, LSat, Alpha);
  end;
end;

procedure TMyColorQuad.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LLum, LSat: Single;
begin
  inherited;

  LLum := Lum;
  LSat := Sat;
  if Height <> 0 then
    LLum := 1 - ((Y) / (Height));
  if Width <> 0 then
    LSat := ((X) / (Width));

  SetColor(Hue, LLum, LSat, Alpha);
end;

procedure TMyColorQuad.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorBox) then
    ColorBox := nil;
end;

procedure TMyColorQuad.Paint;
var
  I, J: Integer;
  R: TRectF;
  H, S, L: Single;
  M: TBitmapData;
begin
  H := Hue;
  S := Sat;
  L := Lum;

  if not Assigned(FColorBitmap) then
  begin
    FColorBitmap := TBitmap.Create(Trunc(Width), Trunc(Height));
    if Assigned(FColorBitmap) then
    begin
      if FColorBitmap.Map(TMapAccess.Write, M) then
      try
        for I := 0 to FColorBitmap.Width - 1 do
          for J := 0 to FColorBitmap.Height - 1 do
            M.SetPixel(I, J, HSLtoRGB(H, I / FColorBitmap.Width, (1 - (J / FColorBitmap.Height))));
      finally
        FColorBitmap.Unmap(M);
      end;
    end;
  end;
  if Assigned(FColorBitmap) then
    Canvas.DrawBitmap(FColorBitmap, RectF(0, 0, FColorBitmap.Width, FColorBitmap.Height), RectF(0, 0, Width, Height),
      AbsoluteOpacity);
  { current }

  R := RectF(S * (Width), (1 - L) * (Height), S * (Width), (1 - L) * (Height));

  InflateRect(R, ColorPickSize / 2, ColorPickSize / 2);
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Color := $FF000000;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  InflateRect(R, -1, -1);
  Canvas.Stroke.Color := $FFFFFFFF;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  InflateRect(R, -1, -1);
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := HSLtoRGB(H, S, L);
  Canvas.FillEllipse(R, AbsoluteOpacity);
end;

procedure TMyColorQuad.SetAlpha(const Value: Single);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    if FAlpha < 0 then
      FAlpha := 0;
    if FAlpha > 1 then
      FAlpha := 1;
    if Assigned(FColorBox) then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if (not FPressed) and Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TMyColorQuad.SetHue(const Value: Single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    if FHue < 0 then
      FHue := 0;
    if FHue > 1 then
      FHue := 1;
    if Assigned(FColorBitmap) then
      FreeAndNil(FColorBitmap);
    if Assigned(FColorBox) then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if (not FPressed) and Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TMyColorQuad.SetLum(const Value: Single);
begin
  if FLum <> Value then
  begin
    FLum := Value;
    if FLum < 0 then
      FLum := 0;
    if FLum > 1 then
      FLum := 1;
    if Assigned(FColorBox) then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if (not FPressed) and Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TMyColorQuad.SetSat(const Value: Single);
begin
  if FSat <> Value then
  begin
    FSat := Value;
    if FSat < 0 then
      FSat := 0;
    if FSat > 1 then
      FSat := 1;
    if Assigned(FColorBox) then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if (not FPressed) and Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TMyColorQuad.SetColor(const ValHue, ValLum, ValSat, ValAlpha: Single);
begin
  if FPendingChanges then
    Exit;
  FPendingChanges := true;

  FHue := ValHue;
  if FHue < 0 then
    FHue := 0;
  if FHue > 1 then
    FHue := 1;

  FLum := ValLum;
  if FLum < 0 then
    FLum := 0;
  if FLum > 1 then
    FLum := 1;

  FSat := ValSat;
  if FSat < 0 then
    FSat := 0;
  if FSat > 1 then
    FSat := 1;

  FAlpha := ValAlpha;
  if FAlpha < 0 then
    FAlpha := 0;
  if FAlpha > 1 then
    FAlpha := 1;

  if Assigned(FColorBitmap) then
    FreeAndNil(FColorBitmap);
  if Assigned(FColorBox) then
    FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;

  FPendingChanges := false;
end;

procedure TMyColorQuad.SetColorBox(const Value: TMyColorBox);
begin
  if FColorBox <> Value then
  begin
    FColorBox := Value;
    if Assigned(FColorBox) and (not FPressed) then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
  end;
end;

{ TColorPicker }

constructor TMyColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := True;
  SetAcceptsControls(False);
end;

destructor TMyColorPicker.Destroy;
begin
  if Assigned(FHueBitmap) then
    FreeAndNil(FHueBitmap);
  inherited;
end;

procedure TMyColorPicker.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorQuad) then
    ColorQuad := nil;
end;

function TMyColorPicker.GetAbsoluteRect: TRectF;
begin
  Result := inherited GetAbsoluteRect;
  InflateRect(Result, 0, ColorPickSize / 2);
end;

function TMyColorPicker.PointInObjectLocal(X, Y: Single): Boolean;
begin
  Result := False;
  if (X > 0) and (X < Width) and (Y > -ColorPickSize / 2) and (Y < Height + ColorPickSize / 2) then
    Result := True;
end;

procedure TMyColorPicker.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FPressed then
    if Height <> 0 then
      Hue := ((Y) / (Height));
end;

procedure TMyColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FPressed then
    MouseMove([ssLeft], X, Y);
  inherited;
end;

procedure TMyColorPicker.Paint;
var
  I, J: Integer;
  R: TRectF;
  M: TBitmapData;
begin
  if not Assigned(FHueBitmap) then
  begin
    FHueBitmap := TBitmap.Create(Trunc(Width), Trunc(Height));
    if Assigned(FHueBitmap) then
    begin
      if FHueBitmap.Map(TMapAccess.Write, M) then
      try
        for J := 0 to FHueBitmap.Height - 1 do
          for I := 0 to FHueBitmap.Width - 1 do
            M.SetPixel(I, J, HSLtoRGB(J / FHueBitmap.Height, 0.9, 0.5));
      finally
        FHueBitmap.Unmap(M);
      end;
    end;
  end;

  if Assigned(FHueBitmap) then
    Canvas.DrawBitmap(FHueBitmap, RectF(0, 0, FHueBitmap.Width, FHueBitmap.Height), RectF(0, 0, Width, Height),
      AbsoluteOpacity);

  { hue pos }
  R := RectF(Width / 2, FHue * (Height), Width / 2, FHue * (Height));
  InflateRect(R, ColorPickSize / 2, ColorPickSize / 2);
  // OffsetRect(R, 01, StrokeThickness);
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Color := $FF000000;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  InflateRect(R, -1, -1);
  Canvas.Stroke.Color := $FFFFFFFF;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  InflateRect(R, -1, -1);
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Fill.Color := HSLtoRGB(Hue, 0.9, 0.5);
  Canvas.FillEllipse(R, AbsoluteOpacity);
end;

function TMyColorPicker.GetColor: TAlphaColor;
begin
  Result := HSLtoRGB(Hue, 1, 0.5)
end;

procedure TMyColorPicker.SetColor(const Value: TAlphaColor);
var
  H, S, L: Single;
begin
  RGBtoHSL(Value, H, S, L);
  Hue := H;
  if Assigned(FColorQuad) then
  begin
    FColorQuad.Alpha := TAlphaColorRec(Value).a / $FF;
    FColorQuad.Hue := H;
    FColorQuad.Sat := S;
    FColorQuad.Lum := L;
  end;
end;

procedure TMyColorPicker.SetHue(const Value: Single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    if FHue < 0 then
      FHue := 0;
    if FHue > 1 then
      FHue := 1;
    if Assigned(FColorQuad) then
      FColorQuad.Hue := FHue;
    Repaint;
  end;
end;

{ TGradientEdit }

constructor TMyGradientEdit.Create(AOwner: TComponent);
begin
  inherited;
  FGradient := TGradient.Create;
  FGradient.OnChanged := DoChanged;
  Width := 200;
  Height := 20;
  AutoCapture := True;
  SetAcceptsControls(False);
end;

destructor TMyGradientEdit.Destroy;
begin
  if Assigned(FBitmap) then
    FreeAndNil(FBitmap);
  if Assigned(FGradient) then
    FreeAndNil(FGradient);
  inherited;
end;

function TMyGradientEdit.GetPointRect(const Point: Integer): TRectF;
begin
  if (Point >= 0) and (Point < FGradient.Points.Count) then
  begin
    Result := RectF(0 + ColorPickSize + (FGradient.Points[Point].Offset * (Width - ((0 + ColorPickSize) * 2))),
      Height - 0 - ColorPickSize, 0 + ColorPickSize + (FGradient.Points[Point].Offset * (Width - ((0 + ColorPickSize) *
      2))), Height - 0);
    InflateRect(Result, ColorPickSize / 2, 0);
  end
  else
    Result := RectF(0, 0, 0, 0);
end;

procedure TMyGradientEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  NewOffset: Single;
  NewColor: TAlphaColor;
  I: Integer;
  NewGradientPoint: TGradientPoint;
begin
  inherited;
  FMoving := False;
  if Button = TMouseButton.mbLeft then
  begin
    { select point }
    for I := 0 to FGradient.Points.Count - 1 do
      if GetPointRect(I).Contains(PointF(X, Y)) then
      begin
        CurrentPoint := I;
        if Assigned(OnSelectPoint) then
          OnSelectPoint(Self);
        FMoving := True;
        Repaint;
        Exit;
      end;
    { add new point }
    if (Y > 0) and (Y < Height - 0 - ColorPickSize) then
    begin
      NewOffset := ((X - 0 - ColorPickSize) /
        (Width - ((0 + ColorPickSize) * 2)));
      if NewOffset < 0 then
        NewOffset := 0;
      if NewOffset > 1 then
        NewOffset := 1;
      NewColor := FGradient.InterpolateColor(NewOffset);
      for I := 1 to FGradient.Points.Count - 1 do
        if NewOffset < FGradient.Points[I].Offset then
        begin
          NewGradientPoint := TGradientPoint(FGradient.Points.Add);
          NewGradientPoint.Index := I;
          CurrentPoint := I;
          NewGradientPoint.IntColor := NewColor;
          NewGradientPoint.Offset := NewOffset;
          Repaint;
          DoChanged(Self);
          Break;
        end;
    end;
  end;
end;

procedure TMyGradientEdit.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    if FMoving then
    begin
      FCurrentPointInvisible := ((Y < -10) or (Y > Height + 10)) and (FGradient.Points.Count > 1) and
        (CurrentPoint <> 0) and (CurrentPoint <> FGradient.Points.Count - 1);
      { move }
      FGradient.Points[CurrentPoint].Offset := ((X - 0 - ColorPickSize) / (Width - ((0 + ColorPickSize) * 2)));
      if FGradient.Points[CurrentPoint].Offset < 0 then
        FGradient.Points[CurrentPoint].Offset := 0;
      if FGradient.Points[CurrentPoint].Offset > 1 then
        FGradient.Points[CurrentPoint].Offset := 1;
      { move right }
      if CurrentPoint < FGradient.Points.Count - 1 then
        if FGradient.Points[CurrentPoint].Offset > FGradient.Points[CurrentPoint + 1].Offset then
        begin
          FGradient.Points[CurrentPoint].Index := FGradient.Points[CurrentPoint].Index + 1;
          CurrentPoint := CurrentPoint + 1;
        end;
      { move left }
      if CurrentPoint > 0 then
        if FGradient.Points[CurrentPoint].Offset < FGradient.Points[CurrentPoint - 1].Offset then
        begin
          FGradient.Points[CurrentPoint].Index := FGradient.Points[CurrentPoint].Index - 1;
          CurrentPoint := CurrentPoint - 1;
        end;
      Repaint;
      DoChanged(Self);
    end;
  end;
end;

procedure TMyGradientEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FCurrentPointInvisible := False;
  if FMoving then
  begin
    { delete }
    if (Y > Height + 10) and (FGradient.Points.Count > 2) then
    begin
      FGradient.Points.Delete(CurrentPoint);
      CurrentPoint := CurrentPoint - 1;
      if CurrentPoint < 0 then
        CurrentPoint := 0;
      Repaint;
      DoChanged(Self);
      FMoving := False;
      Exit;
    end;
  end;
  FMoving := False;
end;

procedure TMyGradientEdit.Paint;

  procedure DrawBackground;
  var
    GradientRect: TRectF;
  begin
    GradientRect := RectF(ColorPickSize, 0, Width - ColorPickSize, Height - ColorPickSize);
    MakeChessBoardBrush(Canvas.Fill.Bitmap, 10);
    Canvas.Fill.Kind := TBrushKind.Bitmap;
    Canvas.FillRect(GradientRect, 0, 0, AllCorners, AbsoluteOpacity);
  end;

  procedure DrawGradient;
  var
    GradientRect: TRectF;
  begin
    GradientRect := RectF(ColorPickSize, 0, Width - ColorPickSize, Height - ColorPickSize);

    Canvas.Stroke.Kind := TBrushKind.None;
    Canvas.Fill.Kind := TBrushKind.Gradient;
    Canvas.Fill.Gradient.Assign(FGradient);
    Canvas.Fill.Gradient.Style := TGradientStyle.Linear;
    Canvas.Fill.Gradient.StartPosition.SetPointNoChange(TPointF.Create(0, 0));
    Canvas.Fill.Gradient.StopPosition.SetPointNoChange(TPointF.Create(1, 0));

    Canvas.FillRect(GradientRect, 0, 0, AllCorners, AbsoluteOpacity);
  end;

  procedure DrawPoints;
  var
    I: Integer;
    PointRect: TRectF;
  begin
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.StrokeThickness := 1;
    for I := 0 to FGradient.Points.Count - 1 do
    begin
      if FCurrentPointInvisible and (I = CurrentPoint) then
        Continue;
      PointRect := GetPointRect(I);
      InflateRect(PointRect, -1, -1);
      Canvas.Stroke.Color := $FF757575;
      Canvas.Fill.Color := FGradient.Points[I].IntColor;
      Canvas.FillEllipse(PointRect, AbsoluteOpacity);
      Canvas.DrawEllipse(PointRect, AbsoluteOpacity);
      if CurrentPoint = I then
      begin
        InflateRect(PointRect, 1, 1);
        Canvas.Stroke.Color := TAlphaColorRec.White;
        Canvas.DrawEllipse(PointRect, AbsoluteOpacity);
      end;
    end;
  end;

begin
  DrawBackground;
  DrawGradient;
  DrawPoints;
end;

procedure TMyGradientEdit.SetGradient(const Value: TGradient);
begin
  FGradient.Assign(Value);
end;

procedure TMyGradientEdit.DoChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  UpdateGradient;
end;

procedure TMyGradientEdit.SetCurrentPoint(const Value: Integer);
begin
  if FCurrentPoint <> Value then
  begin
    FCurrentPoint := Value;
    if Assigned(OnSelectPoint) then
      OnSelectPoint(Self);
    if Assigned(FColorPicker) and (CurrentPoint >= 0) then
      FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
  end;
end;

procedure TMyGradientEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorPicker) then
    ColorPicker := nil;
end;

procedure TMyGradientEdit.SetColorPicker(const Value: TMyColorPicker);
begin
  FColorPicker := Value;
  if Assigned(FColorPicker) and (CurrentPoint >= 0) then
    FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
end;

procedure TMyGradientEdit.UpdateGradient;
begin
  if Assigned(FColorPicker) and (CurrentPoint >= 0) then
    FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
end;

{ TColorPanel }

constructor TMyColorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FUseAlpha := True;
  Width := 150;
  Height := 150;
  FAlphaTrack := TMyAlphaTrackBar.Create(Self);
  FAlphaTrack.Parent := Self;
  FAlphaTrack.Align := TAlignLayout.Bottom;
  FAlphaTrack.Stored := False;
  FAlphaTrack.Name := 'AlphaTrack';
  FAlphaTrack.Locked := True;
  FAlphaTrack.Margins.Rect := RectF(0, 0, 15, 0);
  FAlphaTrack.Height := 25;
  FAlphaTrack.DisableFocusEffect := True;
  FAlphaTrack.OnChange := DoAlphaChange;

  FHueTrack := TMyHueTrackBar.Create(Self);
  FHueTrack.Parent := Self;
  FHueTrack.Align := TAlignLayout.Right;
  FHueTrack.Stored := False;
  FHueTrack.Locked := True;
  FHueTrack.Name := 'HueTrack';
  FHueTrack.Margins.Rect := RectF(0, 0, 0, 0);
  FHueTrack.Orientation := TOrientation.Vertical;
  FHueTrack.Width := 15;
  FHueTrack.DisableFocusEffect := True;
  FHueTrack.OnChange := DoHueChange;

  FColorQuad := TMyColorQuad.Create(Self);
  FColorQuad.Parent := Self;
  FColorQuad.Align := TAlignLayout.Client;
  FColorQuad.Name := 'ColorQuad';
  FColorQuad.Stored := False;
  FColorQuad.Locked := True;
  FColorQuad.Margins.Rect := RectF(5, 5, 3, 3);
  FColorQuad.OnChange := DoQuadChange;

  Color := TAlphaColors.White;
  SetAcceptsControls(False);
end;

destructor TMyColorPanel.Destroy;
begin
  inherited;
end;

function TMyColorPanel.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditLinkID then
    Result := True;
  if ID = TObserverMapping.ControlValueID then
    Result := True;
end;

procedure TMyColorPanel.DoAlphaChange(Sender: TObject);
begin
  FColorQuad.Alpha := FAlphaTrack.Value;
end;

procedure TMyColorPanel.DoHueChange(Sender: TObject);
begin
  FColorQuad.Hue := FHueTrack.Value;
end;

procedure TMyColorPanel.DoQuadChange(Sender: TObject);
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    if TLinkObservers.EditLinkIsReadOnly(Observers) then
      Exit;
    if Assigned(FColorBox) then
      FColorBox.Color := Color;
    if Assigned(OnChange) then
      OnChange(Self);
    if TLinkObservers.EditLinkEdit(Observers) then
      TLinkObservers.EditLinkModified(Observers)
    else
    begin
      TLinkObservers.EditLinkReset(Observers);
      Exit;
    end;
    TLinkObservers.EditLinkUpdate(Observers);
  end
  else
  begin
    if Assigned(FColorBox) then
      FColorBox.Color := Color;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
  begin
    TLinkObservers.ControlValueModified(Observers);
    TLinkObservers.ControlValueUpdate(Observers);
  end;
end;

function TMyColorPanel.GetColor: TAlphaColor;
begin
  Result := MakeColor(HSLtoRGB(FColorQuad.Hue, FColorQuad.Sat, FColorQuad.Lum), FColorQuad.Alpha);
end;

function TMyColorPanel.GetData: TValue;
begin
  Result := TValue.From<TAlphaColor>(Color);
end;

function TMyColorPanel.GetObservers: TObservers;
begin
  if Assigned(Owner) and FOwnerObserves then
    Result := Owner.Observers
  else
    Result := inherited;
end;

procedure TMyColorPanel.Loaded;
begin
  inherited;
  Color := Color;
end;

procedure TMyColorPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorBox) then
    ColorBox := nil;
end;

procedure TMyColorPanel.OwnerObserves;
begin
  FOwnerObserves := True;
end;

procedure TMyColorPanel.SetColor(const Value: TAlphaColor);
var
  SavedOnQuadChange : TNotifyEvent;
  SavedOnHueChange : TNotifyEvent;
  SavedOnAlphaChange : TNotifyEvent;
  SaveOnChange: TNotifyEvent;

  procedure StoreChangeEvents;
  begin
    SaveOnChange := FOnChange;
    SavedOnQuadChange := FColorQuad.OnChange;
    SavedOnHueChange := FHueTrack.OnChange;
    SavedOnAlphaChange := FAlphaTrack.OnChange;
  end;

  procedure ResetChangeEvents;
  begin
    FOnChange := nil;
    FColorQuad.OnChange := nil;
    FHueTrack.OnChange := nil;
    FAlphaTrack.OnChange := nil;
  end;

  procedure RestoreChangeEvents;
  begin
    FOnChange := SaveOnChange;
    FColorQuad.OnChange := SavedOnQuadChange;
    FHueTrack.OnChange := SavedOnHueChange;
    FAlphaTrack.OnChange := SavedOnAlphaChange;
  end;

  procedure FillControls(AColor: TAlphaColor);
  var
    H, S, L: Single;
  begin
    RGBtoHSL(AColor, H, S, L);
    FColorQuad.Lum := L;
    FColorQuad.Sat := S;
    FColorQuad.Hue := H;
    FColorQuad.Alpha := TAlphaColorRec(AColor).A / $FF;
    FHueTrack.Value := H;
    FAlphaTrack.Value := TAlphaColorRec(AColor).A / $FF;
  end;

begin
  if Value <> Color then
  begin
    StoreChangeEvents;
    try
      ResetChangeEvents;
      FillControls(Value);

      if not (csLoading in ComponentState) then
        DoQuadChange(Self);
    finally
      RestoreChangeEvents;
    end;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TMyColorPanel.SetColorBox(const Value: TMyColorBox);
begin
  if FColorBox <> Value then
  begin
    FColorBox := Value;
    if Assigned(FColorBox) then
      FColorBox.Color := Color;
  end;
end;

procedure TMyColorPanel.SetUseAlpha(const Value: Boolean);
begin
  if FUseAlpha <> Value then
  begin
    FUseAlpha := Value;
    FAlphaTrack.Visible := FUseAlpha;
  end;
end;

{ TComboColorBox }

constructor TMyComboColorBox.Create(AOwner: TComponent);
begin
  inherited;
  Width := 60;
  Height := 22;
  CanFocus := True;
  AutoCapture := True;
  FPopup := TPopup.Create(Self);
  FPopup.StyleLookup := 'combopopupstyle';
  FPopup.PlacementTarget := Self;
  FPopup.Stored := False;
  FPopup.Parent := Self;
  FPopup.Locked := True;
  FPopup.DragWithParent := True;
  FPopup.DesignVisible:= False;
  FPopup.Width := 240;
  FPopup.Height := 160;
  FPopup.Padding.Rect := RectF(5, 5, 5, 5);
  FColorBox := TMyColorBox.Create(nil);
  FColorBox.Width := 50;
  FColorBox.Parent := FPopup;
  FColorBox.Stored := False;
  FColorBox.Align := TAlignLayout.HorzCenter;
  FColorBox.Margins.Rect := RectF(15, 60, 15, 40);
  FColorText := TEdit.Create(Self);
  FColorText.Parent := FPopup;
  FColorText.Stored := False;
  FColorText.Locked := True;
  FColorText.FilterChar := '#0123456789abcdefABCDEF';
  FColorText.BoundsRect := RectF(164, 20, 164 + 70, 20 + 22);
  FColorText.DisableFocusEffect := True;
  FColorText.OnChange := DoTextChange;
  FColorPanel := TMyColorPanel.Create(Self);
  FColorPanel.Parent := FPopup;
  FColorPanel.Stored := False;
  FColorPanel.DisableFocusEffect := True;
  FColorPanel.Align := TAlignLayout.Left;
  FColorPanel.Width := 156;
  FColorPanel.OnChange := DoColorChange;
  FColorPanel.OwnerObserves;
  FColorPanel.ColorBox := FColorBox;
  SetAcceptsControls(False);
end;

destructor TMyComboColorBox.Destroy;
begin
  if Assigned(FColorPanel) then
    FreeAndNil(FColorPanel);
  if Assigned(FColorText) then
    FreeAndNil(FColorText);
  if Assigned(FColorBox) then
    FreeAndNil(FColorBox);
  if Assigned(FPopup) then
    FreeAndNil(FPopup);
  inherited;
end;

function TMyComboColorBox.GetData: TValue;
begin
  Result := TValue.From<TAlphaColor>(Color);
end;

procedure TMyComboColorBox.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then
    OnChange := Value.AsType<TNotifyEvent>()
  else if Value.IsType<TAlphaColor> then
    Color := Value.AsType<TAlphaColor>
  else
    Color := StringToAlphaColor(Value.ToString);
end;

procedure TMyComboColorBox.DoTextChange(Sender: TObject);
var
  C: TAlphaColor;
begin
  C := TAlphaColorRec.White;
  try
    C := Color;
    Color := StringToAlphaColor(FColorText.Text);
  except
    Color := C;
  end;
end;

procedure TMyComboColorBox.CalcSize;
var
  W, DW, LWidth: Single;
begin
  TCanvasManager.MeasureCanvas.Font.Assign(FColorText.ResultingTextSettings.Font);
  if TOpenEdit(FColorText).Content <> nil then
    DW := FColorText.Width - TOpenEdit(FColorText).Content.Width
  else
    DW := 2;
  W := Max(FColorText.Width, Ceil(TCanvasManager.MeasureCanvas.TextWidth('#DDDDDDDD') + DW));
  FColorText.Width := W;
  LWidth := Max(FPopup.Width, FColorText.Position.X + W + 8);
  if LWidth > Width then
    FPopup.PopupFormSize := TSizeF.Create(LWidth, 0);
end;

procedure TMyComboColorBox.DoColorChange(Sender: TObject);
begin
  FColorText.Text := AlphaColorToString(Color);
  Repaint;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMyComboColorBox.DropDown;
begin
  if not FPopup.IsOpen then
  begin
    FPopup.Placement := FPlacement;
    FPopup.ApplyStyleLookup;
    FColorText.ApplyStyleLookup;
    CalcSize;
    FPopup.IsOpen := True;
  end
  else
    FPopup.IsOpen := False;
end;

procedure TMyComboColorBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) then
    DropDown;
end;

function TMyComboColorBox.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditLinkID then
    Result := True;
  if ID = TObserverMapping.ControlValueID then
    Result := True;
end;

procedure TMyComboColorBox.ChangeParent;
begin
  inherited;
end;

function TMyComboColorBox.GetValue: TAlphaColor;
begin
  Result := FColorPanel.Color
end;

procedure TMyComboColorBox.SetValue(const Value: TAlphaColor);
begin
  FColorPanel.Color := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMyComboColorBox.ApplyStyle;
var
  T: TFmxObject;
begin
  inherited;
  T := FindStyleResource('Content');
  if Assigned(T) and (T is TControl) then
    TControl(T).OnPaint := DoContentPaint;
end;

procedure TMyComboColorBox.DoContentPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  R: TRectF;
  State: TCanvasSaveState;
  I, J: Integer;
begin
  R := ARect;
  R.Inflate(-0.5 - 2, -0.5 - 2);
  R.Offset(0.5, 0.5);
  { draw back }
  State := Canvas.SaveState;
  try
    Canvas.IntersectClipRect(R);
    Canvas.Stroke.Kind := TBrushKind.None;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := $FFFFFFFF;
    Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.Fill.Color := $FFD3D3D3;
    for I := 0 to Trunc(RectWidth(R) / 5) + 1 do
      for J := 0 to Trunc(RectHeight(R) / 5) + 1 do
        if Odd(I + J) then
          Canvas.FillRect(RectF(I * 5, J * 5, (I + 1) * 5, (J + 1) * 5), 0, 0, AllCorners, AbsoluteOpacity);
    { color }
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := Color;
    Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.Stroke.Color := TAlphaColors.Black;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  finally
    Canvas.RestoreState(State);
  end;
end;

function TMyComboColorBox.GetUseAlpha: Boolean;
begin
  Result := FColorPanel.UseAlpha;
end;

procedure TMyComboColorBox.SetUseAlpha(const Value: Boolean);
begin
  FColorPanel.UseAlpha := Value;
end;

function TMyComboColorBox.GetDefaultStyleLookupName: string;
begin
  Result := 'comboboxstyle';
end;

{ TColorButton }

constructor TMyColorButton.Create(AOwner: TComponent);
begin
  inherited;
  AutoTranslate := False;
  FColor := $FF000000;
end;

destructor TMyColorButton.Destroy;
begin
  inherited;
end;

procedure TMyColorButton.ApplyStyle;
var
  O: TFmxObject;
begin
  inherited;
  O := FindStyleResource('fill');
  if Assigned(O) and (O is TShape) then
  begin
    FFill := TShape(O);
    FFill.Fill.Color := FColor;
  end;
end;

procedure TMyColorButton.FreeStyle;
begin
  inherited;
  FFill := nil;
end;

procedure TMyColorButton.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  if Assigned(FFill) then
    FFill.Fill.Color := FColor;
  if not(csLoading in ComponentState) then
    if Assigned(FOnChange) then
      FOnChange(Self);
end;

function SwapColor(const C: TAlphaColor): TAlphaColor;
begin
  Result := C;
  TAlphaColorRec(Result).R := TAlphaColorRec(C).B;
  TAlphaColorRec(Result).B := TAlphaColorRec(C).R;
end;

type
  THackListBox = class(TListBox);

type
  TAlphaColorMapEntry = record
    Value: TAlphaColor;
    Name: string;
  end;

  TRTLColors = class
  strict private
    FColors: array of TAlphaColorMapEntry;
    procedure GetAlphaColorValuesProc(const AColorName: string);
    function GetColor(AIndex : Integer) : TAlphaColorMapEntry;
  public
    constructor Create;
    function Count : Integer;
    property Color[Index : Integer] : TAlphaColorMapEntry read GetColor;
  end;

function TRTLColors.GetColor(AIndex: Integer): TAlphaColorMapEntry;
begin
  Result := FColors[AIndex];
end;

function TRTLColors.Count: Integer;
begin
  Result := Length(FColors);
end;

constructor TRTLColors.Create;
begin
  GetAlphaColorValues(GetAlphaColorValuesProc);
end;

procedure TRTLColors.GetAlphaColorValuesProc(const AColorName: string);
var
  LNewIndex : Integer;
begin
  LNewIndex := Count;
  SetLength(FColors, LNewIndex + 1);
  FColors[LNewIndex].Name := AColorName;
  FColors[LNewIndex].Value := StringToAlphaColor(AColorName);
end;

var
  ColorsMap : TRTLColors;

{ TColorListBox }

constructor TMyColorListBox.Create(AOwner: TComponent);
begin
  inherited;
  RebuildList;
  SetAcceptsControls(False);
end;

destructor TMyColorListBox.Destroy;
begin
  inherited;
end;

procedure TMyColorListBox.RebuildList;
var
  I, SaveIndex: Integer;
  Item: TListBoxItem;
begin
  if FUpdating > 0 then
    Exit;
  if csDestroying in ComponentState then
    Exit;

  BeginUpdate;
  SaveIndex := ItemIndex;
  FItemIndex := -1;
  Clear;
  for I := 0 to ColorsMap.Count - 1 do
  begin
    Item := TListBoxItem.Create(nil);
    Item.Parent := Self;
    Item.Width := Item.DefaultSize.Width;
    Item.Height := Item.DefaultSize.Height;
    Item.Stored := False;
    Item.Locked := True;
    Item.Text := ColorsMap.Color[I].Name;
    Item.Tag := I;
    Item.StyleLookup := 'colorlistboxitemstyle';
    Item.OnApplyStyleLookup := DoItemApplyStyleLookup;
  end;
  EndUpdate;
  FItemIndex := SaveIndex;
end;

procedure TMyColorListBox.SetColor(const Value: TAlphaColor);
var
  I: Integer;
begin
  for I := 0 to ColorsMap.Count - 1 do
    if ColorsMap.Color[I].Value = Value then
    begin
      ItemIndex := I;
      Break;
    end;
end;

procedure TMyColorListBox.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then
    OnChange := Value.AsType<TNotifyEvent>()
  else if Value.IsType<TAlphaColor> then
    Color := Value.AsType<TAlphaColor>
  else
    Color := StringToAlphaColor(Value.ToString);
end;

procedure TMyColorListBox.DoItemApplyStyleLookup(Sender: TObject);
var
  ColorObj: TFmxObject;
begin
  ColorObj := TListBoxItem(Sender).FindStyleResource('color');
  if ColorObj is TShape then
    TShape(ColorObj).Fill.Color := ColorsMap.Color[TListBoxItem(Sender).Tag].Value;
end;

function TMyColorListBox.GetColor: TAlphaColor;
begin
  if (0 <= ItemIndex) and (ItemIndex < Count) then
    Result := ColorsMap.Color[ItemIndex].Value
  else
    Result := TAlphaColorRec.Null;
end;

function TMyColorListBox.GetData: TValue;
begin
  Result := TValue.From<TAlphaColor>(Color);
end;

function TMyColorListBox.GetDefaultStyleLookupName: string;
begin
  Result := 'listboxstyle';
end;

{ TColorComboBox }

constructor TMyColorComboBox.Create(AOwner: TComponent);
var
  DefaultValueService: IFMXDefaultPropertyValueService;
  LDropDownKind: TValue;
begin
  inherited;

  LDropDownKind := TValue.Empty;
  if SupportsPlatformService(IFMXDefaultPropertyValueService, IInterface(DefaultValueService)) then
    LDropDownKind := DefaultValueService.GetDefaultPropertyValue(Self.ClassName, 'dropdownkind');

  if not LDropDownKind.IsEmpty then
    DropDownKind := LDropDownKind.AsType<TDropDownKind>
  else
    DropDownKind := TDropDownKind.Custom;

  RebuildList;
  SetAcceptsControls(False);
end;

procedure TMyColorComboBox.RebuildList;
var
  I, SaveIndex: Integer;
  Item: TListBoxItem;
begin
  if FUpdating > 0 then Exit;
  if csDestroying in ComponentState then Exit;

  BeginUpdate;
  SaveIndex := FListbox.ItemIndex;
  THackListBox(FListbox).FItemIndex := -1;
  Clear;
  for I := 0 to ColorsMap.Count - 1 do
  begin
    Item := TListBoxItem.Create(nil);
    Item.Parent := Self;
    Item.Stored := False;
    Item.Locked := True;
    Item.Text := ColorsMap.Color[I].Name;
    Item.Tag := I;
    Item.StyleLookup := 'colorlistboxitemstyle';
    Item.OnApplyStyleLookup := DoItemApplyStyleLookup;
  end;
  EndUpdate;
  THackListBox(FListbox).FItemIndex := SaveIndex;
end;

procedure TMyColorComboBox.SetColor(const Value: TAlphaColor);
var
  I: Integer;
begin
  for I := 0 to ColorsMap.Count - 1 do
    if ColorsMap.Color[I].Value = Value then
    begin
      ItemIndex := I;
      Break;
    end;
end;

procedure TMyColorComboBox.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then
    OnChange := Value.AsType<TNotifyEvent>()
  else if Value.IsType<TAlphaColor> then
    Color := Value.AsType<TAlphaColor>
  else
    Color := StringToAlphaColor(Value.ToString);
end;

procedure TMyColorComboBox.DoItemApplyStyleLookup(Sender: TObject);
var
  ColorObj: TFmxObject;
begin
  ColorObj := TListBoxItem(Sender).FindStyleResource('color');
  if ColorObj is TShape then
    TShape(ColorObj).Fill.Color := ColorsMap.Color[TListBoxItem(Sender).Tag].Value;
end;

function TMyColorComboBox.GetColor: TAlphaColor;
begin
  if (0 <= ItemIndex) and (ItemIndex < Count) then
    Result := ColorsMap.Color[ItemIndex].Value
  else
    Result := TAlphaColors.Null;
end;

function TMyColorComboBox.GetData: TValue;
begin
  Result := TValue.From<TAlphaColor>(Color);
end;

function TMyColorComboBox.GetDefaultStyleLookupName: string;
begin
  Result := 'comboboxstyle';
end;

procedure InitColorsMap;
begin
  ColorsMap := TRTLColors.Create;
end;

procedure DestroyColorsMap;
begin
  if ColorsMap <> nil then
    FreeAndNil(ColorsMap);
end;

initialization
  InitColorsMap;
  RegisterFmxClasses([TMyHueTrackBar, TMyAlphaTrackBar, TMyBWTrackBar, TMyColorQuad, TMyColorPicker, TMyGradientEdit, TMyColorBox,
    TMyColorPanel, TMyComboColorBox, TMyColorButton, TMyColorComboBox, TMyColorListBox]);

finalization
  DestroyColorsMap;

end.
