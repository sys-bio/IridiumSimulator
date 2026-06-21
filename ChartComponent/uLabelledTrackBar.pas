unit uLabelledTrackBar;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.UITypes,
  System.Types,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Layouts,
  Math;

type
  TLabelledTrackBar = class(TLayout)
  private
    FTrackBar: TTrackBar;
    FCaptionLabel: TLabel;
    FValueLabel: TLabel;

    FDecimalPlaces: Integer;
    FShowCaption: Boolean;
    FShowValue: Boolean;
    FCaptionWidth: Single;
    FValueWidth: Single;

    // NEW
    FCaptionOffsetX: Single;
    FCaptionOffsetY: Single;
    FValueOffsetX: Single;
    FValueOffsetY: Single;

    FOnChange: TNotifyEvent;

    function GetMin: Single;
    function GetMax: Single;
    function GetValue: Single;
    function GetFrequency: Single;
    function GetOrientation: TOrientation;
    function GetCaption: String;

    procedure SetMin(V: Single);
    procedure SetMax(V: Single);
    procedure SetValue(V: Single);
    procedure SetFrequency(V: Single);
    procedure SetOrientation(V: TOrientation);
    procedure SetCaption(const V: String);

    procedure SetDecimalPlaces(V: Integer);
    procedure SetShowCaption(V: Boolean);
    procedure SetShowValue(V: Boolean);
    procedure SetCaptionWidth(V: Single);
    procedure SetValueWidth(V: Single);

    // NEW
    procedure SetCaptionOffsetX(V: Single);
    procedure SetCaptionOffsetY(V: Single);
    procedure SetValueOffsetX(V: Single);
    procedure SetValueOffsetY(V: Single);

    procedure TrackBarChanged(Sender: TObject);
    procedure UpdateValueLabel;
    procedure ApplyLayout;

  protected
    procedure Loaded; override;
    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;

    property TrackBar: TTrackBar read FTrackBar;

  published
    property Align;
    property Anchors;
    property Margins;
    property Padding;
    property Position;
    property Width;
    property Height;
    property Opacity;
    property Visible;
    property Enabled;

    property Min: Single read GetMin write SetMin;
    property Max: Single read GetMax write SetMax;
    property Value: Single read GetValue write SetValue;
    property Frequency: Single read GetFrequency write SetFrequency;

    property Orientation: TOrientation
      read GetOrientation
      write SetOrientation
      default TOrientation.Horizontal;

    property Caption: String read GetCaption write SetCaption;

    property DecimalPlaces: Integer
      read FDecimalPlaces
      write SetDecimalPlaces
      default 2;

    property ShowCaption: Boolean
      read FShowCaption
      write SetShowCaption
      default True;

    property ShowValue: Boolean
      read FShowValue
      write SetShowValue
      default True;

    property CaptionWidth: Single
      read FCaptionWidth
      write SetCaptionWidth;

    property ValueWidth: Single
      read FValueWidth
      write SetValueWidth;

    // NEW POSITIONING PROPERTIES
    property CaptionOffsetX: Single
      read FCaptionOffsetX
      write SetCaptionOffsetX;

    property CaptionOffsetY: Single
      read FCaptionOffsetY
      write SetCaptionOffsetY;

    property ValueOffsetX: Single
      read FValueOffsetX
      write SetValueOffsetX;

    property ValueOffsetY: Single
      read FValueOffsetY
      write SetValueOffsetY;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;
  end;

implementation

const
  DEFAULT_CAPTION_WIDTH = 90.0;
  DEFAULT_VALUE_WIDTH = 48.0;
  LABEL_MARGIN = 6.0;

constructor TLabelledTrackBar.Create(AOwner: TComponent);
begin
  inherited;

  Width := 220;
  Height := 36;

  ClipChildren := False;

  FDecimalPlaces := 2;
  FShowCaption := True;
  FShowValue := True;
  FCaptionWidth := DEFAULT_CAPTION_WIDTH;
  FValueWidth := DEFAULT_VALUE_WIDTH;

  FCaptionLabel := TLabel.Create(Self);
  FCaptionLabel.Parent := Self;
  FCaptionLabel.Stored := False;
  FCaptionLabel.AutoSize := False;
  FCaptionLabel.TextSettings.VertAlign := TTextAlign.Center;
  FCaptionLabel.TextSettings.HorzAlign := TTextAlign.Trailing;
  FCaptionLabel.Height := Height;

  FValueLabel := TLabel.Create(Self);
  FValueLabel.Parent := Self;
  FValueLabel.Stored := False;
  FValueLabel.AutoSize := False;
  FValueLabel.TextSettings.VertAlign := TTextAlign.Center;
  FValueLabel.TextSettings.HorzAlign := TTextAlign.Leading;
  FValueLabel.Height := Height;

  FTrackBar := TTrackBar.Create(Self);
  FTrackBar.Parent := Self;
  FTrackBar.Stored := False;
  FTrackBar.Min := 0;
  FTrackBar.Max := 100;
  FTrackBar.Value := 0;
  FTrackBar.OnChange := TrackBarChanged;

  UpdateValueLabel;
  ApplyLayout;
end;

procedure TLabelledTrackBar.Loaded;
begin
  inherited;

  if FTrackBar <> nil then
    FTrackBar.OnChange := TrackBarChanged;

  ApplyLayout;
end;

procedure TLabelledTrackBar.Resize;
begin
  inherited;
  ApplyLayout;
end;

procedure TLabelledTrackBar.ApplyLayout;
var
  LeftSpace: Single;
  RightSpace: Single;
begin
  if FTrackBar = nil then
    Exit;

    if Orientation = TOrientation.Horizontal then
    begin
      LeftSpace := 0;
      RightSpace := 0;

      if FShowCaption and (FCaptionLabel.Text <> '') then
        LeftSpace := FCaptionWidth + LABEL_MARGIN;

      // reserve only the requested spacing,
      // but keep the label itself visible
      if FShowValue then
        RightSpace := Math.Max(0, FValueWidth);

      FTrackBar.SetBounds(
        LeftSpace,
        0,
        Width - LeftSpace - RightSpace,
        Height
      );

      FCaptionLabel.Visible := FShowCaption;
      FCaptionLabel.SetBounds(
        FCaptionOffsetX,
        FCaptionOffsetY,
        FCaptionWidth,
        Height
      );

      FValueLabel.Visible := FShowValue;

      // keep actual label width sensible
      FValueLabel.SetBounds(
        Width - Math.Max(24, FValueWidth) + FValueOffsetX,
        FValueOffsetY,
        Math.Max(24, FValueWidth),
        Height
      );
    end
  else
  begin
    FTrackBar.SetBounds(
      0,
      24,
      Width,
      Height - 48
    );

    FValueLabel.SetBounds(
      FValueOffsetX,
      FValueOffsetY,
      Width,
      22
    );

    FCaptionLabel.SetBounds(
      FCaptionOffsetX,
      Height - 22 + FCaptionOffsetY,
      Width,
      22
    );
  end;
end;

procedure TLabelledTrackBar.TrackBarChanged(Sender: TObject);
begin
  UpdateValueLabel;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLabelledTrackBar.UpdateValueLabel;
var
  Fmt: String;
begin
  if FDecimalPlaces = 0 then
    Fmt := '0'
  else
    Fmt := '0.' + DupeString('0', FDecimalPlaces);

  FValueLabel.Text := FormatFloat(Fmt, FTrackBar.Value);
end;

function TLabelledTrackBar.GetMin: Single;
begin
  Result := FTrackBar.Min;
end;

function TLabelledTrackBar.GetMax: Single;
begin
  Result := FTrackBar.Max;
end;

function TLabelledTrackBar.GetValue: Single;
begin
  Result := FTrackBar.Value;
end;

function TLabelledTrackBar.GetFrequency: Single;
begin
  Result := FTrackBar.Frequency;
end;

function TLabelledTrackBar.GetOrientation: TOrientation;
begin
  Result := FTrackBar.Orientation;
end;

function TLabelledTrackBar.GetCaption: String;
begin
  Result := FCaptionLabel.Text;
end;

procedure TLabelledTrackBar.SetMin(V: Single);
begin
  FTrackBar.Min := V;
end;

procedure TLabelledTrackBar.SetMax(V: Single);
begin
  FTrackBar.Max := V;
end;

procedure TLabelledTrackBar.SetValue(V: Single);
begin
  FTrackBar.Value := V;
  UpdateValueLabel;
end;

procedure TLabelledTrackBar.SetFrequency(V: Single);
begin
  FTrackBar.Frequency := V;
end;

procedure TLabelledTrackBar.SetOrientation(V: TOrientation);
begin
  FTrackBar.Orientation := V;
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetCaption(const V: String);
begin
  FCaptionLabel.Text := V;
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetDecimalPlaces(V: Integer);
begin
  if V < 0 then
    V := 0;

  FDecimalPlaces := V;
  UpdateValueLabel;
end;

procedure TLabelledTrackBar.SetShowCaption(V: Boolean);
begin
  FShowCaption := V;
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetShowValue(V: Boolean);
begin
  FShowValue := V;
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetCaptionWidth(V: Single);
begin
  FCaptionWidth := Math.Max(0, V);
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetValueWidth(V: Single);
begin
  FValueWidth := Math.Max(0, V);
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetCaptionOffsetX(V: Single);
begin
  FCaptionOffsetX := V;
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetCaptionOffsetY(V: Single);
begin
  FCaptionOffsetY := V;
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetValueOffsetX(V: Single);
begin
  FValueOffsetX := V;
  ApplyLayout;
end;

procedure TLabelledTrackBar.SetValueOffsetY(V: Single);
begin
  FValueOffsetY := V;
  ApplyLayout;
end;

end.

