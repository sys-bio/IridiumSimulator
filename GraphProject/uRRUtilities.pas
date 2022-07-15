unit uRRUtilities;

interface

Uses System.Types, uRRCommon, FMX.Types, FMX.Graphics, FMX.TextLayout;

function boxToRectF (box : TBox) : TRectF;
function selectedObjectToId  (obj : TSubGraphSelectedObjectType) : integer;

procedure DrawRotatedText(Canvas: TCanvas; const P: TPointF; RadAngle: Single;
  const str : string; HTextAlign, VTextAlign: TTextAlign);
procedure drawDottedBox (canvas : TCanvas; x1, y1, x2, y2 : single);
function rectToBox (value : TRectF): TBox;

procedure setCursor (newCursor : integer);


implementation

Uses System.Math.Vectors, System.UIConsts, FMX.Platform;

function boxToRectF (box : TBox) : TRectF;
begin
  result.Left := box.left; result.Top := box.top;
  result.Right := box.left + box.w;
  result.Bottom := box.top + box.h;
end;


function selectedObjectToId  (obj : TSubGraphSelectedObjectType) : integer;
begin
  case obj of
     coGraphingArea : result := graphingAreaId;
     //coMainTitle : result := mainTitleId;
     //coXAxisTitle : result := xaxisTitleId;
     //coYAxisTitle : result := yaxisTitleId;
     coXAxis : result := xaxisId;
     coYAxis : result := yaxisId;
     coLegend : result := -1;
  end;
end;


procedure setCursor (newCursor : integer);
var cs: IFMXCursorService;
begin
   if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
      begin
      cs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
      cs.SetCursor(newCursor);
      end;
end;

function rectToBox (value : TRectF): TBox;
begin
  result.left := value.Left;
  result.top := value.Top;
  result.w := value.Right - value.Left;
  result.h := value.Bottom - value.Top;
end;


{ Draw a box in dotted outline }
procedure drawDottedBox (canvas : TCanvas; x1, y1, x2, y2 : single);
begin
  canvas.stroke.Dash := TStrokeDash.Dot;
  canvas.stroke.color := claBlack;
  canvas.Stroke.Thickness := 2;
  canvas.Stroke.Kind := TBrushKind.Solid;
  canvas.DrawLine (pointf (x1, y1), pointf (x2, y1), 1.0);
  canvas.DrawLine (pointf (x2, y1), pointf (x2, y2), 1.0);
  canvas.DrawLine (pointf (x2, y2), pointf (x1, y2), 1.0);
  canvas.DrawLine (pointf (x1, y2), pointf (x1, y1), 1.0);
end;

procedure DrawRotatedText(Canvas: TCanvas; const P: TPointF; RadAngle: Single;
  const str: String; HTextAlign, VTextAlign: TTextAlign);
var
  W: Single;
  H: Single;
  R: TRectF;
  SaveMatrix: TMatrix;
  Matrix: TMatrix;
  MyLayout : TTextLayout;
begin
  W := Canvas.TextWidth(str);
  H := Canvas.TextHeight(str);
  case HTextAlign of
    TTextAlign.Center:   R.Left := -W / 2;
    TTextAlign.Leading:  R.Left := 0;
    TTextAlign.Trailing: R.Left := -W;
  end;
  R.Width := W;
  case VTextAlign of
    TTextAlign.Center:   R.Top := -H / 2;
    TTextAlign.Leading:  R.Top := 0;
    TTextAlign.Trailing: R.Top := -H;
  end;
  R.Height := H;
  SaveMatrix := Canvas.Matrix;
  Matrix := TMatrix.CreateRotation(RadAngle);
  Matrix.m31 := P.X;
  Matrix.m32 := P.Y;
  Canvas.MultiplyMatrix(Matrix);

  MyLayout := TTextLayoutManager.DefaultTextLayout.Create;
  MyLayout.BeginUpdate;
  MyLayout.TopLeft := TPointF.Create(R.Left, R.Top);
  MyLayout.Text := str;
  MyLayout.Color := canvas.fill.color;
  MyLayout.Font.Size := canvas.font.size;
  MyLayout.Font.Style := canvas.Font.Style;
  MyLayout.EndUpdate;
  MyLayout.RenderLayout(Canvas);
  MyLayout.Free;

  Canvas.SetMatrix(SaveMatrix);
end;

end.
