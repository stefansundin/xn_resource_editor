unit cmpGradientShape;

interface

uses Windows, Messages, Classes, SysUtils, Controls, Graphics;

type

TCustomGradientShape = class (TGraphicControl)
private
  fPen: TPen;
  fStartColor: TColor;
  procedure SetPen(const Value: TPen);
  procedure SetStartColor(const Value: TColor);
  procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
protected
  procedure StyleChanged(Sender: TObject); virtual;
  property Pen : TPen read fPen write SetPen;
  property StartColor : TColor read fStartColor write SetStartColor;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
end;

TGradientShapeRectType = (gsrLR, gsrTB, gsrTLBR, gsrBLTR);

TCustomGradientShapeRect = class (TCustomGradientShape)
private
  fRectType: TGradientShapeRectType;
  procedure SetRectType(const Value: TGradientShapeRectType);
protected
  procedure Paint; override;

  property RectType : TGradientShapeRectType read fRectType write SetRectType;
end;

TGradientShapeRect = class (TCustomGradientShapeRect)
published
  property Color;
  property RectType;
  property StartColor;
end;

procedure GradientRect (dc : HDC; rectType : TGradientShapeRectType; l, t, r, b : Integer; startColor, endColor : DWORD);

implementation

procedure GradientRect (dc : HDC; rectType : TGradientShapeRectType; l, t, r, b : Integer; startColor, endColor : DWORD);
var
  vert : array [0..3] of TriVertex;
  gRect : GRADIENT_RECT;
  mode : DWORD;
  nVertices, nElements : Integer;
  gTria : array [0..1] of GRADIENT_TRIANGLE;
  p : Pointer;
  sr, sg, sb, er, eg, eb, ar, ag, ab : Integer;
begin
  FillChar (vert, SizeOf (vert), 0);

  sr := GetRValue (ColorToRGB (startColor));
  sg := GetGValue (ColorToRGB (startColor));
  sb := GetBValue (ColorToRGB (startColor));
  er := GetRValue (ColorToRGB (endColor));
  eg := GetGValue (ColorToRGB (endColor));
  eb := GetBValue (ColorToRGB (endColor));

  if RectType in [gsrTLBR, gsrBLTR] then
  begin
    ar := sr shl 8 + (er - sr) shl 7;
    ag := sg shl 8 + (eg - sg) shl 7;
    ab := sb shl 8 + (eb - sb) shl 7;
  end
  else
  begin
    ar := 0;
    ag := 0;
    ab := 0
  end;

  sr := sr shl 8;
  sg := sg shl 8;
  sb := sb shl 8;

  er := er shl 8;
  eg := eg shl 8;
  eb := eb shl 8;

  vert [0].x := l;
  vert [0].y := t;

  vert [1].x := r;
  vert [1].y := b;

  if RectType = gsrBLTR then
  begin
    vert [0].Red := ar;
    vert [0].Green := ag;
    vert [0].Blue := ab;

    vert [1].Red := ar;
    vert [1].Green := ag;
    vert [1].Blue := ab;
  end
  else
  begin
    vert [0].Red := sr;
    vert [0].Green := sg;
    vert [0].Blue  := sb;

    vert [1].Red := er;
    vert [1].Green := eg;
    vert [1].Blue  := eb
  end;

  if RectType in [gsrLR, gsrTB] then
  begin
    nVertices := 2;
    nElements := 1;
    p := @gRect;
    gRect.UpperLeft := 0;
    gRect.LowerRight := 1;
    if RectType = gsrLR then
      mode := GRADIENT_FILL_RECT_H
    else
      mode := GRADIENT_FILL_RECT_V;
  end
  else
  begin
    nVertices := 4;
    nElements := 2;
    mode := GRADIENT_FILL_TRIANGLE;

    p := @gTria [0];

    vert [2].x := r;
    vert [2].y := t;
    vert [3].x := l;
    vert [3].y := b;

    if RectType = gsrTLBR then
    begin
      gTria [0].Vertex1 := 0;
      gTria [0].Vertex2 := 2;
      gTria [0].Vertex3 := 3;
      gTria [1].Vertex1 := 3;
      gTria [1].Vertex2 := 2;
      gTria [1].Vertex3 := 1;

      vert [2].Red := ar;
      vert [2].Green := ag;
      vert [2].Blue := ab;

      vert [3].Red := ar;
      vert [3].Green := ag;
      vert [3].Blue := ab;
    end
    else
    begin
      gTria [0].Vertex1 := 3;
      gTria [0].Vertex2 := 1;
      gTria [0].Vertex3 := 0;
      gTria [1].Vertex1 := 0;
      gTria [1].Vertex2 := 1;
      gTria [1].Vertex3 := 2;

      vert [2].Red := er;
      vert [2].Green := eg;
      vert [2].Blue := eb;

      vert [3].Red := sr;
      vert [3].Green := sg;
      vert [3].Blue := sb;
    end
  end;

  GradientFill (dc, @vert [0], nVertices, p, nElements, mode);
end;

{ TCustomGradientShape }

constructor TCustomGradientShape.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable, csOpaque];
  Width := 65;
  Height := 65;
  FPen := TPen.Create;
  FPen.OnChange := StyleChanged;
end;

destructor TCustomGradientShape.Destroy;
begin
  inherited;
end;

procedure TCustomGradientShape.SetPen(const value : TPen);
begin
  fPen.Assign(Value);
end;

procedure TCustomGradientShape.SetStartColor(const value : TColor);
begin
  if fStartColor <> value then
  begin
    fStartColor := Value;
    Invalidate
  end
end;

procedure TCustomGradientShape.StyleChanged(Sender: TObject);
begin
  Invalidate
end;

procedure TCustomGradientShape.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1

end;

{ TCustomGradientShapeRect }

procedure TCustomGradientShapeRect.Paint;
begin
  GradientRect (Canvas.Handle, RectType, left, top, left + width, top + height, StartColor, color);
end;

procedure TCustomGradientShapeRect.SetRectType(const Value: TGradientShapeRectType);
begin
  if Value <> fRectType then
  begin
    fRectType := Value;
    Invalidate
  end
end;

end.
