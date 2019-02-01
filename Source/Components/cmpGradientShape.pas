unit cmpGradientShape;

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics;

type
  TCustomGradientShape = class (TGraphicControl)
  private
    FPen: TPen;
    FStartColor: TColor;
    procedure SetPen(const Value: TPen);
    procedure SetStartColor(const Value: TColor);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure StyleChanged(Sender: TObject); virtual;
    property Pen: TPen read FPen write SetPen;
    property StartColor: TColor read FStartColor write SetStartColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TGradientShapeRectType = (gsrLR, gsrTB, gsrTLBR, gsrBLTR);

  TCustomGradientShapeRect = class (TCustomGradientShape)
  private
    FRectType: TGradientShapeRectType;
    procedure SetRectType(const Value: TGradientShapeRectType);
  protected
    procedure Paint; override;

    property RectType: TGradientShapeRectType read FRectType write SetRectType;
  end;

  TGradientShapeRect = class (TCustomGradientShapeRect)
  published
    property Color;
    property RectType;
    property StartColor;
  end;

procedure GradientRect(dc: HDC; rectType: TGradientShapeRectType; l, t, r, b: Integer; startColor, endColor: DWORD);

implementation

procedure GradientRect(dc: HDC; rectType: TGradientShapeRectType; l, t, r, b: Integer; startColor, endColor: DWORD);
var
  Vertex: array [0..3] of TriVertex;
  gRect: GRADIENT_RECT;
  mode: DWORD;
  nVertices, nElements: Integer;
  gTria: array [0..1] of GRADIENT_TRIANGLE;
  p: Pointer;
  sr, sg, sb, er, eg, eb, ar, ag, ab: Integer;
begin
  FillChar (Vertex, SizeOf(Vertex), 0);

  sr := GetRValue(ColorToRGB (startColor));
  sg := GetGValue(ColorToRGB (startColor));
  sb := GetBValue(ColorToRGB (startColor));
  er := GetRValue(ColorToRGB (endColor));
  eg := GetGValue(ColorToRGB (endColor));
  eb := GetBValue(ColorToRGB (endColor));

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

  Vertex [0].x := l;
  Vertex [0].y := t;

  Vertex [1].x := r;
  Vertex [1].y := b;

  if RectType = gsrBLTR then
  begin
    Vertex [0].Red := ar;
    Vertex [0].Green := ag;
    Vertex [0].Blue := ab;

    Vertex [1].Red := ar;
    Vertex [1].Green := ag;
    Vertex [1].Blue := ab;
  end
  else
  begin
    Vertex [0].Red := sr;
    Vertex [0].Green := sg;
    Vertex [0].Blue  := sb;

    Vertex [1].Red := er;
    Vertex [1].Green := eg;
    Vertex [1].Blue  := eb
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

    Vertex [2].x := r;
    Vertex [2].y := t;
    Vertex [3].x := l;
    Vertex [3].y := b;

    if RectType = gsrTLBR then
    begin
      gTria [0].Vertex1 := 0;
      gTria [0].Vertex2 := 2;
      gTria [0].Vertex3 := 3;
      gTria [1].Vertex1 := 3;
      gTria [1].Vertex2 := 2;
      gTria [1].Vertex3 := 1;

      Vertex [2].Red := ar;
      Vertex [2].Green := ag;
      Vertex [2].Blue := ab;

      Vertex [3].Red := ar;
      Vertex [3].Green := ag;
      Vertex [3].Blue := ab;
    end
    else
    begin
      gTria [0].Vertex1 := 3;
      gTria [0].Vertex2 := 1;
      gTria [0].Vertex3 := 0;
      gTria [1].Vertex1 := 0;
      gTria [1].Vertex2 := 1;
      gTria [1].Vertex3 := 2;

      Vertex [2].Red := er;
      Vertex [2].Green := eg;
      Vertex [2].Blue := eb;

      Vertex [3].Red := sr;
      Vertex [3].Green := sg;
      Vertex [3].Blue := sb;
    end
  end;

  GradientFill (dc, @Vertex [0], nVertices, p, nElements, mode);
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

procedure TCustomGradientShape.SetPen(const value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TCustomGradientShape.SetStartColor(const value: TColor);
begin
  if FStartColor <> value then
  begin
    FStartColor := Value;
    Invalidate;
  end
end;

procedure TCustomGradientShape.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomGradientShape.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

{ TCustomGradientShapeRect }

procedure TCustomGradientShapeRect.Paint;
begin
  GradientRect(Canvas.Handle, RectType, left, top, left + width, top + height, StartColor, color);
end;

procedure TCustomGradientShapeRect.SetRectType(const Value: TGradientShapeRectType);
begin
  if Value <> FRectType then
  begin
    FRectType := Value;
    Invalidate;
  end;
end;

end.
