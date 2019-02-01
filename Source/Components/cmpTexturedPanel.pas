unit cmpTexturedPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TTextureKind = (tkTile, tkTileLeft, tkTileTop);
  TTexturedPanel = class(TPanel)
  private
    FPicture: TPicture;
    FBitmap : TBitmap;
    FTextureKind: TTextureKind;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    procedure TileRect(rect : TRect);
    procedure SetTextureKind(const Value: TTextureKind);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property Bitmap : TBitmap read fBitmap;
  published
    property Picture : TPicture read FPicture write SetPicture;
    property TExtureKind : TTextureKind read FTextureKind write SetTextureKind;
  end;

implementation

{ TTexturedPanel }

constructor TTexturedPanel.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FBitmap := TBitmap.Create;
  FPicture.OnChange := PictureChanged;
end;

destructor TTexturedPanel.Destroy;
begin
  FBitmap.Free;
  FPicture.Free;
  inherited;
end;

procedure TTexturedPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  FontHeight: Integer;
  Flags: Longint;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;

  if Assigned (Picture.Graphic) and not Picture.Graphic.Empty then
  begin
    if TExtureKind <> tkTile then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(Rect)
    end;
    TileRect(rect);
    InflateRect(rect, -BorderWidth, -BorderWidth)
  end
  else
    Frame3D(Canvas, Rect, Color, Color, BorderWidth);
    
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  with Canvas do
  begin
    Brush.Color := Color;

    if not Assigned (Picture.Graphic) or Picture.Graphic.Empty then
      FillRect(Rect);
    Brush.Style := bsClear;
    Font := Self.Font;
    FontHeight := TextHeight('W');
    with Rect do
    begin
      Top := ((Bottom + Top) - FontHeight) div 2;
      Bottom := Top + FontHeight;
    end;
    Flags := DT_NOPREFIX or DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
    Flags := DrawTextBiDiModeFlags(Flags);
    DrawText(Handle, PChar(Caption), -1, Rect, Flags);
  end;
end;

procedure TTexturedPanel.PictureChanged(Sender: TObject);
begin
  FBitmap.Assign (FPicture.Graphic);
  Invalidate
end;

procedure TTexturedPanel.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TTexturedPanel.SetTextureKind(const Value: TTextureKind);
begin
  if Value <> FTextureKind then
  begin
    FTextureKind := Value;
    Invalidate
  end
end;

procedure TTexturedPanel.TileRect(rect: TRect);
var
  r : TRect;
  procedure BltRect(rect : TRect);
  begin
    BitBlt(Canvas.Handle, rect.Left, rect.Top, rect.Right - rect.Left, rect.Bottom - rect.Top, FBitmap.Canvas.Handle, 0, 0, SRCCOPY)
  end;
begin
  r.Top := rect.Top;

  repeat
    r.Left := rect.Left;
    repeat
      r.Right := r.Left + FBitmap.Width;
      r.Bottom := r.Top + FBitmap.Height;

      if r.Right > rect.Right then
        r.Right := rect.Right;

      if r.Bottom > rect.Bottom then
        r.Bottom := rect.Bottom;

      BltRect(r);

      r.Left := r.Right;
      if textureKind = tkTileLeft then
        break
    until r.Right = rect.Right;

    r.Top := r.Bottom;
    if textureKind = tkTileTop then
      break
  until r.Bottom = rect.Bottom
end;

end.
