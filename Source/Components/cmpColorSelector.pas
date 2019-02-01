unit cmpColorSelector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, unitExGraphics;
                                                            
type
  TPaletteEntries = array of TPaletteEntry;
  TColorSelector = class(TCustomControl)
  private
    FPalette: HPalette;
    FPaletteEntries: TPaletteEntries;
    FColWidth, FRowHeight, FColCount, FRowCount: Integer;
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    FOnColorSelect: TNotifyEvent;
    FSelectedIdx: Integer;
    FSelectedColor: TColor;   // For >256 colors only
    FColorCount: Integer;
    FLum: Integer;
    procedure SetPalette(const Value: HPalette);
    procedure SetColorCount(const Value: Integer);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetColumnCount(const Value: Integer);
    procedure PaintHSL (canvas: TCanvas);
    procedure SetLum(const Value: Integer);
    procedure CalcSettings;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    function GetColor(idx: Integer): TCOlor;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Palette: HPalette read FPalette write SetPalette;
    procedure SetSelectedPaletteColor (color: TColor);
    function GetSelectedPaletteColor: TColor;
    property Color [idx: Integer]: TCOlor read GetColor;
  published
    property Align;
    property Anchors;
    property Constraints;
    property ColorCount: Integer read FColorCount write SetColorCount default 16;
    property ForegroundColor: TColor read FForegroundColor write SetForegroundColor;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property OnColorSelect: TNotifyEvent read FOnColorSelect write FOnColorSelect;
    property ColumnCount: Integer read FColCount write SetColumnCount default 8;
    property OnDblClick;
    property Luminescence: Integer read FLum write SetLum default HLSMAX div 2;
  end;

  EColorError = class (Exception)
  end;


implementation

{ TColorSelector }

procedure TColorSelector.CalcSettings;
begin
  if FPalette <> 0 then
  begin
    if FColorCount mod FColCount = 0 then
      FRowCount := FColorCount div FColCount
    else
      FRowCount := FColorCount div FColCount + 1;
      
    FColWidth := Width div FColCount;
    FRowHeight := Height div FRowCount
  end
end;

constructor TColorSelector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLum := HLSMAX div 2;
  FColCount := 8;
  Palette := SystemPalette16;
  Width := 100;
  Height := 100;
  DoubleBuffered := True;
  CalcSettings;
end;

destructor TColorSelector.Destroy;
begin
  if FPalette <> 0 then
    DeleteObject(FPalette);
  inherited
end;

function TColorSelector.GetColor(idx: Integer): TCOlor;
begin
  with FPaletteEntries[idx] do
    Result := RGB (peRed, peGreen, peBlue);
end;

function TColorSelector.GetSelectedPaletteColor: TColor;
begin
  if FPalette <> 0 then
    with FPaletteEntries[FSelectedIdx] do
      Result := RGB (peRed, peGreen, peBlue)
  else
    Result := FSelectedColor;
end;

procedure TColorSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  selectedColor: TColor;
  changed: Boolean;
  selectedIdx: Integer;
  b: TBitmap;
begin
  SetFocus;
  p.x := x;
  p.y := y;
  ScreenToClient(p);

  selectedIdx := -1;
  if FPalette = 0 then
  begin
    b := TBitmap.Create;
    try
      b.Width := ClientWidth;
      b.Height := ClientHeight;
      PaintHSL (b.Canvas);
      selectedColor := b.Canvas.Pixels[x, y] // RGB (50, 60, 70) // Canvas.Pixels[x, y];
    finally
      b.Free
    end
  end
  else
  begin
    selectedIdx := (x div FColWidth) + (y div FRowHeight) * FColCount;
    if selectedIdx < ColorCount then
    begin

      with FPaletteEntries[selectedIdx] do
        selectedColor := RGB (peRed, peGreen, peBlue)
    end
    else
      selectedColor := 0;
  end;

  if (FPalette = 0) or (selectedIdx < ColorCount) then
  begin
    changed := False;

    if Button = mbLeft then
    begin
      changed := FForegroundColor <> selectedColor;
      FForegroundColor := selectedColor
    end;

    if Button = mbRight then
    begin
      changed := FBackgroundColor <> selectedColor;
      FBackgroundColor := selectedColor
    end;

    FSelectedIdx := selectedIdx;
    FSelectedColor := selectedColor;
    if Assigned(FOnColorSelect) and changed and not (csDestroying in ComponentState) then
      OnColorSelect(Self)
  end
end;

procedure TColorSelector.Paint;
var
  i, x, y: Integer;
begin
  if ColorCount = -1 then
    PaintHSL (canvas)
  else
  begin
    x := 0;
    y := 0;
    for i := 0 to ColorCount - 1 do
    begin
      with FPaletteEntries[i] do
        Canvas.Brush.Color := RGB (peRed, peGreen, peBlue);

      Canvas.Rectangle(x, y, x + FColWidth, y + FRowHeight);
      Inc(x, FColWidth);
      if x + FColWidth > width then
      begin
        x := 0;
        Inc(y, FRowHeight)
      end
    end
  end
end;

procedure TColorSelector.PaintHSL (canvas: TCanvas);
var
  hue, sat, bpss, w, h: Integer;
  r, g, b: Integer;
  rt: TRGBTriple;
  bmp: TBitmap;
  ps, ps1: PRGBTriple;
begin
  bmp := TBitmap.Create;
  try
    w := Width;
    h := Height;
    bmp.Width := w;
    bmp.Height := h;
    bmp.PixelFormat := pf24Bit;

    bpss := BytesPerScanLine(w, 24, 32);
    ps1 := bmp.ScanLine [0];

    try
      for sat := 0 to h - 1 do
      begin
        ps := PRGBTriple(PChar (ps1) - bpss * sat);
        for hue := 0 to w - 1 do
        begin
          iHLSToRGB (hue * HLSMAX div w, FLum, (h - sat) * HLSMAX div h, r, g, b);
          rt.rgbtRed := r;
          rt.rgbtGreen := g;
          rt.rgbtBlue := b;
          ps^ := rt;
          Inc(ps);
        end
      end;
    except
    end;

    Canvas.Draw (0, 0, bmp);
  finally
    bmp.Free
  end
end;


procedure TColorSelector.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
end;

procedure TColorSelector.SetColorCount(const Value: Integer);
begin
  if Value <> ColorCount then
  begin
    case value of
      2: Palette := SystemPalette2;
      16: Palette := SystemPalette16;
      256: Palette := SystemPalette256;
      -1: Palette := 0;
    end
  end;
end;

procedure TColorSelector.SetColumnCount(const Value: Integer);
begin
  if value <> FColCount then
  begin
    FColCount := value;
    if FColCount = 0 then
      FColCount := 1;

    CalcSettings;

    Invalidate
  end
end;

procedure TColorSelector.SetForegroundColor(const Value: TColor);
begin
  FForegroundColor := Value;
end;

procedure TColorSelector.SetLum(const Value: Integer);
begin
  if Value <> FLum then
  begin
    if FLum in [0..HLSMAX] then
    begin
      FLum := Value;
      Invalidate
    end
    else
      raise EColorError.Create('Luminescense must be in range 0..' + IntToStr (HLSMAX));
  end
end;

procedure TColorSelector.SetPalette(const Value: HPalette);
var
  colorCount: word;
begin
  if Value <> FPalette then
  begin
    if FPalette <> 0 then
    begin
      DeleteObject(FPalette);
      FPalette := 0
    end;

    if value <> 0 then
      if GetObject(Value, SizeOf(colorCount), @colorCount) <> 0 then
      begin
        SetLength(FPaletteEntries, colorCount);
        FColorCount := colorCount;
        GetPaletteEntries (Value, 0, colorCount, FPaletteEntries[0]);
        FPalette := CopyPalette(Value);
        CalcSettings;
        Invalidate
      end
      else
        raiseLastOSError
    else
    begin
      FColorCount := -1;
      Invalidate
    end
  end
end;

procedure TColorSelector.SetSelectedPaletteColor(color: TColor);
var
  colorCount: word;
  rgb, i: LongInt;
  NewPal: HPALETTE;
  logPal: PLogPalette;
begin
  if FPalette = 0 then
    Exit;

  if GetObject(FPalette, SizeOf(colorCount), @colorCount) <> 0 then
  begin
    rgb := ColorToRGB (color);
    SetLength(FPaletteEntries, colorCount);
    GetPaletteEntries (FPalette, 0, colorCount, FPaletteEntries[0]);
    FPaletteEntries[FSelectedIdx].peRed   := GetRValue(rgb);
    FPaletteEntries[FSelectedIdx].peGreen := GetGValue(rgb);
    FPaletteEntries[FSelectedIdx].peBlue  := GetBValue(rgb);

    GetMem (logPal, SizeOf(TLogPalette) + 256 * SizeOf(TPaletteEntry));
    try
      logPal^.palVersion := $300;
      logPal^.palNumEntries := colorCount;

      for i := 0 to colorCount - 1 do
        logPal^.palPalEntry [i] := FPaletteEntries[i];

      NewPal := CreatePalette(logPal^);
      if NewPal <> 0 then
      try
        Palette := NewPal;
      finally
        DeleteObject(NewPal)
      end
    finally
      FreeMem (logPal)
    end
  end
  else
    raiseLastOSError
end;

procedure TColorSelector.WMSize(var Message: TWMSize);
begin
  inherited;

  CalcSettings;
end;

end.
