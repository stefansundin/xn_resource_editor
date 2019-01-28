unit cmpColorSelector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, unitExGraphics;
                                                            
type
  TPaletteEntries = array of TPaletteEntry;
  TColorSelector = class(TCustomControl)
  private
    fPalette: HPalette;
    fPaletteEntries : TPaletteEntries;
    fColWidth, fRowHeight, fColCount, fRowCount : Integer;
    fBackgroundColor: TColor;
    fForegroundColor: TColor;
    fOnColorSelect: TNotifyEvent;
    fSelectedIdx : Integer;
    fSelectedColor : TColor;   // For >256 colors only
    fColorCount : Integer;
    fLum: Integer;
    procedure SetPalette(const Value: HPalette);
    procedure SetColorCount(const Value: Integer);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetForegroundColor(const Value: TColor);
    procedure SetColumnCount(const Value: Integer);
    procedure PaintHSL (canvas : TCanvas);
    procedure SetLum(const Value: Integer);
    procedure CalcSettings;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    function GetColor(idx: Integer): TCOlor;
    { Private declarations }
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    { Protected declarations }
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    property Palette : HPalette read fPalette write SetPalette;
    procedure SetSelectedPaletteColor (color : TColor);
    function GetSelectedPaletteColor : TColor;
    property Color [idx : Integer] : TCOlor read GetColor;
  published
    property Align;
    property Anchors;
    property Constraints;
    property ColorCount : Integer read fColorCount write SetColorCount default 16;
    property ForegroundColor : TColor read fForegroundColor write SetForegroundColor;
    property BackgroundColor : TColor read fBackgroundColor write SetBackgroundColor;
    property OnColorSelect : TNotifyEvent read fOnColorSelect write fOnColorSelect;
    property ColumnCount : Integer read fColCount write SetColumnCount default 8;
    property OnDblClick;
    property Luminescence : Integer read fLum write SetLum default HLSMAX div 2;
  end;

  EColorError = class (Exception)
  end;


implementation

{ TColorSelector }

procedure TColorSelector.CalcSettings;
begin
  if fPalette <> 0 then
  begin
    if fColorCount mod fColCount = 0 then
      fRowCount := fColorCount div fColCount
    else
      fRowCount := fColorCount div fColCount + 1;
      
    fColWidth := Width div fColCount;
    fRowHeight := Height div fRowCount
  end
end;

constructor TColorSelector.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  fLum := HLSMAX div 2;
  fColCount := 8;
  Palette := SystemPalette16;
  Width := 100;
  Height := 100;
  DoubleBuffered := True;
  CalcSettings;
end;

destructor TColorSelector.Destroy;
begin
  if fPalette <> 0 then
    DeleteObject (fPalette);
  inherited
end;

function TColorSelector.GetColor(idx: Integer): TCOlor;
begin
  with fPaletteEntries [idx] do
    result := RGB (peRed, peGreen, peBlue);
end;

function TColorSelector.GetSelectedPaletteColor: TColor;
begin
  if fPalette <> 0 then
    with fPaletteEntries [fSelectedIdx] do
      result := RGB (peRed, peGreen, peBlue)
  else
    result := fSelectedColor;
end;

procedure TColorSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p : TPoint;
  selectedColor : TColor;
  changed : boolean;
  selectedIdx : Integer;
  b : TBitmap;
begin
  SetFocus;
  p.x := x;
  p.y := y;
  ScreenToClient (p);

  selectedIdx := -1;
  if fPalette = 0 then
  begin
    b := TBitmap.Create;
    try
      b.Width := ClientWidth;
      b.Height := ClientHeight;
      PaintHSL (b.Canvas);
      selectedColor := b.Canvas.Pixels [x, y] // RGB (50, 60, 70) // Canvas.Pixels [x, y];
    finally
      b.Free
    end
  end
  else
  begin
    selectedIdx := (x div fColWidth) + (y div fRowHeight) * fColCount;
    if selectedIdx < ColorCount then
    begin

      with fPaletteEntries [selectedIdx] do
        selectedColor := RGB (peRed, peGreen, peBlue)
    end
    else
      selectedColor := 0;
  end;

  if (fPalette = 0) or (selectedIdx < ColorCount) then
  begin
    changed := False;

    if Button = mbLeft then
    begin
      changed := fForegroundColor <> selectedColor;
      fForegroundColor := selectedColor
    end;

    if Button = mbRight then
    begin
      changed := fBackgroundColor <> selectedColor;
      fBackgroundColor := selectedColor
    end;

    fSelectedIdx := selectedIdx;
    fSelectedColor := selectedColor;
    if Assigned (fOnColorSelect) and changed and not (csDestroying in ComponentState) then
      OnColorSelect (self)
  end
end;

procedure TColorSelector.Paint;
var
  i, x, y : Integer;
begin
  if ColorCount = -1 then
    PaintHSL (canvas)
  else
  begin
    x := 0;
    y := 0;
    for i := 0 to ColorCount - 1 do
    begin
      with fPaletteEntries [i] do
        Canvas.Brush.Color := RGB (peRed, peGreen, peBlue);

      Canvas.Rectangle (x, y, x + fcolWidth, y + fRowHeight);
      Inc (x, fcolWidth);
      if x + fcolWidth > width then
      begin
        x := 0;
        Inc (y, fRowHeight)
      end
    end
  end
end;

procedure TColorSelector.PaintHSL (canvas : TCanvas);
var
  hue, sat, bpss, w, h : Integer;
  r, g, b : Integer;
  rt : TRGBTriple;
  bmp : TBitmap;
  ps, ps1 : PRGBTriple;
begin
  bmp := TBitmap.Create;
  try
    w := Width;
    h := Height;
    bmp.Width := w;
    bmp.Height := h;
    bmp.PixelFormat := pf24Bit;

    bpss := BytesPerScanLine (w, 24, 32);
    ps1 := bmp.ScanLine [0];

    try
      for sat := 0 to h - 1 do
      begin
        ps := PRGBTriple (PChar (ps1) - bpss * sat);
        for hue := 0 to w - 1 do
        begin
          iHLSToRGB (hue * HLSMAX div w, fLum, (h - sat) * HLSMAX div h, r, g, b);
          rt.rgbtRed := r;
          rt.rgbtGreen := g;
          rt.rgbtBlue := b;
          ps^ := rt;
          Inc (ps);
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
  fBackgroundColor := Value;
end;

procedure TColorSelector.SetColorCount(const Value: Integer);
begin
  if Value <> ColorCount then
  begin
    case value of
      2 : Palette := SystemPalette2;
      16 : Palette := SystemPalette16;
      256 : Palette := SystemPalette256;
      -1 : Palette := 0;
    end
  end;
end;

procedure TColorSelector.SetColumnCount(const Value: Integer);
begin
  if value <> fColCount then
  begin
    fColCount := value;
    if fColCount = 0 then
      fColCount := 1;

    CalcSettings;

    Invalidate
  end
end;

procedure TColorSelector.SetForegroundColor(const Value: TColor);
begin
  fForegroundColor := Value;
end;

procedure TColorSelector.SetLum(const Value: Integer);
begin
  if Value <> fLum then
  begin
    if fLum in [0..HLSMAX] then
    begin
      fLum := Value;
      Invalidate
    end
    else
      raise EColorError.Create ('Luminescense must be in range 0..' + IntToStr (HLSMAX));
  end
end;

procedure TColorSelector.SetPalette(const Value: HPalette);
var
  colorCount : word;
begin
  if Value <> fPalette then
  begin
    if fPalette <> 0 then
    begin
      DeleteObject (fPalette);
      fPalette := 0
    end;

    if value <> 0 then
      if GetObject (Value, sizeof (colorCount), @colorCount) <> 0 then
      begin
        SetLength (fPaletteEntries, colorCount);
        fColorCount := colorCount;
        GetPaletteEntries (Value, 0, colorCount, fPaletteEntries [0]);
        fPalette := CopyPalette (Value);
        CalcSettings;
        Invalidate
      end
      else
        raiseLastOSError
    else
    begin
      fColorCount := -1;
      Invalidate
    end
  end
end;

procedure TColorSelector.SetSelectedPaletteColor(color: TColor);
var
  colorCount : word;
  rgb, i : LongInt;
  fNewPal : HPALETTE;
  logPal : PLogPalette;
begin
  if fPalette = 0 then
    Exit;

  if GetObject (fPalette, sizeof (colorCount), @colorCount) <> 0 then
  begin
    rgb := ColorToRGB (color);
    SetLength (fPaletteEntries, colorCount);
    GetPaletteEntries (fPalette, 0, colorCount, fPaletteEntries [0]);
    fPaletteEntries [fSelectedIdx].peRed   := GetRValue (rgb);
    fPaletteEntries [fSelectedIdx].peGreen := GetGValue (rgb);
    fPaletteEntries [fSelectedIdx].peBlue  := GetBValue (rgb);

    GetMem (logPal, sizeof (TLogPalette) + 256 * sizeof (TPaletteEntry));
    try
      logPal^.palVersion := $300;
      logPal^.palNumEntries := colorCount;

      for i := 0 to colorCount - 1 do
        logPal^.palPalEntry [i] := fPaletteEntries [i];

      fNewPal := CreatePalette (logPal^);
      if fNewPal <> 0 then
      try
        Palette := fNewPal;
      finally
        DeleteObject (fNewPal)
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
