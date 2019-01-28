unit cmpBitmapEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TDrawingTool = (dtNone, dtPencil, dtLine,
                  dtFrameRect, dtFillRect, dtRect,
                  dtFloodFill,
                  dtFrameRoundRect, dtFillRoundRect, dtRoundRect,
                  dtFrameEllipse, dtFillEllipse, dtEllipse,
                  dtMagnifier,
                  dtBrush,
                  dtSelectRect,
                  dtSelectArea,
                  dtDropper, dtText, dtEraser, dtAirbrush,
                  dtGradRectLR, dtGradRectTB, dtGradRectTLBR, dtGradRectBLTR);

  TOnGetText = procedure (sender : TObject; font : TFont; var txt : WideString) of object;

  TBitmapEditor = class(TCustomControl)
  private
    fPicture: TPicture;
    fMagnification: Integer;
    fGridLines: Integer;
    fBorderStyle: TBorderStyle;
    fDrawingTool: TDrawingTool;
    fLastDrawingTool: TDrawingTool;    // Reselect after using dropper
    fDrawBrush: TBrush;
    fDrawPen: TPen;
    fEraser : TPen;
    fDrawBmp : TBitmap;
    fScratchBmp : TBitmap;
    fSelectionBmp : TBitmap;
    fPos : TPoint;
    fOnChange: TNotifyEvent;
    fTransparentColor : TColor;
    fOnDrawToolChange: TNotifyEvent;
    fCrossX, fCrossY : Integer;
    fSelectionRect : TRect;
    fLButtonIsDown : boolean;
    fMouseCaptured : boolean;
    fOnEndChange: TNotifyEvent;
    fCallEndChange : boolean;
    fOnSelectionRectChange: TNotifyEvent;
    fClipboardPalette: HPALETTE;
    fClipboardPixelFormat: TPixelFormat;
    fOnGetText: TOnGetText;
    fHotSpotX: Integer;
    fHotSpotY: Integer;

    procedure SetPicture(const Value: TPicture);
    procedure PaintBitmap (bmp : TBitmap);
    procedure SetMagnification(const Value: Integer);
    procedure SizeToPicture;
    procedure SetGridLines(const Value: Integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetDrawBrush(const Value: TBrush);
    procedure SetDrawingTool(const Value: TDrawingTool);
    procedure SetDrawPen(const Value: TPen);
    procedure ChangeSelectionRect (const rect : TRect);
    procedure RedrawBitmap;
    procedure Initialize;
    procedure SetTransparentColor(const Value: TColor);
    procedure DisplayCrossHairs;
    function GetSelectionValid: boolean;
    procedure SetHotSpotX(const Value: Integer);
    procedure SetHotSpotY(const Value: Integer);
    procedure DrawHotSpot(canvas: TCanvas);
  protected
    procedure Paint; override;
    procedure CreateParams (var params : TCreateParams ); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ZoomIn;
    procedure ZoomOut;
    property DrawBmp : TBitmap read fDrawBmp;
    property SelectionValid : boolean read GetSelectionValid;
    property SelectionRect : TRect read fSelectionRect;
    property ClipboardPalette : HPALETTE read fClipboardPalette write fClipboardPalette;
    property ClipboardPixelFormat : TPixelFormat read fClipboardPixelFormat write fClipboardPixelFormat;
    procedure SelectAll;
    procedure DeleteSelection;
    procedure CopySelection;
    procedure CutSelection;
    procedure PasteSelection;
    procedure PictureChanged;

    procedure Rotate180;
    procedure Rotate90;
    procedure Rotate270;

    function GetDrawingChangeDescription : string;

    property HotSpotX : Integer read fHotSpotX write SetHotSpotX default -1;
    property HotSpotY : Integer read fHotSpotY write SetHotSpotY default -1;

  published
    { Published declarations }
    property Picture : TPicture read fPicture write SetPicture;
    property TransparentColor : TColor read fTransparentColor write SetTransparentColor default clTeal;
    property Magnification : Integer read fMagnification write SetMagnification default 4;
    property GridLines : Integer read fGridLines write SetGridLines default 4;
    property BorderStyle : TBorderStyle read fBorderStyle write SetBorderStyle default bsSingle;
    property DoubleBuffered;
    property PopupMenu;
    property TabStop;
    property DrawingTool : TDrawingTool read fDrawingTool write SetDrawingTool;
    property DrawPen : TPen read fDrawPen write SetDrawPen;
    property DrawBrush : TBrush read fDrawBrush write SetDrawBrush;
    property OnChange : TNotifyEvent read fOnChange write fOnChange;
    property OnEndChange : TNotifyEvent read fOnEndChange write fOnEndChange;
    property OnDrawToolChange : TNotifyEvent read fOnDrawToolChange write fOnDrawToolChange;
    property OnSelectionRectChange : TNotifyEvent read fOnSelectionRectChange write fOnSelectionRectChange;
    property OnGetText : TOnGetText read fOnGetText write fOnGetText;
  end;

const
  crPencil = 1;
  crPotOPaint = 2;
  crMagnifier = 3;
  crDotCross = 4;
  crDropper = 5;
  crAirbrush = 6;

  DrawingCursors : array [TDrawingTool] of TCursor =
    (crArrow, crPencil, crCross, crCross, crCross, crCross, crPotOPaint, crCross, crCross, crCross, crCross, crCross, crCross,
     crMagnifier, crDotCross, crNone, crNone, crDropper, crIBeam, crCross, crAirbrush, crCross, crCross, crCross, crCross);

implementation

{$R BitmapEditorCursors.res}

uses GraphFlip, Clipbrd, cmpGradientShape, GifImage;

{ TBitmapEditor }

procedure TBitmapEditor.ChangeSelectionRect(const rect: TRect);
var
  oldValid : Boolean;
begin
  oldValid := SelectionValid;

  if rect.Left <> -2 then fSelectionRect.Left := rect.Left;
  if rect.Top <> -2 then fSelectionRect.Top := rect.Top;
  if rect.Right <> -2 then fSelectionRect.Right := rect.Right;
  if rect.Bottom <> -2 then fSelectionRect.Bottom := rect.Bottom;

  if SelectionValid or oldValid then
    Invalidate;

  if Assigned (OnSelectionRectChange) then
    OnSelectionRectChange (Self)
end;

procedure TBitmapEditor.CopySelection;
var
  b : TBitmap;
  s : TMemoryStream;
  AData : THandle;
  p : PChar;
  Size : Integer;
  r : TRect;

begin
  s := nil;
  b := TBitmap.Create;
  try
    s := TMemoryStream.Create;
    b.PixelFormat := ClipboardPixelFormat;
    b.Palette := ClipboardPalette;
    b.Width := fSelectionRect.Right - fSelectionRect.Left + 1;
    b.Height := fSelectionRect.Bottom - fSelectionRect.Top + 1;

    r := fSelectionRect;
    Inc (r.Right);
    Inc (r.Bottom);

    b.Canvas.CopyRect (rect (0, 0, b.Width, b.Height), DrawBmp.Canvas, r);

    b.SaveToStream (s);
    Size := s.Size - SizeOf (TBitmapFileHeader);

    AData := GlobalAlloc (GMEM_DDESHARE, size);
    try
      p := GlobalLock (AData);
      Move ((PChar (s.Memory) + SizeOf (TBitmapFileHeader))^, p^, size);
      GlobalUnlock (AData);
      clipboard.SetAsHandle (CF_DIB, AData);
    except
      GlobalFree (AData);
      raise
    end
  finally
    b.Free;
    s.Free;
  end
end;

constructor TBitmapEditor.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  fSelectionRect.Right := -1;
  fSelectionRect.Bottom := -1;
  fPicture := TPicture.Create;
  fMagnification := 4;
  fGridLines := 4;
  fBorderStyle := bsSingle;
  fDrawBrush := TBrush.Create;
  fDrawPen := TPen.Create;
  fEraser := TPen.Create;
  fHotSpotX := -1;
  fHotSpotY := -1;

  fDrawBmp := TBitmap.Create;

  fScratchBmp := TBitmap.Create;
  fSelectionBmp := TBitmap.Create;
  fTransparentColor := clTeal;
  Screen.Cursors [crPencil] := LoadCursor (HInstance, 'CR_PENCIL');
  Screen.Cursors [crPotOPaint] := LoadCursor (HInstance, 'CR_POTOPAINT');
  Screen.Cursors [crMagnifier] := LoadCursor (HInstance, 'CR_MAGNIFIER');
  Screen.Cursors [crDotCross] := LoadCursor (HInstance, 'CR_DOTCROSS');
  Screen.Cursors [crDropper] := LoadCursor (HInstance, 'CR_DROPPER');
  Screen.Cursors [crAirbrush] := LoadCursor (HInstance, 'CR_SPRAYGUN');
  Width := 32 * 4;
  Height := 32 * 4;
  Cursor := crArrow
end;

procedure TBitmapEditor.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams (params);
  if BorderStyle = bsSingle then
    params.Style := params.Style or WS_BORDER;
end;

procedure TBitmapEditor.CutSelection;
begin
  CopySelection;
  DeleteSelection
end;

procedure TBitmapEditor.DeleteSelection;
var
  hrgn : THandle;
  Brush : TBrush;
begin
  Brush := nil;
  hrgn := CreateRectRgn (fSelectionRect.Left, fSelectionRect.Top, fSelectionRect.Right, fSelectionRect.Bottom);
  if hrgn = 0 then
    RaiseLastOSError;

  try
    Brush := TBrush.Create;
    Brush.Color := TransparentColor;
    FillRgn (DrawBmp.Canvas.Handle, hrgn, Brush.Handle);
  finally
    Brush.Free;
    DeleteObject (hrgn)
  end;
  RedrawBitmap
end;

destructor TBitmapEditor.Destroy;
begin
  fPicture.Free;
  fDrawPen.Free;
  fDrawBrush.Free;
  fDrawBmp.Free;
  fScratchBmp.Free;
  fSelectionBmp.Free;
  fEraser.Free;
  inherited
end;

procedure TBitmapEditor.DisplayCrossHairs;
var
  pt : TPoint;
  oldColor : TColor;
  oldMode : TPenMode;
begin
  GetCursorPos (pt);
  MapWindowPoints (HWND_DESKTOP, handle, pt, 1);
  pt.x := pt.x div Magnification;
  pt.y := pt.y div Magnification;

  if (pt.x <> fCrossX) or (pt.y <> fCrossY) then
  begin
    oldColor := Canvas.Pen.Color;
    oldMode := Canvas.Pen.Mode;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := Magnification;
    Canvas.Pen.Mode := pmNotXor;

    try
      if pt.x <> fCrossX then
      begin
        fCrossX := pt.x;
        Canvas.MoveTo (fCrossX * Magnification + Magnification div 2, 0);
        Canvas.LineTo (fCrossX * Magnification + Magnification div 2, ClientHeight)
      end;
      if pt.y <> fCrossY then
      begin
        fCrossY := pt.y;
        Canvas.MoveTo (0, fCrossY * Magnification + Magnification div 2);
        Canvas.LineTo (ClientWidth, fCrossY * Magnification + Magnification div 2)
      end;
    finally
      Canvas.Pen.Color := oldColor;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Mode := oldMode;
    end
  end
end;

function TBitmapEditor.GetDrawingChangeDescription: string;
resourcestring
  rstFreeDraw       = 'freehand drawing';
  rstLine           = 'line';
  rstFrameRect      = 'frame';
  rstFillRect       = 'filled frame';
  rstRect           = 'rectangle';
  rstFloodFill      = 'flood fill';
  rstFrameRoundRect = 'rounded frame';
  rstFillRoundRect  = 'filled rounded frame';
  rstRoundRect      = 'rounded rectangle';
  rstFrameEllipse   = 'elliptical frame';
  rstFillEllipse    = 'filled ellipse';
  rstEllipse        = 'ellipse';
  rstBrushStroke    = 'brush stroke';
  rstText           = 'text';
  rstEraser         = 'eraser';
  rstAirbrush       = 'airbrush stroke';
  rstGradientRect   = 'gradient rectangle';

const
  DrawingToolDescription : array [TDrawingTool] of string = (
    '', rstFreeDraw, rstLine,
    rstFrameRect, rstFillRect, rstRect,
    rstFloodFill,
    rstFrameRoundRect, rstFillRoundRect, rstRoundRect,
    rstFrameEllipse, rstFillEllipse, rstEllipse,
    '',
    rstBrushStroke,
    '', '', '', rstText, rstEraser, rstAirbrush,
    rstGradientRect, rstGradientRect, rstGradientRect, rstGradientRect);
begin
  result := DrawingToolDescription [DrawingTool]
end;

function TBitmapEditor.GetSelectionValid: boolean;
begin
  result := (fSelectionRect.Right <> -1) and (fSelectionRect.Bottom <> -1);
end;

procedure TBitmapEditor.Initialize;
var
  r : TRect;
begin
  if Assigned (fPicture.Graphic) then
  begin
    fDrawBmp.Assign (fPicture.Graphic);
    if fPicture.Graphic is TGifImage then
      fDrawBmp.Transparent := fPicture.Graphic.Transparent;
    if fDrawBmp.Transparent then
      fTransparentColor := fDrawBmp.TransparentColor
  end
  else
  begin
    r.Left := 0;
    r.top := 0;
    r.right := 32;
    r.bottom := 32;
    fDrawBmp.TransparentColor := TransparentColor;
    fDrawBmp.Width := r.right;
    fDrawBmp.Height := r.bottom;
    fDrawBmp.Canvas.pen.Color := clWhite;
    fDrawBmp.Canvas.FillRect (r)
  end;

  ChangeSelectionRect (rect (-1, -1, -1, -1));

  ClientWidth := fDrawBmp.Width * Magnification;
  ClientHeight := fDrawBmp.Height * Magnification;

  if fMouseCaptured then
  begin
    ReleaseCapture;
    fMouseCaptured := False
  end
end;

procedure TBitmapEditor.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p : TPoint;
  st : WideString;
begin
  SetFocus;
  fCallEndChange := DrawingTool in [dtPencil..dtEllipse, dtBrush, dtText, dtEraser, dtAirbrush, dtGradRectLR, dtGradRectTB, dtGradRectTLBR, dtGradRectBLTR];
  p.x := x div magnification;
  p.y := y div magnification;
  fScratchBmp.Assign (fDrawBmp);

  if Button = mbLeft then
  begin
    fLButtonIsDown := True;
    fPos := p;

    case DrawingTool of
      dtPencil, dtLine :
        begin
          fDrawBmp.Canvas.Pixels [p.x, p.y] := fDrawPen.Color;
          RedrawBitmap
        end;
      dtFloodFill :
        begin
          with fDrawBmp.Canvas do
          begin
            Brush := fDrawBrush;
            FloodFill (p.x, p.y, Pixels [p.x, p.y], fsSurface);
          end;
          RedrawBitmap
        end;
      dtMagnifier :
        ZoomIn;

      dtDropper :
        begin
          fDrawPen.Color := fDrawBmp.Canvas.Pixels [p.x, p.y];
          DrawingTool := fLastDrawingTool;
          ReleaseCapture;
          if Assigned (fOnDrawToolChange) and not (csDestroying in ComponentState) then
            OnDrawToolChange (self);
        end;

      dtSelectRect,
      dtSelectArea :
          ChangeSelectionRect (rect (p.x, p.y, -1, -1));
      dtEraser :
        begin
          fEraser.Color := TransparentColor;
          fDrawBmp.Canvas.Pixels [p.x, p.y] := fEraser.Color;
          RedrawBitmap
        end;
      dtText :
        if Assigned (OnGetText) then
        begin
          fDrawBmp.Canvas.Font.Color := fDrawPen.Color;
          OnGetText (self, fDrawBmp.Canvas.Font, st);
          if st <> '' then
          begin
            SetBkMode (fDrawBmp.Canvas.Handle, TRANSPARENT);
            ExtTextOutW (fDrawBmp.Canvas.Handle, p.x, p.y, 0, Nil, PWideChar (st), Length (st), Nil); 
            RedrawBitmap;
            if fCallEndChange then
            begin
              fCallEndChange := False;
              Picture.Graphic.Assign (drawBmp);
              if Assigned (OnEndChange) then
                OnEndChange (self)
            end
          end
        end
    end
  end
  else
    if Button = mbRight then
      case DrawingTool of
        dtMagnifier : ZoomOut;
        dtDropper :
          begin
            fDrawBrush.Color := fDrawBmp.Canvas.Pixels [p.x, p.y];
            DrawingTool := fLastDrawingTool;
            ReleaseCapture;
            if Assigned (fOnDrawToolChange) and not (csDestroying in ComponentState) then
              OnDrawToolChange (self);
          end
        end

end;

procedure TBitmapEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  p : TPoint;
  hrgn : THandle;
  r : TRect;
  inRect : Boolean;
  gradType : TGradientShapeRectType;
begin
  p.x := x;
  p.y := y;
  ScreenToClient (p);
  if Parent is TScrollingWinControl then
  begin
    r := Parent.ClientRect;
    MapWindowPoints (Parent.Handle, handle, r, 2);
    IntersectRect (r, r, ClientRect)
  end
  else
    r := ClientRect;

  inRect := PtInRect (r, p);
  if inRect  or (ssLeft in Shift) then
  begin
    p.x := p.x div magnification;
    p.y := p.y div magnification;
    if inRect and not fMouseCaptured then
    begin
      SetCapture (handle);
      fMouseCaptured := True
    end;

    if not InRect and (DrawingTool in [dtSelectRect, dtSelectArea]) then
    begin
      if p.x >= drawBmp.Width then p.x := drawBmp.Width - 1;
      if p.y >= drawBmp.Height then p.y := drawBmp.Height - 1;
      ChangeSelectionRect (rect (-2, -2, p.x, p.y));
    end
    else
    begin
      if (p.x <> fPos.x) or (p.y <> fPos.y) then
        if FLButtonIsDown then
        begin
          case DrawingTool of
            dtPencil :
              with fDrawBmp.Canvas do
                begin
                  Pen := fDrawPen;
                  MoveTo (fPos.x, fPos.y);
                  LineTo (p.x, p.y);
                  Pixels [p.x, p.y] := fDrawPen.Color;
                  fPos := p;
                end;

            dtLine :
              begin
                fDrawBmp.Assign (fScratchBmp);
                with fDrawBmp.Canvas do
                begin
                  Pen := fDrawPen;
                  MoveTo (fPos.x, fPos.y);
                  LineTo (p.x, p.y);
                  Pixels [p.x, p.y] := fDrawPen.Color;
                end
              end;
            dtFrameRect, dtFillRect, dtRect,
            dtFrameEllipse, dtFillEllipse, dtEllipse,
            dtFrameRoundRect, dtFillRoundRect, dtRoundRect,
            dtGradRectLR, dtGradRectTB, dtGradRectTLBR, dtGradRectBLTR :
              begin
                fDrawBmp.Assign (fScratchBmp);
                case DrawingTool of
                  dtFrameRect, dtFillRect, dtRect                : hrgn := CreateRectRgn (fPos.x, fPos.y, p.x + 1, p.y + 1);
                  dtFrameEllipse, dtFillEllipse, dtEllipse       : hrgn := CreateEllipticRgn (fPos.x, fPos.y, p.x + 1, p.y + 1);
                  dtFrameRoundRect, dtFillRoundRect, dtRoundRect : hrgn := CreateRoundRectRgn (fPos.x, fPos.y, p.x + 1, p.y + 1, 5, 5);
                  else
                    hrgn := 0;
                end;
                if hrgn <> 0 then
                  with fDrawBmp.Canvas do
                  try
                    case DrawingTool of
                      dtRect,
                      dtEllipse,
                      dtRoundRect  :
                        FillRgn (handle, hrgn, DrawBrush.Handle);

                      dtFillRect,
                      dtFillEllipse,
                      dtFillRoundRect  :
                        begin
                          FillRgn (handle, hrgn, DrawBrush.Handle);
                          Brush.Color := DrawPen.Color;
                          FrameRgn (handle, hrgn, brush.Handle, 1, 1)
                        end;

                      dtFrameRect,
                      dtFrameEllipse,
                      dtFrameRoundRect :
                        begin
                          Brush.Color := DrawPen.Color;
                          FrameRgn (handle, hrgn, brush.Handle, 1, 1);
                        end
                    end
                  finally
                    DeleteObject (hrgn)
                  end
                else
                  case DrawingTool of
                    dtGradRectLR, dtGradRectTB, dtGradRectTLBR, dtGradRectBLTR :
                      begin
                        case DrawingTool of
                          dtGradRectLR : gradType := gsrLR;
                          dtGradRectTB : gradType := gsrTB;
                          dtGradRectTLBR : gradType := gsrTLBR;
                          dtGradRectBLTR : gradType := gsrBLTR;
                          else
                            gradType := gsrLR
                        end;
                        GradientRect (fDrawBmp.Canvas.Handle, gradType, fPos.x, fPos.y, p.x + 1, p.y + 1, DrawPen.Color, DrawBrush.Color);
                      end
                  end
              end;
            dtSelectRect,
            dtSelectArea :
              ChangeSelectionRect (rect (-2, -2, p.x, p.y));
            dtEraser :
              with fDrawBmp.Canvas do
              begin
                Pen := fEraser;
                MoveTo (fPos.x, fPos.y);
                LineTo (p.x, p.y);
                Pixels [p.x, p.y] := fEraser.Color;
                fPos := p;
              end;
          end;

          if not (DrawingTool in [dtSelectRect, dtSelectArea]) then
            RedrawBitmap
          else
            Invalidate
        end;

        if Cursor = crNone then
          Invalidate
      end
  end
  else
  begin  // Pt not in rect, and not LButtonDown
    ReleaseCapture;
    fMouseCaptured := False;
    Invalidate
  end
end;

procedure TBitmapEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  p : TPoint;
begin
  if button = mbLeft then
    fLButtonIsDown := False;

  p.x := x;
  p.y := y;
  ScreenToClient (p);

  if PtInRect (ClientRect, p) then
  begin
    SetCapture (handle);
    fMouseCaptured := True
  end;

  if fCallEndChange then
  begin
    fCallEndChange := False;
    Picture.Graphic.Assign (drawBmp);
    if Assigned (OnEndChange) then
      OnEndChange (self)
  end
end;

procedure TBitmapEditor.DrawHotSpot(canvas : TCanvas);
var
  hsx, hsy : Integer;
begin
  if (fHotSpotX <> -1) and (fHotSpotY <> -1) then
  begin
    hsx := fHotSpotX * Magnification;
    hsy := fHotSpotY * Magnification;
    if Magnification = 1 then
      Canvas.Pixels [hsx, hsy] := clLime
    else
    begin
      Canvas.Brush.Color := clLime;
      Canvas.FillRect (Rect (hsx, hsy, hsx+Magnification-1, hsy + Magnification-1))
    end
  end
end;

procedure TBitmapEditor.Paint;
var
  x : Integer;
  r : TRect;
  pts : array [0..4] of TPoint;
begin
  PaintBitmap (fDrawbmp);

  if GridLines <= Magnification then
  begin
    x := Magnification - 1;
    while x < Width - 1 do
    begin
      Canvas.MoveTo (x, 0);
      Canvas.LineTo (x, Height);
      Inc (x, Magnification)
    end;

    x := Magnification - 1;
    while x < Height - 1do
    begin
      Canvas.MoveTo (0, x);
      Canvas.LineTo (Width, x);
      Inc (x, Magnification)
    end
  end;

  fCrossX := -1;
  fCrossY := -1;

  if SelectionValid then
    with fSelectionRect do
    begin
      r.left := left * Magnification + Magnification div 2;
      r.top := top * Magnification + Magnification div 2;
      r.right := right * Magnification + Magnification div 2;
      r.bottom := bottom * Magnification + Magnification div 2;

      pts [0] := r.TopLeft;
      pts [1].x := r.Right;
      pts [1].y := r.Top;
      pts [2] := r.BottomRight;
      pts [3].x := r.Left;
      pts [3].y := r.Bottom;
      pts [4] := pts [0];

      Canvas.Pen.Width := Magnification;
      Canvas.Pen.Mode := pmNotXOR;
      Canvas.PolyLine (pts);
      Canvas.Pen.Width := 1;
      Canvas.Pen.Mode := pmCopy;
    end;

  if fMouseCaptured and (Cursor = crNone) and not fLButtonIsDown then
    DisplayCrossHairs;

  DrawHotspot (Canvas);
end;

procedure TBitmapEditor.PaintBitmap (bmp : TBitmap);
begin
  if bmp.Transparent then
  begin
    Canvas.Brush.Color := TransparentColor;
    Canvas.FillRect (ClientRect);
  end;
  Canvas.StretchDraw (ClientRect, bmp);
end;

procedure TBitmapEditor.PasteSelection;
var
  b : TBitmap;
  s : TMemoryStream;
  f : TBitmapFileHeader;
  Data : THandle;
  p : PChar;
  Size : Integer;
  r : TRect;
begin
  s := nil;
  b := TBitmap.Create;
  try
    data := clipboard.GetAsHandle (CF_DIB);
    if data = 0 then
      RaiseLastOSError;

    s := TMemoryStream.Create;

    FillChar(f, sizeof(f), 0);
    f.bfType := $4D42;

    s.Write (f, SizeOf (f));

    Size := GlobalSize (data);
    p := GlobalLock (data);
    try
      s.Write (p^, Size);
    finally
      GlobalUnlock (data)
    end;
    s.Seek (0, soFromBeginning);
    b.LoadFromStream (s);

    b.Palette := fClipboardPalette;
    b.PixelFormat := fClipboardPixelFormat;

    r := fSelectionRect;
    Inc (r.Right);
    Inc (r.Bottom);

    fDrawBmp.Canvas.StretchDraw (r, b);
    Invalidate;
    Picture.Graphic.Assign (drawBmp);
    if Assigned (OnEndChange) then
      OnEndChange (self)
  finally
    b.Free;
    s.Free
  end
end;

procedure TBitmapEditor.PictureChanged;
begin
  Initialize;
  Invalidate;
end;

procedure TBitmapEditor.RedrawBitmap;
begin
  Invalidate;
  if not (csDestroying in ComponentState) and Assigned (OnChange) then
    OnChange (self);
end;

procedure TBitmapEditor.Rotate180;
var
  destRect, srcRect : TRect;
begin
  with fDrawBmp do
  begin
    srcRect := Rect (0, 0, Width, Height);

      // Should technically be Width-1, Height-1,
      // But work round a bug in CopyRect...

    destRect := srcRect;
    destRect.Left := srcRect.Right;
    destRect.Top := srcRect.Bottom;
    destRect.Right := 0;
    destRect.Bottom := 0;
    Canvas.CopyRect (destRect, Canvas, srcRect);
    RedrawBitmap
  end

end;

procedure TBitmapEditor.Rotate270;
var
  newBmp : TBitmap;
begin
  fDrawBmp.PixelFormat := pf24Bit;
  newBmp := RotateBitmap270 (fDrawBmp);
  fDrawBmp.Free;

  newBmp.PixelFormat := pfDevice;
  fDrawBmp := NewBmp;
  ClientWidth := fDrawBmp.Width * Magnification;
  ClientHeight := fDrawBmp.Height * Magnification;
  RedrawBitmap
end;

procedure TBitmapEditor.Rotate90;
var
  newBmp : TBitmap;
begin
  fDrawBmp.PixelFormat := pf24Bit;
  newBmp := RotateBitmap90 (fDrawBmp);
  fDrawBmp.Free;

  newBmp.PixelFormat := pfDevice;
  fDrawBmp := NewBmp;
  ClientWidth := fDrawBmp.Width * Magnification;
  ClientHeight := fDrawBmp.Height * Magnification;
  RedrawBitmap
end;

procedure TBitmapEditor.SelectAll;
begin
  ChangeSelectionRect (rect (0, 0,drawBmp.Width - 1, drawBmp.Height - 1));
end;

procedure TBitmapEditor.SetBorderStyle(const Value: TBorderStyle);
begin
  if fBorderStyle <> value then
  begin
    fBorderStyle := Value;
    RecreateWnd
  end
end;

procedure TBitmapEditor.SetDrawBrush(const Value: TBrush);
begin
  fDrawBrush.Assign (value)
end;

procedure TBitmapEditor.SetDrawingTool(const Value: TDrawingTool);
begin
  if value <> fDrawingTool then
  begin
    fLastDrawingTool := fDrawingTool;
    fDrawingTool := Value;
    Cursor := drawingCursors [fDrawingTool];
    if SelectionValid then
      ChangeSelectionRect (rect (-1, -1, -1, -1));
  end
end;

procedure TBitmapEditor.SetDrawPen(const Value: TPen);
begin
  fDrawPen.Assign (value)
end;

procedure TBitmapEditor.SetGridLines(const Value: Integer);
begin
  if fGridLines <> value then
  begin
    fGridLines := Value;
    Invalidate
  end
end;

procedure TBitmapEditor.SetHotSpotX(const Value : Integer);
begin
  if Value <> fHotSpotX then
  begin
    fHotSpotX := Value;
    Invalidate
  end
end;

procedure TBitmapEditor.SetHotSpotY(const Value : Integer);
begin
  if Value <> fHotSpotY then
  begin
    fHotSpotY := Value;
    Invalidate
  end
end;

procedure TBitmapEditor.SetMagnification(const Value: Integer);
begin
  if fMagnification <> value then
  begin
    fMagnification := Value;
    SizeToPicture;
    Invalidate
  end
end;

procedure TBitmapEditor.SetPicture(const Value: TPicture);
begin
  fPicture.Assign (Value);
  if Value.Graphic is TGifImage then
    fPicture.Graphic.Transparent := Value.Graphic.Transparent;
  
  PictureChanged;
end;

procedure TBitmapEditor.SetTransparentColor(const Value: TColor);
begin
  if (value <> ftransparentColor) then
  begin
    fTransparentColor := value;
    Initialize;
    Invalidate;
  end
end;

procedure TBitmapEditor.SizeToPicture;
begin
  if Assigned (fDrawBmp) and (fDrawBmp.Width > 0) then
  begin
    ClientWidth := fDrawBmp.Width * Magnification;
    ClientHeight := fDrawBmp.Height * Magnification
  end
  else
  if fPicture.Width = 0 then
  begin
    ClientWidth := 32 * Magnification;
    ClientHeight := 32 * Magnification
  end
  else
  begin
    ClientWidth := fPicture.Width * Magnification;
    ClientHeight := fPicture.Height * Magnification
  end
end;

procedure TBitmapEditor.ZoomIn;
begin
  if Magnification = 1 then
    Magnification := 2
  else
    if Magnification < 32 then
      Magnification := Magnification + 2
end;

procedure TBitmapEditor.ZoomOut;
begin
  if Magnification = 2 then
    Magnification := 1
  else
    if Magnification > 1 then
      Magnification := Magnification - 2
end;

end.
