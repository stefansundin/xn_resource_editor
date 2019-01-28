unit cmpSuDoku;

interface

uses Windows, Messages, Classes, SysUtils, Controls, Graphics, StdCtrls;

type
TSuDokuData = array [0..8, 0..8] of Integer;
TCustomSuDoku = class (TCustomControl)
private
  fGridXCoords : array [0..9] of Integer;
  fGridYCoords : array [0..9] of Integer;
  fGotGridCoords : boolean;

  fData : TSuDokuData;
  fCellColors : TSuDokuData;

  fSelX : Integer;
  fSelY : Integer;
  fOnSelect: TNotifyEvent;
  fUpdateCount : Integer;
  fAutoFontSize: boolean;
  fOrigFontSize : Integer;

  procedure CalcGridCoords (const r : TRect);

  procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;

  procedure DoOnSelect;

  function GetValue(x, y: Integer): Integer;
  procedure SetValue(x, y: Integer; const Value: Integer);
  procedure SetAutoFontSize(const Value: boolean);
  function GetSquareColor(x, y : Integer): Integer;
  procedure SetSquareColor(x, y : Integer; const Value: Integer);
protected
  property AutoFontSize : boolean read fAutoFontSize write SetAutoFontSize default True;
  procedure Loaded; override;
  procedure Paint; override;
  procedure ReDisplay;
  procedure Resize; override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  property OnSelect : TNotifyEvent read fOnSelect write fOnSelect;
public
  constructor Create (AOwner : TComponent); override;
  procedure BeginUpdate;
  procedure Clear;
  procedure EndUpdate;
  procedure SelectSquare (x, y : Integer);
  procedure UnselectAll;
  property SelX : Integer read fSelX;
  property SelY : Integer read fSelY;
  property Value [x, y : Integer] : Integer read GetValue write SetValue;
  property SquareColor [x, y : Integer] : Integer read GetSquareColor write SetSquareColor;
end;

TSuDoku = class (TCustomSuDoku)
published
  property Align;
  property Anchors;
  property BevelEdges;
  property BevelInner default bvLowered;
  property BevelKind default bkTile;
  property BevelOuter default bvLowered;
  property BiDiMode;
  property Caption;
  property Color;
  property Constraints;
  property DragCursor;
  property DragKind;
  property DragMode;
  property Enabled;
  property Font;
  property ParentBiDiMode;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ShowHint;
  property TabOrder;
  property TabStop default True;
  property Visible;
  property OnClick;
  property OnContextPopup;
  property OnDragDrop;
  property OnDragOver;
  property OnEndDock;
  property OnEndDrag;
  property OnEnter;
  property OnExit;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
  property OnMouseDown;
  property OnMouseMove;
  property OnMouseUp;
  property OnSelect;
  property OnStartDock;
  property OnStartDrag;
end;

implementation

{ TCustomSuDoku }

procedure TCustomSuDoku.BeginUpdate;
begin
  Inc (fUpdateCount);
end;

procedure TCustomSuDoku.CalcGridCoords (const r : TRect);
var
  i, w, h : Integer;
begin
  fGotGridCoords := True;
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;
  fGridXCoords [0] := 0;

  for i := 1 to 8 do
  begin
    fGridXCoords [i] := (w * i) div 9;
    fGridYCoords [i] := (h * i) div 9;
  end;

  fGridXCoords [9] := w;
  fGridYCoords [9] := h;

  Canvas.Pen.Width := 1;
  Canvas.Pen.Mode := pmCopy;
  Canvas.Pen.Color := clBtnHighlight;

  if fAutoFontSize then
    Canvas.Font.Height := fGridYCoords [1]
  else
    Canvas.Font.Height := fOrigFontSize;
end;

procedure TCustomSuDoku.Clear;
var
  x, y : Integer;
begin
  for x := 0 to 8 do
    for y := 0 to 8 do
    begin
      fData [x, y] := 0;
      fCellColors [x, y] := -1
    end;
  Redisplay
end;

procedure TCustomSuDoku.CMColorChanged(var Message: TMessage);
begin
  if Canvas <> nil then
  begin
    Canvas.Brush.Color := Color;
    Invalidate
  end
end;

procedure TCustomSuDoku.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font := Self.Font;
  Invalidate
end;

var
  defaultData : TSuDokuData = (
   (0, 0, 5, 4, 0, 0, 6, 0, 0),
   (0, 0, 0, 0, 0, 2, 0, 0, 0),
   (9, 0, 7, 0, 6, 0, 4, 0, 1),
   (0, 7, 0, 2, 0, 8, 0, 0, 5),
   (0, 0, 8, 0, 0, 0, 1, 0, 0),
   (4, 0, 0, 3, 0, 1, 0, 8, 0),
   (8, 0, 2, 0, 5, 0, 9, 0, 6),
   (0, 0, 0, 8, 0, 0, 0, 0, 0),
   (0, 0, 4, 0, 0, 7, 8, 0, 0));

constructor TCustomSuDoku.Create(AOwner: TComponent);
var
  x, y : Integer;
begin
  inherited;
  Width := 300;
  Height := 300;
  fSelX := -1;
  fSelY := -1;
  BevelKind := bkTile;
  BevelInner := bvLowered;
  BevelOuter := bvLowered;
  fAutoFontSize := True;
  DoubleBuffered := True;

  if csDesigning in ComponentState then
    for y := 0 to 8 do
      for x := 0 to 8 do
        fData [x, y] := defaultData [y, x];

  for y := 0 to 8 do
    for x := 0 to 8 do
      fCellColors [x, y] := -1;
end;

procedure TCustomSuDoku.DoOnSelect;
begin
  if Assigned (OnSelect) then
    OnSelect (self)
end;

procedure TCustomSuDoku.EndUpdate;
begin
  Dec (fUpdateCount);
  if fUpdateCount = 0 then
    Invalidate
end;

function TCustomSuDoku.GetSquareColor(x, y : Integer): Integer;
begin
  result := fCellColors [x, y]

end;

function TCustomSuDoku.GetValue(x, y: Integer): Integer;
begin
  result := fData [x, y];
end;

procedure TCustomSuDoku.Loaded;
begin
  inherited;

  fOrigFontSize := Font.Height;
end;

procedure TCustomSuDoku.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i, sx, sy : Integer;

begin
  inherited;

  sx := -1;
  sy := -1;
  for i := 0 to 8 do
  begin
    if (sx = -1) and (x < fGridXCoords [i+1]) then
      sx := i;
    if (sy = -1) and (y < fGridYCoords [i+1]) then
      sy := i;
  end;

  if (sx <> -1) and (sy <> -1) then
  begin
    SelectSquare (sx, sy);
    DoOnSelect
  end
end;

procedure TCustomSuDoku.Paint;
var
  r, r1 : TRect;
  i, x, y : Integer;
  clr : TColor;
begin
  Inherited;
  Canvas.FillRect(ClientRect);

  r := ClientRect;
  if not fGotGridCoords then
    CalcGridCoords (r);

  for i := 1 to 8 do
  begin
    if not ((i = 3) or (i = 6)) then
    begin
      Canvas.MoveTo(fGridXCoords [i], r.Top - 2);
      Canvas.LineTo(fGridXCoords [i], r.Bottom + 2);
      Canvas.MoveTo(r.Left - 2, fGridYCoords [i]);
      Canvas.LineTo(r.Right + 2, fGridYCoords [i]);
    end
  end;

  for i := 1 to 8 do
  begin
    if (i = 3) or (i = 6) then
    begin
      r1 := Rect (fGridXCoords [i] - 2, r.Top - 1, fGridXCoords [i] + 2, r.Bottom + 2);
      DrawEdge (Canvas.Handle, r1, EDGE_SUNKEN, BF_RECT);
      r1 := Rect (r.Left - 2, fGridYCoords [i] - 1, r.Right + 2, fGridYCoords [i] + 2);
      DrawEdge (Canvas.Handle, r1, EDGE_SUNKEN, BF_RECT)
    end
  end;

  for x := 0 to 8 do
    for y := 0 to 8 do
    begin
      i := fData [x, y];
      if i > 0 then
      begin
        r.Left := fGridXCoords [x];
        r.Top := fGridYCoords [y];
        r.Right := r.Left + fGridXCoords [1];
        r.Bottom := r.Top + fGridYCoords [1];
        SetBkMode (Canvas.Handle, TRANSPARENT);
        clr := fCellColors [x, y];
        if clr <> -1 then
          Canvas.Font.Color := clr;
        DrawText (canvas.Handle, PChar (IntToStr (i)), 1, r, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
        if clr <> -1 then
          Canvas.Font.Color := Font.Color
      end;

      if (x = fSelX) and (y = fSelY) then
      begin
        r.Left := fGridXCoords [x];
        r.Top := fGridYCoords [y];
        r.Right := r.Left + fGridXCoords [1];
        r.Bottom := r.Top + fGridYCoords [1];
        InflateRect (r, -1, -1);
        Inc (r.Bottom);
        Inc (r.Right);
        DrawEdge (Canvas.Handle, r, EDGE_ETCHED, BF_RECT);
      end
    end
end;

procedure TCustomSuDoku.ReDisplay;
begin
  if fUpdateCount = 0 then
    Invalidate
end;

procedure TCustomSuDoku.Resize;
begin
  inherited;
  fGotGridCoords := False;
end;

procedure TCustomSuDoku.SelectSquare(x, y: Integer);
begin
  fSelX := x;
  fSelY := y;
  ReDisplay
end;

procedure TCustomSuDoku.SetAutoFontSize(const Value: boolean);
begin
  if value <> fAutoFontSize then
  begin
    fAutoFontSize := value;
    fGotGridCoords := False;
    ReDisplay
  end
end;

procedure TCustomSuDoku.SetSquareColor(x, y : Integer; const Value: Integer);
begin
  fCellColors [x, y] := Value;
  ReDisplay
end;

procedure TCustomSuDoku.SetValue(x, y: Integer; const Value: Integer);
begin
  fData [x, y] := Value;
  ReDisplay;
end;

procedure TCustomSuDoku.UnselectAll;
begin
  fSelX := -1;
  fSelY := -1;
  ReDisplay
end;

procedure TCustomSuDoku.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;

end;

end.