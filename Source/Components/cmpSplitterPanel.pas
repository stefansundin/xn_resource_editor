(*======================================================================*
 | cmpSplitterPanel                                                     |
 |                                                                      |
 | TSplitterPanel component.  A panel with the option of splitters down |
 | each edge                                                            |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/08/2003  CPWW  Original                                  |
 *======================================================================*)


unit cmpSplitterPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms;

type
  TSplitterPanelSplitter = (splLeft, splTop, splRight, splBottom);
  TSplitterPanelSplitters = set of TSplitterPanelSplitter;

  TCustomSplitterPanel = class(TCustomPanel)
  private
    FSplitters: TSplitterPanelSplitters;
    FSplitterWidth: Integer;
    FPainting: Boolean;
    FLineDC: HDC;
    FBrush: Tbrush;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FActiveControl: TWinControl;
    FOldKeyDown: TKeyEvent;
    FMaxSize: Integer;
    FLineVisible: Boolean;
    FSplitterRect: TRect;
    FDownPos: TPoint;
    FLastDownPos: TPoint;
    FCurrentSplitter: TSplitterPanelSplitter;
    FSizing: Boolean;
    FBevelled: Boolean;
    FSplitterCursorShowing: Boolean;
    FOldCursor: TCursor;
    procedure SetSplitters(const Value: TSplitterPanelSplitters);
    procedure SetSplitterWidth(const Value: Integer);

    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure InternalAdjustClientRect(var Rect: TRect);
    function FindSplitterRect(x, y: Integer; var splt: TSplitterPanelSplitter; var rr: TRect): Boolean;
    procedure AllocateLineDC;
    procedure ReleaseLineDC;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StopSizing;
    procedure DrawLine;
    procedure UpdateControlSize(x, y: Integer);
    procedure SetBevelled(const Value: Boolean);
    function GetSplitterRect(splt: TSplitterPanelSplitter; var r: TRect): Boolean;
  protected
    function GetClientRect: TRect; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure AdjustSize; override;
    procedure Paint; override;

    property Splitters: TSplitterPanelSplitters read FSplitters write SetSplitters;
    property SplitterWidth: Integer read FSplitterWidth write SetSplitterWidth;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsPattern;
    property BevelledSplitters: Boolean read FBevelled write SetBevelled;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSplitterPanel = class (TCustomSplitterPanel)
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelledSplitters;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ResizeStyle;
    property ShowHint;
    property Splitters;
    property SplitterWidth;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

type
  TWinControlAccess = class(TWinControl);

{ TCustomSplitterPanel }

procedure TCustomSplitterPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  InternalAdjustClientRect(rect);
end;

procedure TCustomSplitterPanel.AdjustSize;
begin
  if not fSizing then
    inherited;
end;

procedure TCustomSplitterPanel.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
{$IFDEF MSWINDOWS}
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
{$ENDIF}
{$IFDEF LINUX}
      FBrush.Color := clGray;
{$ENDIF}
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

constructor TCustomSplitterPanel.Create(AOwner: TComponent);
begin
  inherited;
  FResizeStyle := rsPattern
end;

procedure TCustomSplitterPanel.DrawLine;
var
  r: TRect;
begin
  FLineVisible := not FLineVisible;
  r := FSplitterRect;
  MapWindowPoints (Handle, Parent.Handle, r, 2);
  with r do
    PatBlt(FLineDC, Left, Top, Right - Left, Bottom - Top, PATINVERT);
end;

function TCustomSplitterPanel.FindSplitterRect(x, y: Integer; var splt: TSplitterPanelSplitter; var rr: TRect): Boolean;
var
  r: TRect;
  s: TSplitterPanelSplitter;
begin
  Result := False;

  for s := Low (TSplitterPanelSplitter) to High(TSplitterPanelSplitter) do
    if GetSplitterRect(s, r) and PtInRect(r, point(x, y)) then
    begin
      Result := True;
      rr := r;
      splt := s;
      break
    end
end;

procedure TCustomSplitterPanel.FocusKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

function TCustomSplitterPanel.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if FPainting then
    InternalAdjustClientRect(Result)
end;

function TCustomSplitterPanel.GetSplitterRect(splt: TSplitterPanelSplitter;
  var r: TRect): Boolean;
begin
  Result := splt in Splitters;

  case splt of
    splLeft  : r := Rect(0, 0, SplitterWidth, Height);
    splRight : r := Rect(Width - SplitterWidth, 0, Width, Height);
    splTop   : r := Rect(0, 0, Width, SplitterWidth);
    splBottom: r := Rect(0, Height - SplitterWidth, Width, Height)
  end
end;

procedure TCustomSplitterPanel.InternalAdjustClientRect(var Rect: TRect);
begin
  if splLeft in Splitters then Inc(rect.left, SplitterWidth);
  if splRight in Splitters then Dec(rect.Right, SplitterWidth);
  if splTop in Splitters then Inc(rect.top, SplitterWidth);
  if splBottom in Splitters then Dec(rect.Bottom, SplitterWidth);
end;

procedure TCustomSplitterPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
  s: TSplitterPanelSplitter;
  i: Integer;
begin
  Inherited;
  if FindSplitterRect(x, y, s, r) then
  begin

    FDownPos := Point(X, Y);
    FLastDownPos := FDownPos;
    FSplitterRect := r;
    FCurrentSplitter := s;
    FSizing := True;

    if s in [splLeft, splRight] then
    begin
      FMaxSize := Parent.ClientWidth - Constraints.MinWidth;
      for I := 0 to Parent.ControlCount - 1 do
        with Parent.Controls[I] do
          if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
      Inc(FMaxSize, Width);
    end
    else
    begin
      FMaxSize := Parent.ClientHeight - Constraints.MinHeight;
      for I := 0 to Parent.ControlCount - 1 do
        with Parent.Controls[I] do
          if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
      Inc(FMaxSize, Height);
    end;

    AllocateLineDC;

    with ValidParentForm(Self) do
      if ActiveControl <> nil then
      begin
        FActiveControl := ActiveControl;
        FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
        TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
      end;
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
  end
end;

procedure TCustomSplitterPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: Integer;
  needsRedraw: Boolean;
  r: TRect;
  s: TSplitterPanelSplitter;
begin
  inherited;
  if FSizing then
  begin
    dx := x - FLastDownPos.X;
    dy := y - FLastDownPos.Y;

    needsRedraw := False;
    case FCurrentSplitter of
      splLeft, splRight: needsRedraw := dx <> 0;
      splTop, splBottom: needsRedraw := dy <> 0
    end;

    FLastDownPos := Point(x, y);

    if needsRedraw and (ResizeStyle in [rsLine, rsPattern]) then
    begin
      DrawLine;

      case FCurrentSplitter of
        splLeft, splRight: OffsetRect(FSplitterRect, dx, 0);
        splTop, splBottom: OffsetRect(FSplitterRect, 0, dy);
      end;

      DrawLine
    end
    else
      if ResizeStyle = rsUpdate then
        UpdateControlSize(x, y);
  end
  else
  begin
    if FindSplitterRect(x, y, s, r) then
    begin
      if not FSplitterCursorShowing then
      begin
        FOldCursor := Cursor;
        if (s = splLeft) or (s = splRight) then
          Cursor := crHSplit
        else
          Cursor := crVSplit;
        FSplitterCursorShowing := True
      end
    end
    else
      if FSplitterCursorShowing then
      begin
        Cursor := FOldCursor;
        FSplitterCursorShowing := False
      end
  end
end;

procedure TCustomSplitterPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ResizeStyle in [rsLine, rsPattern] then DrawLine;
  if FSizing then
  begin
    UpdateControlSize(x, y);
    StopSizing
  end
end;

procedure TCustomSplitterPanel.Paint;
var
  r: TRect;
  s: TSplitterPanelSplitter;

  procedure PaintSplitter (rect: TRect);
  var
    oldColor: TColor;
  begin
    oldColor := canvas.Brush.Color;
    canvas.Brush.Color := Color;
    Canvas.FillRect(rect);
    canvas.Brush.Color := oldColor;
    if FBevelled then
      Frame3D (Canvas, rect, clBtnShadow, clBtnHighlight, 1);
  end;

begin
  FPainting := True;
  try

    for s := Low (TSplitterPanelSplitter) to High(TSplitterPanelSplitter) do
      if GetSplitterRect(s, r) then
        PaintSplitter (r);
    inherited;
  finally
    FPainting := False
  end
end;

procedure TCustomSplitterPanel.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end
end;

procedure TCustomSplitterPanel.SetBevelled(const Value: Boolean);
begin
  if value <> FBevelled then
  begin
    FBevelled := value;
    Realign;
    Invalidate
  end
end;

procedure TCustomSplitterPanel.SetSplitters(
  const Value: TSplitterPanelSplitters);
begin
  if value <> FSplitters then
  begin
    FSplitters := value;
    Realign;
    Invalidate
  end
end;

procedure TCustomSplitterPanel.SetSplitterWidth(const Value: Integer);
begin
  if value <> FSplitterWidth then
  begin
    FSplitterWidth := Value;
    Realign;
    Invalidate
  end
end;

procedure TCustomSplitterPanel.StopSizing;
begin
  if FLineVisible then DrawLine;
  ReleaseLineDC;
  if Assigned(FActiveControl) then
  begin
    TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
    FActiveControl := nil;
  end;
  FSizing := False;
end;

procedure TCustomSplitterPanel.UpdateControlSize(x, y: Integer);
var
  dx, dy: Integer;
  oldAutoSize: Boolean;
begin
  oldAutoSize := AutoSize;
  AutoSize := False;
  try
    dx := x - FDownPos.X;
    dy := y - FDownPos.Y;

    case FCurrentSplitter of
      splRight: Width := Width + dx;
      splLeft :
        begin
          Left := Left - dx;
          Width := Width - dx
        end;
      splTop :
        begin
          Top := Top - dy;
          Height := Height - dy
        end;
      splBottom: Height := Height + dy
    end;

    FDownPos := point(x, y);
  finally
    AutoSize := oldAutoSize
  end
end;

procedure TCustomSplitterPanel.WMWindowPosChanged(
  var Message: TWMWindowPosChanged);
var
  r: TRect;
  s: TSplitterPanelSplitter;

begin
  inherited;

  if (Splitters <> []) and (not FullRepaint) and (Caption = '') then
  begin
    for s := Low (TSplitterPanelSplitter) to High(TSplitterPanelSplitter) do
      if GetSplitterRect(s, r) then
        InvalidateRect(Handle, @r, True)
  end
end;

end.
