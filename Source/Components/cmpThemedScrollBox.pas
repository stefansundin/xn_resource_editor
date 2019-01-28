unit cmpThemedScrollBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, UxTheme,Themes;

type
  TThemedScrollBox = class(TScrollBox)
  private
    fUseTheme: boolean;
    fTheme : HTHEME;
    procedure DoWMNCPaint (DC : HDC);
    procedure SetUseTheme(const Value: boolean);
    procedure WMNCPaint (var msg : TwmNCPaint); message WM_NCPAINT;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
  protected
    procedure CreateWnd; override;
  public
    constructor Create (AOwner : TComponent); override;
    { Public declarations }
  published
    property UseTheme : boolean read fUseTheme write SetUseTheme default True;
  end;

implementation

var
  IsWinNT: Boolean;          // necessary to fix a bug in Win95/WinME regarding non-client area region intersection
                             // and to allow for check of system dependent hint animation
  IsWin2K: Boolean;          // nessary to provide correct string shortage
  IsWinXP: Boolean;          // necessary to paint the correct frame for the string edit

{ TThemedScrollBox }

constructor TThemedScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  fUseTheme := True;

end;

procedure TThemedScrollBox.CreateWnd;
begin
  inherited;
  if IsWinXP then
    Perform (WM_THEMECHANGED, 0, 0);
end;

procedure TThemedScrollBox.DoWMNCPaint(DC: HDC);
// Unfortunately, the VCL does a bad job regarding non-client area painting in TWinControl to paint a window's bevel
// which results often in heavy flickering. This method is a copy of TWinControl.WMNCPaintHandler adjusted to take
// the passed update region into account (actually, this happens already in the WMNCPaint).
// Since the advent of themes this method also draws the theme border.

const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);

var
  RC, RW: TRect;
  EdgeSize: Integer;
  Styles: Integer;
  HasClientEdge: Boolean;

begin
  // Determine outer rectangle to draw.
  RC := Rect(0, 0, Width, Height);
  Styles := GetWindowLong(Handle, GWL_EXSTYLE);
  HasClientEdge := (Styles and WS_EX_CLIENTEDGE) <> 0;

  // Draw control frame first
  if HasClientEdge then
  begin

    if fUseTheme and (FTheme <> 0) then
    begin
      ExcludeClipRect(DC, RC.Left + 2, RC.Top + 2, RC.Right - 2, RC.Bottom - 2);
      DrawThemeBackground(FTheme, DC, 0, 0, RC, nil);
    end;
    InflateRect(RC, -2, -2);
  end;

  if (BevelKind <> bkNone) or (BorderWidth > 0) then
  begin
    Styles := GetWindowLong(Handle, GWL_STYLE);
    if (Styles and WS_BORDER) <> 0 then
      InflateRect(RC, -1, -1);
    if (Styles and WS_THICKFRAME) <> 0 then
      InflateRect(RC, -3, -3);

    RW := RC;

    if BevelKind <> bkNone then
    begin
      DrawEdge(DC, RC, InnerStyles[BevelInner] or OuterStyles[BevelOuter], Byte(BevelEdges) or EdgeStyles[BevelKind] or
        Ctl3DStyles[Ctl3D]);

      EdgeSize := 0;
      if BevelInner <> bvNone then
        Inc(EdgeSize, BevelWidth);
      if BevelOuter <> bvNone then
        Inc(EdgeSize, BevelWidth);
      with RC do
      begin
        if beLeft in BevelEdges then
          Inc(Left, EdgeSize);
        if beTop in BevelEdges then
          Inc(Top, EdgeSize);
        if beRight in BevelEdges then
          Dec(Right, EdgeSize);
        if beBottom in BevelEdges then
          Dec(Bottom, EdgeSize);
      end;
    end;

    // Repaint only the part in the original clipping region and not yet drawn parts.
    IntersectClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);

    // Determine inner rectangle to exclude (RC corresponds then to the client area).
    InflateRect(RC, -BorderWidth, -BorderWidth);

    // Remove the inner rectangle.
    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);

    // Erase parts not drawn.
    Brush.Color := clBtnFace; // FColors.BorderColor;
    Windows.FillRect(DC, RW, Brush.Handle);
  end
end;

procedure TThemedScrollBox.SetUseTheme(const Value: boolean);
begin
  if value <> fUseTheme then
  begin
    fUseTheme := Value;
    if IsWinXP then
      Perform (WM_THEMECHANGED, 0, 0);
  end
end;

procedure TThemedScrollBox.WMNCPaint(var msg: TwmNCPaint);
var
  DC: HDC;

begin
  // Don't use the inherited NC paint handler as it doesn't consider the current clipping region
  // leading so to annoying flicker.
  // If the tree is themed then the border which is drawn by the inherited handler will be overpainted.
  // This will, at time, cause a bit flicker, but since I found nowhwere documentation about how to do it right
  // I have to live with that for the time being.
  DefaultHandler(Msg);

  dc := GetWindowDC (Handle);

  if DC <> 0 then
  begin
    DoWMNCPaint (DC);
    ReleaseDC(Handle, DC);
  end;

  Msg.Result := 0;
end;

procedure TThemedScrollBox.WMThemeChanged(var Message: TMessage);
begin
  if FTheme <> 0 then
  begin
    CloseThemeData(FTheme);
    FTheme := 0;
  end;

  if fUseTheme then
    FTheme := OpenThemeData(Handle, 'listbox');

  RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_NOERASE or RDW_NOCHILDREN);
end;

procedure InitializeGlobalStructures;
begin
  IsWinNT := (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0;
  IsWin2K := IsWinNT and (Win32MajorVersion > 4);
  isWinXP := IsWin2K and (Win32MinorVersion > 0);
end;

begin
  InitializeGlobalStructures
end.
