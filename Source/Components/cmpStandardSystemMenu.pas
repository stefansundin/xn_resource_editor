(*======================================================================*
 | cmpStandardSystemMenu unit for MiscUnits package                     |
 |                                                                      |
 | Drop one of these on your application's main form, and you get all   |
 | five items in the task bar icon menu (Restore, Move, Size, Minimize, |
 | Maximize Close) instead of the measly three items that Windows gives |
 | you by default                                                       |
 |                                                                      |
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
 | Copyright © Colin Wilson 2002.  All Rights Reserved                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      2001        CPWW  Original                                  |
 | 1.1      26/02/2002  CPWW  Fixed design-time problems                |
 *======================================================================*)

unit cmpStandardSystemMenu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus;

const
  scxRESTORE  = $fff0;
  scxMINIMIZE = SC_MINIMIZE;
  scxMAXIMIZE = $fff3;
  scxSIZE     = $fff4;
  scxMOVE     = $fff5;

type
//---------------------------------------------------------------------------
// TStandardSystemMenu class
  TStandardSystemMenu = class(TComponent)
  private
    FMenuHandle: HMenu;
    FWindowMenuHandle: HMenu;
    FObjectInstance: Pointer;
    FOldOwnerWindowProc: TFNWndProc;

    FSysObjectInstance: Pointer;
    FOldSysWindowProc: TFNWndProc;

    FIconic: Boolean;
    FMaximized: Boolean;

    procedure CloneSystemMenu;
    procedure OwnerWindowProc(var msg: TMessage);
    procedure SysOwnerWindowProc(var msg: TMessage);

    procedure OnMinimized;
    procedure OnMaximized;
    procedure OnRestored (resetmax: Boolean);

    function HookProc (var Msg: TMessage): Boolean;

  protected
    procedure Loaded; override;
  public
    procedure SetItemState (itemID, state: Integer);
    destructor Destroy; override;
  end;

implementation

{ TStandardSystemMenu }

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.CloneSystemMenu                                  |
 |                                                                      |
 | Make the (hidden) application's system menu a copy of the main       |
 | form's system menu.                                                  |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.CloneSystemMenu;
var
  count: Integer;
  item: TMenuItemInfo;
  buffer: array [0..256] of char;
  i: Integer;
begin
  count := GetMenuItemCount (FMenuHandle);      // Delete all application's system
  while count > 0 do                            // menu items.
  begin
    DeleteMenu (FMenuHandle, 0, MF_BYPOSITION);
    Dec (count)
  end;

  count := GetMenuItemCount (FWindowMenuHandle);

                                                // Now copy entries from the main form's
                                                // system menu to the application's system menu
  for i := 0 to count - 1 do
  begin
    FillChar (item, sizeof (item), 0);

    if (Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and (Win32MinorVersion >= 10)) then               // Ie Win2K or '98
    begin
      item.cbSize := sizeof (item);
      item.fMask := MIIM_STATE or MIIM_BITMAP or MIIM_ID or MIIM_STRING or MIIM_FTYPE;
    end
    else
    begin
      item.cbSize := 44;                        // Sizeof old-style MENUITEMINFO
      item.fMask := MIIM_STATE or MIIM_ID or MIIM_TYPE;
    end;
    item.cch := sizeof (buffer);
    item.dwTypeData := buffer;
                                                // Get details from window system menu
    if GetMenuItemInfo (FWindowMenuHandle, i, True, item) then
    begin
      case item.wID of
        SC_RESTORE: item.wID := scxRestore;
        SC_MINIMIZE: item.wID := scxMinimize;
        SC_MAXIMIZE: item.wID := scxMaximize;
        SC_MOVE   : item.wID := scxMove;
        SC_SIZE   : item.wID := scxSize;
      end;
                                                // Add item to application system menu.
      InsertMenuItem (FMenuHandle, i, True, item)
    end
    else
      RaiseLastOSError
  end
end;

(*----------------------------------------------------------------------*
 | destructor TStandardSystemMenu.Destroy                               |
 |                                                                      |
 | Tidy up                                                              |
 *----------------------------------------------------------------------*)
destructor TStandardSystemMenu.Destroy;
begin
  if Assigned (FObjectInstance) then
    Classes.FreeObjectInstance (FObjectInstance);

  if Assigned (FSysObjectInstance) then
    Classes.FreeObjectInstance (FSysObjectInstance);

  inherited;
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.HookProc                                         |
 |                                                                      |
 | Intercept WM_WINDOWPASCHANGING messages
 *----------------------------------------------------------------------*)
function TStandardSystemMenu.HookProc(var Msg: TMessage): Boolean;
var
  LocalFlags: word;
begin
  Result := false;
  if Msg.Msg = WM_WindowPosChanging then
  begin
    with TWMWindowPosMsg(Msg).WindowPos^ do
    begin
      if (hWnd = Application.Handle) and
         not IsIconic(hWnd)          and
         (cx > 0) and (cy > 0)       then
      begin
        LocalFlags := flags or SWP_NoZOrder;
        if TForm (Owner).BorderStyle = bsSizeable then
          LocalFlags := LocalFlags and not SWP_NoSize
        else
          LocalFlags := LocalFlags or SWP_NoSize;
        SetWindowPos(TForm (Owner).Handle, 0, x, y, cx, cy, LocalFlags);
        TForm (Owner).Invalidate
      end
    end
  end
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.Loaded                                           |
 |                                                                      |
 | Subclass the main form and the hidden application window             |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FMenuHandle := GetSystemMenu (Application.Handle, False);
    FWindowMenuHandle := GetSystemMenu ((Owner as TForm).Handle, False);
    CloneSystemMenu;

    FObjectInstance := Classes.MakeObjectInstance (OwnerWindowProc);
    FOldOwnerWindowProc := TfnWndProc (SetWindowLong (TForm (Owner).Handle, GWL_WNDPROC, Integer (FObjectInstance)));

    FSysObjectInstance := Classes.MakeObjectInstance (SysOwnerWindowProc);
    FOldSysWindowProc := TfnWndProc (SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (FSysObjectInstance)));

    Application.HookMainWindow(HookProc);
  end
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.OnMaximized                                      |
 |                                                                      |
 | Main window maximized.  Set the menu item states to reflect this.    |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.OnMaximized;
begin
  FIconic := False;
  FMaximized := True;
  SetItemState (scxMinimize, MFS_ENABLED);
  SetItemState (scxMaximize, MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxMove,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxSize,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxRestore,  MFS_ENABLED);
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.OnMinimized                                      |
 |                                                                      |
 | Main window minimized.  Set the menu item states to reflect this.    |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.OnMinimized;
begin
  FIconic := True;
  SetItemState (scxMinimize, MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxMaximize, MFS_ENABLED);
  SetItemState (scxMove,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxSize,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxRestore,  MFS_ENABLED);
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.OnRestored                                       |
 |                                                                      |
 | Main window restored.  Set the menu item states to reflect this.     |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.OnRestored (resetmax: Boolean);
begin
  FIconic := False;
  if resetmax then FMaximized := False;
  if FMaximized then
    OnMaximized
  else
  begin
    SetItemState (scxMinimize, MFS_ENABLED);
    SetItemState (scxMaximize, MFS_ENABLED);
    SetItemState (scxMove,     MFS_ENABLED);
    SetItemState (scxSize,     MFS_ENABLED);
    SetItemState (scxRestore,  MFS_DISABLED or MFS_GRAYED)
  end
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.OwnerWindowProc                                  |
 |                                                                      |
 | Grab messages sent to the main form                                  |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.OwnerWindowProc (var msg: TMessage);
begin
  with msg do
  begin
    if msg = WM_SIZE then       // Window sized.  Set menu item states.
    begin
      case wParam of
        SIZE_MAXIMIZED: OnMaximized;
        SIZE_MINIMIZED: OnMinimized;
        SIZE_RESTORED: OnRestored (true)
      end
    end
    else
      if msg = WM_DESTROY then  // Window destroyed - unsubclass
      begin
        SetWindowLong (TForm (Owner).Handle, GWL_WNDPROC, Integer (FOldOwnerWindowProc));
        SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (FOldSysWindowProc));
        Application.UnHookMainWindow(HookProc);
      end;
    Result := CallWindowProc (FOldOwnerWindowProc, TForm (Owner).Handle, msg, wParam, lParam)
  end
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.SetItemState                                     |
 |                                                                      |
 | Set the required menu item state                                     |
 |                                                                      |
 | Parameters:                                                          |
 |   itemID: Integer           The id of the item to adjust            |
 |   state: Integer             The new state                           |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.SetItemState(itemID, state: Integer);
var
  item: TMenuItemInfo;
begin
  FillChar (item, SizeOf (item), 0);
  item.cbSize := 44;
  item.fMask := MIIM_STATE;
  if GetMenuItemInfo (FMenuHandle, itemID, False, item) then
  begin
    item.fState := state;
    SetMenuItemInfo (FMenuHandle, itemID, False, item)
  end
end;

(*----------------------------------------------------------------------*
 | TStandardSystemMenu.SysOwnerWindowProc                               |
 |                                                                      |
 | Intercept messages to the (hidden) application window                |
 *----------------------------------------------------------------------*)
procedure TStandardSystemMenu.SysOwnerWindowProc(var msg: TMessage);
var
  m: Integer;
begin
  with msg do
  begin
    if msg = WM_SYSCOMMAND then
    begin
      m := -1;
      case wParam of
        scxRestore: m := SC_RESTORE;
        scxMinimize: m := SC_MINIMIZE;
        scxMaximize: if FMaximized then  // It's also minimized, but it *was* maximized so restore!
                        SendMessage (Application.Handle, WM_SYSCOMMAND, SC_RESTORE, lParam)
                      else
                      begin
                        if FIconic then
                          SendMessage (Application.Handle, WM_SYSCOMMAND, SC_RESTORE, lParam);
                        SendMessage (TForm (owner).Handle, WM_SYSCOMMAND, SC_MAXIMIZE, lParam);
                      end;

        scxMove   : m := SC_MOVE;
        scxSize   : m := SC_SIZE;
      end;

      if m <> -1 then
        if FIconic then
          SendMessage (Application.Handle, WM_SYSCOMMAND, m, lParam)
        else
          SendMessage (TForm (owner).Handle, WM_SYSCOMMAND, m, lParam);
    end
    else
      if msg = WM_SIZE then
        case wParam of
          SIZE_MAXIMIZED:
            OnMaximized;
          SIZE_MINIMIZED:
            OnMinimized;
          SIZE_RESTORED:
            OnRestored (false)
        end;

    Result := CallWindowProc (FOldSysWindowProc, Application.Handle, msg, wParam, lParam);
  end
end;

end.
