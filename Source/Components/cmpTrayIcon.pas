unit cmpTrayIcon;

(*======================================================================*
 | cmpTrayIcon                                                          |
 |                                                                      |
 |   Tray icon component.  Copyright(c) Colin Wilson 1997-2002         |
 |                                                                      |
 | NB.  To prevent your main form displaying at startup, select         |
 |      View / Project source from the menu and insert the lines        |
 |                                                                      |
 |   ShowWindow(Application.Handle, SW_HIDE);                           |
 |   Application.ShowMainForm := FALSE;                                 |
 |                                                                      |
 |      before the Application.Run line.                                |
 | NNTP Services for News Reader 3                                      |
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
 *======================================================================*)


{$S-,W-,R-}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI, Menus, SyncObjs;

type
  TTrayIcon = class;

  TTaskbarCreatedEvent = procedure(Sender: TObject; var RedoIcon: Boolean) of object;
  TXPFastUserSwitchEvent = procedure(Sender: TObject; StatusCode, SessionID: DWORD) of object;

  TTrayIcon = class(TComponent)
  private
    FWindowHandle: HWND;
    FIcon: TIcon;
    FEnabled: Boolean;
    FIconThere: Boolean;
    FHint: string;
    FPopupMenu: TPopupMenu;
    FAutoShow: Boolean;
    FObjectInstance: pointer;
    FOldMainWProc: pointer;

    FOnLeftBtnClick: TNotifyEvent;
    FOnLeftBtnDblClick: TNotifyEvent;
    FOnRightBtnClick: TNotifyEvent;
    FOnMouseMove: TNotifyEvent;
    FOnEndSession: TNotifyEvent;
    FOnTaskbarCreated: TTaskbarCreatedEvent;
    FOnXPFastUserSwitch: TXPFastUserSwitchEvent;

    procedure WProc(var Msg: TMessage);
    procedure MainWProc (var Msg: TMessage);
    procedure UpdateIcon (Flags: integer);

    procedure SetIcon (Value: TIcon);
    procedure SetEnabled (Value: Boolean);
    procedure SetHint(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RedoIcon;
    procedure SetIconHandle(Handle: HICON);
  published
    property Icon: TIcon read FIcon write SetIcon;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Hint: string read FHint write SetHint;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property AutoShow: Boolean read FAutoShow write FAutoShow;

    property OnLeftBtnClick: TNotifyEvent read FOnLeftBtnClick write FOnLeftBtnClick;
    property OnLeftBtnDblClick: TNotifyEvent read FOnLeftBtnDblClick write FOnLeftBtnDblClick;
    property OnRightBtnClick: TNotifyEvent read FOnRightBtnClick write FOnRightBtnClick;
    property OnMouseMove: TNotifyEvent read FOnMouseMove write FOnMouseMove;
    property OnEndSession: TNotifyEvent read FOnEndSession write FOnEndSession;
    property OnTaskbarCreated: TTaskbarCreatedEvent read FOnTaskbarCreated write FOnTaskbarCreated;
    property OnXPFastUserSwitch: TXPFastUserSwitchEvent read FOnXPFastUserSwitch write FOnXPFastUserSwitch;

  end;

implementation

uses
  WTSAPI32;

const WM_ICONMESSAGE = WM_USER + $200;
var WM_TASKBARCREATED: DWORD;

constructor TTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIcon := TIcon.Create;
  FIcon.Assign (Application.Icon);

  WM_TASKBARCREATED := RegisterWindowMessage('TaskbarCreated');

  if not (csDesigning in ComponentState) then
  begin

    FObjectInstance := Classes.MakeObjectInstance(MainWProc);
    FOldMainWProc := Pointer (SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (FObjectInstance)));

    FWindowHandle := Classes.AllocateHWND (WProc);


    if WTSAPIOk then
      WTSRegisterSessionNotification (Application.Handle, NOTIFY_FOR_ALL_SESSIONS);
    FAutoShow := True;

  end
end;

destructor TTrayIcon.Destroy;
begin
  if FIconThere then
  begin
    FIcon := nil;
    UpdateIcon (0);
  end;

  if WTSAPIOk then
    WTSUnRegisterSessionNotification (Application.Handle); 

  if Assigned(FObjectInstance) then
  begin
    SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (FOldMainWProc));
    Classes.FreeObjectInstance(FObjectInstance)
  end;

  if FWindowHandle <> 0 then
    Classes.DeallocateHWND (FWindowHandle);
  FIcon.Free;
  inherited
end;

procedure TTrayIcon.WProc(var Msg: TMessage);
var
  pt: TPoint;
begin
  with msg do
    if msg = WM_ICONMESSAGE then
      case lParam of
        WM_RBUTTONDOWN :
          begin
            if Assigned(FOnRightBtnClick) then
            	OnRightBtnClick(Self);

            if Assigned(FPopupMenu) then
            begin
              GetCursorPos (pt);
              if AutoShow then FPopupMenu.Items[0].default := True;
              SetForegroundWindow (PopupList.Window);
              FPopupMenu.Popup (pt.x, pt.y);
              PostMessage(PopupList.Window, WM_NULL, 0, 0);
            end
          end;

        WM_LBUTTONDOWN :
          if Assigned(FOnLeftBtnClick) then
            OnLeftBtnClick(Self);

        WM_LBUTTONDBLCLK :
        begin
          if Assigned(FOnLeftBtnDblClick) then
            OnLeftBtnDblClick(Self);

          if Assigned(FPopupMenu) and AutoShow then with FPopupMenu do
            if Assigned(items[0]) then
              items[0].click
        end;

        WM_MOUSEMOVE :
          if Assigned(FOnMouseMove) then
            OnMouseMove(Self);
      end
    else
      if msg = WM_QUERYENDSESSION then
        Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam)
end;

procedure TTrayIcon.UpdateIcon (flags: Integer);
var
  iconData: TNotifyIconData;
begin
  if not (csDesigning in ComponentState) then
  begin
    iconData.cbSize := SizeOf(iconData);
    iconData.Wnd := FWindowHandle;
    iconData.uID := Tag;
    iconData.uFlags :=NIF_ICON or NIF_TIP or NIF_MESSAGE;
    iconData.uCallbackMessage := WM_ICONMESSAGE;
    StrPCopy(iconData.szTip, FHint);

    if Assigned(FIcon) and FEnabled then
    begin
      iconData.hIcon := FIcon.Handle;
      if FIconThere then
      begin
    	iconData.uFlags := flags;
        Shell_NotifyIcon (NIM_MODIFY, @iconData)
      end
      else
      begin
	Shell_NotifyIcon (NIM_ADD, @iconData);
        FIconThere := True
      end
    end
    else
    begin
      Shell_NotifyIcon (NIM_DELETE, @iconData);
      FIconThere := False
    end
  end
end;

procedure TTrayIcon.SetIcon (Value: TIcon);
begin
  if FIcon <> Value then
  begin
    FIcon.Assign (Value);
    UpdateIcon (NIF_ICON);
  end
end;

procedure TTrayIcon.SetHint(Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    UpdateIcon (NIF_TIP);
  end
end;

procedure TTrayIcon.SetEnabled (Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateIcon (NIF_ICON or NIF_TIP or NIF_MESSAGE);
  end
end;

procedure TTrayIcon.RedoIcon;
begin
  FIconThere := False;
  UpdateIcon (NIF_ICON or NIF_TIP or NIF_MESSAGE);
end;

procedure TTrayIcon.MainWProc(var Msg: TMessage);
var
  redoI: Boolean;
begin
  if Msg.Msg = WM_TASKBARCREATED then
  begin
    redoI := True;
    if Assigned(OnTaskbarCreated) then
      OnTaskbarCreated (Self, redoI);
    if redoI then
      RedoIcon
  end
  else
    if Msg.Msg = WM_QUERYENDSESSION then
    begin
      if Assigned(OnEndSession) then
        OnEndSession (Self)
    end
    else
      if Msg.Msg = WM_WTSSESSION_CHANGE then
      begin
        if Assigned(OnXPFastUserSwitch) then
          OnXPFastUserSwitch(Self, Msg.WParam, Msg.LParam)
      end;

  Msg.Result := CallWindowProc (FOldMainWProc, Application.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TTrayIcon.SetIconHandle(handle: HICON);
begin
  if FIcon.Handle <> handle then
  begin
    FIcon.Handle := handle;
    UpdateIcon (NIF_ICON);
  end
end;

end.
