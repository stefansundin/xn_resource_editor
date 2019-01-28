unit cmpTrayIcon;

(*======================================================================*
 | cmpTrayIcon                                                          |
 |                                                                      |
 |   Tray icon component.  Copyright (c) Colin Wilson 1997-2002         |
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI, Menus, SyncObjs;

type
  TTrayIcon = class;

  TTaskbarCreatedEvent = procedure (sender : TObject; var RedoIcon : boolean) of object;
  TXPFastUserSwitchEvent = procedure (sender : TObject; StatusCode, SessionID : DWORD) of object;

  TTrayIcon = class(TComponent)
  private
    fWindowHandle : HWND;
    fIcon : TIcon;
    fEnabled : boolean;
    fIconThere : boolean;
    fHint : string;
    fPopupMenu : TPopupMenu;
    fAutoShow : boolean;
    fObjectInstance : pointer;
    fOldMainWProc : pointer;

    fOnLeftBtnClick : TNotifyEvent;
    fOnLeftBtnDblClick : TNotifyEvent;
    fOnRightBtnClick : TNotifyEvent;
    fOnMouseMove : TNotifyEvent;
    fOnEndSession: TNotifyEvent;
    fOnTaskbarCreated: TTaskbarCreatedEvent;
    fOnXPFastUserSwitch: TXPFastUserSwitchEvent;

    procedure WProc(var Msg: TMessage);
    procedure MainWProc (var Msg : TMessage);
    procedure UpdateIcon (Flags : integer);

    procedure SetIcon (value : TIcon);
    procedure SetEnabled (value : boolean);
    procedure SetHint (value : string);
  protected
    { Protected declarations }
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RedoIcon;
    procedure SetIconHandle (handle : HICON);
  published
    property Icon : TIcon read fIcon write SetIcon;
    property Enabled : boolean read fEnabled write SetEnabled;
    property Hint : string read fHint write SetHint;
    property PopupMenu : TPopupMenu read fPopupMenu write fPopupMenu;
    property AutoShow : boolean read fAutoShow write fAutoShow;

    property OnLeftBtnClick : TNotifyEvent read fOnLeftBtnClick write fOnLeftBtnClick;
    property OnLeftBtnDblClick : TNotifyEvent read fOnLeftBtnDblClick write fOnLeftBtnDblClick;
    property OnRightBtnClick : TNotifyEvent read fOnRightBtnClick write fOnRightBtnClick;
    property OnMouseMove : TNotifyEvent read fOnMouseMove write fOnMouseMove;
    property OnEndSession : TNotifyEvent read fOnEndSession write fOnEndSession;
    property OnTaskbarCreated : TTaskbarCreatedEvent read fOnTaskbarCreated write fOnTaskbarCreated;
    property OnXPFastUserSwitch : TXPFastUserSwitchEvent read fOnXPFastUserSwitch write fOnXPFastUserSwitch;

  end;

implementation

uses WTSAPI32;

const WM_ICONMESSAGE = WM_USER + $200;
var WM_TASKBARCREATED : DWORD;

constructor TTrayIcon.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);

  fIcon := TIcon.Create;
  fIcon.Assign (Application.Icon);

  WM_TASKBARCREATED := RegisterWindowMessage ('TaskbarCreated');

  if not (csDesigning in ComponentState) then
  begin

    fObjectInstance := Classes.MakeObjectInstance (MainWProc);
    fOldMainWProc := Pointer (SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (fObjectInstance)));

    fWindowHandle := Classes.AllocateHWND (WProc);


    if WTSAPIOk then
      WTSRegisterSessionNotification (Application.Handle, NOTIFY_FOR_ALL_SESSIONS);
    fAutoShow := True;

  end
end;

destructor TTrayIcon.Destroy;
begin
  if fIconThere then
  begin
    fIcon := Nil;
    UpdateIcon (0);
  end;

  if WTSAPIOk then
    WTSUnRegisterSessionNotification (Application.Handle); 

  if Assigned (fObjectInstance) then
  begin
    SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (fOldMainWProc));
    Classes.FreeObjectInstance (fObjectInstance)
  end;

  if fWindowHandle <> 0 then
    Classes.DeallocateHWND (fWindowHandle);
  fIcon.Free;
  inherited
end;

procedure TTrayIcon.WProc(var Msg: TMessage);
var
  pt : TPoint;
begin
  with msg do
    if msg = WM_ICONMESSAGE then
      case lParam of
        WM_RBUTTONDOWN :
          begin
            if Assigned (fOnRightBtnClick) then
            	OnRightBtnClick (self);

            if Assigned (fPopupMenu) then
            begin
              GetCursorPos (pt);
              if AutoShow then fPopupMenu.Items [0].default := True;
              SetForegroundWindow (PopupList.Window);
              fPopupMenu.Popup (pt.x, pt.y);
              PostMessage (PopupList.Window, WM_NULL, 0, 0);
            end
          end;

        WM_LBUTTONDOWN :
          if Assigned (fOnLeftBtnClick) then
            OnLeftBtnClick (self);

        WM_LBUTTONDBLCLK :
        begin
          if Assigned (fOnLeftBtnDblClick) then
            OnLeftBtnDblClick (self);

          if Assigned (fPopupMenu) and AutoShow then with fPopupMenu do
            if Assigned (items [0]) then
              items [0].click
        end;

        WM_MOUSEMOVE :
          if Assigned (fOnMouseMove) then
            OnMouseMove (self);
      end
    else
      if msg = WM_QUERYENDSESSION then
        Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam)
end;

procedure TTrayIcon.UpdateIcon (flags : Integer);
var
  iconData : TNotifyIconData;
begin
  if not (csDesigning in ComponentState) then
  begin
    iconData.cbSize := SizeOf (iconData);
    iconData.Wnd := fWindowHandle;
    iconData.uID := Tag;
    iconData.uFlags :=NIF_ICON or NIF_TIP or NIF_MESSAGE;
    iconData.uCallbackMessage := WM_ICONMESSAGE;
    StrPCopy (iconData.szTip, fHint);

    if Assigned (fIcon) and fEnabled then
    begin
      iconData.hIcon := fIcon.Handle;
      if fIconThere then
      begin
    	iconData.uFlags := flags;
        Shell_NotifyIcon (NIM_MODIFY, @iconData)
      end
      else
      begin
	Shell_NotifyIcon (NIM_ADD, @iconData);
        fIconThere := True
      end
    end
    else
    begin
      Shell_NotifyIcon (NIM_DELETE, @iconData);
      fIconThere := False
    end
  end
end;

procedure TTrayIcon.SetIcon (value : TIcon);
begin
  if fIcon <> value then
  begin
    fIcon.Assign (value);
    UpdateIcon (NIF_ICON);
  end
end;

procedure TTrayIcon.SetHint (value : String);
begin
  if fHint <> value then
  begin
    fHint := value;
    UpdateIcon (NIF_TIP);
  end
end;

procedure TTrayIcon.SetEnabled (value : boolean);
begin
  if value <> fEnabled then
  begin
    fEnabled := value;
    UpdateIcon (NIF_ICON or NIF_TIP or NIF_MESSAGE);
  end
end;

procedure TTrayIcon.RedoIcon;
begin
  fIconThere := False;
  UpdateIcon (NIF_ICON or NIF_TIP or NIF_MESSAGE);
end;

procedure TTrayIcon.MainWProc(var Msg: TMessage);
var
  redoI : boolean;
begin
  if Msg.Msg = WM_TASKBARCREATED then
  begin
    redoI := True;
    if Assigned (OnTaskbarCreated) then
      OnTaskbarCreated (self, redoI);
    if redoI then
      RedoIcon
  end
  else
    if Msg.Msg = WM_QUERYENDSESSION then
    begin
      if Assigned (OnEndSession) then
        OnEndSession (self)
    end
    else
      if Msg.Msg = WM_WTSSESSION_CHANGE then
      begin
        if Assigned (OnXPFastUserSwitch) then
          OnXPFastUserSwitch (self, Msg.WParam, Msg.LParam)
      end;

  Msg.Result := CallWindowProc (fOldMainWProc, Application.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TTrayIcon.SetIconHandle(handle: HICON);
begin
  if fIcon.Handle <> handle then
  begin
    fIcon.Handle := handle;
    UpdateIcon (NIF_ICON);
  end
end;

end.
