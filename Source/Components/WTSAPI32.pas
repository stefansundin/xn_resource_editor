unit WTSAPI32;

interface

uses Windows;

const
  NOTIFY_FOR_ALL_SESSIONS = 1;
  NOTIFY_FOR_THIS_SESSION = 0;

  WM_WTSSESSION_CHANGE = $2B1;

  WTS_CONSOLE_CONNECT        = $1;
  WTS_CONSOLE_DISCONNECT     = $2;
  WTS_REMOTE_CONNECT         = $3;
  WTS_REMOTE_DISCONNECT      = $4;
  WTS_SESSION_LOGON          = $5;
  WTS_SESSION_LOGOFF         = $6;
  WTS_SESSION_LOCK           = $7;
  WTS_SESSION_UNLOCK         = $8;
  WTS_SESSION_REMOTE_CONTROL = $9;

  WTS_CURRENT_SERVER_HANDLE  = 0;
  WTS_CURRENT_SESSION = $ffffffff;

type

  WTS_PROCESS_INFO = record
    SessionID: DWORD;
    ProcessID: DWORD;
    pProcessName: PChar;
    pUserSID: PSID;
  end;
  PWTS_PROCESS_INFO = ^WTS_PROCESS_INFO;

  WTS_INFO_CLASS = (
    WTSInitialProgram,
    WTSApplicationName,
    WTSWorkingDirectory,
    WTSOEMId,
    WTSSessionId,
    WTSUserName,
    WTSWinStationName,
    WTSDomainName,
    WTSConnectState,
    WTSClientBuildNumber,
    WTSClientName,
    WTSClientDirectory,
    WTSClientProductId,
    WTSClientHardwareId,
    WTSClientAddress,
    WTSClientDisplay,
    WTSClientProtocolType
  );

  TfnWTSRegisterSessionNotification = function (handle: HWND; flags: DWORD): BOOL; stdcall;
  TfnWTSUnRegisterSessionNotification = function (handle: HWND): BOOL; stdcall;
  TfnWTSGetActiveConsoleSessionID = function: DWORD; stdcall;
  TfnWTSEnumerateProcesses = function (hServer: THandle; reserved, Version: DWORD; var ppProcessInfo: PWTS_PROCESS_INFO; var pCount: DWORD): BOOL; stdcall;
  TfnWTSFreeMemory = procedure(mem: pointer); stdcall;
  TfnWTSQueryUserToken = function (sessionId: ULONG; var token: THandle): BOOL; stdcall;
  TfnWTSQuerySessionInformation = function (hServer: THandle; sessionID: DWORD; WTSInfoClass: WTS_INFO_CLASS; var buffer: pointer; var size: DWORD): BOOL; stdcall;

var
  WTSRegisterSessionNotification: TfnWTSRegisterSessionNotification;
  WTSUnRegisterSessionNotification: TfnWTSUnRegisterSessionNotification;
  WTSGetActiveConsoleSessionID: TfnWTSGetActiveConsoleSessionID = nil;
  WTSEnumerateProcesses: TfnWTSEnumerateProcesses;
  WTSFreeMemory: TfnWTSFreeMemory;
  WTSQueryUserToken: TfnWTSQueryUserToken;
  WTSQuerySessionInformation: TfnWTSQuerySessionInformation;

function WTSAPIOk: Boolean;
function WTSGetCurrentConsoleSessionID: Integer;

implementation

var
  gDLLHandle: THandle;
  gKernelDLLHandle: THandle;

function WTSAPIOk: Boolean;
begin
  Result := gDLLHandle <> 0
end;

function WTSGetCurrentConsoleSessionID: Integer;
begin
  Result := 0;
  if Assigned(WTSGetActiveConsoleSessionID) then
    Result := WTSGetActiveConsoleSessionID
end;

procedure InitWTSAPI;
begin
  gDLLHandle := LoadLibrary('wtsapi32.dll');
  if gDLLHandle <> 0 then
  begin
    WTSRegisterSessionNotification := GetProcAddress (gDLLHandle, 'WTSRegisterSessionNotification');
    WTSUnRegisterSessionNotification := GetProcAddress (gDLLHandle, 'WTSUnRegisterSessionNotification');
    WTSEnumerateProcesses := GetProcAddress (gDLLHandle, 'WTSEnumerateProcessesA');
    WTSFreeMemory := GetProcAddress (gDLLHandle, 'WTSFreeMemory');
    WTSQueryUserToken := GetProcAddress (gDLLHandle, 'WTSQueryUserToken');
    WTSQuerySessionInformation := GetProcAddress (gDLLHandle, 'WTSQuerySessionInformationA');

    if not Assigned(WTSRegisterSessionNotification) or not Assigned(WTSUnRegisterSessionNotification) then
    begin
      FreeLibrary(gDLLHandle);
      gDLLHandle := 0
    end;
  end;
  gKernelDLLHandle := LoadLibrary('kernel32.dll');
  if gKernelDLLHandle <> 0 then
    WTSGetActiveConsoleSessionID := GetProcAddress (gKernelDLLHandle, 'WTSGetActiveConsoleSessionId');
end;

procedure TerminateWTSAPI;
begin
(*  if gDLLHandle <> 0 then
    FreeLibrary(gDLLHandle)
*)
end;

initialization
  InitWTSAPI;
finalization
  TerminateWTSAPI
end.
