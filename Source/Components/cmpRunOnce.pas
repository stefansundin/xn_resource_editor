unit cmpRunOnce;

interface

uses
  Windows, Messages, SysUtils, classes, Forms;

const
  WM_PARAMS = WM_USER + $200;

type
  TOnOtherInstance = procedure (Sender : TObject; ParamCount : DWORD; ParamStr : array of string) of object;

  TRunOnce = class(TComponent)
  private
    fOtherWindowHandle : HWND;
    fUniqueMessage : DWORD;
    fParamsMessage : DWORD;
    fOldOwnerWindowProc : TFNWndProc;
    fObjectInstance : pointer;
    fOnOtherInstance: TOnOtherInstance;
    fMutex : THandle;
    fName : string;
    function CheckOtherApp (hwnd : HWND) : boolean;
    procedure OwnerWindowProc(var msg: TMessage);
    procedure ProcessParameters (remoteMemHandle : THandle; remoteProcessID : DWORD);

  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property OnOtherInstance : TOnOtherInstance read fOnOtherInstance write fOnOtherInstance;
  end;

implementation

{ TRunOnce }

function TRunOnce.CheckOtherApp(hwnd: HWND): boolean;
var
  msgResult : DWORD;
begin
  result := False;
  if hwnd <> TForm (Owner).Handle then
  begin
    if GetWindowLong (hwnd, GWL_USERDATA) = $badf00d then
      if (SendMessageTimeout (hwnd, fUniqueMessage, 0, 0, SMTO_BLOCK or SMTO_ABORTIFHUNG, 1000, msgResult) <> 0) and (msgResult = fUniqueMessage) then
      begin
        fOtherWindowHandle := hwnd;
        result := True
      end
  end
end;

constructor TRunOnce.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
end;

destructor TRunOnce.Destroy;
begin
  if Assigned (fObjectInstance) then
    Classes.FreeObjectInstance (fObjectInstance);


  if fMutex <> 0 then
  begin
    ReleaseMutex (fMutex);
    CloseHandle (fMutex)
  end;
  inherited;
end;

function EnumWindowsProc (hwnd : HWND; lParam : LPARAM) : BOOL; stdcall;
begin
  result := not TRunOnce (lParam).CheckOtherApp (hwnd)
end;

procedure TRunOnce.OwnerWindowProc (var msg : TMessage);
begin
  with msg do
    if Msg = fUniqueMessage then
      result := fUniqueMessage
    else
      if Msg = fParamsMessage then
      try
        ProcessParameters (wParam, lParam)
      except
        Application.HandleException (self)
      end
      else
        result := CallWindowProc (fOldOwnerWindowProc, TForm (Owner).Handle, msg, wParam, lParam);
end;

procedure TRunOnce.Loaded;
var
  mapHandle : THandle;
  paramPtr, p : PChar;
  paramSize : DWORD;
  i : Integer;
begin
  inherited;
  if not (csDesigning in ComponentState) and (Owner is TForm) then
  begin
    fName := UpperCase (ExtractFileName (Application.Exename));
    fMutex := CreateMutex (Nil, True, PChar (fName));
    if GetLastError <>  0 then
    begin
      CloseHandle (fMutex);
      fMutex := 0
    end;
    fUniqueMessage := RegisterWindowMessage (PChar (fName));
    fParamsMessage := RegisterWindowMessage ('WoozleRunOnce');

    fObjectInstance := Classes.MakeObjectInstance (OwnerWindowProc);
    fOldOwnerWindowProc := TfnWndProc (SetWindowLong (TForm (Owner).Handle, GWL_WNDPROC, Integer (fObjectInstance)));

    if fMutex = 0 then
    begin
      Sleep (100);
      EnumWindows (@EnumWindowsProc, LPARAM (self));

      if fOtherWindowHandle <> 0 then
      begin
        paramSize := 1;
        for i := 0 to ParamCount do
          Inc (paramSize, 1 + Length (ParamStr (i)));
        mapHandle := CreateFileMapping ($ffffffff, Nil, PAGE_READWRITE, 0, 65536, Nil);
        if mapHandle <> 0 then
        try
          paramPtr := MapViewOfFile (mapHandle, FILE_MAP_WRITE, 0, 0, paramSize);
          if paramPtr <> Nil then
          try
            p := paramPtr;
            for i := 0 to ParamCount do
            begin
              lstrcpy (p, PChar (ParamStr (i)));
              Inc (p, Length (ParamStr (i)) + 1)
            end;
            p^ := #0;
          finally
            UnmapViewOfFile (paramPtr);
          end
          else
            RaiseLastOSError;

          SendMessage (fOtherWindowHandle, fParamsMessage, mapHandle, GetCurrentProcessID);
        finally
          CloseHandle (mapHandle);
        end
        else
          RaiseLastOSError;

        SetForegroundWindow (fOtherWindowHandle)
      end;
      Halt
    end
    else
    SetWindowLong (TForm (Owner).Handle, GWL_USERDATA, $badf00d)
  end
end;

procedure TRunOnce.ProcessParameters(remoteMemHandle : THandle; remoteProcessID: DWORD);
var
  memHandle : THandle;
  remoteProcessHandle : THandle;
  paramPtr : PChar;
  p : PChar;
  paramCount : DWORD;
  params : array of string;
  i : Integer;
begin
  remoteProcessHandle := OpenProcess (PROCESS_DUP_HANDLE, false, remoteProcessID);
  if remoteProcessHandle <> 0 then
  try
    if DuplicateHandle (remoteProcessHandle, remoteMemHandle, GetCurrentProcess, @memHandle, FILE_MAP_READ, False, 0) then
    try
      paramPtr := MapViewOfFile (memHandle, FILE_MAP_READ, 0, 0, 65536);
      if paramPtr <> Nil then
      try
        if Assigned (fOnOtherInstance) and not (csDestroying in ComponentState) then
        begin
          p := paramPtr;
          paramCount := 0;
          while p^ <> #0 do
          begin
            Inc (paramCount);
            Inc (p, lstrlen (p) + 1)
          end;
          SetLength (params, paramCount);
          p := paramPtr;
          i := 0;
          while p^ <> #0 do
          begin
            params [i] := p;
            Inc (p, lstrlen (p) + 1);
            Inc (i);
          end;

          OnOtherInstance (self, paramCount - 1, params)
        end
      finally
        UnmapViewOfFile (paramPtr)
      end
      else
        RaiseLastOSError
    finally
      CloseHandle (memHandle);
    end
    else
      RaiseLastOSError
  finally
    CloseHandle (remoteProcessHandle)
  end
  else
    RaiseLastOSError;
end;

end.
