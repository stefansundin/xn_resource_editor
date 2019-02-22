unit unitDefRegistry;

interface

uses
  Windows, Classes, SysUtils, Registry;

type
  EDefRegistry = class (Exception)
  end;

  TDefRegistry = class (TRegistry)
  public
    function GetValue(const Name, Default: string): string; overload;
    function GetValue(const Name: string; Default: Integer): Integer; overload;
    function GetValue(const Name: string; Default: Boolean): Boolean; overload;
    procedure GetValue(const Name: string; Strings: TStringList); overload;

    procedure SetValue(const Name, value, Default: string); overload;
    procedure SetValue(const Name: string; value, Default: Integer); overload;
    procedure SetValue(const Name: string; value, Default: Boolean); overload;
    procedure SetValue(const Name: string; Strings: TStringList); overload;
  end;

implementation

{ TDefRegistry }

(*----------------------------------------------------------------------*
 | function TDefRegistry.GetValue                                       |
 |                                                                      |
 | Get an Integer value from the registry.  Return 'default' if the     |
 | value does not exist.                                                |
 *----------------------------------------------------------------------*)
function TDefRegistry.GetValue(const Name: string;
  Default: Integer): Integer;
begin
  if ValueExists (Name) then
    Result := ReadInteger (Name)
  else
    Result := Default
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.GetValue                                       |
 |                                                                      |
 | Get a Boolean value from the registry.  Return 'default' if the      |
 | value does not exist.                                                |
 *----------------------------------------------------------------------*)
function TDefRegistry.GetValue(const Name: string;
  Default: Boolean): Boolean;
begin
  if ValueExists (Name) then
    Result := ReadBool (Name)
  else
    Result := Default
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.GetValue                                       |
 |                                                                      |
 | Get a string value from the registry.  Return 'default' if the       |
 | value does not exist.                                                |
 *----------------------------------------------------------------------*)
function TDefRegistry.GetValue(const Name, Default: string): string;
begin
  if ValueExists (Name) then
    Result := ReadString (Name)
  else
    Result := Default
end;

procedure TDefRegistry.GetValue(const Name: string; Strings: TStringList);
var
  valueType: DWORD;
  valueLen: DWORD;
  p, buffer: PChar;
begin
  Strings.Clear;
  if not ValueExists (Name) then Exit;
  SetLastError (RegQueryValueEx (CurrentKey, PChar (Name), Nil, @valueType, Nil, @valueLen));
  if GetLastError = ERROR_SUCCESS then
    if valueType = REG_MULTI_SZ then
    begin
      GetMem (buffer, valueLen);
      try
        RegQueryValueEx (CurrentKey, PChar (Name), Nil, Nil, PBYTE (buffer), @valueLen);
        p := buffer;
        while p^ <> #0 do
        begin
          Strings.Add (p);
          Inc(p, lstrlen (p) + 1)
        end
      finally
        FreeMem (buffer)
      end
    end
    else
      raise ERegistryException.Create('String list expected')
  else
    raise ERegistryException.Create('Unable read MULTI_SZ value')
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.SetValue                                       |
 |                                                                      |
 | Set a string value in the registry - or delete the value if it       |
 | matches 'default'                                                    |
 *----------------------------------------------------------------------*)
procedure TDefRegistry.SetValue(const Name, value, Default: string);
begin
  if value = Default then
    DeleteValue(Name)
  else
    WriteString (Name, value)
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.SetValue                                       |
 |                                                                      |
 | Set a Boolean value in the registry - or delete the value if it      |
 | matches 'default'                                                    |
 *----------------------------------------------------------------------*)
procedure TDefRegistry.SetValue(const Name: string; value,
  Default: Boolean);
begin
  if value = Default then
    DeleteValue(Name)
  else
    WriteBool (Name, value)
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.SetValue                                       |
 |                                                                      |
 | Set an Integer value in the registry - or delete the value if it     |
 | matches 'default'                                                    |
 *----------------------------------------------------------------------*)
procedure TDefRegistry.SetValue(const Name: string; value,
  Default: Integer);
begin
  if value = Default then
    DeleteValue(Name)
  else
    WriteInteger (Name, value)
end;


procedure TDefRegistry.SetValue(const Name: string; Strings: TStringList);
var
  p, buffer: PChar;
  i: Integer;
  size: DWORD;
begin
  if Strings.Count = 0 then
  begin
    DeleteValue(Name);
    exit
  end;
  size := 0;
  for i := 0 to Strings.Count - 1 do
    Inc(size, Length(Strings[i]) + 1);
  Inc(size);
  GetMem (buffer, size);
  try
    p := buffer;
    for i := 0 to Strings.count - 1 do
    begin
      lstrcpy(p, PChar (Strings[i]));
      Inc(p, lstrlen (p) + 1)
    end;
    p^ := #0;
    SetLastError (RegSetValueEx (CurrentKey, PChar (Name), 0, REG_MULTI_SZ, buffer, size));
    if GetLastError <> ERROR_SUCCESS then
      raise ERegistryException.Create('Unable to write MULTI_SZ value');
  finally
    FreeMem (buffer)
  end
end;

end.
