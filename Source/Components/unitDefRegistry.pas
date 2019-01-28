unit unitDefRegistry;

interface

uses Windows, Classes, SysUtils, Registry;

type

EDefRegistry = class (Exception)
end;

TDefRegistry = class (TRegistry)
public
  function GetValue (const name, default : string) : string; overload;
  function GetValue (const name : string; default : integer) : Integer; overload;
  function GetValue (const name : string; default : boolean) : boolean; overload;
  procedure GetValue (const name : string; strings : TStringList); overload;

  procedure SetValue (const name, value, default : string); overload;
  procedure SetValue (const name : string; value, default : Integer); overload;
  procedure SetValue (const name : string; value, default : boolean); overload;
  procedure SetValue (const name : string; strings : TStringList); overload;
end;

implementation

{ TDefRegistry }

(*----------------------------------------------------------------------*
 | function TDefRegistry.GetValue                                       |
 |                                                                      |
 | Get an integer value from the registry.  Return 'default' if the     |
 | value does not exist.                                                |
 *----------------------------------------------------------------------*)
function TDefRegistry.GetValue(const name: string;
  default: integer): Integer;
begin
  if ValueExists (name) then
    result := ReadInteger (name)
  else
    result := default
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.GetValue                                       |
 |                                                                      |
 | Get a boolean value from the registry.  Return 'default' if the      |
 | value does not exist.                                                |
 *----------------------------------------------------------------------*)
function TDefRegistry.GetValue(const name: string;
  default: boolean): boolean;
begin
  if ValueExists (name) then
    result := ReadBool (name)
  else
    result := default
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.GetValue                                       |
 |                                                                      |
 | Get a string value from the registry.  Return 'default' if the       |
 | value does not exist.                                                |
 *----------------------------------------------------------------------*)
function TDefRegistry.GetValue(const name, default: string): string;
begin
  if ValueExists (name) then
    result := ReadString (name)
  else
    result := default
end;

procedure TDefRegistry.GetValue(const name: string; strings: TStringList);
var
  valueType : DWORD;
  valueLen : DWORD;
  p, buffer : PChar;
begin
  strings.Clear;
  if not ValueExists (name) then Exit;
  SetLastError (RegQueryValueEx (CurrentKey, PChar (name), Nil, @valueType, Nil, @valueLen));
  if GetLastError = ERROR_SUCCESS then
    if valueType = REG_MULTI_SZ then
    begin
      GetMem (buffer, valueLen);
      try
        RegQueryValueEx (CurrentKey, PChar (name), Nil, Nil, PBYTE (buffer), @valueLen);
        p := buffer;
        while p^ <> #0 do
        begin
          strings.Add (p);
          Inc (p, lstrlen (p) + 1)
        end
      finally
        FreeMem (buffer)
      end
    end
    else
      raise ERegistryException.Create ('String list expected')
  else
    raise ERegistryException.Create ('Unable read MULTI_SZ value')
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.SetValue                                       |
 |                                                                      |
 | Set a string value in the registry - or delete the value if it       |
 | matches 'default'                                                    |
 *----------------------------------------------------------------------*)
procedure TDefRegistry.SetValue(const name, value, default: string);
begin
  if value = default then
    DeleteValue (name)
  else
    WriteString (name, value)
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.SetValue                                       |
 |                                                                      |
 | Set a boolean value in the registry - or delete the value if it      |
 | matches 'default'                                                    |
 *----------------------------------------------------------------------*)
procedure TDefRegistry.SetValue(const name: string; value,
  default: boolean);
begin
  if value = default then
    DeleteValue (name)
  else
    WriteBool (name, value)
end;

(*----------------------------------------------------------------------*
 | function TDefRegistry.SetValue                                       |
 |                                                                      |
 | Set an integer value in the registry - or delete the value if it     |
 | matches 'default'                                                    |
 *----------------------------------------------------------------------*)
procedure TDefRegistry.SetValue(const name: string; value,
  default: Integer);
begin
  if value = default then
    DeleteValue (name)
  else
    WriteInteger (name, value)
end;


procedure TDefRegistry.SetValue(const name: string; strings: TStringList);
var
  p, buffer : PChar;
  i : Integer;
  size : DWORD;
begin
  if strings.Count = 0 then
  begin
    DeleteValue (name);
    exit
  end;
  size := 0;
  for i := 0 to strings.Count - 1 do
    Inc (size, Length (strings [i]) + 1);
  Inc (size);
  GetMem (buffer, size);
  try
    p := buffer;
    for i := 0 to strings.count - 1 do
    begin
      lstrcpy (p, PChar (strings [i]));
      Inc (p, lstrlen (p) + 1)
    end;
    p^ := #0;
    SetLastError (RegSetValueEx (CurrentKey, PChar (name), 0, REG_MULTI_SZ, buffer, size));
    if GetLastError <> ERROR_SUCCESS then
      raise ERegistryException.Create ('Unable to write MULTI_SZ value');
  finally
    FreeMem (buffer)
  end
end;

end.
