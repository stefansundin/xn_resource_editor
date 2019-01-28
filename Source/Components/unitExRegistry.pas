(*======================================================================*
 | unitExRegistry unit                                                  |
 |                                                                      |
 | Extended registry classes                                            |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/08/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitEXRegistry;

interface

uses windows, classes, sysutils, registry;

type

TWalkProc = procedure (const keyName, valueName : string; dataType : DWORD; data : pointer; DataLen : Integer) of object;

TSearchParam = (rsKeys, rsValues, rsData);
TSearchParams = set of TSearchParam;

TSearchNode = class
  fValueNames : TStringList;
  fKeyNames : TStringList;
  fCurrentKey : string;
  fPath: string;
  fValueIDX, fKeyIDX : Integer;
  fRegRoot : HKEY;
  constructor Create (ARegRoot : HKEY; const APath : string);
  destructor Destroy; override;

  procedure LoadKeyNames;
  procedure LoadValueNames;
end;

TExRegistry = class (TRegistry)
private
  fSaveServer : string;
  fExportStrings : TStrings;
  fExportExcludeKeys : TStrings;
  fLastExportKey : string;
  fSearchParams : TSearchParams;
  fSearchString : string;
  fSearchStack : TList;
  fMatchWholeString : boolean;
  fCancelSearch : boolean;
  fLocalRoot : HKEY;
  fValuesSize : Integer;
  procedure ValuesSizeProc (const keyName, valueName : string; dataType : DWORD; data : pointer; DataLen : Integer);
  procedure ClearSearchStack;
protected
  procedure StartExport;
  procedure ExportProc (const keyName, valueName : string; dataType : DWORD; data : pointer; DataLen : Integer);
  function EndExport : string;

public
  destructor Destroy; override;
  procedure SetRoot (root : HKey; const server : string);
  procedure CopyValueFromReg (const valueName : string; otherReg : TExRegistry; deleteSource : boolean);
  procedure CopyKeyFromReg (const keyName : string; otherReg : TExRegistry; deleteSource : boolean);
  function GetValueType (const valueName : string) : DWORD;
  procedure ReadStrings (const valueName : string; strings : TStrings);
  procedure WriteStrings (const valueName : string; strings : TStrings);
  procedure ExportKey (const fileName : string);
  procedure ExportToStream (strm : TStream; ExcludeKeys : TStrings = Nil);
  procedure ImportRegFile (const fileName : string);
  procedure ImportFromStream (stream : TStream);
  procedure WriteTypedBinaryData (const valueName : string; tp : Integer; var data; size : Integer);
  procedure Walk (walkProc : TWalkProc; valuesRequired : boolean);
  function FindFirst (const data : string; params : TSearchParams; MatchWholeString : boolean; var retPath, retValue : string) : boolean;
  function FindNext (var retPath, retValue : string) : boolean;
  procedure CancelSearch;
  procedure GetValuesSize (var size : Integer);

  property LocalRoot : HKEY read fLocalRoot;
  property SearchString : string read fSearchString;
  property Server : string read fSaveServer;
end;

EExRegistryException = class (ERegistryException)
private
  fCode: Integer;
  function GetError : string;
public
  constructor CreateLastError (const st : string);
  constructor Create (code : DWORD; const st : string);
  property Code : Integer read fCode;
end;

implementation

function MakeCStringConst (const s : string; len : Integer = -1) : string;
var
  i : Integer;
begin
  result := '';
  if len = -1 then
    len := Length (s);
  for i := 1 to len do
  begin
    if s [i] in ['\', '"'] then
      result := result + '\';
    result := result + s [i]
  end;
  result := PChar (result)
end;

{ TExRegistry }

resourcestring
  errUnableToConnect = 'Unable to connect to the registry on %s (%d)';

type
  TRootRec = record
    key : HKEY;
    name : string;
  end;

const
  NO_ROOT_KEYS = 7;
  RootKeys : array [0..NO_ROOT_KEYS - 1] of TRootRec = (
    (key : HKEY_CLASSES_ROOT;     name : 'HKEY_CLASSES_ROOT'),
    (key : HKEY_CURRENT_USER;     name : 'HKEY_CURRENT_USER'),
    (key : HKEY_LOCAL_MACHINE;    name : 'HKEY_LOCAL_MACHINE'),
    (key : HKEY_USERS;            name : 'HKEY_USERS'),
    (key : HKEY_PERFORMANCE_DATA; name : 'HKEY_PERFORMANCE_DATA'),
    (key : HKEY_CURRENT_CONFIG;   name : 'HKEY_CURRENT_CONFIG'),
    (key : HKEY_DYN_DATA;         name : 'HKEY_DYN_DATA'));


function RootKeyName (key : HKEY) : string;
var
  i : Integer;
begin
  result := '';
  for i := 0 to NO_ROOT_KEYS - 1 do
    if RootKeys [i].key = key then
    begin
      result := RootKeys [i].name;
      break
    end
end;

function RootKeyVal (const st : string) : HKEY;
var
  i : Integer;
begin
  result := $ffffffff;
  for i := 0 to NO_ROOT_KEYS - 1 do
    if RootKeys [i].name = st then
    begin
      result := RootKeys [i].key;
      break
    end
end;


procedure TExRegistry.CancelSearch;
begin
  fCancelSearch := True;
end;

procedure TExRegistry.ClearSearchStack;
var
  i : Integer;
begin
  if Assigned (fSearchStack) then
  begin
    for i := 0 to fSearchStack.Count - 1 do
      TSearchNode (fSearchStack [i]).Free;
    fSearchStack.Free;
    fSearchStack := Nil
  end
end;

procedure TExRegistry.CopyKeyFromReg(const keyName: string;
  otherReg: TExRegistry; deleteSource : boolean);
var
  i : Integer;
  values : TStringList;
  sourceReg : TExRegistry;
  destReg : TExRegistry;
begin
  sourceReg := TExRegistry.Create;
  destReg := TExRegistry.Create;
  values := TStringList.Create;
  try
    sourceReg.RootKey := otherReg.CurrentKey;
    if deleteSource then
      sourceReg.OpenKey (keyName, False)
    else
      sourceReg.OpenKeyReadOnly (keyName);
    sourceReg.GetValueNames (values);

    destReg.RootKey := CurrentKey;
    if destReg.OpenKey (keyName, True) then
    begin
      for i := 0 to values.Count - 1 do
        destReg.CopyValueFromReg (values [i], sourceReg, deleteSource);

      sourceReg.GetKeyNames (values);
      for i := 0 to values.Count - 1 do
        destReg.CopyKeyFromReg (values [i], sourceReg, deleteSource);

      if DeleteSource then
        if not otherReg.DeleteKey (keyName) then
          Raise ERegistryException.Create ('Unable to delete moved key')
    end
    else
      raise ERegistryException.Create ('Unable to open destination');
  finally
    values.Free;
    destReg.Free;
    sourceReg.Free
  end
end;

procedure TExRegistry.CopyValueFromReg(const valueName: string;
  otherReg: TExRegistry; deleteSource : boolean);
var
  buffer : PByte;
  BufSize : DWORD;
  DataType : DWORD;
begin
  BufSize := 65536;
  GetMem (buffer, BufSize);
  try
    DataType := REG_NONE;

    SetLastError (RegQueryValueEx(otherReg.CurrentKey, PChar(valueName), nil, @DataType, Buffer,
      @BufSize));

     if GetLastError <> ERROR_SUCCESS then
      raise EExRegistryException.CreateLastError ('Unable to copy value');

    SetLastError (RegSetValueEx (CurrentKey, PChar (valueName), 0, DataType, buffer, BufSize));
    if GetLastError <> ERROR_SUCCESS then
      raise EExRegistryException.CreateLastError ('Unable to copy value');

    if deleteSource then
      if not otherReg.DeleteValue (valueName) then
        raise ERegistryException.Create ('Unable to delete moved value')
  finally
    FreeMem (buffer)
  end
end;


destructor TExRegistry.Destroy;
begin
  ClearSearchStack;
  inherited Destroy
end;

function TExRegistry.EndExport: string;
begin
  if Assigned (fExportStrings) then
  begin
    result := fExportStrings.Text;
    freeandNil (fExportStrings)
  end
  else
    result := '';
end;

procedure TExRegistry.ExportKey(const fileName: string);
begin
  fExportStrings := TStringList.Create;
  fExportStrings.Add ('REGEDIT4');
  try
    fLastExportKey := '';
    Walk (ExportProc, True);
    fExportStrings.Add ('');
  finally
    fExportStrings.SaveToFile (fileName);
    fExportStrings.Free;
  end
end;

procedure TExRegistry.ExportProc(const keyName, valueName: string;
  dataType: DWORD; data: pointer; DataLen: Integer);

var
  st : string;
  st1 : string;
  j : Integer;
  localRoot : HKey;

begin
  localRoot := fLocalRoot;
  if localRoot = 0 then
    localRoot := RootKey;

  if fLastExportKey <> keyName then
  begin
    fExportStrings.Add ('');
// TODO    fExportStrings.Add (Format ('[%s\%s]', [rootKeyName(localRoot), keyName]));

    fLastExportKey := keyName;
  end;

  if dataLen <> 0 then
  begin
    if valueName = '' then
      st := '@='
    else
      st := Format ('"%s"=', [MakeCStringConst (valueName)]);

    case dataType of
      REG_DWORD :
      begin
        st1 := LowerCase (Format ('%8.8x', [PDWORD (data)^]));
        st := st + format ('dword:%s', [st1])
      end;

      REG_SZ    :
          st := st + format ('"%s"', [MakeCStringConst (PChar (data), dataLen - 1)]);

      else
      begin
        if dataType = REG_BINARY then
          st := st + 'hex:'
        else
          st := st + format ('hex(%d):', [dataType]);
        for j := 0 to dataLen - 1 do
        begin
          st1 := LowerCase (format ('%02.2x', [Byte (PChar (data) [j])]));
          if j < dataLen - 1 then
            st1 := st1 + ',';

          if Length (st) + Length (st1) >= 77 then
          begin
            fExportStrings.Add (st + st1 + '\');
            st := '  ';
          end
          else
            st := st + st1;
        end
      end
    end;
    fExportStrings.Add (st);
  end
end;

procedure TExRegistry.ExportToStream(strm: TStream; ExcludeKeys : TStrings = Nil);
begin
  fExportExcludeKeys := ExcludeKeys;
  fExportStrings := TStringList.Create;
  fExportStrings.Add ('REGEDIT4');
  try
    fLastExportKey := '';
    Walk (ExportProc, True);
    fExportStrings.Add ('');
  finally
    fExportStrings.SaveToStream (strm);
    fExportStrings.Free;
    fExportExcludeKeys := Nil;
  end
end;

function TExRegistry.FindFirst(const data: string; params: TSearchParams; MatchWholeString : boolean;
  var retPath, retValue: string): boolean;
var
  path, nPath, keyName : string;
  p : Integer;
  n : TSearchNode;
begin
  ClearSearchStack;

  fSearchStack := TList.Create;
  path := currentPath;


  nPath := '';
  repeat
    p := Pos ('\', path);
    if p > 0 then
    begin
      nPath := nPath + '\' + Copy (path, 1, p - 1);
      path := Copy (path, p + 1, MaxInt);
      n := TSearchNode.Create (RootKey, nPath);
      n.LoadKeyNames;
      p := Pos ('\', path);
      if p > 0 then
        keyName := Copy (path, 1, p - 1)
      else
        keyName := path;

      n.fKeyIDX := n.fKeyNames.IndexOf (keyName);

      fSearchStack.Add (n);
    end
  until p = 0;

  n := TSearchNode.Create (RootKey, nPath + '\' + path);
  fSearchStack.Add (n);

  fSearchString := UpperCase (data);
  fSearchParams := params;
  fMatchWholeString := MatchWholeString;
  result := FindNext (retPath, retValue);
end;

function TExRegistry.FindNext(var retPath, retValue: string): boolean;
var
  n : TSearchNode;
  found : boolean;
  k : string;
  msg : TMsg;
begin
  found := False;
  fCancelSearch := False;
  while (not found) and (not fCancelSearch) and (fSearchStack.Count > 0) do
  begin
    while PeekMessage (msg, 0, 0, 0, PM_REMOVE) do
    begin
      TranslateMessage (msg);
      DispatchMessage (msg)
    end;

    n := TSearchNode (fSearchStack [fSearchStack.Count - 1]);
    if rsValues in fSearchParams then
    begin
      n.LoadValueNames;
      with n do
        if fValueIdx < fValueNames.Count then
        repeat
          Inc (fValueIdx);
          if fValueIdx < fValueNames.Count then
          begin
            if fMatchWholeString then
              found := fSearchString = fValueNames [fValueIdx]
            else
              found := Pos (fSearchString, fValueNames [fValueIdx]) > 0
          end
        until fCancelSearch or found or (fValueIdx = fValueNames.Count)
    end;

    if not fCancelSearch and not found then
    begin
      n.LoadKeyNames;
      with n do
        if fKeyIdx < fKeyNames.Count then
        begin
          Inc (fKeyIdx);
          if fKeyIdx < fKeyNames.Count then
          begin

            if rsKeys in fSearchParams then
              if fMatchWholeString then
                found := fSearchString = fKeyNames [fKeyIdx]
              else
                found := Pos (fSearchString, fKeyNames [fKeyIdx]) > 0;

            if not found then
            begin
              if n.fPath = '\' then
                k := '\' + fKeyNames [fKeyIdx]
              else
                k := n.fPath + '\' + fKeyNames [fKeyIdx];

              fSearchStack.Add (TSearchNode.Create (RootKey, k));

              continue
            end
          end
      end
    end;

    if fCancelSearch then
      Break;

    if not found then
    begin
      n.Free;
      fSearchStack.Delete (fSearchStack.Count - 1)
    end
    else
    begin
      retPath := n.fPath;
      if n.fKeyIdx > -1 then
        retPath := retPath + '\' + n.fKeyNames [n.fKeyIdx];

      if rsValues in fSearchParams then
        if (n.fValueIdx > -1) and (n.fValueIdx < n.fValueNames.Count) then
          retValue := n.fValueNames [n.fValueIdx]
        else
          retValue := '';
    end
  end;
  result := found
end;

procedure TExRegistry.GetValuesSize(var size: Integer);
begin
  fValuesSize := 0;
  Walk (ValuesSizeProc, False);
  if fValuesSize = 0 then
    fValuesSize := -1;
  size := fValuesSize
end;

function TExRegistry.GetValueType(const valueName: string): DWORD;
var
  valueType : DWORD;
begin
  SetLastError (RegQueryValueEx (CurrentKey, PChar (valueName), Nil, @valueType, Nil, Nil));
  if GetLastError = ERROR_SUCCESS then
    result := valueType
  else
    raise EExRegistryException.CreateLastError ('Unable to get value type');
end;

procedure TExRegistry.ImportFromStream(stream: TStream);
var
  strings : TStrings;
  st : string;
  i : Integer;

  procedure SyntaxError;
  begin
    raise Exception.CreateFmt ('Syntax error in reg stream at line %d', [i])
  end;

  procedure CreateNewKey;
  var
    s : string;
    p : Integer;
    r : HKEY;
  begin
    Delete (st, 1, 1);
    if st [Length (st)] <> ']' then
      SyntaxError;

    Delete (st, Length (st), 1);

    p := pos ('\', st);
    if p = 0 then
      SyntaxError;
    s := Copy (st, 1, p - 1);
    st := Copy (st, p + 1, MaxInt);

    if st = '' then
      SyntaxError;

    r := RootKeyVal (s);
    if r = $ffffffff then
      SyntaxError;

    SetRoot (r, fSaveServer);
    OpenKey ('\' + st, True)
  end;

  function GetCString (st : string; var n : Integer) : string;
  var
    i : Integer;
    eos : boolean;
  begin
    result := '';
    i := 2;
    repeat
      eos := False;
      while i <= Length (st) do
      begin
        if st [i] = '"' then
        begin
          eos := True;
          break
        end;

        if st [i] = '\' then
          Inc (i);

        if i <= Length (st) then
          result := result + st [i];

        Inc (i)
      end;

      if not eos then
      begin
        result := result + #13#10;
        Inc (n);
        st := strings [n];
        i := 1
      end
    until eos
  end;

  function GetBinaryBuffer (const st : string) : string;
  var
    i : Integer;
    val : string;
  begin
    i := 1;
    result := '';
    while i <= Length (st) do
    begin
      if st [i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
        val := val + st [i]
      else
      begin
        if val <> '' then
        begin
          result := result + chr (StrToInt ('$' + val));
          val := ''
        end
      end;

      Inc (i)
    end
  end;

  procedure CreateNewValue (var i : Integer);
  var
    s : string;
    fn : string;
    p : Integer;
    tp : Integer;
    buf : string;
  begin
    if st [1] = '"' then
    begin
      Delete (st, 1, 1);
      p := Pos ('"', st);
      if p = 0 then
        SyntaxError;

      s := Copy (st, 1, p - 1);
      st := Copy (st, p + 1, MaxInt)
    end
    else
    begin
      Delete (st, 1, 1);
      s := ''
    end;

    st := TrimLeft (st);

    if st = '' then
      SyntaxError;

    if st [1] <> '=' then
      SyntaxError;

    Delete (st, 1, 1);

    st := TrimLeft (st);

    if st [1] = '"' then
      WriteString (s, GetCString (st, i))
    else
    begin
      p := 1;
      while (p <= Length (st)) and not (st [p] in [':', '(', ' ']) do
        Inc (p);

      fn := Copy (st, 1, p - 1);

      st := TrimLeft (Copy (st, p, MaxInt));

      if CompareText (fn, 'hex') = 0 then
      begin
        tp := 3;
        if st [1] = '(' then
        begin
          Delete (st, 1, 1);
          fn := '';
          p := 1;
          while (p <= Length (st)) and (st [p] <> ')') do
          begin
            fn := fn + st [p];
            Inc (p)
          end;

          tp := StrToInt (fn);
          st := Trim (Copy (st, p + 1, MaxInt));
        end;

        if st [1] <> ':' then
          SyntaxError;

        Delete (st, 1, 1);

        buf := GetBinaryBuffer (st);

        WriteTypedBinaryData (s, tp, PChar (buf)^, Length (buf));
      end
      else
        if CompareText (fn, 'dword') = 0 then
        begin
          if st [1] <> ':' then
            SyntaxError;

          Delete (st, 1, 1);
          WriteInteger (s, StrToInt ('$' + TrimLeft (st)))
        end
        else
          SyntaxError
    end
  end;

begin
  strings := TStringList.Create;
  try
    strings.LoadFromStream(stream);

    while (strings.Count > 0) do
    begin
      st := Trim (strings [0]);
      if (st = '') or (st [1] = ';') then
        strings.Delete (0)
      else
        break
    end;

    if strings [0] <> 'REGEDIT4' then
      raise Exception.Create ('Bad file format.  Missing REGEDIT4 in first line.');

    i := 1;
    while i < strings.Count do
    begin
      st := Trim (strings [i]);

      if st <> '' then
        while st [Length (st)] = '\' do
        begin
          Inc (i);
          Delete (st, Length (st), 1);
          if i < strings.Count then
            st := st + strings [i]
          else
            break
        end;

      if (Length (st) > 0) and (st [1] <> ';') then
      begin
        case st [1] of
          '[' : CreateNewKey;
          '"' : CreateNewValue (i);
          '@' : CreateNewValue (i);
          else
            SyntaxError
        end
      end;

      Inc (i)
    end
  finally
    strings.Free
  end
end;

procedure TExRegistry.ImportRegFile(const fileName: string);
var
  f : TFileStream;
begin
  f := TFileStream.Create (fileName, fmOpenRead or fmShareDenyNone);
  try
    ImportFromStream (f)
  finally
    f.Free
  end
end;

procedure TExRegistry.ReadStrings(const valueName: string;
  strings: TStrings);
var
  valueType : DWORD;
  valueLen : DWORD;
  p, buffer : PChar;
begin
  strings.Clear;
  SetLastError (RegQueryValueEx (CurrentKey, PChar (valueName), Nil, @valueType, Nil, @valueLen));
  if GetLastError = ERROR_SUCCESS then
    if valueType = REG_MULTI_SZ then
    begin
      GetMem (buffer, valueLen);
      try
        RegQueryValueEx (CurrentKey, PChar (valueName), Nil, Nil, PBYTE (buffer), @valueLen);
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
    raise EExRegistryException.CreateLastError ('Unable read MULTI_SZ value')
end;

procedure TExRegistry.SetRoot(root: HKey; const server: string);
begin
  fSaveServer := server;
  RootKey := root;
  fLocalRoot := root;
  if server <> '' then
    if not RegistryConnect ('\\' + server) then
      Raise Exception.CreateFmt (errUnableToConnect, [server, GetLastError])
end;

procedure TExRegistry.StartExport;
begin
  fLastExportKey := '';
  fExportStrings := TStringList.Create
end;

procedure TExRegistry.ValuesSizeProc(const keyName, valueName: string;
  dataType: DWORD; data: pointer; DataLen: Integer);
begin
  Inc (fValuesSize, DataLen);
end;

procedure TExRegistry.Walk(walkProc: TWalkProc; valuesRequired : boolean);

var
  defaultValue : array [0..256] of char;
  defaultValueLen : DWORD;

  valueName : array [0..256] of char;
  valueNameLen : DWORD;

  keyName : array [0..256] of char;

  cValues : DWORD;
  tp : DWORD;

  buffer : PChar;
  bufSize : DWORD;

  valueLen, maxValueLen : DWORD;
  keyLen : DWORD;
  level : DWORD;

  procedure DoWalk (const pathName : string);
  var
    k : HKEY;
    err : Integer;
    i : Integer;
    cSubKeys : DWORD;
  begin
    err := RegOpenKeyEx (RootKey, PChar (pathName), 0, KEY_READ, k);
    if err = ERROR_SUCCESS then
    try
      Inc (level);
      defaultValueLen := sizeof (defaultValue);

      err := RegQueryInfoKey (k, defaultValue, @defaultValueLen, Nil, @cSubkeys, Nil, Nil, @cValues, nil, @maxValueLen, nil, nil);
      if (err <> ERROR_SUCCESS) and (err <> ERROR_ACCESS_DENIED) then
        raise EExRegistryException.Create (err, 'Unable to query key info');

      if err = ERROR_SUCCESS then
      begin
        if cValues > 0 then
        begin
          if maxValueLen > bufSize then
          begin
            bufSize := 65536 * ((maxValueLen + 65536) div 65536);
            ReallocMem (buffer, bufSize)
          end;

          for i := 0 to cValues - 1 do
          begin
            valueNameLen := sizeof (valueName);
            valueLen := maxValueLen;
            if valuesRequired then
              err := RegEnumValue (k, i, valueName, valueNameLen, Nil, @tp, PByte (buffer), @valueLen)
            else
              err := RegEnumValue (k, i, valueName, valueNameLen, Nil, @tp, Nil, @valueLen);
            if err <> ERROR_SUCCESS then
              raise EExRegistryException.Create (err, 'Unable to get value info');

            walkProc (pathName, valueName, tp, buffer, valueLen);
          end
        end
        else
          walkProc (pathName, '', 0, Nil, 0);

        for i := 0 to cSubkeys - 1 do
        begin
          keyLen := sizeof (keyName);
          RegEnumKey (k, i, keyName, keyLen);

          if not ((level = 1) and Assigned (fExportExcludeKeys) and (fExportExcludeKeys.IndexOf(keyName) >= 0)) then
            if pathName = '' then
              DoWalk (keyName)
            else
              DoWalk (pathName + '\' + keyName)
        end
      end
    finally
      RegCloseKey (k);
      Dec (level);
    end
  end;

begin
  bufSize := 65536;
  level := 0;
  GetMem (buffer, bufSize);

  try
    if Assigned (walkProc) then
      DoWalk (CurrentPath);
  finally
    FreeMem (buffer)
  end
end;

procedure TExRegistry.WriteStrings(const valueName: string;
  strings: TStrings);
var
  p, buffer : PChar;
  i : Integer;
  size : DWORD;
begin
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
    SetLastError (RegSetValueEx (CurrentKey, PChar (valueName), 0, REG_MULTI_SZ, buffer, size));
    if GetLastError <> ERROR_SUCCESS then
      raise EExRegistryException.CreateLastError ('Unable to write MULTI_SZ value');
  finally
    FreeMem (buffer)
  end
end;

procedure TExRegistry.WriteTypedBinaryData(const valueName: string;
  tp: Integer; var data; size: Integer);
begin
  if RegSetValueEx (CurrentKey, PChar(valueName), 0, tp, @data, size) <> ERROR_SUCCESS then
    raise ERegistryException.CreateFmt('Unable to set registry data for %s', [valueName]);
end;

{ EExRegistryException }

constructor EExRegistryException.Create(code: DWORD; const st: string);
begin
  fCode := code;
  inherited Create (GetError + ':' + st);
end;

constructor EExRegistryException.CreateLastError(const st: string);
begin
  fCode := GetLastError;
  inherited Create (GetError + ':' + st);
end;

function EExRegistryException.GetError: string;
var
  msg : string;

  function GetErrorMessage (code : Integer) : string;
  var
    hErrLib : THandle;
    msg : PChar;
    flags : Integer;

    function MAKELANGID (p, s : word) : Integer;
    begin
      result := (s shl 10) or p
    end;

  begin
    hErrLib := LoadLibraryEx ('netmsg.dll', 0, LOAD_LIBRARY_AS_DATAFILE);

    try

      flags := FORMAT_MESSAGE_ALLOCATE_BUFFER or
               FORMAT_MESSAGE_IGNORE_INSERTS or
               FORMAT_MESSAGE_FROM_SYSTEM;

      if hErrLib <> 0 then
        flags := flags or FORMAT_MESSAGE_FROM_HMODULE;

      if FormatMessage (flags, pointer (hErrLib), code,
                        MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
                        PChar (@msg), 0, Nil) <> 0 then
        try
          result := msg;

        finally
          LocalFree (Integer (msg));
        end

    finally
      if hErrLib <> 0 then
        FreeLibrary (hErrLib)
    end
  end;

begin
  msg := GetErrorMessage (fCode);
  if msg = '' then
    result := Format ('Error %d', [fCode])
  else
    result := Format ('Error %d : %s', [fCode, msg])
end;

{ TSearchNode }

constructor TSearchNode.Create (ARegRoot : HKEY; const APath : string);
begin
  fRegRoot := ARegRoot;
  fValueIDX := -1;
  fKeyIdx := -1;
  fPath := APath
end;

destructor TSearchNode.Destroy;
begin
  fValueNames.Free;
  fKeyNames.Free;
  inherited Destroy
end;

procedure TSearchNode.LoadKeyNames;
var
  r : TExRegistry;
  i : Integer;
begin
  if not Assigned (fKeyNames) then
  begin
    fKeyNames := TStringList.Create;
    r := TExRegistry.Create;
    try
      r.RootKey := fRegRoot;
      r.OpenKey (fPath, False);
      r.GetKeyNames (fKeyNames);
    finally
      r.Free
    end;
    
    for i := 0 to fKeyNames.Count - 1 do
      fKeyNames [i] := UpperCase (fKeyNames [i]);
  end
end;

procedure TSearchNode.LoadValueNames;
var
  r : TExRegistry;
  i : Integer;
begin
  if not Assigned (fValueNames) then
  begin
    fValueNames := TStringList.Create;
    r := TExRegistry.Create;
    try
      r.RootKey := fRegRoot;
      r.OpenKey (fPath, False);
      r.GetValueNames (fValueNames);
    finally
      r.Free
    end;

    for i := 0 to fValueNames.Count - 1 do
      fValueNames [i] := UpperCase (fValueNames [i]);
  end
end;

end.

