unit unitClipExRegistry;

interface

uses Windows, Classes, Sysutils, unitExRegistry;

type
TCopyFormat = (cfRegEdt, cfText);
TCopyFormats = set of TCopyFormat;

TClipExRegistry = class (TExRegistry)
public
  procedure CopyKeyToClipboard (const keyName : string);
  procedure CopyValuesToClipboard (Values : TStrings; fmt : TCopyFormats = [cfRegEdt]);
  function PasteKeyFromClipboard : string;
  procedure PasteValuesFromClipboard (Values : TStrings = nil);
end;

function ClipboardHasRegEdtValue : boolean;
function ClipboardHasRegEdtKey : boolean;

var
  CF_HIVE, CF_VALUES : UINT;

implementation

uses ClipBrd;

type
TKeyData = record
  noValues : DWORD;
  valuesOffset : DWORD;
  noSubkeys : DWORD;
  subKeysOffset : DWORD;
end;
PKeyData = ^TKeyData;

TValueData = record
  dataLen : DWORD;
  valueType : DWORD;
end;
PValueData = ^TValueData;

function ClipboardHasRegEdtValue : boolean;
begin
  result := Clipboard.HasFormat (CF_VALUES)
end;

function ClipboardHasRegEdtKey : boolean;
begin
  result := Clipboard.HasFormat (CF_HIVE)
end;

{ TClipExRegistry }

procedure TClipExRegistry.CopyKeyToClipboard(const keyName: string);
var
  buf : array [0..511] of char;
  ddeBuf : PChar;
  ddeBufPos : DWORD;
  ddeSize : DWORD;
  ddeHandle : THandle;

  function GetRequiredSize (const name : string; key : HKEY) : DWORD;
  var
    cbClass, cbValueName, cbData : DWORD;
    cSubkeys : DWORD;
    i, cValues : DWORD;
    err : DWORD;
    k : HKEY;
  begin
    err := RegOpenKeyEx (key, PChar (name), 0, KEY_READ, k);
    if err = ERROR_SUCCESS then
    try
      result := sizeof (TKeyData) + Length (name) + 1;
      cbClass := 0;
      err := RegQueryInfoKey (k, Nil, @cbClass, Nil, @cSubkeys, Nil, Nil, @cValues, nil, nil, nil, nil);
      if err = ERROR_SUCCESS then
      begin
        Inc (result, cbClass + 1);
        Inc (result, cValues * sizeof (TValueData));

        if cValues > 0 then
          for i := 0 to cValues - 1 do
          begin
            cbValueName := sizeof (buf);
            cbData := 0;
            if RegEnumValue (k, i, buf, cbValueName, Nil, Nil, Nil, @cbData) = ERROR_SUCCESS then
            begin
              Inc (result, cbValueName + 1);
              Inc (result, cbData)
            end
          end;

        if cSubkeys > 0 then
          for i := 0 to cSubkeys - 1 do
            if RegEnumKey (k, i, buf, sizeof (buf)) = ERROR_SUCCESS then
              result := result + GetRequiredSize (buf, k);
      end
    finally
      RegCloseKey (k)
    end
    else
      raise EExRegistryException.Create (err, 'Unable to open key')
  end;

  procedure CopyData (const name : string; key : HKEY);
  var
    cbClass, cbValueName, cbData : DWORD;
    cSubkeys, cValues : DWORD;
    i : DWORD;
    pData : PKeyData;
    pValue : PValueData;
    err : DWORD;
    k : HKEY;
    ddeRootPos : DWORD;
    pkName, pkClass : PChar;
  begin
    err := RegOpenKeyEx (key, PChar (name), 0, KEY_READ, k);
    if err = ERROR_SUCCESS then
    try
      cbClass := sizeof (buf);
      err := RegQueryInfoKey (k, buf, @cbClass, Nil, @cSubkeys, Nil, Nil, @cValues, nil, nil, nil, nil);
      if err <> ERROR_SUCCESS then
        raise EExRegistryException.Create (err, 'Unable to get key info');


      ddeRootPos := ddeBufPos;
      pData := pKeyData (ddeBuf + ddeBufPos);
      Inc (ddeBufPos, sizeof (TKeyData));

      pkName := ddeBuf + ddeBufPos;
      lstrcpy (pkName, PChar (name));
      Inc (ddeBufPos, Length (name) + 1);

      pkClass := PChar (ddeBuf + ddeBufPos);
      lstrcpy (pkClass, buf);
      Inc (ddeBufPos, lstrlen (buf) + 1);

      pData^.noValues := cValues;
      pData^.valuesOffset := ddeBufPos - ddeRootPos;

      if cValues > 0 then
        for i := 0 to cValues - 1 do
        begin
          cbValueName := sizeof (buf);
          cbData := 0;
          err := RegEnumValue (k, i, buf, cbValueName, Nil, Nil, Nil, @cbData);
          if err <> ERROR_SUCCESS then
            raise EExRegistryException.Create (err, 'Unable to enumerate values');

          pValue := PValueData (ddeBuf + ddeBufPos);
          Inc (ddeBufPos, sizeof (TValueData));
          Move (buf [0], (ddeBuf + ddeBufPos)^, cbValueName + 1);
          Inc (ddeBufPos, cbValueName + 1);

          pValue^.dataLen := cbData;
          cbValueName := sizeof (buf);
          err := RegEnumValue (k, i, buf, cbValueName, Nil, @pValue^.valueType, PBYTE (ddeBuf + ddeBufPos), @cbData);
          if err <> ERROR_SUCCESS then
            raise EExRegistryException.Create (err, 'Unable to enumerate values');

          Inc (ddeBufPos, cbData)
        end;

      pData^.noSubKeys := cSubkeys;
      pData^.subKeysOffset := ddeBufPos - ddeRootPos;

      if err = ERROR_SUCCESS then
      begin
        if cSubkeys > 0 then
          for i := 0 to cSubkeys - 1 do
          begin
            err := RegEnumKey (k, i, buf, sizeof (buf));
            if err <> ERROR_SUCCESS then
              raise EExRegistryException.Create (err, 'Unable to enunerate sub-keys');
            CopyData (buf, k)
          end
      end
    finally
      RegCloseKey (k);
    end
    else
      raise EExRegistryException.Create (err, 'Unable to open key')
  end;

begin  // CopyKeyToClipboard
  ddeSize := GetRequiredSize (keyName, CurrentKey);
  ddeHandle := GlobalAlloc (GMEM_MOVEABLE, ddeSize);
  try
    ddeBuf := GlobalLock (ddeHandle);
    try
      ddeBufPos := 0;
      CopyData (keyName, CurrentKey)
    finally
      GlobalUnlock (ddeHandle)
    end;
    Clipboard.SetAsHandle (CF_HIVE, ddeHandle)
  except
    GlobalFree (ddeHandle);
    raise
  end
end;

procedure TClipExRegistry.CopyValuesToClipboard(Values: TStrings; fmt : TCopyFormats = [cfRegEdt]);
var
  ddeSize : DWORD;
  i, err : Integer;
  ddeHandle : THandle;
  ddeBuf : PChar;
  ddeBufPos, cbData : DWORD;
  pValue : PValueData;
  txt : string;
  pName, pData : PChar;
begin
  ddeSize := sizeof (DWORD) + Values.count * sizeof (TValueData);
  for i := 0 to Values.count - 1 do
  begin
    err := RegQueryValueEx (CurrentKey, PChar (Values [i]), Nil, Nil, Nil, @cbData);
    if (Values [i] = '') and (err = ERROR_FILE_NOT_FOUND) then
      cbData := 0
    else
      if err <> ERROR_SUCCESS then
        raise EExRegistryException.Create (err, 'Unable to get value info');
    Inc (ddeSize, DWORD (Length (Values [i])) + 1 + cbData)
  end;

  ddeHandle := GlobalAlloc (GMEM_MOVEABLE, ddeSize);
  try
    if cfText in fmt then
      StartExport;
    ddeBuf := GlobalLock (ddeHandle);
    try
      ddeBufPos := 0;
      PDWORD (ddeBuf)^ := Values.count;
      Inc (ddeBufPos, sizeof (DWORD));

      for i := 0 to Values.count - 1 do
      begin
        pValue := PValueData (ddeBuf + ddeBufPos);
        Inc (ddeBufPos, sizeof (TValueData));
        pName := ddeBuf + ddeBufPos;
        Move (PChar (Values [i])^, pName^, Length (Values [i]) + 1);
        Inc (ddeBufPos, Length (Values [i]) + 1);
        pData := ddeBuf + ddeBufPos;

        pValue^.dataLen := ddeSize - ddeBufPos;

        err := RegQueryValueEx (CurrentKey, PChar (Values [i]), Nil, @pValue^.valueType, PBYTE (ddeBuf + ddeBufPos), @pValue^.dataLen);
        if not ((Values [i] = '') and (err = ERROR_FILE_NOT_FOUND)) then
        begin
          if err <> ERROR_SUCCESS then
            raise EExRegistryException.Create (err, 'Unable to query value info.');
          Inc (ddeBufPos, pValue^.dataLen);
          if cfText in fmt then
            ExportProc ('', pName, pValue^.valueType, pData, pValue^.dataLen)

        end
        else
          pValue^.dataLen := 0
      end
    finally
      GlobalUnlock (ddeHandle);
      txt := EndExport
    end;
    Clipboard.Open;
    try
      if cfRegEdt in fmt then
        Clipboard.SetAsHandle (CF_VALUES, ddeHandle)
      else
      begin
        GlobalFree (ddeHandle);
        ddeHandle := 0
      end;

      if cfText in fmt then
        Clipboard.AsText := txt
    finally
      Clipboard.Close
    end;
  except
    if ddeHandle <> 0 then
      GlobalFree (ddeHandle);
    raise
  end
end;


function TClipExRegistry.PasteKeyFromClipboard: string;
var
  ddeHandle : THandle;
  ddeBuf : PChar;

  function PasteKey (keyData : PKeyData; destKey : HKEY) : DWORD;
  var
    err, disposition : DWORD;
    k : HKEY;
    i : Integer;
    valueData : PValueData;
    subKeyData : PKeyData;
    len : DWORD;
    pName, pData, pkName, pkClass : PChar;
  begin
    result := sizeof (TKeyData);
    pkName := PChar (keyData) + result;
    pkClass := pkName + lstrlen (pkName) + 1;
    Inc (result, lstrlen (pkName) + lstrlen (pkClass) + 2);
    err := RegCreateKeyEx (destKey, pkName, 0, pkClass, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, Nil, k, @disposition);
    if err = ERROR_SUCCESS then
    try
      valueData := PValueData (PChar (keyData) + keyData^.valuesOffset);
      if keyData^.noValues > 0 then
        for i := 0 to keyData^.noValues - 1 do
        begin
          pName := PChar (valueData) + sizeof (TValueData);
          pData := pName + lstrlen (pName) + 1;
          err := RegSetValueEx (k, pName, 0, valueData^.valueType, pData, valueData^.dataLen);
          if err <> ERROR_SUCCESS then
            raise EExRegistryException.Create (err, 'Unable to set value');
          len := sizeof (TValueData) + DWORD (lstrlen (pName)) + 1 + valueData^.dataLen;
          valueData := PValueData (PChar (valueData) + len);
          Inc (result, len);
        end;

      subKeyData := PKeyData (PChar (keyData) + keyData^.subKeysOffset);
      if keyData^.noSubkeys > 0 then
        for i := 0 to keyData^.noSubkeys - 1 do
        begin
          len := PasteKey (subKeyData, k);
          subKeyData := PKeyData (PChar (subKeyData) + len);
          Inc (result, len);
        end
    finally
      RegCloseKey (k)
    end
    else
      raise EExRegistryException.Create (err, 'Unable to open key')
  end;

begin
  ddeHandle := Clipboard.GetAsHandle (CF_HIVE);
  if ddeHandle <> INVALID_HANDLE_VALUE then
  begin
    ddeBuf := GlobalLock (ddeHandle);
    try
      PasteKey (PKeyData (ddeBuf), CurrentKey);
      result := ddeBuf + sizeof (TKeyData);
    finally
      GlobalUnlock (ddeHandle)
    end
  end
end;


procedure TClipExRegistry.PasteValuesFromClipboard(Values: TStrings = nil);
var
  ddeHandle : THandle;
  ddeBuf, p : PChar;
  count, ddeBufPos, i : DWORD;
  value : PValueData;
  err : Integer;
  pName, pData : PChar;
begin
  if Assigned(Values) then
    Values.Clear;
  ddeHandle := Clipboard.GetAsHandle (CF_VALUES);
  if ddeHandle <> INVALID_HANDLE_VALUE then
  begin
    ddeBuf := GlobalLock (ddeHandle);
    try
      p := ddeBuf;
      count := PDWORD (p)^;
      Inc (p, sizeof (DWORD));
      ddeBufPos := 0;

      for i := 0 to count - 1 do
      begin
        value := PValueData (p + ddeBufPos);
        Inc (ddeBufPos, sizeof (TValueData));
        pName := p + ddeBufPos;

        Inc (ddeBufPos, lstrlen (pName) + 1);
        pData := p + ddeBufPos;
        Inc (ddeBufPos, value^.dataLen);
        err := RegSetValueEx (CurrentKey, pName, 0, value^.valueType, pData, value^.dataLen);
        if err <> ERROR_SUCCESS then
          raise EExRegistryException.Create (err, 'Unable to set value');
        if Assigned(Values) then
          Values.Add (pName);
      end

    finally
      GlobalUnlock (ddeHandle)
    end
  end
end;


initialization
  CF_HIVE := RegisterClipboardFormat ('RegEdtHive');
  CF_VALUES := RegisterClipboardFormat ('RegEdtValues');
end.
