unit unitResourceMenus;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceDetails, Menus;

type
TMenuResourceDetails = class (TResourceDetails)
  private
    fHelpID: Integer;                    // Extended menu's help ID
  protected
    constructor Create(AParent: TResourceModule; ALanguage: Integer; const AName, AType: WideString; ASize: Integer; AData: pointer); override;

  public
    destructor Destroy; override;

    class function GetBaseType: WideString; override;
    procedure ChangeData(newData: TMemoryStream); override;

    procedure InitNew; override;
    procedure GetItems (items: TMenuItem);
    procedure SetItems (items: TMenuItem);
end;

implementation

type
  TMenuExTemplateHeader = packed record
    wVersion: word;
    wOffset: word;
    dwHelpId: DWORD;
  end;
  PMenuExTemplateHeader = ^TMenuExTemplateHeader;

  TMenuHeader = packed record
    wVersion: word;
    cbHeaderSize: word;
  end;
  PMenuHeader = ^TMenuHeader;

  TNormalMenuItem = packed record
    resInfo: word;
    menuText: array [0..10] of WideChar;
  end;
  PNormalMenuItem = ^TNormalMenuItem;

{ TMenuResourceDetails }

procedure TMenuResourceDetails.ChangeData(newData: TMemoryStream);
begin
  inherited;

end;

constructor TMenuResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: WideString; ASize: Integer;
  AData: pointer);
begin
  inherited Create(AParent, ALanguage, AName, AType, ASize, AData);
end;

destructor TMenuResourceDetails.Destroy;
begin
  inherited;
end;

class function TMenuResourceDetails.GetBaseType: WideString;
begin
  Result := IntToStr (Integer (RT_MENU));
end;

procedure TMenuResourceDetails.GetItems(items: TMenuItem);
var
  p: PChar;
  owner: TComponent;

  procedure GetNormalItems (rootItem: TMenuItem);
  var
    flags, id: word;
    caption: WideString;
    item: TMenuItem;
  begin
    repeat
      flags := PWord (p)^;
      Inc(p, sizeof (word));
      if (flags and MF_POPUP) <> 0 then
      begin
        caption := PWideChar (p);
        Inc(p, (lstrlenW (PWideChar (p)) + 1) * sizeof (WideChar));
        item := TMenuItem.Create(owner);
        GetNormalItems (item)
      end
      else
      begin
        id := PWord (p)^;
        Inc(p, sizeof (word));
        caption := PWideChar (p);
        if caption = '' then
          caption := '-';
        Inc(p, (lstrlenW (PWideChar (p)) + 1) * sizeof (WideChar));
        item := TMenuItem.Create(owner);
        if caption = '-' then
          item.Tag := -1
        else
          item.Tag := id
      end;

      item.Caption := Utf8Encode(caption);

      if (flags and MF_GRAYED) <> 0 then
        item.Enabled := False;

      if (flags and MF_CHECKED) <> 0 then
        item.Checked := True;

      rootItem.Add (item)
    until (flags and MF_END) <> 0
  end;


  procedure GetExtendedItems (rootItem: TMenuItem);
  var
    tp: DWORD;
    state: DWORD;
    uID: UINT;
    bResInfo: word;
    caption: WideString;
    helpID: DWORD;
    item: TMenuItem;

  begin
    repeat
      tp := PDWORD (p)^;
      Inc(p, sizeof (DWORD));
      state := PDWORD (p)^;
      Inc(p, sizeof (DWORD));
      uID := PUINT (p)^;
      Inc(p, sizeof (UINT));
      bResInfo := PWORD (p)^;
      Inc(p, sizeof (word));

      if (tp and MFT_SEPARATOR) = 0 then
      begin
        caption := PWideChar (p);
        Inc(p, (lstrlenW (PWideChar (p)) + 1) * sizeof (WideChar));
      end
      else
        caption := '-';

      while(Integer (p) mod 4) <> 0 do
        Inc(p);

      if (bResInfo and $01) <> 0 then
      begin
        helpID := PDWORD (p)^;
        Inc(p, sizeof (PDWORD))
      end
      else
        helpID := 0;

      item := TMenuItem.Create(owner);
      item.Tag := uID;
      item.Caption := UTF8Encode(caption);
      item.HelpContext := helpID;

      if (state and MFS_DISABLED) <> 0 then
        item.Enabled := False;

      if (state and MFS_CHECKED) <> 0 then
        item.Checked := True;

      if (bResInfo and $01) <> 0 then
        GetExtendedItems (item);
      rootItem.Add (item);
    until (bResInfo and $80) <> 0;
  end;

begin
  p := PChar (Data.Memory);
  owner := Items.Owner;

  case PMenuHeader (p)^.wVersion of
    0 :
      begin
        Inc(p, Sizeof (TMenuHeader));
        GetNormalItems (items);
      end;

    1 :
      begin
        if PMenuHeader (p)^.cbHeaderSize = 4 then
        begin
          Inc(p, SizeOf (TMenuHeader));
          fHelpID := PDWORD (p)^;
          Inc(p, Sizeof (DWORD));
          GetExtendedItems (items);
        end
      end
  end
end;

procedure TMenuResourceDetails.InitNew;
var
  p: PChar;
begin
  Data.SetSize(sizeof (TMenuExTemplateHeader) + 20);
  ZeroMemory(data.Memory, data.Size);

  PMenuExTemplateHeader (data.Memory)^.wVersion := 1;
  PMenuExTemplateHeader (data.Memory)^.wOffset := 4;  // 4?  Don't ask!  But look up MENUEXTEMPLATEHEADER in MSDN!

  p := PChar (data.Memory) + sizeof (TMenuExTemplateHeader);

//  Menus must have at least one item.  Set up first item.

  PDWORD (p)^ := MFT_STRING;    // dwType
  Inc(p, sizeof (DWORD));

  PDWORD (p)^ := MFS_ENABLED;   // dwState
  Inc(p, sizeof (DWORD));

  PUINT (p)^ := 0;              // uId;
  Inc(p, sizeof (UINT));

  PWORD (p)^ := $80;            // bResInfo (word for 32-bit OS)
  Inc(p, sizeof (WORD));

  PWideChar (p)^ := #0;         // szText
  Inc(p, sizeof (WideChar));

  PDWORD (p)^ := 0;             // dwHelpID
end;

procedure TMenuResourceDetails.SetItems(items: TMenuItem);
var
  st: TMemoryStream;
  offset: Integer;
  i: Integer;

  procedure SaveOldStyleMenu(rootItem: TMenuItem; lastItem: Boolean);
  var
    flags: word;
    id: word;
    i: Integer;
    wCaption: WideString;
    c: byte;
  begin
    flags := 0;
    c := 0;
    if rootItem.Count > 0 then
      flags := flags or MF_POPUP;

    if not rootItem.Enabled then
      flags := flags or MF_GRAYED;

    if rootItem.Checked then
      flags := flags or MF_CHECKED;

    if lastItem then
      flags := flags or MF_END;

    st.Write(flags, sizeof (flags));
    if rootItem.Count = 0 then
    begin
      if rootItem.Caption = '-' then
        id := 0
      else
        id := rootItem.Tag;
      st.Write(id, sizeof (id))
    end;

    if rootItem.Caption = '-' then
      wCaption := ''
    else
      wCaption := Utf8Decode(rootItem.caption);

    if wCaption = '' then
    begin
      st.Write(c, 1);
      st.Write(c, 1)
    end
    else
      st.Write(PWideChar (wCaption)^, (Length (rootItem.Caption) + 1) * sizeof (WideChar));

    for i := 0 to rootItem.Count - 1 do
      SaveOldStyleMenu(rootItem.Items[i], i = rootItem.Count - 1)
  end;

  procedure SaveNewStyleMenu(rootItem: TMenuItem; lastItem: Boolean);
  var
    tp: DWORD;
    state: DWORD;
    uID: UINT;
    bResInfo: word;
    wCaption: WideString;
    helpID: DWORD;
    c: byte;
    i: Integer;

  begin
    tp := 0;
    c := 0;

    if rootItem.Caption = '-' then
      tp := tp or MFT_SEPARATOR;

    state := 0;
    if not rootItem.Enabled then
      state := state or MFS_DISABLED;

    if rootItem.Checked then
      state := state or MFS_CHECKED;

    uID := rootItem.Tag;

    bResInfo := 0;
    if rootItem.Count > 0 then
      bResInfo := bResInfo or $01;

    if lastItem then
      bResInfo := bResInfo or $80;

    st.Write(tp, sizeof (tp));
    st.Write(state, sizeof (state));
    st.Write(uID, sizeof (uID));
    st.Write(bresInfo, sizeof (bResInfo));

    if (tp and MFT_SEPARATOR) = 0 then
    begin
      wCaption := Utf8Decode(rootItem.caption);

      if wCaption = '' then
      begin
        st.Write(c, 1);
        st.Write(c, 1)
      end
      else
        st.Write(wCaption [1], (Length (wCaption) + 1) * sizeof (WideChar));
    end;

    while(st.Size mod 4) <> 0 do
      st.Write(c, 1);

    if (bResInfo and $01) <> 0 then
    begin
      helpID := rootItem.HelpContext;
      st.Write(helpID, sizeof (helpID))
    end;

    for i := 0 to rootItem.Count - 1 do
      SaveNewStyleMenu(rootItem.Items[i], i = rootItem.Count - 1)
  end;

begin
  offset := 0;
  st := TMemoryStream.Create;
  try
    data.Seek(0, soFromBeginning);
    st.CopyFrom (data, sizeof (TMenuHeader));

    case PMenuHeader (st.Memory)^.wVersion of
      0: for i:= 0 to items.Count - 1 do
            SaveOldStyleMenu(items.Items[i], i = items.Count - 1);
      1 :
        begin
          st.Write(fHelpId, sizeof (fHelpID));
          for i := 0 to items.Count - 1 do
            SaveNewStyleMenu(items.Items[i], i = items.Count - 1);
        end
    end;

    st.Seek(offset, soFromBeginning);
    data.Seek(0, soFromBeginning);
    data.size := 0;
    data.CopyFrom (st, st.Size - offset);
  finally
    st.Free;
  end;
end;

initialization
  RegisterResourceDetails (TMenuResourceDetails);
finalization
  UnregisterResourceDetails (TMenuResourceDetails);
end.


