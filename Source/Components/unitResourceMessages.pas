(*======================================================================*
 | unitResourceMessages                                                 |
 |                                                                      |
 | Handle string and message resources.                                 |
 |                                                                      |
 | ** Gold code - 24/4/2001 **                                          |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      24/04/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitResourceMessages;

interface

uses
  Windows, Classes, SysUtils, Contnrs, unitResourceDetails;

type
  //-------------------------------------------------------------------------
  // TStringInfo

  TStringInfo = class
  private
    FId: Integer;
    FST: WideString;
  public
    constructor Create(const ASt: WideString; AId: Integer);
    function IsUnicode: Boolean;

    property Id: Integer read FId write FId;
    property St: WideString read FST write FST;
  end;

  //-------------------------------------------------------------------------
  // TTextResourceDetails
  //
  // Base class for messages & strings
  TTextResourceSort = (trString, trID, trReverseString, trReverseID);
  TTextResourceDetails = class (TResourceDetails)
  private
    FStrings: TObjectList;
    FUpdating: Boolean;
    function GetCount: Integer;
    procedure GetStrings;
    function GetString(idx: Integer): WideString;
    function GetId(idx: Integer): Integer;
    procedure SetId(idx: Integer; const Value: Integer);
    procedure SetString(idx: Integer; const Value: WideString);
  protected
    procedure DecodeStrings; virtual; abstract;
    procedure EncodeStrings; virtual; abstract;
  public
    destructor Destroy; override;
    procedure ChangeData(newData: TMemoryStream); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Delete(idx: Integer);
    procedure Sort(sortType: TTextResourceSort = trString);
    function IndexOfID (id: Integer): Integer;

    property Count: Integer read GetCount;
    property Strings[idx: Integer]: WideString read GetString write SetString; default;
    property Ids[idx: Integer]: Integer read GetId write SetId;
  end;

  //-------------------------------------------------------------------------
  // TMessageResourceDetails

  TMessageResourceDetails = class (TTextResourceDetails)
  protected
    procedure DecodeStrings; override;
    procedure EncodeStrings; override;
    procedure InitNew; override;
  public
    class function GetBaseType: WideString; override;
  end;

  //-------------------------------------------------------------------------
  // TStringResourceDetails

  TStringResourceDetails = class (TTextResourceDetails)
  protected
    procedure DecodeStrings; override;
    procedure EncodeStrings; override;
    procedure InitNew; override;
    procedure SetResourceName(const Value: WideString); override;
  public
    class function GetBaseType: WideString; override;
  end;

//-------------------------------------------------------------------------
// Global functions declarations

function ResIdToStringsId (const resourceName: string): string;
function StringsIdToResId (const stringsId: string): string;

implementation

type
  TMessageResourceBlock = record
    lowID: DWORD;
    highID: DWORD;
    entryOffset: DWORD;  // Offset to entries from the start of the message resource
  end;
  PMessageResourceBlock = ^TMessageResourceBlock;

  TMessageResourceEntry = packed record
    len: WORD;         // Sizeof the structure(not length of the string!)
    flags: WORD;
    st: array [0..0] of char;
  end;
  PMessageResourceEntry = ^TMessageResourceEntry;

//-------------------------------------------------------------------------
// Global functions definitions

function ResIdToStringsId (const resourceName: string): string;
begin
  Result := IntToStr ((StrToInt(resourceName) - 1) * 16)
end;

function StringsIdToResId (const stringsId: string): string;
begin
  Result := IntToStr (StrToInt(stringsId) div 16 + 1)
end;


{ TStringResourceDetails }

(*----------------------------------------------------------------------*
 | TStringResourceDetails.DecodeStrings                                 |
 |                                                                      |
 | Extract strings from string table into FStrings list                 |
 *----------------------------------------------------------------------*)
procedure TStringResourceDetails.DecodeStrings;
var
  p: PWideChar;
  cnt, id: Integer;
  st: WideString;
begin
  p := PWideChar (Data.Memory);
  cnt := 0;

  while Cnt < 16 do
  begin
    id := (StrToInt(ResourceName) - 1) * 16 + cnt;
    st := ResourceWideCharToWideStr (p);
    FStrings.Add (TStringInfo.Create(st, id));
    Inc(Cnt);
  end
end;

(*----------------------------------------------------------------------*
 | TStringResourceDetails.EncodeStrings                                 |
 |                                                                      |
 | Encode strings from FStrings list into string table                  |
 *----------------------------------------------------------------------*)
procedure TStringResourceDetails.EncodeStrings;
var
  n, i: Integer;
  p: PWideChar;
begin
                                // Calculate total size of the 16 null-terminated
                                // wide strings.
  n := 16 * SizeOf(WideChar);
  for i := 0 to Count - 1 do
    Inc(n, (Length(Strings[i])) * SizeOf(WideChar));

  Data.Size := n;
  p := PWideChar (Data.Memory);
  ZeroMemory(p, n);

  for i := 0 to Count - 1 do
    ResourceWideStrToWideChar (Strings[i], p);
end;

class function TStringResourceDetails.GetBaseType: WideString;
begin
  Result := IntToStr (Integer (RT_STRING));
end;

procedure TStringResourceDetails.InitNew;
var
  i: Integer;
  wc: WideChar;
begin
  wc := #0;
  for i := 0 to 15 do
    Data.Write(wc, SizeOf(wc))
end;

procedure TStringResourceDetails.SetResourceName(const Value: WideString);
var
  i, lid: Integer;

begin
  inherited;

  lid := (StrToInt(Value) - 1) * 16;

  for i := 0 to Count - 1 do
    with TStringInfo (FStrings[i]) do
      FId := lid + i
end;

{ TMessageResourceDetails }

procedure TMessageResourceDetails.DecodeStrings;
var
  i, blockCount, id: Integer;
  Block: PMessageResourceBlock;
  Entry: PMessageResourceEntry;
  p: PChar;
begin
  blockCount := PInteger (Data.Memory)^;

  Block := PMessageResourceBlock(PChar (Data.Memory) + SizeOf(Integer));

  for i := 0 to blockCount - 1 do
  begin
    id := Block^.lowID;
    p := Data.memory;
    Inc(p, Block^.entryOffset);              // 'p' points to the Block's messages

    Entry := PMessageResourceEntry(p);
    while id <= Integer (Block^.highID) do
    begin
                                             // nb.  Entry^.len is the *size*
                                             // of the Entry - not the length
                                             // of the string!

      if (Entry^.flags and 1) = 1 then       // Unicode
        FStrings.Add (TStringInfo.Create(PWideChar (@Entry^.st [0]), id))
      else
        FStrings.Add (TStringInfo.Create(PChar (@Entry^.st [0]), id));

      Entry := PMessageResourceEntry(PChar (Entry) + Entry^.len);
      Inc(id)
    end;
    Inc(Block)
  end
end;

procedure TMessageResourceDetails.EncodeStrings;
var
  i, id, lastId, dataSize, blockCount, len: Integer;
  Block: PMessageResourceBlock;
  Offset: DWORD;
  st: String;
  Unicode: word;
begin
  dataSize := SizeOf(DWORD);   // sizeof initial NumberOfBlocks DWORD;
  lastId := -2;
  blockCount := 0;
  for i := 0 to FStrings.Count - 1 do   // Count non-contiguous blocks & calculate total size
  begin
    id := TStringInfo (FStrings[i]).FId;
    Unicode := Ord (TStringInfo (FStrings[i]).IsUnicode);
    if id <> lastId + 1 then
    begin
      Inc(blockCount);
      Inc(dataSize, SizeOf(TMessageResourceBlock));
    end;

    lastId := id;

    len := Length(Strings[i]) + 1;
    if Unicode <> 0 then
      len := len * 2;
                                        // len is now length of string in
                                        // bytes -including nul terminator.

    Inc(len, 2 * SizeOf(WORD));       // Add sizeof flags & len

    len := (len + 3) div 4 * 4;         // DWORD align

    Inc(dataSize, len)
  end;

  Data.Size := dataSize;
  PInteger (Data.Memory)^ := blockCount;
  Offset := SizeOf(Integer) + blockCount * SizeOf(TMessageResourceBlock);
  blockCount := 0;
  Block := nil;

  lastId := -2;
  for i := 0 to FStrings.Count - 1 do
  begin
    id := TStringInfo (FStrings[i]).FId;
    Unicode := Ord (TStringInfo (FStrings[i]).IsUnicode);
    if id <> lastId + 1 then
    begin
      if lastId <> -2 then
        Block^.highID := lastId;
      Block := PMessageResourceBlock(PChar (Data.Memory) + SizeOf(Integer));
      Inc(Block, blockCount);
      Block^.lowID := id;
      Block^.entryOffset := Offset;
      Inc(blockCount)
    end;

    lastId := id;
    len := Length(Strings[i]) + 1;
    if Unicode <> 0 then
      len := len * 2;

    len := (len + 3) div 4 * 4;

    PWORD (PChar (Data.Memory) + Offset)^ := 2 * SizeOf(word) + len;
    Inc(Offset, SizeOf(WORD));

    PWORD (PChar (Data.Memory) + Offset)^ := Unicode;
    Inc(Offset, SizeOf(WORD));

    ZeroMemory(PChar (Data.Memory) + Offset, len);
    if Unicode = 0 then
    begin
      st := Strings[i];
      lstrcpy(PChar (Data.Memory) + Offset, PChar (st))
    end
    else
      lstrcpyw (PWideChar (PChar (Data.Memory) + Offset), PWideChar (Strings[i]));
    Inc(Offset, len)
  end;
  if lastId <> -2 then
    Block^.highID := lastId;
end;

class function TMessageResourceDetails.GetBaseType: WideString;
begin
  Result := IntToStr (Integer (RT_MESSAGETABLE));
end;

procedure TMessageResourceDetails.InitNew;
const
  zero: Integer = 0;
begin
  Data.Write(zero, SizeOf(zero));
end;

{ TTextResourceDetails }

procedure TTextResourceDetails.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TTextResourceDetails.ChangeData(newData: TMemoryStream);
begin
  inherited;

  FreeAndNil (FStrings)
end;

procedure TTextResourceDetails.Delete(idx: Integer);
begin
  if idx < FStrings.Count then
  begin
    FStrings.Delete(idx);

    if not FUpdating then
      EncodeStrings
  end
end;

destructor TTextResourceDetails.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TTextResourceDetails.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating := False;
    EncodeStrings
  end
end;

function TTextResourceDetails.GetCount: Integer;
begin
  GetStrings;
  Result := FStrings.Count
end;

function TTextResourceDetails.GetId(idx: Integer): Integer;
begin
  GetStrings;
  Result := TStringInfo (FStrings[idx]).FId;
end;

function TTextResourceDetails.GetString(idx: Integer): WideString;
begin
  GetStrings;
  Result := TStringInfo (FStrings[idx]).FST;
end;

procedure TTextResourceDetails.GetStrings;
begin
  if not Assigned(FStrings) then
  begin
    FStrings := TObjectList.Create;
    DecodeStrings
  end
end;

function TTextResourceDetails.IndexOfID(id: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Assigned(FStrings) then
    for i := 0 to FStrings.Count - 1 do
      if TStringInfo (FStrings[i]).FId = id then
      begin
        Result := i;
        break
      end
end;

procedure TTextResourceDetails.SetId(idx: Integer; const Value: Integer);
begin
  with TStringInfo (FStrings[idx]) do
    FId := Value;

  if not FUpdating then
    EncodeStrings
end;

procedure TTextResourceDetails.SetString(idx: Integer;
  const Value: WideString);
begin
  if idx = FStrings.Count then
    FStrings.Add (TStringInfo.Create(Value, idx))
  else
    TStringInfo (FStrings[idx]).FST := Value;

  if not FUpdating then
    EncodeStrings
end;

function CompareStringInfo (p1, p2: Pointer): Integer;
begin
  Result := WideCompareText(TStringInfo (p1).FST, TStringInfo (p2).FST)
end;

function ReverseCompareStringInfo (p1, p2: Pointer): Integer;
begin
  Result := -WideCompareText(TStringInfo (p1).FST, TStringInfo (p2).FST)
end;

function CompareIDS (p1, p2: Pointer): Integer;
begin
  Result := TStringInfo (p1).FId - TStringInfo (p2).FId
end;

function ReverseCompareIDS (p1, p2: Pointer): Integer;
begin
  Result := TStringInfo (p2).FId - TStringInfo (p1).FId
end;

procedure TTextResourceDetails.Sort(sortType: TTextResourceSort);
begin
  case sortType of
    trString       : FStrings.Sort(CompareStringInfo);
    trID           : FStrings.Sort(CompareIDS);
    trReverseString: FStrings.Sort(ReverseCompareStringInfo);
    trReverseID    : FStrings.Sort(ReverseCompareIDS)
  end
end;

{ TStringInfo }

constructor TStringInfo.Create(const ASt: WideString; AId: Integer);
begin
  FST := ASt;
  FId := AId;
end;

function TStringInfo.IsUnicode: Boolean;
begin
  Result := WideCharToMultiByte(CP_UTF8, 0, PWideChar (FST), -1, Nil, 0, Nil, Nil) <> Length(FST) + 1;
end;

initialization
  RegisterResourceDetails(TMessageResourceDetails);
  RegisterResourceDetails(TStringResourceDetails);
finalization
  UnregisterResourceDetails(TStringResourceDetails);
  UnregisterResourceDetails(TMessageResourceDetails);
end.
