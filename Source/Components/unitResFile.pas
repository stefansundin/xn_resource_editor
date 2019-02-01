unit unitResFile;

interface

uses
  Windows, Classes, SysUtils, Contnrs, unitResourceDetails;

type
  TResourceList = class (TResourceModule)
  private
    FResourceList: TObjectList;
  protected
    function GetResourceCount: Integer; override;
    function GetResourceDetails(Index: Integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign (Src: TResourceModule);
    procedure InsertResource(Index: Integer; details: TResourceDetails); override;
    procedure DeleteResource(Index: Integer); override;
    function AddResource(details: TResourceDetails): Integer; override;
    function IndexOfResource(details: TResourceDetails): Integer; override;
    procedure SortResources; override;
  end;

  TResModule = class (TResourceList)
  private
    F16Bit: Boolean;
    procedure ParseResource(Header, data: PChar; DataSize: Integer);
  public
    procedure SaveToStream (Stream: TStream); override;
    procedure LoadFromStream (Stream: TStream); override;
  end;

implementation

{ TResModule }

procedure TResModule.ParseResource(Header, data: PChar; DataSize: Integer);
var
  p: PChar;
  sName, sType: WideString;
  ResourceDetails: TResourceDetails;
  language, memoryFlags: word;
  version, dataVersion, Characteristics: DWORD;

  function GetName: WideString;
  begin
    if PWord (p)^ = $ffff then
    begin
      Inc(p, SizeOf(word));
      Result := IntToStr (PWord (p)^);
      Inc(p, SizeOf(word))
    end
    else
    begin
      Result := WideString (PWideChar (p));
      Inc(p, (Length(Result) + 1) * SizeOf(WideChar))
    end
  end;

begin
  try
    p := Header;
    Inc(p, 2 * SizeOf(Integer));
    sType := GetName;
    sName := GetName;

    if (Integer (p) mod 4) <> 0 then
      Inc(p, 4 - Integer (p) mod 4);

    dataVersion := PDWORD (p)^;
    Inc(p, SizeOf(DWORD));
    memoryFlags := PWORD (p)^;
    Inc(p, SizeOf(word));
    language := PWORD (p)^;
    Inc(p, SizeOf(word));
    version := PDWORD (p)^;
    Inc(p, SizeOf(DWORD));
    Characteristics := PDWORD (p)^;
    Inc(p, SizeOf(DWORD));

    if (DataSize <> 0) or (sName <> '0') then
    begin
      ResourceDetails := TResourceDetails.CreateResourceDetails(Self, language, sName, sType, DataSize, data);
      ResourceDetails.Characteristics := Characteristics;
      ResourceDetails.Version := version;
      ResourceDetails.MemoryFlags := memoryFlags;
      ResourceDetails.DataVersion := dataVersion;
      AddResource(ResourceDetails)
    end
    else       // NB!!!  32 bit .ResourceDetails files start with a dummy '32-bit indicator'
               // resource !!!  Is this documented?  I don't think so!

      F16Bit := False;
  except
    raise Exception.Create('The resource file is corrupt');

  end;
end;

procedure TResModule.LoadFromStream(Stream: TStream);
var
  buffer, p, q: PChar;
  bufLen, n, DataSize, HeaderSize, ChunkSize: Integer;
begin
  bufLen := Stream.Size;
  GetMem (buffer, bufLen);
  try
    Stream.ReadBuffer (buffer^, bufLen);             // Read the entite file

    p := buffer;
    n := 0;
    F16Bit := True;
                                              // Parse each resource
    while n + 2 * SizeOf(Integer) < bufLen do
    begin
      DataSize := PInteger (p)^;
      q := p;
      Inc(q, SizeOf  (Integer));
      HeaderSize := PInteger (q)^;
      q := p;
      Inc(q, HeaderSize);

      ParseResource(p, q, DataSize);
      ChunkSize := DataSize + HeaderSize;
      ChunkSize := ((ChunkSize + 3) div 4) * 4;
      Inc(p, ChunkSize);
      Inc(n, ChunkSize);
    end;

  finally
    FreeMem (buffer)
  end;
  SortResources
end;

procedure TResModule.SaveToStream(Stream: TStream);
var
  ResourceDetail: TResourceDetails;
  DataSize, HeaderSize, TotalSize: Integer;
  Header: array [0..1023] of char;
  i: Integer;

  function GetResHeader (Header: PChar): DWORD;
  var
    pos: DWORD;
    len, dw: DWORD;
    w: word;
    i: Integer;
    ws: WideString;
  begin
    pos := 0;
    ZeroMemory(Header, 1024);

    i := ResourceNameToInt(ResourceDetail.ResourceType);
    if i = -1 then
    begin
      ws := ResourceDetail.ResourceType;
      len := (Length(ws) + 1) * SizeOf(WideChar);
      Move(PWideChar (ws)^, Header [pos], len);
      Inc(pos, len)
    end
    else
    begin
      w := $ffff;
      Move(w, Header [pos], SizeOf(w));
      Inc(pos, SizeOf(w));

      w := Word (i);
      Move(w, Header [pos], SizeOf(w));
      Inc(pos, SizeOf(w))
    end;

    i := ResourceNameToInt(ResourceDetail.ResourceName);
    if i = -1 then
    begin
      ws := ResourceDetail.ResourceName;
      len := (Length(ws) + 1) * SizeOf(WideChar);
      Move(PWideChar (ws)^, Header [pos], len);
      Inc(pos, len)
    end
    else
    begin
      w := $ffff;
      Move(w, Header [pos], SizeOf(w));
      Inc(pos, SizeOf(w));

      w := Word (i);
      Move(w, Header [pos], SizeOf(w));
      Inc(pos, SizeOf(w))
    end;

    if (pos mod 4) <> 0 then
      Inc(pos, 4 - (pos mod 4));

    dw := ResourceDetail.DataVersion;
    Move(dw, Header [pos], SizeOf(DWORD));
    Inc(pos, SizeOf(DWORD));

    w := ResourceDetail.MemoryFlags;
    Move(w, Header [pos], SizeOf(WORD));
    Inc(pos, SizeOf(WORD));

    w := ResourceDetail.ResourceLanguage;
    Move(w, Header [pos], SizeOf(WORD));
    Inc(pos, SizeOf(WORD));

    dw := ResourceDetail.Version;
    Move(dw, Header [pos], SizeOf(DWORD));
    Inc(pos, SizeOf(DWORD));

    dw := ResourceDetail.Characteristics;
    Move(dw, Header [pos], SizeOf(DWORD));
    Inc(pos, SizeOf(DWORD));
    Result := pos;
  end;

begin
  if not F16Bit then               // Write 32-bit resource indicator (An empty type 0 resource)
  begin
    ResourceDetail := TResourceDetails.CreateNew (nil, 0, '0');
    try
      DataSize := ResourceDetail.Data.Size;

      Stream.WriteBuffer (DataSize, SizeOf(DataSize));
      HeaderSize := GetResHeader (Header);

      TotalSize := HeaderSize + 2 * SizeOf(DWORD);

      Stream.WriteBuffer (TotalSize, SizeOf(HeaderSize));
      Stream.WriteBuffer (Header, HeaderSize);
    finally
      ResourceDetail.Free
    end
  end;

  DataSize := 0;
  if ResourceCount > 0 then
    for i := 0 to ResourceCount - 1 do
    begin
      ResourceDetail := ResourceDetails[i];
      DataSize := ResourceDetail.Data.Size;

      Stream.WriteBuffer (DataSize, SizeOf(DataSize));
      HeaderSize := GetResHeader (Header);

      TotalSize := HeaderSize + 2 * SizeOf(DWORD);

      Stream.WriteBuffer (TotalSize, SizeOf(HeaderSize));
      Stream.WriteBuffer (Header, HeaderSize);
      Stream.WriteBuffer (ResourceDetail.Data.Memory^, DataSize);

      TotalSize := DataSize + TotalSize;
      ZeroMemory(@Header, SizeOf(Header));

      if (TotalSize mod 4) <> 0 then
        Stream.WriteBuffer (Header, 4 - (TotalSize mod 4));
    end
end;

{ TResourceList }

function TResourceList.AddResource(details: TResourceDetails): Integer;
begin
  Result := FResourceList.Add (details);
end;

procedure TResourceList.Assign(Src: TResourceModule);
var
  i: Integer;
  ResourceDetails: TResourceDetails;
begin
  FResourceList.Clear;

  for i := 0 to Src.ResourceCount - 1 do
  begin
    ResourceDetails := TResourceDetails.CreateResourceDetails (
      Self,
      Src.ResourceDetails[i].ResourceLanguage,
      Src.ResourceDetails[i].ResourceName,
      Src.ResourceDetails[i].ResourceType,
      Src.ResourceDetails[i].Data.Size,
      Src.ResourceDetails[i].Data.Memory);

    FResourceList.Add (ResourceDetails)
  end
end;

constructor TResourceList.Create;
begin
  FResourceList := TObjectList.Create;
end;

procedure TResourceList.DeleteResource(Index: Integer);
var
  ResourceDetail: TResourceDetails;
begin
  ResourceDetail := ResourceDetails[Index];
  inherited;
  Index := IndexOfResource(ResourceDetail);
  if Index <> -1 then
    FResourceList.Delete(Index)
end;

destructor TResourceList.Destroy;
begin
  FResourceList.Free;
  inherited;
end;

function TResourceList.GetResourceCount: Integer;
begin
  Result := FResourceList.Count
end;

function TResourceList.GetResourceDetails(Index: Integer): TResourceDetails;
begin
  Result := TResourceDetails (FResourceList [Index])
end;

function TResourceList.IndexOfResource(details: TResourceDetails): Integer;
begin
  Result := FResourceList.IndexOf (details)
end;

procedure TResourceList.InsertResource(Index: Integer;
  details: TResourceDetails);
begin
  FResourceList.Insert(Index, details)
end;

procedure TResourceList.SortResources;
begin
  FResourceList.Sort(compareDetails);
end;

end.
