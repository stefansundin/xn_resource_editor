unit unitStreamTextReader;

interface

uses
  Windows, Classes, SysUtils;

type
  TStreamTextReader = class
  private
    FStream: TStream;
    FBuffer: PChar;

    FBufPos: Integer;
    FBufSize: Integer;
    FBlockSize: Integer;

    procedure GetChunk;
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    procedure SetStream(const Value: TStream);

  public
    constructor Create(AStream: TStream; blockSize: Integer = 1024);
    destructor Destroy; override;
    function GetChar: Char;
    function ReadLn (var st: string; continuationChar: Char = #0): Boolean;
    procedure ReadChunk(var chunk; offset, length: Integer);
    property Position: Integer read GetPosition write SetPosition;
    function Search (const st: string): Integer;
    property Stream: TStream read FStream write SetStream;
  end;

  TStreamWideTextReader = class
  private
    FStream: TStream;
    FBuffer: PWideChar;

    FBufPos: Integer;
    FBufSize: Integer;
    FBlockSize: Integer;

    procedure GetChunk;
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    procedure SetStream(const Value: TStream);

  public
    constructor Create(AStream: TStream; blockSize: Integer = 1024);
    destructor Destroy; override;
    function GetChar: WideChar;
    function ReadLn (var st: WideString; continuationChar: WideChar = #0): Boolean;
    property Position: Integer read GetPosition write SetPosition;
    property Stream: TStream read FStream write SetStream;
  end;

  TMappedFile = class
  private
    FSize: Integer;
    FMemory: PChar;
    FFileHandle, FMappingHandle: THandle;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    property Size: Integer read FSize;
    property Memory: PChar read FMemory;
  end;

  TTextFileReader = class (TMappedFile)
  private
    FPosition: Integer;
  public
    function ReadLn (var st: string): Boolean;
  end;

  TBufferedFileWriter = class
  private
    FStream: TFileStream;
    FBuffer: PChar;
    FBufPos, FBufSize: Integer;
    function GetPosition: Integer;

  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Write(const data; dataLen: Integer);

    procedure FlushBuffer;
    property Position: Integer read GetPosition;
  end;

  TTextFileWriter = class (TBufferedFileWriter)
  public
    procedure Write(const st: string);
    procedure WriteLn (const st: string);
  end;


implementation

uses
  unitSearchString;

(*----------------------------------------------------------------------*
 | StrLScan                                                             |
 |                                                                      |
 | Search for a character in the first 'len' bytes of the specified     |
 | block of memory                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   const Str: PChar; ch: Char; len: DWORD                           |
 |                                                                      |
 | The function returns PChar                                           |
 *----------------------------------------------------------------------*)
function StrLScan(const Str: PChar; ch: Char; len: DWORD): PChar;
asm
        // EAX = Str
        // DL = Char
        // ECX = len
        
        PUSH    EDI
        MOV     EDI, EAX
        MOV     AL, DL
        REPNE   SCASB
        JNE     @1              // Fail
        MOV     EAX, EDI
        DEC     EAX
        POP     EDI
        RET

@1:     XOR     EAX, EAX
        POP     EDI
end;

{ TStreamTextReader }

constructor TStreamTextReader.Create(AStream: TStream; blockSize: Integer);
begin
  FStream := AStream;
  FBlockSize := blockSize;
  GetMem (FBuffer, FBlockSize + 1);
  FBuffer [blockSize] := #0
end;

destructor TStreamTextReader.Destroy;
begin
  FreeMem (FBuffer);
  inherited;
end;

function TStreamTextReader.GetChar: Char;
begin
  GetChunk;
  if FBufPos < FBufSize then
  begin
    Result := FBuffer [FBufPos];
    Inc(FBufPos)
  end
  else
    Result := #0
end;

procedure TStreamTextReader.GetChunk;
begin
  if FBufPos = FBufSize then
  begin
    FBufPos := 0;
    FBufSize := FBlockSize;
    if FBufSize > FStream.Size - FStream.Position then
      FBufSize := FStream.Size - FStream.Position;

    FBufSize := FStream.Read(FBuffer^, FBufSize);
    if FBufSize < FBlockSize then
      FBuffer [FBufSize] := #10
  end;
end;

function TStreamTextReader.GetPosition: Integer;
begin
  Result := FStream.Position - FBufSize + FBufPos;
end;

procedure TStreamTextReader.ReadChunk(var chunk; offset, length: Integer);
begin
  Position := offset;
  FStream.Read(chunk, length)
end;

function TStreamTextReader.ReadLn(var st: string; continuationChar: Char): Boolean;
var
  l, lineStartPos: Integer;
  pch, pch1: PChar;
  st1: string;
  cont, scont: Boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;
  st := '';
  while cont do
  begin
    GetChunk;
    if FBufPos = FBufSize then
      break;

    pch := FBuffer;
    Inc(pch, FBufPos);
    pch1 := StrScan (pch, #10);

    if pch1 <> nil then
    begin
      l := Integer (pch1) - Integer (pch);
      Inc(FBufPos, l + 1);
      if FBufPos > FBufSize then
        FBufPos := FBufSize;
      if l > 0 then
      begin
        repeat
          Dec(pch1);
          if pch1^ = #13 then
            Dec(l)
          else
            break
        until pch1 = pch;
        cont := pch1^ = continuationChar
      end
      else
        cont := scont;
      SetLength (st1, l);
      if l > 0 then
        Move(pch^, PChar (st1)^, l);
    end
    else
    begin
      st1 := pch;
      l := Length (st1);

      while(l > 0) and (st1 [l] = #13) do
        Dec(l);

      if l < Length (st1) then
        SetLength (st1, l);

      scont := (l > 0) and (st1 [l] = continuationChar);

      FBufPos := FBufSize
    end;

    st := st + st1
  end;

  if cont then
  begin
    Position := lineStartPos;
    Result := False
  end
  else
    Result := True;
end;

function TStreamTextReader.Search(const st: string): Integer;
var
  p, p1: PChar;
begin
  Result := -1;
  if Length (st) <> 0 then
  repeat
    GetChunk;
    if FBufSize < Length (st) then
      Exit;

    p := FBuffer;
    Inc(p, FBufPos);

    p1 := StrPos (p, PChar (st));

    if p1 <> Nil then
    begin
      Result := (FStream.Position - FBufSize) + Integer (p1) - Integer (FBuffer);
      FBufPos := Integer (p1) - Integer (FBuffer) + Length (st);
      break
    end;

    FStream.Position := FStream.Position - (Length (st) - 1);
    FBufPos := 0;
    FBufSize := 0;
  until False
end;

procedure TStreamTextReader.SetPosition(const Value: Integer);
begin
  if Value <> Position then
  begin
    FBufSize := 0;
    FBufPos := 0;
    FStream.Position := Value
  end
end;

procedure TStreamTextReader.SetStream(const Value: TStream);
begin
  FStream := Value;
  FBufPos := 0;
  FBufSize := 0
end;

{ TMappedFile }

constructor TMappedFile.Create(const AFileName: string);
begin
  FFileHandle := CreateFile(PChar (AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  FSize := GetFileSize(FFileHandle, nil);
  if FSize <> 0 then
  begin
    FMappingHandle := CreateFileMapping (FFileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if FMappingHandle = 0 then
      RaiseLastOSError;

    FMemory := MapViewOfFile(FMappingHandle, FILE_MAP_READ, 0, 0, 0);
    if FMemory = Nil then
      RaiseLastOSError;
  end
end;

destructor TMappedFile.Destroy;
begin
  if FMemory <> Nil then
    UnmapViewOfFile(FMemory);

  if FMappingHandle <> 0 then
    CloseHandle(FMappingHandle);

  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);

  inherited;
end;

{ TTextFileReader }

function TTextFileReader.ReadLn(var st: string): Boolean;
var
  p, p1: PChar;
  l: Integer;
begin
  l := FSize - FPosition;
  if l > 0 then
  begin
    Result := True;
    p1 := FMemory + FPosition;
    p := StrLScan (p1, #10, l);

    if p <> Nil then
    begin
      l := Integer (p) - Integer (p1);
      Inc(FPosition, l+1);

      while(l > 0) and (p1 [l - 1] = #13) do
        Dec(l)
    end
    else
      Inc(FPosition, l);

    SetString (st, p1, l)
  end
  else
    Result := False;
end;

{ TTextFileWriter }

procedure TTextFileWriter.Write(const st: string);
begin
  inherited Write(st [1], Length (st));
end;

procedure TTextFileWriter.WriteLn(const st: string);
begin
  Write(st + #13#10)
end;

{ TStreamWideTextReader }

constructor TStreamWideTextReader.Create(AStream: TStream;
  blockSize: Integer);
begin
  FStream := AStream;
  FBlockSize := blockSize;
  GetMem (FBuffer, (FBlockSize + 1) * sizeof (WideChar));
  FBuffer [blockSize] := #0;
  Position := 0;
end;

destructor TStreamWideTextReader.Destroy;
begin
  FreeMem (FBuffer);
  inherited;
end;

function TStreamWideTextReader.GetChar: WideChar;
begin
  GetChunk;
  if FBufPos < FBufSize then
  begin
    Result := FBuffer [FBufPos];
    Inc(FBufPos)
  end
  else
    Result := #0
end;

procedure TStreamWideTextReader.GetChunk;
var
  ps: Integer;
begin
  if FBufPos = FBufSize then
  begin
    ps := (FStream.Size - FStream.Position) div sizeof (WideChar);
    FBufPos := 0;
    FBufSize := FBlockSize;
    if FBufSize > ps then
      FBufSize := ps;

    FBufSize := FStream.Read(FBuffer^, FBufSize * sizeof (WideChar)) div sizeof (WideChar);
    if FBufSize < FBlockSize then
      FBuffer [FBufSize] := #10
  end;
end;

function TStreamWideTextReader.GetPosition: Integer;
begin
  Result := ((FStream.Position - 2) div sizeof (WideChar)) - FBufSize + FBufPos;
end;

function TStreamWideTextReader.ReadLn(var st: WideString;
  continuationChar: WideChar): Boolean;
var
  l, lineStartPos: Integer;
  pch, pch1: PWideChar;
  st1: WideString;
  cont, scont: Boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;
  st := '';
  while cont do
  begin
    GetChunk;
    if FBufPos = FBufSize then
      break;

    pch := FBuffer;
    Inc(pch, FBufPos);
    pch1 := WideStrScan (pch, #10);

    if pch1 <> nil then
    begin
      l := (Integer (pch1) - Integer (pch)) div sizeof (WideChar);
      Inc(FBufPos, l + 1);
      if FBufPos > FBufSize then
        FBufPos := FBufSize;
      if l > 0 then
      begin
        repeat
          Dec(pch1);
          if pch1^ = #13 then
            Dec(l)
          else
            break
        until pch1 = pch;
        cont := pch1^ = continuationChar
      end
      else
        cont := scont;
      SetLength (st1, l);
      if l > 0 then
        Move(pch^, PWideChar (st1)^, l * sizeof (WideChar));
    end
    else
    begin
      st1 := pch;
      l := Length (st1);

      while(l > 0) and (st1 [l] = #13) do
        Dec(l);

      if l < Length (st1) then
        SetLength (st1, l);

      scont := (l > 0) and (st1 [l] = continuationChar);

      FBufPos := FBufSize
    end;

    st := st + st1
  end;

  if cont then
  begin
    Position := lineStartPos;
    Result := False
  end
  else
    Result := True;
end;

procedure TStreamWideTextReader.SetPosition(const Value: Integer);
begin
  if Value <> Position then
  begin
    FBufSize := 0;
    FBufPos := 0;
    FStream.Position := Value * sizeof (WideChar) + 2;
  end
end;

procedure TStreamWideTextReader.SetStream(const Value: TStream);
begin
  FStream := Value;
  FBufPos := 0;
  FBufSize := 0;
  Position := 0;
end;


{ TBufferedFileWriter }

procedure TBufferedFileWriter.Write(const data; dataLen: Integer);
var
  stPos, chunkLen: Integer;
  p: PChar;
begin
  stPos := 0;
  while stPos < dataLen do
  begin
    chunkLen := FBufSize - FBufPos;
    if chunkLen = 0 then
    begin
      FlushBuffer;
      chunkLen := FBufSize - FBufPos
    end;

    if chunkLen > dataLen - stPos then
      chunkLen := dataLen - stPos;

    p := PChar (@data);
    Inc(p, stPos);
    Move(p^, (FBuffer + FBufPos)^, chunkLen);
    Inc(stPos, chunkLen);
    Inc(FBufPos, chunkLen)
  end
end;

constructor TBufferedFileWriter.Create(const AFileName: string);
begin
  FBufSize := 65536;
  GetMem (FBuffer, FBufSize);
  FStream := TFileStream.Create(AFileName, fmCreate);
end;

procedure TBufferedFileWriter.FlushBuffer;
begin
  FStream.Write(FBuffer^, FBufPos);
  FBufPos := 0
end;

destructor TBufferedFileWriter.Destroy;
begin
  if Assigned(FStream) then
  begin
    FlushBuffer;
    FStream.Free
  end;
  FreeMem (FBuffer);

  inherited;
end;

function TBufferedFileWriter.GetPosition: Integer;
begin
  Result := FStream.Position + FBufPos;
end;

end.
