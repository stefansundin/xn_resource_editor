unit unitStreamTextReader;

interface

uses Windows, Classes, SysUtils;

type

TStreamTextReader = class
private
  fStream : TStream;
  fBuffer : PChar;

  fbufPos : Integer;
  fBufSize : Integer;
  fBlockSize : Integer;

  procedure GetChunk;
  function GetPosition: Integer;
  procedure SetPosition(const Value: Integer);
  procedure SetStream(const Value: TStream);

public
  constructor Create (AStream : TStream; blockSize : Integer = 1024);
  destructor Destroy; override;
  function GetChar : char;
  function ReadLn (var st : string; continuationChar : char = #0) : boolean;
  procedure ReadChunk (var chunk; offset, length : Integer);
  property Position : Integer read GetPosition write SetPosition;
  function Search (const st : string) : Integer;
  property Stream : TStream read fStream write SetStream;
end;

TStreamWideTextReader = class
private
  fStream : TStream;
  fBuffer : PWideChar;

  fbufPos : Integer;
  fBufSize : Integer;
  fBlockSize : Integer;

  procedure GetChunk;
  function GetPosition: Integer;
  procedure SetPosition(const Value: Integer);
  procedure SetStream(const Value: TStream);

public
  constructor Create (AStream : TStream; blockSize : Integer = 1024);
  destructor Destroy; override;
  function GetChar : WideChar;
  function ReadLn (var st : WideString; continuationChar : WideChar = #0) : boolean;
  property Position : Integer read GetPosition write SetPosition;
  property Stream : TStream read fStream write SetStream;
end;

TMappedFile = class
private
  fSize : Integer;
  fMemory : PChar;
  fFileHandle, fMappingHandle : THandle;
public
  constructor Create (const AFileName : string);
  destructor Destroy; override;

  property Size : Integer read fSize;
  property Memory : PChar read fMemory;
end;

TTextFileReader = class (TMappedFile)
private
  fPosition : Integer;
public
  function ReadLn (var st : string) : boolean;
end;

TBufferedFileWriter = class
private
  fStream : TFileStream;
  fBuffer : PChar;
  fBufPos, fBufSize : Integer;
  function GetPosition: Integer;

public
  constructor Create (const AFileName : string);
  destructor Destroy; override;
  procedure Write (const data; dataLen : Integer);

  procedure FlushBuffer;
  property Position : Integer read GetPosition;
end;

TTextFileWriter = class (TBufferedFileWriter)
public
  procedure Write (const st : string);
  procedure WriteLn (const st : string);
end;


implementation

uses unitSearchString;

(*----------------------------------------------------------------------*
 | StrLScan                                                             |
 |                                                                      |
 | Search for a character in the first 'len' bytes of the specified     |
 | block of memory                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   const Str: PChar; ch : char; len : DWORD                           |
 |                                                                      |
 | The function returns PChar                                           |
 *----------------------------------------------------------------------*)
function StrLScan(const Str: PChar; ch : char; len : DWORD): PChar;
asm
        // EAX = Str
        // DL = char
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
  fStream := AStream;
  fBlockSize := blockSize;
  GetMem (fBuffer, fBlockSize + 1);
  fBuffer [blockSize] := #0
end;

destructor TStreamTextReader.Destroy;
begin
  FreeMem (fBuffer);
  inherited;
end;

function TStreamTextReader.GetChar: char;
begin
  GetChunk;
  if fBufPos < fBufSize then
  begin
    result := fBuffer [fBufPos];
    Inc (fBufPos)
  end
  else
    result := #0
end;

procedure TStreamTextReader.GetChunk;
begin
  if fBufPos = fBufSize then
  begin
    fBufPos := 0;
    fBufSize := fBlockSize;
    if fBufSize > fStream.Size - fStream.Position then
      fBufSize := fStream.Size - fStream.Position;

    fBufSize := fStream.Read(fBuffer^, fBufSize);
    if fBufSize < fBlockSize then
      fBuffer [fBufSize] := #10
  end;
end;

function TStreamTextReader.GetPosition: Integer;
begin
  result := fStream.Position - fBufSize + fBufPos;
end;

procedure TStreamTextReader.ReadChunk(var chunk; offset, length: Integer);
begin
  Position := offset;
  fStream.Read(chunk, length)
end;

function TStreamTextReader.ReadLn(var st: string; continuationChar: char): boolean;
var
  l, lineStartPos : Integer;
  pch, pch1 : PChar;
  st1 : string;
  cont, scont : boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;
  st := '';
  while cont do
  begin
    GetChunk;
    if fBufPos = fBufSize then
      break;

    pch := fBuffer;
    Inc (pch, fBufPos);
    pch1 := StrScan (pch, #10);

    if pch1 <> nil then
    begin
      l := Integer (pch1) - Integer (pch);
      Inc (fBufPos, l + 1);
      if fBufPos > fBufSize then
        fBufPos := fBufSize;
      if l > 0 then
      begin
        repeat
          Dec (pch1);
          if pch1^ = #13 then
            Dec (l)
          else
            break
        until pch1 = pch;
        cont := pch1^ = continuationChar
      end
      else
        cont := scont;
      SetLength (st1, l);
      if l > 0 then
        Move (pch^, PChar (st1)^, l);
    end
    else
    begin
      st1 := pch;
      l := Length (st1);

      while (l > 0) and (st1 [l] = #13) do
        Dec (l);

      if l < Length (st1) then
        SetLength (st1, l);

      scont := (l > 0) and (st1 [l] = continuationChar);

      fBufPos := fBufSize
    end;

    st := st + st1
  end;

  if cont then
  begin
    Position := lineStartPos;
    result := False
  end
  else
    result := True;
end;

function TStreamTextReader.Search(const st: string): Integer;
var
  p, p1 : PChar;
begin
  result := -1;
  if Length (st) <> 0 then
  repeat
    GetChunk;
    if fBufSize < Length (st) then
      Exit;

    p := fBuffer;
    Inc (p, fBufPos);

    p1 := StrPos (p, PChar (st));

    if p1 <> Nil then
    begin
      result := (fStream.Position - fBufSize) + Integer (p1) - Integer (fBuffer);
      fBufPos := Integer (p1) - Integer (fBuffer) + Length (st);
      break
    end;

    fStream.Position := fStream.Position - (Length (st) - 1);
    fBufPos := 0;
    fBufSize := 0;
  until False
end;

procedure TStreamTextReader.SetPosition(const Value: Integer);
begin
  if Value <> Position then
  begin
    fBufSize := 0;
    fBufPos := 0;
    fStream.Position := Value
  end
end;

procedure TStreamTextReader.SetStream(const Value: TStream);
begin
  fStream := Value;
  fBufPos := 0;
  fBufSize := 0
end;

{ TMappedFile }

constructor TMappedFile.Create(const AFileName: string);
begin
  fFileHandle := CreateFile (PChar (AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if fFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  fSize := GetFileSize (fFileHandle, nil);
  if fSize <> 0 then
  begin
    fMappingHandle := CreateFileMapping (fFileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if fMappingHandle = 0 then
      RaiseLastOSError;

    fMemory := MapViewOfFile (fMappingHandle, FILE_MAP_READ, 0, 0, 0);
    if fMemory = Nil then
      RaiseLastOSError;
  end
end;

destructor TMappedFile.Destroy;
begin
  if fMemory <> Nil then
    UnmapViewOfFile (fMemory);

  if fMappingHandle <> 0 then
    CloseHandle (fMappingHandle);

  if fFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle (fFileHandle);

  inherited;
end;

{ TTextFileReader }

function TTextFileReader.ReadLn(var st: string): boolean;
var
  p, p1 : PChar;
  l : Integer;
begin
  l := fSize - fPosition;
  if l > 0 then
  begin
    result := True;
    p1 := fMemory + fPosition;
    p := StrLScan (p1, #10, l);

    if p <> Nil then
    begin
      l := Integer (p) - Integer (p1);
      Inc (fPosition, l+1);

      while (l > 0) and (p1 [l - 1] = #13) do
        Dec (l)
    end
    else
      Inc (fPosition, l);

    SetString (st, p1, l)
  end
  else
    result := False;
end;

{ TTextFileWriter }

procedure TTextFileWriter.Write(const st: string);
begin
  inherited Write (st [1], Length (st));
end;

procedure TTextFileWriter.WriteLn(const st: string);
begin
  Write (st + #13#10)
end;

{ TStreamWideTextReader }

constructor TStreamWideTextReader.Create(AStream: TStream;
  blockSize: Integer);
begin
  fStream := AStream;
  fBlockSize := blockSize;
  GetMem (fBuffer, (fBlockSize + 1) * sizeof (WideChar));
  fBuffer [blockSize] := #0;
  Position := 0;
end;

destructor TStreamWideTextReader.Destroy;
begin
  FreeMem (fBuffer);
  inherited;
end;

function TStreamWideTextReader.GetChar: WideChar;
begin
  GetChunk;
  if fBufPos < fBufSize then
  begin
    result := fBuffer [fBufPos];
    Inc (fBufPos)
  end
  else
    result := #0
end;

procedure TStreamWideTextReader.GetChunk;
var
  ps : Integer;
begin
  if fBufPos = fBufSize then
  begin
    ps := (fStream.Size - fStream.Position) div sizeof (WideChar);
    fBufPos := 0;
    fBufSize := fBlockSize;
    if fBufSize > ps then
      fBufSize := ps;

    fBufSize := fStream.Read(fBuffer^, fBufSize * sizeof (WideChar)) div sizeof (WideChar);
    if fBufSize < fBlockSize then
      fBuffer [fBufSize] := #10
  end;
end;

function TStreamWideTextReader.GetPosition: Integer;
begin
  result := ((fStream.Position - 2) div sizeof (WideChar)) - fBufSize + fBufPos;
end;

function TStreamWideTextReader.ReadLn(var st: WideString;
  continuationChar: WideChar): boolean;
var
  l, lineStartPos : Integer;
  pch, pch1 : PWideChar;
  st1 : WideString;
  cont, scont : boolean;
begin
  lineStartPos := Position;
  cont := True;
  scont := false;
  st := '';
  while cont do
  begin
    GetChunk;
    if fBufPos = fBufSize then
      break;

    pch := fBuffer;
    Inc (pch, fBufPos);
    pch1 := WideStrScan (pch, #10);

    if pch1 <> nil then
    begin
      l := (Integer (pch1) - Integer (pch)) div sizeof (WideChar);
      Inc (fBufPos, l + 1);
      if fBufPos > fBufSize then
        fBufPos := fBufSize;
      if l > 0 then
      begin
        repeat
          Dec (pch1);
          if pch1^ = #13 then
            Dec (l)
          else
            break
        until pch1 = pch;
        cont := pch1^ = continuationChar
      end
      else
        cont := scont;
      SetLength (st1, l);
      if l > 0 then
        Move (pch^, PWideChar (st1)^, l * sizeof (WideChar));
    end
    else
    begin
      st1 := pch;
      l := Length (st1);

      while (l > 0) and (st1 [l] = #13) do
        Dec (l);

      if l < Length (st1) then
        SetLength (st1, l);

      scont := (l > 0) and (st1 [l] = continuationChar);

      fBufPos := fBufSize
    end;

    st := st + st1
  end;

  if cont then
  begin
    Position := lineStartPos;
    result := False
  end
  else
    result := True;
end;

procedure TStreamWideTextReader.SetPosition(const Value: Integer);
begin
  if Value <> Position then
  begin
    fBufSize := 0;
    fBufPos := 0;
    fStream.Position := Value * sizeof (WideChar) + 2;
  end
end;

procedure TStreamWideTextReader.SetStream(const Value: TStream);
begin
  fStream := Value;
  fBufPos := 0;
  fBufSize := 0;
  Position := 0;
end;

{ TBufferedFileWriter }

procedure TBufferedFileWriter.Write(const data; dataLen: Integer);
var
  stPos, chunkLen : Integer;
  p :PChar;
begin
  stPos := 0;
  while stPos < dataLen do
  begin
    chunkLen := fBufSize - fBufPos;
    if chunkLen = 0 then
    begin
      FlushBuffer;
      chunkLen := fBufSize - fBufPos
    end;

    if chunkLen > dataLen - stPos then
      chunkLen := dataLen - stPos;

    p := PChar (@data);
    Inc (p, stPos);
    Move (p^, (fBuffer + fBufPos)^, chunkLen);
    Inc (stPos, chunkLen);
    Inc (fBufPos, chunkLen)
  end
end;

constructor TBufferedFileWriter.Create(const AFileName: string);
begin
  fBufSize := 65536;
  GetMem (fBuffer, fBufSize);
  fStream := TFileStream.Create(AFileName, fmCreate);
end;

procedure TBufferedFileWriter.FlushBuffer;
begin
  fStream.Write(fBuffer^, fBufPos);
  fBufPos := 0
end;

destructor TBufferedFileWriter.Destroy;
begin
  if Assigned (fStream) then
  begin
    FlushBuffer;
    fStream.Free
  end;
  FreeMem (fBuffer);

  inherited;
end;

function TBufferedFileWriter.GetPosition: Integer;
begin
  result := fStream.Position + fBufPos;
end;

end.
