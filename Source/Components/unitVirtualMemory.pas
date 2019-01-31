unit unitVirtualMemory;

interface

uses
  Windows, Classes, SysUtils;

type
  TVirtualMemoryStream = class (TCustomMemoryStream)
  private
    FReserved: Integer;
    FChunkSize: Integer;
  protected
    procedure SetSize(NewSize: Integer); override;
  public
    constructor Create(AReserved, AInitialSize: Integer);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Reserved: Integer read FReserved;
    property ChunkSize: Integer read FChunkSize write FChunkSize;
  end;

  EVirtualMemory = class (Exception);

implementation

{ TVirtualMemoryStream }

constructor TVirtualMemoryStream.Create(AReserved, AInitialSize: Integer);
begin
  FReserved := AReserved;
  FChunkSize := 1024;
  SetPointer (VirtualAlloc (Nil, AReserved, MEM_RESERVE, PAGE_READWRITE), AInitialSize);
  if AInitialSize > 0 then
    VirtualAlloc (Memory, AInitialSize, MEM_COMMIT, PAGE_READWRITE);
end;

destructor TVirtualMemoryStream.Destroy;
begin
  VirtualFree(Memory, 0, MEM_RELEASE);
  inherited;
end;

procedure TVirtualMemoryStream.SetSize(NewSize: Integer);
var
  oldSize: Integer;
  commitSize: Integer;
begin
  oldSize := Size;
  if NewSize <> oldSize then
    if NewSize <= Reserved then
    begin
      if NewSize > oldSize then          // Grow the buffer
      begin
        commitSize := NewSize - oldSize;
        if commitSize < ChunkSize then
          commitSize := ChunkSize;
        if commitSize + oldSize > Reserved then
           commitSize := Reserved - oldSize;
        NewSize := oldSize + commitSize;

        VirtualAlloc (PChar (memory) + oldSize, commitSize, MEM_COMMIT, PAGE_READWRITE)
      end
      else                           // Shrink the buffer (lop off the end)
        VirtualFree(PChar (Memory) + NewSize, oldSize - NewSize, MEM_DECOMMIT);
      SetPointer (Memory, NewSize);
    end
    else raise EVirtualMemory.Create('Size exceeds capacity');
end;

function TVirtualMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Integer;
begin
  Pos := Seek(0, soFromCurrent);
  if Pos + count > Size then
    Size := Pos + count;
  Move(buffer, PChar (Integer (memory) + Pos)^, count);
  Seek(count, soFromCurrent);
  Result := Count
end;

end.
