unit unitMidiTrackStream;

interface

uses
  Classes, SysUtils, Windows, unitVirtualMemory, unitMidiGlobals;

type
  TFindEventType = (feFirst, feLast, feAny);

  TMidiTrackStream = class (TVirtualMemoryStream)
  private
    FEventCount: Integer;
    FTrackName: PMidiEventData;
    FPatch: TPatchNo;
    FBank: TBankNo;
    FChannel: TChannel;
    FChanges: Boolean;
    FUpdateCount: Integer;

    function GetEvent (idx: Integer): PMidiEventData;
    procedure SetTrackName(const value: string);
    function GetTrackName: string;
    procedure SetChannel (value: TChannel);
    procedure SetPatch (value: TPatchNo);

  public
    InstrumentName: string;
    TempPort: Integer;

    constructor Create(MaxEvents: Integer);
    destructor Destroy; override;
    procedure CalcOnOffPointers;
    procedure Init;

    procedure LoadFromSMFStream (SMFstream: TStream);
    procedure SaveToSMFStream (SMFStream: TStream);

    function FindEventNo (pos: Integer; tp: TFindEventType): Integer;
    function FindEvent (pos: Integer; tp: TFindEventType): PMidiEventData;
    function InsertEvent (pos: Integer; var data: TEventData; sysexSize: Integer): PMidiEventData;
    function InsertMetaEvent (pos: Integer; metaEvent: byte; data: PChar; dataLen: Integer): PMidiEventData;
    procedure DeleteEvent (eventNo: Integer);
    procedure EraseNonMetaEvents;
    function IndexOf (p: PMidiEventData): Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    // Clipboard support
    function GetEventRange(startPos, endPos: Integer; var startEvent, endEvent: Integer): Boolean;
    function CalcRangeDataSize(startEvent, endEvent, startPos, endPos: Integer): Integer;
    procedure GetRangeData(buffer: PChar; startPos, startEvent, endEvent, endPos: Integer);
    procedure DeleteRange(startPos, endPos: Integer);
    procedure CopyToClipboard (startPos, endPos: Integer);
    procedure DeleteToClipboard (startPos, endPos: Integer);
    procedure CutToClipboard (startPos, endPos: Integer);
    procedure PasteFromClipboard (Pos: Integer);

    property EventCount: Integer read FEventCount;
    property Event [idx: Integer]: PMidiEventData read GetEvent;
    property TrackName: string read GetTrackName write SetTrackName;
    property Channel: TChannel read FChannel write SetChannel;
    property Patch: TPatchNo read FPatch write SetPatch;
    property Bank: TBankNo read FBank write FBank;
    property Changes: Boolean read FChanges write FChanges;
  end;

  EMidiTrackStream = class (Exception);

implementation

uses
  Clipbrd;

const
  ChanType: array [0..15] of Integer = (0, 0, 0, 0, 0, 0, 0, 0,
                                         2, 2, 2, 2, 1, 1, 2, 0);
var
  TrackClipboardFormat: Word;

constructor TMidiTrackStream.Create(maxEvents: Integer);
begin
  inherited Create(maxEvents * sizeof (TMidiEventData), 0);
end;

(*---------------------------------------------------------------------*
 | destructor TMidiTrackStream.Destroy;                                |
 |                                                                     |
 | Free the event data buffers.                                        |
 *---------------------------------------------------------------------*)
destructor TMidiTrackStream.Destroy;
var
  event: PMidiEventData;
  i: Integer;
begin
  event := Memory;
                            // Go thru the event buffer...
  for i := 0 to FEventCount - 1 do
  begin
                            // If its a sysex message($f0); a sysex continuation ($f7)
                            // or a meta event ($ff), we need to free the sysex
                            // data
    if event^.data.status in [midiSysex, midiSysexCont, midiMeta] then
      FreeMem (event^.data.sysex);
    Inc(event)
  end;
  inherited
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.Init;                                    |
 |                                                                     |
 | Initialise a blank track by creating TrackStart, TrackName &        |
 | TrackEnd meta events.                                               |
 *---------------------------------------------------------------------*)
procedure TMidiTrackStream.Init;
begin
  InsertMetaEvent (0, metaTrackEnd, Nil, 0);
  InsertMetaEvent (0, metaTrackStart, Nil, 0);
  FTrackName := InsertMetaEvent (0, metaTrackName, Nil, 0);
end;

procedure TMidiTrackStream.LoadFromSMFStream (SMFstream: TStream);
var
  hdr: array [0..3] of char;
  trackSize: LongInt;
  buffer: TMemoryStream;
  gotEndOfTrack: Boolean;
  divi: Integer;

  // return no of events.
  function DoPass (pass2: Boolean): Integer;
  var
    sysexFlag: Boolean;
    l, pos: Integer;
    c, c1, status, runningStatus, mess: byte;
    events: PMidiEventData;
    notGotPatch, notGotChannel, newStatus: Boolean;
    eventCount: Integer;
  //-----------------------------------------------------------------------
  //  function GetFVariNum: Integer;
  //
  //  Get a variable length Integer from the SMF data.  The first byte is
  // the most significant.  Use onlu the lower 7 bits of each bytes - the
  // eigth is set if there are more bytes.

    function GetFVariNum: Integer;
    var
      l: Integer;
      b: byte;
    begin
      l := 0;
      repeat
        b := PByte(Integer (buffer.Memory) + pos)^;
        Inc(pos);
        l := (l shl 7) + (b and $7f);  // Add it to what we've already got
      until (b and $80) = 0;           // Finish when the 8th bit is clear.
      Result := l
    end;

  //-----------------------------------------------------------------------
  //  function GetFChar: Integer;
  //
  // Get a byte from the SMF stream

    function GetFChar: byte;
    begin
      Result := PByte(Integer (buffer.Memory) + pos)^;
      Inc(pos);
    end;

  begin
    events := Memory;
    eventCount := 0;
    runningStatus := 0;              // Clear 'running status'
    divi := 0;                       // Current position (in ticks) is zero
    newStatus := False;
    pos := 0;                        // Start at the beginning of the buffer
    sysexFlag := False;              // Clear flag - we're not in the middle of
                                     // a sysex message

    notGotChannel := True;
    notGotPatch := True;

    while pos < trackSize do
    begin
      Inc(divi, GetFVariNum);       // Get event position

      c := GetFChar;                 // Get first byte of event status if it's >= $80

                                     // If we're in the middle of a sysex msg, this
                                     // must be a sysex continuation event
      if sysexFlag and (c <> midiSysexCont) then
        raise EMidiTrackStream.Create('Error in Sysex');

      if (c and midiStatus) <> 0 then
      begin                          // It's a 'status' byte
        status := c;
        newStatus := True;           // Get the first data byte
      end
      else
      begin
        status := runningStatus;
        if status = 0 then
                                     // byte indicates 'running status' but we don't
                                     // know the status
          raise EMidiTrackStream.Create('Error in Running Status')
      end;

      if pass2 then
      begin
        events^.pos := divi;
        events^.data.status := status
      end;

      if status < midiSysex then           // Is it a 'channel' message
      begin
        if NewStatus then
        begin
          c := GetFChar;
          NewStatus := False;
          runningStatus := status
        end;

        mess := (status shr 4);      // the top four bits of the status
                                     // Get the second data byte if there is one.
        if ChanType [mess] > 1 then c1 := GetFChar else c1 := 0;

        if not pass2 then
        begin
          if notGotPatch and (mess = $c) then
          begin                         // It's  the first 'patch change' message
            notGotPatch := False;
            FPatch := c
          end;

          if notGotChannel then
          begin                         // It's the first 'channel' message
            notGotChannel := False;
            FChannel := status and midiChannelMask;
          end
        end
        else
          with events^ do
          begin
            data.b2 := c;              // Save the data bytes
            data.b3 := c1
          end
      end
      else
      begin                          // It's a meta event or sysex.
        newStatus := False;
        case status of
          midiMeta:                      // Meta event
            begin
              c1 := GetFChar;        // Get meta type
              l := GetFVariNum;      // Get data len

                                     // Allocate space for message(including meta type)
              if pass2 then
              begin
                events^.sysexSize := l + 1;
                GetMem (events^.data.sysex, events^.sysexSize);

                events^.data.sysex [0] := char (c1);
                Move(pointer (Integer (buffer.Memory) + pos)^, events^.data.sysex [1], l);
                case c1 of             // Save 'track name' event
                  metaTrackName :
                    FTrackName := events;
                  metaText: if FTrackName = Nil then FTrackName := events;
                end
              end
              else
                if c1 = metaTrackEnd then
                  if not gotEndOfTrack then
                    gotEndOfTrack := True;

              Inc(pos, l);

            end;

          midiSysex, midiSysexCont:  // Sysex event
            begin
              l := GetFVariNum;     // Get length of sysex data

              if pass2 then
              begin
                                    // Allocate a buffer, and copy it in.
                events^.sysexSize := l;
                GetMem (events^.data.sysex, l);
                Move(pointer (Integer (buffer.Memory) + pos)^, events^.data.sysex [0], l);
              end;
              Inc(pos, l);
                                    // Set flag if the message doesn't end with f7
              sysexFlag := PChar (Integer (buffer.Memory) + pos - 1)^ <> char (midiSysexCont);
            end
        end
      end;
      Inc(eventCount);
      Inc(events);
    end;
    Result := eventCount
  end;

begin // LoadFromSMFStream
  SMFStream.Read (hdr, sizeof (hdr));      // Read the track header
  if StrLComp (hdr, 'MTrk', sizeof (hdr)) <> 0 then
    raise EMidiTrackStream.Create('Invalid track start ID');

  SMFStream.ReadBuffer (trackSize, sizeof (trackSize));
  trackSize := SwapLong (trackSize);

  buffer := TMemoryStream.Create;
  try
    buffer.CopyFrom (SMFStream, trackSize);

    gotEndOfTrack := False;
    FEventCount := DoPass (False);
                                     // We now know how many events there are.
                                     // Set the buffer size(commits the memory)
    Size := FEventCount * sizeof (TMidiEventData);
    DoPass (True);

    if not GotEndOfTrack then       // Add end of track if not found
      InsertMetaEvent (divi, metaTrackEnd, Nil, 0);
  finally
    buffer.Free
  end;
  Seek(EventCount * sizeof (TMidiEventData), soFromBeginning);
  CalcOnOffPointers;
end;

procedure TMidiTrackStream.SaveToSMFStream (SMFStream: TStream);
var
  trackSize, ts: Integer;
  buffer: TMemoryStream;

//-----------------------------------------------------------------------
//  function DoPass.  Returns the track size
//
  function DoPass (pass2: Boolean): Integer;
  var
    p: PMidiEventData;
    i, lastPos: Integer;
    c, status: byte;
    pos: Integer;

  //-----------------------------------------------------------------------
  //  procedure SetByte(b: byte);
  //
  // Set a byte of SMF data
    procedure SetByte(b: byte);
    begin
     if pass2 then
       pByte(Integer (buffer.Memory) + pos)^ := b;
     Inc(pos);
    end;

  //-----------------------------------------------------------------------
  //  procedure SetVariNum (n: LongInt; mask: byte);
  //
  // Set a variable length Integer.  See GetFVariNum above
    procedure SetVariNum (n: LongInt; mask: byte);
    var
      b: byte;
      r: Longint;
    begin
      b := n and $7f;
      r := n shr 7;
      if r > 0 then
        SetVariNum (r, $80);
      SetByte(b or mask)
    end;

  //-----------------------------------------------------------------------
  // procedure SetBlock(data: PChar; size: LongInt);
  //
  // Set a sysex or meta block.
    procedure SetBlock(data: PChar; size: LongInt);
    begin
      SetVariNum (size, 0);
      if pass2 then
        Move(data^, Pointer (Integer (buffer.Memory) + pos)^, size);
      Inc(pos, size);
    end;

  //-----------------------------------------------------------------------
  // procedure SetMeta(data: PChar; size: LongInt);
  //
  // Set a meta block.  Meta event type first then size, then meta event data
    procedure SetMeta(data: PChar; size: LongInt);
    begin
      SetByte(byte(data [0]));
      SetBlock(data + 1, size - 1)
    end;

  begin // DoPass
    p := Memory;
    pos := 0;
    lastPos := 0;
    status := 0;                      // Initialise running status.

    for i := 0 to FEventCount - 1 do
    begin
      SetVariNum (p^.pos - lastPos, 0);
      lastPos := p^.pos;
                                     // Save the event position
      c := p^.data.status;           // Get the status

      if c < midiSysex then          // Channel message ?
      begin
        if c <> status then          // If the status is the same as before,
        begin                        // don't save it.
          status := c;
          SetByte(status)
        end;
                                     // Save the first data byte
        SetByte(p^.data.b2);
                                     // Save the optional second data byte
        if ChanType [status shr 4] = 2 then
          SetByte(p^.data.b3);
      end
      else
      begin                          // Sysex or meta event
        SetByte(c);                 // Save the status byte
        if (c = midiMeta) then       // Save the data.
          SetMeta(p^.data.sysex, p^.sysexSize)
        else
          SetBlock(p^.data.sysex, p^.sysexSize);
        Status := 0;                 // Clear running status
      end;

      Inc(p)
    end;
    Result := pos;
  end;

begin
  trackSize := DoPass (False);

  SMFStream.WriteBuffer ('MTrk', 4);
  ts := SwapLong (trackSize);
  SMFStream.WriteBuffer (ts, sizeof (ts));

  buffer := TMemoryStream.Create;
  try
    buffer.Size := trackSize;

    DoPass (True);
    buffer.Seek(0, soFromBeginning);
    SMFStream.CopyFrom (buffer, trackSize)
  finally
    buffer.Free
  end;
  Seek(EventCount * sizeof (TMidiEventData), soFromBeginning);
end;

(*---------------------------------------------------------------------*
 | function TMidiTrackStream.GetEvent (): PMidiEventData;             |
 |                                                                     |
 | Get the 'idx'th event in the buffer                                 |
 |                                                                     |
 | Parameters:                                                         |
 |   idx: Integer  The event to get                                   |
 |                                                                     |
 | The function returns a pointer to the specified event.              |
 *---------------------------------------------------------------------*)

function TMidiTrackStream.GetEvent (idx: Integer): PMidiEventData;
begin
  if (idx >= 0) and (idx < EventCount) then
  begin
    Result := PMidiEventData(Memory);
    Inc(Result, idx)
  end
  else Result := Nil
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStrean.SetTrackName();                         |
 |                                                                     |
 | Sets the track name by modifying the 'meta track name' midi event   |
 |                                                                     |
 | Parameters:                                                         |
 |   value: string  The new track name                                |
 *---------------------------------------------------------------------*)
procedure TMidiTrackStream.SetTrackName(const value: string);
var
  Event: TEventData;

  procedure SetNameEvent (var event: TEventData);
  var len: Integer;
  begin
    len := Length (value);
    event.status := midiMeta;
    GetMem (Event.sysex, len + 1);
    Event.sysex [0] := char (metaTrackName);
    if len > 0 then Move(value [1], Event.sysex [1], Len);
  end;

begin
  if FTrackName <> Nil then
  begin
    FreeMem (FTrackName^.data.sysex);
    SetNameEvent (FTrackName.data);
    FTrackName^.sysexSize := Length (value) + 1;
    FChanges := True;
  end
  else
  begin
    SetNameEvent (Event);
    FTrackName := InsertEvent (0, Event, Length (value) + 1)
  end
end;

(*---------------------------------------------------------------------*
 | function TMidiTrackStream.GetTrackName: string;                    |
 |                                                                     |
 | Get the track name                                                  |
 |                                                                     |
 | The function returns the track name                                 |
 *---------------------------------------------------------------------*)
function TMidiTrackStream.GetTrackName;
var len: Integer;
begin
  if FTrackName = Nil then
    Result := ''
  else
  begin
    len := FTrackName^.sysexSize ;
    if len = 0 then
      Result := ''
    else
    begin
      SetLength (Result, len - 1);
      Move((FTrackName^.data.sysex + 1)^, Result [1], len - 1);
      Result := PChar (Result);
    end
  end
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.SetChannel ();                           |
 |                                                                     |
 | Set the channel by modifying all 'channel' events                   |
 |                                                                     |
 | Parameters:                                                         |
 |   value: TChannel                // The new channel.               |
 *---------------------------------------------------------------------*)
procedure TMidiTrackStream.SetChannel (value: TChannel);
var
  status: byte;
  p :PMidiEventData;
  i: Integer;
begin
  if value <> FChannel then
  begin
    FChannel := value;

    for i := 0 to EventCount - 1 do          // Go thru the buffer...
    begin
      p := Event [i];
      status := p^.data.status;
      if status < midiSysex then      // Is it a 'channel' event
        status := status and midiStatusMask or FChannel;
      p^.data.status := status;
    end;
    FChanges := True;
  end
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.SetPatch ();                             |
 |                                                                     |
 | Set the ***Initial** patch by modifying all program change events   |
 | before the first Note message, or inserting one if there aren't any |
 |                                                                     |
 | Parameters:                                                         |
 |   value: TChannel                // The new channel.               |
 *---------------------------------------------------------------------*)
procedure TMidiTrackStream.SetPatch (value: TPatchNo);
var
  p: PMidiEventData;
  newEvent: TEventData;
  i: Integer;
  gotPatchChange: Boolean;
  status: byte;
begin
  if value <> FPatch then
  begin
    FPatch := value;
    gotPatchChange := False;

    for i := 0 to EventCount - 1 do
    begin
      p := Event [i];
      status := p^.data.status;
      case status and midiStatusMask of
        midiNoteOn, midiNoteOff :
          break;
        midiProgramChange :
        begin
          p^.data.b2 := value;
          gotPatchChange := True
        end
      end
    end;

    if not gotPatchChange then
    begin
      newEvent.status := midiProgramChange + Channel;
      newEvent.b2 := value;
      newEvent.b3 := 0;
      InsertEvent (0, newEvent, 0)
    end;
  end
end;

(*----------------------------------------------------------------------------*
 | function TMidiTrackStream.FindEventNo  (): Integer                        |
 |                                                                            |
 | Find event at position.  If there isn't an event at the position, return   |
 | the next event after the position.  If we're beyond the last event, return |
 | -1                                                                         |
 |                                                                            |                                                                |
 | Parameters:                                                                |
 |   pos: Integer         The position to find.                              |
 |   tp : TFindEventType  Whether to find the first event, the last event    |
 |                         or any event.                                      |
 |                                                                            |
 | The function returns the index of the event.                               |
 *----------------------------------------------------------------------------*)

function TMidiTrackStream.FindEventNo (pos: Integer; tp: TFindEventType): Integer;
var
  sp, ep, mp: Integer;
  mev: PMidiEventData;
  p: PMidiEventData;
begin
  p := Memory;
  sp := 0;
  ep := EventCount - 1;
  mp := 0;
  mev := Nil;

  while ep >= sp do                  // Do a binary search on the event buffer
  begin
    mp := sp + (ep - sp) div 2;
    mev := p;
    Inc(mev, mp);                   // Get the middle event

    if pos > mev^.pos then
      sp := mp + 1                   // Search the upper half of the buffer
    else
      if pos < mev^.pos then
        ep := mp - 1                 // Search the lower half of the buffer
      else
        break                        // We've found the event
  end;

  while(mev <> Nil) and (mev^.pos < pos) do
  begin
    Inc(mp);                        // Make sure we're on the next event if the
    if mp < EventCount then
      mev := Event [mp]
    else
      break
  end;                               // position wasn't found.

  if mp >= EventCount then           // End of buffer ?
    mp := -1
  else
  begin
    mev := p;                        // Now go to the first or last matching event
    Inc(mev, mp);
    if mev^.pos = pos then
    case tp of
      feLast : while(mp + 1 < EventCount) and (Event [mp + 1]^.pos = pos) do Inc(mp);
      feFirst: while(mp - 1 >= 0) and (Event [mp - 1]^.pos = pos) do Dec(mp)
    end;

  end;

  Result := mp
end;

(*---------------------------------------------------------------------*
 | function TMidiTrackStream.FindEvent (): PMidiEventData;            |
 |                                                                     |
 | Find an event at a specified position (in ticks).  The tyype(tp)   |
 | indicates whether the function should return any event at the       |
 | specified position; the first event at the specified position, or   |
 | the last event at the specified position.  If no event is found at  |
 | the position, the function returns the next event after the         |
 | position, or Nil if there aren't any.                               |
 |                                                                     |
 | Parameters:                                                         |
 |   pos: Integer;                                                    |
 |   tp: TFindEventType                                               |
 |                                                                     |
 | The function returns the specified event.                           |
 *---------------------------------------------------------------------*)
function TMidiTrackStream.FindEvent (pos: Integer; tp: TFindEventType): PMidiEventData;
begin
  Result := Event [FindEventNo (pos, tp)];
end;

function TMidiTrackStream.InsertEvent (pos: Integer; var data: TEventData; sysexSize: Integer): PMidiEventData;
var
  no: Integer;
  p, p1: PMidiEventData;
  RecalcFlag: Boolean;
begin
  RecalcFlag := False;
  p := Event [EventCount - 1];
  if (p <> Nil) and (p^.data.status = midiMeta) and (p^.data.sysex [0] = char (metaTrackEnd)) and (p^.pos < pos) then
    p^.pos := pos;

  if Size < (EventCount + 1) * sizeof (TMidiEventData) then
    Size := (EventCount + 1) * sizeof (TMidiEventData);

  if EventCount > 0 then
  begin
    no := FindEventNo (pos, feLast);
    if no = -1 then       // End of track must exist !
      raise EMidiTrackStream.Create('System error inserting event');
  end
  else no := -1;          // No events at all.  Must be inserting end of track.

  p := Memory;
  if no <> -1 then
  begin
    Inc(p, no);

    p1 := p;
    Inc(p1);

    if (no < EventCount - 1) and (pos = p^.pos) then
    begin
      p := p1;
      Inc(p1);
      Inc(no)
    end;
    Move(p^, p1^, sizeof (TMidiEventData) * (EventCount - no));

    if no < EventCount - 1 then
      RecalcFlag := True;
  end;

  p^.pos := pos;
  p^.sysexSize := sysexSize;
  p^.data := data;
  Inc(FEventCount);
  if RecalcFlag and (FUpdateCount = 0) then
    CalcOnOffPointers;
  FChanges := True;
  Result := p
end;

function TMidiTrackStream.InsertMetaEvent (pos: Integer; metaEvent: byte; data: PChar; dataLen: Integer): PMidiEventData;
var
  event: TEventData;
begin
  event.status := midiMeta;
  GetMem (event.sysex, dataLen + 1);
  event.sysex [0] := char (metaEvent);
  if dataLen > 0 then
    Move(data [0], event.sysex [1], dataLen);
  Result := InsertEvent (pos, event, dataLen + 1);
end;


procedure TMidiTrackStream.DeleteEvent (eventNo: Integer);
var
  p1, p2: PMidiEventData;
begin
  p1 := Event [eventNo];
  if Assigned(p1) then
  begin
    if p1^.sysexSize > 0 then
      FreeMem (p1^.data.sysex);

    if eventNo < EventCount - 1 then
    begin
      p2 := p1;
      Inc(p2);

      Move(p2^, p1^, (EventCount - EventNo - 1) * sizeof (TMidiEventData))
    end;
    FChanges := True;
    Dec(FEventCount);
    if FUpdateCount = 0 then
      CalcOnOffPointers;
  end
  else
    raise Exception.Create('Invalid event no');
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.GetEventRange();                        |
 |                                                                     |
 | Get range of events to be deleted, copied, etc.  The range may      |
 | contain 'stubs'; where note-on is outside the range(shouldn't be   |
 | operated on), or 'orphans' where the note-off is outside the range, |
 | and should be operated on (including the orphan).                   |
 |                                                                     |
 | Parameters:                                                         |
 |   startPos                  Range start position.                   |
 |   endPos                    Range end position                      |
 |   startEvent                Returns the first event in the range    |
 |   endEvent                  Returns the last event in the range.    |
 *---------------------------------------------------------------------*)
function TMidiTrackStream.GetEventRange(startPos, endPos: Integer; var startEvent, endEvent: Integer): Boolean;
begin
  startEvent := FindEventNo (startPos, feFirst);   // Find first event in range
  endEvent := FindEventNo (endPos, feLast);        // Find last event in range

  if startEvent <> -1 then
  begin
    if endEvent = -1 then                   // endPos is beyond end of song.
      endEvent := EventCount - 2            // Make it the event before the
                                            // end-of-track
    else
    begin
                                            // FindEventNo will return the
                                            // next event if no event is
                                            // found at the position.
      if Event [endEvent].pos > endPos then
        Dec(endEvent);                     // Adjust for this...

      if endEvent >= EventCount - 1 then    // protect 'end of track' meta event.
        Dec(endEvent)
    end;

    Result := endEvent >= startEvent
  end
  else Result := False
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.CalcRangeDataSize();                    |
 |                                                                     |
 | Calc size of requied DDE buffer to save events into.  Includes size |
 | of the events (excluding stubs, but including orphans), and size of |
 | sysex.                                                              |
 |                                                                     |
 | Parameters:                                                         |
 |   startEvent                First event in the range                |
 |   endEvent                  Last event in the range.                |
 |   startPos                  Range start position.                   |
 |   endPos                    Range end position                      |
 *---------------------------------------------------------------------*)
function TMidiTrackStream.CalcRangeDataSize(startEvent, endEvent, startPos, endPos: Integer): Integer;
var
  blockSize: Integer;
  p: PMidiEventData;
  sts: byte;
begin
                               // Calculate size of DDE buffer - big enough for
                               // header & events
  blockSize := (endEvent - startEvent + 1) * sizeof (TMidiEventData) + sizeof (TMidiEventClipboardHeader);
  p := Event [startEvent];
                               // Go through selected events, add size of sysex messages,
                               // and adjust for stubs and orphans.

  while endEvent >= startEvent do
  begin
    if p^.sysexSize > 0 then   // Add sysex size
      Inc(blockSize, p^.sysexSize);

    sts := p^.data.status and midiStatusMask;

                               // Add space for orphan midi-off
    if (sts = midiNoteOn) and (p^.OnOffEvent^.pos > endPos) then
      Inc(blockSize, sizeof (TMidiEventData));

                               // Remove space for stub midi-on.
    if ((sts = midiNoteOff) or ((sts = midiNoteOn) and (p^.data.b3 = 0))) and (p^.OnOffEvent^.pos < startPos) then
      Dec(blockSize, sizeof (TMidiEventData));

    Inc(p);
    Dec(endEvent)
  end;

  Result := blockSize
end;


(*---------------------------------------------------------------------*
 | function CompareEvents (): Integer                                  |
 |                                                                     |
 | 'Compare' function used in sorting orphan lists.                    |
 |                                                                     |
 | Parameters:                                                         |
 |   p1, p2                    Events to compare.                      |
 *---------------------------------------------------------------------*)
function CompareEvents (p1, p2: pointer): Integer;
begin
  if PMidiEventData(p1)^.pos > PMidiEventData(p2)^.pos then
    Result := 1
  else
    if PMidiEventData(p1)^.pos < PMidiEventData(p2)^.pos then
      Result :=-1
    else
      if Integer (p1) > Integer (p2) then
        Result := 1
      else
        Result := -1
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.GetRangeData();                         |
 |                                                                     |
 | Fill DDE buffer from range. Buffer ends up with:                    |
 |    Header                                                           |
 |    Events                                                           |
 |    Orphans                                                          |
 |    Sysex Data                                                       |
 |                                                                     |
 | Parameters:                                                         |
 |   buffer                    The buffer to fill                      |
 |   startPos                  Range start position.                   |
 |   startEvent                First event in the range                |
 |   endEvent                  Last event in the range.                |
 |   endPos                    Range end position                      |
 *---------------------------------------------------------------------*)
procedure TMidiTrackStream.GetRangeData(buffer: PChar; startPos, startEvent, endEvent, endPos: Integer);
var
  i, bufferedEvents: Integer;
  pSrc, pDst: PMidiEventData;
  buf: PChar;
  OrphanList: TList;
  sts: byte;

begin
  buf := buffer;
                             // Set up DDE header in buffer
  with PMidiEventClipboardHeader (buf)^ do
  begin
    noEvents := endEvent - startEvent + 1; // Save nominal event count.  Stubs
                                           // will be removed and orphans added...
    startPosn := startPos
  end;

                                           // Copy events into buffer
  pSrc := Event [startEvent];
  pDst := PMidiEventData(buffer + sizeof (TMidiEventClipboardHeader));

  OrphanList := Nil;
  bufferedEvents := 0;

  for i := startEvent to endEvent do
  begin
    sts := pSrc^.data.status and midiStatusMask;
    if (sts = midiNoteOn) and (pSrc^.OnOffEvent^.pos > endPos) then
    begin                                  // Add orphaned note-off to orphan list
      if not Assigned(OrphanList) then
      begin
        OrphanList := TList.Create;
        OrphanList.Capacity := 64
      end;
      OrphanList.Add (pSrc^.OnOffEvent)
    end;

    if not (((sts = midiNoteOff) or ((sts = midiNoteOn) and (pSrc^.data.b3 = 0))) and (pSrc^.OnOffEvent^.pos < startPos)) then
    begin                                   // Copy event if it's not a 'stub'
      Move(pSrc^, pDst^, sizeof (TMidiEventData));
      Inc(pDst);
      Inc(bufferedEvents)
    end
    else                                    // Remove stub from header event count.
      Dec(PMidiEventClipboardHeader (buf)^.noEvents);

    Inc(pSrc);
  end;

  if Assigned(OrphanList) then             // Copy orphans to buffer
  try
                                            // Add orphans count to header event count
    with PMidiEventClipboardHeader (buf)^ do
      Inc(noEvents, OrphanList.Count);

    if OrphanList.Count > 1 then            // .. because of VCL bug...
      OrphanList.Sort (CompareEvents);

    for i := 0 to OrphanList.Count - 1 do   // Copy orphans
    begin
      Move(OrphanList [i]^, pDst^, sizeof (TMidiEventData));
      Inc(pDst);
    end
  finally
    OrphanList.Free
  end;


  pSrc := PMidiEventData(buffer + sizeof (TMidiEventClipboardHeader));
  buffer := PChar (pDst);                   // sysex data comes after orphans
                                            // Move sysex data for each event into buffer
  for i := 0 to bufferedEvents - 1 do
  begin
    if pSrc^.sysexSize > 0 then
    begin
      move(pSrc^.data.sysex^,buffer^, pSrc^.sysexSize);
      pSrc^.data.sysex := buffer;           // Make the buffered event point to
                                            // the buffered sysex, instead of it's
                                            // original sysex.
      Inc(buffer, pSrc^.sysexSize)
    end;

    Inc(pSrc)
  end
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.DeleteRange();                          |
 |                                                                     |
 | Delete data between the specified range.  Don't delete stubs, but   |
 | delete orphans.                                                     |
 |                                                                     |
 | Parameters:                                                         |
 |   startPos                  Range start position.                   |
 |   endPos                    Range end position                      |
 *---------------------------------------------------------------------*)
procedure TMidiTrackStream.DeleteRange(startPos, endPos: Integer);
var
  n1, n2, i: Integer;
  p, p1, pClearRange: PMidiEventData;
  deleteNoEvents: Integer;
  sts: byte;
  eventNo: Integer;
  OrphanList: TList;
begin
  if GetEventRange(startPos, endPos, n1, n2) then
  begin
    deleteNoEvents := n2 - n1 + 1;  // Nominal no of events to delete
    p := Event [n1];
    pClearRange := p;

    OrphanList := Nil;

    // First pass.  Delete thhe sysex data, get the orphan list, and calculate
    // the 'clear range'.  The clear range can be deleted in one hit.  It starts
    // after the last stub.

    for i := 0 to deleteNoEvents - 1 do
    begin
      sts := p^.data.status and midiStatusMask;
      if (sts = midiNoteOn) and (p^.OnOffEvent^.pos > endPos) then
      begin                                         // Add orphan to list.
        if not Assigned(OrphanList) then
        begin
          OrphanList := TList.Create;
          OrphanList.Capacity := 64
        end;
        OrphanList.Add (p^.OnOffEvent)
      end;

      if not (((sts = midiNoteOff) or ((sts = midiNoteOn) and (p^.data.b3 = 0))) and (p^.OnOffEvent^.pos < startPos)) then
      begin                              // Not a stub.  Delete it's sysex
        if p^.sysexSize <> 0 then        // and mark it for deletion by setting sysexSize to MaxWord
          FreeMem (p^.data.sysex);
        p^.sysexSize := MaxWord
      end
      else
      begin
        pClearRange  := p;
        Inc(pClearRange)
      end;
      Inc(p);
    end;

    // Delete orphans - they're off the end of the range, and may not be consecutive
    if Assigned(OrphanList) then
    try
      if OrphanList.Count > 1 then
        OrphanList.Sort (CompareEvents);

      for i := OrphanList.Count - 1 downto 0 do
      begin
        p1 := OrphanList [i];
        Inc(p1);
        eventNo := (Integer (p1) - Integer (Event [0])) div sizeof (TMidiEventData);
        Move(p1^, OrphanList [i]^, (EventCount - eventNo - 1) * sizeof (TMidiEventData));
        Dec(FEventCount)
      end
    finally
      OrphanList.Free
    end;

    // Delete clear range - ie. most of the events.  the clear range starts after the
    // last stub.
    eventNo := (Integer (pClearRange) - Integer (Event [0])) div sizeof (TMidiEventData);
    if p <> pClearRange then
    begin
      Move(p^, pClearRange^, (EventCount - n2 - 1) * sizeof (TMidiEventData));
      Dec(FEventCount, n2 - eventNo + 1)
    end;

    // Delete events before the clear range that are marked for deletion (ie. which aren't stubs)
    p := Event [n1];
    i := n1;
    while i < eventNo do
    begin
      if ((p^.data.status and midiStatusMask) <> midiSysex) and ( p^.SysexSize = MaxWord) then
      begin
        p1 := p;
        Inc(p1);
        Move(p1^, p^, (EventCount - i) * sizeof (TMidiEventData));
        Dec(FEventCount);
        Dec(EventNo)
      end
      else
      begin
        Inc(i);
        Inc(p)
      end
    end;

    CalcOnOffPointers;
    FChanges := True;

    // ** TODO - maybe decrease buffer size here.
  end
end;

procedure TMidiTrackStream.CopyToClipboard (startPos, endPos: Integer);
var
  startEvent, endEvent, blockSize: Integer;
  data: THandle;
  ptr: PChar;
begin
  if GetEventRange(startPos, endPos, startEvent, endEvent) then
  begin
    blockSize := CalcRangeDataSize(startEvent, endEvent, startPos, endPos);
                            // Allocate DDE buffer
    data := GlobalAlloc (GMEM_MOVEABLE or GMEM_DDESHARE, blockSize);
    ptr := GlobalLock(data);
    if ptr <> Nil then
    try
      GetRangeData(ptr, startPos, startEvent, endEvent, endPos);
      GlobalUnlock(data);
    except
      GlobalUnlock(data);
      GlobalFree(data);
      raise
    end;

    Clipboard.SetAsHandle(TrackClipboardFormat, data);
  end
end;

procedure TMidiTrackStream.DeleteToClipboard (startPos, endPos: Integer);
begin
  CopyToClipboard (startPos, endPos);
  DeleteRange(startPos, endPos)
end;

procedure TMidiTrackStream.CutToClipboard (startPos, endPos: Integer);
begin
  CopyToClipboard (startPos, endPos);
  DeleteRange(startPos, endPos);
  // ** TODO Shrink events
end;

procedure TMidiTrackStream.PasteFromClipboard (Pos: Integer);
var
  Handle: THandle;
begin
  Clipboard.Open;
  try
    Handle := Clipboard.GetAsHandle(TrackClipboardFormat);
    if Handle <> 0 then
    begin
      MessageBeep ($ffff);
      // ** Todo - finish the thang
    end
  finally
    Clipboard.Close
  end
end;

(*---------------------------------------------------------------------*
 | procedure TMidiTrackStream.EraseNonMetaEvents ();                   |
 |                                                                     |
 | Erase all events from a track except for meta events.               |
 *---------------------------------------------------------------------*)
procedure TMidiTrackStream.EraseNonMetaEvents;
var
  i, count: Integer;
  sp, ep: PMidiEventData;
begin
  FChanges := True;
  count := EventCount;
  sp := Memory;
  i := 0;
  while i < count do
  begin
    if sp^.data.status <> midiMeta then
                                    // Not a meta event.  Find the next meta
                                    // meta event, and delete everything up to it.
    begin
      ep := sp;
      while i < count do
      begin
        Inc(ep);
        Dec(count);
        if ep^.data.status = midiMeta then break
      end;
                                    // Found the next meta event.
      if i < count then Move(ep^, sp^, (count - i) * sizeof (TMidiEventData));
    end;
    Inc(sp);
    Inc(i)
  end;
  FEventCount := count
end;

procedure TMidiTrackStream.CalcOnOffPointers;
var
  i, count, t, s, b, n: Integer;
  sp: PMidiEventData;
  noteOns: array [0..4, TNote] of PMidiEventData;
  noteOnCount: array [TNote] of byte;
begin
  count := EventCount;
  sp := Memory;
  i := 0;
  ZeroMemory(@noteOnCount, sizeof (noteOnCount));
  while i < count do
  begin
    t := 0;
    s := sp^.data.status and midiStatusMask;
    if s = midiNoteOn then
      if sp^.data.b3 <> 0 then
        t := 1
      else
        t := 2
    else
      if s = midiNoteOff then t := 2;

    case t of
      1 :
        begin
          b := sp^.data.b2;
          n := NoteOnCount [b];
          if n < 5 then
          begin
            noteOns[n, b] := sp;
            NoteOnCount [b] := n + 1
          end
        end;

      2 :
        begin
          b := sp^.data.b2;
          n := NoteOnCount [b];
          if n > 0 then
          begin
            Dec(n);
            sp^.OnOffEvent := NoteOns[n, b];
            NoteOns[n, b]^.OnOffEvent := sp;
            NoteOnCount [b] := n
          end
        end
    end;

    Inc(i);
    Inc(sp);

  end;

  try
    for i := Low (TNote) to High (TNote) do
      if NoteOnCount [i] <> 0 then
//        raise Exception.Create('Note ons don''t match note offs');
  except
    raise
  end
end;

function TMidiTrackStream.IndexOf (p: PMidiEventData): Integer;
var
  q: PMidiEventData;
begin
  q := Memory;
  Result := (Integer (p) - Integer (q)) div sizeof (TMidiEventData);
end;

procedure TMidiTrackStream.BeginUpdate;
begin
  Inc(FUpdateCount)
end;

procedure TMidiTrackStream.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      CalcOnOffPointers
  end
end;

procedure TMidiTrackStream.CancelUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

initialization
  TrackClipboardFormat := RegisterClipboardFormat ('PowerseqMidiTrackData');
end.
