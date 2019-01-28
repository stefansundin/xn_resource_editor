unit unitMidiGlobals;

interface

const
  midiNoteOff         = $80;
  midiNoteOn            = $90;
  midiKeyAftertouch   = $a0;
  midiController   = $b0;
  midiProgramChange   = $c0;
  midiChannelAftertouch = $d0;
  midiPitchBend   = $e0;
  midiSysex         = $f0;
  midiSysexCont    = $f7;
  midiMeta    = $ff;
  midiStatusMask  = $f0;
  midiStatus    = $80;
  midiChannelMask  = $0f;
  metaSeqno    = $00;
  metaText    = $01;
  metaCopyright    = $02;
  metaTrackName    = $03;
  metaInstrumentName  = $04;
  metaLyric    = $05;
  metaMarker    = $06;
  metaCuePoint    = $07;
  metaMiscText0    = $08;
  metaMiscText1    = $09;
  metaMiscText2    = $0a;
  metaMiscText3    = $0b;
  metaMiscText4    = $0c;
  metaMiscText5    = $0d;
  metaMiscText6    = $0e;
  metaMiscText7    = $0f;
  metaTrackStart  = $21;
  metaTrackEnd    = $2f;
  metaTempoChange  = $51;
  metaSMPTE    = $54;
  metaTimeSig    = $58;
  metaKeySig    = $59;
  metaSequencer    = $7f;

type
  TTrack = 0..255;
  TChannel = 0..15;
  TNote = 0..127;
  TController = 0..127;
  TPatchNo = 0..127;
  TBankNo = 0..127;
  TControllerValue = 0..127;

//---------------------------------------------------------------------------
// Four byte MIDI message.  (No running status, but Note off may be Note on
//                           with zero velocity )

  TEventData = packed record    // ** nb takes 5 bytes
  case status : byte of
    0 : (b2, b3 : byte);
    1 : (sysex : PChar)
  end;
  PEventData = ^TEventData;

//---------------------------------------------------------------------------
// Midi event
  PMidiEventData = ^TMidiEventData;
  TMidiEventData = packed record // ** nb takes 11 bytes
    pos : LongInt;               // Position in ticks from start of song.
    sysexSize : word;            // Size of sysex or meta message
    data : TEventData;           // Event data
    OnOffEvent : PMidiEventData;
  end;

  TMidiEventClipboardHeader = packed record
    noEvents, startPosn : Integer;
  end;
  PMidiEventClipboardHeader = ^TMidiEventClipboardHeader;

const
  ControllerNames : array [TController] of string [20] = (
  '',              'Mod. Wheel',        'Breath Controller',           '',
  'Foot Control',  'Portamento Time',   'Data Entry Slider',           'Volume',
  'Balance',       '',                  'Pan',                         'Expression',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  'Sustain',       'Portamento',        'Sostenuto',                   'Soft Pedal',
  'General 4',     'Hold 2',            '',                           '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  'General 5',     'General 6',         'General 7',                   'General 8',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  'Tremolo Depth', 'Chorus Depth',      'Detune',                      'Phaser Depth',
  'Data Entry +1', 'Data Entry -1',     'Non reg lsb',                 'Non reg msb',
  'Reg msb',       'Reg lsb',           '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              '',                  '',                            '',
  '',              'Reset Controllers', 'Local Mode',                  'All Notes Off',
  'Omni Mode Off', 'Omni Mode On',      'Mono Mode On',                '');

function AdjustForTimesig (n, beatDiv : Integer) : Integer;
function UnadjustForTimesig (n, beatDiv : Integer) : Integer;
function GetBPM (tempo, beatDiv : Integer) : Integer;
function SwapLong (value : LongInt) : LongInt;
function GetNoteName (note : Integer) : string;

implementation

uses sysutils;

(*---------------------------------------------------------------------*
 | function SwapLong () : LongInt;                                     |
 |                                                                     |
 | Byte swaps a four-byte integer eg: $01020304 becomes $04030201      |
 |                                                                     |
 | Parameters:                                                         |
 |   value : LongInt      The integer to swap                          |
 |                                                                     |
 | The function returns the swapped integer                            |
 *---------------------------------------------------------------------*)
function SwapLong (value : LongInt) : LongInt;
var
  r : packed record case Integer of
    1 : (a : byte; b : word; c : byte);
    2 : (l : longint);
  end;
  t : byte;

begin
  r.l := value;
  r.b := swap (r.b);
  t := r.a; r.a := r.c; r.c := t;
  result := r.l;
end;


function AdjustForTimesig (n, beatDiv : Integer) : Integer;
begin
  if BeatDiv > 2 then
    result := n shr (BeatDiv - 2)
  else
    if BeatDiv < 2 then
      result := n shl (2 - BeatDiv)
    else
      result := n;
end;



function UnAdjustForTimesig (n, beatDiv : Integer) : Integer;
begin
  if BeatDiv > 2 then
    result := n shl (BeatDiv - 2)
  else
    if BeatDiv < 2 then
      result := n shr (2 - BeatDiv)
    else
      result := n;
end;

function GetBPM (tempo, beatDiv : Integer) : Integer;
begin
  result := UnAdjustForTimesig (60000 div tempo, beatDiv);
end;

function GetNoteName (note : Integer) : string;
var
  ch : char;
  Octave : Integer;
begin
  Octave := note div 12;
  Note := Note mod 12;
  case note of
    0, 1 : ch := 'C';
    2, 3 : ch := 'D';
    4 : ch := 'E';
    5, 6 : ch := 'F';
    7, 8 : ch := 'G';
    9, 10 : ch := 'A';
    11 : ch := 'B';
    else ch := '?'
  end;

  if note in [1, 3, 6, 8, 10] then
    result := ch + '#' + IntToStr (Octave)
  else
    result := ch + IntToStr (Octave);
end;

end.
