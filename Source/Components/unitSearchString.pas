unit unitSearchString;

interface

uses
  Windows, Classes, SysUtils, StrUtils, unitCharsetMap;

type
  TStrArray = array of string;
  TStringSearcher = class
  protected
    FSearchString: String;
    FOrWords: TStrArray;
    FNotWords: TStrArray;
    FAndWords: TStrArray;

    FCountOrWords: Integer;
    FCountAndWords: Integer;
    FCountNotWords: Integer;
    FCaseSensitive: Boolean;
  public
    constructor Create (const ASearchString: string; AcaseSensitive: Boolean);

    function Matches (AString: string): Boolean;
    procedure Parse (searchString: string); virtual; abstract;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

  TGoogleLikeStringSearcher = class (TStringSearcher)
  public
    procedure Parse (searchString: string); override;
  end;

  TWStrArray = array of WideString;
  TWideStringSearcher = class
  protected
    FCaseSensitive: Boolean;
    FSearchString: WideString;
    FOrWords: TWStrArray;
    FNotWords: TWStrArray;
    FAndWords: TWStrArray;

    FCountOrWords: Integer;
    FCountAndWords: Integer;
    FCountNotWords: Integer;
  public
    constructor Create (const ASearchString: WideString; ACaseSensitive: Boolean);

    function Matches (AString: WideString): Boolean;
    procedure Parse (searchString: WideString); virtual; abstract;
  end;

  TGoogleLikeWideStringSearcher = class (TWideStringSearcher)
  public
    procedure Parse (searchString: WideString); override;
  end;

function ExtractString (const search: string; var s: string): string;
function SplitString (const search: string; var s: string): string;
function SplitToken (var st: string): string;
function DelimPos (const delims: string; const st: string; out delim: char): Integer;
function DelimSplitString (const search: string; var s: string; out delim: char): string;
function WideContainsText (const AText, ASubText: WideString): Boolean;
function WidePosEx(const SubStr, S: WideString; Offset: Cardinal = 1): Integer;
function WideSplitString (const search: WideString; var s: WideString): WideString;
function WideStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
function SearchStringArray (arr: array of string; const st: string): Integer;
function StringArrayContains (arr: array of string; const st: string): Boolean;
function WideDequotedStr (st: WideString; q: WideChar = '"'): WideString;
function WideQuotedStr (st: WideString; q: WideChar = '"'): WideString;
function WildContains (const a, b: string): Boolean;
function WWildContains (const a, b: WideString): Boolean;


implementation

// nb.  Pos is a 'magic' function that internally has an (undocumented) wide version

function WideContainsText (const AText, ASubText: WideString): Boolean;
begin
  Result := Pos (WideUpperCase (ASubText), WideUpperCase (AText)) > 0;
end;

function WideContainsStr (const AText, ASubText: WideString): Boolean;
begin
  Result := Pos (ASubText, AText) > 0;
end;

function WidePosEx(const SubStr, S: WideString; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

(*----------------------------------------------------------------------*
 | function ExtractString: string                                      |
 |                                                                      |
 | Search for a substring.  If the substring is found, return the       |
 | characters up to the substring, and set the soure string to the      |
 | characters after it.  If it was not found, return an empty string    |
 | and leave the source string unchanged.                               |
 *----------------------------------------------------------------------*)
function ExtractString (const search: string; var s: string): string;
var
  p, l: Integer;
  pc: PChar;

begin
  l := Length (search);
  if l = 1 then
  begin
    pc := AnsiStrScan (PChar (s), search [1]);
    if pc = nil then
      p := 0
    else
      p := Integer (pc) - Integer (PChar (s)) + 1
  end
  else
    p := Pos (search, s);
  if p > 0 then
  begin
    Result := Trim (Copy (s, 1, p - 1));
    s := Trim (Copy (s, p + l, maxInt))
  end
  else
    Result := ''
end;

(*----------------------------------------------------------------------*
 | function SplitString: string                                        |
 |                                                                      |
 | Search for a substring.  If the substring is found, return the       |
 | characters up to the substring, and set the soure string to the      |
 | characters after it.  If it was not found, return the entire source  |
 | string, and set the source string to an empty string                 |
 *----------------------------------------------------------------------*)
function SplitString (const search: string; var s: string): string;
var
  p, l: Integer;
  pc: PChar;
begin
  l := Length (search);
  if l = 1 then
  begin
    pc := AnsiStrScan (PChar (s), search [1]);
    if pc = nil then
      p := 0
    else
      p := Integer (pc) - Integer (PChar (s)) + 1
  end
  else
    p := Pos (search, s);
  if p > 0 then
  begin
    Result := Trim (Copy (s, 1, p - 1));
    s := Trim (Copy (s, p + l, maxInt))
  end
  else
  begin
    Result := Trim (s);
    s := ''
  end
end;

function SplitToken (var st: string): string;
var
  p, p1: Integer;
begin
  p := Pos (' ', st);
  p1 := Pos (#9, st);
  if p = 0 then p := MaxInt;
  if p1 = 0 then p1 := MaxInt;
  if p < p1 then
    Result := SplitString (' ', st)
  else
    Result := SplitString (#9, st)
end;

function WideStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function WideSplitString (const search: WideString; var s: WideString): WideString;
var
  p, l: Integer;
  pc: PWideChar;
begin
  l := Length (search);
  if l = 1 then
  begin
    pc := WideStrScan (PWideChar (s), search [1]);
    if pc = nil then
      p := 0
    else
      p := (Integer (pc) - Integer (PWideChar (s))) div sizeof (WideChar) + 1
  end
  else
    p := Pos (search, s);
  if p > 0 then
  begin
    Result := Trim (Copy (s, 1, p - 1));
    s := Trim (Copy (s, p + l, maxInt))
  end
  else
  begin
    Result := Trim (s);
    s := ''
  end
end;

function WildContains (const a, b: string): Boolean;
var
  p, offs, l, l1: Integer;
begin
  l := Length (a);
  l1 := Length (b);

  if (l1 = 0) or (l = 0) then
  begin
    Result := False;
    Exit
  end;

  if b [l1] = '*' then
  begin
    if l1 = 1 then
      Result := True
    else
      Result := AnsiContainsStr (a, Copy (b, 1, l1 - 1));
    exit
  end;

  if b [1] = '*' then
  begin
    Result := AnsiContainsStr (a, Copy (b, 2, l1 - 1));
    exit
  end;

  offs := 1;
  repeat
    p := PosEx (b, a, offs);
    offs := 0;
    if p > 0 then
    begin
      if (p > 1) and (a [p - 1] in ['A'..'Z', '0'..'9']) then
      begin
        offs := p + 1;
        p := 0;
        continue
      end;

      if ((p + l1) < l) and (a [p + l1] in ['A'..'Z', '0'..'9']) then
      begin
        offs := p + l1 + 1;
        p := 0;
        continue
      end
    end
  until (p <> 0) or (offs = 0);

  Result := p <> 0
end;

function WWildContains (const a, b: WideString): Boolean;
var
  p, offs, l, l1: Integer;
begin
  l := Length (a);
  l1 := Length (b);

  if (l1 = 0) or (l = 0) then
  begin
    Result := False;
    Exit
  end;

  if b [l1] = '*' then
  begin
    Result := WideContainsStr (a, Copy (b, 1, l1 - 1));
    exit
  end;

  offs := 1;
  repeat
    p := WidePosEx (b, a, offs);
    offs := 0;
    if p > 0 then
    begin
      if (p > 1) and IsWideCharAlNum (a [p - 1]) then
      begin
        offs := p + 1;
        p := 0;
        continue
      end;

      if ((p + l1) < l) and IsWideCharAlNum (a [p + l1]) then
      begin
        offs := p + l1 + 1;
        p := 0;
        continue
      end
    end
  until (p <> 0) or (offs = 0);

  Result := p <> 0
end;

{ TStringSearcher }

constructor TStringSearcher.Create(const ASearchString: string;  ACaseSensitive: Boolean);
begin
  FCaseSensitive := ACaseSensitive;
  FSearchString := ASearchString;
  Parse (ASearchString)
end;

function TStringSearcher.Matches(AString: string): Boolean;
type
  TMatch = (mYes, mNo, mMaybe);
var
  i: Integer;
  ok: TMatch;

begin
  if not FCaseSensitive then
    AString := UpperCase (AString);
  ok := mMaybe;

  for i := 0 to FCountOrWords - 1 do
    if WildContains (AString, FOrWords [i]) then
    begin
      ok := mYes;
      break
    end;

  if ok = mMaybe then
    for i := 0 to FCountAndWords - 1 do
      if not WildContains (AString, FAndWords [i]) then
      begin
        ok := mNo;
        break
      end;

  if ok = mMaybe then
    for i := 0 to FCountNotWords - 1 do
      if WildContains (AString, FNotWords [i]) then
      begin
        ok := mNo;
        break
      end;

  if ok = mMaybe then
    Result := (FCountAndWords > 0) or (FCountNotWords > 0)
  else
    Result := ok = mYes

end;

{ TGoogleLikeStringSearcher }

procedure TGoogleLikeStringSearcher.Parse(searchString: string);
type
  tOP = (opAnd, opOr, opNot);
var
  l: Integer;
  s1: string;
  op: tOp;

  procedure AddToVarArray (var arr: TStrArray; const st: string; var n: Integer);
  begin
    if n = Length (arr) then
      SetLength (arr, n + 5);
    arr [n] := st;
    Inc (n)
  end;

begin
  if CompareText (FSearchString, searchString) = 0 then
    Exit;
  FSearchString := searchString;
  FCountAndWords := 0;
  FCountOrWords := 0;
  FCountNotWords := 0;
  if not FCaseSensitive then
    searchString := UpperCase (searchString);

  l := Length (searchString);
  op := opAnd;
  while l > 0 do
  begin
    case searchString [1] of
      '+' :
        begin
          op := opAnd;
          Delete (searchString, 1, 1);
          l := Length (searchString);
        end;
      '-' :
        begin
          op := opNot;
          Delete (searchString, 1, 1);
          l := Length (searchString);
        end
    end;

    if l = 0 then break;

    if searchString [1] = '"' then
    begin
      Delete (searchString, 1, 1);
      s1 := SplitString ('"', searchString)
    end
    else
    begin
      s1 := SplitString (' ', searchString);
      if UpperCase (s1) = 'OR' then
      begin
        op := opOR;
        l := Length (searchString);
        continue
      end
    end;

    if s1 <> '' then
      case op of
        opAnd: AddToVarArray (FAndWords, s1, FCountAndWords);
        opOr: AddToVarArray (FOrWords, s1, FCountOrWords);
        opNot: AddToVarArray (FNotWords, s1, FCountNotWords)
      end;

    op := opAnd;
    l := Length (searchString)
  end
end;

{ TWideStringSearcher }

constructor TWideStringSearcher.Create(const ASearchString: WideString; ACaseSensitive: Boolean);
begin
  FCaseSensitive := ACaseSensitive;
  FSearchString := ASearchString;
  Parse (ASearchString)
end;

function TWideStringSearcher.Matches(AString: WideString): Boolean;
type
  TMatch = (mYes, mNo, mMaybe);
var
  i: Integer;
  ok: TMatch;

begin
  if not FCaseSensitive then
    AString := WideUpperCase (AString);
  ok := mMaybe;

  for i := 0 to FCountOrWords - 1 do
    if WWildContains (AString, FOrWords [i]) then
    begin
      ok := mYes;
      break
    end;

  if ok = mMaybe then
    for i := 0 to FCountAndWords - 1 do
      if not WWildContains (AString, FAndWords [i]) then
      begin
        ok := mNo;
        break
      end;

  if ok = mMaybe then
    for i := 0 to FCountNotWords - 1 do
      if WWildContains (AString, FNotWords [i]) then
      begin
        ok := mNo;
        break
      end;

  if ok = mMaybe then
    Result := (FCountAndWords > 0) or (FCountNotWords > 0)
  else
    Result := ok = mYes
end;

{ TGoogleLikeWideStringSearcher }

procedure TGoogleLikeWideStringSearcher.Parse(searchString: WideString);
type
  tOP = (opAnd, opOr, opNot);
var
  l: Integer;
  s1: WideString;
  op: tOp;

  procedure AddToVarArray (var arr: TWStrArray; const st: WideString; var n: Integer);
  begin
    if n = Length (arr) then
      SetLength (arr, n + 5);
    arr [n] := st;
    Inc (n)
  end;

begin
  if WideCompareText (FSearchString, searchString) = 0 then
    Exit;
  FSearchString := searchString;
  FCountAndWords := 0;
  FCountOrWords := 0;
  FCountNotWords := 0;
  if not FCaseSensitive then
    searchString := WideUpperCase (searchString);

  l := Length (searchString);
  op := opAnd;
  while l > 0 do
  begin
    case searchString [1] of
      '+' :
        begin
          op := opAnd;
          Delete (searchString, 1, 1);
          l := Length (searchString);
        end;
      '-' :
        begin
          op := opNot;
          Delete (searchString, 1, 1);
          l := Length (searchString);
        end
    end;

    if l = 0 then break;

    if searchString [1] = '"' then
    begin
      Delete (searchString, 1, 1);
      s1 := WideSplitString ('"', searchString)
    end
    else
    begin
      s1 := WideSplitString (' ', searchString);
      if WideUpperCase (s1) = 'OR' then
      begin
        op := opOR;
        l := Length (searchString);
        continue
      end
    end;

    if s1 <> '' then
      case op of
        opAnd:
          AddToVarArray (FAndWords, s1, FCountAndWords);
        opOr:
          AddToVarArray (FOrWords, s1, FCountOrWords);
        opNot:
          AddToVarArray (FNotWords, s1, FCountNotWords)
      end;

    op := opAnd;
    l := Length (searchString)
  end
end;

function SearchStringArray (arr: array of string; const st: string): Integer;

  function bsearch (s, e: Integer): Integer;
  var
    m, c: Integer;
  begin
    if s <= e then
    begin
      m := s + (e - s) div 2;

      c := CompareText (st, arr [m]);

      if c = 0 then
        Result := m
      else
        if c > 0 then
          Result := bsearch (m + 1, e)
        else
          Result := bsearch (s, m - 1)
    end
    else
      Result := -1
  end;

begin
  Result := bsearch (Low (arr), High (arr))
end;

function StringArrayContains (arr: array of string; const st: string): Boolean;
begin
  Result := SearchStringArray (arr, st) >= 0
end;

function WideDequotedStr (st: WideString; q: WideChar): WideString;
var
  i, l: Integer;
begin
  l := Length (st);
  if (l > 0) and (st [1] = q) then
  begin
    i := 2;
    while i <= l do
      if st [i] = q then
      begin
        if (i + 1 <= l) and (st [i + 1] = q) then
        begin
          Delete (st, i, 1);
          Inc (i);
          Dec (l);
        end
        else
        begin
          Dec (i,2);
          break
        end
      end
      else
        Inc (i);
    Result := Copy (st, 2, i);
  end
end;

function WideQuotedStr (st: WideString; q: WideChar): WideString;
var
  I: Integer;
begin
  Result := St;
  for I := Length(Result) downto 1 do
    if Result[I] = q then Insert(q, Result, I);
  Result := q + Result + q;
end;

function DelimPos (const delims: string; const st: string; out delim: char): Integer;
var
  i, p: Integer;
begin
  if delims = '' then
  begin
    Result := 0;
    exit
  end;

  Result := MaxInt;
  for i := 1 to Length (delims) do
  begin
    p := Pos (delims [i], st);
    if (p > 0) and (p < Result) then
    begin
      delim := delims [i];
      Result := p
    end
  end;

  if Result = MaxInt then
    Result := 0
end;

function DelimSplitString (const search: string; var s: string; out delim: char): string;
var
  p: Integer;
begin
  p := DelimPos (search, s, delim);
  if p > 0 then
  begin
    Result := Trim (Copy (s, 1, p - 1));
    s := Trim (Copy (s, p + 1, maxInt))
  end
  else
  begin
    Result := Trim (s);
    s := ''
  end
end;

end.
