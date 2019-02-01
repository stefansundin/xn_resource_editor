(*======================================================================*
 | cmpSpellChecker                                                      |
 |                                                                      |
 | ISpell spell checker component.                                      |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2003  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      13/02/2003  CPWW  Original                                  |
 *======================================================================*)


unit cmpSpellChecker;

interface

uses
  Windows, SysUtils, Classes, Contnrs, StrUtils;

type
//==============================================================
// TSpeller class
//
// Class for controlling ISpell.exe

  TSpeller = class
  private
    FCodePage: Integer;
    FProcessInformation: TProcessInformation;
    FHInputWrite, FHErrorRead, FHOutputRead: THandle;
  public
    constructor Create(const APath, ACmd: string; ACodePage: Integer);
    destructor Destroy; override;
    procedure SpellCommand (const cmd: string);
    function GetResponse: string;
    function GetCheckResponse: string;
    property CodePage: Integer read FCodePage;
  end;

//==============================================================
// TISpellLanguage class
//
// Class used in fISpellLanguages object list.  Contains the details
// of the supported language variants

  TISpellLanguage = class
  private
    FCmd: string;
    FPath: string;
    FCodePage: Integer;
    FLang: Integer;
    FName: string;
  public
    constructor Create(const APath, AName: string; ACodePage, ALang: Integer; const ACmd: string);

    property CodePage: Integer read FCodePage;
    property Cmd: string read FCmd;      // Cmd to send to ISpell.exe
    property Lang: Integer read FLang;   // Language ID
    property Name: string read FName;
    property Path: string read FPath;
  end;

//==============================================================
// TSpellChecker class
//
// Class for checking words or strings against the ISpell
// dictionary

  TSpellChecker = class(TComponent)
  private
    FLanguageIdx: Integer;
    FSpeller: TSpeller;
    FQuoteChars: string;
    procedure SetLanguageIdx(const Value: Integer);
    procedure Initialize;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function LanguageCount: Integer;
    class function Language(idx: Integer): TISpellLanguage;

    function CheckWord (const ws: WideString; Suggestions: TStrings = Nil): Boolean;
    function Check(const ws: WideString; startPos: Integer; var selStart, selEnd: Integer; Suggestions: TStrings = nil; SkipFirstLine: Boolean = False): Boolean;
    procedure Add (const word: WideString);
    property LanguageIdx: Integer read FLanguageIdx write SetLanguageIdx default -1;
    property QuoteChars: string read FQuoteChars write FQuoteChars;
  end;

EISpell = class (Exception)
end;

var
  gDefaultISpellLanguage: Integer = -1;

implementation

uses
  ActiveX, unitDefRegistry, iniFiles, unitCharsetMap, unitSearchString;

var
  gISpellLanguages: TObjectList = nil;
  gCoInitialize: Boolean = False;


{ TSpellChecker }

(*----------------------------------------------------------------------*
 | procedure TSpellChecker.Add                                          |
 |                                                                      |
 | Add a word to the ISpell dictionary                                  |
 |                                                                      |
 | Parameters:                                                          |
 |   const word: WideString             The word to add                 |
 *----------------------------------------------------------------------*)
procedure TSpellChecker.Add(const word: WideString);
begin
  if not Assigned(FSpeller) then
    Initialize;
  if Assigned(FSpeller) then
    FSpeller.SpellCommand('*' + WideStringToString (word, FSpeller.CodePage));
end;

(*----------------------------------------------------------------------*
 | function TSpellChecker.Check                                         |
 |                                                                      |
 | Check the words in a string.  Return false on the first misspelt     |
 | word, or True if there aren't any misspelt words.                    |
 |                                                                      |
 | If it returns false, also fill in the selStart and selEnd variables  |
 | to indicate the(1-based) position and length of the misspelt        |
 | word, and fil in the optional list of Suggestions.                   |
 |                                                                      |
 | Parameters:                                                          |
 |   const ws: WideString;      The string of words to check            |
 |   var startPos: Integer;     The starting position for the search    |
 |   var selStart: Integer     Returns position of first misspelling   |
 |   var selEnd: Integer;      Returns the end position of the "       |
 |   Suggestions: TStrings     Optional list of Suggestions filled in  |
 |                              when there is a misspelling.            |
 *----------------------------------------------------------------------*)
function TSpellChecker.Check(const ws: WideString; startPos: Integer;
  var selStart, selEnd: Integer; Suggestions: TStrings; SkipFirstLine: Boolean): Boolean;
var
  l, p, lp: Integer;
  sw, ew: Integer;
  ch: WideChar;
  InQuoteLine: Boolean;

  //--------------------------------------------------------------
  // Check the individual word at sw..ew
  function DoCheck: Boolean;
  begin
    if sw <> -1 then
    begin
      Result := CheckWord (Copy(ws, sw, ew - sw + 1), Suggestions);
      if not Result then
      begin
        selStart := sw;
        selEnd := ew
      end;
      sw := -1
    end
    else
      Result := True;
  end;

begin
  l := Length(ws);
  if l = 0 then
  begin
    Result := True;
    exit
  end;

  sw := -1;
  ew := -1;
  p := StartPos;

  // Skip first line if requested - eg. to climb over
  // quote header, etc.
  if SkipFirstLine then
  begin
    ch := ' ';
    while(p <= l) do
    begin
      ch := ws[p];
      if (ch = #$a) or (ch = #$d) then
        break
      else
        Inc(p)
    end;

    while(p <= l) and ((ch = #$a)or (ch = #$d)) do
    begin
      Inc(p);
      ch := ws[p]
    end;
  end;

  Result := True;

  InQuoteLine := False;
  lp := p;

  // If the StartPos wasn't at the beginning, detect
  // whether we're in the middle of a quote line.

  while(lp > 1) do
  begin
    ch := ws[lp - 1];
    if (ch = #$d) or (ch = #$a) then
    begin
      InQuoteLine := Pos (ws[lp], QuoteChars) > 0;
      break
    end
    else
      Dec(lp)
  end;

                                // Find each word
  while Result and (p <= l) do
  begin
    ch := ws[p];

        // Keep track of 'start of line' so we can
        // detect quoted lines.
    if (ch = #$d) or (ch = #$a) then
      lp := 0
    else
      if lp = 1 then
        InQuoteLine := Pos (ch, QuoteChars) > 0;

    if IsWideCharAlNum (ch) or (word (ch) = Word ('''')) then
    begin
      if sw = -1 then
        sw := p;
      ew := p
    end
    else
      if sw <> -1 then
        if not InQuoteLine then
          Result := DoCheck       // End of word found.  Check it.
        else
          sw := -1;
    Inc(p);
    Inc(lp);
  end;
  if Result and (sw <> -1) and not InQuoteLine then
    Result := DoCheck;          // Check final word
end;

(*----------------------------------------------------------------------*
 | function TSpellChecker.CheckWord                                     |
 |                                                                      |
 | Check an individual word                                             |
 |                                                                      |
 | Parameters:                                                          |
 |   const ws: WideString               The word to check               |
 |   Suggestions: TStrings              Optional Suggestions returned   |
 |                                      if the word was misspelt        |
 |                                                                      |
 | The function returns False if the word was misspelt                  |
 *----------------------------------------------------------------------*)
function TSpellChecker.CheckWord(const ws: WideString;
  Suggestions: TStrings): Boolean;
var
  resp: string;
begin
  if not Assigned(FSpeller) then
    Initialize;
  if Assigned(FSpeller) then
  begin                         // Send word to ispell.exe
    FSpeller.SpellCommand(WideStringToString (ws, FSpeller.CodePage));
    resp := FSpeller.GetResponse;
    Result := resp = '';        // If blank line received the word was
                                // spelt OK.
    if Assigned(Suggestions) then
    begin
      Suggestions.BeginUpdate;
      Suggestions.Clear;
      try
        if (not Result) then
        begin                   // Misspelt word.  Parse the Suggestions
          SplitString (':', resp);

          while resp <> '' do
            Suggestions.Add(SplitString (',', resp))
        end
      finally
        Suggestions.EndUpdate
      end
    end
  end
  else
    Result := True;
end;

(*----------------------------------------------------------------------*
 | constructor TSpellChecker.Create                                     |
 |                                                                      |
 | Constructor for TSpellChecker                                        |
 *----------------------------------------------------------------------*)
constructor TSpellChecker.Create(AOwner: TComponent);
begin
  inherited;
  FLanguageIdx := -1;
end;

(*----------------------------------------------------------------------*
 | destructor TSpellChecker.Destroy                                     |
 |                                                                      |
 | Destructor for TSpellChecker                                         |
 *----------------------------------------------------------------------*)
destructor TSpellChecker.Destroy;
begin
  FSpeller.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | Initialize the speller with the appropriate language                 |
 *----------------------------------------------------------------------*)
procedure TSpellChecker.Initialize;
begin
  if FLanguageIdx = -1 then        // If not language was specified...
    FLanguageIdx := gDefaultISpellLanguage;

  FreeAndNil (FSpeller);
  if (FLanguageIdx >= 0) and Assigned(gISpellLanguages) and (FLanguageIdx < gISpellLanguages.Count) then
    with TISpellLanguage(gISpellLanguages[FLanguageIdx]) do
      FSpeller := TSpeller.Create(Path, Cmd, CodePage);
end;

(*----------------------------------------------------------------------*
 | procedure TSpellChecker.SetLanguage                                  |
 |                                                                      |
 | 'Set' method for the Language property                               |
 *----------------------------------------------------------------------*)
class function TSpellChecker.Language(idx: Integer): TISpellLanguage;
begin
  if (idx >= 0) and (idx < gISpellLanguages.Count) then
    Result := TISpellLanguage(gISpellLanguages[idx])
  else
    Result := nil
end;

class function TSpellChecker.LanguageCount: Integer;
begin
  if Assigned(gISpellLanguages) then
    Result := gISpellLanguages.Count
  else
    Result := 0
end;

procedure TSpellChecker.SetLanguageIDx(const Value: Integer);
begin
  if FLanguageIdx <> Value then
  begin
    FLanguageIdx := Value;
    if not (csDesigning in ComponentState) then
      Initialize
  end
end;

{ TISpellLanguage }

(*----------------------------------------------------------------------*
 | constructor TISpellLanguage.Create                                   |
 |                                                                      |
 | Constructor for TISpellLanguage                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   const APath,               // Initial values of fields             |
 |   const AName: string;                                               |
 |   ACodePage,                                                         |
 |   ALang: Integer;                                                    |
 |   const ACmd: string                                                 |
 *----------------------------------------------------------------------*)
constructor TISpellLanguage.Create(const APath, AName: string; ACodePage, ALang: Integer;
  const ACmd: string);
begin
  FPath := APath;
  FName := AName;
  FCodePage := ACodePage;
  FLang := ALang;
  FCmd := ACmd;
end;

procedure CreateSTDHandles (const sa: TSecurityAttributes; var hInputRead, hInputWrite, hOutputRead, hOutputWrite, hErrorRead, hErrorWrite: THandle);
var
  hOutputReadTmp: THandle;
  hInputWriteTmp: THandle;
  hErrorReadTmp: THandle;
begin
  hOutputReadTmp := 0;
  hInputWriteTmp := 0;
  hErrorReadTmp := 0;
  hInputRead := 0;
  hInputWrite := 0;
  hOutputRead := 0;
  hOutputWrite := 0;
  hErrorRead := 0;
  hErrorWrite := 0;

  try
    if not CreatePipe(hOutputReadTmp, hOutputWrite, @sa, 0) then
      RaiseLastOSError;

    if not CreatePipe(hErrorReadTmp, hErrorWrite, @sa, 0) then
      RaiseLastOSError;

    if not CreatePipe(hInputRead, hInputWriteTmp, @sa, 0) then
      RaiseLastOSError;

    if not DuplicateHandle(GetCurrentProcess, hOutputReadTmp, GetCurrentProcess, @hOutputRead, 0, FALSE, DUPLICATE_SAME_ACCESS) then
      RaiseLastOSError;

    if not DuplicateHandle(GetCurrentProcess, hErrorReadTmp, GetCurrentProcess, @hErrorRead, 0, FALSE, DUPLICATE_SAME_ACCESS) then
      RaiseLastOSError;

    if not DuplicateHandle(GetCurrentProcess, hInputWriteTmp, GetCurrentProcess, @hInputWrite, 0, FALSE, DUPLICATE_SAME_ACCESS) then
      RaiseLastOSError;

    CloseHandle(hOutputReadTmp); hOutputReadTmp := 0;
    CloseHandle(hInputWriteTmp); hInputWriteTmp := 0;
    CloseHandle(hErrorReadTmp);  hErrorReadTmp  := 0;

  except
    if hOutputReadTmp  <> 0 then CloseHandle(hOutputReadTmp);
    if hInputWriteTmp  <> 0 then CloseHandle(hInputWriteTmp);
    if hErrorReadTmp   <> 0 then CloseHandle(hErrorReadTmp);
    if hInputRead      <> 0 then CloseHandle(hInputRead);    hInputRead   := 0;
    if hInputWrite     <> 0 then CloseHandle(hInputWrite);   hInputWrite  := 0;
    if hOutputRead     <> 0 then CloseHandle(hOutputRead);   hOutputRead  := 0;
    if hOutputWrite    <> 0 then CloseHandle(hOutputWrite);  hOutputWrite := 0;
    if hErrorRead      <> 0 then CloseHandle(hErrorRead);    hErrorRead   := 0;
    if hErrorWrite     <> 0 then CloseHandle(hErrorWrite);   hErrorWrite  := 0;

    raise
  end
end;


{ TSpeller }

(*----------------------------------------------------------------------*
 | constructor TSpeller.Create                                          |
 |                                                                      |
 | Create the ISpell.exe controller class.  Run ISpell with input       |
 | output and error pipes.                                              |
 |                                                                      |
 | Parameters:                                                          |
 |   const APath,                                                       |
 |   ACmd: string;                                                      |
 |   ACodePage: Integer                                                |
 *----------------------------------------------------------------------*)
constructor TSpeller.Create(const APath, ACmd: string; ACodePage: Integer);
var
  si: TStartupInfo;
  buf: string;
  hInputRead, hOutputWrite, hErrorWrite: THandle;
  sa: TSecurityAttributes;
begin
  FCodePage := ACodePage;
  SetEnvironmentVariable('HOME', PChar (APath));

  sa.nLength := SizeOf(TSecurityAttributes);
  sa.lpSecurityDescriptor := nil;
  sa.bInheritHandle := True;

  FillChar (FProcessInformation, SizeOf(FProcessInformation), 0);
  FillChar (si, SizeOf(si), 0);

  CreateStdHandles (sa, hInputRead, FHInputWrite, FHOutputRead, hOutputWrite, FHErrorRead, hErrorWrite);

  try
    si.cb := SizeOf(si);
    si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    si.hStdOutput := hOutputWrite;
    si.hStdInput := hInputRead;
    si.hStdError := hErrorWrite;
    si.wShowWindow := SW_HIDE;

    if not CreateProcess (nil, PChar (ACmd),  @sa, @sa, True, 0, nil, nil, si, FProcessInformation) then
      RaiseLastOSError;
  finally
    CloseHandle(hOutputWrite);
    CloseHandle(hInputRead);
    CloseHandle(hErrorWrite);
  end;

  buf := GetCheckResponse;
  SpellCommand('!')
end;

(*----------------------------------------------------------------------*
 | destructor TSpeller.Destroy                                          |
 |                                                                      |
 | Destructor for ISpell.exe controller class                           |
 *----------------------------------------------------------------------*)
destructor TSpeller.Destroy;
begin
  if FProcessInformation.hProcess <> 0 then
  begin
    SpellCommand(^Z);     // Send Ctrl-Z to ISpell to terminate it
    WaitForSingleObject(FProcessInformation.hProcess, 1000);      // Wait for it to finish
  end;

  try
    if FProcessInformation.hThread <> 0 then CloseHandle(FProcessInformation.hThread);
    if FProcessInformation.hProcess <> 0 then CloseHandle(FProcessInformation.hProcess);
    if FHInputWrite <> 0 then CloseHandle(FHInputWrite);
    if FHErrorRead <> 0 then CloseHandle(FHErrorRead);
    if FHOutputRead <> 0 then CloseHandle(FHOutputRead);
  except on E: Exception do
    TerminateProcess(FProcessInformation.hProcess,0);
  end;
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TSpeller.GetCheckResponse                                   |
 |                                                                      |
 | Get a response.  If the response was on stderr, raise an exception   |
 *----------------------------------------------------------------------*)
function TSpeller.GetCheckResponse: string;
var
  l: DWORD;

 // ReadPipe
 //
 // Read from a pipe.  If there's nothing in the pipe return an empty string
  function ReadPipe(pipeHandle: THandle; var s: string): Boolean;
  var
    avail: DWORD;
  begin
    if not PeekNamedPipe(pipeHandle, nil, 0, nil, @avail, nil) then
      RaiseLastOSError;

    if avail > 0 then
    begin
      SetLength(s, avail + 512);

      if not ReadFile(pipeHandle, s[1], avail + 512, avail, nil) then
        RaiseLastOSError;

      SetLength(s, avail);
      Result := True
    end
    else
      Result := False
  end;

begin { GetCheckResponse }
  l := 20;
  Sleep (100);
  repeat
        { Try stdout first }
    if ReadPipe(FHOutputRead, Result) then
      break;

      {  Try stderr}
    if ReadPipe(FHErrorRead, Result) then
      raise EISpell.Create(Result);

    Sleep (100);
    Dec(l)
  until l = 0;

  if l = 0 then
    Result := ''; // raise EISpell.Create('Timeout in ISpell');

  Result := Trim (Result);
end;

(*----------------------------------------------------------------------*
 | function TSpeller.GetResponse                                        |
 |                                                                      |
 | Blocking call to get response from ISpell                            |
 *----------------------------------------------------------------------*)
function TSpeller.GetResponse: string;
var
  avail: DWORD;
begin
  SetLength(Result, 2048);
  if ReadFile(FHOutputRead, Result [1], 2048, avail, nil) then
  begin
    SetLength(Result, avail);
    Result := Trim (Result)
  end
  else
    raiseLastOSError;
end;

(*----------------------------------------------------------------------*
 | procedure TSpeller.SpellCommand                                      |
 |                                                                      |
 | Send a command to ISpell                                             |
 *----------------------------------------------------------------------*)
procedure TSpeller.SpellCommand(const cmd: string);
var
  n: DWORD;
begin
  WriteFile(FHInputWrite, (cmd + #13#10) [1], Length(cmd) + 2, n, Nil);
end;

procedure InitISpell;
var
  reg: TDefRegistry;
  ISpellPath, path, s, name, cmd: string;
  f: TSearchRec;
  sections: TStrings;
  i: Integer;
  sectionLanguage: Integer;

  function SpellerForLocale(locale: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;

    for i := 0 to gISpellLanguages.Count - 1 do
      if TISpellLanguage(gISpellLanguages[i]).Lang = locale then
      begin
        Result := i;
        break
      end
  end;

begin
  if CoInitialize(nil) = S_OK then
    gCoInitialize := True;
  sections := nil;
  reg := TDefRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\ispell.exe', False) then

    begin                       // Get the ISpell.exe path from the registry

      ISpellPath := IncludeTrailingPathDelimiter (reg.ReadString ('Path'));
      path := ExtractFilePath(ExcludeTrailingPathDelimiter (ISpellPath));

      if FindFirst(ISpellPath + '*.*', faDirectory, f) = 0 then
      try
        sections := TStringList.Create;

                                // Enumerate the subdirectories of the ISpell
                                // directory.  Each one will containe a language
        repeat
          if ((f.Attr and faDirectory) <> 0) and (Copy(f.Name, 1, 1) <> '.') then
            with TInIFile.Create(ISpellPath + f.Name + '\ISpell.ini') do
            try
              ReadSections (sections);
              sectionLanguage := 1033;
              for i := 0 to sections.Count - 1 do
              begin
                                // Read the language settings from the .INI file
                                // in the language directory
                s := sections[i];
                if s = '' then name := f.Name else name := s;

                cmd := ReadString (s, 'Cmd', '');
                cmd := StringReplace(cmd, '%UniRed%',path, [rfReplaceAll, rfIgnoreCase]);

                if not Assigned(gISpellLanguages) then
                  gISpellLanguages := TObjectList.Create;

                sectionLanguage := ReadInteger (s, 'LangNo', sectionLanguage);

                gISpellLanguages.Add(TISpellLanguage.Create(
                  ExcludeTrailingPathDelimiter (ISpellPath),
                  name,
                  MIMECharsetNameToCodePage(ReadString (s, 'Charset', 'us-ascii')),
                  sectionLanguage,
                  cmd))

              end;
            finally
              Free
            end
        until FindNext(f) <> 0
      finally
        FindClose(f)
      end;

      gDefaultISpellLanguage := SpellerForLocale(GetThreadLocale);

      if gDefaultISpellLanguage = -1 then
        gDefaultISpellLanguage := SpellerForLocale(GetUserDefaultLCID);

      if gDefaultISpellLanguage = -1 then
        gDefaultISpellLanguage := SpellerForLocale(GetSystemDefaultLCID);

      if gDefaultISpellLanguage = -1 then
        gDefaultISpellLanguage := SpellerForLocale(1033); // US English

      if (gDefaultISpellLanguage = -1) and (gISpellLanguages.Count > 0) then
        gDefaultISpellLanguage := 0
    end
  finally
    sections.Free;
    reg.Free
  end
end;

initialization
  InitISpell;

finalization
  gISpellLanguages.Free;
  if gCoInitialize then
    CoUninitialize
end.
