{*======================================================================*
 | unitRTF2HTML                                                         |
 |                                                                      |
 | RTF to HTML converter.                                               |
 |                                                                      |
 | Limitations                                                          |
 | -----------                                                          |
 | This is designed to convert XanaNews messages only, so its scope is  |
 | limited.  It doesn't do tables, embedded images, etc. etc. etc.      |
 |
 | Handling of out-of-order tags is limited - eg.                       
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
 | Copyright © Colin Wilson 2004  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      07/03/2005  CPWW  Original                                  |
 *======================================================================*}


unit unitRTF2HTML;

interface

uses
  Windows, Classes, SysUtils;

function RTF2HTML(const RTF: string; RawFragment: Boolean = False): string;

implementation

type
// nb.  The token strings below *must* be in alphabetical order
  TRTFTokens = (rtAnsi, rtAnsiCPG,
                rtBold, rtBoldNone,
                rtCf, rtColorTbl,
                rtDeff, rtDefLang,
                RTF, rtFontTbl, rtFs,
                rtItalic, rtItalicNone,
                rtPar, rtPard, rtPlain,
                rtRtf,
                rtUc,
                rtUnderline,
                rtUnderlineNone,
                rtViewkind, rtUnknown);
var
  RTFTokens: array [Low (TRTFTokens)..Pred (rtUnknown)] of string =
    ('ansi', 'ansicpg',
     'b', 'bnone',
     'cf', 'colortbl',
     'deff', 'deflang',
     'f', 'fonttbl', 'fs',
     'i', 'inone',
     'par', 'pard', 'plain',
     'rtf',
     'uc', 'ul', 'ulnone',
     'viewkind');

{*----------------------------------------------------------------------*
 | function FindToken                                                   |
 |                                                                      |
 | Convert a token string to a token                                    |
 *----------------------------------------------------------------------*}
function FindToken (const token: string): TRTFTokens;

  function bsearch (s, e: TRTFTokens): TRTFTokens;
  var
    m: TRTFTokens;
    c: Integer;
    si, ei: Integer;
  begin
    si := Integer (s);
    ei := Integer (e);
    if ei = 255 then
      ei := -1;
    if si <= ei then
    begin
      m := TRTFTokens (si + (ei - si) div 2);
      c := AnsiCompareText (token, RTFTokens [m]);
      if c > 0 then
        Result := bsearch (Succ (m), e)
      else
        if c < 0 then
          Result := bsearch (s, Pred (m))
        else
          Result := m
    end
    else
      Result := rtUnknown;
  end;

begin
  Result := bsearch (Low (TRTFTokens), Pred (rtUnknown))
end;

{*----------------------------------------------------------------------*
 | function RTF2HTML                                                    |
 |                                                                      |
 | Convert an RTF string to an HTML string                              |
 *----------------------------------------------------------------------*}
function RTF2HTML (const RTF: string; RawFragment: Boolean = False): string;
var
  p: PChar;
  ch: char;
  value: string;
  token: TRTFTokens;
  HTMLTagStack: TStringList;
  inBody: Boolean;
  colors: TList;
  inTags: Boolean;

  procedure Error (const st: string);
  begin
    raise Exception.Create(st);
  end;

  procedure CheckInBody; forward;

// ------ Basic HTML generation routines...
  procedure EmitChar (ch: char);
  begin
    CheckInBody;
    Result := Result + ch;
  end;

  procedure EmitStr (const st: string);
  begin
    CheckInBody;
    Result := Result + st
  end;

  procedure EmitText (const st: string);
  begin
    if inTags then
    begin
      inTags := False;
      EmitStr (#13#10)
    end;
    EmitStr (st)
  end;

  procedure EmitTextChar (ch: char);
  begin
    if inTags then
    begin
      inTags := False;
      EmitStr (#13#10)
    end;
    case ch of
      '>': EmitStr ('&gt;');
      '<': EmitStr ('&lt;');
      '"': EmitStr ('&quot;');
      '&': EmitStr ('&amp;');
      else
        EmitChar (ch)
    end
  end;

  //
  // Emit an HTML tag and push it onto the HTML tag stack so that it will
  // automatically get closed.
  //
  procedure EmitTag (const tag, params: string);
  begin
    // Optimization - don't do <BODY><P>.  Just <BODY> will do

    if not inBody and (tag = 'P') and (params = '') then
    begin
      CheckInBody;
      exit
    end;

    // Emit the tag and parameters
    EmitChar ('<');
    EmitStr (tag);
    if params <> '' then
    begin
      EmitChar (' ');
      EmitStr (params)
    end;
    EmitChar ('>');

    // Add the tag to the HTML tag stack
    if Copy (tag, 1, 1) <> '/' then
    begin
      HTMLTagStack.Insert(0, tag);
      inTags := True
    end
  end;

  //
  // *only* if the tag is on the stack, pop and emit all tags up to and
  // including the tag.
  //
  procedure PopTag (const tag: string);
  var
    st: string;
  begin
    if HTMLTagStack.IndexOf(tag) >= 0 then
      while (HTMLTagStack.Count > 0) do
      begin
        st := HTMLTagStack [0];

        HTMLTagStack.Delete(0);
        if st <> '{' then
        begin
          EmitTag ('/' + st, '');
          EmitStr (#13#10)
        end;
        if st = tag then
          break
      end
  end;

  procedure CheckInBody;
  begin
    if not inBody then
    begin
      inBody := True;
      if not RawFragment then
        EmitTag ('BODY', '');
    end
  end;

// -----  Basic parsing routines

  function GetChar: char;
  begin
    ch := p^;
    Result := ch;
    Inc (p)
  end;

  function GetValue: string;
  begin
    value := '';
    while not (GetChar in [' ', '\', '{', ';', #13, #10]) do
      value := value + ch;
    if ch <> ' ' then Dec (p);
    Result := value
  end;

  function GetToken: TRTFTokens;
  var
    st: string;
  begin
    st := '';
    while GetChar in ['A'..'Z', 'a'..'z'] do
      st := st + ch;
    Dec (p);
    token := FindToken (st);
    Result := token
  end;

  function GetTokenValue: string;
  begin
    value := '';
    while GetChar in ['A'..'Z', 'a'..'z'] do
      value := value + ch;
    Dec (p);
    Result := value;
  end;

//----- Parse a font group structure

  procedure GetFontGroup;

    procedure GetFontNameValue;
    begin
      Value := '';

      while not (GetChar in [';', '\']) do
        Value := Value + ch;
      Dec (p);
    end;

  begin
    while not (GetChar in [#0, '}']) do
      case ch of
        '\': begin
                GetToken;
                GetValue;
              end;
        ' ': GetFontNameValue;
        ';': if GetChar <> '}' then
                Error ('} expected after ; in font table')
              else
                break;
        '{' :
          Error ('Unexpected { in font table');
      end
  end;

//----- Parse a colour group

  procedure GetColorGroup;
  var
    st: string;
    rVal, gVal, bVal: byte;
  begin
    rVal := 0;
    gVal := 0;
    bVal := 0;
    while ch <> #0 do
      case ch of
        '\': begin
                st := GetTokenValue;
                GetValue;

                if SameText (st, 'red') then
                  rVal := StrToIntDef (value, 0)
                else
                  if SameText (st, 'green') then
                    gVal := StrToIntDef (value, 0)
                  else
                    if SameText (st, 'blue') then
                      bVal := StrToIntDef (value, 0);
                while GetChar = ' ' do;
              end;
        ';': begin
                if colors = Nil then colors := TList.Create;
                colors.Add(Pointer (RGB (rVal, gVal, bVal)));
                GetChar;
                break
              end;
        ' ': GetChar ();
        else
          Error ('\ expected in color table');
      end
  end;

// -----  Parse a group (between '{' and '}').  Does most of the work

  procedure GetGroup;
  var
    intVal: Integer;

    procedure ProcessFontTable;
    begin
      while GetChar = '{' do
        GetFontGroup;
      if ch <> '}' then
        Error ('} expected in Font Table')
      else
        Dec (p)
    end;

    procedure ProcessColorTable;
    begin
      while not (ch in  ['}', #0]) do
        GetColorGroup;
      Dec (p)
    end;

    procedure ProcessToken;
    begin
      GetToken;
      GetValue;
      case Token of
        rtFontTbl:
          ProcessFontTable;
        rtColorTbl:
          ProcessColorTable;

        rtBold:
          if value = '0' then PopTag ('B') else EmitTag ('B', '');
        rtBoldNone:
          PopTag ('B');

        rtItalic:
          if value = '0' then PopTag ('I') else EmitTag ('I', '');
        rtItalicNone:
          PopTag ('I');

        rtUnderline:
          if value = '0' then PopTag ('U') else EmitTag ('U', '');
        rtUnderlineNone:
          PopTag ('U');

        rtPard:
          begin
            PopTag ('P');
            EmitTag ('P', '');
          end;
        rtPar:
          EmitText ('<BR>'+#13#10);

        rtPlain:
          begin
            PopTag ('B');
            PopTag ('U');
            PopTag ('I');
            PopTag ('FONT')
          end;
        rtCf:
          begin
            intVal := StrToIntDef (value, -1);
            if (intVal >= 0) and (colors <> Nil) and (intVal < colors.Count) then
            begin
              intVal := Integer (colors [intVal]);
              PopTag ('FONT');
              EmitTag ('FONT', Format ('Color=#%2.2x%2.2x%2.2x', [
                getRValue (intVal),
                getGValue (intVal),
                getBValue (intVal)]));
            end
          end
      end
    end;

  begin { GetGroup }
    HTMLTagStack.Insert(0, '{');
    while not (GetChar in [#0, '}']) do
      case ch of
        #10, #13 :;
        '\' :
          case p [0] of
            '''':
              begin // Hex literal
                Value := p [1] + p [2];
                Inc (p, 3);
                EmitTextChar (Char (StrToInt ('$' + Value)));
              end;
            '{', '}', '/' :
              EmitTextChar (GetChar);
            else
              ProcessToken
          end;
        '{': GetGroup
        else
          if (ch = ' ') and (p [0] = ' ') then
            EmitTextChar (#$a0)
          else
            EmitTextChar (ch)
      end;
    PopTag ('{');
  end;

begin { RTF2HTML }
  inBody := False;              // Initialize globals
  Result := '';
  colors := nil;
  inTags := False;
  p := @RTF [1];

  HTMLTagStack := TStringList.Create;
  try
    HTMLTagStack.CaseSensitive := False;
    while GetChar <> #0 do
      case ch of
        #0: break;
        '{': GetGroup;
        '}': Error ('Mismatched curly brackets')
      end
  finally
                        // The HTML tag stack should be empty - but
                        // make sure...
    while HTMLTagStack.Count > 0 do
      PopTag (HTMLTagStack [HTMLTagStack.Count - 1]);

    HTMLTagStack.Free;
    colors.Free
  end
end;

end.
