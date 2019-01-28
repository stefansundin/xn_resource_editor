unit unitCharsetMap;

interface

uses
  Windows, Classes, SysUtils, Graphics, MultiLanguage_TLB, RichEdit;

var
  CP_USASCII : Integer = 0;
  DefaultCodePage : Integer = 0;

function MIMECharsetNameToCodepage (const MIMECharsetName : string) : Integer;
function CharsetNameToCodepage (const CharsetName : string) : Integer;
function CodepageToMIMECharsetName (codepage : Integer) : string;
function CodepageToCharsetName (codepage : Integer) : string;
function CharsetToCodePage (FontCharset : TFontCharset) : Integer;
function CodePageToCharset (codePage : Integer) : TFontCharset;
function WideStringToString (const ws : WideString; codePage : Integer) : string;
function StringToWideString (const st : string; codePage : Integer) : WideString;
function StringToGDIString (const s : string; codePage : Integer) : string;
function GDIStringToString (const s : string; codePage : Integer) : string;
function URLSuffixToCodePage (urlSuffix : string) : Integer;
procedure GetCharsetNames (sl : TStrings);
function TrimEx(const S: string): string;
function IsWideCharAlpha (ch : WideChar) : boolean;
function IsWideCharAlnum (ch : WideChar) : boolean;
procedure FontToCharFormat(font: TFont; codePage : Integer; var Format: TCharFormatW);
function WideStringToUTF8 (const ws : WideString) : string;
function UTF8ToWideString (const st : string) : WideString;

var
  gIMultiLanguage : IMultiLanguage = Nil;
  gMultilanguageLoaded : boolean = False;
  gMultiLang : boolean = False;

implementation

uses
  ActiveX;

type
  TCharsetRec = record
    Name : string;
    URLSuffix : string;
    CodePage : Integer;
    CharSet : TFontCharSet;
    MIMECharsetName : string;
    CharsetName : string
  end;

var
  gCharsetMap : array [0..35] of TCharsetRec = (
    (Name:'US ASCII';                      URLSuffix:'';    CodePage: 0; Charset:0;   MIMECharsetName:'';            CharsetName:'ANSI_CHARSET'       ),
    (Name:'US ASCII';                      URLSuffix:'';    CodePage: 0; Charset:0;   MIMECharsetName:'us-ascii';    CharsetName:'ANSI_CHARSET'       ),
    (Name:'Western Europe (ISO)';          URLSuffix:'';    CodePage:28591; Charset:0;   MIMECharsetName:'iso-8859-1';        CharsetName:'ANSI_CHARSET'       ),
    (Name:'Western Europe (Windows)';      URLSuffix:'';    CodePage:01252; Charset:0;   MIMECharsetName:'windows-1252';      CharsetName:'ANSI_CHARSET'       ),
    (Name:'Latin (3)';                     URLSuffix:'';    CodePage:28593; Charset:162; MIMECharsetName:'iso-8859-3';        CharsetName:'TURKISH_CHARSET'     ),
    (Name:'Latin (9)';                     URLSuffix:'';    CodePage:28605; Charset:0;   MIMECharsetName:'iso-8859-15';       CharsetName:'ANSI_CHARSET'       ),
    (Name:'Thai';                          URLSuffix:'.th'; CodePage:00874; Charset:222; MIMECharsetName:'iso-8859-11';       CharsetName:'ARABIC_CHARSET'     ),
    (Name:'Japanese (JIS)';                URLSuffix:'.jp'; CodePage:50220; Charset:128; MIMECharsetName:'iso-2022-jp';       CharsetName:'SHIFTJIS_CHARSET'   ),
    (Name:'Japanese (EUC)';                URLSuffix:'';    CodePage:51932; Charset:128; MIMECharsetName:'euc-jp';          CharsetName:'SHIFTJIS_CHARSET'   ),
    (Name:'Japanese (EUC)';                URLSuffix:'';    CodePage:51932; Charset:128; MIMECharsetName:'x-euc-jp';          CharsetName:'SHIFTJIS_CHARSET'   ),
    (Name:'Japanese (Shift-JIS)';          URLSuffix:'';    CodePage:00932; Charset:128; MIMECharsetName:'shift-jis';    CharsetName:'SHIFTJIS_CHARSET'   ),
    (Name:'Japanese (Shift-JIS)';          URLSuffix:'';    CodePage:00932; Charset:128; MIMECharsetName:'x-shift-jis';    CharsetName:'SHIFTJIS_CHARSET'   ),
    (Name:'Chinese - Simplified (GB2312)'; URLSuffix:'';    CodePage:00936; Charset:134; MIMECharsetName:'gb2312';         CharsetName:'GB2312_CHARSET'     ),
    (Name:'Chinese - Simplified (HZ)';     URLSuffix:'';    CodePage:52936; Charset:134; MIMECharsetName:'hz-gb-2312';          CharsetName:'GB2312_CHARSET'     ),
    (Name:'Chinese - Traditional (Big5)';  URLSuffix:'.cn'; CodePage:00950; Charset:136; MIMECharsetName:'big5';              CharsetName:'CHINESEBIG5_CHARSET'),
    (Name:'Chinese - Traditional (Big5)';  URLSuffix:'';    CodePage:00950; Charset:136; MIMECharsetName:'x-big5';            CharsetName:'CHINESEBIG5_CHARSET'),
    (Name:'Korean';                        URLSuffix:'.kr'; CodePage:00949; Charset:129; MIMECharsetName:'iso-2022-kr';       CharsetName:'HANGEUL_CHARSET'    ),
    (Name:'Korean';                        URLSuffix:'';    CodePage:00949; Charset:129; MIMECharsetName:'KS_C_5601-1987';       CharsetName:'HANGEUL_CHARSET'    ),
    (Name:'Korean (EUC)';                  URLSuffix:'';    CodePage:51949; Charset:129; MIMECharsetName:'euc-kr';          CharsetName:'HANGEUL_CHARSET'    ),
    (Name:'Korean (EUC)';                  URLSuffix:'';    CodePage:51949; Charset:129; MIMECharsetName:'x-euc-kr';          CharsetName:'HANGEUL_CHARSET'    ),
    (Name:'Central European (ISO)';        URLSuffix:'';    CodePage:28592; Charset:238; MIMECharsetName:'iso-8859-2';        CharsetName:'EASTEUROPE_CHARSET' ),
    (Name:'Central European (Windows)';    URLSuffix:'';    CodePage:01250; Charset:238; MIMECharsetName:'windows-1250';        CharsetName:'EASTEUROPE_CHARSET' ),
    (Name:'Russian (KOI8-R)';              URLSuffix:'.ru'; CodePage:20878; Charset:204; MIMECharsetName:'koi8-r';            CharsetName:'RUSSIAN_CHARSET'    ),
    (Name:'Russian (KOI8-R)';              URLSuffix:'';    CodePage:20878; Charset:204; MIMECharsetName:'x-koi8-r';            CharsetName:'RUSSIAN_CHARSET'    ),
    (Name:'Russian (KOI8)';                URLSuffix:'';    CodePage:20866; Charset:204; MIMECharsetName:'koi8';            CharsetName:'RUSSIAN_CHARSET'    ),
    (Name:'Russian (KOI8)';                URLSuffix:'';    CodePage:20866; Charset:204; MIMECharsetName:'x-koi8';            CharsetName:'RUSSIAN_CHARSET'    ),
    (Name:'Russian (ISO)';                 URLSuffix:'';    CodePage:28595; Charset:204; MIMECharsetName:'iso-8859-5';        CharsetName:'RUSSIAN_CHARSET'    ),
    (Name:'Russian (Windows)';             URLSuffix:'';    CodePage:01251; Charset:204; MIMECharsetName:'windows-1251';              CharsetName:'RUSSIAN_CHARSET'    ),
    (Name:'Greek (ISO)';                   URLSuffix:'';    CodePage:28597; Charset:161; MIMECharsetName:'iso-8859-7';        CharsetName:'GREEK_CHARSET'      ),
    (Name:'Greek (Windows)';               URLSuffix:'.gr'; CodePage:01253; Charset:161; MIMECharsetName:'windows-1253';        CharsetName:'GREEK_CHARSET'      ),
    (Name:'Turkish (ISO)';                 URLSuffix:'.tr'; CodePage:28599; Charset:162; MIMECharsetName:'iso-8859-9';        CharsetName:'TURKISH_CHARSET'    ),
    (Name:'Hebrew (Windows)';              URLSuffix:'.il'; CodePage:01255; Charset:177; MIMECharsetName:'windows-1255';        CharsetName:'HEBREW_CHARSET'     ),
    (Name:'Hebrew (ISO)';                  URLSuffix:'';    CodePage:28598; Charset:177; MIMECharsetName:'iso-8859-9';        CharsetName:'HEBREW_CHARSET'     ),
    (Name:'Arabic (ISO)';                  URLSuffix:'';    CodePage:28596; Charset:178; MIMECharsetName:'iso-8859-6';        CharsetName:'ARABIC_CHARSET'     ),
    (Name:'Baltic (ISO)';                  URLSuffix:'';    CodePage:28594; Charset:186; MIMECharsetName:'iso-8859-4';        CharsetName:'BALTIC_CHARSET'     ),
    (Name:'Unicode (UTF-8)';               URLSuffix:'';    CodePage:65001; Charset:0;   MIMECharsetName:'utf-8';             CharsetName:'BALTIC_CHARSET'     )
  );

  CharsetMap : array of TCharsetRec;



procedure LoadMultilanguage;
type
  PMIMECPInfo = ^tagMIMECPInfo;
var
  enum : IEnumCodepage;
  p, info : PMIMECPInfo;
  i, j, c, ct : DWORD;
  found : boolean;
begin
  if (not gMultilanguageLoaded) or (giMultiLanguage = Nil) then
  begin
    gMultilanguageLoaded := True;
    gIMultiLanguage := CoCMultiLanguage.Create;
    gMultiLang := False;

    if Assigned(gIMultiLanguage) then
    begin
      gIMultiLanguage.EnumCodePages(MIMECONTF_MAILNEWS, enum);
      info := CoTaskMemAlloc (10* sizeof (tagMIMECPInfo));;
      try
        c := 2;
        while SUCCEEDED (enum.Next (10, info^, ct)) and (ct <> 0) do
        begin
          SetLength (CharsetMap, c + ct);
          if c = 2 then
          begin
            CharsetMap [0] := gCharsetMap [0];
            CharsetMap [1] := gCharsetMap [5]
          end;
          p := info;

          for i := 0 to ct - 1 do
          begin
            CharsetMap [i + c].name := p^.wszDescription;
            CharsetMap [i + c].CodePage := p^.uiCodePage;
            CharsetMap [i + c].CharSet  := p^.bGDICharset;
            CharsetMap [i + c].MIMECharsetName := p^.wszWebCharset;
            CharsetMap [i + c].CharsetName := '';
            Inc (p);
          end;
          Inc (c, ct);
        end;

        for i := 0 to c - 1 do
          for j := Low (gCharsetMap) to High (gCharsetMap) do
            if gCharsetMap [j].CodePage = charsetMap [i].CodePage then
            begin
              if charsetMap [i].URLSuffix = '' then
                charsetMap [i].URLSuffix := gCharsetMap [j].URLSuffix;
              if charsetMap [i].URLSuffix <> '' then
                break
            end;

        found := False;
        for i := 0 to c - 1 do
          if charsetMap [i].CodePage = DefaultCodePage then
          begin
            found := True;
            break
          end;

        if not Found then
          for i := Low (gCharsetMap) to High (gCharsetMap) do
            if gCharsetMap [i].CodePage = DefaultCodePage then
            begin
              SetLength (charsetMap, c + 1);
              charsetMap [c] := gCharsetMap [i];
              break;
            end;

      finally
        CoTaskMemFree (info)
      end;
      gMultiLang := True
    end
    else
    begin
      SetLength (CharsetMap, High (gCharsetMap) + 1);
      for i := Low (gCharsetMap) to High (gCharsetMap) do
        CharsetMap [i] := gCharsetMap [i]
    end
  end
end;

function MIMECharsetNameToCodepage (const MIMECharsetName : string) : Integer;
var
  i : Integer;
begin
  LoadMultiLanguage;

  if CompareText (MIMECharsetName, 'us-ascii') = 0 then
    result := CP_USASCII
  else
  begin
    result := 0;
    i := Pos ('-', MIMECharsetName);
    if i > 0 then
      if CompareText (Copy (MIMECharsetName, 1, i - 1), 'windows') = 0 then
        result := StrToIntDef (Copy (MIMECharsetName, i + 1, MaxInt), 1252);

    if result = 0 then
    begin
      result := CP_USASCII;
      for i := Low (CharsetMap) to High (CharsetMap) do
        if CompareText (CharsetMap [i].MIMECharsetName, MIMECharsetName) = 0 then
        begin
          result := CharsetMap [i].codepage;
          break
        end
    end
  end
end;

function CharsetNameToCodepage (const CharsetName : string) : Integer;
var
  i : Integer;
begin
  LoadMultiLanguage;
  result := CP_USASCII;
  for i := Low (CharsetMap) to High (CharsetMap) do
    if CompareText (CharsetMap [i].Name, CharsetName) = 0 then
    begin
      result := CharsetMap [i].codepage;
      break
    end
end;

function CodepageToMIMECharsetName (codepage : Integer) : string;
var
  i : Integer;
begin
  LoadMultiLanguage;
  result := '';

  for i := Low (CharsetMap) to High (CharsetMap) do
    if CharsetMap [i].Codepage = codepage then
    begin
      result := CharsetMap [i].MIMECharsetName;
      break
    end
end;

function CodepageToCharsetName (codepage : Integer) : string;
var
  i : Integer;
begin
  LoadMultiLanguage;
  result := '';

  for i := Low (CharsetMap) to High (CharsetMap) do
    if CharsetMap [i].Codepage = codepage then
    begin
      result := CharsetMap [i].Name;
      break
    end
end;

function CharsetToCodePage (FontCharset : TFontCharset) : Integer;
var
  i : Integer;
begin
  LoadMultiLanguage;
  result := CP_USASCII;
  for i := Low (CharsetMap) to High (CharsetMap) do
    if CharsetMap [i].CharSet = FontCharset then
    begin
      result := CharsetMap [i].CodePage;
      break
    end
end;

function CodePageToCharset (codePage : Integer) : TFontCharset;
var
  i : Integer;
begin
  LoadMultiLanguage;
  result := 0;
  if codepage <> 65001 then
    for i := Low (CharsetMap) to High (CharsetMap) do
      if CharsetMap [i].Codepage = codepage then
      begin
        result := CharsetMap [i].CharSet;
        break
      end
end;

function WideStringToString (const ws : WideString; codePage : Integer) : string;
var
  dlen, len : DWORD;
  mode : DWORD;
begin
  LoadMultiLanguage;

  len := Length (ws);
  dlen := len * 4;
  SetLength (result, dlen);  // Dest string may be longer than source string if it's UTF-8
  if codePage = -1 then
    codePage := CP_USASCII;

  if gMultiLang and (codePage <> CP_ACP) then
  begin
    mode := 0;
    if not SUCCEEDED (gIMultiLanguage.ConvertStringFromUnicode(mode, codepage, PWideChar (ws), len, PChar (result), dlen)) then
      dlen := 0
  end
  else
    dlen := WideCharToMultiByte(codePage, 0, PWideChar (ws), len, PAnsiChar(result), len * 4, nil, nil);

  if dlen = 0 then
    result := ws
  else
    SetLength (result, dlen)
end;

function StringToWideString (const st : string; codePage : Integer) : WideString;
var
  len, dlen, mode : DWORD;
begin
  LoadMultiLanguage;
  if codePage = -1 then
    codePage := CP_USASCII;
  if st = '' then
    result := ''
  else
  begin
    len := Length (st);
    dlen := len * 4;
    SetLength (result, dlen);

    if gMultiLang and (codePage <> CP_ACP) then
    begin
      mode := 0;
      if not SUCCEEDED (gIMultiLanguage.ConvertStringToUnicode(mode, codepage, PChar (st), len, PWideChar (result), dlen)) then
        dlen := 0;
    end
    else
      dlen := MultiByteToWideChar (codepage, 0, PAnsiChar(st), len, PWideChar (result), len * 4);

    if dlen = 0 then
      result := st
    else
      SetLength (result, dlen)
  end
end;

function URLSuffixToCodePage (urlSuffix : string) : Integer;
var
  i : Integer;
begin
  LoadMultiLanguage;
  urlSuffix := LowerCase (urlSuffix);
  result := CP_USASCII;
  if urlSuffix <> '' then
    for i := Low (CharsetMap) to High (CharsetMap) do
      if CharsetMap [i].URLSuffix = urlSuffix then
      begin
        result := CharsetMap [i].CodePage;
        break
      end
end;

procedure GetCharsetNames (sl : TStrings);
var
  i : Integer;
  lst : string;
begin
  LoadMultiLanguage;
  sl.Clear;
  lst := '~';
  for i := Low (CharsetMap) to High (CharsetMap) do
    if lst <> CharsetMap [i].Name then
    begin
      lst := CharsetMap [i].Name;
      sl.Add (lst)
    end
end;

function TrimEx(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and ((S[I] = ' ') or (s [i] = #9)) do Inc(I);
  if I > L then Result := '' else
  begin
    while (S[L] = ' ') or (s [l] = #9) do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;


function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result:= StrToIntDef(Buffer, GetACP);
end;

function StringToGDIString (const s : String; codePage : Integer) : string;
var
  cs : TFontCharset;
  i : Integer;
  destCP : Integer;
  mode : DWORD;
  len, dlen : DWORD;
begin
  LoadMultiLanguage;
  cs := CodePageToCharset (codePage);

  destCP := -1;
  for i := Low (CharsetMap) to High (CharsetMap) do
    if (CharsetMap [i].CharSet = cs) and (CharsetMap [i].CodePage < 2000) then
    begin
      destCP := CharsetMap [i].codePage;
      break
    end;

  if (destCP = -1) or (destCP = codePage) or (gIMultiLanguage.IsConvertible (codePage, destCP) <> S_OK) then
    result := s
  else
  begin
    mode := 0;
    len := Length (s);
    dlen := len * 4;
    SetLength (result, dlen);
    if not SUCCEEDED (gIMultiLanguage.ConvertString (mode, codepage, destCP, PChar (s), len, PChar (result), dlen)) then
      dlen := 0;

    if dlen = 0 then
      result := s
    else
      SetLength (result, dlen)
  end
end;

function GDIStringToString (const s : string; codePage : Integer) : string;
var
  cs : TFontCharset;
  i : Integer;
  srcCP : Integer;
  mode : DWORD;
  len, dlen : DWORD;
begin
  LoadMultiLanguage;
  cs := CodePageToCharset (codePage);

  srcCP := -1;
  for i := Low (CharsetMap) to High (CharsetMap) do
    if (CharsetMap [i].CharSet = cs) and (CharsetMap [i].CodePage < 2000) then
    begin
      srcCP := CharsetMap [i].codePage;
      break
    end;

  if (srcCP = -1) or (srcCP = codePage) or (gIMultiLanguage.IsConvertible (srcCP, codePage) <> S_OK) then
    result := s
  else
  begin
    mode := 0;
    len := Length (s);
    dlen := len * 4;
    SetLength (result, dlen);
    if not SUCCEEDED (gIMultiLanguage.ConvertString (mode, srcCP, codepage, PChar (s), len, PChar (result), dlen)) then
      dlen := 0;

    if dlen = 0 then
      result := s
    else
      SetLength (result, dlen)
  end
end;

function IsWideCharAlpha (ch : WideChar) : boolean;
var
  w : word;
begin
  w := Word (ch);

  if w < $80 then       // Ascii range
    result := (w >=  $41) and (w <=  $5a) or
              (w >=  $61) and (w <=  $7a)
  else
  if w < $250 then     // Latin & extensions
    result := (w >=  $c0) and (w <=  $d6) or
              (w >=  $d8) and (w <=  $f6) or
              (w >=  $f7) and (w <=  $ff) or
              (w >= $100) and (w <= $17f) or
              (w >= $180) and (w <= $1bf) or
              (w >= $1c4) and (w <= $233)
  else
  if w < $370 then  // IPA Extensions
    result := (w >= $250) and (w <= $2ad)

  else
  if w < $400 then // Greek & Coptic
    result := (w >= $386) and (w < $3ff)

  else
  if w < $530 then      // Cryllic
    result := (w >= $400) and (w <= $47f) or
              (w >= $500) and (w <= $52f)
  else
  if w < $590 then      // Armenian
    result := (w >= $531) and (w <= $556) or
              (w >= $561) and (w <= $587)
  else

    result := True;     // Can't be bothered to do any more - for the moment!
end;

function IsWideCharAlnum (ch : WideChar) : boolean;
var
  w : word;
begin
  w := Word (ch);
  if (w >= $30) and (w <= $39) then
    result := True
  else
    result := IsWideCharAlpha (ch)
end;

procedure FontToCharFormat(font: TFont; codePage : Integer; var Format: TCharFormatW);
var
  wFontName : WideString;
begin
  FillChar (Format, SizeOf (Format), 0);

  Format.cbSize := sizeof (Format);
  Format.dwMask := Integer (CFM_SIZE or CFM_COLOR or CFM_FACE or CFM_CHARSET or CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_STRIKEOUT);

  if Font.Color = clWindowText then
    Format.dwEffects := CFE_AUTOCOLOR
  else
    Format.crTextColor := ColorToRGB (Font.Color);

  if fsBold in Font.Style then
    Format.dwEffects := Format.dwEffects or CFE_BOLD;

  if fsItalic in Font.Style then
    Format.dwEffects := Format.dwEffects or CFE_ITALIC;

  if fsUnderline in Font.Style then
    Format.dwEffects := Format.dwEffects or CFE_UNDERLINE;

  if fsStrikeOut in Font.Style then
    Format.dwEffects := Format.dwEffects or CFE_STRIKEOUT;


  Format.yHeight := Abs (Font.Size) * 20;
  Format.yOffset := 0;
  Format.bCharSet := CodePageToCharset (codePage);

  case Font.Pitch of
    fpVariable: Format.bPitchAndFamily := VARIABLE_PITCH;
    fpFixed: Format.bPitchAndFamily := FIXED_PITCH;
    else Format.bPitchAndFamily := DEFAULT_PITCH;
  end;

  wFontName := font.Name;
  lstrcpynw (Format.szFaceName, PWideChar (wFontName),LF_FACESIZE - 1) ;
end;

function WideStringToUTF8 (const ws : WideString) : string;
begin
  result := WideStringToString (ws, CP_UTF8);
end;

function UTF8TOWideString (const st : string) : WideString;
begin
  result := StringToWideString (st, CP_UTF8);
end;

initialization
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
    CP_USASCII := 20127
  else
    CP_USASCII := CP_ACP;

  gCharsetMap [0].CodePage := CP_USASCII;
  gCharsetMap [1].CodePage := CP_USASCII;

  DefaultCodePage := LCIDToCodePage (SysLocale.DefaultLCID);
end.
