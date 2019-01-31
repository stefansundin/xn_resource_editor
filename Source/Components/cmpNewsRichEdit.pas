(*======================================================================*
 | cmpNewsRichEdit                                                      |
 |                                                                      |
 | Handles separate fonts for quote levels, signatures & headers.       |
 | Handles Bold, Underline, Italic.                                     |
 |                                                                      |
 | nb.  TNewsRichEdit automatically detects quotes and signatures, but  |
 |      it can't detect header lines.  Instead, you must prefix header  |
 |      lines with character (1)                                        |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      26/11/2002  CPWW  Original                                  |
 *======================================================================*)

unit cmpNewsRichEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics, RichEdit, cmpCWRichEdit;

type
  TNewsRichEdit = class;

//----------------------------------------------------------------------
// TNewsCharFormatter - Formats NNTP messages.  Handles separate fonts
// for quote levels, signatures & headers.  Handles Bold, Underline, Italic

  TNewsCharFormatter = class (TCharFormatter)
  private
    FIsFormatted: Boolean;
    FEOL: Boolean;
    FInSignature: Boolean;
    FInHighlight: Boolean;
    FStrictSigSeparator: Boolean;
  public
    constructor Create;
    procedure Reset; override;
    procedure GetFormattedChars (stream: TRichEditStream; var fc: TCharFormat); override;
  end;

//----------------------------------------------------------------------
// TNewsRichEdit class
  TNewsRichEdit = class(TExRichEdit)
  private
    FLevel3QuoteFont: TFont;
    FLevel1QuoteFont: TFont;
    FLevel2QuoteFont: TFont;
    FFormatter: TCharFormatter;
    FHeaderFont: TFont;
    FSignatureFont: TFont;
    FRawText: Boolean;
    FTruncateFrom: WideString;
    FStrictSigSeparator: Boolean;
    procedure SetLevel1QuoteFont(const Value: TFont);
    procedure SetLevel2QuoteFont(const Value: TFont);
    procedure SetLevel3QuoteFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetSignatureFont(const Value: TFont);
    procedure SetRawText(const Value: Boolean);
  protected
    function GetCharFormatter: TCharFormatter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TruncateFrom: WideString read FTruncateFrom write FTruncateFrom;
    property StrictSigSeparator: Boolean read FStrictSigSeparator write FStrictSigSeparator;
  published
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property Level1QuoteFont: TFont read FLevel1QuoteFont write SetLevel1QuoteFont;
    property Level2QuoteFont: TFont read FLevel2QuoteFont write SetLevel2QuoteFont;
    property Level3QuoteFont: TFont read FLevel3QuoteFont write SetLevel3QuoteFont;
    property SignatureFont: TFont read FSignatureFont write SetSignatureFont;
    property RawText: Boolean read FRawText write SetRawText;
  end;

implementation

uses unitCharsetMap;

{ TNewsRichEdit }

(*----------------------------------------------------------------------*
 | constructor TNewsRichEdit.Create                                     |
 |                                                                      |
 | Constructor for TNewsRichEdit                                        |
 *----------------------------------------------------------------------*)
constructor TNewsRichEdit.Create(AOwner: TComponent);
begin
  inherited;

  RawPaste := True;

  FLevel1QuoteFont := TFont.Create;
  FLevel2QuoteFont := TFont.Create;
  FLevel3QuoteFont := TFont.Create;
  FHeaderFont := TFont.Create;
  FSignatureFont := TFont.Create;

  FHeaderFont.Color := clGray;          // Set default colours
  FLevel1QuoteFont.Color := clTeal;
  FLevel2QuoteFont.Color := clOlive;
  FLevel3QuoteFont.Color := clMaroon;
  FSignatureFont.Color := clGreen;
end;

(*----------------------------------------------------------------------*
 | destructor TNewsRichEdit.Destroy                                     |
 |                                                                      |
 | Destructor for TNewsRichEdit                                         |
 *----------------------------------------------------------------------*)
destructor TNewsRichEdit.Destroy;
begin
  FHeaderFont.Free;
  FLevel1QuoteFont.Free;
  FLevel2QuoteFont.Free;
  FLevel3QuoteFont.Free;
  FSignatureFont.Free;

  FreeAndNil (FFormatter);

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TNewsRichEdit.GetCharFormatter                              |
 |                                                                      |
 | Get the TCharFormatter.  This will be a TNewsCharFormatter unless    |
 | 'RawText' is specified - in which case it will be just a             |
 | TCharFormatter                                                       |
 *----------------------------------------------------------------------*)
function TNewsRichEdit.GetCharFormatter: TCharFormatter;
begin
  if not Assigned (FFormatter) then
    if RawText then
      FFormatter := TCharFormatter.Create
    else
    begin
      FFormatter := TNewsCharFormatter.Create;
      TNewsCharFormatter (FFormatter).FStrictSigSeparator := StrictSigSeparator
    end;
  Result := FFormatter;
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetHeaderFont                                |
 |                                                                      |
 | Set method for the HeaderFont property                               |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetLevel1QuoteFont                           |
 |                                                                      |
 | Set method for the Level1QuoteFont property                          |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetLevel1QuoteFont(const Value: TFont);
begin
  FLevel1QuoteFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetLevel2QuoteFont                           |
 |                                                                      |
 | Set method for the Level2QuoteFont property                          |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetLevel2QuoteFont(const Value: TFont);
begin
  FLevel2QuoteFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetLevel3QuoteFont                           |
 |                                                                      |
 | Set method for the Level3QuoteFont property                          |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetLevel3QuoteFont(const Value: TFont);
begin
  FLevel3QuoteFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetRawText                                   |
 |                                                                      |
 | Set method for the RawText property                                  |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetRawText(const Value: Boolean);
begin
  if FRawText <> Value then
  begin
    FRawText := Value;
    FreeAndNil (FFormatter)     // It will be recreated when it's next required
                                // in the appropriate raw or not raw type
  end
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetSignatureFont                             |
 |                                                                      |
 | Set method for the SignatureFont property                            |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetSignatureFont(const Value: TFont);
begin
  FSignatureFont.Assign (Value)
end;

{ TNewsCharFormatter }

(*----------------------------------------------------------------------*
 | constructor TNewsCharFormatter.Create                                |
 |                                                                      |
 | Constructor for TNewsCharFormatter                                   |
 *----------------------------------------------------------------------*)
constructor TNewsCharFormatter.Create;
begin
  Reset
end;

(*----------------------------------------------------------------------*
 | TNewsCharFormatter.GetFormattedChars                                 |
 |                                                                      |
 | Bit of a black hole this - sorry!                                    |
 *----------------------------------------------------------------------*)
procedure TNewsCharFormatter.GetFormattedChars(stream: TRichEditStream; var fc: TCharFormat);
var
  c, pc: WideChar;
  p, p1: PWideChar;
  i, n, n1: DWORD;
  sol, trc: Boolean;
  own: TNewsRichEdit;
  quoteLevel: Integer;
  hl: TFontStyles;

begin
  own := TNewsRichEdit (stream.Owner);
  trc := own.TruncateFrom <> '';
  with stream do
  begin
    i := ChunkStart;
    if ChunkStart >= Len then Exit;
    p := Buffer;
    Inc(p, ChunkStart);
    sol := False;

    if FEOL then        // When we finished last time, we were at the end of
    begin               // a line.  So cancel all formatting
      fc := Owner.DefaultCharFormat;
      FIsFormatted := False;
      sol := True;
    end;

    if FInHighlight then        // When we finished last time we were at the end
    begin                       // of a highlighted block - before the terminating
      FInHighlight := False;    // indicators.  Clear the highlight flags and skip
                                // the terminating indicators
      while(p^ = '*') or (p^ = '/') or (p^ = '_') do
      begin
        Inc(p);
        Inc(i);
        ChunkStart := ChunkStart + 1;
      end;
      fc.dwEffects := fc.dwEffects and not (CFE_BOLD or CFE_ITALIC or CFE_UNDERLINE or CFE_LINK);
    end;

    pc := #13;
    while i < Len do
    begin
      c := p^;
                                // We're at the start of a line so look for quote
                                // level or signature indicators.  Also, lines that
                                // start with #1 are 'header' lines.
      if sol and not FInSignature then
      begin

        if c = #1 then          // Is it a header line?
        begin
          Owner.FontToCharFormat(own.FHeaderFont, fc);
          FIsFormatted := True;
          Inc(p);
          Inc(i);
          sol := False;
          stream.ChunkStart := stream.ChunkStart + 1;
          continue
        end;

        quoteLevel := 0;        // Calculate the quote level
        p1 := p;
        while(p1^ = '>') or (p1^ = '|') do
        begin
          Inc(quoteLevel);
          Inc(p1);
          if p1^ = ' ' then Inc(p1)
        end;

        if quoteLevel > 0 then
        begin
                                // If the 'Chunk' is not empty, exit, so that
                                // it can be displayed
          if i <> ChunkStart then
            break;
                                // The chunk was empty, so select the new font
                                // an use it for the chunk
          case QuoteLevel of
            1:
              Owner.FontToCharFormat(own.FLevel1QuoteFont, fc);
            2:
              Owner.FontToCharFormat(own.FLevel2QuoteFont, fc);
            else
              Owner.FontToCharFormat(own.FLevel3QuoteFont, fc)
          end;

          FIsFormatted := True
        end;


        if p^ = '-' then        // Detect signature indicator.  Allow a line
        begin                   // containing '-- ' or '--'
          p1 := p;
          Inc(p1);
          if p1^ = '-' then
          begin
            Inc(p1);
            if (p1^ = ' ') or not FStrictSigSeparator then
            begin
              if p1^ = ' ' then Inc(p1);
              if p1^ = #13 then
              begin
                if i <> ChunkStart then
                  break;
                FInSignature := True;
                Owner.FontToCharFormat(own.SignatureFont, fc);
              end
            end
          end
        end
      end;

      if sol and trc and FInSignature then
        if WideSameText (Copy(p, 1, Length (own.TruncateFrom)), own.TruncateFrom) then
        begin
          stream.Truncate := True;
          break;
        end;


      if c = #13 then           // End of line.
      begin
        FEOL := True;
        sol := True;
        Inc(p);
        Inc(i);
        if p^ = #10 then
        begin
          Inc(i);
          Inc(p)
        end;                    // If it's formatted then output this line
        if FIsFormatted then
          break
        else
          continue;
      end;

      sol := False;
      FEOL := False;

                                // Is it an indicator char, with the previous char = whitespace ???
      if ((pc = ' ') or (pc = #13) or (pc = #10)) and ((c = '*') or (c = '/') or (c = '_')) then

      begin
                                // This *may* be the start of highlighted text!
        p1 := p;
        hl := [];
        n := 0;
        c := p1^;
                                // Group all potential indicator chars together.
        while(c = '*') or (c = '/') or (c = '_') do
        begin
          Inc(n);
          if c = '*' then
            hl := hl + [fsBold]
          else
            if c = '/' then
              hl := hl + [fsItalic]
            else
              if c = '_' then
                hl := hl + [fsUnderline];
          Inc(p1);
          c := p1^
        end;

                                // Skip word after indicator chars
        n1 := 0;
        while IsWideCharAlNum (p1^) or (p1^ = '''') do
        begin
          Inc(n1);
          Inc(p1)
        end;

                                // Did we have whitespace, indicator chars, alpha chars, indicator chars ???
        if (p1^ = '*') or (p1^ = '/') or (p1^ = '_') then
        begin
          repeat
            Inc(p1)
          until not ((p1^ = '*') or (p1^ = '/') or (p1^ = '_'));

                                // It looks like a highlighted word - but be careful we're not
                                // in the middle of a URL or filename!
          if not IsWideCharAlNum (p1^) and not (Char (p1^) in ['@', '#', '£', '''', '"', '&']) then
          begin
            if i <> ChunkStart then
              break
            else
            begin               // We're in a highlighted word.  Skip the indicator chars
              ChunkStart := ChunkStart + n;
              ChunkEnd := ChunkStart + n1;

              if fsBold in hl then
              begin
                fc.dwMask := fc.dwMask or CFM_BOLD;
                fc.dwEffects := fc.dwEffects or CFE_BOLD
              end;

              if fsItalic in hl then
              begin
                fc.dwMask := fc.dwMask or CFM_ITALIC;
                fc.dwEffects := fc.dwEffects or CFE_ITALIC
              end;

              if fsUnderline in hl then
              begin
                fc.dwMask := fc.dwMask or CFM_UNDERLINE;
                fc.dwEffects := fc.dwEffects or CFE_UNDERLINE
              end;

              FInHighlight := True;
              FIsFormatted := True;
              exit
            end
          end
        end
      end;

      pc := c;
      Inc(p);
      Inc(i)
    end; // End while

    ChunkEnd := i
  end  // end with
end;

procedure TNewsCharFormatter.Reset;
begin
  FEOL := True;
  FInSignature := False;
  FInHighlight := False;
  FIsFormatted := False;
end;

end.
