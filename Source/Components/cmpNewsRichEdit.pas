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
    fIsFormatted : boolean;
    fEOL : boolean;
    fInSignature : boolean;
    fInHighlight : boolean;
    fStrictSigSeparator : boolean;
  public
    constructor Create;
    procedure Reset; override;
    procedure GetFormattedChars (stream : TRichEditStream; var fc : TCharFormat); override;
  end;

//----------------------------------------------------------------------
// TNewsRichEdit class
  TNewsRichEdit = class(TExRichEdit)
  private
    fLevel3QuoteFont: TFont;
    fLevel1QuoteFont: TFont;
    fLevel2QuoteFont: TFont;
    fFormatter : TCharFormatter;
    fHeaderFont: TFont;
    fSignatureFont: TFont;
    fRawText: boolean;
    fTruncateFrom: WideString;
    fStrictSigSeparator: boolean;
    procedure SetLevel1QuoteFont(const Value: TFont);
    procedure SetLevel2QuoteFont(const Value: TFont);
    procedure SetLevel3QuoteFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetSignatureFont(const Value: TFont);
    procedure SetRawText(const Value: boolean);
   { Private declarations }
  protected
    function GetCharFormatter : TCharFormatter; override;
    { Protected declarations }
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    property TruncateFrom : WideString read fTruncateFrom write fTruncateFrom;
    property StrictSigSeparator : boolean read fStrictSigSeparator write fStrictSigSeparator;
    { Public declarations }
  published
    property HeaderFont : TFont read fHeaderFont write SetHeaderFont;
    property Level1QuoteFont : TFont read fLevel1QuoteFont write SetLevel1QuoteFont;
    property Level2QuoteFont : TFont read fLevel2QuoteFont write SetLevel2QuoteFont;
    property Level3QuoteFont : TFont read fLevel3QuoteFont write SetLevel3QuoteFont;
    property SignatureFont : TFont read fSignatureFont write SetSignatureFont;
    property RawText : boolean read fRawText write SetRawText;
    { Published declarations }
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

  fLevel1QuoteFont := TFont.Create;
  fLevel2QuoteFont := TFont.Create;
  fLevel3QuoteFont := TFont.Create;
  fHeaderFont := TFont.Create;
  fSignatureFont := TFont.Create;

  fHeaderFont.Color := clGray;          // Set default colours
  fLevel1QuoteFont.Color := clTeal;
  fLevel2QuoteFont.Color := clOlive;
  fLevel3QuoteFont.Color := clMaroon;
  fSignatureFont.Color := clGreen;
end;

(*----------------------------------------------------------------------*
 | destructor TNewsRichEdit.Destroy                                     |
 |                                                                      |
 | Destructor for TNewsRichEdit                                         |
 *----------------------------------------------------------------------*)
destructor TNewsRichEdit.Destroy;
begin
  fHeaderFont.Free;
  fLevel1QuoteFont.Free;
  fLevel2QuoteFont.Free;
  fLevel3QuoteFont.Free;
  fSignatureFont.Free;

  FreeAndNil (fFormatter);

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
 if not Assigned (fFormatter) then
   if RawText then
     fFormatter := TCharFormatter.Create
   else
   begin
     fFormatter := TNewsCharFormatter.Create;
     TNewsCharFormatter (fFormatter).fStrictSigSeparator := StrictSigSeparator
   end;
 result := fFormatter;
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetHeaderFont                                |
 |                                                                      |
 | Set method for the HeaderFont property                               |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetHeaderFont(const Value: TFont);
begin
  fHeaderFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetLevel1QuoteFont                           |
 |                                                                      |
 | Set method for the Level1QuoteFont property                          |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetLevel1QuoteFont(const Value: TFont);
begin
  fLevel1QuoteFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetLevel2QuoteFont                           |
 |                                                                      |
 | Set method for the Level2QuoteFont property                          |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetLevel2QuoteFont(const Value: TFont);
begin
  fLevel2QuoteFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetLevel3QuoteFont                           |
 |                                                                      |
 | Set method for the Level3QuoteFont property                          |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetLevel3QuoteFont(const Value: TFont);
begin
  fLevel3QuoteFont.Assign (Value);
end;

(*----------------------------------------------------------------------*
 | procedure TNewsRichEdit.SetRawText                                   |
 |                                                                      |
 | Set method for the RawText property                                  |
 *----------------------------------------------------------------------*)
procedure TNewsRichEdit.SetRawText(const Value: boolean);
begin
  if fRawText <> Value then
  begin
    fRawText := Value;
    FreeAndNil (fFormatter)     // It will be recreated when it's next required
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
  fSignatureFont.Assign (Value)
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
procedure TNewsCharFormatter.GetFormattedChars(stream: TRichEditStream; var fc : TCharFormat);
var
  c, pc : WideChar;
  p, p1 : PWideChar;
  i, n, n1 : DWORD;
  sol, trc : boolean;
  own : TNewsRichEdit;
  quoteLevel : Integer;
  hl : TFontStyles;

begin
  own := TNewsRichEdit (stream.Owner);
  trc := own.TruncateFrom <> '';
  with stream do
  begin
    i := ChunkStart;
    if ChunkStart >= Len then Exit;
    p := Buffer;
    Inc (p, ChunkStart);
    sol := False;

    if fEOL then        // When we finished last time, we were at the end of
    begin               // a line.  So cancel all formatting
      fc := Owner.DefaultCharFormat;
      fIsFormatted := False;
      sol := True;
    end;

    if fInHighlight then        // When we finished last time we were at the end
    begin                       // of a highlighted block - before the terminating
      fInHighlight := False;    // indicators.  Clear the highlight flags and skip
                                // the terminating indicators
      while (p^ = '*') or (p^ = '/') or (p^ = '_') do
      begin
        Inc (p);
        Inc (i);
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
      if sol and not fInSignature then
      begin

        if c = #1 then          // Is it a header line?
        begin
          Owner.FontToCharFormat(own.fHeaderFont, fc);
          fIsFormatted := True;
          Inc (p);
          Inc (i);
          sol := False;
          stream.ChunkStart := stream.ChunkStart + 1;
          continue
        end;

        quoteLevel := 0;        // Calculate the quote level
        p1 := p;
        while (p1^ = '>') or (p1^ = '|') do
        begin
          Inc (quoteLevel);
          Inc (p1);
          if p1^ = ' ' then Inc (p1)
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
            1 : Owner.FontToCharFormat(own.fLevel1QuoteFont, fc);
            2 : Owner.FontToCharFormat(own.fLevel2QuoteFont, fc);
            else Owner.FontToCharFormat(own.fLevel3QuoteFont, fc)
          end;

          fIsFormatted := True
        end;


        if p^ = '-' then        // Detect signature indicator.  Allow a line
        begin                   // containing '-- ' or '--'
          p1 := p;
          Inc (p1);
          if p1^ = '-' then
          begin
            Inc (p1);
            if (p1^ = ' ') or not fStrictSigSeparator then
            begin
              if p1^ = ' ' then Inc (p1);
              if p1^ = #13 then
              begin
                if i <> ChunkStart then
                  break;
                fInSignature := True;
                Owner.FontToCharFormat(own.SignatureFont, fc);
              end
            end
          end
        end
      end;

      if sol and trc and fInSignature then
        if WideSameText (Copy (p, 1, Length (own.TruncateFrom)), own.TruncateFrom) then
        begin
          stream.Truncate := True;
          break;
        end;


      if c = #13 then           // End of line.
      begin
        fEOL := True;
        sol := True;
        Inc (p);
        Inc (i);
        if p^ = #10 then
        begin
          Inc (i);
          Inc (p)
        end;                    // If it's formatted then output this line
        if fIsFormatted then
          break
        else
          continue;
      end;

      sol := False;
      fEOL := False;

                                // Is it an indicator char, with the previous char = whitespace ???
      if ((pc = ' ') or (pc = #13) or (pc = #10)) and ((c = '*') or (c = '/') or (c = '_')) then

      begin
                                // This *may* be the start of highlighted text!
        p1 := p;
        hl := [];
        n := 0;
        c := p1^;
                                // Group all potential indicator chars together.
        while (c = '*') or (c = '/') or (c = '_') do
        begin
          Inc (n);
          if c = '*' then
            hl := hl + [fsBold]
          else
            if c = '/' then
              hl := hl + [fsItalic]
            else
              if c = '_' then
                hl := hl + [fsUnderline];
          Inc (p1);
          c := p1^
        end;

                                // Skip word after indicator chars
        n1 := 0;
        while IsWideCharAlNum (p1^) or (p1^ = '''') do
        begin
          Inc (n1);
          Inc (p1)
        end;

                                // Did we have whitespace, indicator chars, alpha chars, indicator chars ???
        if (p1^ = '*') or (p1^ = '/') or (p1^ = '_') then
        begin
          repeat
            Inc (p1)
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

              fInHighlight := True;
              fIsFormatted := True;
              exit
            end
          end
        end
      end;

      pc := c;
      Inc (p);
      Inc (i)
    end; // End while

    ChunkEnd := i
  end  // end with
end;

procedure TNewsCharFormatter.Reset;
begin
  fEOL := True;
  fInSignature := False;
  fInHighlight := False;
  fIsFormatted := False;
end;

end.
