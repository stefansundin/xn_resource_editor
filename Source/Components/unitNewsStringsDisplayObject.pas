(*======================================================================*
 | unitNewsStringsDisplayObject unit for NewsReader3                    |
 |                                                                      |
 | Work with TMessageDisplay to decode and display the text part of     |
 | nntp-style messages                                                  |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      19/07/2002  CPWW  Original                                  |
 *======================================================================*)

unit unitNewsStringsDisplayObject;

interface

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, Forms,
  cmpMessageDisplay, cmpNewsRichEdit, Dialogs, SyncObjs, ComCtrls, StrUtils;

type
  TNewsRichEditX = class (TNewsRichEdit)
  private
    FAutoSize: Boolean;
    FTimerScrollDelta: Integer;
    FRightMargin: Integer;
    FObjectLink: TDisplayObjectLink;
    FScrollingParent: TScrollingWinControl;
    procedure SetRightMargin(const Value: Integer);
    procedure ScrollIntoView;
  protected
    procedure SetAutoSize(Value: Boolean); override;
    procedure RequestSize(const Rect: TRect); override;
    procedure CreateParams (var params: TCreateParams); override;

    function FindScrollingParent: TScrollingWinControl;

    function LinesOnPage: Integer;
    function MoveCursor (lines: Integer; moveUp: Boolean; shiftState: TShiftState): Boolean;

    procedure WMLButtonDown (var msg: TwmLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown (var msg: TwmRButtonDown); message WM_RBUTTONDOWN;
    procedure WMKeyDown (var msg: TwmKeyDown); message WM_KEYDOWN;
    procedure WMMouseMove(var msg: TwmMouseMove); message WM_MOUSEMOVE;
    procedure WMTimer (var msg: TwmTimer); message WM_TIMER;
    procedure WMMouseWheel (var msg: TwmMouseWheel); message WM_MOUSEWHEEL;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 76;
  end;

  TNewsStringsDisplayObjectLink = class (TWinControlObjectLink)
  private
    FTextObjects: TList;
    FUpdating: Boolean;
    FLevel1QuotesFontColor: TColor;
    FFooterFontColor: TColor;
    FHeaderFontColor: TColor;
    FLevel2QuotesFontColor: TColor;
    FLevel3QuotesFontColor: TColor;
    FLastNoChunks: Integer;
    FLastChunkLen: Integer;
    FOwner: TMessageDisplay;
    function GetRichEdit: TNewsRichEditX;
    procedure LoadFromTextObjects;
    function GetTextObjectCount: Integer;
    function GetRightMargin: Integer;
    procedure SetRightMargin(const Value: Integer);
    procedure SetTruncateFrom(const Value: WideString);
    function GetTruncateFrom: WideString;
    procedure DoOnURLMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetStrictSigSeparator: Boolean;
    procedure SetStrictSigSeparator(const Value: Boolean);

  protected
    class function DisplaysObject(obj: TObject): Boolean; override;
    procedure GetSelectedText(var txt: WideString); override;
    procedure SetSelectedText(const txt: WideString); override;
    function GetSelLength: Integer; override;
    procedure GetText(var txt: WideString); override;
    procedure GetHTML (var txt: string; rawFragment: Boolean = false); override;
    procedure Refresh; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetHasText: Boolean; override;
    function FindText(const SearchStr: string; NewSearch: Boolean; Options: TStringSearchOptions; y: Integer): Boolean; override;
  public
    constructor Create(AOwner: TMessageDisplay; AObj: TObject; codepage: Integer); override;
    destructor Destroy; override;
    procedure SetTextObject(objNo: Integer; obj: TObject);
    procedure AddTextObject(obj: TObject);
    procedure Print; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    property HeaderFontColor: TColor read FHeaderFontColor write FHeaderFontColor;
    property FooterFontColor: TColor read FFooterFontColor write FFooterFontColor;
    property Level1QuotesFontColor: TColor read FLevel1QuotesFontColor write FLevel1QuotesFontColor;
    property Level2QuotesFontColor: TColor read FLevel2QuotesFontColor write FLevel2QuotesFontColor;
    property Level3QuotesFontColor: TColor read FLevel3QuotesFontColor write FLevel3QuotesFontColor;

    property TextObjectCount: Integer read GetTextObjectCount;
    property RichEdit: TNewsRichEditX read GetRichEdit;

    property RightMargin: Integer read GetRightMargin write SetRightMargin;
    property TruncateFrom: WideString read GetTruncateFrom write SetTruncateFrom;
    property StrictSigSeparator: Boolean read GetStrictSigSeparator write SetStrictSigSeparator;
  end;

implementation

uses
  RichEdit, Types, unitCharsetMap, unitRTF2HTML;

{ TNewsStringsDisplayObjectLink }

procedure TNewsStringsDisplayObjectLink.AddTextObject(obj: TObject);
begin
  FTextObjects.Add(obj);
  LoadFromTextObjects
end;

procedure TNewsStringsDisplayObjectLink.BeginUpdate;
begin
  FUpdating := True
end;

constructor TNewsStringsDisplayObjectLink.Create(AOwner: TMessageDisplay;
  AObj: TObject;  codepage: Integer);
var
  ctrl: TNewsRichEditX;
  w, w1: Integer;
  tm: TTextMetric;
begin
  FOwner := AOwner;
  FTextObjects := TList.Create;
  FTextObjects.Add(AObj);
  ctrl := TNewsRichEditX.Create(AOwner.Owner);
  ctrl.Parent := AOwner;
  ctrl.CodePage := codepage;

  if AOwner.Parent is TScrollingWinControl then
    ctrl.FScrollingParent := TScrollingWinControl (AOwner.Parent);

  ctrl.FObjectLink := self;
  inherited Create(AOwner, ctrl, codepage);
  BeginUpdate;
  ctrl.BorderStyle := bsNone;

  if Assigned(ctrl.FScrollingParent) then
    w := ctrl.FScrollingParent.Width - Margin * 2 - GetSystemMetrics (SM_CXVSCROLL)
  else
    w := AOwner.Width;

  GetTextMetrics (AOwner.Canvas.Handle, tm);
  w1 := tm.tmAveCharWidth * ctrl.RightMargin;
  if w1 > w then w := w1 else ctrl.FRightMargin := 0;

  ctrl.AutoSize := True;
  ctrl.Width := w;
  ctrl.ReadOnly := True;
  ctrl.WordWrap := True;
  ctrl.ParentColor := True;
  ctrl.AutoURLDetect := True;
  ctrl.AutoURLExecute := True;
  ctrl.HideSelection := False;
  ctrl.Font.Assign(Font);
  ctrl.OnURLMouseDown := DoOnURLMouseDown;
  LoadFromTextObjects
end;

destructor TNewsStringsDisplayObjectLink.Destroy;
begin
  Obj.Free;
  FTextObjects.Free;

  inherited;
end;

class function TNewsStringsDisplayObjectLink.DisplaysObject(
  obj: TObject): Boolean;
begin
  Result := obj is TStrings;
end;

procedure TNewsStringsDisplayObjectLink.DoOnURLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FOwner.OnURLClick(Sender, Button, Shift, RichEdit.URLText);
end;

procedure TNewsStringsDisplayObjectLink.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating := False;
    LoadFromTextObjects
  end
end;

function TNewsStringsDisplayObjectLink.FindText(const SearchStr: string;
  NewSearch: Boolean; Options: TStringSearchOptions; y: Integer): Boolean;
var
  ops: TSearchTypes;
  StartPos: Integer;
begin
  RichEdit.SetFocus;
  ops := [];

  if soMatchCase in Options then
    ops := ops + [stMatchCase];

  if soWholeWord in Options then
    ops := ops + [stWholeWord];

  if newSearch then
    StartPos := 0
  else
    StartPos := RichEdit.SelStart + RichEdit.SelLength;

  StartPos := RichEdit.FindText(SearchStr, StartPos, MaxInt, ops);

  if StartPos >= 0 then
  begin
    RichEdit.SelStart := StartPos;
    RichEdit.SelLength := Length(SearchStr);
    Result := True;
  end
  else
    Result := False
end;

function TNewsStringsDisplayObjectLink.GetHasText: Boolean;
begin
  Result := True;
end;

function TNewsStringsDisplayObjectLink.GetHeight: Integer;
begin
  try
    Result := inherited GetHeight;
  except
    Result := 0
  end
end;

procedure TNewsStringsDisplayObjectLink.GetHTML(var txt: string; rawFragment :Boolean = false);
var
  re: TNewsRichEditX;
  st: string;
begin
  re := GetRichEdit;
  re.StreamRTF := True;
  try
    st := GetRichEdit.Text;
    txt := RTF2HTML (st, rawFragment);
  finally
    re.StreamRTF := False
  end
end;

function TNewsStringsDisplayObjectLink.GetRichEdit: TNewsRichEditX;
begin
  Result := TNewsRichEditX (Obj);
end;

function TNewsStringsDisplayObjectLink.GetRightMargin: Integer;
begin
  Result := RichEdit.RightMargin
end;

procedure TNewsStringsDisplayObjectLink.GetSelectedText(var txt: WideString);
begin
  Txt := GetRichEdit.SelText
end;

function TNewsStringsDisplayObjectLink.GetSelLength: Integer;
begin
  Result := GetRichEdit.SelLength
end;

function TNewsStringsDisplayObjectLink.GetStrictSigSeparator: Boolean;
begin
  Result := GetRichEdit.StrictSigSeparator
end;

procedure TNewsStringsDisplayObjectLink.GetText(var txt: WideString);
begin
  txt := GetRichEdit.Text
end;

function TNewsStringsDisplayObjectLink.GetTextObjectCount: Integer;
begin
  Result := FTextObjects.Count
end;

function TNewsStringsDisplayObjectLink.GetTruncateFrom: WideString;
begin
  Result := GetRichEdit.TruncateFrom;
end;

function TNewsStringsDisplayObjectLink.GetWidth: Integer;
begin
  try
    Result := inherited GetWidth
  except
    Result := 0
  end
end;

(*
function StringReplaceEx(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags; Offset: Integer = 0): string;
var
  SearchStr, NewStr: string;
begin
  SearchStr := S;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(OldPattern), MaxInt);
    if SearchStr <> '' then
      Offset := Pos(OldPattern, SearchStr);
  end;
end;
*)
(*
procedure TNewsStringsDisplayObjectLink.LoadFromTextObjects;
var
  i, j, offset, count, p, quoteLevel: Integer;
  s: TStrings;
  line, st: string;
  ctrl: TNewsRichEditX;
  rtf: Boolean;
  inHeader: Boolean;
  inQuote: Boolean;
  rtfCount: Integer;

  procedure AddColorTableEntry(var st: string; color: TColor);
  var
    rgb : DWORD;
  begin
    rgb := ColorTORGB (color);
    st := st + '\red' + IntToStr (GetRValue(rgb));
    st := st + '\green' + IntToStr (GetGValue(rgb));
    st := st + '\blue' + IntToStr (GetBValue(rgb));
    st := st + ';'
  end;

  function FormatWordsInLine(const st: string): string;
  var
    i, len, p, p1, l: Integer;
    ch, ch1, ch2: char;
    styles, styles1: TFontStyles;
  begin
    i := 1;
    styles := [];
    Result := '';

    len := Length(st);
    while i <= len do
    begin
      ch := st [i];
      if i > 1 then
        ch1 := st [i - 1]
      else
        ch1 := #0;
      if i < len then
        ch2 := st [i + 1]
      else
        ch2 := #0;

      if (ch1 in [#0, ' ', #9, ',', ';', '.']) and (ch in ['*', '\', '_']) and (ch <> ch2) then
      begin
        p := i;
        ch1 := ch;
        repeat
          case ch1 of
            '*': styles := styles + [fsBold];
            '_': styles := styles + [fsUnderline];
            '\': styles := styles + [fsItalic];
          end;

          Inc(p);
          ch1 := st [p]
        until not (ch1 in ['*', '\', '_']);

        p1 := p;
        while st [p] in ['A'..'Z', '0'..'9', 'a'..'z'] do
          Inc(p);
        l := p - p1;

        styles1 := styles;
        while st [p] in ['*', '\', '_'] do
        begin
          case st [p] of
            '*': styles := styles - [fsBold];
            '_': styles := styles - [fsUnderline];
            '\': styles := styles - [fsItalic]
          end;
          Inc(p)
        end;

        if styles = [] then
        begin
          Result := Result + #2;
          if fsBold in styles1 then
            Result := Result + #3'b';
          if fsUnderline in styles1 then
            Result := Result + #3'ul';
          if fsItalic in styles1 then
            Result := Result + #3'i';

          Result := Result + ' ' + Copy(st, p1, l) + #4 + FormatWordsInLine(Copy(st, p, MaxInt));
          exit
        end
      end;
      Result := Result + ch;
      styles := [];
      Inc(i)
    end
  end;

begin
  if FUpdating then Exit;
  count := 0;
  ctrl := GetRichEdit;
  rtf := not Owner.RawMode;

  inHeader := False;
  inQuote := False;
  rtfCount := 0;
  if rtf then
  begin
    st := '{\rtf1 {{\colortbl;';
    AddColorTableEntry(st, HeaderFontColor);
    AddColorTableEntry(st, FooterFontColor);
    AddColorTableEntry(st, Level1QuotesFontColor);
    AddColorTableEntry(st, Level2QuotesFontColor);
    AddColorTableEntry(st, Level3QuotesFontColor);
    st := st + '}';
    Inc(rtfCount, 2)
  end
  else
    st := '';

  for j := 0 to FTextObjects.Count - 1 do
  begin
    s := TStrings (FTextObjects[j]);
    if not rtf then
    begin
      st := st + s.Text;
      Inc(count);
      continue
    end;

    for i := 0 to s.Count - 1 do
    begin
      Inc(count);
      line := s[i];

      if rtf then
      begin
        line := FormatWordsInLine(line);

        offset := Pos ('\', line); if offset > 0 then line := StringReplaceEx (line, '\', '\\', [rfReplaceAll], offset);
        offset := Pos ('{', line); if offset > 0 then line := StringReplaceEx (line, '{', '\{', [rfReplaceAll], offset);
        offset := Pos ('}', line); if offset > 0 then line := StringReplaceEx (line, '}', '\}', [rfReplaceAll], offset);
        offset := Pos ( #2, line); if offset > 0 then line := StringReplaceEx (line,  #2, '{',  [rfReplaceAll], offset);
        offset := Pos ( #3, line); if offset > 0 then line := StringReplaceEx (line,  #3, '\',  [rfReplaceAll], offset);
        offset := Pos ( #4, line); if offset > 0 then line := StringReplaceEx (line,  #4, '}',  [rfReplaceAll], offset);
      end;

      if (Length(line) > 0) and (line [1] = #1) then
      begin
        if not inHeader and rtf then
        begin
          inHeader := True;
          st := st + '{\cf1 ';
          Inc(rtfCount)
        end;
        Delete(line, 1, 1)
      end
      else
      begin
        if inHeader and rtf then
        begin
          st := st + '}';
          Dec(rtfCount);
        end;
        inHeader := False;
      end;

      if rtf then
      begin
        if (line = '>') or (Copy(line, 1, 2) = '> ') then
        begin
          p := 3;
          quoteLevel := 1;
          while line [p] in [' ', '>'] do
          begin
            if line [p] = '>' then
              Inc(quoteLevel);
            Inc(p);

            if quoteLevel = 3 then
              break
          end;
          st := st + '{\cf' + IntToStr (2 + QuoteLevel) + ' ';
          Inc(rtfCount);
          inQuote := True
        end;

        if (line = '-- ') or (line = '--') then
        begin
          st := st + '{\cf2 ';
          Inc(rtfCount)
        end;

        st := st + line;
        if inQuote then
        begin
          st := st + '}';
          Dec(rtfCount);
          inQuote := False
        end;
        st := st + '\par ';
      end
      else
        st := st + line + #13#10
    end
  end;

  for i := 0 to rtfCount - 1 do
    st := st + '}';

  Ctrl.Text := st;

  if count = 0 then
    Height := 0;
end;
*)

(*
var
  urlIndicators: array [0..1] of string =('://', 'www.');

procedure UnwrapURLS (var st: string);
var
  i, len: Integer;

  procedure Unwrap (const indicator: string);

  var
    i, p, l: Integer;
    ch: char;
  begin
    l := Length(indicator);
    i := 1;
    while i + l - 1 <= len do
    begin
      p := PosEx (indicator, st, i);
      if p = 0 then
        break;

      i := p + l;

      while i <= len do
      begin
        ch := st [i];
        case ch of
          #$0d: if (i + 2 < len) and (st [i + 1] = #$0a) and not (st [i + 2] in [' ', #9, ',']) then
                 begin
                   Delete(st, i, 2);
                   Dec(len, 2)
                 end
                 else
                   Inc(i, 3);
          ' ', #9, ',', #0: break;
          else
            Inc(i)
        end
      end
    end
  end;

begin
 len := Length(st);
  for i := Low (urlIndicators) to High(urlIndicators) do
    Unwrap (urlIndicators[i])
end;
*)

procedure TNewsStringsDisplayObjectLink.LoadFromTextObjects;
var
  ctrl: TNewsRichEditX;
  i: Integer;
  st: string;
  ws, ws1: WideString;
  cp, n, lastLen: Integer;

begin
  if FUpdating then Exit;
  ctrl := GetRichEdit;
  ctrl.RawText := Owner.RawMode;

  ws1 := '';
  cp := ctrl.CodePage;
//  if cp = 1252 then
//    cp := CP_ACP;
  n := 0;
  lastLen := 0;
  for i := 0 to FTextObjects.Count - 1 do
  begin
    Inc(n);
    st := StringReplace(TStrings (FTextObjects[i]).Text, 'url:', 'url: ', [rfReplaceAll, rfIgnoreCase]);
//    UnwrapURLS (st);
    ws := StringToWideString (st, cp);
    lastLen := Length(ws);
    ws1 := ws1 + ws;
  end;

  if (n <> FLastNoChunks) or (lastLen <> FLastChunkLen) then
  begin // It's important to *only* set ctrl.Text when we have to - for
        // performances and flicker reasons.

    FLastNoChunks := n;
    FLastChunkLen := lastLen;
    ctrl.Text := ws1
  end
end;

procedure TNewsStringsDisplayObjectLink.Print;
begin
  with GetRichEdit do
  begin
    PageRect := self.PageRect;
    Print
  end
end;

procedure TNewsStringsDisplayObjectLink.Refresh;
begin
  LoadFromTextObjects;
end;

procedure TNewsStringsDisplayObjectLink.SetRightMargin(
  const Value: Integer);
begin
  RichEdit.RightMargin := Value
end;

procedure TNewsStringsDisplayObjectLink.SetSelectedText(const txt: WideString);
begin
  GetRichEdit.SelText := txt
end;

procedure TNewsStringsDisplayObjectLink.SetStrictSigSeparator(
  const Value: Boolean);
begin
  GetRichEdit.StrictSigSeparator := Value
end;

procedure TNewsStringsDisplayObjectLink.SetTextObject(objNo: Integer;
  obj: TObject);
begin
  FTextObjects[objNo] := obj;
  LoadFromTextObjects
end;

procedure TNewsStringsDisplayObjectLink.SetTruncateFrom(
  const Value: WideString);
begin
  GetRichEdit.TruncateFrom := Value;
end;

{ TNewsRichEditX }

constructor TNewsRichEditX.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize := True;
  FRightMargin := 76;
end;

procedure TNewsRichEditX.CreateParams(var params: TCreateParams);
begin
  inherited;

  params.WindowClass.style := params.WindowClass.style or CS_HREDRAW;
end;

destructor TNewsRichEditX.Destroy;
begin
  inherited;
end;

function TNewsRichEditX.FindScrollingParent: TScrollingWinControl;
var
  p: TWinControl;
begin
  p := Parent;
  while Assigned(p) and not (p is TScrollingWinControl) do
    p := p.Parent;

  Result := TScrollingWinControl (p)
end;


function TNewsRichEditX.LinesOnPage: Integer;
var
  sp: TScrollingWinControl;
  lineHeight: Integer;
begin
  Result := 0;
  sp := FindScrollingParent;
  if not Assigned(sp) then exit;
  lineHeight := Abs (font.Height);

  Result := (sp.ClientHeight div lineHeight) ;
  if sp.ClientHeight mod lineHeight > 0 then
    Inc(Result)
end;

function TNewsRichEditX.MoveCursor(lines: Integer; moveUp: Boolean; shiftState: TShiftState): Boolean;
var
  line, pos, spos, l: Integer;
  sp: TScrollingWinControl;
begin
  sp := FindScrollingParent;

  if ssShift in shiftState then
    spos := SelStart + SelLength
  else
    spos := SelStart;

  line := SendMessage(handle, EM_LINEFROMCHAR, spos, 0);

  spos := spos - SendMessage(handle, EM_LINEINDEX, line, 0);

  if moveUp then
    if lines > line then lines := -line else lines := -lines
  else
    if line + lines >= lineCount then
      lines := lineCount - line - 1;

  if Assigned(sp) then
    sp.VertScrollBar.Position := sp.VertScrollBar.Position + lines * Abs (Font.Height);

  pos := SendMessage(handle, EM_LINEINDEX, line + lines, 0);
  l := SendMessage(handle, EM_LINELENGTH, line + lines, 0);

  if spos > l then
    spos := l;
  Inc(pos, spos);

  Result := True;
  if ssShift in shiftState then
    SelLength := pos - SelStart
  else
  begin
    SelStart := pos;
    Result := pos <> SelStart
  end
end;

procedure TNewsRichEditX.RequestSize(const Rect: TRect);
begin
  inherited;

  if FAutoSize then
    BoundsRect := rect;
end;

procedure TNewsRichEditX.ScrollIntoView;
var
  sp: TScrollingWinControl;
  pt: TPoint;
  deltaY, lineHeight: Integer;
begin
  sp := FindScrollingParent;
  if not Assigned(sp) then exit;

  SendMessage(handle, EM_POSFROMCHAR, Integer (@pt), self.SelStart + self.SelLength);

  lineHeight := Abs (font.Height);
  if Top + pt.y + lineHeight >= sp.VertScrollBar.Position + sp.ClientHeight then
  begin

    DeltaY := (Top + pt.y + lineHeight) - (sp.VertScrollBar.Position + sp.ClientHeight);

    DeltaY := (DeltaY + lineHeight - 1) div lineHeight * lineHeight;
    sp.VertScrollBar.Position := sp.VertScrollBar.Position + DeltaY
  end
  else
    if Top + pt.Y < sp.VertScrollBar.Position then
    begin
      DeltaY := (Top + pt.Y) - sp.VertScrollBar.Position;
      DeltaY := (DeltaY - lineHeight - 1) div lineHeight * lineHeight;
      if sp.VertScrollBar.Position + DeltaY < 0 then
        DeltaY := -sp.VertScrollBar.Position;
      sp.VertScrollBar.Position := sp.VertScrollBar.Position + DeltaY
    end
end;

procedure TNewsRichEditX.SetAutoSize(Value: Boolean);
begin
//  inherited;

  if value <> AutoSize then
  begin
    FAutoSize := Value;
    if Value then
      SendMessage(Handle, EM_REQUESTRESIZE, 0, 0)
  end
end;

procedure TNewsRichEditX.SetRightMargin(const Value: Integer);
var
  tm: TTextMetric;
  canvas: TControlCanvas;
begin
  if value <> FRightMargin then
  begin
    FRightMargin := Value;

    if not (csDesigning in ComponentState) then
      if value = 0 then
        if Assigned(FScrollingParent) then
          width := FScrollingParent.Width - FObjectLink.Margin * 2 - GetSystemMetrics (SM_CXVSCROLL)
        else
          width := Parent.Width
      else
      begin
        canvas := TControlCanvas.Create;
        try
          canvas.Control := self;
          canvas.Font.Assign(Font);
          GetTextMetrics (canvas.Handle, tm);
          width := tm.tmAveCharWidth * FRightMargin
        finally
          canvas.Free
        end
      end
  end
end;


procedure TNewsRichEditX.WMKeyDown(var msg: TwmKeyDown);
var
  ShiftState: TShiftState;
begin
  ShiftState := KeyDataToShiftState(msg.KeyData);
  if (msg.CharCode = VK_NEXT) or ((msg.CharCode = VK_SPACE) and not (ssCtrl in ShiftState)) or (msg.CharCode = VK_PRIOR) then
  begin
    if (not MoveCursor (LinesOnPage - 1, msg.CharCode = VK_PRIOR, shiftState)) and (msg.CharCode = VK_SPACE) then
      inherited
  end
  else
    inherited;

  if (msg.CharCode <> VK_SHIFT) and (msg.CharCode <> VK_CONTROL) then
    ScrollIntoView;
end;

procedure TNewsRichEditX.WMLButtonDown(var msg: TwmLButtonDown);
var
  sp: TScrollingWinControl;
  xPos, yPos: Integer;
begin
  if Assigned(FObjectLink) then
    FObjectLink.Owner.FocusObject(FObjectLink);
  if not Focused then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      xPos := sp.HorzScrollBar.Position;
      yPos := sp.VertScrollBar.Position;
      inherited;
      sp.HorzScrollBar.Position := xPos;
      sp.VertScrollBar.Position := yPos
    end
    else
      inherited
  end
  else
    inherited
end;

procedure TNewsRichEditX.WMMouseMove(var msg: TwmMouseMove);
var
  p: TPoint;
  sp: TScrollingWinControl;
begin
  FTimerScrollDelta := 0;
  if (msg.Keys and MK_LBUTTON) <> 0 then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      p := Point(msg.xPos, msg.yPos);
      MapWindowPoints (Handle, sp.Handle, p, 1);
      if p.Y < sp.ClientRect.Top then
        FTimerScrollDelta := p.Y - sp.ClientRect.Top
      else
        if p.Y > sp.ClientRect.Bottom then
          FTimerScrollDelta := p.Y - sp.ClientRect.Bottom
    end
  end;
  inherited
end;

procedure TNewsRichEditX.WMMouseWheel(var msg: TwmMouseWheel);
begin
  inherited
end;

procedure TNewsRichEditX.WMRButtonDown(var msg: TwmRButtonDown);
var
  sp: TScrollingWinControl;
  xPos, yPos: Integer;
begin
  if Assigned(FObjectLink) then
    FObjectLink.Owner.FocusObject(FObjectLink);
  if not Focused then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      xPos := sp.HorzScrollBar.Position;
      yPos := sp.VertScrollBar.Position;
      SetFocus;
      inherited;
      sp.HorzScrollBar.Position := xPos;
      sp.VertScrollBar.Position := yPos
    end
    else
      inherited
  end
  else
    inherited
end;

procedure TNewsRichEditX.WMTimer(var msg: TwmTimer);
var
  delta, ad: Integer;
  sp: TScrollingWinControl;
begin
  if FTimerScrollDelta <> 0 then
  begin
    sp := FindScrollingParent;
    if Assigned(sp) then
    begin
      delta := self.Font.Height;
      ad := Abs (FTimerScrollDelta);
      if ad < delta then
        delta := delta div 2
      else
        if ad > 4 * delta then
          delta := delta * 2;

      if FTimerScrollDelta < 0 then
        delta := -delta;

      sp.VertScrollBar.Position := sp.VertScrollBar.Position - delta
    end
  end;
  inherited
end;

initialization
  RegisterDisplayObjectLink(TNewsStringsDisplayObjectLink, 0)
end.
