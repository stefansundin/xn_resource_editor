unit cmpHexDump;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

const
  MAXDIGITS = 16;

{ THexDump }

type

  THexStr = array[0..2] of Char;
  THexStrArray = array[0..MAXDIGITS-1] of THexStr;

  THexDump = class(TCustomControl)
  private
    FActive: Boolean;
    FAddress: Pointer;
    FDataSize: Integer;
    FTopLine: Integer;
    FCurrentLine: Integer;
    FVisibleLines: Integer;
    FLineCount: Integer;
    FBytesPerLine: Integer;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FFileColors: array[0..2] of TColor;
    FShowCharacters: Boolean;
    FShowAddress: Boolean;
    FBorder: TBorderStyle;
    FHexData: THexStrArray;
    FLineAddr: array[0..15] of Char;
    FReadOnly: Boolean;
    FCurrentLinePos: Integer;
    FAddressWidth : Integer;
    FEditCharacters : Boolean;
    FLowNibble : Boolean;
    FChanges: Boolean;
    FOnChanges: TNotifyEvent;
    FAddressOffset: Integer;

    procedure CalcPaintParams;
    procedure SetTopLine(Value: Integer);
    procedure SetCurrentLine(Value: Integer);
    procedure SetFileColor(Index: Integer; Value: TColor);
    function GetFileColor(Index: Integer): TColor;
    procedure SetShowCharacters(Value: Boolean);
    procedure SetShowAddress(Value: Boolean);
    procedure SetBorder(Value: TBorderStyle);
    procedure SetAddress(Value: Pointer);
    procedure SetDataSize(Value: Integer);
    procedure AdjustScrollBars;
    function LineAddr(Index: Integer): PChar;
    function LineData(Index: Integer): PChar;
    function LineChars(Index: Integer): PChar;
    function ScrollIntoView: Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMLostFocus); message CM_EXIT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus (var Message : TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus (var Message : TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar (var Message : TWMChar); message WM_CHAR;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetCurrentLinePos(const Value: Integer);
    procedure SetCaretPos;
    procedure SetEditCharacters(const Value: Boolean);
    procedure SetLowNibble(const Value: Boolean);
    procedure SetChanged;
    procedure SetAddressOffset(const Value: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentLine: Integer read FCurrentLine write SetCurrentLine;
    property CurrentLinePos : Integer read FCurrentLinePos write SetCurrentLinePos;
    property EditCharacters : Boolean read FEditCharacters write SetEditCharacters;
    property Address: Pointer read FAddress write SetAddress;
    property DataSize: Integer read FDataSize write SetDataSize;
    property AddressOffset : Integer read FAddressOffset write SetAddressOffset;
    property LowNibble : Boolean read FLowNibble write SetLowNibble;
    property Changes : Boolean read FChanges write FChanges;
  published
    property Align;
    property Anchors;
    property Border: TBorderStyle read FBorder write SetBorder;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BevelKind;
    property BiDiMode;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly;
    property ShowAddress: Boolean read FShowAddress write SetShowAddress default True;
    property ShowCharacters: Boolean read FShowCharacters write SetShowCharacters default True;
    property AddressColor: TColor index 0 read GetFileColor write SetFileColor default clBlack;
    property HexDataColor: TColor index 1 read GetFileColor write SetFileColor default clBlack;
    property AnsiCharColor: TColor index 2 read GetFileColor write SetFileColor default clBlack;
    property OnChanges : TNotifyEvent read FOnChanges write FOnChanges;
  end;

function CreateHexDump(AOwner: TWinControl): THexDump;

implementation

{ Form Methods }

function CreateHexDump(AOwner: TWinControl): THexDump;
begin
  Result := THexDump.Create(AOwner);
  with Result do
  begin
    Parent := AOwner;
    Font.Name := 'FixedSys';
    ShowCharacters := True;
    Align := alClient;
  end;
end;

{ THexDump }

constructor THexDump.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csFramed, csCaptureMouse, csClickEvents, csDoubleClicks];
  FBorder := bsSingle;
  Color := clWhite;
  FShowAddress := True;
  FShowCharacters := True;
  Width := 300;
  Height := 200;
  FillChar(FHexData, SizeOf(FHexData), #9);
end;

destructor THexDump.Destroy;
begin
  inherited Destroy;
end;

procedure THexDump.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FBorder = bsSingle then
      Style := Style or WS_BORDER;
    Style := Style or WS_VSCROLL;
  end;
end;

{ VCL Command Messages }

procedure THexDump.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Self.Font;
  FItemHeight := Canvas.TextHeight('A') + 2;
  FItemWidth := Canvas.TextWidth('D') + 1;
  CalcPaintParams;
  AdjustScrollBars;
end;

procedure THexDump.CMEnter;
begin
  inherited;
{  InvalidateLineMarker; }
end;

procedure THexDump.CMExit;
begin
  inherited;
{  InvalidateLineMarker; }
end;

{ Windows Messages }

procedure THexDump.WMSize(var Message: TWMSize);
var
  Offset : Integer;
  Obpl : Integer;
begin
  inherited;
  Obpl := fBytesPerLine;
  Offset := CurrentLine * FBytesPerLine + CurrentLinePos;
  CalcPaintParams;
  if (FBytesPerLine > 0) and (Obpl <> FBytesPerLine) then
  begin
    FCurrentLine := Offset div FBytesPerLine;
    FCurrentLinePos := Offset mod FBytesPerLine;
    SetCaretPos;
  end;
  AdjustScrollBars;
end;

procedure THexDump.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
  if not ReadOnly then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure THexDump.WMVScroll(var Message: TWMVScroll);
var
  NewTopLine: Integer;
  LinesMoved: Integer;
  R: TRect;
begin
  inherited;
  NewTopLine := FTopLine;
  case Message.ScrollCode of
    SB_LINEDOWN: Inc(NewTopLine);
    SB_LINEUP: Dec(NewTopLine);
    SB_PAGEDOWN: Inc(NewTopLine, FVisibleLines - 1);
    SB_PAGEUP: Dec(NewTopLine, FVisibleLines - 1);
    SB_THUMBPOSITION, SB_THUMBTRACK: NewTopLine := Message.Pos;
  end;

  if NewTopLine < 0 then NewTopLine := 0;
  if NewTopLine >= FLineCount then
    NewTopLine := FLineCount - 1;

  if NewTopLine <> FTopLine then
  begin
    LinesMoved := FTopLine - NewTopLine;
    FTopLine := NewTopLine;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);

    if Abs(LinesMoved) = 1 then
    begin
      R := Bounds(0, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);

      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);

      if LinesMoved = -1 then
      begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else
      begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;

      Windows.InvalidateRect(Handle, @R, False);

    end
    else Invalidate;
  end;
  SetCaretPos
end;

{ Painting Related }

procedure THexDump.CalcPaintParams;
const
  Divisor: array[Boolean] of Integer = (3,4);
var
  CharsPerLine: Integer;

begin
  if FItemHeight < 1 then Exit;
  FVisibleLines := (ClientHeight div FItemHeight) + 1;
  CharsPerLine := ClientWidth div FItemWidth;
  if FShowAddress then Dec(CharsPerLine, 10);
  FBytesPerLine := CharsPerLine div Divisor[FShowCharacters];
  if FBytesPerLine < 1 then
    FBytesPerLine := 1
  else if FBytesPerLine > MAXDIGITS then
    FBytesPerLine := MAXDIGITS;
  FLineCount := (DataSize div FBytesPerLine);
  if Boolean(DataSize mod FBytesPerLine) then Inc(FLineCount);
  if FShowAddress then
    FAddressWidth := FItemWidth*10
  else
    FAddressWidth := 0;
end;

procedure THexDump.AdjustScrollBars;
begin
  SetScrollRange(Handle, SB_VERT, 0, FLineCount - 1, True);
end;

function THexDump.ScrollIntoView: Boolean;
begin
  Result := False;
  if FCurrentLine < FTopLine then
  begin
    Result := True;
    SetTopLine(FCurrentLine);
  end
  else if FCurrentLine >= (FTopLine + FVisibleLines) - 1 then
  begin
    SetTopLine(FCurrentLine - (FVisibleLines - 2));
    Result := True;
  end;
end;

procedure THexDump.SetTopLine(Value: Integer);
var
  LinesMoved: Integer;
  R: TRect;
begin
  if Value <> FTopLine then
  begin
    if Value < 0 then Value := 0;
    if Value >= FLineCount then Value := FLineCount - 1;

    LinesMoved := FTopLine - Value;
    FTopLine := Value;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);

    if Abs(LinesMoved) = 1 then
    begin
      R := Bounds(1, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then OffsetRect(R, 0, FItemHeight);

      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @R, nil);

      if LinesMoved = -1 then
      begin
        R.Top := ClientHeight - FItemHeight;
        R.Bottom := ClientHeight;
      end
      else
      begin
        R.Top := 0;
        R.Bottom := FItemHeight;
      end;

      InvalidateRect(Handle, @R, False);

    end
    else Invalidate;
  end;
end;

procedure THexDump.SetCurrentLine(Value: Integer);
var
  R: TRect;
begin
  if Value <> FCurrentLine then
  begin
    if Value < 0 then Value := 0;
    if Value >= FLineCount then Value := FLineCount - 1;

    if (FCurrentLine >= FTopLine) and (FCurrentLine < FTopLine + FVisibleLines - 1) then
    begin
      R := Bounds(0, 0, 1, FItemHeight);
      OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
      Windows.InvalidateRect(Handle, @R, True);
    end;
    FCurrentLine := Value;

    R := Bounds(0, 0, 1, FItemHeight);
    OffsetRect(R, 0, (FCurrentLine - FTopLine) * FItemHeight);
    Windows.InvalidateRect(Handle, @R, True);
    ScrollIntoView;
    SetCaretPos
  end;
end;

procedure THexDump.Paint;
var
  R: TRect;
  I: Integer;
  TabStop: Integer;
  ByteCnt: Integer;
begin
  if Focused then
    HideCaret(handle);
  try
    inherited Paint;
    Canvas.Brush.Color := Self.Color;
    R := Bounds(1, 0, ClientWidth, FItemHeight);
    TabStop := FItemWidth*3;
    Canvas.Font.Color := FFileColors[1];
    ByteCnt := FBytesPerLine;
    for I := 0 to FVisibleLines - 1 do
    begin
      R.Left := 1;
      if (FLineCount > 0) and (I + FTopLine < FLineCount) then
      begin
        if FShowAddress then
        begin
          Canvas.Font.Color := FFileColors[0];
          R.Right := R.Left + FAddressWidth;
          ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED, @R, LineAddr(I+FTopLine), 9, nil);
          R.Left := R.Right;
          R.Right := ClientWidth;
          Canvas.Font.Color := FFileColors[1];
        end;
        if (I+FTopLine = FLineCount-1) and ((DataSize mod FBytesPerLine) > 0) then
          ByteCnt := DataSize mod FBytesPerLine;
        TabbedTextOut(Canvas.Handle, R.Left, R.Top, LineData(I+FTopLine),
          (ByteCnt*3)-1, 1, TabStop, R.Left);
        if FShowCharacters then
        begin
          R.Left := FAddressWidth+(FItemWidth*(FBytesPerLine*3));
          Canvas.Font.Color := FFileColors[2];
          ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED, @R, LineChars(I+FTopLine), ByteCnt, nil);
        end;
      end
      else ExtTextOut(Canvas.Handle, R.Left, R.Top, ETO_OPAQUE or ETO_CLIPPED,
        @R, nil, 0, nil);
      OffsetRect(R, 0, FItemHeight);
    end;
  finally
    if Focused then
      ShowCaret(handle)
  end
end;

{ Event Overrides }

procedure THexDump.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not FActive then Exit;

  case Key of
    VK_DOWN: CurrentLine := CurrentLine + 1;
    VK_UP: CurrentLine := CurrentLine - 1;
    VK_NEXT: CurrentLine := CurrentLine + FVisibleLines;
    VK_PRIOR: CurrentLine := CurrentLine - FVisibleLines;
    VK_HOME: CurrentLine := 0;
    VK_END: CurrentLine := FLineCount - 1;

    VK_LEFT : if EditCharacters or not LowNibble then
              begin
                FLowNibble := True;
                CurrentLinePos := CurrentLinePos - 1
              end
              else
                LowNibble := False;

    VK_RIGHT : if EditCharacters or LowNibble then
               begin
                 FLowNibble := False;
                 CurrentLinePos := CurrentLinePos + 1
               end
               else
                 LowNibble := True;
    VK_TAB : EditCharacters := not EditCharacters
  end;
end;

procedure THexDump.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Focused then SetFocus;
  if (Button = mbLeft) and FActive then
    CurrentLine := FTopLine + (Y div FItemHeight);
end;

{ Property Set/Get Routines }

procedure THexDump.SetBorder(Value: TBorderStyle);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    RecreateWnd;
  end;
end;

procedure THexDump.SetShowAddress(Value: Boolean);
begin
  if FShowAddress <> Value then
  begin
    FShowAddress := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetShowCharacters(Value: Boolean);
begin
  if Value <> FShowCharacters then
  begin
    FShowCharacters := Value;
    Invalidate;
  end;
end;

procedure THexDump.SetFileColor(Index: Integer; Value: TColor);
begin
  if FFileColors[Index] <> Value then
  begin
    FFileColors[Index] := Value;
    Invalidate;
  end;
end;

function THexDump.GetFileColor(Index: Integer): TColor;
begin
  Result := FFileColors[Index];
end;

procedure THexDump.SetAddress(Value: Pointer);
begin
  FActive := Value <> nil;
  FAddress := Value;
  FCurrentLine := 0;
  FTopLine := 0;
  SetScrollPos(Handle, SB_VERT, FTopLine, True);
  Invalidate;
end;

procedure THexDump.SetDataSize(Value: Integer);
begin
  FDataSize := Value;
  CalcPaintParams;
  Invalidate;
  AdjustScrollBars;
end;

function THexDump.LineAddr(Index: Integer): PChar;
begin
  Result := StrFmt(FLineAddr, '%p:', [Pointer(PChar(AddressOffset)+Index*FBytesPerLine)]);
end;

function THexDump.LineData(Index: Integer): PChar;

  procedure SetData(P: PChar);
  const
    HexDigits : array[0..15] of Char = '0123456789ABCDEF';
  var
    I: Integer;
    B: Byte;
  begin
    for I := 0 to FBytesPerLine-1 do
    begin
      try
        B := Byte(P[I]);
        FHexData[I][0] := HexDigits[B SHR $04];
        FHexData[I][1] := HexDigits[B AND $0F];
      except
        FHexData[I][0] := '?';
        FHexData[I][1] := '?';
      end;

    end;
  end;

begin
  SetData(PChar(FAddress) + Index*FBytesPerLine);
  Result := FHexData[0];
end;

function THexDump.LineChars(Index: Integer): PChar;
begin
  Result := PChar(FAddress) + Index*FBytesPerLine;
end;

procedure THexDump.CreateWnd;
begin
  inherited;
  Canvas.Font := Self.Font;
  FItemHeight := Canvas.TextHeight('A') + 2;
  FItemWidth := Canvas.TextWidth('D') + 1;
end;

procedure THexDump.SetReadOnly(const Value: Boolean);
begin
  if value <> FReadOnly then
  begin
    FReadOnly := Value;
    RecreateWnd
  end
end;

procedure THexDump.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  CreateCaret(handle, 0, 1, FItemHeight);
  SetCaretPos;
  ShowCaret(Handle)
end;

procedure THexDump.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  HideCaret(Handle);
  DestroyCaret
end;

procedure THexDump.SetCurrentLinePos(const Value: Integer);
var
  v : Integer;
begin
  if Value <> FCurrentLinePos then
  begin
    v := Value;
    while(V >= FBytesPerLine) and (CurrentLine < FLineCount - 1) do
    begin
      CurrentLine := CurrentLine + 1;
      Dec(V, FBytesPerLine)
    end;

    if V >= FBytesPerLine then
    begin
      V := FBytesPerLine - 1;
      FLowNibble := True
    end;

    while(V < 0) and (CurrentLine > 0) do
    begin
      CurrentLine := CurrentLine - 1;
      Inc(V, FBytesPerLine)
    end;

    if V < 0 then
    begin
      V := 0;
      FLowNibble := False
    end;

    FCurrentLinePos := V;
    SetCaretPos
  end
end;

procedure THexDump.SetCaretPos;
var
  x, y : Integer;
begin
  if Focused then
  begin
    y := FItemHeight * (CurrentLine - FTopLine);
    if FEditCharacters then
      x := FAddressWidth+(FItemWidth*(FBytesPerLine*3)) + (FItemWidth - 1) * CurrentLinePos
    else
    begin
      x := (FItemWidth) * 3 * CurrentLinePos + FAddressWidth;
      if FLowNibble then
        Inc(x, FItemWidth - 1)
    end;
    Windows.SetCaretPos (x, y)
  end
end;

procedure THexDump.SetEditCharacters(const Value: Boolean);
begin
  if FEditCharacters <> Value then
  begin
    FEditCharacters := Value and ShowCharacters;
    SetCaretPos
  end
end;

procedure THexDump.SetLowNibble(const Value: Boolean);
begin
  if FLowNibble <> Value then
  begin
    FLowNibble := Value;
    SetCaretPos
  end
end;

procedure THexDump.WMChar(var Message: TWMChar);
var
  ch : Char;
  Offset : Integer;
  data : byte;
  changes : Boolean;
  b : byte;
begin
  inherited;

  ch := Char (message.CharCode);
  if ch in [' '..#$ff] then
  begin
    Offset := CurrentLine * FBytesPerLine + CurrentLinePos;
    changes := False;
    if EditCharacters then
      changes := True
    else
      if ch in ['0'..'9', 'A'..'F', 'a'..'f'] then
      begin
        data := Byte(PChar (Address) [Offset]);
        changes := True;
        b := StrToInt('$' + ch);
        if LowNibble then
          ch := Char (data and $f0 + b)
        else
          ch := Char (data and $0f + (b shl 4));
      end;

    if changes then
    begin
      PChar (Address) [Offset] := ch;
      SetChanged;
      if EditCharacters or LowNibble then
      begin
        FLowNibble := False;
        CurrentLinePos := CurrentLinePos + 1
      end
      else
        LowNibble := True;
      Invalidate
    end
  end
end;

procedure THexDump.SetChanged;
begin
  if not Changes then
  begin
    FChanges := True;
    if Assigned(OnChanges) then
      OnChanges(Self)
  end
end;

procedure THexDump.SetAddressOffset(const Value: Integer);
begin
  FAddressOffset := Value;
  Invalidate
end;

end.
