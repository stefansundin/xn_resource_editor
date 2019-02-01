(*======================================================================*
 | cmpCWRichEdit                                                        |
 |                                                                      |
 | Rich edit control with support for version 2 & 3.  Supports custom   |
 | character formatting for descendant objects.                         |
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


unit cmpCWRichEdit;

interface

uses
  Windows, Messages, Classes, SysUtils, Forms, Graphics, Controls, StdCtrls,
  ComCtrls, StdActns, RichEdit, Dialogs, RichOLE;

type
  TCustomExRichEdit = class;

//--------------------------------------------------------------------
// Provide rich edit services from the Microsoft richedit control.
// Overriden for each version of richedit(v1, v2, v3) so that we
// can take their individual requirements into account.
//
// nb. v3 is supported by W2K, XP
//     v2 is supported by NT4, W98, W95 with appropriate DLL
//     v1 is supported by NT 3.51 & W95

  TRichEditProvider = class
  private
    FOwner: TCustomExRichEdit;

  protected
    procedure CreateSubclass (var Params: TCreateParams); virtual; abstract;
    procedure SetText(const st: WideString); virtual; abstract;
    function GetText: WideString; virtual; abstract;
    function GetSelText: WideString; virtual; abstract;
    function GetTextLen: Integer; virtual; abstract;
    function GetCRLFTextLen: Integer; virtual; abstract;
    procedure SetSelText(const value: WideString); virtual; abstract;
    function UsesCRLF: Boolean; virtual; abstract;
    function GetIRichEditOLE: IRichEditOLE; virtual;
  public
    constructor Create(AOwner: TCustomExRichEdit);

    property Owner: TCustomExRichEdit read FOwner;
  end;

//--------------------------------------------------------------
// Stream class used to support EM_STREAMIN & EM_STREAMOUT
  TRichEditStream = class
  private
    FBuffer: PWideChar;
    FLen: DWORD;
    FChunkStart, FChunkEnd: DWORD;
    FPos: DWORD;
    FOwner: TCustomExRichEdit;
    FOwnsBuffer: Boolean;
    FTruncate: Boolean;
  public
    constructor Create(AOwner: TCustomExRichEdit);
    destructor Destroy; override;
    procedure GetWideString (var ws: WideString);
    procedure GetString (var st: string);
    property Owner: TCustomExRichEdit read FOwner;
    property ChunkStart: DWORD read FChunkStart write FChunkStart;
    property ChunkEnd: DWORD read FChunkEnd write FChunkEnd;
    property Truncate: Boolean read FTruncate write FTruncate;
    property Len: DWORD read FLen;
    property Buffer: PWideChar read FBuffer;
  end;

//---------------------------------------------------------------------
// TCharFormatter is used to format characters.  It's GetFormattedChars
// method is called (repeatedly) during streaming-in.  Each time it is
// called it should set the stream's ChunkStart & ChunkEnd values,
// indicating which portion of the stream's buffer should be formatted with
// the returned TCharFormat details.
//
// This base class simply sets ChunkStart to the start of the stream,
// ChunkEnd to the end of the stream, and returns details for the default
// Font.
//
// To use a different TCharFormatter, override the rich edit's GetCharFormatter
// to return a descendant.

  TCharFormatter = class
  public
    procedure GetFormattedChars (stream: TRichEditStream; var fc: TCharFormat); virtual;
    procedure Reset; virtual;
  end;

  TExRichEditResizeEvent = procedure(Sender: TObject; Rect: TRect) of object;
  TExRichEditProtectChange = procedure(Sender: TObject; StartPos, EndPos: Integer; var AllowChange: Boolean) of object;
  TExRichEditSaveClipboard = procedure(Sender: TObject; NumObjects, NumChars: Integer; var SaveClipboard: Boolean) of object;

//---------------------------------------------------------------------
// TCustomExRichEdit
  TCustomExRichEdit = class(TWinControl)
  private
    FWantReturns: Boolean;
    FWantTabs: Boolean;
    FWordWrap: Boolean;
    FBorderStyle: TBorderStyle;
    FAlignment: TAlignment;
    FScrollBars: TScrollStyle;
    FHideSelection: Boolean;
    FHideScrollBars: Boolean;
    FRichEditProvider: TRichEditProvider;
    FReadOnly: Boolean;
    FMaxLength: Integer;
    FOnProtectChange: TExRichEditProtectChange;
    FOnResizeRequest: TExRichEditResizeEvent;
    FOnSaveClipboard: TExRichEditSaveClipboard;
    FOnSelChange: TNotifyEvent;
    FCharFormatter: TCharFormatter;
    FDefaultCharFormat: TCharFormat;
    FAutoURLDetect: Boolean;
    FAutoURLExecute: Boolean;
    FURLText: string;
    FOnURLMouseDown: TMouseEvent;
    FOnURLMouseUp: TMouseEvent;
    FOnURLMouseMove: TMouseMoveEvent;
    FOnURLClick: TNotifyEvent;
    FOnURLDblClick: TNotifyEvent;
    FCodePage: Integer;
    FUpdateCount: Integer;
    FRightMargin: Integer;
    FOnFontChange: TNotifyEvent;
    FFixedFont: Boolean;
    FAveCharWidth: Integer;
    FInFontChange: Boolean;
    FPageRect: TRect;
    FMultiLine: Boolean;
    FInSetText: Boolean;
    FInGetText: Boolean;
    FStreamRTF: Boolean;
    FRawPaste: Boolean;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMRecreateWnd(var Message: TMessage); message CM_RECREATEWND;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure DoURLMouseDown(var Message: TWMMouse; Button: TMouseButton; Shift: TShiftState);
    procedure DoURLMouseUp(var Message: TWMMouse; Button: TMouseButton; Shift: TShiftState);
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;

    procedure SetUpFont(Font: TFont; All: Boolean);
    function ProtectChange(StartPos, EndPos: Integer): Boolean;
    function SaveClipboard(NumObj, NumChars: Integer): Boolean;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetHideScrollBars(const Value: Boolean);
    procedure SetHideSelection(const Value: Boolean);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetMaxLength(const Value: Integer);
    procedure SetText(const Value: WideString);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
    procedure WmGetText(var Message: TWMGetText); message WM_GETTEXT;
    procedure WmSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WmGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    function GetText: WideString;
    procedure SetAutoURLDetect(const Value: Boolean);
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    function GetCanUndo: Boolean;
    function GetSelText: WideString;
    procedure SetSelText(const Value: WideString);
    function GetUpdating: Boolean;
    procedure SetRightMargin(const Value: Integer);
    function GetCodePage: Integer;
    function GetCanRedo: Boolean;
    procedure SetMultiLine(const Value: Boolean);
    function GetUsesCRLF: Boolean;
    function GetLineCount: Integer;
  protected
    function GetCharFormatter: TCharFormatter; virtual;
    function CreateAppropriateProvider: TRichEditProvider; virtual;
    procedure CreateParams (var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoSetMaxLength(Value: Integer); virtual;
    procedure KeyPress(var Key: Char); override;
    procedure RequestSize(const Rect: TRect); virtual;
    procedure SelectionChange; dynamic;
    procedure URLClick; dynamic;
    procedure URLDblClick; dynamic;
    procedure URLMouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer); dynamic;
    procedure URLMouseUp(Button: TMouseButton; Shift: TShiftState;X, Y: Integer); dynamic;
    procedure URLMouseMove(Shift: TShiftState; X, Y: Integer); dynamic;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoURLDetect: Boolean read FAutoURLDetect write SetAutoURLDetect;
    property AutoURLExecute: Boolean read FAutoURLExecute write FAutoURLExecute;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property CharFormatter: TCharFormatter read GetCharFormatter;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property HideScrollBars: Boolean read FHideScrollBars write SetHideScrollBars default True;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default True;
    property ParentColor default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssNone;
    property WantReturns: Boolean read FWantReturns write FWantReturns default True;
    property WantTabs: Boolean read FWantTabs write FWantTabs default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;

    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;
    property OnSaveClipboard: TExRichEditSaveClipboard read FOnSaveClipboard write FOnSaveClipboard;
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnProtectChange: TExRichEditProtectChange read FOnProtectChange write FOnProtectChange;
    property OnResizeRequest: TExRichEditResizeEvent read FOnResizeRequest write FOnResizeRequest;
    property OnURLMouseDown: TMouseEvent read FOnURLMouseDown write FOnURLMouseDown;
    property OnURLMouseUp: TMouseEvent read FOnURLMouseUp write FOnURLMouseUp;
    property OnURLMouseMove: TMouseMoveEvent read FOnURLMouseMove write FOnURLMouseMove;
    property OnURLDblClick: TNotifyEvent read FOnURLDblClick write FOnURLDblClick;
    property OnURLClick: TNotifyEvent read FOnURLClick write FOnURLClick;
    property InUpdate: Boolean read GetUpdating;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearUndoBuffer;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function FindText(const SearchStr: string; StartPos, Length: Integer; Options: TSearchTypes): Integer;
    procedure FontToCharFormat(Font: TFont; var Format: TCharFormat);
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure PasteTextFromClipboard;
    procedure Print;
    property PageRect: TRect read FPageRect write FPageRect;
    procedure SelectAll;
    procedure SetupRightMargin;
    procedure Undo;
    procedure Redo;
    function GetTextLen: Integer;
    function GetCRLFTextLen: Integer;

    procedure SetRawSelection (ss, se: Integer);
    procedure GetRawSelection (var ss, se: Integer);

    procedure BeginUpdate;
    procedure EndUpdate;

    property CodePage: Integer read GetCodePage write FCodePage;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property DefaultCharFormat: TCharFormat read fDefaultCharFormat;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelText: WideString read GetSelText write SetSelText;
    property URLText: string read FURLText;
    property AveCharWidth: Integer read FAveCharWidth;
    property FixedFont: Boolean read FFixedFont;

    property Font;
    property UsesCRLF: Boolean read GetUsesCRLF;
    property StreamRTF: Boolean read FStreamRTF write FStreamRTF;
    property RawPaste: Boolean read FRawPaste write FRawPaste;
    function GetIRichEditOLE: IRichEditOLE;
    property LineCount: Integer read GetLineCount;

  published
    property Text: WideString read GetText write SetText;
    property RightMargin: Integer read FRightMargin write SetRightMargin;
  end;

//---------------------------------------------------------------------
// TRichEdit1Provider.  Provider for RichEdit version 1
  TRichEdit1Provider = class (TRichEditProvider)
  private
  protected
    procedure CreateSubclass (var Params: TCreateParams); override;
    procedure SetText(const st: WideString); override;
    function GetText: WideString; override;
    function GetSelText: WideString; override;
    function GetTextLen: Integer; override;
    function GetCRLFTextLen: Integer; override;
    function UsesCRLF: Boolean; override;
    procedure SetSelText(const value: WideString); override;
  end;

//---------------------------------------------------------------------
// TRichEdit1Provider.  Provider for RichEdit version 2 & 3.  They are
// so similar that it's worth providing this base class
  TRichEdit2_3Provider = class (TRichEditProvider)
  private
    FIRichEditOLE: IRichEditOLE;
    FGotIRichEditOLE: Boolean;
    function RawGetText(sel: Boolean): WideString;
    procedure RawSetText(const st: WideString; sel: Boolean);
  protected
    procedure CreateSubclass (var Params: TCreateParams); override;
    procedure SetText(const st: WideString); override;
    function GetText: WideString; override;
    function GetSelText: WideString; override;
    function GetTextLen: Integer; override;
    function GetCRLFTextLen: Integer; override;
    procedure SetSelText(const value: WideString); override;
    function UsesCRLF: Boolean; override;
    function GetIRichEditOLE: IRichEditOLE; override;
  public
    destructor Destroy; override;

  end;

//---------------------------------------------------------------------
// TRichEdit2Provider.  Provider for RichEdit version 2
  TRichEdit2Provider = class (TRichEdit2_3Provider)
  end;

//---------------------------------------------------------------------
// TRichEdit3Provider.  Provider for RichEdit version 3
  TRichEdit3Provider = class (TRichEdit2_3Provider)
  end;

//---------------------------------------------------------------------
// TExRichEdit
  TExRichEdit = class (TCustomExRichEdit)
    property Align;
    property Alignment;
    property Anchors;
    property AutoURLDetect;
    property AutoURLExecute;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property ImeMode;
    property ImeName;
    property Constraints;
//    property Lines;
    property MaxLength;
    property MultiLine;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
//    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordWrap;
//    property OnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFontChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
    property OnURLMouseDown;
    property OnURLMouseUp;
    property OnURLMouseMove;
    property OnURLDblClick;
    property OnURLClick;
  end;

  EcwRichEdit = class (Exception);

implementation

uses
  ShellAPI, ClipBrd, Printers, unitCharsetMap;

var
  gRichEditModule: THandle;

resourcestring
  rstInvalidOSVersion = 'Rich Edit control is not supported on this operating system';

{ TCustomExRichEdit }

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.BeginUpdate                              |
 |                                                                      |
 | Call this to prevent further updates intil EndUpdate is called       |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then
    SendMessage(Handle, WM_SETREDRAW, 0, 0)
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.Clear                                    |
 |                                                                      |
 | Clear the rich edit's text                                           |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.Clear;
begin
  SetWindowText(Handle, '');
  StrDispose(WindowText);
  WindowText := Nil
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CMBiDiModeChanged                        |
 |                                                                      |
 | Message handler for CM_BIDIMODECHANGED message                       |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  HandleNeeded; { we REALLY need the handle for BiDi }
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CMColorChanged                           |
 |                                                                      |
 | Message handler for CM_COLORCHANGED message                          |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CMColorChanged(var Message: TMessage);
begin
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color))
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CMFontChanged                            |
 |                                                                      |
 | Message handler for CM_FONTCHANGED message                           |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CMFontChanged(var Message: TMessage);
begin
  SetUpFont(Font, True);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CNNotify                                 |
 |                                                                      |
 | Message handler for CN_NOTIFY message                                |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CNNotify(var Message: TWMNotify);
type
  PENLink = ^TENLink;
var
  ENLink: PENLink;
  mouseMsg: TWMMouse;
  textRange: TTextRangeA;
begin
  with Message do
    case NMHdr^.code of
      EN_SELCHANGE: SelectionChange;
      EN_REQUESTRESIZE: RequestSize(PReqSize(NMHdr)^.rc);
      EN_SAVECLIPBOARD:
        with PENSaveClipboard(NMHdr)^ do
          if not SaveClipboard(cObjectCount, cch) then Result := 1;
      EN_PROTECTED:
        with PENProtected(NMHdr)^.chrg do
          if not ProtectChange(cpMin, cpMax) then Result := 1;

      EN_LINK :
        begin
          ENLink := PENLink(NMHdr);
          mouseMsg.Msg := ENLink^.Msg;
          mouseMsg.Keys := ENLink^.wParam;
          mouseMsg.XPos := LoWord (ENLink^.lParam);
          mouseMsg.YPos := HiWord (ENLink^.lParam);
          mouseMsg.Result := 0;

          SetLength(FURLText, 65536);
          textRange.chrg := ENLink^.chrg;
          textRange.lpstrText := PAnsiChar(FURLText);
          SendMessage(Handle, EM_GETTEXTRANGE, 0, lParam (@textRange));
          FURLText := PChar (FURLText);

          case ENLink^.Msg of
            WM_LBUTTONDOWN :
              begin
                if csClickEvents in ControlStyle then URLClick;
                DoURLMouseDown (mouseMsg, mbLeft, []);
              end;
            WM_RBUTTONDOWN: DoURLMouseDown (mouseMsg, mbRight, []);
            WM_MBUTTONDOWN: DoURLMouseDown (mouseMsg, mbMiddle, []);
            WM_LBUTTONUP: DoURLMouseUp (mouseMsg, mbLeft, []);
            WM_RBUTTONUP: DoURLMouseUp (mouseMsg, mbRight, []);
            WM_MBUTTONUP: DoURLMouseUp (mouseMsg, mbMiddle, []);
            WM_LBUTTONDBLCLK :
              begin
                if csClickEvents in ControlStyle then URLDblClick;
                DoUrlMouseDown(mouseMsg, mbLeft, [ssDouble]);
              end;
            WM_MOUSEMOVE:
              if not (csNoStdEvents in ControlStyle) then
                with mouseMsg do
                  if (Width > 32768) or (Height > 32768) then
                    with CalcCursorPos do
                      URLMouseMove(KeysToShiftState(Keys), X, Y)
                  else
                    URLMouseMove(KeysToShiftState(Keys), XPos, YPos);
          end
        end
    end;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CopyToClipboard                          |
 |                                                                      |
 | Copy selected text to clipboard                                      |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CopyToClipboard;
begin
  SendMessage(Handle, WM_COPY, 0, 0);
end;

(*----------------------------------------------------------------------*
 | constructor TCustomExRichEdit.Create                                 |
 |                                                                      |
 | Constructor for TCustomExRichEdit                                    |
 *----------------------------------------------------------------------*)
constructor TCustomExRichEdit.Create(AOwner: TComponent);
const
  EditStyle = [csClickEvents, csSetCaption, csDoubleClicks, csNeedsBorderPaint];
begin
  FRichEditProvider := CreateAppropriateProvider;
                        // Create appropriate provider for the operating system
  FCodePage := -1;      // RichEdit1 only!

  inherited;

  if NewStyleControls then
    ControlStyle := EditStyle else
    ControlStyle := EditStyle + [csFramed];

  Width := 185;         // Set defaults
  Height := 89;
  ParentBackground := False;

  FWordWrap := True;
  FWantReturns := True;
  FBorderStyle := bsSingle;
  FHideSelection := True;
  FHideScrollBars := True;
  FMultiLine := True;

  ParentColor := False;
  TabStop := True;

  Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.CreateAppropriateProvider                 |
 |                                                                      |
 | Create an appropriate provider for the operating system              |
 |                                                                      |
 | The function returns the appropriate provder.                        |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.CreateAppropriateProvider: TRichEditProvider;
begin
  if Win32MajorVersion >= 5 then  // W2K, XP
    Result := TRichEdit3Provider.Create(Self)
  else
    if Win32MajorVersion = 4 then // 95, 98, ME, NT4
    begin
      if Win32MinorVersion >= 90 then // ME
        Result := TRichEdit3Provider.Create(Self)
      else
        if Win32MinorVersion >= 10 then // 98
          Result := TRichEdit2Provider.Create(Self)
        else
          if Win32Platform = VER_PLATFORM_WIN32_NT then // NT 4
            Result := TRichEdit2Provider.Create(Self)
          else
            Result := TRichEdit1Provider.Create(Self)
    end
    else
      if Win32MajorVersion = 3 then
        Result := TRichEdit1Provider.Create(Self)
      else
        raise EcwRichEdit.Create(rstInvalidOSVersion) // Impossible!
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CreateParams                             |
 |                                                                      |
 | Overridden 'CreateParams'  Set the appropriate styles for window     |
 | creation.                                                            |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[Boolean, TAlignment] of DWORD = ((ES_LEFT, ES_RIGHT, ES_CENTER),(ES_RIGHT, ES_LEFT, ES_CENTER));
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  MultiLines: array[Boolean] of DWORD = (0, ES_MULTILINE);
  HideScrollBars: array[Boolean] of DWORD = (ES_DISABLENOSCROLL, 0);
  HideSelections: array[Boolean] of DWORD = (ES_NOHIDESEL, 0);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ReadOnlys: array[Boolean] of DWORD = (0, ES_READONLY);
begin
  inherited CreateParams (Params);

  with Params do
  begin
    Style := Style or (ES_AUTOHSCROLL or ES_AUTOVSCROLL) or
      BorderStyles[FBorderStyle] or
      ReadOnlys[FReadOnly] or
      Alignments[UseRightToLeftAlignment, FAlignment] or ScrollBar[FScrollBars] or
      HideScrollBars[FHideScrollBars] or
      HideSelections[FHideSelection] or
      MultiLines[FMultiLine];

    Style := Style and not WordWraps[FWordWrap];

    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
  end;

  FRichEditProvider.CreateSubclass(Params);

  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CreateWnd                                |
 |                                                                      |
 | Overridden CreateWnd.  Perform extra steps after the window handle   |
 | is created                                                           |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CreateWnd;
var
  eventMask: DWORD;
begin
  inherited CreateWnd;
  if (SysLocale.FarEast) and not (SysLocale.PriLangID = LANG_JAPANESE) then
    Font.Charset := GetDefFontCharSet;

  eventMask := ENM_CHANGE or ENM_SELCHANGE or ENM_REQUESTRESIZE or ENM_PROTECTED;

  if FAutoURLDetect then
  begin
    SendMessage(Handle, EM_AUTOURLDETECT, 1, 0);
    EventMask := EventMask or ENM_LINK;
  end;

  SendMessage(Handle, EM_SETEVENTMASK, 0, eventMask);
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color));
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CreateWindow                             |
 |                                                                      |
 | Overridden CreateWindowHandle.                                       |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  with Params do
  begin
    if SysLocale.FarEast and (Win32Platform <> VER_PLATFORM_WIN32_NT) and
      ((Style and ES_READONLY) <> 0) then
    begin
      // Work around Far East Win95 API/IME bug.
      WindowHandle := CreateWindowEx(ExStyle, WinClassName, '',
        Style and (not ES_READONLY),
        X, Y, Width, Height, WndParent, 0, HInstance, Param);
      if WindowHandle <> 0 then
        SendMessage(WindowHandle, EM_SETREADONLY, Ord(True), 0);
    end
    else
      WindowHandle := CreateWindowEx(ExStyle, WinClassName, '', Style, X, Y, Width, Height, WndParent, 0, WindowClass.hInstance, Param);

    if WindowHandle = 0 then
      Raise Exception.Create('Unable to create window handle');

    DoSetMaxLength(fMaxLength);
    if SendMessage(WindowHandle, WM_SETTEXT, 0, Longint(Caption)) = 0 then
      RaiseLastOSError;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.CutToClipboard                           |
 |                                                                      |
 | Cut selected text to clipboard                                       |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.CutToClipboard;
begin
  SendMessage(Handle, WM_CUT, 0, 0);
end;

(*----------------------------------------------------------------------*
 | destructor TCustomExRichEdit.Destroy                                 |
 |                                                                      |
 | Destructor for TCustomExRichEdit                                     |
 *----------------------------------------------------------------------*)
destructor TCustomExRichEdit.Destroy;
begin
  FRichEditProvider.Free;
  fCharFormatter.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.DoSetMaxLength                           |
 |                                                                      |
 | Set the maximum length of the control.                               |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.DoSetMaxLength(Value: Integer);
begin
  SendMessage(Handle, EM_EXLIMITTEXT, 0, Value);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.DoURLMouseDown                           |
 |                                                                      |
 | Handle URL MouseDown messages                                        |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.DoURLMouseDown(var Message: TWMMouse;
  Button: TMouseButton; Shift: TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      if (Width > 32768) or (Height > 32768) then
        with CalcCursorPos do
          URLMouseDown(Button, KeysToShiftState(Keys) + Shift, X, Y)
      else
        URLMouseDown(Button, KeysToShiftState(Keys) + Shift, Message.XPos, Message.YPos);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.DoURLMouseUp                             |
 |                                                                      |
 | Handle URL MouseUp messages                                          |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.DoURLMouseUp(var Message: TWMMouse;
  Button: TMouseButton; Shift: TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
    with Message do
      if (Width > 32768) or (Height > 32768) then
        with CalcCursorPos do
          URLMouseUp (Button, KeysToShiftState(Keys) + Shift, X, Y)
      else
        URLMouseUp(Button, KeysToShiftState(Keys) + Shift, Message.XPos, Message.YPos);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.EndUpdate                                |
 |                                                                      |
 | End updating                                                         |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    SendMessage(Handle, EM_REQUESTRESIZE, 0, 0);
  end
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.FindText                                  |
 |                                                                      |
 | Find text.  Return it's position                                     |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.FindText(const SearchStr: string; StartPos,
  Length: Integer; Options: TSearchTypes): Integer;
const
  EM_FINDTEXTW = WM_USER + 123;
  FT_DOWN = 1;
  FT_FINDNEXT = 8;
var
  Find: TFindText;
  Flags: Integer;
  ws: WideString;
begin
  with Find.chrg do
  begin
    cpMin := StartPos;
    cpMax := -1; // cpMin + Length;
  end;
  Flags := FT_DOWN;
  if stWholeWord in Options then Flags := Flags or FT_WHOLEWORD;
  if stMatchCase in Options then Flags := Flags or FT_MATCHCASE;
  ws := SearchStr;
  Find.lpstrText := PChar (SearchStr);
  Result := SendMessage(Handle, EM_FINDTEXT, Flags, LongInt(@Find));
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.FontToCharFormat                         |
 |                                                                      |
 | Fill in a TCharFormat structure from a given TFont's details.        |
 |                                                                      |
 | Parameters:                                                          |
 |   Font: TFont                // The Font to use                      |
 |   var Format: TCharFormat    // The stucture to fill                 |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.FontToCharFormat(Font: TFont; var Format: TCharFormat);
begin
  FillChar (Format, SizeOf(Format), 0);

  Format.cbSize := SizeOf(Format);
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
  Format.bCharSet := CodePageToCharset(CodePage);

  case Font.Pitch of
    fpVariable: Format.bPitchAndFamily := VARIABLE_PITCH;
    fpFixed: Format.bPitchAndFamily := FIXED_PITCH;
    else Format.bPitchAndFamily := DEFAULT_PITCH;
  end;

  lstrcpyn (Format.szFaceName, PChar (Font.Name),LF_FACESIZE - 1) ;
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetCanUndo                                |
 |                                                                      |
 | Get method for CanUndo property                                      |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetCanUndo: Boolean;
begin
  Result := False;
  if HandleAllocated then Result := SendMessage(Handle, EM_CANUNDO, 0, 0) <> 0;
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetCharFormatter                          |
 |                                                                      |
 | Return a TCharFormatter object.  This isan object of the default     |
 | TCharFormatter type, but it can be overridden to return a different  |
 | formatter.                                                           |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetCharFormatter: TCharFormatter;
begin
  if fCharFormatter = Nil then
    fCharFormatter := TCharFormatter.Create;
  Result := fCharFormatter
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetSelLength                              |
 |                                                                      |
 | Get method for SelLength property                                    |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetSelLength: Integer;
var
  range: TCharRange;
begin
  SendMessage(Handle, EM_EXGETSEL, 0, LongInt(@range));
  Result := range.cpMax - range.cpMin
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetSelStart                               |
 |                                                                      |
 | Get method for SelStart property                                     |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetSelStart: Integer;
var
  range: TCharRange;
begin
  SendMessage(Handle, EM_EXGETSEL, 0, LongInt(@range));
  Result := range.cpMin
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetSelText                                |
 |                                                                      |
 | Getmethod for SelText property                                       |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetSelText: WideString;
begin
  Result := FRichEditProvider.GetSelText;
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetText                                   |
 |                                                                      |
 | Get method for Text property                                         |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetText: WideString;
begin
  Result := FRichEditProvider.GetText
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetTextLen                                |
 |                                                                      |
 | Get method for TextLen property                                      |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetTextLen: Integer;
begin
  Result := FRichEditProvider.GetTextLen
end;

(*----------------------------------------------------------------------*
 | function TCustomExRichEdit.GetUpdating                               |
 |                                                                      |
 | Get methpd for the IsUpdating property                               |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.GetUpdating: Boolean;
begin
  Result := FUpdateCount > 0
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.KeyPress                                 |
 |                                                                      |
 | Filter out carriage returns if required                              |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.KeyPress(var Key: Char);
begin
  if (Key = Char(VK_RETURN)) and (not FWantReturns) then Key := #0;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.PasteFromClipboard                       |
 |                                                                      |
 | Replace selection with clipboard text                                |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.PasteFromClipboard;
begin
  SendMessage(Handle, WM_PASTE, 0, 0);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.ProtectChange                            |
 |                                                                      |
 | Handle 'Protect Change' notification messages                        |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.ProtectChange(StartPos,
  EndPos: Integer): Boolean;
begin
  Result := False;
  if Assigned(OnProtectChange) then OnProtectChange(Self, StartPos, EndPos, Result);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.RequestSize                              |
 |                                                                      |
 | Handle 'Request Size' notification messages sent by bottomless       |
 | richedit controls.  RichEdit sends these when it's size needs to     |
 | change because text has been added or removed.                       |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.RequestSize(const Rect: TRect);
begin
  if InUpdate then              // Because we set formatted text in several
    Exit;                       // chunks we need to prevent the control from
                                // resizing itself except at at the end.
                                // Otherwise it's all terribly slow.

  if Assigned(OnResizeRequest) then OnResizeRequest(Self, Rect);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SaveClipboard                            |
 |                                                                      |
 | Handle to SaveClipboard notification messages                        |
 *----------------------------------------------------------------------*)
function TCustomExRichEdit.SaveClipboard(NumObj, NumChars: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnSaveClipboard) then OnSaveClipboard(Self, NumObj, NumChars, Result);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SelectAll                                |
 |                                                                      |
 | Select All text                                                      |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SelectAll;
begin
  SendMessage(Handle, EM_SETSEL, 0, -1);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SelectionChange                          |
 |                                                                      |
 | Handle 'selection change' notification messages                      |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SelectionChange;
begin
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetAlignment                             |
 |                                                                      |
 | Set method for Alignment property                                    |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetAutoURLDetect                         |
 |                                                                      |
 | Set method for AutoURLDetect property                                |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetAutoURLDetect(const Value: Boolean);
var
  eventMask: Integer;
begin
  if Value <> FAutoURLDetect then
  begin
    FAutoURLDetect := Value;
    if HandleAllocated then
    begin
      if SendMessage(Handle, EM_AUTOURLDETECT, Integer (Value), 0) = 0 then
      begin
        if FAutoURLDetect then
          EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0) or ENM_LINK
        else
          EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0) and not ENM_LINK;
        SendMessage(Handle, EM_SETEVENTMASK, 0, EventMask);
      end
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetBorderStyle                           |
 |                                                                      |
 | Set method for BorderStyle property                                  |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetHideScrollBars                        |
 |                                                                      |
 | Set method for HideScrollBars property                               |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetHideScrollBars(const Value: Boolean);
begin
  if HideScrollBars <> Value then
  begin
    FHideScrollBars := value;
    RecreateWnd
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetHideSelection                         |
 |                                                                      |
 | Set method for HideSelection property                                |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetHideSelection(const Value: Boolean);
begin
  if HideSelection <> Value then
  begin
    FHideSelection := Value;
    SendMessage(Handle, EM_HIDESELECTION, Ord(HideSelection), LongInt(True))
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetMaxLength                             |
 |                                                                      |
 | Set method for MaxLength property                                    |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetMaxLength(const Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
    if HandleAllocated then DoSetMaxLength(Value);
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetReadOnly                              |
 |                                                                      |
 | Set method for ReadOnly property                                     |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    if HandleAllocated then
      SendMessage(Handle, EM_SETREADONLY, Ord(Value), 0);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetScrollBars                            |
 |                                                                      |
 | Set method for ScrollBars property                                   |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if Value <> FScrollBars then
  begin
    FScrollBars := Value;
    RecreateWnd
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetSelLength                             |
 |                                                                      |
 | Set method for SelLength property                                    |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetSelLength(const Value: Integer);
var
  range: TCharRange;
begin
  SendMessage(Handle, EM_EXGETSEL, 0, Longint(@range));
  range.cpMax := range.cpMin + Value;
  SendMessage(Handle, EM_EXSETSEL, 0, Longint(@range));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetSelText                               |
 |                                                                      |
 | Set method for SelText property                                      |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetSelText(const Value: WideString);
begin
  FRichEditProvider.SetSelText(value);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetSelStart                              |
 |                                                                      |
 | Set method for SelStart property                                     |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetSelStart(const Value: Integer);
var
  range: TCharRange;
begin
  range.cpMin := Value;
  range.cpMax := Value;
  SendMessage(Handle, EM_EXSETSEL, 0, Longint(@range));
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetText                                  |
 |                                                                      |
 | 'Set' method for Text property                                       |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetText(const Value: WideString);
var
  st: string;
begin
  if HandleAllocated then
  begin
    if Value = '' then
      Clear
    else
      FRichEditProvider.SetText(value);
    ClearUndoBuffer
  end
  else
  begin
    st := WideStringToString (Value, FCodePage);
    StrDispose(WindowText);
    WindowText := StrNew (PChar (st))
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetUpFont                                |
 |                                                                      |
 | Set the Font parameters for All text or just selected text.          |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetUpFont(Font: TFont; All: Boolean);
var
  Format: TCharFormat;
  Flag: DWORD;
  canvas: TControlCanvas;
  tm: TTextMetric;
begin
  if HandleAllocated then
  begin
    if All then
      Flag := SCF_ALL
    else
      Flag := SCF_SELECTION;

    FontToCharFormat(Font, Format);

    SendMessage(Handle, EM_SETCHARFORMAT, Flag, LPARAM(@Format));

    fDefaultCharFormat := Format;

    canvas := TControlCanvas.Create;
    try
      canvas.Control := Self;
      canvas.Font.Assign(Font);
      GetTextMetrics (canvas.Handle, tm);

      FAveCharWidth := tm.tmAveCharWidth;
      FFixedFont := (tm.tmPitchAndFamily and TMPF_FIXED_PITCH) = 0;
    finally
      canvas.Free
    end;

    SetupRightMargin;

    if not FInFontChange then
    try
      FInFontChange := True;
      if Assigned(OnFontChange) and not (csDestroying in ComponentState) then
        OnFontChange(Self);
    finally
      FInFontChange := False
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.SetWordWrap                              |
 |                                                                      |
 | Set method for WordWrap property                                     |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.SetWordWrap(const Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    RecreateWnd
  end
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.Undo                                     |
 |                                                                      |
 | Undo last action                                                     |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.Undo;
begin
  SendMessage(Handle, WM_UNDO, 0, 0);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.URLClick                                 |
 |                                                                      |
 | Respond to URL Click notification messages                           |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.URLClick;
begin
  if Assigned(OnURLClick) then OnURLClick(Self);
  if AutoURLExecute then
    ShellExecute(HWND_DESKTOP, 'open', PChar (FURLText), nil, nil, SW_SHOWNORMAL);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.URLDblClick                              |
 |                                                                      |
 | Respond to URL DblClick notification messages                        |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.URLDblClick;
begin
  if Assigned(OnURLDblClick) then OnURLDblClick(Self);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.URLMouseDown                             |
 |                                                                      |
 | Respond to URL MouseDown notification messages                       |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.URLMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnURLMouseDown) then
    OnURLMouseDown(Self, Button, Shift, X, Y)
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.URLMouseMove                             |
 |                                                                      |
 | Respond to URL Mouse Move notification messages                      |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.URLMouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(OnURLMouseMove) then OnURLMouseMove(Self, Shift, X, Y);
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.URLMouseUp                               |
 |                                                                      |
 | Respond to URL MouseUp notification messag                           |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.URLMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnURLMouseUp) then
    OnURLMouseUp(Self, Button, Shift, X, Y)
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.WMGetDlgCode                             |
 |                                                                      |
 | Message handler for WM_GETDLGCODE message.  Indicate that we want    |
 | tabs and carriage returns                                            |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
var
  state: SHORT;
begin
  inherited;
  state := GetAsyncKeyState(VK_SHIFT);
  if (state = 0) and FWantTabs then Message.Result := Message.Result or DLGC_WANTTAB
  else Message.Result := Message.Result and not DLGC_WANTTAB;
  if not FWantReturns then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

var
  Painting: Boolean = False;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.WMPaint                                  |
 |                                                                      |
 | Respond to WMPaint messages                                          |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.WMPaint(var Message: TWMPaint);
var
  R, R1: TRect;
begin
  if GetUpdateRect(Handle, R, True) then
  begin
    with ClientRect do R1 := Rect(Right - 3, Top, Right, Bottom);
    if IntersectRect(R, R, R1) then InvalidateRect(Handle, @R1, True);
  end;
  if Painting then
    Invalidate
  else begin
    Painting := True;
    try
      inherited;
    finally
      Painting := False;
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.WMSetCursor                              |
 |                                                                      |
 | Respond to WM_SETCURSOR messages.  Set the correct cursor - Arrow    |
 | or IBeam                                                             |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  inherited;
  if Message.Result = 0 then
  begin
    Message.Result := 1;
    GetCursorPos(P);
    with PointToSmallPoint(P) do
      case Perform(WM_NCHITTEST, 0, MakeLong(X, Y)) of
        HTVSCROLL,
        HTHSCROLL:      // Arrow on scroll-bars
          Windows.SetCursor(Screen.Cursors[crArrow]);
        HTCLIENT:       // I-Beam on client
          Windows.SetCursor(Screen.Cursors[crIBeam]);
      end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TCustomExRichEdit.WMSetFont                                |
 |                                                                      |
 | Respond to WMFont messages.  Set the default Font                    |
 *----------------------------------------------------------------------*)
procedure TCustomExRichEdit.WMSetFont(var Message: TWMSetFont);
begin
  SetUpFont(Font, True);
end;

{ TRichEditProvider }

(*----------------------------------------------------------------------*
 | constructor TRichEditProvider.Create                                 |
 |                                                                      |
 | Constructor for the TRichEditProvider basse class                    |
 *----------------------------------------------------------------------*)
constructor TRichEditProvider.Create(AOwner: TCustomExRichEdit);
begin
  FOwner := AOwner
end;

{ TRichEdit1Provider }
(*----------------------------------------------------------------------*
 | procedure TRichEdit1Provider.CreateSubclass                          |
 |                                                                      |
 | Use the Rich Edit v1 control                                         |
 *----------------------------------------------------------------------*)
procedure TRichEdit1Provider.CreateSubclass(var Params: TCreateParams);
begin
  if gRichEditModule = 0 then
    gRichEditModule := LoadLibrary('RICHED32.DLL');
  Owner.CreateSubClass(Params, 'RICHEDIT');
end;

(*----------------------------------------------------------------------*
 | function TRichEdit1Provider.GetSelText                               |
 |                                                                      |
 | (v1)  Get the selected text                                          |
 *----------------------------------------------------------------------*)
function TRichEdit1Provider.GetCRLFTextLen: Integer;
begin
  Result := GetTextLen;
end;

function TRichEdit1Provider.GetSelText: WideString;
var
  Length: Integer;
  st: string;
begin
  SetLength(st, Owner.SelLength + 1);
  Length := SendMessage(Owner.Handle, EM_GETSELTEXT, 0, Longint(PChar(st)));
  SetLength(st, Length);
  Result := StringToWideString (st, Owner.CodePage);
end;

(*----------------------------------------------------------------------*
 | function TRichEdit1Provider.GetText                                  |
 |                                                                      |
 | (v1) GetText                                                         |
 *----------------------------------------------------------------------*)
function TRichEdit1Provider.GetText: WideString;
var
  s: string;
  len: Integer;
begin
  len := GetWindowTextLength(Owner.Handle);
  SetLength(s, len);
  GetWindowText(Owner.Handle, PChar (s), len);
  Result := StringToWideString (s, Owner.CodePage);
end;

(*----------------------------------------------------------------------*
 | function TRichEdit1Provider.GetTextLen                               |
 |                                                                      |
 | (v1) Return the text's length                                        |
 *----------------------------------------------------------------------*)
function TRichEdit1Provider.GetTextLen: Integer;
begin
  Result := GetWindowTextLength(Owner.Handle)
end;

(*----------------------------------------------------------------------*
 | procedure TRichEdit1Provider.SetSelText                              |
 |                                                                      |
 | (v1)  Replace the selected text                                      |
 *----------------------------------------------------------------------*)
procedure TRichEdit1Provider.SetSelText(const value: WideString);
begin
  SendMessage(Owner.Handle, EM_REPLACESEL, 0, LongInt(PChar (WideStringToString (Value, Owner.CodePage))));
end;

(*----------------------------------------------------------------------*
 | procedure TRichEdit1Provider.SetText                                 |
 |                                                                      |
 | (v1)  SetText                                                        |
 *----------------------------------------------------------------------*)
procedure TRichEdit1Provider.SetText(const st: WideString);
begin
  SetWindowText(Owner.Handle, PChar (WideStringToString (st, Owner.CodePage)));
end;

{ TRichEditStream }

(*----------------------------------------------------------------------*
 | constructor TRichEditStream.Create                                   |
 |                                                                      |
 | Constructor for TRichEditStream                                      |
 *----------------------------------------------------------------------*)
constructor TRichEditStream.Create(AOwner: TCustomExRichEdit);
begin
  FOwner := AOwner;
end;

(*----------------------------------------------------------------------*
 | destructor TRichEditStream.Destroy                                   |
 |                                                                      |
 | Destructor for TRichEditStream                                       |
 *----------------------------------------------------------------------*)
destructor TRichEditStream.Destroy;
begin
  if FOwnsBuffer then
    ReallocMem (FBuffer, 0);

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TRichEditStream.GetWideString                              |
 |                                                                      |
 | Get the buffer's text chunk                                          |
 *----------------------------------------------------------------------*)
procedure TRichEditStream.GetString(var st: string);
begin
  SetLength(st, ChunkEnd - ChunkStart);
  Move((FBuffer + ChunkStart)^, PChar (st)^, (ChunkEnd - ChunkStart));
end;

procedure TRichEditStream.GetWideString(var ws: WideString);
begin
  SetLength(ws, ChunkEnd - ChunkStart);
  Move((FBuffer + ChunkStart)^, PWideChar (ws)^, (ChunkEnd - ChunkStart) * SizeOf(WideChar));
end;

{ TRichEdit2_3Provider }

(*----------------------------------------------------------------------*
 | function EditStreamICallback                                         |
 |                                                                      |
 | Callback function for EM_STREAMIN                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   dwCookie: DWORD           Instance of the TRichEditStream to get  |
 |                              data from                               |
 |                                                                      |
 |   pbBuff: pointer           The data buffer to fill                 |
 |   cb: DWORD;                The max length of data                  |
 |   var ret: DWORD            Length of data actually returned        |
 |                                                                      |
 | The function returns 0 if it the callback should continue steaming   |
 | in text - or <> 0 = an error code.                                   |
 *----------------------------------------------------------------------*)
function EditStreamICallback(dwCookie: DWORD; pbBuff: pointer; cb: DWORD; var ret: DWORD): DWORD; stdcall;
var
  stream: TRichEditStream;
begin
  stream := TRichEditStream (dwCookie);

  ret := (stream.FChunkEnd - stream.FChunkStart - stream.FPos) * SizeOf(WideChar);
  if ret > cb then
    ret := cb;

  Move((stream.FBuffer + stream.FChunkStart + stream.FPos)^, pbBuff^, ret);
  Inc(stream.FPos, ret div SizeOf(WideChar));
  Result := 0
end;

(*----------------------------------------------------------------------*
 | EditStreamIRTFCallback                                               |
 |                                                                      |
 | Callback fpr EM_SETSTREAM when it's in SF_RTF mode.  In this case we |
 | need to convert the unicode chars in the RichEditStream to ansi      |
 | chars.                                                               |
 |                                                                      |
 | Parameters:                                                          |
 |   dwCookie: DWORD; pbBuff: pointer; cb: DWORD; var ret: DWORD
 *----------------------------------------------------------------------*)
function EditStreamIRTFCallback(dwCookie: DWORD; pbBuff: pointer; cb: DWORD; var ret: DWORD): DWORD; stdcall;
var
  stream: TRichEditStream;
  i: Integer;
  p1: PChar;
  p: PWideChar;
begin
  stream := TRichEditStream (dwCookie);

  ret := (stream.FChunkEnd - stream.FChunkStart - stream.FPos);
  if ret > cb then
    ret := cb;

  if ret > 0 then
  begin
    p := PWideChar (stream.FBuffer);
    Inc(p, stream.FChunkStart + stream.FPos);

    p1 := PChar (pbBuff);

    for i := 0 to ret - 1 do
    begin
      p1^ := char (Integer (p^));
      Inc(p1);
      Inc(p);
      Inc(stream.FPos);
    end
  end;

  Result := 0
end;

(*----------------------------------------------------------------------*
 | function EditStreamOCallback                                         |
 |                                                                      |
 | Callback function for EM_STREAMOUT                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   dwCookie: DWORD;          Instance of the TRichEditStream to fill |
 |   pbBuff: pointer;          Pointer to the data  to use             |
 |   cb: DWORD;                Size of the data                        |
 |  var ret: DWORD             Bytes actually copied                   |
 |                                                                      |
 | The function returns <> 0 if the function failed                     |
 *----------------------------------------------------------------------*)
function EditStreamOCallback(dwCookie: DWORD; pbBuff: pointer; cb: DWORD; var ret: DWORD): DWORD; stdcall;
var
  stream: TRichEditStream;
begin
  stream := TRichEditStream (dwCookie);

  if cb > 0 then with stream do
  begin
    FLen := FPos * SizeOf(WideChar) + cb;
    ReallocMem (FBuffer, FLen);
    FOwnsBuffer := True;
    FLen := FLen div SizeOf(WideChar);
    Move(pbBuff^, (stream.FBuffer + stream.FPos)^, cb);
    Inc(FPos, cb div SizeOf(WideChar));
    ret := cb
  end;

  stream.FChunkEnd := stream.FPos;

  Result := 0
end;

function EditStreamOchCallback(dwCookie: DWORD; pbBuff: pointer; cb: DWORD; var ret: DWORD): DWORD; stdcall;
var
  stream: TRichEditStream;
begin
  stream := TRichEditStream (dwCookie);

  if cb > 0 then with stream do
  begin
    FLen := FPos + cb;
    ReallocMem (FBuffer, FLen);
    FOwnsBuffer := True;
    Move(pbBuff^, (stream.FBuffer + stream.FPos)^, cb);
    Inc(FPos, cb);
    ret := cb
  end;

  stream.FChunkEnd := stream.FPos;

  Result := 0
end;

{ TRichEdit2_3Provider }

(*----------------------------------------------------------------------*
 | procedure TRichEdit2_3Provider.CreateSubclass                        |
 |                                                                      |
 | Subclass for version 2 & 3 richedit                                  |
 *----------------------------------------------------------------------*)
procedure TRichEdit2_3Provider.CreateSubclass(var Params: TCreateParams);
begin
  if gRichEditModule = 0 then
    gRichEditModule := LoadLibrary('RICHED20.DLL');
  Owner.CreateSubClass (Params, 'RichEdit20A');
                // nb.
                //
                // RichEdit20W doesn't work in '98
end;

(*----------------------------------------------------------------------*
 | function TRichEdit2_3Provider.GetSelText                             |
 |                                                                      |
 | (v2&3) GetSelectedText                                               |
 *----------------------------------------------------------------------*)
destructor TRichEdit2_3Provider.Destroy;
begin
  FIRichEditOLE := nil;
  inherited;
end;

function TRichEdit2_3Provider.GetCRLFTextLen: Integer;
var
  ltx: TGetTextLengthEx;
begin
  ltx.flags := GTL_NUMCHARS or GTL_PRECISE or GTL_USECRLF;
  ltx.codepage := 1200; // Unicode
  Result := SendMessage(Owner.Handle, EM_GETTEXTLENGTHEX, LongInt(@ltx), 0);
end;

function TRichEdit2_3Provider.GetIRichEditOLE: IRichEditOLE;
begin
  if not FGotIRichEditOLE then
  begin
    SendMessage(Owner.Handle, EM_GETOLEINTERFACE, 0, Integer (@FIRichEditOLE));
    FGotIRichEditOLE := True
  end;
   Result := FIRichEditOLE
end;

function TRichEdit2_3Provider.GetSelText: WideString;
begin
  Result := RawGetText(True)
end;

(*----------------------------------------------------------------------*
 | function TRichEdit2_3Provider.GetText                                |
 |                                                                      |
 | (v2&3) GetText                                                       |
 *----------------------------------------------------------------------*)
function TRichEdit2_3Provider.GetText: WideString;
begin
  Result := RawGetText(False)
end;

(*----------------------------------------------------------------------*
 | function TRichEdit2_3Provider.GetTextLen                             |
 |                                                                      |
 | (v2&3) GettextLen                                                    |
 *----------------------------------------------------------------------*)
function TRichEdit2_3Provider.GetTextLen: Integer;
var
  ltx: TGetTextLengthEx;
begin
  ltx.flags := GTL_NUMCHARS or GTL_PRECISE;
  ltx.codepage := 1200; // Unicode
  Result := SendMessage(Owner.Handle, EM_GETTEXTLENGTHEX, LongInt(@ltx), 0);
end;

(*----------------------------------------------------------------------*
 | function TRichEdit2_3Provider.RawGetText                             |
 |                                                                      |
 | Get text - selected or otherwise                                     |
 *----------------------------------------------------------------------*)
function TRichEdit2_3Provider.RawGetText(sel: Boolean): WideString;
var
  editStream: TEditStream;
  stream: TRichEditStream;
  flags: DWORD;
  st: string;
begin
  flags := 0;
  if sel then
    flags := flags or SFF_SELECTION;
  if Owner.StreamRTF then
  begin
    flags := flags or SF_RTF;
    Owner.StreamRTF := False
  end
  else
    flags := flags or SF_TEXT or SF_UNICODE;
  stream := TRichEditStream.Create(Owner);
  try
    editStream.dwCookie := DWORD (stream);
    editStream.dwError := 0;
    if (flags and SF_UNICODE) <> 0 then
      editStream.pfnCallback := @EditStreamOCallback
    else
      editStream.pfnCallback := @EditStreamOchCallback;
    SendMessage(Owner.Handle, EM_STREAMOUT, flags, LongInt(@editStream));
    if (flags and SF_UNICODE) <> 0 then
      stream.GetWideString(Result)
    else
    begin
      stream.GetString (st);
      Result := st
    end
  finally
    stream.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRichEdit2_3Provider.RawSetText                            |
 |                                                                      |
 | Set the text - Raw or otherwise                                      |
 *----------------------------------------------------------------------*)
procedure TRichEdit2_3Provider.RawSetText(const st: WideString;
  sel: Boolean);
var
  editStream: TEditStream;
  stream: TRichEditStream;
  fc: TCharFormat;
  tc: TCharRange;
  flags: DWORD;
begin
  Owner.BeginUpdate;
  stream := TRichEditStream.Create(Owner);
  try
    stream.FBuffer := PWideChar (st);
    stream.FLen := Length(st);
    editStream.dwCookie := DWORD (stream);
    editStream.dwError := 0;
    editStream.pfnCallback := @EditStreamICallback;

    SendMessage(Owner.Handle, EM_EXGETSEL, 0, Integer (@tc));

    if not sel then
    begin
      Owner.SelStart := 0;
      Owner.SelLength := -1
    end;
    fc := Owner.DefaultCharFormat;

    Owner.CharFormatter.Reset;

    flags := SFF_SELECTION;
    if Owner.StreamRTF then
    begin
      flags := flags or SF_RTF;
      Owner.StreamRTF := False;
      editStream.pfnCallback := @EditStreamIRTFCallback;
    end
    else
      flags := flags or SF_TEXT or SF_UNICODE;


    while stream.ChunkEnd < stream.Len do
    begin
      Owner.CharFormatter.GetFormattedChars(stream, fc);

      if fc.dwMask <> 0 then
        SendMessage(Owner.Handle, EM_SETCHARFORMAT, SCF_SELECTION, LongInt(@fc));

      stream.FPos := 0;
      SendMessage(Owner.Handle, EM_STREAMIN, flags, LongInt(@editStream));

      stream.FChunkStart := stream.FChunkEnd;
      if stream.Truncate then
        break;
    end;

    SendMessage(Owner.Handle, EM_EXSETSEL, 0, Integer (@tc));

  finally
    Owner.EndUpdate;
    stream.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRichEdit2_3Provider.SetSelText                            |
 |                                                                      |
 | (v2&3)  Set Selected Text                                            |
 *----------------------------------------------------------------------*)
procedure TRichEdit2_3Provider.SetSelText(const value: WideString);
begin
  RawSetText(value, True);
  Owner.Invalidate
end;

(*----------------------------------------------------------------------*
 | procedure TRichEdit2_3Provider.SetText                               |
 |                                                                      |
 | (v2&3)  Set Text                                                     |
 *----------------------------------------------------------------------*)
procedure TRichEdit2_3Provider.SetText(const st: WideString);
begin
  RawSetText(st, False);
  Owner.Invalidate;
end;

{ TCharFormatter }

(*----------------------------------------------------------------------*
 | procedure TCharFormatter.GetFormattedChars                           |
 |                                                                      |
 | Make the streams chunk return the whole stream.                      |
 *----------------------------------------------------------------------*)
procedure TCharFormatter.GetFormattedChars (stream: TRichEditStream; var fc: TCharFormat);
begin
  stream.FChunkStart := 0;
  stream.FChunkEnd := stream.FLen;
end;

function TCustomExRichEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action);

  if not Result then
  begin
    Result := Action is TEditCopy;
    if Result then
    begin
      CopyToClipboard;
      Exit
    end;

    Result := Action is TEditSelectAll;
    if Result then
    begin
      SelectAll;
      Exit
    end
  end
end;

procedure TCustomExRichEdit.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
  swallow: Boolean;
begin

  swallow := False;
  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);
    if (message.CharCode = VK_RETURN) and (ShiftState <> []) then
      swallow := True;

    // nb.
    //
    // Rich Edit controls don't get a WM_PASTE when you press Shift-Insert,
    // so we have to trap the key sequence directly

    if FRawPaste and ((message.CharCode = VK_INSERT) and (ssShift in ShiftState)) or
                     ((message.CharCode = Ord ('v')) and (ssCtrl in ShiftState)) then
    begin
      PasteTextFromClipboard;
      exit
    end
  end;

  if not swallow then
    inherited
  else
    SendMessage(parent.Handle, WM_KEYDOWN, message.CharCode, message.KeyData)
end;

procedure TCustomExRichEdit.SetRightMargin(const Value: Integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Value;
    SetupRightMargin
  end
end;

procedure TCustomExRichEdit.SetupRightMargin;
var
  rm: Integer;
begin
  if FAveCharWidth <> 0 then
  begin
    if (FRightMargin = 0) or not FixedFont then
      rm := 0
    else
    begin
      rm := ClientWidth - AveCharWidth * (FRightMargin + 1);

      if rm < 0 then
        rm := 0
    end;

    SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong (0, rm))
  end
end;

procedure TCustomExRichEdit.WmGetText(var Message: TWMGetText);
var
  txt: string;
begin
  if fInGetText then
    inherited
  else
  begin
    fInGetText := True;
    try
      if HandleAllocated then
        txt := WideStringToString (GetText, FCodePage)
      else
        text := WindowText;

      with Message do
        Result := StrLen(StrLCopy(PChar(Text), PChar(txt), TextMax - 1))
    finally
      fInGetText := False
    end
  end
end;

procedure TCustomExRichEdit.WmGetTextLength(var Message: TWMGetTextLength);
begin
  if not (csDestroying in ComponentState) and HandleAllocated then
    Message.Result := GetCRLFTextLen
  else
    Message.Result := Length(WindowText);
end;

procedure TCustomExRichEdit.WmSetText(var Message: TWMSetText);
var
  txt: string;
begin
  if fINSetText then
    inherited
  else
  begin
    fInSetText := True;
    try
      txt := Message.Text;
      SetText(StringToWideString (txt, FCodePage));
      Message.Result := 1
    finally
      fInSetText := False
    end
  end
end;

function TCustomExRichEdit.GetCodePage: Integer;
begin
  if FCodePage = -1 then
    Result := CharsetToCodePage(Font.Charset)
  else
    Result := FCodePage;
end;

procedure TCustomExRichEdit.CMRecreateWnd(var Message: TMessage);
var
  ss, sl: Integer;
begin
  ss := selStart;
  sl := selLength;
  inherited;
  selStart := ss;
  selLength := sl
end;

procedure TCustomExRichEdit.ClearUndoBuffer;
begin
  if HandleAllocated then
    SendMessage(Handle, EM_EMPTYUNDOBUFFER, 0, 0);
end;

function TCustomExRichEdit.GetCanRedo: Boolean;
begin
  Result := False;
  if HandleAllocated then Result := SendMessage(Handle, EM_CANREDO, 0, 0) <> 0;
end;

procedure TCustomExRichEdit.Redo;
begin
  SendMessage(Handle, EM_REDO, 0, 0);
end;

function TCustomExRichEdit.GetCRLFTextLen: Integer;
begin
  Result := FRichEditProvider.GetCRLFTextLen
end;

procedure TCustomExRichEdit.Print;
var
  Range: TFormatRange;
  LastChar, MaxLen, LogX, LogY, OldMap: Integer;
  SaveRect: TRect;
begin
  FillChar(Range, SizeOf(TFormatRange), 0);
  with Printer, Range do
  begin
    hdc := Handle;
    hdcTarget := hdc;
    LogX := GetDeviceCaps(Handle, LOGPIXELSX);
    LogY := GetDeviceCaps(Handle, LOGPIXELSY);
    if IsRectEmpty(PageRect) then
    begin
      rc.right := PageWidth * 1440 div LogX;
      rc.bottom := PageHeight * 1440 div LogY;
    end
    else begin
      rc.left := PageRect.Left * 1440 div LogX;
      rc.top := PageRect.Top * 1440 div LogY;
      rc.right := PageRect.Right * 1440 div LogX;
      rc.bottom := PageRect.Bottom * 1440 div LogY;
    end;
    rcPage := rc;
    SaveRect := rc;
    LastChar := 0;
    MaxLen := GetTextLen;
    chrg.cpMax := -1;
    // ensure printer DC is in text map mode
    OldMap := SetMapMode(hdc, MM_TEXT);
    SendMessage(Self.Handle, EM_FORMATRANGE, 0, 0);    // flush buffer
    try
      repeat
        rc := SaveRect;
        chrg.cpMin := LastChar;
        LastChar := SendMessage(Self.Handle, EM_FORMATRANGE, 1, Longint(@Range));
        if (LastChar < MaxLen) and (LastChar <> -1) then NewPage;
      until (LastChar >= MaxLen) or (LastChar = -1);
    finally
      SendMessage(Self.Handle, EM_FORMATRANGE, 0, 0);  // flush buffer
      SetMapMode(hdc, OldMap);       // restore previous map mode
    end;
  end;
end;

procedure TCustomExRichEdit.SetMultiLine(const Value: Boolean);
begin
  if Value <> FMultiLine then
  begin
    FMultiLine := Value;
    RecreateWnd
  end
end;

function TCustomExRichEdit.GetUsesCRLF: Boolean;
begin
  Result := FRichEditProvider.UsesCRLF
end;

function TRichEdit1Provider.UsesCRLF: Boolean;
begin
  Result := True
end;

function TRichEdit2_3Provider.UsesCRLF: Boolean;
begin
  Result := False
end;

function AdjustForCRLF (ss: Integer; const txt: WideString): Integer;
var
  i: Integer;
begin
  Result := ss;
  for i := 0 to ss - 1 do
   if txt [i+1] = #10 then
     Dec(Result)
end;

function UnAdjustForCRLF (ss: Integer; const txt: WideString): Integer;
var
  i: Integer;
begin
  i := 0;
  while i < ss do
  begin
    if txt [i+1] = #13 then
      Inc(ss);

    Inc(i)
  end;
  Result := ss
end;

procedure TCustomExRichEdit.SetRawSelection(ss, se: Integer);
var
  sl: Integer;
begin
  Dec(ss);
  if se = -1 then
    sl := -1
  else
    sl := se - ss;

  if not UsesCRLF then
    ss := AdjustForCRLF (ss, Text);

  SelStart := ss;
  SelLength := sl
end;

procedure TCustomExRichEdit.GetRawSelection(var ss, se: Integer);
begin
  ss := SelStart;

  if (ss > 0) and not UsesCRLF then
    ss := UnAdjustForCRLF (ss, Text);

  Inc(ss);
  se := ss + SelLength - 1
end;

procedure TCharFormatter.Reset;
begin
// Stub
end;

function TRichEditProvider.GetIRichEditOLE: IRichEditOLE;
begin
  Result := Nil
end;

function TCustomExRichEdit.GetIRichEditOLE: IRichEditOLE;
begin
  Result := FRichEditProvider.GetIRichEditOLE
end;

function TCustomExRichEdit.GetLineCount: Integer;
begin
  Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
end;

procedure TCustomExRichEdit.PasteTextFromClipboard;
begin
  if FRawPaste then
    if Clipboard.HasFormat(CF_UNICODETEXT) then
      SendMessage(Handle, EM_PASTESPECIAL, CF_UNICODETEXT, 0)
    else
      SendMessage(Handle, EM_PASTESPECIAL, CF_TEXT, 0)
  else
    PasteFromClipboard
end;

initialization

finalization
  if gRichEditModule <> 0 then
    FreeLibrary(gRichEditModule);
end.
