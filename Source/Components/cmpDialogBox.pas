(*======================================================================*
 | DialogBox                                                            |
 |                                                                      |
 | Display a dialog box from a dialog resource Template                 |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      30/05/2001  CPWW  Original                                  |
 *======================================================================*)

unit cmpDialogBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CommCtrl, RichEdit, DialogConsts, DialogStrings, Menus;

type
  TOnDlgMessage = procedure(Sender: TObject; var Msg: TMessage; bcontinue: Boolean) of object;
  TOnGetControlImage = procedure(Sender: TObject; Tp: Integer; const Id: string; var Handle: HGDIOBJ) of object;

//-----------------------------------------------------------------------
// The DialogBox class

  TDialogBox = class(TWinControl)
  private
    FResourceTemplate: Pointer;
    FExtendedTemplate: Boolean;

    FHwndDlg: HWND;
    FHFontDlg: HFONT;
    FOnDlgMessage: TOnDlgMessage;

    FBaseUnitX, FBaseUnitY: Double;
    FMargin: Integer;
    FWidthAdjust: Integer;
    FHeightAdjust: Integer;
    FOnShow: TNotifyEvent;
    FOnGetControlImage: TOnGetControlImage;

    FOrigStyle: DWORD;
    FMenu: TMenuItem;

    procedure SetResourceTemplate(const Value: Pointer);
    procedure WmDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure InitDialogControls;

  protected
    FOrigX, FOrigY: Integer;
    FInitializing: Boolean;
    procedure HandleDlgMessage(var Msg: TMessage); virtual;
    procedure PaintWindow(DC: HDC); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    property WidthAdjust: Integer read FWidthAdjust write FWidthAdjust;
    property HeightAdjust: Integer read FHeightAdjust write FHeightAdjust;
    procedure InitDlg(Template: Pointer; const FontName: string; FontPoints, FontWeight, FontCharset: Integer; FontItalic: Boolean; const Menu, Cls: TSzOrID); virtual;
    procedure InitCtrl(n: Integer; Template: Pointer; ExtraCount: Integer;
      ExtraData: PWideChar; TitleSzOrID: TSzOrID); virtual;

    property ExtendedTemplate: Boolean read FExtendedTemplate;

    property OrigStyle: DWORD read FOrigStyle write FOrigStyle;
    property OrigX: Integer read FOrigX write FOrigX;
    property OrigY: Integer read FOrigY write FOrigY;

    function PointToDialogPoint(pt: TPoint): TPoint;
    function DialogPointtoPoint(pt: TPoint): TPoint;
    function RectToDialogRect(r: TRect): TRect;
    function DialogRectToRect(r: TRect): TRect;

    property FontHandle: HFONT read FHFontDlg; // Must be read only!  Don't even think about it!
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Margin: Integer read FMargin write FMargin;
    property ResourceTemplate: Pointer read FResourceTemplate write SetResourceTemplate;

    property DialogHandle: HWND read FHwndDlg;
    procedure SetCtrlImage(HwndCtrl: HWND; IsBtn: Boolean; Tp: Integer; Handle: HGDIOBJ);
    procedure GetImageType(const ctrlClass: TSZOrID; Style: DWORD; var IsBtn: Boolean; var Tp: Integer);
    procedure SetCtrlImage1(HwndCtrl: HWND; IsBtn: Boolean; Tp: Integer; Id: TszOrID);

  published
    property OnDlgMessage: TOnDlgMessage read FOnDlgMessage write FOnDlgMessage;
    property AutoSize default True;
    property Color default clBtnFace;
    property OnKeyDown;
    property ParentColor;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnGetControlImage: TOnGetControlImage read FOnGetControlImage write FOnGetControlImage;
  end;

var
  gWndAtom: TAtom;

procedure GetSzOrID(var p: PChar; var szOrID: TSzOrID);

implementation

var
  gTmpFont: HFONT = 0;
  FRichEditModule: THandle;

(*----------------------------------------------------------------------*
 | DialogProc                                                           |
 |                                                                      |
 | Handle dialog messages.                                              |
 |                                                                      |
 | When we receive WM_INITDIALOG, set up the thunk to the TDialogBox    |
 | instance using SetProp.  Once we've got the TDialogBox thunk, we can |
 | call TDialogBox.HandleDlgMessage to do the work.                     |
 |                                                                      |
 | The only snag is that dialogs can get sent a WM_SETFONT message,     |
 | even before the WM_INITDIALOG.  So we save the font handle in        |
 | gTmpFont, then send on the WM_SETFONT after we've received           |
 | WM_INITDIALOG.  Sorry about the global, but heaven help you if       |
 | try to simultaneously create two dialogs from diffent threads!       |
 *----------------------------------------------------------------------*)
function DialogProc(hwndDlg: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
var
  dlgInstance: TDialogBox;
  Msg: TMessage;

begin
  result := False;
  if uMsg = WM_INITDIALOG then
  begin
    dlgInstance := TDialogBox(lParam);                 // Get the TDialogBox instance from lparam
    SetProp(hwndDlg, PChar(gWndAtom), lParam);        // ... and save it in the gWndAtom property
    dlgInstance.FHwndDlg := hwndDlg;

    if gTmpFont <> 0 then                               // Now do delayed WM_SETFONT.
    begin
      SendMessage(hwndDlg, WM_SETFONT, gTmpFont, 0);
      gTmpFont := 0
    end
  end
  else                                                  // Look up TDialogBox in prop.

    dlgInstance := TDialogBox(GetProp(hwndDlg, PChar(gWndAtom)));

  if Assigned(dlgInstance) then
  begin
    Msg.Msg := uMsg;
    Msg.WParam := wParam;
    Msg.LParam := lParam;
    Msg.Result := 0;

    dlgInstance.HandleDlgMessage(Msg);
    result := Bool(Msg.Result);

    if uMsg = WM_DESTROY then                           // WM_DESTROY - remove the prop, otherwise
    begin                                               // the window won't be destroyed!
      SetParent(dlgInstance.FHwndDlg, 0);
      dlgInstance.FHwndDlg := 0;
      RemoveProp(hwndDlg, PChar(gWndAtom));
      result := True;
    end
  end
  else                       // No TDialogBox - implies we haven't yet received WM_INITDIALOG
                             // The only message we accept at this stage is WM_SETFONT...
    if uMsg = WM_SETFONT then
      gTmpFont := wParam
end;

(*----------------------------------------------------------------------*
 | GetSzOrID                                                            |
 |                                                                      |
 | Get a 'string or ID' from a class, Menu, title dialog Template value |
 | Pointer.                                                             |
 *----------------------------------------------------------------------*)
procedure GetSzOrID(var p: PChar; var szOrID: TSzOrID);
begin
  if(PWord(p)^ = $ffff) then
  begin
    Inc(p, SizeOf(Word));
    szOrID.isID := True;
    szOrId.Id := PWord(p)^;
    szOrId.sz := '';
    Inc(p, SizeOf(Word));
  end
  else
  begin
    szOrId.isID := False;
    szOrId.sz := PWideChar(p);
    szOrId.Id := 0;
    Inc(p, SizeOf(WideChar) *(Length(szOrId.sz) + 1))
  end
end;

{ TDialogBox }

(*----------------------------------------------------------------------*
 | TDialogBox.CanAutoSize                                               |
 |                                                                      |
 | Override 'CanAutoSize' to allow size for the margin.  The margin is  |
 | important an TDialogEditor, as it holds the resizer control for the  |
 | dialog itself.                                                       |
 *----------------------------------------------------------------------*)
function TDialogBox.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  r: TRect;
  w, h: Integer;
begin
  Result := True;
  if not(csDesigning in ComponentState) then
  begin
    if FHwndDlg <> 0 then
      GetWindowRect(FHwndDlg, r)
    else
      FillChar(r, SizeOf(r), 0);

    w := r.Right - r.Left + 1;
    h := r.Bottom - r.Top + 1;

    if Align in [alNone, alLeft, alRight] then
      NewWidth := w + 2 * Margin + WidthAdjust;

    if Align in [alNone, alTop, alBottom] then
      NewHeight := h + 2 * Margin + HeightAdjust;
  end
end;

(*----------------------------------------------------------------------*
 | TDialogBox.Create                                                    |
 |                                                                      |
 | Constructor.  Set property defaults.                                 |
 *----------------------------------------------------------------------*)
constructor TDialogBox.Create(AOwner: TComponent);
begin
  inherited;
  width := 180;
  height := 120;
  Color := clBtnFace;
  AutoSize := True;
end;

(*----------------------------------------------------------------------*
 | TDialogBox.DialogPointToPoint                                        |
 |                                                                      |
 | Convert a point from dialog units to pixels                          |
 *----------------------------------------------------------------------*)
destructor TDialogBox.Destroy;
begin
  FMenu.Free;

  inherited;
end;

function TDialogBox.DialogPointToPoint(pt: TPoint): TPoint;
begin
  if fBaseUnitX = 0 then
    result := pt
  else
  begin
    result.x := Round(pt.x * fBaseUnitX / 4);
    result.y := Round(pt.y * fBaseUnitY / 8)
  end
end;

(*----------------------------------------------------------------------*
 | TDialogBox.DialogRectToRect                                          |
 |                                                                      |
 | Convert a rect from dialog units to pixels.                          |
 *----------------------------------------------------------------------*)
function TDialogBox.DialogRectToRect(r: TRect): TRect;
begin
  MapDialogRect(DialogHandle, r);
  result := r;
end;


(*----------------------------------------------------------------------*
 | TDialogBox.HandleDlgMessage                                          |
 |                                                                      |
 | HAndle dialog messages.                                              |
 *----------------------------------------------------------------------*)
procedure TDialogBox.GetImageType(const ctrlClass: TSZOrID; Style: DWORD; var IsBtn: Boolean; var Tp: Integer);
begin
  IsBtn := False;
  Tp := -1;

  if ctrlClass.Id = BUTTON_ID then
  begin
    IsBtn := True;
    if(Style and BS_ICON) <> 0 then
      Tp := IMAGE_ICON
    else
      if(Style and BS_BITMAP) <> 0 then
        Tp := IMAGE_BITMAP
  end
  else
    if ctrlClass.Id = STATIC_ID then
    case Style and SS_TYPEMASK of
      SS_ICON: Tp := IMAGE_ICON;
      SS_BITMAP: Tp := IMAGE_BITMAP;
      SS_ENHMETAFILE: Tp := IMAGE_ENHMETAFILE
    end;
end;

procedure TDialogBox.HandleDlgMessage(var Msg: TMessage);
var
  p: PWindowPos;
  continueProcessing: Boolean;
  r: TRect;

  procedure GetFontBaseUnits(var baseX, baseY: Double);
  var
    r: TRect;
  begin
    r := Rect(0, 0, 4, 8);
    MapDialogRect(FHwndDlg, r);
    baseX := r.Right;
    baseY := r.Bottom;
  end;

begin
  ContinueProcessing := True;
  if Assigned(FOnDlgMessage) and not(csDestroying in ComponentState) then
  begin
    Msg.result := Ord(False);
    OnDlgMessage(self, Msg, ContinueProcessing)
  end;

  if continueProcessing then
  begin
    Msg.result := Ord(True);
    case Msg.Msg of
      WM_INITDIALOG :
        begin           // Set the control bounds to the size of the dialog box
          FInitializing := True;
          try
            GetWindowRect(FHwndDlg, r);
            MapWindowPoints(HWND_DESKTOP, Parent.Handle, r, 2);
            with r do SetBounds(Self.Left, self.Top, right - left, bottom - top);
            GetFontBaseUnits(fBaseUnitX, fBaseUnitY)
          finally
            FInitializing := False
          end
        end;

      WM_SETFONT:      // Save the font so we can use it for(eg) newly created
                        // controls
        begin
          FHFontDlg := Msg.wParam;
          Msg.Result := Ord(False)
        end;

      WM_CLOSE:        // Close clicked(etc.)  Destroy the dialog
        DestroyWindow(FHwndDlg);

                        // Activate the dialog if it's clicked
      WM_LBUTTONDOWN,
      WM_RBUTTONDOWN,
      WM_NCLBUTTONDOWN,
      WM_NCRBUTTONDOWN :
        begin
          BringWindowToTop(Handle);
          SetActiveWindow(Handle);
          Msg.result := Ord(False)
        end;

                        // Don't allow moving the dialog
      WM_WINDOWPOSCHANGING :
      begin
        if not FInitializing then
        begin
          p := PWindowPos(Msg.LParam);
          p.Flags := p.Flags or SWP_NOMOVE
        end
      end;

      else
        Msg.result := Ord(FALSE);
    end
  end
end;


(*----------------------------------------------------------------------*
 | TDialogBox.InitCtrl                                                  |
 |                                                                      |
 | Called for each control.  Override it to cache control info.         |
 *----------------------------------------------------------------------*)
procedure TDialogBox.InitCtrl(n: Integer; Template: Pointer;
  ExtraCount: Integer; ExtraData: PWideChar; TitleSzOrID: TSzOrID);
begin
  // stub
end;

(*----------------------------------------------------------------------*
 | TDialogBox.InitDialogControls                                        |
 |                                                                      |
 | Call InitDlg to cache dialog settings, then call InitCtrl for each   |
 | control.  Also set control images, etc.                              |
 *----------------------------------------------------------------------*)
procedure TDialogBox.InitDialogControls;
type
  pbytebool = ^ByteBool;
var
  Template: PDlgTemplate;
  ItemTemplate: PdlgItemTemplate;
  ExTemplate: PDlgTemplateEx;
  ExItemTemplate: PDlgItemTemplateEx;
  p: PWideChar;
  tempSzOrID, dlgMenu, ctrlClass, ctrlTitle: TSzOrID;
  i, ctrlCount, Style, Id, fontPoint, FontWeight, FontCharset: Integer;
  FontItalic: Boolean;
  ExtraCount: Word;
  Tp: Integer;
  gdiobj: HGDIOBJ;
  szId, FontName: string;
  IsBtn: Boolean;
begin
  Template := PDlgTemplate(FResourceTemplate);
  p := PWideChar(FResourceTemplate);
  if ExtendedTemplate then
  begin
    ExTemplate := PDlgTemplateEx(FResourceTemplate);
    Inc(p, SizeOf(TDlgTemplateEx));
    ctrlCount := ExTemplate^.cDlgItems;
    Style := ExTemplate^.Style;
  end
  else
  begin
    ExTemplate := nil;
    Inc(p, SizeOf(TDlgTemplate));
    ctrlCount := Template^.cdit;
    Style := Template^.style
  end;

  GetSzOrID(p, dlgMenu);  // Menu
  GetSzOrID(p, ctrlClass);  // class
  GetSzOrID(p, tempSzOrID);  // title

  FontName := '';
  FontCharset := ANSI_CHARSET;
  fontPoint := 8;
  FontWeight := FW_NORMAL;
  FontItalic := False;

  if(Style and DS_SETFONT) <> 0 then
  begin
    fontPoint := PWORD(p)^;
    Inc(p, SizeOf(Word));     // pointsize

    if ExtendedTemplate then
    begin
      FontWeight := PWORD(p)^;
      Inc(p, SizeOf(Word));   // weight

      FontItalic := PBYTEBOOL(p)^;
      Inc(p);                  // italic

      FontCharset := PBYTE(p)^;
      Inc(p);                  // Italic
    end;

    GetSzOrId(p, tempSzOrID);   // Typeface
    FontName := tempSzOrID.sz;
  end;

  if ExtendedTemplate then
    InitDlg(ExTemplate, FontName, fontPoint, FontWeight, FontCharset, FontItalic, dlgMenu, ctrlClass)
  else
    InitDlg(Template, FontName, fontPoint, FontWeight, FontCharset, FontItalic, dlgMenu, ctrlClass);

  if Assigned(OnGetControlImage) and(not dlgMenu.isID or(dlgMenu.Id > 0)) then
  begin
    if dlgMenu.isID then
      szId := IntToStr(dlgMenu.Id)
    else
      szId := dlgMenu.sz;

    gdiObj := 0;
    OnGetControlImage(Self, -1, szId, gdiobj);
    FreeAndNil(FMenu);
    FMenu := TMenuItem(gdiObj);
  end;

  for i := 0 to ctrlCount - 1 do
  begin
    p := PChar((Integer(p) + 3) div 4 * 4);  // Align on DWORD

    if ExtendedTemplate then
    begin
      ExItemTemplate := PDlgItemTemplateEx(p);
      ItemTemplate := nil;
      Inc(p, SizeOf(TDlgItemTemplateEx));
      p := PChar((Integer(p) + 3) div 4 * 4);  // Align on DWORD
      Style := ExItemTemplate^.Style;
      Id := ExItemTemplate^.Id
    end
    else
    begin
      ItemTemplate := PDlgITemTemplate(p);
      ExItemTemplate := nil;
      Inc(p, SizeOf(TDlgItemTemplate));
      Style := ItemTemplate^.Style;
      Id := ItemTemplate^.Id
    end;

    GetSzOrID(p, ctrlClass);     // control class
    GetSzOrID(p, ctrlTitle);     // title

    ExtraCount := PWord(p)^;
    Inc(p, SizeOf(Word));

    Inc(p, ExtraCount);

    GetImageType(ctrlClass, Style, IsBtn, Tp);

    if Tp <> -1 then
      SetCtrlImage1(GetDlgItem(FHwndDlg, Id), IsBtn, Tp, ctrlTitle);

    if ExtendedTemplate then
      InitCtrl(i, ExItemTemplate, ExtraCount, p, ctrlTitle)
    else
      InitCtrl(i, ItemTemplate, ExtraCount, p, ctrlTitle)
  end
end;

procedure TDialogBox.InitDlg(Template: Pointer; const FontName: string; FontPoints, FontWeight, FontCharset: Integer; FontItalic: Boolean; const Menu, Cls: TSzOrID);
begin
// stub
end;

(*----------------------------------------------------------------------*
 | TDialogBox.PaintWindow                                               |
 |                                                                      |
 | If we're designing, draw an edge to show where the dialog box is     |
 | going to go.                                                         |
 *----------------------------------------------------------------------*)
procedure TDialogBox.PaintWindow(DC: HDC);
var
  r: TRect;
begin
  inherited;
  if(csDesigning in ComponentState) then
  begin
    r := ClientRect;
    InflateRect(r, -2, -2);
    DrawEdge(DC, r, EDGE_RAISED, BF_RECT)
  end
end;

(*----------------------------------------------------------------------*
 | TDialogBox.PointToDialogPoint                                        |
 |                                                                      |
 | Convert a point from pixels to dialog units.                         |
 *----------------------------------------------------------------------*)
function TDialogBox.PointToDialogPoint(pt: TPoint): TPoint;
begin
  if fBaseUnitX = 0 then
    result := pt
  else
  begin
    result.x := Round(pt.x * 4 / fBaseUnitX);
    result.y := Round(pt.y * 8 / fBaseUnitY)
  end
end;

(*----------------------------------------------------------------------*
 | TDialogBox.RectToDialogRect                                          |
 |                                                                      |
 | Convert a rect from pixels to dialog units.                          |
 *----------------------------------------------------------------------*)
function TDialogBox.RectToDialogRect(r: TRect): TRect;
begin
  if fBaseUnitX = 0 then
    result := r
  else
  begin
    result.Left := Round(r.Left * 4 / fBaseUnitX);
    result.Top := Round(r.Top * 8 / fBaseUnitY);
    result.Right := Round(r.Right * 4 / fBaseUnitX);
    result.Bottom := Round(r.Bottom * 8 / fBaseUnitY)
  end
end;

(*----------------------------------------------------------------------*
 | TDialogBox.SetBounds                                                 |
 |                                                                      |
 | TDialogBox has moved.  Move the dialog too.                          |
 *----------------------------------------------------------------------*)
procedure TDialogBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FHwndDlg <> 0 then
    SetWindowPos(FHwndDlg, 0, Margin, Margin, AWidth, AHeight, SWP_NOZORDER);

  inherited;
end;

(*----------------------------------------------------------------------*
 | TDialogBox.SetCtrlImage                                              |
 |                                                                      |
 | Set a static or button control's image.                              |
 *----------------------------------------------------------------------*)
procedure TDialogBox.SetCtrlImage(HwndCtrl: HWND; IsBtn: Boolean;
  Tp: Integer; Handle: HGDIOBJ);
begin
  if Handle = 0 then
    if Tp = IMAGE_ICON then
      Handle := LoadIcon(0, IDI_WINLOGO)
    else
      Handle := LoadBitmap(HInstance, 'PREVIEW');

  if IsBtn then
    SendMessage(HwndCtrl, BM_SETIMAGE, Tp, Handle)
  else
    SendMessage(HwndCtrl, STM_SETIMAGE, Tp, Handle)
end;

(*----------------------------------------------------------------------*
 | TDialogBox.SetResourceTemplate                                       |
 |                                                                      |
 | Change the resource Template.  Remove the old dialog(if any), and   |
 | show the new one instead.                                            |
 *----------------------------------------------------------------------*)
procedure TDialogBox.SetCtrlImage1(HwndCtrl: HWND; IsBtn: Boolean;
  Tp: Integer; Id: TszOrID);
var
  gdiObj: HGDIOBJ;
  szId: string;
  pszId: PWideChar;
begin
  if Tp <> -1 then
  begin
    gdiobj := 0;
    if Assigned(OnGetControlImage) then
    begin
      if Id.isID then
        szId := IntToStr(Id.Id)
      else
        szId := Id.sz;
      OnGetControlImage(Self, Tp, szId, gdiobj)
    end
    else
    begin
      if Id.isID then
        pszId := PWideChar(Id.Id)
      else
        pszId := PWideChar(Id.sz);

      gdiobj := LoadImageW(hInstance, pszId, Tp, 0, 0, 0);
    end;

    SetCtrlImage(HwndCtrl, IsBtn, Tp, gdiObj)
  end
end;

procedure TDialogBox.SetResourceTemplate(const Value: Pointer);
var
  s: TMemoryStream;

  //-------------------------------------------------------------------------
  // Create a extended resource Template from the original one, but with the
  // appropriate style, no Menu, etc.

  function CreateAdjustedResourceTemplate: TMemoryStream;
  var
    Template: PDlgTemplate;
    ExTemplate: PDlgTemplateEx;
    newTemplate: TDlgTemplateEx;
    p: PChar;
    szOrId: TSzOrId;
    w: Word;
    b: Byte;
    i: Integer;
    newItemTemplate: TDlgItemTemplateEx;
    ItemTemplate: PDlgItemTemplate;
    ExItemTemplate: PDlgItemTemplateEx;

  begin
    Result := TMemoryStream.Create;
    try
      Template := PDlgTemplate(FResourceTemplate);
      if HiWord(Template^.Style) = $ffff then
      begin
        FExtendedTemplate := True;
        ExTemplate := PDlgTemplateEx(FResourceTemplate);
        FOrigStyle := ExTemplate^.Style;
        FOrigX := ExTemplate^.x;
        FOrigY := ExTemplate^.y;

        newTemplate := ExTemplate^;
        newTemplate.style :=(ExTemplate^.style and not(WS_POPUP or DS_CENTER or DS_CENTERMOUSE or DS_ABSALIGN)) or WS_CHILD or WS_VISIBLE;
        newTemplate.x := 0;
        newTemplate.y := 0;
        p := PChar(FResourceTemplate) + SizeOf(TDlgTemplateEx);
      end
      else
      begin
        FExtendedTemplate := False;
        FOrigStyle := Template^.Style;
        FOrigX := Template^.x;
        FOrigY := Template^.y;

        newTemplate.dlgVer := 1;
        newTemplate.signature := $ffff;
        newTemplate.helpID := 0;
        newTemplate.exStyle := Template^.dwExtendedStyle;
        newTemplate.Style :=(Template^.Style and not(WS_POPUP or DS_CENTER or DS_CENTERMOUSE or DS_ABSALIGN)) or WS_CHILD or WS_VISIBLE;
        newTemplate.cDlgItems := Template^.cdit;
        newTemplate.x := 0;
        newTemplate.y := 0;
        newTemplate.cx := Template^.cx;
        newTemplate.cy := Template^.cy;
        p := PChar(FResourceTemplate) + SizeOf(TDlgTemplate);
      end;

      Result.Write(newTemplate, SizeOf(newTemplate));

      GetSzOrID(p, szOrId);      // Menu
      szOrId.isID := False;         // Get rid of the Menu!
      szOrID.sz := '';
      WriteSzOrId(Result, szOrId);

      GetSzOrID(p, szOrID);      // class
      WriteSzOrID(Result, szOrID);

      GetSzOrID(p, szOrID);      // title
      WriteSzOrID(Result, szOrID);

      if(FOrigStyle and DS_SETFONT) <> 0 then
      begin
                                  // Font point
        Result.Write(PWord(p)^, SizeOf(Word));
        Inc(p, SizeOf(Word));

        if FExtendedTemplate then
        begin
          Result.Write(PWord(p)^, SizeOf(Word));
          Inc(p, SizeOf(Word)); // Font weight

          Result.Write(PByte(p)^, SizeOf(Byte));
          Inc(p, SizeOf(Byte)); // Font italic

          Result.Write(PByte(p)^, SizeOf(Byte));
          Inc(p, SizeOf(Byte)); // Font charset
        end
        else
        begin
          w := FW_NORMAL;
          Result.Write(w, SizeOf(w));

          b := 0;
          Result.Write(b, SizeOf(b));

          b := ANSI_CHARSET;
          Result.Write(b, SizeOf(b))
        end;

        GetSzOrID(p, szOrID);
        WriteSzOrID(Result, szOrID);
      end;

      for i := 0 to newTemplate.cDlgItems - 1 do
      begin
        pad(Result);

        p := PChar((Integer(p) + 3) div 4 * 4);  // Align on DWORD

        if ExtendedTemplate then
        begin
          ExItemTemplate := PDlgItemTemplateEx(p);
          Inc(p, SizeOf(TDlgItemTemplateEx));
          p := PChar((Integer(p) + 3) div 4 * 4);  // Align on DWORD

          newItemTemplate := ExItemTemplate^;
        end
        else
        begin
          ItemTemplate := PDlgITemTemplate(p);
          Inc(p, SizeOf(TDlgItemTemplate));

          newItemTemplate.helpID := 0;
          newItemTemplate.exStyle := ItemTemplate^.dwExtendedStyle;
          newItemTemplate.Style := ITemTemplate^.Style;
          newItemTemplate.x := ItemTemplate^.x;
          newItemTemplate.y := ItemTemplate^.y;
          newItemTemplate.cx := ItemTemplate^.cx;
          newItemTemplate.cy := ItemTemplate^.cy;
          newItemTemplate.Id := ItemTemplate^.Id
        end;

        Result.Write(newItemTemplate, SizeOf(newItemTemplate));
        pad(Result);

        GetSzOrID(p, szOrID);          // Class
        WriteSzOrID(Result, szOrID);

        GetSzOrID(p, szOrID);          // Title;
        WriteSzOrID(Result, szOrID);

        w := PWord(p)^;
        Inc(p, SizeOf(Word));
        Result.Write(w, SizeOf(w));

        if w > 0 then
        begin
          Result.Write(p^, w);
          Inc(p, w)
        end
      end
    except
      result.Free;
      raise
    end
  end;

begin { SetResourceTemplate }

  if FHwndDlg <> 0 then                 // Get rid of the old dialog(if any)
    SendMessage(FHwndDlg, WM_CLOSE, 0, 0);

  FResourceTemplate := value;

  if Assigned(FResourceTemplate) then
  begin
    s := CreateAdjustedResourceTemplate;
    try
      // Create the dialog window.
      if CreateDialogIndirectParamW(hInstance, PDlgTemplate(s.Memory)^, handle, @DialogProc, LPARAM(self)) = 0 then
        RaiseLastOSError;
    finally
      s.Free
    end;

    if FHwndDlg <> 0 then               // Initialize and display the dialog.
    begin
      Parent.Invalidate;
      InitDialogControls;
      ShowWindow(FHwndDlg, SW_SHOW);
      Realign;
      Resize;
      Invalidate;
      if Assigned(FOnShow) then
        FOnShow(Self);
    end
  end
end;

(*----------------------------------------------------------------------*
 | TDialogBox.WmDestroy                                                 |
 |                                                                      |
 | TDialogBox has been destroyed.  Destroy the dialog too.              |
 *----------------------------------------------------------------------*)
procedure TDialogBox.WmDestroy(var Msg: TWMDestroy);
begin
  if FHwndDlg <> 0 then
    SendMessage(FHwndDlg, WM_CLOSE, 0, 0);
  inherited;
end;

//---------------------------------------------------------------------
// Create an atom for thunking from dialog handle to TDialogBox, using
// SetProp / GetProp.

var
  icc: TInitCommonControlsEx;
const
  RichEditModuleName = 'RICHED32.DLL';

initialization
  icc.dwSize := SizeOf(icc);
  icc.dwICC := ICC_INTERNET_CLASSES or ICC_USEREX_CLASSES or ICC_DATE_CLASSES;
  InitCommonControlsEx(icc);

  FRichEditModule := LoadLibrary(RichEditModuleName);
  if FRichEditModule <= HINSTANCE_ERROR then FRichEditModule := 0;

  gWndAtom := GlobalAddAtom('DlgBox');
finalization
  FreeLibrary(FRichEditModule);
  GlobalDeleteAtom(gWndAtom);
end.
