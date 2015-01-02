(*======================================================================*
 | cmpDialogEditor unit for PEResourceExplorer                          |
 |                                                                      |
 | Dialog editor component.  Ouch.  This one hurt!                      |
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
 | Copyright © Colin Wilson 2002.  All Rights Reserved                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.1      14/02/2002  CPWW  Original                                  |
 *======================================================================*)

unit cmpDialogEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, commctrl, richedit, cmpDialogBox, extCtrls, DialogConsts;

const
  WM_RECREATEDLG = WM_USER + $212;
  WM_CTLPROPERTYCHANGE = WM_USER + $213;

type
  TPropertyKind = (pkGeneral, pkStyle, pkExtended);

  TDropControl = (drNone, drPicture, drStatic, drEdit, drGroupBox, drButton, drCheckBox,
                  drRadioButton, drComboBox, drListBox, drHScroll, drVScroll, drUpDown,
                  drProgressBar, drSlider, drHotKey, drListCtrl, drTreeCtrl, drPageCtrl,
                  drAnimate, drRichEdit, drDateTimePicker, drMonthCal, drIPAddress, drCustom,
                  drExtComboBox);

  TPropertyType = (ptString, ptInteger, ptBoolean, ptEnum, ptSpecial);

  TDialogEditor = class;

  //----------------------------------------------------------------------
// TControlInfo allows us to manipulate controls on the dialog.  One is
// created automatically for each dialog control.
//
// nb.  To drop a new control on the dialog, set the TDialogEditor's
// 'DropControl' property to the required control type, then click on
// the dialog.

  TCreateControlParams = record
    Style, exStyle : Integer;
    cx, cy : Integer;
  end;

  TControlInfo = class
  private
    fOwner : TDialogEditor;
    fItemID : Integer;
    fHelpID : Integer;
    fControlHWND : HWND;

    fOldWindowProc : pointer;
    fStyle : Integer;
    fExStyle : Integer;
    fExtraCount : Integer;
    fExtraData : PChar;
    fDropControl : TDropControl;

    function WindowProc (uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT;

    function GetWindowRect: TRect;
    procedure SetWindowRect(const Value: TRect);
    function GetHasStyle(Style: DWORD): Boolean;
    function GetHasExStyle(ExStyle: DWORD): Boolean;
    function GetWindowText: WideString;
    procedure SetWindowText(const Value: WideString);
  protected
    fTitleSzOrID : TSZOrID;
    fOrigStyle : Integer;
    fGotStyle : boolean;

    constructor Create (AOwner : TDialogEditor; AItemID : Integer; AControlHWND : HWND; ADropControl : TDropControl); virtual;
    function IsGraphicControl : boolean;
    function GetPropertyCount(kind: TPropertyKind): Integer; virtual;
    function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; virtual;
    function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; virtual;
    function GetPropertyName(kind: TPropertyKind; idx: Integer): string; virtual;
    function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; virtual;
    function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; virtual;
    procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); virtual;
    function GetClassSzOrID : TszOrID; virtual;
    procedure DoPropertyChange;

    function GetStyle : DWORD; virtual;
    function GetExStyle : DWORD; virtual;
    procedure SetExStyle(st: DWORD; value: boolean); virtual;
    procedure SetStyle(st: DWORD; value: boolean); virtual;

    procedure RecreateWnd;
    procedure Init; virtual;
  public

    class procedure CreateControlParams (var params : TCreateControlParams); virtual;
    class function GetDescription : string; virtual;

    destructor Destroy; override;

    property Owner : TDialogEditor read fOwner;
    property ItemID : Integer read fItemID;
    property WindowRect : TRect read GetWindowRect write SetWindowRect;
    property ControlHandle : HWND read fControlHWND;

    function FindProperty (kind : TPropertyKind; const Name : string) : Integer;
    property WindowText : WideString read GetWindowText write SetWindowText;
    property Style : DWORD read GetStyle;

    property PropertyCount [kind : TPropertyKind] : Integer read GetPropertyCount;
    property PropertyName [kind : TPropertyKind; idx : Integer] : string read GetPropertyName;
    property PropertyType [kind : TPropertyKind; idx : Integer] : TPropertyType read GetPropertyType;
    property PropertyValue [kind : TPropertyKind; idx : Integer] : Variant read GetPropertyValue write SetPropertyValue;
    property PropertyEnumCount [kind : TPropertyKind; idx : Integer] : Integer read GetPropertyEnumCount;
    property PropertyEnumName [kind : TPropertyKind; idx, enum : Integer] : string read GetPropertyEnumName;
    property HasStyle [Style : DWORD] : Boolean read GetHasStyle write SetStyle;
    property HasExStyle [ExStyle : DWORD] : Boolean read GetHasExStyle write SetExStyle;
    procedure SetMaskedStyle (Style, mask : DWORD);
  end;

  TControlInfoClass = class of TControlInfo;

//----------------------------------------------------------------------
// Derive new TControlInfo based classes for each control type

//-----------------------------------------------------------------------
// Control Info type for the dialog box itself
  TDialogControlInfo = class (TControlInfo)
  public
    class function GetDescription : string; override;
    function GetPropertyCount(kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
    function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
    function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
    function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
    function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
    procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
  end;

//-----------------------------------------------------------------------
// Standard Control Info type - base class for the other controls
  TStandardControlInfo = class (TControlInfo)
  public
    function GetPropertyCount(kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
    function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
    function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
    function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
    function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
    procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
  end;

//---------------------------------------------------------------------
// TResizeControl is attached to a control (or the dialog box itself)
// when it is selected

  TSnibType = (stLT, stMT, stRT, stLM, stRM, stLB, stMB, stRB, stFrame);

  TResizeControl = class (TWinControl)
  private
    fControl : TControlInfo;
    fDialogBox: TDialogEditor;
    FSizerWidth: Integer;
    fBasePT : TPoint;
    fBaseSnib : TSnibType;
    fControlHwnd : HWND;
    procedure SetControl (Value : TControlInfo);
    procedure SetSizerWidth(const Value: Integer);
  protected
    procedure WmPaint (var msg : TWMPaint); message WM_PAINT;
    procedure PaintWindow (DC : HDC); override;
    procedure CreateWindowHandle (const Params : TCreateParams); override;
    procedure CreateParams (var Params : TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create (AOwner : TComponent); override;
    property DialogBox : TDialogEditor read fDialogBox write fDialogBox;
    property Control : TControlInfo read fControl write SetControl;
    property SizerWidth : Integer read FSizerWidth write SetSizerWidth;

    function PtInSnib (pt : TPoint; var snibType : TSnibType) : boolean;
    procedure RecalcSize;
  end;

  TOnDropControl   = procedure (sender : TObject; x, y : Integer; ctrl : TControlInfo) of object;
  TOnControlResize = procedure (Sender: TObject; ctrlInfo: TControlInfo; newRect: TRect) of object;
  TOnControlPropertyChange = procedure (Sender: TObject; ctrlInfo: TControlInfo) of object;
  TOnDeleteControl = procedure (Sender: TObject; ctrl: TControlInfo) of object;

//---------------------------------------------------------------------
// Dialog editor component
  TDialogEditor = class (TDialogBox)
  private
    fDialogInfo : TControlInfo;
    fResizeControl : TResizeControl;
    fDropControl : TDropControl;
    fDropping : boolean;
    fOnDropControl: TOnDropControl;
    fControlInfoList : TList;
    fSelectedControl : TControlInfo;
    fOnDesignModeSelectedItemChange : TNotifyEvent;
    fOnControlResize: TOnControlResize;
    fOnControlPropertyChange: TOnControlPropertyChange;
    fOnDeleteControl: TOnDeleteControl;

    fFontName : string;
    fFontWeight, fFontPoint, fFontCharset : Integer;
    fFontItalic : Boolean;

    fDlgMenu : TszOrID;
    fDlgClass : TszOrID;

    procedure SetDropControl(const Value: TDropControl);
    procedure SubclassDialogControls;
    procedure RestoreDialogControls;
    function EnumDialogControls (hwndCtrl : HWND) : boolean;
    procedure SetSelectedControl(const Value: TControlInfo);
    function GetControlInfo(idx: Integer): TControlInfo;
    function GetControlInfoCount: Integer;
    function GetUniqueID : Integer;
    procedure WmRecreateDlg(var Msg: TMessage); message WM_RECREATEDLG;
    procedure WmCtrlPropertyChange (var Msg : TMessage); message WM_CTLPROPERTYCHANGE;
  protected
    procedure HandleDlgMessage (var Msg : TMessage); override;
    function HandleControlMessage (control : TControlInfo; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT; virtual;

    procedure DestroyWnd; override;
    procedure InitCtrl (n : Integer; template : Pointer; extraCount : Integer; extraData : PChar; titleSzOrID : TSzOrID); override;
    procedure InitDlg (template : Pointer; const fontName : string; fontPoints, fontWeight, fontCharset : Integer; fontItalic : Boolean; const menu, cls : TSzOrID); override;

  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure RecreateDlg;
    procedure DeleteControl (control : TControlInfo);

    property SelectedControl : TControlInfo read fSelectedControl write SetSelectedControl;
    property DropControl : TDropControl read fDropControl write SetDropControl;
    property DialogInfo : TControlInfo read fDialogInfo;
    property ControlInfoCount : Integer read GetControlInfoCount;
    property ControlInfo [idx : Integer] : TControlInfo read GetControlInfo;
    procedure SaveToStream (stream : TStream);
    procedure SetTemplateFont (Font : TFont);
    property FontHandle;

  published
    property OnDesignModeSelectedItemChange : TNotifyEvent read fOnDesignModeSelectedItemChange write fOnDesignModeSelectedItemChange;
    property OnDesignModeDropControl : TOnDropControl read fOnDropControl write fOnDropControl;
    property OnControlResize : TOnControlResize read fOnControlResize write fOnControlResize;
    property OnControlPropertyChange : TOnControlPropertyChange read fOnControlPropertyChange write fOnControlPropertyChange;
    property OnDeleteControl : TOnDeleteControl read fOnDeleteControl write fOnDeleteControl;
  end;

function GetControlInfoClass (dropControl : TDropControl) : TControlInfoClass;

implementation

uses Variants,
     DialogStaticControls,
     DialogButtonControls,
     DialogEditControls,
     DialogListboxControls,
     DialogComboBoxControls,
     DialogScrollbarControls,
     DialogUpDownControls,
     DialogProgressBarControls,
     DialogSliderControls,
     DialogHotkeyControls,
     DialogStrings,
     DialogListViewControls;

resourcestring
  rstDefaultText = 'Text';

const

//---------------------------------------
// Property control arrays for all controls
  ControlPropertyGeneralCount = 4;
  ControlPropertyStyleCount = 2;
  ControlPropertyExtendedCount = 7;
  ControlPropertyCount : array [TPropertyKind] of Integer = (ControlPropertyGeneralCount, ControlPropertyStyleCount, ControlPropertyExtendedCount);
  ControlPropertyGeneralName : array [0..ControlPropertyGeneralCount - 1] of string = (rstHeight, rstLeft, rstTop, rstWidth);
  ControlPropertyStyleName : array [0..ControlPropertyStyleCount - 1] of string = (rstDisabled, rstVisible);
  ControlPropertyExtendedName : array [0..ControlPropertyExtendedCount - 1] of string = (rstAcceptFiles, rstClientEdge, rstStaticEdge, rstTransparent,rstRTLReadingOrder, rstRightAlignedText, rstLeftScrollBar);
  ControlPropertyGeneralType : array [0..ControlPropertyGeneralCount - 1] of TPropertyType = (ptInteger, ptInteger, ptInteger, ptInteger);
  ControlPropertyStyleType : array [0..ControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean, ptBoolean);
  ControlPropertyExtendedType : array [0..ControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean);

//----------------------------------------------
// Property control arrays for standard controls
  StandardControlPropertyGeneralCount = 4;
  StandardControlPropertyStyleCount = 0;
  StandardControlPropertyExtendedCount = 1;
  StandardControlPropertyCount : array [TPropertyKind] of Integer = (StandardControlPropertyGeneralCount, StandardControlPropertyStyleCount, StandardControlPropertyExtendedCount);
  StandardControlPropertyGeneralName : array [0..StandardControlPropertyGeneralCount - 1] of string = (rstID, rstContextHelp, rstGroup, rstTabstop);
//  StandardControlPropertyStyleName : array [0..StandardControlPropertyStyleCount - 1] of string = (rstDisabled, rstVisible);
  StandardControlPropertyExtendedName : array [0..StandardControlPropertyExtendedCount - 1] of string = (rstModalFrame);
  StandardControlPropertyGeneralType : array [0..StandardControlPropertyGeneralCount - 1] of TPropertyType = (ptInteger, ptBoolean, ptBoolean, ptBoolean);
//  StandardControlPropertyStyleType : array [0..StandardControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean, ptBoolean);
  StandardControlPropertyExtendedType : array [0..StandardControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean);

//----------------------------------------------------
// Property control arrays for the dialog info control

  DialogControlPropertyGeneralCount = 3;
  DialogControlPropertyStyleCount = 20;
  DialogControlPropertyExtendedCount = 3;
  DialogControlPropertyCount : array [TPropertyKind] of Integer = (DialogControlPropertyGeneralCount, DialogControlPropertyStyleCount, DialogControlPropertyExtendedCount);
  DialogControlPropertyGeneralName : array [0..DialogControlPropertyGeneralCount - 1] of string = (rstCaption, rstFont, rstMenu);
  DialogControlPropertyStyleName : array [0..DialogControlPropertyStyleCount - 1] of string = (
    rstAbsoluteAlign,
    rstBorder,
    rstCenter,
    rstCenterMouse,
    rstClipChildren,
    rstClipSiblings,
    rstContextHelp,
    rstControl,
    rstLocalEdit,
    rstMaximimizeBox,
    rstMinimizeBox,
    rstNoFailCreate,
    rstNoIdleMsg,
    rstHScroll,
    rstVScroll,
    rstSetForeground,
    rstStyle,
    rstSystemMenu,
    rstSystemModal,
    rstTitleBar);
  DialogControlPropertyExtendedName : array [0..DialogControlPropertyExtendedCount - 1] of string = (rstControlParent, rstNoParentNotify,rstToolWindow);
  DialogControlPropertyGeneralType : array [0..DialogControlPropertyGeneralCount - 1] of TPropertyType = (ptString, ptSpecial, ptString);
  DialogControlPropertyStyleType : array [0..DialogControlPropertyStyleCount - 1] of TPropertyType = (
    ptBoolean,
    ptEnum,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptBoolean,
    ptEnum,
    ptBoolean,
    ptBoolean,
    ptBoolean);
  DialogControlPropertyExtendedType : array [0..DialogControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean, ptBoolean);

//--------------------------------------------------------------
// Classes names for each of the dropable controls
  controlWindowClasses : array [TDropControl] of string =
   ('', 'STATIC', 'STATIC', 'EDIT', 'Button', 'Button', 'Button',
    'BUTTON', 'COMBOBOX', 'LISTBOX', 'SCROLLBAR', 'SCROLLBAR', UPDOWN_CLASS,
    PROGRESS_CLASS, TRACKBAR_CLASS, HOTKEYCLASS, WC_LISTVIEW, WC_TREEVIEW, WC_TABCONTROL,
    ANIMATE_CLASS, RICHEDIT_CLASS, DATETIMEPICK_CLASS, MONTHCAL_CLASS, WC_IPADDRESS, 'Custom',
    WC_COMBOBOXEX);

(*----------------------------------------------------------------------*
 | MaskDWORD                                                            |
 |                                                                      |
 | Set or remove a mask from a DWORD                                    |
 *----------------------------------------------------------------------*)
function MaskDWORD (w, mask : DWORD; Value : Boolean) : DWORD;
begin
  if Value then
    result := w or mask
  else
    result := w and not mask
end;

(*----------------------------------------------------------------------*
 | function GetDropControl () : TDropControl                            |
 |                                                                      |
 | Get our 'TDropControl' type from a windows class name and style.     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   const className : string        The Windows class name             |
 |   style : DWORD                   The window style                   |
 |                                                                      |
 | The function returns the TDropControl represntation of the windows   |
 | class type.                                                          |
 *----------------------------------------------------------------------*)
function GetDropControl (const className : string; style : DWORD) : TDropControl;
var
  i : TDropControl;

  function GetStaticDropControl (style : DWORD) : TDropControl;
  begin
    if (style and SS_BITMAP) <> 0 then
      result := drPicture
    else
      result := drStatic
  end;

  function GetButtonDropControl (style : DWORD) : TDropControl;
  begin
    case style and $1f of
      0..1, 8, 10, 11 : result := drButton;
      2..3, 5..6      : result := drCheckBox;
      4, 9            : result := drRadioButton;
      7               : result := drGroupBox;
      else
        result := drButton
    end
  end;

  function GetScrollbarDropControl (style : DWORD) : TDropControl;
  begin
    if (style and SB_VERT) <> 0 then
      result := drVScroll
    else
      result := drHScroll
  end;

begin { GetDropControl }
  result := drCustom;
  if CompareText (className, 'STATIC') = 0 then
    result := GetStaticDropControl (style)
  else
    if CompareText (className, 'BUTTON') = 0 then
      result := GetButtonDropControl (style)
    else
      if CompareText (className, 'SCROLLBAR') = 0 then
        result := GetScrollbarDropControl (style)
      else
        for i := Low (TDropControl) to High (TDropControl) do
          if CompareText (className, controlWindowClasses [i]) = 0 then
          begin
            result := i;
            break
          end
end;


(*----------------------------------------------------------------------*
 | function GetControlInfoClass () : TControlInfoClass                  |
 |                                                                      |
 | Get the TControlInfo derived class for a specified control           |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   dropControl : TDropControl      The control type to find           |
 |                                                                      |
 | The function returns the derived TControlInfo class                  |
 *----------------------------------------------------------------------*)
function GetControlInfoClass (dropControl : TDropControl) : TControlInfoClass;
begin
  case dropControl of
    drPicture      : result := TPictureControlInfo;
    drStatic       : result := TStaticTextControlInfo;
    drEdit         : result := TEditControlInfo;
    drGroupBox     : result := TGroupBoxControlInfo;
    drButton       : result := TPushbuttonControlInfo;
    drCheckbox     : result := TCheckboxControlInfo;
    drRadioButton  : Result := TRadioButtonControlInfo;
    drListbox      : Result := TListboxControlInfo;
    drComboBox     : Result := TComboBoxControlInfo;
    drHScroll      : Result := THScrollbarControlInfo;
    drVScroll      : Result := TVScrollbarControlInfo;
    drUpDown       : Result := TUpDownControlInfo;
    drProgressBar  : Result := TProgressBarControlInfo;
    drSlider       : result := TSliderControlInfo;
    drHotKey       : result := THotkeyControlInfo;
    drListCtrl     : result := TListviewControlInfo;
  else
    result := TControlInfo;
  end;
end;

(*----------------------------------------------------------------------*
 | function ControlWindowProc () : LResult                              |
 |                                                                      |
 | WindowProc for subclassed dialog controls                            |
 |                                                                      |
 | Takes standard windows proc parameters, and returns standard lResult |
 *----------------------------------------------------------------------*)
function ControlWindowProc (wnd : HWND; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : LResult; stdcall;
var
  ctrlThunk : TControlInfo;
begin
  result := 0;
  ctrlThunk := TControlInfo (GetProp (wnd, PChar (gWndAtom)));

  if Assigned (ctrlThunk) then
    result := ctrlThunk.WindowProc (uMsg, wParam, lParam)
end;

(*----------------------------------------------------------------------*
 | function EnumChildProc                                               |
 |                                                                      |
 | Callback function for EnumChildWindows                               |
 *----------------------------------------------------------------------*)
function EnumChildProc (hWnd : HWND; lParam : LPARAM) : BOOL; stdcall;
begin
  result := TDialogEditor (lParam).EnumDialogControls (hWnd)
end;

(*----------------------------------------------------------------------*
 | SzOrIDToString                                                       |
 |                                                                      |
 | Converts an SzOrID to a string                                       |
 *----------------------------------------------------------------------*)
function SzOrIDToString (const id : TSzOrID) : string;
begin
  if id.isID then
    Result := IntToStr (id.id)
  else
    Result := id.sz
end;

(*----------------------------------------------------------------------*
 | StringToSzOrID                                                       |
 |                                                                      |
 | Converts a strring to an SzOrID                                      |
 *----------------------------------------------------------------------*)
function StringToSzOrID (const st : string) : TszOrID;
var
  i : Integer;
begin
  Result.isID := Length (st) > 0;

  for i := 1 to Length (st) do          // Is it all numeric ??
    if not (st [i] in ['0'..'9']) then
    begin
      Result.isID := False;
      break
    end;

  if Result.isID then
  begin
    Result.id := StrToInt (st);         // Numeric IDs must be word-sized
    if Result.id >= 65536 then
      Result.isID := False
  end;

  if not Result.isID then               // Non-numeric - or too big.  Keep string
  begin
    Result.id := 0;
    Result.sz := st
  end
end;

{ TDialogEditor }

(*----------------------------------------------------------------------*
 | constructor TDialogEditor.Create ();                                 |
 |                                                                      |
 | Constructor for TDialogEditor                                        |
 *----------------------------------------------------------------------*)
constructor TDialogEditor.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  Margin := 5;
  fControlInfoList := TList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TDialogEditor.Destroy ();                                 |
 |                                                                      |
 | Destructor for TDialogEditor                                         |
 *----------------------------------------------------------------------*)
destructor TDialogEditor.Destroy;
begin
  fControlInfoList.Free;        // Free the ControlInfo list.
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TDialogEditor.DestroyWnd ();                               |
 |                                                                      |
 | Free the dialog's controlInfo.  That one's not on the control info   |
 | list                                                                 |
 *----------------------------------------------------------------------*)
procedure TDialogEditor.DestroyWnd;
begin
  FreeAndNil (fDialogInfo);
  inherited
end;

(*----------------------------------------------------------------------*
 | procedure TDialogEditor.EnumDialogControls                           |
 |                                                                      |
 | Called for each control.  Thunk the control to a new instance of the |
 | correct controlInfo class.                                           |
 *----------------------------------------------------------------------*)
function TDialogEditor.EnumDialogControls(hwndCtrl: HWND): boolean;
var
  parent : HWND;
  className : string;
  style : DWORD;
  dropControl : TDropControl;
  controlInfoClass : TControlInfoClass;
begin
  parent := GetParent (hwndCtrl);
  SetLength (className, 256);
  GetClassName (hwndCtrl, PChar (classname), 256);
  className := PChar (className);

  style := GetWindowLong (hwndCtrl, GWL_STYLE);

  dropControl := GetDropControl (className, style);
  controlInfoClass := GetControlInfoClass (dropControl);

  if parent = DialogHandle then
    fControlInfoList.Add (controlInfoClass.Create (Self, GetDlgCtrlID (hwndCtrl), hwndCtrl, dropControl));
  result := True;
end;

(*----------------------------------------------------------------------*
 | TDialogEditor.GetControlInfo                                         |
 |                                                                      |
 | Get the ControlInfo for the nth control                              |
 *----------------------------------------------------------------------*)
function TDialogEditor.GetControlInfo(idx: Integer): TControlInfo;
begin
  Result := TControlInfo (fControlInfoList [idx]);
end;

(*----------------------------------------------------------------------*
 | TDialogEditor.GetControlInfoCount                                    |
 |                                                                      |
 | Get the number of subclassed controls on the dialog                  |
 *----------------------------------------------------------------------*)
function TDialogEditor.GetControlInfoCount: Integer;
begin
  Result := fControlInfoList.Count
end;

(*----------------------------------------------------------------------*
 | TDialogEditor.GetUniqueID                                            |
 |                                                                      |
 | Get a unique ID for a new control                                    |
 *----------------------------------------------------------------------*)
function TDialogEditor.GetUniqueID: Integer;
var
  i, n : Integer;
begin
  n := 0;

  for i := 0 to ControlInfoCount - 1 do
    if ControlInfo [i].fItemID > n then
      n := ControlInfo [i].fItemID;

  Result := n + 1
end;

(*----------------------------------------------------------------------*
 | procedure TDialogEditor.HandleControlMessage                         |
 |                                                                      |
 | Handle a windows message for a control                               |
 *----------------------------------------------------------------------*)
function TDialogEditor.HandleControlMessage(control: TControlInfo;
  uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  wmk : TWMKey;
  Msg : TMessage;
  pt : TPoint;
  unicode : boolean;
begin
  result := 0;
  unicode := IsWindowUnicode (control.fControlHWND);
  case uMsg of
    WM_LBUTTONDOWN :
      if fDropping then
      begin
        Msg.Msg := uMsg;
        Msg.wParam := wParam;

        pt.x := LOWORD (lParam);
        pt.y := HIWORD (lParam);
        MapWindowPoints (control.fControlHWND, DialogHandle, pt, 1);
        Msg.lParam := MakeLong (pt.x, pt.y);
        Msg.Result := 0;
        HandleDlgMessage (Msg)
      end
      else
        SelectedControl := control;

    WM_SETFOCUS:;

    WM_NCACTIVATE:;

    WM_ACTIVATE:;

    WM_SYSCOMMAND:;

    WM_STYLECHANGED:
      begin
        if wParam = GWL_STYLE then
          control.fStyle := PStyleStruct (lParam).styleNew;
        if wParam = GWL_EXSTYLE then
          control.fExStyle := PStyleStruct (lParam).styleNew;
      end;

    WM_KEYDOWN:
      begin
        wmk.Msg := WM_KEYDOWN;
        wmk.CharCode := wParam;
        wmk.KeyData := lParam;
        DoKeyDown (wmk);
      end;

    WM_CHAR:;

    WM_KEYUP:;

    WM_LBUTTONDBLCLK :;

    WM_SIZE :
    begin
       if unicode then
         result := CallWindowProcW (control.fOldWindowProc, control.fControlHWND, uMsg, wParam, lParam)
       else
         result := CallWindowProcA (control.fOldWindowProc, control.fControlHWND, uMsg, wParam, lParam);
      if Assigned (fResizeControl) and (control.fControlHWND = fResizeControl.fControlHwnd) then
        fResizeControl.RecalcSize;
    end;

    else
      if unicode then
        result := CallWindowProcW (control.fOldWindowProc, control.fControlHWND, uMsg, wParam, lParam)
      else
        result := CallWindowProcA (control.fOldWindowProc, control.fControlHWND, uMsg, wParam, lParam)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TDialogEditor.HandleDlgMessage                             |
 |                                                                      |
 | Overriden from TDialogBox base class to handle dialog box messages   |
 *----------------------------------------------------------------------*)
procedure TDialogEditor.HandleDlgMessage (var Msg : TMessage);
var
  continueProcessing : boolean;
  pt, ps : TPoint;
  r : TRect;
  i : Integer;
  ctrlClass : TControlInfoClass;
  ctrl : TControlInfo;
  hwndCtrl : HWND;
  params : TCreateControlParams;
  className : WideString;

begin
  if Msg.Msg <> WM_SYSCOMMAND then
    inherited HandleDlgMessage (Msg);

  case msg.Msg of
    WM_INITDIALOG:
      begin     // Initialize things.
        SubclassDialogControls;
        fDialogInfo := TDialogControlInfo.Create (Self, 0, DialogHandle, drNone);
      end;

    WM_DESTROY: // Tidy up things.
      begin
        FreeAndNil (fDialogInfo);
        FreeAndNil (fResizeControl);
        RestoreDialogControls;
      end;

    WM_NCLBUTTONDOWN :
    begin
      SelectedControl := fDialogInfo;
      Msg.Result := Ord (True)
    end;

                // Handle 'design mode' clicks
    WM_LBUTTONDOWN:

                // DropControl is valid.  Tell the UI to drop one.
      if fDropping then
      begin
        fDropping := False;
        ctrlClass := GetControlInfoClass (fDropControl);
        if Assigned (ctrlClass) then
        begin
          pt.x := LOWORD (msg.LParam);
          pt.y := HIWORD (msg.LParam);
          i := GetUniqueID;

          params.cx := 50;
          params.cy := 14;
          params.Style := WS_VISIBLE or WS_CHILD;
          params.exStyle := 0;
          className := controlWindowClasses [fDropControl];

          ctrlClass.CreateControlParams (params);

          ps.x := params.cx;
          ps.y := params.cy;

          ps := DialogPointToPoint (ps);

          with params do
          begin
            hwndCtrl := CreateWindowExW (ExStyle, PWideChar (className), '', Style, pt.x, pt.y, ps.x, ps.y, DialogHandle, i, hInstance, nil);

            if hwndCtrl = 0 then
              RaiseLastOSError;
            SendMessage (hwndCtrl, WM_SETFONT, FontHandle, 0);
          end;

          ctrl := ctrlClass.Create (Self, i, hwndCtrl, fDropControl);
          fControlInfoList.Add (ctrl);

          if ctrl.IsGraphicControl then
          begin
            ctrl.fTitleSzOrID.isID := True;
            ctrl.fTitleSzOrID.id := 0;
            SetCtrlImage1 (hwndCtrl, className = 'BUTTON', IMAGE_BITMAP, ctrl.fTitleSzOrID);
          end
          else
            ctrl.SetWindowText(rstDefaultText);

          ctrl.Init;
          if Assigned (OnDesignModeDropControl) then
            OnDesignModeDropControl (self, pt.x, pt.y, ctrl)
        end
      end
      else
      begin
        continueProcessing := False;

        pt.x := LOWORD (msg.LParam);
        pt.y := HIWORD (msg.LParam);

        for i := fControlInfoList.Count - 1 downto 0 do  // When controls are disabled they dont
        begin                                            // receive clicks, and we end up here.
                                                         // Forward the click to the control's handler

          GetWindowRect (TControlInfo (fControlInfoList [i]).fControlHWND, r);
          MapWindowPoints (HWND_DESKTOP, DialogHandle, r, 2);

          if PtInRect (r, pt) then
          begin
            SendMessage (TControlInfo (fControlInfoList [i]).fControlHWND, WM_LBUTTONDOWN, 0, 0);
            ContinueProcessing := True;
            break
          end
        end;

        if not ContinueProcessing then                  // No disabled control clicked
           SelectedControl := fDialogInfo
      end;

    WM_SIZE:
    begin
      if DialogHandle = fResizeControl.fControlHwnd then
        fResizeControl.RecalcSize;
    end
  end
end;

(*----------------------------------------------------------------------*
 | TDialogEditor.InitCtrl                                               |
 |                                                                      |
 |
 |                                                                      |
 | Parameters:
 |   n: Integer; template: Pointer; extraCount : Integer; extraData : PChar; titleSzOrID : TSzOrID
 |
 | The function returns None                                                |
 *----------------------------------------------------------------------*)
procedure TDialogEditor.InitCtrl(n: Integer; template: Pointer; extraCount : Integer; extraData : PChar; titleSzOrID : TSzOrID);
var
  ctrl : TControlInfo;
  id : Integer;
  helpID : Integer;
begin
  if n < ControlInfoCount then
  begin
    ctrl := ControlInfo [n];

    if ExtendedTemplate then
    begin
      id := PDlgItemTemplateEx (template)^.id;
      helpId := PDlgItemTemplateEx (template)^.helpId;
      ctrl.fOrigStyle :=PDlgItemTemplateEx (template)^.Style
    end
    else
    begin
      id := PDlgItemTemplate (template)^.id;
      helpID := -1;
      ctrl.fOrigStyle := PDlgItemTemplate (template)^.style
    end;

    if id = ctrl.fItemID then
    begin
      ctrl.fHelpID := helpID;
      ctrl.fTitleSzOrID := titleSzOrID
    end
    else
      Windows.Beep (880, 10);

    if extraCount > 0 then
    begin
      ctrl.fExtraCount := extraCount;
      GetMem (ctrl.fExtraData, extraCount);
      Move (extraData^, ctrl.fExtraData^, ctrl.fExtraCount)
    end;

    ctrl.Init
  end
end;

procedure TDialogEditor.InitDlg (template : Pointer; const fontName : string; fontPoints, fontWeight, fontCharset : Integer; fontItalic : Boolean; const menu, cls : TSzOrID);
var
  helpID : Integer;
begin
  fFontName := fontName;
  fFontWeight := fontWeight;
  fFontPoint := fontPoints;
  fFontItalic := fontItalic;
  fFontCharset := fontCharset;

  fDlgMenu := menu;
  fDlgClass := cls;

  if ExtendedTemplate then
  begin
    helpId := PDlgTemplateEx (template)^.helpId
  end
  else
  begin
    helpId := -1
  end;

  fDialogInfo.fHelpID := helpId;
  SelectedControl := fDialogInfo
end;

(*----------------------------------------------------------------------*
 | procedure TDialogEditor.RestoreDialogControls                        |
 |                                                                      |
 | Un-thunk the dialog controls, and clear the controls list            |
 *----------------------------------------------------------------------*)
procedure TDialogEditor.RestoreDialogControls;
var
  i : Integer;
begin
  for i := 0 to fControlInfoList.Count - 1 do
    TControlInfo (fControlInfoList [i]).Free;

  fControlInfoList.Clear;
end;

(*----------------------------------------------------------------------*
 | TDialogEditor.SetDropControl                                         |
 |                                                                      |
 | Set the control to drop                                              |
 *----------------------------------------------------------------------*)
procedure TDialogEditor.SetDropControl (const Value: TDropControl);
begin
  fDropControl := Value;
  fDropping := True;
end;

procedure TDialogEditor.SetTemplateFont(Font: TFont);
begin
  fFontName := Font.Name;
  fFontPoint := Font.Size;
  if fsBold in Font.Style then
    fFontWeight := FW_BOLD              // But windows sets it back to FW_NORMAL when creating dialogs
  else                                  // see the docs for DLGTEMPLATEEX !
    fFontWeight := FW_NORMAL;

  fFontItalic := fsItalic in Font.Style;

  // Now save the template (with SaveToStream) and reload it.  Changing the font
  // may compltely re-size the dialog, etc.
end;

procedure TDialogEditor.SetSelectedControl(const Value: TControlInfo);
begin
  if fSelectedControl <> value then
  begin
    if fResizeControl <> Nil then
      FreeAndNil (fResizeControl);

    if Assigned (Value) then
    begin
      fResizeControl := TResizeControl.Create (owner);
      fResizeControl.DialogBox := self;
      fResizeControl.Control := value
    end;

    FSelectedControl := value;
    Invalidate;

    if Assigned (OnDesignModeSelectedItemChange) and not (csDestroying in ComponentState) then
       OnDesignModeSelectedItemChange (self);
  end
end;

(*----------------------------------------------------------------------*
 | TDialogEditor.SubclassDialogControls                                 |
 |                                                                      |
 | Enumerate the controls, and subclass each one.                       |
 *----------------------------------------------------------------------*)
procedure TDialogEditor.SubclassDialogControls;
begin
  if DialogHandle <> 0 then
    EnumChildWindows (DialogHandle, @EnumChildProc, LPARAM (self));
end;


{ TControlInfo }

constructor TControlInfo.Create(AOwner : TDialogEditor; AItemID: Integer; AControlHWND: HWND; ADropControl : TDropControl);
var
  unicode : boolean;
begin
  fDropControl := ADropControl;
  fOwner := AOWner;
  fItemID := AItemID;
  fHelpID := -1;

  fControlHWnd := ACOntrolHWND;

  if fControlHWND <> AOwner.DialogHandle then

  begin
    unicode := IsWindowUnicode (fControlHWnd);
    if unicode then
    begin
      SetPropW (fControlHWND, PWideChar (gWndAtom), Integer (Self));
      fOldWindowProc := Pointer (SetWindowLongW (fControlHWND, GWL_WNDPROC, Integer (@ControlWindowProc)))
    end
    else
    begin
      SetProp (fControlHWND, PChar (gWndAtom), Integer (Self));
      fOldWindowProc := Pointer (SetWindowLong (fControlHWND, GWL_WNDPROC, Integer (@ControlWindowProc)))
    end
  end
end;

class procedure TControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
// stub
end;

destructor TControlInfo.Destroy;
begin
  ReallocMem (fExtraData, 0);
  RemoveProp (fControlHWND, PChar (gWndAtom));

  if IsWindowUnicode (fControlHWND) then
    SetWindowLongW (fControlHWND, GWL_WNDPROC, Integer (fOldWindowProc))
  else
    SetWindowLongA (fControlHWND, GWL_WNDPROC, Integer (fOldWindowProc));
  inherited;
end;

procedure TControlInfo.DoPropertyChange;
begin
  PostMessage (fOwner.Handle, WM_CTLPROPERTYCHANGE, WPARAM (self), 0)
end;

function TControlInfo.FindProperty(kind: TPropertyKind; const Name: string): Integer;
var
  i : Integer;
begin
  Result := -1;

  for i := 0 to PropertyCount [kind] - 1 do
    if PropertyName [kind, i] = Name then
    begin
      Result := i;
      break
    end
end;

function TControlInfo.GetClassSzOrID: TszOrID;
var
  params : TCreateControlParams;
begin
  CreateControlParams (params);
  Result.isID := False;
  Result.sz := controlWindowClasses [fDropControl]; // params.className
end;

class function TControlInfo.GetDescription: string;
begin
  Result := rstControl
end;

function TControlInfo.GetExStyle: DWORD;
begin
  if not fGotStyle then
  begin
    fStyle := GetWindowLong (fControlHWND, GWL_STYLE);
    fExStyle := GetWindowLong (fControlHWND, GWL_EXSTYLE);
    fGotStyle := True
  end;

  result := fExStyle
end;

function TControlInfo.GetHasExStyle(ExStyle: DWORD): Boolean;
begin
  Result := (GetExStyle and ExStyle) = ExStyle
end;

function TControlInfo.GetHasStyle(Style: DWORD): Boolean;
begin
  Result := (GetStyle and Style) = style
end;

function TControlInfo.GetPropertyCount(kind: TPropertyKind): Integer;
begin
  Result := ControlPropertyCount [kind];
end;

function TControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  Result := 0;
end;

function TControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  Result := ''
end;

function TControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  Result := '';
  case kind of
    pkGeneral : Result := ControlPropertyGeneralName [idx];
    pkStyle : Result := ControlPropertyStyleName [idx];
    pkExtended : Result := ControlPropertyExtendedName [idx];
  end
end;

function TControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  Result := ptInteger;
  case kind of
    pkGeneral : Result := ControlPropertyGeneralType [idx];
    pkStyle : Result := ControlPropertyStyleType [idx];
    pkExtended : Result := ControlPropertyExtendedType [idx];
  end
end;

function TControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
var
  r : TRect;
begin
  Result := null;
  case kind of
    pkGeneral:
      begin
        r := WindowRect;
        case idx of
          0 : Result := r.Bottom - r.Top;
          1 : if Self = fOwner.fDialogInfo then Result := fOwner.OrigX else Result := r.Left;
          2 : if Self = fOwner.fDialogInfo then Result := fOwner.OrigY else Result := r.Top;
          3 : Result := r.Right - r.Left
        end
      end;

    pkStyle :
      case idx of
        0 : Result := HasStyle [WS_DISABLED];
        1 : if Self = fOwner.fDialogInfo then
              Result := (fOwner.OrigStyle and WS_VISIBLE) <> 0
            else
              Result := HasStyle [WS_VISIBLE];
      end;

    pkExtended :
      case idx of
        0 : Result := HasExStyle [WS_EX_ACCEPTFILES];
        1 : Result := HasExStyle [WS_EX_CLIENTEDGE];
        2 : Result := HasExStyle [WS_EX_STATICEDGE];
        3 : Result := HasExStyle [WS_EX_TRANSPARENT];
        4 : Result := HasExStyle [WS_EX_RTLREADING];
        5 : Result := HasExStyle [WS_EX_RIGHT];
        6 : Result := HasExStyle [WS_EX_LEFTSCROLLBAR];
      end
  end
end;

function TControlInfo.GetStyle: DWORD;
begin
  if not fGotStyle then
  begin
    fStyle := GetWindowLong (fControlHWND, GWL_STYLE);
    fExStyle := GetWindowLong (fControlHWND, GWL_EXSTYLE);
    fGotStyle := True
  end;

  result := fStyle
end;

function TControlInfo.GetWindowRect: TRect;
var
  dw, dh : Integer;
begin
  if Self = fOwner.fDialogInfo then
  begin
    windows.GetClientRect (fControlHWND, result);
    if HasStyle [WS_VSCROLL] then dw := GetSystemMetrics (SM_CXVSCROLL) else dw := 0;
    if HasStyle [WS_HSCROLL] then dh := GetSystemMetrics (SM_CYHSCROLL) else dh := 0;

    Inc (Result.Right, dw);
    Inc (Result.Bottom, dh);

    OffsetRect (result, -fOwner.Margin, -fOwner.Margin);
  end
  else
  begin
    windows.GetWindowRect (fControlHWND, result);
    MapWindowPoints (HWND_DESKTOP, GetParent (fControlHWND), result, 2);
  end;
  result := fOwner.RectToDialogRect (result)
end;

function TControlInfo.GetWindowText: WideString;
begin
  SetLength (Result, 1024);
  windows.GetWindowTextW (fControlHWND, PWideChar (Result), 1024);
  Result := PWideChar (Result);
end;

procedure TControlInfo.Init;
begin
// stub
end;

function TControlInfo.IsGraphicControl: boolean;
var
  szOrID : TSzOrID;
begin
  szOrID := GetClassSzOrID;

  if szOrID.id = BUTTON_ID then
    result := ((Style and BS_ICON) <> 0) or ((Style and BS_BITMAP) <> 0)
  else
    if szOrID.id = STATIC_ID then
      result := (Style and SS_TYPEMASK) in [SS_ICON, SS_BITMAP, SS_ENHMETAFILE]
    else
      result := False
end;

procedure TControlInfo.RecreateWnd;
var
  newStyle, newExStyle : DWORD;
  newTitle : WideString;
  newID : DWORD;
  rect : TRect;
  params : TCreateControlParams;
  className : WideString;
  setResizeControl : Boolean;
  szOrID : TSzOrID;
  isBtn : Boolean;
  tp : Integer;
begin
  if Self = fOwner.fDialogInfo then
    fOwner.RecreateDlg
  else
  begin
    newStyle := GetStyle;
    newExStyle := GetExStyle;
    newTitle := WindowText;
    newID := itemID;

    setResizeControl := False;
    if fOwner.fResizeControl.fControl = Self then
    begin
      FreeAndNil (fOwner.fResizeControl);
      setResizeControl := True
    end;

    windows.GetWindowRect (fControlHWND, rect);
    MapWindowPoints (HWND_DESKTOP, Owner.DialogHandle, rect, 2);

    RemoveProp (fControlHWND, PChar (gWndAtom));
    if IsWindowUnicode (fControlHWND) then
      SetWindowLongW (fControlHWND, GWL_WNDPROC, Integer (fOldWindowProc))
    else
      SetWindowLongA (fControlHWND, GWL_WNDPROC, Integer (fOldWindowProc));
    DestroyWindow (fControlHWND);

    className := controlWindowClasses [fDropControl];
    CreateControlParams (params);

    fControlHWND := CreateWindowExW (
      newExStyle,
      PWideChar (className),
      PWideChar (newTitle),
      newStyle, rect.Left, rect.Top, rect.Right - rect.Left, rect.Bottom - rect.Top,
      Owner.DialogHandle, newId, hInstance, nil);

    SendMessage (fControlHWND, WM_SETFONT, Owner.FontHandle, 0);

    if IsWindowUnicode (fControlHWND) then
    begin
      fOldWindowProc := Pointer (SetWindowLongW (fControlHWND, GWL_WNDPROC, Integer (@ControlWindowProc)));
      SetPropW (fControlHWND, PWideChar (gWndAtom), Integer (Self));
    end
    else
    begin
      fOldWindowProc := Pointer (SetWindowLong (fControlHWND, GWL_WNDPROC, Integer (@ControlWindowProc)));
      SetPropA (fControlHWND, PChar (gWndAtom), Integer (Self));
    end;


    szOrID := GetClassSzOrID;

    Owner.GetImageType (szOrID, newStyle, isBtn, tp);

    if tp <> -1 then
      Owner.SetCtrlImage1 (fControlHWND, isBtn, tp, fTitleSzOrID);

    if setResizeControl then
    begin
      fOwner.fResizeControl := TResizeControl.Create (fOwner.Owner);
      fOwner.fResizeControl.DialogBox := fOwner;
      fOwner.fResizeControl.Control := self
    end;

    Init
  end
end;

procedure TControlInfo.SetExStyle (st : DWORD; value : boolean);
var
  oldExStyle : DWORD;
begin
  oldExStyle := GetEXStyle;
  if value then
  begin
    if oldExStyle <> oldExStyle or st then
    begin
      SetWindowLong (fControlHWnd, GWL_EXSTYLE, oldExStyle or st);
      InvalidateRect (GetParent (fControlHWND), Nil, True)
    end
  end
  else
  begin
    if oldExStyle <> oldExStyle and not st then
    begin
      SetWindowLong (fControlHWnd, GWL_EXSTYLE, oldExStyle and not st);
      InvalidateRect (GetParent (fControlHWND), Nil, True)
    end
  end;
  fGotStyle := False
end;

procedure TControlInfo.SetMaskedStyle(Style, mask: DWORD);
var
  oldStyle : DWORD;
begin
  oldStyle := GetStyle;
  SetWindowLong (fControlHWND, GWL_STYLE, (oldStyle and not mask) or style);
  if Assigned (fOwner.fResizeControl) and (fOwner.fResizeControl.fControlHWND = fControlHWND) then
    fOwner.fResizeControl.RecalcSize;
  fGotStyle := False;
end;

procedure TControlInfo.SetPropertyValue(kind: TPropertyKind; idx: Integer;
  const Value: Variant);
var
  r, r1 : TRect;
  recreateRequired : Boolean;
  frameChanged : Boolean;
begin
  recreateRequired := False;
  frameChanged := False;

  case kind of
    pkGeneral :
      begin
        r := WindowRect;
        r1 := r;
        case idx of
{ height }0 : r := rect (r.Left, r.Top, r.Right, r.Top + Value);
{ left }  1 : if Self = fOwner.fDialogInfo then fOwner.fOrigX := Value else r := rect (Value, r.Top, r.Right, r.Bottom);
{ top  }  2 : if Self = fOwner.fDialogInfo then fOwner.fOrigY := Value else r := rect (r.Left, Value, r.Right, r.Bottom);
{ width}  3 : r := rect (r.Left, r.Top, r.Left + Value, r.Bottom)
        end;
        if not EqualRect (r1, r) then
          WindowRect := r
      end;
    pkStyle :
      case idx of
        0 : HasStyle [WS_DISABLED] := Value;
        1 : if Self = fOwner.fDialogInfo then
              Owner.OrigStyle := MaskDWORD (Owner.OrigStyle, WS_VISIBLE, Value)
            else
              HasStyle [WS_VISIBLE] := Value
      end;

    pkExtended:
      begin
        case idx of
          0 : HasExStyle [WS_EX_ACCEPTFILES] := Value;
          1 : begin HasExStyle [WS_EX_CLIENTEDGE] := Value; frameChanged := True end;
          2 : begin HasExStyle [WS_EX_STATICEDGE] := Value; frameChanged := True end;
          3 : begin HasExStyle [WS_EX_TRANSPARENT] := Value; recreateRequired := True; end;
          4 : HasExStyle [WS_EX_RTLREADING] := Value;
          5 : begin HasExStyle [WS_EX_RIGHT] := Value; recreateRequired := True; end;
          6 : HasExStyle [WS_EX_LEFTSCROLLBAR] := Value;
        end
      end
  end;

  if frameChanged then
    SetWindowPos(ControlHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);

  if recreateRequired then
    RecreateWnd;
end;

procedure TControlInfo.SetStyle (st : DWORD; value : boolean);
begin
  if Value then
    SetMaskedStyle (st, 0)
  else
    SetMaskedStyle (0, st)
end;


procedure TControlInfo.SetWindowRect(const Value: TRect);
var
  r : TRect;
  frameWidth, frameHeight : Integer;
  titleHeight : Integer;
begin
  r := WindowRect;
  if value.Left   <> -1 then r.left   := Value.Left;
  if value.Top    <> -1 then r.top    := Value.Top;
  if value.Right  <> -1 then r.right  := Value.Right;
  if value.Bottom <> -1 then r.bottom := Value.Bottom;

  r := fOwner.DialogRectToRect (r);

  if Self = fOwner.fDialogInfo then
  begin
    frameWidth := GetSystemMetrics (SM_CXDLGFRAME);
    frameHeight := GetSystemMetrics (SM_CYDLGFRAME);
    titleHeight := GetSystemMetrics (SM_CYCAPTION);

    Inc (r.Bottom, 2 * frameHeight + titleHeight);
    Inc (r.Right, 2 * frameWidth);
  end;

  SetWindowPos (fControlHWnd, 0, r.left, r.top, r.right - r.left, r.bottom - r.top, SWP_NOZORDER);
end;

procedure TControlInfo.SetWindowText(const Value: WideString);
begin
  windows.SetWindowTextW (fControlHWND, PWideChar (Value));
end;

function TControlInfo.WindowProc(uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT;
begin
  result := fOwner.HandleControlMessage (self, uMsg, wParam, lParam);
end;

{ TResizeControl }

constructor TResizeControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks];
  FSizerWidth := 5;
end;

procedure TResizeControl.CreateParams(var Params: TCreateParams);
var
  r : TRect;

begin
  inherited;

  GetWindowRect (fControl.fControlHWND, r);
  MapWindowPoints (HWND_DESKTOP, GetParent (fControlHWND), r, 2);
  InflateRect (r, SizerWidth, SizerWidth);

  params.X := r.Left;
  params.Y := r.Top;
  params.Width := r.Right - r.Left;
  params.Height := r.Bottom - r.Top;
  params.Style := WS_VISIBLE or WS_CHILD;
  params.WindowClass.style := CS_VREDRAW + CS_HREDRAW + CS_DBLCLKS;
end;

procedure TResizeControl.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  RecalcSize;
end;

procedure TResizeControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fBasePt := point (x, y);

  // Keep BasePt in screen units
  MapWindowPoints (Handle, HWND_DESKTOP, fBasePt, 1);
  PtInSnib (point (x, y), fBaseSnib);
end;

procedure TResizeControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r, br : TRect;
  deltaX, deltaY : Integer;
  r1, r2 : HRGN;
  sizeChanged : boolean;
  p : TPoint;
  adjX, adjY : Integer;
const
  inMouseMove : Integer = 0;

  procedure AdjustRect (var r : TRect);
  var
    r1 : TRect;
  begin
    r1 := r;
    case fBaseSnib of
      stLT : begin Inc (r.Left, deltaX); Inc (r.Top, DeltaY); end;
      stMT : Inc (r.Top, DeltaY);
      stRT : begin Inc (r.Right, deltaX); Inc (r.Top, DeltaY); end;
      stLM : Inc (r.Left, DeltaX);
      stRM : Inc (r.Right, DeltaX);
      stLB : begin Inc (r.Left, deltaX); Inc (r.Bottom, DeltaY); end;
      stMB : Inc (r.Bottom, DeltaY);
      stRB : begin Inc (r.Right, deltaX); Inc (r.Bottom, DeltaY); end;

      stFrame :
        begin
          Inc (r.Left, DeltaX);
          Inc (r.Right, DeltaX);
          Inc (r.Top, DeltaY);
          Inc (r.Bottom, DeltaY)
        end
    end;

    if fControl = fDialogBox.fDialogInfo then
    begin
      r.Left := r1.Left;
      r.Top := r1.Top
    end

  end;

begin
  if ssLeft in Shift then
  begin
    p := Point (x, y);
    MapWindowPoints (handle, HWND_DESKTOP, p, 1);

    DeltaX := p.x - fBasePt.x;
    DeltaY := p.y - fBasePt.y;

    GetWindowRect (handle, r);
    MapWindowPoints (HWND_DESKTOP, ParentWindow, r, 2);

    InvalidateRect (parentWindow, Nil, True);

    br := r;
    AdjustRect (r);   // BR & R in parent units...

    fBasePt := p;
    if (br.Top <> r.Top) or (br.Bottom <> r.Bottom) or (br.Right <> r.Right) or (br.Left <> r.Left)then
    begin
      sizeChanged := ((r.bottom - r.Top) <> (br.Bottom - br.Top)) or ((r.Left - r.Right) <> (br.Left - br.Right));

      MoveWindow (Handle, r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top, False);

      if sizeChanged then  // Redo mask...
      begin
        br := r;        // br in control units
        MapWindowPoints (ParentWindow, Handle, br, 2);

        r1 := CreateRectRgnIndirect (br);
        InflateRect (br, -SizerWidth, -SizerWidth);
        r2 := CreateRectRgnIndirect (br);

        CombineRgn (r1, r1, r2, RGN_DIFF);
        DeleteObject (r2);
        SetWindowRgn (Handle, r1, False);

        if fControl = fDialogBox.fDialogInfo then
        begin

          GetWindowRect (fControlHWND, r);
          adjX := (br.Right - br.Left) - (r.right - r.left);
          adjY := (br.Bottom - br.Top) - (r.bottom - r.top);
          if adjX > 0 then
            DialogBox.WidthAdjust := adjX
          else
            DialogBox.WidthAdjust := 0;

          if adjY > 0 then
            DialogBox.HeightAdjust := adjY
          else
            DialogBox.HeightAdjust := 0;

          DialogBox.AdjustSize
        end
      end;
      UpdateWindow (parentWindow)
    end
  end
end;

procedure TResizeControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  r : TRect;
begin
  inherited;

  if Button = mbLeft then
  begin
    GetWindowRect (handle, r);
    MapWindowPoints (HWND_DESKTOP, parentWindow, r, 2);
    InflateRect (r, -SizerWidth, -SizerWidth);

    if fControl = fDialogBox.fDialogInfo then
    begin
      SetWindowPos (fControlHwnd, 0, r.Left, r.top, r.Right - r.Left, r.Bottom - r.Top, SWP_NOZORDER);
      DialogBox.WidthAdjust := 0;
      DialogBox.HeightAdjust := 0;
      DialogBox.AdjustSize
    end
    else
    begin
      SetWindowPos (fControlHwnd, 0, r.Left, r.top, r.Right - r.Left, r.Bottom - r.Top, SWP_NOZORDER or SWP_NOREDRAW);
      InvalidateRect (parentWindow, Nil, True)
    end;

    if Assigned (fControl.fOwner.OnControlResize) then
      fControl.fOwner.OnControlResize (fControl.fOwner, fControl, fControl.WindowRect);
  end
end;

procedure TResizeControl.PaintWindow(DC: HDC);
var
  r : TRect;
  w2, h2 : Integer;

  procedure DrawSnib (x, y : Integer);
  begin
    Rectangle (DC, x, y, x + SizerWidth, y + SizerWidth);
  end;

begin
  GetWindowRect (handle, r);
  MapWindowPoints (HWND_DESKTOP, Handle, r, 2);
  FillRect (DC, r, GetStockObject (GRAY_BRUSH));

  w2 := (ClientWidth - SizerWidth) div 2;
  h2 := (ClientHeight - SizerWidth) div 2;

  DrawSnib (r.Left, r.Top);
  DrawSnib (w2, r.Top);
  DrawSnib (r.Right - SizerWidth, r.Top);
  DrawSnib (r.Left, h2);
  DrawSnib (r.Right - SizerWidth, h2);
  DrawSnib (r.Left, ClientHeight - SizerWidth);
  DrawSnib (w2, ClientHeight - SizerWidth);
  DrawSnib (ClientWidth - SizerWidth, ClientHeight - SizerWidth);
end;

function TResizeControl.PtInSnib(pt: TPoint;
  var snibType: TSnibType): boolean;
var
  r, b, w2, h2 : Integer;
begin
  result := True;
  snibType := stFrame;

  r := ClientWidth - 1;
  b := ClientHeight - 1;
  w2 := (ClientWidth - SizerWidth) div 2;
  h2 := (ClientHeight - SizerWidth) div 2;

  if PtInRect (Rect (0, 0, r, SizerWidth), pt) then
  begin
    if pt.x <= sizerWidth then
      snibType := stLT
    else
      if pt.x >= ClientWidth - SizerWidth then
        snibType := stRT
      else
        if (pt.x >= w2) and (pt.x <= w2 + SizerWidth) then
          snibType := stMT
  end
  else
  if PtInRect (Rect (0, b - SizerWidth, r, b), pt) then
  begin
    if pt.x <= sizerWidth then
      snibType := stLB
    else
      if pt.x >= ClientWidth - SizerWidth then
        snibType := stRB
      else
        if (pt.x >= w2) and (pt.x <= w2 + SizerWidth) then
          snibType := stMB
  end
  else
    if PtInRect (Rect (0, h2, SizerWidth, h2 + SizerWidth), pt) then
      snibType := stLM
    else
      if PtInRect (Rect (r - sizerWidth, h2, r, h2 + SizerWidth), pt) then
        snibType := stRM
end;

procedure TResizeControl.RecalcSize;
var
  r1, r2 : HRgn;
  r : TRect;
begin
  GetWindowRect (fControl.fControlHWND, r);
  MapWindowPoints (HWND_DESKTOP, GetParent (fControlHWND), r, 2);
  InflateRect (r, SizerWidth, SizerWidth);
  SetWindowPos (handle, 0, r.left, r.top, r.right - r.left, r.bottom - r.top, SWP_NOZORDER or SWP_NOREDRAW);

  MapWindowPoints (parentWindow, handle, r, 2);

  r1 := CreateRectRgnIndirect (r);
  InflateRect (r, -SizerWidth, -SizerWidth);
  r2 := CreateRectRgnIndirect (r);

  CombineRgn (r1, r1, r2, RGN_DIFF);
  DeleteObject (r2);
  SetWindowRgn (Handle, r1, False);
  InvalidateRect (parentWindow, Nil, True);
end;

procedure TResizeControl.SetControl(Value: TControlInfo);
begin
  fControl := Value;
  fControlHWND := value.fControlHWND;

  if fControl = fDialogBox.fDialogInfo then
    parentWindow := fDialogBox.Handle
  else
    parentWindow := GetParent (fControl.fControlHWND);

end;

procedure TResizeControl.SetSizerWidth(const Value: Integer);
begin
  if FSizerWidth <> value then
  begin
    FSizerWidth := Value;
    RecreateWnd
  end
end;

procedure TResizeControl.WmPaint(var msg: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

{ TDialogControlInfo }

{ TStandardControlInfo }

function TStandardControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + StandardControlPropertyCount [kind]
end;

function TStandardControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := 0;
  end
end;

function TStandardControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
  end

end;

function TStandardControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
      pkGeneral : Result := StandardControlPropertyGeneralName [idx];
//      pkStyle   : Result := StandardControlPropertyStyleName [idx];
      pkExtended : Result := StandardControlPropertyExtendedName [idx];
    end
  end
end;

function TStandardControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
      pkGeneral : Result := StandardControlPropertyGeneralType [idx];
//      pkStyle   : Result := StandardControlPropertyStyleType [idx];
      pkExtended : Result := StandardControlPropertyExtendedType [idx];
    end
  end
end;

function TStandardControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral :
        case idx of
          0 : Result := fItemID;
          1 : Result := fHelpID <> -1;
          2 : Result := HasStyle [WS_GROUP];
          3 : Result := HasStyle [WS_TABSTOP];
        end;

      pkExtended :
        case idx of
          0 : Result := HasExStyle [WS_EX_DLGMODALFRAME]
        end;
    end
  end

end;

procedure TStandardControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  recreateRequired : Boolean;
  frameChanged : Boolean;
begin
  recreateRequired := False;
  frameChanged := False;
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral :
        case idx of
          0 : fItemID := Value;
          1 : if Value then fHelpID := fItemID else fHelpID := -1;
          2 : HasStyle [WS_GROUP] := Value;
          3 : HasStyle [WS_TABSTOP] := Value
        end;
      pkExtended :
        case idx of
          0 : begin HasExStyle [WS_EX_DLGMODALFRAME] := Value; frameChanged := True; end;
        end
    end
  end;

  if frameChanged then
    SetWindowPos(ControlHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);

  if recreateRequired then
    RecreateWnd
end;


{ TDialogControlInfo }

class function TDialogControlInfo.GetDescription: string;
begin
  result := rstDialog;
end;

function TDialogControlInfo.GetPropertyCount(kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + DialogControlPropertyCount [kind]
end;

function TDialogControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
    Result := 0;
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkStyle :
        case idx of
           1 : Result := 4;
          16 : Result := 3
        end
    end;
  end
end;

function TDialogControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
    Result := '';
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkStyle :
        case idx of
          1 :
            case enum of
              0 : Result := rstNone;
              1 : Result := rstThin;
              2 : Result := rstResizing;
              3 : Result := rstDialog
            end;
          16 :
            case enum of
              0 : Result := rstPopup;
              1 : Result := rstOverlapped;
              2 : Result := rstChild
            end
        end
    end
  end
end;

function TDialogControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
      pkGeneral : Result := DialogControlPropertyGeneralName [idx];
      pkStyle   : Result := DialogControlPropertyStyleName [idx];
      pkExtended : Result := DialogControlPropertyExtendedName [idx];
    end
  end
end;

function TDialogControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
      pkGeneral : Result := DialogControlPropertyGeneralType [idx];
      pkStyle   : Result := DialogControlPropertyStyleType [idx];
      pkExtended : Result := DialogControlPropertyExtendedType [idx];
    end
  end
end;

function TDialogControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral :
        case idx of
          0 : Result := WindowText;
          1 :
            Result := Format (rstFontDesc, [Owner.fFontPoint, Owner.fFontName]);
          2 : Result := SzOrIDToString (Owner.fDlgMenu);
        end;
      pkStyle :
        case idx of
            0 : Result := (fOwner.OrigStyle and DS_ABSALIGN) <> 0;
            1 : if HasStyle [DS_MODALFRAME] then
                  Result := 3
                else
                  if HasStyle [WS_THICKFRAME] then
                    Result := 2
                  else
                    if HasStyle [WS_BORDER] then
                      Result := 1
                    else
                      Result := 0;
            2 : Result := (fOwner.OrigStyle and DS_CENTER) <> 0;
            3 : Result := (fOwner.OrigStyle and DS_CENTERMOUSE) <> 0;
            4 : Result := HasStyle [WS_CLIPCHILDREN];
            5 : Result := HasStyle [WS_CLIPSIBLINGS];
            6 : Result := HasStyle [DS_CONTEXTHELP];
            7 : Result := HasStyle [DS_CONTROL];
            8 : Result := HasStyle [DS_LOCALEDIT];
            9 : Result := HasStyle [WS_MAXIMIZEBOX];
           10 : Result := HasStyle [WS_MINIMIZEBOX];
           11 : Result := HasStyle [DS_NOFAILCREATE];
           12 : Result := HasStyle [DS_NOIDLEMSG];
           13 : Result := HasStyle [WS_HSCROLL];
           14 : Result := HasStyle [WS_VSCROLL];
           15 : Result := HasStyle [DS_SETFOREGROUND];
           16 : if ((fOwner.OrigStyle and WS_POPUP) <> 0) then
                 Result := 0
               else
                 if ((fOwner.OrigStyle and WS_CHILD) <> 0) then
                   Result := 2
                 else
                   Result := 1;
           17 : Result := HasStyle [WS_SYSMENU];
           18 : Result := HasStyle [DS_SYSMODAL];
           19 : Result := HasStyle [WS_CAPTION];
        end;
      pkExtended :
        case idx of
          0 : Result := HasExStyle [WS_EX_CONTROLPARENT];
          1 : Result := HasExStyle [WS_EX_NOPARENTNOTIFY];
          2 : Result := HasExStyle [WS_EX_TOOLWINDOW];
        end
    end;
  end
end;

procedure TDialogControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  recreateRequired : Boolean;
begin
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    recreateRequired := False;
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral :
        case idx of
          0 : WindowText := Value;
          1 : ; // ptSpecial property - handled differently
          2 : Owner.fDlgMenu := StringToSzOrID (Value)
        end;

      pkStyle :
        case idx of
          0 : fOwner.OrigStyle := MaskDWORD (fOwner.OrigStyle, DS_ABSALIGN, Value);
          1 :
            begin
              case Value of
                0 : SetMaskedStyle (0, WS_CAPTION or WS_THICKFRAME or DS_MODALFRAME);
                1 : SetMaskedStyle (WS_BORDER, WS_BORDER or WS_THICKFRAME or DS_MODALFRAME);
                2 : if HasStyle [WS_CAPTION] then
                      SetMaskedStyle (WS_THICKFRAME, DS_MODALFRAME)
                    else
                      SetMaskedStyle (WS_THICKFRAME, WS_BORDER or DS_MODALFRAME);

                3 : if HasStyle [WS_CAPTION] then
                      SetMaskedStyle (DS_MODALFRAME, WS_THICKFRAME)
                    else
                      SetMaskedStyle (DS_MODALFRAME, WS_BORDER or WS_THICKFRAME);
              end;
              recreateRequired := True
            end;

          2 : fOwner.OrigStyle := MaskDWORD (fOwner.OrigStyle, DS_CENTER, Value);
          3 : fOwner.OrigStyle := MaskDWORD (fOwner.OrigStyle, DS_CENTERMOUSE, Value);
          4 : begin HasStyle [WS_CLIPCHILDREN] := Value; RecreateRequired := True end;
          5 : HasStyle [WS_CLIPSIBLINGS] := Value;
          6 : HasStyle [DS_CONTEXTHELP] := Value;
          7 : HasStyle [DS_CONTROL] := Value;
          8 : HasStyle [DS_LOCALEDIT] := Value;
          9 : HasStyle [WS_MAXIMIZEBOX] := Value;
          10 : HasStyle [WS_MINIMIZEBOX] := Value;
          11 : HasStyle [DS_NOFAILCREATE] := Value;
          12 : HasStyle [DS_NOIDLEMSG] := Value;
          13 : begin HasStyle [WS_HSCROLL] := Value; RecreateRequired := True end;
          14 : begin HasStyle [WS_VSCROLL] := Value; RecreateRequired := True end;
          15 : HasStyle [DS_SETFOREGROUND] := Value;
          16 :
            case Value of
              0 : fOwner.OrigStyle := (fOwner.OrigStyle and not $c0000000) or WS_POPUP;
              1 : fOwner.OrigStyle := (fOwner.OrigStyle and not $c0000000) or WS_OVERLAPPED;
              2 : fOwner.OrigStyle := (fOwner.OrigStyle and not $c0000000) or WS_CHILD
            end;
          17 : HasStyle [WS_SYSMENU] := Value;
          18 : HasStyle [DS_SYSMODAL] := Value;
          19 : begin HasStyle [WS_CAPTION] := Value; RecreateRequired := True end;
        end;
      pkExtended :
        case idx of
          0 : HasExStyle [WS_EX_CONTROLPARENT] := Value;
          1 : HasExStyle [WS_EX_NOPARENTNOTIFY] := Value;
          2 : begin HasExStyle [WS_EX_TOOLWINDOW] := Value; RecreateRequired := True end;
        end
    end;
    if recreateRequired then
      RecreateWnd
  end
end;

procedure TDialogEditor.SaveToStream(stream: TStream);
var
  Extended : Boolean;
  i : Integer;
  Style : DWORD;
  template : TDlgTemplate;
  templateEx : TDlgTemplateEx;
  itemTemplate : TdlgItemTemplate;
  itemTemplateEx : TdlgItemTemplateEx;
  r : TRect;
  w : Word;
  ws : WideString;
  b : Byte;
  szOrID : TSzOrId;
  info : TControlInfo;

begin
                // Use extended template if:
                // 1.  The original template was extended (get rid of this!)
                // 2.  The dialog, or any control has a help ID
                // 3.  The dialog has a font with a non-standard weight, italic, charset

  Extended := ExtendedTemplate or (fDialogInfo.fHelpID <> -1) or fFontItalic or (fFontWeight <> FW_NORMAL);

  if not Extended then
  begin
    for i := 0 to ControlInfoCount - 1 do
      if ControlInfo [i].fHelpID <> -1 then
      begin
        Extended := True;
        break
      end
  end;
                        // Get style with correct popup / overlapped / child part.

  Style := (fDialogInfo.GetStyle and not (WS_CHILD or WS_VISIBLE)) or (OrigStyle and (WS_CHILD or WS_POPUP or DS_CENTER or WS_VISIBLE));

  if fFontName <> '' then
    Style := Style or DS_SETFONT;

  r := fDialogInfo.WindowRect;


  if Extended then
  begin                 // Set up and save extended template
    templateEx.dlgVer := 1;
    templateEx.signature := $ffff;
    templateEx.helpID := fDialogInfo.fHelpID;
    templateEx.exStyle := fDialogInfo.GetExStyle;
    templateEx.Style := Style;
    templateEx.cDlgItems := ControlInfoCount;
    templateEx.x := OrigX;
    templateEx.Y := OrigY;
    templateEx.cx := r.Right - r.Left;
    templateEx.cy := r.Bottom - r.Top;
    stream.Write (templateEx, SizeOf (templateEx));
  end
  else
  begin                 // Set up and save standard template
    template.Style := Style;
    template.dwExtendedStyle := fDialogInfo.GetExStyle;
    template.cdit := ControlInfoCount;
    template.x := OrigX;
    template.Y := OrigY;
    template.cx := r.Right - r.Left;
    template.cy := r.Bottom - r.Top;
    stream.Write (template, SizeOf (template));
  end;

                        // Write menu, class title sz or ids
  WriteSzOrID (stream, fDlgMenu);
  WriteSzOrID (stream, fDlgClass);

  szOrID.isID := False;
  szOrID.sz := fDialogInfo.WindowText;
  WriteSzOrID (stream, szOrId);

                        // Write additional font stuff.
  if (Style and DS_SETFONT) <> 0 then
  begin
    w := fFontPoint;
    stream.Write (w, SizeOf (w));

    if Extended then
    begin
      w := fFontWeight;
      stream.write (w, SizeOf (w));

      b := Byte (fFontItalic);
      stream.Write (b, SizeOf (b));

      b := Byte (fFontCharset);
      stream.Write (b, SizeOf (b))
    end;

    ws := fFontName;
    stream.Write (ws [1], (Length (ws) + 1) * SizeOf (WideChar));
  end;

  for i := 0 to ControlInfoCount - 1 do
  begin
    Pad (stream);

    info := ControlInfo [i];

    r := info.WindowRect;

    if Extended then
    begin
      itemTemplateEx.helpID := info.fItemID;
      itemTemplateEx.exStyle := info.GetExStyle;
      itemTemplateEx.Style := info.GetStyle;
      itemTemplateEx.x := r.Left;
      itemTemplateEx.y := r.Top;
      itemTemplateEx.cx := r.Right - r.Left;
      itemTemplateEx.cy := r.Bottom - r.Top;
      itemTemplateEx.id := info.fItemID;

      stream.Write (itemTemplateEx, SizeOf (itemTemplateEx));
      pad (stream);
    end
    else
    begin
      itemTemplate.Style := info.GetStyle;
      itemTemplate.dwExtendedStyle := info.GetExStyle;
      itemTemplate.x := r.Left;
      itemTemplate.y := r.Top;
      itemTemplate.cx := r.Right - r.Left;
      itemTemplate.cy := r.Bottom - r.Top;
      itemTemplate.id := info.fItemID;
      stream.Write (itemTemplate, SizeOf (itemTemplate));
    end;

    WriteSzOrID (stream, info.GetClassSzOrID);

    szOrID.isID := False;

    if info.IsGraphicControl then
      szOrID := info.fTitleSzOrID
    else
      szOrID.sz := info.WindowText;

    WriteSzOrID (stream, szOrId);

    w := info.fExtraCount;
    stream.Write (w, SizeOf (w));

    if w > 0 then
      stream.Write (info.fExtraData^, w);
  end
end;

procedure TDialogEditor.RecreateDlg;
begin
  PostMessage (Handle, WM_RECREATEDLG, 0, 0);
end;

procedure TDialogEditor.WmRecreateDlg (var Msg : TMessage);
var
  s : TMemoryStream;
  i, selId : Integer;
begin
  s := TMemoryStream.Create;
  try
    selID := SelectedControl.fItemID;
    SetSelectedControl (Nil);
    SaveToStream (s);
    ResourceTemplate := s.Memory;

    for i := 0 to ControlInfoCount - 1 do
      if ControlInfo [i].ItemID = selID then
      begin
        SetSelectedControl (ControlInfo [i]);
        exit
      end
  finally
    s.Free
  end
end;

procedure TDialogEditor.WmCtrlPropertyChange(var Msg: TMessage);
begin
  if Assigned (fOnControlPropertyChange) then
    OnControlPropertyChange (self, TControlInfo (Msg.wParam))
end;

procedure TDialogEditor.DeleteControl(control: TControlInfo);
var
  wnd : HWND;
begin
  if control <> DialogInfo then
  begin
    SelectedControl := DialogInfo;
    wnd := control.fControlHWND;
    fControlInfoList.Remove(control);
    if Assigned (OnDeleteControl) then
      OnDeleteControl (self, control);
    control.Free;
    DestroyWindow (wnd);
  end
end;

end.
