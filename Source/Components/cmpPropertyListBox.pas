(*======================================================================*
 | cmpPropertyListBox unit Resource Editor Components                   |
 |                                                                      |
 | Display/Edit a list of properties                                    |
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
 | Copyright © Colin Wilson 2002.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      08/02/2002  CPWW  Original                                  |
 *======================================================================*)

unit cmpPropertyListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, buttons, cmpFakeCombobox;

const
  WM_INIT = WM_USER + $201;

type
  TPropertyListBox = class;

  TPropertyType = (ptString, ptInteger, ptBoolean, ptEnum, ptSpecial);

  //--------------------------------------------------------------------
  // TPropertyListProperty class

  TPropertyListProperty = class (TCollectionItem)
  private
    fTag: Integer;
    fPropertyType: TPropertyType;
    fEnumValues : TStrings;
    fEnabled: Boolean;
    fOnSpecialButtonClick: TNotifyEvent;
    fActualValue: variant;
    fParentColor: Boolean;
    fColor: TColor;
    fReadOnly : Boolean;
    function GetValueAsStr: string;
    procedure SetPropertyValue(Value: variant);
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure SetEnabled(const Value: Boolean);
    procedure SetPropertyName(const Value: string);
    procedure SetPropertyType(const Value: TPropertyType);
    procedure SetActualValue(Value: variant);
    function GetActualValueAsStr: string;
    procedure SetColor(const Value: TColor);
    procedure SetParentColor(const Value: Boolean);
  private
    fPropertyName: string;
    fPropertyValue : variant;

    property ValueAsStr : string read GetValueAsStr;
    property ActualValueAsStr : string read GetActualValueAsStr;
  public
    constructor Create (collection : TCollection); override;
    destructor Destroy; override;
    property PropertyValue : variant read fPropertyValue write SetPropertyValue;
    property ActualValue : variant read fActualValue write SetActualValue;
    procedure IncValue;
    procedure DecValue;
  published
    property PropertyName : string read fPropertyName write SetPropertyName;
    property PropertyType : TPropertyType read fPropertyType write SetPropertyType;
    property Tag : Integer read fTag write fTag;
    property EnumValues : TStrings read GetStrings write SetStrings;
    property Enabled : Boolean read fEnabled write SetEnabled default True;
    property OnSpecialButtonClick : TNotifyEvent read fOnSpecialButtonClick write fOnSpecialButtonClick;
    property ParentColor : Boolean read fParentColor write SetParentColor;
    property Color : TColor read fColor write SetColor;
    property ReadOnly : Boolean read fReadOnly write fReadOnly;
  end;

  //--------------------------------------------------------------------
  // TPropertyListProperties class
  TPropertyListProperties = class (TOwnedCollection)
  private
    fParent: TPropertyListBox;
    function GetItem(index: Integer): TPropertyListProperty;
    procedure SetItem(index: Integer; const Value: TPropertyListProperty);
  public
    property Parent : TPropertyListBox read fParent;
    property Items [index : Integer] : TPropertyListProperty read GetItem write SetItem; default;
    procedure EndUpdate; override;
  end;

  //--------------------------------------------------------------------
  // TPropertyListBox class

  TPropertyEditEvent = procedure (Sender : TObject; prop : TPropertyListProperty) of object;
  TPropertyListBox = class(TScrollingWinControl)
  private
    fProperties: TPropertyListProperties;
    fCanvas : TCanvas;
    fNameColWidth: Integer;
    fBorderStyle: TBorderStyle;
    fSelectedPropertyNo: Integer;
    fLineHeight : Integer;
    fPropertyEdit : TFakeCombobox;
    fOnPropertyChanged: TNotifyEvent;
    fFirstCreate : Boolean;
    fChangeWidthX : Integer;
    fChangingWidth : Boolean;
    fChangingColWidth : Integer;
    fActualValueColWidth: Integer;
    fChangingActualValueWidth : Boolean;
    fOnBeginPropertyEdit: TPropertyEditEvent;
    fOnEndPropertyEdit: TPropertyEditEvent;
    procedure WmPaint (var Message : TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure RecalcScrollbars;
    procedure SetNameColWidth(const Value: Integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetSelectedPropertyNo(Value: Integer);
    procedure SetPropertyEdit;

    procedure DoOnPropertyEditExit (Sender : TObject);
    procedure DoOnPropertyEditKeyDown (Sender : TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnPropertyEditDblClick (Sender : TObject);
    procedure DoOnPropertyEditSpecialButtonClick (Sender : TObject);
    procedure PropertyChanged;
    procedure WmInit (var Msg : TMessage); message WM_INIT;

    function GetPropertyEditText : string;
    procedure SetPropertyEditText (const Value : string);

    property PropertyEditText : string read GetPropertyEditText write SetPropertyEditText;
    procedure SetActualValueColWidth(const Value: Integer);
    function GetPropertyValue(const propName: string): Variant;
    procedure SetPropertyValue(const propName: string;
      const Value: Variant);
  protected
    procedure PaintWindow (DC : HDC); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoEnter; override;

    procedure Paint;
    property Canvas : TCanvas read fCanvas;
    procedure Resize; override;
    procedure CreateParams (var Params : TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    function FindProperty (const propName : string) : TPropertyListProperty;
    property SelectedPropertyNo : Integer read fSelectedPropertyNo write SetSelectedPropertyNo;
    procedure Reset;
    property PropertyValue [const propName : string] : Variant read GetPropertyValue write SetPropertyValue;
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property BiDiMode;
    property BorderStyle : TBorderStyle read fBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;

    property Properties : TPropertyListProperties read fProperties write fProperties;
    property NameColWidth : Integer read fNameColWidth write SetNameColWidth default 90;
    property ActualValueColWidth : Integer read fActualValueColWidth write SetActualValueColWidth;
    property OnPropertyChanged : TNotifyEvent read fOnPropertyChanged write fOnPropertyChanged;
    property OnBeginPropertyEdit : TPropertyEditEvent read fOnBeginPropertyEdit write fOnBeginPropertyEdit;
    property OnEndPropertyEdit : TPropertyEditEvent read fOnEndPropertyEdit write fOnEndPropertyEdit;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses Variants;

const
  rstTrue = 'True';
  rstFalse = 'False';
  rstError = 'Error';

{ TPropertyListBox }

(*----------------------------------------------------------------------*
 | TPropertyListBox.Create                                              |
 |                                                                      |
 | Constructor for TPropertyListBox                                     |
 |                                                                      |
 | Parameters:                                                          |
 |   AOwner: TComponent                                                 |
 *----------------------------------------------------------------------*)
constructor TPropertyListBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse,csClickEvents, csSetCaption, csDoubleClicks, csOpaque];
  fFirstCreate := True;
  fBorderStyle := bsSingle;
  Width := 180;
  fNameColWidth := 90;
  Height := 120;
  fCanvas := TControlCanvas.Create;
  TControlCanvas (fCanvas).Control := Self;
  fProperties := TPropertyListProperties.Create (Self, TPropertyListProperty);
  fProperties.fParent := self;
  DoubleBuffered := True;  // Get rid of ghastly flicker!
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.CreateParams                                        |
 |                                                                      |
 | Override CreateParams to set the border style                        |
 |                                                                      |
 | Parameters:                                                          |
 |   var Params: TCreateParams                                          |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;

  if fBorderStyle = bsSingle then
    Params.Style := params.Style or WS_BORDER;
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.CreateWnd                                           |
 |                                                                      |
 | Perform initialization after first creating the window.              |
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.CreateWnd;
var
  p : Integer;
begin
  inherited;
  RecalcScrollBars;
  if fFirstCreate then
  begin
    fFirstCreate := False;
    p := fSelectedPropertyNo;
    fSelectedPropertyNo := -1;

    if GetParentForm (Self).ActiveControl = Self then
      PostMessage (Handle, WM_INIT, p, 0);
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.Destroy                                             |
 |                                                                      |
 | Destructor for TPropertyListBox                                      |
 *----------------------------------------------------------------------*)
destructor TPropertyListBox.Destroy;
begin
  fProperties.Free;
  fCanvas.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.DoOnPropertyEditDblClick                            |
 |                                                                      |
 | OnDblClick handler for Property List Box in Edit mode.  If it's an   |
 | enumerated box, cycle through to the next value.                     |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 |                                                                      |
 | The function returns None                                            |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.DoEnter;
begin
  inherited;

  if fSelectedPropertyNo = -1 then
    SelectedPropertyNo := 0
  else
    if Assigned (fPropertyEdit) then
      fPropertyEdit.SetFocus
end;

procedure TPropertyListBox.DoOnPropertyEditDblClick(Sender: TObject);
var
  prop : TPropertyListProperty;
  i : Integer;
begin
  if fSelectedPropertyNo >= fProperties.Count then Exit;
  prop := fProperties [fSelectedPropertyNo];
  if prop.ReadOnly then Exit;

  case prop.fPropertyType of
    ptBoolean :
      begin
        prop.PropertyValue := not prop.PropertyValue;
        PropertyChanged
      end;
    ptEnum :
      begin
        i := prop.PropertyValue;
        if i = prop.fEnumValues.Count - 1 then
          prop.PropertyValue := 0
        else
          prop.IncValue;
        PropertyChanged
      end
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.DoOnPropertyEditExit                                |
 |                                                                      |
 | The edit box has been exited.  This could be becaue the list-box     |
 | has dropped down - so din't simply set the focus to the property     |
 | list!                                                                |
 |                                                                      |
 | Parameters:                                                          |
 |   Sender: TObject                                                    |
 *----------------------------------------------------------------------*)

procedure TPropertyListBox.DoOnPropertyEditExit(Sender: TObject);
var
  prop : TPropertyListProperty;
  st : string;
begin
  if Assigned (fPropertyEdit) then
  begin
    if fSelectedPropertyNo >= fProperties.Count then Exit;

    prop := fProperties [fSelectedPropertyNo];
    st := GetPropertyEditText;

    if Assigned (OnEndPropertyEdit) then
      OnEndPropertyEdit (Self, prop);

    if prop.ValueAsStr <> st then
    begin
      prop.SetPropertyValue (st);
      PropertyChanged
    end
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.DoOnPropertyEditKeyDown                             |
 |                                                                      |
 | In 'Edit' mode go to the next or previous property if up or down     |
 | is pressed.                                                          |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.DoOnPropertyEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  saveKey : Word;
begin
  if csDesigning in ComponentState then
    Exit;

  saveKey := key;
  key := 0;


  case saveKey of
    VK_UP   : if SelectedPropertyNo > 0 then
                  SelectedPropertyNo := SelectedPropertyNo - 1;

    VK_DOWN : if SelectedPropertyNo < fProperties.Count - 1 then
                SelectedPropertyNo := SelectedPropertyNo + 1;

    else
    begin
      if Assigned (OnBeginPropertyEdit) then
        OnBeginPropertyEdit (Self, nil);
      key := saveKey
    end
  end
end;


(*----------------------------------------------------------------------*
 | TPropertyListBox.DoOnPropertyEditSpecialButtonClick                  |
 |                                                                      |
 | The drop-down or special button has been clicked                     |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.DoOnPropertyEditSpecialButtonClick(
  Sender: TObject);
var
  prop : TPropertyListProperty;
begin
  if fSelectedPropertyNo >= fProperties.Count then Exit;
  prop := fProperties [fSelectedPropertyNo];
  if prop.ReadOnly then Exit;

  if Assigned (prop.OnSpecialButtonClick) then
    prop.OnSpecialButtonClick (prop)
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.FindProperty                                        |
 |                                                                      |
 | Find a property by name.                                             |
 |                                                                      |
 | Parameters:                                                          |
 |   const propName: string             The property to find            |
 |                                                                      |
 | The function returns the found property or nil                       |
 *----------------------------------------------------------------------*)
function TPropertyListBox.FindProperty(
  const propName: string): TPropertyListProperty;
var
  i : Integer;
begin
  result := nil;
  for i := 0 to fProperties.Count - 1 do
    if CompareText (fProperties [i].PropertyName, propName) = 0 then
    begin
      result := fProperties [i];
      break
    end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.GetPropertyEditText                                 |
 |                                                                      |
 | Get method for PropertyEditText property                             |
 |                                                                      |
 | The function returns th text in the proerty editor.                  |
 *----------------------------------------------------------------------*)
function TPropertyListBox.GetPropertyEditText: string;
begin
  Result := fPropertyEdit.Text;
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.MouseDown                                           |
 |                                                                      |
 | Mouse clicked in the property list box.                              |
 |                                                                      |
 | Parameters:                                                          |
 |   Button: TMouseButton; Shift: TShiftState; X, Y: Integer            |
 *----------------------------------------------------------------------*)
function TPropertyListBox.GetPropertyValue(
  const propName: string): Variant;
var
  prop : TPropertyListProperty;
begin
  prop := FindProperty (propName);
  if Assigned (prop) then
    result := prop.PropertyValue
  else
    raise Exception.Create ('Property ' + propName + ' not found');
end;

procedure TPropertyListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  offset : Integer;
  pt : TPoint;
  r : TRect;
  sel : Integer;
  ctrl : TWinControl;

begin
  inherited;

  ctrl := GetParentForm (Self).ActiveControl;
  while (ctrl <> Self) and (ctrl <> nil) do
    ctrl := ctrl.Parent;

  if not Assigned (ctrl) then           // We're not active - and neither are our children
    if SelectedPropertyNo = 0 then      // So tidy up display glitch!
      fSelectedPropertyNo := -1;

  SetFocus;
  pt.x := x;
  pt.y := y;

  r := ClientRect;              // R contains divider between property names and values
  r.Left := NameColWidth - 1;
  r.Right := r.Left + 3;

  if PtInRect (r, pt) then      // Clicked on divider
  begin
    fChangingWidth := True;
    SetCapture (Handle);
    fChangeWidthX := x;
    fChangingColWidth := NameColWidth;
    exit
  end;

  if fActualValueColWidth > 0 then
  begin
    r := ClientRect;
    r.Left := NameColWidth + 4 + ActualValueColWidth - 1;
    r.Right := r.Left + 3;

    if PtInRect (r, pt) then
    begin
      fChangingActualValueWidth := True;
      SetCapture (Handle);
      fChangeWidthx := x;
      fChangingColWidth := ActualValueColWidth;
      exit
    end
  end;

                        // Clicked on the name or value.
  offset := VertScrollBar.Position;

  Inc (y, Offset);

  sel := y div fLineHeight;   // Select the correct property

  if sel < fProperties.Count then
    SelectedPropertyNo := sel
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.MouseMove                                           |
 |                                                                      |
 | Mouse move in property lit box.                                      |
 |                                                                      |
 | Parameters:
 |   Shift: TShiftState; X, Y: Integer
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r : TRect;
  pt : TPoint;
begin
  if fChangingWidth then        // We're adjusting the column widths.  Do it!
  begin
    NameColWidth := fChangingColWidth + x - fChangeWidthX;
    exit
  end;

  if fChangingActualValueWidth then
  begin
    ActualValueColWidth := fChangingColWidth + x - fChangeWidthX;
    exit
  end;

  pt.x := x;
  pt.y := y;

  r := ClientRect;            // Otherwise just ensure that the correct cursor
  r.Left := NameColWidth - 1; // is being displayed.
  r.Right := r.Left + 3;
  if PtInRect (r, pt) then
    Cursor := crHSplit
  else
  begin
    r := ClientRect;
    r.Left := NameColWidth + 4 + ActualValueColWidth - 1;
    r.Right := r.Left + 3;
    if ptINRect (r, pt) then
      Cursor := crHSplit
    else
      Cursor := crDefault
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.MouseUp                                             |
 |                                                                      |
 | Mouse released.  Finalize updated column width, etc.                 |
 |                                                                      |
 | Parameters:                                                          |
 |   Button: TMouseButton; Shift: TShiftState; X, Y: Integer            |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if fChangingWidth then
  begin
    fChangingWidth := False;
    ReleaseCapture
  end;

  if fChangingActualValueWidth then
  begin
    fChangingActualValueWidth := False;
    ReleaseCapture
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.Paint                                               |
 |                                                                      |
 | Paint the property list box.                                         |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.Paint;
var
  x, i, offset, y, indent : Integer;
  prop : TPropertyListProperty;
  r : TRect;
  oldFontColor : TColor;
begin
  if fLineHeight = 0 then Exit;
  if fPropertyEdit = Nil then
    SetPropertyEdit;
  offset := VertScrollBar.Position;
  Canvas.Font := Font;
  Canvas.Font.Color := Font.Color;
  Canvas.Brush.Color := Color;
  indent := 8;

  y := 0;
  i := 0;
  while y < ClientHeight + offset + fLineHeight do
  begin
    if y >= offset - fLineHeight then
    begin
      if i < Properties.Count then
      begin
        prop := fProperties [i];

        r.left := 0;
        r.right := ClientWidth - 1;
        r.top := y - offset + 1;
        r.bottom := r.top + fLineHeight - 1;

        // Properties with 'null' values are disabled.
        oldFontColor := Canvas.Font.Color;

        if not prop.ParentColor then
          oldFontColor := prop.Color;

        try
          if VarIsEmpty (prop.PropertyValue) or ((ActualValueColWidth > 0) and VarIsEmpty (prop.ActualValue)) then
            Canvas.Font.Color := clGrayText
          else
            Canvas.Font.Color := oldFontColor;
          Canvas.TextRect (r, indent, y - offset + 2, prop.PropertyName);

          if ActualValueColWidth > 0 then
          begin
            Canvas.TextOut(NameColWidth + Indent, y - offset, prop.ActualValueAsStr);
            x := NameColWidth + 2 * Indent + ActualValueColWidth
          end
          else
            x := NameColWidth + Indent;

          Canvas.TextOut (x, y - offset, prop.ValueAsStr);
        finally
          Canvas.Font.Color := oldFontColor
        end;

        if i = SelectedPropertyNo then
          Frame3d (Canvas, r, clBtnShadow, clBtnHighlight, 1);

        Inc (i)
      end;
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo (0, y - offset);
      Canvas.LineTo (ClientWidth, y - offset);
    end
    else
      Inc (i);

    Inc (y, fLineHeight);
  end;

  r := ClientRect;
  r.Left := NameColWidth;
  r.Right := r.Left;
  DrawEdge (Canvas.Handle, r, EDGE_ETCHED, BF_LEFT);

  if ActualValueColWidth > 0 then
  begin
    r := ClientRect;
    r.Left := NameColWidth + ActualValueColWidth;
    r.Right := r.Left;
    DrawEdge (Canvas.Handle, r, EDGE_ETCHED, BF_LEFT)
  end;
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.PaintWindow                                         |
 |                                                                      |
 | Paint                                                                |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.PropertyChanged                                     |
 |                                                                      |
 | Update display to relect new property value                          |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.PropertyChanged;
var
  prop : TPropertyListProperty;
begin
  if fSelectedPropertyNo >= fProperties.Count then exit;
  prop := fproperties [fSelectedPropertyNo];
  if Assigned (fPropertyEdit) then
    PropertyEditText := prop.ValueAsStr;

  Invalidate;

  if Assigned (OnPropertyChanged) and not (csDestroying in ComponentState) then
    OnPropertyChanged (self)
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.RecalcScrollbars                                    |
 |                                                                      |
 | Work out the scroll bars.                                            |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.RecalcScrollbars;
begin
  fLineHeight := Canvas.TextHeight ('M') + 4;
  VertScrollBar.Range := fLineHeight * fProperties.Count;
  VertScrollBar.Increment := fLineHeight
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.Reset                                               |
 |                                                                      |
 | Reset the property list box.                                         |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.Reset;
begin
  fSelectedPropertyNo := -1;
  SetPropertyEdit;
  Invalidate
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.Resize                                              |
 |                                                                      |
 | Property List Box resized.  Adjust the scroll bars.                  |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.Resize;
begin
  inherited;
  RecalcScrollBars;
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.SetBorderStyle                                      |
 |                                                                      |
 | Set method for BorderStyle property                                  |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.SetActualValueColWidth(const Value: Integer);
begin
  if (Value <> fActualValueColWidth) and (Value >= 0) and (Value < ClientWidth) then
  begin
    fActualValueColWidth := Value;

    if Assigned (fPropertyEdit) then
    begin
      fPropertyEdit.Left := fActualValueColWidth + fNameColWidth + 2;
      fPropertyEdit.Width := ClientWidth - fNameColWidth - fActualValueColWidth - 2;
      if fActualValueColWidth > 0 then
      begin
        fPropertyEdit.Left := fPropertyEdit.Left + 2;
        fPropertyEdit.Width := fPropertyEdit.Width - 2
      end
    end;
    Invalidate;
  end
end;

procedure TPropertyListBox.SetBorderStyle(const Value: TBorderStyle);
begin
  if value <> fBorderStyle then
  begin
    fBorderStyle := Value;
    RecreateWnd
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.SetNameColWidth                                     |
 |                                                                      |
 | Set the divider position                                             |
 *----------------------------------------------------------------------*)

procedure TPropertyListBox.SetNameColWidth(const Value: Integer);
begin
  if (value <> fNameColWidth) and (Value > 0) and (Value < ClientWidth) then
  begin
    fNameColWidth := Value;
    if Assigned (fPropertyEdit) then
    begin
      fPropertyEdit.Left := fActualValueColWidth + fNameColWidth + 2;
      fPropertyEdit.Width := ClientWidth - fNameColWidth - fActualValueColWidth - 2;
      if fActualValueColWidth > 0 then
      begin
        fPropertyEdit.Left := fPropertyEdit.Left + 2;
        fPropertyEdit.Width := fPropertyEdit.Width - 2
      end
    end;
    Invalidate;
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.SetPropertyEdit                                     |
 |                                                                      |
 | Display the editor in the currently selected property                |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.SetPropertyEdit;
var
  prop : TPropertyListProperty;
  n : Integer;
  st : string;
  parentForm : TCustomForm;
begin
  parentForm := GetParentForm (Self);
  if Assigned (parentForm) and (parentForm.ActiveControl = Self) then
  begin
    FreeAndNil (fPropertyEdit);

    if csDesigning in ComponentState then Exit;
    if (SelectedPropertyNo < 0) or (fSelectedPropertyNo >= fProperties.Count) then
      Exit;

    prop := fProperties [fSelectedPropertyNo];
    if prop.ReadOnly then Exit;

    fPropertyEdit := TFakeCombobox.Create (Self);
    fPropertyEdit.OnExit := DoOnPropertyEditExit;
    fPropertyEdit.OnKeyDown := DoOnPropertyEditKeyDown;
    fPropertyEdit.OnDblClick := DoOnPropertyEditDblClick;
    fPropertyEdit.OnSpecialButtonClick := DoOnPropertyEditSpecialButtonClick;
    fPropertyEdit.Parent := Self;

    fPropertyEdit.Height := fLineHeight;
    fPropertyEdit.Left := fNameColWidth + 2;
    fPropertyEdit.Top := fSelectedPropertyNo * fLineHeight - VertScrollBar.Position;
    fPropertyEdit.Width := ClientWidth - fNameColWidth - 2;

    if fActualValueColWidth > 0 then
    begin
      fPropertyEdit.Left := fPropertyEdit.Left + fActualValueColWidth + 2;
      fPropertyEdit.Width := fPropertyEdit.Width - fActualValueColWidth - 2
    end;

    st := prop.ValueAsStr;

    case prop.PropertyType of
      ptEnum :
        for n := 0 to prop.fEnumValues.Count - 1 do
          fPropertyEdit.Items.Add (prop.fEnumValues [n]);
      ptBoolean :
        begin
          fPropertyEdit.Items.Add (rstFalse);
          fPropertyEdit.Items.Add (rstTrue)
        end;
      ptSpecial :
        begin
          fPropertyEdit.ReadOnly := True;
          fPropertyEdit.SpecialButton := True
        end
    end;

    PropertyEditText := st;
    fPropertyEdit.Show;
    fPropertyEdit.SetFocus
  end
  else
    FreeAndNil (fPropertyEdit);
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.SetPropertyEditText                                 |
 |                                                                      |
 | Set method for ProperyEditText property                              |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.SetPropertyEditText(const Value: string);
begin
  fPropertyEdit.Text := Value
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.SetSelectedPropertyNo                               |
 |                                                                      |
 | Set method for SelectedPropertyNo property                           |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.SetPropertyValue(const propName: string;
  const Value: Variant);
var
  prop : TPropertyListProperty;
begin
  prop := FindProperty (propName);
  if Assigned (prop) then
    prop.PropertyValue := Value
  else
    raise Exception.Create ('Property ' + propName + ' not found');
end;

procedure TPropertyListBox.SetSelectedPropertyNo(Value: Integer);
begin
  if Value <> -1 then
  begin
    if Value > fSelectedPropertyNo then         // Skip disabled properties
    begin
      while (Value < Properties.Count) and not Properties [Value].Enabled do
        Inc (Value);

      if Value = Properties.Count then Value := -1
    end
    else
      while (Value >= 0) and not Properties [Value].Enabled do
        Dec (Value)
  end;

  if value <> fSelectedPropertyNo then
  begin
    if csDesigning in ComponentState then
      fSelectedPropertyNo := Value
    else
    begin
      if fSelectedPropertyNo <> -1 then         // Remove old property editor
      begin
        DoOnPropertyEditExit (fPropertyEdit);
        SetFocus
      end;

      fSelectedPropertyNo := Value;
      SetPropertyEdit;

      Invalidate
    end
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.WmInit                                              |
 |                                                                      |
 | Handle the WmInit message by selecting the first property            |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.WmInit(var Msg: TMessage);
begin
  SelectedPropertyNo := Msg.wParam
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.WmPaint                                             |
 |                                                                      |
 | WmPaint message handler.                                             |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.WmPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

(*----------------------------------------------------------------------*
 | TPropertyListBox.WMSize                                              |
 |                                                                      |
 | WmSize message handler                                               |
 *----------------------------------------------------------------------*)
procedure TPropertyListBox.WMSize(var Message: TWMSize);
begin
  inherited;

  if Assigned (fPropertyEdit) then
    fPropertyEdit.Width := ClientWidth - fNameColWidth - 2;
end;

{ TPropertyListProperties }

(*----------------------------------------------------------------------*
 | TPropertyListProperties.EndUpdate                                    |
 |                                                                      |
 | Property list has changed.  Update the display.                      |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperties.EndUpdate;
begin
  inherited;

  if Parent.ComponentState = [] then
  begin
    Parent.RecalcScrollBars;
    Parent.Invalidate;

    if not (csLoading in Parent.Owner.ComponentState) then
      Parent.SetPropertyEdit
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperties.GetItem                                      |
 |                                                                      |
 | Type-safe get item method/                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   index: Integer                                                     |
 |                                                                      |
 | The function returns TPropertyListProperty                           |
 *----------------------------------------------------------------------*)
function TPropertyListProperties.GetItem(
  index: Integer): TPropertyListProperty;
begin
  result := inherited Items [index] as TPropertyListProperty;
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperties.SetItem                                      |
 |                                                                      |
 | Type-safe set item method.                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   index: Integer; const Value: TPropertyListProperty                 |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperties.SetItem(index: Integer;
  const Value: TPropertyListProperty);
begin
  inherited Items [index] := Value
end;

{ TPropertyListProperty }

(*----------------------------------------------------------------------*
 | TPropertyListProperty.Create                                         |
 |                                                                      |
 | Constructor for a property                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   collection: TCollection                                            |
 *----------------------------------------------------------------------*)
constructor TPropertyListProperty.Create(collection: TCollection);
begin
  inherited;
  fEnumValues := TStringList.Create;
  fEnabled := True;
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.DecValue                                       |
 |                                                                      |
 | Decrement the property value.                                        |
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperty.DecValue;
begin
  case PropertyType of
    ptInteger : fPropertyValue := fPropertyValue - 1;
    ptBoolean : fPropertyValue := not fPropertyValue;
    ptEnum    : if fPropertyValue = 0 then
                  fPropertyValue := fEnumValues.Count - 1 else
                fPropertyValue := fPropertyValue - 1
  end;

  TPropertyListProperties (collection).Parent.PropertyChanged
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.Destroy                                        |
 |                                                                      |
 | Destructor for a property                                            |
 *----------------------------------------------------------------------*)
destructor TPropertyListProperty.Destroy;
begin
  fEnumValues.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.GetStrings                                     |
 |                                                                      |
 | Return the properties enum values.                                   |
 *----------------------------------------------------------------------*)
function TPropertyListProperty.GetActualValueAsStr: string;
begin
  if VarIsEmpty (fActualValue) then
    result := ''
  else
  try
    case propertyType of
      ptBoolean : if fActualValue then
                    Result := rstTrue
                  else
                    Result := rstFalse;

      ptEnum :
        if fActualValue < fEnumValues.Count then
          Result := fEnumValues [fActualValue]
        else
          Result := rstError;
      else
        result := fActualValue
    end
  except
    result := rstError
  end
end;

function TPropertyListProperty.GetStrings: TStrings;
begin
  Result := fEnumValues
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.GetValueAsStr                                  |
 |                                                                      |
 | Get string representation of the properties value                    |
 *----------------------------------------------------------------------*)
function TPropertyListProperty.GetValueAsStr: string;
begin
  if VarIsEmpty (fPropertyValue) then
    result := ''
  else
  try
    case propertyType of
      ptBoolean : if fPropertyValue then
                    Result := rstTrue
                  else
                    Result := rstFalse;

      ptEnum :
        if fPropertyValue < fEnumValues.Count then
          Result := fEnumValues [fPropertyValue]
        else
          Result := rstError;
      else
        result := fPropertyValue
    end
  except
    result := rstError
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.IncValue                                       |
 |                                                                      |
 | Increment the properties value                                       |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperty.IncValue;
begin
  case PropertyType of
    ptInteger : fPropertyValue := fPropertyValue + 1;
    ptBoolean : fPropertyValue := not fPropertyValue;
    ptEnum    : if fPropertyValue = fEnumValues.Count - 1 then
                  fPropertyValue := 0
                else
                  fPropertyValue := fPropertyValue + 1;
  end;

  TPropertyListProperties (collection).Parent.PropertyChanged;
end;

procedure TPropertyListProperty.SetActualValue(Value: variant);
var
  i : Integer;
begin
  if (PropertyType = ptEnum) and (VarType (Value) = varString) then
  begin
    for i := 0 to EnumValues.Count - 1 do
      if CompareText (EnumValues [i], Value) = 0 then
      begin
        Value := i;
        break
      end;

    if VarType (Value) <> varInteger then
      raise Exception.Create ('Invalid enumerated value');
  end;

  if VarIsEmpty (fActualValue) or VarIsEmpty (Value) or (fActualValue <> Value) then
  begin
    fActualValue := value;
    TPropertyListProperties (collection).Parent.Invalidate
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.SetEnabled                                     |
 |                                                                      |
 | Enable/disable the property                                          |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: Boolean                                               |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperty.SetColor(const Value: TColor);
begin
  if fColor <> Value then
  begin
    if value <> TPropertyListProperties (Collection).Parent.Color then
      fParentColor := False;
    fColor := Value;
    TPropertyListProperties (Collection).Parent.Invalidate
  end
end;

procedure TPropertyListProperty.SetEnabled(const Value: Boolean);
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    TPropertyListProperties (collection).Parent.Invalidate
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.SetPropertyName                                |
 |                                                                      |
 | Set method for properties 'PropertyName' property                    |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                                                |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperty.SetParentColor(const Value: Boolean);
begin
  if Value <> fParentColor then
  begin
    fParentColor := Value;
    if fParentColor then
      Color := TPropertyListProperties (Collection).Parent.Color
    else
      TPropertyListProperties (Collection).Parent.Invalidate
  end
end;

procedure TPropertyListProperty.SetPropertyName(const Value: string);
begin
  if fPropertyName <> Value then
  begin
    fPropertyName := Value;
    TPropertyListProperties (collection).Parent.Invalidate
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.SetPropertyType                                |
 |                                                                      |
 | Set method for properties 'PropertyType' property                    |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: TPropertyType                                         |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperty.SetPropertyType(
  const Value: TPropertyType);
begin
  if fPropertyType <> Value then
  begin
    fPropertyType := Value;
    TPropertyListProperties (collection).Parent.Invalidate
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.SetPropertyValue                               |
 |                                                                      |
 | Set method for properties 'ProperyValue' method                      |
 |                                                                      |
 | Parameters:                                                          |
 |   Value: variant                                                     |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperty.SetPropertyValue(Value: variant);
var
  i : Integer;
  p : TPropertyListBox;
begin
  if (PropertyType = ptEnum) and (VarType (Value) = varString) then
  begin
    for i := 0 to EnumValues.Count - 1 do
      if CompareText (EnumValues [i], Value) = 0 then
      begin
        Value := i;
        break
      end;

    if VarType (Value) <> varInteger then
      raise Exception.Create ('Invalid enumerated value');
  end;

  if VarIsEmpty (fPropertyValue) or VarIsEmpty (Value) or (fPropertyValue <> Value) then
  begin
    fPropertyValue := value;

    p := TPropertyListProperties (Collection).Parent;
    if p.fSelectedPropertyNo = Index then
      if Assigned (p.fPropertyEdit) then
        p.PropertyEditText := ValueAsStr;

    TPropertyListProperties (collection).Parent.Invalidate
  end
end;

(*----------------------------------------------------------------------*
 | TPropertyListProperty.SetStrings                                     |
 |                                                                      |
 | Set method for enum values 'strings' property                        |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: TStrings                                              |
 *----------------------------------------------------------------------*)
procedure TPropertyListProperty.SetStrings(const Value: TStrings);
begin
  fEnumValues.Assign (Value);
end;

end.
