(*======================================================================*
 | cmpFakeCombobox                                                      |
 |                                                                      |
 | Fake a combobox, providing additional capabilities.                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      31/05/2001  CPWW  Original                                  |
 *======================================================================*)

unit cmpFakeCombobox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

const
  WM_HIDELIST = WM_USER + $201;
type

  TFakeCombobox = class(TCustomControl)
  private
    fButtonPressed: Boolean;
    fOnKeyDown: TKeyEvent;
    fSpecialButton: Boolean;
    fOnSpecialButtonClick: TNotifyEvent;
    function GetDropdownButtonWidth: Integer;
    function GetButtonRect: TRect;
    procedure SetButtonPressed(const Value: Boolean);
    function GetOnDblClick: TNotifyEvent;
    procedure SetOnDblClick(const Value : TNotifyEvent);
    procedure StringListOnChange (Sender : TObject);
    procedure SetSpecialButton(const Value: Boolean);
  private
    fEdit : TEdit;
    fItems : TStrings;
    fListBox : TListBox;

    function GetAutoSelect: Boolean;
    procedure SetAutoSelect(const Value: Boolean);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
    function GetCursor: TCursor;
    procedure SetCursor(const Value: TCursor);
    function GetHideSelection: Boolean;
    procedure SetHideSelection(const Value: Boolean);
    function GetMaxLength: Integer;
    function GetOEMConvert: boolean;
    procedure SetMaxLength(const Value: Integer);
    procedure SetOEMConvert(const Value: boolean);
    function GetPAsswordChar: char;
    procedure SetPasswordChar(const Value: char);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetItems(const Value: TStrings);

    procedure DoOnListboxMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnListboxKeyDown (Sender : TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnEditKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoOnListBoxExit (Sender : TObject);


    procedure ShowList (show, update : Boolean);

    procedure WmHideList (var Msg : TMessage); message WM_HIDELIST;

    property DropdownButtonWidth : Integer read GetDropdownButtonWidth;
    property ButtonRect : TRect read GetButtonRect;
    property ButtonPressed : Boolean read fButtonPressed write SetButtonPressed;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    function Focused : Boolean; override;
  published
    property Anchors;
    property AutoSelect : Boolean read GetAutoSelect write SetAutoSelect;
    property BiDiMode;
    property CharCase : TEditCharCase read GetCharCase write SetCharCase;
    property Color default clWhite;
    property Constraints;
    property Cursor : TCursor read GetCursor write SetCursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HelpContext;
    property HideSelection : Boolean read GetHideSelection write SetHideSelection;
    property Items : TStrings read fItems write SetItems;
    property MaxLength : Integer read GetMaxLength write SetMaxLength;
    property OEMConvert : boolean read GetOEMConvert write SetOEMConvert;
    property PasswordChar : char read GetPAsswordChar write SetPasswordChar;
    property PopupMenu;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property ShowHint;
    property SpecialButton : Boolean read fSpecialButton write SetSpecialButton;
    property TabOrder;
    property TabStop default True;
    property Text : string read GetText write SetText;
    property Visible;

    property OnEnter;
    property OnExit;
    property OnDblClick : TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnKeyDown : TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnSpecialButtonClick : TNotifyEvent read fOnSpecialButtonClick write fOnSpecialButtonClick;
  end;

implementation

{ TFakeCombobox }

constructor TFakeCombobox.Create(AOwner: TComponent);
begin
  inherited;

  Width := 145;
  Height := 21;
  Color := clWhite;

  ControlStyle := [csFramed, csOpaque];
  TabStop := True;

  fItems := TStringList.Create;
  TStringList (fItems).OnChange := StringListOnChange;

  fEdit := TEdit.Create (self);
  fEdit.Parent := Self;
  fEdit.Ctl3D := False;
  fEdit.BorderStyle := bsNone;
  fEdit.Left := 2;
  fEdit.Top := 2;
  fEdit.ParentBiDiMode := True;
  fEdit.ParentColor := True;
  fEdit.ParentFont := True;
  fEdit.ParentShowHint := True;
  fEdit.OnKeyDown := DoOnEditKeyDown;
end;

destructor TFakeCombobox.Destroy;
begin
  fItems.Free;
  inherited
end;

procedure TFakeCombobox.DoOnListBoxExit(Sender: TObject);
begin
  ShowList (False, False);
end;

procedure TFakeCombobox.DoOnListboxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PostMessage (Handle, WM_HIDELIST, 1, 0);
end;

procedure TFakeCombobox.DoOnListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then
    PostMessage (Handle, WM_HIDELIST, 1, 0)
  else
    if (Key = VK_ESCAPE) or (Key = VK_PRIOR) then
      PostMessage (Handle, WM_HIDELIST, 0, 0)
end;

function TFakeCombobox.Focused: Boolean;
begin
  Result := fEdit.Focused
end;

function TFakeCombobox.GetAutoSelect: Boolean;
begin
  Result := fEdit.AutoSelect;
end;

function TFakeCombobox.GetButtonRect: TRect;
begin
  result := ClientRect;
  Dec (result.Right);
  result.Top := result.Top + 1;
  result.Bottom := result.Bottom - 1;
  result.Left := result.Right - DropdownButtonWidth;
end;

function TFakeCombobox.GetCharCase: TEditCharCase;
begin
  Result := fEdit.CharCase;
end;

function TFakeCombobox.GetCursor: TCursor;
begin
  Result := fEdit.Cursor
end;

function TFakeCombobox.GetDropdownButtonWidth: Integer;
begin
  if SpecialButton then
    Result := 17
  else
    if Items.Count > 0 then
      Result := 13
    else
      Result := 0
end;

function TFakeCombobox.GetHideSelection: Boolean;
begin
  Result := fEdit.HideSelection
end;

function TFakeCombobox.GetMaxLength: Integer;
begin
  Result := fEdit.MaxLength
end;

function TFakeCombobox.GetOEMConvert: boolean;
begin
  Result := fEdit.OEMConvert
end;

function TFakeCombobox.GetOnDblClick: TNotifyEvent;
begin
  Result := fEdit.OnDblClick
end;

function TFakeCombobox.GetPasswordChar: char;
begin
  Result := fEdit.PasswordChar
end;

function TFakeCombobox.GetReadOnly: Boolean;
begin
  Result := fEdit.ReadOnly
end;

function TFakeCombobox.GetText: string;
begin
  Result := fEdit.Text
end;

procedure TFakeCombobox.Loaded;
begin
  inherited;
  Resize
end;

procedure TFakeCombobox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  SetFocus;
  if ((Items.Count > 0) or SpecialButton) then
    if PtInRect (ButtonRect, Point (X, Y)) then
    begin
      ButtonPressed := True;
      if not SpecialButton then
        ShowList (True, False)
      else
        if Assigned (OnSpecialButtonClick) then
          OnSpecialButtonClick (Self);
    end
end;

procedure TFakeCombobox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not PtInRect (ButtonRect, Point (X, Y)) then
    ButtonPressed := False
  else
    if ssLeft in Shift then
      ButtonPressed := True
end;

procedure TFakeCombobox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  ButtonPressed := False
end;

procedure TFakeCombobox.Paint;
var
  r : TRect;
  Style : Integer;
  oldMode : Integer;
begin
  inherited;
  r := ClientRect;
  Frame3d (Canvas, r, clBlack, clBtnFace, 1);
  Frame3d (Canvas, r, clWhite, clWhite, 1);

  if SpecialButton then
  begin
    r := ButtonRect;
    Style := DFCS_BUTTONPUSH;
    if ButtonPressed then
      Style := Style or DFCS_PUSHED;
    DrawFrameControl (Canvas.Handle, r, DFC_BUTTON, Style);
    oldMode := SetBkMode (Canvas.Handle, TRANSPARENT);
    OffsetRect (r, 0, -4);
    DrawText (Canvas.Handle, '...', 3, r, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    SetBkMode (Canvas.Handle, oldMode);
  end
  else
    if Items.Count > 0 then
    begin
      r := ButtonRect;
      Style := DFCS_SCROLLCOMBOBOX;
      if ButtonPressed then
        Style := Style or DFCS_PUSHED;
      DrawFrameControl (Canvas.Handle, r, DFC_SCROLL, Style);
    end
end;

procedure TFakeCombobox.Resize;
begin
  inherited;

  fEdit.Height := ClientHeight - 4;
  fEdit.Width := ClientWidth - 4 - DropdownButtonWidth
end;

procedure TFakeCombobox.SetAutoSelect(const Value: Boolean);
begin
  fEdit.AutoSelect := Value
end;

procedure TFakeCombobox.SetButtonPressed(const Value: Boolean);
begin
  if fButtonPressed <> Value then
  begin
    if Value then
      SetCapture (Handle)
    else
      ReleaseCapture;

    fButtonPressed := Value;
    Invalidate
  end
end;

procedure TFakeCombobox.SetCharCase(const Value: TEditCharCase);
begin
  fEdit.CharCase := value
end;

procedure TFakeCombobox.SetCursor(const Value: TCursor);
begin
  fEdit.Cursor := Value
end;

procedure TFakeCombobox.SetFocus;
begin
  fEdit.SetFocus
end;

procedure TFakeCombobox.SetHideSelection(const Value: Boolean);
begin
  fEdit.HideSelection := Value
end;

procedure TFakeCombobox.SetItems(const Value: TStrings);
var
  oldCount : Integer;
begin
  oldCount := Items.Count;
  fItems.Assign (Value);
  if (oldCount = 0) <> (fItems.Count = 0) then
  begin
    Resize;
    Invalidate
  end
end;

procedure TFakeCombobox.SetMaxLength(const Value: Integer);
begin
  fEdit.MaxLength := Value
end;

procedure TFakeCombobox.SetOEMConvert(const Value: boolean);
begin
  fEdit.OEMConvert := value
end;

procedure TFakeCombobox.SetOnDblClick(const Value : TNotifyEvent);
begin
  fEdit.OnDblClick := Value
end;

procedure TFakeCombobox.SetPasswordChar(const Value: char);
begin
  fEdit.PasswordChar := Value
end;

procedure TFakeCombobox.SetReadOnly(const Value: Boolean);
begin
  fEdit.ReadOnly := Value
end;

procedure TFakeCombobox.SetText(const Value: string);
begin
  fEdit.Text := value
end;

procedure TFakeCombobox.ShowList(show, update: Boolean);
var
  i : Integer;
begin
  if Show then
  begin
    if not Assigned (fListBox) then
    begin
      fListBox := TListBox.Create (Self);
      fListBox.Parent := Parent;
      fListBox.Top := Top + ClientHeight - 2;
      fListBox.Left := Left - 1;
      fListBox.Width := ClientWidth + 1;
      fListBox.OnExit := DoOnListboxExit;
      fListBox.OnKeyDown := DoOnListBoxKeyDown;
      fListBox.OnMouseDown := DoOnListboxMouseDown;
    end
    else
      fListBox.Clear;

    if fItems.Count > 6 then
      i := 6
    else
      i := fItems.Count;

    fListBox.ItemHeight := ClientHeight;

    fListBox.Height := ClientHeight * i;

    fListBox.Items.AddStrings (fItems);
    fListBox.ItemIndex := fItems.IndexOf (fEdit.Text);
    Application.ProcessMessages;
    fListBox.SetFocus
  end
  else
  begin
    if (fListBox.ItemIndex >= 0) and (fListBox.ItemIndex < fItems.Count) and Update then
      fEdit.Text := fListBox.Items [fListBox.ItemIndex];
    FreeAndNil (fListBox);

    if Assigned (OnExit) and Update then
      OnExit (Self);
    SetFocus
  end
end;

procedure TFakeCombobox.StringListOnChange(Sender: TObject);
begin
  if Items.Count in [0..1] then
    Resize
end;

procedure TFakeCombobox.WmHideList(var Msg: TMessage);
begin
  ShowList (False, Boolean (Msg.wParam));
end;

procedure TFakeCombobox.DoOnEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_NEXT then
  begin
    if Items.Count > 0 then
      ShowList (True, False)
    else
      if SpecialButton and Assigned (OnSpecialButtonClick) then
        OnSpecialButtonClick (Self);
  end
  else
    if Assigned (fOnKeyDown) then
      fOnKeyDown (Self, key, shift);
end;

procedure TFakeCombobox.SetSpecialButton(const Value: Boolean);
begin
  if Value <> fSpecialButton then
  begin
    fSpecialButton := Value;
    Resize;
    invalidate
  end
end;

end.
