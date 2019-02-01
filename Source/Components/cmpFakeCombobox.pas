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
    FButtonPressed: Boolean;
    FOnKeyDown: TKeyEvent;
    FSpecialButton: Boolean;
    FOnSpecialButtonClick: TNotifyEvent;
    function GetDropdownButtonWidth: Integer;
    function GetButtonRect: TRect;
    procedure SetButtonPressed(const Value: Boolean);
    function GetOnDblClick: TNotifyEvent;
    procedure SetOnDblClick(const Value: TNotifyEvent);
    procedure StringListOnChange(Sender: TObject);
    procedure SetSpecialButton(const Value: Boolean);
  private
    FEdit: TEdit;
    FItems: TStrings;
    FListBox: TListBox;

    function GetAutoSelect: Boolean;
    procedure SetAutoSelect(const Value: Boolean);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
    function GetCursor: TCursor;
    procedure SetCursor(const Value: TCursor);
    function GetHideSelection: Boolean;
    procedure SetHideSelection(const Value: Boolean);
    function GetMaxLength: Integer;
    function GetOEMConvert: Boolean;
    procedure SetMaxLength(const Value: Integer);
    procedure SetOEMConvert(const Value: Boolean);
    function GetPAsswordChar: char;
    procedure SetPasswordChar(const Value: char);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetItems(const Value: TStrings);

    procedure DoOnListboxMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnListboxKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnEditKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoOnListBoxExit(Sender: TObject);


    procedure ShowList(show, update: Boolean);

    procedure WmHideList(var Msg: TMessage); message WM_HIDELIST;

    property DropdownButtonWidth: Integer read GetDropdownButtonWidth;
    property ButtonRect: TRect read GetButtonRect;
    property ButtonPressed: Boolean read FButtonPressed write SetButtonPressed;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    function Focused: Boolean; override;
  published
    property Anchors;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect;
    property BiDiMode;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase;
    property Color default clWhite;
    property Constraints;
    property Cursor: TCursor read GetCursor write SetCursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HelpContext;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection;
    property Items: TStrings read FItems write SetItems;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property OEMConvert: Boolean read GetOEMConvert write SetOEMConvert;
    property PasswordChar: char read GetPAsswordChar write SetPasswordChar;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ShowHint;
    property SpecialButton: Boolean read FSpecialButton write SetSpecialButton;
    property TabOrder;
    property TabStop default True;
    property Text: string read GetText write SetText;
    property Visible;

    property OnEnter;
    property OnExit;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnSpecialButtonClick: TNotifyEvent read FOnSpecialButtonClick write FOnSpecialButtonClick;
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

  FItems := TStringList.Create;
  TStringList(FItems).OnChange := StringListOnChange;

  FEdit := TEdit.Create(self);
  FEdit.Parent := Self;
  FEdit.Ctl3D := False;
  FEdit.BorderStyle := bsNone;
  FEdit.Left := 2;
  FEdit.Top := 2;
  FEdit.ParentBiDiMode := True;
  FEdit.ParentColor := True;
  FEdit.ParentFont := True;
  FEdit.ParentShowHint := True;
  FEdit.OnKeyDown := DoOnEditKeyDown;
end;

destructor TFakeCombobox.Destroy;
begin
  FItems.Free;
  inherited
end;

procedure TFakeCombobox.DoOnListBoxExit(Sender: TObject);
begin
  ShowList(False, False);
end;

procedure TFakeCombobox.DoOnListboxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PostMessage(Handle, WM_HIDELIST, 1, 0);
end;

procedure TFakeCombobox.DoOnListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then
    PostMessage(Handle, WM_HIDELIST, 1, 0)
  else
    if (Key = VK_ESCAPE) or (Key = VK_PRIOR) then
      PostMessage(Handle, WM_HIDELIST, 0, 0)
end;

function TFakeCombobox.Focused: Boolean;
begin
  Result := FEdit.Focused
end;

function TFakeCombobox.GetAutoSelect: Boolean;
begin
  Result := FEdit.AutoSelect;
end;

function TFakeCombobox.GetButtonRect: TRect;
begin
  Result := ClientRect;
  Dec(Result.Right);
  Result.Top := Result.Top + 1;
  Result.Bottom := Result.Bottom - 1;
  Result.Left := Result.Right - DropdownButtonWidth;
end;

function TFakeCombobox.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;

function TFakeCombobox.GetCursor: TCursor;
begin
  Result := FEdit.Cursor
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
  Result := FEdit.HideSelection
end;

function TFakeCombobox.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength
end;

function TFakeCombobox.GetOEMConvert: Boolean;
begin
  Result := FEdit.OEMConvert
end;

function TFakeCombobox.GetOnDblClick: TNotifyEvent;
begin
  Result := FEdit.OnDblClick
end;

function TFakeCombobox.GetPasswordChar: char;
begin
  Result := FEdit.PasswordChar
end;

function TFakeCombobox.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly
end;

function TFakeCombobox.GetText: string;
begin
  Result := FEdit.Text
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
    if PtInRect(ButtonRect, Point(X, Y)) then
    begin
      ButtonPressed := True;
      if not SpecialButton then
        ShowList(True, False)
      else
        if Assigned (OnSpecialButtonClick) then
          OnSpecialButtonClick(Self);
    end
end;

procedure TFakeCombobox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not PtInRect(ButtonRect, Point(X, Y)) then
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
  r: TRect;
  Style: Integer;
  OldMode: Integer;
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
    OldMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    OffsetRect(r, 0, -4);
    DrawText(Canvas.Handle, '...', 3, r, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    SetBkMode(Canvas.Handle, OldMode);
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

  FEdit.Height := ClientHeight - 4;
  FEdit.Width := ClientWidth - 4 - DropdownButtonWidth
end;

procedure TFakeCombobox.SetAutoSelect(const Value: Boolean);
begin
  FEdit.AutoSelect := Value
end;

procedure TFakeCombobox.SetButtonPressed(const Value: Boolean);
begin
  if FButtonPressed <> Value then
  begin
    if Value then
      SetCapture(Handle)
    else
      ReleaseCapture;

    FButtonPressed := Value;
    Invalidate
  end
end;

procedure TFakeCombobox.SetCharCase(const Value: TEditCharCase);
begin
  FEdit.CharCase := value
end;

procedure TFakeCombobox.SetCursor(const Value: TCursor);
begin
  FEdit.Cursor := Value
end;

procedure TFakeCombobox.SetFocus;
begin
  FEdit.SetFocus
end;

procedure TFakeCombobox.SetHideSelection(const Value: Boolean);
begin
  FEdit.HideSelection := Value
end;

procedure TFakeCombobox.SetItems(const Value: TStrings);
var
  OldCount: Integer;
begin
  OldCount := Items.Count;
  FItems.Assign (Value);
  if (OldCount = 0) <> (FItems.Count = 0) then
  begin
    Resize;
    Invalidate
  end
end;

procedure TFakeCombobox.SetMaxLength(const Value: Integer);
begin
  FEdit.MaxLength := Value
end;

procedure TFakeCombobox.SetOEMConvert(const Value: Boolean);
begin
  FEdit.OEMConvert := value
end;

procedure TFakeCombobox.SetOnDblClick(const Value: TNotifyEvent);
begin
  FEdit.OnDblClick := Value
end;

procedure TFakeCombobox.SetPasswordChar(const Value: char);
begin
  FEdit.PasswordChar := Value
end;

procedure TFakeCombobox.SetReadOnly(const Value: Boolean);
begin
  FEdit.ReadOnly := Value
end;

procedure TFakeCombobox.SetText(const Value: string);
begin
  FEdit.Text := value
end;

procedure TFakeCombobox.ShowList(show, update: Boolean);
var
  i: Integer;
begin
  if Show then
  begin
    if not Assigned (FListBox) then
    begin
      FListBox := TListBox.Create(Self);
      FListBox.Parent := Parent;
      FListBox.Top := Top + ClientHeight - 2;
      FListBox.Left := Left - 1;
      FListBox.Width := ClientWidth + 1;
      FListBox.OnExit := DoOnListboxExit;
      FListBox.OnKeyDown := DoOnListBoxKeyDown;
      FListBox.OnMouseDown := DoOnListboxMouseDown;
    end
    else
      FListBox.Clear;

    if FItems.Count > 6 then
      i := 6
    else
      i := FItems.Count;

    FListBox.ItemHeight := ClientHeight;

    FListBox.Height := ClientHeight * i;

    FListBox.Items.AddStrings (FItems);
    FListBox.ItemIndex := FItems.IndexOf (FEdit.Text);
    Application.ProcessMessages;
    FListBox.SetFocus
  end
  else
  begin
    if (FListBox.ItemIndex >= 0) and (FListBox.ItemIndex < FItems.Count) and Update then
      FEdit.Text := FListBox.Items[FListBox.ItemIndex];
    FreeAndNil (FListBox);

    if Assigned (OnExit) and Update then
      OnExit(Self);
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
  ShowList(False, Boolean (Msg.wParam));
end;

procedure TFakeCombobox.DoOnEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_NEXT then
  begin
    if Items.Count > 0 then
      ShowList(True, False)
    else
      if SpecialButton and Assigned (OnSpecialButtonClick) then
        OnSpecialButtonClick(Self);
  end
  else
    if Assigned (FOnKeyDown) then
      FOnKeyDown (Self, key, shift);
end;

procedure TFakeCombobox.SetSpecialButton(const Value: Boolean);
begin
  if Value <> FSpecialButton then
  begin
    FSpecialButton := Value;
    Resize;
    Invalidate;
  end
end;

end.
