unit AcceleratorResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Menus, Graphics, Controls, Forms,
  Dialogs, ResourceForm, ComCtrls, unitResourceAccelerator, StdCtrls, ActnList,
  System.Actions;

type
  TFormAcceleratorResource = class(TFormResource)
    ActionAccelAdd: TAction;
    ActionAccelChangeFlags: TAction;
    ActionAccelChangeID: TAction;
    ActionAccelDelete: TAction;
    ActionAccelModify: TAction;
    ActionList: TActionList;
    ComboBoxKey: TComboBox;
    ComboBoxType: TComboBox;
    ListViewAccelerator: TListView;
    MainMenuAccelMenu: TMainMenu;
    MenuItemAccelerators: TMenuItem;
    MenuItemAddAccelerator: TMenuItem;
    MenuItemChangeAddAccelerator2: TMenuItem;
    MenuItemChangeChangeID2: TMenuItem;
    MenuItemChangeDeleteAccelerator2: TMenuItem;
    MenuItemChangeFlags: TMenuItem;
    MenuItemChangeFlags2: TMenuItem;
    MenuItemChangeID: TMenuItem;
    MenuItemChangeModifyAccelerator2: TMenuItem;
    MenuItemDeleteAccelerator: TMenuItem;
    MenuItemModifyAccelerator: TMenuItem;
    PopupMenuAccel: TPopupMenu;
    procedure ActionAccelDeleteExecute(Sender: TObject);
    procedure ComboBoxTypeExit(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure ActionAccelChangeFlagsExecute(Sender: TObject);
    procedure ActionAccelChangeIDExecute(Sender: TObject);
    procedure ComboBoxKeyChange(Sender: TObject);
    procedure ListViewAcceleratorEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ListViewAcceleratorDblClick(Sender: TObject);
    procedure ComboBoxKeyExit(Sender: TObject);
    procedure ActionAccelModifyExecute(Sender: TObject);
    procedure ActionAccelAddExecute(Sender: TObject);
    procedure ListViewAcceleratorEdited(Sender: TObject; Item: TListItem;
      var S: string);
  private
    FAdding: Boolean;
    FKeyChanged: Boolean;
    FDetails: TAcceleratorResourceDetails;
    FAllowEdit: Boolean;
  protected
    function GetMenuItem: TMenuItem; override;
    procedure SetObject(const Value: TObject); override;
  public
    procedure SaveResource(const UndoDetails: string);
  end;

implementation

{$R *.dfm}

resourcestring
  rstAddAccelerator    = 'add accelerator';
  rstDeleteAccelerator = 'delete accelerator';
  rstChangeAccelerator = 'change accelerator';
  rstChangeAcceleratorType = 'change accelerator type';
  rstChangeID          = 'change accelerator ID';
  rstDuplicateMessageID = 'Duplicate Accelerator ID';
  rstVirtKey = 'Virtual Key';
  rstCharCode = 'Character Code';


{ TfmAcceleratorResource }

function AcceleratorToText(const Accel: TAccelerator): string;
begin
  if (Accel.Flags and FVIRTKEY) <> 0 then
    Result := ShortcutToText(Accel.Code)
  else
    if Accel.Code < 32 then
      Result := 'Ctrl+' + Char(Accel.Code + Ord ('@'))
    else
      Result := Char(Accel.Code);

  if (Accel.Flags and FSHIFT) <> 0 then
    Result := 'Shift+' + Result;

  if (Accel.Flags and FALT) <> 0 then
    Result := 'Alt+' + Result;

  if (Accel.Flags and FCONTROL) <> 0 then
    Result :='Ctrl+' + Result
end;

procedure TextToAccelerator(const st: string; VirtKey: Boolean; var Code, Flags: Integer);
var
  Temp: Integer;
  Key: Word;
  Shift: TShiftState;
  Ok: Boolean;
begin
  Ok := False;
  if not VirtKey then
  begin
    Temp := TextToShortcut(st);
    Key := Temp and not (scAlt or scShift or scCtrl);
    if (Key >= $30) and (Key <= $39) or
       (Key >= $41) and (Key <= $5A) then
    begin
      ShortcutToKey(Temp, Key, Shift);
      Code := Key;
      Flags := 0;
      if ssShift in Shift then
        Flags := Flags or FSHIFT;
      if ssAlt in Shift then
        Flags := Flags or FALT;
      if ssCtrl in Shift then
        Flags := Flags or FCONTROL;
        Ok := True;
    end
  end;

  if not Ok then
  begin
    Code := TextToShortcut(st);
    Flags := FVIRTKEY;
    if (Code and scAlt) <> 0 then
      Flags := Flags or FALT;
    if (Code and scShift) <> 0 then
      Flags := Flags or FSHIFT;
    if (Code and scCtrl) <> 0 then
      Flags := Flags or FCONTROL;
    Code := Code and not (scAlt or scShift or scCtrl);
  end
end;

procedure TFormAcceleratorResource.SetObject(const Value: TObject);
var
  Index: Integer;
  Accel: TAccelerator;
begin
  inherited;            // *Must* call inherited
  Application.ProcessMessages;

  FDetails := Obj as TAcceleratorResourceDetails;

  ListViewAccelerator.Items.BeginUpdate;
  try
    ListViewAccelerator.Items.Clear;
    for Index := 0 to FDetails.Count - 1 do
    begin
      Accel := FDetails.Accelerator[Index];
      with ListViewAccelerator.Items.Add do
      begin
        Caption := IntToStr(Accel.Id);
        SubItems.Add(AcceleratorToText(Accel));
        if (Accel.Flags and FVIRTKEY) <> 0 then
          SubItems.Add(rstVirtKey)
        else
          SubItems.Add(rstCharCode);
//        SubItems.Add(IntToStr(Accel.Flags))
      end
    end
  finally
    ListViewAccelerator.Items.EndUpdate
  end
end;

procedure TFormAcceleratorResource.ListViewAcceleratorEdited(Sender: TObject;
  Item: TListItem; var S: string);
var
  NewID: Integer;
  Index: Integer;
begin
  inherited;
  FAllowEdit := False;
  ActionAccelDelete.ShortCut := ActionAccelDelete.Tag;    // Restore 'Delete' shortcut
  if s <> Item.Caption then
  try
    NewID := StrToInt(s);

    for Index := 0 to FDetails.Count - 1 do
      if FDetails.Accelerator[Index].Id = NewID then
        raise Exception.Create(rstDuplicateMessageID);

    Item.Caption := s;          // need to do this so 'SaveResource' picks it up...
    SaveResource(rstChangeID);
  except
    s := Item.Caption;
    raise
  end
end;

function TFormAcceleratorResource.GetMenuItem: TMenuItem;
begin
  Result := MenuItemAccelerators
end;

procedure TFormAcceleratorResource.ActionAccelAddExecute(Sender: TObject);
var
  Item: TListItem;
  NewID: Integer;
begin
  Item := ListViewAccelerator.Items.Add;

  // Work out string / message Id
  if Item.Index = 0 then
    Item.Caption := '1'
  else
  begin
    NewID := StrToInt(ListViewAccelerator.Items[ListViewAccelerator.Items.Count - 2].Caption);
    Item.Caption := IntToStr(NewID + 1)
  end;

  Item.SubItems.Add('');
  Item.SubItems.Add(rstVirtKey);
  Item.Selected := True;

  FAdding := True;                    // Clear in mmoMessageExit

  ActionAccelModifyExecute(Sender);
end;

procedure TFormAcceleratorResource.ActionAccelModifyExecute(Sender: TObject);
var
  t: Integer;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    ComboBoxKey.Width := ListViewAccelerator.Columns[1].Width - 2;
    ComboBoxKey.Left := ListViewAccelerator.Columns[0].Width + 2;
    t := ListViewAccelerator.Selected.DisplayRect(drLabel).Top - 3;
    if t < 0 then
      t := 0;
    ComboBoxKey.Top := t;
    ComboBoxKey.Text := ListViewAccelerator.Selected.SubItems[0];
    ComboBoxKey.Visible := True;
    ComboBoxKey.SetFocus
  end
  else
    ActionAccelAdd.Execute;
end;

procedure TFormAcceleratorResource.ComboBoxKeyExit(Sender: TObject);
var
  st: string;
  Adding: Boolean;
begin
  Adding := FAdding;
  FAdding := False;
  ComboBoxKey.Visible := False;
  with ListViewAccelerator do if Assigned(Selected) then
  begin
    if FKeyChanged or FAdding then  // ie.  If it's changed
    begin
      Selected.SubItems[0] := ComboBoxKey.Text;

      if Adding then
        st := rstAddAccelerator
      else
        st := rstChangeAccelerator;

      SaveResource(st);
    end
  end;
  FKeyChanged := False
end;

procedure TFormAcceleratorResource.SaveResource(const UndoDetails: string);
var
  Index, Flags, Code, Id: Integer;
  Item: TListItem;
  VirtKey: Boolean;
begin
  AddUndoEntry(UndoDetails);
  for Index := 0 to ListViewAccelerator.Items.Count - 1 do
  begin
    Item := ListViewAccelerator.Items[Index];
    Id := StrToInt(Item.Caption);
    VirtKey := Item.SubItems[1] = rstVirtKey;
    TextToAccelerator(Item.SubItems[0], VirtKey, Code, Flags);

    if not VirtKey and ((Flags and FVIRTKEY) <> 0) then
      Item.SubItems[1] := rstVirtKey;

    if Index < FDetails.Count then
      FDetails.SetAccelDetails(Index, Flags, Code, Id)
    else
      FDetails.Add(Flags, Code, Id)
  end;

  Index := ListViewAccelerator.Items.Count;
  while Index < FDetails.Count do
    FDetails.Delete(Index);
end;

procedure TFormAcceleratorResource.ListViewAcceleratorDblClick(Sender: TObject);
var
  p: TPoint;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints(HWND_DESKTOP, ListViewAccelerator.Handle, p, 1);

    if p.x > ListViewAccelerator.Columns[0].Width + ListViewAccelerator.Columns[1].Width then
      ActionAccelChangeFlags.Execute
    else
    if p.x > ListViewAccelerator.Columns[0].Width then
      ActionAccelModify.Execute
    else
      ActionAccelChangeID.Execute
  end
  else
    ActionAccelAdd.Execute
end;

procedure TFormAcceleratorResource.FormCreate(Sender: TObject);

  procedure AddShortcutRange(const st: string; lowRange, highRange: char);
  var
    ch: char;
  begin
    for ch := lowRange to highRange do
      ComboBoxKey.Items.Add(st + ch)
  end;

begin
  inherited;

  ComboBoxKey.Items.BeginUpdate;
  try
    AddShortcutRange('Ctrl+', 'A', 'Z');
    AddShortcutRange('Ctrl+Alt+', 'A', 'Z');
    AddShortcutRange('F', '1', '9');
    AddShortcutRange('F1', '0', '2');
    AddShortcutRange('Ctrl+F', '1', '9');
    AddShortcutRange('Ctrl+F1', '0', '2');
    AddShortcutRange('Shift+F', '1', '9');
    AddShortcutRange('Shift+F1', '0', '2');
    AddShortcutRange('Shift+Ctrl+F', '1', '9');
    AddShortcutRange('Shift+Ctrl+F1', '0', '2');
    AddShortcutRange('In', 's', 's');
    AddShortcutRange('Shift+In', 's', 's');
    AddShortcutRange('Ctrl+In', 's', 's');
    AddShortcutRange('De', 'l', 'l');
    AddShortcutRange('Shift+De', 'l', 'l');
    AddShortcutRange('Ctrl+De', 'l', 'l');
    AddShortcutRange('Alt+BkS', 'p', 'p');
    AddShortcutRange('Shift+Alt+BkS', 'p', 'p');
  finally
    ComboBoxKey.Items.EndUpdate
  end;

  ComboBoxType.Items.Add(rstVirtKey);
  ComboBoxType.Items.Add(rstCharCode);
end;

procedure TFormAcceleratorResource.ListViewAcceleratorEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := FAllowEdit;
  ActionAccelDelete.Tag := ActionAccelDelete.ShortCut;    // Suspend 'Delete' shortcut.
  ActionAccelDelete.ShortCut := 0;
end;

procedure TFormAcceleratorResource.ComboBoxKeyChange(Sender: TObject);
begin
  inherited;
  FKeyChanged := True;
end;

procedure TFormAcceleratorResource.ActionAccelChangeIDExecute(Sender: TObject);
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    FAllowEdit := True;
    ListViewAccelerator.Selected.EditCaption;
  end
end;

procedure TFormAcceleratorResource.ActionAccelChangeFlagsExecute(Sender: TObject);
var
  t: Integer;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    ComboBoxType.Width := ListViewAccelerator.Columns[2].Width - 2;
    ComboBoxType.Left := ListViewAccelerator.Columns[0].Width + ListViewAccelerator.Columns[1].Width + 2;
    t := ListViewAccelerator.Selected.DisplayRect(drLabel).Top - 3;
    if t < 0 then
      t := 0;
    ComboBoxType.Top := t;
    ComboBoxType.Text := ListViewAccelerator.Selected.SubItems[1];
    ComboBoxType.Visible := True;
    ComboBoxType.SetFocus
  end
end;

procedure TFormAcceleratorResource.ComboBoxTypeChange(Sender: TObject);
begin
  FKeyChanged := True;
end;

procedure TFormAcceleratorResource.ComboBoxTypeExit(Sender: TObject);
begin
  ComboBoxType.Visible := False;
  with ListViewAccelerator do if Assigned(Selected) then
  begin
    if FKeyChanged then  // ie.  If it's changed
    begin
      Selected.SubItems[1] := ComboBoxType.Text;

      SaveResource(rstChangeAcceleratorType);
    end
  end;
  FKeyChanged := False
end;

procedure TFormAcceleratorResource.ActionAccelDeleteExecute(Sender: TObject);
var
  Index: Integer;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    Index := ListViewAccelerator.Selected.Index;
    ListViewAccelerator.Selected.Free;

    // Select next string
    if Index < ListViewAccelerator.Items.Count then
      ListViewAccelerator.Selected := ListViewAccelerator.Items[Index]
    else
    begin
      Dec(Index);
      if Index >= 0 then
        ListViewAccelerator.Selected := ListViewAccelerator.Items[Index]
    end;

   SaveResource(rstDeleteAccelerator)
  end
end;

end.
