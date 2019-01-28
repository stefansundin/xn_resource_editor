unit AcceleratorResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Menus, Graphics, Controls, Forms,
  Dialogs, ResourceForm, ComCtrls, unitResourceAccelerator, StdCtrls, ActnList,
  System.Actions;

type
  TfmAcceleratorResource = class(TfmResource)
    actAccelAdd: TAction;
    actAccelChangeFlags: TAction;
    actAccelChangeID: TAction;
    actAccelDelete: TAction;
    actAccelModify: TAction;
    ActionList: TActionList;
    MenuItemChangeAddAccelerator2: TMenuItem;
    ComboBoxKey: TComboBox;
    ComboBoxType: TComboBox;
    MenuItemChangeFlags2: TMenuItem;
    MenuItemChangeChangeID2: TMenuItem;
    MenuItemChangeDeleteAccelerator2: TMenuItem;
    ListViewAccelerator: TListView;
    MenuItemAddAccelerator: TMenuItem;
    MenuItemChangeFlags: TMenuItem;
    MenuItemChangeID: TMenuItem;
    MenuItemDeleteAccelerator: TMenuItem;
    MenuItemModifyAccelerator: TMenuItem;
    MenuItemAccelerators: TMenuItem;
    mnuAccelMenu: TMainMenu;
    MenuItemChangeModifyAccelerator2: TMenuItem;
    PopupMenuAccel: TPopupMenu;
    procedure actAccelDeleteExecute(Sender: TObject);
    procedure ComboBoxTypeExit(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure actAccelChangeFlagsExecute(Sender: TObject);
    procedure actAccelChangeIDExecute(Sender: TObject);
    procedure ComboBoxKeyChange(Sender: TObject);
    procedure ListViewAcceleratorEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ListViewAcceleratorDblClick(Sender: TObject);
    procedure ComboBoxKeyExit(Sender: TObject);
    procedure actAccelModifyExecute(Sender: TObject);
    procedure actAccelAddExecute(Sender: TObject);
    procedure ListViewAcceleratorEdited(Sender: TObject; Item: TListItem;
      var S: string);
  private
    FAdding : Boolean;
    FKeyChanged : Boolean;
    FDetails : TAcceleratorResourceDetails;
    FAllowEdit : Boolean;
  protected
    function GetMenuItem : TMenuItem; override;
    procedure SetObject(const Value: TObject); override;
  public
    procedure SaveResource(const undoDetails : string);
  end;

var
  fmAcceleratorResource: TfmAcceleratorResource;

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

function AcceleratorToText(const Accel : TAccelerator) : string;
begin
  if (Accel.flags and FVIRTKEY) <> 0 then
    Result := ShortcutToText(Accel.code)
  else
    if Accel.code < 32 then
      Result := 'Ctrl+' + Char(Accel.code + Ord ('@'))
    else
      Result := Char(Accel.code);

  if (Accel.flags and FSHIFT) <> 0 then
    Result := 'Shift+' + Result;

  if (Accel.flags and FALT) <> 0 then
    Result := 'Alt+' + Result;

  if (Accel.flags and FCONTROL) <> 0 then
    Result :='Ctrl+' + Result
end;

procedure TextToAccelerator(const st : string; virtKey : Boolean; var code, flags : Integer);
var
  Temp : Integer;
  Key : Word;
  Shift : TShiftState;
  Ok : Boolean;
begin
  Ok := False;
  if not virtKey then
  begin
    Temp := TextToShortcut(st);
    Key := Temp and not (scAlt or scShift or scCtrl);
    if (Key >= $30) and (Key <= $39) or
       (Key >= $41) and (Key <= $5A) then
    begin
      ShortcutToKey(Temp, Key, Shift);
      code := Key;
      flags := 0;
      if ssShift in Shift then
        flags := flags or FSHIFT;
      if ssAlt in Shift then
        flags := flags or FALT;
      if ssCtrl in Shift then
        flags := flags or FCONTROL;
        Ok := True;
    end
  end;

  if not Ok then
  begin
    code := TextToShortcut(st);
    flags := FVIRTKEY;
    if (code and scAlt) <> 0 then
      flags := flags or FALT;
    if (code and scShift) <> 0 then
      flags := flags or FSHIFT;
    if (code and scCtrl) <> 0 then
      flags := flags or FCONTROL;
    code := code and not (scAlt or scShift or scCtrl);
  end
end;

procedure TfmAcceleratorResource.SetObject(const Value: TObject);
var
  i : Integer;
  Accel : TAccelerator;
begin
  inherited;            // *Must* call inherited
  Application.ProcessMessages;

  FDetails := Obj as TAcceleratorResourceDetails;

  ListViewAccelerator.Items.BeginUpdate;
  try
    ListViewAccelerator.Items.Clear;
    for i := 0 to FDetails.Count - 1 do
    begin
      Accel := FDetails.Accelerator[i];
      with ListViewAccelerator.Items.Add do
      begin
        Caption := IntToStr(Accel.id);
        SubItems.Add(AcceleratorToText(Accel));
        if (Accel.flags and FVIRTKEY) <> 0 then
          SubItems.Add(rstVirtKey)
        else
          SubItems.Add(rstCharCode);
//        SubItems.Add(IntToStr(Accel.flags))
      end
    end
  finally
    ListViewAccelerator.Items.EndUpdate
  end
end;

procedure TfmAcceleratorResource.ListViewAcceleratorEdited(Sender: TObject;
  Item: TListItem; var S: string);
var
  newId : Integer;
  i : Integer;
begin
  inherited;
  FAllowEdit := False;
  actAccelDelete.ShortCut := actAccelDelete.Tag;    // Restore 'Delete' shortcut
  if s <> item.Caption then
  try
    newID := StrToInt(s);

    for i := 0 to FDetails.Count - 1 do
      if FDetails.Accelerator[i].id = newid then
        raise Exception.Create(rstDuplicateMessageID);

    item.Caption := s;          // need to do this so 'SaveResource' picks it up...
    SaveResource(rstChangeID);
  except
    s := item.Caption;
    raise
  end
end;

function TfmAcceleratorResource.GetMenuItem: TMenuItem;
begin
  Result := MenuItemAccelerators
end;

procedure TfmAcceleratorResource.actAccelAddExecute(Sender: TObject);
var
  item : TListItem;
  newID : Integer;
begin
  item := ListViewAccelerator.Items.Add;

  // Work out string / message ID
  if item.Index = 0 then
    item.Caption := '1'
  else
  begin
    newId := StrToInt(ListViewAccelerator.Items[ListViewAccelerator.Items.Count - 2].Caption);
    item.Caption := IntToStr(newId + 1)
  end;

  item.SubItems.Add('');
  item.SubItems.Add(rstVirtKey);
  item.Selected := True;

  FAdding := True;                    // Clear in mmoMessageExit

  actAccelModifyExecute(Sender)
end;

procedure TfmAcceleratorResource.actAccelModifyExecute(Sender: TObject);
var
  t : Integer;
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
    actAccelAdd.Execute;
end;

procedure TfmAcceleratorResource.ComboBoxKeyExit(Sender: TObject);
var
  st : string;
  Adding : Boolean;
begin
  Adding := FAdding;
  FAdding := False;
  ComboBoxKey.Visible := False;
  with ListViewAccelerator do if Assigned(Selected) then
  begin
    if FKeyChanged or FAdding then  // ie.  If it's changed
    begin
      selected.SubItems[0] := ComboBoxKey.Text;

      if Adding then
        st := rstAddAccelerator
      else
        st := rstChangeAccelerator;

      SaveResource(st);
    end
  end;
  FKeyChanged := False
end;

procedure TfmAcceleratorResource.SaveResource(const undoDetails: string);
var
  i, flags, code, id : Integer;
  item : TListItem;
  virtKey : Boolean;
begin
  AddUndoEntry(undoDetails);
  for i := 0 to ListViewAccelerator.Items.Count - 1 do
  begin
    item := ListViewAccelerator.Items[i];
    id := StrToInt(item.Caption);
    virtKey := item.SubItems[1] = rstVirtKey;
    TextToAccelerator(item.SubItems[0], virtKey, code, flags);

    if not virtKey and ((flags and FVIRTKEY) <> 0) then
      item.SubItems[1] := rstVirtKey;

    if i < FDetails.Count then
      FDetails.SetAccelDetails(i, flags, code, id)
    else
      FDetails.Add(flags, code, id)
  end;

  i := ListViewAccelerator.Items.Count;
  while i < FDetails.Count do
    FDetails.Delete(i);
end;

procedure TfmAcceleratorResource.ListViewAcceleratorDblClick(Sender: TObject);
var
  p : TPoint;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints(HWND_DESKTOP, ListViewAccelerator.Handle, p, 1);

    if p.x > ListViewAccelerator.Columns[0].Width + ListViewAccelerator.Columns[1].Width then
      actAccelChangeFlags.Execute
    else
    if p.x > ListViewAccelerator.Columns[0].Width then
      actAccelModify.Execute
    else
      actAccelChangeID.Execute
  end
  else
    actAccelAdd.Execute
end;

procedure TfmAcceleratorResource.FormCreate(Sender: TObject);

  procedure AddShortcutRange(const st : string; lowRange, highRange : char);
  var
    ch : char;
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

procedure TfmAcceleratorResource.ListViewAcceleratorEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := FAllowEdit;
  actAccelDelete.Tag := actAccelDelete.ShortCut;    // Suspend 'Delete' shortcut.
  actAccelDelete.ShortCut := 0;
end;

procedure TfmAcceleratorResource.ComboBoxKeyChange(Sender: TObject);
begin
  inherited;
  FKeyChanged := True;
end;

procedure TfmAcceleratorResource.actAccelChangeIDExecute(Sender: TObject);
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    FAllowEdit := true;
    ListViewAccelerator.Selected.EditCaption
  end
end;

procedure TfmAcceleratorResource.actAccelChangeFlagsExecute(Sender: TObject);
var
  t : Integer;
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

procedure TfmAcceleratorResource.ComboBoxTypeChange(Sender: TObject);
begin
  FKeyChanged := True;
end;

procedure TfmAcceleratorResource.ComboBoxTypeExit(Sender: TObject);
begin
  ComboBoxType.Visible := False;
  with ListViewAccelerator do if Assigned(Selected) then
  begin
    if FKeyChanged then  // ie.  If it's changed
    begin
      selected.SubItems[1] := ComboBoxType.Text;

      SaveResource(rstChangeAcceleratorType);
    end
  end;
  FKeyChanged := False
end;

procedure TfmAcceleratorResource.actAccelDeleteExecute(Sender: TObject);
var
  idx : Integer;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    idx := ListViewAccelerator.Selected.Index;
    ListViewAccelerator.Selected.Free;

    // Select next string
    if idx < ListViewAccelerator.Items.Count then
      ListViewAccelerator.Selected := ListViewAccelerator.Items[idx]
    else
    begin
      Dec(idx);
      if idx >= 0 then
        ListViewAccelerator.Selected := ListViewAccelerator.Items[idx]
    end;

   SaveResource(rstDeleteAccelerator)
  end
end;

end.
