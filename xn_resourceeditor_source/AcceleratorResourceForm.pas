unit AcceleratorResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Menus, Graphics, Controls, Forms,
  Dialogs, ResourceForm, ComCtrls, unitResourceAccelerator, StdCtrls, ActnList;

type
  TfmAcceleratorResource = class(TfmResource)
    lvAccelerator: TListView;
    ActionList1: TActionList;
    pomAccel: TPopupMenu;
    mnuAccelMenu: TMainMenu;
    actAccelAdd: TAction;
    actAccelDelete: TAction;
    actAccelModify: TAction;
    actAccelChangeID: TAction;
    AddAccelerator1: TMenuItem;
    ModifyAccelerator1: TMenuItem;
    DeleteAccelerator1: TMenuItem;
    ChangeID1: TMenuItem;
    mnuAccelerators: TMenuItem;
    AddAccelerator2: TMenuItem;
    ModifyAccelerator2: TMenuItem;
    DeleteAccelerator2: TMenuItem;
    ChangeID2: TMenuItem;
    cbKey: TComboBox;
    cbType: TComboBox;
    actAccelChangeFlags: TAction;
    ChangeFlags1: TMenuItem;
    ChangeFlags2: TMenuItem;
    procedure actAccelDeleteExecute(Sender: TObject);
    procedure cbTypeExit(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure actAccelChangeFlagsExecute(Sender: TObject);
    procedure actAccelChangeIDExecute(Sender: TObject);
    procedure cbKeyChange(Sender: TObject);
    procedure lvAcceleratorEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lvAcceleratorDblClick(Sender: TObject);
    procedure cbKeyExit(Sender: TObject);
    procedure actAccelModifyExecute(Sender: TObject);
    procedure actAccelAddExecute(Sender: TObject);
    procedure lvAcceleratorEdited(Sender: TObject; Item: TListItem;
      var S: string);
  private
    fAdding : boolean;
    fKeyChanged : boolean;
    fDetails : TAcceleratorResourceDetails;
    fAllowEdit : boolean;
  protected
    function GetMenuItem : TMenuItem; override;
    procedure SetObject(const Value: TObject); override;
  public
    procedure SaveResource (const undoDetails : string);
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

function AcceleratorToText (const accel : TAccelerator) : string;
begin
  if (accel.flags and FVIRTKEY) <> 0 then
    result := ShortcutToText (accel.code)
  else
    if accel.code < 32 then
      result := 'Ctrl+' + char (accel.code + Ord ('@'))
    else
      result := Char (accel.code);

  if (accel.flags and FSHIFT) <> 0 then
    result := 'Shift+' + result;

  if (accel.flags and FALT) <> 0 then
    result := 'Alt+' + result;

  if (accel.flags and FCONTROL) <> 0 then
    result :='Ctrl+' + result
end;

procedure TextToAccelerator (const st : string; virtKey : boolean; var code, flags : Integer);
var
  temp : Integer;
  key : word;
  shift : TShiftState;
  ok : boolean;
begin
  ok := False;
  if not virtKey then
  begin
    temp := TextToShortcut (st);
    key := temp and not (scAlt or scShift or scCtrl);
    if (key >= $30) and (key <= $39) or
       (key >= $41) and (key <= $5A) then
    begin
      ShortcutToKey (temp, key, shift);
      code := key;
      flags := 0;
      if ssShift in shift then
        flags := flags or FSHIFT;
      if ssAlt in shift then
        flags := flags or FALT;
      if ssCtrl in shift then
        flags := flags or FCONTROL;
        ok := True;
    end
  end;

  if not ok then
  begin
    code := TextToShortcut (st);
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
  accel : TAccelerator;
begin
  inherited;            // *Must* call inherited
  Application.ProcessMessages;

  fDetails := Obj as TAcceleratorResourceDetails;

  lvAccelerator.Items.BeginUpdate;
  try
    lvAccelerator.Items.Clear;
    for i := 0 to fDetails.Count - 1 do
    begin
      accel := fDetails.Accelerator [i];
      with lvAccelerator.Items.Add do
      begin
        Caption := IntToStr (accel.id);
        SubItems.Add(AcceleratorToText (accel));
        if (accel.flags and FVIRTKEY) <> 0 then
          SubItems.Add(rstVirtKey)
        else
          SubItems.Add(rstCharCode);
//        SubItems.Add(IntToStr (accel.flags))
      end
    end
  finally
    lvAccelerator.Items.EndUpdate
  end
end;

procedure TfmAcceleratorResource.lvAcceleratorEdited(Sender: TObject;
  Item: TListItem; var S: string);
var
  newId : Integer;
  i : Integer;
begin
  inherited;
  fAllowEdit := False;
  actAccelDelete.ShortCut := actAccelDelete.Tag;    // Restore 'Delete' shortcut
  if s <> item.Caption then
  try
    newID := StrToInt (s);

    for i := 0 to fDetails.Count - 1 do
      if fDetails.Accelerator [i].id = newid then
        raise Exception.Create (rstDuplicateMessageID);

    item.Caption := s;          // need to do this so 'SaveResource' picks it up...
    SaveResource (rstChangeID);
  except
    s := item.Caption;
    raise
  end
end;

function TfmAcceleratorResource.GetMenuItem: TMenuItem;
begin
  result := mnuAccelerators
end;

procedure TfmAcceleratorResource.actAccelAddExecute(Sender: TObject);
var
  item : TListItem;
  newID : Integer;
begin
  item := lvAccelerator.Items.Add;

                                        // Work out string / message ID
  if item.Index = 0 then
    item.Caption := '1'
  else
  begin
    newId :=StrToInt (lvAccelerator.Items [lvAccelerator.Items.Count - 2].Caption);
    item.Caption := IntToStr (newId + 1)
  end;

  item.SubItems.Add ('');
  item.SubItems.Add (rstVirtKey);
  item.Selected := True;

  fAdding := True;                    // Clear in mmoMessageExit

  actAccelModifyExecute (Sender)
end;

procedure TfmAcceleratorResource.actAccelModifyExecute(Sender: TObject);
var
  t : Integer;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    cbKey.Width := lvAccelerator.Columns [1].Width - 2;
    cbKey.Left := lvAccelerator.Columns [0].Width + 2;
    t := lvAccelerator.Selected.DisplayRect (drLabel).Top - 3;
    if t < 0 then
      t := 0;
    cbKey.Top := t;
    cbKey.Text := lvAccelerator.Selected.SubItems [0];
    cbKey.Visible := True;
    cbKey.SetFocus
  end
  else
    actAccelAdd.Execute;
end;

procedure TfmAcceleratorResource.cbKeyExit(Sender: TObject);
var
  st : string;
  adding : Boolean;
begin
  adding := fAdding;
  fAdding := False;
  cbKey.Visible := False;
  with lvAccelerator do if Assigned (Selected) then
  begin
    if fKeyChanged or fAdding then  // ie.  If it's changed
    begin
      selected.SubItems [0] := cbKey.Text;

      if adding then
        st := rstAddAccelerator
      else
        st := rstChangeAccelerator;

      SaveResource (st);
    end
  end;
  fKeyChanged := False
end;

procedure TfmAcceleratorResource.SaveResource(const undoDetails: string);
var
  i, flags, code, id : Integer;
  item : TListItem;
  virtKey : boolean;
begin
  AddUndoEntry (undoDetails);
  for i := 0 to lvAccelerator.Items.Count - 1 do
  begin
    item := lvAccelerator.Items [i];
    id := StrToInt (item.Caption);
    virtKey := item.SubItems [1] = rstVirtKey;
    TextToAccelerator (item.SubItems [0], virtKey, code, flags);

    if not virtKey and ((flags and FVIRTKEY) <> 0) then
      item.SubItems [1] := rstVirtKey;

    if i < fDetails.Count then
      fDetails.SetAccelDetails (i, flags, code, id)
    else
      fDetails.Add(flags, code, id)
  end;

  i := lvAccelerator.Items.Count;
  while i < fDetails.Count do
    fDetails.Delete(i);
end;

procedure TfmAcceleratorResource.lvAcceleratorDblClick(Sender: TObject);
var
  p : TPoint;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints (HWND_DESKTOP, lvAccelerator.Handle, p, 1);

    if p.x > lvAccelerator.Columns [0].Width + lvAccelerator.Columns [1].Width then
      actAccelChangeFlags.Execute
    else
      if p.x > lvAccelerator.Columns [0].Width then
        actAccelModify.Execute
      else
        actAccelChangeID.Execute
  end
  else
    actAccelAdd.Execute
end;

procedure TfmAcceleratorResource.FormCreate(Sender: TObject);

  procedure AddShortcutRange (const st : string; lowRange, highRange : char);
  var
    ch : char;
  begin
    for ch := lowRange to highRange do
      cbKey.Items.Add(st + ch)
  end;

begin
  inherited;

  cbKey.Items.BeginUpdate;
  try
    AddShortcutRange ('Ctrl+', 'A', 'Z');
    AddShortcutRange ('Ctrl+Alt+', 'A', 'Z');
    AddShortcutRange ('F', '1', '9');
    AddShortcutRange ('F1', '0', '2');
    AddShortcutRange ('Ctrl+F', '1', '9');
    AddShortcutRange ('Ctrl+F1', '0', '2');
    AddShortcutRange ('Shift+F', '1', '9');
    AddShortcutRange ('Shift+F1', '0', '2');
    AddShortcutRange ('Shift+Ctrl+F', '1', '9');
    AddShortcutRange ('Shift+Ctrl+F1', '0', '2');
    AddShortcutRange ('In', 's', 's');
    AddShortcutRange ('Shift+In', 's', 's');
    AddShortcutRange ('Ctrl+In', 's', 's');
    AddShortcutRange ('De', 'l', 'l');
    AddShortcutRange ('Shift+De', 'l', 'l');
    AddShortcutRange ('Ctrl+De', 'l', 'l');
    AddShortcutRange ('Alt+BkS', 'p', 'p');
    AddShortcutRange ('Shift+Alt+BkS', 'p', 'p');
  finally
    cbKey.Items.EndUpdate
  end;

  cbType.Items.Add(rstVirtKey);
  cbType.Items.Add(rstCharCode);
end;

procedure TfmAcceleratorResource.lvAcceleratorEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := fAllowEdit;
  actAccelDelete.Tag := actAccelDelete.ShortCut;    // Suspend 'Delete' shortcut.
  actAccelDelete.ShortCut := 0;
end;

procedure TfmAcceleratorResource.cbKeyChange(Sender: TObject);
begin
  inherited;
  fKeyChanged := True;
end;

procedure TfmAcceleratorResource.actAccelChangeIDExecute(Sender: TObject);
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    fAllowEdit := true;
    lvAccelerator.Selected.EditCaption
  end
end;

procedure TfmAcceleratorResource.actAccelChangeFlagsExecute(Sender: TObject);
var
  t : Integer;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    cbType.Width := lvAccelerator.Columns [2].Width - 2;
    cbType.Left := lvAccelerator.Columns [0].Width + lvAccelerator.Columns [1].Width + 2;
    t := lvAccelerator.Selected.DisplayRect (drLabel).Top - 3;
    if t < 0 then
      t := 0;
    cbType.Top := t;
    cbType.Text := lvAccelerator.Selected.SubItems [1];
    cbType.Visible := True;
    cbType.SetFocus
  end
end;

procedure TfmAcceleratorResource.cbTypeChange(Sender: TObject);
begin
  fKeyChanged := True;
end;

procedure TfmAcceleratorResource.cbTypeExit(Sender: TObject);
begin
  cbType.Visible := False;
  with lvAccelerator do if Assigned (Selected) then
  begin
    if fKeyChanged then  // ie.  If it's changed
    begin
      selected.SubItems [1] := cbType.Text;

      SaveResource (rstChangeAcceleratorType);
    end
  end;
  fKeyChanged := False
end;

procedure TfmAcceleratorResource.actAccelDeleteExecute(Sender: TObject);
var
  idx : Integer;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    idx := lvAccelerator.Selected.Index;
    lvAccelerator.Selected.Free;

                                // Select next string
    if idx < lvAccelerator.Items.Count then
      lvAccelerator.Selected := lvAccelerator.Items [idx]
    else
    begin
      Dec (idx);
      if idx >= 0 then
        lvAccelerator.Selected := lvAccelerator.Items [idx]
    end;

   SaveResource (rstDeleteAccelerator)
  end
end;

end.
