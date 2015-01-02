unit MenuResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, unitResourceMenus, cmpMenuDesigner, menus, ExtCtrls,
  cmpPropertyListBox, ActnList;

type
  TfmMenuResource = class(TfmResource)
    Splitter1: TSplitter;
    Panel1: TPanel;
    PropertyListBox1: TPropertyListBox;
    Panel2: TPanel;
    MenuDesigner1: TMenuDesigner;
    alMenu: TActionList;
    mnuMenu: TMainMenu;
    PopupMenu1: TPopupMenu;
    actMenuDeleteItem: TAction;
    pomMenuDeleteITem: TMenuItem;
    mnuMenuITem: TMenuItem;
    mnuMenuDeleteItem: TMenuItem;
    actMenuInsertItem: TAction;
    InsetrItem1: TMenuItem;
    InsetrItem2: TMenuItem;
    actMenuAppendItem: TAction;
    actMenuAddChildItem: TAction;
    AddItemAfter1: TMenuItem;
    AddChildItem1: TMenuItem;
    N1: TMenuItem;
    AddItemAfter2: TMenuItem;
    AddChildItem2: TMenuItem;
    N2: TMenuItem;
    procedure MenuDesigner1SelectedItemChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actMenuDeleteItemExecute(Sender: TObject);
    procedure MenuDesigner1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PropertyListBox1PropertyChanged(Sender: TObject);
    procedure actMenuInsertItemExecute(Sender: TObject);
    procedure actMenuAppendItemExecute(Sender: TObject);
    procedure actMenuAddChildItemExecute(Sender: TObject);
  private
    fDetails : TMenuResourceDetails;
    procedure SaveResource (const undoDetails : string);
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem : TMenuItem; override;
  public
    procedure UpdateFonts; override;
  end;

var
  fmMenuResource: TfmMenuResource;

implementation

{$R *.DFM}

resourcestring
  rstCaption = 'Caption';
  rstShortcut = 'Shortcut';
  rstID = 'ID';
  rstEnabled = 'Enabled';
  rstChecked = 'Checked';
  rstNone = '(None)';
  rstCtrl = 'Ctrl';
  rstDel = 'Del';
  rstIns = 'Ins';

  rstDeleteItem = 'delete menu item';
  rstInsertItem = 'insert menu item';
  rstChangeItemCaption = 'change menu item caption';
  rstChangeItemShortcut = 'change menu item shortcut';
  rstChangeItemID = 'change menu item ID';
  rstChangeItemEnabled = 'change menu item enabled state';
  rstChangeItemChecked = 'change menu item checked state';

const
  taCaption = 0;
  taShortcut = 1;
  taID = 2;
  taEnabled = 3;
  taChecked = 4;

{ TfmMenuResource }

procedure TfmMenuResource.SetObject(const Value: TObject);
var
  item : TMenuItem;
begin
  inherited;
  fDetails := Obj as TMenuResourceDetails;

  item := TMenuItem.Create (nil);
  try
    fDetails.GetItems (item);
    MenuDesigner1.SetItems (item, False)
  finally
    item.Free
  end
end;

procedure TfmMenuResource.UpdateFonts;
begin
  useInternationalFont (PropertyListBox1.Font);
  useInternationalFont (MenuDesigner1.Font);
end;

procedure TfmMenuResource.MenuDesigner1SelectedItemChange(Sender: TObject);
var
  item : TDesignerMenuItem;
  s : WideString;
begin
  inherited;

  item := TDesignerMenuItem (MenuDesigner1.SelectedItem);

  with PropertyListBox1.FindProperty (rstCaption) do
  begin
    Tag := taCaption;
    PropertyValue := ExtractCaption (utf8Decode (item.Caption));
  end;

  with PropertyListBox1.FindProperty (rstShortcut) do
  begin
    Tag := taShortcut;
    s := ExtractShortcut (Utf8Decode (item.Caption));

    if s = '' then
      s := rstNone;

    PropertyValue := s
  end;

  with PropertyListBox1.FindProperty (rstID) do
  begin
    Tag := taID;
    PropertyValue := IntToStr (item.ID);
  end;

  with PropertyListBox1,FindProperty (rstEnabled) do
  begin
    Tag := taEnabled;
    PropertyValue := item.Enabled
  end;

  with PropertyListBox1,FindProperty (rstChecked) do
  begin
    Tag := taChecked;
    PropertyValue := item.Checked
  end
end;

procedure TfmMenuResource.FormShow(Sender: TObject);
var
  prop : TPropertyListProperty;

  procedure AddShortcutRange (lo, hi, flags : Word);
  var
    w : Word;
    s : string;
  begin
    for w := lo to hi do
    begin
      s := ShortCutToText (w + flags);
      if s <> '' then
        prop.EnumValues.Add (s)
    end
  end;


begin
  prop := PropertyListBox1.FindProperty (rstShortcut);

  prop.EnumValues.Add (rstNone);

  AddShortcutRange (Word ('A'), Word ('Z'), scCtrl);
  AddShortcutRange (Word ('A'), Word ('Z'), scCtrl + scAlt);

  AddShortcutRange (VK_F1, VK_F12, 0);
  AddShortcutRange (VK_F1, VK_F12, scCtrl);
  AddShortcutRange (VK_F1, VK_F12, scShift);
  AddShortcutRange (VK_F1, VK_F12, scCtrl + scCtrl);

  AddShortcutRange (VK_INSERT, VK_INSERT, 0);
  AddShortcutRange (VK_INSERT, VK_INSERT, scCtrl);
  AddShortcutRange (VK_INSERT, VK_INSERT, scShift);

  AddShortcutRange (VK_DELETE, VK_DELETE, 0);
  AddShortcutRange (VK_DELETE, VK_DELETE, scCtrl);
  AddShortcutRange (VK_DELETE, VK_DELETE, scShift);

  AddShortcutRange (VK_BACK, VK_BACK, scAlt);
  AddShortcutRange (VK_BACK, VK_BACK, scAlt + scShift);
end;

function TfmMenuResource.GetMenuItem: TMenuItem;
begin
  Result := mnuMenuItem
end;

procedure TfmMenuResource.actMenuDeleteItemExecute(Sender: TObject);
begin
  if Assigned (menuDesigner1.SelectedItem) then
  begin
    menuDesigner1.DeleteItem (menuDesigner1.SelectedItem);
    SaveResource (rstDeleteItem)
  end
end;

procedure TfmMenuResource.MenuDesigner1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;
  if key = VK_DELETE then
    actMenuDeleteItem.Execute;
end;

procedure TfmMenuResource.SaveResource(const undoDetails: string);
var
  sel : TMenuItem;
begin
  AddUndoEntry (undoDetails);
  sel := MenuDesigner1.SelectedItem;
  try
    MenuDesigner1.SelectedItem := nil;
    MenuDesigner1.RestoreTags;    // Ensure that tags hold the item ID - even if the item is
                                  // selected - that's what SetItems expects.

    fDetails.SetItems (MenuDesigner1.Items);
  finally
    MenuDesigner1.SelectedItem := sel
  end
end;

procedure TfmMenuResource.PropertyListBox1PropertyChanged(Sender: TObject);
var
  prop : TPropertyListProperty;
  s : WideString;
  idx : Integer;

begin
  s := '';
  prop := PropertyListBox1.Properties [PropertyListBox1.SelectedPropertyNo];
  case PropertyListBox1.SelectedPropertyNo of
    taCaption :
      begin
        MenuDesigner1.SelectedItem.Caption := Utf8Encode (MergeCaption (prop.PropertyValue, ExtractShortcut (Utf8Decode (MenuDesigner1.SelectedItem.Caption))));
        s := rstChangeItemCaption
      end;

    taShortcut :
      begin
        idx := prop.PropertyValue;
        s := prop.EnumValues [idx];
        if s = rstNone then
          s := '';
        MenuDesigner1.SelectedItem.Caption := Utf8Encode (MergeCaption (ExtractCaption (Utf8Decode (MenuDesigner1.SelectedItem.Caption)), s));
        s := rstChangeItemShortcut
      end;

    taID :
      begin
        if MenuDesigner1.SelectedItem is TDesignerMenuItem then
          TDesignerMenuItem (MenuDesigner1.SelectedItem).ID := prop.PropertyValue;
        s := rstChangeItemID
      end;

    taEnabled :
      begin
        MenuDesigner1.SelectedItem.Enabled := prop.PropertyValue;
        s := rstChangeItemEnabled
      end;

    taChecked:
      begin
        MenuDesigner1.SelectedItem.Checked := prop.PropertyValue;
        s := rstChangeItemChecked
      end
  end;

  if s <> '' then
    SaveResource (s)
end;

procedure TfmMenuResource.actMenuInsertItemExecute(Sender: TObject);
begin
  menuDesigner1.InsertItem (menuDesigner1.SelectedItem);
  SaveResource (rstInsertItem)
end;

procedure TfmMenuResource.actMenuAppendItemExecute(Sender: TObject);
begin
  menuDesigner1.AppendItem (menuDesigner1.SelectedItem);
  SaveResource (rstInsertItem)
end;

procedure TfmMenuResource.actMenuAddChildItemExecute(Sender: TObject);
begin
  menuDesigner1.AddChildItem (menuDesigner1.SelectedItem);
  SaveResource (rstInsertItem)
end;

end.
