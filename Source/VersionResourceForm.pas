(*======================================================================*
 | VersionResourceForm                                                  |
 |                                                                      |
 | Display/Edit version resources                                       |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 *======================================================================*)

unit VersionResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList,
  cmpPropertyListBox, unitResourceVersionInfo, cmpCWRichEdit;

//=======================================================================
// TfmResource class

type
  TfmVersionResource = class(TfmResource)
    MainMenu1: TMainMenu;
    mnuStrings: TMenuItem;
    mnuAddString: TMenuItem;
    mnuModifyString: TMenuItem;
    mnuDeleteString: TMenuItem;
    ActionList1: TActionList;
    actStringAddString: TAction;
    actStringModifyString: TAction;
    actStringDeleteString: TAction;
    actStringModifyStringName: TAction;
    PopupMenu1: TPopupMenu;
    AddString2: TMenuItem;
    ModifyString2: TMenuItem;
    DeleteString2: TMenuItem;
    Splitter1: TSplitter;
    lvVersionStrings: TListView;
    actStringModifyStringName1: TMenuItem;
    ModifyStringName1: TMenuItem;
    Panel1: TPanel;
    PropertyListBox1: TPropertyListBox;
    mmoMessage: TExRichEdit;
    procedure FormResize(Sender: TObject);
    procedure actStringDeleteStringExecute(Sender: TObject);
    procedure actStringModifyStringExecute(Sender: TObject);
    procedure mmoMessageExit(Sender: TObject);
    procedure lvVersionStringsEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure actStringModifyStringNameExecute(Sender: TObject);
    procedure lvVersionStringsDblClick(Sender: TObject);
    procedure actStringAddStringExecute(Sender: TObject);
    procedure PropertyListBox1PropertyChanged(Sender: TObject);
  private
    fInitializing : Boolean;
    fSelectedItem : TListItem;
    fAdding : Boolean;
    fDetails : TVersionInfoResourceDetails;
    procedure SaveFlags;
    function GetNewStringName : string;
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem : TMenuItem; override;
    procedure UpdateActions; override;
  public
    { Public declarations }
  end;

var
  fmVersionResource: TfmVersionResource;

implementation

uses DialogStrings;

{$R *.DFM}

//=======================================================================
// Translatable strings

resourcestring
  rstVersionFormatError   = 'Error in version string';
  rstChangeFlags          = 'version flags change';
  rstChangeProductVersion = 'product version change';
  rstChangeFileVersion    = 'file version change';
  rstDeleteString         = 'delete version string';
  rstChangeString         = 'modify version string';
  rstChangeStringName     = 'modify version string name';
  rstAddString            = 'add version string';
  rstNewString            = 'New String %d';

//=======================================================================
// Property constants

const
  prProductVersion = 0;
  prFileVersion = 1;
  prDebug = 2;
  prInferred = 3;
  prPatched = 4;
  prPreRelease = 5;
  prPrivateBuild = 6;
  prSpecialBuild = 7;

(*----------------------------------------------------------------------------*
 | function VersionToString ()                                                |
 |                                                                            |
 | Convert a version large integer to a string                                |
 |                                                                            |
 | Parameters:                                                                |
 |   version : TULargeInteger     The version integer to convert              |
 |                                                                            |
 | The function returns string representation of the version no               |
 *----------------------------------------------------------------------------*)
function VersionToString (version : TULargeInteger) : string;
begin
  with version do
    result := Format ('%d.%d.%d.%d', [HiWord (HighPart), LoWord (HighPart), HiWord (LowPart), LoWord (LowPart)]);
end;

(*----------------------------------------------------------------------------*
 | function StringToVersion ()                                                |
 |                                                                            |
 | Convert a version string to a large integer                                |
 |                                                                            |
 | Parameters:                                                                |
 |   version : string       The version string to convert                     |
 |                                                                            |
 | The function returns the integer representation of the version string      |
 *----------------------------------------------------------------------------*)
function StringToVersion (const version : string) : TULargeInteger;
var
  p : Integer;
  s : string;
  hh, h, l, ll : word;
  ok : boolean;
begin
  hh := 0;
  ll := 0;
  h := 0;
  l := 0;

  s := version;
  p := Pos ('.', s);
  ok := False;
  if p > 0 then
  begin
    hh := StrToInt (Copy (s, 1, p - 1));
    s := Copy (s, p + 1, MaxInt);
    p := Pos ('.', s);
    if p > 0 then
    begin
      h := StrToInt (Copy (s, 1, p - 1));
      s := Copy (s, p + 1, MaxInt);
      p := Pos ('.', s);
      if p > 0 then
      begin
        l := StrToInt (Copy (s, 1, p - 1));
        ll := StrToInt (Copy (s, p + 1, MaxInt));
        ok := True;
      end
    end
  end;

  if not ok then
    raise exception.Create (rstVersionFormatError);

  result.HighPart := 65536 * hh + h;
  result.LowPart := 65536 * l + ll;
end;


{ TfmVersionResource }

(*----------------------------------------------------------------------*
 | TfmVersionResource.GetMenuItem                                       |
 |                                                                      |
 | Return out forms menu item to the framework.                         |
 *----------------------------------------------------------------------*)
function TfmVersionResource.GetMenuItem: TMenuItem;
begin
  Result := mnuStrings
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.SaveFlags                                         |
 |                                                                      |
 | Save the version flags, product version and file version if they've  |
 | changed.                                                             |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.SaveFlags;
var
  flags : TVersionFileFlags;
  v : TULargeInteger;
begin
  if not fInitializing then     // Ignore check box 'OnClick' handlers
                                // when we're being initialized.

    with fDetails do
    begin
      flags := FileFlags;
      if PropertyListBox1.Properties [prDebug].PropertyValue then flags := flags + [ffDebug] else flags := flags - [ffDebug];
      if PropertyListBox1.Properties [prInferred].PropertyValue then flags := flags + [ffInfoInferred] else flags := flags - [ffInfoInferred];
      if PropertyListBox1.Properties [prPatched].PropertyValue then flags := flags + [ffPatched] else flags := flags - [ffPatched];
      if PropertyListBox1.Properties [prPreRelease].PropertyValue then flags := flags + [ffPreRelease] else flags := flags - [ffPreRelease];
      if PropertyListBox1.Properties [prPrivateBuild].PropertyValue then flags := flags + [ffPrivateBuild] else flags := flags - [ffPrivateBuild];
      if PropertyListBox1.Properties [prSpecialBuild].PropertyValue then flags := flags + [ffSpecialBuild] else flags := flags - [ffSpecialBuild];

      if flags <> FileFlags then        // Has a flag changed ?
      begin
        AddUndoEntry (rstChangeFlags);
        FileFlags := flags
      end;

      v := StringToVersion (PropertyListBox1.Properties [prProductVersion].PropertyValue);
                                        // Has the product version changed ??
      if v.QuadPart <> ProductVersion.QuadPart then
      begin
        AddUndoEntry (rstChangeProductVersion);
        ProductVersion := v
      end;

      v := StringToVersion (PropertyListBox1.Properties [prFileVersion].PropertyValue);
                                        // Has the file version changed ??
      if v.QuadPart <> FileVersion.QuadPart then
      begin
        AddUndoentry (rstChangeFileVersion);
        FileVersion := v
      end
    end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.SetObject                                         |
 |                                                                      |
 | Overriden 'Set' method for ancestor Obj property.  The 'Obj' must be |
 | a TVersionInfoResourceDetails object.                                |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.SetObject(const Value: TObject);
var
  fVersion : TULargeInteger;
  pVersion : TULargeInteger;
  flags : TVersionFileFlags;
  k : TVersionStringValue;
  i : Integer;
begin
  inherited;
  fDetails := ResourceDetails as TVersionInfoResourceDetails;
  fInitializing := True;
  with fDetails do
  try
    fVersion := FileVersion;
    pVersion := ProductVersion;
    flags := FileFlags;

    // Initialize the form

    PropertyListBox1.Properties [prFileVersion].PropertyValue := VersionToString (fVersion);
    PropertyListBox1.Properties [prProductVersion].PropertyValue := VersionToString (pVersion);

    PropertyListBox1.Properties [prDebug].PropertyValue := ffDebug in flags;
    PropertyListBox1.Properties [prInferred].PropertyValue := ffInfoInferred in flags;
    PropertyListBox1.Properties [prPatched].PropertyValue := ffPatched in flags;
    PropertyListBox1.Properties [prPreRelease].PropertyValue := ffPreRelease in flags;
    PropertyListBox1.Properties [prPrivateBuild].PropertyValue := ffPrivateBuild in flags;
    PropertyListBox1.Properties [prSpecialBuild].PropertyValue := ffSpecialBuild in flags;

    lvVersionStrings.Items.BeginUpdate;
    with lvVersionStrings.Items do
    try
      Clear;
      for i := 0 to KeyCount - 1 do
        with Add do
        begin
          k := Key [i];
          Caption := k.KeyName;
          SubItems.Add (StringToCString (k.Value));
        end;
      if Count > 0 then
        lvVersionStrings.ItemIndex := 0
    finally
      EndUpdate
    end
  finally
    fInitializing := False
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringDeleteStringExecute                      |
 |                                                                      |
 | Delete the selected version string                                   |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.actStringDeleteStringExecute(Sender: TObject);
var
  n : Integer;
begin
  if Assigned (lvVersionStrings.Selected) then
  begin
    AddUndoEntry (rstDeleteString);
    n := lvVersionStrings.Selected.Index;

    // Delete the string from the resouce
    fDetails.DeleteKey (n);

    // Delete the string from the list view
    lvVersionStrings.Selected.Delete;

    // Select the next entry in the list after deleting
    if n >= lvVersionStrings.Items.Count then
      n := lvVersionStrings.Items.Count - 1;

    if n >= 0 then
      lvVersionStrings.Items.Item [n].Selected := True
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringModifyStringExecute                      |
 |                                                                      |
 | Start modifying the version string.  Reposition and reveal the       |
 | hidden memo.                                                         |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.actStringModifyStringExecute(Sender: TObject);
var
  idx : Integer;
begin
  if Assigned (lvVersionStrings.Selected) then
  begin
    fSelectedItem := lvVersionStrings.Selected;
    idx := fDetails.IndexOf(fSelectedItem.Caption);
    if idx > -1 then
    begin
      mmoMessage.Width := lvVersionStrings.Width - 2;
      mmoMessage.Top := lvVersionStrings.Selected.DisplayRect (drLabel).Bottom + 1;
      mmoMessage.Left := lvVersionStrings.Left + 1;
      mmoMessage.Text := fDetails.Key [idx].Value;
      mmoMessage.Visible := True;
      mmoMessage.Enabled := True;
      mmoMessage.SetFocus
    end
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.mmoMessageExit                                    |
 |                                                                      |
 | They'v existed the memo.  Re-coneal it, and change the modified      |
 | string valur                                                         |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.mmoMessageExit(Sender: TObject);
begin
  mmoMessage.Enabled := False;
  mmoMessage.Visible := False;
  if mmoMessage.CanUndo or fAdding then
  begin
    if fAdding then
      AddUndoEntry (rstAddString)
    else
      AddUndoEntry (rstChangeString);

                                // Update the resource
    fDetails.SetKeyValue (fSelectedItem.Caption, mmoMessage.Text);

                                // Update the list view
    fSelectedItem.SubItems [0] := StringToCString (mmoMessage.Text)
  end;
  fAdding := False;
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.lvVersionStringsEdited                            |
 |                                                                      |
 | They've finished editing the key name.  Update the resource          |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.lvVersionStringsEdited(Sender: TObject;
  Item: TListItem; var S: String);
begin
  if s <> Item.Caption then
  begin
    AddUndoEntry (rstChangeStringName);
    fDetails.ChangeKey (Item.Caption, s)
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringModifyStringNameExecute                  |
 |                                                                      |
 | Start modifying the key name                                         |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.actStringModifyStringNameExecute(
  Sender: TObject);
begin
  if Assigned (lvVersionStrings.Selected) then
    lvVersionStrings.Selected.EditCaption
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.lvVersionStringsDblClick                          |
 |                                                                      |
 | Check where abouts on the list view is being clicked.                |
 |                                                                      |
 | If a selected item is clicked in the 'Key Name' column, start        |
 | editing the key name.                                                |
 |                                                                      |
 | If a selected item is clicked in the 'String' column, start editing  |
 | the string value.                                                    |
 |                                                                      |
 | If no item was selcted, add a new string.                            |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.lvVersionStringsDblClick(Sender: TObject);
var
  p : TPoint;
begin
  if Assigned (lvVersionStrings.Selected) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints (HWND_DESKTOP, lvVersionStrings.Handle, p, 1);

    if p.x > lvVersionStrings.Columns [0].Width then
      actStringModifyString.Execute
    else
      actStringModifyStringName.Execute
  end
  else
    actStringAddString.Execute
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.UpdateActions                                     |
 |                                                                      |
 | Enable or disable the 'Strings' menu items appropriately             |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.UpdateActions;
var
  sel : Boolean;
begin
  sel := Assigned (lvVersionStrings.Selected) and lvVersionStrings.Focused;
  actStringDeleteString.Enabled := sel;
  actStringModifyString.Enabled := sel;
  actStringModifyStringName.Enabled := sel
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringAddStringExecute                         |
 |                                                                      |
 | Add a new string.  Simply add it to the list view, then call Modify  |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.actStringAddStringExecute(Sender: TObject);
var
  keyName : string;
begin
  keyName := GetNewStringName;
  with lvVersionStrings.Items.Add do
  begin
    Caption := keyName;
    SubItems.Add ('');
    Selected := True;
    fAdding := True;
    actStringModifyString.Enabled := True;
    actStringModifyString.Execute
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.GetNewStringName                                  |
 |                                                                      |
 | Calculate the default name for new string values.                    |
 *----------------------------------------------------------------------*)
function TfmVersionResource.GetNewStringName: string;
var
  m : Integer;
begin
  m := 1;
  repeat
    Result := Format (rstNewString, [m]);

    if lvVersionStrings.FindCaption (0, Result, False, True, False) = nil then
      break;

    Inc (m)
  until m = 0
end;

procedure TfmVersionResource.PropertyListBox1PropertyChanged(
  Sender: TObject);
begin
  SaveFlags
end;

procedure TfmVersionResource.FormResize(Sender: TObject);
begin
  inherited;

  if mmoMessage.Visible then
    mmoMessage.Width := lvVersionStrings.Width - 2;
end;

end.
