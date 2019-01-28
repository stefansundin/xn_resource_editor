(*======================================================================*
 | unit TextResourceForm                                                |
 |                                                                      |
 | Display / edit text (string and message) resources                   |
 |                                                                      |
 | Beware - string table ids must be numeric, consecutive, etc.         |
 |                                                                      |
 | ** Gold code - 24/4/2001 **                                          |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      31/01/2001  CPWW  Original                                  |
 *======================================================================*)

unit TextResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, ComCtrls, unitResourceMessages, ActnList, Menus, StdCtrls, ConTnrs,
  cmpCWRichEdit, VirtualTrees;

type
  TfmTextResource = class(TfmResource)
    mnuStringsMenu: TMainMenu;
    mnuStrings: TMenuItem;
    ActionList1: TActionList;
    actStringsAdd: TAction;
    actStringsModify: TAction;
    actStringsDelete: TAction;
    AddString1: TMenuItem;
    ModifyString1: TMenuItem;
    DeleteString1: TMenuItem;
    pomStrings: TPopupMenu;
    AddString2: TMenuItem;
    ModifyString2: TMenuItem;
    DeleteString2: TMenuItem;
    actStringsChangeID: TAction;
    ChangeID1: TMenuItem;
    ChangeID2: TMenuItem;
    mmoMessage: TExRichEdit;
    vstStrings: TVirtualStringTree;
    procedure vstStringsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure FormShow(Sender: TObject);
    procedure vstStringsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstStringsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstStringsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstStringsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure actStringsModifyExecute(Sender: TObject);
    procedure mmoMessageExit(Sender: TObject);
    procedure actStringsDeleteExecute(Sender: TObject);
    procedure actStringsAddExecute(Sender: TObject);
    procedure actStringsChangeIDExecute(Sender: TObject);
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem : TMenuItem; override;
    procedure UpdateActions; override;
  private
    fDetails : TTextResourceDetails;
    fAdding : Boolean;
    fChangeId : Boolean;
    fHexMode : Boolean;
    fIsStrings : Boolean;
    fWorkStrings : TObjectList;
    procedure SaveResource (const undoDetails : string);
    function SelectedString : TStringInfo;
    function NodeString(node: PVirtualNode): TStringInfo;
    procedure UpdateDisplay (selectedString : TStringInfo);
    function NodeN (n : Integer) : PVirtualNode;
  public
    procedure TidyUp; override;
    procedure UpdateFonts; override;
  end;

var
  fmTextResource: TfmTextResource;

implementation

uses DialogStrings;

{$R *.DFM}

resourcestring
  rstChangeString  = 'change string';
  rstDeleteString  = 'delete string';
  rstChangeMessage = 'change message';
  rstDeleteMessage = 'delete message';
  rstAddString     = 'add string';
  rstAddMessage    = 'add message';
  rstChangeID      = 'change message ID';
  rstDuplicateMessageID = 'Duplicate Message ID';

{ TfmTextResource }

(*----------------------------------------------------------------------*
 | TfmTextResource.actStringsAddExecute                                 |
 |                                                                      |
 | Add a new string.  Just create a new item in the list view, then     |
 | modify it.                                                           |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.actStringsAddExecute(Sender: TObject);
var
  newID : Integer;
  si : TStringInfo;
begin
  if (fWorkStrings.Count < 16) or not fIsStrings then
  begin
    if fWorkStrings.Count = 0 then
      if fIsStrings then
        newID := StrToIntDef (ResIdToStringsId (fDetails.ResourceName), 1)
      else
        newID := 1
    else
      newId := TStringInfo (fWorkStrings [fWorkStrings.Count - 1]).Id + 1;

    si := TStringInfo.Create('', newId);
    fWorkStrings.Add(si);
    UpdateDisplay (si);
    actStringsModifyExecute (nil);
  end
end;

procedure TfmTextResource.actStringsChangeIDExecute(Sender: TObject);
begin
  if Assigned (vstStrings.FocusedNode) then
    if not fIsStrings then
      vstStrings.EditNode(vstStrings.FocusedNode, 0)
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.actStringsDeleteExecute                              |
 |                                                                      |
 | Delete the selected string                                           |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.actStringsDeleteExecute(Sender: TObject);
var
  idx : Integer;
  p : PVirtualNode;
begin
  if Assigned (vstStrings.FocusedNode) then
  begin
    idx := vstStrings.FocusedNode^.Index;
    fWorkStrings.Delete(idx);
    UpdateDisplay (Nil);

    while idx >= fWorkStrings.Count do
      Dec (idx);

    p := NodeN (idx);
    if Assigned (p) then
      vstStrings.Selected [p] := True;

    if fDetails is TMessageResourceDetails then
      SaveResource (rstDeleteMessage)
    else
      SaveResource (rstDeleteString)
  end
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.actStringsModifyExecute                              |
 |                                                                      |
 | Allow the user to edit a string by unhiding and repositioning the    |
 | hidden memo                                                          |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.actStringsModifyExecute(Sender: TObject);
var
  r : TRect;
begin
  if Assigned (vstStrings.FocusedNode) then
  begin
    mmoMessage.Width := vstStrings.Width - 2;
    r := vstStrings.GetDisplayRect(vstStrings.FocusedNode, 1, False);
    mmoMessage.Left := r.Left + 2;
    mmoMessage.Width := r.Right - r.Left + 2;
    mmoMessage.Top := r.Bottom;
    if mmoMessage.Top + mmoMessage.Height > vstStrings.Top + vstStrings.Height then
      mmoMessage.Top := r.Top - mmoMessage.Height;
    mmoMessage.Visible := True;
    mmoMessage.Text := SelectedString.St;
    mmoMessage.SetFocus
  end
  else
    actStringsAdd.Execute;
end;

procedure TfmTextResource.FormCreate(Sender: TObject);
begin
  inherited;

  fWorkStrings := TObjectList.Create
end;

procedure TfmTextResource.FormDestroy(Sender: TObject);
begin
  fWorkStrings.Free;
  inherited;
end;

procedure TfmTextResource.FormShow(Sender: TObject);
begin
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.GetMenuItem                                          |
 |                                                                      |
 | Override GetMenuItem to return our menu to the resource editor       |
 | framework                                                            |
 *----------------------------------------------------------------------*)
function TfmTextResource.GetMenuItem: TMenuItem;
begin
  result := mnuStrings
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.mmoMessageExit                                       |
 |                                                                      |
 | Finished editing the string.  Hide the memo, and update the resource |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.mmoMessageExit(Sender: TObject);
var
  st : string;
  adding : Boolean;
  si : TStringInfo;
  ws : WideString;
begin
  adding := fAdding;
  fAdding := False;
  mmoMessage.Visible := False;

  si := SelectedString;

  if mmoMessage.CanUndo or adding then  // ie.  If it's changed
  begin
    ws := mmoMessage.Text;
    si.St := ws;

    if adding then
      if fDetails is TMessageResourceDetails then
        st := rstAddmessage
      else
        st := rstAddString
    else
      if fDetails is TMessageResourceDetails then
        st := rstChangeMessage
      else
        st := rstChangeString;

    SaveResource (st);
    mmoMessage.ClearUndoBuffer;
  end
end;

function TfmTextResource.NodeN(n: Integer): PVirtualNode;
begin
  if n >= 0 then
  begin
    result := vstStrings.GetFirst;
    while (result <> Nil) and (n > 0) do
    begin
      result := vstStrings.GetNextSibling(result);
      Dec (n)
    end
  end
  else
    result := Nil
end;

function TfmTextResource.NodeString(node : PVirtualNode) : TStringInfo;
begin
  if Assigned (node) and (Integer (node^.Index) < fWorkStrings.Count) then
    result := TStringInfo (fWorkStrings [node^.Index])
  else
    result := Nil
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.SaveResource                                         |
 |                                                                      |
 | Update the resource from the list box strings.                      |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.SaveResource(const undoDetails: string);
var
  i : Integer;
  si : TStringInfo;
begin
  AddUndoEntry (undoDetails);
  fDetails.BeginUpdate;
  try
    for i := 0 to fWorkStrings.Count - 1 do
    begin
      si := TStringInfo(fWorkStrings [i]);

      fDetails.Strings [i] := si.St;
      fDetails.Ids [i] := si.Id
    end;

    i := fWorkStrings.Count;
    while i < fDetails.Count do
      fDetails.Delete(i);
  finally
    fDetails.EndUpdate
  end
end;

function TfmTextResource.SelectedString: TStringInfo;
begin
  result := NodeString (vstStrings.FocusedNode);
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.SetObject                                            |
 |                                                                      |
 | Initialize the form with data from the object                        |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.SetObject(const Value: TObject);
var
  i : Integer;
begin
  inherited;            // *Must* call inherited

  fDetails := ResourceDetails as TTextResourceDetails;

  fIsStrings := not (fDetails is TMessageResourceDetails);
  fHexMode := not fIsStrings;

  fWorkStrings.Clear;
  for i := 0 to fDetails.Count - 1 do
    fWorkStrings.Add(TStringInfo.Create(fDetails [i], fDetails.Ids [i]));

  UpdateDisplay (Nil);
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.UpdateActions                                        |
 |                                                                      |
 | Disallow modifying/deleting if nothing's selected                    |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.UpdateActions;
var
  sel : Boolean;
begin
  sel := Assigned (vstStrings.FocusedNode);

  actStringsAdd.Enabled := fDetails is TMessageResourceDetails;
  actStringsModify.Enabled := sel;
  actStringsDelete.Enabled := sel;
  actStringsChangeID.Enabled := sel and not fIsStrings;

  if fChangeId then
  begin
    fChangeID := False;
    actStringsChangeId.Execute
  end
end;

procedure TfmTextResource.UpdateDisplay(selectedString: TStringInfo);
var
  p : PVirtualNode;
  i : Integer;
begin
  vstStrings.BeginUpdate;
  try
    if fWorkStrings.Count <> Integer (vstStrings.RootNodeCount) then
      vstStrings.RootNodeCount := fWorkStrings.Count
    else
      vstStrings.ReinitNode(Nil, True);
  finally
    vstStrings.EndUpdate
  end;

  vstStrings.ClearSelection;
  if Assigned (selectedString) then
  begin
    i := fWorkStrings.IndexOf (selectedString);
    p := NodeN (i);
    if p <> Nil then
    begin
      vstStrings.Selected [p] := True;
      vstStrings.FocusedNode := p
    end
  end
end;

procedure TfmTextResource.UpdateFonts;
begin
  UseInternationalFont (mmoMessage.Font);
  UseInternationalFont (vstStrings.Font);
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.vstStringsDblClick                                    |
 |                                                                      |
 | Respond to double click.  Modify (or add) a string                   |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.vstStringsDblClick(Sender: TObject);
var
  p : TPoint;
begin
  if Assigned (vstStrings.FocusedNode) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints (0, vstStrings.Handle, p, 1);
    if p.X <= vstStrings.Header.Columns [0].Width then
      fChangeId := True
    else
      actStringsModify.Execute
  end
  else
    actStringsAdd.Execute
end;

procedure TfmTextResource.vstStringsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  si : TStringInfo;
begin
  si := NodeString (Node);

  if si <> Nil then
  case Column of
    0 : if fHexMode then
          CellText := IntToHex (si.Id , 8)
        else
          CellText := IntToStr (si.Id);
    1 : CellText := si.St
  end
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.vstStringsKeyDown                                     |
 |                                                                      |
 | Respond to the 'Return' key - Modify the string if it's pressed.     |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.vstStringsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    actStringsModify.Execute
end;

procedure TfmTextResource.vstStringsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  i, newID : Integer;
  st : TStringInfo;
begin
  if Column <> 0 then
    Exit;

  st := NodeString (node);
  if not Assigned (st) then
    Exit;

  try
    if fHexMode then
      newId := StrToInt ('$' + NewText)
    else
      newID := StrToInt (NewText);

    if st.Id <> newID then
    begin
      for i := 0 to fDetails.Count - 1 do
        if fDetails.Ids [i] = newID then
          raise Exception.Create (rstDuplicateMessageID);

      st.Id := NewId;
      SaveResource (rstChangeID)
    end
  except
    raise
  end
end;

procedure TfmTextResource.vstStringsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (Column = 0) and (NodeString (Node) <> Nil);
end;

procedure TfmTextResource.TidyUp;
begin
 if mmoMessage.Visible then
   mmoMessageExit (nil);
end;

end.
