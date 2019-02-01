{*======================================================================*
 | PropertyBaseForm                                                     |
 |                                                                      |
 | Base class for property tree/Page forms                              |
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
 | Copyright © Colin Wilson 2005  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      19/01/2005  CPWW  Original                                  |
 *======================================================================*}

unit PropertyBaseForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Contnrs, StdCtrls, Menus, VirtualTrees, PropertyPageForm,
  cmpPersistentPosition;

const
  WM_UPDATESPLITTER = WM_USER + $201;

type
  TFormPropertyBase = class;

//----------------------------------------------------------------------
// TPropertyPageDetails class maintains the tree structure for the form.
//
// Each Node of the tree contains a TPropertyPageClass containing the
// form's class, an optional TPropertyPageData class containing persistent
// data, and Child, Parent and Sibling classes to make up the tree.

  TPropertyPageDetails = class
  private
    FPropertyPageClass: TPropertyPageClass;
    FChild: TPropertyPageDetails;
    FSibling: TPropertyPageDetails;
    FParent: TPropertyPageDetails;
    FData: TPropertyPageData;
    function GetChild(idx: Integer): TPropertyPageDetails;
    function GetChildCount: Integer;
  public
    constructor Create(AOwner: TFormPropertyBase; APropertyPageClass: TPropertyPageClass; AParent: TPropertyPageDetails; const ACaption, AHelpText, AHelpKeyword: string; AParam: Integer = 0);
    destructor Destroy; override;
    property PropertyPageClass: TPropertyPageClass read FPropertyPageClass;

    property Sibling: TPropertyPageDetails read FSibling;
    property FirstChild: TPropertyPageDetails read FChild;
    property Parent: TPropertyPageDetails read FParent;
    property ChildCount: Integer read GetChildCount;
    property Child [idx: Integer]: TPropertyPageDetails read GetChild;
    property Data: TPropertyPageData read FData;
  end;

  TPropertyPageDetailsProc = procedure(Page: TPropertyPageDetails; param: pointer; var continue: Boolean) of object;

//----------------------------------------------------------------------
// TFormPropertyBase is the base class for derived property tree forms

  TFormPropertyBase = class(TForm)
    PanelOptions: TPanel;
    vstSections: TVirtualStringTree;
    Splitter: TSplitter;
    PanelButtons: TPanel;
    Bevel: TBevel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    PersistentPosition: TPersistentPosition;
    ButtonApply: TButton;
    PopupMenu: TPopupMenu;
    MenuItemExpandAll: TMenuItem;
    MenuItemCollapseAll: TMenuItem;
    ButtonHelp: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonHelpClick(Sender: TObject);
    procedure MenuItemCollapseAllClick(Sender: TObject);
    procedure MenuItemExpandAllClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vstSectionsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ButtonOKClick(Sender: TObject);
    procedure vstSectionsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstSectionsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure FormDestroy(Sender: TObject);
    procedure vstSectionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FPropertyPageDetails: TObjectList;
    FDetailsConstraints: TPoint;
    FUseConstraints: Boolean;
    FSaved: Boolean;
    FSelectedPage: TFormPropertyPage;
    FOrigHelpType: THelpType;
    FOrigHelpContext: Integer;
    FOrigHelpKeyword: string;

    procedure DoGetLargestConstraints (Details: TPropertyPageDetails; param: pointer; var continue: Boolean);
    procedure DoSavePropertyPageSettings (Page: TPropertyPageDetails; param: pointer; var continue: Boolean);
    procedure DoCancelPropertyPageSettings (Page: TPropertyPageDetails; param: pointer; var continue: Boolean);
    procedure DoCheckFormClassMatches (Details: TPropertyPageDetails; param: pointer; var continue: Boolean);
    procedure DoCheckDataMatches (Details: TPropertyPageDetails; param: pointer; var continue: Boolean);


    function GetNodePropertyPageDetails (Node: PVirtualNode): TPropertyPageDetails;
    procedure SetNodePropertyPageDetails (Node: PVirtualNode; Details: TPropertyPageDetails);
    procedure SelectPage(Details: TPropertyPageDetails);
    procedure SaveSettings;
    procedure CancelChanges;
    function FindSameData(formClass: TPropertyPageClass): TPropertyPageData;
    function FindDetailsWithData(data: TPropertyPageData): TPropertyPageDetails;
    procedure WmUpdateSplitter (var msg: TMessage); message WM_UPDATESPLITTER;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddPropertyPageDetails (APropertyPageClass: TPropertyPageClass; AParent: TPropertyPageDetails; const ACaption: string = ''; const AHelpText: string = ''; const AHelpKeyword: string = ''; AParam: Integer = 0): TPropertyPageDetails;
    function ForEachPropertyPageDetails (proc: TPropertyPageDetailsProc; param: pointer): TPropertyPageDetails;
  end;

//----------------------------------------------------------------------
// TFormPropertyPageDummy class provides an empty 'separator' property
// Page.  It's also used as a cracker class(!)
  TFormPropertyPageDummy = class (TFormPropertyPage)
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls (AData: TPropertyPageData); override;
  end;

implementation

uses
  Math, unitCredProperties;

{$R *.dfm}

type
  PObject = ^TObject;

{*----------------------------------------------------------------------*
 | procedure FixFormConstraints                                         |
 |                                                                      |
 | It seems that Delphi gets the constraints wrong for the child forms. |
 | At design time you specify the constraints - including the size of   |
 | the frame.  But at runtime there's no frame, so they are too big.    |
 | Reduce the constraints using this function after creating the child  |
 | windows                                                              |
 |                                                                      |
 | Parameters:                                                          |
 |   form: TForm                       The form to fix                 |
 *----------------------------------------------------------------------*}
procedure FixFormConstraints (form: TForm);
begin
  with form do
  begin
    Constraints.MinWidth := Constraints.MinWidth - GetSystemMetrics (SM_CXSIZEFRAME) * 2;
    Constraints.MinHeight := Constraints.MinHeight - GetSystemMetrics (SM_CYSIZE) - 2 * getSystemMetrics (SM_CYSIZEFRAME);
  end
end;

{ TFormPropertyBase }

{*----------------------------------------------------------------------*
 | function TFormPropertyBase.AddPropertyPageDetails                      |
 |                                                                      |
 | Create and add a new TPropertyPageDetails class to the tree.  The    |
 | Details class holds data for the class in a TPropetryPageData        |
 | derived instance.  It also holds the class reference of a            |
 | TPropetryPageForm, which it uses to create the form dynamically      |
 | when it's tree item is selected.                                     |
 |                                                                      |
 | Parameters:                                                          |
 |   APropertyPageClass: TPropertyPageClass                             |
 |                                      The class of the property Page  |
 |                                      form to add.                    |
 |                                                                      |
 |   AParent: TPropertyPageDetails      The parent Page Details         |
 |   const ACaption: string            Form caption.                   |
 |   const AHelpText: string           Form help text                  |
 |   AParam: Integer                   Parameter passed to Page data   |
 |                                                                      |
 | The function returns the TPropertyPageDetails class for the Page     |
 *----------------------------------------------------------------------*}
function TFormPropertyBase.AddPropertyPageDetails(
  APropertyPageClass: TPropertyPageClass; AParent: TPropertyPageDetails; const ACaption: string; const AHelpText, AHelpKeyword: string; AParam: Integer): TPropertyPageDetails;
var
  Details: TPropertyPageDetails;
  p: ^TPropertyPageDetails;
begin
  Details := TPropertyPageDetails.Create(Self, APropertyPageClass, AParent, ACaption, AHelpText, AHelpKeyword, AParam);
  if AParent = Nil then
  begin
    FPropertyPageDetails.Add(Details);
    vstSections.RootNodeCount := FPropertyPageDetails.Count
  end
  else
  begin
    p := @AParent.FirstChild;
    while p^ <> Nil do
      p := @p^.Sibling;
    p^ := Details
  end;

  Result := Details;
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.btnOKClick                                 |
 |                                                                      |
 | Onclick handler for the OK button.                                   |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.ButtonOKClick(Sender: TObject);
begin
  FSaved := True;
  SaveSettings;
end;

procedure TFormPropertyBase.CancelChanges;
begin
  ForEachPropertyPageDetails (DoCancelPropertyPageSettings, nil);
end;

{*----------------------------------------------------------------------*
 | constructor TFormPropertyBase.Create                                   |
 |                                                                      |
 | Constructor.  Create the list of PropertyPageDetails root items.     |
 *----------------------------------------------------------------------*}
constructor TFormPropertyBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPropertyPageDetails := TObjectList.Create;
  FPropertyPageDetails.OwnsObjects := True;
end;

{*----------------------------------------------------------------------*
 | destructor TFormPropertyBase.Destroy                                   |
 |                                                                      |
 | Destructor. Destroy the list of root Details items.  This will       |
 | destroy the items themselves, and destroying an item destroys its    |
 | child items, too.                                                    |
 *----------------------------------------------------------------------*}
destructor TFormPropertyBase.Destroy;
begin
  FPropertyPageDetails.Free;
  inherited;
end;

procedure TFormPropertyBase.DoCancelPropertyPageSettings(
  Page: TPropertyPageDetails; param: pointer; var continue: Boolean);
begin
  if Assigned(Page.FData) then
                // nb.  FData.Initialized won't be set(and its data won't be
                //      valid unless a Page has been selected.
    if Page.FData.Initialized then
      Page.FData.Cancel
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.DoCheckDataMatches                         |
 |                                                                      |
 | ForEach callback routine used in FindDetailsWithData.  Set           |
 | 'continue := False' when the function is passed a 'details' class    |
 | whose data matches the data given                                    |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.DoCheckDataMatches(Details: TPropertyPageDetails;
  param: pointer; var continue: Boolean);
begin
  continue := Details.FData <> param
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.DoCheckFormClassMatches                    |
 |                                                                      |
 | ForEach callback routine used in FindSameData.  Set 'continue :=     |
 | False' when the function is passed a 'Details' class whose form      |
 | class matches the given form class.                                  |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.DoCheckFormClassMatches(
  Details: TPropertyPageDetails; param: pointer; var continue: Boolean);
begin
  continue := Details.FPropertyPageClass <> param
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.DoGetLargestConstraints                    |
 |                                                                      |
 | ForEach handler used to find the largest form's constraints.         |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.DoGetLargestConstraints(Details: TPropertyPageDetails;
  param: pointer; var continue: Boolean);
var
  p: PPoint;
begin
  p := PPoint(param);

  p.X := Max(Details.FData.MinX, p.X);
  p.Y := Max(Details.FData.MinY, p.Y)
end;

{*----------------------------------------------------------------------*
 | prcoedure TFormPropertyBase.DoSavePropertyPageSettings                 |
 |                                                                      |
 | For Each handler used to apply the settings for a given form         |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.DoSavePropertyPageSettings(
  Page: TPropertyPageDetails; param: pointer; var continue: Boolean);
begin
  if Assigned(Page.FData) then
                // nb.  FData.Initialized won't be set(and its data won't be
                //      valid unless a Page has been selected.
    if Page.FData.Initialized then
      Page.FData.Apply
end;

{*----------------------------------------------------------------------*
 | function TFormPropertyBase.FindDetailsWithData                         |
 |                                                                      |
 | Find the property Page whose data matches the given data.            |
 *----------------------------------------------------------------------*}
function TFormPropertyBase.FindDetailsWithData(
  data: TPropertyPageData): TPropertyPageDetails;
begin
  Result := ForEachPropertyPageDetails(DoCheckDataMatches, data);
end;

{*----------------------------------------------------------------------*
 | function TFormPropertyBase.FindSameData                                |
 |                                                                      |
 | Find the first Page whose form class matches the given form class    |
 *----------------------------------------------------------------------*}
function TFormPropertyBase.FindSameData(
  formClass: TPropertyPageClass): TPropertyPageData;
var
  Details: TPropertyPageDetails;
begin
  Details := ForEachPropertyPageDetails (DoCheckFormClassMatches, formClass);
  if Assigned(Details) then
    Result := Details.FData
  else
    Result := nil
end;

{*----------------------------------------------------------------------*
 | function TFormPropertyBase.ForEachPropertyPageDetails                  |
 |                                                                      |
 | Iterate through the tree of Page Details, calling 'proc' for each    |
 | one.                                                                 |
 |                                                                      |
 | 'Proc' may request that the iteration stops at a particular          |
 | Page Details.  In which case, the function returns this.             |
 |                                                                      |
 | Parameters:                                                          |
 |   proc: TPropertyPageDetailsProc     Method to call for each Node    |
 |   param: pointer                    Parameter to pass to the        |
 |                                      iterator proc.                  |
 *----------------------------------------------------------------------*}
function TFormPropertyBase.ForEachPropertyPageDetails(proc: TPropertyPageDetailsProc; param: pointer): TPropertyPageDetails;
var
  i: Integer;
  cont: Boolean;
  rv: TPropertyPageDetails;

  procedure DoForEach(Details: TPropertyPageDetails);
  begin
    if Assigned(Details) then
    begin
      rv := Details;
      proc (Details, param, cont);
      if cont then
        DoForEach(Details.FirstChild);
      if cont then
        DoForEach(Details.Sibling)
    end
  end;

begin
  rv := nil;
  cont := True;
  for i := 0 to FPropertyPageDetails.Count - 1 do
  begin
    DoForEach(TPropertyPageDetails (FPropertyPageDetails[i]));
    if not cont then
      break
  end;

  // If 'cont', the iterator reached the end without being told to 'stop' on
  // a particular PropertyPageDetails.
  if cont then Result := Nil else Result := rv
end;

{*----------------------------------------------------------------------*
 | TFormPropertyBase.FormShow                                             |
 |                                                                      |
 | OnShow handler.  Initialize the form.                                |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.FormShow(Sender: TObject);
var
  n: PVirtualNode;
begin
  FDetailsConstraints := Point(0, 0);
  FOrigHelpType := HelpType;
  FOrigHelpContext := HelpContext;
  FOrigHelpKeyword := HelpKeyword;

  HelpKeyword := '';
  HelpContext := 0;
  HelpType := htContext;

  // Get the largest constraints
  ForEachPropertyPageDetails (DoGetLargestConstraints, @FDetailsConstraints);
  FUseConstraints := (FDetailsConstraints.X <> 0) and (FDetailsConstraints.Y <> 0);

  if FUseConstraints then
  begin
    PanelOptions.Constraints.MinWidth := FDetailsConstraints.X;
    PanelOptions.Constraints.MinHeight := FDetailsConstraints.Y + Bevel.Height;
  end;

  n := vstSections.GetFirst;
  if n <> Nil then
  begin
    vstSections.FocusedNode := n;
    vstSections.Selected [n] := True
  end;

  vstSections.FullExpand;

  PostMessage(Handle, WM_UPDATESPLITTER, 0, 0);
end;

{*----------------------------------------------------------------------*
 | function TFormPropertyBase.GetNodePropertyPageDetails                  |
 |                                                                      |
 | Return the TPropertyPageDetails assiciated with a tree node          |
 *----------------------------------------------------------------------*}
function TFormPropertyBase.GetNodePropertyPageDetails(
  Node: PVirtualNode): TPropertyPageDetails;
var
  Obj: PObject;
begin
  Obj := vstSections.GetNodeData(Node);

  if Assigned(Obj) then
    Result := TPropertyPageDetails (Obj^)
  else
    Result := Nil
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.SaveSettings                               |
 |                                                                      |
 | Apply the settings held in the property Details data to the actual   |
 | settings held for XanaNews.  (This is called when the 'OK' button    |
 | is clicked)                                                          |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.SaveSettings;
begin
  ForEachPropertyPageDetails (DoSavePropertyPageSettings, nil);
end;

type
  TMyPanel = class (TPanel)
  end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.SelectPage                                 |
 |                                                                      |
 | Called when a Node is selected in the tree.  Create a form of the    |
 | correct class and initialize it with data held in Details.FData      |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.SelectPage(Details: TPropertyPageDetails);
var
  Page: TFormPropertyPage;
  NewPage: Boolean;
begin
        // (try to) provent flickering!
  SendMessage(PanelOptions.Handle, WM_SETREDRAW, 0, 0);
  try
                // Free the old form (if there was one)
    if PanelOptions.ControlCount > 1 then
    begin
      Page := PanelOptions.Controls[1] as TFormPropertyPage;

      if Assigned(Details) and (Page.ClassType <> Details.FPropertyPageClass) then
        FreeAndNil (Page);
    end
    else
      Page := nil;

    if Assigned(Details) then
    begin
      if not Assigned(Page) then
      begin
                  // Create new form of the correct class.
        Page := Details.FPropertyPageClass.Create(Self);
        FixFormConstraints (Page);
        Page.Parent := PanelOptions;
        NewPage := True;
      end
      else
        NewPage := False;

                // Populate the form
      TFormPropertyPageDummy(Page).fPopulating := True;
      try
        Page.PopulateControls(Details.FData);
      finally
        TFormPropertyPageDummy(Page).fPopulating := False
      end;

      if NewPage then
      begin
        Page.Visible := True;
        Page.Align := alClient
      end
    end;

    FSelectedPage := Page;
  finally
    SendMessage(PanelOptions.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(PanelOptions.Handle,nil,0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_UPDATENOW);
  end
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.SetNodePropertyPageDetails                 |
 |                                                                      |
 | Set the given tree Node's data to point to the given details class   |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.SetNodePropertyPageDetails(Node: PVirtualNode;Details: TPropertyPageDetails);
var
  Obj: PObject;
begin
  Obj := vstSections.GetNodeData(Node);

  if Assigned(Obj) then
    Obj^ := Details
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.vstSectionsFocusChanged                    |
 |                                                                      |
 | OnFocusedChanged handler for the tree.  Create the Page for the      |
 | newly selected Node.                                                 |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.vstSectionsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  SelectPage(GetNodePropertyPageDetails(Node));
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.vstSectionsGetText                         |
 |                                                                      |
 | OnGetText handler for the tree                                       |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.vstSectionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Details: TPropertyPageDetails;
begin
  Details := GetNodePropertyPageDetails(Node);
  CellText := Details.FData.Caption
end;

{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.vstSectionsInitChildren                    |
 |                                                                      |
 | OnInitChild function for the tree                                    |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.vstSectionsInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Details: TPropertyPageDetails;
begin
  Details := GetNodePropertyPageDetails (Node);
  if Assigned(Details) then
    ChildCount := Details.ChildCount
  else
    ChildCount := 0
end;


{*----------------------------------------------------------------------*
 | procedure TFormPropertyBase.vstSectionsInitNode                        |
 |                                                                      |
 | OnInitNode handler for the tree                                      |
 *----------------------------------------------------------------------*}
procedure TFormPropertyBase.vstSectionsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Details, parentDetails: TPropertyPageDetails;
begin
  if ParentNode = Nil then
    Details := TPropertyPageDetails (FPropertyPageDetails[Node.Index])
  else
  begin
    parentDetails := GetNodePropertyPageDetails (ParentNode);
    Details := parentDetails.Child [Node.Index]
  end;
  SetNodePropertyPageDetails (Node, Details);
  if Details.ChildCount > 0 then
    InitialStates := InitialStates + [ivsHasChildren];
end;

{ TPropertyPageDetails }

{*----------------------------------------------------------------------*
 | constructor TPropertyPageDetails.Create                              |
 |                                                                      |
 | Create a TPropetryPageDetails (tree Node class) and create it's data |
 | class.  Don't initialize the data class (until its page is displayed)|
 | - but fill in its MinX, MaxX, Caption and HelpText properties        |
 *----------------------------------------------------------------------*}
constructor TPropertyPageDetails.Create(AOwner: TFormPropertyBase; APropertyPageClass: TPropertyPageClass;
  AParent: TPropertyPageDetails; const ACaption, AHelpText, AHelpKeyword: string; AParam: Integer);
var
  dataClass: TPropertyPageDataClass;
  tempPropertyPage: TFormPropertyPage;
  tempData: TPropertyPageData;
  caption: string;
  helpText: string;
  helpKeyword: string;
  minX, minY: Integer;
begin
  FPropertyPageClass := APropertyPageClass;
  FParent := AParent;
  tempPropertyPage := nil;
  try

  // In order to get the form's constraints, caption, etc. we need
  // to create a temporary instance of one.  But only the first
  // time for each form class.  If we've already created a temporary
  // instance for this class, use it's constraints & caption details.

    dataClass := FPropertyPageClass.GetDataClass;
    tempData := AOwner.FindSameData(FPropertyPageClass);

    if Assigned(tempData) then

    begin       // We've already created data for this form class
                // so use its settings

      if ACaption <> '' then
        caption := ACaption
      else
        caption := tempData.Caption;

      if AHelpText <> '' then
        helpText := AHelpText
      else
        helpText := tempData.HelpText;

      if AHelpKeyword <> '' then
        helpKeyword := AHelpKeyword
      else
        helpKeyword := tempData.HelpKeyword;

      minX := tempData.MinX;
      minY := tempData.MinY
    end
    else

    begin       // First time data is created for this form class.  So
                // create a tempoary instance of the form to get its
                // constraints and caption.

      tempPropertyPage := FPropertyPageClass.Create(nil);
      FixFormConstraints (tempPropertyPage);

      if (ACaption = '') and (tempPropertyPage.Caption <> tempPropertyPage.Name) then
        caption := tempPropertyPage.Caption
      else
        caption := ACaption;

      helpKeyword := AHelpKeyword;

      if AHelpText <> '' then
        helpText := AHelpText
      else
        helpText := tempPropertyPage.StaticTextSectionDetails.Caption;

      minX := tempPropertyPage.Constraints.MinWidth;
      minY := tempPropertyPage.Constraints.MinHeight;
    end;

    // Create the data class.
    FData := dataClass.Create(caption, helpText, helpKeyword, minX, minY, AParam);
  finally
    tempPropertyPage.Free
  end
end;

{*----------------------------------------------------------------------*
 | destructor TPropertyPageDetails.Destroy                              |
 |                                                                      |
 | Recursively destroy a property Page Details class and its siblings & |
 | chidren.                                                             ||
 *----------------------------------------------------------------------*}
destructor TPropertyPageDetails.Destroy;
begin
  FData.Free;
  FChild.Free;
  FSibling.Free;  // Obviously the root items 'Sibling's aren't populated!

  inherited;
end;

{*----------------------------------------------------------------------*
 | function TPropertyPageDetails.GetChild                               |
 |                                                                      |
 | Get the 'nth' child Node of a Details class                          |
 *----------------------------------------------------------------------*}
function TPropertyPageDetails.GetChild(idx: Integer): TPropertyPageDetails;
begin
  Result := FChild;
  while(Result <> Nil) and (idx > 0) do
  begin
    Result := Result.Sibling;
    Dec(idx)
  end
end;

{*----------------------------------------------------------------------*
 | function TPropertyPageDetails.GetChildCount                          |
 |                                                                      |
 | Count the children for a given Details Node                          |
 *----------------------------------------------------------------------*}
function TPropertyPageDetails.GetChildCount: Integer;
var
  p: TPropertyPageDetails;
begin
  Result := 0;
  p := FChild;
  while p <> Nil do
  begin
    Inc(Result);
    p := p.Sibling
  end
end;

{ TFormPropertyPageDummy }

class function TFormPropertyPageDummy.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageData
end;

procedure TFormPropertyPageDummy.PopulateControls(AData: TPropertyPageData);
begin
  StaticTextSectionDetails.Caption := AData.HelpText
end;

procedure TFormPropertyBase.FormDestroy(Sender: TObject);
begin
  PersistentPosition.SetValue('Splitter', vstSections.Width);
  if not FSaved then
    CancelChanges;
end;

procedure TFormPropertyBase.WmUpdateSplitter(var msg: TMessage);
var
  w: Integer;
begin
  w := PersistentPosition.GetValue('Splitter');

  if (w > 0) and (w < ClientWidth) then
    vstSections.Width := w;
end;

procedure TFormPropertyBase.ButtonApplyClick(Sender: TObject);
begin
  SaveSettings;
  PostMessage(Application.MainForm.Handle,  WM_PROPERTIES_CHANGED, 0, 0);
end;

procedure TFormPropertyBase.MenuItemExpandAllClick(Sender: TObject);
begin
  vstSections.FullExpand;
end;

procedure TFormPropertyBase.MenuItemCollapseAllClick(Sender: TObject);
begin
  vstSections.FullCollapse;
end;

procedure TFormPropertyBase.ButtonHelpClick(Sender: TObject);
var
  kw: string;
  co: Integer;
begin
  kw := '';
  co := 0;

  if Assigned(FSelectedPage) then
    if (FSelectedPage.HelpType = htKeyword) and (FSelectedPage.HelpKeyword <> '') then
    begin
      kw := FSelectedPage.HelpKeyword;
      if kw = '' then
        kw := FSelectedPage.AltKeyword
    end
    else
      if (FSelectedPage.HelpType = htContext ) then
        co := FSelectedPage.HelpContext;

  if (kw = '') and (co = 0) then
    if FOrigHelpType = htContext then
      co := FOrigHelpContext
    else
      kw := FOrigHelpKeyword;

  if kw <> '' then
    Application.HelpKeyword(kw)
  else
    if co <> 0 then
      Application.HelpContext(co)
end;

procedure TFormPropertyBase.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_f1 then
  begin
    ButtonHelpClick(nil);
    Key := 0;
  end
end;

end.
