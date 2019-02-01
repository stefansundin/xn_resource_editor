(*======================================================================*
 | MainForm                                                             |
 |                                                                      |
 | Main Form for PE Resource Explorer /Editor                           |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      05/01/2001  CPWW  Original                                  |
 *======================================================================*)

unit MainForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

{$region 'Interface Uses Section'}
//------------------------------------------------------
uses
  Windows, Messages, SysUtils, SysConst, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, ToolWin, ExtCtrls, ImgList, StdActns, ActnList,
  XPMan, Actions, ImageList, StdCtrls, ShellApi, AppEvnts, ActnMan, ActnCtrls,
  ActnMenus, XPStyleActnCtrls, ExtDlgs, Jpeg, PngImage, unitResourceDetails,
  ComponentStandardSystemMenu, ComponentPersistentPosition, ComponentMRUList,
  ComponentNTAboutBox, ResourceForm, GifImage, unitCREdProperties,
  unitHTMLHelpViewer, unitResourceExaminer, VirtualTrees, ExVirtualStringTree;
{$endregion}

{$region 'Constant Definitions'}
//------------------------------------------------------
const
  WM_INITIALIZE = WM_USER + $200;
  WM_STATUSBAR = WM_USER + $203;
  WM_ADDIMAGERESOURCE = WM_USER + $204;
{$endregion}

{$region 'Type Definitions'}
//------------------------------------------------------
type

//======================================================================
// TFormMain
  TFormMain = class(TForm)
    MainMenuMain: TMainMenu;
    ImageListMain: TImageList;
    ActionListMain: TActionList;
    ActionHelpContents: THelpContents;
    ActionHelpHelpOnHelp: THelpOnHelp;
    ActionHelpTopicSearch: THelpTopicSearch;
    MenuItemEdit: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    N1: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemHelpContents: TMenuItem;
    MenuItemHelpHelpOnHelp: TMenuItem;
    MenuItemHelpTopicSearch: TMenuItem;
    ActionFileNew: TAction;
    ActionFileOpenFile: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    ActionFilePrint: TAction;
    MenuItemFile: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    N2: TMenuItem;
    MenuItemFilePrint: TMenuItem;
    N3: TMenuItem;
    ActionFileExit: TAction;
    MenuItemFileExit: TMenuItem;
    StatusBarMain: TStatusBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    Splitter: TSplitter;
    MRUList: TMRUList;
    PersistentPosition: TPersistentPosition;
    StandardSystemMenu: TStandardSystemMenu;
    PopupMenuMRU: TPopupMenu;
    PanelResource: TPanel;
    MenuItemResource: TMenuItem;
    ActionResourceAddResource: TAction;
    ActionResourceDeleteResource: TAction;
    MenuItemResourceAddResource: TMenuItem;
    MenuItemResourceDeleteResource: TMenuItem;
    N4: TMenuItem;
    ActionViewToolbar: TAction;
    ActionViewStatusbar: TAction;
    ActionViewProperties: TAction;
    MenuItemView: TMenuItem;
    MenuItemViewToolbar: TMenuItem;
    MenuItemViewStatusbar: TMenuItem;
    N5: TMenuItem;
    MenuItemViewProperties: TMenuItem;
    ActionEditCopy: TAction;
    ActionEditPaste: TAction;
    ActionEditCut: TAction;
    ActionEditSelectAll: TAction;
    ActionEditUndo: TAction;
    ActionEditRedo: TAction;
    MenuItemEditRedo: TMenuItem;
    MenuItemOpen: TMenuItem;
    ActionHelpAbout: TAction;
    N6: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    NTAboutBox: TNTAboutBox;
    ActionResourceExportResource: TAction;
    MenuItemExportResource1: TMenuItem;
    ImageListResources: TImageList;
    ActionResourceImportResource: TAction;
    MenuItemImportImageResource: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    ActionResourceProperties: TAction;
    N8: TMenuItem;
    MenuItemResourceProperties: TMenuItem;
    PopupMenuResources: TPopupMenu;
    MenuItemAddResource: TMenuItem;
    MenuItemDeleteResource: TMenuItem;
    N9: TMenuItem;
    MenuItemImportResource: TMenuItem;
    MenuItemExportResource: TMenuItem;
    N10: TMenuItem;
    MenuItemProperties: TMenuItem;
    ActionEditDelete: TAction;
    MenuItemEditDelete: TMenuItem;
    ActionResourceGrab: TAction;
    MenuItemGrab: TMenuItem;
    MenuItemGrabBitmap: TMenuItem;
    SavePictureDialog: TSavePictureDialog;
    ApplicationEvents: TApplicationEvents;
    ActionResourceImportRCDataResource: TAction;
    MenuItemImportRCDataResource2: TMenuItem;
    MenuItemImportRCDataResource: TMenuItem;
    OpenDialogAnyFile: TOpenDialog;
    ActionResourceImportOtherResource: TAction;
    MenuItemImportUserResource2: TMenuItem;
    N11: TMenuItem;
    MenuItemImportUserResource: TMenuItem;
    N12: TMenuItem;
    ActionResourceClone: TAction;
    MenuItemCloneResource: TMenuItem;
    vstResources: TExVirtualStringTree;
    XPManifest: TXPManifest;
    ToolbarMenu: TToolBar;
    ToolButtonFile: TToolButton;
    ToolButtonEdit: TToolButton;
    ToolButtonView: TToolButton;
    ToolButtonResource: TToolButton;
    ToolButtonResourceObject: TToolButton;
    ToolButtonHelp: TToolButton;
    ToolbarMain: TToolBar;
    ToolButtonFileNew: TToolButton;
    ToolButtonFileOpen: TToolButton;
    ToolButtonFileSave: TToolButton;
    ToolButtonSplitter: TToolButton;
    ToolButtonAddResource: TToolButton;
    ToolButtonDeleteResource: TToolButton;
    procedure vstResourcesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure vstResourcesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstResourcesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure vstResourcesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vstResourcesInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vstResourcesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstResourcesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ActionResourceGrabExecute(Sender: TObject);
    procedure ActionResourceCloneExecute(Sender: TObject);
    procedure ActionFileOpenFileExecute(Sender: TObject);
    procedure ActionFileSaveAsExecute(Sender: TObject);
    procedure ActionFilePrintExecute(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MRUListPopupMenuClick(Sender: TObject);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionViewToolbarExecute(Sender: TObject);
    procedure ActionViewStatusbarExecute(Sender: TObject);
    procedure ActionViewPropertiesExecute(Sender: TObject);
    procedure ActionEditUndoExecute(Sender: TObject);
    procedure ActionEditRedoExecute(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionResourceExportResourceExecute(Sender: TObject);
    procedure ActionResourceAddResourceExecute(Sender: TObject);
    procedure ActionResourceDeleteResourceExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionResourceImportResourceExecute(Sender: TObject);
    procedure ActionResourcePropertiesExecute(Sender: TObject);
    procedure ActionEditSelectAllExecute(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ActionResourceImportRCDataResourceExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionResourceImportOtherResourceExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG;
      var Handled: Boolean);
    function ApplicationEventsHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    FResourceModule: TResourceModule;
    FUndo, FRedo: string;
    FFileName: string;
    FIgnoreChar: Boolean;
    FWasDirty: Boolean;
    FExaminer: TResourceExaminer;

    procedure SetCaption;
    procedure UpdateDisplay(selectDetails: TResourceDetails);
    procedure OpenFile(const FileName: string);
    procedure SwitchView (ResourceDetails: TObject);
    procedure WmInitialize(var msg: TMessage); message WM_INITIALIZE;
    procedure WmStatusBar (var Msg: TMessage); message WM_STATUSBAR;
    procedure WmPropertiesChanged (var msg: TMessage); message WM_PROPERTIES_CHANGED;
    procedure WmAddImageResource(var msg: TMessage); message WM_ADDIMAGERESOURCE;
    procedure SetFormMenuButton (Item: TMenuItem);
    procedure SetFileName(const Value: string);

    function SaveFile: Boolean;
    function SaveFileAs: Boolean;
    function SaveChanges: Boolean;
    function IsDirty: Boolean;

    function GetNodeElement(node: PVirtualNode; var Element: TResExamElement): Boolean;
    function GetNodeResourceDetails (node: PVirtualNode): TResourceDetails;
    function GetResourceDetailsNode(ResourceDetails: TResourceDetails): PVirtualNode;
    function SelectedResourceDetails: TResourceDetails;
    function ResourceForm: TFormResource;
    procedure CheckDetails (p: PVirtualNode; Param: Integer; var Continue: Boolean);

    property FileName: string read FFileName write SetFileName;
    function GetPersistDirectoryName(section: string): string;
    procedure SetPersistDirectoryName(section: string;
      const Value: string);
    function CloneResource(ResourceDetails: TResourceDetails; NewName: string; NewLanguage: Integer): TResourceDetails;
    function EditAllowed (node: PVirtualNode): Boolean;
    procedure UpdateResourceNodes (ResourceDetails: TResourceDetails; node: PVirtualNode);
  protected
    procedure Loaded; override;
    procedure UpdateActions; override;
  public
    property PersistDirectoryName [section: string]: string read GetPersistDirectoryName write SetPersistDirectoryName;
  end;
{$endregion}

//----------------------------------------------------------------------
var
  FormMain: TFormMain;

//----------------------------------------------------------------------
procedure AttachMenu(const ButtonCaption: string; Menu: TMenuItem);
procedure DetachMenu(const ButtonCaption: string);
function GetTypeImage(const tp: string): Integer;

implementation

{$R *.DFM}

{$region 'Implementation Uses Section'}
uses
  Registry, RawResourceForm,
  unitPEFile,                        // Accept resources from PE files
  unitNTModule,                      // Use this instead if NT
  unitResFile,                       // Accept resources from .ResourceDetails files
  unitRCFile,                        // Accept resources from .RC files

  unitResourceGraphics,              // Decoder unit for Icons, Bitmaps, Cursors
  unitResourceMessages,              //    "     "    "  String and Message tables
  unitResourceVersionInfo,           //    "     "    "  Version Info
  unitResourceMenus,                 //    "     "    "  Menus
  unitResourceDialogs,               //    "     "    "  Dialogs
  unitResourceRCData,                //    "     "    "  RCData
  unitResourceJPEG,                  //    "     "    "  JPEG Images
  unitResourceGIF,                   //    "     "    "  GIF Images
  unitResourceXPManifests,           //    "     "    "  XP Manifests
  unitResourceAccelerator,           //    "     "    "  Accelerator tables
  unitResourceToolbar,

  GroupResourceForm,                 // Display Form for Icons & cursor groups
  IconGraphicsResourceForm,          // Editor   "   "  Icons
  CursorGraphicsResourceForm,        // Editor   "   "  Cursors
  GraphicsResourceForm,              // Editor   "   "  Other graphics - bitmaps etc.
  TextResourceForm,                  //   "      "   "   String and MEssage tables
  VersionResourceForm,               //   "      "   "   Version Info
  MenuResourceForm,                  //   "      "   "   Menus
  DialogResourceForm,                //   "      "   "   Dialogs
  DescriptionRCDataResourceForm,     //   "      "   "   RC Data Description
  PackagesResourceForm,              //   "      "   "   Borland 'package' RC data
  FormResourceForm,                  //   "      "   "   Borland TForm data
  XPManifestResourceForm,            //   "      "   "   XML XP Manifest
  AcceleratorResourceForm,           //   "      "   "   Accelerators


  PropertiesForm,                    // Program properties dialog
  AddResourceDialog,                 // Add Resource dialog
  ResourcePropertiesDialog,
  unitExIcon,
  HelpContext, ResourceObjectForm, unitSearchString, CloneResourceDialog;
{$endregion}

{$region 'Resource String Definitions' }
//----------------------------------------------------------------------
resourcestring
  rstColors = '%d colours';
  rstHighColor = 'High Colour';
  rstTrueColor = 'True Colour';
  rstLanguageNeutral = 'Language Neutral';
  rstDuplicateResourceName = 'Duplicate Resource Name';
  rstAnyFileFilter = 'Any File(*.*)|*.*';

  rstBitmap       = 'Bitmap';
  rstIcon         = 'Icon';
  rstCursor       = 'Cursor';
  rstMenu         = 'Menu';
  rstToolbar      = 'Toolbar';
  rstDialog       = 'Dialog';
  rstAccelerator  = 'Accelerator Table';
  rstString       = 'String Table';
  rstRCData       = 'RC Data';
  rstMessageTable = 'MessageTable';
  rstVersion      = 'Version';
  rstGroupCursor  = 'Cursor Group';
  rstGroupIcon    = 'Icon Group';
  rstXPManifest   = 'XP Theme Manifest';

  rstUndo         = '&Undo %s';
  rstRedo         = '&Redo %s';
  rstUntitled     = '<Untitled>';
  rstResFilter    = 'Resource Files (*.res;*.dcr)|*.RES;*.DCR';
  rstModuleFilter = 'Module Files (*.exe;*.dll;*.bpl;*.scr;*.cpl;*.ocx)|*.EXE;*.DLL;*.OCX;*.BPL;*.SCR;*.OCX';
  rstChanges      = 'Save changes to %s';

  rstChangeResourceProperties = 'change resource properties';

const
  c = RT_TOOLBAR;
{$endregion}

{$region 'Local Definitions'}
const
  imgClosedFolder = 10;
  imgOpenFolder = 11;
{$endregion}

{$region 'Helper Functions'}

(*----------------------------------------------------------------------*
 | procedure AttachMenu                                                 |
 |                                                                      |
 | Global procedure - attaches a menu to a button (the ...Object one)   |
 *----------------------------------------------------------------------*)
procedure AttachMenu(const ButtonCaption: string; Menu: TMenuItem);
var
  i: Integer;
  btn: TToolButton;
begin
  Exit;
  if csDestroying in FormMain.ComponentState then
    Exit;

  btn := nil;
  for i := 0 to FormMain.ToolbarMenu.ButtonCount - 1 do
    if FormMain.ToolbarMenu.Buttons[i].Caption = ButtonCaption then
    begin
      btn := FormMain.ToolbarMenu.Buttons[i];
      break
    end;

  if Assigned(btn) then
  begin
    btn.MenuItem := Menu;
    btn.Visible := True
  end
end;

(*----------------------------------------------------------------------*
 | procedure DetachMenu                                                 |
 |                                                                      |
 | Global procedure - detaches a menu to a button (the ...Object one)   |
 *----------------------------------------------------------------------*)
procedure DetachMenu(const ButtonCaption: string);
var
  i: Integer;
  btn: TToolButton;
begin
  if csDestroying in FormMain.ComponentState then
    Exit;

  btn := nil;
  for i := 0 to FormMain.ToolBarMenu.ButtonCount - 1 do
    if FormMain.ToolBarMenu.Buttons[i].Caption = ButtonCaption then
    begin
      btn := FormMain.ToolBarMenu.Buttons[i];
      break
    end;

  if Assigned(btn) then
  begin
    btn.Visible := False;
    btn.MenuItem := nil
  end
end;

(*----------------------------------------------------------------------*
 | function GetTypeImage                                                |
 |                                                                      |
 | Get the image index for a resource type.  The 'magic number'         |
 | returned is an index in the ilResources image list.                  |
 *----------------------------------------------------------------------*)
function GetTypeImage(const tp: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := ResourceNametoInt(tp);

  case i of
    Integer (RT_VERSION)     : i := 8;
    Integer (RT_MESSAGETABLE): i := Integer (RT_STRING)
  end;

  if (i >= 0) and (i < 10) then
    Result := i
  else
    if tp = 'MIDI' then
      Result := 12
    else
      if tp = 'WAVE' then
       Result := 13
end;

(*----------------------------------------------------------------------*
 | function GetGraphicsClass: TGraphicClass                            |
 |                                                                      |
 | Get a graphic class associated with an extension                     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   Ext: string                 The extension to find.                |
 |                                                                      |
 | The function returns the graphics class                              |
 *----------------------------------------------------------------------*)
function GetGraphicsClass(Ext: string): TGraphicClass;
begin
  Ext := UpperCase(Ext);
  if (Ext <> '') and (Ext [1] = '.') then
    Delete(Ext, 1, 1);

  if Ext = 'BMP' then
    Result := TBitmap
  else
    if (Ext = 'WMF') or (Ext = 'EMF') then
      Result := TMetafile
    else
      if Ext = 'ICO' then
        Result := TExIcon
      else
        if Ext = 'CUR' then
          Result := TExCursor
        else
          if (Ext = 'JPG') or (Ext = 'JPEG') then
            Result := TJpegImage
          else
            if Ext = 'GIF' then
              Result := TGIFImage
            else
              if Ext = 'PNG' then
                Result := TPngImage
              else
                Result := nil
end;

{*----------------------------------------------------------------------*
 | function CreateCompatibleGraphic                                     |
 |                                                                      |
 | Create a graphic of a specified type, using the image and dimensions |
 | of another graphic.                                                  |
 |                                                                      |
 | Parameters:                                                          |
 |   graphic: TGraphic;                The original graphic            |
 |   newCls: TGraphicClass             The new graphic class           |
 *----------------------------------------------------------------------*}
function CreateCompatibleGraphic(graphic: TGraphic; newCls: TGraphicClass): TGraphic;
var
  viaBitmap: Boolean;
  bmp: TBitmap;
  gif: TGifImage;
begin
  viaBitmap := (newCls = TPNGImage);

  Result := newCls.Create;
  try
    if viaBitmap then           // Some conversions work better if you convert
    begin                       // the original graphic to a bitmap, then convert
      bmp := TBitmap.Create;    // that to the new format.
      try
        bmp.Assign (graphic);
        Result.Assign (bmp)
      finally
        bmp.Free
      end
    end
    else
      if newCls = TGifImage then        // GIF images can do cool things with
      begin                             // dithering and palettes.  Treat as a
        gif := TGifImage(Result);      // a special case, so that these get used
        gif.DitherMode := dmFloydSteinberg;
        gif.ColorReduction := rmQuantizeWindows;
        Result.Assign(graphic);
      end
      else
        Result.Assign(graphic);
  except
    FreeAndNil(Result);
  end
end;
{$endregion}

{ TFormMain }

(*----------------------------------------------------------------------*
 | TFormMain.actEditCopyExecute                                           |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditCopyExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Copy;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actEditCutExecute                                            |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditCutExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Cut;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actEditDeleteExecute                               |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditDeleteExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.EditDelete;
end;


(*----------------------------------------------------------------------*
 | TFormMain.actEditPasteExecute                                          |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditPasteExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Paste;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actEditRedoExecute                                           |
 |                                                                      |
 | Handler for Edit/Redo.  Tell the Form to redo changes.               |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditRedoExecute(Sender: TObject);
var
  ResForm: TFormResource;
  Element: TResExamElement;
begin
  ResForm := ResourceForm;
  if Assigned(ResForm) then
  begin
    ResForm.Redo;
    fmResourceObject.Obj := ResForm.ResourceDetails;
    UpdateResourceNodes (SelectedResourceDetails, vstResources.FocusedNode);
  end
  else
  begin
    GetNodeElement(vstResources.FocusedNode, Element);
    if Element is TResExamName then
    begin
      TResExamName(Element).Redo;
      vstResources.InvalidateNode(vstResources.FocusedNode)
    end
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.ActionEditSelectAll                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditSelectAllExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.SelectAll
end;

(*----------------------------------------------------------------------*
 | TFormMain.actEditUndoExecute                                           |
 |                                                                      |
 | Handler for Edit/Undo.  Tell the Form to undo changes.               |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditUndoExecute(Sender: TObject);
var
  ResForm: TFormResource;
  Element: TResExamElement;
begin
  ResForm := ResourceForm;
  if Assigned(ResForm) then
  begin

    ResForm.Undo;   // Perform the undo
    fmResourceObject.Obj := ResForm.ResourceDetails;

                    // Update the resource tree to reflect changes
                    // in the resource Name/language
    UpdateResourceNodes (SelectedResourceDetails, vstResources.FocusedNode);
  end
  else
  begin
    GetNodeElement(vstResources.FocusedNode, Element);
    if Element is TResExamName then
    begin
      TResExamName(Element).Undo;
      vstResources.InvalidateNode(vstResources.FocusedNode)
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileExitExecute                                 |
 |                                                                      |
 | FileExit action handler                                              |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileExitExecute(Sender: TObject);
begin
  Close
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileNewExecute                                  |
 |                                                                      |
 | FileNew action handler                                               |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileNewExecute(Sender: TObject);
begin
  if SaveChanges then
  begin
    FreeAndNil(FResourceModule);
    ClearUndoDetails;
    FResourceModule := TResModule.Create; // Create an empty .ResourceDetails module
    FileName := '';
    UpdateDisplay(nil)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileOpenExecute                                 |
 |                                                                      |
 | FileOpen action handler.                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileOpenFileExecute(Sender: TObject);
begin
  Application.ProcessMessages;  // Ensures that toolbar button doesn't temporarily disappear

  OpenDialog.InitialDir := MRUList.MRUDirectory;
  if OpenDialog.Execute then
    OpenFile(OpenDialog.FileName);
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFilePrintExecute                                |
 |                                                                      |
 | FilePrint action handler                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFilePrintExecute(Sender: TObject);
begin
  if PrintDialog.Execute then
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileSaveAsExecute                               |
 |                                                                      |
 | FileSaveAs action handler                                            |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileSaveAsExecute(Sender: TObject);
begin
  SaveFileAs;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileSaveExecute                                 |
 |                                                                      |
 | File/Save handler.  If we know the file Name, overwrite it.          |
 | Otherwise do File/Save As instead.                                   |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileSaveExecute(Sender: TObject);
begin
  SaveFile;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actHelpAboutExecute                                          |
 |                                                                      |
 | Display/execute the about box.                                       |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  NTAboutBox.Execute;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceAddResourceExecute                                |
 |                                                                      |
 | Display/execute the Add Resource dialog.                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceAddResourceExecute(Sender: TObject);
var
  Dialog: TDialogAddResource;
  ResourceDetails: TResourceDetails;
begin
  Dialog := TDialogAddResource.Create(nil);
  try
    if Dialog.ShowModal = mrOk then
    begin
      ResourceDetails := Dialog.ResourceDetailsClass.CreateNew (FResourceModule, 0, FResourceModule.GetUniqueResourceName(Dialog.ResourceDetailsClass.GetBaseType));
      ResourceDetails.Dirty := True;
      FResourceModule.SortResources;
      UpdateDisplay(ResourceDetails);
    end
  finally
    Dialog.Free
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceDeleteResourceExecute                             |
 |                                                                      |
 | Delete the selected resource, and delete the node(s) that refer to   |
 | it.                                                                  |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceDeleteResourceExecute(Sender: TObject);
var
  p: PVirtualNode;
  ResourceDetails: TResourceDetails;
  Index: Integer;
  NextAction: Integer;
begin
  p := vstResources.FocusedNode;
  ResourceDetails := GetNodeResourceDetails (p);
  if not Assigned(ResourceDetails) then Exit;

  if vstResources.GetNextSibling(p) <> nil then
    NextAction := 1
  else
    if p.Index > 0 then
      NextAction := 2
    else
      NextAction := 0;


  Index := FResourceModule.IndexOfResource(ResourceDetails);
  FResourceModule.DeleteResource(Index);

  if NextAction = 1 then
    ResourceDetails := FResourceModule.ResourceDetails[Index]
  else
    if NextAction = 2 then
      ResourceDetails := FResourceModule.ResourceDetails[Index - 1]
    else
      ResourceDetails := nil;
  UpdateDisplay(ResourceDetails);
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceExportResourceExecute                             |
 |                                                                      |
 | Display/execute the 'export resource' dialog.  This is either a      |
 | 'save' dialog, or a 'save picture' dialog, depending on the          |
 | resource to be exported.                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceExportResourceExecute(Sender: TObject);
var
  Pict: TPicture;
  cls, cls1: TGraphicClass;
  Ext: string;
  newGraphic: TGraphic;
const
  pfNames: array [TPixelFormat] of string = ('Same as monitor', '2 Colour', '16 Colour', '256 Colour', '', '65536 Colour', 'High Colour', 'True Colour', '');
begin
  Pict := nil;

  if fmResourceObject.Obj is TIconCursorGroupResourceDetails then
  begin
    Pict := TPicture.Create;
    TIconCursorGroupResourceDetails (fmResourceObject.Obj).GetImage(Pict)
  end
  else
    if fmResourceObject.Obj is TGraphicsResourceDetails then
    begin
      Pict := TPicture.Create;
      TGraphicsResourceDetails (fmResourceObject.Obj).GetImage(Pict)
    end;

  if Assigned(Pict) then
  try
    if Assigned(Pict.Graphic) then
    begin
      SavePictureDialog.InitialDir := PersistDirectoryName ['Export'];

      cls := TGraphicClass (Pict.Graphic.ClassType);
      SavePictureDialog.DefaultExt := GraphicExtension (cls);

      if SavePictureDialog.Execute then
      begin
        Ext := ExtractFileExt(SavePictureDialog.FileName);
        cls1 := GetGraphicsClass (Ext);

        if cls <> cls1 then
        begin
          newGraphic := CreateCompatibleGraphic (Pict.Graphic, cls1);
          try
            newGraphic.SaveToFile(SavePictureDialog.FileName);
          finally
            newGraphic.Free
          end
        end
        else
          Pict.SaveToFile(SavePictureDialog.FileName);

        PersistDirectoryName ['Export'] := ExtractFilePath(SavePictureDialog.FileName)
      end
    end
  finally
    Pict.Free
  end
  else if fmResourceObject.Obj is TResourceDetails then
  begin
    SaveDialog.Filter := rstAnyFileFilter;
    SaveDialog.InitialDir := ExtractFilePath(SaveDialog.FileName);
    SaveDialog.FileName := TResourceDetails (fmResourceObject.Obj).ResourceName;
    if SaveDialog.Execute then
      TResourceDetails (fmResourceObject.Obj).Data.SaveToFile(SaveDialog.FileName)
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceImportOtherResourceExecute                        |
 |                                                                      |
 | Import a 'User' resource                                             |
 |                                                                      |
 | Get the resource Name and type from the file Name and extension      |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceImportOtherResourceExecute(Sender: TObject);
var
  ResourceDetails: TResourceDetails;
  MemoryStream: TMemoryStream;
  ResType, ResName: string;
begin
  OpenDialogAnyFile.InitialDir := PersistDirectoryName ['Import'];

  if OpenDialogAnyFile.Execute then
  begin
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.LoadFromFile(OpenDialogAnyFile.FileName);

      ResType := UpperCase(ExtractFileName(OpenDialogAnyFile.FileName));
      ResName := SplitString ('.', ResType);

      if ResType = '' then
      begin
        ResType := ResName;
        ResName := FResourceModule.GetUniqueResourceName(ResType)
      end
      else
        if FResourceModule.FindResource(ResType, ResName, 0) <> nil then
          ResName := FResourceModule.GetUniqueResourceName(ResType);

      ResourceDetails := TResourceDetails.CreateResourceDetails (
        FResourceModule, 0,
        ResName,
        ResType,
        MemoryStream.Size, MemoryStream.Memory);

      FResourceModule.AddResource(ResourceDetails);

      ResourceDetails.Dirty := True;

      PersistDirectoryName ['Import'] := ExtractFilePath(OpenDialogAnyFile.FileName);

      FResourceModule.SortResources;
      UpdateDisplay(ResourceDetails);
    finally
      MemoryStream.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceImportRCDataResourceExecute                       |
 |                                                                      |
 | Import an RC Data resource                                           |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceImportRCDataResourceExecute(Sender: TObject);
var
  ResourceDetails: TResourceDetails;
  MemoryStream: TMemoryStream;
begin
  OpenDialogAnyFile.InitialDir := PersistDirectoryName ['Import'];
  if OpenDialogAnyFile.Execute then
  begin
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.LoadFromFile(OpenDialogAnyFile.FileName);

      ResourceDetails := TResourceDetails.CreateResourceDetails (
        FResourceModule, 0,
        FResourceModule.GetUniqueResourceName(IntToStr (Integer (RT_RCDATA))),
        IntToStr (Integer (RT_RCDATA)),
        MemoryStream.Size, MemoryStream.Memory);

      FResourceModule.AddResource(ResourceDetails);

      ResourceDetails.Dirty := True;

      PersistDirectoryName ['Import'] := ExtractFilePath(OpenDialogAnyFile.FileName);

      FResourceModule.SortResources;
      UpdateDisplay(ResourceDetails)
    finally
      MemoryStream.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceImportResourceExecute                             |
 |                                                                      |
 | Import a graphic resource                                            |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceImportResourceExecute(Sender: TObject);
var
  ResourceDetailsClass: TResourcedetailsClass;
  Ext: string;
  ResourceDetails: TResourceDetails;
  Image: TPicture;
begin
  OpenPictureDialog.InitialDir := PersistDirectoryName ['Import'];

  if OpenPictureDialog.Execute then
  begin
    Ext := UpperCase(ExtractFileExt(OpenPictureDialog.FileName));

    if Ext = '.ICO' then
      ResourceDetailsClass := TIconGroupResourceDetails
    else
      if (Ext = '.CUR') or (Ext = '.ANI') then
        ResourceDetailsClass := TCursorGroupResourceDetails
      else
        if (Ext = '.GIF') then
          ResourceDetailsClass := TGIFResourceDetails
        else
          if (Ext = '.JPG') or (Ext = '.JPEG') then
            ResourceDetailsClass := TJPEGResourceDetails
          else
            ResourceDetailsClass := TBitmapResourceDetails;

    ResourceDetails := ResourceDetailsClass.CreateNew (FResourceModule, 0, FResourceModule.GetUniqueResourceName(ResourceDetailsClass.GetBaseType));
    ResourceDetails.Dirty := True;

    if ResourceDetails is TIconCursorGroupResourceDetails then
      TIconCursorGroupResourceDetails(ResourceDetails).LoadImage(OpenPictureDialog.FileName)
    else
    begin
      Image := TPicture.Create;
      try
        Image.LoadFromFile(OpenPictureDialog.FileName);
        TGraphicsResourceDetails(ResourceDetails).SetImage(Image);
      finally
        Image.Free
      end
    end;

    PersistDirectoryName ['Import'] := ExtractFilePath(OpenPictureDialog.FileName);

    FResourceModule.SortResources;
    UpdateDisplay(ResourceDetails);
  end

end;

procedure TFormMain.ActionResourcePropertiesExecute(Sender: TObject);
var
  Dialog: TdlgResourceProperties;
  fm: TFormResource;
  NewLanguage: LCID;
  NewName: WideString;
  ResourceDetails, r: TResourceDetails;
begin
  fm := ResourceForm;
  ResourceDetails := SelectedResourceDetails;

  if Assigned(ResourceDetails) and Assigned(fm) and not (ResourceDetails is TIconCursorResourceDetails) then
  begin
    Dialog := TdlgResourceProperties.Create(nil);
    try
      Dialog.ResourceDetails := ResourceDetails;

      if Dialog.ShowModal = mrOK then
      begin
        NewLanguage := Dialog.Language;
        NewName := Dialog.EditName.Text;

        if ResourceDetails is TStringResourceDetails then
          NewName := StringsIDToResID(NewName);

        if (NewLanguage <> ResourceDetails.ResourceLanguage) or (NewName <> ResourceDetails.ResourceName) then
        begin
          r := FResourceModule.FindResource(ResourceDetails.ResourceType, NewName, NewLanguage);
          if Assigned(r) and (r <> ResourceDetails) and (r.ResourceLanguage = NewLanguage) then
            raise Exception.Create(rstDuplicateResourceName);

          fm.AddUndoEntry(rstChangeResourceProperties);


          if NewLanguage <> ResourceDetails.ResourceLanguage then
            ResourceDetails.ResourceLanguage := NewLanguage;

          if NewName <> ResourceDetails.ResourceName then
          begin
            ResourceDetails.ResourceName := NewName;
            if ResourceDetails is TStringResourceDetails then
              NewName := ResIdToStringsId (NewName);

            UpdateResourceNodes (ResourceDetails, vstResources.FocusedNode);
            if ResourceDetails is TStringResourceDetails then
              SwitchView (ResourceDetails);
          end
          else
            vstResources.Update
        end
      end
    finally
      Dialog.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actViewPropertiesExecute                                     |
 |                                                                      |
 | Display/execute the 'Properties' dialog                              |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionViewPropertiesExecute(Sender: TObject);
var
  Dialog: TFormProperties;
begin
  Dialog := TFormProperties.Create(nil);
  try
    Dialog.ShowModal;
  finally
    Dialog.Free
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actViewStatusbarExecute                                      |
 |                                                                      |
 | Turn the status bar on/off                                           |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionViewStatusbarExecute(Sender: TObject);
begin
  gProperties.ShowStatusBar := not gProperties.ShowStatusBar
end;

(*----------------------------------------------------------------------*
 | TFormMain.actViewToolbarExecute                                        |
 |                                                                      |
 | Turn the toolbar on/off.                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionViewToolbarExecute(Sender: TObject);
begin
  gProperties.ShowToolbar := not gProperties.ShowToolbar;
end;

function TFormMain.ApplicationEventsHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := not (command = HELP_FINDER);
  if not CallHelp then
    PostMessage(Application.Handle, CM_INVOKEHELP, HELP_CONTENTS, 0);
  Result := False
end;

procedure TFormMain.ApplicationEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  Handled := not HHPreTranslateMessage(Msg);
end;

procedure TFormMain.CheckDetails(p: PVirtualNode; Param: Integer;
  var Continue: Boolean);
begin
  Continue := Integer (GetNodeResourceDetails (p)) <> Param
end;

function TFormMain.CloneResource(ResourceDetails: TResourceDetails; NewName: string; NewLanguage: Integer): TResourceDetails;
var
  i: Integer;
  icg, clicg: TIconCursorGroupResourceDetails;
  Data: Pointer;
begin
  if ResourceDetails is TIconCursorGroupResourceDetails then
    Data := nil
  else
    Data := ResourceDetails.Data.Memory;

  if NewLanguage <> -1 then
    Result := TResourceDetails.CreateResourceDetails (
                                ResourceDetails.Parent,
                                NewLanguage,
                                ResourceDetails.ResourceName,
                                ResourceDetails.GetBaseType, ResourceDetails.Data.Size, Data)
  else
  begin
    if NewName = '' then
      NewName := FResourceModule.GetUniqueResourceName(ResourceDetails.GetBaseType);

    Result := TResourceDetails.CreateResourceDetails (
                                ResourceDetails.Parent,
                                ResourceDetails.ResourceLanguage,
                                NewName,
                                ResourceDetails.GetBaseType, ResourceDetails.Data.Size, Data)
  end;

  if ResourceDetails is TIconCursorGroupResourceDetails then
  begin
    icg := TIconCursorGroupResourceDetails (ResourceDetails);
    clicg := Result as TIconCursorGroupResourceDetails;
    for i := 0 to icg.ResourceCount - 1 do
      clicg.AddToGroup(TIconCursorResourceDetails (CloneResource(icg.ResourceDetails[i], '', NewLanguage)));

      // Remove first empty image that was created along with
      // the group resource.  Note that you have to do this
      // *after* adding the other images - otherwise the newly
      // created group resource will be zapped!

    clicg.RemoveFromGroup(clicg.ResourceDetails[0]);
  end;
  Result.Dirty := True;
  FResourceModule.AddResource(Result);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SaveChanges
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.FormDestroy                                        |
 |                                                                      |
 | Tidy up                                                              |
 *----------------------------------------------------------------------*)
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FResourceModule.Free;
  gProperties.Free;
  FExaminer.Free
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fmResourceObject) then
  begin
    fmResourceObject.PreviewKey(Key, Shift);
    FIgnoreChar := Key = 0;
  end
end;

procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if FIgnoreChar then
    Key := #0
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  InitializeHTMLHelp;
  ToolBarMenu.Font := Screen.MenuFont;
  UseInternationalFont(vstresources.Font);
end;


function TFormMain.GetNodeElement(node: PVirtualNode;
  var Element: TResExamElement): Boolean;
var
  obj: TObject;
begin
  obj := vstResources.NodeObject [node];
  Result := Assigned(obj) and (obj is TResExamElement);
  if Result then
    Element := TResExamElement(obj)
  else
    Element := nil;
end;

function TFormMain.GetNodeResourceDetails(node: PVirtualNode): TResourceDetails;
var
  Element: TResExamElement;
begin
  if GetNodeElement(node, Element) and (Element is TResExamResource) then
    Result := TResExamResource(Element).ResourceDetails
  else
    Result := nil;
end;

function TFormMain.GetPersistDirectoryName(section: string): string;
var
  Registry: TRegistry;
begin
  Result := '';
  Registry := TRegistry.Create(KEY_READ);
  try
    if Registry.OpenKey('\' + PersistentPosition.ApplicationKey + '\Directories', False) then
      Result := Registry.ReadString (section);
  finally
    Registry.Free;
  end;
end;

function TFormMain.GetResourceDetailsNode(ResourceDetails: TResourceDetails): PVirtualNode;
begin
  Result := vstResources.ForEach(CheckDetails, Integer (ResourceDetails))
end;

function TFormMain.IsDirty: Boolean;
begin
  if Assigned(FResourceModule) then
    Result := FResourceModule.Dirty
  else
    Result := False;
end;

(*----------------------------------------------------------------------*
 | TFormMain.Loaded                                                       |
 |                                                                      |
 | Initialize.                                                          |
 *----------------------------------------------------------------------*)
procedure TFormMain.Loaded;
begin
  inherited;
  SetCaption;
  gProperties := TPEResourceExplorerProperties.Create(Self);
  PostMessage(handle, WM_INITIALIZE, 0, 0);
end;

(*----------------------------------------------------------------------*
 | TFormMain.mnuEditClick                                                 |
 |                                                                      |
 | Before displaying the 'Edit' Menu, set the undo and redo             |
 | descriptions                                                         |
 *----------------------------------------------------------------------*)
procedure TFormMain.MenuItemEditClick(Sender: TObject);
var
  s: string;
  ResForm: TFormResource;
  Element: TResExamElement;
  Name: TResExamName;
begin
  ResForm := ResourceForm;
  if Assigned(ResForm) then
  begin
    s := ResForm.UndoDescription;

    if s = '' then
      MenuItemEditUndo.Caption := FUndo
    else
      MenuItemEditUndo.Caption := Format(rstUndo, [s]);

    s := ResForm.RedoDescription;

    if s = '' then
      MenuItemEditRedo.Caption := FRedo
    else
      MenuItemEditRedo.Caption := Format(rstRedo, [s])
  end
  else
  begin
    GetNodeElement(vstResources.FocusedNode, Element);
    if Element is TResExamName then
    begin
      Name := TResExamName(Element);
      s := Name.UndoDescription;
      if s = '' then
        MenuItemEditUndo.Caption := FUndo
      else
        MenuItemEditUndo.Caption := Format(rstUndo, [s]);

      s := Name.RedoDescription;

      if s = '' then
        MenuItemEditRedo.Caption := FRedo
      else
        MenuItemEditRedo.Caption := Format(rstRedo, [s]);
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.MRUList1PopupMenuClick                             |
 |                                                                      |
 | Open a file from the MRU list popup.                                 |
 *----------------------------------------------------------------------*)
procedure TFormMain.MRUListPopupMenuClick(Sender: TObject);
var
  Item: TMenuItem;
  p: Integer;
begin
  if Sender is TMenuItem then
  begin
    Item := TMenuItem (Sender);
    p := Pos (' ', Item.Caption);
    if p > 0 then
      OpenFile(Copy(Item.Caption, p + 1, MaxInt))
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.OpenFile                                           |
 |                                                                      |
 | Open the specified module or .ResourceDetails file                               |
 *----------------------------------------------------------------------*)
procedure TFormMain.OpenFile(const FileName: string);
var
  Ext: string;
begin
  if SaveChanges then
  begin
    FreeAndNil (FResourceModule);
    ClearUndoDetails;

    Ext := UpperCase(ExtractFileExt(FileName));
    if (Ext = '.RES') or (Ext = '.DCR') then
      FResourceModule := TResModule.Create
    else
      if (Ext = '.RC') then
      begin
        FResourceModule := TRCModule.Create;
        TRCModule(FResourceModule).IncludePath := gProperties.IncludePath;
      end
      else
        if (Win32Platform = VER_PLATFORM_WIN32_NT) and (gProperties.ParserType = 0) then
          FResourceModule := TNTModule.Create
        else
          FResourceModule := TPEResourceModule.Create;

    FResourceModule.LoadFromFile(FileName);
    Self.FileName := FileName;
    MRUList.AddFile(FileName);
    UpdateDisplay(Nil);
  end
end;

function TFormMain.ResourceForm: TFormResource;
begin
  if Assigned(fmResourceObject) and (fmResourceObject is TFormResource) then
    Result := TFormResource(fmResourceObject)
  else
    Result := Nil
end;

function TFormMain.SaveChanges: Boolean;
var
  s: string;
begin
  if Assigned(fmResourceObject) then
    fmResourceObject.TidyUp;
  if FileName = '' then
    s := rstUntitled
  else
    s := ExtractFileName(FileName);
  if ISDirty then
    case MessageBox (Handle, PChar (Format(rstChanges, [s])), PChar (Application.Title), MB_YESNOCANCEL or MB_ICONQUESTION) of
      ID_YES: Result := SaveFile;
      ID_CANCEL: Result := False;
      else
        Result := True
    end
  else
    Result := True
end;

function TFormMain.SaveFile: Boolean;
begin
  if FFileName = '' then
    Result := SaveFileAs
  else
  begin
    Application.ProcessMessages;
    FResourceModule.SaveToFile(FFileName);
    Result := True;
  end
end;

function TFormMain.SaveFileAs: Boolean;
var
  s, FilName: string;
  NewModule: TResModule;
  p: Integer;
  ResourceDetails: TResourceDetails;
  tp, nm: string;
  lg: Integer;
begin
  Result := False;
  Application.ProcessMessages;  // Ensures that toolbar button doesn't temporarily disappear
  if Assigned(FResourceModule) then
  begin
    FilName := FileName;
    if FResourceModule is TRCModule then
    begin
      p := Length(FilName);
      while p > 0 do
        if FilName [p] = '.' then
          break
        else
          Dec(p);

      if p = 0 then
        FilName := FilName + '.res'
      else
        FilName := Copy(s, 1, p - 1) + '.res';
    end;

    SaveDialog.FileName := FilName;

    s := '';
    if FilName <> '' then
    begin
      s := ExtractFileExt(FilName);
      if Length(s) > 0 then
        s := Copy(s, 2, MaxInt);
    end
    else
      if FResourceModule is TResModule then
        s := 'RES';

    SaveDialog.DefaultExt := s;
    if (FResourceModule is TResModule) or (FResourceModule is TRCModule)  then
      SaveDialog.Filter := rstRESFilter
    else
      SaveDialog.Filter := rstModuleFilter + '|' + rstRESFilter;

    if SaveDialog.Execute then
    begin
      s := UpperCase(ExtractFileExt(SaveDialog.FileName));

      if ((FResourceModule is TPEModule) or (FResourceModule is TRCModule)) and ((s = '.DCR') or (s = '.RES')) then
      begin
        ResourceDetails := SelectedResourceDetails;
        if ResourceDetails <> Nil then
        begin
          nm := ResourceDetails.ResourceName;
          tp := ResourceDetails.ResourceType;
          lg := ResourceDetails.ResourceLanguage;
        end
        else
        begin
          nm := '';
          lg := 0;
          tp := ''
        end;
        NewModule := TResModule.Create;
        NewModule.Assign (FResourceModule);
        FreeAndNil (FResourceModule);
        ClearUndoDetails;
        FResourceModule := NewModule;
        if nm <> '' then
          ResourceDetails := NewModule.FindResource(tp, nm, lg)
        else
          ResourceDetails := nil;
        UpdateDisplay(ResourceDetails);
      end;
      FResourceModule.SaveToFile(SaveDialog.FileName);
      FileName := SaveDialog.FileName;
      MRUList.AddFile(FileName);
      Result := True;
    end
  end
end;

function TFormMain.SelectedResourceDetails: TResourceDetails;
begin
  Result := GetNodeResourceDetails (vstResources.FocusedNode);
end;

(*----------------------------------------------------------------------*
 | TFormMain.SetCaption                                                   |
 |                                                                      |
 | Set the caption to display the loaded file.                          |
 *----------------------------------------------------------------------*)
procedure TFormMain.SetCaption;
var
  st: string;
begin
  if FileName = '' then
    st := Application.Title
  else
  begin
    st := ExtractFileName(FileName + ' - ' + Application.Title);
    if IsDirty then
      st := '*' + st
  end;

  Caption := st
end;

(*----------------------------------------------------------------------*
 | TFormMain.SetFileName                                                  |
 |                                                                      |
 | Set the file Name.                                                   |
 *----------------------------------------------------------------------*)
procedure TFormMain.SetFileName(const Value: string);
begin
  if FileName <> Value then
  begin
    FFileName := Value;
    SetCaption
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.SetFormMenuButton                                            |
 |                                                                      |
 | Turn on or off the '...object' Menu button.  This is set to the      |
 | Form's menu item.                                                    |
 *----------------------------------------------------------------------*)
procedure TFormMain.SetFormMenuButton(Item: TMenuItem);
begin
  if Assigned(Item) then
  begin
    ToolButtonResourceObject.Caption := Item.Caption;
    ToolButtonResourceObject.MenuItem := Item;
    ToolButtonResourceObject.Visible := True;
  end
  else
  begin
    ToolButtonResourceObject.Visible := False;
    ToolButtonResourceObject.MenuItem := nil;
    ToolButtonResourceObject.Caption := '...object'
  end
end;


procedure TFormMain.SetPersistDirectoryName(section: string;
  const Value: string);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    if Registry.OpenKey('\' + PersistentPosition.ApplicationKey + '\Directories', True) then
      Registry.WriteString (section, Value)
  finally
    Registry.Free
  end
end;

type
  TObjectFormRec = record
    ResourceDetails: TClass;
    Form: TResourceObjectFormClass;
  end;

const
  NoObjectDetails = 14;
  NoHeaderForms = 3;

var
  ObjectForms: array [0..NoObjectDetails - 1] of TObjectFormRec = (
    (ResourceDetails: TIconResourceDetails;                    Form: TFormIconGraphicsResource),
    (ResourceDetails: TCursorResourceDetails;                  Form: TFormCursorGraphicsResource),
    (ResourceDetails: TGraphicsResourceDetails;                Form: TFormGraphicsResource),
    (ResourceDetails: TTextResourceDetails;                    Form: TFormTextResource),
    (ResourceDetails: TIconCursorGroupResourceDetails;         Form: TFormGroupResource),
    (ResourceDetails: TVersionInfoResourceDetails;             Form: TFormVersionResource),
    (ResourceDetails: TMenuResourceDetails;                    Form: TFormMenuResource),
    (ResourceDetails: TDialogResourceDetails;                  Form: TFormDialogResource),
    (ResourceDetails: TRCDataDescriptionResourceDetails;       Form: TFormRCDataDescriptionResource),
    (ResourceDetails: TRCDataPackagesResourceDetails;          Form: TFormPackagesResource),
    (ResourceDetails: TRCDataFormResourceDetails;              Form: TFormRCDataFormResource),
    (ResourceDetails: TXPManifestResourceDetails;              Form: TFormXPManifestResource),
    (ResourceDetails: TAcceleratorResourceDetails;             Form: TFormAcceleratorResource),
    (ResourceDetails: TResourceDetails;                        Form: TFormRawResource)  // Must be last entry!
  );

(*----------------------------------------------------------------------*
 | procedure TFormMain.SwitchView (ResourceDetails: TObject)                     |
 |                                                                      |
 | 'details' can be:                                                    |
 |                                                                      |
 |   *  A TResourceDetails object                                       |
 |   *  Nil                                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.SwitchView(ResourceDetails: TObject);

var
  i: Integer;
  FormClass: TResourceObjectFormClass;

begin { SwitchView }
  FormClass := nil;
  if Integer (ResourceDetails) > 32 then        // It's a genuine TResourceDetails or TImageSection
  begin
    for i := 0 to NoObjectDetails - 1 do
      if ResourceDetails is ObjectForms[i].ResourceDetails then
      begin
        FormClass := ObjectForms[i].Form;
        break
      end
  end;

  // FormClass is now a valid Form class - or Nil

  if not Assigned(fmResourceObject) or not Assigned(FormClass) or not (fmResourceObject.ClassType = FormClass) then
  begin

    if Assigned(fmResourceObject) then  // Get rid of the old resource Form
    begin
      SetFormMenuButton (Nil);
      FreeAndNil (fmResourceObject);
    end;

    if Assigned(FormClass) then         // Create the new resource Form
    begin
      fmResourceObject := FormClass.Create(Nil);
      fmResourceObject.Parent := PanelResource;
      fmResourceObject.TabStop := True;
      fmResourceObject.ResourceModule := FResourceModule;
      SetFormMenuButton (fmResourceObject.Menu);
      fmResourceObject.Show;
      fmResourceObject.Font := Self.Font;
      fmResourceObject.Obj := ResourceDetails;
    end
  end
  else  // Form class is valid, and hasn't changed. Update the form.

    if Assigned(fmResourceObject) then
      fmResourceObject.obj := ResourceDetails;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.UpdateActions                                      |
 |                                                                      |
 | Update the action list, depending on the state.                      |
 *----------------------------------------------------------------------*)
procedure TFormMain.UpdateActions;
var
  ResourceDetails: TResourceDetails;
  Element: TResExamElement;
  ResForm: TFormResource;
begin
  GetNodeElement(vstResources.FocusedNode, Element);
  if (Element is TResExamResource) then
    ResourceDetails := TResExamResource(Element).ResourceDetails
  else
    ResourceDetails := nil;

  ActionResourceDeleteResource.Enabled := ResourceDetails <> nil;
  ResForm := ResourceForm;
  if Assigned(ResForm) then
  begin
    ActionEditUndo.Enabled := ResForm.CanUndo;
    ActionEditRedo.Enabled := ResForm.CanRedo;

    ActionEditCut.Enabled := ResForm.CanCut;
    ActionEditCopy.Enabled := ResForm.CanCopy;
    ActionEditPaste.Enabled := ResForm.CanPaste;
    ActionEditSelectAll.Enabled := ResForm.CanSelectAll;
    ActionEditDelete.Enabled := ResForm.CanDelete;

    ActionResourceExportResource.Enabled := Assigned(ResourceDetails);
    ActionResourceClone.Enabled := Assigned(ResourceDetails);
    ActionResourceProperties.Enabled := Assigned(ResourceDetails) and not (ResourceDetails is TIconCursorResourceDetails);
  end
  else
  begin
    if Element is TResExamName then
    begin
      ActionEditUndo.Enabled := TResExamName(Element).CanUndo;
      ActionEditRedo.Enabled := TResExamName(Element).CanRedo
    end
    else
    begin
      ActionEditUndo.Enabled := False;
      ActionEditRedo.Enabled := False;
    end;

    ActionEditCut.Enabled := False;
    ActionEditCopy.Enabled := False;
    ActionEditPaste.Enabled := False;
    ActionEditSelectAll.Enabled := False;
    ActionResourceExportResource.Enabled := False;
    ActionResourceProperties.Enabled := False;
    ActionResourceClone.Enabled := False;
    ActionEditDelete.Enabled := False;
  end;

  if FResourceModule is TRCModule then
    ActionFileSave.Enabled := False
  else
    ActionFileSave.Enabled := IsDirty;

  if FWasDirty <> IsDirty then
  begin
    SetCaption;
    FWasDirty := IsDirty
  end;
(*
  if Assigned(ActiveControl) then
  sbMain.Panels[0].Text := ActiveControl.Name; *)
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.UpdateDisplay                                      |
 |                                                                      |
 | Update the display by building the tree view.                        |
 *----------------------------------------------------------------------*)
procedure TFormMain.UpdateDisplay(selectDetails: TResourceDetails);
var
  ResSection: TResExamSection;
begin { UpdateDisplay }
  SwitchView (Nil);

  if not Assigned(FResourceModule) then Exit;

  FExaminer.SetResourceModule(FResourceModule, false);
  ResSection := FExaminer.ResourceSection;

  vstResources.BeginUpdate;
  try
    if Assigned(ResSection) then
      vstResources.RootNodeCount := ResSection.Count
    else
      vstResources.RootNodeCount := 0;
    vstResources.ReinitNode(nil, true);
  finally
    vstResources.EndUpdate
  end;

  if Assigned(SelectDetails) then
  begin
    vstResources.SelectAndFocusNode(GetResourceDetailsNode(SelectDetails));
    SwitchView (SelectDetails)
  end
  else
  begin
    SwitchView (Nil);
    vstResources.FullCollapse;
    vstResources.SelectAndFocusNode(vstResources.GetFirst);
  end
end;

procedure TFormMain.WmAddImageResource(var msg: TMessage);
var
  o: TResourceDetails;
  grp: TIconCursorGroupResourceDetails;
  ResourceDetails: TIconCursorResourceDetails;
  p: PVirtualNode;
begin
  p := vstResources.FocusedNode;
  o := GetNodeResourceDetails (p);

  if Assigned(o) and (o is TIconCursorResourceDetails) then
  begin
    ResourceDetails := TIconCursorResourceDetails (o);
    p := p.Parent
  end
  else
    ResourceDetails := nil;

  if (p <> Nil) and (ResourceDetails <> Nil) then
  begin
    o := GetNodeResourceDetails (p);

    if o is TIconCursorGroupResourceDetails then
    begin
      grp := TIconCursorGroupResourceDetails (o);
      ResourceDetails := TIconCursorResourceDetails (CloneResource(ResourceDetails, '', -1));
      grp.AddToGroup (ResourceDetails);
      FResourceModule.SortResources;
      UpdateDisplay(ResourceDetails);
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.WmInitialize                                       |
 |                                                                      |
 | WM_INITIALIZE handler.  Start with a blank .ResourceDetails file.                |
 *----------------------------------------------------------------------*)
procedure TFormMain.WmInitialize(var msg: TMessage);
begin
  FUndo := ActionEditUndo.Caption;
  FRedo := ActionEditRedo.Caption;
  FExaminer := TResourceExaminer.Create(nil);

  if ParamCount = 0 then
    ActionFileNew.Execute
  else
    OpenFile(ParamStr (1))
end;

(*----------------------------------------------------------------------*
 | TFormMain.WmPropertiesChanged                                          |
 |                                                                      |
 | Handle our 'WM_PROPERTIESCHANGED' message.  Apply persistent         |
 | properties                                                           |
 *----------------------------------------------------------------------*)
procedure TFormMain.WmPropertiesChanged(var msg: TMessage);
begin
  ToolbarMain.Visible := gProperties.ShowToolbar;
  ActionViewToolbar.Checked := ToolbarMain.Visible;

  StatusBarMain.Visible := gProperties.ShowStatusBar;
  ActionViewStatusbar.Checked := StatusBarMain.Visible;
  UseInternationalFont(vstResources.Font);

  if Assigned(fmResourceObject) then
    fmResourceObject.UpdateFonts
end;

procedure TFormMain.WmStatusBar(var Msg: TMessage);
begin
  if Msg.lParam <> 0 then
    StatusBarMain.Panels[1].Text := PChar(Msg.lParam) + '     ';

  if Msg.wParam <> 0 then
    StatusBarMain.Panels[0].Text := PChar(Msg.wParam);
end;

procedure TFormMain.ActionResourceCloneExecute(Sender: TObject);
var
  ResourceDetails: TResourceDetails;
  Dialog: TDialogCloneResource;
begin
  ResourceDetails := SelectedResourceDetails;
  if Assigned(ResourceDetails) then
  begin
    Dialog := TDialogCloneResource.Create(nil);
    try
      Dialog.ResourceDetails := ResourceDetails;
      if Dialog.ShowModal = mrOk then
      begin
        if Dialog.RadioButtonByLanguage.Checked then
          ResourceDetails := CloneResource(ResourceDetails, '', Dialog.Language)
        else
          ResourceDetails := CloneResource(ResourceDetails, Dialog.EditName.Text, -1);
        FResourceModule.SortResources;
        UpdateDisplay(ResourceDetails)
      end
    finally
      Dialog.Free
    end
  end
end;

procedure TFormMain.ActionResourceGrabExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.vstResourcesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  SwitchView (GetNodeResourceDetails (Node));
end;

procedure TFormMain.vstResourcesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  ResourceDetails: TResourceDetails;
begin
  if Kind = ikOverlay then
    Exit;

  ResourceDetails := GetNodeResourceDetails (Node);
  if Assigned(ResourceDetails) then
    ImageIndex := GetTypeImage(ResourceDetails.ResourceType)
  else
    if vsExpanded in Node^.States then
      ImageIndex := imgOpenFolder
    else
      ImageIndex := imgClosedFolder
end;

procedure TFormMain.vstResourcesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Element: TResExamElement;
begin
  if GetNodeElement(Node, Element) then
    CellText := Element.DisplayName
end;

procedure TFormMain.vstResourcesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Element: TResExamElement;
begin
  if GetNodeElement(Node, Element) then
    ChildCount := Element.Count
  else
    ChildCount := 0
end;

procedure TFormMain.vstResourcesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Element, pelem: TResExamElement;
begin
  if not Assigned(ParentNode) then
    pelem := FExaminer.ResourceSection
  else
    GetNodeElement(ParentNode, pelem);

  if Assigned(pelem) then
  begin
    Element := pelem.Element [Node.Index];
    vstResources.NodeObject [Node] := Element;
    if Element.Count > 0 then
      Include(InitialStates, ivsHasChildren)
  end
end;

procedure TFormMain.vstResourcesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Element: TResExamElement;
begin
  if GetNodeElement(node, Element) then
    if (Element is TResExamName) then
      TResExamName(Element).Name := NewText
    else
      if (Element is TResExamType) then
        TResExamType(Element).Name := NewText;

  vstResources.InvalidateNode(node)
end;

procedure TFormMain.vstResourcesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := EditAllowed (Node);
end;

function TFormMain.EditAllowed(node: PVirtualNode): Boolean;
var
  Element: TResExamElement;
  tp: TResExamType;
begin
  Result := False;
  if GetNodeElement(node, Element) then
    if Element is TResExamName then
    begin
      Result := True;
      node := node^.Parent;
      if GetNodeElement(node, Element) and (Element is TResExamType) then
      begin
        tp := TResExamType(Element);
        if StrToIntDef (tp.Name, -1) = Integer (RT_STRING) then
          Result := False
      end
    end
    else
      if (Element is TResExamType) then
      begin
        tp := TResExamType(Element);
        Result:= tp.Name = tp.DisplayName
      end
end;

procedure TFormMain.UpdateResourceNodes(ResourceDetails: TResourceDetails;
  node: PVirtualNode);
var
  nm, tp: WideString;
  Element: TResExamElement;
begin
  if Assigned(ResourceDetails) and Assigned(node) then
  begin
    if ResourceDetails is TStringResourceDetails then
      nm := ResIdToStringsId (ResourceDetails.ResourceName)
    else
      nm := ResourceDetails.ResourceName;

    tp := ResourceDetails.ResourceType;

    while Assigned(node) do
    begin
      if GetNodeElement(node, Element) then
        if Element is TResExamName then
          TResExamName(Element).Name := nm
        else
          if Element is TResExamType then
            TResExamType(Element).Name := tp;
      vstResources.InvalidateNode(node);
      node := node.Parent;
      if node = vstResources.RootNode then
        break
    end
  end
end;


end.
