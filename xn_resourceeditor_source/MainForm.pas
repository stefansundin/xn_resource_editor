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
  Windows, Messages, SysUtils, SysConst, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, ToolWin, ExtCtrls, ImgList, StdActns, ActnList, unitResourceDetails,
  cmpStandardSystemMenu, cmpPersistentPosition, cmpMRUList,
  StdCtrls, shellapi, unitCREdProperties,
  cmpNTAboutBox, ExtDlgs, ResourceForm, GifImage, JPeg, PngImage,
  AppEvnts, unitHTMLHelpViewer, ActnMan, ActnCtrls, ActnMenus, XPStyleActnCtrls,
  VirtualTrees, ExVirtualStringTree, unitResourceExaminer, XPMan;
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
// TfmMain
  TfmMain = class(TForm)
    mnuMain: TMainMenu;
    ilMain: TImageList;
    alMain: TActionList;
    actHelpContents: THelpContents;
    actHelpHelpOnHelp: THelpOnHelp;
    actHelpTopicSearch: THelpTopicSearch;
    mnuEdit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuEditUndo: TMenuItem;
    N1: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuEditSelectAll: TMenuItem;
    mnuHelpContents: TMenuItem;
    mnuHelpHelpOnHelp: TMenuItem;
    mnuHelpTopicSearch: TMenuItem;
    actFileNew: TAction;
    actFileOpenFile: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFilePrint: TAction;
    mnuFile: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    N2: TMenuItem;
    mnuFilePrint: TMenuItem;
    N3: TMenuItem;
    actFileExit: TAction;
    mnuFileExit: TMenuItem;
    sbMain: TStatusBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    Splitter1: TSplitter;
    MRUList1: TMRUList;
    PersistentPosition1: TPersistentPosition;
    StandardSystemMenu1: TStandardSystemMenu;
    pomMRU: TPopupMenu;
    pnlResource: TPanel;
    mnuResource: TMenuItem;
    actResourceAddResource: TAction;
    actResourceDeleteResource: TAction;
    mnuResourceAddResource: TMenuItem;
    mnuResourceDeleteResource: TMenuItem;
    N4: TMenuItem;
    actViewToolbar: TAction;
    actViewStatusbar: TAction;
    actViewProperties: TAction;
    mnuView: TMenuItem;
    mnuViewToolbar: TMenuItem;
    mnuViewStatusbar: TMenuItem;
    N5: TMenuItem;
    mnuViewProperties: TMenuItem;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditCut: TAction;
    actEditSelectAll: TAction;
    actEditUndo: TAction;
    actEditRedo: TAction;
    mnuEditRedo: TMenuItem;
    Open1: TMenuItem;
    actHelpAbout: TAction;
    N6: TMenuItem;
    mnuHelpAbout: TMenuItem;
    NTAboutBox1: TNTAboutBox;
    actResourceExportResource: TAction;
    ExportResource1: TMenuItem;
    ilResources: TImageList;
    actResourceImportResource: TAction;
    N7: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    actResourceProperties: TAction;
    N8: TMenuItem;
    mnuResourceProperties: TMenuItem;
    pomResources: TPopupMenu;
    AddResource1: TMenuItem;
    DeleteResource1: TMenuItem;
    N9: TMenuItem;
    ImportResource1: TMenuItem;
    ExportResource2: TMenuItem;
    N10: TMenuItem;
    Properties1: TMenuItem;
    actEditDelete: TAction;
    mnuEditDelete: TMenuItem;
    actResourceGrab: TAction;
    Grab1: TMenuItem;
    GrabBitmap1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    ApplicationEvents1: TApplicationEvents;
    actResourceImportRCDataResource: TAction;
    ImportRCDataResource1: TMenuItem;
    ImportRCDataResource2: TMenuItem;
    OpenDialog1: TOpenDialog;
    actResourceImportOtherResource: TAction;
    ImportUserResource1: TMenuItem;
    N11: TMenuItem;
    ImportUserResource2: TMenuItem;
    N12: TMenuItem;
    actResourceClone: TAction;
    CloneResource1: TMenuItem;
    vstResources: TExVirtualStringTree;
    XPManifest1: TXPManifest;
    tbMenu: TToolBar;
    btnFile: TToolButton;
    btnEdit: TToolButton;
    ToolButton2: TToolButton;
    ToolButton1: TToolButton;
    btnResourceObject: TToolButton;
    btnHelp: TToolButton;
    tbMain: TToolBar;
    btnFileNew: TToolButton;
    btnFileOpen: TToolButton;
    btnFileSave: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure vstResourcesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure vstResourcesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstResourcesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstResourcesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vstResourcesInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vstResourcesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vstResourcesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure actResourceGrabExecute(Sender: TObject);
    procedure actResourceCloneExecute(Sender: TObject);
    procedure actFileOpenFileExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFilePrintExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MRUList1PopupMenuClick(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actViewToolbarExecute(Sender: TObject);
    procedure actViewStatusbarExecute(Sender: TObject);
    procedure actViewPropertiesExecute(Sender: TObject);
    procedure actEditUndoExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure actHelpAboutExecute(Sender: TObject);
    procedure actResourceExportResourceExecute(Sender: TObject);
    procedure actResourceAddResourceExecute(Sender: TObject);
    procedure actResourceDeleteResourceExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actResourceImportResourceExecute(Sender: TObject);
    procedure actResourcePropertiesExecute(Sender: TObject);
    procedure actEditSelectAllExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    function ApplicationEvents1Help(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
    procedure actResourceImportRCDataResourceExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actResourceImportOtherResourceExecute(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG;
      var Handled: Boolean);
  private
    fResourceModule : TResourceModule;
    fUndo, fRedo : string;
    fFileName : string;
    fIgnoreChar : Boolean;
    fWasDirty : boolean;
    fExaminer : TResourceExaminer;

    procedure SetCaption;
    procedure UpdateDisplay (selectDetails : TResourceDetails);
    procedure OpenFile (const fileName : string);
    procedure SwitchView (details : TObject);
    procedure WmInitialize (var msg : TMessage); message WM_INITIALIZE;
    procedure WmStatusBar (var Msg : TMessage); message WM_STATUSBAR;
    procedure WmPropertiesChanged (var msg : TMessage); message WM_PROPERTIES_CHANGED;
    procedure WmAddImageResource (var msg : TMessage); message WM_ADDIMAGERESOURCE;
    procedure SetFormMenuButton (item : TMenuItem);
    procedure SetFileName(const Value: string);

    function SaveFile : boolean;
    function SaveFileAs : Boolean;
    function SaveChanges : Boolean;
    function IsDirty : Boolean;

    function GetNodeElement (node : PVirtualNode; var elem : TResExamElement) : boolean;
    function GetNodeResourceDetails (node : PVirtualNode) : TResourceDetails;
    function GetResourceDetailsNode (details : TResourceDetails) : PVirtualNode;
    function SelectedResourceDetails : TResourceDetails;
    function ResourceForm : TfmResource;
    procedure CheckDetails (p : PVirtualNode; param : Integer; var continue : boolean);

    property FileName : string read fFileName write SetFileName;
    function GetPersistDirectoryName(section: string): string;
    procedure SetPersistDirectoryName(section: string;
      const Value: string);
    function CloneResource(res: TResourceDetails; newName : string; newLanguage : Integer): TResourceDetails;
    function EditAllowed (node : PVirtualNode) : boolean;
    procedure UpdateResourceNodes (res : TResourceDetails; node : PVirtualNode);
  protected
    procedure Loaded; override;
    procedure UpdateActions; override;
  public
    property PersistDirectoryName [section : string] : string read GetPersistDirectoryName write SetPersistDirectoryName;
    { Public declarations }
  end;
{$endregion}

//----------------------------------------------------------------------
var
  fmMain: TfmMain;

//----------------------------------------------------------------------
procedure AttachMenu (const buttonCaption : string; menu : TMenuItem);
procedure DetachMenu (const buttonCaption : string);
function GetTypeImage (const tp : string) : Integer;

implementation

{$R *.DFM}

{$region 'Implementation Uses Section'}
uses Registry, RawResourceForm,
     unitPEFile,                        // Accept resources from PE files
     unitNTModule,                      // Use this instead if NT
     unitResFile,                       // Accept resources from .RES files
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

     GroupResourceForm,                 // Display form for Icons & cursor groups
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
  rstAnyFileFilter = 'Any File (*.*)|*.*';

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
procedure AttachMenu (const buttonCaption : string; menu : TMenuItem);
var
  i : Integer;
  btn : TToolButton;
begin
  exit;
  if csDestroying in fmMain.ComponentState then Exit;
  btn := Nil;
  for i := 0 to fmMain.tbMenu.ButtonCount - 1 do
    if fmMain.tbMenu.Buttons [i].Caption = buttonCaption then
    begin
      btn := fmMain.tbMenu.Buttons [i];
      break
    end;

  if Assigned (btn) then
  begin
    btn.MenuItem := menu;
    btn.Visible := True
  end
end;

(*----------------------------------------------------------------------*
 | procedure DetachMenu                                                 |
 |                                                                      |
 | Global procedure - detaches a menu to a button (the ...Object one)   |
 *----------------------------------------------------------------------*)
procedure DetachMenu (const buttonCaption : string);
var
  i : Integer;
  btn : TToolButton;
begin
  if csDestroying in fmMain.ComponentState then Exit;
  btn := Nil;
  for i := 0 to fmMain.tbMenu.ButtonCount - 1 do
    if fmMain.tbMenu.Buttons [i].Caption = buttonCaption then
    begin
      btn := fmMain.tbMenu.Buttons [i];
      break
    end;

  if Assigned (btn) then
  begin
    btn.Visible := False;
    btn.MenuItem := Nil
  end
end;

(*----------------------------------------------------------------------*
 | function GetTypeImage                                                |
 |                                                                      |
 | Get the image index for a resource type.  The 'magic number'         |
 | returned is an index in the ilResources image list.                  |
 *----------------------------------------------------------------------*)
function GetTypeImage (const tp : string) : Integer;
var
  i : Integer;
begin
  Result := 0;
  i := ResourceNametoInt (tp);

  case i of
    Integer (RT_VERSION)      : i := 8;
    Integer (RT_MESSAGETABLE) : i := Integer (RT_STRING)
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
 | function GetGraphicsClass : TGraphicClass                            |
 |                                                                      |
 | Get a graphic class associated with an extension                     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   ext : string                 The extension to find.                |
 |                                                                      |
 | The function returns the graphics class                              |
 *----------------------------------------------------------------------*)
function GetGraphicsClass (ext : string) : TGraphicClass;
begin
  ext := UpperCase (ext);
  if (ext <> '') and (ext [1] = '.') then
    Delete (ext, 1, 1);

  if ext = 'BMP' then
    Result := TBitmap
  else
    if (ext = 'WMF') or (ext = 'EMF') then
      Result := TMetafile
    else
      if ext = 'ICO' then
        Result := TExIcon
      else
        if ext = 'CUR' then
          Result := TExCursor
        else
          if (ext = 'JPG') or (ext = 'JPEG') then
            Result := TJpegImage
          else
            if ext = 'GIF' then
              Result := TGIFImage
            else
              if ext = 'PNG' then
                result := TPngObject
              else
                Result := Nil
end;

{*----------------------------------------------------------------------*
 | function CreateCompatibleGraphic                                     |
 |                                                                      |
 | Create a graphic of a specified type, using the image and dimensions |
 | of another graphic.                                                  |
 |                                                                      |
 | Parameters:                                                          |
 |   graphic : TGraphic;                The original graphic            |
 |   newCls : TGraphicClass             The new graphic class           |
 *----------------------------------------------------------------------*}
function CreateCompatibleGraphic (graphic : TGraphic; newCls : TGraphicClass) : TGraphic;
var
  viaBitmap : boolean;
  bmp : TBitmap;
  gif : TGifImage;
begin
  viaBitmap := (newCls = TPNGObject);

  result := newCls.Create;
  try
    if viaBitmap then           // Some conversions work better if you convert
    begin                       // the original graphic to a bitmap, then convert
      bmp := TBitmap.Create;    // that to the new format.
      try
        bmp.Assign (graphic);
        result.Assign (bmp)
      finally
        bmp.Free
      end
    end
    else
      if newCls = TGifImage then        // GIF images can do cool things with
      begin                             // dithering and palettes.  Treat as a
        gif := TGifImage (result);      // a special case, so that these get used
        gif.DitherMode := dmFloydSteinberg;
        gif.ColorReduction := rmQuantizeWindows;
        result.Assign (graphic);
      end
      else
        result.Assign (graphic)
  except
    FreeAndNil (result)
  end
end;
{$endregion}

{ TfmMain }

(*----------------------------------------------------------------------*
 | TfmMain.actEditCopyExecute                                           |
 *----------------------------------------------------------------------*)
procedure TfmMain.actEditCopyExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Copy
end;

(*----------------------------------------------------------------------*
 | TfmMain.actEditCutExecute                                            |
 *----------------------------------------------------------------------*)
procedure TfmMain.actEditCutExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Cut
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.actEditDeleteExecute                               |
 *----------------------------------------------------------------------*)
procedure TfmMain.actEditDeleteExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.EditDelete
end;


(*----------------------------------------------------------------------*
 | TfmMain.actEditPasteExecute                                          |
 *----------------------------------------------------------------------*)
procedure TfmMain.actEditPasteExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Paste
end;

(*----------------------------------------------------------------------*
 | TfmMain.actEditRedoExecute                                           |
 |                                                                      |
 | Handler for Edit/Redo.  Tell the form to redo changes.               |
 *----------------------------------------------------------------------*)
procedure TfmMain.actEditRedoExecute(Sender: TObject);
var
  resForm : TfmResource;
  elem : TResExamElement;
begin
  resForm := ResourceForm;
  if Assigned (resForm) then
  begin
    resForm.Redo;
    fmResourceObject.Obj := resForm.ResourceDetails;
    UpdateResourceNodes (SelectedResourceDetails, vstResources.FocusedNode);
  end
  else
  begin
    GetNodeElement (vstResources.FocusedNode, elem);
    if elem is TResExamName then
    begin
      TResExamName (elem).Redo;
      vstResources.InvalidateNode(vstResources.FocusedNode)
    end
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.actEditSelectAll                                             |
 *----------------------------------------------------------------------*)
procedure TfmMain.actEditSelectAllExecute(Sender: TObject);
begin
  if ResourceForm <> Nil then
    ResourceForm.SelectAll
end;

(*----------------------------------------------------------------------*
 | TfmMain.actEditUndoExecute                                           |
 |                                                                      |
 | Handler for Edit/Undo.  Tell the form to undo changes.               |
 *----------------------------------------------------------------------*)
procedure TfmMain.actEditUndoExecute(Sender: TObject);
var
  resForm : TfmResource;
  elem : TResExamElement;
begin
  resForm := ResourceForm;
  if Assigned (resForm) then
  begin

    resForm.Undo;   // Perform the undo
    fmResourceObject.Obj := resForm.ResourceDetails;

                    // Update the resource tree to reflect changes
                    // in the resource name/language
    UpdateResourceNodes (SelectedResourceDetails, vstResources.FocusedNode);
  end
  else
  begin
    GetNodeElement (vstResources.FocusedNode, elem);
    if elem is TResExamName then
    begin
      TResExamName (elem).Undo;
      vstResources.InvalidateNode(vstResources.FocusedNode)
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.actFileExitExecute                                 |
 |                                                                      |
 | FileExit action handler                                              |
 *----------------------------------------------------------------------*)
procedure TfmMain.actFileExitExecute(Sender: TObject);
begin
  Close
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.actFileNewExecute                                  |
 |                                                                      |
 | FileNew action handler                                               |
 *----------------------------------------------------------------------*)
procedure TfmMain.actFileNewExecute(Sender: TObject);
begin
  if SaveChanges then
  begin
    FreeAndNil (fResourceModule);
    ClearUndoDetails;
    fResourceModule := TResModule.Create; // Create an empty .RES module
    FileName := '';
    UpdateDisplay (nil)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.actFileOpenExecute                                 |
 |                                                                      |
 | FileOpen action handler.                                             |
 *----------------------------------------------------------------------*)
procedure TfmMain.actFileOpenFileExecute(Sender: TObject);
begin
  Application.ProcessMessages;  // Ensures that toolbar button doesn't temporarily disappear

  OpenDialog.InitialDir := MRUList1.MRUDirectory;
  if OpenDialog.Execute then
    OpenFile (OpenDialog.FileName);
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.actFilePrintExecute                                |
 |                                                                      |
 | FilePrint action handler                                             |
 *----------------------------------------------------------------------*)
procedure TfmMain.actFilePrintExecute(Sender: TObject);
begin
  if PrintDialog.Execute then
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.actFileSaveAsExecute                               |
 |                                                                      |
 | FileSaveAs action handler                                            |
 *----------------------------------------------------------------------*)
procedure TfmMain.actFileSaveAsExecute(Sender: TObject);
begin
  SaveFileAs
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.actFileSaveExecute                                 |
 |                                                                      |
 | File/Save handler.  If we know the file name, overwrite it.          |
 | Otherwise do File/Save As instead.                                   |
 *----------------------------------------------------------------------*)
procedure TfmMain.actFileSaveExecute(Sender: TObject);
begin
  SaveFile
end;

(*----------------------------------------------------------------------*
 | TfmMain.actHelpAboutExecute                                          |
 |                                                                      |
 | Display/execute the about box.                                       |
 *----------------------------------------------------------------------*)
procedure TfmMain.actHelpAboutExecute(Sender: TObject);
begin
  NTAboutBox1.Execute
end;

(*----------------------------------------------------------------------*
 | TfmMain.actResourceAddResourceExecute                                |
 |                                                                      |
 | Display/execute the Add Resource dialog.                             |
 *----------------------------------------------------------------------*)
procedure TfmMain.actResourceAddResourceExecute(Sender: TObject);
var
  dlg : TdlgAddResource;
  res : TResourceDetails;
begin
  dlg := TdlgAddResource.Create (nil);
  try
    if dlg.ShowModal = mrOk then
    begin
      res := dlg.ResourceDetailsClass.CreateNew (fResourceModule, 0, fResourceModule.GetUniqueResourceName (dlg.ResourceDetailsClass.GetBaseType));
      res.Dirty := True;
      fResourceModule.SortResources;
      UpdateDisplay (res);
    end
  finally
    dlg.Free
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.actResourceDeleteResourceExecute                             |
 |                                                                      |
 | Delete the selected resource, and delete the node(s) that refer to   |
 | it.                                                                  |
 *----------------------------------------------------------------------*)
procedure TfmMain.actResourceDeleteResourceExecute(Sender: TObject);
var
  p : PVirtualNode;
  details : TResourceDetails;
  idx : Integer;
  nextAction : Integer;
begin
  p := vstResources.FocusedNode;
  details := GetNodeResourceDetails (p);
  if not Assigned (details) then Exit;

  if vstResources.GetNextSibling(p) <> Nil then
    nextAction := 1
  else
    if p.Index > 0 then
      nextAction := 2
    else
      nextAction := 0;


  idx := fResourceModule.IndexOfResource(details);
  fResourceModule.DeleteResource(idx);

  if nextAction = 1 then
    details := fResourceModule.ResourceDetails [idx]
  else
    if nextAction = 2 then
      details := fResourceModule.ResourceDetails [idx - 1]
    else
      details := Nil;
  UpdateDisplay (details);
end;

(*----------------------------------------------------------------------*
 | TfmMain.actResourceExportResourceExecute                             |
 |                                                                      |
 | Display/execute the 'export resource' dialog.  This is either a      |
 | 'save' dialog, or a 'save picture' dialog, depending on the          |
 | resource to be exported.                                             |
 *----------------------------------------------------------------------*)
procedure TfmMain.actResourceExportResourceExecute(Sender: TObject);
var
  pict : TPicture;
  cls, cls1 : TGraphicClass;
  ext : string;
  newGraphic : TGraphic;
const
  pfNames : array [TPixelFormat] of string = ('Same as monitor', '2 Colour', '16 Colour', '256 Colour', '', '65536 Colour', 'High Colour', 'True Colour', '');
begin
  pict := nil;

  if fmResourceObject.Obj is TIconCursorGroupResourceDetails then
  begin
    pict := TPicture.Create;
    TIconCursorGroupResourceDetails (fmResourceObject.Obj).GetImage (pict)
  end
  else
    if fmResourceObject.Obj is TGraphicsResourceDetails then
    begin
      pict := TPicture.Create;
      TGraphicsResourceDetails (fmResourceObject.Obj).GetImage (pict)
    end;

  if Assigned (pict) then
  try
    if Assigned (pict.Graphic) then
    begin
      SavePictureDialog1.InitialDir := PersistDirectoryName ['Export'];

      cls := TGraphicClass (pict.Graphic.ClassType);
      SavePictureDialog1.DefaultExt := GraphicExtension (cls);

      if SavePictureDialog1.Execute then
      begin
        ext := ExtractFileExt (SavePictureDialog1.FileName);
        cls1 := GetGraphicsClass (ext);

        if cls <> cls1 then
        begin
          newGraphic := CreateCompatibleGraphic (pict.Graphic, cls1);
          try
            newGraphic.SaveToFile(SavePictureDialog1.FileName);
          finally
            newGraphic.Free
          end
        end
        else
          pict.SaveToFile (SavePictureDialog1.FileName);

        PersistDirectoryName ['Export'] := ExtractFilePath (SavePictureDialog1.FileName)
      end
    end
  finally
    pict.Free
  end
  else if fmResourceObject.Obj is TResourceDetails then
  begin
    SaveDialog.Filter := rstAnyFileFilter;
    SaveDialog.InitialDir := ExtractFilePath (SaveDialog.FileName);
    SaveDialog.FileName := TResourceDetails (fmResourceObject.Obj).ResourceName;
    if SaveDialog.Execute then
      TResourceDetails (fmResourceObject.Obj).Data.SaveToFile(SaveDialog.FileName)
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.actResourceImportOtherResourceExecute                        |
 |                                                                      |
 | Import a 'User' resource                                             |
 |                                                                      |
 | Get the resource name and type from the file name and extension      |
 *----------------------------------------------------------------------*)
procedure TfmMain.actResourceImportOtherResourceExecute(Sender: TObject);
var
  res : TResourceDetails;
  f : TMemoryStream;
  resType, resName : string;
begin
  OpenDialog1.InitialDir := PersistDirectoryName ['Import'];

  if OpenDialog1.Execute then
  begin
    f := TMemoryStream.Create;
    try
      f.LoadFromFile (OpenDialog1.FileName);

      resType := UpperCase (ExtractFileName (OpenDialog1.FileName));
      resName := SplitString ('.', resType);

      if resType = '' then
      begin
        resType := resName;
        resName := fResourceModule.GetUniqueResourceName (resType)
      end
      else
        if fResourceModule.FindResource(resType, resName, 0) <> Nil then
          resName := fResourceModule.GetUniqueResourceName(resType);

      res := TResourceDetails.CreateResourceDetails (
        fResourceModule, 0,
        resName,
        resType,
        f.Size, f.Memory);

      fResourceModule.AddResource (res);

      res.Dirty := True;

      PersistDirectoryName ['Import'] := ExtractFilePath (OpenDialog1.FileName);

      fResourceModule.SortResources;
      UpdateDisplay (res);
    finally
      f.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.actResourceImportRCDataResourceExecute                       |
 |                                                                      |
 | Import an RC data resource                                           |
 *----------------------------------------------------------------------*)
procedure TfmMain.actResourceImportRCDataResourceExecute(Sender: TObject);
var
  res : TResourceDetails;
  f : TMemoryStream;
begin
  OpenDialog1.InitialDir := PersistDirectoryName ['Import'];
  if OpenDialog1.Execute then
  begin
    f := TMemoryStream.Create;
    try
      f.LoadFromFile (OpenDialog1.FileName);

      res := TResourceDetails.CreateResourceDetails (
        fResourceModule, 0,
        fResourceModule.GetUniqueResourceName (IntToStr (Integer (RT_RCDATA))),
        IntToStr (Integer (RT_RCDATA)),
        f.Size, f.Memory);

      fResourceModule.AddResource (res);

      res.Dirty := True;

      PersistDirectoryName ['Import'] := ExtractFilePath (OpenDialog1.FileName);

      fResourceModule.SortResources;
      UpdateDisplay (res)
    finally
      f.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.actResourceImportResourceExecute                             |
 |                                                                      |
 | Import a graphic resource                                            |
 *----------------------------------------------------------------------*)
procedure TfmMain.actResourceImportResourceExecute(Sender: TObject);
var
  ResourceDetailsClass : TResourcedetailsClass;
  ext : string;
  res : TResourceDetails;
  img : TPicture;
begin
  OpenPictureDialog1.InitialDir := PersistDirectoryName ['Import'];

  if OpenPictureDialog1.Execute then
  begin
    ext := UpperCase (ExtractFileExt (OpenPictureDialog1.FileName));

    if ext = '.ICO' then
      ResourceDetailsClass := TIconGroupResourceDetails
    else
      if (ext = '.CUR') or (ext = '.ANI') then
        ResourceDetailsClass := TCursorGroupResourceDetails
      else
        if (ext = '.GIF') then
          ResourceDetailsClass := TGIFResourceDetails
        else
          if (ext = '.JPG') or (ext = '.JPEG') then
            ResourceDetailsClass := TJPEGResourceDetails
          else
            ResourceDetailsClass := TBitmapResourceDetails;

    res := ResourceDetailsClass.CreateNew (fResourceModule, 0, fResourceModule.GetUniqueResourceName (ResourceDetailsClass.GetBaseType));
    res.Dirty := True;

    if res is TIconCursorGroupResourceDetails then
      TIconCursorGroupResourceDetails (res).LoadImage (OpenPictureDialog1.FileName)
    else
    begin
      img := TPicture.Create;
      try
        img.LoadFromFile (OpenPictureDialog1.FileName);
        TGraphicsResourceDetails (res).SetImage (img);
      finally
        img.Free
      end
    end;

    PersistDirectoryName ['Import'] := ExtractFilePath (OpenPictureDialog1.FileName);

    fResourceModule.SortResources;
    UpdateDisplay (res);
  end

end;

procedure TfmMain.actResourcePropertiesExecute(Sender: TObject);
var
  dlg : TdlgResourceProperties;
  fm : TfmResource;
  newLanguage : LCID;
  newName : WideString;
  res, r : TResourceDetails;
begin
  fm := ResourceForm;
  res := SelectedResourceDetails;

  if Assigned (res) and Assigned (fm) and not (res is TIconCursorResourceDetails) then
  begin
    dlg := TdlgResourceProperties.Create (nil);
    try
      dlg.ResourceDetails := res;

      if dlg.ShowModal = mrOK then
      begin
        newLanguage := dlg.Language;
        newName := dlg.ntedName.Text;

        if res is TStringResourceDetails then
          newName := StringsIDToResID (newName);

        if (newLanguage <> res.ResourceLanguage) or (newName <> res.ResourceName) then
        begin
          r := fResourceModule.FindResource (res.ResourceType, newName, newLanguage);
          if Assigned (r) and (r <> res) and (r.ResourceLanguage = newLanguage) then
            raise Exception.Create (rstDuplicateResourceName);

          fm.AddUndoEntry (rstChangeResourceProperties);


          if newLanguage <> res.ResourceLanguage then
            res.ResourceLanguage := newLanguage;

          if newName <> res.ResourceName then
          begin
            res.ResourceName := newName;
            if res is TStringResourceDetails then
              newName := ResIdToStringsId (newName);

            UpdateResourceNodes (res, vstResources.FocusedNode);
            if res is TStringResourceDetails then
              SwitchView (res);
          end
          else
            vstResources.Update
        end
      end
    finally
      dlg.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.actViewPropertiesExecute                                     |
 |                                                                      |
 | Display/execute the 'Properties' dialog                              |
 *----------------------------------------------------------------------*)
procedure TfmMain.actViewPropertiesExecute(Sender: TObject);
var
  dlg : TfmProperties;
begin
  dlg := TfmProperties.Create (Nil);
  try
    dlg.ShowModal
  finally
    dlg.Free
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.actViewStatusbarExecute                                      |
 |                                                                      |
 | Turn the status bar on/off                                           |
 *----------------------------------------------------------------------*)
procedure TfmMain.actViewStatusbarExecute(Sender: TObject);
begin
  gProperties.ShowStatusBar := not gProperties.ShowStatusBar
end;

(*----------------------------------------------------------------------*
 | TfmMain.actViewToolbarExecute                                        |
 |                                                                      |
 | Turn the toolbar on/off.                                             |
 *----------------------------------------------------------------------*)
procedure TfmMain.actViewToolbarExecute(Sender: TObject);
begin
  gProperties.ShowToolbar := not gProperties.ShowToolbar;
end;

function TfmMain.ApplicationEvents1Help(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := not (command = HELP_FINDER);
  if not CallHelp then
    PostMessage (Application.Handle, CM_INVOKEHELP, HELP_CONTENTS, 0);
  Result := False
end;

procedure TfmMain.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  Handled := not HHPreTranslateMessage (Msg);
end;

procedure TfmMain.CheckDetails(p: PVirtualNode; param: Integer;
  var continue: boolean);
begin
  continue := Integer (GetNodeResourceDetails (p)) <> param
end;

function TfmMain.CloneResource (res : TResourceDetails; newName : string; newLanguage : Integer) : TResourceDetails;
var
  i : Integer;
  icg, clicg : TIconCursorGroupResourceDetails;
  data : pointer;
begin
  if res is TIconCursorGroupResourceDetails then
    data := Nil
  else
    data := res.Data.Memory;

  if newLanguage <> -1 then
    result := TResourceDetails.CreateResourceDetails (
                                res.Parent,
                                newLanguage,
                                res.ResourceName,
                                res.GetBaseType, res.Data.Size, data)
  else
  begin
    if newName = '' then
      newName := fResourceModule.GetUniqueResourceName(res.GetBaseType);

    result := TResourceDetails.CreateResourceDetails (
                                res.Parent,
                                res.ResourceLanguage,
                                newName,
                                res.GetBaseType, res.Data.Size, data)
  end;

  if res is TIconCursorGroupResourceDetails then
  begin
    icg := TIconCursorGroupResourceDetails (res);
    clicg := result as TIconCursorGroupResourceDetails;
    for i := 0 to icg.ResourceCount - 1 do
      clicg.AddToGroup(TIconCursorResourceDetails (CloneResource (icg.ResourceDetails [i], '', newLanguage)));

      // Remove first empty image that was created along with
      // the group resource.  Note that you have to do this
      // *after* adding the other images - otherwise the newly
      // created group resource will be zapped!

    clicg.RemoveFromGroup(clicg.ResourceDetails [0]);
  end;
  result.Dirty := True;
  fResourceModule.AddResource(result);
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SaveChanges
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.FormDestroy                                        |
 |                                                                      |
 | Tidy up                                                              |
 *----------------------------------------------------------------------*)
procedure TfmMain.FormDestroy(Sender: TObject);
begin
  fResourceModule.Free;
  gProperties.Free;
  fExaminer.Free
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned (fmResourceObject) then
  begin
    fmResourceObject.PreviewKey (key, Shift);
    fIgnoreChar := key = 0;
  end
end;

procedure TfmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if fIgnoreChar then
    key := #0
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  InitializeHTMLHelp;
  tbMenu.Font := Screen.MenuFont;
  UseInternationalFont (vstresources.Font);
end;


function TfmMain.GetNodeElement(node: PVirtualNode;
  var elem: TResExamElement): boolean;
var
  obj : TObject;
begin
  obj := vstResources.NodeObject [node];
  result := Assigned (obj) and (obj is TResExamElement);
  if result then
    elem := TResExamElement (obj)
  else
    elem := Nil
end;

function TfmMain.GetNodeResourceDetails(node: PVirtualNode) : TResourceDetails;
var
  elem : TResExamElement;
begin
  if GetNodeElement (node, elem) and (elem is TResExamResource) then
    result := TResExamResource (elem).ResourceDetails
  else
    result := Nil
end;

function TfmMain.GetPersistDirectoryName(section: string): string;
var
  reg : TRegistry;
begin
  result := '';
  reg := TRegistry.Create (KEY_READ);
  try
    if reg.OpenKey ('\' + PersistentPosition1.ApplicationKey + '\Directories', False) then
      result := reg.ReadString (section);
  finally
    reg.Free
  end
end;

function TfmMain.GetResourceDetailsNode(details: TResourceDetails): PVirtualNode;
begin
  result := vstResources.ForEach(CheckDetails, Integer (details))
end;

function TfmMain.IsDirty: Boolean;
begin
  if Assigned (fResourceModule) then
    Result := fResourceModule.Dirty
  else
    Result := False
end;

(*----------------------------------------------------------------------*
 | TfmMain.Loaded                                                       |
 |                                                                      |
 | Initialize.                                                          |
 *----------------------------------------------------------------------*)
procedure TfmMain.Loaded;
begin
  inherited;
  SetCaption;
  gProperties := TPEResourceExplorerProperties.Create (self);
  PostMessage (handle, WM_INITIALIZE, 0, 0);
end;

(*----------------------------------------------------------------------*
 | TfmMain.mnuEditClick                                                 |
 |                                                                      |
 | Before displaying the 'Edit' menu, set the undo and redo             |
 | descriptions                                                         |
 *----------------------------------------------------------------------*)
procedure TfmMain.mnuEditClick(Sender: TObject);
var
  s : string;
  resForm : TfmResource;
  elem : TResExamElement;
  name : TResExamName;
begin
  resForm := ResourceForm;
  if Assigned (resForm) then
  begin
    s := resForm.UndoDescription;

    if s = '' then
      mnuEditUndo.Caption := fUndo
    else
      mnuEditUndo.Caption := Format (rstUndo, [s]);

    s := resForm.RedoDescription;

    if s = '' then
      mnuEditRedo.Caption := fRedo
    else
      mnuEditRedo.Caption := Format (rstRedo, [s])
  end
  else
  begin
    GetNodeElement (vstResources.FocusedNode, elem);
    if elem is TResExamName then
    begin
      name := TResExamName (elem);
      s := name.UndoDescription;
      if s = '' then
        mnuEditUndo.Caption := fUndo
      else
        mnuEditUndo.Caption := Format (rstUndo, [s]);

      s := name.RedoDescription;

      if s = '' then
        mnuEditRedo.Caption := fRedo
      else
        mnuEditRedo.Caption := Format (rstRedo, [s])
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.MRUList1PopupMenuClick                             |
 |                                                                      |
 | Open a file from the MRU list popup.                                 |
 *----------------------------------------------------------------------*)
procedure TfmMain.MRUList1PopupMenuClick(Sender: TObject);
var
  item : TMenuItem;
  p : Integer;
begin
  if Sender is TMenuItem then
  begin
    item := TMenuItem (sender);
    p := Pos (' ', Item.Caption);
    if p > 0 then
      OpenFile (Copy (Item.Caption, p + 1, MaxInt))
  end
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.OpenFile                                           |
 |                                                                      |
 | Open the specified module or .res file                               |
 *----------------------------------------------------------------------*)
procedure TfmMain.OpenFile(const fileName: string);
var
  ext : string;
begin
  if SaveChanges then
  begin
    FreeAndNil (fResourceModule);
    ClearUndoDetails;

    ext := UpperCase (ExtractFileExt (fileName));
    if (ext = '.RES') or (ext = '.DCR') then
      fResourceModule := TResModule.Create
    else
      if (ext = '.RC') then
      begin
        fResourceModule := TRCModule.Create;
        TRCModule (fResourceModule).IncludePath := gProperties.IncludePath;
      end
      else
        if (Win32Platform = VER_PLATFORM_WIN32_NT) and (gProperties.ParserType = 0) then
          fResourceModule := TNTModule.Create
        else
          fResourceModule := TPEResourceModule.Create;

    fResourceModule.LoadFromFile (fileName);
    Self.FileName := FileName;
    MRUList1.AddFile (fileName);
    UpdateDisplay (Nil);
  end
end;

function TfmMain.ResourceForm: TfmResource;
begin
  if Assigned (fmResourceObject) and (fmResourceObject is TfmResource) then
    Result := TfmResource (fmResourceObject)
  else
    Result := Nil
end;

function TfmMain.SaveChanges: Boolean;
var
  s : string;
begin
  if Assigned (fmResourceObject) then
    fmResourceObject.TidyUp;
  if FileName = '' then
    s := rstUntitled
  else
    s := ExtractFileName (FileName);
  if ISDirty then
    case MessageBox (Handle, PChar (Format (rstChanges, [s])), PChar (Application.Title), MB_YESNOCANCEL or MB_ICONQUESTION) of
      ID_YES : Result := SaveFile;
      ID_CANCEL : Result := False;
      else
        Result := True
    end
  else
    Result := True
end;

function TfmMain.SaveFile: boolean;
begin
  if fFileName = '' then
    Result := SaveFileAs
  else
  begin
    Application.ProcessMessages;
    fResourceModule.SaveToFile (fFileName);
    Result := True;
  end
end;

function TfmMain.SaveFileAs: Boolean;
var
  s, fName : string;
  newModule : TResModule;
  p : Integer;
  res : TResourceDetails;
  tp, nm : string;
  lg : Integer;
begin
  Result := False;
  Application.ProcessMessages;  // Ensures that toolbar button doesn't temporarily disappear
  if Assigned (fResourceModule) then
  begin
    fName := FileName;
    if fResourceModule is TRCModule then
    begin
      p := Length (fName);
      while p > 0 do
        if fName [p] = '.' then
          break
        else
          Dec (p);

      if p = 0 then
        fName := fName + '.res'
      else
        fName := Copy (s, 1, p - 1) + '.res';
    end;

    SaveDialog.FileName := fName;

    s := '';
    if fName <> '' then
    begin
      s := ExtractFileExt (fName);
      if Length (s) > 0 then
        s := Copy (s, 2, MaxInt);
    end
    else
      if fResourceModule is TResModule then
        s := 'RES';

    SaveDialog.DefaultExt := s;
    if (fResourceModule is TResModule) or (fResourceModule is TRCModule)  then
      SaveDialog.Filter := rstRESFilter
    else
      SaveDialog.Filter := rstModuleFilter + '|' + rstRESFilter;

    if SaveDialog.Execute then
    begin
      s := UpperCase (ExtractFileExt (SaveDialog.FileName));

      if ((fResourceModule is TPEModule) or (fResourceModule is TRCModule)) and ((s = '.DCR') or (s = '.RES')) then
      begin
        res := SelectedResourceDetails;
        if res <> Nil then
        begin
          nm := res.ResourceName;
          tp := res.ResourceType;
          lg := res.ResourceLanguage;
        end
        else
        begin
          nm := '';
          lg := 0;
          tp := ''
        end;
        newModule := TResModule.Create;
        newModule.Assign (fResourceModule);
        FreeAndNil (fResourceModule);
        ClearUndoDetails;
        fResourceModule := newModule;
        if nm <> '' then
          res := newModule.FindResource(tp, nm, lg)
        else
          res := Nil;
        UpdateDisplay (res);
      end;
      fResourceModule.SaveToFile (SaveDialog.FileName);
      FileName := SaveDialog.FileName;
      MRUList1.AddFile (fileName);
      Result := True;
    end
  end
end;

function TfmMain.SelectedResourceDetails: TResourceDetails;
begin
  result := GetNodeResourceDetails (vstResources.FocusedNode);
end;

(*----------------------------------------------------------------------*
 | TfmMain.SetCaption                                                   |
 |                                                                      |
 | Set the caption to display the loaded file.                          |
 *----------------------------------------------------------------------*)
procedure TfmMain.SetCaption;
var
  st : string;
begin
  if FileName = '' then
    st := Application.Title
  else
  begin
    st := ExtractFileName (FileName + ' - ' + Application.Title);
    if IsDirty then
      st := '*' + st
  end;

  Caption := st
end;

(*----------------------------------------------------------------------*
 | TfmMain.SetFileName                                                  |
 |                                                                      |
 | Set the file name.                                                   |
 *----------------------------------------------------------------------*)
procedure TfmMain.SetFileName(const Value: string);
begin
  if FileName <> Value then
  begin
    fFileName := Value;
    SetCaption
  end
end;

(*----------------------------------------------------------------------*
 | TfmMain.SetFormMenuButton                                            |
 |                                                                      |
 | Turn on or off the '...object' menu button.  This is set to the      |
 | form's menu item.                                                    |
 *----------------------------------------------------------------------*)
procedure TfmMain.SetFormMenuButton(item: TMenuItem);
begin
  if Assigned (item) then
  begin
    btnResourceObject.Caption := item.Caption;
    btnResourceObject.MenuItem := item;
    btnResourceObject.Visible := True;
  end
  else
  begin
    btnResourceObject.Visible := False;
    btnResourceObject.MenuItem := Nil;
    btnResourceObject.Caption := '...object'
  end
end;


procedure TfmMain.SetPersistDirectoryName(section: string;
  const Value: string);
var
  reg : TRegistry;
begin
  reg := TRegistry.Create (KEY_READ or KEY_WRITE);
  try
    if reg.OpenKey ('\' + PersistentPosition1.ApplicationKey + '\Directories', True) then
      reg.WriteString (section, Value)
  finally
    reg.Free
  end
end;

type
  TObjectFormRec = record
    details : TClass;
    form : TResourceObjectFormClass;
  end;

const
  NoObjectDetails = 14;
  NoHeaderForms = 3;

var
  ObjectForms : array [0..NoObjectDetails - 1] of TObjectFormRec = (
    (details : TIconResourceDetails;                    form : TfmIconGraphicsResource),
    (details : TCursorResourceDetails;                  form : TfmCursorGraphicsResource),
    (details : TGraphicsResourceDetails;                form : TfmGraphicsResource),
    (details : TTextResourceDetails;                    form : TfmTextResource),
    (details : TIconCursorGroupResourceDetails;         form : TfmGroupResource),
    (details : TVersionInfoResourceDetails;             form : TfmVersionResource),
    (details : TMenuResourceDetails;                    form : TfmMenuResource),
    (details : TDialogResourceDetails;                  Form : TfmDialogResource),
    (details : TRCDataDescriptionResourceDetails;       Form : TfmRCDataDescriptionResource),
    (details : TRCDataPackagesResourceDetails;          Form : TfmPackagesResource),
    (details : TRCDataFormResourceDetails;              Form : TfmRCDataFormResource),
    (details : TXPManifestResourceDetails;              Form : TfmXPManifestResource),
    (details : TAcceleratorResourceDetails;             Form : TfmAcceleratorResource),
    (details : TResourceDetails;                        form : TfmRawResource)  // Must be last entry!
  );

(*----------------------------------------------------------------------*
 | procedure TfmMain.SwitchView (details : TObject)                     |
 |                                                                      |
 | 'details' can be:                                                    |
 |                                                                      |
 |   *  A TResourceDetails object                                       |
 |   *  Nil                                                             |
 *----------------------------------------------------------------------*)
procedure TfmMain.SwitchView(details: TObject);

var
  i : Integer;
  formClass : TResourceObjectFormClass;

begin { SwitchView }
  formClass := Nil;
  if Integer (details) > 32 then        // It's a genuine TResourceDetails or TImageSection
  begin
    for i := 0 to NoObjectDetails - 1 do
      if details is ObjectForms [i].details then
      begin
        formClass := ObjectForms [i].form;
        break
      end
  end;

  // formClass is now a valid form class - or Nil

  if not Assigned (fmResourceObject) or not Assigned (formClass) or not (fmResourceObject.ClassType = formClass) then
  begin

    if Assigned (fmResourceObject) then  // Get rid of the old resource form
    begin
      SetFormMenuButton (Nil);
      FreeAndNil (fmResourceObject);
    end;

    if Assigned (formClass) then         // Create the new resource form
    begin
      fmResourceObject := formClass.Create (Nil);
      fmResourceObject.Parent := pnlResource;
      fmResourceObject.TabStop := True;
      fmResourceObject.ResourceModule := fResourceModule;
      SetFormMenuButton (fmResourceObject.Menu);
      fmResourceObject.Show;
      fmResourceObject.Font := Self.Font;
      fmResourceObject.Obj := details;
    end
  end
  else  // Form class is valid, and hasn't changed. Update the form.

    if Assigned (fmResourceObject) then
      fmResourceObject.obj := details;
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.UpdateActions                                      |
 |                                                                      |
 | Update the action list, depending on the state.                      |
 *----------------------------------------------------------------------*)
procedure TfmMain.UpdateActions;
var
  res : TResourceDetails;
  elem : TResExamElement;
  resForm : TfmResource;
begin
  GetNodeElement (vstResources.FocusedNode, elem);
  if (elem is TResExamResource) then
    res := TResExamResource (elem).ResourceDetails
  else
    res := Nil;

  actResourceDeleteResource.Enabled := res <> Nil;
  resForm := ResourceForm;
  if Assigned (resForm) then
  begin
    actEditUndo.Enabled := resForm.CanUndo;
    actEditRedo.Enabled := resForm.CanRedo;

    actEditCut.Enabled := resForm.CanCut;
    actEditCopy.Enabled := resForm.CanCopy;
    actEditPaste.Enabled := resForm.CanPaste;
    actEditSelectAll.Enabled := resForm.CanSelectAll;
    actEditDelete.Enabled := resForm.CanDelete;

    actResourceExportResource.Enabled := Assigned (res);
    actResourceClone.Enabled := Assigned (res);
    actResourceProperties.Enabled := Assigned (res) and not (res is TIconCursorResourceDetails);
  end
  else
  begin
    if elem is TResExamName then
    begin
      actEditUndo.Enabled := TResExamName (elem).CanUndo;
      actEditRedo.Enabled := TResExamName (elem).CanRedo
    end
    else
    begin
      actEditUndo.Enabled := False;
      actEditRedo.Enabled := False;
    end;

    actEditCut.Enabled := False;
    actEditCopy.Enabled := False;
    actEditPaste.Enabled := False;
    actEditSelectAll.Enabled := False;
    actResourceExportResource.Enabled := False;
    actResourceProperties.Enabled := False;
    actResourceClone.Enabled := False;
    actEditDelete.Enabled := False;
  end;

  if fResourceModule is TRCModule then
    actFileSave.Enabled := False
  else
    actFileSave.Enabled := IsDirty;

  if fWasDirty <> IsDirty then
  begin
    SetCaption;
    fWasDirty := IsDirty
  end;
(*
  if Assigned (ActiveControl) then
  sbMain.Panels [0].Text := ActiveControl.Name; *)
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.UpdateDisplay                                      |
 |                                                                      |
 | Update the display by building the tree view.                        |
 *----------------------------------------------------------------------*)
procedure TfmMain.UpdateDisplay (selectDetails : TResourceDetails);
var
  resSection : TResExamSection;

begin { UpdateDisplay }
  SwitchView (Nil);

  if not Assigned (fResourceModule) then Exit;

  fExaminer.SetResourceModule(fResourceModule, false);
  resSection := fExaminer.ResourceSection;

  vstResources.BeginUpdate;
  try
    if Assigned (resSection) then
      vstResources.RootNodeCount := resSection.Count
    else
      vstResources.RootNodeCount := 0;
    vstResources.ReinitNode(nil, true);
  finally
    vstResources.EndUpdate
  end;

  if Assigned (SelectDetails) then
  begin
    vstResources.SelectAndFocusNode (GetResourceDetailsNode (SelectDetails));
    SwitchView (SelectDetails)
  end
  else
  begin
    SwitchView (Nil);
    vstResources.FullCollapse;
    vstResources.SelectAndFocusNode(vstResources.GetFirst);
  end
end;

procedure TfmMain.WmAddImageResource(var msg: TMessage);
var
  o : TResourceDetails;
  grp : TIconCursorGroupResourceDetails;
  res : TIconCursorResourceDetails;
  p : PVirtualNode;

begin
  p := vstResources.FocusedNode;
  o := GetNodeResourceDetails (p);

  if Assigned (o) and (o is TIconCursorResourceDetails) then
  begin
    res := TIconCursorResourceDetails (o);
    p := p.Parent
  end
  else
    res := Nil;

  if (p <> Nil) and (res <> Nil) then
  begin
    o := GetNodeResourceDetails (p);

    if o is TIconCursorGroupResourceDetails then
    begin
      grp := TIconCursorGroupResourceDetails (o);
      res := TIconCursorResourceDetails (CloneResource (res, '', -1));
      grp.AddToGroup (res);
      fResourceModule.SortResources;
      UpdateDisplay (res);
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TfmMain.WmInitialize                                       |
 |                                                                      |
 | WM_INITIALIZE handler.  Start with a blank .res file.                |
 *----------------------------------------------------------------------*)
procedure TfmMain.WmInitialize(var msg: TMessage);
begin
  fUndo := actEditUndo.Caption;
  fRedo := actEditRedo.Caption;
  fExaminer := TResourceExaminer.Create(nil);

  if ParamCount = 0 then
    actFileNew.Execute
  else
    OpenFile (ParamStr (1))
end;

(*----------------------------------------------------------------------*
 | TfmMain.WmPropertiesChanged                                          |
 |                                                                      |
 | Handle our 'WM_PROPERTIESCHANGED' message.  Apply persistent         |
 | properties                                                           |
 *----------------------------------------------------------------------*)
procedure TfmMain.WmPropertiesChanged(var msg: TMessage);
begin
  tbMain.Visible := gProperties.ShowToolbar;
  actViewToolbar.Checked := tbMain.Visible;

  sbMain.Visible := gProperties.ShowStatusBar;
  actViewStatusbar.Checked := sbMain.Visible;
  UseInternationalFont (vstResources.Font);

  if Assigned (fmResourceObject) then
    fmResourceObject.UpdateFonts
end;

procedure TfmMain.WmStatusBar(var Msg: TMessage);
begin
  if Msg.lParam <> 0 then
    sbMain.Panels [1].Text := PChar (Msg.lParam) + '     ';

  if Msg.wParam <> 0 then
    sbMain.Panels [0].Text := PChar (Msg.wParam);
end;

procedure TfmMain.actResourceCloneExecute(Sender: TObject);
var
  res : TResourceDetails;
  dlg : TdlgCloneResource;
begin
  res := SelectedResourceDetails;
  if Assigned (res) then
  begin
    dlg := TdlgCloneResource.Create(nil);
    try
      dlg.ResourceDetails := res;
      if dlg.ShowModal = mrOk then
      begin
        if dlg.rbByLanguage.Checked then
          res := CloneResource (res, '', dlg.Language)
        else
          res := CloneResource (res, dlg.ntedName.Text, -1);
        fResourceModule.SortResources;
        UpdateDisplay (res)
      end
    finally
      dlg.Free
    end
  end
end;

procedure TfmMain.actResourceGrabExecute(Sender: TObject);
begin
//
end;

procedure TfmMain.vstResourcesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  SwitchView (GetNodeResourceDetails (Node));
end;

procedure TfmMain.vstResourcesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  elem : TResExamElement;
begin
  if GetNodeElement (Node, elem) then
    CellText := elem.DisplayName
end;

procedure TfmMain.vstResourcesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  elem : TResExamElement;
begin
  if GetNodeElement (Node, elem) then
    ChildCount := elem.Count
  else
    ChildCount := 0
end;

procedure TfmMain.vstResourcesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  elem, pelem : TResExamElement;
begin
  if not Assigned (ParentNode) then
    pelem := fExaminer.ResourceSection
  else
    GetNodeElement (ParentNode, pelem);

  if Assigned (pelem) then
  begin
    elem := pelem.Element [Node.Index];
    vstResources.NodeObject [Node] := elem;
    if elem.Count > 0 then
      Include (InitialStates, ivsHasChildren)
  end
end;

procedure TfmMain.vstResourcesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  details : TResourceDetails;
begin
  if Kind = ikOverlay then
    Exit;
  
  details := GetNodeResourceDetails (Node);
  if Assigned (details) then
    ImageIndex := GetTypeImage (details.ResourceType)
  else
    if vsExpanded in Node^.States then
      ImageIndex := imgOpenFolder
    else
      ImageIndex := imgClosedFolder
end;

procedure TfmMain.vstResourcesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := EditAllowed (Node);
end;

function TfmMain.EditAllowed(node: PVirtualNode): boolean;
var
  elem : TResExamElement;
  tp : TResExamType;
begin
  result := False;
  if GetNodeElement (node, elem) then
    if elem is TResExamName then
    begin
      result := True;
      node := node^.Parent;
      if GetNodeElement (node, elem) and (elem is TResExamType) then
      begin
        tp := TResExamType (elem);
        if StrToIntDef (tp.Name, -1) = Integer (RT_STRING) then
          result := False
      end
    end
    else
      if (elem is TResExamType) then
      begin
        tp := TResExamType (elem);
        result:= tp.Name = tp.DisplayName
      end
end;

procedure TfmMain.vstResourcesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  elem : TResExamElement;

begin
  if GetNodeElement (node, elem) then
    if (elem is TResExamName) then
      TResExamName (elem).Name := NewText
    else
      if (elem is TResExamType) then
        TResExamType (elem).Name := NewText;

  vstResources.InvalidateNode(node)
end;

procedure TfmMain.UpdateResourceNodes(res: TResourceDetails;
  node : PVirtualNode);
var
  nm, tp : WideString;
  elem : TResExamElement;
begin
  if Assigned (res) and Assigned (node) then
  begin
    if res is TStringResourceDetails then
      nm := ResIdToStringsId (res.ResourceName)
    else
      nm := res.ResourceName;

    tp := res.ResourceType;

    while Assigned (node) do
    begin
      if GetNodeElement (node, elem) then
        if elem is TResExamName then
          TResExamName (elem).Name := nm
        else
          if elem is TResExamType then
            TResExamType (elem).Name := tp;
      vstResources.InvalidateNode (node);
      node := node.Parent;
      if node = vstResources.RootNode then
        break
    end
  end
end;



end.
