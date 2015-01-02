(*======================================================================*
 | ResourceForm                                                         |
 |                                                                      |
 | Contains TfmResource base class for all resource editor forms.       |
 |                                                                      |
 | 1.  Provides derived forms with 'ResourceDetails' property           |
 | 2.  Handles unlimited undo/redo functionallity                       |
 |                                                                      |
 | * Gold code.                                                         |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 *======================================================================*)

unit ResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceObjectForm, unitResourceDetails;

type
  TImportExportType = (ixNone, ixFile, ixPicture);

//=======================================================================
// TfmResource class

  TfmResource = class(TfmResourceObject)
  private
    function GetResourceDetails: TResourceDetails;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
    function GetRedoDescription: string;
    function GetUndoDescription: string;
  protected
    procedure SetObject(const Value: TObject); override;
    function GetImportExportType: TImportExportType; virtual;

    function GetCanCopy: Boolean; virtual;
    function GetCanCut: Boolean; virtual;
    function GetCanPaste: Boolean; virtual;
    function GetCanSelectAll : boolean; virtual;
    function GetCanDelete: Boolean; virtual;

  public
    procedure AddUndoEntry (const undoDetails : string);
    procedure Undo;
    procedure Redo;
    procedure Cut; virtual;
    procedure Copy; virtual;
    procedure Paste; virtual;
    procedure SelectAll; virtual;
    procedure EditDelete; virtual;

    property ResourceDetails : TResourceDetails read GetResourceDetails;

    property CanUndo : boolean read GetCanUndo;
    property CanRedo : boolean read GetCanRedo;
    property UndoDescription : string read GetUndoDescription;
    property RedoDescription : string read GetRedoDescription;
    property ImportExportType : TImportExportType read GetImportExportType;

    property CanCopy : Boolean read GetCanCopy;
    property CanCut : Boolean read GetCanCut;
    property CanPaste : Boolean read GetCanPaste;
    property CanSelectAll : Boolean read GetCanSelectAll;
    property CanDelete : Boolean read GetCanDelete;
  end;

var
  fmResource: TfmResource;

procedure ClearUndoDetails;
procedure SetInternationalFont (const name : TFontName; height : Integer);
procedure UseInternationalFont (font : TFont);

implementation

uses ConTnrs, unitCREdProperties;

type
//=======================================================================
// TUndoEntry class

  TUndoEntry = class
    fDetails : string;
    fLanguage : Integer;
    fName : string;
    fData : TMemoryStream;

    constructor Create (const details : string; res : TResourceDetails);
    destructor Destroy; override;
  end;

//=======================================================================
// TUndoDetails class

  TUndoDetails = class
  private
    fUndoStack : TObjectStack;
    fRedoStack : TObjectStack;
    fResourceDetails : TResourceDetails;

    constructor Create (ResourceDetails : TResourceDetails);
    destructor Destroy; override;
    procedure AddUndoEntry (const details : string);
    procedure Undo;
    procedure Redo;
    procedure ClearRedoStack;
  end;


var
  gUndoDetails : TObjectList;
  gInternationalFont : TFont = Nil;

{$R *.DFM}

//=======================================================================
// Global methods

(*----------------------------------------------------------------------*
 | procedue ClearUndoDetails                                            |
 |                                                                      |
 | Clear all undo details.                                              |
 *----------------------------------------------------------------------*)
procedure ClearUndoDetails;
begin
  gUndoDetails.Clear;
end;

procedure CreateInternationalFont;
begin
  gInternationalFont := TFont.Create;
  gInternationalFont.Assign(Application.MainForm.Font);
  gInternationalFont.Name := gProperties.InternationalFontName;
  gInternationalFont.Height := gProperties.InternationalFontHeight;
end;

procedure UseInternationalFont (font : TFont);
begin
  if not Assigned (gInternationalFont) then
    CreateInternationalFont;

  if Assigned (gInternationalFont) then
    font.Assign(gInternationalFont)
end;

procedure SetInternationalFont (const name : TFontName; height : Integer);
begin
  FreeAndNil (gInternationalFont);
  gProperties.InternationalFontName := name;
  gProperties.InternationalFontHeight := height;
end;

{ TfmResource }

//=======================================================================
// TfmResource methods

(*----------------------------------------------------------------------*
 | TfmResource.AddUndoEntry                                             |
 |                                                                      |
 *----------------------------------------------------------------------*)
procedure TfmResource.AddUndoEntry (const undoDetails: string);
var
  details : TUndoDetails;
begin
  if ResourceDetails.Tag = 0 then       // Any existing undo info?
  begin
                                         // No.  Create it
    details := TUndoDetails.Create (ResourceDetails);
    resourceDetails.Dirty := True;
    resourceDetails.Tag := Integer (details);
    gUndoDetails.Add (details)
  end
  else
    details := TUndoDetails (resourceDetails.Tag);

  details.ClearRedoStack;               // Clear the redo stack to prevent memory run-away

  details.AddUndoEntry (undoDetails);
end;

procedure TfmResource.Copy;
begin
// Stub
end;

procedure TfmResource.Cut;
begin
// Stub
end;

procedure TfmResource.EditDelete;
begin
// stub
end;

function TfmResource.GetCanCopy: Boolean;
begin
  Result := False
end;

function TfmResource.GetCanCut: Boolean;
begin
  Result := False
end;

function TfmResource.GetCanDelete: Boolean;
begin
  Result := False
end;

function TfmResource.GetCanPaste: Boolean;
begin
  Result := False
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetCanRedo                                               |
 |                                                                      |
 | 'Get' handler for CanRedo property.  Returns true if the resource    |
 | details has items in it's redo list                                  |
 *----------------------------------------------------------------------*)
function TfmResource.GetCanRedo: boolean;
var
  details : TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    details := TUndoDetails (resourceDetails.Tag);
    result := details.fRedoStack.Count > 0
  end
  else
    result := False
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetCanUndo                                               |
 |                                                                      |
 | 'Get' handler for CanUndo property.  Returns true if the resource    |
 | details has items in it's undo list                                  |
 *----------------------------------------------------------------------*)
function TfmResource.GetCanSelectAll: boolean;
begin
  Result := False
end;

function TfmResource.GetCanUndo: boolean;
var
  details : TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    details := TUndoDetails (resourceDetails.Tag);
    result := details.fUndoStack.Count > 0
  end
  else
    result := False
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetImportExportType                                      |
 |                                                                      |
 | 'Get' handler for ExportType property.  Determine whether the        |
 | resource form can export it's contents                               |
 *----------------------------------------------------------------------*)
function TfmResource.GetImportExportType: TImportExportType;
begin
  Result := ixNone
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetRedoDescription                                       |
 |                                                                      |
 | 'Get' handler for RedoDescription property.  Return the description  |
 | for the entry at the top of the redo stack.                          |
 *----------------------------------------------------------------------*)
function TfmResource.GetRedoDescription: string;
var
  details : TUndoDetails;
  entry : TUndoEntry;
begin
  result := '';
  if ResourceDetails.Tag <> 0 then
  begin
    details := TUndoDetails (resourceDetails.Tag);
    if details.fRedoStack.Count > 0 then
    begin
      entry := TUndoEntry (details.fRedoStack.Peek);

      if Assigned (entry) then
        result := entry.fDetails
    end
  end
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetResourceDetails                                       |
 |                                                                      |
 | 'Get' method for ResourceDetails property.  Check 'Obj' is really    |
 | TResourceDetails, and return it.                                     |
 *----------------------------------------------------------------------*)
function TfmResource.GetResourceDetails: TResourceDetails;
begin
  result := Obj as TResourceDetails
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetUndoDescription                                       |
 |                                                                      |
 | 'Get' handler for UndoDescription property.  Return the description  |
 | for the entry at the top of the redo stack.                          |
 *----------------------------------------------------------------------*)
function TfmResource.GetUndoDescription: string;
var
  details : TUndoDetails;
  entry : TUndoEntry;
begin
  result := '';
  if ResourceDetails.Tag <> 0 then
  begin
    details := TUndoDetails (resourceDetails.Tag);
    if details.fUndoStack.Count > 0 then
    begin
      entry := TUndoEntry (details.fUndoStack.Peek);

      if Assigned (entry) then
        result := entry.fDetails
    end
  end
end;

procedure TfmResource.Paste;
begin

end;

(*----------------------------------------------------------------------*
 | TfmResource.Redo                                                     |
 |                                                                      |
 | 'Redo' the last change for the resource details.                     |
 *----------------------------------------------------------------------*)
procedure TfmResource.Redo;
var
  details : TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    details := TUndoDetails (resourceDetails.Tag);
    details.Redo;
  end
end;

(*----------------------------------------------------------------------*
 | TfmResource.SetObject                                                |
 |                                                                      |
 | 'Set' property handler overridden to initialize the details undo     |
 | stack.  Note that the redo stack is cleared here to prevent          |
 | unlimited memory usage.                                              |
 *----------------------------------------------------------------------*)
procedure TfmResource.SelectAll;
begin
// Stub
end;

procedure TfmResource.SetObject(const Value: TObject);
var
  details : TUndoDetails;
begin
  if Assigned (Obj) and (Value <> Obj) then
    if ResourceDetails.Tag <> 0 then
    begin
      details := TUndoDetails (resourceDetails.Tag);  // ie.   The *old* object's
                                                      // details

      details.ClearRedoStack;

      if details.fUndoStack.Count = 0 then
      begin
        ResourceDetails.Tag := 0;
        gUndoDetails.Remove (details)
      end
    end;
  inherited;  // Call inherited to set the 'obj' property
end;

(*----------------------------------------------------------------------*
 | TfmResource.Undo                                                     |
 |                                                                      |
 | 'Undo' the last change.                                              |
 *----------------------------------------------------------------------*)
procedure TfmResource.Undo;
var
  details : TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    details := TUndoDetails (resourceDetails.Tag);
    details.Undo;
  end
end;

//=======================================================================
// TUndoDetails methods

{ TUndoDetails }

(*----------------------------------------------------------------------*
 | TUndoDetails.AddUndoEntry                                            |
 |                                                                      |
 | Add an entry to the undo list                                        |
 *----------------------------------------------------------------------*)
procedure TUndoDetails.AddUndoEntry(const details: string);
begin
  fUndoStack.Push (TUndoEntry.Create (details, fResourceDetails));
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.ClearRedoStack                                          |
 |                                                                      |
 | Clear the Redo list                                                  |
 *----------------------------------------------------------------------*)
procedure TUndoDetails.ClearRedoStack;
begin
  while fRedoStack.Count > 0 do
    fRedoStack.Pop.Free;
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.Create                                                  |
 |                                                                      |
 | Constructor for TUndoDetails.  Create the undo and redo stacks       |
 *----------------------------------------------------------------------*)
constructor TUndoDetails.Create (ResourceDetails : TResourceDetails);
begin
  fResourceDetails := ResourceDetails;
  fUndoStack := TObjectStack.Create;
  fRedoStack := TObjectStack.Create;
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.Destroy                                                 |
 |                                                                      |
 *----------------------------------------------------------------------*)
destructor TUndoDetails.Destroy;
begin
  ClearRedoStack;

  while fUndoStack.Count > 0 do // Clear the undo stack
    fUndoStack.Pop.Free;

  fUndoStack.Free;              // nb.  Object stacks don't own their Objects!
  fRedoStack.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.Redo                                                    |
 |                                                                      |
 | Redo the changes at the top of the 'redo' stack                      |
 *----------------------------------------------------------------------*)
procedure TUndoDetails.Redo;
var
  entry : TUndoEntry;
begin
  if fRedoStack.Count > 0 then
  begin
    entry := TUndoEntry (fRedoStack.Pop);
    fUndoStack.Push (TUndoEntry.Create (entry.fDetails, fResourceDetails));
    fResourceDetails.ResourceName := entry.fName;
    fResourceDetails.ResourceLanguage := entry.fLanguage;
    fResourceDetails.ChangeData (entry.fData);
    entry.Free
  end
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.Undo                                                    |
 |                                                                      |
 | Undo the changes at the top of the undo stack                        |
 *----------------------------------------------------------------------*)
procedure TUndoDetails.Undo;
var
  entry : TUndoEntry;
begin
  if fUndoStack.Count > 0 then
  begin
    entry := TUndoEntry (fUndoStack.Pop);
    fRedoStack.Push (TUndoEntry.Create (entry.fDetails, fResourceDetails));
    fResourceDetails.ResourceName := entry.fName;
    fResourceDetails.ResourceLanguage := entry.fLanguage;
    fResourceDetails.ChangeData (entry.fData);
    entry.Free
  end
end;

//=======================================================================
// TUndoEntry methods

{ TUndoEntry }

constructor TUndoEntry.Create(const details : string; res : TResourceDetails);
begin
  fData := TMemoryStream.Create;
  fData.CopyFrom (res.Data, 0);
  fLanguage := res.ResourceLanguage;
  fDetails := details;
  fName := res.ResourceName
end;

destructor TUndoEntry.Destroy;
begin
  fData.Free;
  inherited;
end;

initialization
  gUndoDetails := TObjectList.Create;
finalization
  gUndoDetails.Free;
  gInternationalFont.Free;
end.
