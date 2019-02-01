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
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetRedoDescription: string;
    function GetUndoDescription: string;
  protected
    procedure SetObject(const Value: TObject); override;
    function GetImportExportType: TImportExportType; virtual;

    function GetCanCopy: Boolean; virtual;
    function GetCanCut: Boolean; virtual;
    function GetCanPaste: Boolean; virtual;
    function GetCanSelectAll: Boolean; virtual;
    function GetCanDelete: Boolean; virtual;

  public
    procedure AddUndoEntry(const undoDetails: string);
    procedure Undo;
    procedure Redo;
    procedure Cut; virtual;
    procedure Copy; virtual;
    procedure Paste; virtual;
    procedure SelectAll; virtual;
    procedure EditDelete; virtual;

    property ResourceDetails: TResourceDetails read GetResourceDetails;

    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property UndoDescription: string read GetUndoDescription;
    property RedoDescription: string read GetRedoDescription;
    property ImportExportType: TImportExportType read GetImportExportType;

    property CanCopy: Boolean read GetCanCopy;
    property CanCut: Boolean read GetCanCut;
    property CanPaste: Boolean read GetCanPaste;
    property CanSelectAll: Boolean read GetCanSelectAll;
    property CanDelete: Boolean read GetCanDelete;
  end;

procedure ClearUndoDetails;
procedure SetInternationalFont(const name: TFontName; height: Integer);
procedure UseInternationalFont(font: TFont);

implementation

uses
  Contnrs, unitCREdProperties;

type
//=======================================================================
// TUndoEntry class

  TUndoEntry = class
    FDetails: string;
    FLanguage: Integer;
    FName: string;
    FData: TMemoryStream;

    constructor Create(const Details: string; res: TResourceDetails);
    destructor Destroy; override;
  end;

//=======================================================================
// TUndoDetails class

  TUndoDetails = class
  private
    FUndoStack: TObjectStack;
    FRedoStack: TObjectStack;
    FResourceDetails: TResourceDetails;

    constructor Create(ResourceDetails: TResourceDetails);
    destructor Destroy; override;
    procedure AddUndoEntry(const Details: string);
    procedure Undo;
    procedure Redo;
    procedure ClearRedoStack;
  end;


var
  gUndoDetails: TObjectList;
  gInternationalFont: TFont = nil;

{$R *.DFM}

//=======================================================================
// Global methods

(*----------------------------------------------------------------------*
 | procedue ClearUndoDetails                                            |
 |                                                                      |
 | Clear all undo Details.                                              |
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

procedure UseInternationalFont(font: TFont);
begin
  if not Assigned (gInternationalFont) then
    CreateInternationalFont;

  if Assigned (gInternationalFont) then
    font.Assign(gInternationalFont)
end;

procedure SetInternationalFont(const name: TFontName; height: Integer);
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
procedure TfmResource.AddUndoEntry(const undoDetails: string);
var
  Details: TUndoDetails;
begin
  if ResourceDetails.Tag = 0 then       // Any existing undo info?
  begin
                                         // No.  Create it
    Details := TUndoDetails.Create(ResourceDetails);
    ResourceDetails.Dirty := True;
    ResourceDetails.Tag := Integer (Details);
    gUndoDetails.Add (Details)
  end
  else
    Details := TUndoDetails (ResourceDetails.Tag);

  Details.ClearRedoStack;               // Clear the redo stack to prevent memory run-away

  Details.AddUndoEntry(undoDetails);
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
 | Details has items in it's redo list                                  |
 *----------------------------------------------------------------------*)
function TfmResource.GetCanRedo: Boolean;
var
  Details: TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    Details := TUndoDetails (ResourceDetails.Tag);
    Result := Details.FRedoStack.Count > 0
  end
  else
    Result := False
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetCanUndo                                               |
 |                                                                      |
 | 'Get' handler for CanUndo property.  Returns true if the resource    |
 | Details has items in it's undo list                                  |
 *----------------------------------------------------------------------*)
function TfmResource.GetCanSelectAll: Boolean;
begin
  Result := False
end;

function TfmResource.GetCanUndo: Boolean;
var
  Details: TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    Details := TUndoDetails (ResourceDetails.Tag);
    Result := Details.FUndoStack.Count > 0
  end
  else
    Result := False
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
  Details: TUndoDetails;
  entry: TUndoEntry;
begin
  Result := '';
  if ResourceDetails.Tag <> 0 then
  begin
    Details := TUndoDetails (ResourceDetails.Tag);
    if Details.FRedoStack.Count > 0 then
    begin
      entry := TUndoEntry(Details.FRedoStack.Peek);

      if Assigned (entry) then
        Result := entry.FDetails
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
  Result := Obj as TResourceDetails
end;

(*----------------------------------------------------------------------*
 | TfmResource.GetUndoDescription                                       |
 |                                                                      |
 | 'Get' handler for UndoDescription property.  Return the description  |
 | for the entry at the top of the redo stack.                          |
 *----------------------------------------------------------------------*)
function TfmResource.GetUndoDescription: string;
var
  Details: TUndoDetails;
  entry: TUndoEntry;
begin
  Result := '';
  if ResourceDetails.Tag <> 0 then
  begin
    Details := TUndoDetails (ResourceDetails.Tag);
    if Details.FUndoStack.Count > 0 then
    begin
      entry := TUndoEntry(Details.FUndoStack.Peek);

      if Assigned (entry) then
        Result := entry.FDetails
    end
  end
end;

procedure TfmResource.Paste;
begin

end;

(*----------------------------------------------------------------------*
 | TfmResource.Redo                                                     |
 |                                                                      |
 | 'Redo' the last change for the resource Details.                     |
 *----------------------------------------------------------------------*)
procedure TfmResource.Redo;
var
  Details: TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    Details := TUndoDetails (ResourceDetails.Tag);
    Details.Redo;
  end
end;

(*----------------------------------------------------------------------*
 | TfmResource.SetObject                                                |
 |                                                                      |
 | 'Set' property handler overridden to initialize the Details undo     |
 | stack.  Note that the redo stack is cleared here to prevent          |
 | unlimited memory usage.                                              |
 *----------------------------------------------------------------------*)
procedure TfmResource.SelectAll;
begin
// Stub
end;

procedure TfmResource.SetObject(const Value: TObject);
var
  Details: TUndoDetails;
begin
  if Assigned (Obj) and (Value <> Obj) then
    if ResourceDetails.Tag <> 0 then
    begin
      Details := TUndoDetails (ResourceDetails.Tag);  // ie.   The *old* object's
                                                      // Details

      Details.ClearRedoStack;

      if Details.FUndoStack.Count = 0 then
      begin
        ResourceDetails.Tag := 0;
        gUndoDetails.Remove(Details)
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
  Details: TUndoDetails;
begin
  if ResourceDetails.Tag <> 0 then
  begin
    Details := TUndoDetails (ResourceDetails.Tag);
    Details.Undo;
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
procedure TUndoDetails.AddUndoEntry(const Details: string);
begin
  FUndoStack.Push(TUndoEntry.Create(Details, FResourceDetails));
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.ClearRedoStack                                          |
 |                                                                      |
 | Clear the Redo list                                                  |
 *----------------------------------------------------------------------*)
procedure TUndoDetails.ClearRedoStack;
begin
  while FRedoStack.Count > 0 do
    FRedoStack.Pop.Free;
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.Create                                                  |
 |                                                                      |
 | Constructor for TUndoDetails.  Create the undo and redo stacks       |
 *----------------------------------------------------------------------*)
constructor TUndoDetails.Create(ResourceDetails: TResourceDetails);
begin
  FResourceDetails := ResourceDetails;
  FUndoStack := TObjectStack.Create;
  FRedoStack := TObjectStack.Create;
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.Destroy                                                 |
 |                                                                      |
 *----------------------------------------------------------------------*)
destructor TUndoDetails.Destroy;
begin
  ClearRedoStack;

  while FUndoStack.Count > 0 do // Clear the undo stack
    FUndoStack.Pop.Free;

  FUndoStack.Free;              // nb.  Object stacks don't own their Objects!
  FRedoStack.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TUndoDetails.Redo                                                    |
 |                                                                      |
 | Redo the changes at the top of the 'redo' stack                      |
 *----------------------------------------------------------------------*)
procedure TUndoDetails.Redo;
var
  entry: TUndoEntry;
begin
  if FRedoStack.Count > 0 then
  begin
    entry := TUndoEntry(FRedoStack.Pop);
    FUndoStack.Push(TUndoEntry.Create(entry.FDetails, FResourceDetails));
    FResourceDetails.ResourceName := entry.FName;
    FResourceDetails.ResourceLanguage := entry.FLanguage;
    FResourceDetails.ChangeData(entry.FData);
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
  entry: TUndoEntry;
begin
  if FUndoStack.Count > 0 then
  begin
    entry := TUndoEntry(FUndoStack.Pop);
    FRedoStack.Push(TUndoEntry.Create(entry.FDetails, FResourceDetails));
    FResourceDetails.ResourceName := entry.FName;
    FResourceDetails.ResourceLanguage := entry.FLanguage;
    FResourceDetails.ChangeData(entry.FData);
    entry.Free
  end
end;

//=======================================================================
// TUndoEntry methods

{ TUndoEntry }

constructor TUndoEntry.Create(const Details: string; res: TResourceDetails);
begin
  FData := TMemoryStream.Create;
  FData.CopyFrom (res.Data, 0);
  FLanguage := res.ResourceLanguage;
  FDetails := Details;
  FName := res.ResourceName
end;

destructor TUndoEntry.Destroy;
begin
  FData.Free;
  inherited;
end;

initialization
  gUndoDetails := TObjectList.Create;
finalization
  gUndoDetails.Free;
  gInternationalFont.Free;
end.
