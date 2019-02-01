unit ExVirtualStringTree;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, VirtualTrees, Forms;

type
  TVTIteratorProc = procedure(p: PVirtualNode; param: Integer; var continue: Boolean) of object;

  TCustomExVirtualStringTree = class(TCustomVirtualStringTree)
  private
    FProportionalColumnSizes: Boolean;

    FColumnPCs: array of Integer;
    FResizeCount: Integer;
    procedure SetProportionalColumnSizes(const Value: Boolean);
    procedure SaveColumnPCs;
    function GetNodeObject(Node: PVirtualNode): TObject;
    procedure SetNodeObject(Node: PVirtualNode; const Value: TObject);
    procedure CheckObject(p: PVirtualNode; param: Integer; var continue: Boolean);
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure DoColumnResize(Column: TColumnIndex); override;
    procedure DoHeaderMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure LockControl;
    procedure UnlockControl;

    property ProportionalColumnSizes: Boolean read FProportionalColumnSizes write SetProportionalColumnSizes;
    function GetOptionsClass: TTreeOptionsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectAndFocusNode(Node: PVirtualNode; ClearSel: Boolean = True);
    function FindNodeObject(obj: TObject; root: PVirtualNode = Nil): PVirtualNode;
    function ForEach(proc: TVTIteratorProc; param: Integer; root: PVirtualNode = Nil): PVirtualNode;

    property NodeObject[Node: PVirtualNode]: TObject read GetNodeObject write SetNodeObject;
  end;

  TExVirtualStringTree = class (TCustomExVirtualStringTree)
  private
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
  public
    property Canvas;
  published
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property Ctl3D;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DefaultText;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintAnimation;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ProportionalColumnSizes;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
    property Visible;
    property WantTabs;

    property OnAdvancedHeaderDraw;
    property OnAfterCellPaint;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeCellPaint;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnCompareNodes;
    {$ifdef COMPILER_5_UP}
    property OnContextPopup;
    {$endif COMPILER_5_UP}
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
  end;

implementation

type
  PObject = ^TObject;

{ TCustomExVirtualStringTree }

constructor TCustomExVirtualStringTree.Create(AOwner: TComponent);
begin
  inherited;
  NodeDataSize := 4
end;

procedure TCustomExVirtualStringTree.DoColumnResize(Column: TColumnIndex);
begin
  Inc(FResizeCount);
  try
    inherited;
  finally
    Dec(FResizeCount)
  end
end;

procedure TCustomExVirtualStringTree.DoHeaderMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SaveColumnPCs
end;

procedure TCustomExVirtualStringTree.SelectAndFocusNode(Node: PVirtualNode; ClearSel: Boolean = True);
begin
  if ClearSel then
    ClearSelection;
  if Assigned(Node) then
  begin
    Selected [Node] := True;
    FocusedNode := Node
  end
end;

function TCustomExVirtualStringTree.GetNodeObject(
  Node: PVirtualNode): TObject;
var
  obj: PObject;
begin
  obj := GetNodeData(Node);
  if Assigned(obj) and Assigned(obj^) then
    Result := obj^
  else
    Result := nil
end;

function TCustomExVirtualStringTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

procedure TCustomExVirtualStringTree.Loaded;
begin
  inherited;

  if ProportionalColumnSizes then
  begin
    FProportionalColumnSizes := False;
    ProportionalColumnSizes := True
  end
end;

procedure TCustomExVirtualStringTree.LockControl;
begin
  SendMessage(Handle, WM_SETREDRAW, 0, 0);
end;

procedure TCustomExVirtualStringTree.Resize;
var
  i, n, w, ww, bw: Integer;
  hasVertScrollBar: Boolean;
begin
  inherited;
  if ProportionalColumnSizes and (FResizeCount = 0) and not (csDesigning in ComponentState) then
  begin
    Inc(FResizeCount);
    LockControl;
    try
      with Header do
      begin
        bw := BorderWidth;
        if BorderStyle <> bsNone then
          Inc(bw, 2);
        ww := Width - 2 * bw;
        hasVertScrollBar := (GetWindowLong (handle, GWL_STYLE) and WS_VSCROLL) <> 0;

        if hasVertScrollBar then
          ww := ww - GetSystemMetrics (SM_CXVSCROLL);

        w := 0;
        for i := 0 to Columns.Count - 2 do
        begin
          n := FColumnPCs[i] * ww div 100;
          Columns[i].Width := n;
          Inc(w, n)
        end;

//        if hasVertScrollBar then
//          ww := ww + GetSystemMetrics (SM_CXVSCROLL);
        Columns[Columns.Count - 1].Width := ww - w
      end
    finally
      UnlockControl;
      Dec(FResizeCount);
    end
  end
end;

procedure TCustomExVirtualStringTree.SaveColumnPCs;
var
  i, bw, ww: Integer;
begin
  if not ProportionalColumnSizes then
    Exit;
  bw := BorderWidth;
  if BorderStyle <> bsNone then
    Inc(bw, 2);
  ww := Width - 2 * bw;

  with Header do for i := 0 to Columns.Count - 1 do
    FColumnPCs[i] := Columns[i].Width * 100 div ww
end;

procedure TCustomExVirtualStringTree.SetNodeObject(Node: PVirtualNode;
  const Value: TObject);
var
  obj: PObject;
begin
  obj := GetNodeData(Node);
  if Assigned(obj) then
    obj^ := Value
end;

procedure TCustomExVirtualStringTree.SetProportionalColumnSizes(
  const Value: Boolean);
var
  w, i: Integer;
begin
  if FProportionalColumnSizes <> Value then
  begin
    FProportionalColumnSizes := Value;
    if (csLoading in ComponentState) or (csDesigning in ComponentState) then
      Exit;

    Inc(FResizeCount);
    try
      with Header do
      begin
        w := 0;
        for i := 0 to Columns.Count - 2 do
          w := w + Columns[i].Width;

        if ClientWidth - w > Columns[Columns.Count - 1].Width then
          Columns[Columns.Count - 1].Width := ClientWidth - w;
        SetLength(FColumnPCs, Header.Columns.Count);

        SaveColumnPCs
      end
    finally
      Dec(FResizeCount)
    end
  end
end;

procedure TCustomExVirtualStringTree.UnlockControl;
begin
  SendMessage(Handle, WM_SETREDRAW, 1, 0);
  RedrawWindow(Handle,nil,0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN)
end;

function TCustomExVirtualStringTree.FindNodeObject(obj: TObject; root: PVirtualNode): PVirtualNode;
begin
  Result := ForEach(CheckObject, Integer (obj), root)
end;

function TCustomExVirtualStringTree.ForEach(proc: TVTIteratorProc; param: Integer;
  root: PVirtualNode): PVirtualNode;
var
  cont: Boolean;
  Node: PVirtualNode;

  procedure Iterate(n: PVirtualNode);
  begin
    while n <> Nil do
    begin
      proc (n, param, cont);
      if not cont then
      begin
        Node := n;
        break
      end;

      Iterate(GetFirstChild (n));
      n := GetNextSibling(n)
    end
  end;

begin
  if root = Nil then root := RootNode;
  Node := nil;
  Iterate(GetFirstChild (root));
  Result := Node
end;

procedure TCustomExVirtualStringTree.CheckObject(p: PVirtualNode;
  param: Integer; var continue: Boolean);
var
  obj: TObject;
begin
  obj := GetNodeObject(p);
  continue := Integer (obj) <> param
end;


{ TExVirtualStringTree }

function TExVirtualStringTree.GetOptions: TStringTreeOptions;
begin
  Result := inherited TreeOptions as TStringTreeOptions;
end;

procedure TExVirtualStringTree.SetOptions(const Value: TStringTreeOptions);
begin
  inherited TreeOptions.Assign(Value);
end;

end.
