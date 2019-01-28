unit ExVirtualStringTree;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, VirtualTrees, Forms;

type
  TVTIteratorProc = procedure (p : PVirtualNode; param : Integer; var continue : boolean) of object;

  TCustomExVirtualStringTree = class(TCustomVirtualStringTree)
  private
    fProportionalColumnSizes: boolean;

    fColumnPCs : array of Integer;
    fResizeCount : Integer;
    procedure SetProportionalColumnSizes(const Value: boolean);
    procedure SaveColumnPCs;
    function GetNodeObject(node: PVirtualNode): TObject;
    procedure SetNodeObject(node: PVirtualNode; const Value: TObject);
    procedure CheckObject (p : PVirtualNode; param : Integer; var continue : boolean);
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure DoColumnResize(Column: TColumnIndex); override;
    procedure DoHeaderMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure LockControl;
    procedure UnlockControl;

    property ProportionalColumnSizes : boolean read fProportionalColumnSizes write SetProportionalColumnSizes;
    function GetOptionsClass: TTreeOptionsClass; override;
  public
    constructor Create (AOwner : TComponent); override;
    procedure SelectAndFocusNode (node : PVirtualNode; clearSel : boolean = True);
    function FindNodeObject (obj : TObject; root : PVirtualNode = Nil) : PVirtualNode;
    function ForEach (proc : TVTIteratorProc; param : Integer; root : PVirtualNode = Nil) : PVirtualNode;


    property NodeObject [node : PVirtualNode] : TObject read GetNodeObject write SetNodeObject;
  published
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
  Inc (fResizeCount);
  try
    inherited;
  finally
    Dec (fResizeCount)
  end
end;

procedure TCustomExVirtualStringTree.DoHeaderMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SaveColumnPCs
end;

procedure TCustomExVirtualStringTree.SelectAndFocusNode(node: PVirtualNode; clearSel : boolean = True);
begin
  if clearSel then
    ClearSelection;
  if Assigned (node) then
  begin
    Selected [node] := True;
    FocusedNode := node
  end
end;

function TCustomExVirtualStringTree.GetNodeObject(
  node: PVirtualNode): TObject;
var
  obj : PObject;
begin
  obj := GetNodeData (node);
  if Assigned (obj) and Assigned (obj^) then
    result := obj^
  else
    result := nil
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
    fProportionalColumnSizes := False;
    ProportionalColumnSizes := True
  end
end;

procedure TCustomExVirtualStringTree.LockControl;
begin
  SendMessage (Handle, WM_SETREDRAW, 0, 0);
end;

procedure TCustomExVirtualStringTree.Resize;
var
  i, n, w, ww, bw : Integer;
  hasVertScrollBar : boolean;
begin
  inherited;
  if ProportionalColumnSizes and (fResizeCount = 0) and not (csDesigning in ComponentState) then
  begin
    Inc (fResizeCount);
    LockControl;
    try
      with Header do
      begin
        bw := BorderWidth;
        if BorderStyle <> bsNone then
          Inc (bw, 2);
        ww := Width - 2 * bw;
        hasVertScrollBar := (GetWindowLong (handle, GWL_STYLE) and WS_VSCROLL) <> 0;

        if hasVertScrollBar then
          ww := ww - GetSystemMetrics (SM_CXVSCROLL);

        w := 0;
        for i := 0 to Columns.Count - 2 do
        begin
          n := fColumnPCs [i] * ww div 100;
          Columns [i].Width := n;
          Inc (w, n)
        end;

//        if hasVertScrollBar then
//          ww := ww + GetSystemMetrics (SM_CXVSCROLL);
        Columns [Columns.Count - 1].Width := ww - w
      end
    finally
      UnlockControl;
      Dec (fResizeCount);
    end
  end
end;

procedure TCustomExVirtualStringTree.SaveColumnPCs;
var
  i, bw, ww : Integer;
begin
  if not ProportionalColumnSizes then
    Exit;
  bw := BorderWidth;
  if BorderStyle <> bsNone then
    Inc (bw, 2);
  ww := Width - 2 * bw;

  with Header do for i := 0 to Columns.Count - 1 do
    fColumnPCs [i] := Columns [i].Width * 100 div ww
end;

procedure TCustomExVirtualStringTree.SetNodeObject(node: PVirtualNode;
  const Value: TObject);
var
  obj : PObject;
begin
  obj := GetNodeData (node);
  if Assigned (obj) then
    obj^ := Value
end;

procedure TCustomExVirtualStringTree.SetProportionalColumnSizes(
  const Value: boolean);
var
  w, i : Integer;
begin
  if fProportionalColumnSizes <> Value then
  begin
    fProportionalColumnSizes := Value;
    if (csLoading in ComponentState) or (csDesigning in ComponentState) then
      Exit;

    Inc (fResizeCount);
    try
      with Header do
      begin
        w := 0;
        for i := 0 to Columns.Count - 2 do
          w := w + Columns [i].Width;

        if ClientWidth - w > Columns [Columns.Count - 1].Width then
          Columns [Columns.Count - 1].Width := ClientWidth - w;
        SetLength (fColumnPCs, Header.Columns.Count);

        SaveColumnPCs
      end
    finally
      Dec (fResizecount)
    end
  end
end;

procedure TCustomExVirtualStringTree.UnlockControl;
begin
  SendMessage (Handle, WM_SETREDRAW, 1, 0);
  RedrawWindow(Handle,nil,0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN)
end;

function TCustomExVirtualStringTree.FindNodeObject(obj: TObject; root : PVirtualNode): PVirtualNode;
begin
  result := ForEach (CheckObject, Integer (obj), root)
end;

function TCustomExVirtualStringTree.ForEach(proc: TVTIteratorProc; param : Integer;
  root: PVirtualNode) : PVirtualNode;
var
  cont : boolean;
  node : PVirtualNode;

  procedure Iterate (n : PVirtualNode);
  begin
    while n <> Nil do
    begin
      proc (n, param, cont);
      if not cont then
      begin
        node := n;
        break
      end;

      Iterate (GetFirstChild (n));
      n := GetNextSibling(n)
    end
  end;

begin
  if root = Nil then root := RootNode;
  node := Nil;
  Iterate (GetFirstChild (root));
  result := node
end;

procedure TCustomExVirtualStringTree.CheckObject(p: PVirtualNode;
  param: Integer; var continue: boolean);
var
  obj : TObject;
begin
  obj := GetNodeObject (p);
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
