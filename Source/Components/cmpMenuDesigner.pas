(*===========================================================================*
 | unit cmpMenuDesigner                                                      |
 |                                                                           |
 | Menu Designer Component                                                   |
 |                                                                           |
 | Version  Date      By    Description                                      |
 | -------  --------  ----  -------------------------------------------------|
 | 1.0      05/07/00  CPWW  Original                                         |
 *===========================================================================*)

unit cmpMenuDesigner;

interface

uses
  Windows, Messages, Menus, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls;

type

//=============================================================================
// TBaseMenuDesigner class.  Base class for TMenuDesigner and TPopupMenuDesigner
//
// NB.  Tags is used to hold the menu item ID.  If it's selected then Tags is
//      -(menu item ID + 3)
//
// NB.  Each time an item is selected, the path to it is save in the fPositionSnapshot
//      list.  If SetItems is called with the 'KeepPosition flag set', the item at the
//      snapshot position will be selected.
//
// NB.  We should really use 'TMenuItem.Command' to hold this - but we can't set it because it's
//      read-only.

  TDesignerMenuItem = class (TMenuItem)
  private
    function GetID: Integer;
    procedure SetID(const Value: Integer);
    function GetSelected: boolean;
    procedure SetSelected(const Value: boolean);
  protected
    procedure MenuChanged(Rebuild: Boolean); override;
  public
    property ID : Integer read GetID write SetID;
    property Selected : boolean read GetSelected write SetSelected;
  end;

  TBaseMenuDesigner = class (TCustomControl)
  private
    fItems: TMenuItem;
    fSelectedItem: TMenuItem;
    fOnSelectedItemChange: TNotifyEvent;
    fDirty : boolean;
    fPositionSnapshot : TList;
    procedure PaintItems (x, y : Integer; items : TMenuItem);
    procedure CalcItemsSize (items : TMenuItem; var stW, shortcutW, h : Integer);
    function DrawTextWidth (lm, rm : Integer; const st : WideString) : Integer;
    procedure SetSelectedItem(const Value: TMenuItem);
    procedure WmGetDLGCode (var msg : TwmGetDlgCode); message WM_GETDLGCODE;
    procedure DoChangeSelectedItem (value : TMenuItem);

    function AddChildItemAt (parent : TMenuItem; index : Integer) : TMenuItem;
    procedure TakeSnapshot;
    function GetSnapshotItem : TMenuItem;
    function GetSelectedItem: TMenuItem;

    function DrawItem (item : TMenuITem; x, y, stw, shw, leftMargin, rightMargin, sth : Integer; vert : boolean) : Integer;
    { Private declarations }
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;

    function ItemAt (X, Y : Integer) : TMenuItem; virtual;
    function ItemAtOffset (items : TMenuItem; XOffset, YOffset, X, Y : Integer) : TMenuItem;
    procedure CalcSize (var w, h : Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    procedure DoEnter; override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    property Items : TMenuItem read fItems;
    property SelectedItem : TMenuItem read GetSelectedItem write SetSelectedItem;
    procedure DeleteItem (item : TMenuItem);

    function InsertItem (beforeItem : TMenuItem) : TMenuItem;
    function AppendItem (afterItem : TMenuItem) : TMenuItem;
    function AddChildItem (parentItem : TMenuItem) : TMenuItem;

    procedure RestoreTags;

    property Dirty : boolean read fDirty;
    procedure SetItems(const Value: TMenuItem; keepPosition : boolean = False);

  published
    property OnSelectedItemChange : TNotifyEvent read fOnSelectedItemChange write fOnSelectedItemChange;
    property Align;
    property Anchors;
    property AutoSize;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyUp;
    property OnKeyPress;
    property OnKeyDown;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TMenuDesigner = class(TBaseMenuDesigner)
  private
    { Private declarations }
  protected
    procedure Paint; override;
    function ItemAt (X, Y : Integer) : TMenuItem; override;
    procedure CalcSize (var w, h : Integer); override;
  public
    constructor Create (AOwner : TComponent); override;
  published
  end;

  TPopupMenuDesigner = class (TBaseMenuDesigner)
  private
    { Private declarations }
  protected
    procedure Paint; override;
    function ItemAt (X, Y : Integer) : TMenuItem; override;
    procedure CalcSize (var w, h : Integer); override;
  public
    constructor Create (AOwner : TComponent); override;
  published
    { Published declarations }
  end;

  TMenuItemDesigner = class (TCustomControl)
  end;

function ExtractCaption (const st : WideString) : WideString;
function ExtractShortcut (const st : WideString) : WideString;
function MergeCaption (const st, shortcut : WideString) : WideString;

implementation

{ TBaseMenuDesigner }

const
  menuLeftMargin = 16;
  menuRightMargin = 16;
  menuTopMargin = 5;
  menuBottomMargin = 5;

  mainMenuLeftMargin = 7;
  mainMenuRightMargin = 7;

function ExtractCaption (const st : WideString) : WideString;
var
  p : Integer;
begin
  result := st;
  p := Pos (#9, result);
  if p > 0 then
    result := Copy (result, 1, p - 1)
end;

function ExtractShortcut (const st : WideString) : WideString;
var
  p : Integer;
begin
  result := st;
  p := Pos (#9, result);
  if p > 0 then
    result := Copy (result, p + 1, MaxInt)
  else
    result := ''
end;

function MergeCaption (const st, shortcut : WideString) : WideString;
begin
  if shortcut <> '' then
    result := st + #9 + shortcut
  else
    result := st
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.AddChildItem()                                     |
 |                                                                      |
 | Add a sub-menu.                                                      |
 |                                                                      |
 | Parameters                                                           |
 |    parentItem : TMenuItem          The parent of the new child menu  |
 *----------------------------------------------------------------------*)
function TBaseMenuDesigner.AddChildItem(parentItem: TMenuItem): TMenuItem;
begin
  result := AddChildItemAt (parentItem, parentItem.Count);
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.AddChildItemAt ()                                  |
 |                                                                      |
 | Add a child item at the specified position.  Private                 |
 |                                                                      |
 | Parameters                                                           |
 |   parent : TMenuItem         The parent of the new child item        |
 |   index : Integer            Position of the child item.             |
 *----------------------------------------------------------------------*)
function TBaseMenuDesigner.AddChildItemAt(parent : TMenuItem; index: Integer): TMenuItem;
begin
  if Assigned (parent) then
  begin
    result := TDesignerMenuItem.Create (self);
    parent.Insert (index, result);
    SelectedItem := result;
    Invalidate
  end
  else
    result := Nil
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.AppendItem()                                       |
 |                                                                      |
 | Append an item to a menu.                                            |
 |                                                                      |
 | Parameters                                                           |
 |    afterItem : TMenuItem          The item to insert after           |
 *----------------------------------------------------------------------*)
function TBaseMenuDesigner.AppendItem(afterItem: TMenuItem): TMenuItem;
var
  idx : Integer;
begin
  if Assigned (afterItem) and Assigned (afterItem.Parent) then
  begin
    idx := afterItem.Parent.IndexOf (afterItem);
    result := AddChildItemAt (afterItem.parent, idx + 1)
  end
  else
    result := Nil
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.CalcItemsSize                                      |
 |                                                                      |
 | Calculate the width & height of a pop-up or child menu               |
 |                                                                      |
 | The height is the height of each item + the top margin + the bottom  |
 | margin.                                                              |
 |                                                                      |
 | Both the widest item text width and widest shortcut text width are   |
 | returned.  Each of these is the left margin, the right margin and    |
 | the width of the text.  This implies that the separation between the |
 | text and shortcut text is the left margin + the right margin.        |
 |                                             |                        |
 | Parameters                                                           |
 |    items : TMenuItem         The items to evaluate                   |
 |    var stW : Integer         The widest text width                   |
 |    var shortcutW : Integer   The widest shortcut width               |
 |    var h : Integer           The hieght of th menu.                  |
 *----------------------------------------------------------------------*)
procedure TBaseMenuDesigner.CalcItemsSize (items: TMenuItem; var stW, shortcutW, h : Integer);
var
  st, s1 : WideString;
  i, w0, w1, lh : Integer;

begin
  inherited;

  stW := 0;
  shortcutW := 0;
  h := menuTopMargin + menuBottomMargin;
  lh := GetSystemMetrics (SM_CYMENU);

  for i := 0 to Items.Count - 1 do
  begin
    st := ExtractCaption (Utf8Decode (items.Items [i].Caption));
    s1 := ExtractShortcut (Utf8Decode (items.Items [i].Caption));

    if st <> '-' then
    begin
      if s1 <> '' then  // Calculate the shortcut width
      begin
        w1 := DrawTextWidth (menuLeftMargin, menuRightMargin, s1);
        if w1 > shortcutW then
          shortcutW := w1
      end;

                        // Calculate the text width
      w0 := DrawTextWidth (menuLeftMargin, menuRightMargin, st);
    end
    else                // Nominal width for empty item
      w0 := 50 + menuLeftMargin + menuRightMargin;

    if w0 > stW then
      stW := w0;
    Inc (h, lh)
  end
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.CalcSize                                           |
 |                                                                      |
 | Return the width and height of a bounding rectangle that would       |
 | completely cover the fully expanded menu.                            |
 |                                                                      |
 | This is overridden by TMenuDesigner and TPopupMenuDesigner.          |
 *----------------------------------------------------------------------*)
procedure TBaseMenuDesigner.CalcSize(var w, h: Integer);
begin
  w := 0;
  h := 0;
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.CanAutoSize                                        |
 |                                                                      |
 | Returns the width and height to the VCL so alignment/auto-sizing     |
 | works.                                                               |
 *----------------------------------------------------------------------*)
function TBaseMenuDesigner.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
var
  calced : boolean;
  w, h : Integer;
begin
  Result := True;
  if not (csDesigning in ComponentState) then
  begin
    calced := False;
    if Align in [alNone, alLeft, alRight] then
    begin
      CalcSize (w, h);
      calced := True;
      NewWidth := w
    end;

    if Align in [alNone, alTop, alBottom] then
    begin
      if not calced then CalcSize (w, h);
      NewHeight := h
    end
  end
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.Create ()                                          |
 |                                                                      |
 | Constructor for TBaseMenuDesigner                                    |
 *----------------------------------------------------------------------*)
constructor TBaseMenuDesigner.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReflector];
  DoubleBuffered := True;
  fItems := TDesignerMenuItem.Create (self);
  fPositionSnapshot := TList.Create;
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.DeleteItem ()                                      |
 |                                                                      |
 | Delete an item.  Select the nearest item if the currently selected   |
 | one is deleted                                                       |
 *----------------------------------------------------------------------*)
procedure TBaseMenuDesigner.DeleteItem(item: TMenuItem);
var
  selIdx : Integer;
  parent : TMenuItem;
begin
  if Assigned (item) then
  begin
    selIDx := -1;
    if fSelectedItem = item then
    begin
      parent := item.Parent;
      if parent <> Nil then
        selIdx := parent.IndexOf (item)
    end
    else
      parent := Nil;

    item.Free;

    if Assigned (parent) then
    begin
      while (selIdx <> -1) and (selIdx >= parent.Count)  do
        Dec (selIdx);

      if selIdx <> -1 then
        SelectedItem  := parent.Items [selIdx]
      else
        SelectedItem := Parent
    end
    else
      SelectedItem := Nil;

    Invalidate
  end
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.Destroy ()                                         |
 |                                                                      |
 | Destructor for the designer                                          |
 *----------------------------------------------------------------------*)
destructor TBaseMenuDesigner.Destroy;
begin
  fPositionSnapshot.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.DoChangeSelectedItem ()                            |
 |                                                                      |
 | Internally set the selected item, take a position snapshot, and      |
 | raise an event if necessary.                                         |
 *----------------------------------------------------------------------*)
procedure TBaseMenuDesigner.DoChangeSelectedItem(value: TMenuItem);
begin
  fSelectedItem := value;

  TakeSnapshot;

  if Assigned (fOnSelectedItemChange) and Assigned (fSelectedItem) and not (csDestroying in ComponentState) then
    OnSelectedItemChange (self);
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.DrawItem                                           |
 |                                                                      |
 | Draw an item.                                                        |
 |                                                                      |
 | Parmeters:                                                           |
 |   item : TMenuItem                   The item to draw                |
 |   x : Integer                        Horizontal position             |
 |   stw : Integer                      Caption width.  -1 to calculate.|
 |   shw : Integer                      Shortcut width                  |
 |   leftMargin : Integer               Left margin                     |
 |   rightMargin : Integer              Right Margin                    |
 |   sth : Integer                      Item height                     |
 |   vert : boolean                     True if popup or drop down.     |
 |                                      False if main menu.             |
 |                                                                      |
 | nb.  If 'Vert' is set, the bo0unding rectangle is shrunk to avoid    |
 | the edge borders, and '-' captions display a full 'Center Line'      |
 |                                                                      |
 | The function returns the horizontal position for the next menu item. |
 | If vert is set this is meaningless                                   |
 *----------------------------------------------------------------------*)
procedure TBaseMenuDesigner.DoEnter;
begin
  inherited;
  Invalidate
end;

procedure TBaseMenuDesigner.DoExit;
begin
  inherited;
  Invalidate
end;

function TBaseMenuDesigner.DrawItem(item: TMenuITem; x, y, stw, shw, leftMargin, rightMargin, sth : Integer; vert: boolean) : Integer;
var
  st, s1 : WideString;
  params : TDrawTextParams;
  r : TRect;
  extent, oldMode : Integer;
  b : TBitmap;

// -----------------------------------------------------------------------
// Helper function draws string in correct color, depending on item params
  procedure DrawStr (left : Integer; const st : WideString);
  var
    r : TRect;
    defFColor : TColor;
  begin
    oldMode := SetBkMode (Canvas.Handle, TRANSPARENT);
    defFColor := Canvas.Font.Color;
    try
      r := Rect (left, y, extent, y + sth);
      if not Item.Enabled then                  // Get the correct font color
      begin
        Canvas.Font.Color := clBtnHighlight;    // Disabled item.  Draw highlight then
        OffsetRect (r, 1, 1)                    // shadow below
      end
      else
        if TDesignerMenuItem (Item).Selected and Focused then
          Canvas.Font.Color := clHighlightText;

                                                // Draw the text

      DrawTextExW (Canvas.Handle, PWideChar (st), -1, r, DT_LEFT or DT_SINGLELINE or DT_EXPANDTABS or DT_VCENTER, @params);

      if Item.Checked then                      // Draw a tick if it's checked
      begin
        b := TBitmap.Create;
        try
          b.Height := sth - 2;
          b.Width := b.Height;
          DrawFrameControl (b.Canvas.Handle, RECT (0, 2, sth - 2, sth), DFC_MENU, DFCS_MENUCHECK);
          b.TransparentColor := clWhite;
          b.Transparent := True;

          Canvas.Draw (r.Left, r.Top + 1, b)
        finally
          b.Free
        end
      end;

      if not Item.Enabled then
      begin
        Canvas.Font.Color := clBtnShadow;       // Draw shadow if not enabled
        r := Rect (left, y, extent, y + sth);
        DrawTextExW (Canvas.Handle, PWideChar (st), -1, r, DT_LEFT or DT_SINGLELINE or DT_EXPANDTABS or DT_VCENTER, @params);
      end
    finally
      Canvas.Font.Color := defFColor;
      SetBkMode (Canvas.Handle, oldMode)
    end
  end;

begin
  FillChar (params, sizeof (params), 0);        // Set up DrawTextEx params
  params.cbSize := sizeof (params);
  params.iLeftMargin := leftMargin;
  params.iRightMargin := rightMargin;
  params.iTabLength := 0;

  st := ExtractCaption (Utf8Decode (item.Caption));          // Extract caption & shortcut
  s1 := ExtractShortcut (Utf8Decode (item.Caption));

  if stw = -1 then                              // Calculate string width if required (horiz menus)
    stw := DrawTextWidth (leftMargin, RightMargin, st);

  if vert then                                  // Adjust x for popup/droppdown borders
    Inc (x, GetSystemMetrics (SM_CXEDGE));

  extent := x + stw + shw;                      // Get width of highlight rectangle

  if vert then                                  // Adjust width for popup/dropdown menus
    Dec (extent, 2 * GetSystemMetrics (SM_CXEDGE));

  r := Rect (x, y, extent, y + sth);            // Get highlight rectangle

  if TDesignerMenuItem (Item).Selected then
    if Focused then    // Get correct brush color...
      Canvas.Brush.Color := clHighlight
    else
      Canvas.Brush.Color := clBtnShadow
  else
    Canvas.Brush.Color := Color;

  Canvas.FillRect (r);                          // .. and fill the background

  if st <> '-' then                             // Draw the main caption
    DrawStr (x, st);

  if vert then
  begin
    if st = '-' then                            // Draw a separator if necessary
    begin
      r.Bottom := y + sth div 2;
      DrawEdge (Canvas.Handle, r, EDGE_ETCHED, BF_BOTTOM);
    end
    else if s1 <> '' then                       // Draw the shortcut
      DrawStr (x + stw, s1);
  end;
  result := extent
end;

(*----------------------------------------------------------------------*
 | TBaseMenuDesigner.DrawTextWidth                                      |
 |                                                                      |
 | Return the width of a bounding rectangle that can contain a string   |
 | including a right and left margin.                                   |
 *----------------------------------------------------------------------*)
function TBaseMenuDesigner.DrawTextWidth(lm, rm : Integer; const st: WideString): Integer;
var
  r : TRect;
  params : TDrawTextParams;
begin
  if st = '' then
    result := lm + rm
  else
  begin
    r := Rect (0, 0, 0, 0);

    FillChar (params, sizeof (params), 0);
    params.cbSize := sizeof (params);
    params.iLeftMargin := lm;
    params.iRightMargin := rm;
    params.iTabLength := 0;

    // nb.  DT_CALCRECT ensures that the text isn't actually drawn - just the rect is returned.
    DrawTextExW (Canvas.Handle, PWideChar (st), Length (st), r, DT_LEFT or DT_SINGLELINE or DT_CALCRECT, @params);
    result := r.Right
  end
end;

function TBaseMenuDesigner.GetSelectedItem: TMenuItem;
begin
  if fSelectedItem is TDesignerMenuItem then
    result := TDesignerMenuItem (fSelectedItem)
  else
    result := Nil
end;

function TBaseMenuDesigner.GetSnapshotItem: TMenuItem;
var
  i, v : Integer;
  p : TMenuItem;
begin
  p := fItems;
  for i := 0 to fPositionSnapshot.Count - 1 do
  begin
    if not Assigned (p) then break;
    v := Integer (fPositionSnapshot [i]);
    if v <> -1 then
      p := p.Items [v]
  end;

  result := p;
end;

function TBaseMenuDesigner.InsertItem(beforeItem: TMenuItem): TMenuItem;
var
  idx : Integer;
begin
  if Assigned (beforeItem) and Assigned (beforeItem.Parent) then
  begin
    idx := beforeItem.Parent.IndexOf (beforeItem);
    result := AddChildItemAt (beforeItem.parent, idx);
    SelectedItem := result
  end
  else
    result := Nil
end;

function TBaseMenuDesigner.ItemAt(X, Y: Integer): TMenuItem;
begin
  result := Nil
end;

function TBaseMenuDesigner.ItemAtOffset(items : TMenuItem; XOffset, YOffset, X,
  Y: Integer): TMenuItem;
var
  w, m, h : Integer;
  r : TRect;
  i, lh, ew : Integer;
  item : TMenuItem;

begin
  result := Nil;
  CalcItemsSize (items, w, m, h);

  r.Left := XOffset;
  r.Right := XOffset + w + m;
  r.top := YOffset;
  r.bottom := YOffset + h;

  lh := GetSystemMetrics (SM_CYMENU);
  ew := GetSystemMetrics (SM_CXEDGE);

  Inc (YOffset, menuTopMargin);

  for i := 0 to items.Count - 1 do
  begin
    item := items.Items [i];

    r.Top := YOffset;
    r.Bottom := YOffset + lh;
    r.Left := XOffset + ew;
    r.Right := XOffset + w + m - ew;

    if PtInRect (r, Point (X, Y)) then
    begin
      result := item;
      break
    end;

    if (item.Count > 0) and TDesignerMenuItem (Item).Selected then
      result := ItemAtOffset (item, XOffset + w + m, YOffset, X, Y);

    YOffset := YOffset + lh;
  end
end;

procedure TBaseMenuDesigner.KeyDown(var Key: Word; Shift: TShiftState);
var
  vertMenu, vertParent : boolean;
  parent, grandparent : TMenuItem;
  gidx, idx : Integer;
begin
  if Assigned (SelectedItem) and Assigned (selectedItem.Parent) then
  begin
    parent := SelectedItem.Parent;
    grandparent := parent.parent;
    idx := parent.IndexOf (SelectedItem);
    vertMenu := not ((self is TMenuDesigner) and not Assigned (grandparent));
    vertParent := not ((self is TMenuDesigner) and Assigned (grandparent) and not Assigned (grandparent.parent));

    if not vertParent then
      gidx := grandparent.IndexOf (parent)
    else
      gidx := 0;

    case Key of
      VK_RIGHT :
        if vertMenu then
        begin
          if SelectedItem.Count > 0 then
            SelectedItem := SelectedItem.Items [0]
          else
            if (not vertParent) and (gidx < grandparent.Count - 1) then
              SelectedItem := grandparent.Items [gidx + 1]
        end
        else
          if idx = parent.Count - 1 then
            SelectedItem := parent.Items [0]
          else
            SelectedItem := parent.Items [idx + 1];

      VK_LEFT :
        if vertMenu then
        begin
          if (idx = 0) and vertParent then
            SelectedItem := parent
          else
            if (not vertParent) and (gidx > 0) then
              SelectedItem := grandparent.Items [gidx - 1]
        end
        else
          if idx = 0 then
            SelectedItem := parent.Items [parent.Count - 1]
          else
            SelectedItem := parent.Items [idx - 1];

        VK_UP :
          if vertMenu then
            if idx > 0 then
              SelectedItem := parent.Items [idx - 1]
            else
              if not vertParent then
                SelectedItem := grandparent.Items [gidx];

        VK_DOWN :
          if vertMenu then
          begin
            if idx < parent.Count - 1 then
              SelectedItem := parent.Items [idx + 1]
          end
          else
            if SelectedItem.Count > 0 then
              SelectedItem := SelectedItem.Items [0];
      end
  end;
  inherited;
end;

procedure TBaseMenuDesigner.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i : TMenuItem;
begin
  SetFocus;
  i := ItemAt (X, Y);
  if i <> Nil then
    SelectedItem := i;
  inherited;
end;

procedure TBaseMenuDesigner.PaintItems(x, y: Integer; items: TMenuItem);
var
  w, m, h : Integer;
  lh, i : Integer;
  item : TMenuItem;
  r : TRect;
begin
  CalcItemsSize (items, w, m, h);

  r.Left := x;
  r.Right := x + w + m;
  r.top := y;
  r.bottom := y + h;

  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect (r);
  Canvas.Font := Font;

  DrawEdge (Canvas.Handle, r, EDGE_RAISED, BF_RECT);

  lh := GetSystemMetrics (SM_CYMENU);

  Inc (y, menuTopMargin);

  for i := 0 to items.Count - 1 do
  begin
    item := items.Items [i];
    DrawItem (item, x, y, w, m, menuLeftMargin, menuRightMargin, lh, True);

    if (item.Count > 0) and TDesignerMenuItem (item).Selected then
      PaintItems (x + w + m, y - menuTopMargin, item);
    y := y + lh
  end
end;

procedure TBaseMenuDesigner.RestoreTags;

  procedure RestoreItemTags (item : TMenuItem);
  var
    i : Integer;
  begin
    TDesignerMenuItem (item).Selected := False;
    for i := 0 to item.Count - 1 do
      RestoreItemTags (item.Items [i])
  end;

begin
  RestoreItemTags (Items)
end;

procedure TBaseMenuDesigner.SetItems(const Value: TMenuItem; keepPosition : boolean);
var
  selItem : TMenuItem;

  procedure AssignItem (src, dest : TMenuItem);
  var
    i : Integer;
    newItem : TMenuItem;
  begin
    dest.Caption := src.Caption;
    dest.Tag := src.Tag;
    dest.ShortCut := src.ShortCut;
    dest.Enabled := src.Enabled;
    dest.Checked := src.Checked;

    for i := 0 to src.Count - 1 do
    begin
      newItem := TDesignerMenuItem.Create (self);
      dest.Add (newItem);
      AssignItem (src.Items [i], newItem)
    end
  end;

begin
  Items.Clear;
  fSelectedItem := Nil;
  AssignItem (value, items);

  if KeepPosition then
  begin
    selItem := GetSnapshotItem;
    if Assigned (selItem) then
      SelectedItem := selItem
    else
      SelectedITem := Items [0]
  end
  else
    SelectedItem := Items [0];
  ReAlign;
  fDirty := False;
  Invalidate
end;

procedure TBaseMenuDesigner.SetSelectedItem(const Value: TMenuItem);
var
  p : TMenuItem;

  procedure ClearSelection (items : TMenuItem);
  var
    i : Integer;
    item : TMenuItem;
  begin
    for i := 0 to items.Count - 1 do
    begin
      item := items.Items [i];
      if TDesignerMenuItem (Item).Selected then
      begin
        TDesignerMenuItem (item).Selected := False;
        ClearSelection (item)
      end
    end
  end;

begin
  if Assigned (value) and not (value is TDesignerMenuItem) then
    raise Exception.Create ('Can''t select item');

  if fSelectedItem <> value then
  begin
    ClearSelection (fItems);
    p := value;
    while Assigned (p) do
    begin
      TDesignerMenuItem (p).Selected := True;
      p := p.Parent
    end;
    DoChangeSelectedItem (Value);
    Invalidate
  end
  else
  begin
    DoChangeSelectedItem (Value);
    Invalidate
  end
end;

procedure TBaseMenuDesigner.TakeSnapshot;
  procedure Snapshot (item : TMenuItem);
  begin
    if Assigned (item) then
    begin
      Snapshot (item.Parent);
      fPositionSnapshot.Add (pointer (item.MenuIndex))
    end
  end;
begin
  fPositionSnapshot.Clear;
  Snapshot (fSelectedItem);
end;

procedure TBaseMenuDesigner.WmGetDLGCode(var msg: TwmGetDlgCode);
begin
  msg.Result := DLGC_WANTARROWS
end;

{ TMenuDesigner }

procedure TMenuDesigner.CalcSize(var w, h: Integer);
var
  i : Integer;
  w1, h1, y : Integer;

  procedure CalcSubmenuExtent (item : TMenuItem; x, y : Integer; var w, h : Integer);
  var
    i, wST, wShortCut, w1, h1 : Integer;
  begin
    if item.Count > 0 then
    begin
      CalcItemsSize (item, wST, wShortCut, h1);
      w := x + wST + wShortCut + 2 * GetSystemMetrics (SM_CXEDGE);
      h := y + h1 + 2 * GetSystemMetrics (SM_CYEDGE);

      for i := 0 to item.Count - 1 do
      begin
        CalcSubmenuExtent (item.Items [i], x + w, y + GetSystemMetrics (SM_CYMENU) * i, w1, h1);

        if h1 > h then h := h1;
        if w1 > w then w := w1
      end
    end
    else
    begin
      w := x;
      h := y
    end
  end;

begin
  h := 3 + GetSystemMetrics (SM_CYMENU);
  w := menuLeftMargin + menuRightMargin;
  y := h;

  for i := 0 to items.Count - 1 do
  begin
    CalcSubmenuExtent (items.items [i], 0, y, w1, h1);

    if w1 > w then w := w1;
    if h1 > h then h := h1
  end
end;

constructor TMenuDesigner.Create(AOwner: TComponent);
begin
  inherited;
  Align := alTop;
  Height := 182;
end;

function TMenuDesigner.ItemAt(X, Y: Integer): TMenuItem;
var
  i, tm : Integer;
  st : WideString;
  item : TMenuItem;
  r : TRect;
  xp : Integer;
begin
  tm := 3;
  xp := 0;
  result := Nil;
  for i := 0 to Items.Count - 1 do
  begin
    item := Items.Items [i];
    st := Utf8Decode (item.Caption);
    r.Left := xp;
    r.Right := xp + DrawTextWidth (mainMenuLeftMargin, mainMenuRightMargin, st);
    r.Top := tm;
    r.Bottom := tm + GetSystemMetrics (SM_CYMENU);

    if PtInRect (r, Point (X, Y)) then
    begin
      result := Item;
      break
    end;

    if (item.Count > 0) and TDesignerMenuItem (item).Selected then
      result := ItemAtOffset (item, xp + 1, r.Bottom, X, Y);
    xp := r.right;
  end
end;

procedure TMenuDesigner.Paint;
var
  x, x1, i, tm : Integer;
  item : TMenuItem;
  r : TRect;
begin
  inherited;

  tm := 3;
  x := 0;

  r := Rect (0, 0, ClientWidth, GetSystemMetrics (SM_CYMENU) + 7);
  DrawEdge (Canvas.Handle, r, EDGE_ETCHED, BF_BOTTOM);

  for i := 0 to Items.Count - 1 do
  begin
    item := Items.Items [i];

    x1 := DrawItem (item, x, tm, -1, -1, mainMenuLeftMargin, mainMenuRightMargin, GetSystemMetrics (SM_CYMENU), False);

    if (item.Count > 0) and TDesignerMenuItem (item).Selected then
      PaintItems (x + 1, r.Bottom, item);

    x := x1
  end
end;

{ TPopupMenuDesigner }

procedure TPopupMenuDesigner.CalcSize(var w, h: Integer);
begin

end;

constructor TPopupMenuDesigner.Create(AOwner: TComponent);
begin
  inherited;
  Width := 185;
  Height := 41;
end;

function TPopupMenuDesigner.ItemAt(X, Y: Integer): TMenuItem;
begin
  result := Nil;
end;

procedure TPopupMenuDesigner.Paint;
begin
  inherited;
end;

{ TDesignerMenuItem }

function TDesignerMenuItem.GetID: Integer;
begin
  result := Tag;
  if result < -1 then
     result := (-result) - 3
end;

function TDesignerMenuItem.GetSelected: boolean;
begin
  result := Tag < -1
end;

procedure TDesignerMenuItem.MenuChanged(Rebuild: Boolean);
begin
  inherited;
  TBaseMenuDesigner (Owner).fDirty := True;
  TBaseMenuDesigner (Owner).Invalidate
end;

procedure TDesignerMenuItem.SetID(const Value: Integer);
var
  p : TMenuItem;

  procedure CheckDuplicateIds (p : TMenuItem);
  var
    i : Integer;
  begin
    if (p is TDesignerMenuItem) and (p <> Self) and (TDesignerMenuItem (p).ID = Value) and (TDesignerMenuItem (p).ID <> -1) and (TDesignerMenuItem (p).ID <> 0) then
      raise Exception.Create ('Duplicate menu ID');

    for i := 0 to p.Count - 1 do
      CheckDuplicateIds (p.Items [i])
  end;
begin
  p := Self;
  while Assigned (p.Parent) do
    p := p.Parent;

  CheckDuplicateIDs (p);

  if Selected then
    Tag := -(value + 3)
  else
    Tag := value;

  MenuChanged (True)
end;

procedure TDesignerMenuItem.SetSelected(const Value: boolean);
begin
  if Value <> Selected then
    if Value then
      Tag := -(Tag + 3)
    else
      Tag := ID
end;

end.
