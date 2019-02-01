unit CursorGraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IconCursorGraphicsResourceForm, Menus, ActnList, ImgList,
  ComCtrls, cmpColorSelector, ExtCtrls, ToolWin, cmpSizingPageControl,
  cmpBitmapEditor, cmpPropertyListBox, unitExIcon, System.Actions,
  System.ImageList;

type
  TFormCursorGraphicsResource = class(TFormIconCursorGraphicsResource)
    MenuItemSetHotspot: TMenuItem;
    procedure actImageHotSpotExecute(Sender: TObject);
    procedure MenuItemSetHotspotClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetCursor: TExCursor;
    procedure SetHotspot(const ScreenPt: TPoint);
  protected
    procedure SetObject(const Value: TObject); override;
    procedure PropertyListBoxPropertyChanged(Sender: TObject); override;
  public
    procedure PreviewKey(var key: Word; shift: TShiftState); override;
    procedure SaveResource(const undoDetails: string); override;

    property Cursor: TExCursor read GetCursor;
  end;

implementation

{$R *.dfm}

resourcestring
  rstHotX = 'Hot Spot Left';
  rstHotY = 'Hot Spot Top';
  rstHotspotChanged = 'hotspot change';

const
  crCurrentCursor = 10;

  taHotX = 20;
  taHotY = 21;


{ TfmCursorGraphicsResource }

procedure TFormCursorGraphicsResource.SetHotspot(const ScreenPt: TPoint);
var
  pt: TPoint;
  change: string;
begin
  pt := ScreenPt;
  MapWindowPoints (0, BitmapEditor.Handle, pt, 1);
  pt.X := pt.X div BitmapEditor.Magnification;
  pt.Y := pt.Y div BitmapEditor.Magnification;

  Cursor.Hotspot := pt.X + pt.Y shl 16;
  change := rstHotspotChanged;
  BitmapEditor.HotSpotX := LoWord(Self.Cursor.Hotspot);
  BitmapEditor.HotSpotY := HiWord(Self.Cursor.Hotspot);
  BitmapEditor.Picture := Image.Picture;      // Set the editor picture
  SaveResource(change);
end;

procedure TFormCursorGraphicsResource.actImageHotSpotExecute(Sender: TObject);
begin
  inherited;
//
end;

procedure TFormCursorGraphicsResource.FormCreate(Sender: TObject);
begin
  inherited;

  with TPropertyListProperty(PropertyListBox.Properties.Add) do
  begin
    PropertyName := 'Hot Spot Left';
    PropertyType := ptInteger;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
  with TPropertyListProperty(PropertyListBox.Properties.Add) do
  begin
    PropertyName := 'Hot Spot Top';
    PropertyType := ptInteger;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
end;

function TFormCursorGraphicsResource.GetCursor: TExCursor;
begin
  Result := TExCursor(Image.Picture.Graphic);
end;

procedure TFormCursorGraphicsResource.PreviewKey(var key: Word;
  shift: TShiftState);
begin
  inherited;
  case key of
    VK_MULTIPLY :
      SetHotspot(Mouse.CursorPos);
  end;
end;

procedure TFormCursorGraphicsResource.PropertyListBoxPropertyChanged(
  Sender: TObject);
var
  change: string;
  prop: TPropertyListProperty;
begin
  with PropertyListBox do
    prop := Properties[SelectedPropertyNo];

  change := '';

  case Prop.Tag of
    taHotX :
      begin
        Cursor.Hotspot := (Cursor.Hotspot and $ffff0000) or Prop.PropertyValue;
        change := rstHotspotChanged;
        BitmapEditor.HotSpotX := LoWord (Self.Cursor.Hotspot);
      end;
    taHotY :
      begin
        Cursor.Hotspot := (Cursor.Hotspot and $0000ffff) or (Prop.PropertyValue shl 16);
        change := rstHotspotChanged;
        BitmapEditor.HotSpotY := HiWord (Self.Cursor.Hotspot);
      end;
  end;

  if change <> '' then
  begin
    BitmapEditor.Picture := Image.Picture;      // Set the editor picture
    SaveResource(change);
  end
  else
    inherited;
end;

procedure TFormCursorGraphicsResource.SaveResource(const undoDetails: string);
begin
  inherited;
  Screen.Cursors[crCurrentCursor] := Cursor.Handle;
end;

type
  TCrackPopupMenu = class (TPopupMenu)
  end;

procedure TFormCursorGraphicsResource.MenuItemSetHotspotClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := PopupMenu.PopupPoint;
  if pt.X = -1 then
    pt := Mouse.CursorPos;
  SetHotspot(pt);
  pt.x := -1;
  pt.y := -1;
  TCrackPopupMenu(PopupMenu).SetPopupPoint(pt);
end;

procedure TFormCursorGraphicsResource.SetObject(const Value: TObject);
begin
  inherited;

  with PropertyListBox do                      // Set the properties
  begin
    with FindProperty(rstHotX) do
    begin
      Tag := taHotX;
      PropertyValue := LoWord (Self.Cursor.Hotspot);
    end;

    with FindProperty(rstHotY) do
    begin
      Tag := taHotY;
      PropertyValue := HiWord (Self.Cursor.Hotspot);
    end
  end;

  Screen.Cursors[crCurrentCursor] := Cursor.Handle;
  ScrollBoxThumbnail.Cursor := crCurrentCursor;
  BitmapEditor.HotSpotX := LoWord (Self.Cursor.Hotspot);
  BitmapEditor.HotSpotY := HiWord (Self.Cursor.Hotspot);
end;

end.
