unit CursorGraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IconCursorGraphicsResourceForm, Menus, ActnList, ImgList,
  ComCtrls, cmpColorSelector, ExtCtrls, ToolWin, cmpSizingPageControl,
  cmpBitmapEditor, cmpPropertyListBox, unitExIcon;

type
  TfmCursorGraphicsResource = class(TfmIconCursorGraphicsResource)
    SetHotspot1: TMenuItem;
    procedure actImageHotSpotExecute(Sender: TObject);
    procedure SetHotspot1Click(Sender: TObject);
    procedure PropertyListBox1PropertyChanged(Sender: TObject);
  private
    function GetCursor: TExCursor;
    procedure SetHotspot(const ScreenPt : TPoint);
    { Private declarations }
  protected
    procedure SetObject (const Value : TObject); override;
  public
    procedure PreviewKey (var key : Word; shift : TShiftState); override;
    procedure SaveResource (const undoDetails : string); override;

    property Cursor : TExCursor read GetCursor;
  end;

var
  fmCursorGraphicsResource: TfmCursorGraphicsResource;

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

procedure TfmCursorGraphicsResource.SetHotspot (const ScreenPt : TPoint);
var
  pt : TPoint;
  change : string;
begin
  pt := ScreenPt;
  MapWindowPoints (0, BitmapEditor1.Handle, pt, 1);
  pt.X := pt.X div BitmapEditor1.Magnification;
  pt.Y := pt.Y div BitmapEditor1.Magnification;

  Cursor.Hotspot := pt.X + pt.Y shl 16;
  change := rstHotspotChanged;
  BitmapEditor1.HotSpotX := LoWord (self.Cursor.Hotspot);
  BitmapEditor1.HotSpotY := HiWord (self.Cursor.Hotspot);
  BitmapEditor1.Picture := Image1.Picture;      // Set the editor picture
  SaveResource (change);
end;

procedure TfmCursorGraphicsResource.actImageHotSpotExecute(Sender: TObject);
begin
  inherited;
//
end;

function TfmCursorGraphicsResource.GetCursor : TExCursor;
begin
  result := TExCursor (Image1.Picture.Graphic);
end;

procedure TfmCursorGraphicsResource.PreviewKey(var key: Word;
  shift: TShiftState);
begin
  inherited;
  case key of
    VK_MULTIPLY :
      SetHotspot (Mouse.CursorPos);
  end;
end;

procedure TfmCursorGraphicsResource.PropertyListBox1PropertyChanged(
  Sender: TObject);
var
  change : string;
  prop : TPropertyListProperty;
begin
  with PropertyListBox1 do
    prop := Properties [SelectedPropertyNo];

  change := '';

  case Prop.Tag of
    taHotX :
      begin
        Cursor.Hotspot := (Cursor.Hotspot and $ffff0000) or Prop.PropertyValue;
        change := rstHotspotChanged;
        BitmapEditor1.HotSpotX := LoWord (self.Cursor.Hotspot);
      end;
    taHotY :
      begin
        Cursor.Hotspot := (Cursor.Hotspot and $0000ffff) or (Prop.PropertyValue shl 16);
        change := rstHotspotChanged;
        BitmapEditor1.HotSpotY := HiWord (self.Cursor.Hotspot);
      end;
  end;

  if change <> '' then
  begin
    BitmapEditor1.Picture := Image1.Picture;      // Set the editor picture
    SaveResource (change);
  end
  else
    inherited;
end;

procedure TfmCursorGraphicsResource.SaveResource(const undoDetails: string);
begin
  inherited;
  Screen.Cursors [crCurrentCursor] := Cursor.Handle;
end;

type
  TCrackPopupMenu = class (TPopupMenu)
  end;
procedure TfmCursorGraphicsResource.SetHotspot1Click(Sender: TObject);
var
  pt : TPoint;
begin
  pt := PopupMenu1.PopupPoint;
  if pt.X = -1 then
    pt := Mouse.CursorPos;
  SetHotspot (pt);
  pt.x := -1;
  pt.y := -1;
  TCrackPopupMenu (PopupMenu1).SetPopupPoint(pt);
end;

procedure TfmCursorGraphicsResource.SetObject(const Value: TObject);
begin
  inherited;

  with PropertyListBox1 do                      // Set the properties
  begin
    with FindProperty (rstHotX) do
    begin
      Tag := taHotX;
      PropertyValue := LoWord (self.Cursor.Hotspot);
    end;

    with FindProperty (rstHotY) do
    begin
      Tag := taHotY;
      PropertyValue := HiWord (self.Cursor.Hotspot);
    end
  end;

  Screen.Cursors [crCurrentCursor] := Cursor.Handle;
  sbThumbnail.Cursor := crCurrentCursor;
  BitmapEditor1.HotSpotX := LoWord (self.Cursor.Hotspot);
  BitmapEditor1.HotSpotY := HiWord (self.Cursor.Hotspot);
end;

end.
