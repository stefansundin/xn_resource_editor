(*======================================================================*
 | GraphicsResourceForm                                                 |
 |                                                                      |
 | Remember:                                                            |
 |                                                                      |
 | The thumbnail image(Image) contains the master image.  It is in    |
 | the correct format, has the correct palette, is the correct graphic  |
 | type, etc.                                                           |
 |                                                                      |
 | The BitmapEditor image is a pf24Bit format bitmap regardless of the  |
 | thumbnail image's type and format, so it can display non-palette     |
 | color backgrounds for images with palettes.                          |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      05/01/2001  CPWW  Original                                  |
 *======================================================================*)

unit GraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, ExtCtrls, ComCtrls, ImgList, ToolWin, ActnList, Menus,
  GifImage, Actions, ImageList, cmpSizingPageControl, cmpPropertyListBox,
  cmpBitmapEditor, unitResourceGraphics, cmpColorSelector, unitExIcon;

const
  WM_STATUSBAR = WM_USER + $203;
  WM_ADDIMAGERESOURCE = WM_USER + $204;

type
  TFormGraphicsResource = class(TFormResource)
    ActionImageAddImage: TAction;
    ActionImageColorsPalette: TAction;
    ActionImageToolsPalette: TAction;
    ActionImageZoomIn: TAction;
    ActionImageZoomOut: TAction;
    ActionList: TActionList;
    BitmapEditor: TBitmapEditor;
    ColorDialog: TColorDialog;
    ColorSelector: TColorSelector;
    Image: TImage;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItemAddImage1: TMenuItem;
    MenuItemAddImage2: TMenuItem;
    MenuItemColorsPalette1: TMenuItem;
    MenuItemColorsPalette2: TMenuItem;
    MenuItemDrawingToolsPalette1: TMenuItem;
    MenuItemDrawingToolsPalette2: TMenuItem;
    MenuItemImage: TMenuItem;
    MenuItemToolPalettes1: TMenuItem;
    MenuItemToolPalettes2: TMenuItem;
    MenuItemZoomIn1: TMenuItem;
    MenuItemZoomIn2: TMenuItem;
    MenuItemZoomOut1: TMenuItem;
    MenuItemZoomOut2: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    PanelMain: TPanel;
    PanelColors: TPanel;
    PanelGraphics: TPanel;
    PanelLeft: TPanel;
    PanelTransparent: TPanel;
    PopupMenu: TPopupMenu;
    ScrollBoxThumbnail: TScrollBox;
    ScrollBoxBitmapEditor: TScrollBox;
    ShapeBack: TShape;
    ShapeFore: TShape;
    SizingPageControl: TSizingPageControl;
    Splitter: TSplitter;
    Splitter2: TSplitter;
    tbAirbrush: TToolButton;
    tbBrush: TToolButton;
    tbDropper: TToolButton;
    tbEllipse: TToolButton;
    tbEraser: TToolButton;
    tbFillEllipse: TToolButton;
    tbFillRect: TToolButton;
    tbFillRoundRect: TToolButton;
    tbFloodFill: TToolButton;
    tbFrameEllipse: TToolButton;
    tbFrameRect: TToolButton;
    tbFrameRoundRect: TToolButton;
    tbMagnifier: TToolButton;
    tbPencil: TToolButton;
    tbRect: TToolButton;
    tbRoundRect: TToolButton;
    tbSelectRect: TToolButton;
    tbSelectShape: TToolButton;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton10: TToolButton;
    ToolButton12: TToolButton;
    TrackBar: TTrackBar;
    procedure BitmapEditorGetText(Sender: TObject; Font: TFont;
      var txt: WideString);
    procedure FormShow(Sender: TObject);
    procedure SizingPageControlDockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure SizingPageControlUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure ColorSelectorColorSelect(Sender: TObject);
    procedure tbPencilClick(Sender: TObject);
    procedure ActionImageZoomInExecute(Sender: TObject);
    procedure ActionImageZoomOutExecute(Sender: TObject);
    procedure ColorSelectorDblClick(Sender: TObject);
    procedure BitmapEditorEndChange(Sender: TObject);
    procedure BitmapEditorChange(Sender: TObject);
    procedure PanelTransparentMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelTransparentDblClick(Sender: TObject);
    procedure BitmapEditorDrawToolChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure ActionImageColorsPaletteExecute(Sender: TObject);
    procedure ActionImageToolsPaletteExecute(Sender: TObject);
    procedure MenuItemImageClick(Sender: TObject);
    procedure BitmapEditorSelectionRectChange(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure ActionImageAddImageExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPCWidth: Integer;
    FPropertyListBox: TPropertyListBox;
    FDetails: TGraphicsResourceDetails;

    procedure SetPaletteForPixelFormat(reset: Boolean);
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem: TMenuItem; override;
    function GetImportExportType: TImportExportType; override;

    function GetCanCut: Boolean; override;
    function GetCanCopy: Boolean; override;
    function GetCanPaste: Boolean; override;
    function GetCanSelectAll: Boolean; override;
    function GetCanDelete: Boolean; override;
    procedure UpdateActions; override;

    procedure PropertyListBoxPropertyChanged(Sender: TObject); virtual;
  public
    PropertyListBox: TPropertyListBox;
    procedure PreviewKey(var key: Word; shift: TShiftState); override;
    procedure SaveResource(const undoDetails: string); virtual;
    procedure SelectAll; override;
    procedure EditDelete; override;
    procedure Copy; override;
    procedure Cut; override;
    procedure Paste; override;
  end;

var
  fmGraphicsResource: TFormGraphicsResource;

implementation

uses
  ClipBrd, Jpeg, FormTextInput;

{$R *.DFM}

resourcestring
  rstPaletteChange = 'change palette';          // 'Undo' descriptions
  rstRotate270     = 'rotate 90° anticlockwise';
  rstRotate90      = 'rotate 90° clockwise';
  rstRotate180     = 'rotate 180°';
  rstResizeImage   = 'resize image';
  rstFormatChange  = 'change resolution';
  rstPasteImage    = 'paste image';
  rstWidthChanged  = 'width change';
  rstHeightChanged = 'height change';
  rstPixelFormatChanged = 'pixel format change';

  rstWidth = 'Width';
  rstHeight = 'Height';
  rstPixelFormat = 'Pixel Format';

  rstCutImage = 'cut cmage';
  rstDeleteImage = 'delete image';

const
  taWidth = 0;
  taHeight = 1;
  taPixelFormat = 2;

function GetPixelFormat(graphic: TGraphic): TPixelFormat;
begin
  if graphic is TGifImage then
  begin
    case TGifImage(graphic).BitsPerPixel of
      1: Result := pf1Bit;
      4: Result := pf4Bit;
      else Result := pf8Bit;
    end
  end
  else
    Result := unitExIcon.GetPixelFormat(graphic)
end;

(*----------------------------------------------------------------------*
 | ResizePicture                                                        |
 |                                                                      |
 | Resize a picture, handling anomolies like JPEG                       |
 |                                                                      |
 | Parameters:                                                          |
 |   p: TPicture;        The picture to resize                         |
 |   newWidth: Integer   The new width                                 |
 |   newHeight: Integer  The new height                                |
 *----------------------------------------------------------------------*)
procedure ResizePicture(p: TPicture; newWidth, newHeight: Integer);
var
  b: TBitmap;
begin
  if (p.graphic is TJPegImage) or (p.Graphic is TGifImage) then
  begin
    b := TBitmap.Create;
    try
      b.Width := newWidth;
      b.Height := newHeight;
      b.PixelFormat := pf24Bit;
      b.Canvas.StretchDraw(Rect(0, 0, newWidth, newHeight), p.Graphic);
      p.Graphic.Assign(b)
    finally
      b.Free
    end
  end
  else
  begin
    p.Graphic.Width := newWidth;
    p.Graphic.Height := newHeight
  end
end;

{ TFormGraphicsResource }

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.SetObject                                        |
 |                                                                      |
 | Called when the resource changes - when a new one is loaded, or      |
 | after undo/redo.                                                     |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.SetObject(const Value: TObject);
var
  NewImage: Boolean;
  Transp: Boolean;
begin
  NewImage := Value <> Obj;                     // If 'obj' hasn't changed, it must
                                                // be after an 'Undo'  Try to preserve
                                                // the selected palette colours, etc.

  inherited;                                    // Must call inherited to set
                                                // up underlying stuff.

  PropertyListBox.Reset;
  FDetails := obj as TGraphicsResourceDetails;

  FDetails.GetImage(Image.Picture);            // Set the thumbnail picture
  if Image.Picture.Graphic is TGifImage then
  begin
//    Transp := TGIFImage(Image.Picture.Graphic).IsTransparent;
    Transp := False;                            // Can't get transparent GIFs
                                                // to work in initial D2006 release
    Image.Transparent := Transp
  end
  else
    Transp := Image.Picture.Graphic.Transparent;

  BitmapEditor.Picture := Image.Picture;      // Set the editor picture

                                                // Setting BitmapEditor.Picture may
                                                // Change the TransparentColor...

  ScrollBoxThumbnail.Cursor := crDefault;

  if Transp then     // Set the 'Transparent' Color panel
  begin
    PanelTransparent.Color := BitmapEditor.TransparentColor;
    PanelTransparent.Visible := True
  end
  else
    PanelTransparent.Visible := False;

  with PropertyListBox do                      // Set the properties
  begin

    with FindProperty(rstWidth) do
    begin
      Tag := taWidth;                           // Save taWidth constant in properties
      PropertyValue := Image.Picture.Width     // tag so we can use the 'OnPropertyChanged' event
    end;

    with FindProperty(rstHeight) do
    begin
      Tag := taHeight;
      PropertyValue := Image.Picture.Height
    end;

    with FindProperty(rstPixelFormat) do
    begin
      Tag := taPixelFormat;

      case GetPixelFormat(Image.Picture.Graphic) of
        pf1Bit: PropertyValue := 0;
        pf4Bit: PropertyValue := 1;
        pf8Bit: PropertyValue := 2;
        pf24Bit: PropertyValue := 3;
        pf32Bit: propertyValue := 4;
        else
          PropertyValue := 3; // 24 bit ??
      end;
    end
  end;

  SetPaletteForPixelFormat(NewImage)
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.FormShow                                         |
 |                                                                      |
 | Initialize the form.                                                 |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.FormCreate(Sender: TObject);
begin
  PropertyListBox := TPropertyListBox.Create(PanelLeft);
  PropertyListBox.Name := 'PropertyListBox1';
  PropertyListBox.Parent := PanelLeft;
  PropertyListBox.Left := 1;
  PropertyListBox.Top := 1;
  PropertyListBox.Width := 183;
  PropertyListBox.Height := 120;
  PropertyListBox.Align := alTop;
  PropertyListBox.BorderStyle := bsNone;
  PropertyListBox.PopupMenu := PopupMenu;
  PropertyListBox.TabOrder := 1;
  PropertyListBox.TabStop := False;
  with TPropertyListProperty(PropertyListBox.Properties.Add) do
  begin
    PropertyName := 'Width';
    PropertyType := ptInteger;
    Tag := 0;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
  with TPropertyListProperty(PropertyListBox.Properties.Add) do
  begin
    PropertyName := 'Height';
    PropertyType := ptInteger;
    Tag := 0;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
  with TPropertyListProperty(PropertyListBox.Properties.Add) do
  begin
    PropertyName := 'Pixel Format';
    PropertyType := ptEnum;
    Tag := 0;
    EnumValues.Add('1 Bit');
    EnumValues.Add('4 Bit');
    EnumValues.Add('8 Bit');
    EnumValues.Add('24 Bit');
    EnumValues.Add('32 Bit');
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
  PropertyListBox.ActualValueColWidth := 0;
  PropertyListBox.OnPropertyChanged := PropertyListBoxPropertyChanged;
end;

procedure TFormGraphicsResource.FormShow(Sender: TObject);
begin
  ScrollBoxThumbnail.DoubleBuffered := True;

  ColorSelector.ForegroundColor := clWhite;    // Set the initial drawing colours
  ColorSelector.BackgroundColor := clBlack;

  ShapeFore.Brush.Color := ColorSelector.ForegroundColor;
  ShapeBack.Brush.Color := ColorSelector.BackgroundColor;

  BitmapEditor.DrawPen.Color := ColorSelector.ForegroundColor;
  BitmapEditor.DrawBrush.Color := ColorSelector.BackgroundColor;
  BitmapEditor.DrawingTool := dtPencil;
                                                // Save the palette panel Width,
                                                // so we can restore it if we dock.
  FPCWidth := PanelGraphics.Width;
  if PanelColors.Width > FPCWidth then
    FPCWidth := PanelColors.Width;


                                                // Manually dock the panels
  PanelGraphics.ManualDock(SizingPageControl, nil, alNone);
  PanelColors.ManualDock(SizingPageControl, nil, alNone);
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.SizingPageControl1DockDrop                       |
 |                                                                      |
 | When a panel is dropped on the page control (which creates a new     |
 | tab) - set the tab's caption                                         |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.SizingPageControlDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  i: Integer;
begin
  with SizingPageControl do
  begin
    for i := 0 to PageCount - 1 do
      Pages[i].Caption := TPanel (Pages[i].Controls[0]).Caption;

    Width := FPCWidth + 8;      // Restore the width to it's original setting
                                // - we've got at least one tab.
  end
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.SizingPageControl1UnDock                         |
 |                                                                      |
 | A panel is being undocked.  Set size to 0 if it's the last one.      |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.SizingPageControlUnDock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
                                // If we're undocking the last tab, set the
                                // width to 0
  if SizingPageControl.PageCount = 1 then
    SizingPageControl.Width := 0;
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.ColorSelector1ColorSelect                        |
 |                                                                      |
 | A new color (foreground or background) colour has been selected...   |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.ColorSelectorColorSelect(Sender: TObject);
begin
  BitmapEditor.DrawPen.Color := ColorSelector.ForegroundColor;
  BitmapEditor.DrawBrush.Color := ColorSelector.BackgroundColor;
  ShapeFore.Brush.Color := ColorSelector.ForegroundColor;
  ShapeBack.Brush.Color := ColorSelector.BackgroundColor;
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.tbPencilClick                                    |
 |                                                                      |
 | Not just the pencil - used by all drawing tool buttons.              |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.tbPencilClick(Sender: TObject);
begin
  if Sender is TToolButton then
    BitmapEditor.DrawingTool := TDrawingTool (TToolButton (Sender).Tag);
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.actImageZoomInExecute                            |
 |                                                                      |
 | Zoom In                                                              |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.ActionImageZoomInExecute(Sender: TObject);
begin
  BitmapEditor.ZoomIn
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.actImageZoomOutExecute                           |
 |                                                                      |
 | Zoom out.                                                            |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.ActionImageZoomOutExecute(Sender: TObject);
begin
  BitmapEditor.ZoomOut
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.GetMenuItem                                      |
 |                                                                      |
 | Return the main menu item to merge                                   |
 *----------------------------------------------------------------------*)
function TFormGraphicsResource.GetMenuItem: TMenuItem;
begin
  Result := MenuItemImage
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.ColorSelector1DblClick                           |
 |                                                                      |
 | Colour selector double clicked.  Execute the color dialog so they    |
 | Change the palette.                                                  |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.ColorSelectorDblClick(Sender: TObject);
begin
  ColorDialog.Color := BitmapEditor.DrawPen.Color;
  if ColorDialog.Execute then
  begin
                        // Adjust the palette in the colour selector

    ColorSelector.SetSelectedPaletteColor (ColorDialog.Color);

                        // Change the thumbnail palette...
    Image.Picture.Graphic.Palette := ColorSelector.Palette;
                        // Reload the editor to show the changed thumbnail
    BitmapEditor.Picture := Image.Picture;
                        // Update the resource
    SaveResource(rstPaletteChange);

                        // Update the pen
    BitmapEditor.DrawPen.Color := ColorSelector.GetSelectedPaletteColor;
    ShapeFore.Brush.Color := ColorSelector.GetSelectedPaletteColor;

    BitmapEditor.Invalidate;
  end
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.SaveResource                                     |
 |                                                                      |
 | Take an undo snapshot, then update the image in the resource         |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.SaveResource(const undoDetails: string);
var
  FDetails: TGraphicsResourceDetails;
begin
  AddUndoEntry(undoDetails);           // Call inherited to take undo snapshot
  FDetails := obj as TGraphicsResourceDetails;
  FDetails.SetImage(BitmapEditor.Picture);
  FDetails.GetImage(Image.Picture);    // Make sure the thumnail *really*
                                        // relflects what we've got..
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.BitmapEditor1EndChange                           |
 |                                                                      |
 | Update the resource graphic only at end of a drawing.  That way we   |
 | don't get an 'undo' for every pixel changed.                         |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.BitmapEditorEndChange(Sender: TObject);
begin
  SaveResource(BitmapEditor.GetDrawingChangeDescription);
end;

procedure TFormGraphicsResource.BitmapEditorGetText(Sender: TObject; Font: TFont;
  var txt: WideString);
var
  TextInputForm: TFormTextInput;
begin
  TextInputForm := TFormTextInput.Create(nil);
  try
    TextInputForm.mmoText.Font.Assign(Font);
    if TextInputForm.ShowModal = mrOK then
    begin
      txt := TextInputForm.mmoText.Text;
      Font.Assign (TextInputForm.mmoText.Font)
    end
    else
      txt := ''
  finally
    TextInputForm.Free
  end;
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.BitmapEditor1Change                              |
 |                                                                      |
 | While we're drawing, update the thumbnail too (but not the           |
 | underlying graphic resource.                                         |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.BitmapEditorChange(Sender: TObject);
begin
  inherited;
  Image.Picture.Graphic.Assign (BitmapEditor.DrawBmp);
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.pnlTransparentMouseDown                          |
 |                                                                      |
 | Select the transparent color if the 'transparent color' panel is     |
 | clicked.                                                             |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.PanelTransparentMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tc: TColor;
begin
  tc := BitmapEditor.TransparentColor;
  if Button = mbLeft then
    BitmapEditor.DrawPen.Color := tc
  else
    if Button = mbRight then
      BitmapEditor.DrawBrush.Color := tc;

  ShapeFore.Brush.Color := BitmapEditor.DrawPen.Color;
  ShapeBack.Brush.Color := BitmapEditor.DrawBrush.Color;

  ColorSelector.ForegroundColor := ShapeFore.Brush.Color;
  ColorSelector.BackgroundColor := ShapeBack.Brush.Color;
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.pnlTransparentDblClick                           |
 |                                                                      |
 | Allow them to Change the color displayed in 'transparent' areas.     |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.PanelTransparentDblClick(Sender: TObject);
begin
  inherited;
  ColorDialog.Color := BitmapEditor.TransparentColor;
  if ColorDialog.Execute then
  begin
    BitmapEditor.TransparentColor := ColorDialog.Color;
    PanelTransparent.Color := ColorDialog.Color
  end
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.BitmapEditor1DrawToolChange                      |
 |                                                                      |
 | The bitmap editor has (itself) changed the drawing tool.  (After     |
 | using the dropper it reverts back to the previous tool...)           |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.BitmapEditorDrawToolChange(Sender: TObject);
var
  dt: TDrawingTool;
  i: Integer;
begin
  dt := BitmapEditor.DrawingTool;

                        // Make sure the new drawing tool's button is pressed
  for i := 0 to Toolbar.ControlCount - 1 do
    if (Toolbar.Controls[i] is TToolButton) and (Toolbar.Controls[i].Tag = Ord (dt)) then
    begin
      TToolButton (Toolbar.Controls[i]).Down := True;
      break
    end;

                        // Using the dropper causes the colour to Change, too
  ShapeFore.Brush.Color := BitmapEditor.DrawPen.Color;
  ShapeBack.Brush.Color := BitmapEditor.DrawBrush.Color;
end;

function CreatePaletteBitmap (pf: TPixelFormat): TBitmap;
var
  i: Integer;
  ColorCount: DWORD;
  paletteEntries: array [0..255] of TPaletteEntry;
  pal: HPalette;
begin
  Result := nil;
  case pf of
    pf1Bit: pal := SystemPalette2;
    pf4Bit: pal := SystemPalette16;
    pf8Bit: pal := SystemPalette256;
    else
      Exit;
  end;

  ColorCount := 0;
  if GetObject(pal, SizeOf(ColorCount), @ColorCount) = 0 then
    RaiseLastOSError;

  if ColorCount = 0 then
  begin
    Result := nil;
    exit
  end;

  i := GetPaletteEntries (pal, 0, ColorCount, paletteEntries);
  if i = 0 then
    RaiseLastOSError;

  Result := TBitmap.Create;

  Result.PixelFormat := pf;
  Result.Palette := CopyPalette(pal);

  Result.Height := 1;
  Result.Width := ColorCount;

  for i := 0 to ColorCount - 1 do
    Result.Canvas.Pixels[i, 0] := RGB (paletteEntries[i].peRed, paletteEntries[i].peGreen, paletteEntries[i].peBlue);
end;

(*----------------------------------------------------------------------*
 | TFormGraphicsResource.PropertyListBox1PropertyChanged                  |
 |                                                                      |
 | A property(width, height, pixelformat) has been changed.  Update    |
 |the image                                                             |
 *----------------------------------------------------------------------*)
procedure TFormGraphicsResource.PropertyListBoxPropertyChanged(
  Sender: TObject);
var
  Prop: TPropertyListProperty;
  Change: string;
  OldPixelFormat, NewPixelFormat: TPixelFormat;
  b1, bmp, b2: TBitmap;
  l: TList;
  pal: HPalette;
begin
  with PropertyListBox do
    Prop := Properties[SelectedPropertyNo];

  Change := '';
  NewPixelFormat := pfDevice;

  case Prop.Tag of
    taWidth: if Image.Picture.Graphic.Width <> Prop.PropertyValue then
              begin
                Change := rstWidthChanged;
                ResizePicture(Image.Picture, Prop.PropertyValue, Image.Picture.Graphic.Height);
              end;

    taHeight: if Image.Picture.Graphic.Height <> Prop.PropertyValue then
               begin
                 Change := rstHeightChanged;
                 ResizePicture(Image.Picture, Image.Picture.Graphic.Width, Prop.PropertyValue);
               end;

    taPixelFormat :
               begin
                 OldPixelFormat := GetPixelFormat(Image.Picture.Graphic);
                 case Prop.PropertyValue of
                   0: NewPixelFormat := pf1Bit;
                   1: NewPixelFormat := pf4Bit;
                   2: NewPixelFormat := pf8Bit;
                   3: NewPixelFormat := pf24Bit;
                   4: NewPixelFormat := pf32Bit;
                 end;
                 if OldPixelFormat <> NewPixelFormat then
                 begin
                   Change := rstPixelFormatChanged;

                   if Image.Picture.Graphic is TBitmap then
                   begin
                     bmp := TBitmap (Image.Picture.Graphic);

                     if NewPixelFormat in [pf1Bit..pf8Bit] then
                     begin
                       b1 := nil;
                       b2 := nil;
                       try
                         if (NewPixelFormat = pf8Bit) and (OldPixelFormat > pf8Bit) and not bmp.Empty then
                         begin
                           b1:= ReduceColors (bmp, rmQuantizeWindows, dmFloydSteinberg, GetPixelFormatBitCount(NewPixelFormat), 0);
                         end
                         else
                         begin
                           // ReduceColors always returns a pf8Bit bitmap...
                           b1 := TBitmap.Create;
                           b1.PixelFormat := NewPixelFormat;
                           case NewPixelFormat of
                             pf1Bit: b1.Palette := SystemPalette2;
                             pf4Bit: b1.Palette := SystemPalette16;
                             pf8Bit: b1.Palette := CopyPalette(SystemPalette256); // unitExIcon.WebPalette
                           end;

                           b1.Width := bmp.Width;
                           b1.Height := bmp.Height;
                           b1.Canvas.Draw(0, 0, bmp);
                         end;
                         Image.Picture.Graphic := b1;
                         bmp := TBitmap (Image.Picture.Graphic);
                       finally
                         b1.Free;
                         b2.Free
                       end
                     end
                     else
                       bmp.PixelFormat := NewPixelFormat;

                     bmp.IgnorePalette := NewPixelFormat > pf8Bit;
                   end
                   else
                     if Image.Picture.Graphic is TExIconCursor then
                       if NewPixelFormat in [pf24Bit, pf32Bit] then
                         TExIconCursor (Image.Picture.Graphic).PixelFormat := NewPixelFormat
                       else
                       begin
                         pal := 0;
                         bmp := nil;
                         l := TList.Create;
                         try
                           bmp := TBitmap.Create;
                           bmp.Assign(Image.Picture.Graphic);
                           l.Add (bmp);
                           l.Add (CreatePaletteBitmap (NewPixelFormat));
                           pal := CreateOptimizedPaletteFromManyBitmaps (l, GetPixelFormatNumColors (NewPixelFormat), GetPixelFormatBitCount(NewPixelFormat), False);
                           TExIconCursor (Image.Picture.Graphic).Palette := pal;
                         finally
                           l.Free;
                           bmp.Free;
                           if pal <> 0 then
                             DeleteObject(pal)
                         end
                       end
                 end;

                 SetPaletteForPixelFormat(NewPixelFormat < OldPixelFormat);
               end
  end;

  if Change <> '' then
  begin
    BitmapEditor.Picture := Image.Picture;      // Set the editor picture
    SaveResource(Change);
  end
end;

procedure TFormGraphicsResource.TrackBarChange(Sender: TObject);
begin
  ColorSelector.Luminescence := 240 - TrackBar.Position
end;

function TFormGraphicsResource.GetImportExportType: TImportExportType;
begin
  Result := ixPicture
end;

procedure TFormGraphicsResource.SetPaletteForPixelFormat(reset: Boolean);
var
  fc, bc: TColor;
  pf: TPixelFormat;
begin
  fc := clWhite;
  bc := clBlack;
  pf := GetPixelFormat(Image.Picture.Graphic);
  if pf in [pf1Bit..pf8Bit] then
  begin
    ColorSelector.Palette := Image.Picture.Graphic.Palette;
    fc := ColorSelector.Color [ColorSelector.ColorCount - 1];
    bc := ColorSelector.Color [0]
  end
  else
    ColorSelector.Palette := 0;

  BitmapEditor.ClipboardPalette := ColorSelector.Palette;
  BitmapEditor.ClipboardPixelFormat := pf;
                                                // Adjust colour selector Size, etc.
  if ColorSelector.ColorCount = -1 then
    TrackBar.Visible := True
  else
  begin
    TrackBar.Visible := False;
    if ColorSelector.ColorCount <= 16 then
      ColorSelector.ColumnCount := 4
    else
      ColorSelector.ColumnCount := 8
  end;

  if reset then
  begin
    ColorSelector.ForegroundColor := fc;
    ColorSelector.BackgroundColor := bc;

    ShapeFore.Brush.Color := fc;
    ShapeBack.Brush.Color := bc;

    BitmapEditor.DrawPen.Color := fc;
    BitmapEditor.DrawBrush.Color := bc
  end
end;

function TFormGraphicsResource.GetCanCopy: Boolean;
begin
  Result := BitmapEditor.SelectionValid
end;

function TFormGraphicsResource.GetCanCut: Boolean;
begin
  Result := BitmapEditor.SelectionValid
end;

function TFormGraphicsResource.GetCanPaste: Boolean;
begin
  Result := Clipboard.HasFormat(CF_METAFILEPICT) or Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_PICTURE)
end;

function TFormGraphicsResource.GetCanSelectAll: Boolean;
begin
  Result := True
end;

procedure TFormGraphicsResource.SelectAll;
begin
  BitmapEditor.SelectAll
end;

function TFormGraphicsResource.GetCanDelete: Boolean;
begin
  Result := BitmapEditor.SelectionValid
end;

procedure TFormGraphicsResource.EditDelete;
begin
  SaveResource(rstDeleteImage);
  BitmapEditor.DeleteSelection
end;

procedure TFormGraphicsResource.Copy;
begin
  BitmapEditor.CopySelection
end;

procedure TFormGraphicsResource.Cut;
begin
  SaveResource(rstCutImage);
  BitmapEditor.CutSelection
end;

procedure TFormGraphicsResource.Paste;
begin
  if not BitmapEditor.SelectionValid then
    BitmapEditor.SelectAll;
  BitmapEditor.PasteSelection;
  SaveResource(rstPasteImage);
end;

procedure TFormGraphicsResource.PreviewKey(var key: Word;
  shift: TShiftState);
begin

  case key of
    VK_ADD :
      begin
        ActionImageZoomIn.Execute;
        key := 0
      end;

    VK_SUBTRACT :
      begin
        ActionImageZoomOut.Execute;
        key := 0
      end
  end

end;

procedure TFormGraphicsResource.ActionImageColorsPaletteExecute(
  Sender: TObject);
begin
  PanelColors.Visible := not PanelColors.Visible;
end;

procedure TFormGraphicsResource.ActionImageToolsPaletteExecute(Sender: TObject);
begin
  PanelGraphics.Visible := not PanelGraphics.Visible
end;

procedure TFormGraphicsResource.MenuItemImageClick(Sender: TObject);
begin
  ActionImageToolsPalette.Checked := PanelGraphics.Visible;
  ActionImageColorsPalette.Checked := PanelColors.Visible
end;

procedure TFormGraphicsResource.BitmapEditorSelectionRectChange(
  Sender: TObject);
var
  Msg: string;
begin
  if BitmapEditor.SelectionValid then
    with BitmapEditor.SelectionRect do
      Msg := Format('%d,%d %dx%d', [Left, Top, Right - Left + 1, Bottom - Top + 1])
  else
    Msg := '';

  SendMessage(Application.MainForm.Handle, WM_STATUSBAR, 0, Integer (PChar (Msg)))
end;

procedure TFormGraphicsResource.PopupMenuPopup(Sender: TObject);
begin
  ActionImageToolsPalette.Checked := PanelGraphics.Visible;
  ActionImageColorsPalette.Checked := PanelColors.Visible
end;

procedure TFormGraphicsResource.ActionImageAddImageExecute(Sender: TObject);
var
  tp: Integer;
begin
  if Image.Picture.Graphic is TExCursor then
    tp := 0
  else
    if Image.Picture.Graphic is TExIcon then
      tp := 1
    else
      tp := -1;

  SendMessage(Application.MainForm.Handle, WM_ADDIMAGERESOURCE, tp, 0);
end;

procedure TFormGraphicsResource.UpdateActions;
var
  dt: Boolean;
begin
  ActionImageAddImage.Enabled := ResourceDetails is TIconCursorResourceDetails;

  // Disable the right-click popup menu if the dropper or
  // magnifier is selected.

  dt := BitmapEditor.DrawingTool in [dtDropper, dtMagnifier];
  if dt then
    PopupMenu.AutoPopup := False
  else
    if PopupMenu.AutoPopup = False then
      if not (ssRight in KeyboardStateToShiftState) then
        PopupMenu.AutoPopup := True
end;

end.
