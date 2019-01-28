(*======================================================================*
 | GraphicsResourceForm                                                 |
 |                                                                      |
 | Remember:                                                            |
 |                                                                      |
 | The thumbnail image (Image1) contains the master image.  It is in    |
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
  ResourceForm, cmpPropertyListBox, ExtCtrls, cmpBitmapEditor, unitResourceGraphics,
  cmpColorSelector, unitExIcon, ComCtrls, ImgList, ToolWin,
  cmpSizingPageControl, ActnList, Menus, GifImage;

const
  WM_STATUSBAR = WM_USER + $203;
  WM_ADDIMAGERESOURCE = WM_USER + $204;

type
  TfmGraphicsResource = class(TfmResource)
    pnlLeft: TPanel;
    Splitter1: TSplitter;
    PropertyListBox1: TPropertyListBox;
    Splitter2: TSplitter;
    sbThumbnail: TScrollBox;
    Image1: TImage;
    Panel1: TPanel;
    ScrollBox2: TScrollBox;
    BitmapEditor1: TBitmapEditor;
    SizingPageControl1: TSizingPageControl;
    pnlGraphics: TPanel;
    ImageList1: TImageList;
    pnlColours: TPanel;
    shpBack: TShape;
    shpFore: TShape;
    ColorSelector1: TColorSelector;
    pnlTransparent: TPanel;
    TrackBar1: TTrackBar;
    ToolBar1: TToolBar;
    tbSelectRect: TToolButton;
    tbSelectShape: TToolButton;
    tbDropper: TToolButton;
    tbEraser: TToolButton;
    tbFloodFill: TToolButton;
    tbMagnifier: TToolButton;
    tbPencil: TToolButton;
    tbBrush: TToolButton;
    tbAirbrush: TToolButton;
    ToolButton10: TToolButton;
    ToolButton12: TToolButton;
    tbFrameRect: TToolButton;
    tbFillRect: TToolButton;
    tbRect: TToolButton;
    tbFrameRoundRect: TToolButton;
    tbFillRoundRect: TToolButton;
    tbRoundRect: TToolButton;
    tbFrameEllipse: TToolButton;
    tbFillEllipse: TToolButton;
    tbEllipse: TToolButton;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    mnuImage: TMenuItem;
    actImageZoomIn: TAction;
    actImageZoomOut: TAction;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    ColorDialog1: TColorDialog;
    actImageToolsPalette: TAction;
    actImageColorsPalette: TAction;
    N1: TMenuItem;
    ToolPalettes1: TMenuItem;
    ColorsPalette1: TMenuItem;
    ToolsPalette1: TMenuItem;
    actImageAddImage: TAction;
    AddImage1: TMenuItem;
    N2: TMenuItem;
    PopupMenu1: TPopupMenu;
    AddImage2: TMenuItem;
    N3: TMenuItem;
    ZoomIn2: TMenuItem;
    ZoomOut2: TMenuItem;
    N4: TMenuItem;
    ToolPalettes2: TMenuItem;
    ColorsPalette2: TMenuItem;
    DrawingToolsPalette1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure BitmapEditor1GetText(sender: TObject; font: TFont;
      var txt: WideString);
    procedure FormShow(Sender: TObject);
    procedure SizingPageControl1DockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure SizingPageControl1UnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure ColorSelector1ColorSelect(Sender: TObject);
    procedure tbPencilClick(Sender: TObject);
    procedure actImageZoomInExecute(Sender: TObject);
    procedure actImageZoomOutExecute(Sender: TObject);
    procedure ColorSelector1DblClick(Sender: TObject);
    procedure BitmapEditor1EndChange(Sender: TObject);
    procedure BitmapEditor1Change(Sender: TObject);
    procedure pnlTransparentMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlTransparentDblClick(Sender: TObject);
    procedure BitmapEditor1DrawToolChange(Sender: TObject);
    procedure PropertyListBox1PropertyChanged(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure actImageColorsPaletteExecute(Sender: TObject);
    procedure actImageToolsPaletteExecute(Sender: TObject);
    procedure mnuImageClick(Sender: TObject);
    procedure BitmapEditor1SelectionRectChange(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure actImageAddImageExecute(Sender: TObject);
  private
    fPCWidth : Integer;
    details : TGraphicsResourceDetails;

    procedure SetPaletteForPixelFormat (reset : Boolean);
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem : TMenuItem; override;
    function GetImportExportType: TImportExportType; override;

    function GetCanCut : Boolean; override;
    function GetCanCopy : Boolean; override;
    function GetCanPaste : Boolean; override;
    function GetCanSelectAll : Boolean; override;
    function GetCanDelete : Boolean; override;
    procedure UpdateActions; override;

  public
    procedure PreviewKey (var key : Word; shift : TShiftState); override;
    procedure SaveResource (const undoDetails : string); virtual;
    procedure SelectAll; override;
    procedure EditDelete; override;
    procedure Copy; override;
    procedure Cut; override;
    procedure Paste; override;
  end;

var
  fmGraphicsResource: TfmGraphicsResource;

implementation

uses ClipBrd, Jpeg, FormTextInput;

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

function GetPixelFormat (graphic : TGraphic) : TPixelFormat;
begin
  if graphic is TGifImage then
  begin
    case TGifImage (graphic).BitsPerPixel of
      1 : Result := pf1Bit;
      4 : Result := pf4Bit;
      else Result := pf8Bit;
    end
  end
  else
    Result := unitExIcon.GetPixelFormat (graphic)
end;

(*----------------------------------------------------------------------*
 | ResizePicture                                                        |
 |                                                                      |
 | Resize a picture, handling anomolies like JPEG                       |
 |                                                                      |
 | Parameters:                                                          |
 |   p : TPicture;        The picture to resize                         |
 |   newWidth : Integer   The new width                                 |
 |   newHeight : Integer  The new height                                |
 *----------------------------------------------------------------------*)
procedure ResizePicture (p : TPicture; newWidth, newHeight : Integer);
var
  b : TBitmap;
begin
  if (p.graphic is TJPegImage) or (p.Graphic is TGifImage) then
  begin
    b := TBitmap.Create;
    try
      b.Width := newWidth;
      b.Height := newHeight;
      b.PixelFormat := pf24Bit;
      b.Canvas.StretchDraw(Rect (0, 0, newWidth, newHeight), p.Graphic);
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

{ TfmGraphicsResource }

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.SetObject                                        |
 |                                                                      |
 | Called when the resource changes - when a new one is loaded, or      |
 | after undo/redo.                                                     |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.SetObject(const Value: TObject);
var
  newImage : Boolean;
  transp : boolean;
begin
  newImage := Value <> Obj;                     // If 'obj' hasn't changed, it must
                                                // be after an 'Undo'  Try to preserve
                                                // the selected palette colours, etc.

  inherited;                                    // Must call inherited to set
                                                // up underlying stuff.

  PropertyListBox1.Reset;
  details := obj as TGraphicsResourceDetails;

  details.GetImage (Image1.Picture);            // Set the thumbnail picture
  if Image1.Picture.Graphic is TGifImage then
  begin
//    transp := TGIFImage (Image1.Picture.Graphic).IsTransparent;
    transp := False;                            // Can't get transparent GIFs
                                                // to work in initial D2006 release
    Image1.Transparent := transp
  end
  else
    transp := Image1.Picture.Graphic.Transparent;

  BitmapEditor1.Picture := Image1.Picture;      // Set the editor picture

                                                // Setting BitmapEditor1.Picture may
                                                // change the TransparentColor...

  sbThumbnail.Cursor := crDefault;

  if transp then     // Set the 'Transparent' Color panel
  begin
    pnlTransparent.Color := BitmapEditor1.TransparentColor;
    pnlTransparent.Visible := True
  end
  else
    pnlTransparent.Visible := False;

  with PropertyListBox1 do                      // Set the properties
  begin

    with FindProperty (rstWidth) do
    begin
      Tag := taWidth;                           // Save taWidth constant in properties
      PropertyValue := Image1.Picture.Width     // tag so we can use the 'OnPropertyChanged' event
    end;

    with FindProperty (rstHeight) do
    begin
      Tag := taHeight;
      PropertyValue := Image1.Picture.Height
    end;

    with FindProperty (rstPixelFormat) do
    begin
      Tag := taPixelFormat;

      case GetPixelFormat (Image1.Picture.Graphic) of
        pf1Bit : PropertyValue := 0;
        pf4Bit : PropertyValue := 1;
        pf8Bit : PropertyValue := 2;
        pf24Bit : PropertyValue := 3;
        pf32Bit : propertyValue := 4;
        else
          PropertyValue := 3; // 24 bit ??
      end;
    end
  end;

  SetPaletteForPixelFormat (newImage)
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.FormShow                                         |
 |                                                                      |
 | Initialize the form.                                                 |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.FormShow(Sender: TObject);
begin
  sbThumbnail.DoubleBuffered := True;

  ColorSelector1.ForegroundColor := clWhite;    // Set the initial drawing colours
  ColorSelector1.BackgroundColor := clBlack;

  shpFore.Brush.Color := ColorSelector1.ForegroundColor;
  shpBack.Brush.Color := ColorSelector1.BackgroundColor;

  BitmapEditor1.DrawPen.Color := ColorSelector1.ForegroundColor;
  BitmapEditor1.DrawBrush.Color := ColorSelector1.BackgroundColor;
  BitmapEditor1.DrawingTool := dtPencil;
                                                // Save the palette panel Width,
                                                // so we can restore it if we dock.
  fPCWidth := pnlGraphics.Width;
  if pnlColours.Width > fPCWidth then
    fPCWidth := pnlColours.Width;


                                                // Manually dock the panels
  pnlGraphics.ManualDock (SizingPageControl1, Nil, alNone);
  pnlColours.ManualDock (SizingPageControl1, Nil, alNone);
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.SizingPageControl1DockDrop                       |
 |                                                                      |
 | When a panel is dropped on the page control (which creates a new     |
 | tab) - set the tab's caption                                         |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.SizingPageControl1DockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  i : Integer;
begin
  with SizingPageControl1 do
  begin
    for i := 0 to PageCount - 1 do
      Pages [i].Caption := TPanel (Pages [i].Controls [0]).Caption;

    Width := fPCWidth + 8;      // Restore the width to it's original setting
                                // - we've got at least one tab.
  end
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.SizingPageControl1UnDock                         |
 |                                                                      |
 | A panel is being undocked.  Set size to 0 if it's the last one.      |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.SizingPageControl1UnDock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
                                // If we're undocking the last tab, set the
                                // width to 0
  if SizingPageControl1.PageCount = 1 then
    SizingPageControl1.Width := 0;
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.ColorSelector1ColorSelect                        |
 |                                                                      |
 | A new color (foreground or background) colour has been selected...   |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.ColorSelector1ColorSelect(Sender: TObject);
begin
  BitmapEditor1.DrawPen.Color := ColorSelector1.ForegroundColor;
  BitmapEditor1.DrawBrush.Color := ColorSelector1.BackgroundColor;
  shpFore.Brush.Color := ColorSelector1.ForegroundColor;
  shpBack.Brush.Color := ColorSelector1.BackgroundColor;
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.tbPencilClick                                    |
 |                                                                      |
 | Not just the pencil - used by all drawing tool buttons.              |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.tbPencilClick(Sender: TObject);
begin
  if Sender is TToolButton then
    BitmapEditor1.DrawingTool := TDrawingTool (TToolButton (sender).Tag);
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.actImageZoomInExecute                            |
 |                                                                      |
 | Zoom In                                                              |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.actImageZoomInExecute(Sender: TObject);
begin
  BitmapEditor1.ZoomIn
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.actImageZoomOutExecute                           |
 |                                                                      |
 | Zoom out.                                                            |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.actImageZoomOutExecute(Sender: TObject);
begin
  BitmapEditor1.ZoomOut
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.GetMenuItem                                      |
 |                                                                      |
 | Return the main menu item to merge                                   |
 *----------------------------------------------------------------------*)
function TfmGraphicsResource.GetMenuItem: TMenuItem;
begin
  result := mnuImage
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.ColorSelector1DblClick                           |
 |                                                                      |
 | Colour selector double clicked.  Execute the color dialog so they    |
 | change the palette.                                                  |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.ColorSelector1DblClick(Sender: TObject);
begin
  ColorDialog1.Color := BitmapEditor1.DrawPen.Color;
  If ColorDialog1.Execute then
  begin
                        // Adjust the palette in the colour selector

    ColorSelector1.SetSelectedPaletteColor (ColorDialog1.Color);

                        // Change the thumbnail palette...
    Image1.Picture.Graphic.Palette := ColorSelector1.Palette;
                        // Reload the editor to show the changed thumbnail
    BitmapEditor1.Picture := Image1.Picture;
                        // Update the resource
    SaveResource (rstPaletteChange);

                        // Update the pen
    BitmapEditor1.DrawPen.Color := ColorSelector1.GetSelectedPaletteColor;
    shpFore.Brush.Color := ColorSelector1.GetSelectedPaletteColor;

    BitmapEditor1.Invalidate;
  end
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.SaveResource                                     |
 |                                                                      |
 | Take an undo snapshot, then update the image in the resource         |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.SaveResource(const undoDetails: string);
var
  details : TGraphicsResourceDetails;
begin
  AddUndoEntry (undoDetails);           // Call inherited to take undo snapshot
  details := obj as TGraphicsResourceDetails;
  details.SetImage (BitmapEditor1.Picture);
  details.GetImage (Image1.Picture);    // Make sure the thumnail *really*
                                        // relflects what we've got..
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.BitmapEditor1EndChange                           |
 |                                                                      |
 | Update the resource graphic only at end of a drawing.  That way we   |
 | don't get an 'undo' for every pixel changed.                         |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.BitmapEditor1EndChange(Sender: TObject);
begin
  SaveResource (BitmapEditor1.GetDrawingChangeDescription);
end;

procedure TfmGraphicsResource.BitmapEditor1GetText(sender: TObject; font: TFont;
  var txt: WideString);
var
  frm : TfmTextInput;
begin
  frm := TfmTextInput.Create(nil);
  try
    frm.mmoText.Font.Assign(font);
    if frm.ShowModal = mrOK then
    begin
      txt := frm.mmoText.Text;
      font.Assign (frm.mmoText.Font)
    end
    else
      txt := ''
  finally
    frm.Free
  end;
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.BitmapEditor1Change                              |
 |                                                                      |
 | While we're drawing, update the thumbnail too (but not the           |
 | underlying graphic resource.                                         |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.BitmapEditor1Change(Sender: TObject);
begin
  inherited;
  Image1.Picture.Graphic.Assign (BitmapEditor1.DrawBmp);
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.pnlTransparentMouseDown                          |
 |                                                                      |
 | Select the transparent color if the 'transparent color' panel is     |
 | clicked.                                                             |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.pnlTransparentMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tc : TColor;
begin
  tc := BitmapEditor1.TransparentColor;
  if Button = mbLeft then
    BitmapEditor1.DrawPen.Color := tc
  else
    if Button = mbRight then
      BitmapEditor1.DrawBrush.Color := tc;

  shpFore.Brush.Color := BitmapEditor1.DrawPen.Color;
  shpBack.Brush.Color := BitmapEditor1.DrawBrush.Color;

  ColorSelector1.ForegroundColor := shpFore.Brush.Color;
  ColorSelector1.BackgroundColor := shpBack.Brush.Color;
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.pnlTransparentDblClick                           |
 |                                                                      |
 | Allow them to change the color displayed in 'transparent' areas.     |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.pnlTransparentDblClick(Sender: TObject);
begin
  inherited;
  ColorDialog1.Color := BitmapEditor1.TransparentColor;
  If ColorDialog1.Execute then
  begin
    BitmapEditor1.TransparentColor := ColorDialog1.Color;
    pnlTransparent.Color := ColorDialog1.Color
  end
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.BitmapEditor1DrawToolChange                      |
 |                                                                      |
 | The bitmap editor has (itself) changed the drawing tool.  (After     |
 | using the dropper it reverts back to the previous tool...)           |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.BitmapEditor1DrawToolChange(Sender: TObject);
var
  dt : TDrawingTool;
  i : Integer;
begin
  dt := BitmapEditor1.DrawingTool;

                        // Make sure the new drawing tool's button is pressed
  for i := 0 to Toolbar1.ControlCount - 1 do
    if (Toolbar1.Controls [i] is TToolButton) and (Toolbar1.Controls [i].Tag = Ord (dt)) then
    begin
      TToolButton (Toolbar1.Controls [i]).Down := True;
      break
    end;

                        // Using the dropper causes the colour to change, too
  shpFore.Brush.Color := BitmapEditor1.DrawPen.Color;
  shpBack.Brush.Color := BitmapEditor1.DrawBrush.Color;
end;

function CreatePaletteBitmap (pf : TPixelFormat) : TBitmap;
var
  i : Integer;
  colorCount : DWORD;
  paletteEntries : array [0..255] of TPaletteEntry;
  pal : HPalette;
begin
  result := Nil;
  case pf of
    pf1Bit : pal := SystemPalette2;
    pf4Bit : pal := SystemPalette16;
    pf8Bit : pal := SystemPalette256;
    else
      Exit;
  end;

  colorCount := 0;
  if GetObject (pal, sizeof (colorCount), @colorCount) = 0 then
    RaiseLastOSError;

  if ColorCount = 0 then
  begin
    result := Nil;
    exit
  end;

  i := GetPaletteEntries (pal, 0, ColorCount, paletteEntries);
  if i = 0 then
    RaiseLastOSError;

  result := TBitmap.Create;

  result.PixelFormat := pf;
  result.Palette := CopyPalette (pal);

  result.Height := 1;
  result.Width := colorCount;

  for i := 0 to ColorCount - 1 do
    result.Canvas.Pixels [i, 0] := RGB (paletteEntries [i].peRed, paletteEntries [i].peGreen, paletteEntries [i].peBlue);
end;

(*----------------------------------------------------------------------*
 | TfmGraphicsResource.PropertyListBox1PropertyChanged                  |
 |                                                                      |
 | A property (width, height, pixelformat) has been changed.  Update    |
 |the image                                                             |
 *----------------------------------------------------------------------*)
procedure TfmGraphicsResource.PropertyListBox1PropertyChanged(
  Sender: TObject);
var
  prop : TPropertyListProperty;
  change : string;
  oldPf, newPf : TPixelFormat;
  b1, bmp, b2 : TBitmap;
  l : TList;
  pal : HPalette;
begin
  with PropertyListBox1 do
    prop := Properties [SelectedPropertyNo];

  change := '';
  newPf := pfDevice;

  case prop.Tag of
    taWidth : if Image1.Picture.Graphic.Width <> prop.PropertyValue then
              begin
                change := rstWidthChanged;
                ResizePicture (Image1.Picture, prop.PropertyValue, Image1.Picture.Graphic.Height);
              end;

    taHeight : if Image1.Picture.Graphic.Height <> prop.PropertyValue then
               begin
                 change := rstHeightChanged;
                 ResizePicture (Image1.Picture, Image1.Picture.Graphic.Width, prop.PropertyValue);
               end;

    taPixelFormat :
               begin
                 oldPf := GetPixelFormat (Image1.Picture.Graphic);
                 case prop.PropertyValue of
                   0 : newPf := pf1Bit;
                   1 : newPf := pf4Bit;
                   2 : newPf := pf8Bit;
                   3 : newPf := pf24Bit;
                   4 : newPf := pf32Bit;
                 end;
                 if oldPf <> newPf then
                 begin
                   change := rstPixelFormatChanged;

                   if Image1.Picture.Graphic is TBitmap then
                   begin
                     bmp := TBitmap (Image1.Picture.Graphic);

                     if newPf in [pf1Bit..pf8Bit] then
                     begin
                       b1 := Nil;
                       b2 := Nil;
                       try
                         if (newPf = pf8Bit) and (oldPf > pf8Bit) and not bmp.Empty then
                         begin
                           b1:= ReduceColors (bmp, rmQuantizeWindows, dmFloydSteinberg, GetPixelFormatBitCount (newPf), 0);
                         end
                         else
                         begin
                           // ReduceColors always returns a pf8Bit bitmap...
                           b1 := TBitmap.Create;
                           b1.PixelFormat := newPF;
                           case newPF of
                             pf1Bit : b1.Palette := SystemPalette2;
                             pf4Bit : b1.Palette := SystemPalette16;
                             pf8Bit : b1.Palette := CopyPalette (SystemPalette256); // unitExIcon.WebPalette
                           end;

                           b1.Width := bmp.Width;
                           b1.Height := bmp.Height;
                           b1.Canvas.Draw(0, 0, bmp);
                         end;
                         Image1.Picture.Graphic := b1;
                         bmp := TBitmap (Image1.Picture.Graphic);
                       finally
                         b1.Free;
                         b2.Free
                       end
                     end
                     else
                       bmp.PixelFormat := newPf;

                     bmp.IgnorePalette := newPf > pf8Bit;
                   end
                   else
                     if Image1.Picture.Graphic is TExIconCursor then
                       if newPf in [pf24Bit, pf32Bit] then
                         TExIconCursor (Image1.Picture.Graphic).PixelFormat := newPf
                       else
                       begin
                         pal := 0;
                         bmp := nil;
                         l := TList.Create;
                         try
                           bmp := TBitmap.Create;
                           bmp.Assign(Image1.Picture.Graphic);
                           l.Add (bmp);
                           l.Add (CreatePaletteBitmap (newPF));
                           pal := CreateOptimizedPaletteFromManyBitmaps (l, GetPixelFormatNumColors (newPf), GetPixelFormatBitCount (newPF), False);
                           TExIconCursor (Image1.Picture.Graphic).Palette := pal;
                         finally
                           l.Free;
                           bmp.Free;
                           if pal <> 0 then
                             DeleteObject (pal)
                         end
                       end
                 end;

                 SetPaletteForPixelFormat (newPf < oldPf);
               end
  end;

  if change <> '' then
  begin
    BitmapEditor1.Picture := Image1.Picture;      // Set the editor picture
    SaveResource (change);
  end
end;

procedure TfmGraphicsResource.TrackBar1Change(Sender: TObject);
begin
  ColorSelector1.Luminescence := 240 - TrackBar1.Position
end;

function TfmGraphicsResource.GetImportExportType: TImportExportType;
begin
  Result := ixPicture
end;

procedure TfmGraphicsResource.SetPaletteForPixelFormat(reset: Boolean);
var
  fc, bc : TColor;
  pf : TPixelFormat;
begin
  fc := clWhite;
  bc := clBlack;
  pf := GetPixelFormat (Image1.Picture.Graphic);
  if pf in [pf1Bit..pf8Bit] then
  begin
    ColorSelector1.Palette := Image1.Picture.Graphic.Palette;
    fc := ColorSelector1.Color [ColorSelector1.ColorCount - 1];
    bc := ColorSelector1.Color [0]
  end
  else
    ColorSelector1.Palette := 0;

  BitmapEditor1.ClipboardPalette := ColorSelector1.Palette;
  BitmapEditor1.ClipboardPixelFormat := pf;
                                                // Adjust colour selector Size, etc.
  if ColorSelector1.ColorCount = -1 then
    TrackBar1.Visible := True
  else
  begin
    TrackBar1.Visible := False;
    if ColorSelector1.ColorCount <= 16 then
      ColorSelector1.ColumnCount := 4
    else
      ColorSelector1.ColumnCount := 8
  end;

  if reset then
  begin
    ColorSelector1.ForegroundColor := fc;
    ColorSelector1.BackgroundColor := bc;

    shpFore.Brush.Color := fc;
    shpBack.Brush.Color := bc;

    BitmapEditor1.DrawPen.Color := fc;
    BitmapEditor1.DrawBrush.Color := bc
  end
end;

function TfmGraphicsResource.GetCanCopy: Boolean;
begin
  Result := BitmapEditor1.SelectionValid
end;

function TfmGraphicsResource.GetCanCut: Boolean;
begin
  Result := BitmapEditor1.SelectionValid
end;

function TfmGraphicsResource.GetCanPaste: Boolean;
begin
  Result := Clipboard.HasFormat (CF_METAFILEPICT) or Clipboard.HasFormat (CF_BITMAP) or Clipboard.HasFormat (CF_PICTURE)
end;

function TfmGraphicsResource.GetCanSelectAll: Boolean;
begin
  Result := True
end;

procedure TfmGraphicsResource.SelectAll;
begin
  BitmapEditor1.SelectAll
end;

function TfmGraphicsResource.GetCanDelete: Boolean;
begin
  Result := BitmapEditor1.SelectionValid
end;

procedure TfmGraphicsResource.EditDelete;
begin
  SaveResource (rstDeleteImage);
  BitmapEditor1.DeleteSelection
end;

procedure TfmGraphicsResource.Copy;
begin
  BitmapEditor1.CopySelection
end;

procedure TfmGraphicsResource.Cut;
begin
  SaveResource (rstCutImage);
  BitmapEditor1.CutSelection
end;

procedure TfmGraphicsResource.Paste;
begin
  if not BitmapEditor1.SelectionValid then
    BitmapEditor1.SelectAll;
  BitmapEditor1.PasteSelection;
  SaveResource (rstPasteImage);
end;

procedure TfmGraphicsResource.PreviewKey(var key: Word;
  shift: TShiftState);
begin

  case key of
    VK_ADD :
      begin
        actImageZoomIn.Execute;
        key := 0
      end;

    VK_SUBTRACT :
      begin
        actImageZoomOut.Execute;
        key := 0
      end
  end

end;

procedure TfmGraphicsResource.actImageColorsPaletteExecute(
  Sender: TObject);
begin
  pnlColours.Visible := not pnlColours.Visible;
end;

procedure TfmGraphicsResource.actImageToolsPaletteExecute(Sender: TObject);
begin
  pnlGraphics.Visible := not pnlGraphics.Visible
end;

procedure TfmGraphicsResource.mnuImageClick(Sender: TObject);
begin
  actImageToolsPalette.Checked := pnlGraphics.Visible;
  actImageColorsPalette.Checked := pnlColours.Visible
end;

procedure TfmGraphicsResource.BitmapEditor1SelectionRectChange(
  Sender: TObject);
var
  Msg : string;
begin
  if BitmapEditor1.SelectionValid then
    with BitmapEditor1.SelectionRect do
      Msg := Format ('%d,%d %dx%d', [Left, Top, Right - Left + 1, Bottom - Top + 1])
  else
    Msg := '';

  SendMessage (Application.MainForm.Handle, WM_STATUSBAR, 0, Integer (PChar (Msg)))
end;

procedure TfmGraphicsResource.PopupMenu1Popup(Sender: TObject);
begin
  actImageToolsPalette.Checked := pnlGraphics.Visible;
  actImageColorsPalette.Checked := pnlColours.Visible
end;

procedure TfmGraphicsResource.actImageAddImageExecute(Sender: TObject);
var
  tp : Integer;
begin
  if Image1.Picture.Graphic is TExCursor then
    tp := 0
  else
    if Image1.Picture.Graphic is TExIcon then
      tp := 1
    else
      tp := -1;

  SendMessage (Application.MainForm.Handle, WM_ADDIMAGERESOURCE, tp, 0);
end;

procedure TfmGraphicsResource.UpdateActions;
var
  dt : boolean;
begin
  actImageAddImage.Enabled := ResourceDetails is TIconCursorResourceDetails;


  // Disable the right-click popup menu if the dropper or
  // magnifier is selected.

  dt := BitmapEditor1.DrawingTool in [dtDropper, dtMagnifier];
  if dt then
    PopupMenu1.AutoPopup := False
  else
    if PopupMenu1.AutoPopup = False then
      if not (ssRight in KeyboardStateToShiftState) then
        PopupMenu1.AutoPopup := True
end;

end.
