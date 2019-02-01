unit XFace;

{$HINTS OFF}

{   XFace decoding and encoding Delphi interface
    Copyright(C) 2002 Matthijs Laan
    This file licensed as in license.txt
    http://www.xs4all.nl/~walterln/winface/

    Uses:
    
    Compface - 48x48x1 image compression and decompression
    http://www.syseng.anu.edu.au/~jaa/
    ftp://syseng.anu.edu.au/pub/jaa/compface.tar.gz
    
    The *.c files are from this library; see compface-license.txt
    for the license of those files
}

{ New version using static linking of the compface library. The compface 
  library is again compiled with Borland C++, but as the setjmp/longjmp
  code is removed this still runs OK with 1000+ char X-Faces

  Uses asm code from WinFace 1.5, use NASM (http://nasm.2y.net) to assemble
  the x-face.asm file 
}

interface

uses Windows, Graphics;

const
  ERR_OK        =  0;   { successful completion }
  ERR_EXCESS    =  1;   { completed OK but some input was ignored }
  ERR_INSUFF    = -1;   { insufficient input.  Bad face format? }
  ERR_INTERNAL  = -2;   { arithmetic overflow or buffer overflow }

  ERR_BADFACE   = -3;   { bad face source bitmap }

function XFaceToBitmap(XFace: string; var Bitmap: TBitmap): Integer;
function BitmapToXFace(Bitmap: TBitmap; var XFace: string): Integer;

{ Extra Boolean for inverting image. On a Windows 95 system some 24-bit images
  get inverted somehow - use this boolean to work around it }
function BitmapToXFaceI(Bitmap: TBitmap; var XFace: string; Invert: Boolean): Integer;

implementation

uses SysUtils;

var
  DIB: HBITMAP;         { 48x48x1 DIB }
  Pixels: pointer;      { pointer to the pixels of the DIB }

  buffer: array[0..2047] of Byte;

function decode_face(src: pchar; dest: pointer; pad: integer): integer; stdcall; external;
function encode_face(src: pointer; dest: pchar; pad: integer): integer; stdcall; external;

{$L 'OBJ\x-face.obj'}

{$L 'OBJ\arith.obj'}
{$L 'OBJ\file.obj'}
{$L 'OBJ\compface.obj'}
{$L 'OBJ\compress.obj'}
{$L 'OBJ\gen.obj'}
{$L 'OBJ\uncompface.obj'}

{ Expose some symbols to make the linker happy - these are of course not the
  complete function signatures }

procedure _BigMul; external;
procedure _BigAdd; external;
procedure _BigDiv; external;
procedure _BigClear; external;
procedure _BigRead; external;
procedure _BigPush; external;
procedure _BigPop; external;
procedure _BigWrite; external;
procedure _RevPush; external;
procedure _ReadFace; external;
procedure _UnCompAll; external;
procedure _UnGenFace; external;
procedure _WriteFace; external;

function _strlen(const s: pchar): integer; cdecl;
begin
  result := StrLen(s);
end;

function XFaceToBitmap(XFace: string; var Bitmap: TBitmap): Integer;
var
  src, dst: HDC;
  l: integer;
begin
  if(Length(XFace) > 2047) then begin
    l := 2047;
  end else begin
    l := Length(XFace);
  end;

  move(XFace[1], buffer[0], l);

  buffer[l] := 0;

  Result := decode_face(@buffer[0], Pixels, 2);

  Bitmap.HandleType := bmDIB;

  src := CreateCompatibleDC(0);
  SelectObject(src, DIB);
  dst := CreateCompatibleDC(0);
  SelectObject(dst, Bitmap.Handle);

  BitBlt(dst, 0, 0, 48, 48, src, 0, 0, SRCCOPY);

  DeleteDC(src);
  DeleteDC(dst);
end;

function BitmapToXFace(Bitmap: TBitmap; var XFace: string): Integer;
begin
  result := BitmapToXFaceI(Bitmap, XFace, False);
end;

function BitmapToXFaceI(Bitmap: TBitmap; var XFace: string; Invert: Boolean): Integer;
var
  src, dst: HDC;
  l: integer;
begin
  Bitmap.HandleType := bmDIB;

  src := CreateCompatibleDC(0);
  SelectObject(src, Bitmap.Handle);
  dst := CreateCompatibleDC(0);
  SelectObject(dst, DIB);

  if(Invert) then begin
    StretchBlt(dst, 0, 0, 48, 48, src, 0, 0, Bitmap.Width, Bitmap.Height, NOTSRCCOPY);
  end else begin
    StretchBlt(dst, 0, 0, 48, 48, src, 0, 0, Bitmap.Width, Bitmap.Height, SRCCOPY);
  end;

  DeleteDC(src);
  DeleteDC(dst);

  encode_face(Pixels, @buffer[0], 2);

  l := strlen(PAnsiChar(@buffer[0]));

  SetLength(XFace, l);

  move(buffer, XFace[1], l);

  Result := 0;
end;

{ Define a new sort-of BITMAPINFO type for use with CreateDIBSection() }

type
  myBITMAPINFO = packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..1] of TRGBQuad;
  end;

var BI: myBITMAPINFO = (bmiHeader:
                         (biSize: SizeOf(BITMAPINFOHEADER);
                          biWidth: 48;
                          biHeight: -48;	{ Top-down bitmap instead of bottom-up }
                          biPlanes: 1;
                          biBitCount: 1;
                          biCompression: BI_RGB;
                          biSizeImage: 0;
                          biXPelsPerMeter: 0;
                          biYPelsPerMeter: 0;
                          biClrUsed: 2;
                          biClrImportant: 2);
                        bmiColors:
                         ( (rgbBlue: 255;
                            rgbGreen: 255;
                            rgbRed: 255;
                            rgbReserved: 0)
                          ,
                           (rgbBlue: 0;
                            rgbGreen: 0;
                            rgbRed: 0;
                            rgbReserved: 0)
                         )
                       );

{ Avoid Delphi type-checking }

var
  xBI: BITMAPINFO absolute BI;
  dc : HDC;

initialization
  dc := GetDC (0);
  try
    DIB := CreateDIBSection(dc, xBI, DIB_RGB_COLORS, Pixels, 0, 0);
  finally
    ReleaseDC (0, dc)
  end;

finalization
  DeleteObject(DIB);
end.

