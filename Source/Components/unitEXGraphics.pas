(*======================================================================*
 | unitExGraphics unit                                                  |
 |                                                                      |
 | Simple graphics functions to convert HSL to RGB.  Provides 2-color   |
 | 256 color palettes                                                   |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 2.0      23/2/2000   CPWW  Original                                      |
 *======================================================================*)

unit unitEXGraphics;

interface

uses Windows, Sysutils, Classes, Graphics, clipbrd, commctrl, controls;

const
  HLSMAX = 240;

var
  SystemPalette256 : HPALETTE;  // 256 color 'web' palette.
  SystemPalette2 : HPALETTE;

procedure iHLSToRGB (hue, lum, sat : Integer; var r, g, b : Integer);
procedure RGBtoHLS(lRGBColor : TColor; var H, L, S : Integer);

implementation

const
  RGBMAX = 255;
  UNDEFINED = HLSMAX * 2 div 3;

function HueToRGB(n1,n2,hue : Integer) : word;
begin
  while hue > hlsmax do
    Dec (hue, HLSMAX);

  while hue < 0 do
    Inc (hue, HLSMAX);

  if hue < HLSMAX div 6 then
    Result := ( n1 + (((n2-n1)*hue+(HLSMAX div 12)) div (HLSMAX div 6)) )
  else
    if hue < HLSMAX div 2 then
      Result := n2
    else
      if hue < (HLSMAX*2) div 3 then
        Result := n1 + (((n2-n1)*(((HLSMAX*2) div 3)-hue)+(HLSMAX div 12)) div (HLSMAX div 6))
      else
        Result := n1
end;

procedure iHLSToRGB (hue, lum, sat : Integer; var r, g, b : Integer);
var
  Magic1,Magic2 : Integer;        // calculated magic numbers (really!) */

begin
  if sat = 0 then              // achromatic case */
  begin
    r := (lum * RGBMAX) div HLSMAX;
    g := r;
    b := r;
//    if hue <> UNDEFINED then
//      raise EColorError.Create ('Bad hue value')
  end
  else
  begin
    if lum <= HLSMAX div 2 then
      Magic2 := (lum*(HLSMAX + sat) + (HLSMAX div 2)) div HLSMAX
    else
      Magic2 := lum + sat - ((lum*sat) + (HLSMAX div 2)) div HLSMAX;

    Magic1 := 2*lum-Magic2;

    r := (HueToRGB (Magic1,Magic2,hue+(HLSMAX div 3))*RGBMAX + (HLSMAX div 2)) div HLSMAX;
    g := (HueToRGB (Magic1,Magic2,hue)*RGBMAX + (HLSMAX div 2)) div HLSMAX;
    b := (HueToRGB (Magic1,Magic2,hue-(HLSMAX div 3))*RGBMAX + (HLSMAX div 2)) div HLSMAX;
  end;
end;

function max (x, y : Integer) : Integer;
begin
  if x > y then
    Result := x
  else
    Result := y
end;

function min (x, y : Integer) : Integer;
begin
  if x < y then
    Result := x
  else
    Result := y
end;


procedure RGBtoHLS(lRGBColor : TColor; var H, L, S : Integer);
var
  R, G, B : Integer;
  cMax, cMin : Integer;
  rDelta, gDelta, bDelta : Integer;
begin
  R := GetRValue (lRGBColor);
  G := GetGValue (lRGBColor);
  B := GetBValue (lRGBColor);

  cMax := max (max (R, G), B);
  cMin := min (min (R, G), B);

  L := (((cMax+cMin)*HLSMAX) + RGBMAX ) div (2*RGBMAX);

  if (cMax = cMin) then
  begin
    S := 0;
    H := -1
  end
  else
  begin
    if L <= (HLSMAX div 2) then
      S := ( ((cMax-cMin)*HLSMAX) + ((cMax+cMin) div 2) )  div (cMax+cMin)
    else
      S := ( ((cMax-cMin)*HLSMAX) + ((2*RGBMAX-cMax-cMin) div 2) ) div (2*RGBMAX-cMax-cMin);

         (* hue *)
      Rdelta := ( ((cMax-R)*(HLSMAX div 6)) + ((cMax-cMin) div 2) ) div (cMax-cMin);
      Gdelta := ( ((cMax-G)*(HLSMAX div 6)) + ((cMax-cMin) div 2) ) div (cMax-cMin);
      Bdelta := ( ((cMax-B)*(HLSMAX div 6)) + ((cMax-cMin) div 2) ) div (cMax-cMin);

      if (R = cMax) then
        H := Bdelta - Gdelta
      else if (G = cMax) then
        H := (HLSMAX div 3) + Rdelta - Bdelta
      else
        H := ((2*HLSMAX) div 3) + Gdelta - Rdelta;

      if (H < 0) then
        Inc (H, HLSMAX);
      if (H > HLSMAX) then
        Dec (H, HLSMAX)
  end
end;

function WebPalette: HPalette;
type
  TLogWebPalette	= packed record
    palVersion		: word;
    palNumEntries	: word;
    PalEntries		: array [0..5,0..5,0..5] of TPaletteEntry;
    MonoEntries         : array [0..23] of TPaletteEntry;
    StdEntries          : array [0..15] of TPaletteEntry;
  end;
var
  r, g, b		: byte;
  LogWebPalette		: TLogWebPalette;
  LogPalette		: TLogpalette absolute LogWebPalette; // Stupid typecast
begin
  with LogWebPalette do
  begin
    GetPaletteEntries (SystemPalette16, 0, 16, StdEntries);
    palVersion:= $0300;
    palNumEntries:= 256;

    g := 10;
    for r := 0 to 23 do
    begin
      MonoEntries [r].peRed := g;
      MonoEntries [r].peGreen := g;
      MonoEntries [r].peBlue := g;
      MonoEntries [r].peFlags := 0;
      Inc (g, 10)
    end;

    for r:=0 to 5 do
      for g:=0 to 5 do
        for b:=0 to 5 do
        begin
          with PalEntries[r,g,b] do
          begin
            peRed := 51 * r;
            peGreen := 51 * g;
            peBlue := 51 * b;
            peFlags := 0;
          end;
        end;
  end;
  Result := CreatePalette(Logpalette);
end;

(*----------------------------------------------------------------------------*
 | function Create2ColorPalette;                                              |
 |                                                                            |
 | Does what it says on the tin..                                             |
 *----------------------------------------------------------------------------*)
function Create2ColorPalette : HPALETTE;
const
  palColors2 : array [0..1] of TColor = ($000000, $ffffff);
var
  logPalette : PLogPalette;
  i, c : Integer;

begin
  GetMem (logPalette, sizeof (logPalette) + 2 * sizeof (PALETTEENTRY));

  try
    logPalette^.palVersion := $300;
    logPalette^.palNumEntries := 2;
{$R-}
    for i := 0 to 1 do
      with logPalette^.palPalEntry [i] do
      begin
        c := palColors2 [i];

        peRed := c and $ff;
        peGreen := c shr 8 and $ff;
        peBlue :=  c shr 16 and $ff
      end;
{$R+}
    Result := CreatePalette (logPalette^);
  finally
    FreeMem (logPalette)
  end
end;

initialization
  SystemPalette256 := WebPalette;
  SystemPalette2 := Create2ColorPalette;
finalization
  DeleteObject (SystemPalette2);
  DeleteObject (SystemPalette256);
end.
