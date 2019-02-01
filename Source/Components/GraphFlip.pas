(*======================================================================*
 | GraphFlip unit                                                       | 
 |                                                                      | 
 | Simple functions to flip and rotate 24-bit bitmaps                   | 
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
 | Copyright ? Colin Wilson 2002  All Rights Reserved                   | 
 |                                                                      | 
 | Version  Date        By    Description                               | 
 | -------  ----------  ----  ------------------------------------------| 
 | 1.0      29/08/2002  CPWW  Original                                  | 
 *======================================================================*) 
unit GraphFlip; 
 
interface 
 
uses
  Windows, Classes, Sysutils, Graphics;
 
function RotateBitmap270 (const Bitmap: TBitmap): TBitmap;
function RotateBitmap90 (const Bitmap: TBitmap): TBitmap;
function ConvertToGrayscale(const Bitmap: TBitmap): TBitmap;
function ConvertToNegative(const Bitmap: TBitmap): TBitmap;
procedure DrawTextW(DC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: Cardinal; 
  AdjustRight: Boolean); 
 
implementation 
 
function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint; 
begin 
  Dec(Alignment); 
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end; 
 
 
function RotateBitmap270(const Bitmap: TBitmap): TBitmap;
var 
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bpss, bpsr: Integer;
 
begin 
  if Bitmap.PixelFormat <> pf24Bit then
    raise Exception.Create('Invalid pixel format'); 
  Result := TBitmap.Create;
  Result.PixelFormat := Bitmap.PixelFormat;
  Result.Height := Bitmap.Width;
  Result.Width := Bitmap.Height;
 
  ps1 := Bitmap.ScanLine [0];
  pr1 := Result.ScanLine [Bitmap.Width - 1];
 
  bpss := BytesPerScanLine(Bitmap.Width, 24, 32);
  bpsr := BytesPerScanLine(Result.Width, 24, 32);
 
  for y := 0 to Bitmap.Height - 1 do
  begin 
    ps := PRGBTriple(PChar (ps1) - bpss * y); 
 
    for x := 0 to Bitmap.Width - 1 do
    begin 
      pr := PRGBTriple(PChar (pr1) + bpsr * x); 
      Inc(pr, y); 
      pr^ := ps^; 
      Inc(ps) 
    end 
  end; 
  GDIFlush 
end; 
 
function RotateBitmap90(const Bitmap: TBitmap): TBitmap;
var 
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bpss, bpsr: Integer;
 
begin 
  if Bitmap.PixelFormat <> pf24Bit then
    raise Exception.Create('Invalid pixel format'); 
  Result := TBitmap.Create;
  Result.PixelFormat := Bitmap.PixelFormat;
  Result.Height := Bitmap.Width;
  Result.Width := Bitmap.Height;
 
  ps1 := Bitmap.ScanLine [Bitmap.Height - 1];
  pr1 := Result.ScanLine [0];
 
  bpss := BytesPerScanLine(Bitmap.Width, 24, 32);
  bpsr := BytesPerScanLine(Result.Width, 24, 32);
 
  for y := 0 to Bitmap.Height - 1 do
  begin 
    ps := PRGBTriple(PChar (ps1) + bpss * y); 
 
    for x := 0 to Bitmap.Width - 1 do
    begin 
      pr := PRGBTriple(PChar (pr1) - bpsr * x); 
      Inc(pr, y); 
      pr^ := ps^; 
      Inc(ps) 
    end 
  end; 
  GDIFlush 
end; 
 

function ConvertToGrayscale(const Bitmap: TBitmap): TBitmap;
var 
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bps: Integer;
  n: Integer;
 
begin 
  if Bitmap.PixelFormat <> pf24Bit then
    raise Exception.Create('Invalid pixel format'); 
  Result := TBitmap.Create;
  Result.PixelFormat := Bitmap.PixelFormat;
  Result.Height := Bitmap.Height;
  Result.Width := Bitmap.Width;
 
  ps1 := Bitmap.ScanLine [0];
  pr1 := Result.ScanLine [0];
 
  bps := BytesPerScanLine(Bitmap.Width, 24, 32);
 
  for y := 0 to Bitmap.Height - 1 do
  begin 
    ps := PRGBTriple(PChar (ps1) - bps * y); 
    pr := PRGBTriple(PChar (pr1) - bps * y); 
 
    for x := 0 to Bitmap.Width - 1 do
    begin 
      n := (ps^.rgbtBlue + ps^.rgbtGreen + ps^.rgbtRed) div 3; 
      pr^.rgbtBlue := n; 
      pr^.rgbtGreen := n; 
      pr^.rgbtRed := n; 
 
      Inc(pr); 
      Inc(ps) 
    end 
  end;
  GDIFlush 
end; 
 
function ConvertToNegative(const Bitmap: TBitmap): TBitmap;
var 
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bps: Integer;
begin 
  if Bitmap.PixelFormat <> pf24Bit then
    raise Exception.Create('Invalid pixel format'); 
  Result := TBitmap.Create;
  Result.PixelFormat := Bitmap.PixelFormat;
  Result.Height := Bitmap.Height;
  Result.Width := Bitmap.Width;
 
  ps1 := Bitmap.ScanLine [0];
  pr1 := Result.ScanLine [0];
 
  bps := BytesPerScanLine(Bitmap.Width, 24, 32);
 
  for y := 0 to Bitmap.Height - 1 do
  begin 
    ps := PRGBTriple(PChar (ps1) - bps * y); 
    pr := PRGBTriple(PChar (pr1) - bps * y); 
 
    for x := 0 to Bitmap.Width - 1 do
    begin 
      pr^.rgbtBlue := 255 - ps^.rgbtBlue; 
      pr^.rgbtGreen := 255 - ps^.rgbtGreen; 
      pr^.rgbtRed := 255 - ps^.rgbtRed; 
 
      Inc(pr); 
      Inc(ps) 
    end 
  end; 
  GDIFlush 
end; 
 
 
const 
  WideNull = WideChar(#0); 
  WideCR = WideChar(#13); 
  WideLF = WideChar(#10); 
  WideLineSeparator = WideChar(#2028); 
 
procedure DrawTextW(DC: HDC; lpString: PWideChar; nCount: Integer; var lpRect: TRect; uFormat: Cardinal; 
  AdjustRight: Boolean); 
 
// This procedure implements a subset of Window's DrawText API for Unicode which is not available for 
// Windows 9x. For a description of the parameters see DrawText in the online help. 
// Supported flags are currently: 
//   - DT_LEFT 
//   - DT_TOP 
//   - DT_CALCRECT 
//   - DT_NOCLIP 
//   - DT_RTLREADING 
//   - DT_SINGLELINE 
//   - DT_VCENTER 
// Differences to the DrawTextW Windows API: 
//   - The additional parameter AdjustRight determines whether to adjust the right border of the given rectangle to 
//     accomodate the largest line in the text. It has only a meaning if also DT_CALCRECT is specified. 
 
var 
  Head, Tail: PWideChar; 
  Size: TSize; 
  MaxWidth: Integer; 
  TextOutFlags: Integer; 
  TextAlign, 
  OldTextAlign: Cardinal; 
  TM: TTextMetric; 
  TextHeight: Integer; 
  LineRect: TRect; 
  TextPosY, 
  TextPosX: Integer; 
 
  CalculateRect: Boolean; 
 
begin 
  // Prepare some work variables. 
  MaxWidth := 0; 
  Head := lpString; 
  GetTextMetrics(DC, TM); 
  TextHeight := TM.tmHeight; 
  if uFormat and DT_SINGLELINE <> 0 then 
    LineRect := lpRect 
  else 
    LineRect := Rect(lpRect.Left, lpRect.Top, lpRect.Right, lpRect.Top + TextHeight); 
 
  CalculateRect := uFormat and DT_CALCRECT <> 0; 
 
  // Prepare text output. 
  TextOutFlags := 0; 
  if uFormat and DT_NOCLIP = 0 then 
    TextOutFlags := TextOutFlags or ETO_CLIPPED; 
  if uFormat and DT_RTLREADING <> 0 then 
    TextOutFlags := TextOutFlags or ETO_RTLREADING; 
 
  // Determine horizontal and vertical text alignment. 
  OldTextAlign := GetTextAlign(DC); 
  TextAlign := TA_LEFT or TA_TOP; 
  TextPosX := lpRect.Left;        
  if uFormat and DT_RIGHT <> 0 then 
  begin 
    TextAlign := TextAlign or TA_RIGHT and not TA_LEFT; 
    TextPosX := lpRect.Right; 
  end 
  else 
    if uFormat and DT_CENTER <> 0 then 
    begin 
      TextAlign := TextAlign or TA_CENTER and not TA_LEFT; 
      TextPosX := (lpRect.Left + lpRect.Right) div 2; 
    end; 
 
  TextPosY := lpRect.Top; 
  if uFormat and DT_VCENTER <> 0 then 
  begin 
    // Note: vertical alignment does only work with single line text ouput! 
    TextPosY := (lpRect.Top + lpRect.Bottom - TextHeight) div 2; 
  end; 
  SetTextAlign(DC, TextAlign); 
 
  if uFormat and DT_SINGLELINE <> 0 then 
  begin 
    if CalculateRect then 
    begin 
      GetTextExtentPoint32W(DC, Head, nCount, Size); 
      if Size.cx > MaxWidth then 
        MaxWidth := Size.cx; 
    end 
    else 
      ExtTextOutW(DC, TextPosX, TextPosY, TextOutFlags, @LineRect, Head, nCount, nil); 
    OffsetRect(LineRect, 0, TextHeight); 
  end 
  else 
  begin 
    while(nCount > 0) and (Head^ <> WideNull) do 
    begin 
      Tail := Head; 
      // Look for the end of the current line. A line is finished either by the string end or a line break. 
      while(nCount > 0) and not (Tail^ in [WideNull, WideCR, WideLF]) and (Tail^ <> WideLineSeparator) do 
      begin 
        Inc(Tail); 
        Dec(nCount); 
      end; 
 
      if CalculateRect then 
      begin 
        GetTextExtentPoint32W(DC, Head, Tail - Head, Size); 
        if Size.cx > MaxWidth then 
          MaxWidth := Size.cx; 
      end 
      else 
        ExtTextOutW(DC, TextPosX, LineRect.Top, TextOutFlags, @LineRect, Head, Tail - Head, nil); 
      OffsetRect(LineRect, 0, TextHeight); 
 
      // Get out of the loop if the rectangle is filled up. 
      if (nCount = 0) or (not CalculateRect and (LineRect.Top >= lpRect.Bottom)) then 
        Break; 
 
      if (nCount > 0) and (Tail^ = WideCR) or (Tail^ = WideLineSeparator) then 
      begin 
        Inc(Tail); 
        Dec(nCount); 
      end; 
 
      if (nCount > 0) and (Tail^ = WideLF) then 
      begin 
        Inc(Tail); 
        Dec(nCount); 
      end; 
      Head := Tail; 
    end; 
  end; 
 
  SetTextAlign(DC, OldTextAlign); 
  if CalculateRect then 
  begin 
    if AdjustRight then 
      lpRect.Right := lpRect.Left + MaxWidth; 
    lpRect.Bottom := LineRect.Top; 
  end; 
end; 
 
 
end. 
