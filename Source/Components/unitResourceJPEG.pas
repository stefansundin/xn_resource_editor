(*======================================================================*
 | unitResourceJPEG                                                     |
 |                                                                      |
 | Encapsulates JPEG images in custom (non-RC data) resources.          |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      14/06/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitResourceJPEG;

interface

uses
  Windows, Classes, SysUtils, Graphics, jpeg, unitResourceDetails,
  unitResourceGraphics;

type
//------------------------------------------------------------------------
// JPeg resource details class

  TJPegResourceDetails = class (TGraphicsResourceDetails)
  protected
    fWidth, fHeight : Integer;
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    procedure InitNew; override;
    class function SupportsData(Size : Integer; data : Pointer) : Boolean; override;
  public
    class function GetBaseType : WideString; override;
    procedure GetImage(picture : TPicture); override;
    procedure SetImage(image : TPicture); override;
  end;

implementation

function FindJPegSegment(var data : PChar; segment : byte) : Boolean;
const
  ParameterlessSegments = #$01#$d0#$d1#$d2#$d3#$d4#$d5#$d6#$d7#$d8#$d9;
var
  p : PChar;
  seg : Byte;
  len : Word;
begin
  p := data;
  Result := False;

  repeat
    if p^ <> #$ff then
      raise Exception.Create('Invalid JPEG Image');

    Inc(p);

    seg := Byte(p^);

    if seg <> $ff then
    begin
      Inc(p);

      if seg = segment then
      begin
        Result := True;
        data := p;
        break
      end;

      if seg = $d9 then // end of image
        break;

      if Pos (char (seg), ParameterlessSegments) = 0 then
      begin
        len := 256 * Byte(p^) + Byte((p + 1)^);
        Inc(p, len)
      end
    end
  until False
end;

procedure GetJPegSize(data : PChar; var Width, Height : Integer);
var
  len : Integer;
begin
  if FindJPegSegment(data, $c0) then
  begin
    len := 256 * Byte(data^) + Byte((data + 1)^);

    if len > 5 then
    begin
      Inc(data, 3);  // Skip len word & precision byte
      Height := 256 * Byte(data^) + Byte((data + 1)^);

      Inc(data, 2);
      Width := 256 * Byte(data^) + Byte((data + 1)^);
    end
  end
end;

{ TJPegResourceDetails }

class function TJPegResourceDetails.GetBaseType: WideString;
begin
  Result := 'JPEG'
end;

function TJPegResourceDetails.GetHeight: Integer;
begin
  if fHeight = 0 then
    GetJPegSize(data.Memory, FWidth, FHeight);

  Result := fHeight;
end;

procedure TJPegResourceDetails.GetImage(picture: TPicture);
begin
  picture.graphic := TJPegImage.Create;
  data.Seek(0, soFromBeginning);
  TJpegImage(picture.graphic).LoadFromStream (data);
  fWidth := picture.graphic.Width;
  fHeight := picture.graphic.Height;
end;

function TJPegResourceDetails.GetPixelFormat: TPixelFormat;
begin
  Result := pf24Bit;
end;

function TJPegResourceDetails.GetWidth: Integer;
begin
  if fWidth = 0 then
    GetJPegSize(data.Memory, FWidth, FHeight);
  Result := fWidth;
end;

procedure TJPegResourceDetails.InitNew;
var
  img : TJPegImage;
  bmp : TBitmap;
begin
  bmp := nil;
  img := TJPegImage.Create;
  try
    bmp := TBitmap.Create;
    bmp.Width := 64;
    bmp.Height := 64;
    img.Assign(bmp);
    img.SaveToStream (data);
  finally
    img.Free;
    bmp.Free
  end
end;

procedure TJPegResourceDetails.SetImage(image: TPicture);
begin
  inherited;
  fWidth := image.Width;
  fHeight := image.Height;
end;

class function TJPegResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  len : Integer;
begin
  Result := False;
  if PWORD (data)^ = $d8ff then
    if FindJPegSegment(PChar (data), $e0) then
    begin
      len := 256 * Byte(PChar (data)^) + Byte((PChar (data) + 1)^);

      if len >= 16 then
      begin
        Inc(PChar (data), 2);

        if StrLIComp (data, 'JFIF', 4) = 0 then
          Result := True
      end
    end
end;

initialization
  RegisterResourceDetails (TJPEGResourceDetails);
finalization
  UnRegisterResourceDetails (TJPEGResourceDetails);
end.
