unit unitResourcePNG;

interface

uses Windows, Classes, SysUtils, graphics, pngimage, unitResourceDetails, unitResourceGraphics;

type
//------------------------------------------------------------------------
// PNG resource details class

  TPngResourceDetails = class (TGraphicsResourceDetails)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
  public
    class function GetBaseType : WideString; override;
    procedure GetImage (picture : TPicture); override;
  end;


implementation

{ TPngResourceDetails }

class function TPngResourceDetails.GetBaseType: WideString;
begin
  Result := 'PNG';
end;

function TPngResourceDetails.GetHeight: Integer;
begin
  Result := PWORD (PChar (data) + 6 + SizeOf (Word))^;
end;

procedure TPngResourceDetails.GetImage(picture: TPicture);
begin
  picture.graphic := TPngObject.Create;
  data.Seek (0, soFromBeginning);
  TPngObject (picture.graphic).LoadFromStream (data)
end;

function TPngResourceDetails.GetPixelFormat: TPixelFormat;
begin
  Result := pf8Bit;
end;

function TPngResourceDetails.GetWidth: Integer;
begin
  Result := PWORD (PChar (data) + 6)^;
end;

class function TPngResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PChar;
begin
  p := PChar (data);
  Inc (p);

  Result := (StrLIComp (p, 'PNG', 3) = 0);
end;

initialization
  RegisterResourceDetails (TPngResourceDetails);
finalization
  UnregisterResourceDetails (TPngResourceDetails);
end.


