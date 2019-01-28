unit unitResourceHTML;

interface

uses Windows, Classes, SysUtils, unitResourceDetails;

const
  RT_HTML = MakeIntResource(23);

type
//------------------------------------------------------------------------
// HTML resource details class

  THTMLResourceDetails = class (TAnsiResourceDetails)
  protected
    procedure InitNew; override;
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
  public
    class function GetBaseType : WideString; override;
  end;


implementation

{ THTMLResourceDetails }

class function THTMLResourceDetails.GetBaseType: WideString;
begin
  result := IntToStr (Integer (RT_HTML))
end;

procedure THTMLResourceDetails.InitNew;
begin
  Text := '<HTML>'#13#10'</HTML>';
end;

class function THTMLResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PChar;
begin
  p := PChar (data);
  Result := (StrLIComp (p, '<HTML', 5) = 0) or (StrLIComp (p, '<!', 2) = 0);
end;

initialization
  RegisterResourceDetails (THTMLResourceDetails);
finalization
  UnregisterResourceDetails (THTMLResourceDetails);
end.
