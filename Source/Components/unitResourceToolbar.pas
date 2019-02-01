unit unitResourceToolbar;

interface

uses
  Windows, Classes, SysUtils, Contnrs, Menus, unitResourceDetails;

const
  RT_TOOLBAR = MakeIntResource(241);

type
TToolbarResourceDetails = class (TResourceDetails)
//  private
//    fHelpID : Integer;                    // Extended menu's help ID
  protected
    constructor Create(AParent : TResourceModule; ALanguage : Integer; const AName, AType : WideString; ASize : Integer; AData : pointer); override;

  public
    destructor Destroy; override;

    class function GetBaseType : WideString; override;
    procedure ChangeData(newData : TMemoryStream); override;

    procedure InitNew; override;
end;

implementation

type

TToolbarData = packed record  // From a CodeGuru message quoting MFC source...
  wVersion : word;
  wBtnWidth : word;
  wBtnHeight : word;
  wBtnCount : word;
  wButtonIDs : array [0..0] of word;
end;

{ TToolbarResourceDetails }

procedure TToolbarResourceDetails.ChangeData(newData: TMemoryStream);
begin
  inherited;
end;

constructor TToolbarResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: WideString; ASize: Integer;
  AData: pointer);
begin
  inherited Create(AParent, ALanguage, AName, AType, ASize, AData);
end;

destructor TToolbarResourceDetails.Destroy;
begin
  inherited;
end;

class function TToolbarResourceDetails.GetBaseType: WideString;
begin
  Result := IntToStr (Integer (RT_TOOLBAR));
end;

procedure TToolbarResourceDetails.InitNew;
var
  dat : TToolbarData;
begin
  dat.wVersion := 1;
  dat.wBtnWidth := 16;
  dat.wBtnHeight := 15;
  dat.wBtnCount := 0;

  data.Write(dat, SizeOf(dat) - SizeOf(dat.wButtonIDs))
end;

initialization
  RegisterResourceDetails (TToolbarResourceDetails);
finalization
  UnregisterResourceDetails (TToolbarResourceDetails);
end.


