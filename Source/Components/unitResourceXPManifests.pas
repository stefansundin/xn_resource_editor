unit unitResourceXPManifests;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceDetails, AxCtrls;

type
TXPManifestResourceDetails = class (TAnsiResourceDetails)
public
  class function GetBaseType : WideString; override;
  procedure InitNew; override;
end;

const
  RT_XPMANIFEST = MakeIntResource(24);

implementation

{ TXPManifestResourceDetails }

const

  manifest : string =
'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#13#10+
'<assembly xmlns="urn:schemas-microsoft-com:asm.v1"'#13#10+
'manifestVersion="1.0">'#13#10+
'<assemblyIdentity'#13#10+
'    name="Woozle.PEResourceExplorer.XPManifest"'#13#10+
'    processorArchitecture="x86"'#13#10+
'    version="1.0.0.0"'#13#10+
'    type="win32"/>'#13#10+
'<description>Windows Shell</description>'#13#10+
'<dependency>'#13#10+
'    <dependentAssembly>'#13#10+
'        <assemblyIdentity'#13#10+
'            type="win32"'#13#10+
'            name="Microsoft.Windows.Common-Controls"'#13#10+
'            version="6.0.0.0"'#13#10+
'            processorArchitecture="x86"'#13#10+
'            publicKeyToken="6595b64144ccf1df"'#13#10+
'            language="*"'#13#10+
'        />'#13#10+
'    </dependentAssembly>'#13#10+
'</dependency>'#13#10+
'</assembly>';


class function TXPManifestResourceDetails.GetBaseType: WideString;
begin
  Result := IntToStr (Integer (RT_XPMANIFEST));
end;

procedure TXPManifestResourceDetails.InitNew;
begin
  Data.Clear;
  Data.Write(PChar (manifest)^, Length (manifest))
end;

initialization
  RegisterResourceDetails (TXPManifestResourceDetails);
finalization
  UnregisterResourceDetails (TXPManifestResourceDetails);
end.
