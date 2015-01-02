unit unitIncludePathPackages;

interface

uses Windows, Classes, SysUtils, unitIncludePaths, ShFolder;

type

TBCB5IncludePathPackage = class (TIncludePathPackage)
protected
  function GetRootDirectory : string; override;
  function GetName : string; override;
end;

TBCB6IncludePathPackage = class (TIncludePathPackage)
protected
  function GetRootDirectory : string; override;
  function GetName : string; override;
end;

TBDS4IncludePathPackage = class (TIncludePathPackage)
protected
  function GetRootDirectory : string; override;
  function GetName : string; override;
end;

TVsIncludePathPackage = class (TIncludePathPackage)
protected
  function GetRootDirectory : string; override;
  function GetName : string; override;
  function GetIncludePath: string; override;
end;

TMSDotNetIncludePathPackage = class (TIncludePathPackage)
protected
  function GetRootDirectory : string; override;
  function GetName : string; override;
  function GetIncludePath: string; override;
end;

TMSDotNet2003IncludePathPackage = class (TMSDotNetIncludePathPackage)
  function GetRootDirectory : string; override;
  function GetName : string; override;
  function GetIncludePath: string; override;
end;

implementation

uses Registry;

function ProgramFilesPath : string;
const
  SHGFP_TYPE_CURRENT = 0;
  SHGFP_TYPE_DEFAULT = 1;
begin
  SetLength (result, MAX_PATH + 1);
  if Succeeded (ShGetFolderPath (0, CSIDL_PROGRAM_FILES, 0, SHGFP_TYPE_CURRENT, PChar (result))) then
    result := PChar (result)
  else
    result := 'c:\Program Files'
end;

function GetPathFromHKLM (const key, value : string) : string;
var
  reg : TRegistry;
begin
  result := '';
  reg := TRegistry.Create (KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey(key, false) then
    begin
      result := reg.ReadString(value);
      if (result <> '') and (result [Length (result)] = '\') then
        Delete (result, Length (result), 1)
    end
  finally
    reg.Free
  end
end;

{ TBCB5IncludePathPackage }

function TBCB5IncludePathPackage.GetName: string;
begin
  result := 'Borland C++ Builder 5.0';
end;

function TBCB5IncludePathPackage.GetRootDirectory: string;
begin
  result := ProgramFilesPath + '\Borland\CBuilder5';
end;

{ TBCB6IncludePathPackage }

function TBCB6IncludePathPackage.GetName: string;
begin
  result := 'Borland C++ Builder 6.0';
end;

function TBCB6IncludePathPackage.GetRootDirectory: string;
begin
  result := ProgramFilesPath + '\Borland\CBuilder6';
end;

{ TBDS4IncludePathPackage }

function TBDS4IncludePathPackage.GetName: string;
begin
  result := 'Borland Developer Studio 2006';
end;

function TBDS4IncludePathPackage.GetRootDirectory: string;
begin
  result := GetPathFromHKLM ('Software\Borland\BDS\4.0', 'RootDir');
end;

{ TVsIncludePathPackage }

function TVsIncludePathPackage.GetIncludePath: string;
begin
  result := inherited GetIncludePath;
  result := result + ';' + GetRootDirectory + '\MFC\Include';
end;

function TVsIncludePathPackage.GetName: string;
begin
  result := 'Microsoft Visual Studio C++ 6.0';
end;

function TVsIncludePathPackage.GetRootDirectory: string;
begin
  result := GetPathFromHKLM ('Software\Microsoft\VisualStudio\6.0\Setup\Microsoft Visual C++', 'ProductDir');
end;

{ TMSDotNetIncludePathPackage }

function TMSDotNetIncludePathPackage.GetIncludePath: string;
var
  rootDir : string;
  frameworkDir : string;
begin
  result := GetPathFromHKLM ('Software\Microsoft\VisualStudio\7.0\VC\VC_OBJECTS_PLATFORM_INFO\Win32\Directories', 'Include Dirs');
  rootDir := GetRootDirectory + '\';
  frameworkDir := ProgramFilesPath + '\Microsoft.net\sdk\';

  result := StringReplace (result, '$(VCInstallDir)', rootDir, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace (result, '$(FrameworkSDKDir)', frameworkDir, [rfReplaceAll, rfIgnoreCase]);
end;

function TMSDotNetIncludePathPackage.GetName: string;
begin
  result := 'Microsoft Visual Studio .Net C++';
end;

function TMSDotNetIncludePathPackage.GetRootDirectory: string;
begin
  result := GetPathFromHKLM ('Software\Microsoft\VisualStudio\7.0\Setup\VC', 'ProductDir');
end;

{ TMSDotNet2003IncludePathPackage }

function TMSDotNet2003IncludePathPackage.GetIncludePath: string;
var
  rootDir : string;
  frameworkDir : string;
begin
  result := GetPathFromHKLM ('Software\Microsoft\VisualStudio\7.1\VC\VC_OBJECTS_PLATFORM_INFO\Win32\Directories', 'Include Dirs');
  rootDir := GetRootDirectory + '\';
  frameworkDir := ProgramFilesPath + '\Microsoft.net\sdk\';

  result := StringReplace (result, '$(VCInstallDir)', rootDir, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace (result, '$(FrameworkSDKDir)', frameworkDir, [rfReplaceAll, rfIgnoreCase]);
end;

function TMSDotNet2003IncludePathPackage.GetName: string;
begin
  result := 'Microsoft Visual Studio .Net 2003 C++';
end;

function TMSDotNet2003IncludePathPackage.GetRootDirectory: string;
begin
  result := GetPathFromHKLM ('Software\Microsoft\VisualStudio\7.1\Setup\VC', 'ProductDir');
end;

begin
  RegisterIncludePathPackage ('BCB5',   TBCB5IncludePathPackage);
  RegisterIncludePathPackage ('BCB6',   TBCB6IncludePathPackage);
  RegisterIncludePathPackage ('BDS4',   TBDS4IncludePathPackage);
  RegisterIncludePathPackage ('MSVC6',  TVsIncludePathPackage);
  RegisterIncludePathPackage ('MSVC7',  TMSDotNetIncludePathPackage);
  RegisterIncludePathPackage ('MSVC71', TMSDotNet2003IncludePathPackage);
end.
