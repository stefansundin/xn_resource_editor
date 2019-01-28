unit unitIncludePaths;

interface

uses
  Windows, Classes, SysUtils, Contnrs;

type
  TIncludePathPackage = class
  protected
    function GetRootDirectory : string; virtual; abstract;
    function GetInstalled: boolean; virtual;
    function GetIncludePath: string; virtual;
    function GetName: string; virtual;
  public
    property Installed : boolean read GetInstalled;
    property IncludePath : string read GetIncludePath;
    property Name : string read GetName;
  end;

  TIncludePathPackageClass = class of TIncludePathPackage;

  TIncludePathPackages = class
  private
    fIncludePathPackages : TObjectList;
    procedure Analyze;

    function GetCount: Integer;
    function GetPackage(idx: Integer): TIncludePathPackage;
    function GetIncludePath(const PackageName: string): string;
    function FindIncludePathPackage (const PackageName : string) : TIncludePathPackage;
  public
    property Count : Integer read GetCount;
    property Package [idx : Integer] : TIncludePathPackage read GetPackage;
    property IncludePath [const PackageName : string] : string read GetIncludePath;
  end;

function GetIncludePathForPackage (const PackageName : string) : string;
procedure RegisterIncludePathPackage (const key : string; cls : TIncludePathPackageClass);

implementation

uses
  unitObjectCache;

var
  gRegisteredPackages : TClassStringAssociations;

procedure RegisterIncludePathPackage (const key : string; cls : TIncludePathPackageClass);
begin
  if gRegisteredPackages = Nil then
    gRegisteredPackages := TClassStringAssociations.Create;

  gRegisteredPackages.Associate(key, cls);
end;

function GetIncludePathForPackage (const PackageName : string) : string;
var
  packages : TIncludePathPackages;
begin
  packages := TIncludePathPackages.Create;
  Result := packages.IncludePath [PackageName]
end;

{ TIncludePathPackages }

procedure TIncludePathPackages.Analyze;
var
  i : Integer;
  pckg : TIncludePathPackage;
begin
  if fIncludePathPackages <> Nil then Exit;

  fIncludePathPackages := TObjectList.Create;

  for i := gRegisteredPackages.Count - 1 downto 0 do
  begin
    pckg := TIncludePathPackageClass (gRegisteredPackages.Classes [i]).Create;
    if pckg.Installed then
      fIncludePathPackages.Add(pckg)
    else
      pckg.Free
  end
end;

function TIncludePathPackages.FindIncludePathPackage(
  const PackageName: string): TIncludePathPackage;
var
  i : Integer;
begin
  Result := Nil;
  for i := 0 to Count - 1 do
    if SameText (Package [i].Name, PackageName) then
    begin
      Result := Package [i];
      break
    end
end;

function TIncludePathPackages.GetCount: Integer;
begin
  Analyze;
  Result := fIncludePathPackages.Count;
end;

function TIncludePathPackages.GetIncludePath(const PackageName: string): string;
var
  incl : TIncludePathPackage;
begin
  incl := FindIncludePathPackage (PackageName);
  if Assigned(incl) then
    Result := incl.IncludePath
  else
    Result := ''
end;

function TIncludePathPackages.GetPackage(idx: Integer): TIncludePathPackage;
begin
  Analyze;
  Result := TIncludePathPackage (fIncludePathPackages [idx]);
end;

{ TIncludePathPackage }

function TIncludePathPackage.GetIncludePath: string;
begin
  Result := GetRootDirectory + '\Include'
end;

function TIncludePathPackage.GetInstalled: boolean;
begin
  Result := DirectoryExists (GetRootDirectory);
end;

function TIncludePathPackage.GetName: string;
var
  i : Integer;
begin
  for i := 0 to gRegisteredPackages.Count - 1 do
    if self is gRegisteredPackages.Classes [i] then
    begin
      Result := gRegisteredPackages.Strings [i];
      break
    end
end;

initialization

finalization
  gRegisteredPackages.Free
end.
