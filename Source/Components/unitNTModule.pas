(*======================================================================*
 | unitNTModule unit                                                    |
 |                                                                      |
 | Load resources from a module                                         |
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
 | Copyright © Colin Wilson 2002.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      04/04/2002  CPWW  Original                                  |
 |          28/05/2005  CPWW  Implemented changing resources.           |
 *======================================================================*)

unit unitNTModule;

interface

uses
  Windows, Classes, SysUtils, unitResourceDetails, Contnrs;

type
  TNTModule = class (TResourceModule)
  private
    FDetailList: TObjectList;
    FTag: Integer;
    FOrigFileName: string;

    procedure AddResourceToList (AType, AName: PWideChar; ADataLen:  Integer; AData: pointer; ALang: Word);
    function LoadResourceFromModule (hModule: Integer; const ResType, ResName: PWideChar; language: Word): Boolean;
  protected
    function GetResourceCount: Integer; override;
    function GetResourceDetails(idx: Integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile (const FileName: string); override;
    procedure SaveToFile (const FileName: string); override;
    procedure LoadResources (const fileName: string; tp: PWideChar);
    procedure DeleteResource (resourceNo: Integer); override;
    procedure InsertResource (idx: Integer; Details: TResourceDetails); override;
    function AddResource (Details: TResourceDetails): Integer; override;
    function IndexOfResource (Details: TResourceDetails): Integer; override;
    procedure SortResources; override;
    property Tag: Integer read FTag write FTag;
  end;

implementation

resourcestring
  rstCantUpdate = 'Must use Windows NT, 2000 or XP to update resoures';

type
  TfnBeginUpdateResource = function (pFileName: PWideChar; bDeleteExistingResources: BOOL): THandle; stdcall;
  TfnUpdateResource = function (hUpdate: THandle; lpType, lpName: PWideChar; wLanguage: Word; lpData: Pointer; cbData: DWORD): BOOL; stdcall;
  TfnEndUpdateResource = function (hUpdate: THandle; fDiscard: BOOL): BOOL; stdcall;

var
  fnBeginUpdateResource: TfnBeginUpdateResource = Nil;
  fnEndUpdateResource: TfnEndUpdateResource = Nil;
  fnUpdateResource: TfnUpdateResource = Nil;

(*----------------------------------------------------------------------------*
 | function EnumResLangProc ()                                                |
 |                                                                            |
 | Callback for EnumResourceLanguages                                         |
 |                                                                            |
 | lParam contains the resource module instance.                              |
 *----------------------------------------------------------------------------*)
function EnumResLangProc (hModule: Integer; ResType, ResName: PWideChar; wIDLanguage: Word; lParam: Integer): BOOL; stdcall;
begin
  TNTModule (lParam).LoadResourceFromModule (hModule, ResType, ResName, wIDLanguage);
  Result := True
end;

(*----------------------------------------------------------------------*
 | EnumResNamesProc                                                     |
 |                                                                      |
 | Callback for EnumResourceNames                                       |
 |                                                                      |
 | lParam contains the resource module instance.                        |
 *----------------------------------------------------------------------*)
function EnumResNamesProc (hModule: Integer; ResType, ResName: PWideChar; lParam: Integer): BOOL; stdcall;
begin
  if not EnumResourceLanguagesW (hModule, ResType, ResName, @EnumResLangProc, lParam) then
    RaiseLastOSError;
  Result := True;
end;

(*----------------------------------------------------------------------*
 | EnumResTypesProc                                                     |
 |                                                                      |
 | Callback for EnumResourceTypes                                       |
 |                                                                      |
 | lParam contains the resource module instance.                        |
 *----------------------------------------------------------------------*)
function EnumResTypesProc (hModule: Integer; ResType: PWideChar; lParam: Integer): BOOL; stdcall;
begin
  EnumResourceNamesW (hModule, ResType, @EnumResNamesProc, lParam);
  Result := True;
end;


{ TNTModule }

const
  rstNotSupported = 'Not supported';

(*----------------------------------------------------------------------*
 | TNTModule.AddResourceToList                                          |
 |                                                                      |
 | Add resource to the resource Details list                            |
 *----------------------------------------------------------------------*)
function TNTModule.AddResource(Details: TResourceDetails): Integer;
begin
  Result := FDetailList.Add(Details)
end;

procedure TNTModule.AddResourceToList(AType, AName: PWideChar;
  ADataLen: Integer; AData: pointer; ALang: Word);
var
  Details: TResourceDetails;

  function ws (ws: PWideChar): WideString;
  begin
    if (Integer (ws) and $ffff0000) <> 0 then
      Result := ws
    else
      Result := IntToStr (Integer (ws))
  end;

begin
  Details := TResourceDetails.CreateResourceDetails(Self, ALang, ws (AName),
    ws (AType), ADataLen, AData);
  FDetailList.Add(Details);
end;

(*----------------------------------------------------------------------*
 | TNTModule.Create                                                     |
 |                                                                      |
 | Constructor for TNTModule                                            |
 *----------------------------------------------------------------------*)
constructor TNTModule.Create;
begin
  inherited Create;
  FDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | TNTModule.Destroy                                                    |
 |                                                                      |
 | Destructor for TNTModule                                             |
 *----------------------------------------------------------------------*)
procedure TNTModule.DeleteResource(resourceNo: Integer);
var
  res: TResourceDetails;
begin
  res := ResourceDetails [resourceNo];
  inherited;
  resourceNo := IndexOfResource (Res);
  if resourceNo <> -1 then
    FDetailList.Delete (resourceNo);
end;

destructor TNTModule.Destroy;
begin
  FDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TNTModule.GetResourceCount                                           |
 |                                                                      |
 | Get method for ResourceCount property                                |
 *----------------------------------------------------------------------*)
function TNTModule.GetResourceCount: Integer;
begin
  Result := FDetailList.Count
end;

(*----------------------------------------------------------------------*
 | TNTModule.GetResourceDetails                                         |
 |                                                                      |
 | Get method for resource Details property                             |
 *----------------------------------------------------------------------*)
function TNTModule.GetResourceDetails(idx: Integer): TResourceDetails;
begin
  Result := TResourceDetails (FDetailList [idx])
end;

(*----------------------------------------------------------------------*
 | TNTModule.IndexOfResource                                            |
 |                                                                      |
 | Find the index for specified resource Details                        |
 *----------------------------------------------------------------------*)
function TNTModule.IndexOfResource(Details: TResourceDetails): Integer;
begin
  Result := FDetailList.IndexOf (Details);
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadFromFile                                               |
 |                                                                      |
 | Load all of a module's resources                                     |
 *----------------------------------------------------------------------*)
procedure TNTModule.InsertResource(idx: Integer; Details: TResourceDetails);
begin
  FDetailList.Insert(idx, Details);
end;

procedure TNTModule.LoadFromFile(const FileName: string);
begin
  LoadResources (FileName, Nil);
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadResourceFromModule                                     |
 |                                                                      |
 | Load a particular resource from a resource handle.  Called from      |
 | EnumResLangProc when enumerating resources                           |
 *----------------------------------------------------------------------*)
function TNTModule.LoadResourceFromModule(hModule: Integer; const ResType,
  ResName: PWideChar; language: Word): Boolean;
var
  ResourceHandle: Integer;
  InfoHandle, Size: Integer;
  p: PChar;
  pt, pn: PWideChar;
  wType, wName: WideString;
begin
  Result := True;
  ResourceHandle := Windows.FindResourceW (hModule, ResName, ResType);
  if ResourceHandle <> 0 then
  begin
    Size := SizeOfResource (hModule, ResourceHandle);
    InfoHandle := LoadResource (hModule, ResourceHandle);
    if InfoHandle <> 0 then
    try
      p := LockResource (InfoHandle);

      if (Integer (ResType) and $ffff0000) = 0 then
        pt := PWideChar (ResType)
      else
      begin
        wType := ResType;
        pt := PWideChar (wType)
      end;

      if (Integer (ResName) and $ffff0000) = 0 then
        pn := PWideChar (ResName)
      else
      begin
        wName := ResName;
        pn := PWideChar (wName)
      end;

      AddResourceToList (pt, pn, Size, p, language);
    finally
      FreeResource (InfoHandle)
    end
    else
      RaiseLastOSError;
  end
  else
    RaiseLastOSError;
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadResources                                              |
 |                                                                      |
 | Load resources of a particular type                                  |
 *----------------------------------------------------------------------*)
procedure TNTModule.LoadResources(const fileName: string; tp: PWideChar);
var
  Instance: THandle;
begin
  Instance := LoadLibraryEx (PChar (fileName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Instance <> 0 then
  try
    FOrigFileName := fileName;
    FDetailList.Clear;
    if tp = Nil then
      EnumResourceTypesW (Instance, @EnumResTypesProc, Integer (Self))
    else
    begin                           // ... no.  Load specified type...
                                    // ... but if that's an Icon or Cursor group, load
                                    // the icons & cursors, too!

      if tp = PWideChar (RT_GROUP_ICON) then
        EnumResourceNamesW (Instance, PWideChar (RT_ICON), @EnumResNamesProc, Integer (Self))
      else
        if tp = PWideChar (RT_GROUP_CURSOR) then
          EnumResourceNamesW (Instance, PWideChar (RT_CURSOR), @EnumResNamesProc, Integer (Self));

      EnumResourceNamesW (Instance, tp, @EnumResNamesProc, Integer (Self))
    end
  finally
    FreeLibrary (Instance)
  end
  else
    RaiseLastOSError;
end;

(*----------------------------------------------------------------------*
 | TNTModule.SaveToFile                                                 |
 |                                                                      |
 | Update the module's resources.                                       |
 *----------------------------------------------------------------------*)
procedure TNTModule.SaveToFile(const FileName: string);
var
  UpdateHandle: THandle;
  i: integer;
  Details: TResourceDetails;
  Discard: Boolean;
  Namest, tpst: Widestring;
  fn1, fn2: string;

  function ResourceNameInt (const name: WideString): PWideChar;
  var
    n: Integer;
  begin
    n := WideResourceNameToInt (name);
    if n = -1 then
      Result := PWideChar (name)
    else
      Result := PWideChar (n)
  end;

begin
  if not Assigned(fnUpdateResource) or not Assigned(fnBeginUpdateResource) or not Assigned(fnEndUpdateResource) then
    raise Exception.Create (rstCantUpdate);

  if (FOrigFileName <> '') then
  begin
    fn1 := ExpandFileName (FOrigFileName);
    fn2 := ExpandFileName (FileName);

    if not SameText (fn1, fn2) then
      CopyFile (PChar (fn1), PChar (fn2), false);
  end;

  Discard := True;
  UpdateHandle := fnBeginUpdateResource (PWideChar (WideString (FileName)), true);
  if UpdateHandle <> 0 then
  try
    for i := 0 to ResourceCount - 1 do
    begin
      Details := ResourceDetails [i];

      Namest := Details.ResourceName;
      tpst   := Details.ResourceType;

      if not fnUpdateResource (UpdateHandle,
                      ResourceNameInt (tpst),
                      ResourceNameInt (Namest),
                      Details.ResourceLanguage,
                      Details.Data.Memory,
                      Details.Data.Size) then
        RaiseLastOSError;
    end;
    ClearDirty;
    Discard := False
  finally
    fnEndUpdateResource (UpdateHandle, Discard);
    FOrigFileName := FileName;
  end
  else
    RaiseLastOSError
end;

procedure Initialize;
var
  hkernel: THandle;
begin
  hkernel := LoadLibrary ('kernel32.dll');
  fnBeginUpdateResource := TfnBeginUpdateResource (GetProcAddress (hkernel, 'BeginUpdateResourceW'));
  fnEndUpdateResource := TfnEndUpdateResource (GetProcAddress (hkernel, 'EndUpdateResourceW'));
  fnUpdateResource := TfnUpdateResource (GetProcAddress (hkernel, 'UpdateResourceW'));
end;

procedure TNTModule.SortResources;
begin
  FDetailList.Sort (compareDetails);
end;

begin
  Initialize
end.
