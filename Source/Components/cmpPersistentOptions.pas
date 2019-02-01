(*======================================================================*
 | cmpPersistentOptions                                                 |
 |                                                                      |
 | TRegistryPersistentOptions & TIniFilePersistentOptions components    |
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
 | Copyright © Colin Wilson 2003  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      10/09/2003  CPWW  Original                                  |
 | 2.0      27/04/2004  CPWW  Added impersonation so that interactive   |
 |                            services can access HKEY_CURRENT_USER     |
 |                            for the logged-on user.                   |
 *======================================================================*)

unit cmpPersistentOptions;

interface

uses
  Windows, SysUtils, Classes, Registry, IniFiles, Menus;

const
  systemUser = 'SYSTEM';

type
  TOptions = class;
  TSections = class;
  TPersistentOptions = class;

  TOptionType = (otInteger, otBoolean, otString, otEnum);

//---------------------------------------------------------------------
// TOption class.
  TOption = class (TCollectionItem)
  private
    FDefaultValue: string;
    FName: string;
    FEnumValues: TStringList;
    FOptionType: TOptionType;
    FIntVal: Integer;
    FStrVal: string;
    FBoolVal: Boolean;
    FDirty: Boolean;

    function GetBase: TPersistentOptions;
    function GetAsBoolean: Boolean;
    function GetAsInteger: Integer;
    function GetAsString: string;
    function GetAsEnum: string;

    procedure SetEnumValues(const Value: TStrings);
    function GetEnumValues: TStrings;
    procedure SetOptionType(const Value: TOptionType);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsEnum(const Value: string);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: string);
    procedure Flush;
    function GetHasDefaultValue: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Base: TPersistentOptions read GetBase;

    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsEnum: string read GetAsEnum write SetAsEnum;

    property HasDefaultValue: Boolean read GetHasDefaultValue;

  published
    property Name: string read FName write FName;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property EnumValues: TStrings read GetEnumValues write SetEnumValues;
    property OptionType: TOptionType read FOptionType write SetOptionType;
  end;

//---------------------------------------------------------------------
// TOptions class - a collection of options
  TOptions = class (TOwnedCollection)
  private
    FDeleteObsoleteOptions: Boolean;
    function GetOption(idx: Integer): TOption;
    function GetOptionByName(const name: string): TOption;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    property Option [idx: Integer]: TOption read GetOption; default;
    property OptionByName [const name: string]: TOption read GetOptionByName;
  published
    property DeleteObsoleteOptions: Boolean read FDeleteObsoleteOptions write FDeleteObsoleteOptions default True;
  end;

//---------------------------------------------------------------------
// TSection class
  TSection = class (TCollectionItem)
  private
    FName: string;
    FOptions: TOptions;
    FSections: TSections;
    function GetOption(const name: string): TOption;
    function GetSection(const name: string): TSection;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Option [const name: string]: TOption read GetOption;
    property Section [const name: string]: TSection read GetSection;
  published
    property Name: string read FName write FName;
    property Options: TOptions read FOptions write FOptions;
    property Sections: TSections read FSections write FSections;
  end;

//---------------------------------------------------------------------
// TSections class - a collection of sections.
  TSections = class (TOwnedCollection)
  private
    FDeleteObsoleteSections: Boolean;
    function GetSection(idx: Integer): TSection;
    function GetSectionByName(const name: string): TSection;
  public
    property Section [idx: Integer]: TSection read GetSection; default;
    property SectionByName [const name: string]: TSection read GetSectionByName;
  published
    property DeleteObsoleteSections: Boolean read FDeleteObsoleteSections write FDeleteObsoleteSections default False;
  end;

//---------------------------------------------------------------------
// TPersistentOptions - base class for TRegistryPersistentOptions and
// TIniFilePersistentOptions
  TPersistentOptions = class(TComponent)
  private
    function GetPersist: Boolean;
    function GetLoading: Boolean;
  private
    FManufacturer: string;
    FApplication: string;
    FVersion: string;
    FOptions: TOptions;
    FSections: TSections;
    FUpdating: Boolean;
    function GetDesigning: Boolean;
    function GetOption(path: string): TOption;
    function GetSection(name: string): TSection;
    procedure SetApplication(const Value: string);
    procedure SetManufacturer(const Value: string);
    procedure SetVersion(const Value: string);
    procedure RemoveLeadingSlash(var path: string);

    property Designing: Boolean read GetDesigning;
    property Loading: Boolean read GetLoading;
    property Persist: Boolean read GetPersist;
    function GetDirty: Boolean;
  protected
    procedure Loaded; override;
    procedure DeleteOldOptions (const application, manufacturer, version: string); virtual; abstract;
    property InUpdate: Boolean read FUpdating;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Option [path: string]: TOption read GetOption; default;
    property Section [name: string]: TSection read GetSection;
    property Dirty: Boolean read GetDirty;
  published
    property Application: string read FApplication write SetApplication;
    property Manufacturer: string read FManufacturer write SetManufacturer;
    property Version: string read FVersion write SetVersion;
    property Options: TOptions read FOptions write FOptions;
    property Sections: TSections read FSections write FSections;
  end;

  TRegistryOptionsType = (otUser, otMachine);
//---------------------------------------------------------------------
// TRegistryPersistentOptions class  - persistent options held in the
// registry
  TRegistryPersistentOptions = class (TPersistentOptions)
  private
    FRegistryOptionsType: TRegistryOptionsType;
    function GetRootKeyName: string;
    procedure LoadOptions (reg: TRegistry; Options: TOptions; forceDefaults: Boolean);
    procedure LoadSections (reg: TRegistry; Sections: TSections; forceDefaults: Boolean);
    procedure SaveOptions (reg: TRegistry; Options: TOptions);
    procedure SaveSections (reg: TRegistry; Sections: TSections);
  protected
    procedure DeleteOldOptions (const application, manufacturer, version: string); override;
  public
    procedure Load; override;
    procedure Save; override;
    property RootKeyName: string read GetRootKeyName;
  published
    property OptionsType: TRegistryOptionsType read FRegistryOptionsType write FRegistryOptionsType;
  end;

//---------------------------------------------------------------------
// TRegistryPersistentOptions class  - persistent options held in an INI
// file
  TIniFilePersistentOptions = class (TPersistentOptions)
  private
    FFileName: string;
    function GetFileName: string;
    procedure LoadOptions (iniFile: TIniFile; const section: string; Options: TOptions; forceDefaults: Boolean);
    procedure LoadSections (iniFile: TIniFile; const section: string; Sections: TSections; forceDefaults: Boolean);
    procedure SaveOptions (iniFile: TIniFile; const section: string; Options: TOptions);
    procedure SaveSections (iniFile: TIniFile; const section: string; Sections: TSections);
    procedure SetFileName(const Value: string);
  protected
    procedure DeleteOldOptions (const application, manufacturer, version: string); override;
  public
    procedure Load; override;
    procedure Save; override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

  EOptionError = class (Exception)
  end;

  EOptionTypeMismatch = class (EOptionError)
  public
    constructor Create(Option: TOption);
  end;

implementation

resourcestring
  rstNotEnum = 'Not an enum type';
  rstTypeMismatch = '%s is not of %s type';
  rstNoAppName = 'Application property can not be blank';
  rstSectionNotFound = 'Section %s not found';
  rstOptionNotFound = 'Option %s not found in section %s';

const
  OptionTypeNames: array [TOptionType] of string = ('integer', 'boolean', 'string', 'emumerated');

{ TPersistentOptions }

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.BeginUpdate                             |
 |                                                                      |
 | Start updating.  Must be matched with EndUpdate                      |A
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.BeginUpdate;
begin
  FUpdating := True
end;

(*----------------------------------------------------------------------*
 | constructor TPersistentOptions.Create                                |
 |                                                                      |
 | Constructor for TPersistentOptions                                   |
 *----------------------------------------------------------------------*)
constructor TPersistentOptions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TOptions.Create(self, TOption);
  FSections := TSections.Create(self, TSection);
end;

(*----------------------------------------------------------------------*
 | destructor TPersistentOptions.Destroy                                |
 |                                                                      |
 | Destructor for TPersistentOptions                                    |
 *----------------------------------------------------------------------*)
destructor TPersistentOptions.Destroy;
begin
  if Dirty then
    Save;
  FOptions.Free;
  FSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.EndUpdate                               |
 |                                                                      |
 | End a batch update started with BeginUpdate                          |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.EndUpdate;
begin
  if FUpdating and Dirty then
    Save;

  FUpdating := False
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDesigning                             |
 |                                                                      |
 | Return True if the component is in 'design' mode.                    |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDesigning: Boolean;
begin
  Result := csDesigning in ComponentState
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDirty                                 |
 |                                                                      |
 | Return True if any option has had it's value changed, but has not    |
 | yet been persisted to the registry or INI file.                      |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDirty: Boolean;

  function OptionsDirty(options: TOptions): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to options.Count - 1 do
      if options[i].FDirty then
      begin
        Result := True;
        break
      end
  end;

  function SectionsDirty(sections: TSections): Boolean;
  var
    i: Integer;
    section: TSection;
  begin
    Result := False;
    for i := 0 to sections.Count - 1 do
    begin
      section := sections[i];
      Result := OptionsDirty(section.Options);
      if not Result then
        Result := SectionsDirty(section.Sections);
      if Result then
        break
    end
  end;

begin
  Result := OptionsDirty(Options) or SectionsDirty(Sections)
end;

(*----------------------------------------------------------------------*
 | TPersistentOptions.GetOption                                         |
 |                                                                      |
 | Return an option by name or path.  Note that as a shortcut the       |
 | path passed can contain sections - so you can say                    |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position'].Option ['Width']          |
 |                                                                      |
 | or                                                                   |
 |                                                                      |
 |   MyPersistentOptions.Option ['Position\Width']                      |
 |                                                                      |
 | or even (because it's the default property):                         |
 |                                                                      |
 |   MyPersistentOptions['Position\Width']                             |
 |                                                                      |
 | Parameters:                                                          |
 |   path: string               The option name or path                 |
 |                                                                      |
 | The function always returns a valid TOption, or raised an exception  |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetLoading: Boolean;
begin
  Result := (csLoading in ComponentState);

end;

function TPersistentOptions.GetOption(path: string): TOption;
var
  p: PChar;
  n: Integer;
  s: TSection;
  secName : string;
begin
  RemoveLeadingSlash(path);
  p := StrRScan (PChar (path), '\');

  s := nil;
  if Assigned (p) then
  begin
    n := Integer (p) - Integer (PChar (path)) + 1;
    s := Sections.SectionByName [Trim (Copy(path, 1, n - 1))];
    path := Trim (Copy(path, n + 1, MaxInt));
  end;

  if Assigned (s) then
    Result := s.Options.OptionByName [path]
  else
    Result := Options.OptionByName [path];

  if Result = Nil then
  begin
    if Assigned (s) then
      secName := s.Name
    else
      secName := '[Default]';
    raise EOptionError.CreateFmt(rstOptionNotFound, [path, secName])
  end
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetPersist                               |
 |                                                                      |
 | Return true if changes to the option values should be persisted to   |
 | the registry or INI file - ie. it's not in design mode, and it's not |
 | Loading.                                                             |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetPersist: Boolean;
begin
  Result := not Designing and not (csLoading in ComponentState);
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetSection                               |
 |                                                                      |
 | Return a section by name or path.  Note that as a shortcut the       |
 | path passed can contain sub-sections - so you can say                |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position'].Section ['Attributes']    |
 |                                                                      |
 | or                                                                   |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position\Attributes']                |
 |                                                                      |
 | Parameters:                                                          |
 |   name: string               The section name or path                |
 |                                                                      |
 | The function returns a valid TSection, or raises an exception        |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetSection(name: string): TSection;
begin
  RemoveLeadingSlash(name);
  Result := Sections.SectionByName [name]
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.Loaded                                  |
 |                                                                      |
 | Overridden 'Loaded' method.  Load the registry or ini file           |
 | information.                                                         |
 *----------------------------------------------------------------------*)

procedure TPersistentOptions.Loaded;
begin
  inherited;
  Load
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.RemoveLeadingSlash                      |
 |                                                                      |
 | Remove the leading slash from a path.                                |
 *----------------------------------------------------------------------*)

procedure TPersistentOptions.RemoveLeadingSlash(var path: string);
begin
  if Copy(path, 1, 1) = '\' then
    path := Copy(path, 2, MaxInt);
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetApplication                          |
 |                                                                      |
 | Set method for 'Application' property.  If this is changed at        |
 | runtime clear the old .ini file, or delete the old registry entries  |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Application' value         |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetApplication(const Value: string);
var
  oldApplication: string;
begin
  if FApplication <> Value then
  begin
    oldApplication := FApplication;
    try
      FApplication := Value;

      if not (csLoading in ComponentState) then
        Save;
    except
      FApplication := oldApplication;
      raise
    end;

    DeleteOldOptions (oldApplication, Manufacturer, Version);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetManufacturer                         |
 |                                                                      |
 | Set method for 'Manufacturer' property.  If this is changed at       |
 | runtime clear the old .ini file, or delete the old registry entries  |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Manufacturer' value        |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetManufacturer(const Value: string);
var
  oldManufacturer: string;
begin
  if FManufacturer <> Value then
  begin
    oldManufacturer := FManufacturer;
    try
      FManufacturer := Value;
      if not (csLoading in ComponentState) then
        Save;
    except
      FManufacturer := oldManufacturer;
      raise
    end;

    DeleteOldOptions (Application, oldManufacturer, Version);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetVersion                              |
 |                                                                      |
 | Set method for 'Version' property.  If this is changed at runtime    |
 | clear the old .ini file, or delete the old registry entries          |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Version' value             |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetVersion(const Value: string);
var
  oldVersion: string;
begin
  if FVersion <> Value then
  begin
    oldVersion := FVersion;
    try
      FVersion := Value;
      if not (csLoading in ComponentState) then
        Save
    except
      FVersion := oldVersion;
      raise
    end;

    DeleteOldOptions (Application, Manufacturer, oldVersion)
  end
end;

{ TIniFilePersistentOptions }

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.DeleteOldOptions                 |
 |                                                                      |
 | Delete old .ini file if it's name has changed.                       |
 |                                                                      |
 | Parameters:                                                          |
 |   const application, manufacturer, version: string                   |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.DeleteOldOptions(const application,
  manufacturer, version: string);
var
  appSave, manSave, verSave, f: string;
begin
  if not Persist then Exit;

  f := GetFileName;
  appSave := self.Application;
  manSave := self.Application;
  verSave := self.Application;
  try
    self.FApplication := application;
    self.FManufacturer := manufacturer;
    self.FVersion := version;

    if GetFileName <> f then
      DeleteFile(GetFileName)
  finally
    self.FApplication := application;
    self.FManufacturer := manSave;
    self.FVersion := verSave
  end
end;

(*----------------------------------------------------------------------*
 | function TIniFilePersistentOptions.GetFileName: string              |
 |                                                                      |
 | Get the INI file name                                                |
 *----------------------------------------------------------------------*)
function TIniFilePersistentOptions.GetFileName: string;
var
  st: string;
  p: Integer;
begin
  if FFileName <> '' then       // Use the FileName property if it's been set
  begin
    if (Copy(fileName, 1, 1) = '\') or (Pos (':', fileName) <> 0) then
      Result := FFileName
    else
      Result := ExtractFilePath(ParamStr (0)) + FFileName
  end
  else                          // Otherwise derive the file name from the
  begin                         // application name
    if Application = '' then
    begin
      st := ExtractFileName(ParamStr (0));
      p := Pos ('.', st);
      if p > 0 then
        st := Copy(st, 1, p - 1)
    end
    else
      st := Application;

    Result := ExtractFilePath(ParamStr (0)) + st + '.ini'
  end
end;

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.Load                             |
 |                                                                      |
 | Load all the options from the INI file                               |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.Load;
var
  f: TIniFile;
  openedOK: Boolean;
  fileName: string;
begin
  if not Persist then Exit;

  fileName := GetFileName;
  openedOK := FileExists (fileName);
  f := TIniFile.Create(fileName);
  try
    LoadOptions (f, '', Options, not openedOK);
    LoadSections (f, '', Sections, not openedOK)
  finally
    f.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.LoadOptions                      |
 |                                                                      |
 | Load the options from a section.                                     |
 |                                                                      |
 | Parameters:                                                          |
 |   iniFile: TIniFile;         The INI file to load from               |
 |   const section: string;    The section to load                     |
 |   Options: TOptions;         Options to populate                     |
 |   forceDefaults: Boolean     Use default values, rather than load    |
 |                              from the INI file.  This will be true   |
 |                              if the INI file could not be found.     |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.LoadOptions(iniFile: TIniFile; const section: string;
  Options: TOptions; forceDefaults: Boolean);
var
  i: Integer;
  option: TOption;
  sec: string;
  sl: THashedStringList;
begin
  sec := section;
  if sec = '' then
    sec := 'Defaults';

  sl := THashedStringList.Create;
  try
    if not forceDefaults then
    begin
      iniFile.ReadSectionValues(sec, sl);
      forceDefaults := sl.Count = 0
    end;

    for i := 0 to Options.Count - 1 do
    begin
      option := Options[i];

      if forceDefaults or (sl.IndexOfName(option.Name) = -1) then
      case option.OptionType of
        otString : option.FStrVal  := option.FDefaultValue;
        otInteger: option.FIntVal  := StrToIntDef (option.FDefaultValue, 0);
        otBoolean: option.FBoolVal := StrToBoolDef (option.FDefaultValue, False);
        otEnum   : option.FIntVal  := option.FEnumValues.IndexOf(option.FDefaultValue)
      end
      else
      case option.OptionType of
        otString : option.FStrVal  := sl.Values[option.Name];
        otInteger: option.FIntVal  := StrToIntDef (sl.Values[option.Name], StrToIntDef (option.FDefaultValue, 0));
        otBoolean: option.FBoolVal := StrToBoolDef (sl.Values[option.Name], StrToBoolDef (option.FDefaultValue, False));
        otEnum   : option.FIntVal  := StrToIntDef (sl.Values[option.Name], option.FEnumValues.IndexOf(option.FDefaultValue))
      end;
      option.FDirty := False
    end
  finally
    sl.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.LoadSections                     |
 |                                                                      |
 | Load sections (and their options & subsections) from the INI file    |
 |                                                                      |
 | Parameters:                                                          |
 |   iniFile: TIniFile;                 The INI file to load from       |
 |   const section: string             Root section name               |
 |   Sections: TSections                The sections to load            |
 |   forceDefaults: Boolean             True if default values should   |
 |                                      always be used for the options. |
 |                                      (ie if the INI file could not   |
 |                                       be found.)                     |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.LoadSections(iniFile: TIniFile; const section: string;
  Sections: TSections; forceDefaults: Boolean);
var
  i: Integer;
  sect: TSection;
  ss: string;
begin
  for i := 0 to Sections.Count - 1 do
  begin
    sect := Sections[i];
    if section = '' then
      ss := sect.Name
    else
      ss := section + '\' + sect.Name;

    LoadOptions (iniFile, ss, sect.Options, forceDefaults);
    LoadSections (iniFile, ss, sect.Sections, forceDefaults);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.Save                             |
 |                                                                      |
 | Save the sections/options in the INI file                            |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.Save;
var
  f: TIniFile;
begin
  if not Persist then Exit;

  f := TIniFile.Create(GetFileName);
  try
    SaveOptions (f, '', Options);
    SaveSections (f, '', Sections)
  finally
    f.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.SaveOptions                      |
 |                                                                      |
 | Save a section's options in the INI file.                            |
 |                                                                      |
 | Parameters:                                                          |
 |   iniFile: TIniFile                  The INI file to save to.        |
 |   const section: string             The section to save             |
 |   Options: TOptions                  Options to save in the section  |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.SaveOptions(iniFile: TIniFile; const section: string;
  Options: TOptions);
var
  i: Integer;
  option: TOption;
  sec: string;

begin
  sec := section;
  if sec = '' then
     sec := 'Defaults';

  if Options.DeleteObsoleteOptions then
    iniFile.EraseSection(sec);

  for i := 0 to Options.Count - 1 do
  begin
    option := Options[i];

    if not option.HasDefaultValue then
    case option.OptionType of
      otString : iniFile.WriteString(sec, option.Name, option.AsString);
      otInteger: iniFile.WriteInteger(sec, option.Name, option.AsInteger);
      otBoolean: iniFile.WriteBool(sec, option.Name, option.AsBoolean);
      otEnum   : iniFile.WriteString(sec, option.Name, option.GetAsEnum);
    end;
    option.FDirty := False;
  end
end;

(*----------------------------------------------------------------------*
 | TIniFilePersistentOptions.SaveSections                               |
 |                                                                      |
 | Save sections (and their options & subsections) to an INI file       |
 |                                                                      |
 | Parameters:                                                          |
 |   iniFile: TIniFile                  The INI file to save to.        |
 |   const section: string             The root section to save        |
 |   Sections: TSections                Sections to save                |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.SaveSections(iniFile: TIniFile; const section: string;
  Sections: TSections);
var
  i: Integer;
  sect: TSection;
  ss: string;
begin
  for i := 0 to Sections.Count - 1 do
  begin
    sect := Sections[i];
    if section = '' then
      ss := sect.Name
    else
      ss := section + '\' + sect.Name;

    SaveOptions (iniFile, ss, sect.Options);
    SaveSections (iniFile, ss, sect.Sections);
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.SetFileName                      |
 |                                                                      |
 | 'Set' method for the FileName property                               |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.SetFileName(const Value: string);
begin
  if value <> FFileName then
  begin
    FFileName := Value;

    // Delete the old INI file if we're not in design mode.
    DeleteOldOptions (Application, Manufacturer, Version);
  end
end;

{ TRegistryPersistentOptions }

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.DeleteOldOptions                |
 |                                                                      |
 | Delete the old registry settings after changing the root key(by     |
 | changine the application, manufacturer or version properties)        |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.DeleteOldOptions(const application,
  manufacturer, version: string);
var
  keyName: string;
  reg: TRegistry;
begin
  if not Persist then Exit;

  if Application = '' then
    raise EOptionError.Create(rstNoAppName);

  if Manufacturer <> '' then
    keyName := '\Software\' + Manufacturer + '\' + Application
  else
    keyName := '\Software\' + Application;

  if Version <> '' then
    keyName := keyName + '\' + Version;

  if keyName <> GetRootKeyName then
  begin
    reg := TRegistry.Create(KEY_READ or KEY_WRITE);
    try
      reg.DeleteKey(keyName)
    finally
      reg.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | function TRegistryPersistentOptions.GetRootKeyName                   |
 |                                                                      |
 | Get the root key name for the options.  This is...                   |
 |                                                                      |
 | \Software\[<Manufacturer>\]<Application\[<Version>]                  |
 *----------------------------------------------------------------------*)
function TRegistryPersistentOptions.GetRootKeyName: string;
begin
  if Application = '' then
    raise EOptionError.Create(rstNoAppName);

  if Manufacturer <> '' then
    Result := '\Software\' + Manufacturer + '\' + Application
  else
    Result := '\Software\' + Application;

  if Version <> '' then
    Result := Result + '\' + Version
end;

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.Load                            |
 |                                                                      |
 | Load all the sections & options from the registry                    |
 *----------------------------------------------------------------------*)

procedure TRegistryPersistentOptions.Load;
var
  reg: TRegistry;
  openedOK: Boolean;
begin
  if not Persist then Exit;

  reg := TRegistry.Create(KEY_READ);
  try
    if OptionsType = otMachine then
      Reg.RootKey := HKEY_LOCAL_MACHINE;

    openedOK := Reg.OpenKey(RootKeyName, False);
    LoadOptions (reg, Options, not openedOK);
    LoadSections (reg, Sections, not openedOK)
  finally
    reg.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.LoadOptions                     |
 |                                                                      |
 | Load options from a registry branch                                  |
 |                                                                      |
 | Parameters:                                                          |
 |   reg: TRegistry                     Registry to load from           |
 |   Options: TOptions                  Options to load                 |
 |   forceDefaults: Boolean            True if defaults should be used |
 |                                      This is set if the root         |
 |                                      registry key didn't exist.      |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.LoadOptions(reg: TRegistry; Options: TOptions; forceDefaults: Boolean);
var
  i: Integer;
  option: TOption;
begin
  for i := 0 to Options.Count - 1 do
  begin
    option := Options[i];

    if forceDefaults or not reg.ValueExists(option.Name) then
    case option.OptionType of
      otString : option.FStrVal  := option.FDefaultValue;
      otInteger: option.FIntVal  := StrToIntDef (option.FDefaultValue, 0);
      otBoolean: option.FBoolVal := StrToBoolDef (option.FDefaultValue, False);
      otEnum   : option.FIntVal  := option.FEnumValues.IndexOf(option.FDefaultValue)
    end
    else
    case option.OptionType of
      otString : option.FStrVal  := reg.ReadString  (option.Name);
      otInteger: option.FIntVal  := reg.ReadInteger (option.Name);
      otBoolean: option.FBoolVal := reg.ReadBool    (option.Name);
      otEnum   : option.FIntVal  := reg.ReadInteger (option.Name)
    end;
    option.FDirty := False
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.LoadSections                    |
 |                                                                      |
 | Load sections (and their options and subsections) from the registry  |
 |                                                                      |
 | Parameters:                                                          |
 |   reg: TRegistry             Root key                                |
 |   Sections: TSections        Sections to load                        |
 |   forceDefaults: Boolean    'True' is defaults should be used       |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.LoadSections(reg: TRegistry; Sections: TSections; forceDefaults: Boolean);
var
  i: Integer;
  section: TSection;
  reg1: TRegistry;
begin
  if not forceDefaults then
    reg1 := TRegistry.Create(KEY_READ)
  else
    reg1 := nil;

  try
    for i := 0 to Sections.Count - 1 do
    begin
      section := Sections[i];

      if not forceDefaults then
      begin
        reg1.RootKey := reg.CurrentKey;
        forceDefaults := not reg1.OpenKey(section.Name, False)
      end;
      LoadOptions (reg1, section.Options, forceDefaults);
      LoadSections (reg1, section.Sections, forceDefaults)
    end
  finally
    reg1.Free
  end
end;


(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.Save                            |
 |                                                                      |
 | Save all sections & options to the registry                          |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.Save;
var
  reg: TRegistry;
begin
  if not Persist then Exit;

  reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    if OptionsType = otMachine then
      reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey(RootKeyName, True) then
    begin
      SaveOptions (reg, Options);
      SaveSections (reg, Sections)
    end
  finally
    reg.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.SaveOptions                     |
 |                                                                      |
 | Save options in the registry                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   reg: TRegistry             The registry to save to                 |
 |   Options: TOptions          The options to save                     |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.SaveOptions(reg: TRegistry;
  Options: TOptions);
var
  i, idx: Integer;
  deleteValues: TStringList;
  option: TOption;

begin
  deleteValues := TStringList.Create;
  try
  // Get a list of values to delete.  Start by filling it with all the values
  // then remove values as we save them, leaving only the obsolete ones.

    deleteValues.CaseSensitive := False;
    reg.GetValueNames(deleteValues);
    deleteValues.Sort;

    for i := 0 to Options.Count - 1 do
    begin
      Option := Options[i];

      if not Option.HasDefaultValue then        // Save the value if it's not the
      begin                                     // default value for the option

                                                // Remove the value from the
                                                // deleteValues list
        idx := deleteValues.IndexOf(Option.Name);
        if idx >= 0 then
          deleteValues.Delete(idx);

        case Option.OptionType of
          otString : reg.WriteString  (Option.Name, Option.FStrVal);
          otInteger: reg.WriteInteger (Option.Name, Option.FIntVal);
          otBoolean: reg.WriteBool    (Option.Name, Option.FBoolVal);
          otEnum   : reg.WriteInteger (Option.Name, Option.FIntVal)
        end
      end
      else
        reg.DeleteValue(Option.Name);           // Delete 'default' values
      Option.FDirty := False
    end;

    if Options.DeleteObsoleteOptions then          // Delete obsolete values.
      for i := 0 to deleteValues.count - 1 do
        reg.DeleteValue(deleteValues[i])
  finally
    deleteValues.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.SaveSections                    |
 |                                                                      |
 | Save sections and their options & subsections to the registry        |
 |                                                                      |
 | Parameters:                                                          |
 |   reg: TRegistry             // Root registry key                    |
 |   Sections: TSections        // The sections to save                 |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.SaveSections(reg: TRegistry;
  Sections: TSections);
var
  i, idx: Integer;
  section: TSection;
  reg1: TRegistry;
  deleteSections: TStringList;
begin
  reg1 := nil;
  deleteSections := TStringList.Create;
  try
  // Build list of obsolete sections to delete.

    deleteSections.CaseSensitive := False;
    reg.GetKeyNames(deleteSections);
    deleteSections.Sort;

  // Save the sections & options
    reg1 := TRegistry.Create(KEY_READ or KEY_WRITE);
    for i := 0 to Sections.Count - 1 do
    begin
      section := Sections[i];

      idx := deleteSections.IndexOf(Section.Name);
      if idx >= 0 then
        deleteSections.Delete(idx);
      reg1.RootKey := reg.CurrentKey;
      if reg1.OpenKey(section.Name, True) then
      begin
        SaveOptions (reg1, section.Options);
        SaveSections (reg1, section.Sections)
      end
    end;

    // Delete the obsolete sections
    reg1.RootKey := reg.CurrentKey;
    if Sections.DeleteObsoleteSections then
      for i := 0 to deleteSections.Count - 1 do
        reg1.DeleteKey(deleteSections[i])
  finally
    deleteSections.Free;
    reg1.Free
  end
end;

{ TOptions }

(*----------------------------------------------------------------------*
 | constructor TOptions.Create                                          |
 |                                                                      |
 | Constructor for TOptions collection                                  |
 *----------------------------------------------------------------------*)
constructor TOptions.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  FDeleteObsoleteOptions := True
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOption                                         |
 |                                                                      |
 | Get method for 'Option' array property                               |
 *----------------------------------------------------------------------*)
function TOptions.GetOption(idx: Integer): TOption;
begin
  Result := TOption (Items[idx])
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOptionByName                                   |
 |                                                                      |
 | 'Get' method for OptionByName array property                         |
 *----------------------------------------------------------------------*)
function TOptions.GetOptionByName(const name: string): TOption;
var
  o: TOption;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    o := Option [i];
    if AnsiSameText(Name, o.Name) then
    begin
      Result := o;
      break
    end
  end
end;

{ TOption }

(*----------------------------------------------------------------------*
 | constructor TOption.Create                                           |
 |                                                                      |
 | Constructor for TOption class                                        |
 *----------------------------------------------------------------------*)
constructor TOption.Create(Collection: TCollection);
begin
  inherited;
  if Base.Designing or Base.Loading then
  begin
    FEnumValues := TStringList.Create;
    FEnumValues.CaseSensitive := False
  end
end;

(*----------------------------------------------------------------------*
 | destructor TOption.Destroy                                           |
 |                                                                      |
 | Destructor for TOption class                                         |
 *----------------------------------------------------------------------*)
destructor TOption.Destroy;
begin
  FEnumValues.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TOption.Flush                                              |
 |                                                                      |
 | Save the option.                                                     |
 *----------------------------------------------------------------------*)
procedure TOption.Flush;
begin
  if FDirty and not Base.InUpdate then
    Base.Save
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsBoolean                                        |
 |                                                                      |
 | Return the option value if it's a boolean option - otherwise raise   |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsBoolean: Boolean;
begin
  if OptionType = otBoolean then
    Result := FBoolVal
  else
    raise EOptionTypeMismatch(self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsEnum                                           |
 |                                                                      |
 | Return the option value if it's an enum option - otherwise raise an  |
 | exception.                                                           |
 *----------------------------------------------------------------------*)
function TOption.GetAsEnum: string;
begin
  if OptionType = otEnum then
    Result := EnumValues[FIntVal]
  else
    raise EOptionTypeMismatch(self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsInteger                                        |
 |                                                                      |
 | Return the option value if it's an integer option - otherwise raise  |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsInteger: Integer;
begin
  if OptionType = otInteger then
    Result := FIntVal
  else
    raise EOptionTypeMismatch(self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsString                                         |
 |                                                                      |
 | Return the option value if it's a string option - otherwise raise    |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsString: String;
begin
  if OptionType = otString then
    Result := FStrVal
  else
    raise EOptionTypeMismatch(self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetBase                                             |
 |                                                                      |
 | Return the owning 'TPersistentOptions' derived object.               |
 *----------------------------------------------------------------------*)
function TOption.GetBase: TPersistentOptions;
var
  own: TPersistent;
begin
  own := TOwnedCollection (Collection).Owner;

  while own is TSection do
    own := TOwnedCollection (TSection (own).Collection).Owner;

  Result := own as TPersistentOptions
end;

(*----------------------------------------------------------------------*
 | function TOption.GetDisplayName                                      |
 |                                                                      |
 | Overridden from TCollectionItem base.  Helps the designer.           |
 *----------------------------------------------------------------------*)
function TOption.GetDisplayName: string;
begin
  Result := Name
end;

(*----------------------------------------------------------------------*
 | function TOption.GetEnumValues: TStringList                         |
 |                                                                      |
 | 'Get' method for EnumValues property                                 |
 *----------------------------------------------------------------------*)
function TOption.GetEnumValues: TStrings;
begin
  Result := FEnumValues
end;

(*----------------------------------------------------------------------*
 | function TOption.GetHasDefaultValue: Boolean                        |
 |                                                                      |
 | Return True if the option's current value is it's default.           |
 *----------------------------------------------------------------------*)
function TOption.GetHasDefaultValue: Boolean;
begin
  Result := False;
  case OptionType of
    otString : Result := AnsiCompareStr (DefaultValue, FStrVal) = 0;
    otInteger: Result := StrToIntDef (DefaultValue, 0) = FIntVal;
    otBoolean: Result := StrToBoolDef (DefaultValue, False) = FBoolVal;
    otEnum   : Result := FIntVal = EnumValues.IndexOf(DefaultValue)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsBoolean                                       |
 |                                                                      |
 | Set the option's value if it's a Boolean option - otherwise raise    |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsBoolean(const Value: Boolean);
begin
  if OptionType <> otBoolean then
    raise EOptionTypeMismatch(self);

  if Value <> FBoolVal then
  begin
    FDirty := True;
    FBoolVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsEnum                                          |
 |                                                                      |
 | Set the option's value if it's an enum option - otherwise raise      |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsEnum(const Value: string);
begin
  if Value <> AsEnum then
  begin
    FDirty := True;
    FIntVal := EnumValues.IndexOf(Value);
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsInteger                                       |
 |                                                                      |
 | Set the option's value if it's an integer option - otherwise raise   |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsInteger(const Value: Integer);
begin
  if OptionType <> otInteger then
    raise EOptionTypeMismatch(self);

  if Value <> FIntVal then
  begin
    FDirty := True;
    FIntVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsString                                        |
 |                                                                      |
 | Set the option's value if it's a string option - otherwise raise     |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsString(const Value: string);
begin
  if OptionType <> otString then
    raise EOptionTypeMismatch(self);

  if Value <> FStrVal then
  begin
    FDirty := True;
    FStrVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetEnumValues                                      |
 |                                                                      |
 | 'Set' method for EnumValues property                                 |
 *----------------------------------------------------------------------*)
procedure TOption.SetEnumValues(const Value: TStrings);
begin
  if (OptionType = otEnum) then
    FEnumValues.Assign(Value)
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetOptionType                                      |
 |                                                                      |
 | 'Set' method for the OptionType property.                            |
 *----------------------------------------------------------------------*)
procedure TOption.SetOptionType(const Value: TOptionType);
begin
  if FOptionType <> Value then
  begin
    if Base.Designing then
    begin
      if not Base.Loading then FEnumValues.Clear
    end
    else
      if not Base.Loading or (Value <> otEnum) then
        FreeAndNil (FEnumValues);

    FOptionType := Value;

    if FOptionType = otEnum then
      if not Base.Designing and not Base.Loading then
        FEnumValues := TStringList.Create
  end
end;

{ TSection }

(*----------------------------------------------------------------------*
 | constructor TSection.Create                                          |
 |                                                                      |
 | Constructor for TSection                                             |
 *----------------------------------------------------------------------*)
constructor TSection.Create(Collection: TCollection);
begin
  inherited;

  FOptions := TOptions.Create(self, TOption);
  FSections := TSections.Create(self, TSection);
end;

(*----------------------------------------------------------------------*
 | destructor TSection.Destroy                                          |
 |                                                                      |
 | Destructor for TSection                                              |
 *----------------------------------------------------------------------*)
destructor TSection.Destroy;
begin
  FOptions.Free;
  FSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TSection.GetDisplayName                                     |
 |                                                                      |
 | Override TCollectionItem method to help the designer                 |
 *----------------------------------------------------------------------*)
function TSection.GetDisplayName: string;
begin
  Result := Name
end;

(*----------------------------------------------------------------------*
 | function TSection.GetOption                                          |
 |                                                                      |
 | 'Get' method for Option property                                     |
 *----------------------------------------------------------------------*)
function TSection.GetOption(const name: string): TOption;
begin
  Result := Options.OptionByName [name];
end;

(*----------------------------------------------------------------------*
 | function TSection.GetSection                                         |
 |                                                                      |
 | 'Get' method for Section propery                                     |
 *----------------------------------------------------------------------*)
function TSection.GetSection(const name: string): TSection;
begin
  Result := Sections.SectionByName [name];
end;

{ EOptionTypeMismatch }

constructor EOptionTypeMismatch.Create(Option: TOption);
begin
  inherited CreateFmt(rstTypeMismatch, [Option.Name, OptionTypeNames[Option.OptionType]])
end;

{ TSections }

(*----------------------------------------------------------------------*
 | function TSections.GetSection                                        |
 |                                                                      |
 | Get method for Section property.                                     |
 *----------------------------------------------------------------------*)
function TSections.GetSection(idx: Integer): TSection;
begin
  Result := TSection (Items[idx])
end;

(*----------------------------------------------------------------------*
 | function TSections.GetSectionByName                                  |
 |                                                                      |
 | 'Get' method for SectionByName property                              |
 *----------------------------------------------------------------------*)
function TSections.GetSectionByName(const name: string): TSection;
var
  i, p: Integer;
  s: TSection;
begin
  Result := nil;

  p := Pos ('\', name);
  if p > 0 then
  begin
    s := SectionByName [Trim (Copy(name, 1, p - 1))];
    if Assigned (s) then
      Result := s.Sections.SectionByName [Trim (Copy(name, p + 1, MaxInt))]
    else
      raise EOptionError.CreateFmt(rstSectionNotFound, [s.Name]);
  end
  else
  for i := 0 to Count - 1 do
  begin
    s := Section [i];
    if AnsiSameText(name, s.Name) then
    begin
      Result := s;
      break
    end
  end;

  if not Assigned (Result) then
    raise EOptionError.CreateFmt(rstSectionNotFound, [name])
end;

end.
