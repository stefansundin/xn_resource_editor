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
    fDefaultValue: string;
    fName: string;
    fEnumValues: TStringList;
    fOptionType: TOptionType;
    fIntVal : Integer;
    fStrVal : string;
    fBoolVal : boolean;
    fDirty : boolean;

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
    function GetHasDefaultValue: boolean;
  protected
    function GetDisplayName : string; override;
  public
    constructor Create (Collection : TCollection); override;
    destructor Destroy; override;
    property Base : TPersistentOptions read GetBase;

    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsString : string read GetAsString write SetAsString;
    property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    property AsEnum : string read GetAsEnum write SetAsEnum;

    property HasDefaultValue : boolean read GetHasDefaultValue;

  published
    property Name : string read fName write fName;
    property DefaultValue : string read fDefaultValue write fDefaultValue;
    property EnumValues : TStrings read GetEnumValues write SetEnumValues;
    property OptionType : TOptionType read fOptionType write SetOptionType;
  end;

//---------------------------------------------------------------------
// TOptions class - a collection of options
  TOptions = class (TOwnedCollection)
  private
    fDeleteObsoleteOptions: boolean;
    function GetOption(idx: Integer): TOption;
    function GetOptionByName(const name: string): TOption;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    property Option [idx : Integer] : TOption read GetOption; default;
    property OptionByName [const name : string] : TOption read GetOptionByName;
  published
    property DeleteObsoleteOptions : boolean read fDeleteObsoleteOptions write fDeleteObsoleteOptions default True;
  end;

//---------------------------------------------------------------------
// TSection class
  TSection = class (TCollectionItem)
  private
    fName: string;
    fOptions: TOptions;
    fSections: TSections;
    function GetOption(const name: string): TOption;
    function GetSection(const name: string): TSection;
  protected
    function GetDisplayName : string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Option [const name : string] : TOption read GetOption;
    property Section [const name : string] : TSection read GetSection;
  published
    property Name : string read fName write fName;
    property Options : TOptions read fOptions write fOptions;
    property Sections : TSections read fSections write fSections;
  end;

//---------------------------------------------------------------------
// TSections class - a collection of sections.
  TSections = class (TOwnedCollection)
  private
    fDeleteObsoleteSections: boolean;
    function GetSection(idx: Integer): TSection;
    function GetSectionByName(const name: string): TSection;
  public
    property Section [idx : Integer] : TSection read GetSection; default;
    property SectionByName [const name : string] : TSection read GetSectionByName;
  published
    property DeleteObsoleteSections : boolean read fDeleteObsoleteSections write fDeleteObsoleteSections default False;
  end;

//---------------------------------------------------------------------
// TPersistentOptions - base class for TRegistryPersistentOptions and
// TIniFilePersistentOptions
  TPersistentOptions = class(TComponent)
  private
    function GetPersist: boolean;
    function GetLoading: boolean;
  private
    fManufacturer: string;
    fApplication: string;
    fVersion: string;
    fOptions: TOptions;
    fSections: TSections;
    fUpdating: boolean;
    function GetDesigning: boolean;
    function GetOption(path: string): TOption;
    function GetSection(name: string): TSection;
    procedure SetApplication(const Value: string);
    procedure SetManufacturer(const Value: string);
    procedure SetVersion(const Value: string);
    procedure RemoveLeadingSlash (var path : string);

    property Designing : boolean read GetDesigning;
    property Loading : boolean read GetLoading;
    property Persist : boolean read GetPersist;
    function GetDirty: boolean;
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure DeleteOldOptions (const application, manufacturer, version : string); virtual; abstract;
    property InUpdate : boolean read fUpdating;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Save; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Option [path : string] : TOption read GetOption; default;
    property Section [name : string] : TSection read GetSection;
    property Dirty : boolean read GetDirty;
    { Public declarations }
  published
    property Application : string read fApplication write SetApplication;
    property Manufacturer : string read fManufacturer write SetManufacturer;
    property Version : string read fVersion write SetVersion;
    property Options : TOptions read fOptions write fOptions;
    property Sections : TSections read fSections write fSections;
  end;

  TRegistryOptionsType = (otUser, otMachine);
//---------------------------------------------------------------------
// TRegistryPersistentOptions class  - persistent options held in the
// registry
  TRegistryPersistentOptions = class (TPersistentOptions)
  private
    fRegistryOptionsType: TRegistryOptionsType;
    function GetRootKeyName: string;
    procedure LoadOptions (reg : TRegistry; Options : TOptions; forceDefaults : boolean);
    procedure LoadSections (reg : TRegistry; Sections : TSections; forceDefaults : boolean);
    procedure SaveOptions (reg : TRegistry; Options : TOptions);
    procedure SaveSections (reg : TRegistry; Sections : TSections);
  protected
    procedure DeleteOldOptions (const application, manufacturer, version : string); override;
  public
    procedure Load; override;
    procedure Save; override;
    property RootKeyName : string read GetRootKeyName;
  published
    property OptionsType : TRegistryOptionsType read fRegistryOptionsType write fRegistryOptionsType;
  end;

//---------------------------------------------------------------------
// TRegistryPersistentOptions class  - persistent options held in an INI
// file
  TIniFilePersistentOptions = class (TPersistentOptions)
  private
    fFileName: string;
    function GetFileName : string;
    procedure LoadOptions (iniFile : TIniFile; const section : string; Options : TOptions; forceDefaults : boolean);
    procedure LoadSections (iniFile : TIniFile; const section : string; Sections : TSections; forceDefaults : boolean);
    procedure SaveOptions (iniFile : TIniFile; const section : string; Options : TOptions);
    procedure SaveSections (iniFile : TIniFile; const section : string; Sections : TSections);
    procedure SetFileName(const Value: string);
  protected
    procedure DeleteOldOptions (const application, manufacturer, version : string); override;
  public
    procedure Load; override;
    procedure Save; override;
  published
    property FileName : string read fFileName write SetFileName;
  end;

  EOptionError = class (Exception)
  end;

  EOptionTypeMismatch = class (EOptionError)
  public
    constructor Create (Option : TOption);
  end;

implementation

resourcestring
  rstNotEnum = 'Not an enum type';
  rstTypeMismatch = '%s is not of %s type';
  rstNoAppName = 'Application property can not be blank';
  rstSectionNotFound = 'Section %s not found';
  rstOptionNotFound = 'Option %s not found in section %s';

const
  OptionTypeNames : array [TOptionType] of string = ('integer', 'boolean', 'string', 'emumerated');

{ TPersistentOptions }

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.BeginUpdate                             |
 |                                                                      |
 | Start updating.  Must be matched with EndUpdate                      |A
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.BeginUpdate;
begin
  fUpdating := True
end;

(*----------------------------------------------------------------------*
 | constructor TPersistentOptions.Create                                |
 |                                                                      |
 | Constructor for TPersistentOptions                                   |
 *----------------------------------------------------------------------*)
constructor TPersistentOptions.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  fOptions := TOptions.Create(self, TOption);
  fSections := TSections.Create(self, TSection);
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
  fOptions.Free;
  fSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.EndUpdate                               |
 |                                                                      |
 | End a batch update started with BeginUpdate                          |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.EndUpdate;
begin
  if fUpdating and Dirty then
    Save;

  fUpdating := False
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDesigning                             |
 |                                                                      |
 | Return True if the component is in 'design' mode.                    |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDesigning: boolean;
begin
  result := csDesigning in ComponentState
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDirty                                 |
 |                                                                      |
 | Return True if any option has had it's value changed, but has not    |
 | yet been persisted to the registry or INI file.                      |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDirty: boolean;

  function OptionsDirty (options : TOptions) : boolean;
  var
    i : Integer;
  begin
    result := False;
    for i := 0 to options.Count - 1 do
      if options [i].fDirty then
      begin
        result := True;
        break
      end
  end;

  function SectionsDirty (sections : TSections) : boolean;
  var
    i : Integer;
    section : TSection;
  begin
    result := False;
    for i := 0 to sections.Count - 1 do
    begin
      section := sections [i];
      result := OptionsDirty (section.Options);
      if not result then
        result := SectionsDirty (section.Sections);
      if result then
        break
    end
  end;

begin
  result := OptionsDirty (Options) or SectionsDirty (Sections)
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
 |   MyPersistentOptions ['Position\Width']                             |
 |                                                                      |
 | Parameters:                                                          |
 |   path: string               The option name or path                 |
 |                                                                      |
 | The function always returns a valid TOption, or raised an exception  |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetLoading: boolean;
begin
  result := (csLoading in ComponentState);

end;

function TPersistentOptions.GetOption(path: string): TOption;
var
  p : PChar;
  n : Integer;
  s : TSection;
  secName  : string;
begin
  RemoveLeadingSlash (path);
  p := StrRScan (PChar (path), '\');

  s := Nil;
  if Assigned (p) then
  begin
    n := Integer (p) - Integer (PChar (path)) + 1;
    s := Sections.SectionByName [Trim (Copy (path, 1, n - 1))];
    path := Trim (Copy (path, n + 1, MaxInt));
  end;

  if Assigned (s) then
    result := s.Options.OptionByName [path]
  else
    result := Options.OptionByName [path];

  if result = Nil then
  begin
    if Assigned (s) then
      secName := s.Name
    else
      secName := '[Default]';
    raise EOptionError.CreateFmt (rstOptionNotFound, [path, secName])
  end
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetPersist                               |
 |                                                                      |
 | Return true if changes to the option values should be persisted to   |
 | the registry or INI file - ie. it's not in design mode, and it's not |
 | Loading.                                                             |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetPersist: boolean;
begin
  result := not Designing and not (csLoading in ComponentState);
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
  RemoveLeadingSlash (name);
  result := Sections.SectionByName [name]
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

procedure TPersistentOptions.RemoveLeadingSlash (var path : string);
begin
  if Copy (path, 1, 1) = '\' then
    path := Copy (path, 2, MaxInt);
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
  oldApplication : string;
begin
  if fApplication <> Value then
  begin
    oldApplication := fApplication;
    try
      fApplication := Value;

      if not (csLoading in ComponentState) then
        Save;
    except
      fApplication := oldApplication;
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
  oldManufacturer : string;
begin
  if fManufacturer <> Value then
  begin
    oldManufacturer := fManufacturer;
    try
      fManufacturer := Value;
      if not (csLoading in ComponentState) then
        Save;
    except
      fManufacturer := oldManufacturer;
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
  oldVersion : string;
begin
  if fVersion <> Value then
  begin
    oldVersion := fVersion;
    try
      fVersion := Value;
      if not (csLoading in ComponentState) then
        Save
    except
      fVersion := oldVersion;
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
  appSave, manSave, verSave, f : string;
begin
  if not Persist then Exit;

  f := GetFileName;
  appSave := self.Application;
  manSave := self.Application;
  verSave := self.Application;
  try
    self.fApplication := application;
    self.fManufacturer := manufacturer;
    self.fVersion := version;

    if GetFileName <> f then
      DeleteFile (GetFileName)
  finally
    self.fApplication := application;
    self.fManufacturer := manSave;
    self.fVersion := verSave
  end
end;

(*----------------------------------------------------------------------*
 | function TIniFilePersistentOptions.GetFileName : string              |
 |                                                                      |
 | Get the INI file name                                                |
 *----------------------------------------------------------------------*)
function TIniFilePersistentOptions.GetFileName: string;
var
  st : string;
  p : Integer;
begin
  if fFileName <> '' then       // Use the FileName property if it's been set
  begin
    if (Copy (fileName, 1, 1) = '\') or (Pos (':', fileName) <> 0) then
      result := fFileName
    else
      result := ExtractFilePath (ParamStr (0)) + fFileName
  end
  else                          // Otherwise derive the file name from the
  begin                         // application name
    if Application = '' then
    begin
      st := ExtractFileName (ParamStr (0));
      p := Pos ('.', st);
      if p > 0 then
        st := Copy (st, 1, p - 1)
    end
    else
      st := Application;

    result := ExtractFilePath (ParamStr (0)) + st + '.ini'
  end
end;

(*----------------------------------------------------------------------*
 | procedure TIniFilePersistentOptions.Load                             |
 |                                                                      |
 | Load all the options from the INI file                               |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.Load;
var
  f : TIniFile;
  openedOK : boolean;
  fileName : string;
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
 |   const section : string;    The section to load                     |
 |   Options: TOptions;         Options to populate                     |
 |   forceDefaults: boolean     Use default values, rather than load    |
 |                              from the INI file.  This will be true   |
 |                              if the INI file could not be found.     |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.LoadOptions(iniFile: TIniFile; const section : string;
  Options: TOptions; forceDefaults: boolean);
var
  i : Integer;
  option : TOption;
  sec : string;
  sl : THashedStringList;
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
      option := Options [i];

      if forceDefaults or (sl.IndexOfName(option.Name) = -1) then
      case option.OptionType of
        otString  : option.fStrVal  := option.fDefaultValue;
        otInteger : option.fIntVal  := StrToIntDef (option.fDefaultValue, 0);
        otBoolean : option.fBoolVal := StrToBoolDef (option.fDefaultValue, False);
        otEnum    : option.fIntVal  := option.fEnumValues.IndexOf(option.fDefaultValue)
      end
      else
      case option.OptionType of
        otString  : option.fStrVal  := sl.Values [option.Name];
        otInteger : option.fIntVal  := StrToIntDef (sl.Values [option.Name], StrToIntDef (option.fDefaultValue, 0));
        otBoolean : option.fBoolVal := StrToBoolDef (sl.Values [option.Name], StrToBoolDef (option.fDefaultValue, False));
        otEnum    : option.fIntVal  := StrToIntDef (sl.Values [option.Name], option.fEnumValues.IndexOf(option.fDefaultValue))
      end;
      option.fDirty := False
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
 |   const section : string             Root section name               |
 |   Sections: TSections                The sections to load            |
 |   forceDefaults: boolean             True if default values should   |
 |                                      always be used for the options. |
 |                                      (ie if the INI file could not   |
 |                                       be found.)                     |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.LoadSections(iniFile: TIniFile; const section : string;
  Sections: TSections; forceDefaults: boolean);
var
  i : Integer;
  sect : TSection;
  ss : string;
begin
  for i := 0 to Sections.Count - 1 do
  begin
    sect := Sections [i];
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
  f : TIniFile;
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
 |   const section : string             The section to save             |
 |   Options: TOptions                  Options to save in the section  |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.SaveOptions(iniFile: TIniFile; const section : string;
  Options: TOptions);
var
  i : Integer;
  option : TOption;
  sec : string;

begin
  sec := section;
  if sec = '' then
     sec := 'Defaults';

  if Options.DeleteObsoleteOptions then
    iniFile.EraseSection(sec);

  for i := 0 to Options.Count - 1 do
  begin
    option := Options [i];

    if not option.HasDefaultValue then
    case option.OptionType of
      otString  : iniFile.WriteString(sec, option.Name, option.AsString);
      otInteger : iniFile.WriteInteger(sec, option.Name, option.AsInteger);
      otBoolean : iniFile.WriteBool(sec, option.Name, option.AsBoolean);
      otEnum    : iniFile.WriteString(sec, option.Name, option.GetAsEnum);
    end;
    option.fDirty := False;
  end
end;

(*----------------------------------------------------------------------*
 | TIniFilePersistentOptions.SaveSections                               |
 |                                                                      |
 | Save sections (and their options & subsections) to an INI file       |
 |                                                                      |
 | Parameters:                                                          |
 |   iniFile: TIniFile                  The INI file to save to.        |
 |   const section : string             The root section to save        |
 |   Sections: TSections                Sections to save                |
 *----------------------------------------------------------------------*)
procedure TIniFilePersistentOptions.SaveSections(iniFile: TIniFile; const section : string;
  Sections: TSections);
var
  i : Integer;
  sect : TSection;
  ss : string;
begin
  for i := 0 to Sections.Count - 1 do
  begin
    sect := Sections [i];
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
  if value <> fFileName then
  begin
    fFileName := Value;

    // Delete the old INI file if we're not in design mode.
    DeleteOldOptions (Application, Manufacturer, Version);
  end
end;

{ TRegistryPersistentOptions }

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.DeleteOldOptions                |
 |                                                                      |
 | Delete the old registry settings after changing the root key (by     |
 | changine the application, manufacturer or version properties)        |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.DeleteOldOptions(const application,
  manufacturer, version: string);
var
  keyName : string;
  reg : TRegistry;
begin
  if not Persist then Exit;

  if Application = '' then
    raise EOptionError.Create (rstNoAppName);

  if Manufacturer <> '' then
    keyName := '\Software\' + Manufacturer + '\' + Application
  else
    keyName := '\Software\' + Application;

  if Version <> '' then
    keyName := keyName + '\' + Version;

  if keyName <> GetRootKeyName then
  begin
    reg := TRegistry.Create (KEY_READ or KEY_WRITE);
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
    raise EOptionError.Create (rstNoAppName);

  if Manufacturer <> '' then
    result := '\Software\' + Manufacturer + '\' + Application
  else
    result := '\Software\' + Application;

  if Version <> '' then
    result := result + '\' + Version
end;

(*----------------------------------------------------------------------*
 | procedure TRegistryPersistentOptions.Load                            |
 |                                                                      |
 | Load all the sections & options from the registry                    |
 *----------------------------------------------------------------------*)

procedure TRegistryPersistentOptions.Load;
var
  reg : TRegistry;
  openedOK : boolean;
begin
  if not Persist then Exit;

  reg := TRegistry.Create (KEY_READ);
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
 |   forceDefaults : boolean            True if defaults should be used |
 |                                      This is set if the root         |
 |                                      registry key didn't exist.      |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.LoadOptions(reg: TRegistry; Options: TOptions; forceDefaults : boolean);
var
  i : Integer;
  option : TOption;
begin
  for i := 0 to Options.Count - 1 do
  begin
    option := Options [i];

    if forceDefaults or not reg.ValueExists(option.Name) then
    case option.OptionType of
      otString  : option.fStrVal  := option.fDefaultValue;
      otInteger : option.fIntVal  := StrToIntDef (option.fDefaultValue, 0);
      otBoolean : option.fBoolVal := StrToBoolDef (option.fDefaultValue, False);
      otEnum    : option.fIntVal  := option.fEnumValues.IndexOf(option.fDefaultValue)
    end
    else
    case option.OptionType of
      otString  : option.fStrVal  := reg.ReadString  (option.Name);
      otInteger : option.fIntVal  := reg.ReadInteger (option.Name);
      otBoolean : option.fBoolVal := reg.ReadBool    (option.Name);
      otEnum    : option.fIntVal  := reg.ReadInteger (option.Name)
    end;
    option.fDirty := False
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
 |   forceDefaults : boolean    'True' is defaults should be used       |
 *----------------------------------------------------------------------*)
procedure TRegistryPersistentOptions.LoadSections(reg: TRegistry; Sections: TSections; forceDefaults : boolean);
var
  i : Integer;
  section : TSection;
  reg1 : TRegistry;
begin
  if not forceDefaults then
    reg1 := TRegistry.Create (KEY_READ)
  else
    reg1 := Nil;

  try
    for i := 0 to Sections.Count - 1 do
    begin
      section := Sections [i];

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
  reg : TRegistry;
begin
  if not Persist then Exit;

  reg := TRegistry.Create (KEY_READ or KEY_WRITE);
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
  i, idx : Integer;
  deleteValues : TStringList;
  option : TOption;

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
      Option := Options [i];

      if not Option.HasDefaultValue then        // Save the value if it's not the
      begin                                     // default value for the option

                                                // Remove the value from the
                                                // deleteValues list
        idx := deleteValues.IndexOf(Option.Name);
        if idx >= 0 then
          deleteValues.Delete(idx);

        case Option.OptionType of
          otString  : reg.WriteString  (Option.Name, Option.fStrVal);
          otInteger : reg.WriteInteger (Option.Name, Option.fIntVal);
          otBoolean : reg.WriteBool    (Option.Name, Option.fBoolVal);
          otEnum    : reg.WriteInteger (Option.Name, Option.fIntVal)
        end
      end
      else
        reg.DeleteValue(Option.Name);           // Delete 'default' values
      Option.fDirty := False
    end;

    if Options.DeleteObsoleteOptions then          // Delete obsolete values.
      for i := 0 to deleteValues.count - 1 do
        reg.DeleteValue(deleteValues [i])
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
  i, idx : Integer;
  section : TSection;
  reg1 : TRegistry;
  deleteSections : TStringList;
begin
  reg1 := Nil;
  deleteSections := TStringList.Create;
  try
  // Build list of obsolete sections to delete.

    deleteSections.CaseSensitive := False;
    reg.GetKeyNames(deleteSections);
    deleteSections.Sort;

  // Save the sections & options
    reg1 := TRegistry.Create (KEY_READ or KEY_WRITE);
    for i := 0 to Sections.Count - 1 do
    begin
      section := Sections [i];

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
        reg1.DeleteKey(deleteSections [i])
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
  inherited Create (AOwner, ItemClass);
  fDeleteObsoleteOptions := True
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOption                                         |
 |                                                                      |
 | Get method for 'Option' array property                               |
 *----------------------------------------------------------------------*)
function TOptions.GetOption(idx: Integer): TOption;
begin
  result := TOption (Items [idx])
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOptionByName                                   |
 |                                                                      |
 | 'Get' method for OptionByName array property                         |
 *----------------------------------------------------------------------*)
function TOptions.GetOptionByName(const name: string): TOption;
var
  o : TOption;
  i : Integer;
begin
  result := Nil;
  for i := 0 to Count - 1 do
  begin
    o := Option [i];
    if AnsiSameText (Name, o.Name) then
    begin
      result := o;
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
    fEnumValues := TStringList.Create;
    fEnumValues.CaseSensitive := False
  end
end;

(*----------------------------------------------------------------------*
 | destructor TOption.Destroy                                           |
 |                                                                      |
 | Destructor for TOption class                                         |
 *----------------------------------------------------------------------*)
destructor TOption.Destroy;
begin
  fEnumValues.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TOption.Flush                                              |
 |                                                                      |
 | Save the option.                                                     |
 *----------------------------------------------------------------------*)
procedure TOption.Flush;
begin
  if fDirty and not Base.InUpdate then
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
    result := fBoolVal
  else
    raise EOptionTypeMismatch (self)
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
    result := EnumValues [fIntVal]
  else
    raise EOptionTypeMismatch (self)
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
    result := fIntVal
  else
    raise EOptionTypeMismatch (self)
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
    result := fStrVal
  else
    raise EOptionTypeMismatch (self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetBase                                             |
 |                                                                      |
 | Return the owning 'TPersistentOptions' derived object.               |
 *----------------------------------------------------------------------*)
function TOption.GetBase: TPersistentOptions;
var
  own : TPersistent;
begin
  own := TOwnedCollection (Collection).Owner;

  while own is TSection do
    own := TOwnedCollection (TSection (own).Collection).Owner;

  result := own as TPersistentOptions
end;

(*----------------------------------------------------------------------*
 | function TOption.GetDisplayName                                      |
 |                                                                      |
 | Overridden from TCollectionItem base.  Helps the designer.           |
 *----------------------------------------------------------------------*)
function TOption.GetDisplayName: string;
begin
  result := Name
end;

(*----------------------------------------------------------------------*
 | function TOption.GetEnumValues : TStringList                         |
 |                                                                      |
 | 'Get' method for EnumValues property                                 |
 *----------------------------------------------------------------------*)
function TOption.GetEnumValues: TStrings;
begin
  result := fEnumValues
end;

(*----------------------------------------------------------------------*
 | function TOption.GetHasDefaultValue : boolean                        |
 |                                                                      |
 | Return True if the option's current value is it's default.           |
 *----------------------------------------------------------------------*)
function TOption.GetHasDefaultValue: boolean;
begin
  result := False;
  case OptionType of
    otString  : result := AnsiCompareStr (DefaultValue, fStrVal) = 0;
    otInteger : result := StrToIntDef (DefaultValue, 0) = fIntVal;
    otBoolean : result := StrToBoolDef (DefaultValue, False) = fBoolVal;
    otEnum    : result := fIntVal = EnumValues.IndexOf(DefaultValue)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsBoolean                                       |
 |                                                                      |
 | Set the option's value if it's a boolean option - otherwise raise    |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsBoolean(const Value: Boolean);
begin
  if OptionType <> otBoolean then
    raise EOptionTypeMismatch (self);

  if Value <> fBoolVal then
  begin
    fDirty := True;
    fBoolVal := Value;
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
    fDirty := True;
    fIntVal := EnumValues.IndexOf(Value);
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
    raise EOptionTypeMismatch (self);

  if Value <> fIntVal then
  begin
    fDirty := True;
    fIntVal := Value;
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
    raise EOptionTypeMismatch (self);

  if Value <> fStrVal then
  begin
    fDirty := True;
    fStrVal := Value;
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
    fEnumValues.Assign(Value)
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetOptionType                                      |
 |                                                                      |
 | 'Set' method for the OptionType property.                            |
 *----------------------------------------------------------------------*)
procedure TOption.SetOptionType(const Value: TOptionType);
begin
  if fOptionType <> Value then
  begin
    if Base.Designing then
    begin
      if not Base.Loading then fEnumValues.Clear
    end
    else
      if not Base.Loading or (Value <> otEnum) then
        FreeAndNil (fEnumValues);

    fOptionType := Value;

    if fOptionType = otEnum then
      if not Base.Designing and not Base.Loading then
        fEnumValues := TStringList.Create
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

  fOptions := TOptions.Create(self, TOption);
  fSections := TSections.Create(self, TSection);
end;

(*----------------------------------------------------------------------*
 | destructor TSection.Destroy                                          |
 |                                                                      |
 | Destructor for TSection                                              |
 *----------------------------------------------------------------------*)
destructor TSection.Destroy;
begin
  fOptions.Free;
  fSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TSection.GetDisplayName                                     |
 |                                                                      |
 | Override TCollectionItem method to help the designer                 |
 *----------------------------------------------------------------------*)
function TSection.GetDisplayName: string;
begin
  result := Name
end;

(*----------------------------------------------------------------------*
 | function TSection.GetOption                                          |
 |                                                                      |
 | 'Get' method for Option property                                     |
 *----------------------------------------------------------------------*)
function TSection.GetOption(const name: string): TOption;
begin
  result := Options.OptionByName [name];
end;

(*----------------------------------------------------------------------*
 | function TSection.GetSection                                         |
 |                                                                      |
 | 'Get' method for Section propery                                     |
 *----------------------------------------------------------------------*)
function TSection.GetSection(const name: string): TSection;
begin
  result := Sections.SectionByName [name];
end;

{ EOptionTypeMismatch }

constructor EOptionTypeMismatch.Create(Option: TOption);
begin
  inherited CreateFmt (rstTypeMismatch, [Option.Name, OptionTypeNames [Option.OptionType]])
end;

{ TSections }

(*----------------------------------------------------------------------*
 | function TSections.GetSection                                        |
 |                                                                      |
 | Get method for Section property.                                     |
 *----------------------------------------------------------------------*)
function TSections.GetSection(idx: Integer): TSection;
begin
  result := TSection (Items [idx])
end;

(*----------------------------------------------------------------------*
 | function TSections.GetSectionByName                                  |
 |                                                                      |
 | 'Get' method for SectionByName property                              |
 *----------------------------------------------------------------------*)
function TSections.GetSectionByName(const name: string): TSection;
var
  i, p : Integer;
  s : TSection;
begin
  result := Nil;

  p := Pos ('\', name);
  if p > 0 then
  begin
    s := SectionByName [Trim (Copy (name, 1, p - 1))];
    if Assigned (s) then
      result := s.Sections.SectionByName [Trim (Copy (name, p + 1, MaxInt))]
    else
      raise EOptionError.CreateFmt(rstSectionNotFound, [s.Name]);
  end
  else
  for i := 0 to Count - 1 do
  begin
    s := Section [i];
    if AnsiSameText (name, s.Name) then
    begin
      result := s;
      break
    end
  end;

  if not Assigned (result) then
    raise EOptionError.CreateFmt (rstSectionNotFound, [name])
end;

end.
