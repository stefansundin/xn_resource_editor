unit cmpMRUList;

interface

uses
  Windows, Classes, SysUtils, Menus, Forms;

type
  TMRUList = class(TComponent)
  private
    FMRU: TStringList;
    FManufacturer: string;
    FCapacity: Integer;
    FLoaded: Boolean;
    FPopupMenu: TPopupMenu;
    FOnPopupMenuClick: TNotifyEvent;
    FAppSection: string;
    FApplication: string;

    procedure SetManufacturer(const Value: string);
    procedure SetCapacity(const Value: Integer);
    function GetStrings: TStrings;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure PopulateMenu;
    procedure PopupMenuItemOnClick(sender: TObject);
    procedure SetAppSection(const Value: string);
    function GetMRUDirectory: string;
    function GetMRUFile: string;
    procedure SetApplication(const Value: string);
  protected
    function GetKeyName: string; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddFile(fileName: string);
    procedure SaveList;
    procedure LoadList;
    property Strings: TStrings read GetStrings;
    property MRUFile: string read GetMRUFile;
    property MRUDirectory: string read GetMRUDirectory;


  published
    property Manufacturer: string read FManufacturer write SetManufacturer;
    property Application: string read FApplication write SetApplication;
    property AppSection: string read FAppSection write SetAppSection;
    property Capacity: Integer read FCapacity write SetCapacity default 5;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;

    property OnPopupMenuClick: TNotifyEvent read FOnPopupMenuClick write FOnPopupMenuClick;
  end;

implementation

uses Registry;

{ TMRUList }

procedure TMRUList.AddFile(fileName: string);
var
  idx: Integer;
begin
  fileName := LowerCase(fileName);
  LoadList;
  idx := FMRU.IndexOf (fileName);
  if idx >= 0 then
    FMRU.Delete(idx);

  while FMRU.Count >= Capacity do
    FMRU.Delete(FMRU.Count - 1);

  FMRU.Insert (0, fileName);
  PopulateMenu
end;

constructor TMRUList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMRU := TStringList.Create;
  FCapacity := 5
end;

destructor TMRUList.Destroy;
begin
  if not (csDesigning in ComponentState) then
    SaveList;
  FMRU.Free;
  inherited;
end;

function TMRUList.GetKeyName: string;
var
  app: string;
begin
  if Application = '' then
    app := Forms.Application.Title
  else
    app := Application;
  Result := Format ('SOFTWARE\%s\%s\Recent Files', [Manufacturer, Application]);
  if FAppSection <> '' then
    Result := Result + '\' + FAppSection
end;

function TMRUList.GetMRUDirectory: string;
begin
  Result := ExtractFilePath (MRUFile)
end;

function TMRUList.GetMRUFile: string;
begin
  if strings.Count > 0 then
    Result := strings[0]
  else
    Result := ''
end;

function TMRUList.GetStrings: TStrings;
begin
  LoadList;
  Result := FMRU
end;

procedure TMRUList.LoadList;
var
  values: TStringList;
  i: Integer;
begin
  if not FLoaded then
  begin
    FMRU.Clear;
    if Manufacturer <> '' then
      with TRegistry.Create do
      try
        if OpenKeyReadOnly(GetKeyName) then
        begin
          values := TStringList.Create;
          try
            GetValueNames (values);
            for i := 0 to values.Count - 1 do
              FMRU.Add (ReadString (values[i]));
          finally
            values.Free
          end
        end
      finally
        Free
      end;
    FLoaded := True
  end
end;

procedure TMRUList.PopulateMenu;
var
  i: Integer;
  item: TMenuItem;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned (FPopupMenu) then
    begin
      FPopupMenu.Items.Clear;
      for i := 0 to Strings.Count - 1 do
      begin
        item := TMenuItem.Create(Self);
        item.Caption := '&' + IntToHex (i, 0) + ' ' + Strings[i];
        item.OnClick := PopupMenuItemOnClick;
        PopupMenu.Items.Add (item)
      end
    end
  end
end;

procedure TMRUList.PopupMenuItemOnClick(sender: TObject);
begin
  if Assigned (OnPopupMenuClick) then
    OnPopupMenuClick(sender);
end;

procedure TMRUList.SaveList;
var
  i: Integer;
begin
  if (Manufacturer <> '') and FLoaded then
    if FMRU.Count > 0 then
      with TRegistry.Create do
      try
        if OpenKey(GetKeyName, True) then
          for i := 0 to FMRU.Count - 1 do
            WriteString (Format ('File %d', [i]), FMRU [i])
      finally
        Free
      end
    else
      RegDeleteKey(HKEY_CURRENT_USER, PChar (GetKeyName));
end;

procedure TMRUList.SetApplication(const Value: string);
begin
  if FApplication <> value then
  begin
    SaveList;
    FApplication := Value;
    FLoaded := False
  end
end;

procedure TMRUList.SetAppSection(const Value: string);
begin
  if FAppSection <> value then
  begin
    SaveList;
    FAppSection := Value;
    FLoaded := False
  end
end;

procedure TMRUList.SetCapacity(const Value: Integer);
begin
  if Capacity <> value then
  begin
    FCapacity := Value
  end
end;

procedure TMRUList.SetManufacturer(const Value: string);
begin
  if FManufacturer <> value then
  begin
    SaveList;
    FManufacturer := Value;
    FLoaded := False
  end
end;

procedure TMRUList.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> value then
  begin
    FPopupMenu := value;
    PopulateMenu
  end
end;

end.
