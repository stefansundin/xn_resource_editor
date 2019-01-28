unit cmpMRUList;

interface

uses
  Windows, Classes, SysUtils, Menus, Forms;

type
  TMRUList = class(TComponent)
  private
    fMRU : TStringList;
    fManufacturer : string;
    fCapacity: Integer;
    fLoaded : boolean;
    fPopupMenu: TPopupMenu;
    fOnPopupMenuClick: TNotifyEvent;
    fAppSection: string;
    fApplication: string;

    procedure SetManufacturer(const Value: string);
    procedure SetCapacity(const Value: Integer);
    function GetStrings : TStrings;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure PopulateMenu;
    procedure PopupMenuItemOnClick (sender : TObject);
    procedure SetAppSection(const Value: string);
    function GetMRUDirectory: string;
    function GetMRUFile: string;
    procedure SetApplication(const Value: string);
  protected
    function GetKeyName : string; virtual;

  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;

    procedure AddFile (fileName : string);
    procedure SaveList;
    procedure LoadList;
    property Strings : TStrings read GetStrings;
    property MRUFile : string read GetMRUFile;
    property MRUDirectory : string read GetMRUDirectory;


  published
    property Manufacturer : string read fManufacturer write SetManufacturer;
    property Application : string read fApplication write SetApplication;
    property AppSection : string read fAppSection write SetAppSection;
    property Capacity : Integer read fCapacity write SetCapacity default 5;
    property PopupMenu : TPopupMenu read fPopupMenu write SetPopupMenu;

    property OnPopupMenuClick : TNotifyEvent read fOnPopupMenuClick write fOnPopupMenuClick;
  end;

implementation

uses Registry;

{ TMRUList }

procedure TMRUList.AddFile(fileName: string);
var
  idx : Integer;
begin
  fileName := LowerCase (fileName);
  LoadList;
  idx := fMRU.IndexOf (fileName);
  if idx >= 0 then
    fMRU.Delete (idx);

  while fMRU.Count >= Capacity do
    fMRU.Delete (fMRU.Count - 1);

  fMRU.Insert (0, fileName);
  PopulateMenu
end;

constructor TMRUList.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  fMRU := TStringList.Create;
  fCapacity := 5
end;

destructor TMRUList.Destroy;
begin
  if not (csDesigning in ComponentState) then
    SaveList;
  fMRU.Free;
  inherited;
end;

function TMRUList.GetKeyName: string;
var
  app : string;
begin
  if Application = '' then
    app := Forms.Application.Title
  else
    app := Application;
  result := Format ('SOFTWARE\%s\%s\Recent Files', [Manufacturer, Application]);
  if fAppSection <> '' then
    result := result + '\' + fAppSection
end;

function TMRUList.GetMRUDirectory: string;
begin
  result := ExtractFilePath (MRUFile)
end;

function TMRUList.GetMRUFile: string;
begin
  if strings.Count > 0 then
    result := strings [0]
  else
    result := ''
end;

function TMRUList.GetStrings : TStrings;
begin
  LoadList;
  result := fMRU
end;

procedure TMRUList.LoadList;
var
  values : TStringList;
  i : Integer;
begin
  if not fLoaded then
  begin
    fMRU.Clear;
    if Manufacturer <> '' then
      with TRegistry.Create do
      try
        if OpenKeyReadOnly (GetKeyName) then
        begin
          values := TStringList.Create;
          try
            GetValueNames (values);
            for i := 0 to values.Count - 1 do
              fMRU.Add (ReadString (values [i]));
          finally
            values.Free
          end
        end
      finally
        Free
      end;
    fLoaded := True
  end
end;

procedure TMRUList.PopulateMenu;
var
  i : Integer;
  item : TMenuItem;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned (fPopupMenu) then
    begin
      fPopupMenu.Items.Clear;
      for i := 0 to Strings.Count - 1 do
      begin
        item := TMenuItem.Create (Self);
        item.Caption := '&' + IntToHex (i, 0) + ' ' + Strings [i];
        item.OnClick := PopupMenuItemOnClick;
        PopupMenu.Items.Add (item)
      end
    end
  end
end;

procedure TMRUList.PopupMenuItemOnClick(sender: TObject);
begin
  if Assigned (OnPopupMenuClick) then
    OnPopupMenuClick (sender);
end;

procedure TMRUList.SaveList;
var
  i : Integer;
begin
  if (Manufacturer <> '') and fLoaded then
    if fMRU.Count > 0 then
      with TRegistry.Create do
      try
        if OpenKey (GetKeyName, True) then
          for i := 0 to fMRU.Count - 1 do
            WriteString (Format ('File %d', [i]), fMRU [i])
      finally
        Free
      end
    else
      RegDeleteKey (HKEY_CURRENT_USER, PChar (GetKeyName));
end;

procedure TMRUList.SetApplication(const Value: string);
begin
  if fApplication <> value then
  begin
    SaveList;
    fApplication := Value;
    fLoaded := False
  end
end;

procedure TMRUList.SetAppSection(const Value: string);
begin
  if fAppSection <> value then
  begin
    SaveList;
    fAppSection := Value;
    fLoaded := False
  end
end;

procedure TMRUList.SetCapacity(const Value: Integer);
begin
  if Capacity <> value then
  begin
    fCapacity := Value
  end
end;

procedure TMRUList.SetManufacturer(const Value: string);
begin
  if fManufacturer <> value then
  begin
    SaveList;
    fManufacturer := Value;
    fLoaded := False
  end
end;

procedure TMRUList.SetPopupMenu(const Value: TPopupMenu);
begin
  if fPopupMenu <> value then
  begin
    fPopupMenu := value;
    PopulateMenu
  end
end;

end.
