unit unitCREdProperties;

interface

uses
  Windows, Messages, Classes, SysUtils, Forms;

const
  WM_PROPERTIES_CHANGED = WM_USER + $30E;

const
  includePathPackage = 0;
  includePathEnv = 1;
  includePathCustom = 2;

  lstrCustomIncludeIdx = 0;
  lstrIncludePathPackageIdx = 1;
  lstrFontNameIdx =2;
  noLStrings = 3;

type
  TPropertyEnum = (
    peShowToolbar,
    peShowStatusbar,
    peRCInclude,
    peRCCustomInclude,
    peRCIncludePathPackage,
    peParserType,
    peFontName,
    peFontHeight);

  TPropertyType = (ptInteger, ptBool, ptString, ptLString);

  TPropertyValue = record
    case tp: TPropertyType of
      ptInteger: (intVal: Integer);
      ptBool: (boolVal: Boolean);
      ptString: (stringVal: string [64]);
      ptLString: (strIdx: Integer);
  end;

  TPropertyRec = record
    name: string;
    value: TPropertyValue;
    defltStr: string;
  end;

const
  Properties: array [TPropertyEnum] of TPropertyRec = (
    (name: 'Show Toolbar';       value: (tp: ptBool;    boolVal: True)),
    (name: 'Show Statusbar';     value: (tp: ptBool;    boolVal: True)),
    (name: 'RC Include Path';    value: (tp: ptInteger; intVal : 0)),
    (name: 'RC Custom Include';  value: (tp: ptLString; strIdx: lstrCustomIncludeIdx)),
    (name: 'RC Include Path Package';  value: (tp: ptLString; strIdx: lstrIncludePathPackageIdx)),
    (name: 'Parser Type';        value: (tp: ptInteger; intVal: 0)),
    (name: 'International Font Name';   value: (tp: ptLString; strIdx: lstrFontNameIdx); defltStr: 'Arial Unicode MS'),
    (name: 'International Font Height'; value: (tp: ptInteger; intVal: 16))
  );

type
  TPEResourceExplorerProperties = class
  private
    FNotifyWindow: TForm;
    FUpdating: Boolean;
    FNeedsSave: Boolean;
    FValues: array [TPropertyEnum] of TPropertyValue;
    FLStrings: array [0..noLStrings - 1] of string;
    procedure Load;
    procedure Notify;

    function GetBoolPropertyValue(idx: TPropertyEnum): Boolean;
    procedure SetBoolPropertyValue(idx: TPropertyEnum; const Value: Boolean);
    function GetIntPropertyValue(idx: TPropertyEnum): Integer;
    procedure SetIntPropertyValue(idx: TPropertyEnum; const Value: Integer);
    function GetLStringPropertyValue(const idx: TPropertyEnum): string;
    procedure SetLStringPropertyValue(const idx: TPropertyEnum; const Value: string);

    function GetIncludePath: string;
    procedure SetIncludePath(const Value: string);

    property CustomIncludePath: string index peRCCustomInclude read GetLStringPropertyValue write SetLStringPropertyValue;
  public
    constructor Create(ANotifyWindow: TForm);
    procedure Save;

    property ShowToolbar: Boolean index peShowToolbar           read GetBoolPropertyValue write SetBoolPropertyValue;
    property ShowStatusBar: Boolean index peShowStatusbar       read GetBoolPropertyValue write SetBoolPropertyValue;
    property IncludePathType: Integer index peRCInclude         read GetIntPropertyValue write SetIntPropertyValue;
    property IncludePath: string read GetIncludePath write SetIncludePath;
    property IncludePathPackageName: string index peRCIncludePathPackage read GetLStringPropertyValue write SetLStringPropertyValue;
    property ParserType: integer index peParserType read GetIntPropertyValue write SetIntPropertyValue;
    property InternationalFontName: string index peFontName read GetLStringPropertyValue write SetLStringPropertyValue;
    property InternationalFontHeight: Integer index peFontHeight read GetIntPropertyValue write SetIntPropertyValue;

    procedure BeginUpdate;
    procedure EndUpdate;
  end;

var
  gProperties: TPEResourceExplorerProperties;

implementation

uses
  Registry, unitIncludePaths;

{ TPEResourceExplorerProperties }

procedure TPEResourceExplorerProperties.BeginUpdate;
begin
  FUpdating := True;
end;

constructor TPEResourceExplorerProperties.Create(ANotifyWindow: TForm);
var
  i: TPropertyEnum;
begin
  FNotifyWindow := ANotifyWindow;

  for i := Low (TPropertyEnum) to High(TPropertyEnum) do
  begin
    FValues[i] := Properties[i].value;
    if FValues[i].tp = ptLString then
      FLStrings[FValues[i].strIdx] := Properties[i].defltStr
  end;

  Load
end;

procedure TPEResourceExplorerProperties.EndUpdate;
begin
  if FUpdating then
  begin
    FUpdating := False;
    if FNeedsSave then
      Save;
    Notify
  end
end;

function TPEResourceExplorerProperties.GetBoolPropertyValue(
  idx: TPropertyEnum): Boolean;
begin
  Result := FValues[idx].boolVal
end;

function TPEResourceExplorerProperties.GetIncludePath: string;
begin
  case IncludePathType of
    includePathPackage: Result := GetIncludePathForPackage(IncludePathPackageName);
    includePathEnv: Result := GetEnvironmentVariable('Include');
    includePathCustom: Result := CustomIncludePath;
    else
      Result := ''
  end
end;

function TPEResourceExplorerProperties.GetIntPropertyValue(
  idx: TPropertyEnum): Integer;
begin
  Result := FValues[idx].intVal;
end;

function TPEResourceExplorerProperties.GetLStringPropertyValue(
  const Idx: TPropertyEnum): string;
var
  strIdx: Integer;
begin
  strIdx := FValues[idx].strIdx;
  Result := FLStrings[strIdx];
end;

procedure TPEResourceExplorerProperties.Load;
var
  reg: TRegistry;
  i: TPropertyEnum;
begin
  reg := TRegistry.Create(KEY_READ);
  try
    if reg.OpenKey('Software\Woozle\' + Application.Title + '\Properties', False) then
    begin
      BeginUpdate;
      try
        for i := Low (TPropertyEnum) to High(TPropertyEnum) do
          if reg.ValueExists (Properties[i].Name) then
            case Properties[i].value.tp of
              ptBool: FValues[i].boolVal := reg.ReadBool (Properties[i].Name);
              ptInteger: FValues[i].intVal := reg.ReadInteger (Properties[i].Name);
              ptString: FValues[i].stringVal := reg.ReadString (Properties[i].Name);
              ptLString: FLStrings[FValues[i].strIdx] := reg.ReadString (Properties[i].Name);
            end
      finally
        EndUpdate
      end
    end
  finally
    reg.Free
  end
end;

procedure TPEResourceExplorerProperties.Notify;
begin
  if not FUpdating then
    PostMessage(FNotifyWindow.Handle, WM_PROPERTIES_CHANGED, 0, 0)
end;

procedure TPEResourceExplorerProperties.Save;
var
  reg: TRegistry;
  i: TPropertyEnum;
begin
  if FUpdating then
  begin
    FNeedsSave := True;
    Exit
  end;

  reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    if reg.OpenKey('Software\Woozle\' + Application.Title + '\Properties', True) then
    begin
      for i := Low (TPropertyEnum) to High(TPropertyEnum) do
         case Properties[i].value.tp of
           ptBool: reg.WriteBool (Properties[i].Name, FValues[i].boolVal);
           ptInteger: reg.WriteInteger (Properties[i].Name, FValues[i].intVal);
           ptString: reg.WriteString (Properties[i].Name, FValues[i].stringVal);
           ptLString: reg.WriteString (Properties[i].Name, FLStrings[FValues[i].strIdx])
         end
    end
  finally
    reg.Free
  end;
  FNeedsSave := False
end;

procedure TPEResourceExplorerProperties.SetBoolPropertyValue(
  idx: TPropertyEnum; const Value: Boolean);
begin
  if value <> FValues[idx].boolVal then
  begin
    FValues[idx].boolVal := Value;
    Save;
    Notify
  end
end;

procedure TPEResourceExplorerProperties.SetIncludePath(const Value: string);
begin
  if (IncludePathType <> includePathCustom) or (value <> CustomIncludePath) then
  begin
    IncludePathType := includePathCustom;
    CustomIncludePath := Value
  end
end;

procedure TPEResourceExplorerProperties.SetIntPropertyValue(idx: TPropertyEnum;
  const Value: Integer);
begin
  if value <> FValues[idx].intVal then
  begin
    FValues[idx].intVal := Value;
    Save;
    Notify
  end
end;

procedure TPEResourceExplorerProperties.SetLStringPropertyValue(const Idx: TPropertyEnum;
  const Value: string);
var
  strIdx: Integer;
begin
  strIdx := FValues[idx].stridx;
  FLStrings[strIdx] := Value;
  Save;
  Notify
end;

end.
