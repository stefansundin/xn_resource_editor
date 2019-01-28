unit unitCREdProperties;

interface

uses Windows, Messages, Classes, SysUtils, Forms;

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
    case tp : TPropertyType of
      ptInteger : (intVal : Integer);
      ptBool : (boolVal : boolean);
      ptString : (stringVal : string [64]);
      ptLString : (strIdx : Integer);
  end;

  TPropertyRec = record
    name : string;
    value : TPropertyValue;
    defltStr : string;
  end;

const
  Properties : array [TPropertyEnum] of TPropertyRec = (
    (name : 'Show Toolbar';       value : (tp : ptBool;    boolVal : True)),
    (name : 'Show Statusbar';     value : (tp : ptBool;    boolVal : True)),
    (name : 'RC Include Path';    value : (tp : ptInteger; intVal  : 0)),
    (name : 'RC Custom Include';  value : (tp : ptLString; strIdx : lstrCustomIncludeIdx)),
    (name : 'RC Include Path Package';  value : (tp : ptLString; strIdx : lstrIncludePathPackageIdx)),
    (name : 'Parser Type';        value : (tp : ptInteger; intVal : 0)),
    (name : 'International Font Name';   value : (tp : ptLString; strIdx : lstrFontNameIdx); defltStr : 'Arial Unicode MS'),
    (name : 'International Font Height'; value : (tp : ptInteger; intVal : 16))
  );

type
TPEResourceExplorerProperties = class
private
  fNotifyWindow : TForm;
  fUpdating : boolean;
  fNeedsSave : boolean;
  fValues : array [TPropertyEnum] of TPropertyValue;
  fLStrings : array [0..noLStrings - 1] of string;
  procedure Load;
  procedure Notify;

  function GetBoolPropertyValue (idx : TPropertyEnum) : boolean;
  procedure SetBoolPropertyValue(idx : TPropertyEnum; const Value : boolean);
  function GetIntPropertyValue (idx : TPropertyEnum) : Integer;
  procedure SetIntPropertyValue(idx : TPropertyEnum; const Value : Integer);
  function GetLStringPropertyValue(const idx: TPropertyEnum): string;
  procedure SetLStringPropertyValue(const idx: TPropertyEnum; const Value: string);

  function GetIncludePath : string;
  procedure SetIncludePath (const Value: string);

  property CustomIncludePath : string index peRCCustomInclude read GetLStringPropertyValue write SetLStringPropertyValue;
public
  constructor Create (ANotifyWindow : TForm);
  procedure Save;

  property ShowToolbar : boolean index peShowToolbar           read GetBoolPropertyValue write SetBoolPropertyValue;
  property ShowStatusBar : boolean index peShowStatusbar       read GetBoolPropertyValue write SetBoolPropertyValue;
  property IncludePathType : Integer index peRCInclude         read GetIntPropertyValue write SetIntPropertyValue;
  property IncludePath : string read GetIncludePath write SetIncludePath;
  property IncludePathPackageName : string index peRCIncludePathPackage read GetLStringPropertyValue write SetLStringPropertyValue;
  property ParserType : integer index peParserType read GetIntPropertyValue write SetIntPropertyValue;
  property InternationalFontName : string index peFontName read GetLStringPropertyValue write SetLStringPropertyValue;
  property InternationalFontHeight : Integer index peFontHeight read GetIntPropertyValue write SetIntPropertyValue;

  procedure BeginUpdate;
  procedure EndUpdate;
end;

var
  gProperties : TPEResourceExplorerProperties;

implementation

uses Registry, unitIncludePaths;

{ TPEResourceExplorerProperties }

procedure TPEResourceExplorerProperties.BeginUpdate;
begin
  fUpdating := True;
end;

constructor TPEResourceExplorerProperties.Create (ANotifyWindow : TForm);
var
  i : TPropertyEnum;
begin
  fNotifyWindow := ANotifyWindow;

  for i := Low (TPropertyEnum) to High (TPropertyEnum) do
  begin
    fValues [i] := Properties [i].value;
    if fValues [i].tp = ptLString then
      fLStrings [fValues [i].strIdx] := Properties [i].defltStr
  end;

  Load
end;

procedure TPEResourceExplorerProperties.EndUpdate;
begin
  if fUpdating then
  begin
    fUpdating := False;
    if fNeedsSave then
      Save;
    Notify
  end
end;

function TPEResourceExplorerProperties.GetBoolPropertyValue(
  idx: TPropertyEnum): boolean;
begin
  result := fValues [idx].boolVal
end;

function TPEResourceExplorerProperties.GetIncludePath: string;
begin
  case IncludePathType of
    includePathPackage : result := GetIncludePathForPackage (IncludePathPackageName);
    includePathEnv : result := GetEnvironmentVariable ('Include');
    includePathCustom : result := CustomIncludePath;
    else
      result := ''
  end
end;

function TPEResourceExplorerProperties.GetIntPropertyValue(
  idx: TPropertyEnum): Integer;
begin
  result := fValues [idx].intVal;
end;

function TPEResourceExplorerProperties.GetLStringPropertyValue(
  const Idx : TPropertyEnum): string;
var
  strIdx : Integer;
begin
  strIdx := fValues [idx].strIdx;
  result := fLStrings [strIdx];
end;

procedure TPEResourceExplorerProperties.Load;
var
  reg : TRegistry;
  i : TPropertyEnum;
begin
  reg := TRegistry.Create (KEY_READ);
  try
    if reg.OpenKey ('Software\Woozle\' + Application.Title + '\Properties', False) then
    begin
      BeginUpdate;
      try
        for i := Low (TPropertyEnum) to High (TPropertyEnum) do
          if reg.ValueExists (Properties [i].Name) then
            case Properties [i].value.tp of
              ptBool : fValues [i].boolVal := reg.ReadBool (Properties [i].Name);
              ptInteger : fValues [i].intVal := reg.ReadInteger (Properties [i].Name);
              ptString : fValues [i].stringVal := reg.ReadString (Properties [i].Name);
              ptLString : fLStrings [fValues [i].strIdx] := reg.ReadString (Properties [i].Name);
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
  if not fUpdating then
    PostMessage (fNotifyWindow.Handle, WM_PROPERTIES_CHANGED, 0, 0)
end;

procedure TPEResourceExplorerProperties.Save;
var
  reg : TRegistry;
  i : TPropertyEnum;
begin
  if fUpdating then
  begin
    fNeedsSave := True;
    Exit
  end;

  reg := TRegistry.Create (KEY_READ or KEY_WRITE);
  try
    if reg.OpenKey ('Software\Woozle\' + Application.Title + '\Properties', True) then
    begin
      for i := Low (TPropertyEnum) to High (TPropertyEnum) do
         case Properties [i].value.tp of
           ptBool : reg.WriteBool (Properties [i].Name, fValues [i].boolVal);
           ptInteger : reg.WriteInteger (Properties [i].Name, fValues [i].intVal);
           ptString : reg.WriteString (Properties [i].Name, fValues [i].stringVal);
           ptLString : reg.WriteString (Properties [i].Name, fLstrings [fValues [i].strIdx])
         end
    end
  finally
    reg.Free
  end;
  fNeedsSave := False
end;

procedure TPEResourceExplorerProperties.SetBoolPropertyValue(
  idx: TPropertyEnum; const Value: boolean);
begin
  if value <> fValues [idx].boolVal then
  begin
    fValues [idx].boolVal := Value;
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
  if value <> fValues [idx].intVal then
  begin
    fValues [idx].intVal := Value;
    Save;
    Notify
  end
end;

procedure TPEResourceExplorerProperties.SetLStringPropertyValue (const Idx: TPropertyEnum;
  const Value: string);
var
  strIdx : Integer;
begin
  strIdx := fValues [idx].stridx;
  fLStrings [strIdx] := Value;
  Save;
  Notify
end;

end.
