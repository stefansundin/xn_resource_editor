unit PropertyPageRCSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, StdCtrls, VirtualTrees, PropertyPageForm,
  ComponentPersistentPosition, unitIncludePaths;

type
  TPropertyPageRCSettingsData = class(TPropertyPageData)
  private
    FIncludePathType: Integer;
    FCustomIncludePath: string;
    FIncludePathPackageName: string;
  protected
    procedure Initialize; override;
  public
    procedure Apply; override;
  end;

  TFormPropertyPageRCSettings = class(TFormPropertyPage)
    vstIncludePackages: TVirtualStringTree;
    RadioButtonCustomIncludePath: TRadioButton;
    EditCustomIncludePath: TEdit;
    ButtonCustomIncludePath: TButton;
    RadioButtonCompilerIncludePath: TRadioButton;
    RadioButtonEnvironmentVariableIncludePath: TRadioButton;
    procedure RadioButtonCustomIncludePathClick(Sender: TObject);
    procedure vstIncludePackagesChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstIncludePackagesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormDestroy(Sender: TObject);
    procedure vstIncludePackagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    FIncludePathPackages: TIncludePathPackages;
    FData: TPropertyPageRCSettingsData;
    function GetNodePackage(Node: PVirtualNode): TIncludePathPackage;
    function GetNodeForPackage(const packageName: string): PVirtualNode;
    procedure UpdateCustomText;
  protected
    procedure UpdateActions; override;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls (AData: TPropertyPageData); override;
  end;

implementation

uses
  unitCREdProperties;

{$R *.DFM}

type
  PObject = ^TObject;

{ TFormPropertyPageRCSettings }

class function TFormPropertyPageRCSettings.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageRCSettingsData;
end;

procedure TFormPropertyPageRCSettings.PopulateControls(AData: TPropertyPageData);
var
  Node: PVirtualNode;
begin
  Inherited;
  FIncludePathPackages := TIncludePathPackages.Create;
  vstIncludePackages.RootNodeCount := FIncludePathPackages.Count;

  FData := TPropertyPageRCSettingsData(AData);

  case FData.FIncludePathType of
    includePathPackage:
      begin
        RadioButtonCompilerIncludePath.Checked := True;
        Node := GetNodeForPackage(FData.FIncludePathPackageName);
        if Assigned(Node) then
          Node.CheckState := csCheckedNormal
      end;
    includePathEnv:
      RadioButtonEnvironmentVariableIncludePath.Checked:= True;
    includePathCustom:
      begin
        RadioButtonCustomIncludePath.Checked := True;
        EditCustomIncludePath.Text := FData.FCustomIncludePath;
      end;
  end;
  UpdateCustomText;
end;

procedure TFormPropertyPageRCSettings.UpdateActions;
begin
  case FData.FIncludePathType of
    includePathPackage:
      begin
        vstIncludePackages.Enabled := True;
        ButtonCustomIncludePath.Enabled := False;
        EditCustomIncludePath.Enabled := False;
      end;
    includePathCustom:
      begin
        vstIncludePackages.Enabled := False;
        ButtonCustomIncludePath.Enabled := True;
        EditCustomIncludePath.Enabled := True;
      end;
    includePathEnv:
      begin
        vstIncludePackages.Enabled := False;
        ButtonCustomIncludePath.Enabled := False;
        EditCustomIncludePath.Enabled := False;
      end;
  end
end;

function TFormPropertyPageRCSettings.GetNodePackage(
  Node: PVirtualNode): TIncludePathPackage;
var
  Data: PObject;
begin
  Data := PObject(vstIncludePackages.GetNodeData(Node));
  Result := TIncludePathPackage(Data^);
end;

function TFormPropertyPageRCSettings.GetNodeForPackage(
  const packageName: string): PVirtualNode;
begin
  Result := vstIncludePackages.GetFirst;
  while Assigned(Result) do
  begin
    if SameText(GetNodePackage(Result).Name, packageName) then
      Break
    else
      Result := vstIncludePackages.GetNext(Result)
  end
end;

procedure TFormPropertyPageRCSettings.UpdateCustomText;
var
  st: string;
begin
  case FData.FIncludePathType of
    includePathPackage:
      st := GetIncludePathForPackage(FData.FIncludePathPackageName);
    includePathCustom:
      st := FData.FCustomIncludePath;
    includePathEnv:
      st := GetEnvironmentVariable('Include');
  end;

  EditCustomIncludePath.Text := st;
  FData.FCustomIncludePath := st;
end;

{ TPropertyPageRCSettingsData }

procedure TPropertyPageRCSettingsData.Apply;
begin
  gProperties.IncludePathType := FIncludePathType;
  case FIncludePathType of
    includePathPackage:
      gProperties.IncludePathPackageName := FIncludePathPackageName;
    includePathCustom:
      gProperties.IncludePath := FCustomIncludePath;
  end
end;

procedure TPropertyPageRCSettingsData.Initialize;
begin
  FIncludePathType := gProperties.IncludePathType;
  FCustomIncludePath := gProperties.IncludePath;
  FIncludePathPackageName := gProperties.IncludePathPackageName
end;

procedure TFormPropertyPageRCSettings.FormDestroy(Sender: TObject);
begin
  inherited;
  FIncludePathPackages.Free;
end;

procedure TFormPropertyPageRCSettings.vstIncludePackagesInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PObject;
begin
  Data := PObject(vstIncludePackages.GetNodeData(Node));
  Data^ := FIncludePathPackages.Package [Node^.Index];
  Node^.CheckType := ctRadioButton ;
end;

procedure TFormPropertyPageRCSettings.vstIncludePackagesChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  if Node.CheckState = csCheckedNormal then
  begin
    FData.FIncludePathPackageName := GetNodePackage(Node).Name;
    UpdateCustomText
  end;
end;

procedure TFormPropertyPageRCSettings.vstIncludePackagesGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  package: TIncludePathPackage;
begin
  Package := GetNodePackage(Node);
  if Assigned(package) then
    CellText := package.Name
end;

procedure TFormPropertyPageRCSettings.RadioButtonCustomIncludePathClick(Sender: TObject);
var
  CompilerPath: Boolean;
  p: PVirtualNode;
  Package: TIncludePathPackage;
begin
  CompilerPath := RadioButtonCompilerIncludePath.Checked;

  p := vstIncludePackages.GetFirst;
  while Assigned(p) do
  begin
    package := GetNodePackage(p);
    if CompilerPath and SameText(package.Name, FData.FIncludePathPackageName) then
      p.CheckState := csCheckedNormal
    else
      p.CheckState := csUncheckedNormal;

    p := vstIncludePackages.GetNext(p);
  end;

  if RadioButtonCompilerIncludePath.Checked then
    FData.FIncludePathType := includePathPackage
  else
    if RadioButtonEnvironmentVariableIncludePath.Checked then
      FData.FIncludePathType := includePathEnv
    else
      FData.FIncludePathType := includePathCustom;

  UpdateCustomText;
end;

end.
