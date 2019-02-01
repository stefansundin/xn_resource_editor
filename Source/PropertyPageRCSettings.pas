unit PropertyPageRCSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PropertyPageForm, Menus, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls, unitIncludePaths;

type
  TPropertyPageRCSettingsData = class (TPropertyPageData)
  private
    FIncludePathType: Integer;
    FCustomIncludePath: string;
    FIncludePathPackageName: string;
  protected
    procedure Initialize; override;
  public
    procedure Apply; override;
  end;

  TfmPropertyPageRCSettings = class(TfmPropertyPage)
    vstIncludePackages: TVirtualStringTree;
    rbCustomIncludePath: TRadioButton;
    edCustomIncludePath: TEdit;
    btnCustomIncludePath: TButton;
    rbCompilerIncludePath: TRadioButton;
    rbEnvironmentVariableIncludePath: TRadioButton;
    procedure rbCustomIncludePathClick(Sender: TObject);
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

var
  fmPropertyPageRCSettings: TfmPropertyPageRCSettings;

implementation

uses
  unitCREdProperties;

{$R *.DFM}

type
  PObject = ^TObject;

{ TfmPropertyPageRCSettings }

class function TfmPropertyPageRCSettings.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageRCSettingsData;
end;

procedure TfmPropertyPageRCSettings.PopulateControls(AData: TPropertyPageData);
var
  Node: PVirtualNode;
begin
  Inherited;
  FIncludePathPackages := TIncludePathPackages.Create;
  vstIncludePackages.RootNodeCount := FIncludePathPackages.Count;

  FData := TPropertyPageRCSettingsData(AData);

  case FData.FIncludePathType of
    includePathPackage: begin
                           rbCompilerIncludePath.Checked := True;
                           Node := GetNodeForPackage(FData.FIncludePathPackageName);
                           if Assigned (Node) then
                             Node.CheckState := csCheckedNormal
                         end;

    includePathEnv    : rbEnvironmentVariableIncludePath.Checked:= True;

    includePathCustom : begin
                           rbCustomIncludePath.Checked := True;
                           edCustomIncludePath.Text := FData.FCustomIncludePath
                         end;
  end;
  UpdateCustomText
end;

procedure TfmPropertyPageRCSettings.UpdateActions;
begin
  case FData.FIncludePathType of
    includePathPackage: begin
                           vstIncludePackages.Enabled := True;
                           btnCustomIncludePath.Enabled := False;
                           edCustomIncludePath.Enabled := False;
                         end;
    includePathCustom : begin
                           vstIncludePackages.Enabled := False;
                           btnCustomIncludePath.Enabled := True;
                           edCustomIncludePath.Enabled := True;
                         end;
    includePathEnv    : begin
                           vstIncludePackages.Enabled := False;
                           btnCustomIncludePath.Enabled := False;
                           edCustomIncludePath.Enabled := False;
                         end;
  end
end;

function TfmPropertyPageRCSettings.GetNodePackage(
  Node: PVirtualNode): TIncludePathPackage;
var
  Data: PObject;
begin
  Data := PObject(vstIncludePackages.GetNodeData(Node));
  Result := TIncludePathPackage(Data^);
end;

function TfmPropertyPageRCSettings.GetNodeForPackage(
  const packageName: string): PVirtualNode;
begin
  Result := vstIncludePackages.GetFirst;
  while Assigned (Result) do
  begin
    if SameText(GetNodePackage(Result).Name, packageName) then
      break
    else
      Result := vstIncludePackages.GetNext(Result)
  end
end;

procedure TfmPropertyPageRCSettings.UpdateCustomText;
var
  st: string;
begin
  case FData.FIncludePathType of
    includePathPackage: st := GetIncludePathForPackage(FData.FIncludePathPackageName);
    includePathCustom : st := FData.FCustomIncludePath;
    includePathEnv    : st := GetEnvironmentVariable('Include');
  end;

  edCustomIncludePath.Text := st;
  FData.FCustomIncludePath := st;
end;

{ TPropertyPageRCSettingsData }

procedure TPropertyPageRCSettingsData.Apply;
begin
  gProperties.IncludePathType := FIncludePathType;
  case FIncludePathType of
    includePathPackage:  gProperties.IncludePathPackageName := FIncludePathPackageName;
    includePathCustom :  gProperties.IncludePath := FCustomIncludePath;
  end
end;

procedure TPropertyPageRCSettingsData.Initialize;
begin
  FIncludePathType := gProperties.IncludePathType;
  FCustomIncludePath := gProperties.IncludePath;
  FIncludePathPackageName := gProperties.IncludePathPackageName
end;

procedure TfmPropertyPageRCSettings.FormDestroy(Sender: TObject);
begin
  inherited;
  FIncludePathPackages.Free;
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PObject;
begin
  Data := PObject(vstIncludePackages.GetNodeData(Node));
  Data^ := FIncludePathPackages.Package [Node^.Index];
  Node^.CheckType := ctRadioButton ;
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  if Node.CheckState = csCheckedNormal then
  begin
    FData.FIncludePathPackageName := GetNodePackage(Node).Name;
    UpdateCustomText
  end;
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  package: TIncludePathPackage;
begin
  Package := GetNodePackage(Node);
  if Assigned (package) then
    CellText := package.Name
end;

procedure TfmPropertyPageRCSettings.rbCustomIncludePathClick(Sender: TObject);
var
  CompilerPath: Boolean;
  p: PVirtualNode;
  Package: TIncludePathPackage;
begin
  CompilerPath := rbCompilerIncludePath.Checked;

  p := vstIncludePackages.GetFirst;
  while Assigned (p) do
  begin
    package := GetNodePackage(p);
    if CompilerPath and SameText(package.Name, FData.FIncludePathPackageName) then
      p.CheckState := csCheckedNormal
    else
      p.CheckState := csUncheckedNormal;

    p := vstIncludePackages.GetNext(p);
  end;

  if rbCompilerIncludePath.Checked then
    FData.FIncludePathType := includePathPackage
  else
    if rbEnvironmentVariableIncludePath.Checked then
      FData.FIncludePathType := includePathEnv
    else
      FData.FIncludePathType := includePathCustom;

  UpdateCustomText;
end;

end.
