unit PropertyPageRCSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PropertyPageForm, Menus, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls, unitIncludePaths;

type
  TPropertyPageRCSettingsData = class (TPropertyPageData)
  private
    fIncludePathType : Integer;
    fCustomIncludePath : string;
    fIncludePathPackageName : string;
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
    procedure vstIncludePackagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vstIncludePackagesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormDestroy(Sender: TObject);
  private
    fIncludePathPackages : TIncludePathPackages;
    fData : TPropertyPageRCSettingsData;
    function GetNodePackage (node : PVirtualNode) : TIncludePathPackage;
    function GetNodeForPackage (const packageName : string) : PVirtualNode;
    procedure UpdateCustomText;
  protected
    procedure UpdateActions; override;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageRCSettings: TfmPropertyPageRCSettings;

implementation

uses unitCREdProperties;

{$R *.DFM}

type
  PObject = ^TObject;

{ TfmPropertyPageRCSettings }

class function TfmPropertyPageRCSettings.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageRCSettingsData;
end;

procedure TfmPropertyPageRCSettings.PopulateControls(AData: TPropertyPageData);
var
  node : PVirtualNode;
begin
  Inherited;
  fIncludePathPackages := TIncludePathPackages.Create;
  vstIncludePackages.RootNodeCount := fIncludePathPackages.Count;

  fData := TPropertyPageRCSettingsData (AData);

  case fData.fIncludePathType of
    includePathPackage : begin
                           rbCompilerIncludePath.Checked := True;
                           node := GetNodeForPackage (fData.fIncludePathPackageName);
                           if Assigned (node) then
                             node.CheckState := csCheckedNormal
                         end;

    includePathEnv     : rbEnvironmentVariableIncludePath.Checked:= True;

    includePathCustom  : begin
                           rbCustomIncludePath.Checked := True;
                           edCustomIncludePath.Text := fData.fCustomIncludePath
                         end;
  end;
  UpdateCustomText
end;

procedure TfmPropertyPageRCSettings.UpdateActions;
begin
  case fData.fIncludePathType of
    includePathPackage : begin
                           vstIncludePackages.Enabled := True;
                           btnCustomIncludePath.Enabled := False;
                           edCustomIncludePath.Enabled := False;
                         end;
    includePathCustom  : begin
                           vstIncludePackages.Enabled := False;
                           btnCustomIncludePath.Enabled := True;
                           edCustomIncludePath.Enabled := True;
                         end;
    includePathEnv     : begin
                           vstIncludePackages.Enabled := False;
                           btnCustomIncludePath.Enabled := False;
                           edCustomIncludePath.Enabled := False;
                         end;
  end
end;

function TfmPropertyPageRCSettings.GetNodePackage(
  node: PVirtualNode): TIncludePathPackage;
var
  data : PObject;
begin
  data := PObject (vstIncludePackages.GetNodeData(node));
  result := TIncludePathPackage (data^);
end;

function TfmPropertyPageRCSettings.GetNodeForPackage(
  const packageName: string): PVirtualNode;
begin
  result := vstIncludePackages.GetFirst;
  while Assigned (result) do
  begin
    if SameText (GetNodePackage (result).Name, packageName) then
      break
    else
      result := vstIncludePackages.GetNext(result)
  end
end;

procedure TfmPropertyPageRCSettings.UpdateCustomText;
var
  st : string;
begin
  case fData.fIncludePathType of
    includePathPackage : st := GetIncludePathForPackage (fData.fIncludePathPackageName);
    includePathCustom  : st := fData.fCustomIncludePath;
    includePathEnv     : st := GetEnvironmentVariable ('Include');
  end;

  edCustomIncludePath.Text := st;
  fData.fCustomIncludePath := st;
end;

{ TPropertyPageRCSettingsData }

procedure TPropertyPageRCSettingsData.Apply;
begin
  gProperties.IncludePathType := fIncludePathType;
  case fIncludePathType of
    includePathPackage :  gProperties.IncludePathPackageName := fIncludePathPackageName;
    includePathCustom  :  gProperties.IncludePath := fCustomIncludePath;
  end
end;

procedure TPropertyPageRCSettingsData.Initialize;
begin
  fIncludePathType := gProperties.IncludePathType;
  fCustomIncludePath := gProperties.IncludePath;
  fIncludePathPackageName := gProperties.IncludePathPackageName
end;

procedure TfmPropertyPageRCSettings.FormDestroy(Sender: TObject);
begin
  inherited;
  fIncludePathPackages.Free;
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  data : PObject;
begin
  data := PObject (vstIncludePackages.GetNodeData(node));
  data^ := fIncludePathPackages.Package [node^.Index];
  Node^.CheckType := ctRadioButton ;
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  package : TIncludePathPackage;
begin
  package := GetNodePackage(node);
  if Assigned (package) then
    CellText := package.Name
end;

procedure TfmPropertyPageRCSettings.vstIncludePackagesChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  inherited;
  if node.CheckState = csCheckedNormal then
  begin
    fData.fIncludePathPackageName := GetNodePackage (Node).Name;
    UpdateCustomText
  end;
end;

procedure TfmPropertyPageRCSettings.rbCustomIncludePathClick(Sender: TObject);
var
  compilerPath : boolean;
  p : PVirtualNode;
  package : TIncludePathPackage;
begin
  compilerPath := rbCompilerIncludePath.Checked;

  p := vstIncludePackages.GetFirst;
  while Assigned (p) do
  begin
    package := GetNodePackage(p);
    if compilerPath and SameText (package.Name, fData.fIncludePathPackageName) then
      p.CheckState := csCheckedNormal
    else
      p.CheckState := csUncheckedNormal;

    p := vstIncludePackages.GetNext(p);
  end;

  if rbCompilerIncludePath.Checked then
    fData.fIncludePathType := includePathPackage
  else
    if rbEnvironmentVariableIncludePath.Checked then
      fData.fIncludePathType := includePathEnv
    else
      fData.fIncludePathType := includePathCustom;

  UpdateCustomText;
end;

end.
