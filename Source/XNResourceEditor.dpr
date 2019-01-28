(*======================================================================*
 | program XNResourceEditor                                                     |
 |                                                                      |
 | Colin's Resource Editor.                                             |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      07/02/2001  CPWW  Original                                  |
 | 2.0      25/11/2004  CPWW  Further enhancements for new 'XN Resource |
 |                            Editor' product                           |
 | 3.0      13/06/2005  CPWW  Unicode enabled.  Support for Windows 98  |
 |                            dropped                                   |
 *======================================================================*)

program XNResourceEditor;

uses
  Forms,
  MainForm in 'MainForm.pas' {fmMain},
  ResourceObjectForm in 'ResourceObjectForm.pas' {fmResourceObject},
  ResourceForm in 'ResourceForm.pas' {fmResource},
  RawResourceForm in 'RawResourceForm.pas' {fmRawResource},
  GraphicsResourceForm in 'GraphicsResourceForm.pas' {fmGraphicsResource},
  TextResourceForm in 'TextResourceForm.pas' {fmTextResource},
  unitCREdProperties in 'unitCREdProperties.pas',
  PropertiesForm in 'PropertiesForm.pas' {fmProperties},
  GroupResourceForm in 'GroupResourceForm.pas' {fmGroupResource},
  AddResourceDialog in 'AddResourceDialog.pas' {dlgAddResource},
  VersionResourceForm in 'VersionResourceForm.pas' {fmVersionResource},
  MenuResourceForm in 'MenuResourceForm.pas' {fmMenuResource},
  ResourcePropertiesDialog in 'ResourcePropertiesDialog.pas' {dlgResourceProperties},
  DialogResourceForm in 'DialogResourceForm.pas' {fmDialogResource},
  DescriptionRCDataResourceForm in 'DescriptionRCDataResourceForm.pas' {fmRCDataDescriptionResource},
  PackagesResourceForm in 'PackagesResourceForm.pas' {fmPackagesResource},
  FormResourceForm in 'FormResourceForm.pas' {fmRCDataFormResource},
  XPManifestResourceForm in 'XPManifestResourceForm.pas' {fmXPManifestResource},
  AcceleratorResourceForm in 'AcceleratorResourceForm.pas' {fmAcceleratorResource},
  DialogStrings in 'DialogStrings.pas',
  DialogSliderControls in 'DialogSliderControls.pas',
  CloneResourceDialog in 'CloneResourceDialog.pas' {dlgCloneResource},
  PropertyBaseForm in 'PropertyBaseForm.pas' {fmPropertyBase},
  PropertyPageForm in 'PropertyPageForm.pas' {fmPropertyPage},
  PropertyPageRCSettings in 'PropertyPageRCSettings.pas' {fmPropertyPageRCSettings},
  PropertyPageProgramSettings in 'PropertyPageProgramSettings.pas' {fmPropertyPageProgramSettings},
  unitIncludePathPackages in 'unitIncludePathPackages.pas',
  unitIncludePaths in 'unitIncludePaths.pas',
  HelpContext in 'HelpContext.pas',
  IconCursorGraphicsResourceForm in 'IconCursorGraphicsResourceForm.pas' {fmIconCursorGraphicsResource},
  CursorGraphicsResourceForm in 'CursorGraphicsResourceForm.pas' {fmCursorGraphicsResource},
  IconGraphicsResourceForm in 'IconGraphicsResourceForm.pas' {fmIconGraphicsResource},
  DialogListViewControls in 'DialogListViewControls.pas',
  FormTextInput in 'FormTextInput.pas' {fmTextInput};

{$R *.RES}
{$R i.res}

begin
  Application.Initialize;
  Application.Title := 'XN Resource Editor';
  Application.HelpFile := 'XNResourceEditor.chm';
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmIconCursorGraphicsResource, fmIconCursorGraphicsResource);
  Application.CreateForm(TfmCursorGraphicsResource, fmCursorGraphicsResource);
  Application.CreateForm(TfmIconGraphicsResource, fmIconGraphicsResource);
  Application.CreateForm(TfmTextInput, fmTextInput);
  Application.Run;
end.
