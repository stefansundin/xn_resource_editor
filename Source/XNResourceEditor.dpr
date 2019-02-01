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
  MainForm in 'MainForm.pas' {FormMain},
  AcceleratorResourceForm in 'AcceleratorResourceForm.pas' {FormAcceleratorResource},
  AddResourceDialog in 'AddResourceDialog.pas' {DialogAddResource},
  CloneResourceDialog in 'CloneResourceDialog.pas' {DialogCloneResource},
  ComponentBitmapEditor in 'Components\ComponentBitmapEditor.pas',
  ComponentColorSelector in 'Components\ComponentColorSelector.pas',
  ComponentCWRichEdit in 'Components\ComponentCWRichEdit.pas',
  ComponentCWSpellChecker in 'Components\ComponentCWSpellChecker.pas',
  ComponentDialogBox in 'Components\ComponentDialogBox.pas',
  ComponentDialogEditor in 'Components\ComponentDialogEditor.pas',
  ComponentExSplitter in 'Components\ComponentExSplitter.pas',
  ComponentExWebBrowser in 'Components\ComponentExWebBrowser.pas',
  ComponentFakeCombobox in 'Components\ComponentFakeCombobox.pas',
  ComponentGradientShape in 'Components\ComponentGradientShape.pas',
  ComponentHexDump in 'Components\ComponentHexDump.pas',
  ComponentHyperlinkButton in 'Components\ComponentHyperlinkButton.pas',
  ComponentMenuDesigner in 'Components\ComponentMenuDesigner.pas',
  ComponentMessageDisplay in 'Components\ComponentMessageDisplay.pas',
  ComponentMRUList in 'Components\ComponentMRUList.pas',
  ComponentNTAboutBox in 'Components\ComponentNTAboutBox.pas' {FormNTAboutBox},
  ComponentPersistentOptions in 'Components\ComponentPersistentOptions.pas',
  ComponentPersistentPosition in 'Components\ComponentPersistentPosition.pas',
  ComponentPropertyListBox in 'Components\ComponentPropertyListBox.pas',
  ComponentRuler in 'Components\ComponentRuler.pas',
  ComponentSizingPageControl in 'Components\ComponentSizingPageControl.pas',
  ComponentSpellChecker in 'Components\ComponentSpellChecker.pas',
  ComponentStandardSystemMenu in 'Components\ComponentStandardSystemMenu.pas',
  CursorGraphicsResourceForm in 'CursorGraphicsResourceForm.pas' {FormCursorGraphicsResource},
  DescriptionRCDataResourceForm in 'DescriptionRCDataResourceForm.pas' {FormRCDataDescriptionResource},
  DialogButtonControls in 'Components\DialogButtonControls.pas',
  DialogComboBoxControls in 'Components\DialogComboBoxControls.pas',
  DialogConsts in 'Components\DialogConsts.pas',
  DialogEditControls in 'Components\DialogEditControls.pas',
  DialogHotkeyControls in 'Components\DialogHotkeyControls.pas',
  DialogListboxControls in 'Components\DialogListboxControls.pas',
  DialogListViewControls in 'Components\DialogListViewControls.pas',
  DialogProgressBarControls in 'Components\DialogProgressBarControls.pas',
  DialogResourceForm in 'DialogResourceForm.pas' {FormDialogResource},
  DialogScrollbarControls in 'Components\DialogScrollbarControls.pas',
  DialogSliderControls in 'Components\DialogSliderControls.pas',
  DialogStaticControls in 'Components\DialogStaticControls.pas',
  DialogStrings in 'Components\DialogStrings.pas',
  DialogUpDownControls in 'Components\DialogUpDownControls.pas',
  ExVirtualStringTree in 'Components\ExVirtualStringTree.pas',
  FormResourceForm in 'FormResourceForm.pas' {FormRCDataFormResource},
  FormTextInput in 'FormTextInput.pas' {FormTextInput},
  GIFImage in 'Components\GIFImage.pas',
  GraphFlip in 'Components\GraphFlip.pas',
  GraphicsResourceForm in 'GraphicsResourceForm.pas' {FormGraphicsResource},
  GroupResourceForm in 'GroupResourceForm.pas' {FormGroupResource},
  HelpContext in 'HelpContext.pas',
  IconCursorGraphicsResourceForm in 'IconCursorGraphicsResourceForm.pas' {FormIconCursorGraphicsResource},
  IconGraphicsResourceForm in 'IconGraphicsResourceForm.pas' {FormIconGraphicsResource},
  MenuResourceForm in 'MenuResourceForm.pas' {FormMenuResource},
  MultiLanguage_TLB in 'Components\MultiLanguage_TLB.pas',
  PackagesResourceForm in 'PackagesResourceForm.pas' {FormPackagesResource},
  PropertiesForm in 'PropertiesForm.pas' {FormProperties},
  PropertyBaseForm in 'PropertyBaseForm.pas' {FormPropertyBase},
  PropertyPageForm in 'PropertyPageForm.pas' {FormPropertyPage},
  PropertyPageProgramSettings in 'PropertyPageProgramSettings.pas' {FormPropertyPageProgramSettings},
  PropertyPageRCSettings in 'PropertyPageRCSettings.pas' {FormPropertyPageRCSettings},
  RawResourceForm in 'RawResourceForm.pas' {FormRawResource},
  ResourceForm in 'ResourceForm.pas' {FormResource},
  ResourceObjectForm in 'ResourceObjectForm.pas' {FormResourceObject},
  ResourcePropertiesDialog in 'ResourcePropertiesDialog.pas' {dlgResourceProperties},
  RichOle in 'Components\RichOle.pas',
  SpellCheckerForm in 'Components\SpellCheckerForm.pas',
  TextResourceForm in 'TextResourceForm.pas' {FormTextResource},
  VersionResourceForm in 'VersionResourceForm.pas' {FormVersionResource},
  XPManifestResourceForm in 'XPManifestResourceForm.pas' {FormXPManifestResource},
  unitBTree in 'Components\unitBTree.pas',
  unitCExpression in 'Components\unitCExpression.pas',
  unitCharsetMap in 'Components\unitCharsetMap.pas',
  unitClipExRegistry in 'Components\unitClipExRegistry.pas',
  unitCREdProperties in 'unitCREdProperties.pas',
  unitDefRegistry in 'Components\unitDefRegistry.pas',
  unitEXGraphics in 'Components\unitEXGraphics.pas',
  unitExIcon in 'Components\unitExIcon.pas',
  unitExRegistry in 'Components\unitExRegistry.pas',
  unitHTMLHelp in 'Components\unitHTMLHelp.pas',
  unitHTMLHelpViewer in 'Components\unitHTMLHelpViewer.pas',
  unitHTMLStringsDisplayObject in 'Components\unitHTMLStringsDisplayObject.pas',
  unitIncludePathPackages in 'unitIncludePathPackages.pas',
  unitIncludePaths in 'unitIncludePaths.pas',
  unitMidiGlobals in 'unitMidiGlobals.pas',
  unitMidiTrackStream in 'unitMidiTrackStream.pas',
  unitNTModule in 'Components\unitNTModule.pas',
  unitObjectCache in 'Components\unitObjectCache.pas',
  unitParser in 'Components\unitParser.pas',
  unitPEFile in 'Components\unitPEFile.pas',
  unitRCFile in 'Components\unitRCFile.pas',
  unitResFile in 'Components\unitResFile.pas',
  unitResourceAccelerator in 'Components\unitResourceAccelerator.pas',
  unitResourceDetails in 'Components\unitResourceDetails.pas',
  unitResourceDialogs in 'Components\unitResourceDialogs.pas',
  unitResourceExaminer in 'Components\unitResourceExaminer.pas',
  unitResourceGIF in 'Components\unitResourceGIF.pas',
  unitResourceGraphics in 'Components\unitResourceGraphics.pas',
  unitResourceHTML in 'Components\unitResourceHTML.pas',
  unitResourceJPEG in 'Components\unitResourceJPEG.pas',
  unitResourceMenus in 'Components\unitResourceMenus.pas',
  unitResourceMessages in 'Components\unitResourceMessages.pas',
  unitResourcePNG in 'Components\unitResourcePNG.pas',
  unitResourceRCData in 'Components\unitResourceRCData.pas',
  unitResourceToolbar in 'Components\unitResourceToolbar.pas',
  unitResourceVersionInfo in 'Components\unitResourceVersionInfo.pas',
  unitResourceXPManifests in 'Components\unitResourceXPManifests.pas',
  unitSearchString in 'Components\unitSearchString.pas',
  unitStreamTextReader in 'Components\unitStreamTextReader.pas',
  unitVersionInfo in 'Components\unitVersionInfo.pas';

{$R i.res}

begin
  Application.Initialize;
  Application.Title := 'XN Resource Editor';
  Application.HelpFile := 'XNResourceEditor.chm';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
