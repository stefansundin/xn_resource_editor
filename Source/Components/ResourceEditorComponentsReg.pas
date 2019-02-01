unit ResourceEditorComponentsReg;

interface

procedure Register;

{$R MiscUnitsReg.dcr}

implementation

uses
  Classes,
  ComponentBitmapEditor,
  ComponentMenuDesigner,
  ComponentDialogBox,
  ComponentDialogEditor,
  ComponentStandardSystemMenu,
  ComponentPersistentPosition,
  ComponentNTAboutBox,
  ComponentHyperlinkButton,
  ComponentExSplitter,
  ComponentMessageDisplay,
  ComponentMRUList,
  ComponentPropertyListBox,
  ComponentFakeComboBox,
  ComponentCWRichEdit,
  ComponentRuler,
  ComponentHexDump,
  ComponentSizingPageControl,
  ComponentSpellChecker,
  ComponentCWSpellChecker,
  ComponentExWebBrowser,
  ComponentColorSelector,
  ComponentPersistentOptions,
  ExVirtualStringTree;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [
    TBitmapEditor,
    TMenuDesigner,
    TPopupMenuDesigner,
    TDialogBox,
    TDialogEditor,
    TStandardSystemMenu,
    TPersistentPosition,
    TNTAboutBox,
    THyperlinkButton,
    TExSplitter,
    TMessageDisplay,
    TMRUList,
    TPropertyListBox,
    TFakeComboBox,
    TExRichEdit,
    TRuler,
    THexDump,
    TSizingPageControl,
    TSpellChecker,
    TCWSpellChecker,
    TExWebBrowser,
    TColorSelector,
    TRegistryPersistentOptions,
    TIniFilePersistentOptions,
    TExVirtualStringTree
  ])
end;

end.
