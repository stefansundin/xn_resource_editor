unit MiscUnitsReg;

interface

procedure Register;

implementation

{$R MiscUnitsReg.dcr}

uses
  Classes,
  cmpStandardSystemMenu,
  cmpPersistentPosition,
  cmpNTAboutBox,
  cmpHyperlinkButton,
  cmpExSplitter,
  cmpMessageDisplay,
  cmpMRUList,
  cmpPropertyListBox,
  cmpFakeComboBox,
  cmpCWRichEdit,
  cmpRuler,
  cmpHexDump,
  cmpSizingPageControl,
  cmpSpellChecker,
  cmpCWSpellChecker,
  cmpExWebBrowser,
  cmpColorSelector,
  cmpPersistentOptions;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [
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
    TIniFilePersistentOptions
  ]);
end;

end.
