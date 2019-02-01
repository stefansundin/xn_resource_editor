unit MiscUnitsReg;

interface

procedure Register;

implementation

{$R MiscUnitsReg.dcr}

uses
  Classes,
  cmpRunOnce,
  cmpStandardSystemMenu,
  cmpPersistentPosition,
  cmpNTAboutBox,
  cmpHyperlinkButton,
  cmpExSplitter,
  cmpTrayIcon,
  cmpMessageDisplay,
  cmpMRUList,
  cmpPropertyListBox,
  cmpFakeComboBox,
  cmpThemedScrollBox,
  cmpCWRichEdit,
  cmpNewsRichEdit,
  cmpRuler,
  cmpHexDump,
  cmpSizingPageControl,
  cmpSpellChecker,
  cmpCWSpellChecker,
  cmpExWebBrowser,
  cmpTexturedPanel,
  cmpColorSelector,
  cmpPersistentOptions,
  cmpFileCopier,
  cmpSplitterPanel,
  cmpTextDisplay,
  cmpUCtrls;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [
    TRunOnce,
    TStandardSystemMenu,
    TPersistentPosition,
    TNTAboutBox,
    THyperlinkButton,
    TExSplitter,
    TMessageDisplay,
    TMRUList,
    TPropertyListBox,
    TFakeComboBox,
    TThemedScrollBox,
    TExRichEdit,
    TNewsRichEdit,
    TRuler,
    THexDump,
    TSizingPageControl,
    TSpellChecker,
    TCWSpellChecker,
    TExWebBrowser,
    TTexturedPanel,
    TColorSelector,
    TRegistryPersistentOptions,
    TIniFilePersistentOptions,
    TFileCopier,
    TSplitterPanel,
    TTextDisplay,
    TUEdit,
    TUComboBox
  ]);
end;

end.
