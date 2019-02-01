unit MultiLanguage_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly(via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 16/12/2002 13:07:40 from Type Library described below.

// ************************************************************************  //
// Type Lib: mlang.tlb (1)
// LIBID: {275C23E0-3747-11D0-9FEA-00AA003F8646}
// LCID: 0
// Helpfile: 
// HelpString: MultiLanguage Object Model
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
// Errors:
//   Hint: Symbol 'type' renamed to 'type_'
//   Hint: Symbol 'type' renamed to 'type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses
  Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  MultiLanguageMajorVersion = 0;
  MultiLanguageMinorVersion = 2;

  LIBID_MultiLanguage: TGUID = '{275C23E0-3747-11D0-9FEA-00AA003F8646}';

  IID_IMLangStringBufW: TGUID = '{D24ACD21-BA72-11D0-B188-00AA0038C969}';
  IID_IMLangStringBufA: TGUID = '{D24ACD23-BA72-11D0-B188-00AA0038C969}';
  IID_IMLangString: TGUID = '{C04D65CE-B70D-11D0-B188-00AA0038C969}';
  IID_IMLangStringWStr: TGUID = '{C04D65D0-B70D-11D0-B188-00AA0038C969}';
  IID_IMLangStringAStr: TGUID = '{C04D65D2-B70D-11D0-B188-00AA0038C969}';
  CLASS_CMLangString: TGUID = '{C04D65CF-B70D-11D0-B188-00AA0038C969}';
  IID_IMLangLineBreakConsole: TGUID = '{F5BE2EE1-BFD7-11D0-B188-00AA0038C969}';
  IID_IEnumCodePage: TGUID = '{275C23E3-3747-11D0-9FEA-00AA003F8646}';
  IID_IEnumRfc1766: TGUID = '{3DC39D1D-C030-11D0-B81B-00C04FC9B31F}';
  IID_IEnumScript: TGUID = '{AE5F1430-388B-11D2-8380-00C04F8F5DA1}';
  IID_IMLangConvertCharset: TGUID = '{D66D6F98-CDAA-11D0-B822-00C04FC9B31F}';
  CLASS_CMLangConvertCharset: TGUID = '{D66D6F99-CDAA-11D0-B822-00C04FC9B31F}';
  IID_IMultiLanguage: TGUID = '{275C23E1-3747-11D0-9FEA-00AA003F8646}';
  IID_IMultiLanguage2: TGUID = '{DCCFC164-2B38-11D2-B7EC-00C04F8F5D9A}';
  IID_ISequentialStream: TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IStream: TGUID = '{0000000C-0000-0000-C000-000000000046}';
  IID_IMLangCodePages: TGUID = '{359F3443-BD4A-11D0-B188-00AA0038C969}';
  IID_IMLangFontLink: TGUID = '{359F3441-BD4A-11D0-B188-00AA0038C969}';
  IID_IMLangFontLink2: TGUID = '{DCCFC162-2B38-11D2-B7EC-00C04F8F5D9A}';
  IID_IMultiLanguage3: TGUID = '{4E5868AB-B157-4623-9ACC-6A1D9CAEBE04}';
  CLASS_CMultiLanguage: TGUID = '{275C23E2-3747-11D0-9FEA-00AA003F8646}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum tagMLSTR_FLAGS
type
  tagMLSTR_FLAGS = TOleEnum;
const
  MLSTR_READ = $00000001;
  MLSTR_WRITE = $00000002;

// Constants for enum tagMIMECONTF
type
  tagMIMECONTF = TOleEnum;
const
  MIMECONTF_MAILNEWS = $00000001;
  MIMECONTF_BROWSER = $00000002;
  MIMECONTF_MINIMAL = $00000004;
  MIMECONTF_IMPORT = $00000008;
  MIMECONTF_SAVABLE_MAILNEWS = $00000100;
  MIMECONTF_SAVABLE_BROWSER = $00000200;
  MIMECONTF_EXPORT = $00000400;
  MIMECONTF_PRIVCONVERTER = $00010000;
  MIMECONTF_VALID = $00020000;
  MIMECONTF_VALID_NLS = $00040000;
  MIMECONTF_MIME_IE4 = $10000000;
  MIMECONTF_MIME_LATEST = $20000000;
  MIMECONTF_MIME_REGISTRY = $40000000;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IMLangStringBufW = interface;
  IMLangStringBufA = interface;
  IMLangString = interface;
  IMLangStringWStr = interface;
  IMLangStringAStr = interface;
  IMLangLineBreakConsole = interface;
  IEnumCodePage = interface;
  IEnumRfc1766 = interface;
  IEnumScript = interface;
  IMLangConvertCharset = interface;
  IMultiLanguage = interface;
  IMultiLanguage2 = interface;
  ISequentialStream = interface;
  IStream = interface;
  IMLangCodePages = interface;
  IMLangFontLink = interface;
  IMLangFontLink2 = interface;
  IMultiLanguage3 = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CMLangString = IMLangString;
  CMLangConvertCharset = IMLangConvertCharset;
  CMultiLanguage = IMultiLanguage;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  wireHWND = ^_RemotableHandle; 
  wireHDC = ^_RemotableHandle; 
  wireHFONT = ^_RemotableHandle; 
  PSmallint1 = ^Smallint; {*}
  PShortint1 = ^Shortint; {*}
  PUserType1 = ^TGUID; {*}
  PByte1 = ^Byte; {*}
  PSYSUINT1 = ^SYSUINT; {*}

  tagMIMECPINFO = packed record
    dwFlags: LongWord;
    uiCodePage: SYSUINT;
    uiFamilyCodePage: SYSUINT;
    wszDescription: array[0..63] of WideChar;
    wszWebCharset: array[0..49] of WideChar;
    wszHeaderCharset: array[0..49] of WideChar;
    wszBodyCharset: array[0..49] of WideChar;
    wszFixedWidthFont: array[0..31] of WideChar;
    wszProportionalFont: array[0..31] of WideChar;
    bGDICharset: Byte;
    padding : array [0..2] of byte;
  end;

  tagRFC1766INFO = packed record
    lcid: LongWord;
    wszRfc1766: array[0..5] of WideChar;
    wszLocaleName: array[0..31] of WideChar;
  end;

  tagSCRIPTINFO = packed record
    ScriptId: Byte;
    uiCodePage: SYSUINT;
    wszDescription: array[0..47] of WideChar;
    wszFixedWidthFont: array[0..31] of WideChar;
    wszProportionalFont: array[0..31] of WideChar;
  end;

  tagMIMECSETINFO = packed record
    uiCodePage: SYSUINT;
    uiInternetEncoding: SYSUINT;
    wszCharset: array[0..49] of WideChar;
  end;

  _LARGE_INTEGER = packed record
    QuadPart: Int64;
  end;

  _ULARGE_INTEGER = packed record
    QuadPart: Largeuint;
  end;

  _FILETIME = packed record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;

  tagSTATSTG = packed record
    pwcsName: PWideChar;
    type_: LongWord;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: LongWord;
    grfLocksSupported: LongWord;
    clsid: TGUID;
    grfStateBits: LongWord;
    reserved: LongWord;
  end;

  tagDetectEncodingInfo = packed record
    nLangID: SYSUINT;
    nCodePage: SYSUINT;
    nDocPercent: SYSINT;
    nConfidence: SYSINT;
  end;


  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;


  tagUNICODERANGE = packed record
    wcFrom: Smallint;
    wcTo: Smallint;
  end;

  tagSCRIPFONTINFO = packed record
    scripts: Int64;
    wszFont: array[0..31] of WideChar;
  end;


// *********************************************************************//
// Interface: IMLangStringBufW
// Flags:     (0)
// GUID:      {D24ACD21-BA72-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangStringBufW = interface(IUnknown)
    ['{D24ACD21-BA72-11D0-B188-00AA0038C969}']
    function GetStatus(out plFlags: Integer; out pcchBuf: Integer): HResult; stdcall;
    function LockBuf(cchOffset: Integer; cchMaxLock: Integer; out ppszBuf: PSmallint1; 
                     out pcchBuf: Integer): HResult; stdcall;
    function UnlockBuf(var pszBuf: Smallint; cchOffset: Integer; cchWrite: Integer): HResult; stdcall;
    function Insert(cchOffset: Integer; cchMaxInsert: Integer; out pcchActual: Integer): HResult; stdcall;
    function Delete(cchOffset: Integer; cchDelete: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangStringBufA
// Flags:     (0)
// GUID:      {D24ACD23-BA72-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangStringBufA = interface(IUnknown)
    ['{D24ACD23-BA72-11D0-B188-00AA0038C969}']
    function GetStatus(out plFlags: Integer; out pcchBuf: Integer): HResult; stdcall;
    function LockBuf(cchOffset: Integer; cchMaxLock: Integer; out ppszBuf: PShortint1; 
                     out pcchBuf: Integer): HResult; stdcall;
    function UnlockBuf(var pszBuf: Shortint; cchOffset: Integer; cchWrite: Integer): HResult; stdcall;
    function Insert(cchOffset: Integer; cchMaxInsert: Integer; out pcchActual: Integer): HResult; stdcall;
    function Delete(cchOffset: Integer; cchDelete: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangString
// Flags:     (0)
// GUID:      {C04D65CE-B70D-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangString = interface(IUnknown)
    ['{C04D65CE-B70D-11D0-B188-00AA0038C969}']
    function Sync(fNoAccess: Integer): HResult; stdcall;
    function GetLength(out plLen: Integer): HResult; stdcall;
    function SetMLStr(lDestPos: Integer; lDestLen: Integer; const pSrcMLStr: IUnknown; 
                      lSrcPos: Integer; lSrcLen: Integer): HResult; stdcall;
    function GetMLStr(lSrcPos: Integer; lSrcLen: Integer; const pUnkOuter: IUnknown; 
                      dwClsContext: LongWord; var piid: TGUID; out ppDestMLStr: IUnknown; 
                      out plDestPos: Integer; out plDestLen: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangStringWStr
// Flags:     (0)
// GUID:      {C04D65D0-B70D-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangStringWStr = interface(IMLangString)
    ['{C04D65D0-B70D-11D0-B188-00AA0038C969}']
    function SetWStr(lDestPos: Integer; lDestLen: Integer; var pszSrc: Smallint; cchSrc: Integer; 
                     out pcchActual: Integer; out plActualLen: Integer): HResult; stdcall;
    function SetStrBufW(lDestPos: Integer; lDestLen: Integer; const pSrcBuf: IMLangStringBufW; 
                        out pcchActual: Integer; out plActualLen: Integer): HResult; stdcall;
    function GetWStr(lSrcPos: Integer; lSrcLen: Integer; out pszDest: Smallint; cchDest: Integer; 
                     out pcchActual: Integer; out plActualLen: Integer): HResult; stdcall;
    function GetStrBufW(lSrcPos: Integer; lSrcMaxLen: Integer; out ppDestBuf: IMLangStringBufW; 
                        out plDestLen: Integer): HResult; stdcall;
    function LockWStr(lSrcPos: Integer; lSrcLen: Integer; lFlags: Integer; cchRequest: Integer; 
                      out ppszDest: PSmallint1; out pcchDest: Integer; out plDestLen: Integer): HResult; stdcall;
    function UnlockWStr(var pszSrc: Smallint; cchSrc: Integer; out pcchActual: Integer; 
                        out plActualLen: Integer): HResult; stdcall;
    function SetLocale(lDestPos: Integer; lDestLen: Integer; locale: LongWord): HResult; stdcall;
    function GetLocale(lSrcPos: Integer; lSrcMaxLen: Integer; out plocale: LongWord; 
                       out plLocalePos: Integer; out plLocaleLen: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangStringAStr
// Flags:     (0)
// GUID:      {C04D65D2-B70D-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangStringAStr = interface(IMLangString)
    ['{C04D65D2-B70D-11D0-B188-00AA0038C969}']
    function SetAStr(lDestPos: Integer; lDestLen: Integer; uCodePage: SYSUINT; 
                     var pszSrc: Shortint; cchSrc: Integer; out pcchActual: Integer; 
                     out plActualLen: Integer): HResult; stdcall;
    function SetStrBufA(lDestPos: Integer; lDestLen: Integer; uCodePage: SYSUINT; 
                        const pSrcBuf: IMLangStringBufA; out pcchActual: Integer; 
                        out plActualLen: Integer): HResult; stdcall;
    function GetAStr(lSrcPos: Integer; lSrcLen: Integer; uCodePageIn: SYSUINT; 
                     out puCodePageOut: SYSUINT; out pszDest: Shortint; cchDest: Integer; 
                     out pcchActual: Integer; out plActualLen: Integer): HResult; stdcall;
    function GetStrBufA(lSrcPos: Integer; lSrcMaxLen: Integer; out puDestCodePage: SYSUINT; 
                        out ppDestBuf: IMLangStringBufA; out plDestLen: Integer): HResult; stdcall;
    function LockAStr(lSrcPos: Integer; lSrcLen: Integer; lFlags: Integer; uCodePageIn: SYSUINT; 
                      cchRequest: Integer; out puCodePageOut: SYSUINT; out ppszDest: PShortint1; 
                      out pcchDest: Integer; out plDestLen: Integer): HResult; stdcall;
    function UnlockAStr(var pszSrc: Shortint; cchSrc: Integer; out pcchActual: Integer; 
                        out plActualLen: Integer): HResult; stdcall;
    function SetLocale(lDestPos: Integer; lDestLen: Integer; locale: LongWord): HResult; stdcall;
    function GetLocale(lSrcPos: Integer; lSrcMaxLen: Integer; out plocale: LongWord; 
                       out plLocalePos: Integer; out plLocaleLen: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangLineBreakConsole
// Flags:     (0)
// GUID:      {F5BE2EE1-BFD7-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangLineBreakConsole = interface(IUnknown)
    ['{F5BE2EE1-BFD7-11D0-B188-00AA0038C969}']
    function BreakLineML(const pSrcMLStr: IMLangString; lSrcPos: Integer; lSrcLen: Integer; 
                         cMinColumns: Integer; cMaxColumns: Integer; out plLineLen: Integer; 
                         out plSkipLen: Integer): HResult; stdcall;
    function BreakLineW(locale: LongWord; var pszSrc: Smallint; cchSrc: Integer; 
                        cMaxColumns: Integer; out pcchLine: Integer; out pcchSkip: Integer): HResult; stdcall;
    function BreakLineA(locale: LongWord; uCodePage: SYSUINT; var pszSrc: Shortint; 
                        cchSrc: Integer; cMaxColumns: Integer; out pcchLine: Integer; 
                        out pcchSkip: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumCodePage
// Flags:     (0)
// GUID:      {275C23E3-3747-11D0-9FEA-00AA003F8646}
// *********************************************************************//
  IEnumCodePage = interface(IUnknown)
    ['{275C23E3-3747-11D0-9FEA-00AA003F8646}']
    function Clone(out ppEnum: IEnumCodePage): HResult; stdcall;
    function Next(celt: LongWord; out rgelt: tagMIMECPINFO; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumRfc1766
// Flags:     (0)
// GUID:      {3DC39D1D-C030-11D0-B81B-00C04FC9B31F}
// *********************************************************************//
  IEnumRfc1766 = interface(IUnknown)
    ['{3DC39D1D-C030-11D0-B81B-00C04FC9B31F}']
    function Clone(out ppEnum: IEnumRfc1766): HResult; stdcall;
    function Next(celt: LongWord; out rgelt: tagRFC1766INFO; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumScript
// Flags:     (0)
// GUID:      {AE5F1430-388B-11D2-8380-00C04F8F5DA1}
// *********************************************************************//
  IEnumScript = interface(IUnknown)
    ['{AE5F1430-388B-11D2-8380-00C04F8F5DA1}']
    function Clone(out ppEnum: IEnumScript): HResult; stdcall;
    function Next(celt: LongWord; out rgelt: tagSCRIPTINFO; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangConvertCharset
// Flags:     (0)
// GUID:      {D66D6F98-CDAA-11D0-B822-00C04FC9B31F}
// *********************************************************************//
  IMLangConvertCharset = interface(IUnknown)
    ['{D66D6F98-CDAA-11D0-B822-00C04FC9B31F}']
    function Initialize(uiSrcCodePage: SYSUINT; uiDstCodePage: SYSUINT; dwProperty: LongWord): HResult; stdcall;
    function GetSourceCodePage(out puiSrcCodePage: SYSUINT): HResult; stdcall;
    function GetDestinationCodePage(out puiDstCodePage: SYSUINT): HResult; stdcall;
    function GetProperty(out pdwProperty: LongWord): HResult; stdcall;
    function DoConversion(var pSrcStr: Byte; var pcSrcSize: SYSUINT; var pDstStr: Byte; 
                          var pcDstSize: SYSUINT): HResult; stdcall;
    function DoConversionToUnicode(var pSrcStr: Shortint; var pcSrcSize: SYSUINT; 
                                   var pDstStr: Smallint; var pcDstSize: SYSUINT): HResult; stdcall;
    function DoConversionFromUnicode(var pSrcStr: Smallint; var pcSrcSize: SYSUINT; 
                                     var pDstStr: Shortint; var pcDstSize: SYSUINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMultiLanguage
// Flags:     (0)
// GUID:      {275C23E1-3747-11D0-9FEA-00AA003F8646}
// *********************************************************************//
  IMultiLanguage = interface(IUnknown)
    ['{275C23E1-3747-11D0-9FEA-00AA003F8646}']
    function GetNumberOfCodePageInfo(out pcCodePage: SYSUINT): HResult; stdcall;
    function GetCodePageInfo(uiCodePage: SYSUINT; out pCodePageInfo: tagMIMECPINFO): HResult; stdcall;
    function GetFamilyCodePage(uiCodePage: SYSUINT; out puiFamilyCodePage: SYSUINT): HResult; stdcall;
    function EnumCodePages(grfFlags: LongWord; out ppEnumCodePage: IEnumCodePage): HResult; stdcall;
    function GetCharsetInfo(const Charset: WideString; out pCharsetInfo: tagMIMECSETINFO): HResult; stdcall;
    function IsConvertible(dwSrcEncoding: LongWord; dwDstEncoding: LongWord): HResult; stdcall;
    function ConvertString(var pdwMode: LongWord; dwSrcEncoding: LongWord; dwDstEncoding: LongWord; 
                           pSrcStr: PChar; var pcSrcSize: SYSUINT; pDstStr: PChar; 
                           var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringToUnicode(var pdwMode: LongWord; dwEncoding: LongWord; 
                                    pSrcStr: PChar; var pcSrcSize: SYSUINT;
                                    pDstStr: PWideChar; var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringFromUnicode(var pdwMode: LongWord; dwEncoding: LongWord;
                                      pSrcStr: PWideChar; var pcSrcSize: SYSUINT;
                                      pDstStr: PChar; var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringReset: HResult; stdcall;
    function GetRfc1766FromLcid(locale: LongWord; out pbstrRfc1766: WideString): HResult; stdcall;
    function GetLcidFromRfc1766(out plocale: LongWord; const bstrRfc1766: WideString): HResult; stdcall;
    function EnumRfc1766(out ppEnumRfc1766: IEnumRfc1766): HResult; stdcall;
    function GetRfc1766Info(locale: LongWord; out pRfc1766Info: tagRFC1766INFO): HResult; stdcall;
    function CreateConvertCharset(uiSrcCodePage: SYSUINT; uiDstCodePage: SYSUINT; 
                                  dwProperty: LongWord; 
                                  out ppMLangConvertCharset: IMLangConvertCharset): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMultiLanguage2
// Flags:     (0)
// GUID:      {DCCFC164-2B38-11D2-B7EC-00C04F8F5D9A}
// *********************************************************************//
  IMultiLanguage2 = interface(IUnknown)
    ['{DCCFC164-2B38-11D2-B7EC-00C04F8F5D9A}']
    function GetNumberOfCodePageInfo(out pcCodePage: SYSUINT): HResult; stdcall;
    function GetCodePageInfo(uiCodePage: SYSUINT; LangId: Word; out pCodePageInfo: tagMIMECPINFO): HResult; stdcall;
    function GetFamilyCodePage(uiCodePage: SYSUINT; out puiFamilyCodePage: SYSUINT): HResult; stdcall;
    function EnumCodePages(grfFlags: LongWord; LangId: Word; out ppEnumCodePage: IEnumCodePage): HResult; stdcall;
    function GetCharsetInfo(const Charset: WideString; out pCharsetInfo: tagMIMECSETINFO): HResult; stdcall;
    function IsConvertible(dwSrcEncoding: LongWord; dwDstEncoding: LongWord): HResult; stdcall;
    function ConvertString(var pdwMode: LongWord; dwSrcEncoding: LongWord; dwDstEncoding: LongWord; 
                           var pSrcStr: Byte; var pcSrcSize: SYSUINT; var pDstStr: Byte; 
                           var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringToUnicode(var pdwMode: LongWord; dwEncoding: LongWord; 
                                    var pSrcStr: Shortint; var pcSrcSize: SYSUINT; 
                                    var pDstStr: Smallint; var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringFromUnicode(var pdwMode: LongWord; dwEncoding: LongWord; 
                                      var pSrcStr: Smallint; var pcSrcSize: SYSUINT; 
                                      var pDstStr: Shortint; var pcDstSize: SYSUINT): HResult; stdcall;
    function ConvertStringReset: HResult; stdcall;
    function GetRfc1766FromLcid(locale: LongWord; out pbstrRfc1766: WideString): HResult; stdcall;
    function GetLcidFromRfc1766(out plocale: LongWord; const bstrRfc1766: WideString): HResult; stdcall;
    function EnumRfc1766(LangId: Word; out ppEnumRfc1766: IEnumRfc1766): HResult; stdcall;
    function GetRfc1766Info(locale: LongWord; LangId: Word; out pRfc1766Info: tagRFC1766INFO): HResult; stdcall;
    function CreateConvertCharset(uiSrcCodePage: SYSUINT; uiDstCodePage: SYSUINT; 
                                  dwProperty: LongWord; 
                                  out ppMLangConvertCharset: IMLangConvertCharset): HResult; stdcall;
    function ConvertStringInIStream(var pdwMode: LongWord; dwFlag: LongWord; 
                                    var lpFallBack: Smallint; dwSrcEncoding: LongWord; 
                                    dwDstEncoding: LongWord; const pstmIn: IStream; 
                                    const pstmOut: IStream): HResult; stdcall;
    function ConvertStringToUnicodeEx(var pdwMode: LongWord; dwEncoding: LongWord; 
                                      var pSrcStr: Shortint; var pcSrcSize: SYSUINT; 
                                      var pDstStr: Smallint; var pcDstSize: SYSUINT; 
                                      dwFlag: LongWord; var lpFallBack: Smallint): HResult; stdcall;
    function ConvertStringFromUnicodeEx(var pdwMode: LongWord; dwEncoding: LongWord; 
                                        var pSrcStr: Smallint; var pcSrcSize: SYSUINT; 
                                        var pDstStr: Shortint; var pcDstSize: SYSUINT; 
                                        dwFlag: LongWord; var lpFallBack: Smallint): HResult; stdcall;
    function DetectCodepageInIStream(dwFlag: LongWord; dwPrefWinCodePage: LongWord; 
                                     const pstmIn: IStream; var lpEncoding: tagDetectEncodingInfo; 
                                     var pnScores: SYSINT): HResult; stdcall;
    function DetectInputCodepage(dwFlag: LongWord; dwPrefWinCodePage: LongWord; 
                                 var pSrcStr: Shortint; var pcSrcSize: SYSINT; 
                                 var lpEncoding: tagDetectEncodingInfo; var pnScores: SYSINT): HResult; stdcall;
    function ValidateCodePage(uiCodePage: SYSUINT; var hwnd: _RemotableHandle): HResult; stdcall;
    function GetCodePageDescription(uiCodePage: SYSUINT; lcid: LongWord; lpWideCharStr: PWideChar; 
                                    cchWideChar: SYSINT): HResult; stdcall;
    function IsCodePageInstallable(uiCodePage: SYSUINT): HResult; stdcall;
    function SetMimeDBSource(dwSource: tagMIMECONTF): HResult; stdcall;
    function GetNumberOfScripts(out pnScripts: SYSUINT): HResult; stdcall;
    function EnumScripts(dwFlags: LongWord; LangId: Word; out ppEnumScript: IEnumScript): HResult; stdcall;
    function ValidateCodePageEx(uiCodePage: SYSUINT; var hwnd: _RemotableHandle; 
                                dwfIODControl: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISequentialStream
// Flags:     (0)
// GUID:      {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ISequentialStream = interface(IUnknown)
    ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
    function RemoteRead(out pv: Byte; cb: LongWord; out pcbRead: LongWord): HResult; stdcall;
    function RemoteWrite(var pv: Byte; cb: LongWord; out pcbWritten: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStream
// Flags:     (0)
// GUID:      {0000000C-0000-0000-C000-000000000046}
// *********************************************************************//
  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function GhostMethod_IStream_0_1: HResult; stdcall;
    function GhostMethod_IStream_4_2: HResult; stdcall;
    function GhostMethod_IStream_8_3: HResult; stdcall;
    function GhostMethod_IStream_12_4: HResult; stdcall;
    function GhostMethod_IStream_16_5: HResult; stdcall;
    function RemoteSeek(dlibMove: _LARGE_INTEGER; dwOrigin: LongWord; 
                        out plibNewPosition: _ULARGE_INTEGER): HResult; stdcall;
    function SetSize(libNewSize: _ULARGE_INTEGER): HResult; stdcall;
    function RemoteCopyTo(const pstm: IStream; cb: _ULARGE_INTEGER; out pcbRead: _ULARGE_INTEGER; 
                          out pcbWritten: _ULARGE_INTEGER): HResult; stdcall;
    function Commit(grfCommitFlags: LongWord): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function UnlockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function Stat(out pstatstg: tagSTATSTG; grfStatFlag: LongWord): HResult; stdcall;
    function Clone(out ppstm: IStream): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangCodePages
// Flags:     (0)
// GUID:      {359F3443-BD4A-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangCodePages = interface(IUnknown)
    ['{359F3443-BD4A-11D0-B188-00AA0038C969}']
    function GetCharCodePages(chSrc: Smallint; out pdwCodePages: LongWord): HResult; stdcall;
    function GetStrCodePages(var pszSrc: Smallint; cchSrc: Integer; dwPriorityCodePages: LongWord; 
                             out pdwCodePages: LongWord; out pcchCodePages: Integer): HResult; stdcall;
    function CodePageToCodePages(uCodePage: SYSUINT; out pdwCodePages: LongWord): HResult; stdcall;
    function CodePagesToCodePage(dwCodePages: LongWord; uDefaultCodePage: SYSUINT; 
                                 out puCodePage: SYSUINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangFontLink
// Flags:     (0)
// GUID:      {359F3441-BD4A-11D0-B188-00AA0038C969}
// *********************************************************************//
  IMLangFontLink = interface(IMLangCodePages)
    ['{359F3441-BD4A-11D0-B188-00AA0038C969}']
    function GetFontCodePages(var hDC: _RemotableHandle; var hFont: _RemotableHandle; 
                              out pdwCodePages: LongWord): HResult; stdcall;
    function MapFont(var hDC: _RemotableHandle; dwCodePages: LongWord; 
                     var hSrcFont: _RemotableHandle; out phDestFont: wireHFONT): HResult; stdcall;
    function ReleaseFont(var hFont: _RemotableHandle): HResult; stdcall;
    function ResetFontMapping: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMLangFontLink2
// Flags:     (0)
// GUID:      {DCCFC162-2B38-11D2-B7EC-00C04F8F5D9A}
// *********************************************************************//
  IMLangFontLink2 = interface(IMLangCodePages)
    ['{DCCFC162-2B38-11D2-B7EC-00C04F8F5D9A}']
    function GetFontCodePages(var hDC: _RemotableHandle; var hFont: _RemotableHandle; 
                              out pdwCodePages: LongWord): HResult; stdcall;
    function ReleaseFont(var hFont: _RemotableHandle): HResult; stdcall;
    function ResetFontMapping: HResult; stdcall;
    function MapFont(var hDC: _RemotableHandle; dwCodePages: LongWord; chSrc: Smallint; 
                     out pFont: wireHFONT): HResult; stdcall;
    function GetFontUnicodeRanges(var hDC: _RemotableHandle; var puiRanges: SYSUINT; 
                                  out pUranges: tagUNICODERANGE): HResult; stdcall;
    function GetScriptFontInfo(sid: Byte; dwFlags: LongWord; var puiFonts: SYSUINT; 
                               out pScriptFont: tagSCRIPFONTINFO): HResult; stdcall;
    function CodePageToScriptID(uiCodePage: SYSUINT; out pSid: Byte): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMultiLanguage3
// Flags:     (0)
// GUID:      {4E5868AB-B157-4623-9ACC-6A1D9CAEBE04}
// *********************************************************************//
  IMultiLanguage3 = interface(IMultiLanguage2)
    ['{4E5868AB-B157-4623-9ACC-6A1D9CAEBE04}']
    function DetectOutboundCodePage(dwFlags: LongWord; lpWideCharStr: PWideChar; 
                                    cchWideChar: SYSUINT; var puiPreferredCodePages: SYSUINT; 
                                    nPreferredCodePages: SYSUINT; 
                                    var puiDetectedCodePages: SYSUINT; 
                                    var pnDetectedCodePages: SYSUINT; var lpSpecialChar: Smallint): HResult; stdcall;
    function DetectOutboundCodePageInIStream(dwFlags: LongWord; const pStrIn: IStream; 
                                             var puiPreferredCodePages: SYSUINT; 
                                             nPreferredCodePages: SYSUINT; 
                                             var puiDetectedCodePages: SYSUINT; 
                                             var pnDetectedCodePages: SYSUINT; 
                                             var lpSpecialChar: Smallint): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoCMLangString provides a Create and CreateRemote method to          
// create instances of the default interface IMLangString exposed by              
// the CoClass CMLangString. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCMLangString = class
    class function Create: IMLangString;
    class function CreateRemote(const MachineName: string): IMLangString;
  end;

// *********************************************************************//
// The Class CoCMLangConvertCharset provides a Create and CreateRemote method to          
// create instances of the default interface IMLangConvertCharset exposed by              
// the CoClass CMLangConvertCharset. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCMLangConvertCharset = class
    class function Create: IMLangConvertCharset;
    class function CreateRemote(const MachineName: string): IMLangConvertCharset;
  end;

// *********************************************************************//
// The Class CoCMultiLanguage provides a Create and CreateRemote method to          
// create instances of the default interface IMultiLanguage exposed by              
// the CoClass CMultiLanguage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCMultiLanguage = class
    class function Create: IMultiLanguage;
    class function CreateRemote(const MachineName: string): IMultiLanguage;
  end;

implementation

uses
  ComObj;

class function CoCMLangString.Create: IMLangString;
begin
  Result := CreateComObject(CLASS_CMLangString) as IMLangString;
end;

class function CoCMLangString.CreateRemote(const MachineName: string): IMLangString;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CMLangString) as IMLangString;
end;

class function CoCMLangConvertCharset.Create: IMLangConvertCharset;
begin
  Result := CreateComObject(CLASS_CMLangConvertCharset) as IMLangConvertCharset;
end;

class function CoCMLangConvertCharset.CreateRemote(const MachineName: string): IMLangConvertCharset;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CMLangConvertCharset) as IMLangConvertCharset;
end;

class function CoCMultiLanguage.Create: IMultiLanguage;
begin
  Result := CreateComObject(CLASS_CMultiLanguage) as IMultiLanguage;
end;

class function CoCMultiLanguage.CreateRemote(const MachineName: string): IMultiLanguage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CMultiLanguage) as IMultiLanguage;
end;

end.
