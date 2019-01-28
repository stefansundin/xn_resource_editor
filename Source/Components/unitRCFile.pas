{*======================================================================*
 | unitRCFile                                                           |
 |                                                                      |
 | .RC file module handler                                              |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2004  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      24/11/2004  CPWW  Original                                  |
 *======================================================================*}

unit unitRCFile;

interface

uses
  Windows, Classes, SysUtils, Menus,Graphics, ConTnrs, unitResFile,
  unitResourceDetails, DialogConsts;

type
  //---------------------------------------------------------------------------
  // RC Module
  TRCModule = class (TResourceList)
  private
    fFileName : string;
    fIncludePath: string;

  public
    constructor Create;

    procedure SaveToStream (stream : TStream); override;
    procedure LoadFromFile (const FileName : string); override;
    procedure LoadFromStream (stream : TStream); override;

    property IncludePath : string read fIncludePath write fIncludePath;
  end;

implementation

uses
  unitStreamTextReader, unitSearchString, unitParser, unitResourceGraphics,
  unitCExpression, unitResourceToolbar, unitResourceDialogs,
  unitResourceMessages, unitResourceAccelerator, unitResourceVersionInfo,
  unitResourceMenus;

const
  BS_PUSHBOX = 10;      // Missing from windows.pas.  Obsolete?

//---------------------------------------------------------------
// Keyword IDs

  kwLanguage        = 1;
  kwIcon            = 2;
  kwDialog          = 3;
  kwMenu            = 4;
  kwBitmap          = 5;
  kwCursor          = 6;
  kwToolbar         = 7;
  kwCaption         = 9;
  kwCharacteristics = 10;
  kwClass           = 11;
  kwExStyle         = 12;
  kwFont            = 13;
  kwStyle           = 14;
  kwVersion         = 15;
  kwDialogEx        = 16;
  kwMessageTable    = 17;
  kwAccelerators    = 18;
  kwStringTable     = 19;
  kwVersionInfo     = 20;

  kwBegin           = 50;
  kwEnd             = 51;

  kwDiscardable     = 100;
  kwPure            = 101;
  kwMoveable        = 102;
  kwPreload         = 103;
  kwLoadOnCall      = 104;

  kwVirtkey         = 110;
  kwAscii           = 111;
  kwNoInvert        = 112;
  kwAlt             = 113;
  kwShift           = 114;

  kwPopup           = 120;
  kwMenuItem        = 121;

  kwButton          = 150;
  kwSeparator       = 151;
  kwLText           = 152;
  kwDefPushButton   = 153;
  kwControl         = 154;
  kwEditText        = 155;
  kwPushButton      = 156;
  kwAuto3State      = 157;
  kwAutoCheckBox    = 158;
  kwAutoRadioButton = 159;
  kwCheckBox        = 160;
  kwState3          = 161;
  kwRadioButton     = 162;
  kwCText           = 163;
  kwRText           = 164;
  kwGroupBox        = 165;
  kwPushBox         = 166;
  kwListBox         = 167;
  kwComboBox        = 168;

type
  kwKeywordRange    = 0..255;
  TSupportedKeywords = set of kwKeywordRange;

const
//---------------------------------------------------------------------------
// Keywords supported for each resource type
  DialogOptionKeywords       : TSupportedKeywords = [kwCaption, kwCharacteristics, kwClass, kwExStyle, kwFont, kwLanguage, kwMenu, kwStyle, kwVersion];
  AcceleratorsOptionKeywords : TSupportedKeywords = [kwCharacteristics, kwLanguage, kwVersion];
  MenuOptionKeywords         : TSupportedKeywords = [kwCharacteristics, kwLanguage, kwVersion];
  RCDataOptionKeywords       : TSupportedKeywords = [kwCharacteristics, kwLanguage, kwVersion];
  StringtableOptionKeywords  : TSupportedKeywords = [kwCharacteristics, kwLanguage, kwVersion];

type
  TKeywordProc = procedure of object;

//----------------------------------------------------------------------------
  TKeywordDetails = class       // Per keyword details - used to populate fKeywords object list
    kw : Integer;               // The Keyword ID
    proc : TKeywordProc;        // Handler method
    constructor Create (AKeyword : Integer; AProc : TKeywordProc);
  end;

  TRCParser = class;

  TResourceOptions = class      // Options - populated by CreateResourceOptions
  private
    fParser : TRCParser;
    fSupportedKeywords : TSupportedKeywords;
    fCharacteristics: DWORD;
    fCaption: string;
    fClass: TSzOrID;
    fFontSize: DWORD;
    fExStyle: DWORD;
    fStyle: DWORD;
    fVersion: DWORD;
    fLanguage: Integer;
    fFontFace: string;
    fMenuId: TSzOrID;
    fFontWeight : Integer;
    fFontItalic : Integer;
    fFontCharset : Integer;
  public
    constructor Create (Parser : TRCParser; SupportedKeywords : TSupportedKeywords);
    property Caption : string read fCaption;
    property Characteristics : DWORD read fCharacteristics;
    property _Class : TSzOrID read fClass;
    property ExStyle : DWORD read fExStyle;
    property FontSize : DWORD read fFontSize;
    property FontFace : string read fFontFace;
    property Language : Integer read fLanguage;
    property MenuId : TSzOrID read fMenuId;
    property Style : DWORD read fStyle;
    property Version : DWORD read fVersion;
  end;

  TControlParamsOption = (cpHasText, cpNeedsCXCY, cpNeedsStyle);
  TControlParamsOptions = set of TControlParamsOption;

//------------------------------------------------------------------------
// Main parser class.  Inherits from TCPreProcessor - which handles all
// '#' directives
  TRCParser = class (TCPreProcessor)
  private
    fName : TValue;
    fEOF : boolean;

    fLangId : Integer;

    fParent : TResourceModule;
    fKeywords : TStringList;

    procedure DoLanguage;
    procedure DoIcon;
    procedure DoCursor;
    procedure DoDialog;
    procedure DoDialogEx;
    procedure DoMenu;
    procedure DoBitmap;
    procedure DoToolbar;
    procedure DoMessageTable;
    procedure DoAccelerators;
    procedure DoVersionInfo;
    procedure DoStringTable;
    procedure SkipYeOldeMemoryAttributes;
    function Keyword (const st : string) : TKeywordDetails;
    function KeyID : Integer;    // Returns keyword ID of current token - or -1
    function CreateResourceOptions (SupportedKeywords : TSupportedKeywords; var validKeywords : TSupportedKeywords) : TResourceOptions;
    function NextExpression (var v : TValue) : boolean;
    function NextIntegerExpression : Integer;

    function GetControlParams (ex : boolean; var text : TSzOrID; var id : DWORD; var x, y, cx, cy : Integer; var style, exStyle, helpId : DWORD; options : TControlParamsOptions) : boolean;
  public
    constructor Create (stream : TStream; AParent : TResourceModule);
    destructor Destroy; override;
    procedure Parse; override;
  end;

type
  TkwRec = record
    kw : string;
    id : Integer;
    pr : pointer;
    resID : PChar;
  end;

const
  NoKeywords = 52;

var
  KeywordTable : array [0..NoKeywords - 1] of TkwRec = (

  // Resource types

    (kw : 'LANGUAGE';        id : kwLanguage;        pr : @TRCParser.DoLanguage),
    (kw : 'ICON';            id : kwIcon;            pr : @TRCParser.DoIcon;    resId : RT_GROUP_ICON) ,
    (kw : 'DIALOG';          id : kwDialog;          pr : @TRCParser.DoDialog;  resId : RT_DIALOG),
    (kw : 'MENU';            id : kwMenu;            pr : @TRCParser.DoMenu;    resId : RT_MENU),
    (kw : 'BITMAP';          id : kwBitmap;          pr : @TRCParser.DoBitmap;  resId : RT_BITMAP),
    (kw : 'CURSOR';          id : kwCursor;          pr : @TRCParser.DoCursor;  resId : RT_GROUP_CURSOR),
    (kw : 'TOOLBAR';         id : kwToolbar;         pr : @TRCParser.DoToolbar; resId : RT_TOOLBAR),
    (kw : 'DIALOGEX';        id : kwDialogEx;        pr : @TRCParser.DoDialogEx),
    (kw : 'MESSAGETABLE';    id : kwMessageTable;    pr : @TRCParser.DoMessageTable; resId : RT_MESSAGETABLE),
    (kw : 'ACCELERATORS';    id : kwAccelerators;    pr : @TRCParser.DoAccelerators; resId : RT_ACCELERATOR),
    (kw : 'STRINGTABLE';     id : kwStringTable;     pr : @TRCParser.DoStringTable;  resId : RT_STRING),
    (kw : 'VERSIONINFO';     id : kwVersionInfo;     pr : @TRCParser.DoVersionInfo;  resId : RT_VERSION),


  // Resource options
    (kw : 'CAPTION';         id : kwCaption;         pr : Nil),
    (kw : 'CHARACTERISTICS'; id : kwCharacteristics; pr : nil),
    (kw : 'CLASS';           id : kwClass;           pr : Nil),
    (kw : 'EXSTYLE';         id : kwExStyle;         pr : Nil),
    (kw : 'FONT';            id : kwFont;            pr : Nil),
    (kw : 'STYLE';           id : kwStyle;           pr : Nil),
    (kw : 'VERSION';         id : kwVersion;         pr : Nil),

    // Obsolete resource memory modifiers
    (kw : 'DISCARDABLE';     id : kwDiscardable;     pr : Nil),
    (kw : 'LOADONCALL';      id : kwLoadOnCall;      pr : Nil),
    (kw : 'MOVEABLE';        id : kwMoveable;        pr : Nil),
    (kw : 'PRELOAD';         id : kwPreload;         pr : Nil),
    (kw : 'PURE';            id : kwPure;            pr : Nil),

    // Accelerator keywords
    (kw : 'VIRTKEY';         id : kwVirtkey;         pr : Nil),
    (kw : 'ASCII';           id : kwAscii;           pr : Nil),
    (kw : 'NOINVERT';        id : kwNoInvert;        pr : Nil),
    (kw : 'ALT';             id : kwAlt;             pr : Nil),
    (kw : 'SHIFT';           id : kwShift;           pr : Nil),
//  nb.  The CONTROL keyword is used for accelerators and also
//       custom controls.  It is defined below

    (kw : 'POPUP';           id : kwPopup;           pr : Nil),
    (kw : 'MENUITEM';        id : kwMenuItem;        pr : Nil),

    (kw : 'BEGIN';           id : kwBegin;           pr : Nil),
    (kw : 'END';             id : kwEnd;             pr : Nil),

// Control identifiers
    (kw : 'BUTTON';          id : kwButton;          pr : Nil),
    (kw : 'SEPARATOR';       id : kwSeparator;       pr : Nil),
    (kw : 'LTEXT';           id : kwLText;           pr : Nil),
    (kw : 'DEFPUSHBUTTON';   id : kwDefPushButton;   pr : Nil),
    (kw : 'CONTROL';         id : kwControl;         pr : Nil),
    (kw : 'EDITTEXT';        id : kwEditText;        pr : Nil),
    (kw : 'PUSHBUTTON';      id : kwPushButton;      pr : Nil),

    (kw : 'AUTO3STATE';      id : kwAuto3State;      pr : Nil),
    (kw : 'AUTOCHECKBOX';    id : kwAutoCheckBox;    pr : Nil),
    (kw : 'AUTORADIOBUTTON'; id : kwAutoRadioButton; pr : Nil),
    (kw : 'CHECKBOX';        id : kwCheckBox;        pr : Nil),
    (kw : 'STATE3';          id : kwState3;          pr : Nil),
    (kw : 'RADIOBUTTON';     id : kwRadioButton;     pr : Nil),
    (kw : 'CTEXT';           id : kwCText;           pr : Nil),
    (kw : 'RTEXT';           id : kwRText;           pr : Nil),
    (kw : 'GROUPBOX';        id : kwGroupBox;        pr : Nil),
    (kw : 'PUSHBOX';         id : kwPushBox;         pr : Nil),
    (kw : 'LISTBOX';         id : kwListBox;         pr : Nil),
    (kw : 'COMBOBOX';        id : kwComboBox;        pr : Nil)
  );

{*----------------------------------------------------------------------*
 | function MakeLangId                                                  |
 |                                                                      |
 | Returns a LANGID from primary and sub language constituents          |
 *----------------------------------------------------------------------*}
function MakeLangId (pri, sec : word) : Integer;
begin
  if (pri > 0) and (sec = 0) then
    sec := 1;
  result := (pri and $3ff) or (sec shl 10)
end;

{*----------------------------------------------------------------------*
 | function ValToStr                                                    |
 |                                                                      |
 | Returns a string representation of a value returned by the C         |
 | pre-processor's expressions evaluator.                               |
 *----------------------------------------------------------------------*}
function ValToStr (val : TValue) : string;
begin
  if val.tp = vInteger then
    result := IntToStr (val.iVal)
  else
    if val.tp = vString then
      result := val.sVal
    else
      raise EParser.Create('Integer or string expected');
end;

{*----------------------------------------------------------------------*
 | function ValToSzOrID                                                 |
 |                                                                      |
 | Returns a resource SzOrID identifier from a value returned by the    |
 | C evaluator                                                          |
 *----------------------------------------------------------------------*}
function ValToSzOrID (val : TValue) : TSzOrId;
begin
  if val.tp = vInteger then
  begin
    result.isID := True;
    result.id := val.iVal
  end
  else
    if val.tp = vString then
    begin
      result.isID := False;
      result.sz := val.sVal
    end
    else
      raise EParser.Create('Type mismatch');
end;

{ TRCModule }

{*----------------------------------------------------------------------*
 | procedure TRCModule.LoadFromFile                                     |
 |                                                                      |
 | Load and parse an RC file.  Overridden from the base class to save   |
 | the passed in path for use in relative #includes etc. (see           |
 | LoadFromStream)                                                      |
 *----------------------------------------------------------------------*}
constructor TRCModule.Create;
begin
  inherited;
  fIncludePath := GetEnvironmentVariable ('include');
end;

procedure TRCModule.LoadFromFile(const FileName: string);
begin
  fFileName := FileName;
  inherited;
end;

{*----------------------------------------------------------------------*
 | procedure TRCModule.LoadFromStream                                   |
 |                                                                      |
 | Load and parse the data from a stream.                               |
 |                                                                      |
 | Parameters:                                                          |
 |   stream: TStream                    The stream to parse             |
 *----------------------------------------------------------------------*}
procedure TRCModule.LoadFromStream(stream: TStream);
var
  parser : TRCParser;
begin
  parser := TRCParser.Create(stream, self);
  parser.PathName := ExtractFilePath (fFileName);

  // Add identifiers so that MSVC resource script load correctly
  parser.AddIdentifier('RC_INVOKED', '');
  parser.AddIdentifier('_WIN32', '');

  parser.IncludePath := IncludePath;
  parser.Parse;
  SortResources;
  ClearDirty
end;

{*----------------------------------------------------------------------*
 | TRCModule.SaveToStream                                               |
 |                                                                      |
 | Save RC data.  Not yet implemented                                   |
 *----------------------------------------------------------------------*}
procedure TRCModule.SaveToStream(stream: TStream);
begin
  inherited;            // Inherited function throws an exception
end;

{ TRCParser }
{*----------------------------------------------------------------------*
 | constructor TRCParser.Create                                         |
 |                                                                      |
 | Create a TRCParser                                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   stream: TStream                    The stream to parse             |
 |   AParent: TResourceModule           Parent module.                  |
 *----------------------------------------------------------------------*}
constructor TRCParser.Create(stream: TStream; AParent: TResourceModule);
type
  t = record case boolean of
    true  : (p : TKeywordProc);
    false : (pt, slf : Pointer);
  end;
var
  i : Integer;
  tt : t;

begin
  fParent := AParent;
  fLangId := SysLocale.DefaultLCID;  // Check - maybe should be US LCID
  inherited Create (stream);

  fKeywords := TStringList.Create;
  fKeywords.CaseSensitive := false;
                                        // Populate the fKeywords object list
                                        // from the KeywordTable
  for i := 0 to NoKeywords - 1 do
    with KeywordTable [i] do
    begin
      tt.pt := pr;
      tt.slf := self;                   // Fixup the 'self' pointer in the
                                        // methood call
      fKeywords.AddObject(kw, TKeywordDetails.Create(id, tt.p));
    end;

  fKeywords.Sorted := True;
end;

{*----------------------------------------------------------------------*
 | function TRCParser.CreateResourceOptions                             |
 |                                                                      |
 | Create a TResourceOptions class, and populate it from the resource   |
 | options for the particular resource type.  Return the options        |
 | keywords actually found, so that the rest of the supported keywords  |
 | can be filled with default values.                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   SupportedKeywords: TSupportedKeywords      Keywords supported by   |
 |                                              the resource type       |
 |                                                                      |
 |   var validKeywords : TSupportedKeywords     Keywords actually found |
 |                                              in the RC stream        |
 |                                                                      |
 | The function creates and returns the TResourceOptions class.         |
 *----------------------------------------------------------------------*}
function TRCParser.CreateResourceOptions(
  SupportedKeywords: TSupportedKeywords; var validKeywords : TSupportedKeywords): TResourceOptions;
var
  kw : Integer;
  v : TValue;
  pri, sec : word;
  hasToken : boolean;
begin
  validKeywords := [];
  if KeyID in SupportedKeywords then
  begin
    result := TResourceOptions.Create(self, SupportedKeywords);
    result.fLanguage := fLangId;
    validKeywords := [kwLanguage];

    repeat
      kw := KeyID;
                        // Finish when we find a keyword that's not
                        // in the supported list.  If everything is going OK
                        // this will be the first token that makes up the
                        // actual resource data.

      if not (kw in SupportedKeywords) then break;

      hasToken := False;
      case kw of
        kwCaption : begin       // Parse a CAPTION statement followed by
                                // "captiontext" in double quotes.
                      result.fCaption := NextString;
                      Include (validKeywords, kwCaption)
                    end;

        kwCharacteristics :     // Parse a CHARACTERISTICS statement followed
                                // by a DWORD value.  Which is irrelevant
                    begin
                      result.fCharacteristics := NextInteger;
                      Include (validKeywords, kwCharacteristics)
                    end;

        kwStyle   : begin       // Parse a STYLE statement followed by a DWORD
                                // expression containing a window style

                      NextExpression (v);
                      hasToken := True;
                      if v.tp = vInteger then
                        result.fStyle := v.iVal
                      else
                        raise Exception.Create ('Integer expected in STYLE');
                      Include (validKeywords, kwStyle);
                    end;

        kwExStyle : begin       // Parse an EXSTYLE statement followed by a DWORD
                                // expressions containing a window Ex Style

                      NextExpression (v);
                      hasToken := True;
                      if v.tp = vInteger then
                        result.fExStyle := v.iVal
                      else
                        raise Exception.Create ('Integer expected in EXSTYLE');
                      Include (validKeywords, kwExStyle);
                    end;

        kwFont    : begin       // Parse a FONT statement followed by the
                                // fontsize DWORD and string font name.  If this
                                // occurs in an EXDIALOG, this is followed by the
                                // Weight, Italic and CharSet values
                      result.fFontSize := NextInteger;
                      NextChar (',');
                      result.fFontFace := NextString;
                      GetToken;
                      hasToken := True;
                      if (TokenType = ttChar) and (TokenChar = ',') then
                      begin
                        result.fFontWeight := NextIntegerExpression;
                        ExpectChar (',');
                        result.fFontItalic := NextIntegerExpression;
                        ExpectChar (',');
                        result.fFontCharset := NextIntegerExpression;
                      end;
                      Include (validKeywords, kwFont);
                    end;

        kwLanguage :begin       // Parse a LANGUAGE statement followed by the
                                // primary and sub language components
                      pri := NextInteger;
                      NextChar (',');
                      sec := NextInteger;
                      result.fLanguage := MakeLangID (pri, sec);
                    end;

        kwMenu     : begin      // Parse a MENU stetement.  Not to be confused
                                // with a menu resource type identifier - this
                                // option may appear in DIALOG and DIALOGEX resources.
                                // The keyword 'MENU' is followed by the menu SZ or ID

                       GetToken;
                       result.fMenuId := ValToSzOrID (ResolveToken);
                       Include (validKeywords, kwMenu);
                     end;

        kwClass    : begin      // Parse a CLASS statement followed by an Sz or ID
                                // window (dialog) class identifier.
                       GetToken;
                       result.fClass := ValToSzOrID (ResolveToken);
                       Include (validKeywords, kwClass);
                     end;

        kwVersion  : begin      // VERSION statement followed by an ignored
                                // version DWORD.  Not to be confused with a
                                // VERSIONINFO resource type
                       result.fVersion := NextInteger;
                       Include (validKeywords, kwVersion)
                     end
      end;
      if not hasToken then
        fEOF := not GetToken;
    until fEOF
  end
  else
    result := Nil;
end;

{*----------------------------------------------------------------------*
 | Destructor for TRCParser                                             |
 *----------------------------------------------------------------------*}
destructor TRCParser.Destroy;
var
  i : Integer;
begin
  for i := fKeywords.Count - 1 downto 0 do
    fKeywords.Objects [i].Free;
  fKeywords.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoAccelerators                                   |
 |                                                                      |
 | Handle an ACCELERATORS resource.                                     |
 |                                                                      |
 | ACCELERATORS:: acctablename ACCELERATORS [optional-statements]       |
 |                             { event, idvalue, | [type] [options]...} |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoAccelerators;
var
  ac : TAcceleratorResourceDetails;
  v : TValue;
  vk : Integer;
  flags : Integer;
  l, id : Integer;
  options : TResourceOptions;
  validKeywords : TSupportedKeywords;

begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  options := CreateResourceOptions (AcceleratorsOptionKeywords, validKeywords);
  try
   if Assigned(options) then
      ac := TAcceleratorResourceDetails.CreateNew(fParent, options.Language, ValToStr (fName))
    else
      ac := TAcceleratorResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));

    if KeyID = kwBegin then
    while NextExpression (v) do // First expression contains the virtual key
    begin
      if KeyID = kwEnd then
        break;
      
      vk := 0;
      if vk <> 0 then;          // Get's rid of erroneous compiler warning
      if v.tp = vString then
      begin
        l := Length (v.sVal);
        if (l = 2) and (v.sVal [1] = '^') then
          vk := Ord (v.sVal [2]) and $1f
        else
          if l = 1 then
            vk := Ord (v.sVal [1])
          else
            if vk = 0 then
              raise EParser.Create('Single character expected')
      end
      else
        if v.tp = vInteger then
          vk := v.iVal
        else
          raise EParser.Create('Character or virtual key code expected');

                                // Next identifier contains the 'ID'
      id := NextIntegerExpression;

      flags := 0;

                                // Now get the (optional) flags.  Technically
                                // VIRTKEY or ASCII should be the first flag if
                                // it's specified.  But no real need to enforce
                                // this.
      while not SOL do
      begin
        GetToken;

        case KeyID of
          kwVirtKey  : flags := flags or FVIRTKEY;
          kwNoInvert : flags := flags or FNOINVERT;
          kwShift    : flags := flags or FSHIFT;
          kwAlt      : flags := flags or FALT;
          kwControl  : flags := flags or FCONTROL;
        end;
      end;

      ac.Add(flags, vk, id);
    end;
    if KeyID <> kwEnd then
      raise EParser.Create('End expected');
    GetToken;
  finally
    options.Free
  end
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoBitmap                                         |
 |                                                                      |
 | Handle a BITMAP resource                                             |
 |                                                                      |
 | BITMAP:: nameID BITMAP filename                                      |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoBitmap;
var
  fFileName : string;
  bmp : TBitmapResourceDetails;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  fFileName := ExpectString ('File name expected');

  bmp := TBitmapResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));
  bmp.LoadImage(PathName + fFileName);
  GetToken;
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoCursor                                         |
 |                                                                      |
 | Handle a CURSOR resource                                             |
 |                                                                      |
 | CURSOR:: nameID CURSOR filename                                      |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoCursor;
var
  fFileName : string;
  cur : TIconCursorGroupResourceDetails;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  fFileName := ExpectString ('File name expected');
  cur := TCursorGroupResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));
  cur.LoadImage(PathName + fFileName);
  GetToken;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoDialog                                         |
 |                                                                      |
 | Handle a DIALOG resource                                             |
 |                                                                      |
 | DIALOG:: namemeID DIALOG x, y, width, height  [optional-statements]  |
 |                             {      control-statement      . . .  }   |
 |                                                                      |
 | control-statement::CONTROLID [,options ...]                          |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoDialog;
var
  x, y, cx, cy, ctlCount : Integer;
  id, style, exstyle, fontSize, dummy : DWORD;
  options : TResourceOptions;
  dlg : TDialogResourceDetails;
  validKeywords : TSupportedKeywords;
  p : PDlgTemplate;
  menu, cls, title : TSzOrID;
  faceName : string;
  v : TValue;
  controlOptions : TControlParamsOptions;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  // Get dialog coordinates
  x := ExpectInteger; NextChar (',');
  y := NextInteger;   NextChar (',');
  cx := NextInteger;  NextChar (',');
  cy := NextInteger;  GetToken;

  // Get dialog options - eg. caption.
  options := CreateResourceOptions (DialogOptionKeywords, validKeywords);
  try

  // Create the dialog resource
    if Assigned(options) then
      dlg := TDialogResourceDetails.CreateNew(fParent, options.fLanguage, ValToStr (fName))
    else
      dlg := TDialogResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));

    // Initialize a blank resource template
    p := PDlgTemplate (dlg.Data.Memory);
    style := p^.style;
    exstyle := p^.dwExtendedStyle;
    ctlCount := 0;
    menu.isID  := False; menu.sz := '';
    cls.isID   := False; cls.sz := '';
    fontSize := 8;
    title.isID := False; title.sz := '';
    faceName := 'MS Shell Dlg';

    // Fill in the resource template options.
    if Assigned(options) then
    begin
      if kwStyle in validKeywords then style := options.Style;
      if kwExStyle in validKeywords then exstyle := options.ExStyle;
      if kwCaption in validKeywords then title := StringToSzOrID (options.fCaption);
      if kwClass   in validKeywords then cls := options.fClass;
      if kwMenu    in validKeywords then menu := options.fMenuId;
      if kwFont    in validKeywords then
      begin
        fontSize := options.fFontSize;
        faceName := options.fFontFace;
      end
    end;

    // Get the dialog controls
    dlg.BeginInit(x, y, cx, cy, style, exstyle, menu, cls, title, fontSize, faceName);
    try
      if KeyID = kwBegin then
      begin
        NextIdentifier;
        repeat
          exStyle := 0;
          if TokenType = ttIdentifier then
          case KeyID of
            kwLText,
            kwCText,
            kwRText,
            kwIcon:
              begin             // Handle Static Control
                cls.isID := True;
                cls.id := STATIC_ID;
                style := WS_CHILD or WS_VISIBLE or WS_GROUP;

                case KeyID of
                  kwLText : style := style or SS_LEFT;
                  kwCText : style := style or SS_CENTER;
                  kwRText : style := style or SS_RIGHT;
                  kwIcon  : begin
                              style := style or SS_ICON;
                              controlOptions := [cpHasText];
                              cx := GetSystemMetrics (SM_CXICON);
                              cy := GetSystemMetrics (SM_CYICON)
                            end
                end;

                if KeyID <> kwICon then
                  controlOptions := [cpHasText, cpNeedsCXCY];

                GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, controlOptions);
                dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                Inc (CtlCount)
              end;

            kwListBox :         // Handle ListBox control
              begin
                cls.isID := True;
                cls.id := LISTBOX_ID;
                style := WS_CHILD or WS_VISIBLE or WS_BORDER or LBS_NOTIFY;
                GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpNeedsCXCY]);
                dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                Inc (CtlCount)
              end;

            kwComboBox :        // Handle ComboBox control
              begin
                cls.isID := True;
                cls.id := COMBOBOX_ID;
                style := WS_CHILD or WS_VISIBLE or CBS_SIMPLE or WS_TABSTOP;
                GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpNeedsCXCY]);
                dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                Inc (CtlCount)
              end;

              kwPushButton,     // Handle Button-type control
              kwDefPushButton,
              kwAuto3State,
              kwAutoCheckBox,
              kwAutoRadioButton,
              kwCheckbox,
              kwRadioButton,
              kwState3,
              kwGroupBox,
              kwPushBox:
                begin
                  cls.isID := True;
                  cls.id := BUTTON_ID;
                  style := WS_CHILD or WS_VISIBLE or WS_TABSTOP;

                  case KeyID of
                    kwPushButton      : style := style or BS_PUSHBUTTON;
                    kwDefPushButton   : style := style or BS_DEFPUSHBUTTON;
                    kwAuto3State      : style := style or BS_AUTO3STATE;
                    kwAutoCheckBox    : style := style or BS_AUTOCHECKBOX;
                    kwAutoRadioButton : style := style or BS_AUTORADIOBUTTON;
                    kwCheckBox        : style := style or BS_CHECKBOX;
                    kwRadioButton     : style := style or BS_RADIOBUTTON;
                    kwState3          : style := style or BS_3STATE;
                    kwGroupBox        : style := (style and (not WS_TABSTOP)) or BS_GROUPBOX;
                    kwPushBox         : style := style or BS_PUSHBOX;
                  end;

                  GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpHasText, cpNeedsCXCY]);
                  dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                  Inc (CtlCount)
                end;

              kwEditText :              // Handle Edit control
                begin
                  cls.isID := True;
                  cls.id := EDIT_ID;
                  style := WS_CHILD or WS_VISIBLE or ES_LEFT or WS_BORDER or WS_TABSTOP;
                  GetControlParams (false, title, id, x, y, cx, cy, style, exstyle, dummy, [cpNeedsCXCY]);
                  dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                  Inc (CtlCount)
                end;

              kwControl :               // Handle custom control
                begin
                  NextExpression (v);
                  title := ValToSZOrId (v);
                  ExpectChar (',');
                  id := NextIntegerExpression;
                  ExpectChar (',');
                  NextExpression (v);
                  cls := ValToSZOrId (v);
                  ExpectChar (',');
                  style := WS_CHILD or WS_VISIBLE;
                  style := style or DWORD (NextIntegerExpression);
                  ExpectChar (',');
                  x := NextIntegerExpression;
                  ExpectChar (',');
                  y := NextIntegerExpression;
                  ExpectChar (',');
                  cx := NextIntegerExpression;
                  ExpectChar (',');
                  cy := NextIntegerExpression;
                  if (TokenType = ttChar) and (TokenChar = ',') then
                    exStyle := NextIntegerExpression;
                  dlg.InitAddControl(cls, id, title, x, y, cx, cy, style, exstyle, 0, id);
                  Inc (CtlCount)
                end
              else
                GetToken;
          end
          else
            GetToken;
        until KeyID = kwEnd
      end
    finally
      dlg.EndInit (CtlCount)
    end
  finally
    options.Free
  end;

  GetToken;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoDialog                                         |
 |                                                                      |
 | Handle a DIALOG resource                                             |
 |                                                                      |
 | DIALOG:: namemeID DIALOG x, y, width, height  [optional-statements]  |
 |                             {      control-statement      . . .  }   |
 |                                                                      |
 | control-statement::CONTROLID [,options ...]                          |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoDialogEx;
var
  x, y, cx, cy, ctlCount : Integer;
  id, style, exstyle, fontSize, helpId, fontWeight, fontItalic, fontCharset : DWORD;
  options : TResourceOptions;
  dlg : TDialogResourceDetails;
  validKeywords : TSupportedKeywords;
  p : PDlgTemplate;
  menu, cls, title : TSzOrID;
  faceName : string;
  v : TValue;
  controlOptions : TControlParamsOptions;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  x := ExpectInteger; NextChar (',');
  y := NextInteger;   NextChar (',');
  cx := NextInteger;  NextChar (',');
  cy := NextInteger;  GetToken;
  helpId := 0;

  if (TokenType = ttChar) and (TokenChar = ',') then
  begin
    helpId := NextInteger;
    GetToken
  end;

  options := CreateResourceOptions (DialogOptionKeywords, validKeywords);
  try
    if Assigned(options) then
    begin
      dlg := TDialogResourceDetails.CreateNew(fParent, options.fLanguage, ValToStr (fName));
      if kwCaption in validKeywords then
    end
    else
      dlg := TDialogResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));

    p := PDlgTemplate (dlg.Data.Memory);
    style := p^.style;
    exstyle := p^.dwExtendedStyle;
    ctlCount := 0;
    menu.isID  := False; menu.sz := '';
    cls.isID   := False; cls.sz := '';
    title.isID := False; title.sz := '';
    fontSize := 8;
    faceName := 'MS Shell Dlg';
    fontWeight := 0;
    fontItalic := 0;
    fontCharset := DEFAULT_CHARSET;

    if Assigned(options) then
    begin
      if kwStyle in validKeywords then style := options.Style;
      if kwExStyle in validKeywords then exstyle := options.ExStyle;
      if kwCaption in validKeywords then title := StringToSzOrID (options.fCaption);
      if kwClass   in validKeywords then cls := options.fClass;
      if kwMenu    in validKeywords then menu := options.fMenuId;
      if kwFont    in validKeywords then
      begin
        fontSize := options.fFontSize;
        faceName := options.fFontFace;
        fontWeight := options.fFontWeight;
        fontItalic := options.fFontItalic;
        fontCharset := options.fFontCharset
      end
    end;

    dlg.BeginInitEx(x, y, cx, cy, style, exstyle, helpId, menu, cls, title, fontSize, fontWeight, fontITalic, fontCharset, faceName);
    try
      if KeyID = kwBegin then
      begin
        NextIdentifier;
        repeat
          exStyle := 0;
          if TokenType = ttIdentifier then
          case KeyID of
            kwLText,
            kwCText,
            kwRText:
              begin
                cls.isID := True;
                cls.id := STATIC_ID;
                style := WS_CHILD or WS_VISIBLE or WS_GROUP;

                case KeyID of
                  kwLText : style := style or SS_LEFT;
                  kwCText : style := style or SS_CENTER;
                  kwRText : style := style or SS_RIGHT;
                  kwIcon  : begin
                              style := style or SS_ICON;
                              controlOptions := [cpHasText];
                              cx := GetSystemMetrics (SM_CXICON);
                              cy := GetSystemMetrics (SM_CYICON)
                            end
                end;

                if KeyID <> SS_ICON then
                  controlOptions := [cpHasText, cpNeedsCXCY];

                GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, controlOptions);
                dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                Inc (CtlCount)
              end;

            kwListBox :
              begin
                cls.isID := True;
                cls.id := LISTBOX_ID;
                style := WS_CHILD or WS_VISIBLE or WS_BORDER or LBS_NOTIFY;
                GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpNeedsCXCY]);
                dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                Inc (CtlCount)
              end;

            kwComboBox :
              begin
                cls.isID := True;
                cls.id := COMBOBOX_ID;
                style := WS_CHILD or WS_VISIBLE or CBS_SIMPLE or WS_TABSTOP;
                GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpNeedsCXCY]);
                dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                Inc (CtlCount)
              end;

              kwPushButton,
              kwDefPushButton,
              kwAuto3State,
              kwAutoCheckBox,
              kwAutoRadioButton,
              kwCheckbox,
              kwRadioButton,
              kwState3,
              kwGroupBox,
              kwPushBox:
                begin
                  cls.isID := True;
                  cls.id := BUTTON_ID;
                  style := WS_CHILD or WS_VISIBLE or WS_TABSTOP;

                  case KeyID of
                    kwPushButton      : style := style or BS_PUSHBUTTON;
                    kwDefPushButton   : style := style or BS_DEFPUSHBUTTON;
                    kwAuto3State      : style := style or BS_AUTO3STATE;
                    kwAutoCheckBox    : style := style or BS_AUTOCHECKBOX;
                    kwAutoRadioButton : style := style or BS_AUTORADIOBUTTON;
                    kwCheckBox        : style := style or BS_CHECKBOX;
                    kwRadioButton     : style := style or BS_RADIOBUTTON;
                    kwState3          : style := style or BS_3STATE;
                    kwGroupBox        : style := (style and (not WS_TABSTOP)) or BS_GROUPBOX;
                    kwPushBox         : style := style or BS_PUSHBOX;
                  end;

                  GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpHasText, cpNeedsCXCY]);
                  dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                  Inc (CtlCount)
                end;

              kwEditText :
                begin
                  cls.isID := True;
                  cls.id := EDIT_ID;
                  style := WS_CHILD or WS_VISIBLE or ES_LEFT or WS_BORDER or WS_TABSTOP;
                  GetControlParams (true, title, id, x, y, cx, cy, style, exstyle, helpId, [cpNeedsCXCY]);
                  dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                  Inc (CtlCount)
                end;

              kwControl :
                begin
                  NextExpression (v);
                  title := ValToSZOrId (v);
                  ExpectChar (',');
                  id := NextIntegerExpression;
                  ExpectChar (',');
                  NextExpression (v);
                  cls := ValToSZOrId (v);
                  ExpectChar (',');
                  style := WS_CHILD or WS_VISIBLE;
                  style := style or DWORD (NextIntegerExpression);
                  ExpectChar (',');
                  x := NextIntegerExpression;
                  ExpectChar (',');
                  y := NextIntegerExpression;
                  ExpectChar (',');
                  cx := NextIntegerExpression;
                  ExpectChar (',');
                  cy := NextIntegerExpression;
                  if (TokenType = ttChar) and (TokenChar = ',') then
                    exStyle := NextIntegerExpression;
                  if (TokenType = ttChar) and (TokenChar = ',') then
                    helpId := NextIntegerExpression;
                  dlg.InitAddControlEx(cls, id, title, x, y, cx, cy, style, exstyle, helpId, 0, id);
                  Inc (CtlCount)
                end
              else
                GetToken;
          end
          else
            GetToken;
        until KeyID = kwEnd
      end
    finally
      dlg.EndInitEx (CtlCount)
    end
  finally
    options.Free
  end;

  GetToken;
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoIcon                                           |
 |                                                                      |
 | Handle an ICON resource                                              |
 |                                                                      |
 | ICON:: nameID ICON filename                                          |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoIcon;
var
  fFileName : string;
  ico : TIconCursorGroupResourceDetails;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  fFileName := ExpectString ('File name expected');
  ico := TIconGroupResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));
  ico.LoadImage(PathName + fFileName);
  GetToken;
end;

{*----------------------------------------------------------------------*
 | procedure TRCParser.DoLanguage                                       |
 |                                                                      |
 | Handle a LANGUAGE statement                                          |
 |                                                                      |
 | LANGUAGE:: Primary Language, SubLanguage                             |
 *----------------------------------------------------------------------*}
procedure TRCParser.DoLanguage;
var
  pri, sec : word;
begin
  pri := NextInteger;
  NextChar (',');
  sec := NextInteger;
  fLangId := MakeLangID (pri, sec);
  GetToken;
end;

(*----------------------------------------------------------------------*
 | procedure TRCParser.DoMenu                                           |
 |                                                                      |
 | Handle a MENU resource                                               |
 |                                                                      |
 | MENU:: menuID MENU  [[optional-statements]]                          |
 |                     {      item-definitions      . . .  }            |
 *----------------------------------------------------------------------*)
procedure TRCParser.DoMenu;
var
  validKeywords : TSupportedKeywords;
  options : TResourceOptions;
  mn : TMenuResourceDetails;
  itm : TMenuItem;

// Get menuitem or popup options - like CHECKED, GRAYED, etc.
  procedure GetMenuOptions;
  begin
    GetRestOfLine;
    GetToken;
  end;

// Create a menu item, and populate it from the RC data.
// Called recursively for popups.
  function CreateMenuItem : TMenuItem;
  var
    st : string;
    id : Integer;
    it : TMenuItem;
  begin
    GetToken;
    result := TMenuItem.Create(nil);
    repeat
      case KeyID of
        kwMenuItem : begin
                       GetToken;  // Menu caption, or SEPARATOR
                       id := 0;
                       if TokenType = ttString then
                       begin
                         st := Token;
                         NextChar (',');
                         id := NextIntegerExpression;
                       end
                       else
                         if (TokenType = ttIdentifier) and (Token = 'SEPARATOR') then
                         begin
                           st := '-';
                           GetToken
                         end
                         else
                           raise EParser.Create('Menu item name or SEPARATOR expected');
                       if TokenType = ttChar then
                         GetMenuOptions;

                       it := TMenuItem.Create(Nil);
                       it.Caption := st;
                       it.Tag := id;    // Can't set menuitem.command because it's
                                        // read-only.  TMenuResourceDetails uses
                                        // Tag instead.
                       result.Add(it);
                     end;
        kwPopup    : begin
                        st := NextString;       // Popup menu caption
                        GetToken;
                        if TokenType = ttChar then
                          GetMenuOptions;

                        if KeyID = kwBegin then
                          it := CreateMenuItem  // Recurse to create the popup menu's items
                        else
                          it := TMenuItem.Create (nil);
                        it.Caption := st;
                        result.Add(it)
                     end;
        else
          break
      end
    until KeyID = kwEnd;
    GetToken
  end;

begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  options := CreateResourceOptions (MenuOptionKeywords, validKeywords);
  try
   if Assigned(options) then
      mn := TMenuResourceDetails.CreateNew(fParent, options.Language, ValToStr (fName))
    else
      mn := TMenuResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));

    if KeyID = kwBegin then
    begin
      itm := CreateMenuItem;
      try
        mn.SetItems(itm);
      finally
        itm.Free
      end
    end
  finally
    options.Free
  end
end;

procedure TRCParser.DoMessageTable;
var
  fFileName : string;
  mt : TMessageResourceDetails;
begin
  SkipWhitespace;
  if Ch = '"' then
    NextString
  else
    GetRestOfLine;
  fFileName := Token;
  mt := TMessageResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));
  mt.Data.LoadFromFile(PathName + fFileName);
  GetToken;
end;

procedure TRCParser.DoStringTable;
var
  kw : DWORD;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;

  if KeyID = kwBegin then
  repeat
    GetToken;
    kw := KeyID;
  until kw = kwEnd;
  GetToken;
end;

procedure TRCParser.DoToolbar;
var
  btnWidth, btnHeight : Integer;
  tb : TToolbarResourceDetails;
  kw : Integer;
  v : TValue;
begin
  GetToken;
  SkipYeOldeMemoryAttributes;
  btnWidth := ExpectInteger;
  NextChar (',');
  btnHeight := NextInteger;
  NextToken;

  tb := TToolbarResourceDetails.CreateNew(fParent, fLangId, ValToStr (fName));

  if KeyID = kwBegin then
  repeat
    GetToken;
    kw := KeyID;

    case kw of
      kwButton :
        begin
          NextIdentifier;
          v := Calc (Token);
          GetToken
        end;

      kwSeparator :
      begin
        NextToken
      end
    end;

    kw := KeyID

  until kw = kwEnd;
  GetToken;
end;

procedure TRCParser.DoVersionInfo;
var
  v : TVersionInfoResourceDetails;
  st : string;
  beginendlevel : Integer;

  procedure GetFIXEDINFOId (const id : string; var st : string);
  var
    msg : string;
  begin
    msg := id + ' expected';
    NextIdentifier (msg);
    if not SameText (id, Token) then
      raise EParser.Create(msg);

    st := GetRestOfLine;
  end;
begin
  repeat
    NextIdentifier ('FIXEDINFO Identifier expected');

    if (Token = 'BLOCK') or (Token = 'BEGIN') then
      break;
    st := GetRestOfLine
  until fEOF;

  if fEOF then Exit;

  v := TVersionInfoResourceDetails.CreateNew(fParent, fLangID, ValToStr (fName));

  if Token = 'BEGIN' then
    beginendlevel := 1
  else
    beginEndLevel := 0;

  repeat
    fEOF := not GetToken;
    if fEOF then
      break;
    if TokenType = ttIdentifier then
      if Token = 'BEGIN' then
        Inc (beginendlevel)
      else
        if Token = 'END' then
          Dec (beginendlevel)
  until beginendlevel = 0;
  GetToken;
end;

function TRCParser.GetControlParams(ex : boolean; var text: TSzOrID; var id: DWORD;
  var x, y, cx, cy: Integer; var style, exStyle, helpId : DWORD; options : TControlParamsOptions) : boolean;
var
  v : TValue;
begin
  if cpHasText in options then
  begin
    NextExpression (v);
    text := ValToSzOrID (v);
    ExpectChar (',')
  end
  else
  begin
    text.isID := False;
    text.sz := ''
  end;
  id := NextIntegerExpression;
  ExpectChar (',');
  x := NextIntegerExpression;
  ExpectChar (',');
  y := NextIntegerExpression;

  if cpNeedsCXCY in options then
    ExpectChar (',');

  if (TokenType = ttChar) and (TokenChar = ',') then
  begin
    cx := NextIntegerExpression;
    ExpectChar (',');
    cy := NextIntegerExpression;

    if cpNeedsStyle in options then
      ExpectChar (',');

    if (TokenType = ttChar) and (TokenChar = ',') then
    begin
      style := style or DWORD (NextIntegerExpression);

      if (TokenType = ttChar) and (TokenChar = ',') then
        exStyle := NextIntegerExpression
    end
  end;

  if ex then
    if (TokenType = ttChar) and (TokenChar = ',') then
      helpId := NextIntegerExpression;
  result := True
end;

function TRCParser.KeyID : Integer;
var
  kw : TKeywordDetails;
begin
  if TokenType <> ttIdentifier then
    result := -1
  else
  begin
    kw := Keyword (Token);
    if Assigned(kw) then
      result := kw.kw
    else
      result := -1
  end
end;

function TRCParser.Keyword(const st: string): TKeywordDetails;
var
  idx : Integer;
begin
  idx := fKeywords.IndexOf(st);
  if idx >= 0 then
    result := TKeywordDetails (fKeywords.Objects [idx])
  else
    result := Nil;
end;

function TRCParser.NextExpression (var v : TValue) : boolean;
var
  st : string;
begin
  st := '';
  repeat
    fEOF := not GetToken;
    if fEOF then break;

    case TokenType of
     ttIdentifier :  if Assigned(KeyWord (Token)) then
                       break
                     else
                       st := st + Token;
     ttChar       :  if TokenChar in ['+', '-', '/', '\', '|', '~', '=', '&', '(', ')', '*'] then
                       st := st + TokenChar
                     else
                       break;

     ttNumber     : st := st + Token;
     ttString     : st := st + '"' + Token + '"'
    end
  until False;

  if st <> '' then
  begin
    v := Calc (st);
    result := True
  end
  else
    result := False;
end;

function TRCParser.NextIntegerExpression: Integer;
var
  v : TValue;
begin
  NextExpression (v);
  if v.tp <> vInteger then
    raise EParser.Create('Integer expression expected');
  result := v.iVal
end;

procedure TRCParser.Parse;
var
  dets : TKeywordDetails;
  tokenHandled : boolean;
  i : Integer;
begin
  GetChar;
  fEOF := not GetToken;
  while not fEOF do
  begin
    tokenHandled := False;
    if TokenType = ttIdentifier then
    begin
      dets := Keyword (Token);
      if not Assigned(dets) then
        fName := ResolveToken
      else
        if Assigned(dets.proc) then
        begin
          dets.proc;
          tokenHandled := True
        end
    end
    else
      if TokenType = ttNumber then
        if TokenSOL then
          fName := ResolveToken
        else
        begin
          for i := 0 to NoKeywords -1 do
            if Integer (KeywordTable [i].resID) = StrToInt (Token) then
            begin
              dets := Keyword (KeywordTable [i].kw);

              if Assigned(dets) and Assigned(dets.proc) then
              begin
                dets.proc;
                tokenHandled := True
              end;

              break;
            end
        end;
    if not tokenHandled then
      fEOF := not GetToken
  end
end;

procedure TRCParser.SkipYeOldeMemoryAttributes;
begin
  while not fEOF and (KeyID in [kwDiscardable, kwPure, kwMoveable, kwPreload, kwLoadOnCall]) do
    fEOF := not GetToken
end;

{ TKeywordDetails }

constructor TKeywordDetails.Create(AKeyword: Integer; AProc: TKeywordProc);
begin
  kw :=AKeyword;
  Proc := AProc;
end;

{ TResourceOptions }

constructor TResourceOptions.Create(Parser : TRCParser; SupportedKeywords: TSupportedKeywords);
begin
  fParser := Parser;
  fSupportedKeywords := SupportedKeywords;
end;

end.
