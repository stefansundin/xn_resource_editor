unit DialogStrings;

interface

uses SysUtils;

resourcestring
  rstDialog = 'Dialog';
  rstControl = 'Control';
  rstButton = 'Button';
  rstCheckbox = 'Check box';
  rstGroupBox = 'Group box';
  rstEdit = 'Edit';
  rstPicture = 'Picture';
  rstRadiobutton = 'Radio Button';
  rstListbox = 'Listbox';
  rstListView = 'List-View';

  rstLeft = 'Left';
  rstTop = 'Top';
  rstWidth = 'Width';
  rstHeight = 'Height';
  rstCaption = 'Caption';
  rstDisabled = 'Disabled';
  rstVisible = 'Visible';
  rstClientEdge = 'Client Edge';
  rstStaticEdge = 'Static Edge';
  rstAcceptFiles = 'Accept Files';
  rstTransparent = 'Transparent';
  rstID = 'ID';
  rstGroup = 'Group';
  rstContextHelp = 'Context Help';
  rstTabstop = 'Tab Stop';
  rstFont = 'Font';
  rstMenu = 'Menu';
  rstFontDesc = '%d pt %s';
  rstStyle = 'Style';
  rstPopup = 'Popup';
  rstOverlapped = 'Overlapped';
  rstChild = 'Child';
  rstBorder = 'Border';
  rstTitleBar = 'Title Bar';
  rstSystemMenu = 'System Menu';
  rstMinimizaBox = 'Minimize Box';
  rstMaximimizeBox = 'Maximize Box';
  rstClipSiblings = 'Clip Siblings';
  rstClipChildren = 'Clip Children';
  rstHScroll = 'Horiz Scroll';
  rstVScroll = 'Vert Scroll';
  rstSystemModal = 'System Modal';
  rstAbsoluteAlign = 'Absolute Align';
  rstCenter = 'Centre';
  rstCenterMouse = 'Centre Mouse';
  rstLocalEdit = 'Local Edit';
  rstMinimizeBox = 'Minimize Box';
  rstMaximizeBox = 'Maximize Box';
  rstNoFailCreate = 'No Fail Create';
  rstNoIdleMsg = 'No Idle Message';
  rstSetForeground = 'Set Foreground';
  rstNone = 'None';
  rstThin = 'Thin';
  rstResizing = 'Resizing';
  rstToolWindow = 'Tool Window';
  rstControlParent = 'Control Parent';
  rstNoParentNotify = 'No Parent Notify';
  rstRTLReadingOrder = 'RTL Reading Order';
  rstRightAlignedText = 'Right Aligned Text';
  rstLeftScrollBar = 'Scroll Bar on Left';
  rstSunken = 'Sunken';
  rstNotify = 'Notify';
  rstColor = 'Colour';
  rstImageID = 'Image ID';
  rstType = 'Type';
  rstBlack = 'Black';
  rstGrey = 'Grey';
  rstWhite = 'White';
  rstEtched = 'Etched';
  rstEtchedHorz = 'Etched Horizontal';
  rstEtchedVert = 'Etched Vertical';
  rstFrame = 'Frame';
  rstRectangle = 'Rectangle';
  rstIcon = 'Icon';
  rstBitmap = 'Bitmap';
  rstEnhancedMetaFile = 'Enhanced Metafile';
  rstAutoHScroll = 'Auto H Scroll';
  rstAutoVScroll = 'Auto VScroll';
  rstMultiline = 'Multiline';
  rstAlign = 'Alignment';
  rstView = 'View';
  rstRight = 'Right';
  rstWantReturn = 'Want Return';
  rstOEMConvert = 'OEM Convert';
  rstReadOnly = 'Read Only';
  rstMask = 'Mask';
  rstUppercase = 'Uppercase';
  rstLowercase = 'Lowercase';
  rstNumeric = 'Numeric';
  rstText = 'Text';
  rstStaticText = 'Static Text';
  rstImagePositioning = 'Image Positioning';
  rstCenterImage = 'Centre Image';
  rstRealSizeImage = 'Real Size Image';
  rstRealSizeControl = 'Real Size Control';
  rstRightJustify = 'Right Justify';
  rstEllipsis = 'Ellipsis';
  rstEndEllipsis = 'End Ellipsis';
  rstWordEllipsis = 'Word Ellipsis';
  rstPathEllipsis = 'Path Ellipsis';
  rstModalFrame = 'Modal Frame';
  rstCenterVertically = 'Centre Vertically';
  rstNoPrefix = 'No Prefix';
  rstNoWrap = 'No Wrap';
  rstSimple = 'Simple';
  rstPassword = 'Password';
  rstHAlign = 'Horizontal Alignment';
  rstVAlign = 'Vertical Alignment';
  rstFlat = 'Flat';
  rstDefault = 'Default';
  rstOwnerDraw = 'Owner Draw';
  rstBottom = 'Bottom';
  rstDefaultButton = 'Default Button';
  rstAuto = 'Auto';
  rstLeftText = 'Left Text';
  rstTriState = 'Tri-state';
  rstPushLike = 'Push-like';
  rstComboBox = 'Combo Box';
  rstSort = 'Sort';
  rstNoIntegralHeight = 'No Integral Height';
  rstDisableNoScroll = 'Disable No Scroll';
  rstDropdown = 'Drop Down';
  rstDropdownList = 'Drop Down List';
  rstNo = 'No';
  rstFixed = 'Fixed';
  rstVariable = 'Variable';
  rstSelection = 'Selection';
  rstMultiColumn = 'Multi Column';
  rstUseTabstops = 'Use Tabstops';
  rstWantKeyInput = 'Want Key Input';
  rstNoredraw = 'No Redraw';
  rstSingle = 'Single';
  rstMultiple = 'Multiple';
  rstExtended = 'Extended';
  rstHScrollbar = 'Horizontal Scrollbar';
  rstVScrollbar = 'Vertical Scrollbar';
  rstOrientation = 'Orientation';
  rstAlignment = 'Alignment';
  rstAutoBuddy = 'Auto Buddy';
  rstSetBuddyInteger = 'Set Buddy Integer';
  rstNoThousands = 'No Thousands';
  rstArrowKeys = 'Arrow Keys';
  rstWrap = 'Wrap';
  rstHotTrack = 'Hot Track';
  rstUpDown = 'Spin';
  rstHorizontal = 'Horizontal';
  rstVertical = 'Vertical';
  rstUnattached = 'Unattached';
  rstSmooth = 'Smooth';
  rstProgressBar = 'Progress Bar';

  rstSlider = 'Slider';
  rstPoint = 'Point';
  rstTickMarks = 'Tick Marks';
  rstAutoTicks = 'Auto Ticks';
  rstEnableSelection = 'Enable Selection';
  rstBoth = 'Both';
  rstTopLeft = 'Top/Left';
  rstBottomRight = 'Bottom/Right';
  rstThumb = 'Thumb';

  rstHotkey = 'Hotkey';
  rstReport = 'Report';
  rstList = 'List';
  rstSmallIcon = 'Small Icon';

  rstAscending = 'Ascending';
  rstDescending = 'Descending';
  rstShareImageLists = 'Share Image Lists';

function StringToCString (const st : string) : string;
function CStringToString (const st : string) : string;

implementation

function StringToCString (const st : string) : string;
var
  i, p, l, c : Integer;
  st1 : string;
begin
  result := st;
  l := Length (st);
  i := 0;
  p := 0;
  while i < l do
  begin
    Inc (i);
    Inc (p);
    if st [i] < ' ' then
    begin
      c := Ord (st [i]);
      result [p] := '\';
      Inc (p);

      case c of
        13 : st1 := 'r';
        10 : st1 := 'n';
         9 : st1 := 't';
        else st1 := 'x' + Format ('%2.2x', [c])
      end;

      Insert (st1, result, p);
      Inc (p, Length (st1)-1)
    end
  end
end;

function CStringToString (const st : string) : string;
var
  i, l : Integer;
begin
  result := st;
  l := Length (st);
  i := 0;
  while i < l do
  begin
    Inc (i);

    if result [i] = '\' then
    begin
      Delete (result, i, 1);
      case result [i] of
        '\' :;
        'n' : result [i] := #10;
        'r' : result [i] := #13;
        't' : result [i] := #9;
        'x' : begin
                result [i] := Char (StrToInt ('$' + Copy (result, i + 1, 2)));
                Delete (result, i + 1, 2)
              end;
//        else
//          raise Exception.Create ('Syntax error in C String')
      end
    end
  end
end;

end.
