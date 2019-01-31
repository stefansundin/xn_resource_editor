unit unitHTMLHelp;

{$WEAKPACKAGEUNIT}
{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses Windows;

const
  HH_DISPLAY_TOPIC        = $0000;
  HH_HELP_FINDER          = $0000;  // WinHelp equivalent
  HH_DISPLAY_TOC          = $0001;  // not currently implemented
  HH_DISPLAY_INDEX        = $0002;  // not currently implemented
  HH_DISPLAY_SEARCH       = $0003;  // not currently implemented
  HH_SET_WIN_TYPE         = $0004;
  HH_GET_WIN_TYPE         = $0005;
  HH_GET_WIN_HANDLE       = $0006;
  HH_GET_INFO_TYPES       = $0007;  // not currently implemented
  HH_SET_INFO_TYPES       = $0008;  // not currently implemented
  HH_SYNC                 = $0009;
  HH_ADD_NAV_UI           = $000A;  // not currently implemented
  HH_ADD_BUTTON           = $000B;  // not currently implemented
  HH_GETBROWSER_APP       = $000C;  // not currently implemented
  HH_KEYWORD_LOOKUP       = $000D;
  HH_DISPLAY_TEXT_POPUP   = $000E;  // display string resource id or text in a popup window
  HH_HELP_CONTEXT         = $000F;  // display mapped numeric value in dwData
  HH_TP_HELP_CONTEXTMENU  = $0010;  // text popup help, same as WinHelp HELP_CONTEXTMENU
  HH_TP_HELP_WM_HELP      = $0011;  // text popup help, same as WinHelp HELP_WM_HELP

  HH_CLOSE_ALL            = $0012;  // close all windows opened directly or indirectly by the caller
  HH_ALINK_LOOKUP         = $0013;  // ALink version of HH_KEYWORD_LOOKUP
  HH_GET_LAST_ERROR       = $0014;  // not currently implemented // See HHERROR.h
  HH_ENUM_CATEGORY        = $0015;	// Get category name, call repeatedly to enumerate, -1 at end
  HH_ENUM_CATEGORY_IT     = $0016;  // Get category info type members, call repeatedly to enumerate, -1 at end
  HH_RESET_IT_FILTER      = $0017;  // Clear the info type filter of all info types.
  HH_SET_INCLUSIVE_FILTER = $0018;  // set inclusive filtering method for untyped topics to be included in display
  HH_SET_EXCLUSIVE_FILTER = $0019;  // set exclusive filtering method for untyped topics to be excluded from display

  HH_INITIALIZE           = $001C;  // Initializes the help system.
  HH_UNINITIALIZE         = $001D;  // Uninitializes the help system.
  HH_PRETRANSLATEMESSAGE  = $00fd;  // Pumps messages. (NULL, NULL, MSG*).

  HHWIN_PROP_ONTOP          = (1 shl 1);        // Top-most window (not currently implemented)
  HHWIN_PROP_NOTITLEBAR     = (1 shl 2);        // no title bar
  HHWIN_PROP_NODEF_STYLES   = (1 shl 3);        // no default window styles (only HH_WINTYPE.dwStyles)
  HHWIN_PROP_NODEF_EXSTYLES = (1 shl 4);        // no default extended window styles (only HH_WINTYPE.dwExStyles)
  HHWIN_PROP_TRI_PANE       = (1 shl 5);        // use a tri-pane window
  HHWIN_PROP_NOTB_TEXT      = (1 shl 6);        // no text on toolbar buttons
  HHWIN_PROP_POST_QUIT      = (1 shl 7);        // post WM_QUIT message when window closes
  HHWIN_PROP_AUTO_SYNC      = (1 shl 8);        // automatically ssync contents and index
  HHWIN_PROP_TRACKING       = (1 shl 9);        // send tracking notification messages
  HHWIN_PROP_TAB_SEARCH     = (1 shl 10);       // include search tab in navigation pane
  HHWIN_PROP_TAB_HISTORY    = (1 shl 11);       // include history tab in navigation pane
  HHWIN_PROP_TAB_FAVORITES  = (1 shl 12);       // include favorites tab in navigation pane
  HHWIN_PROP_CHANGE_TITLE   = (1 shl 13);       // Put current HTML title in title bar
  HHWIN_PROP_NAV_ONLY_WIN   = (1 shl 14);       // Only display the navigation window
  HHWIN_PROP_NO_TOOLBAR     = (1 shl 15);       // Don't display a toolbar

  HHWIN_PARAM_PROPERTIES	= (1 shl 1);	 // valid fsWinProperties
  HHWIN_PARAM_STYLES		= (1 shl 2);	 // valid dwStyles
  HHWIN_PARAM_EXSTYLES		= (1 shl 3);	 // valid dwExStyles
  HHWIN_PARAM_RECT			= (1 shl 4);	 // valid rcWindowPos
  HHWIN_PARAM_NAV_WIDTH		= (1 shl 5);	 // valid iNavWidth
  HHWIN_PARAM_SHOWSTATE		= (1 shl 6);	 // valid nShowState
  HHWIN_PARAM_INFOTYPES		= (1 shl 7);	 // valid ainfoTypes
  HHWIN_PARAM_TB_FLAGS		= (1 shl 8);	 // valid fsToolBarFlags
  HHWIN_PARAM_EXPANSION		= (1 shl 9);	 // valid fNotExpanded
  HHWIN_PARAM_TABPOS		= (1 shl 10);	 // valid tabpos
  HHWIN_PARAM_TABORDER		= (1 shl 11);	 // valid taborder
  HHWIN_PARAM_HISTORY_COUNT	= (1 shl 12);	 // valid cHistory
  HHWIN_PARAM_CUR_TAB 		= (1 shl 13);	 // valid curNavType

  HHWIN_BUTTON_EXPAND       = (1 shl 1);         // Expand/contract button
  HHWIN_BUTTON_BACK         = (1 shl 2);         // Back button
  HHWIN_BUTTON_FORWARD      = (1 shl 3);         // Forward button
  HHWIN_BUTTON_STOP         = (1 shl 4);         // Stop button
  HHWIN_BUTTON_REFRESH      = (1 shl 5);         // Refresh button
  HHWIN_BUTTON_HOME         = (1 shl 6);         // Home button
  HHWIN_BUTTON_BROWSE_FWD   = (1 shl 7);         // not implemented
  HHWIN_BUTTON_BROWSE_BCK   = (1 shl 8);         // not implemented
  HHWIN_BUTTON_NOTES        = (1 shl 9);         // not implemented
  HHWIN_BUTTON_CONTENTS     = (1 shl 10);        // not implemented
  HHWIN_BUTTON_SYNC         = (1 shl 11);        // Sync button
  HHWIN_BUTTON_OPTIONS      = (1 shl 12);        // Options button
  HHWIN_BUTTON_PRINT        = (1 shl 13);        // Print button
  HHWIN_BUTTON_INDEX        = (1 shl 14);        // not implemented
  HHWIN_BUTTON_SEARCH       = (1 shl 15);        // not implemented
  HHWIN_BUTTON_HISTORY      = (1 shl 16);        // not implemented
  HHWIN_BUTTON_FAVORITES    = (1 shl 17);        // not implemented
  HHWIN_BUTTON_JUMP1        = (1 shl 18);
  HHWIN_BUTTON_JUMP2        = (1 shl 19);

  HHWIN_DEF_BUTTONS =
			(HHWIN_BUTTON_EXPAND or
			 HHWIN_BUTTON_BACK or
			 HHWIN_BUTTON_OPTIONS or
			 HHWIN_BUTTON_PRINT);

// Button IDs

  IDTB_EXPAND          = 200;
  IDTB_CONTRACT        = 201;
  IDTB_STOP            = 202;
  IDTB_REFRESH         = 203;
  IDTB_BACK            = 204;
  IDTB_HOME            = 205;
  IDTB_SYNC            = 206;
  IDTB_PRINT           = 207;
  IDTB_OPTIONS         = 208;
  IDTB_FORWARD         = 209;
  IDTB_NOTES           = 210; // not implemented
  IDTB_BROWSE_FWD      = 211;
  IDTB_BROWSE_BACK     = 212;
  IDTB_CONTENTS        = 213; // not implemented
  IDTB_INDEX           = 214; // not implemented
  IDTB_SEARCH          = 215; // not implemented
  IDTB_HISTORY         = 216; // not implemented
  IDTB_FAVORITES       = 217; // not implemented
  IDTB_JUMP1           = 218;
  IDTB_JUMP2           = 219;
  IDTB_CUSTOMIZE       = 221;

// Notification codes

  HHN_FIRST       = -860;
  HHN_LAST        = -879;

  HHN_NAVCOMPLETE = HHN_FIRST-0;
  HHN_TRACK       = HHN_FIRST-1;

type
  THHNNotify = packed record
	hdr : TNMHDR;
	pszUrl : PChar; // Multi-byte, null-terminated string
  end;

  THHPopup = packed record
    cbStruct : Integer;        // sizeof this structure
    hInst : Integer;           // instance handle for string resource
    idString : UINT;           // string resource id, or text id if pszFile is specified in HtmlHelp call
    pszTest : PChar;           // used if idString is zero
    pt : TPoint;               // top center of popup window
    clrForeground : COLORREF;  // use -1 for default
    clrBackground : COLORREF;  // use -1 for default
    rcMargins : TRect;         // amount of space between edges of window and text, -1 for each member to ignore
    pszFont : Pchar            // facename, point size, char set, BOLD ITALIC UNDERLINE
  end;

  THHAKlink = packed record
	cbStruct : Integer;        // sizeof this structure
	fReserved : BOOL;          // must be FALSE (really!)
	pszKeywords : PChar;       // semi-colon separated keywords
	pszUrl : PChar;            // URL to jump to if no keywords found (may be NULL)
	pszMsgText : PChar;        // Message text to display in MessageBox if pszUrl is NULL and no keyword match
	pszMsgTitle : PChar;       // Message text to display in MessageBox if pszUrl is NULL and no keyword match
	pszWindow : PChar;         // Window to display URL in
  end;


  THHAction = (
	HHACT_EXPAND,
	HHACT_CONTRACT,
	HHACT_BACK,
	HHACT_FORWARD,
	HHACT_STOP,
	HHACT_REFRESH,
	HHACT_HOME,
	HHACT_SYNC,
	HHACT_OPTIONS,
	HHACT_PRINT,

	HHACT_TAB_CONTENTS,
	HHACT_TAB_INDEX,
	HHACT_TAB_SEARCH,
	HHACT_TAB_HISTORY,
	HHACT_TAB_FAVORITES
  );

  THHNTrack = packed record
    hdr : TNMHdr;
	pszCurUrl : PChar;      // Multi-byte, null-terminated string
	idAction : THHAction    // HHACT_ value
  end;

  THHNavType = (
	HHWIN_NAVTYPE_TOC,
	HHWIN_NAVTYPE_INDEX,
	HHWIN_NAVTYPE_SEARCH,
	HHWIN_NAVTYPE_HISTORY,          // not implemented
	HHWIN_NAVTYPE_FAVORITES);       // not implemented

  THHInfoType = DWORD;
  PHHInfoType = ^THHInfoType;

  THHNavTab = (
	HHWIN_NAVTAB_TOP,
	HHWIN_NAVTAB_LEFT,
	HHWIN_NAVTAB_BOTTOM);

const
  HH_MAX_TABS = 19;  // maximum number of tabs

type
  THHTab = (
	HH_TAB_CONTENTS,
	HH_TAB_INDEX,
	HH_TAB_SEARCH,
	HH_TAB_HISTORY,
	HH_TAB_FAVORITES);

const
// HH_DISPLAY_SEARCH Command Related Structures and Constants

  HH_FTS_DEFAULT_PROXIMITY = -1;

type
  THHFTSQuery = packed record
	cbStruct : Integer;        // Sizeof structure in bytes.
	fUniCodeStrings : BOOL;    // TRUE if all strings are unicode.
	pszSearchQuery : PChar;    // String containing the search query.
	iProximity : LongInt;      // Word proximity.
	fStemmedSearch : BOOL;     // TRUE for StemmedSearch only.
	fTitleOnly : BOOL;         // TRUE for Title search only.
	fExecute : BOOL;           // TRUE to initiate the search.
	pszWindow : PChar;         // Window to display in
  end;


// HH_WINTYPE Structure

  THHWinType = packed record
	cbStruct : Integer;         // IN: size of this structure including all Information Types
	fUniCodeStrings : BOOL;     // IN/OUT: TRUE if all strings are in UNICODE
	pszType : PChar;            // IN/OUT: Name of a type of window
	fsValidMembers : DWORD;     // IN: Bit flag of valid members (HHWIN_PARAM_)
	fsWinProperties : DWORD;    // IN/OUT: Properties/attributes of the window (HHWIN_)

	pszCaption : PChar;         // IN/OUT: Window title
	dwStyles : DWORD;           // IN/OUT: Window styles
	dwExStyles : DWORD;         // IN/OUT: Extended Window styles
	rcWindowPos : TRect;        // IN: Starting position, OUT: current position
	nShowState : Integer;       // IN: show state(e.g., SW_SHOW)

	hwndHelp : HWND;            // OUT: window handle
	hwndCaller : HWND;          // OUT: who called this window

	paInfoTypes : PHHInfoType;  // IN: Pointer to an array of Information Types

	// The following members are only valid if HHWIN_PROP_TRI_PANE is set

	hwndToolBar : HWND;         // OUT: toolbar window in tri-pane window
	hwndNavigation : HWND;      // OUT: navigation window in tri-pane window
	hwndHTML : HWND;            // OUT: window displaying HTML in tri-pane window
	iNavWidth : Integer;        // IN/OUT: width of navigation window
	rcHTML : TRect;             // OUT: HTML window coordinates

	pszToc : PChar; 		    // IN: Location of the table of contents file
	pszIndex : PChar;		    // IN: Location of the index file
	pszFile : PChar;		    // IN: Default location of the html file
	pszHome : PChar;		    // IN/OUT: html file to display when Home button is clicked
	fsToolBarFlags : DWORD;     // IN: flags controling the appearance of the toolbar
	fNotExpanded : BOOL;	    // IN: TRUE/FALSE to contract or expand, OUT: current state
	curNavType : Integer; 	    // IN/OUT: UI to display in the navigational pane
	tabpos : Integer; 		    // IN/OUT: NAVTAB_TOP, NAVTAB_LEFT, or NAVTAB_BOTTOM
	idNotify : Integer;		    // IN: ID to use for WM_NOTIFY messages
	tabOrder : array [0..HH_MAX_TABS] of byte;	  // IN/OUT: tab order: Contents, Index, Search, History, Favorites, Reserved 1-5, Custom tabs
	cHistory : Integer;		    // IN/OUT: number of history items to keep (default is 30)
	pszJump1 : PChar;		    // Text for HHWIN_BUTTON_JUMP1
	pszJump2 : PChar;		    // Text for HHWIN_BUTTON_JUMP2
	pszUrlJump1 : PChar;	    // URL for HHWIN_BUTTON_JUMP1
	pszUrlJump2 : PChar;	    // URL for HHWIN_BUTTON_JUMP2
	rcMinSize : TRect;		    // Minimum size for window (ignored in version 1)
  end;



const
  ATOM_HTMLHELP_API_ANSI    = 14;
  ATOM_HTMLHELP_API_UNICODE = PChar (15);

function HtmlHelp (hwndCaller : HWND; pszFile : PChar; uCommand : UINT; dwData : DWORD) : DWORD; stdcall;

implementation

function HtmlHelp; external 'HHCTRL.OCX' index ATOM_HTMLHELP_API_ANSI;


end.
