unit RichOle;

interface

uses
  Windows, Classes, ActiveX, RichEdit, Clipbrd;

type

TReObject = record
  cbStruct : DWORD;			// Size of structure
  cp : LongInt;				// Character position of object
  clsid : TCLSID;			// Class ID of object
  poleobj : IOleObject;			// OLE object interface
  pstg : IStorage;			// Associated storage interface
  polesite : IOleClientSite;		// Associated client site interface
  sizel : TSize;                        // Size of object(may be 0,0)
  dvaspect : DWORD;			// Display aspect to use
  dwFlags : DWORD;                      // Object status flags
  dwUser : DWORD;      			// Dword for user's use
end;
PReObject = ^TReObject;

// Flags to specify which interfaces should be returned in the structure above
const
  REO_GETOBJ_NO_INTERFACES	= $00000000;
  REO_GETOBJ_POLEOBJ		= $00000001;
  REO_GETOBJ_PSTG		= $00000002;
  REO_GETOBJ_POLESITE		= $00000004;
  REO_GETOBJ_ALL_INTERFACES	= $00000007;

// Place object at selection
  REO_CP_SELECTION = -1;

// Use character position to specify object instead of index
  REO_IOB_SELECTION = -1;
  REO_IOB_USE_CP = -2;

// Object flags
  REO_NULL		= 0;	// No flags
  REO_READWRITEMASK	= $0000003f;	// Mask out RO bits
  REO_DONTNEEDPALETTE	= $00000020;	// Object doesn't need palette
  REO_BLANK		= $00000010;	// Object is blank
  REO_DYNAMICSIZE	= $00000008;	// Object defines size always
  REO_INVERTEDSELECT	= $00000004;	// Object drawn all inverted if sel
  REO_BELOWBASELINE	= $00000002;	// Object sits below the baseline
  REO_RESIZABLE		= $00000001;	// Object may be resized
  REO_LINK		= $80000000;	// Object is a link(RO)
  REO_STATIC		= $40000000;	// Object is static (RO)
  REO_SELECTED		= $08000000;	// Object selected (RO)
  REO_OPEN		= $04000000;	// Object open in its server (RO)
  REO_INPLACEACTIVE	= $02000000;	// Object in place active(RO)
  REO_HILITED		= $01000000;	// Object is to be hilited (RO)
  REO_LINKAVAILABLE	= $00800000;	// Link believed available(RO)
  REO_GETMETAFILE	= $00400000;	// Object requires metafile(RO)

// flags for IRichEditOle.GetClipboardData,
// IRichEditOleCallback.GetClipboardData and
// IRichEditOleCallback.QueryAcceptData
  RECO_PASTE		= $00000000;	// paste from clipboard
  RECO_DROP		= $00000001;	// drop
  RECO_COPY		= $00000002;	// copy to the clipboard
  RECO_CUT		= $00000003;	// cut to the clipboard
  RECO_DRAG		= $00000004;	// drag

(*
 *	IRichEditOle
 *
 *	Purpose:
 *		Interface used by the client of RichEdit to perform OLE-related
 *		operations.
 *
 *	//$ REVIEW:
 *		The methods herein may just want to be regular Windows messages.
 *)

type
  IRichEditOle = interface(IUnknown)
    ['{00020d00-0000-0000-00c0-000000000046}']
    function GetClientSite(out clientSite: IOleClientSite): HResult; stdcall;
    function GetObjectCount : Longint; stdcall;
    function GetLinkCount : Longint; stdcall;
    function GetObject(iob : Longint; var lpreobject : TReObject; dwFlags : DWORD) : HRESULT; stdcall;
    function InsertObject(const obj : TReObject) : HRESULT; stdcall;
    function ConvertObject(iob : Longint; const rclsidNew : TCLSID; lpstrUserTypeNew : PChar) : HRESULT; stdcall;
    function ActivateAs (const clsid : TCLSID; const clsidAs : TCLSID) : HRESULT; stdcall;
    function SetHostNames (lpstrContainerApp : PChar; lpstrContainerObj : PChar) : HRESULT; stdcall;
    function SetLinkAvailable(iob : Longint; fAvailable : BOOL) : HRESULT; stdcall;
    function SetDvaspect(iob : Longint; dvAspect : DWORD) : HRESULT; stdcall;
    function HandsOffStorage(iob : Longint) : HRESULT; stdcall;
    function SaveComplete(iob : Longint; lpstg : IStorage) : HRESULT; stdcall;
    function InPlaceDeactivate : HRESULT; stdcall;
    function ContextSensitiveHelp (fEnterMode : BOOL) : HRESULT; stdcall;
    function GetClipboardData(const lpchrg : TCharRange; reco : DWORD; out lplpDataObj : IDataObject) : HRESULT; stdcall;
    function ImportDataObject(lpDataObj : IDataObject; cf : TClipFormat; hMetaPict : HGLOBAL) : HRESULT; stdcall;
  end;

  IRichEditOleCallback = interface(IUnknown)
    ['{00020d03-0000-0000-00c0-000000000046}']

    function GetNewStorage(out lplpstg : IStorage) : HRESULT; stdcall;
    function GetInPlaceContext(out lplpFrame : IOleInplaceFrame; out lplpDoc : IOleInplaceUIWindow; const lpFrameInfo : TOleInplaceFrameInfo) : HRESULT; stdcall;
    function ShowContainerUI (fShow : BOOL) : HRESULT; stdcall;
    function QueryInsertObject(const lpclsid : TCLSID; lpstg : IStorage; cp : Longint) : HRESULT; stdcall;
    function DeleteObject(lpOleObj : IOleObject) : HRESULT; stdcall;
    function QueryAcceptData(lpDataObj : IDataObject; var lpcfFormat : TClipFormat; reco : DWORD; fReallity : BOOL; hMetaPict : HGLOBAL) : HRESULT; stdcall;
    function ContextSensitiveHelp (fEnterMode : BOOL) : HRESULT; stdcall;
    function GetClipboardData(var lpchrg : TCharRange; reco : DWORD; out lplpDataObj : IDataObject) : HRESULT; stdcall;
    function GetDragDropEffect(fDrag : BOOL; grfKeyState : DWORD; var pdwEffect : DWORD) : HRESULT; stdcall;
    function GetContextMenu(selType : Word; lpOleObj : IOleObject; var lpchrg : TCharRange; var lphMenu : HMENU) : HRESULT; stdcall;
  end;
implementation

end.
