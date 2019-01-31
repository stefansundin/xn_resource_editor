unit cmpUCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls;

type
  TuEdit = class(TEdit)
  private
    FCodePage: Integer;
    procedure SetCodePage(const Value: Integer);
    procedure WMPaste (var msg: TwmPaste); message WM_PASTE;
    function GetWideText: WideString;
    procedure SetWideText(const Value: WideString);
  public
    property CodePage: Integer read FCodePage write SetCodePage;
    property WideText: WideString read GetWideText write SetWideText;
  end;

  TuComboBox = class(TComboBox)
  private
    FuEditInstance: Pointer;
    FuDefEditProc: Pointer;
    FuListInstance: Pointer;
    FuDefListProc: Pointer;
    FCodePage: Integer;

    procedure ListWndProc(var Message: TMessage);
    procedure SetCodePage(const Value: Integer);
    function GetWideText: WideString;
    procedure SetWideText(const Value: WideString);
    procedure SubclassListBox;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
    procedure DestroyWindowHandle; override;
    procedure DropDown; override;
    procedure EditWndProc(var Message: TMessage); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    property CodePage: Integer read FCodePage write SetCodePage;

    property WideText: WideString read GetWideText write SetWideText;
  end;

implementation

uses unitCharsetMap, ClipBrd;

const
  CF_UNICODETEXT = 13;

function WideStringFromClipboard (CodePage: Integer): WideString;
var
  Data: THandle;
  Unicode: Boolean;
begin
  Clipboard.Open;
  Unicode := Clipboard.HasFormat(CF_UNICODETEXT);
  if Unicode then
    Data := GetClipboardData(CF_UNICODETEXT)
  else
    Data := GetClipboardData(CF_TEXT);

  try
    if Data <> 0 then
      if Unicode then
        result := PWideChar(GlobalLock(Data))
      else
        result := PChar (GlobalLock (Data))
    else
      result := '';
  finally
    if Data <> 0 then GlobalUnlock(Data);
    Clipboard.Close;
  end;
end;


{ TuEdit }

function TuEdit.GetWideText: WideString;
begin
  result := StringToWideString (Text, CodePage);
end;

procedure TuEdit.SetCodePage(const Value: Integer);
begin
  if FCodePage <> Value then
  begin
    Font.Charset := CodePageToCharset (Value);
    FCodePage := Value;
  end
end;

procedure TuEdit.SetWideText(const Value: WideString);
begin
  Text := WideStringToString (Value, codepage);
end;

procedure TuEdit.WMPaste(var msg: TwmPaste);
begin
  WideText := WideStringFromClipboard (CodePage)
end;

{ TuComboBox }


constructor TuComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FuEditInstance := Classes.MakeObjectInstance(EditWndProc);
  FuListInstance := Classes.MakeObjectInstance(ListWndProc);
end;

procedure TuComboBox.CreateWnd;
begin
  inherited;

  FuDefEditProc := Pointer(GetWindowLong(FEditHandle, GWL_WNDPROC));
  SetWindowLong(FEditHandle, GWL_WNDPROC, Longint(FuEditInstance));

  SubclassListBox;
end;

destructor TuComboBox.Destroy;
begin
  FreeObjectInstance(FuEditInstance);
  FreeObjectInstance (FuListInstance);
  inherited;
end;

procedure TuComboBox.DestroyWindowHandle;
begin
  SetWindowLong (FEditHandle, GWL_WNDPROC, LongInt (FuDefEditProc));
  inherited;
end;

procedure TuComboBox.DropDown;
begin

  inherited;
end;

procedure TuComboBox.EditWndProc(var Message: TMessage);
var
  st: string;

begin
  if message.Msg = WM_PASTE then
  begin
    st := WideStringToString (WideStringFromClipboard (CodePage), CodePage);
    SendMessage (FEditHandle, WM_SETFONT, Font.Handle, 1);
    SetWindowText (FEditHandle, PChar (st));
    Message.Result := 0;
  end
  else
    message.Result := CallWindowProc (FuDefEditProc, FEditHandle, Message.Msg, message.WParam, message.LParam);
end;

function TuComboBox.GetWideText: WideString;
begin
  result := StringToWideString (Text, CodePage);
end;

procedure TuComboBox.ListWndProc(var Message: TMessage);
var
  callDefProc: Boolean;
begin
  case message.Msg of
    WM_DESTROY :
      begin
        SetWindowLong (FListHandle, GWL_WNDPROC, LongInt (FuDefListProc));
        callDefProc := True
      end
    else
      callDefProc := True;
  end;


  if callDefProc then
    message.Result := CallWindowProc (FuDefListProc, FListHandle, Message.Msg, message.WParam, message.LParam);
end;

procedure TuComboBox.SetCodePage(const Value: Integer);
begin
  if FCodePage <> Value then
  begin
    Font.Charset := CodePageToCharset (Value);
    FCodePage := Value;
    if FListHandle <> 0 then
      SendMessage (FListHandle, WM_SETFONT, SendMessage (FEditHandle, WM_GETFONT, 0, 0), 0);
  end
end;

procedure TuComboBox.SetWideText(const Value: WideString);
begin
  Text := WideStringToString (Value, CodePage);
end;

procedure TuComboBox.SubclassListBox;
begin
  if fListHandle <> 0 then
  begin
    FuDefListProc := Pointer (GetWindowLong (FListHandle, GWL_WNDPROC));
    SetWindowLong (FListHandle, GWL_WNDPROC, LongInt (FuListInstance));
    SendMessage (FListHandle, WM_SETFONT, SendMessage (FEditHandle, WM_GETFONT, 0, 0), 0);
  end;
end;

procedure TuComboBox.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_CTLCOLORLISTBOX then
  begin
    if fListHandle = 0 then
    begin
      fListHandle := Message.lParam;
      SubclassListBox
    end
  end;
  inherited;
end;

end.
