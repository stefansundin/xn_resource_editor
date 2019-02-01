(*======================================================================*
 | ComponentPersistentPosition unit for MiscUnits package               |
 |                                                                      |
 | Drop one on your main form, and it will run in the same position     |
 | as when it was previously closed.                                    |
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
 | Copyright © Colin Wilson 2005.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      26/02/2002  CPWW  Original                                  |
 | 1.1      11/01/2005  CPWW  Fixed bug with saving position when       |
 |                            maximized.                                |
 | 1.2      12/05/2005  CPWW  Big changes to cope with RecreateHandle   |
 |                            happening on the owner form.              |
 |                                                                      |
 |                            Now restores 'minimized' state            |
 *======================================================================*)

unit ComponentPersistentPosition;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Registry;

type
//------------------------------------------------------------------------
// TPersistentPosition class

  TPersistentPosition = class(TComponent)
  private
    FOldOwnerWindowMethod: TWndMethod;
    FObjectInstance: pointer;
    FManufacturer: string;
    FProduct: string;
    FVersion: string;
    FSubKey: string;
    FSaved: Boolean;
    FMoved: Boolean;
    FEnabled: Boolean;
    FSubclassed: Boolean;
    FSection: string;
    procedure OwnerWindowMethod (var msg: TMessage);
    function GetAppKey: string;
    procedure MoveToPosition;
    function GetPosition: TRect;
    procedure SavePosition;
    function CreateReg(canCreate: Boolean; var reg: TRegistry): Boolean;
    procedure Subclass;
    procedure UnSubClass;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetValue(const valueName: string): Integer;
    function GetSzValue(const valueName: string): string;
    procedure SetValue(const valueName: string; value: Integer);
    procedure SetSzValue(const valueName, value: string);

    property ApplicationKey: string read GetAppKey;
    property Position: TRect read GetPosition;
  published
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Version: string read FVersion write FVersion;
    property Product: string read FProduct write FProduct;
    property SubKey: string read FSubKey write FSubKey;
    property Section: string read FSection write FSection;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

implementation

resourcestring
  rstWoozle = 'Woozle';

{ TPersistentPosition }

{*----------------------------------------------------------------------*
 | constructor TPersistentPosition.Create                               |
 *----------------------------------------------------------------------*}
constructor TPersistentPosition.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FManufacturer := rstWoozle;
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.CreateReg                               |
 |                                                                      |
 | Create a TRegistry object for the application\position               |
 |                                                                      |
 | The function returns false if the registry key couldn't be opened    |
 | - maybe because canCreate was false and it didn't already exist      |
 *----------------------------------------------------------------------*}
function TPersistentPosition.CreateReg(canCreate: Boolean; var reg: TRegistry): Boolean;
var
  attr: DWORD;
begin
  attr := KEY_READ;
  if canCreate then attr := attr or KEY_WRITE;
  reg := TRegistry.Create(attr);
  try
    if FSection = '' then
      Result := reg.OpenKey(ApplicationKey + '\Position', canCreate)
    else
      Result := reg.OpenKey(ApplicationKey + '\Position\' + FSection, canCreate)
  except
    FreeAndNil (reg);
    raise
  end
end;

(*----------------------------------------------------------------------*
 | TPersistentPosition.Destroy                                          |
 |                                                                      |
 | Destructor - Un-subclass main window.                                |
 *----------------------------------------------------------------------*)
destructor TPersistentPosition.Destroy;
begin
  UnSubclass;
  if Assigned(FObjectInstance) then
    Classes.FreeObjectInstance(FObjectInstance);
  inherited;
end;

{*----------------------------------------------------------------------*
 | functionTPersistentPosition.GetAppKey                                |
 |                                                                      |
 | 'Get' method for the ApplicationKey property                         |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetAppKey: string;
var
  prod: string;
begin
  if Product = '' then
    prod := Application.Title
  else
    prod := Product;

  Result := 'Software';
  if Manufacturer <> '' then
    Result := Result + '\' + Manufacturer;

  Result := Result + '\' + Prod;
  if Version <> '' then
    Result := Result + '\' + Version;

  if SubKey <> '' then
    Result := Result + '\' + SubKey
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.GetSzValue                              |
 |                                                                      |
 | Get a string value from the Position area in the registry            |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetSzValue(const valueName: string): string;
var
  reg: TRegistry;
begin
  if CreateReg (false, reg) then
  try
    Result := reg.ReadString(valueName)
  finally
    reg.Free
  end
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.GetValue                                |
 |                                                                      |
 | Get an integer value from the Position area in the registry          |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetValue(const valueName: string): Integer;
var
  reg: TRegistry;
begin
  Result := 0;
  if CreateReg (false, reg) then
  try
    if reg.ValueExists(valueName) then
      Result := reg.ReadInteger(valueName)
  finally
    reg.Free
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.Loaded                                 |
 |                                                                      |
 | Override 'Loaded' to subclass the form's window handle so we can     |
 | intercept it's messages                                              |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    Subclass;
    if Application.MainForm = Nil then
      MoveToPosition
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.MoveToPosition                         |
 |                                                                      |
 | Move to the saved position                                           |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.MoveToPosition;
var
  fm: TForm;
  wp: TWindowPlacement;
  wasVisible: Boolean;
  reg: TRegistry;
begin
  if FMoved or not FEnabled then Exit;
  FMoved := True;
  if Owner is TForm then
  begin
    fm := TForm (Owner);

    if CreateReg (false, reg) then
    try
      try
        FillChar (wp, SizeOf(wp), 0);
        wp.length := SizeOf(wp);

        wasVisible := fm.Visible;
        if wasVisible then
          case TWindowState(reg.ReadInteger ('State')) of
            wsNormal: wp.ShowCmd := SW_SHOW;
            wsMinimized: wp.showCmd := SW_SHOWMINIMIZED;
            wsMaximized: wp.showCmd := SW_SHOWMAXIMIZED
          end
        else
          wp.ShowCmd := 0;
        // You'd be tempted to set ShowCmd to SW_SHOW,
        // SW_SHOWMAXIMIZED, etc.  But that causes a blank
        // window to be shown then filled in after a short
        // delay - which looks ghastly!
        wp.rcNormalPosition.Left := reg.ReadInteger ('Left');
        wp.rcNormalPosition.Top := reg.ReadInteger ('Top');
        wp.rcNormalPosition.Right := reg.ReadInteger ('Width') + wp.rcNormalPosition.Left;
        wp.rcNormalPosition.Bottom := reg.ReadInteger ('Height') + wp.rcNormalPosition.Top;
        SetWindowPlacement(fm.Handle, @wp);
        if not wasVisible then
          fm.WindowState := TWindowState(reg.ReadInteger ('State'));
      except
      end
    finally
      reg.Free
    end
  end
end;

procedure TPersistentPosition.SavePosition;
var
  fm: TForm;
  state: Integer;
  wp: TWindowPlacement;
  reg: TRegistry;
begin
  if FSaved then Exit;
  FSaved := True;

  if FEnabled and (Owner is TForm) then
  begin
    fm := TForm (Owner);
    FillChar (wp, SizeOf(wp), 0);
    wp.length := SizeOf(wp);
    GetWindowPlacement(fm.Handle, @wp);

    if IsIconic (Application.Handle) then
      state := Ord (wsMinimized)
    else
      case wp.showCmd of
        SW_SHOWMINIMIZED: state := Ord (wsMinimized);
        SW_SHOWMAXIMIZED: state := Ord (wsMaximized);
        else
          state := Ord (wsNormal)
      end;

    CreateReg (true, reg);
    with wp.rcNormalPosition do
    try
      reg.WriteInteger ('Left', Left);
      reg.WriteInteger ('Top', Top);
      reg.WriteInteger ('Width', Right - Left);
      reg.WriteInteger ('Height', Bottom - Top);
      reg.WriteInteger ('State', state);
    finally
      reg.Free
    end
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.SetValue                               |
 |                                                                      |
 | Set an integer value in the 'Position' area of the registry          |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.SetValue(const valueName: string;
  value: Integer);
var
  reg: TRegistry;
begin
  if CreateReg (true, reg) then
  try
    reg.WriteInteger(valueName, value)
  finally
    reg.Free
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.SetValue                               |
 |                                                                      |
 | Set a string value in the 'Position' area of the registry            |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.SetSzValue(const valueName, value: string);
var
  reg: TRegistry;
begin
  if CreateReg (true, reg) then
  try
    reg.WriteString(valueName, value)
  finally
    reg.Free
  end
end;

function TPersistentPosition.GetPosition: TRect;
var
  reg: TRegistry;
begin
  if CreateReg (false, reg) then
  try
    try
      Result := Rect(reg.ReadInteger ('Left'), reg.ReadInteger ('Top'), reg.ReadInteger ('Width'), reg.ReadInteger ('Height'));
      Result.Right := Result.Right + Result.Left;
      Result.Bottom := Result.Bottom + Result.Top;
    except
    end
  finally
    reg.Free
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TPersistentPosition.Subclass;
var
  ownerForm: TForm;
begin
  if not FSubclassed then
  begin
    if Owner is TForm then
    begin
      ownerForm := TForm (Owner);
      FOldOwnerWindowMethod := ownerForm.WindowProc;
      ownerForm.WindowProc := OwnerWindowMethod
    end;
    FSubclassed := True
  end
end;

procedure TPersistentPosition.UnSubClass;
var
  ownerForm: TForm;
begin
  if FSubclassed then
  begin
    if Owner is TForm then
    begin
      ownerForm := TForm (Owner);
      ownerForm.WindowProc := FOldOwnerWindowMethod
    end;

    FSubclassed := False
  end
end;

procedure TPersistentPosition.OwnerWindowMethod(var msg: TMessage);
begin
  case Msg.Msg of
    WM_MOVE:
      if not FMoved then
        MoveToPosition;
    WM_DESTROY:
      if csDestroying in ComponentState then
        SavePosition;
  end;

  FOldOwnerWindowMethod (msg)
end;

end.
