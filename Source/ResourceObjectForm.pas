(*======================================================================*
 | ResourceObjectForm                                                   |
 |                                                                      |
 | Ultimate ancestor of object editor forms.  Note that resource editor |
 | forms are derived from TfmResource, which is derived from this form. |
 |                                                                      |
 | * Gold code.                                                         |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 *======================================================================*)

unit ResourceObjectForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, unitResourceDetails, Menus;

type
//=======================================================================
// TfmResourceObject class

  TfmResourceObject = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FObject: TObject;
    FResourceModule: TResourceModule;
  protected
    procedure SetObject(const Value: TObject); virtual;
    function GetMenuItem: TMenuItem; virtual;
  public
    procedure PreviewKey(var key: Word; shift: TShiftState); virtual;
    procedure TidyUp; virtual;
    procedure UpdateFonts; virtual;

    property ResourceModule: TResourceModule read FResourceModule write FResourceModule;
    property Obj: TObject read FObject write SetObject;
    property Menu: TMenuItem read GetMenuItem;
  end;

  TResourceObjectFormClass = class of TfmResourceObject;

var
  fmResourceObject: TfmResourceObject;

implementation

{$R *.DFM}

{ TfmResourceObject }

(*----------------------------------------------------------------------*
 | TfmResourceObject.SetObject                                          |
 |                                                                      |
 | 'Set' method for the 'Object' property                               |
 *----------------------------------------------------------------------*)
procedure TfmResourceObject.SetObject(const Value: TObject);
begin
  FObject := Value;
  UpdateFonts;
end;

(*----------------------------------------------------------------------*
 | TfmResourceObject.FormClose                                          |
 |                                                                      |
 | OnClose handler for the form.  Auto-free.                            |
 *----------------------------------------------------------------------*)
procedure TfmResourceObject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

(*----------------------------------------------------------------------*
 | TfmResourceObject.FormDestroy                                        |
 |                                                                      |
 | OnDestroy handler for the form.  Set the fmresourceObject global     |
 | variable to Nil                                                      |
 *----------------------------------------------------------------------*)
procedure TfmResourceObject.FormDestroy(Sender: TObject);
begin
  fmResourceObject := nil
end;

(*----------------------------------------------------------------------*
 | TfmResourceObject.GetMenuItem                                        |
 |                                                                      |
 | Stub function for editor forms that have a main menu item.           |
 *----------------------------------------------------------------------*)
function TfmResourceObject.GetMenuItem: TMenuItem;
begin
  Result := nil
end;

procedure TfmResourceObject.PreviewKey(var key: Word; shift: TShiftState);
begin

end;

procedure TfmResourceObject.TidyUp;
begin
//
end;

procedure TfmResourceObject.UpdateFonts;
begin
// Stub
end;

end.
