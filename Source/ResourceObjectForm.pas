(*======================================================================*
 | ResourceObjectForm                                                   |
 |                                                                      |
 | Ultimate ancestor of object editor forms.  Note that resource        |
 | editor forms are derived from TFormResource, which is derived        |
 | from this form.                                                      |
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, unitResourceDetails;

type
//=======================================================================
// TFormResourceObject class

  TFormResourceObject = class(TForm)
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

  TResourceObjectFormClass = class of TFormResourceObject;

var
  fmResourceObject: TFormResourceObject;

implementation

{$R *.DFM}

{ TFormResourceObject }

(*----------------------------------------------------------------------*
 | TFormResourceObject.SetObject                                          |
 |                                                                      |
 | 'Set' method for the 'Object' property                               |
 *----------------------------------------------------------------------*)
procedure TFormResourceObject.SetObject(const Value: TObject);
begin
  FObject := Value;
  UpdateFonts;
end;

(*----------------------------------------------------------------------*
 | TFormResourceObject.FormClose                                          |
 |                                                                      |
 | OnClose handler for the form.  Auto-free.                            |
 *----------------------------------------------------------------------*)
procedure TFormResourceObject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

(*----------------------------------------------------------------------*
 | TFormResourceObject.FormDestroy                                        |
 |                                                                      |
 | OnDestroy handler for the form.  Set the fmresourceObject global     |
 | variable to Nil                                                      |
 *----------------------------------------------------------------------*)
procedure TFormResourceObject.FormDestroy(Sender: TObject);
begin
  fmResourceObject := nil
end;

(*----------------------------------------------------------------------*
 | TFormResourceObject.GetMenuItem                                        |
 |                                                                      |
 | Stub function for editor forms that have a main menu item.           |
 *----------------------------------------------------------------------*)
function TFormResourceObject.GetMenuItem: TMenuItem;
begin
  Result := nil
end;

procedure TFormResourceObject.PreviewKey(var key: Word; shift: TShiftState);
begin

end;

procedure TFormResourceObject.TidyUp;
begin
//
end;

procedure TFormResourceObject.UpdateFonts;
begin
// Stub
end;

end.
