unit IconGraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, ImgList, ComCtrls, Actions, ImageList, ExtCtrls, ToolWin,
  IconCursorGraphicsResourceForm, ComponentColorSelector,
  ComponentSizingPageControl, ComponentBitmapEditor,
  ComponentPropertyListBox;

type
  TFormIconGraphicsResource = class(TFormIconCursorGraphicsResource)
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TFormIconGraphicsResource.FormCreate(Sender: TObject);
begin
  inherited;

  with TPropertyListProperty(PropertyListBox.Properties.Add) do
  begin
    PropertyName := 'Hot Spot Left';
    PropertyType := ptInteger;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
  with TPropertyListProperty(PropertyListBox.Properties.Add) do
  begin
    PropertyName := 'Hot Spot Top';
    PropertyType := ptInteger;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
end;

end.
