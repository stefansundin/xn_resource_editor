unit IconGraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IconCursorGraphicsResourceForm, Menus, ActnList, ImgList,
  ComCtrls, cmpColorSelector, ExtCtrls, ToolWin, cmpSizingPageControl,
  cmpBitmapEditor, cmpPropertyListBox, System.Actions, System.ImageList;

type
  TfmIconGraphicsResource = class(TfmIconCursorGraphicsResource)
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TfmIconGraphicsResource.FormCreate(Sender: TObject);
begin
  inherited;

  with TPropertyListProperty(PropertyListBox1.Properties.Add) do
  begin
    PropertyName := 'Hot Spot Left';
    PropertyType := ptInteger;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
  with TPropertyListProperty(PropertyListBox1.Properties.Add) do
  begin
    PropertyName := 'Hot Spot Top';
    PropertyType := ptInteger;
    ParentColor := False;
    Color := clBlack;
    ReadOnly := False;
  end;
end;

end.
