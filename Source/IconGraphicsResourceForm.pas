unit IconGraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IconCursorGraphicsResourceForm, Menus, ActnList, ImgList,
  ComCtrls, cmpColorSelector, ExtCtrls, ToolWin, cmpSizingPageControl,
  cmpBitmapEditor, cmpPropertyListBox;

type
  TfmIconGraphicsResource = class(TfmIconCursorGraphicsResource)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmIconGraphicsResource: TfmIconGraphicsResource;

implementation

{$R *.dfm}

end.
