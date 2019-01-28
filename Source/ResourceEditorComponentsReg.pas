unit ResourceEditorComponentsReg;

interface

procedure Register;

implementation

uses
  Classes,
  cmpBitmapEditor,
  cmpMenuDesigner,
  cmpDialogBox,
  cmpDialogEditor;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [
    TBitmapEditor,
    TMenuDesigner,
    TPopupMenuDesigner,
    TDialogBox,
    TDialogEditor
  ])
end;

end.
