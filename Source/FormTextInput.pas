unit FormTextInput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormTextInput = class(TForm)
    mmoText: TMemo;
    ButtonOK: TButton;
    ButtonSelectFont: TButton;
    ButtonCancel: TButton;
    FontDialog: TFontDialog;
    procedure ButtonSelectFontClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

{ TfmTextInput }

procedure TFormTextInput.ButtonSelectFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(mmoText.Font);
  if FontDialog.Execute then
    mmoText.Font.Assign(FontDialog.Font)
end;

end.
