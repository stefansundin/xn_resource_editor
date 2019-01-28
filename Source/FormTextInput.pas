unit FormTextInput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfmTextInput = class(TForm)
    mmoText: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    FontDialog1: TFontDialog;
    procedure Button2Click(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TfmTextInput.Button2Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(mmoText.Font);
  if FontDialog1.Execute then
    mmoText.Font.Assign (FontDialog1.Font)
end;

end.
