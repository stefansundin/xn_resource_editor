unit FormTextInput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls;

type
  TfmTextInput = class(TForm)
    mmoText: TTntMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    FontDialog1: TFontDialog;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmTextInput: TfmTextInput;

implementation

{$R *.dfm}

procedure TfmTextInput.Button2Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(mmoText.Font);
  if FontDialog1.Execute then
    mmoText.Font.Assign (FontDialog1.Font)
end;

end.
