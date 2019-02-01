unit CloneResourceDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, unitResourceDetails;

type
  TDialogCloneResource = class(TForm)
    ComboBoxLanguage: TComboBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    RadioButtonByName: TRadioButton;
    RadioButtonByLanguage: TRadioButton;
    LabelDescription: TLabel;
    EditName: TEdit;
    procedure FormShow(Sender: TObject);
  private
    function GetLanguage: LCID;
  protected
    procedure UpdateActions; override;
  public
    ResourceDetails: TResourceDetails;

    property Language: LCID read GetLanguage;
  end;

implementation

{$R *.dfm}

uses
  unitResourceMessages, ResourceForm;

resourcestring
  rstNeutral = 'Language Neutral';

procedure TDialogCloneResource.FormShow(Sender: TObject);
var
  i: Integer;
  def: string;
begin
  UseInternationalFont(EditName.Font);
  if Assigned(ResourceDetails) then
  begin
    if ResourceDetails is TStringResourceDetails then
      EditName.Text := ResIdToStringsId (ResourceDetails.ResourceName)
    else
      EditName.Text := ResourceDetails.ResourceName;
  end;

  ComboBoxLanguage.Items.Add ('- ' + rstNeutral);
  def := '-';

  for i := 0 to Languages.Count - 1 do
  begin
    ComboBoxLanguage.Items.Add (Languages.Name [i]);
    if Assigned(ResourceDetails) and (ResourceDetails.ResourceLanguage <> 0) and (DWORD (ResourceDetails.ResourceLanguage) = Languages.LocaleID [i]) then
      def := Languages.Name [i];
  end;

  if def = '-' then
    ComboBoxLanguage.ItemIndex := 0
  else
    ComboBoxLanguage.Text := def;
end;

function TDialogCloneResource.GetLanguage: LCID;
var
  i: Integer;
begin
  Result := 0;
  if ComboBoxLanguage.ItemIndex <> 0 then
    for i := 0 to Languages.Count -1 do
      if Languages.Name [i] = ComboBoxLanguage.Text then
      begin
        Result := Languages.LocaleID [i];
        Break
      end
end;

procedure TDialogCloneResource.UpdateActions;
begin
  EditName.Enabled := RadioButtonByName.Checked;
  ComboBoxLanguage.Enabled := RadioButtonByLanguage.Checked;
end;

end.
