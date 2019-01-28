unit CloneResourceDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, unitResourceDetails, TntStdCtrls;

type
  TdlgCloneResource = class(TForm)
    cbLanguage: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    rbByName: TRadioButton;
    rbByLanguage: TRadioButton;
    Label1: TLabel;
    ntedName: TTntEdit;
    procedure FormShow(Sender: TObject);
  private
    function GetLanguage: LCID;
  protected
    procedure UpdateActions; override;
    { Private declarations }
  public
    ResourceDetails : TResourceDetails;

    property Language : LCID read GetLanguage;
  end;

var
  dlgCloneResource: TdlgCloneResource;

implementation

{$R *.dfm}

uses unitResourceMessages, ResourceForm;

resourcestring
  rstNeutral = 'Language Neutral';

procedure TdlgCloneResource.FormShow(Sender: TObject);
var
  i : Integer;
  def : string;
begin
  UseInternationalFont (ntedName.Font);
  if Assigned (ResourceDetails) then
  begin
    if resourceDetails is TStringResourceDetails then
      ntedName.Text := ResIdToStringsId (ResourceDetails.ResourceName)
    else
      ntedName.Text := ResourceDetails.ResourceName;
  end;

  cbLanguage.Items.Add ('- ' + rstNeutral);
  def := '-';

  for i := 0 to Languages.Count - 1 do
  begin
    cbLanguage.Items.Add (Languages.Name [i]);
    if Assigned (ResourceDetails) and (ResourceDetails.ResourceLanguage <> 0) and (DWORD (ResourceDetails.ResourceLanguage) = Languages.LocaleID [i]) then
      def := Languages.Name [i];
  end;

  if def = '-' then
    cbLanguage.ItemIndex := 0
  else
    cbLanguage.Text := def;
end;

function TdlgCloneResource.GetLanguage: LCID;
var
  i : Integer;
begin
  Result := 0;
  if cbLanguage.ItemIndex <> 0 then
    for i := 0 to Languages.Count -1 do
      if Languages.Name [i] = cbLanguage.Text then
      begin
        result := Languages.LocaleID [i];
        break
      end
end;

procedure TdlgCloneResource.UpdateActions;
begin
  ntedName.Enabled := rbByName.Checked;
  cbLanguage.Enabled := rbByLanguage.Checked;

end;

end.
