unit ResourcePropertiesDialog;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, unitResourceDetails, cmpUCtrls, TntStdCtrls;

type
  TdlgResourceProperties = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    cbLanguage: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    ntedName: TTntEdit;
    procedure FormShow(Sender: TObject);
  private
    function GetLanguage: LCID;
    { Private declarations }
  public
    ResourceDetails : TResourceDetails;

    property Language : LCID read GetLanguage;

    { Public declarations }
  end;

var
  dlgResourceProperties: TdlgResourceProperties;

implementation

{$R *.DFM}

uses unitResourceMessages, ResourceForm;

resourcestring
  rstNeutral = 'Language Neutral';

procedure TdlgResourceProperties.FormShow(Sender: TObject);
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

function TdlgResourceProperties.GetLanguage: LCID;
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

end.
