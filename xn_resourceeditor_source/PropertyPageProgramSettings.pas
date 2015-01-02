unit PropertyPageProgramSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PropertyPageForm, Menus, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls;

type
  TPropertyPageProgramSettingsData = class (TPropertyPageData)
  private
    fInternationalFontName : string;
    fInternationalFontHeight : Integer;
    fParserType : Integer;  // 0 = NT API; 1 = Internal
  protected
    procedure Initialize; override;
  public
    procedure Apply; override;
  end;

  TfmPropertyPageProgramSettings = class(TfmPropertyPage)
    FontDialog1: TFontDialog;
    stFontDetails: TStaticText;
    Button1: TButton;
    Label1: TLabel;
    stModuleParser: TLabel;
    cbModuleParser: TComboBox;
    procedure cbModuleParserChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    fData : TPropertyPageProgramSettingsData;
  protected
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

var
  fmPropertyPageProgramSettings: TfmPropertyPageProgramSettings;

implementation

uses ResourceForm, unitCredProperties;

{$R *.DFM}
{ TfmPropertyPageProgramSettings }

procedure TfmPropertyPageProgramSettings.Button1Click(Sender: TObject);
begin
  FontDialog1.Font.Name := fData.fInternationalFontName;
  FontDialog1.Font.Height := fData.fInternationalFontHeight;
  if FontDialog1.Execute(Handle) then
  begin
    fData.fInternationalFontName := FontDialog1.Font.Name;
    fData.fInternationalFontHeight := FontDialog1.Font.Height;

    stFontDetails.Caption := fData.fInternationalFontName
  end;
end;

procedure TfmPropertyPageProgramSettings.cbModuleParserChange(Sender: TObject);
begin
  fData.fParserType := cbModuleParser.ItemIndex
end;

class function TfmPropertyPageProgramSettings.GetDataClass: TPropertyPageDataClass;
begin
  result := TPropertyPageProgramSettingsData;
end;

procedure TfmPropertyPageProgramSettings.PopulateControls(AData: TPropertyPageData);
begin
  inherited;
  fData := AData as TPropertyPageProgramSettingsData;
  stFontDetails.Caption := fData.fInternationalFontName;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    cbModuleParser.ItemIndex := fData.fParserType
  else
  begin
    cbModuleParser.Visible := False;
    stModuleParser.Visible := False
  end
end;

{ TPropertyPageProgramSettingsData }

procedure TPropertyPageProgramSettingsData.Apply;
begin
  SetInternationalFont (fInternationalFontName, fInternationalFontHeight);
  gProperties.ParserType := fParserType;
end;

procedure TPropertyPageProgramSettingsData.Initialize;
begin
  fInternationalFontName := gProperties.InternationalFontName;
  fInternationalFontHeight := gProperties.InternationalFontHeight;
  fParserType := gProperties.ParserType;
end;

end.
