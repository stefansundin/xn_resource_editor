unit PropertyPageProgramSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ExtCtrls, VirtualTrees, PropertyPageForm,
  cmpPersistentPosition;

type
  TPropertyPageProgramSettingsData = class (TPropertyPageData)
  private
    FInternationalFontName : string;
    FInternationalFontHeight : Integer;
    FParserType : Integer;  // 0 = NT API; 1 = Internal
  protected
    procedure Initialize; override;
  public
    procedure Apply; override;
  end;

  TFormPropertyPageProgramSettings = class(TFormPropertyPage)
    FontDialog: TFontDialog;
    StaticTextFontDetails: TStaticText;
    ButtonSelectFont: TButton;
    LabelSelectFont: TLabel;
    StaticTextModuleParser: TLabel;
    ComboBoxModuleParser: TComboBox;
    procedure ComboBoxModuleParserChange(Sender: TObject);
    procedure ButtonSelectFontClick(Sender: TObject);
  private
    FData : TPropertyPageProgramSettingsData;
  public
    class function GetDataClass : TPropertyPageDataClass; override;
    procedure PopulateControls (AData : TPropertyPageData); override;
  end;

implementation

uses
  ResourceForm, unitCredProperties;

{$R *.DFM}

{ TFormPropertyPageProgramSettings }

procedure TFormPropertyPageProgramSettings.ButtonSelectFontClick(Sender: TObject);
begin
  FontDialog.Font.Name := FData.FInternationalFontName;
  FontDialog.Font.Height := FData.FInternationalFontHeight;
  if FontDialog.Execute(Handle) then
  begin
    FData.FInternationalFontName := FontDialog.Font.Name;
    FData.FInternationalFontHeight := FontDialog.Font.Height;

    StaticTextFontDetails.Caption := FData.FInternationalFontName
  end;
end;

procedure TFormPropertyPageProgramSettings.ComboBoxModuleParserChange(Sender: TObject);
begin
  FData.FParserType := ComboBoxModuleParser.ItemIndex
end;

class function TFormPropertyPageProgramSettings.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageProgramSettingsData;
end;

procedure TFormPropertyPageProgramSettings.PopulateControls(AData: TPropertyPageData);
begin
  inherited;
  FData := AData as TPropertyPageProgramSettingsData;
  StaticTextFontDetails.Caption := FData.FInternationalFontName;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    ComboBoxModuleParser.ItemIndex := FData.FParserType
  else
  begin
    ComboBoxModuleParser.Visible := False;
    StaticTextModuleParser.Visible := False;
  end
end;

{ TPropertyPageProgramSettingsData }

procedure TPropertyPageProgramSettingsData.Apply;
begin
  SetInternationalFont(FInternationalFontName, FInternationalFontHeight);
  gProperties.ParserType := FParserType;
end;

procedure TPropertyPageProgramSettingsData.Initialize;
begin
  FInternationalFontName := gProperties.InternationalFontName;
  FInternationalFontHeight := gProperties.InternationalFontHeight;
  FParserType := gProperties.ParserType;
end;

end.
