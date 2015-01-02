unit PropertyPageForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TPropertyPageData = class
  private
    fCaption: string;
    fHelpText: string;
    fHelpKeyword : string;
    fParam: Integer;
    fMinX : Integer;
    fMinY : Integer;
    fInitialized : boolean;
  protected

    property Param : Integer read fParam;
    procedure Initialize; virtual; abstract;
    function GetCaption: string; virtual;
    function GetHelpText: string; virtual;
  public
    constructor Create (const ACaption, AHelpText, AHelpKeyword : string; AMinCX, AMinCY : Integer; AParam : Integer = 0);
    procedure Apply; virtual;
    procedure Cancel; virtual;

    property Caption : string read GetCaption;
    property Initialized : boolean read fInitialized write fInitialized;
    property HelpText : string read GetHelpText;
    property HelpKeyword : string read fHelpKeyword;

    property MinX : Integer read fMinX;
    property MinY : Integer read fMinY;
  end;

  TPropertyPageDataClass = class of TPropertyPageData;

  TfmPropertyPage = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    stSectionDetails: TLabel;
  private
    fAltKeyword : string;
  protected
    fPopulating : boolean;
  public
    class function GetDataClass : TPropertyPageDataClass; virtual; abstract;
    procedure PopulateControls (AData : TPropertyPageData); virtual;
    property Populating : boolean read fPopulating write fPopulating;
    property AltKeyword : string read fAltKeyword;
  end;

  TPropertyPageClass = class of TfmPropertyPage;

var
  fmPropertyPage: TfmPropertyPage;

implementation

{$R *.dfm}

{ TfmPropertyPage }

procedure TfmPropertyPage.PopulateControls (AData : TPropertyPageData);
begin
  if not AData.fInitialized then
  begin
    AData.Initialize;
    AData.fInitialized := True
  end;
  stSectionDetails.Caption := AData.HelpText;
  fAltKeyword := AData.HelpKeyword;
end;

{ TPropertyPageData }

procedure TPropertyPageData.Apply;
begin
// Stub
end;

procedure TPropertyPageData.Cancel;
begin
// Stub
end;

constructor TPropertyPageData.Create(const ACaption, AHelpText, AHelpKeyword: string; AMinCX, AMinCY : Integer;
  AParam: Integer);
begin
  fCaption := ACaption;
  fHelpText := AHelpText;
  fHelpKeyword := AHelpKeyword;
  fParam := AParam;
  fMiNX := AMinCX;
  fMinY := AMinCY;
end;

function TPropertyPageData.GetCaption: string;
begin
  result := fCaption
end;

function TPropertyPageData.GetHelpText: string;
begin
  result := fHelpText
end;

end.
