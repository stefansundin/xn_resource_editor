unit GroupResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, ExtCtrls;

type
  TfmGroupResource = class(TfmResource)
    ScrollBox1: TScrollBox;
    Image1: TImage;
  private
    { Private declarations }
  protected
    procedure SetObject(const Value: TObject); override;
    function GetImportExportType: TImportExportType; override;
    function GetCanCopy : Boolean; override;
  public
    procedure Copy; override;
  end;

var
  fmGroupResource: TfmGroupResource;

implementation

uses unitResourceGraphics, ClipBrd;

{$R *.DFM}

{ TfmGroupResource }

procedure TfmGroupResource.Copy;
begin
  Clipboard.Assign (Image1.Picture.Graphic);
end;

function TfmGroupResource.GetCanCopy: Boolean;
begin
  Result := True
end;

function TfmGroupResource.GetImportExportType: TImportExportType;
begin
  Result := ixPicture
end;

procedure TfmGroupResource.SetObject(const Value: TObject);
var
  details : TIconCursorGroupResourceDetails;
begin
  inherited;
  
  details := obj as TIconCursorGroupResourceDetails;
  details.GetImage (Image1.Picture);
end;

end.
