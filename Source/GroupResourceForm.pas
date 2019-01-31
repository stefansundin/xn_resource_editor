unit GroupResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, ExtCtrls;

type
  TfmGroupResource = class(TfmResource)
    ScrollBox1: TScrollBox;
    Image1: TImage;
  protected
    procedure SetObject(const Value: TObject); override;
    function GetImportExportType: TImportExportType; override;
    function GetCanCopy : Boolean; override;
  public
    procedure Copy; override;
  end;

implementation

uses
  ClipBrd, unitResourceGraphics;

{$R *.DFM}

{ TfmGroupResource }

procedure TfmGroupResource.Copy;
begin
  Clipboard.Assign(Image1.Picture.Graphic);
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
  details.GetImage(Image1.Picture);
end;

end.
