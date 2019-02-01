unit GroupResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ResourceForm;

type
  TFormGroupResource = class(TFormResource)
    ScrollBox: TScrollBox;
    Image: TImage;
  protected
    procedure SetObject(const Value: TObject); override;
    function GetImportExportType: TImportExportType; override;
    function GetCanCopy : Boolean; override;
  public
    procedure Copy; override;
  end;

implementation

uses
  Clipbrd, unitResourceGraphics;

{$R *.DFM}

{ TfmGroupResource }

procedure TFormGroupResource.Copy;
begin
  Clipboard.Assign(Image.Picture.Graphic);
end;

function TFormGroupResource.GetCanCopy: Boolean;
begin
  Result := True;
end;

function TFormGroupResource.GetImportExportType: TImportExportType;
begin
  Result := ixPicture;
end;

procedure TFormGroupResource.SetObject(const Value: TObject);
var
  Details: TIconCursorGroupResourceDetails;
begin
  inherited;
  
  Details := obj as TIconCursorGroupResourceDetails;
  Details.GetImage(Image.Picture);
end;

end.
