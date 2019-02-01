unit RawResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, ComponentHexDump, unitResourceDetails;

type
  TFormRawResource = class(TFormResource)
    HexDump: THexDump;
  protected
    procedure SetObject(const Value: TObject); override;
  end;

implementation

{$R *.DFM}

{ TfmRawResource }

procedure TFormRawResource.SetObject(const Value: TObject);
var
  Details: TResourceDetails;
begin
  inherited;

  Details := obj as TResourceDetails;

  HexDump.Address := Details.Data.Memory;
  HexDump.DataSize := Details.Data.Size
end;

end.
