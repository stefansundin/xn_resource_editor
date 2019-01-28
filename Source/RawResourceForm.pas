unit RawResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, cmpHexDump, unitResourceDetails;

type
  TfmRawResource = class(TfmResource)
    HexDump1: THexDump;
  private
  protected
    procedure SetObject(const Value: TObject); override;
  public
    { Public declarations }
  end;

var
  fmRawResource: TfmRawResource;

implementation

{$R *.DFM}

{ TfmRawResource }

procedure TfmRawResource.SetObject(const Value: TObject);
var
  details : TResourceDetails;
begin
  inherited;

  details := obj as TResourceDetails;

  HexDump1.Address := details.Data.Memory;
  HexDump1.DataSize := details.Data.Size
end;

end.
