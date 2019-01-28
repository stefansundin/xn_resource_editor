unit PropertiesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PropertyBaseForm, Menus, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls;

type
  TfmProperties = class(TfmPropertyBase)
    procedure FormDestroy(Sender: TObject);
  private
  protected
  public
    constructor Create (AOwner : TComponent); override;
  end;

var
  fmProperties: TfmProperties;

implementation

uses unitCredProperties, PropertyPageProgramSettings, PropertyPageRCSettings;

{$R *.DFM}
{ TfmProperties }

constructor TfmProperties.Create(AOwner: TComponent);
begin
  inherited;

  gProperties.BeginUpdate;
  AddPropertyPageDetails (TfmPropertyPageProgramSettings, Nil);
  AddPropertyPageDetails (TfmPropertyPageRCSettings, Nil);
end;

procedure TfmProperties.FormDestroy(Sender: TObject);
begin
  inherited;
  gProperties.EndUpdate
end;

end.
