unit PropertiesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PropertyBaseForm, Menus, cmpPersistentPosition, StdCtrls, VirtualTrees,
  ExtCtrls;

type
  TFormProperties = class(TFormPropertyBase)
    procedure FormDestroy(Sender: TObject);
  public
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses
  unitCredProperties, PropertyPageProgramSettings, PropertyPageRCSettings;

{$R *.DFM}

{ TFormProperties }

constructor TFormProperties.Create(AOwner: TComponent);
begin
  inherited;

  gProperties.BeginUpdate;
  AddPropertyPageDetails (TFormPropertyPageProgramSettings, nil);
  AddPropertyPageDetails (TFormPropertyPageRCSettings, nil);
end;

procedure TFormProperties.FormDestroy(Sender: TObject);
begin
  inherited;
  gProperties.EndUpdate
end;

end.
