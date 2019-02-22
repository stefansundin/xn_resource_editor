unit DescriptionRCDataResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ResourceForm;

type
  TFormRCDataDescriptionResource = class(TFormResource)
    LabelDescription: TLabel;
    EditDescription: TEdit;
    procedure EditDescriptionExit(Sender: TObject);
  private
    procedure SaveResource(const undoDetails : string);
  protected
    procedure SetObject(const Value: TObject); override;
  public
    procedure UpdateFonts; override;
  end;

implementation

uses
  unitResourceRCData;

{$R *.DFM}

resourcestring
  rstChangeDescription = 'change description';

{ TFormRCDataDescriptionResource }

procedure TFormRCDataDescriptionResource.SetObject(const Value: TObject);
var
  Details : TRCDataDescriptionResourceDetails;
begin
  inherited;

  Details := Obj as TRCDataDescriptionResourceDetails;
  EditDescription.Text := Details.Description
end;

procedure TFormRCDataDescriptionResource.UpdateFonts;
begin
  UseInternationalFont(EditDescription.Font);
end;

procedure TFormRCDataDescriptionResource.SaveResource(const undoDetails : string);
var
  Details: TRCDataDescriptionResourceDetails;
begin
  AddUndoEntry(undoDetails);
  Details := Obj as TRCDataDescriptionResourceDetails;
  Details.Description := EditDescription.Text
end;

procedure TFormRCDataDescriptionResource.EditDescriptionExit(Sender: TObject);
begin
  if EditDescription.CanUndo then
    SaveResource(rstChangeDescription);
end;

end.
