unit DescriptionRCDataResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, StdCtrls, TntStdCtrls;

type
  TfmRCDataDescriptionResource = class(TfmResource)
    Label1: TLabel;
    ntedDescription: TTntEdit;
    procedure ntedDescriptionExit(Sender: TObject);
  private
    procedure SaveResource (const undoDetails : string);
  protected
    procedure SetObject(const Value: TObject); override;
  public
    procedure UpdateFonts; override;
  end;

var
  fmRCDataDescriptionResource: TfmRCDataDescriptionResource;

implementation

uses unitResourceRCData;

{$R *.DFM}

resourcestring
  rstChangeDescription = 'change description';

{ TfmRCDataDescriptionResource }

procedure TfmRCDataDescriptionResource.SetObject(const Value: TObject);
var
  details : TRCDataDescriptionResourceDetails;
begin
  inherited;

  details := Obj as TRCDataDescriptionResourceDetails;
  ntedDescription.Text := details.Description
end;

procedure TfmRCDataDescriptionResource.UpdateFonts;
begin
  UseInternationalFont (ntedDescription.Font);
end;

procedure TfmRCDataDescriptionResource.SaveResource (const undoDetails : string);
var
  details : TRCDataDescriptionResourceDetails;
begin
  AddUndoEntry (undoDetails);
  details := Obj as TRCDataDescriptionResourceDetails;
  details.Description := ntedDescription.Text
end;

procedure TfmRCDataDescriptionResource.ntedDescriptionExit(Sender: TObject);
begin
  if ntedDescription.CanUndo then
    SaveResource (rstChangeDescription);
end;

end.
