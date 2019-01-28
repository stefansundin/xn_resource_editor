unit FormResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, StdCtrls, unitResourceRCData;

type
  TfmRCDataFormResource = class(TfmResource)
    Memo1: TMemo;
    procedure Memo1Exit(Sender: TObject);
  private
    fDetails : TRCDataFormResourceDetails;
    procedure SaveResource (const undoDetails : string);
  public
    procedure SetObject(const Value: TObject); override;
  end;

var
  fmRCDataFormResource: TfmRCDataFormResource;

implementation

{$R *.DFM}

resourcestring
  rstFormChange = 'form change';

{ TfmRCDataFormResource }

procedure TfmRCDataFormResource.SetObject(const Value: TObject);
begin
  inherited;

  fDetails := obj as TRCDataFormResourceDetails;

  Memo1.Text := fDetails.Text
end;

procedure TfmRCDataFormResource.Memo1Exit(Sender: TObject);
begin
  inherited;

  if Memo1.CanUndo then
   SaveResource (rstFormChange);
end;

procedure TfmRCDataFormResource.SaveResource(const undoDetails: string);
begin
  AddUndoEntry (undoDetails);
  fDetails.Text := Memo1.Text
end;

end.
