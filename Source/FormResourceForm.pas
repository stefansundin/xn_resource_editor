unit FormResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, StdCtrls, unitResourceRCData;

type
  TFormRCDataFormResource = class(TFormResource)
    Memo: TMemo;
    procedure MemoExit(Sender: TObject);
  private
    FDetails: TRCDataFormResourceDetails;
    procedure SaveResource(const undoDetails : string);
  public
    procedure SetObject(const Value: TObject); override;
  end;

implementation

{$R *.DFM}

resourcestring
  rstFormChange = 'form change';

{ TFormRCDataFormResource }

procedure TFormRCDataFormResource.SetObject(const Value: TObject);
begin
  inherited;

  FDetails := obj as TRCDataFormResourceDetails;

  Memo.Text := FDetails.Text
end;

procedure TFormRCDataFormResource.MemoExit(Sender: TObject);
begin
  inherited;

  if Memo.CanUndo then
   SaveResource(rstFormChange);
end;

procedure TFormRCDataFormResource.SaveResource(const undoDetails: string);
begin
  AddUndoEntry(undoDetails);
  FDetails.Text := Memo.Text
end;

end.
