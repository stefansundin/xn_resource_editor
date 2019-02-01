unit XPManifestResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ResourceForm, unitResourceXPManifests,
  StdCtrls;

type
  TFormXPManifestResource = class(TFormResource)
    Memo: TMemo;
    procedure MemoExit(Sender: TObject);
  private
    FDetails : TXPManifestResourceDetails;
  protected
    function GetCanCopy: Boolean; override;
    function GetCanCut: Boolean; override;
    function GetCanPaste: Boolean; override;
    function GetCanSelectAll : Boolean; override;
    function GetCanDelete: Boolean; override;

  public
    procedure SetObject(const Value: TObject); override;
    procedure Cut; override;
    procedure Copy; override;
    procedure Paste; override;
    procedure SelectAll; override;
    procedure EditDelete; override;
  end;

implementation

{$R *.dfm}

resourcestring
  rstChangeManifest = 'change manifest';

{ TfmXPManifestResource }

procedure TFormXPManifestResource.Copy;
begin
  Memo.CopyToClipboard;
end;

procedure TFormXPManifestResource.Cut;
begin
  Memo.CutToClipboard;
end;

procedure TFormXPManifestResource.EditDelete;
begin
  Memo.SetSelTextBuf('');
end;

function TFormXPManifestResource.GetCanCopy: Boolean;
begin
  Result := Memo.SelLength > 0;
end;

function TFormXPManifestResource.GetCanCut: Boolean;
begin
  Result := Memo.SelLength > 0;
end;

function TFormXPManifestResource.GetCanDelete: Boolean;
begin
  Result := Memo.SelLength > 0;
end;

function TFormXPManifestResource.GetCanPaste: Boolean;
begin
  Result := Memo.SelLength > 0;
end;

function TFormXPManifestResource.GetCanSelectAll: Boolean;
begin
  Result := Memo.SelLength > 0;
end;

procedure TFormXPManifestResource.Paste;
begin
  Memo.PasteFromClipboard;
end;

procedure TFormXPManifestResource.SelectAll;
begin
  Memo.SelectAll;
end;

procedure TFormXPManifestResource.SetObject(const Value: TObject);
begin
  inherited;

  FDetails := obj as TXPManifestResourceDetails;
  Memo.Lines.Text := FDetails.Text;
end;

procedure TFormXPManifestResource.MemoExit(Sender: TObject);
begin
  if Memo.CanUndo then
  begin
    AddUndoEntry(rstChangeManifest);
    FDetails.Text := Memo.Lines.Text;
  end;
end;

end.
