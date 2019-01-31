unit XPManifestResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ResourceForm, unitResourceXPManifests,
  StdCtrls;

type
  TfmXPManifestResource = class(TfmResource)
    Memo1: TMemo;
    procedure Memo1Exit(Sender: TObject);
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

var
  fmXPManifestResource: TfmXPManifestResource;

implementation

{$R *.dfm}

resourcestring
  rstChangeManifest = 'change manifest';
{ TfmXPManifestResource }

procedure TfmXPManifestResource.Copy;
begin
  Memo1.CopyToClipboard;
end;

procedure TfmXPManifestResource.Cut;
begin
  Memo1.CutToClipboard
end;

procedure TfmXPManifestResource.EditDelete;
begin
  Memo1.SetSelTextBuf('');
end;

function TfmXPManifestResource.GetCanCopy: Boolean;
begin
  Result := Memo1.SelLength > 0
end;

function TfmXPManifestResource.GetCanCut: Boolean;
begin
  Result := Memo1.SelLength > 0
end;

function TfmXPManifestResource.GetCanDelete: Boolean;
begin
  Result := Memo1.SelLength > 0
end;

function TfmXPManifestResource.GetCanPaste: Boolean;
begin
  Result := Memo1.SelLength > 0
end;

function TfmXPManifestResource.GetCanSelectAll: Boolean;
begin
  Result := Memo1.SelLength > 0
end;

procedure TfmXPManifestResource.Paste;
begin
  Memo1.PasteFromClipboard
end;

procedure TfmXPManifestResource.SelectAll;
begin
  Memo1.SelectAll
end;

procedure TfmXPManifestResource.SetObject(const Value: TObject);
begin
  inherited;

  FDetails := obj as TXPManifestResourceDetails;
  Memo1.Lines.Text := FDetails.Text;
end;

procedure TfmXPManifestResource.Memo1Exit(Sender: TObject);
begin
  if Memo1.CanUndo then
  begin
    AddUndoEntry (rstChangeManifest);
    FDetails.Text := Memo1.Lines.Text
  end
end;

end.
