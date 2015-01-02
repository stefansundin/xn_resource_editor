unit AddResourceDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, unitResourceDetails, unitResourceJPEG;

type
  TdlgAddResource = class(TForm)
    ListView1: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    fResourceDetailsClass: TResourceDetailsClass;
    { Private declarations }
  public
    property ResourceDetailsClass : TResourceDetailsClass read fResourceDetailsClass;
    { Public declarations }
  end;

var
  dlgAddResource: TdlgAddResource;

implementation

uses MainForm, unitResourceGraphics, unitResourceMessages, unitResourceDialogs, unitResourceMenus, unitResourceXPManifests, unitResourceGIF, unitResourceVersionInfo, unitResourceToolbar, unitResourceAccelerator, unitResourceExaminer;

{$R *.DFM}

const
  addableItems : array [0..13] of TResourceDetailsClass = (
    TCursorGroupResourceDetails,
    TBitmapResourceDetails,
    TIconGroupResourceDetails,
    TDIBResourceDetails,
    TStringResourceDetails,
    TMessageResourceDetails,
    TMenuResourceDetails,
    TDialogResourceDetails,
    TJPegResourceDetails,
    TGIFResourceDetails,
    TXPManifestResourceDetails,
    TVersionInfoResourceDetails,
    TToolbarResourceDetails,
    TAcceleratorResourceDetails
  );

procedure TdlgAddResource.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := Low (AddableItems) to High (AddableItems) do
    with ListView1.Items.Add do
    begin
      Caption := GetTypeName (AddableItems [i].GetBaseType);
      ImageIndex := GetTypeImage (AddableItems [i].GetBaseType);
    end
end;

procedure TdlgAddResource.btnOKClick(Sender: TObject);
begin
  if Assigned (ListView1.Selected) then
    fResourceDetailsClass := AddableItems [ListView1.Selected.Index]
  else
    fResourceDetailsClass := Nil
end;

procedure TdlgAddResource.ListView1DblClick(Sender: TObject);
begin
  btnOKClick (Self);
  modalResult := mrOK
end;

procedure TdlgAddResource.FormResize(Sender: TObject);
begin
  ListView1.Columns [0].Width := ListView1.Width - 16;
end;

end.
