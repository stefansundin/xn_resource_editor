unit AddResourceDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, unitResourceDetails, unitResourceJPEG;

type
  TdlgAddResource = class(TForm)
    ListView: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FResourceDetailsClass: TResourceDetailsClass;
  public
    property ResourceDetailsClass : TResourceDetailsClass read FResourceDetailsClass;
  end;

implementation

uses
  MainForm, unitResourceGraphics, unitResourceMessages, unitResourceDialogs,
  unitResourceMenus, unitResourceXPManifests, unitResourceGIF,
  unitResourceVersionInfo, unitResourceToolbar, unitResourceAccelerator,
  unitResourceExaminer;

{$R *.DFM}

const
  CAddableItems : array [0..13] of TResourceDetailsClass = (
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
  i: Integer;
begin
  for i := Low(CAddableItems) to High(CAddableItems) do
    with ListView.Items.Add do
    begin
      Caption := GetTypeName(CAddableItems [i].GetBaseType);
      ImageIndex := GetTypeImage(CAddableItems [i].GetBaseType);
    end
end;

procedure TdlgAddResource.btnOKClick(Sender: TObject);
begin
  if Assigned(ListView.Selected) then
    FResourceDetailsClass := CAddableItems[ListView.Selected.Index]
  else
    FResourceDetailsClass := Nil
end;

procedure TdlgAddResource.ListViewDblClick(Sender: TObject);
begin
  btnOKClick(Self);
  modalResult := mrOK;
end;

procedure TdlgAddResource.FormResize(Sender: TObject);
begin
  ListView.Columns [0].Width := ListView.Width - 16;
end;

end.
