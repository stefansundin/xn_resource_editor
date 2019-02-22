(*======================================================================*
 | DialogResourceForm                                                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      14/03/2001  CPWW  Original                                  |
 *======================================================================*)

unit DialogResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ImgList, ComCtrls, ToolWin, StdCtrls, ExtCtrls, ImageList,
  ResourceForm, ComponentSizingPageControl, ComponentDialogBox, ComponentRuler,
  ComponentPropertyListBox, ComponentDialogEditor, unitResourceDialogs;

type
  TFormDialogResource = class(TFormResource)
    ComboBoxControls: TComboBox;
    DialogEditor: TDialogEditor;
    FontDialog: TFontDialog;
    ImageList: TImageList;
    PanelInnerPalette: TPanel;
    PanelPalette: TPanel;
    PanelProperties: TPanel;
    PanelPropertyCombo: TPanel;
    PanelRulerLeft: TPanel;
    PanelRulerTop: TPanel;
    PropertyListBox: TPropertyListBox;
    RulerLeft: TRuler;
    RulerTop: TRuler;
    ScrollBox: TScrollBox;
    SizingPageControl: TSizingPageControl;
    Splitter: TSplitter;
    TabControlPropertyKind: TTabControl;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButtonCheckBox: TToolButton;
    ToolButtonCursor: TToolButton;
    ToolButtonFont: TToolButton;
    ToolButtonImage: TToolButton;
    ToolButtonRadioButton: TToolButton;
    ToolButtonTable: TToolButton;
    ToolButtonTabs: TToolButton;
    ToolButtonText: TToolButton;
    ToolButtonTrackbar: TToolButton;
    ToolButtonVideo: TToolButton;
    procedure DialogEditorResize(Sender: TObject);
    procedure DialogEditorDesignModeSelectedItemChange(Sender: TObject);
    procedure DialogEditorShow(Sender: TObject);
    procedure ComboBoxControlsChange(Sender: TObject);
    procedure PropertyListBoxPropertyChanged(Sender: TObject);
    procedure TabControlPropertyKindChange(Sender: TObject);
    procedure DialogEditorControlResize(Sender: TObject;
      ctrlInfo: TControlInfo; newRect: TRect);
    procedure DialogEditorControlPropertyChange(Sender: TObject;
      ctrlInfo: TControlInfo);
    procedure FormShow(Sender: TObject);
    procedure SizingPageControlUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure SizingPageControlDockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure ToolButtonImageClick(Sender: TObject);
    procedure DialogEditorDesignModeDropControl(Sender: TObject; x,
      y: Integer; Ctrl: TControlInfo);
    procedure DialogEditorDeleteControl(Sender: TObject;
      Ctrl: TControlInfo);
    procedure ToolButtonCursorClick(Sender: TObject);
    procedure DialogEditorGetControlImage(Sender: TObject; tp: Integer;
      const id: string; var Handle: HGDIOBJ);

    procedure DoSpecialButtonClick(Sender: TObject);
  private
    FDetails: TDialogResourceDetails;
    FPCWidth: Integer;

    procedure FillPropertyBox (info: TControlInfo; reset: Boolean);
    procedure SaveResource(const UndoDetails: string);

  protected
    procedure SetObject(const Value: TObject); override;
    function GetCanDelete: Boolean; override;
  public
    procedure EditDelete; override;
    procedure UpdateFonts; override;
  end;

implementation

uses
  Variants, unitResourceDetails, unitResourceGraphics, unitExIcon,
  unitResourceMenus;

resourcestring
  rstChangeProperty = 'change %s';
  rstResize = 'resize';
  rstAddControl = 'add control';
  rstDeleteControl = 'delete control';
  rstSetFont = 'set font';

{$R *.DFM}

{ TFormDialogResource }

(*----------------------------------------------------------------------*
 | TFormDialogResource.SetObject                                          |
 |                                                                      |
 | Set to display a new dialog object                                   |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.SetObject(const Value: TObject);
begin
  inherited;
  FDetails := obj as TDialogResourceDetails;

  DialogEditor.ResourceTemplate := FDetails.Data.Memory;
  TabControlPropertyKind.TabIndex := 0
end;


(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1Resize                                |
 |                                                                      |
 | Dialog box has been resized.  Adjust the rulers                      |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditorResize(Sender: TObject);
begin
  inherited;

  RulerTop.Width := DialogEditor.Width;
  RulerLeft.Height := DialogEditor.Height;
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1DesignModeSelectedItemChange          |
 |                                                                      |
 | A control has been selected.   Find the control in the 'Controls'    |
 | combo, and set the properties to match the contol's                  |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditorDesignModeSelectedItemChange(
  Sender: TObject);
var
  info: TControlInfo;
  i: Integer;
begin
  info := DialogEditor.SelectedControl;

  if Assigned(info) then
  begin
    for i := 0 to ComboBoxControls.Items.Count - 1 do
      if info = TControlInfo(ComboBoxControls.Items.Objects[i]) then
      begin
        ComboBoxControls.ItemIndex := i;
        ComboBoxControls.Hint := ComboBoxControls.Text;
        break
      end;

    FillPropertyBox (info, True)
  end
end;

(*----------------------------------------------------------------------*
 | GetInfoDescription                                                   |
 |                                                                      |
 | Get the description for a control to display in the 'controls'       |
 | combo.                                                               |
 *----------------------------------------------------------------------*)
function GetInfoDescription (info: TControlInfo): string;
var
  idx: Integer;
begin
  idx := Info.FindProperty(pkGeneral, 'Text');
  if idx = -1 then
    idx := Info.FindProperty(pkGeneral, 'Caption');

  if idx = -1 then
    Result := Format('%d %s', [info.ItemID, info.GetDescription])
  else
    Result := Format('%d %s "%s"', [info.ItemID, info.GetDescription, info.PropertyValue [pkGeneral, idx]])
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1Show                                  |
 |                                                                      |
 | OnShow handler for the dialog editor.  Called when the dialog editor |
 | has created the dialog box, so we can read it's info and initialize  |
 | accordingly.  NB  We can't do this in 'SetObject' - the dialog box   |
 | hasn't been created then.                                            |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditorShow(Sender: TObject);
var
  i: Integer;
  info: TControlInfo;
begin
  RulerTop.DialogBox := DialogEditor.DialogHandle;
  RulerLeft.DialogBox := DialogEditor.DialogHandle;

  ComboBoxControls.Items.Clear;               // Fill in the Controls combo
  ComboBoxControls.Items.BeginUpdate;
  try
    info := DialogEditor.DialogInfo;
    ComboBoxControls.Items.AddObject(GetInfoDescription (info), info);

    for i := 0 to DialogEditor.ControlInfoCount - 1 do
    begin
      info := DialogEditor.ControlInfo [i];
      ComboBoxControls.Items.AddObject(GetInfoDescription (info), info);
    end
  finally
    ComboBoxControls.Items.EndUpdate
  end;

  ComboBoxControls.ItemIndex := 0;
  ComboBoxControls.Hint := ComboBoxControls.Text;
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.cbControlsChange                                   |
 |                                                                      |
 | Select a control, based on the controls combo                        |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.ComboBoxControlsChange(Sender: TObject);
var
  info: TControlInfo;
begin
  info := TControlInfo (ComboBoxControls.Items.Objects[ComboBoxControls.ItemIndex]);
  ComboBoxControls.Hint := ComboBoxControls.Text;
  DialogEditor.SelectedControl := info
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.PropertyListBox1PropertyChanged                    |
 |                                                                      |
 | A property has been changed in the property list box.  Update the    |
 | dialog editor.                                                       |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.PropertyListBoxPropertyChanged(
  Sender: TObject);
var
  ControlInfo: TControlInfo;
  Prop: TPropertyListProperty;
  Index: Integer;
  Kind: TPropertyKind;
  PropertyName: string;
begin
  ControlInfo := TControlInfo (ComboBoxControls.Items.Objects[ComboBoxControls.ItemIndex]);

  Prop := PropertyListBox.Properties[PropertyListBox.SelectedPropertyNo];


  case TabControlPropertyKind.TabIndex of
    1:
      Kind := pkStyle;
    2:
      Kind := pkExtended;
    else
      Kind := pkGeneral;
  end;

  PropertyName := Prop.PropertyName;
  Index := ControlInfo.FindProperty(Kind, PropertyName);
  ControlInfo.PropertyValue [Kind, Index] := Prop.PropertyValue;  // nb. May make 'prop' invalid!!

  PropertyName := Format(rstChangeProperty, [PropertyName]);

  Index := ComboBoxControls.ItemIndex;
  ComboBoxControls.Items[Index] := GetInfoDescription (ControlInfo);
  ComboBoxControls.ItemIndex := Index;

  SaveResource(PropertyName);
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.FillPropertyBox                                    |
 |                                                                      |
 | Fill the property list box with properties and their values for the  |
 | control.                                                             |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.FillPropertyBox(info: TControlInfo; reset: Boolean);
var
  Kind: TPropertyKind;
  i, j: Integer;
  Prop: TPropertyListProperty;
  PropName: string;
  Index: Integer;
  Val: Variant;
begin

  case TabControlPropertyKind.TabIndex of       // Get the property Kind
    1:
      Kind := pkStyle;
    2:
      Kind := pkExtended;
    else
      Kind := pkGeneral
  end;

  Index := PropertyListBox.SelectedPropertyNo;

  PropertyListBox.Properties.Clear;
  PropertyListBox.Properties.BeginUpdate;
  try
    for i := 0 to info.PropertyCount [Kind] - 1 do
    begin
      // Add each property.
      Prop := TPropertyListProperty(PropertyListBox.Properties.Add);
      PropName := info.PropertyName [Kind, i];
      Prop.PropertyName := PropName;
      Val := info.PropertyValue [Kind, i];
      Prop.Enabled := not VarIsEmpty(Val);

      case info.PropertyType [Kind, i] of
        ptString:
          Prop.PropertyType := ComponentPropertyListBox.ptString;
        ptInteger:
          Prop.PropertyType := ComponentPropertyListBox.ptInteger;
        ptBoolean:
          Prop.PropertyType := ComponentPropertyListBox.ptBoolean;
        ptSpecial:
          begin
            Prop.PropertyType := ComponentPropertyListBox.ptSpecial;
            Prop.OnSpecialButtonClick := DoSpecialButtonClick
          end;
        ptEnum:
          begin
            // Add enumerated property values
            Prop.PropertyType := ComponentPropertyListBox.ptEnum;
            Prop.EnumValues.Clear;
            Prop.EnumValues.BeginUpdate;
            try
              for j := 0 to info.PropertyEnumCount [Kind, i] - 1 do
                Prop.EnumValues.Add (info.PropertyEnumName [Kind, i, j])
            finally
              Prop.EnumValues.EndUpdate
            end
          end
      end;
                                        // Set the property value
      if not VarIsEmpty(Val) then
        Prop.PropertyValue := Val
    end
  finally
    PropertyListBox.Properties.EndUpdate
  end;

  if reset then
                                        // Select first property
    PropertyListBox.SelectedPropertyNo := 0
  else
    PropertyListBox.SelectedPropertyNo := Index;
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.tcPropertyKindChange                               |
 |                                                                      |
 | A different propery tab has been selected.  Display the appropriate  |
 | properties.                                                          |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.TabControlPropertyKindChange(Sender: TObject);
var
  info: TControlInfo;
begin
  info := DialogEditor.SelectedControl;

  FillPropertyBox (info, True)
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.SaveResource                                       |
 |                                                                      |
 | Get and save a new resource template from the editor.                |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.SaveResource(const UndoDetails: string);
begin
  AddUndoEntry(UndoDetails);

  FDetails.Data.Clear;
  DialogEditor.SaveToStream (FDetails.Data);
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1ControlResize                         |
 |                                                                      |
 | Update the display when a control is resized by dragging             |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditorControlResize(Sender: TObject;
  ctrlInfo: TControlInfo; newRect: TRect);
begin
  FillPropertyBox (ctrlInfo, False);
  SaveResource(rstResize);
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1ControlPropertyChange                 |
 |                                                                      |
 | Update the property display when a control property changes          |
 | 'magically' - eg, when setting Show Maximize Box, automagically      |
 | changes 'Show System Menu'                                           |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditorControlPropertyChange(
  Sender: TObject; ctrlInfo: TControlInfo);
begin
  FillPropertyBox (ctrlInfo, False);
end;

procedure TFormDialogResource.FormShow(Sender: TObject);
var
  i: Integer;
  dc: TDropControl;
  ic: TControlInfoClass;
begin
  RulerTop.Left := DialogEditor.Left + DialogEditor.Margin;
  RulerLeft.Top := DialogEditor.Top + DialogEditor.Margin - PanelRulerTop.Height;
  RulerTop.Width := DialogEditor.Width - 2 * DialogEditor.Margin;
  RulerLeft.Height := DialogEditor.Height - 2 * DialogEditor.Margin;
  FPCWidth := PanelPalette.Width;
  PanelPalette.ManualDock(SizingPageControl, nil, alNone);

  for i := 0 to ToolBar.ButtonCount - 1 do
  begin
    dc := TDropControl (ToolBar.Buttons[i].Tag);

    if dc <> drNone then
    begin
      ic := GetControlInfoClass (dc);
      if Assigned(ic) then
        ToolBar.Buttons[i].Hint := ic.GetDescription
      else
        ToolBar.Buttons[i].ShowHint := False;
    end
    else
      ToolBar.Buttons[i].ShowHint := False;
  end;
end;

procedure TFormDialogResource.SizingPageControlUnDock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  if SizingPageControl.PageCount = 1 then
    SizingPageControl.Width := 0;
end;

procedure TFormDialogResource.SizingPageControlDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  i: Integer;
begin
  with SizingPageControl do
  begin
    for i := 0 to PageCount - 1 do
      Pages[i].Caption := TPanel (Pages[i].Controls[0]).Caption;

    Width := FPCWidth + 8;      // Restore the width to it's original setting
                                // - we've got at least one tab.
  end
end;

procedure TFormDialogResource.ToolButtonImageClick(Sender: TObject);
begin
  DialogEditor.DropControl := TDropControl((Sender as TToolButton).Tag)
end;

procedure TFormDialogResource.UpdateFonts;
begin
  UseInternationalFont(PropertyListBox.Font);
end;

procedure TFormDialogResource.DialogEditorDesignModeDropControl(
  Sender: TObject; x, y: Integer; Ctrl: TControlInfo);
var
  idx: Integer;
begin
  idx := ComboBoxControls.Items.AddObject(GetInfoDescription (Ctrl), Ctrl);
  ComboBoxControls.ItemIndex := idx;
  SaveResource(rstAddControl);
  ToolButtonCursor.Down := True;
  DialogEditor.SelectedControl := Ctrl
end;

function TFormDialogResource.GetCanDelete: Boolean;
begin
  Result := Assigned(DialogEditor.SelectedControl);
end;

procedure TFormDialogResource.EditDelete;
begin
  DialogEditor.DeleteControl (DialogEditor.SelectedControl);
end;

procedure TFormDialogResource.DialogEditorDeleteControl(Sender: TObject;
  Ctrl: TControlInfo);
var
  i: Integer;
begin
  i := 0;
  while i < ComboBoxControls.Items.Count do
    if ComboBoxControls.Items.Objects[i] = Ctrl then
      ComboBoxControls.Items.Delete(i)
    else
      Inc(i);

  SaveResource(rstDeleteControl)
end;

procedure TFormDialogResource.ToolButtonCursorClick(Sender: TObject);
begin
  DialogEditor.DropControl := drNone
end;

procedure TFormDialogResource.DialogEditorGetControlImage(Sender: TObject;
  tp: Integer; const id: string; var Handle: HGDIOBJ);
var
  details: TResourceDetails;
  picture: TPicture;
  resType: Integer;
begin
  if tp = IMAGE_ICON then
    resType := Integer (RT_GROUP_ICON)
  else
    if tp = IMAGE_BITMAP then
      resType := Integer (RT_BITMAP)
    else
      resType := 0;

  details := FDetails.Parent.FindResource(IntToStr (resType), id, FDetails.ResourceLanguage);
  if Assigned(details) then
  begin
    picture := TPicture.Create;
    try
      if details is TIconGroupResourceDetails then
      begin
        TIconGroupResourceDetails (details).GetImage(picture);
        Handle := TExIcon (picture.graphic).ReleaseHandle
      end
      else
        if details is TBitmapResourceDetails then
        begin
          TBitmapResourceDetails (details).GetImage(picture);
          Handle := TBitmap (picture.Graphic).ReleaseHandle
        end
    finally
      picture.Free
    end
  end
end;

procedure TFormDialogResource.DoSpecialButtonClick(Sender: TObject);
var
  prop: TPropertyListProperty;
  st: string;
begin
  prop := TPropertyListProperty(Sender);

  st := prop.PropertyName;

  if st = 'Font' then
  begin
    FontDialog.Font.Handle := DialogEditor.FontHandle;
    if FontDialog.Execute then
    begin
      DialogEditor.SetTemplateFont(FontDialog.Font);
      SaveResource(rstSetFont);
      DialogEditor.ResourceTemplate := FDetails.Data.Memory;
    end
  end
end;

end.
