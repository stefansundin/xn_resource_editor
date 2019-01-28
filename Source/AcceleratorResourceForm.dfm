inherited fmAcceleratorResource: TfmAcceleratorResource
  Left = 283
  Top = 201
  ActiveControl = ListViewAccelerator
  Caption = 'fmAcceleratorResource'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListViewAccelerator: TListView
    Left = 0
    Top = 0
    Width = 701
    Height = 455
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'ID'
        Width = 100
      end
      item
        Caption = 'Key'
        Width = 100
      end
      item
        Caption = 'Type'
        Width = 100
      end>
    ColumnClick = False
    GridLines = True
    RowSelect = True
    PopupMenu = PopupMenuAccel
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListViewAcceleratorDblClick
    OnEdited = ListViewAcceleratorEdited
    OnEditing = ListViewAcceleratorEditing
  end
  object ComboBoxKey: TComboBox
    Left = 104
    Top = 16
    Width = 97
    Height = 21
    TabOrder = 1
    Text = 'ComboBoxKey'
    Visible = False
    OnChange = ComboBoxKeyChange
    OnExit = ComboBoxKeyExit
  end
  object ComboBoxType: TComboBox
    Left = 200
    Top = 16
    Width = 97
    Height = 21
    TabOrder = 2
    Text = 'ComboBoxType'
    Visible = False
    OnChange = ComboBoxTypeChange
    OnExit = ComboBoxTypeExit
  end
  object ActionList: TActionList
    Left = 496
    Top = 32
    object actAccelAdd: TAction
      Category = 'Accelerators'
      Caption = '&Add Accelerator'
      ShortCut = 45
      OnExecute = actAccelAddExecute
    end
    object actAccelDelete: TAction
      Category = 'Accelerators'
      Caption = '&Delete Accelerator'
      ShortCut = 46
      OnExecute = actAccelDeleteExecute
    end
    object actAccelModify: TAction
      Category = 'Accelerators'
      Caption = '&Modify Accelerator'
      OnExecute = actAccelModifyExecute
    end
    object actAccelChangeID: TAction
      Category = 'Accelerators'
      Caption = '&Change ID'
      OnExecute = actAccelChangeIDExecute
    end
    object actAccelChangeFlags: TAction
      Category = 'Accelerators'
      Caption = 'Change &Type'
      OnExecute = actAccelChangeFlagsExecute
    end
  end
  object PopupMenuAccel: TPopupMenu
    Left = 496
    Top = 80
    object MenuItemAddAccelerator: TMenuItem
      Action = actAccelAdd
    end
    object MenuItemModifyAccelerator: TMenuItem
      Action = actAccelModify
    end
    object MenuItemDeleteAccelerator: TMenuItem
      Action = actAccelDelete
    end
    object MenuItemChangeID: TMenuItem
      Action = actAccelChangeID
    end
    object MenuItemChangeFlags: TMenuItem
      Action = actAccelChangeFlags
    end
  end
  object mnuAccelMenu: TMainMenu
    Left = 496
    Top = 128
    object MenuItemAccelerators: TMenuItem
      Caption = 'Accelerators'
      object MenuItemChangeAddAccelerator2: TMenuItem
        Action = actAccelAdd
      end
      object MenuItemChangeModifyAccelerator2: TMenuItem
        Action = actAccelModify
      end
      object MenuItemChangeDeleteAccelerator2: TMenuItem
        Action = actAccelDelete
      end
      object MenuItemChangeChangeID2: TMenuItem
        Action = actAccelChangeID
      end
      object MenuItemChangeFlags2: TMenuItem
        Action = actAccelChangeFlags
      end
    end
  end
end
