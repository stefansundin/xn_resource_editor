inherited FormAcceleratorResource: TFormAcceleratorResource
  Left = 283
  Top = 201
  ActiveControl = ListViewAccelerator
  Caption = 'FormAcceleratorResource'
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
    object ActionAccelAdd: TAction
      Category = 'Accelerators'
      Caption = '&Add Accelerator'
      ShortCut = 45
      OnExecute = ActionAccelAddExecute
    end
    object ActionAccelDelete: TAction
      Category = 'Accelerators'
      Caption = '&Delete Accelerator'
      ShortCut = 46
      OnExecute = ActionAccelDeleteExecute
    end
    object ActionAccelModify: TAction
      Category = 'Accelerators'
      Caption = '&Modify Accelerator'
      OnExecute = ActionAccelModifyExecute
    end
    object ActionAccelChangeID: TAction
      Category = 'Accelerators'
      Caption = '&Change ID'
      OnExecute = ActionAccelChangeIDExecute
    end
    object ActionAccelChangeFlags: TAction
      Category = 'Accelerators'
      Caption = 'Change &Type'
      OnExecute = ActionAccelChangeFlagsExecute
    end
  end
  object PopupMenuAccel: TPopupMenu
    Left = 496
    Top = 80
    object MenuItemAddAccelerator: TMenuItem
      Action = ActionAccelAdd
    end
    object MenuItemModifyAccelerator: TMenuItem
      Action = ActionAccelModify
    end
    object MenuItemDeleteAccelerator: TMenuItem
      Action = ActionAccelDelete
    end
    object MenuItemChangeID: TMenuItem
      Action = ActionAccelChangeID
    end
    object MenuItemChangeFlags: TMenuItem
      Action = ActionAccelChangeFlags
    end
  end
  object MainMenuAccelMenu: TMainMenu
    Left = 496
    Top = 128
    object MenuItemAccelerators: TMenuItem
      Caption = 'Accelerators'
      object MenuItemChangeAddAccelerator2: TMenuItem
        Action = ActionAccelAdd
      end
      object MenuItemChangeModifyAccelerator2: TMenuItem
        Action = ActionAccelModify
      end
      object MenuItemChangeDeleteAccelerator2: TMenuItem
        Action = ActionAccelDelete
      end
      object MenuItemChangeChangeID2: TMenuItem
        Action = ActionAccelChangeID
      end
      object MenuItemChangeFlags2: TMenuItem
        Action = ActionAccelChangeFlags
      end
    end
  end
end
