inherited fmAcceleratorResource: TfmAcceleratorResource
  Left = 283
  Top = 201
  ActiveControl = lvAccelerator
  Caption = 'fmAcceleratorResource'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lvAccelerator: TListView
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
    PopupMenu = pomAccel
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvAcceleratorDblClick
    OnEdited = lvAcceleratorEdited
    OnEditing = lvAcceleratorEditing
  end
  object cbKey: TComboBox
    Left = 104
    Top = 16
    Width = 97
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'cbKey'
    Visible = False
    OnChange = cbKeyChange
    OnExit = cbKeyExit
  end
  object cbType: TComboBox
    Left = 200
    Top = 16
    Width = 97
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'cbType'
    Visible = False
    OnChange = cbTypeChange
    OnExit = cbTypeExit
  end
  object ActionList1: TActionList
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
  object pomAccel: TPopupMenu
    Left = 504
    Top = 72
    object AddAccelerator1: TMenuItem
      Action = actAccelAdd
    end
    object ModifyAccelerator1: TMenuItem
      Action = actAccelModify
    end
    object DeleteAccelerator1: TMenuItem
      Action = actAccelDelete
    end
    object ChangeID1: TMenuItem
      Action = actAccelChangeID
    end
    object ChangeFlags1: TMenuItem
      Action = actAccelChangeFlags
    end
  end
  object mnuAccelMenu: TMainMenu
    Left = 504
    Top = 112
    object mnuAccelerators: TMenuItem
      Caption = 'Accelerators'
      object AddAccelerator2: TMenuItem
        Action = actAccelAdd
      end
      object ModifyAccelerator2: TMenuItem
        Action = actAccelModify
      end
      object DeleteAccelerator2: TMenuItem
        Action = actAccelDelete
      end
      object ChangeID2: TMenuItem
        Action = actAccelChangeID
      end
      object ChangeFlags2: TMenuItem
        Action = actAccelChangeFlags
      end
    end
  end
end
