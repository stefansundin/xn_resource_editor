inherited fmMenuResource: TfmMenuResource
  Left = 315
  Top = 221
  ActiveControl = PropertyListBox1
  Caption = 'fmMenuResource'
  OnShow = FormShow
  ExplicitWidth = 701
  ExplicitHeight = 455
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Height = 455
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 455
    Align = alLeft
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 0
    object PropertyListBox1: TPropertyListBox
      Left = 1
      Top = 1
      Width = 183
      Height = 453
      VertScrollBar.Increment = 19
      VertScrollBar.Range = 95
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
      TabStop = False
      Properties = <
        item
          PropertyName = 'Caption'
          PropertyType = ptString
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Shortcut'
          PropertyType = ptEnum
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'ID'
          PropertyType = ptInteger
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Enabled'
          PropertyType = ptBoolean
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Checked'
          PropertyType = ptBoolean
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end>
      ActualValueColWidth = 0
      OnPropertyChanged = PropertyListBox1PropertyChanged
    end
  end
  object Panel2: TPanel
    Left = 188
    Top = 0
    Width = 513
    Height = 455
    Align = alClient
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 1
    object MenuDesigner1: TMenuDesigner
      Left = 1
      Top = 1
      Width = 511
      Height = 453
      OnSelectedItemChange = MenuDesigner1SelectedItemChange
      Align = alClient
      UseDockManager = False
      PopupMenu = PopupMenu1
      TabOrder = 0
      TabStop = True
      OnKeyDown = MenuDesigner1KeyDown
    end
  end
  object alMenu: TActionList
    Left = 660
    Top = 40
    object actMenuDeleteItem: TAction
      Category = 'Menu'
      Caption = '&Delete Item'
      ShortCut = 46
      OnExecute = actMenuDeleteItemExecute
    end
    object actMenuInsertItem: TAction
      Category = 'Menu'
      Caption = '&Add &Item Before'
      ShortCut = 45
      OnExecute = actMenuInsertItemExecute
    end
    object actMenuAppendItem: TAction
      Category = 'Menu'
      Caption = '&Add Item After'
      ShortCut = 8237
      OnExecute = actMenuAppendItemExecute
    end
    object actMenuAddChildItem: TAction
      Category = 'Menu'
      Caption = 'Add &Child Item'
      OnExecute = actMenuAddChildItemExecute
    end
  end
  object mnuMenu: TMainMenu
    Left = 660
    Top = 80
    object mnuMenuITem: TMenuItem
      Caption = '&Menu'
      object InsetrItem2: TMenuItem
        Action = actMenuInsertItem
      end
      object AddItemAfter2: TMenuItem
        Action = actMenuAppendItem
      end
      object AddChildItem2: TMenuItem
        Action = actMenuAddChildItem
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuMenuDeleteItem: TMenuItem
        Action = actMenuDeleteItem
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 660
    Top = 120
    object InsetrItem1: TMenuItem
      Action = actMenuInsertItem
    end
    object AddItemAfter1: TMenuItem
      Action = actMenuAppendItem
    end
    object AddChildItem1: TMenuItem
      Action = actMenuAddChildItem
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pomMenuDeleteITem: TMenuItem
      Action = actMenuDeleteItem
    end
  end
end
