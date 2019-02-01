inherited FormMenuResource: TFormMenuResource
  Left = 315
  Top = 221
  ActiveControl = PropertyListBox
  Caption = 'Menu Resource'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 185
    Top = 0
    Height = 455
  end
  object PanelProperties: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 455
    Align = alLeft
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 0
    object PropertyListBox: TPropertyListBox
      Left = 1
      Top = 1
      Width = 183
      Height = 453
      VertScrollBar.Increment = 17
      VertScrollBar.Range = 85
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
      OnPropertyChanged = PropertyListBoxPropertyChanged
    end
  end
  object PanelMain: TPanel
    Left = 188
    Top = 0
    Width = 513
    Height = 455
    Align = alClient
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 1
    object MenuDesigner: TMenuDesigner
      Left = 1
      Top = 1
      Width = 511
      Height = 453
      OnSelectedItemChange = MenuDesignerSelectedItemChange
      Align = alClient
      UseDockManager = False
      PopupMenu = PopupMenu
      TabOrder = 0
      TabStop = True
      OnKeyDown = MenuDesignerKeyDown
    end
  end
  object ActionListMenu: TActionList
    Left = 660
    Top = 40
    object ActionMenuDeleteItem: TAction
      Category = 'Menu'
      Caption = '&Delete Item'
      ShortCut = 46
      OnExecute = ActionMenuDeleteItemExecute
    end
    object ActionMenuInsertItem: TAction
      Category = 'Menu'
      Caption = '&Add &Item Before'
      ShortCut = 45
      OnExecute = ActionMenuInsertItemExecute
    end
    object ActionMenuAppendItem: TAction
      Category = 'Menu'
      Caption = '&Add Item After'
      ShortCut = 8237
      OnExecute = ActionMenuAppendItemExecute
    end
    object ActionMenuAddChildItem: TAction
      Category = 'Menu'
      Caption = 'Add &Child Item'
      OnExecute = ActionMenuAddChildItemExecute
    end
  end
  object MainMenuMenu: TMainMenu
    Left = 660
    Top = 80
    object MenuItemMenuITem: TMenuItem
      Caption = '&Menu'
      object MenuItemInsertItem2: TMenuItem
        Action = ActionMenuInsertItem
      end
      object MenuItemAddItemAfter2: TMenuItem
        Action = ActionMenuAppendItem
      end
      object MenuItemAddChildItem2: TMenuItem
        Action = ActionMenuAddChildItem
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuItemMenuDeleteItem2: TMenuItem
        Action = ActionMenuDeleteItem
      end
    end
  end
  object PopupMenu: TPopupMenu
    Left = 660
    Top = 120
    object MenuItemInsertItem: TMenuItem
      Action = ActionMenuInsertItem
    end
    object MenuItemAddItemAfter: TMenuItem
      Action = ActionMenuAppendItem
    end
    object MenuItemAddChildItem: TMenuItem
      Action = ActionMenuAddChildItem
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MenuItemMenuDeleteItem: TMenuItem
      Action = ActionMenuDeleteItem
    end
  end
end
