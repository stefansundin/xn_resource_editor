inherited FormVersionResource: TFormVersionResource
  Left = 174
  Top = 116
  ActiveControl = PropertyListBox
  Caption = 'FormVersionResource'
  ClientHeight = 432
  ClientWidth = 777
  Menu = MainMenu
  OnResize = FormResize
  ExplicitWidth = 777
  ExplicitHeight = 451
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 201
    Top = 0
    Height = 432
  end
  object ListViewVersionStrings: TListView
    Left = 204
    Top = 0
    Width = 573
    Height = 432
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    ColumnClick = False
    GridLines = True
    RowSelect = True
    PopupMenu = PopupMenu
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListViewVersionStringsDblClick
    OnEdited = ListViewVersionStringsEdited
  end
  object PanelStrings: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 432
    Align = alLeft
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 0
    object PropertyListBox: TPropertyListBox
      Left = 1
      Top = 1
      Width = 199
      Height = 430
      VertScrollBar.Increment = 17
      VertScrollBar.Range = 136
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
      TabStop = False
      Properties = <
        item
          PropertyName = 'Product Version'
          PropertyType = ptString
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'File Version'
          PropertyType = ptString
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Debug'
          PropertyType = ptBoolean
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Inferred'
          PropertyType = ptBoolean
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Patched'
          PropertyType = ptBoolean
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Pre Release'
          PropertyType = ptBoolean
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Private Build'
          PropertyType = ptBoolean
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Special Build'
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
  object mmoMessage: TExRichEdit
    Left = 408
    Top = 80
    Width = 185
    Height = 73
    Text = 'mmoMessage'
    RightMargin = 0
    AutoURLDetect = False
    AutoURLExecute = False
    Enabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Visible = False
    OnExit = mmoMessageExit
  end
  object MainMenu: TMainMenu
    Left = 728
    Top = 32
    object MenuItemStrings: TMenuItem
      Caption = '&Strings'
      GroupIndex = 27
      object MenuItemAddString: TMenuItem
        Action = ActionStringAddString
      end
      object MenuItemModifyString: TMenuItem
        Action = ActionStringModifyString
      end
      object MenuItemStringModifyStringName: TMenuItem
        Action = ActionStringModifyStringName
      end
      object MenuItemDeleteString: TMenuItem
        Action = ActionStringDeleteString
      end
    end
  end
  object ActionList: TActionList
    Left = 728
    Top = 64
    object ActionStringAddString: TAction
      Category = 'Strings'
      Caption = '&Add String'
      ShortCut = 45
      OnExecute = ActionStringAddStringExecute
    end
    object ActionStringModifyString: TAction
      Category = 'Strings'
      Caption = '&Modify String'
      OnExecute = ActionStringModifyStringExecute
    end
    object ActionStringDeleteString: TAction
      Category = 'Strings'
      Caption = '&Delete String'
      ShortCut = 46
      OnExecute = ActionStringDeleteStringExecute
    end
    object ActionStringModifyStringName: TAction
      Category = 'Strings'
      Caption = 'Modify String &Name'
      OnExecute = ActionStringModifyStringNameExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 736
    Top = 96
    object MenuItemAddString2: TMenuItem
      Action = ActionStringAddString
    end
    object MenuItemModifyString2: TMenuItem
      Action = ActionStringModifyString
    end
    object MenuItemModifyStringName: TMenuItem
      Action = ActionStringModifyStringName
    end
    object MenuItemDeleteString2: TMenuItem
      Action = ActionStringDeleteString
    end
  end
end
