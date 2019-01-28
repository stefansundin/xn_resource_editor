inherited fmVersionResource: TfmVersionResource
  Left = 174
  Top = 116
  ActiveControl = PropertyListBox1
  Caption = 'fmVersionResource'
  ClientHeight = 432
  ClientWidth = 777
  Menu = MainMenu1
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 201
    Top = 0
    Height = 432
  end
  object lvVersionStrings: TListView
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
    PopupMenu = PopupMenu1
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = lvVersionStringsDblClick
    OnEdited = lvVersionStringsEdited
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 432
    Align = alLeft
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 0
    object PropertyListBox1: TPropertyListBox
      Left = 1
      Top = 1
      Width = 199
      Height = 430
      VertScrollBar.Increment = 17
      VertScrollBar.Range = 136
      Align = alClient
      AutoScroll = False
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
      OnPropertyChanged = PropertyListBox1PropertyChanged
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
    TabOrder = 2
    Visible = False
    OnExit = mmoMessageExit
  end
  object MainMenu1: TMainMenu
    Left = 728
    Top = 32
    object mnuStrings: TMenuItem
      Caption = '&Strings'
      GroupIndex = 27
      object mnuAddString: TMenuItem
        Action = actStringAddString
      end
      object mnuModifyString: TMenuItem
        Action = actStringModifyString
      end
      object actStringModifyStringName1: TMenuItem
        Action = actStringModifyStringName
      end
      object mnuDeleteString: TMenuItem
        Action = actStringDeleteString
      end
    end
  end
  object ActionList1: TActionList
    Left = 728
    Top = 64
    object actStringAddString: TAction
      Category = 'Strings'
      Caption = '&Add String'
      ShortCut = 45
      OnExecute = actStringAddStringExecute
    end
    object actStringModifyString: TAction
      Category = 'Strings'
      Caption = '&Modify String'
      OnExecute = actStringModifyStringExecute
    end
    object actStringDeleteString: TAction
      Category = 'Strings'
      Caption = '&Delete String'
      ShortCut = 46
      OnExecute = actStringDeleteStringExecute
    end
    object actStringModifyStringName: TAction
      Category = 'Strings'
      Caption = 'Modify String &Name'
      OnExecute = actStringModifyStringNameExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 736
    Top = 96
    object AddString2: TMenuItem
      Action = actStringAddString
    end
    object ModifyString2: TMenuItem
      Action = actStringModifyString
    end
    object ModifyStringName1: TMenuItem
      Action = actStringModifyStringName
    end
    object DeleteString2: TMenuItem
      Action = actStringDeleteString
    end
  end
end
