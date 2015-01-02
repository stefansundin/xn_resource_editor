inherited fmTextResource: TfmTextResource
  Caption = 'fmTextResource'
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 701
  ExplicitHeight = 455
  PixelsPerInch = 96
  TextHeight = 13
  object vstStrings: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 701
    Height = 455
    Align = alClient
    Header.AutoSizeIndex = 1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    Header.Style = hsPlates
    PopupMenu = pomStrings
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnDblClick = vstStringsDblClick
    OnEditing = vstStringsEditing
    OnGetText = vstStringsGetText
    OnKeyDown = vstStringsKeyDown
    OnNewText = vstStringsNewText
    Columns = <
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 100
        WideText = 'Id'
      end
      item
        Options = [coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 1
        Width = 597
        WideText = 'String'
      end>
  end
  object mmoMessage: TExRichEdit
    Left = 0
    Top = 48
    Width = 185
    Height = 73
    Text = 'mmoMessage'
    RightMargin = 0
    AutoURLDetect = False
    AutoURLExecute = False
    TabOrder = 0
    Visible = False
    OnExit = mmoMessageExit
  end
  object mnuStringsMenu: TMainMenu
    Left = 648
    Top = 32
    object mnuStrings: TMenuItem
      Caption = '&Strings'
      object AddString1: TMenuItem
        Action = actStringsAdd
      end
      object ModifyString1: TMenuItem
        Action = actStringsModify
      end
      object DeleteString1: TMenuItem
        Action = actStringsDelete
      end
      object ChangeID1: TMenuItem
        Action = actStringsChangeID
      end
    end
  end
  object ActionList1: TActionList
    Left = 648
    Top = 72
    object actStringsAdd: TAction
      Category = 'Strings'
      Caption = '&Add String'
      ShortCut = 45
      OnExecute = actStringsAddExecute
    end
    object actStringsModify: TAction
      Category = 'Strings'
      Caption = '&Modify String'
      OnExecute = actStringsModifyExecute
    end
    object actStringsDelete: TAction
      Category = 'Strings'
      Caption = '&Delete String'
      ShortCut = 46
      OnExecute = actStringsDeleteExecute
    end
    object actStringsChangeID: TAction
      Category = 'Strings'
      Caption = '&Change ID'
      OnExecute = actStringsChangeIDExecute
    end
  end
  object pomStrings: TPopupMenu
    Left = 648
    Top = 120
    object AddString2: TMenuItem
      Action = actStringsAdd
    end
    object ModifyString2: TMenuItem
      Action = actStringsModify
    end
    object DeleteString2: TMenuItem
      Action = actStringsDelete
    end
    object ChangeID2: TMenuItem
      Action = actStringsChangeID
    end
  end
end
