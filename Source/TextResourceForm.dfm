inherited FormTextResource: TFormTextResource
  Caption = 'FormTextResource'
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vstStrings: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 701
    Height = 455
    Align = alClient
    Header.AutoSizeIndex = 1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible]
    Header.Style = hsPlates
    PopupMenu = PopupMenuStrings
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
    Top = 40
    Width = 185
    Height = 73
    Text = 'mmoMessage'
    RightMargin = 0
    AutoURLDetect = False
    AutoURLExecute = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Visible = False
    OnExit = mmoMessageExit
  end
  object MainMenuStrings: TMainMenu
    Left = 648
    Top = 32
    object MenuItemStrings: TMenuItem
      Caption = '&Strings'
      object MenuItemAddString: TMenuItem
        Action = ActionStringsAdd
      end
      object MenuItemModifyString: TMenuItem
        Action = ActionStringsModify
      end
      object MenuItemDeleteString: TMenuItem
        Action = ActionStringsDelete
      end
      object MenuItemChangeID: TMenuItem
        Action = ActionStringsChangeID
      end
    end
  end
  object ActionList: TActionList
    Left = 648
    Top = 72
    object ActionStringsAdd: TAction
      Category = 'Strings'
      Caption = '&Add String'
      ShortCut = 45
      OnExecute = ActionStringsAddExecute
    end
    object ActionStringsModify: TAction
      Category = 'Strings'
      Caption = '&Modify String'
      OnExecute = ActionStringsModifyExecute
    end
    object ActionStringsDelete: TAction
      Category = 'Strings'
      Caption = '&Delete String'
      ShortCut = 46
      OnExecute = ActionStringsDeleteExecute
    end
    object ActionStringsChangeID: TAction
      Category = 'Strings'
      Caption = '&Change ID'
      OnExecute = ActionStringsChangeIDExecute
    end
  end
  object PopupMenuStrings: TPopupMenu
    Left = 648
    Top = 120
    object MenuItemAddString2: TMenuItem
      Action = ActionStringsAdd
    end
    object MenuItemModifyString2: TMenuItem
      Action = ActionStringsModify
    end
    object MenuItemDeleteString2: TMenuItem
      Action = ActionStringsDelete
    end
    object MenuItemChangeID2: TMenuItem
      Action = ActionStringsChangeID
    end
  end
end
