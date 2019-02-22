object FormPropertyBase: TFormPropertyBase
  Left = 374
  Top = 189
  ActiveControl = vstSections
  BorderIcons = [biSystemMenu]
  BorderWidth = 4
  ClientHeight = 358
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 169
    Top = 0
    Width = 4
    Height = 323
    Beveled = True
  end
  object PanelOptions: TPanel
    Left = 173
    Top = 0
    Width = 390
    Height = 323
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object Bevel: TBevel
      Left = 0
      Top = 320
      Width = 390
      Height = 3
      Align = alBottom
      Shape = bsBottomLine
    end
  end
  object vstSections: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 169
    Height = 323
    Align = alLeft
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    NodeDataSize = 4
    PopupMenu = PopupMenu
    TabOrder = 0
    OnFocusChanged = vstSectionsFocusChanged
    OnGetText = vstSectionsGetText
    OnInitChildren = vstSectionsInitChildren
    OnInitNode = vstSectionsInitNode
    Columns = <>
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 323
    Width = 563
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      563
      35)
    object ButtonOK: TButton
      Left = 241
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 318
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object ButtonApply: TButton
      Left = 395
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Apply'
      TabOrder = 2
      OnClick = ButtonApplyClick
    end
    object ButtonHelp: TButton
      Left = 480
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 3
      OnClick = ButtonHelpClick
    end
  end
  object PersistentPosition: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XN Resource Editor'
    Left = 16
    Top = 332
  end
  object PopupMenu: TPopupMenu
    Left = 56
    Top = 328
    object MenuItemExpandAll: TMenuItem
      Caption = '&Expand All'
      OnClick = MenuItemExpandAllClick
    end
    object MenuItemCollapseAll: TMenuItem
      Caption = '&Collapse All'
      OnClick = MenuItemCollapseAllClick
    end
  end
end
