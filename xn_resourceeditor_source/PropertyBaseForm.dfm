object fmPropertyBase: TfmPropertyBase
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
  object Splitter1: TSplitter
    Left = 169
    Top = 0
    Width = 4
    Height = 323
    Beveled = True
  end
  object pnlOptions: TPanel
    Left = 173
    Top = 0
    Width = 390
    Height = 323
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object Bevel1: TBevel
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
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    NodeDataSize = 4
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnFocusChanged = vstSectionsFocusChanged
    OnGetText = vstSectionsGetText
    OnInitChildren = vstSectionsInitChildren
    OnInitNode = vstSectionsInitNode
    Columns = <>
  end
  object pnlButtons: TPanel
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
    object btnOK: TButton
      Left = 241
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
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
    object btnApply: TButton
      Left = 395
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Apply'
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object btnHelp: TButton
      Left = 480
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 3
      OnClick = btnHelpClick
    end
  end
  object PersistentPosition1: TPersistentPosition
    Manufacturer = 'Woozle'
    Product = 'XN Resource Editor'
    Left = 16
    Top = 332
  end
  object PopupMenu1: TPopupMenu
    Left = 56
    Top = 328
    object ExpandAll1: TMenuItem
      Caption = '&Expand All'
      OnClick = ExpandAll1Click
    end
    object CollapseAll1: TMenuItem
      Caption = '&Collapse All'
      OnClick = CollapseAll1Click
    end
  end
end
