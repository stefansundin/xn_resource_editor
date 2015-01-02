object dlgResourceProperties: TdlgResourceProperties
  Left = 199
  Top = 124
  Caption = 'Resource Properties'
  ClientHeight = 134
  ClientWidth = 304
  Color = clBtnFace
  Constraints.MaxHeight = 161
  Constraints.MinHeight = 161
  Constraints.MinWidth = 252
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    304
    134)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 28
    Height = 13
    Caption = '&Name'
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 48
    Height = 13
    Caption = '&Language'
    FocusControl = cbLanguage
  end
  object cbLanguage: TComboBox
    Left = 80
    Top = 52
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 140
    Top = 94
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 218
    Top = 94
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ntedName: TTntEdit
    Left = 80
    Top = 12
    Width = 209
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
end
