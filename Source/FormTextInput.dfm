object fmTextInput: TfmTextInput
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Text Input'
  ClientHeight = 210
  ClientWidth = 390
  Color = clBtnFace
  Constraints.MinHeight = 131
  Constraints.MinWidth = 253
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    390
    210)
  PixelsPerInch = 96
  TextHeight = 13
  object mmoText: TTntMemo
    Left = 8
    Top = 8
    Width = 293
    Height = 191
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 367
    ExplicitHeight = 95
  end
  object Button1: TButton
    Left = 307
    Top = 143
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 420
    ExplicitTop = 46
  end
  object Button2: TButton
    Left = 307
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Font...'
    TabOrder = 2
    OnClick = Button2Click
    ExplicitLeft = 521
  end
  object Button3: TButton
    Left = 307
    Top = 174
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 420
    ExplicitTop = 77
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Left = 320
    Top = 40
  end
end
