object fmPropertyPage: TfmPropertyPage
  Left = 335
  Top = 221
  BorderStyle = bsNone
  Caption = 'fmPropertyPage'
  ClientHeight = 316
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 373
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      373
      41)
    object Bevel1: TBevel
      Left = 0
      Top = 38
      Width = 373
      Height = 3
      Align = alBottom
      Shape = bsBottomLine
    end
    object stSectionDetails: TLabel
      Left = 8
      Top = 6
      Width = 361
      Height = 29
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      ShowAccelChar = False
      WordWrap = True
    end
  end
end
