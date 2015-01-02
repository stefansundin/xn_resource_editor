inherited fmPropertyPageProgramSettings: TfmPropertyPageProgramSettings
  Caption = 'XN Resource Editor Options'
  ClientHeight = 191
  ClientWidth = 318
  Constraints.MinHeight = 191
  Constraints.MinWidth = 318
  ExplicitWidth = 318
  ExplicitHeight = 191
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 16
    Top = 56
    Width = 274
    Height = 13
    Caption = 'Select a font to use when displaying and editing resources'
  end
  object stModuleParser: TLabel [1]
    Left = 16
    Top = 136
    Width = 286
    Height = 13
    Caption = 'Choose which parser to use when loading Windows modules'
  end
  inherited Panel1: TPanel
    Width = 318
    ExplicitWidth = 278
    inherited Bevel1: TBevel
      Width = 318
      ExplicitWidth = 269
    end
    inherited stSectionDetails: TLabel
      Width = 306
      ExplicitWidth = 257
    end
  end
  object stFontDetails: TStaticText
    Left = 24
    Top = 75
    Width = 258
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    BevelInner = bvNone
    BevelKind = bkTile
    TabOrder = 1
  end
  object Button1: TButton
    Left = 288
    Top = 71
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object cbModuleParser: TComboBox
    Left = 24
    Top = 155
    Width = 258
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Text = 'Windows API resource parser'
    OnChange = cbModuleParserChange
    Items.Strings = (
      'Windows API resource parser'
      'XN Resource Editor internal  resource parser')
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Left = 272
    Top = 8
  end
end
