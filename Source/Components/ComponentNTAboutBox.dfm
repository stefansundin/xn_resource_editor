object fmNTAboutBox: TfmNTAboutBox
  Left = 192
  Top = 224
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 287
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 371
    Height = 287
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object icoProduct: TImage
        Left = 8
        Top = 8
        Width = 65
        Height = 65
        AutoSize = True
        Center = True
        Stretch = True
        Transparent = True
      end
      object stProduct: TLabel
        Left = 80
        Top = 16
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stProduct'
        ShowAccelChar = False
      end
      object stVersion: TLabel
        Left = 80
        Top = 32
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stVersion'
        ShowAccelChar = False
      end
      object stCopyright: TLabel
        Left = 80
        Top = 48
        Width = 246
        Height = 13
        Caption = 'Copyright '#169' Colin Wilson 2003.  All Rights Reserved'
        ShowAccelChar = False
      end
      object lblSupport: TLabel
        Left = 80
        Top = 64
        Width = 37
        Height = 13
        Caption = 'Support'
        Visible = False
      end
      object Label1: TLabel
        Left = 80
        Top = 96
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'This product is licensed to:'
      end
      object stLicense1: TLabel
        Left = 80
        Top = 112
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'stLicense1'
        ShowAccelChar = False
      end
      object stLicense2: TLabel
        Left = 80
        Top = 128
        Width = 51
        Height = 13
        Caption = 'stLicense2'
        ShowAccelChar = False
      end
      object Bevel1: TBevel
        Left = 80
        Top = 152
        Width = 281
        Height = 2
        Shape = bsBottomLine
      end
      object stMemAvail: TLabel
        Left = 80
        Top = 160
        Width = 281
        Height = 13
        AutoSize = False
        Caption = 'Physical Memory Available to Windows:'
      end
      object icoProduct1: TImage
        Left = 8
        Top = 184
        Width = 65
        Height = 65
        AutoSize = True
        Center = True
        Transparent = True
      end
      object hlbSupport: THyperlinkButton
        Left = 79
        Top = 220
        Width = 64
        Height = 16
        ImageIndex = 0
        SelectedFontColor = clBlack
        SelectedFontStyles = []
        AutoLink = False
        InPlace = False
      end
      object OKBtn: TButton
        Left = 279
        Top = 220
        Width = 75
        Height = 25
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Thank You!'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object stThankYou: TLabel
        Left = 16
        Top = 16
        Width = 337
        Height = 33
        AutoSize = False
        WordWrap = True
      end
      object lbDonations: TListBox
        Left = 16
        Top = 56
        Width = 337
        Height = 185
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
end
