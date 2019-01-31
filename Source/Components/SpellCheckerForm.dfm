object fmSpellChecker: TfmSpellChecker
  Left = 638
  Top = 203
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Check Spelling'
  ClientHeight = 290
  ClientWidth = 440
  Color = clBtnFace
  Constraints.MinHeight = 257
  Constraints.MinWidth = 291
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    440
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelUnknownWord: TLabel
    Left = 16
    Top = 16
    Width = 75
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Caption = 'Unknown Word'
  end
  object LabelSuggestions: TLabel
    Left = 16
    Top = 96
    Width = 58
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Caption = 'Suggestions'
  end
  object ButtonChange: TButton
    Left = 350
    Top = 112
    Width = 75
    Height = 20
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akTop, akRight]
    Caption = '&Change'
    TabOrder = 0
    OnClick = ButtonChangeClick
  end
  object ButtonChangeAll: TButton
    Left = 350
    Top = 136
    Width = 75
    Height = 20
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akTop, akRight]
    Caption = 'Change A&ll'
    TabOrder = 1
    OnClick = ButtonChangeAllClick
  end
  object ButtonSkipAll: TButton
    Left = 350
    Top = 184
    Width = 75
    Height = 20
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akTop, akRight]
    Caption = 'Skip &All'
    TabOrder = 2
    OnClick = ButtonSkipAllClick
  end
  object ButtonSkip: TButton
    Left = 350
    Top = 160
    Width = 75
    Height = 20
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akTop, akRight]
    Caption = '&Skip'
    TabOrder = 3
    OnClick = ButtonSkipClick
  end
  object ButtonAdd: TButton
    Left = 350
    Top = 208
    Width = 75
    Height = 20
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akTop, akRight]
    Caption = '&Add'
    TabOrder = 4
    OnClick = ButtonAddClick
  end
  object ButtonCancel: TButton
    Left = 271
    Top = 244
    Width = 74
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    ExplicitTop = 251
  end
  object ButtonFinish: TButton
    Left = 350
    Top = 244
    Width = 75
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akRight, akBottom]
    Caption = 'Finish'
    Default = True
    ModalResult = 1
    TabOrder = 7
    ExplicitTop = 251
  end
  object ListViewSuggestions: TListView
    Left = 16
    Top = 112
    Width = 320
    Height = 115
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Suggestions'
      end>
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 6
    ViewStyle = vsReport
    OnDblClick = ListViewSuggestionsDblClick
    ExplicitHeight = 122
  end
  object reText: TExRichEdit
    Left = 16
    Top = 35
    Width = 409
    Height = 56
    Text = ''
    RightMargin = 0
    AutoURLDetect = False
    AutoURLExecute = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object PersistentPosition: TPersistentPosition
    Manufacturer = 'Woozle'
    Left = 56
    Top = 240
  end
end
