object fmSpellChecker: TfmSpellChecker
  Left = 638
  Top = 203
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Check Spelling'
  ClientHeight = 297
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    440
    297)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 75
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Caption = 'Unknown Word'
  end
  object lblSuggestions: TLabel
    Left = 16
    Top = 96
    Width = 58
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Caption = 'Suggestions'
  end
  object btnChange: TButton
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
    OnClick = btnChangeClick
  end
  object btnChangeAll: TButton
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
    OnClick = btnChangeAllClick
  end
  object btnSkipAll: TButton
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
    OnClick = btnSkipAllClick
  end
  object btnSkip: TButton
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
    OnClick = btnSkipClick
  end
  object btnAdd: TButton
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
    OnClick = btnAddClick
  end
  object btnCancel: TButton
    Left = 271
    Top = 251
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
  end
  object btnFinish: TButton
    Left = 350
    Top = 251
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
  end
  object lvSuggestions: TListView
    Left = 16
    Top = 112
    Width = 320
    Height = 122
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
    OnDblClick = lvSuggestionsDblClick
  end
end
