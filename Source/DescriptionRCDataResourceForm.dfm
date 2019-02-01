inherited FormRCDataDescriptionResource: TFormRCDataDescriptionResource
  Left = 330
  Top = 200
  Caption = 'RC Data Description Resource'
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDescription: TLabel
    Left = 16
    Top = 16
    Width = 81
    Height = 13
    AutoSize = False
    Caption = '&Description'
  end
  object EditDescription: TEdit
    Left = 112
    Top = 12
    Width = 185
    Height = 21
    TabOrder = 0
    OnExit = EditDescriptionExit
  end
end
