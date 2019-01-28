inherited fmIconGraphicsResource: TfmIconGraphicsResource
  Caption = 'fmIconGraphicsResource'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlLeft: TPanel
    inherited PropertyListBox1: TPropertyListBox
      Properties = <
        item
          PropertyName = 'Width'
          PropertyType = ptInteger
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Height'
          PropertyType = ptInteger
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Pixel Format'
          PropertyType = ptEnum
          Tag = 0
          EnumValues.Strings = (
            '1 Bit'
            '4 Bit'
            '8 Bit'
            '24 Bit'
            '32 Bit')
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Hot Spot Left'
          PropertyType = ptInteger
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Hot Spot Top'
          PropertyType = ptInteger
          Tag = 0
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end>
    end
  end
end
