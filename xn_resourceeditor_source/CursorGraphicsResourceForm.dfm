inherited fmCursorGraphicsResource: TfmCursorGraphicsResource
  Caption = 'fmCursorGraphicsResource'
  ClientHeight = 500
  ExplicitLeft = -171
  ExplicitWidth = 732
  ExplicitHeight = 500
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter1: TSplitter
    Height = 500
    ExplicitHeight = 508
  end
  inherited pnlLeft: TPanel
    Height = 500
    ExplicitHeight = 508
    inherited PropertyListBox1: TPropertyListBox
      VertScrollBar.Range = 95
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
          Tag = 20
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Hot Spot Top'
          PropertyType = ptInteger
          Tag = 21
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end>
    end
    inherited sbThumbnail: TScrollBox
      Height = 375
      ExplicitHeight = 383
    end
  end
  inherited Panel1: TPanel
    Height = 500
    ExplicitHeight = 508
    inherited ScrollBox2: TScrollBox
      Height = 500
      ExplicitHeight = 508
    end
  end
  inherited SizingPageControl1: TSizingPageControl
    Height = 500
    ExplicitHeight = 508
  end
  inherited PopupMenu1: TPopupMenu
    object SetHotspot1: TMenuItem [4]
      Caption = 'Set &Hotspot'
      ShortCut = 106
      OnClick = SetHotspot1Click
    end
  end
end
