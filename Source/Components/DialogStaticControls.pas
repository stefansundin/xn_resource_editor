unit DialogStaticControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts;

type
  TStaticControlInfo = class (TStandardControlInfo)
  protected
    function GetClassSzOrID: TszOrID; override;
  public
    class procedure CreateControlParams (var params: TCreateControlParams); override;
    function GetPropertyCount(Kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(Kind: TPropertyKind; idx: Integer): Integer; override;
    function GetPropertyEnumName(Kind: TPropertyKind; idx, enum: Integer): string; override;
    function GetPropertyName(Kind: TPropertyKind; idx: Integer): string; override;
    function GetPropertyValue(Kind: TPropertyKind; idx: Integer): Variant; override;
    function GetPropertyType(Kind: TPropertyKind; idx: Integer): TPropertyType; override;
    procedure SetPropertyValue(Kind: TPropertyKind; idx: Integer;const Value: Variant); override;
  end;

  TPictureControlInfo = class (TStaticControlInfo)
  public
    class procedure CreateControlParams (var params: TCreateControlParams); override;
    class function GetDescription: string; override;
    function GetPropertyCount(Kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(Kind: TPropertyKind; idx: Integer): Integer; override;
    function GetPropertyEnumName(Kind: TPropertyKind; idx, enum: Integer): string; override;
    function GetPropertyName(Kind: TPropertyKind; idx: Integer): string; override;
    function GetPropertyValue(Kind: TPropertyKind; idx: Integer): Variant; override;
    function GetPropertyType(Kind: TPropertyKind; idx: Integer): TPropertyType; override;
    procedure SetPropertyValue(Kind: TPropertyKind; idx: Integer;const Value: Variant); override;
  end;

  TCaptionedStaticControlInfo = class (TStaticControlInfo)
  protected
  public
    function GetPropertyCount(Kind: TPropertyKind): Integer; override;
    function GetPropertyEnumCount(Kind: TPropertyKind; idx: Integer): Integer; override;
    function GetPropertyEnumName(Kind: TPropertyKind; idx, enum: Integer): string; override;
    function GetPropertyName(Kind: TPropertyKind; idx: Integer): string; override;
    function GetPropertyValue(Kind: TPropertyKind; idx: Integer): Variant; override;
    function GetPropertyType(Kind: TPropertyKind; idx: Integer): TPropertyType; override;
    procedure SetPropertyValue(Kind: TPropertyKind; idx: Integer;const Value: Variant); override;
  end;

  TStaticTextControlInfo = class (TCaptionedStaticControlInfo)
  public
    class function GetDescription: string; override;
  end;

implementation

uses
  cmpDialogBox, variants, DialogStrings;

const
  SS_REALSIZECONTROL = $40;

  StaticControlPropertyGeneralCount = 0;
  StaticControlPropertyStyleCount = 1;  // Sunken & Border removed - same as Client Edge & Static Edge
  StaticControlPropertyExtendedCount = 0;
  StaticControlPropertyCount: array [TPropertyKind] of Integer = (StaticControlPropertyGeneralCount, StaticControlPropertyStyleCount, StaticControlPropertyExtendedCount);
//  StaticControlPropertyGeneralName: array [0..StaticControlPropertyGeneralCount - 1] of string = (rstCaption);
  StaticControlPropertyStyleName: array [0..StaticControlPropertyStyleCount - 1] of string = (rstNotify);
//  StaticControlPropertyExtendedName: array [0..StaticControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  StaticControlPropertyGeneralType: array [0..StaticControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  StaticControlPropertyStyleType: array [0..StaticControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean);
//  StaticControlPropertyExtendedType: array [0..StaticControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  PictureControlPropertyGeneralCount = 3;
  PictureControlPropertyStyleCount = 2;
  PictureControlPropertyExtendedCount = 0;
  PictureControlPropertyCount: array [TPropertyKind] of Integer = (PictureControlPropertyGeneralCount, PictureControlPropertyStyleCount, PictureControlPropertyExtendedCount);
  PictureControlPropertyGeneralName: array [0..PictureControlPropertyGeneralCount - 1] of string = (rstColor, rstImageID, rstType);
  PictureControlPropertyStyleName: array [0..PictureControlPropertyStyleCount - 1] of string = (rstImagePositioning, rstRightJustify);
//  PictureControlPropertyExtendedName: array [0..PictureControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
  PictureControlPropertyGeneralType: array [0..PictureControlPropertyGeneralCount - 1] of TPropertyType = (ptEnum, ptInteger, ptEnum);
  PictureControlPropertyStyleType: array [0..PictureControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptBoolean);
//  PictureControlPropertyExtendedType: array [0..PictureControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  CaptionedStaticControlPropertyGeneralCount = 1;
  CaptionedStaticControlPropertyStyleCount = 6;
  CaptionedStaticControlPropertyExtendedCount = 2;
  CaptionedStaticControlPropertyCount: array [TPropertyKind] of Integer = (CaptionedStaticControlPropertyGeneralCount, CaptionedStaticControlPropertyStyleCount, CaptionedStaticControlPropertyExtendedCount);
  CaptionedStaticControlPropertyGeneralName: array [0..CaptionedStaticControlPropertyGeneralCount - 1] of string = (rstCaption);
  CaptionedStaticControlPropertyStyleName: array [0..CaptionedStaticControlPropertyStyleCount - 1] of string = (rstAlign, rstEllipsis, rstCenterVertically, rstNoPrefix, rstNoWrap, rstSimple);
  CaptionedStaticControlPropertyExtendedName: array [0..CaptionedStaticControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
  CaptionedStaticControlPropertyGeneralType: array [0..CaptionedStaticControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  CaptionedStaticControlPropertyStyleType: array [0..CaptionedStaticControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
  CaptionedStaticControlPropertyExtendedType: array [0..CaptionedStaticControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TStaticControlInfo }

class procedure TStaticControlInfo.CreateControlParams (var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or WS_GROUP
end;

function TStaticControlInfo.GetClassSzOrID: TszOrID;
begin
  Result.isID := True;
  Result.id := STATIC_ID;
end;

function TStaticControlInfo.GetPropertyCount(Kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (Kind) + StaticControlPropertyCount [Kind]
end;

function TStaticControlInfo.GetPropertyEnumCount(Kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyEnumCount (Kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (Kind));
    Result := 0;
  end
end;

function TStaticControlInfo.GetPropertyEnumName(Kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyEnumName (Kind, idx, enum)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (Kind));
    Result := '';
  end
end;

function TStaticControlInfo.GetPropertyName(Kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyName (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := '';
    case Kind of
//      pkGeneral: Result := StaticControlPropertyGeneralName [idx];
      pkStyle  : Result := StaticControlPropertyStyleName [idx];
//      pkExtended: Result := StaticControlPropertyExtendedName [idx];
    end
  end
end;

function TStaticControlInfo.GetPropertyType(Kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyType (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := ptInteger;
    case Kind of
//      pkGeneral: Result := StaticControlPropertyGeneralType [idx];
      pkStyle  : Result := StaticControlPropertyStyleType [idx];
//      pkExtended: Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

function TStaticControlInfo.GetPropertyValue(Kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyValue (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));

    case Kind of
      pkStyle :
        case idx of
//          0: Result := HasStyle [WS_BORDER];  Same as 'Client Edge'
          0: Result := HasStyle [SS_NOTIFY];
//          2: Result := HasStyle [SS_SUNKEN];  Same as 'Static Edge'
        end
    end
  end
end;

procedure TStaticControlInfo.SetPropertyValue(Kind: TPropertyKind;
  idx: Integer; const Value: Variant);
begin
  if idx < inherited GetPropertyCount (Kind) then
    inherited SetPropertyValue (Kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));

    case Kind of
      pkStyle :
        case idx of
//          0: begin HasStyle [WS_BORDER] := Value; RecreateRequired := True end;
          0: HasStyle [SS_NOTIFY] := Value;
//          2: begin HasStyle [SS_SUNKEN] := Value; RecreateRequired := True; end;
        end
    end
  end
end;

{ TPictureControlInfo }

class procedure TPictureControlInfo.CreateControlParams (var params: TCreateControlParams);
begin
  inherited;
  params.Style := (params.Style and not WS_GROUP) or SS_BITMAP;
  params.cx := 20;
  params.cy := 20
end;

class function TPictureControlInfo.GetDescription: string;
begin
  Result := rstPicture
end;

function TPictureControlInfo.GetPropertyCount(
  Kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (Kind) + PictureControlPropertyCount [Kind];
end;

function TPictureControlInfo.GetPropertyEnumCount(Kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyEnumCount (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := 0;

    case Kind of
      pkGeneral :
        case idx of
          0: if GetPropertyValue (Kind, inherited GetPropertyCount (Kind) + 2) = 0 then
                Result := 4
              else
                Result := 3;
          2: Result := 5
        end;

      pkStyle :
        case idx of
          0: Result := 4
        end
    end
  end
end;

function TPictureControlInfo.GetPropertyEnumName(Kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyEnumName (Kind, idx, enum)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := '';

    case Kind of
      pkGeneral :
        case idx of
          0 :
            case enum of
              0: Result := rstBlack;
              1: Result := rstGrey;
              2: Result := rstWhite;
              3: Result := rstEtched;
            end;

          2 :
            case enum of
              0: Result := rstFrame;
              1: Result := rstRectangle;
              2: Result := rstIcon;
              3: Result := rstBitmap;
              4: Result := rstEnhancedMetaFile
            end
        end;
      pkStyle :
        case idx of
          0 :
            case enum of
              0: Result := rstNone;
              1: Result := rstCenterImage;
              2: Result := rstRealSizeImage;
              3: Result := rstRealSizeControl
            end
        end
    end
  end
end;

function TPictureControlInfo.GetPropertyName(Kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyName (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := '';
    case Kind of
      pkGeneral: Result := PictureControlPropertyGeneralName [idx];
      pkStyle  : Result := PictureControlPropertyStyleName [idx];
//      pkExtended: Result := PictureControlPropertyExtendedName [idx];
    end
  end
end;

function TPictureControlInfo.GetPropertyType(Kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyType (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := ptInteger;
    case Kind of
      pkGeneral: Result := PictureControlPropertyGeneralType [idx];
      pkStyle  : Result := PictureControlPropertyStyleType [idx];
//      pkExtended: Result := PictureControlPropertyExtendedType [idx];
    end
  end
end;

function TPictureControlInfo.GetPropertyValue(Kind: TPropertyKind;
  idx: Integer): Variant;
var
  tp: Integer;
  s: Integer;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyValue (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));

    case Kind of
      pkGeneral:
        begin
          tp := style and SS_TYPEMASK;
          Result := Unassigned;

          case idx of
            0 :
              case tp of
                SS_BLACKRECT, SS_BLACKFRAME: Result := 0;
                SS_GRAYRECT, SS_GRAYFRAME: Result := 1;
                SS_WHITERECT, SS_WHITEFRAME: Result := 2;
                SS_ETCHEDFRAME: Result := 3;
              end;

            1: if IsGraphicControl then
                  if fTitleSzOrID.isID then
                    Result := IntToStr (fTitleSzOrID.ID)
                  else
                    Result := fTitleSzOrID.sz;

            2 :
              if tp in [SS_BLACKFRAME, SS_GRAYFRAME, SS_WHITEFRAME, SS_ETCHEDFRAME] then
                Result := 0
              else
                if tp in [SS_BLACKRECT, SS_GRAYRECT, SS_WHITERECT] then
                  Result := 1
                else
                  if tp = SS_ICON then
                    Result := 2
                  else
                    if tp = SS_BITMAP then
                      Result := 3
                    else
                      if tp = SS_ENHMETAFILE then
                        Result := 4
                      else
                        Result := 0
          end
        end;

      pkStyle :
        case idx of
          0: if GetPropertyValue (pkGeneral, inherited GetPropertyCount (pkGeneral) + 2) < 2 then
                Result := Unassigned
              else
              begin
                s := Style and SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL;
                if (s and SS_CENTERIMAGE) <> 0 then
                  Result := 1
                else
                  if (s and SS_REALSIZEIMAGE) <> 0 then
                    Result := 2
                  else
                    if (s and SS_REALSIZECONTROL) <> 0 then
                      Result := 3
                    else
                      Result := 0
              end;

          1: if GetPropertyValue (pkGeneral, inherited GetPropertyCount (pkGeneral) + 2) < 2 then
                Result := Unassigned
              else
                Result := HasStyle [SS_RIGHTJUST];

        end

    end
  end
end;

procedure TPictureControlInfo.SetPropertyValue(Kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  tp: Integer;
  ColorIdx: Integer;
  isFrame: Boolean;
  FrameChanged, RecreateRequired: Boolean;
begin
  FrameChanged := False;
  RecreateRequired := False;
  if idx < inherited GetPropertyCount (Kind) then
    inherited SetPropertyValue (Kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));

    case Kind of
      pkGeneral :
        begin
          FrameChanged := True;
          tp := style and SS_TYPEMASK;
          isFrame := tp in [SS_BLACKFRAME, SS_GRAYFRAME, SS_WHITEFRAME, SS_ETCHEDFRAME];

          if tp in [SS_BLACKFRAME, SS_BLACKRECT] then
            ColorIdx := 0
          else
            if tp in [SS_GRAYFRAME, SS_GRAYRECT] then
              ColorIdx := 1
            else
              if tp in [SS_WHITEFRAME, SS_WHITERECT] then
                ColorIdx := 2
              else
                ColorIdx := -1;


          case idx of
            0: case value of
                  0: if isFrame then SetMaskedStyle (SS_BLACKFRAME, SS_TYPEMASK) else SetMaskedStyle (SS_BLACKRECT, SS_TYPEMASK);
                  1: if isFrame then SetMaskedStyle (SS_GRAYFRAME, SS_TYPEMASK) else SetMaskedStyle (SS_GRAYRECT, SS_TYPEMASK);
                  2: if isFrame then SetMaskedStyle (SS_WHITEFRAME, SS_TYPEMASK) else SetMaskedStyle (SS_WHITERECT, SS_TYPEMASK);
                  3: SetMaskedStyle (SS_ETCHEDFRAME, SS_TYPEMASK);
                end;

            1: begin
                  fTitleSzOrID := StringtoSzOrID (value);
                  RecreateRequired := True
                end;

            2: case value of
                  0: case ColorIdx of
                        0: SetMaskedStyle (SS_BLACKFRAME, SS_TYPEMASK);
                        1: SetMaskedStyle (SS_GRAYFRAME, SS_TYPEMASK);
                        2: SetMaskedStyle (SS_WHITEFRAME, SS_TYPEMASK);
                        else
                           SetMaskedStyle (SS_ETCHEDFRAME, SS_TYPEMASK)
                      end;
                  1: case ColorIdx of
                        0: SetMaskedStyle (SS_BLACKRECT, SS_TYPEMASK);
                        1: SetMaskedStyle (SS_GRAYRECT, SS_TYPEMASK);
                        2: SetMaskedStyle (SS_WHITERECT, SS_TYPEMASK);
                        else
                          SetMaskedStyle (SS_BLACKRECT, SS_TYPEMASK);
                      end;
                  2: begin
                        SetMaskedStyle (SS_ICON, SS_TYPEMASK);
                        RecreateRequired := True;
                      end;
                  3: begin
                        SetMaskedStyle (SS_BITMAP, SS_TYPEMASK);
                        RecreateRequired := True;
                      end;
                  4: begin
                        SetMaskedStyle (SS_ENHMETAFILE, SS_TYPEMASK);
                        RecreateRequired := True;
                      end;
                end
          end
        end;
      pkStyle:
        begin
          case idx of
            0 :
              begin
                case value of
                  0: SetMaskedStyle (0, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL);
                  1: SetMaskedStyle (SS_CENTERIMAGE, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL);
                  2: SetMaskedStyle (SS_REALSIZEIMAGE, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL);
                  3: SetMaskedStyle (SS_REALSIZECONTROL, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL)
                end
              end;
            1 :
              HasStyle [SS_RIGHTJUST] := Value
          end;
          RecreateRequired := True
        end;
    end;
    DoPropertyChange
  end;
  if FrameChanged then
    SetWindowPos(ControlHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);

  if RecreateRequired then
    RecreateWnd
end;

{ TCaptionedStaticControlInfo }

function TCaptionedStaticControlInfo.GetPropertyCount(
  Kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (Kind) + CaptionedStaticControlPropertyCount [Kind]
end;

function TCaptionedStaticControlInfo.GetPropertyEnumCount(
  Kind: TPropertyKind; idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyEnumCount (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := 0;

    case Kind of
      pkStyle :
        case idx of
          0: Result := 3;
          1: Result := 4
        end
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyEnumName(
  Kind: TPropertyKind; idx, enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyEnumName (Kind, idx, enum)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := '';

    case Kind of
      pkStyle :
        case idx of
          0 :
            case enum of
              0: Result := rstLeft;
              1: Result := rstCenter;
              2: Result := rstRight
            end;
          1 :
            case enum of
              0: Result := rstNone;
              1: Result := rstEndEllipsis;
              2: Result := rstWordEllipsis;
              3: Result := rstPathEllipsis
            end
        end
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyName(Kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyName (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := '';
    case Kind of
      pkGeneral: Result := CaptionedStaticControlPropertyGeneralName [idx];
      pkStyle  : Result := CaptionedStaticControlPropertyStyleName [idx];
      pkExtended: Result := CaptionedStaticControlPropertyExtendedName [idx];
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyType(Kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyType (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));
    Result := ptInteger;
    case Kind of
      pkGeneral: Result := CaptionedStaticControlPropertyGeneralType [idx];
      pkStyle  : Result := CaptionedStaticControlPropertyStyleType [idx];
      pkExtended: Result := CaptionedStaticControlPropertyExtendedType [idx];
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyValue(Kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount (Kind) then
    Result := inherited GetPropertyValue (Kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));

    case Kind of
      pkGeneral :
        case idx of
          0: Result := WindowText;
        end;
      pkStyle :
        case idx of
          0: Result := Style and 3;
          1: case Style and SS_ELLIPSISMASK of
                SS_ENDELLIPSIS: Result := 1;
                SS_WORDELLIPSIS: Result := 2;
                SS_PATHELLIPSIS: Result := 3;
                else
                  Result := 0
              end;
          2: Result := HasStyle [SS_CENTERIMAGE];
          3: Result := HasStyle [SS_NOPREFIX];
          4: Result := HasStyle [SS_LEFTNOWORDWRAP];
          5: Result := HasStyle [SS_SIMPLE]
        end;
      pkExtended:
        case idx of
          0: Result := HasExStyle [WS_EX_RTLREADING];
          1: Result := HasExStyle [WS_EX_RIGHT];
        end
    end
  end
end;

procedure TCaptionedStaticControlInfo.SetPropertyValue(Kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  s: Integer;
  RecreateRequired: Boolean;
begin
  RecreateRequired := False;
  if idx < inherited GetPropertyCount (Kind) then
    inherited SetPropertyValue (Kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (Kind));

    case Kind of
      pkGeneral :
        case idx of
          0: WindowText := Value;
        end;

      pkStyle :
        begin
          RecreateRequired := True; // Maybe not - but the docs say you should...
          case idx of
            0: SetMaskedStyle (Value, 3);
            1 :
              begin
                case Value of
                  1: s := SS_ENDELLIPSIS;
                  2: s := SS_WORDELLIPSIS;
                  3: s := SS_PATHELLIPSIS
                  else
                    s := 0;
                end;
                SetMaskedStyle (s, SS_ELLIPSISMASK)
              end;
            2: HasStyle [SS_CENTERIMAGE] := Value;
            3: HasStyle [SS_NOPREFIX] := Value;
            4: HasStyle [SS_LEFTNOWORDWRAP] := Value;
            5: HasStyle [SS_SIMPLE] := Value;
          end
        end;

      pkExtended :
        case idx of
          0: HasExStyle [WS_EX_RTLREADING] := Value;
          1: HasExStyle [WS_EX_RIGHT] := Value;
        end
    end
  end;
  if RecreateRequired then
    RecreateWnd
end;

{ TStaticTextControlInfo }

class function TStaticTextControlInfo.GetDescription: string;
begin
  Result := rstStaticText;
end;

end.
