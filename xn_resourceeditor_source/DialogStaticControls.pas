unit DialogStaticControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts;

type

TStaticControlInfo = class (TStandardControlInfo)
protected
  function GetClassSzOrID : TszOrID; override;
public
  class procedure CreateControlParams (var params : TCreateControlParams); override;
  function GetPropertyCount(kind: TPropertyKind): Integer; override;
  function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
  function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
  function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
  function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
  function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
  procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
end;

TPictureControlInfo = class (TStaticControlInfo)
public
  class procedure CreateControlParams (var params : TCreateControlParams); override;
  class function GetDescription : string; override;
  function GetPropertyCount(kind: TPropertyKind): Integer; override;
  function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
  function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
  function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
  function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
  function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
  procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
end;

TCaptionedStaticControlInfo = class (TStaticControlInfo)
protected
public
  function GetPropertyCount(kind: TPropertyKind): Integer; override;
  function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
  function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
  function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
  function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
  function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
  procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
end;

TStaticTextControlInfo = class (TCaptionedStaticControlInfo)
public
  class function GetDescription : string; override;
end;

implementation

uses cmpDialogBox, variants, DialogStrings;

const
  SS_REALSIZECONTROL = $40;

  StaticControlPropertyGeneralCount = 0;
  StaticControlPropertyStyleCount = 1;  // Sunken & Border removed - same as Client Edge & Static Edge
  StaticControlPropertyExtendedCount = 0;
  StaticControlPropertyCount : array [TPropertyKind] of Integer = (StaticControlPropertyGeneralCount, StaticControlPropertyStyleCount, StaticControlPropertyExtendedCount);
//  StaticControlPropertyGeneralName : array [0..StaticControlPropertyGeneralCount - 1] of string = (rstCaption);
  StaticControlPropertyStyleName : array [0..StaticControlPropertyStyleCount - 1] of string = (rstNotify);
//  StaticControlPropertyExtendedName : array [0..StaticControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  StaticControlPropertyGeneralType : array [0..StaticControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  StaticControlPropertyStyleType : array [0..StaticControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean);
//  StaticControlPropertyExtendedType : array [0..StaticControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  PictureControlPropertyGeneralCount = 3;
  PictureControlPropertyStyleCount = 2;
  PictureControlPropertyExtendedCount = 0;
  PictureControlPropertyCount : array [TPropertyKind] of Integer = (PictureControlPropertyGeneralCount, PictureControlPropertyStyleCount, PictureControlPropertyExtendedCount);
  PictureControlPropertyGeneralName : array [0..PictureControlPropertyGeneralCount - 1] of string = (rstColor, rstImageID, rstType);
  PictureControlPropertyStyleName : array [0..PictureControlPropertyStyleCount - 1] of string = (rstImagePositioning, rstRightJustify);
//  PictureControlPropertyExtendedName : array [0..PictureControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
  PictureControlPropertyGeneralType : array [0..PictureControlPropertyGeneralCount - 1] of TPropertyType = (ptEnum, ptInteger, ptEnum);
  PictureControlPropertyStyleType : array [0..PictureControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptBoolean);
//  PictureControlPropertyExtendedType : array [0..PictureControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  CaptionedStaticControlPropertyGeneralCount = 1;
  CaptionedStaticControlPropertyStyleCount = 6;
  CaptionedStaticControlPropertyExtendedCount = 2;
  CaptionedStaticControlPropertyCount : array [TPropertyKind] of Integer = (CaptionedStaticControlPropertyGeneralCount, CaptionedStaticControlPropertyStyleCount, CaptionedStaticControlPropertyExtendedCount);
  CaptionedStaticControlPropertyGeneralName : array [0..CaptionedStaticControlPropertyGeneralCount - 1] of string = (rstCaption);
  CaptionedStaticControlPropertyStyleName : array [0..CaptionedStaticControlPropertyStyleCount - 1] of string = (rstAlign, rstEllipsis, rstCenterVertically, rstNoPrefix, rstNoWrap, rstSimple);
  CaptionedStaticControlPropertyExtendedName : array [0..CaptionedStaticControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
  CaptionedStaticControlPropertyGeneralType : array [0..CaptionedStaticControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  CaptionedStaticControlPropertyStyleType : array [0..CaptionedStaticControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
  CaptionedStaticControlPropertyExtendedType : array [0..CaptionedStaticControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TStaticControlInfo }

class procedure TStaticControlInfo.CreateControlParams (var params : TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or WS_GROUP
end;

function TStaticControlInfo.GetClassSzOrID: TszOrID;
begin
  Result.isID := True;
  Result.id := STATIC_ID;
end;

function TStaticControlInfo.GetPropertyCount(kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + StaticControlPropertyCount [kind]
end;

function TStaticControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := 0;
  end
end;

function TStaticControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
  end
end;

function TStaticControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := StaticControlPropertyGeneralName [idx];
      pkStyle   : Result := StaticControlPropertyStyleName [idx];
//      pkExtended : Result := StaticControlPropertyExtendedName [idx];
    end
  end
end;

function TStaticControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := StaticControlPropertyGeneralType [idx];
      pkStyle   : Result := StaticControlPropertyStyleType [idx];
//      pkExtended : Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

function TStaticControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkStyle :
        case idx of
//          0 : Result := HasStyle [WS_BORDER];  Same as 'Client Edge'
          0 : Result := HasStyle [SS_NOTIFY];
//          2 : Result := HasStyle [SS_SUNKEN];  Same as 'Static Edge'
        end
    end
  end
end;

procedure TStaticControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
begin
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkStyle :
        case idx of
//          0 : begin HasStyle [WS_BORDER] := Value; recreateRequired := True end;
          0 : HasStyle [SS_NOTIFY] := Value;
//          2 : begin HasStyle [SS_SUNKEN] := Value; recreateRequired := True; end;
        end
    end
  end
end;

{ TPictureControlInfo }

class procedure TPictureControlInfo.CreateControlParams (var params : TCreateControlParams);
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
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + PictureControlPropertyCount [kind];
end;

function TPictureControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := 0;

    case kind of
      pkGeneral :
        case idx of
          0 : if GetPropertyValue (kind, inherited GetPropertyCount (kind) + 2) = 0 then
                result := 4
              else
                result := 3;
          2 : result := 5
        end;

      pkStyle :
        case idx of
          0 : Result := 4
        end
    end
  end
end;

function TPictureControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';

    case kind of
      pkGeneral :
        case idx of
          0 :
            case enum of
              0 : result := rstBlack;
              1 : result := rstGrey;
              2 : result := rstWhite;
              3 : result := rstEtched;
            end;

          2 :
            case enum of
              0 : result := rstFrame;
              1 : result := rstRectangle;
              2 : result := rstIcon;
              3 : result := rstBitmap;
              4 : result := rstEnhancedMetaFile
            end
        end;
      pkStyle :
        case idx of
          0 :
            case enum of
              0 : Result := rstNone;
              1 : Result := rstCenterImage;
              2 : Result := rstRealSizeImage;
              3 : Result := rstRealSizeControl
            end
        end
    end
  end
end;

function TPictureControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
      pkGeneral : Result := PictureControlPropertyGeneralName [idx];
      pkStyle   : Result := PictureControlPropertyStyleName [idx];
//      pkExtended : Result := PictureControlPropertyExtendedName [idx];
    end
  end
end;

function TPictureControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    result := ptInteger;
    case kind of
      pkGeneral : Result := PictureControlPropertyGeneralType [idx];
      pkStyle   : Result := PictureControlPropertyStyleType [idx];
//      pkExtended : Result := PictureControlPropertyExtendedType [idx];
    end
  end
end;

function TPictureControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
var
  tp : Integer;
  s : Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral:
        begin
          tp := style and SS_TYPEMASK;
          result := Unassigned;

          case idx of
            0 :
              case tp of
                SS_BLACKRECT, SS_BLACKFRAME : result := 0;
                SS_GRAYRECT, SS_GRAYFRAME : result := 1;
                SS_WHITERECT, SS_WHITEFRAME : result := 2;
                SS_ETCHEDFRAME : result := 3;
              end;

            1 : if IsGraphicControl then
                  if fTitleSzOrID.isID then
                    result := IntToStr (fTitleSzOrID.ID)
                  else
                    result := fTitleSzOrID.sz;

            2 :
              if tp in [SS_BLACKFRAME, SS_GRAYFRAME, SS_WHITEFRAME, SS_ETCHEDFRAME] then
                result := 0
              else
                if tp in [SS_BLACKRECT, SS_GRAYRECT, SS_WHITERECT] then
                  result := 1
                else
                  if tp = SS_ICON then
                    result := 2
                  else
                    if tp = SS_BITMAP then
                      result := 3
                    else
                      if tp = SS_ENHMETAFILE then
                        result := 4
                      else
                        result := 0
          end
        end;

      pkStyle :
        case idx of
          0 : if GetPropertyValue (pkGeneral, inherited GetPropertyCount (pkGeneral) + 2) < 2 then
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

          1 : if GetPropertyValue (pkGeneral, inherited GetPropertyCount (pkGeneral) + 2) < 2 then
                Result := Unassigned
              else
                Result := HasStyle [SS_RIGHTJUST];

        end

    end
  end
end;

procedure TPictureControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  tp : Integer;
  colorIdx : Integer;
  isFrame : boolean;
  frameChanged, recreateRequired : Boolean;
begin
  frameChanged := False;
  recreateRequired := False;
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral :
        begin
          frameChanged := True;
          tp := style and SS_TYPEMASK;
          isFrame := tp in [SS_BLACKFRAME, SS_GRAYFRAME, SS_WHITEFRAME, SS_ETCHEDFRAME];

          if tp in [SS_BLACKFRAME, SS_BLACKRECT] then
            colorIdx := 0
          else
            if tp in [SS_GRAYFRAME, SS_GRAYRECT] then
              colorIdx := 1
            else
              if tp in [SS_WHITEFRAME, SS_WHITERECT] then
                colorIdx := 2
              else
                colorIdx := -1;


          case idx of
            0 : case value of
                  0 : if isFrame then SetMaskedStyle (SS_BLACKFRAME, SS_TYPEMASK) else SetMaskedStyle (SS_BLACKRECT, SS_TYPEMASK);
                  1 : if isFrame then SetMaskedStyle (SS_GRAYFRAME, SS_TYPEMASK) else SetMaskedStyle (SS_GRAYRECT, SS_TYPEMASK);
                  2 : if isFrame then SetMaskedStyle (SS_WHITEFRAME, SS_TYPEMASK) else SetMaskedStyle (SS_WHITERECT, SS_TYPEMASK);
                  3 : SetMaskedStyle (SS_ETCHEDFRAME, SS_TYPEMASK);
                end;

            1 : begin
                  fTitleSzOrID := StringtoSzOrID (value);
                  recreateRequired := True
                end;

            2 : case value of
                  0 : case colorIdx of
                        0 : SetMaskedStyle (SS_BLACKFRAME, SS_TYPEMASK);
                        1 : SetMaskedStyle (SS_GRAYFRAME, SS_TYPEMASK);
                        2 : SetMaskedStyle (SS_WHITEFRAME, SS_TYPEMASK);
                        else
                           SetMaskedStyle (SS_ETCHEDFRAME, SS_TYPEMASK)
                      end;
                  1 : case colorIdx of
                        0 : SetMaskedStyle (SS_BLACKRECT, SS_TYPEMASK);
                        1 : SetMaskedStyle (SS_GRAYRECT, SS_TYPEMASK);
                        2 : SetMaskedStyle (SS_WHITERECT, SS_TYPEMASK);
                        else
                          SetMaskedStyle (SS_BLACKRECT, SS_TYPEMASK);
                      end;
                  2 : begin
                        SetMaskedStyle (SS_ICON, SS_TYPEMASK);
                        recreateRequired := True;
                      end;
                  3 : begin
                        SetMaskedStyle (SS_BITMAP, SS_TYPEMASK);
                        recreateRequired := True;
                      end;
                  4 : begin
                        SetMaskedStyle (SS_ENHMETAFILE, SS_TYPEMASK);
                        recreateRequired := True;
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
                  0 : SetMaskedStyle (0, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL);
                  1 : SetMaskedStyle (SS_CENTERIMAGE, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL);
                  2 : SetMaskedStyle (SS_REALSIZEIMAGE, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL);
                  3 : SetMaskedStyle (SS_REALSIZECONTROL, SS_CENTERIMAGE or SS_REALSIZEIMAGE or SS_REALSIZECONTROL)
                end
              end;
            1 :
              HasStyle [SS_RIGHTJUST] := Value
          end;
          recreateRequired := True
        end;
    end;
    DoPropertyChange
  end;
  if frameChanged then
    SetWindowPos(ControlHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);

  if recreateRequired then
    RecreateWnd
end;

{ TCaptionedStaticControlInfo }

function TCaptionedStaticControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + CaptionedStaticControlPropertyCount [kind]
end;

function TCaptionedStaticControlInfo.GetPropertyEnumCount(
  kind: TPropertyKind; idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := 0;

    case kind of
      pkStyle :
        case idx of
          0 : Result := 3;
          1 : Result := 4
        end
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyEnumName(
  kind: TPropertyKind; idx, enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';

    case kind of
      pkStyle :
        case idx of
          0 :
            case enum of
              0 : Result := rstLeft;
              1 : Result := rstCenter;
              2 : Result := rstRight
            end;
          1 :
            case enum of
              0 : Result := rstNone;
              1 : Result := rstEndEllipsis;
              2 : Result := rstWordEllipsis;
              3 : Result := rstPathEllipsis
            end
        end
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
      pkGeneral : Result := CaptionedStaticControlPropertyGeneralName [idx];
      pkStyle   : Result := CaptionedStaticControlPropertyStyleName [idx];
      pkExtended : Result := CaptionedStaticControlPropertyExtendedName [idx];
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
      pkGeneral : Result := CaptionedStaticControlPropertyGeneralType [idx];
      pkStyle   : Result := CaptionedStaticControlPropertyStyleType [idx];
      pkExtended : Result := CaptionedStaticControlPropertyExtendedType [idx];
    end
  end
end;

function TCaptionedStaticControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral :
        case idx of
          0 : Result := WindowText;
        end;
      pkStyle :
        case idx of
          0 : Result := Style and 3;
          1 : case Style and SS_ELLIPSISMASK of
                SS_ENDELLIPSIS : Result := 1;
                SS_WORDELLIPSIS : Result := 2;
                SS_PATHELLIPSIS : Result := 3;
                else
                  Result := 0
              end;
          2 : Result := HasStyle [SS_CENTERIMAGE];
          3 : Result := HasStyle [SS_NOPREFIX];
          4 : Result := HasStyle [SS_LEFTNOWORDWRAP];
          5 : Result := HasStyle [SS_SIMPLE]
        end;
      pkExtended:
        case idx of
          0 : Result := HasExStyle [WS_EX_RTLREADING];
          1 : Result := HasExStyle [WS_EX_RIGHT];
        end
    end
  end
end;

procedure TCaptionedStaticControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  s : Integer;
  recreateRequired : boolean;
begin
  recreateRequired := False;
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkGeneral :
        case idx of
          0 : WindowText := Value;
        end;

      pkStyle :
        begin
          recreateRequired := True; // Maybe not - but the docs say you should...
          case idx of
            0 : SetMaskedStyle (Value, 3);
            1 :
              begin
                case Value of
                  1 : s := SS_ENDELLIPSIS;
                  2 : s := SS_WORDELLIPSIS;
                  3 : s := SS_PATHELLIPSIS
                  else
                    s := 0;
                end;
                SetMaskedStyle (s, SS_ELLIPSISMASK)
              end;
            2 : HasStyle [SS_CENTERIMAGE] := Value;
            3 : HasStyle [SS_NOPREFIX] := Value;
            4 : HasStyle [SS_LEFTNOWORDWRAP] := Value;
            5 : HasStyle [SS_SIMPLE] := Value;
          end
        end;

      pkExtended :
        case idx of
          0 : HasExStyle [WS_EX_RTLREADING] := Value;
          1 : HasExStyle [WS_EX_RIGHT] := Value;
        end
    end
  end;
  if recreateRequired then
    RecreateWnd
end;

{ TStaticTextControlInfo }

class function TStaticTextControlInfo.GetDescription: string;
begin
  Result := rstStaticText;
end;

end.
