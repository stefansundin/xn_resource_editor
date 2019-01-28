unit DialogUpDownControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts, CommCtrl;

type
TUpDownControlInfo = class (TStandardControlInfo)
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

implementation

uses DialogStrings;

const
  UpDownControlPropertyGeneralCount = 0;
  UpDownControlPropertyStyleCount = 8;
  UpDownControlPropertyExtendedCount = 0;
  UpDownControlPropertyCount : array [TPropertyKind] of Integer = (UpDownControlPropertyGeneralCount, UpDownControlPropertyStyleCount, UpDownControlPropertyExtendedCount);
//  UpDownControlPropertyGeneralName : array [0..UpDownControlPropertyGeneralCount - 1] of string = (rstCaption);
  UpDownControlPropertyStyleName : array [0..UpDownControlPropertyStyleCount - 1] of string = (rstOrientation, rstAlignment, rstAutoBuddy, rstSetBuddyInteger, rstNoThousands, rstWrap, rstArrowKeys, rstHotTrack);
//  UpDownControlPropertyExtendedName : array [0..UpDownControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  UpDownControlPropertyGeneralType : array [0..UpDownControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  UpDownControlPropertyStyleType : array [0..UpDownControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
//  UpDownControlPropertyExtendedType : array [0..UpDownControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TUpDownControlInfo }

class procedure TUpDownControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or UDS_ARROWKEYS;
end;

class function TUpDownControlInfo.GetDescription: string;
begin
  Result := rstUpDown
end;

function TUpDownControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + UpDownControlPropertyCount [kind]
end;

function TUpDownControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
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
          0 : Result := 2;
          1 : Result := 3
        end
    end
  end
end;

function TUpDownControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
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
          0 : case enum of
                0 : Result := rstVertical;
                1 : Result := rstHorizontal;
              end;
          1 : case enum of
                0 : Result := rstUnattached;
                1 : Result := rstLeft;
                2 : Result := rstRight
              end
        end
    end
  end
end;

function TUpDownControlInfo.GetPropertyName(kind: TPropertyKind;
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
      pkStyle   : Result := UpDownControlPropertyStyleName [idx];
//      pkExtended : Result := StaticControlPropertyExtendedName [idx];
    end
  end
end;

function TUpDownControlInfo.GetPropertyType(kind: TPropertyKind;
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
      pkStyle   : Result := UpDownControlPropertyStyleType [idx];
//      pkExtended : Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

function TUpDownControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : if HasStyle [UDS_HORZ] then
                Result := 1
              else
                Result := 0;
          1 : case Style and (UDS_ALIGNLEFT or UDS_ALIGNRIGHT) of
                UDS_ALIGNLEFT : Result := 1;
                UDS_ALIGNRIGHT : Result := 2;
                else
                  Result := 0
              end;
          2 : Result := HasStyle [UDS_AUTOBUDDY];
          3 : Result := HasStyle [UDS_SETBUDDYINT];
          4 : Result := HasStyle [UDS_NOTHOUSANDS];
          5 : Result := HasStyle [UDS_WRAP];
          6 : Result := HasStyle [UDS_ARROWKEYS];
          7 : Result := HasStyle [UDS_HOTTRACK];
        end
    end
  end
end;

procedure TUpDownControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  recreateRequired : Boolean;
begin
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    recreateRequired := True;

    case kind of
      pkStyle :
        case idx of
          0 : HasStyle [UDS_HORZ] := Value = 1;
          1 :
              case Value of
                0 : SetMaskedStyle (0, UDS_ALIGNLEFT or UDS_ALIGNRIGHT);
                1 : SetMaskedStyle (UDS_ALIGNLEFT, UDS_ALIGNLEFT or UDS_ALIGNRIGHT);
                2 : SetMaskedStyle (UDS_ALIGNRIGHT, UDS_ALIGNLEFT or UDS_ALIGNRIGHT);
              end;

          2 : HasStyle [UDS_AUTOBUDDY] := Value;
          3 : HasStyle [UDS_SETBUDDYINT] := Value;
          4 : HasStyle [UDS_NOTHOUSANDS] := Value;
          5 : HasStyle [UDS_WRAP] := Value;
          6 : HasStyle [UDS_ARROWKEYS] := Value;
          7 : HasStyle [UDS_HOTTRACK] := Value;
        end
    end;

    if recreateRequired then
      RecreateWnd
  end
end;

end.
