unit DialogSliderControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts, CommCtrl;

type
TSliderControlInfo = class (TStandardControlInfo)
protected
  procedure Init; override;
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
  SliderControlPropertyGeneralCount = 0;
  SliderControlPropertyStyleCount = 6;
  SliderControlPropertyExtendedCount = 0;
  SliderControlPropertyCount : array [TPropertyKind] of Integer = (SliderControlPropertyGeneralCount, SliderControlPropertyStyleCount, SliderControlPropertyExtendedCount);
//  SliderControlPropertyGeneralName : array [0..SliderControlPropertyGeneralCount - 1] of string = (rstCaption);
  SliderControlPropertyStyleName : array [0..SliderControlPropertyStyleCount - 1] of string = (rstVertical, rstPoint, rstTickMarks, rstAutoTicks, rstEnableSelection, rstThumb);
//  SliderControlPropertyExtendedName : array [0..SliderControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  SliderControlPropertyGeneralType : array [0..SliderControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  SliderControlPropertyStyleType : array [0..SliderControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
//  SliderControlPropertyExtendedType : array [0..SliderControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TSliderControlInfo }

class procedure TSliderControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.ExStyle := params.ExStyle;
end;

class function TSliderControlInfo.GetDescription: string;
begin
  Result := rstSlider
end;

function TSliderControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + SliderControlPropertyCount [kind]
end;

function TSliderControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    if idx = 1 then
      result := 3
    else
      Result := 0;
  end
end;

function TSliderControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    if idx = 1 then
      case enum of
        0 : result := rstBoth;
        1 : result := rstTopLeft;
        2 : result := rstBottomRight;
        else
          result := ''
      end
    else
      result := ''
  end
end;

function TSliderControlInfo.GetPropertyName(kind: TPropertyKind;
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
      pkStyle   : Result := SliderControlPropertyStyleName [idx];
//      pkExtended : Result := StaticControlPropertyExtendedName [idx];
    end
  end
end;

function TSliderControlInfo.GetPropertyType(kind: TPropertyKind;
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
      pkStyle   : Result := SliderControlPropertyStyleType [idx];
//      pkExtended : Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

function TSliderControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : Result := HasStyle [TBS_VERT];
          1 : if HasStyle [TBS_BOTH] then
                Result := 0
              else
                if HasStyle [TBS_LEFT] then
                  Result := 1
                else
                  Result := 2;
          2 : result := not HasStyle [TBS_NOTICKS];
          3 : result := HasStyle [TBS_AUTOTICKS];
          4 : result := HasStyle [TBS_ENABLESELRANGE];
          5 : result := not HasStyle [TBS_NOTHUMB];
        end
    end
  end
end;

procedure TSliderControlInfo.Init;
begin
  inherited;

  SendMessage (ControlHandle, TBM_SETPOS, 66, 0);
end;

procedure TSliderControlInfo.SetPropertyValue(kind: TPropertyKind;
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
          0 : HasStyle [TBS_VERT] := Value;
          1 : case Integer (Value) of
                0 : SetMaskedStyle (TBS_BOTH, TBS_BOTH or TBS_LEFT);
                1 : SetMaskedStyle (TBS_LEFT, TBS_BOTH or TBS_LEFT);
                2 : SetMaskedStyle (0,        TBS_BOTH or TBS_LEFT)
              end;
          2 : HasStyle [TBS_NOTICKS] := not Boolean (Value);
          3 : HasStyle [TBS_AUTOTICKS] := Value;
          4 : HasStyle [TBS_ENABLESELRANGE] := Value;
          5 : HasStyle [TBS_NOTHUMB] := not Boolean (Value);
        end
    end;

    if recreateRequired then
      RecreateWnd
  end
end;

end.
