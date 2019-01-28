unit DialogHotkeyControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts, CommCtrl;

type
THotkeyControlInfo = class (TStandardControlInfo)
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
  HotkeyControlPropertyGeneralCount = 0;
  HotkeyControlPropertyStyleCount = 0;
  HotkeyControlPropertyExtendedCount = 0;
  HotkeyControlPropertyCount : array [TPropertyKind] of Integer = (HotkeyControlPropertyGeneralCount, HotkeyControlPropertyStyleCount, HotkeyControlPropertyExtendedCount);
//  HotkeyControlPropertyGeneralName : array [0..HotkeyControlPropertyGeneralCount - 1] of string = (rstCaption);
//  HotkeyControlPropertyStyleName : array [0..HotkeyControlPropertyStyleCount - 1] of string = (rstVertical, rstPoint, rstTickMarks, rstAutoTicks, rstEnableSelection, rstThumb);
//  HotkeyControlPropertyExtendedName : array [0..HotkeyControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  HotkeyControlPropertyGeneralType : array [0..HotkeyControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
//  HotkeyControlPropertyStyleType : array [0..HotkeyControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
//  HotkeyControlPropertyExtendedType : array [0..HotkeyControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ THotkeyControlInfo }

class procedure THotkeyControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.ExStyle := params.ExStyle;
end;

class function THotkeyControlInfo.GetDescription: string;
begin
  Result := rstHotkey
end;

function THotkeyControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + HotkeyControlPropertyCount [kind]
end;

function THotkeyControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
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

function THotkeyControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
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

function THotkeyControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
(*     case kind of
//      pkGeneral : Result := StaticControlPropertyGeneralName [idx];
//      pkStyle   : Result := HotkeyControlPropertyStyleName [idx];
//      pkExtended : Result := StaticControlPropertyExtendedName [idx];
    end
*)
  end
end;

function THotkeyControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
(*
    case kind of
//      pkGeneral : Result := StaticControlPropertyGeneralType [idx];
//      pkStyle   : Result := HotkeyControlPropertyStyleType [idx];
//      pkExtended : Result := StaticControlPropertyExtendedType [idx];
    end
*)
  end
end;

function THotkeyControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
  end
end;

procedure THotkeyControlInfo.Init;
begin
  inherited;

  SendMessage (ControlHandle, TBM_SETPOS, 66, 0);
end;

procedure THotkeyControlInfo.SetPropertyValue(kind: TPropertyKind;
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
