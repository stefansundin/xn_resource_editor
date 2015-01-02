unit DialogProgressbarControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts, CommCtrl;

type
TProgressBarControlInfo = class (TStandardControlInfo)
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
  ProgressBarControlPropertyGeneralCount = 0;
  ProgressBarControlPropertyStyleCount = 2;
  ProgressBarControlPropertyExtendedCount = 0;
  ProgressBarControlPropertyCount : array [TPropertyKind] of Integer = (ProgressBarControlPropertyGeneralCount, ProgressBarControlPropertyStyleCount, ProgressBarControlPropertyExtendedCount);
//  ProgressBarControlPropertyGeneralName : array [0..ProgressBarControlPropertyGeneralCount - 1] of string = (rstCaption);
  ProgressBarControlPropertyStyleName : array [0..ProgressBarControlPropertyStyleCount - 1] of string = (rstVertical, rstSmooth);
//  ProgressBarControlPropertyExtendedName : array [0..ProgressBarControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  ProgressBarControlPropertyGeneralType : array [0..ProgressBarControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  ProgressBarControlPropertyStyleType : array [0..ProgressBarControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean, ptBoolean);
//  ProgressBarControlPropertyExtendedType : array [0..ProgressBarControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TProgressBarControlInfo }

class procedure TProgressBarControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.ExStyle := params.ExStyle or WS_EX_CLIENTEDGE;
end;

class function TProgressBarControlInfo.GetDescription: string;
begin
  Result := rstProgressBar
end;

function TProgressBarControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + ProgressBarControlPropertyCount [kind]
end;

function TProgressBarControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
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

function TProgressBarControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
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

function TProgressBarControlInfo.GetPropertyName(kind: TPropertyKind;
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
      pkStyle   : Result := ProgressBarControlPropertyStyleName [idx];
//      pkExtended : Result := StaticControlPropertyExtendedName [idx];
    end
  end
end;

function TProgressBarControlInfo.GetPropertyType(kind: TPropertyKind;
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
      pkStyle   : Result := ProgressBarControlPropertyStyleType [idx];
//      pkExtended : Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

function TProgressBarControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : Result := HasStyle [PBS_VERTICAL];
          1 : Result := HasStyle [PBS_SMOOTH];
          2 : Result := HasStyle [WS_BORDER];
        end
    end
  end
end;

procedure TProgressBarControlInfo.Init;
begin
  inherited;

  SendMessage (ControlHandle, PBM_SETPOS, 66, 0);

end;

procedure TProgressBarControlInfo.SetPropertyValue(kind: TPropertyKind;
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
          0 : HasStyle [PBS_VERTICAL] := Value;
          1 : HasStyle [PBS_SMOOTH] := Value;
        end
    end;

    if recreateRequired then
      RecreateWnd
  end
end;

end.
