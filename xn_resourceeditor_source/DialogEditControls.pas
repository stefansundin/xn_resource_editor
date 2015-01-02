unit DialogEditControls;

interface

uses Windows, Messages, Classes, SysUtils, cmpDialogEditor, DialogConsts;

type

TEditControlInfo = class (TStandardControlInfo)
protected
  function GetClassSzOrID : TszOrID; override;
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
  EditControlPropertyGeneralCount = 0;
  EditControlPropertyStyleCount = 11;
  EditControlPropertyExtendedCount = 0;
  EditControlPropertyCount : array [TPropertyKind] of Integer = (EditControlPropertyGeneralCount, EditControlPropertyStyleCount, EditControlPropertyExtendedCount);
//  EditControlPropertyGeneralName : array [0..EditControlPropertyGeneralCount - 1] of string = (rstCaption);
  EditControlPropertyStyleName : array [0..EditControlPropertyStyleCount - 1] of string = (rstAlign, rstMultiLine, rstHScroll, rstAutoHScroll, rstVScroll, rstAutoVScroll, rstWantReturn, rstOEMConvert, rstReadOnly, rstMask, rstPassword);
//  EditControlPropertyExtendedName : array [0..EditControlPropertyExtendedCount - 1] of string = ();
//  EditControlPropertyGeneralType : array [0..EditControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  EditControlPropertyStyleType : array [0..EditControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptEnum, ptBoolean);
//  EditControlPropertyExtendedType : array [0..EditControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean, ptBoolean);

{ TEditControlInfo }

class procedure TEditControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  params.Style := params.Style or ES_AUTOHSCROLL or WS_TABSTOP;
  params.exStyle := params.exStyle or WS_EX_CLIENTEDGE;
end;

function TEditControlInfo.GetClassSzOrID: TszOrID;
begin
  Result.isID := True;
  Result.id := EDIT_ID;
end;

class function TEditControlInfo.GetDescription: string;
begin
  Result := rstEdit;
end;

function TEditControlInfo.GetPropertyCount(kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + EditControlPropertyCount [kind]
end;

function TEditControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    Result := 0;
    if kind = pkStyle then
      case idx of
        0 : Result := 3;
        8 : Result := 4;
      end;
  end
end;

function TEditControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';

    if kind = pkStyle then
      case idx of
        0 :
          case enum of
            0 : Result := rstLeft;
            1 : Result := rstCenter;
            2 : Result := rstRight
          end;
        8 :
          case enum of
            0 : Result := rstNone;
            1 : Result := rstUppercase;
            2 : Result := rstLowercase;
            3 : Result := rstNumeric
          end
      end
  end
end;

function TEditControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := EditControlPropertyGeneralName [idx];
      pkStyle   : Result := EditControlPropertyStyleName [idx];
//      pkExtended : Result := EditControlPropertyExtendedName [idx];
    end
  end
end;

function TEditControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := EditControlPropertyGeneralType [idx];
      pkStyle   : Result := EditControlPropertyStyleType [idx];
//      pkExtended : Result := EditControlPropertyExtendedType [idx];
    end
  end
end;

function TEditControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
var
  s : Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkStyle :
        case idx of
          0 : Result := Style and 3;
          1 : Result := HasStyle [ES_MULTILINE];
          2 : Result := HasStyle [WS_HSCROLL];
          3 : Result := HasStyle [ES_AUTOHSCROLL];
          4 : Result := HasStyle [WS_VSCROLL];
          5 : Result := HasStyle [ES_AUTOVSCROLL];
          6 : Result := HasStyle [ES_WANTRETURN];
          7 : Result := HasStyle [ES_OEMCONVERT];
          8 : Result := HasStyle [ES_READONLY];
          9 :
            begin
              s := Style;
              if (s and ES_UPPERCASE) <> 0 then
                Result := 1
              else
                if (s and ES_LOWERCASE) <> 0 then
                  Result := 2
                else
                  if (s and ES_NUMBER) <> 0 then
                    Result := 3
                  else
                    Result := 0
            end;
          10: Result := HasStyle [ES_PASSWORD];
        end
    end
  end
end;

procedure TEditControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  recreateRequired : Boolean;
begin
  recreateRequired := False;
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkStyle :
        case idx of
          0 : begin SetMaskedStyle (Value, 3); recreateRequired := True end;
          1 : HasStyle [ES_MULTILINE] := Value;
          2 : begin HasStyle [WS_HSCROLL] := Value; recreateRequired := True end;
          3 : HasStyle [ES_AUTOHSCROLL] := Value;
          4 : begin HasStyle [WS_VSCROLL] := Value; recreateRequired := True end;
          5 : HasStyle [ES_AUTOVSCROLL] := Value;
          6 : HasStyle [ES_WANTRETURN] := Value;
          7 : HasStyle [ES_OEMCONVERT] := Value;
          8 : begin HasStyle [ES_READONLY] := Value; SendMessage (ControlHandle, EM_SETREADONLY, Integer (Boolean (Value)), 0); end;
          9 : begin
                recreateRequired := True;
                case Value of
                  0 : SetMaskedStyle (0, ES_UPPERCASE or ES_LOWERCASE or ES_NUMBER);
                  1 : SetMaskedStyle (ES_UPPERCASE, ES_UPPERCASE or ES_LOWERCASE or ES_NUMBER);
                  2 : SetMaskedStyle (ES_LOWERCASE, ES_UPPERCASE or ES_LOWERCASE or ES_NUMBER);
                  3 : SetMaskedStyle (ES_NUMBER, ES_UPPERCASE or ES_LOWERCASE or ES_NUMBER)
                end;

                if value = 3 then
                  SetWindowText (ControlHandle, '1234')
                else
                  SetWindowText (ControlHandle, PChar (rstText))
              end;
          10 : begin HasStyle [ES_PASSWORD] := Value; recreateRequired := True end;
        end
    end;
    if recreateRequired then
      RecreateWnd
  end
end;

end.
