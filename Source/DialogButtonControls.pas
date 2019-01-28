unit DialogButtonControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts;

type
TButtonControlInfo = class (TStandardControlInfo)
protected
  function GetClassSzOrID : TszOrID; override;
public
  function GetPropertyCount(kind: TPropertyKind): Integer; override;
  function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
  function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
  function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
  function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
  function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
  procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
end;

TBasicButtonControlInfo = class (TButtonControlInfo)
public
  function GetPropertyCount(kind: TPropertyKind): Integer; override;
  function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
  function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
  function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
  function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
  function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
  procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
end;

TPushbuttonControlInfo = class (TBasicButtonControlInfo)
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

TGroupBoxControlInfo = class (TButtonControlInfo)
  class procedure CreateControlParams (var params : TCreateControlParams); override;
  class function GetDescription : string; override;
end;

TCheckboxControlInfo = class (TBasicButtonControlInfo)
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

TRadiobuttonControlInfo = class (TBasicButtonControlInfo)
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
  ButtonControlPropertyGeneralCount = 1;
  ButtonControlPropertyStyleCount = 4;
  ButtonControlPropertyExtendedCount = 0;
  ButtonControlPropertyCount : array [TPropertyKind] of Integer = (ButtonControlPropertyGeneralCount, ButtonControlPropertyStyleCount, ButtonControlPropertyExtendedCount);
  ButtonControlPropertyGeneralName : array [0..ButtonControlPropertyGeneralCount - 1] of string = (rstCaption);
  ButtonControlPropertyStyleName : array [0..ButtonControlPropertyStyleCount - 1] of string = (rstType, rstFlat, rstNotify, rstHAlign);
//  ButtonControlPropertyExtendedName : array [0..ButtonControlPropertyExtendedCount - 1] of string = ();
  ButtonControlPropertyGeneralType : array [0..ButtonControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  ButtonControlPropertyStyleType : array [0..ButtonControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptBoolean, ptBoolean, ptEnum);
//  ButtonControlPropertyExtendedType : array [0..ButtonControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  BasicButtonControlPropertyGeneralCount = 0;
  BasicButtonControlPropertyStyleCount = 3;
  BasicButtonControlPropertyExtendedCount = 0;
  BasicButtonControlPropertyCount : array [TPropertyKind] of Integer = (BasicButtonControlPropertyGeneralCount, BasicButtonControlPropertyStyleCount, BasicButtonControlPropertyExtendedCount);
//  BasicButtonControlPropertyGeneralName : array [0..BasicButtonControlPropertyGeneralCount - 1] of string = (rstCaption);
  BasicButtonControlPropertyStyleName : array [0..BasicButtonControlPropertyStyleCount - 1] of string = (rstVAlign, rstMultiLine, rstOwnerDraw);
//  BasicButtonControlPropertyExtendedName : array [0..BasicButtonControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  BasicButtonControlPropertyGeneralType : array [0..BasicButtonControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  BasicButtonControlPropertyStyleType : array [0..BasicButtonControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptBoolean, ptBoolean);
//  BasicButtonControlPropertyExtendedType : array [0..BasicButtonControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  PushButtonControlPropertyGeneralCount = 0;
  PushButtonControlPropertyStyleCount = 1;
  PushButtonControlPropertyExtendedCount = 0;
  PushButtonControlPropertyCount : array [TPropertyKind] of Integer = (PushButtonControlPropertyGeneralCount, PushButtonControlPropertyStyleCount, PushButtonControlPropertyExtendedCount);
//  PushButtonControlPropertyGeneralName : array [0..PushButtonControlPropertyGeneralCount - 1] of string = (rstCaption);
  PushButtonControlPropertyStyleName : array [0..PushButtonControlPropertyStyleCount - 1] of string = (rstDefaultButton);
//  PushButtonControlPropertyExtendedName : array [0..PushButtonControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  PushButtonControlPropertyGeneralType : array [0..PushButtonControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  PushButtonControlPropertyStyleType : array [0..PushButtonControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean);
//  PushButtonControlPropertyExtendedType : array [0..PushButtonControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  CheckBoxControlPropertyGeneralCount = 0;
  CheckBoxControlPropertyStyleCount = 4;
  CheckBoxControlPropertyExtendedCount = 0;
  CheckBoxControlPropertyCount : array [TPropertyKind] of Integer = (CheckBoxControlPropertyGeneralCount, CheckBoxControlPropertyStyleCount, CheckBoxControlPropertyExtendedCount);
//  CheckBoxControlPropertyGeneralName : array [0..CheckBoxControlPropertyGeneralCount - 1] of string = (rstCaption);
  CheckBoxControlPropertyStyleName : array [0..CheckBoxControlPropertyStyleCount - 1] of string = (rstAuto, rstLeftText, rstTriState, rstPushLike);
//  CheckBoxControlPropertyExtendedName : array [0..CheckBoxControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  CheckBoxControlPropertyGeneralType : array [0..CheckBoxControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  CheckBoxControlPropertyStyleType : array [0..CheckBoxControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean, ptBoolean, ptBoolean, ptBoolean);
//  CheckBoxControlPropertyExtendedType : array [0..CheckBoxControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

  RadioButtonControlPropertyGeneralCount = 0;
  RadioButtonControlPropertyStyleCount = 3;
  RadioButtonControlPropertyExtendedCount = 0;
  RadioButtonControlPropertyCount : array [TPropertyKind] of Integer = (RadioButtonControlPropertyGeneralCount, RadioButtonControlPropertyStyleCount, RadioButtonControlPropertyExtendedCount);
//  RadioButtonControlPropertyGeneralName : array [0..RadioButtonControlPropertyGeneralCount - 1] of string = (rstCaption);
  RadioButtonControlPropertyStyleName : array [0..RadioButtonControlPropertyStyleCount - 1] of string = (rstAuto, rstLeftText, rstPushLike);
//  RadioButtonControlPropertyExtendedName : array [0..RadioButtonControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  RadioButtonControlPropertyGeneralType : array [0..RadioButtonControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  RadioButtonControlPropertyStyleType : array [0..RadioButtonControlPropertyStyleCount - 1] of TPropertyType = (ptBoolean, ptBoolean, ptBoolean);
//  RadioButtonControlPropertyExtendedType : array [0..RadioButtonControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TButtonControlInfo }

function TButtonControlInfo.GetClassSzOrID: TszOrID;
begin
  Result.isID := True;
  Result.id := BUTTON_ID;
end;

function TButtonControlInfo.GetPropertyCount(kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + ButtonControlPropertyCount [kind]
end;

function TButtonControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
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
          0 : Result := 3;
          3 : Result := 4
        end
    end
  end
end;

function TButtonControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
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
          0 :
            case enum of
              0 : Result := rstText;
              1 : Result := rstIcon;
              2 : Result := rstBitmap
            end;

          3 :
            case enum of
              0 : Result := rstDefault;
              1 : Result := rstLeft;
              2 : Result := rstRight;
              3 : Result := rstCenter
            end
        end
    end
  end
end;

function TButtonControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
      pkGeneral : Result := ButtonControlPropertyGeneralName [idx];
      pkStyle   : Result := ButtonControlPropertyStyleName [idx];
//      pkExtended : Result := ButtonControlPropertyExtendedName [idx];
    end
  end
end;

function TButtonControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
      pkGeneral : Result := ButtonControlPropertyGeneralType [idx];
      pkStyle   : Result := ButtonControlPropertyStyleType [idx];
//      pkExtended : Result := ButtonControlPropertyExtendedType [idx];
    end
  end
end;

function TButtonControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : Result := (Style and $c0) shr 6;
          1 : Result := HasStyle [BS_FLAT];
          2 : Result := HasStyle [BS_NOTIFY];
          3 : Result := (Style and $300) shr 8
        end;
    end
  end
end;

procedure TButtonControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  recreateRequired, frameChanged : Boolean;
begin
  recreateRequired := False;
  frameChanged := False;
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
        case idx of
          0 : begin SetMaskedStyle (Value shl 6, $c0); recreateRequired := True end;
          1 : begin HasStyle [BS_FLAT] := Value; frameChanged := True end;
          2 : HasStyle [BS_NOTIFY] := Value;
          3 : begin SetMaskedStyle (Value shl 8, $300); recreateRequired := True end;
        end;
    end
  end;
  if frameChanged then
    SetWindowPos(ControlHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);

  if recreateRequired then
    RecreateWnd
end;

{ TPushbuttonControlInfo }

class procedure TPushbuttonControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or WS_TABSTOP;
end;

class function TPushbuttonControlInfo.GetDescription: string;
begin
  Result := rstButton;
end;

function TPushbuttonControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + PushButtonControlPropertyCount [kind]
end;

function TPushbuttonControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := 0
  end
end;

function TPushbuttonControlInfo.GetPropertyEnumName(kind: TPropertyKind;
  idx, enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := ''
  end
end;

function TPushbuttonControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := PushButtonControlPropertyGeneralName [idx];
      pkStyle   : Result := PushButtonControlPropertyStyleName [idx];
//      pkExtended : Result := PushButtonControlPropertyExtendedName [idx];
    end
  end
end;

function TPushbuttonControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := PushButtonControlPropertyGeneralType [idx];
      pkStyle   : Result := PushButtonControlPropertyStyleType [idx];
//      pkExtended : Result := PushButtonControlPropertyExtendedType [idx];
    end
  end
end;

function TPushbuttonControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : Result := HasStyle [BS_DEFPUSHBUTTON];
        end;
    end
  end
end;

procedure TPushbuttonControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  frameChanged : Boolean;
begin
  frameChanged := False;
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    case kind of
      pkStyle :
        case idx of
          0 : begin HasStyle [BS_DEFPUSHBUTTON] := Value; frameChanged := True end;
        end;
    end
  end;
  if frameChanged then
    SetWindowPos(ControlHandle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
end;

{ TGroupBoxControlInfo }

class procedure TGroupBoxControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or BS_GROUPBOX
end;

class function TGroupBoxControlInfo.GetDescription: string;
begin
  Result := rstGroupBox;
end;

{ TCheckboxControlInfo }

class procedure TCheckboxControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or (BS_AUTOCHECKBOX or WS_TABSTOP)
end;

class function TCheckboxControlInfo.GetDescription: string;
begin
  Result := rstCheckBox
end;

function TCheckboxControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + CheckBoxControlPropertyCount [kind]
end;

function TCheckboxControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := 0
  end
end;

function TCheckboxControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := ''
  end
end;

function TCheckboxControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := CheckBoxControlPropertyGeneralName [idx];
      pkStyle   : Result := CheckBoxControlPropertyStyleName [idx];
//      pkExtended : Result := CheckBoxControlPropertyExtendedName [idx];
    end
  end
end;

function TCheckboxControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := CheckBoxControlPropertyGeneralType [idx];
      pkStyle   : Result := CheckBoxControlPropertyStyleType [idx];
//      pkExtended : Result := CheckBoxControlPropertyExtendedType [idx];
    end
  end
end;

function TCheckboxControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
var
  tp : Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    tp := Style and $f;

    case kind of
      pkStyle :
        case idx of
          0 : Result := (tp = BS_AUTOCHECKBOX) or (tp = BS_AUTO3STATE);
          1 : Result := HasStyle [BS_LEFTTEXT];
          2 : Result := (tp = BS_3STATE) or (tp = BS_AUTO3STATE);
          3 : Result := HasStyle [BS_PUSHLIKE];
        end;
    end
  end
end;

procedure TCheckboxControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  tp : Integer;
  recreateRequired : Boolean;
begin
  recreateRequired := False;
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    tp := Style and $f;
    case kind of
      pkStyle :
        case idx of
          0 :
            case Boolean (Value) of
              False : if (tp = BS_AUTOCHECKBOX) then
                        SetMaskedStyle (BS_CHECKBOX, $f)
                      else
                        if (tp = BS_AUTO3STATE) then
                          SetMaskedStyle (BS_3STATE, $f);
              True :  if (tp = BS_CHECKBOX) then
                        SetMaskedStyle (BS_AUTOCHECKBOX, $f)
                      else
                        if tp = BS_3STATE then
                          SetMaskedStyle (BS_AUTO3STATE, $f)
            end;

          1 : begin HasStyle [BS_LEFTTEXT] := Value; recreateRequired := True end;
          2 : case Boolean (Value) of
                False : if tp = BS_3STATE then
                          SetMaskedStyle (BS_CHECKBOX, $f)
                        else
                          if tp = BS_AUTO3STATE then
                            SetMaskedStyle (BS_AUTOCHECKBOX, $f);
                True :  if tp = BS_CHECKBOX then
                          SetMaskedStyle (BS_3STATE, $f)
                        else
                          if tp = BS_AUTOCHECKBOX then
                            SetMaskedStyle (BS_AUTO3STATE, $f)
              end;
          3 : begin HasStyle [BS_PUSHLIKE] := Value; recreateRequired := True end;

        end;
    end
  end;
  if recreateRequired then
    RecreateWnd
end;

{ TRadiobuttonControlInfo }

class procedure TRadiobuttonControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or BS_AUTORADIOBUTTON or WS_TABSTOP;
end;

class function TRadiobuttonControlInfo.GetDescription: string;
begin
  Result := rstRadioButton;
end;

function TRadioButtonControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + RadioButtonControlPropertyCount [kind]
end;

function TRadioButtonControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
  idx: Integer): Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumCount (kind, idx)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := 0
  end
end;

function TRadioButtonControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
  enum: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyEnumName (kind, idx, enum)
  else
  begin
//    Dec (idx, inherited GetPropertyCount (kind));
    Result := ''
  end
end;

function TRadioButtonControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := RadioButtonControlPropertyGeneralName [idx];
      pkStyle   : Result := RadioButtonControlPropertyStyleName [idx];
//      pkExtended : Result := RadioButtonControlPropertyExtendedName [idx];
    end
  end
end;

function TRadioButtonControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := RadioButtonControlPropertyGeneralType [idx];
      pkStyle   : Result := RadioButtonControlPropertyStyleType [idx];
//      pkExtended : Result := RadioButtonControlPropertyExtendedType [idx];
    end
  end
end;

function TRadioButtonControlInfo.GetPropertyValue(kind: TPropertyKind;
  idx: Integer): Variant;
var
  tp : Integer;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyValue (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    tp := Style and $f;

    case kind of
      pkStyle :
        case idx of
          0 : Result := tp = BS_AUTORADIOBUTTON;
          1 : Result := HasStyle [BS_LEFTTEXT];
          2 : Result := HasStyle [BS_PUSHLIKE];
        end;
    end
  end
end;

procedure TRadioButtonControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  tp : Integer;
  recreateRequired : Boolean;
begin
  recreateRequired := False;
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));

    tp := Style and $f;
    case kind of
      pkStyle :
        case idx of
          0 :
            case Boolean (Value) of
              False : if (tp = BS_AUTORADIOBUTTON) then
                        SetMaskedStyle (BS_RADIOBUTTON, $f);
              True :  if (tp = BS_RADIOBUTTON) then
                        SetMaskedStyle (BS_AUTORADIOBUTTON, $f)
            end;

          1 : begin HasStyle [BS_LEFTTEXT] := Value; recreateRequired := True end;
          2 : begin HasStyle [BS_PUSHLIKE] := Value; recreateRequired := True end;

        end;
    end
  end;
  if recreateRequired then
    RecreateWnd
end;

{ TBasicButtonControlInfo }

function TBasicButtonControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + BasicButtonControlPropertyCount [kind]
end;

function TBasicButtonControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
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
          0 : Result := 4;
        end
    end
  end
end;

function TBasicButtonControlInfo.GetPropertyEnumName(kind: TPropertyKind;
  idx, enum: Integer): string;
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
              0 : Result := rstDefault;
              1 : Result := rstTop;
              2 : Result := rstBottom;
              3 : Result := rstCenter
            end
        end
    end
  end
end;

function TBasicButtonControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := BasicButtonControlPropertyGeneralName [idx];
      pkStyle   : Result := BasicButtonControlPropertyStyleName [idx];
//      pkExtended : Result := BasicButtonControlPropertyExtendedName [idx];
    end
  end
end;

function TBasicButtonControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := BasicButtonControlPropertyGeneralType [idx];
      pkStyle   : Result := BasicButtonControlPropertyStyleType [idx];
//      pkExtended : Result := BasicButtonControlPropertyExtendedType [idx];
    end
  end
end;

function TBasicButtonControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : Result := (Style and $c00) shr 10;
          1 : Result := HasStyle [BS_MULTILINE];
          2 : Result := HasStyle [BS_OWNERDRAW];
        end
    end
  end
end;

procedure TBasicButtonControlInfo.SetPropertyValue(kind: TPropertyKind;
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
          0 : SetMaskedStyle (Value shl 10, $c00);
          1 : HasStyle [BS_MULTILINE] := Value;
          2 : HasStyle [BS_OWNERDRAW] := Value;
        end;
    end
  end
end;

end.
