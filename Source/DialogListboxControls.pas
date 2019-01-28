unit DialogListboxControls;

interface

uses Windows, Classes, SysUtils, cmpDialogEditor, DialogConsts;

type
TListboxControlInfo = class (TStandardControlInfo)
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
  ListboxControlPropertyGeneralCount = 0;
  ListboxControlPropertyStyleCount = 11;
  ListboxControlPropertyExtendedCount = 0;
  ListboxControlPropertyCount : array [TPropertyKind] of Integer = (ListboxControlPropertyGeneralCount, ListboxControlPropertyStyleCount, ListboxControlPropertyExtendedCount);
//  ListboxControlPropertyGeneralName : array [0..ListboxControlPropertyGeneralCount - 1] of string = (rstCaption);
  ListboxControlPropertyStyleName : array [0..ListboxControlPropertyStyleCount - 1] of string = (rstSelection, rstOwnerDraw, rstSort, rstMultiColumn, rstHScroll, rstVScroll, rstNoRedraw, rstUseTabstops, rstWantKeyInput, rstDisableNoScroll, rstNoIntegralHeight);
//  ListboxControlPropertyExtendedName : array [0..ListboxControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText);
//  ListboxControlPropertyGeneralType : array [0..ListboxControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  ListboxControlPropertyStyleType : array [0..ListboxControlPropertyStyleCount - 1] of TPropertyType = (ptEnum, ptEnum, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean, ptBoolean);
//  ListboxControlPropertyExtendedType : array [0..ListboxControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean);

{ TListboxControlInfo }

class procedure TListboxControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or WS_TABSTOP or LBS_NOTIFY or LBS_SORT or WS_VSCROLL;
  params.ExStyle := params.ExStyle or WS_EX_CLIENTEDGE;

end;

function TListboxControlInfo.GetClassSzOrID: TszOrID;
begin
  Result.isID := True;
  Result.id := LISTBOX_ID
end;

class function TListboxControlInfo.GetDescription: string;
begin
  Result := rstListbox
end;

function TListboxControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + ListboxControlPropertyCount [kind]
end;

function TListboxControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
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
          1 : Result := 3
        end
    end
  end
end;

function TListboxControlInfo.GetPropertyEnumName(kind: TPropertyKind; idx,
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
              0 : Result := rstSingle;
              1 : Result := rstMultiple;
              2 : Result := rstExtended;
              3 : Result := rstNone
            end;
          1 : case enum of
            0 : Result := rstNo;
            1 : Result := rstFixed;
            2 : Result := rstVariable
          end
        end
    end
  end
end;

function TListboxControlInfo.GetPropertyName(kind: TPropertyKind;
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
      pkStyle   : Result := ListboxControlPropertyStyleName [idx];
//      pkExtended : Result := StaticControlPropertyExtendedName [idx];
    end
  end
end;

function TListboxControlInfo.GetPropertyType(kind: TPropertyKind;
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
      pkStyle   : Result := ListboxControlPropertyStyleType [idx];
//      pkExtended : Result := StaticControlPropertyExtendedType [idx];
    end
  end
end;

const
  LBS_SELECTIONMASK = LBS_MULTIPLESEL or LBS_EXTENDEDSEL or LBS_NOSEL;
  LBS_OWNERDRAWMASK = LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE;

function TListboxControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : case Style and LBS_SELECTIONMASK of
                LBS_MULTIPLESEL : Result := 1;
                LBS_EXTENDEDSEL : Result := 2;
                LBS_NOSEL : Result := 3;
                else
                  Result := 0
              end;
          1 : case style and LBS_OWNERDRAWMASK of
                LBS_OWNERDRAWFIXED : Result := 1;
                LBS_OWNERDRAWVARIABLE : Result := 2;
                else
                  Result := 0
              end;
          2 : Result := HasStyle [LBS_SORT];
          3 : Result := HasStyle [LBS_MULTICOLUMN];
          4 : Result := HasStyle [WS_HSCROLL];
          5 : Result := HasStyle [WS_VSCROLL];
          6 : Result := HasStyle [LBS_NOREDRAW];
          7 : Result := HasStyle [LBS_USETABSTOPS];
          8 : Result := HasStyle [LBS_WANTKEYBOARDINPUT];
          9 : Result := HasStyle [LBS_DISABLENOSCROLL];
          10 : Result := HasStyle [LBS_NOINTEGRALHEIGHT];
        end
    end
  end
end;

procedure TListboxControlInfo.SetPropertyValue(kind: TPropertyKind;
  idx: Integer; const Value: Variant);
var
  recreateRequired : Boolean;
begin
  if idx < inherited GetPropertyCount (kind) then
    inherited SetPropertyValue (kind, idx, Value)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    recreateRequired := False;

    case kind of
      pkStyle :
        case idx of
          0 :
            case Value of
              0 : SetMaskedStyle (0, LBS_SELECTIONMASK);
              1 : SetMaskedStyle (LBS_MULTIPLESEL, LBS_SELECTIONMASK);
              2 : SetMaskedStyle (LBS_EXTENDEDSEL, LBS_SELECTIONMASK);
              3 : SetMaskedStyle (LBS_NOSEL, LBS_SELECTIONMASK)
            end;
          1 :
            case Value of
              0 : SetMaskedStyle (0, LBS_OWNERDRAWMASK);
              1 : SetMaskedStyle (LBS_OWNERDRAWFIXED, LBS_OWNERDRAWMASK);
              2 : SetMaskedStyle (LBS_OWNERDRAWVARIABLE, LBS_OWNERDRAWMASK);
            end;

          2 : HasStyle [LBS_SORT] := Value;
          3 : HasStyle [LBS_MULTICOLUMN] := Value;
          4 : begin HasStyle [WS_HSCROLL] := Value; recreateRequired := True end;
          5 : begin HasStyle [WS_VSCROLL] := Value; recreateRequired := True end;
          6 : HasStyle [LBS_NOREDRAW] := Value;
          7 : HasStyle [LBS_USETABSTOPS] := Value;
          8 : HasStyle [LBS_WANTKEYBOARDINPUT] := Value;
          9 : begin HasStyle [LBS_DISABLENOSCROLL] := Value; recreateRequired := True end;
          10 : begin HasStyle [LBS_NOINTEGRALHEIGHT] := Value; recreateRequired := True end;
        end
    end;

    if recreateRequired then
      RecreateWnd
  end
end;

end.
