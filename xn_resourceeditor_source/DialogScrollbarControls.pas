(*======================================================================*
 | DialogScrollbarControls unit for PEResourceExplorer                  |
 |                                                                      |
 | Handles Scrollbar control in the dialog editor                       |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002.  All Rights Reserved                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      15/02/2002  CPWW  Original                                  |
 *======================================================================*)

unit DialogScrollbarControls;

interface

uses Windows, Messages, Classes, SysUtils, cmpDialogEditor, DialogConsts;

type

TScrollbarControlInfo = class (TStandardControlInfo)
protected
  function GetClassSzOrID : TszOrID; override;
public
end;

THScrollbarControlInfo = class (TScrollbarControlInfo)
public
  class function GetDescription : string; override;
  class procedure CreateControlParams (var params : TCreateControlParams); override;
  function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
  function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
  function GetPropertyCount(kind: TPropertyKind): Integer; override;
  function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
  function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
  function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
  procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
end;

TVScrollbarControlInfo = class (TScrollbarControlInfo)
public
  class function GetDescription : string; override;
  class procedure CreateControlParams (var params : TCreateControlParams); override;
  function GetPropertyName(kind: TPropertyKind; idx: Integer): string; override;
  function GetPropertyType(kind: TPropertyKind; idx: Integer): TPropertyType; override;
  function GetPropertyCount(kind: TPropertyKind): Integer; override;
  function GetPropertyEnumCount(kind: TPropertyKind; idx: Integer): Integer; override;
  function GetPropertyEnumName(kind: TPropertyKind; idx, enum: Integer): string; override;
  function GetPropertyValue(kind: TPropertyKind; idx: Integer): Variant; override;
  procedure SetPropertyValue(kind: TPropertyKind; idx: Integer;const Value: Variant); override;
end;

implementation

uses DialogStrings;

const
  ScrollbarControlPropertyGeneralCount = 0;
  ScrollbarControlPropertyStyleCount = 1;
  ScrollbarControlPropertyExtendedCount = 0;
  ScrollbarControlPropertyCount : array [TPropertyKind] of Integer = (ScrollbarControlPropertyGeneralCount, ScrollbarControlPropertyStyleCount, ScrollbarControlPropertyExtendedCount);
//  ScrollbarControlPropertyGeneralName : array [0..ScrollbarControlPropertyGeneralCount - 1] of string = (rstCaption);
  ScrollbarControlPropertyStyleName : array [0..ScrollbarControlPropertyStyleCount - 1] of string = (rstAlign);
//  ScrollbarControlPropertyExtendedName : array [0..ScrollbarControlPropertyExtendedCount - 1] of string = (rstRTLReadingOrder, rstRightAlignedText, rstLeftScrollBar);
//  ScrollbarControlPropertyGeneralType : array [0..ScrollbarControlPropertyGeneralCount - 1] of TPropertyType = (ptString);
  ScrollbarControlPropertyStyleType : array [0..ScrollbarControlPropertyStyleCount - 1] of TPropertyType = (ptEnum);
//  ScrollbarControlPropertyExtendedType : array [0..ScrollbarControlPropertyExtendedCount - 1] of TPropertyType = (ptBoolean, ptBoolean, ptBoolean);

{ TScrollbarControlInfo }

function TScrollbarControlInfo.GetClassSzOrID: TszOrID;
begin
  Result.isID := True;
  Result.id := SCROLLBAR_ID;
end;

{ THScrollbarControlInfo }

class procedure THScrollbarControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.cx := 40;
  params.cy := 11;
end;

class function THScrollbarControlInfo.GetDescription: string;
begin
  Result := rstHScrollbar
end;

function THScrollbarControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + ScrollbarControlPropertyCount [kind]
end;

function THScrollbarControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
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
      end;
  end
end;

function THScrollbarControlInfo.GetPropertyEnumName(kind: TPropertyKind;
  idx, enum: Integer): string;
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
            0 : Result := rstNone;
            1 : Result := rstTop;
            2 : Result := rstBottom
          end;
      end
  end
end;

function THScrollbarControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := ScrollbarControlPropertyGeneralName [idx];
      pkStyle   : Result := ScrollbarControlPropertyStyleName [idx];
//      pkExtended : Result := ScrollbarControlPropertyExtendedName [idx];
    end
  end
end;

function THScrollbarControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := ScrollbarControlPropertyGeneralType [idx];
      pkStyle   : Result := ScrollbarControlPropertyStyleType [idx];
//      pkExtended : Result := ScrollbarControlPropertyExtendedType [idx];
    end
  end
end;

function THScrollbarControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 :
            case Style and 6 of
              0 : Result := 0;
              SBS_TOPALIGN : Result := 1;
              SBS_BOTTOMALIGN : Result := 2
            end
        end

    end
  end
end;

procedure THScrollbarControlInfo.SetPropertyValue(kind: TPropertyKind;
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
          0 :
            case Value of
              0 : SetMaskedStyle (0, 6);
              1 : SetMaskedStyle (SBS_TOPALIGN, 6);
              3 : SetMaskedStyle (SBS_BOTTOMALIGN, 6)
            end
        end
    end;
    if recreateRequired then
      RecreateWnd
  end
end;

{ TVScrollbarControlInfo }

class procedure TVScrollbarControlInfo.CreateControlParams(
  var params: TCreateControlParams);
begin
  inherited;
  params.Style := params.Style or SBS_VERT;
  params.cx := 10;
  params.cy := 40
end;

class function TVScrollbarControlInfo.GetDescription: string;
begin
  Result := rstVScrollbar;
end;

function TVScrollbarControlInfo.GetPropertyCount(
  kind: TPropertyKind): Integer;
begin
  Result := inherited GetPropertyCount (kind) + ScrollbarControlPropertyCount [kind]
end;

function TVScrollbarControlInfo.GetPropertyEnumCount(kind: TPropertyKind;
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
      end;
  end
end;

function TVScrollbarControlInfo.GetPropertyEnumName(kind: TPropertyKind;
  idx, enum: Integer): string;
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
            0 : Result := rstNone;
            1 : Result := rstLeft;
            2 : Result := rstRight;
          end;
      end
  end
end;

function TVScrollbarControlInfo.GetPropertyName(kind: TPropertyKind;
  idx: Integer): string;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyName (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := '';
    case kind of
//      pkGeneral : Result := ScrollbarControlPropertyGeneralName [idx];
      pkStyle   : Result := ScrollbarControlPropertyStyleName [idx];
//      pkExtended : Result := ScrollbarControlPropertyExtendedName [idx];
    end
  end
end;

function TVScrollbarControlInfo.GetPropertyType(kind: TPropertyKind;
  idx: Integer): TPropertyType;
begin
  if idx < inherited GetPropertyCount (kind) then
    Result := inherited GetPropertyType (kind, idx)
  else
  begin
    Dec (idx, inherited GetPropertyCount (kind));
    Result := ptInteger;
    case kind of
//      pkGeneral : Result := ScrollbarControlPropertyGeneralType [idx];
      pkStyle   : Result := ScrollbarControlPropertyStyleType [idx];
//      pkExtended : Result := ScrollbarControlPropertyExtendedType [idx];
    end
  end
end;

function TVScrollbarControlInfo.GetPropertyValue(kind: TPropertyKind;
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
          0 : case Style and 6 of
                0 : Result := 0;
                SBS_LEFTALIGN : Result := 1;
                SBS_RIGHTALIGN : Result := 2
              end
        end

    end
  end
end;

procedure TVScrollbarControlInfo.SetPropertyValue(kind: TPropertyKind;
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
          0 :
            case Value of
              0 : SetMaskedStyle (0, 6);
              1 : SetMaskedStyle (SBS_LEFTALIGN, 6);
              3 : SetMaskedStyle (SBS_RIGHTALIGN, 6)
            end
        end
    end;
    if recreateRequired then
      RecreateWnd
  end
end;

end.
