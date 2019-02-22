unit unitCExpression;

interface

uses
  Classes, SysUtils;

type
  TValueType = (vInteger, vString, vReal);
  TValue = record
    tp: TValueType;
    iVal: Integer;
    sVal: string;
    rVal: extended
  end;

  TStrValue = class
  private
    RValue: string;
  public
    constructor Create(const AValue: string);
    property Value: string read RValue;
  end;

procedure CalcExpression(const st: string; defines: TStringList; var value: TValue);

implementation

type
  TOperator = (opNop, opEq, opNotEq, opLessEq, opGtrEq, opLess, opGtr,
               opAdd, opSub, opLor, opOr, opMult, opDiv, opIDiv, opMod,
               oplAnd, opAnd, opNot);

procedure DoOp(op: TOperator; var x: TValue; y: TValue);

  procedure AssignBool (bool, rev: Boolean);
  begin
    x.tp := vInteger;
    if rev then
      bool := not bool;
    if bool then
      x.iVal := -1
    else
      x.iVal := 0
  end;

  procedure TypeMismatch;
  begin
    raise Exception.Create('Type mismatch in expression');
  end;

begin
  if (x.tp = vReal) and (y.tp = vInteger) then
  begin
    y.rVal := y.iVal;
    y.tp := vReal
  end;

  if (x.tp = vInteger) and (y.tp = vReal) then
  begin
    x.rVal := x.iVal;
    x.tp := vReal
  end;

  if x.tp <> y.tp then
    TypeMismatch;

  if op in [opAnd, opOr, opLand, opLor, opIDiv, opMod] then
    if x.tp <> vInteger then
      TypeMismatch;

  if op in [opSub, opMult, opDiv] then
    if (x.tp <> vInteger) and (x.tp <> vReal) then
      TypeMismatch;

  case op of
    opEq,
    opNotEq: case x.tp of
                vInteger: AssignBool (x.iVal = y.iVal, op = opNotEq);
                vString : AssignBool (x.sVal = y.sVal, op = opNotEq);
                vReal   : AssignBool (x.rVal = y.rVal, op = opNotEq);
              end;

    opLess,
    opGtrEq: case x.tp of
                vInteger: AssignBool (x.iVal < y.iVal, op = opGtrEq);
                vString : AssignBool (x.sVal < y.sVal, op = opGtrEq);
                vReal   : AssignBool (x.rVal < y.rVal, op = opGtrEq);
              end;

    opGtr,
    opLessEq: case x.tp of
                vInteger: AssignBool (x.iVal > y.iVal, op = opLessEq);
                vString : AssignBool (x.sVal > y.sVal, op = opLessEq);
                vReal   : AssignBool (x.rVal > y.rVal, op = opLessEq);
              end;

    oplAnd: AssignBool ((x.iVal and y.iVal) <> 0, false);
    oplOr : AssignBool ((x.iVal or y.iVal) <> 0, false);
    opAnd : x.iVal := x.iVal and y.iVal;
    opOr  : x.iVal := x.iVal or y.iVal;

    opAdd : case x.tp of
               vInteger: x.iVal := x.iVal + y.iVal;
               vReal   : x.rVal := x.rVal + y.rVal;
               vString : x.sVal := x.sVal + y.sVal
             end;

    opSub : if x.tp = vInteger then
               x.iVal := x.iVal - y.iVal
             else
               x.rVal := x.rVal - y.rVal;

    opMult: if x.tp = vInteger then
               x.iVal := x.iVal * y.iVal
             else
               x.rVal := x.rVal * y.rVal;

    opDiv : if x.tp = vInteger then
             begin
               x.rVal := x.iVal div y.iVal;
               x.tp := vReal
             end
             else
               x.rVal := x.rVal / y.rVal;

    opIDiv: x.iVal := x.iVal div y.iVal;
    opMod : x.iVal := x.iVal mod y.iVal;
  end;
end;

procedure DoUnaryOp (op: TOperator; var x: TValue);
begin
  if x.tp = vString then
    raise Exception.Create('Type mismatch in expression');

  case op of
    opSub: if x.tp = vInteger then
              x.iVal := -x.iVal
            else
              x.rVal := -x.rVal;
    opNot: if x.tp <> vInteger then
              raise Exception.Create('Type mismatch in expression')
            else
              if x.iVal <> 0 then
                x.iVal := 0
              else
                x.iVal := -1

  end
end;

procedure CalcExpression (const st: string; defines: TStringList; var value: TValue);
var
  pos: Integer;
  ch: char;

  function CalcBoolean: TValue; forward;

  function GetChar: char;
  begin
    if pos < Length(st) then
    begin
      ch := st [pos + 1];
      Inc(pos)
    end
    else
      ch := #0;
    Result := ch;
  end;

  function GetNonWhitespace: char;
  begin
    repeat GetChar until not (ch in [' ', #9]);
    Result := ch
  end;

  procedure SkipWhitespace;
  begin
    if ch in [' ', #9] then
      GetNonWhitespace
  end;

  function CalcId: TValue;
  var
    id: string;
    idx: Integer;
  begin
    id := ch;
    while GetChar in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
      id := id + ch;
    SkipWhitespace;

    if id = 'defined' then
    begin
      if (ch = '(') then
      begin
        GetNonWhitespace;
        id := ch;
        while GetChar in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
          id := id + ch;
        SkipWhitespace;
        if ch <> ')' then
          raise Exception.Create('Missing '')''');
        idx := defines.IndexOf(id);
        Result.tp := vInteger;
        if idx = -1 then
          Result.iVal := 0
        else
          Result.iVal := -1
      end
      else
        raise Exception.Create('''('' expected');
      GetNonWhitespace;
      exit;
    end;

    idx := defines.IndexOf(id);
    if idx >= 0 then
      CalcExpression (TStrValue(defines.Objects[idx]).RValue, defines, Result)
    else
    begin
      Result.tp := vInteger;
      Result.iVal := 0
    end
  end;

  function CalcNumber: TValue;
  var
    n: string;
    hexFlag: Boolean;
    dotPos: Integer;
  begin
    n := ch;
    hexFlag := False;
    dotPos := 0;

    if GetChar in ['x', 'X'] then
    begin
      n := '';
      hexFlag := True;
      GetChar;
    end;

    while(ch in ['0'..'9']) or (hexFlag and (ch in ['A'..'F', 'a'..'f'])) do
    begin
      n := n + ch;

      if GetChar = '.' then
        if (dotPos = 0) and not hexFlag then
        begin
          dotPos := Length(n);
          n := n + ch;
          GetChar
        end
        else
          Break
    end;
    SkipWhitespace;

    if dotPos = Length(n) then
    begin
      Delete(n, Length(n), 1);
      ch := '.';
      Dec(pos);
      dotPos := 0;
    end;

    if hexFlag then
    begin
      Result.tp := vInteger;
      Result.iVal := StrToInt('$' + n)
    end
    else
      if dotPos = 0 then
      begin
        Result.tp := vInteger;
        Result.iVal := StrToInt(n)
      end
      else
      begin
        Result.tp := vReal;
        Result.rVal := StrToFloat(n)
      end
  end;

  function CalcCString: TValue;
  var
    st: string;
  begin
    st := '';
    while GetChar <> #0 do
    case ch of
      '"': Break;
      '\' :
        case GetChar of
          '"': st := st + '"';
          'n': st := st + #10;
          'r': st := st + #13;
          't': st := st + #9;
          '\': st := st + '\';
          '0': st := st + #0;
          else
            raise EParserError.Create('Invalid escape sequence');
        end;
      else
        st := st + ch
    end;
    GetChar;
    Result.tp := vString;
    Result.sVal := st
  end;

  function CalcTerm: TValue;
  begin
    case ch of
      '(': begin
              GetNonWhitespace;
              Result := CalcBoolean;
              if ch <> ')' then
              raise Exception.Create('Mismatched parentheses')
            end;
       'A'..'Z', 'a'..'z', '_' :
              Result := CalcId;
       '0'..'9' :
              Result := CalcNumber;
       '"' :
              Result := CalcCString;
    end
  end;

  function CalcSignedTerm: TValue;
  begin
    if ch = '+' then
    begin
      GetNonWhitespace;
      Result := CalcSignedTerm
    end
    else
      if ch = '-' then
      begin
        GetNonWhitespace;
        Result := CalcSignedTerm;
        DoUnaryOp (opSub, Result);
      end
      else
        if ch = '!' then
        begin
          GetNonWhitespace;
          Result := CalcSignedTerm;
          DoUnaryOp (opNot, Result)
        end
        else
          Result := CalcTerm
  end;

  function CalcMultiplication: TValue;
  var
    op: TOperator;
  begin
    Result := CalcSignedTerm;
    while ch in ['*', '/', '\', '%', '&'] do
    begin

      op := opNop;
      case ch of
       '*': begin op := opMult; GetChar; end;
       '/': begin op := opDiv;  GetChar; end;
       '\': begin op := opIDiv; GetChar; end;
       '%': begin op := opMod;  GetChar; end;
       '&': if GetChar = '&' then
             begin
               op := oplAnd;
               GetChar
             end
             else
               op := opAnd;
      end;
      SkipWhitespace;
      if op <> opNop then
        DoOp (op, Result, CalcSignedTerm)
      else
        Break
    end
  end;

  function CalcAddition: TValue;
  var
    op: TOperator;
  begin
    Result := CalcMultiplication;
    while ch in ['+', '-', '|'] do
    begin
      op := opNop;
      case ch of
        '+': begin op := opAdd; GetChar; end;
        '-': begin op := opSub; GetChar; end;
        '|': if GetChar = '|' then
              begin
                GetChar;
                op := oplor
              end
              else
                op := opOr;
      end;
      SkipWhitespace;
      if op <> opNop then
        DoOp (op, Result, CalcMultiplication)
      else
        Break
    end
  end;

  function CalcBoolean: TValue;
  var
    op: TOperator;
  begin
    Result := CalcAddition;

    while ch in ['=', '|', '<', '>'] do
    begin

      op := opNop;
      case ch of
        '=': if GetChar = '=' then
              begin
                GetChar;
                op := opEq
              end;
        '!': if GetChar = '=' then
              begin
                GetChar;
                op := opNotEq
              end;
        '<': if GetChar = '=' then
              begin
                op := opLessEq;
                GetChar
              end
              else
                op := opLess;

        '>': if GetChar = '=' then
              begin
                op := opGtrEq;
                GetChar;
              end
              else
                op := opGtr
      end;

      SkipWhitespace;

      if op <> opNop then
        DoOp (op, Result, CalcAddition)
      else
        Break
    end
  end;

begin
  pos := 0;

  GetNonWhitespace;
  value := CalcAddition;
end;

{ TStrValue }

constructor TStrValue.Create(const AValue: string);
begin
  RValue := AValue;
end;

end.
