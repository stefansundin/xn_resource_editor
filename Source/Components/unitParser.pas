unit unitParser;

interface

uses
  Windows, Classes, SysUtils, unitStreamTextReader, unitCExpression;

const
  ttUnknown = 0;
  ttIdentifier = 1;
  ttNumber = 2;
  ttChar = 3;
  ttString = 4;

  ttOpAdd = 10;
  ttOpSub = 11;
  ttOpMul = 12;
  ttOpDiv = 13;

type
  TCharset = set of Char;

  TParser = class
  private
    FReader: TStreamTextReader;

    FLineBuf: string;
    FLineNo: Integer;
    FLinePos: Integer;

    FCh: Char;
    FWhitespace: TCharSet;
    FFirstIdChars: TCharSet;
    FNextIdChars: TCharSet;
    FFirstNumChars: TCharSet;
    FNextNumChars: TCharSet;

    FTokenType: Integer;
    FToken: string;
    FTokenChar: Char;
    FDecimalSeparator: Char;
    FSOL: Boolean;
    FFirstChar: Boolean;
    FTokenSol: Boolean;

  protected
    procedure RawGetToken (charset: TCharset; tp: Integer);

  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    procedure Parse; virtual; abstract;

    function GetChar: Char;
    function GetNonWhitespace: Char;
    procedure SkipLine;
    function GetToken: Boolean; virtual;
    procedure SkipWhitespace;
    procedure NextToken;

    function NextIdentifier (const errMsg: string = 'Identifier expected'): string;
    function NextString (const errMsg: string = 'String constant expected'): string;
    function NextInteger (const errMsg: string = 'Integer constant expected'): Integer;
    function NextChar (ch: Char): Char;

    procedure ExpectChar (ch: Char); virtual;
    function ExpectIdentifier (const errMsg: string = 'Identifier expected'): string;
    function ExpectString (const errMsg: string = 'String constant expected'): string; virtual;
    function ExpectInteger (const errMsg: string = 'Integer constant expected'): Integer; virtual;

    property LinePos: Integer read FLinePos;
    property LineNo: Integer read FLineNo;
    property Ch: Char read FCh;
    property SOL: Boolean read FSOL;
    property TokenType: Integer read FTokenType;
    property Whitespace: TCharSet read FWhitespace write FWhitespace;
    property Token: string read FToken;
    property TokenChar: Char read FTokenChar;
    property TokenSOL: Boolean read FTokenSol;

    property FirstIdChars: TCharSet read FFirstIdChars write FFirstIdChars;
    property FirstNumChars: TCharSet read FFirstNumChars write FFirstNumChars;
    property NextIdChars: TCharSet read FNextIdChars write FNextIdChars;
    property NextNumChars: TCharSet read FNextNumChars write FNextNumChars;
    property DecimalSeparator: Char read FDecimalSeparator write FDecimalSeparator;
  end;

const
  dtInclude = 1;
  dtDefine  = 2;
  dtIfdef   = 3;
  dtIfNDef  = 4;
  dtEndif   = 5;
  dtUndef   = 6;
  dtElse    = 7;
  dtIf      = 8;
  dtPragma  = 9;
  dtError   = 10;

type
  TCPreProcessor = class (TParser)
  private
    FPathName: string;
    FIdentifiers: TStringList;
    FIfLevel: Integer;
    FIncludePath: string;
    FDirectives: TStringList;
    FIncludeFile: string;

    procedure CheckIdentifiers;
    procedure DoInclude;
    procedure DoDefine;
    procedure DoIfDef;
    procedure DoIfNDef;
    procedure DoEndif;
    procedure DoUndef;
    procedure DoElse;
    procedure DoIf;
    procedure DoPragma;
    procedure DoError;
    function GetIncludePathName(const FileName: string): string;
    procedure SkipIfElseBlock;
  protected
    procedure HandleDirective;
    procedure HandlePragma(const st: string); virtual;
    function GetRestOfLine: string;
    procedure NextFileString (const errMsg: string);
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    property PathName: string read FPathName write FPathName;
    property IncludePath: string read FIncludePath write FIncludePath;
    property IncludeFile: string read FIncludeFile write FIncludeFile;
    function GetToken: Boolean; override;

    procedure ExpectChar (ch: Char); override;
    function ExpectString (const errMsg: string = 'String constant expected'): string; override;
    function ExpectInteger (const errMsg: string = 'Integer constant expected'): Integer; override;

    function Defined (id: string): Boolean;
    procedure AddIdentifier (const id, line: string);
    procedure DeleteIdentifier (const id: string);
    function IsIdentifier (const id: string) :Boolean;
    function Resolve(var TokenType: Integer; var st: string): TValue;
    function Calc (const st: string): TValue;
    function ResolveToken: TValue;
  end;

  EParser = class (Exception);
  EErrorPragma = class (Exception);

implementation

uses
  unitSearchString;

{ TParser }

constructor TParser.Create(AStream: TStream);
begin
  FReader := TStreamTextReader.Create(AStream);
  FWhitespace := [' ', #9, #10];
  FFirstIdChars := ['a'..'z', 'A'..'Z', '_'];
  FNextIdChars := FFirstIdChars + ['0'..'9'];
  FNextNumChars := ['0'..'9'];
  FFirstNumChars := ['$', '0'..'9'];
  FDecimalSeparator := '.';
  FSOL := True;
  FFirstChar := True;
end;

destructor TParser.Destroy;
begin
  FReader.Free;

  inherited;
end;

procedure TParser.ExpectChar(ch: Char);
begin
  if (TokenType <> ttChar) or (FTokenChar <> ch) then
    raise EParser.Create(ch + ' expected');
end;

function TParser.ExpectIdentifier(const errMsg: string): string;
begin
  if TokenType <> ttIdentifier then
    raise EParser.Create(errMsg);
  Result := FToken;
end;

function TParser.ExpectInteger(const errMsg: string): Integer;
begin
  if tokenType = ttNumber then
    Result := StrToInt(token)
  else
    raise EParser.Create(errMsg);
end;

function TParser.ExpectString(const errMsg: string): string;
begin
  if tokenType = ttString then
    Result := Token
  else
    raise EParser.Create(errMsg);
end;

function TParser.GetChar: Char;
var
  lineCont: Boolean;
begin
  lineCont := False;
  repeat
    if lineCont then
      MessageBeep ($ffff);
    while FLinePos >= Length(FLineBuf) do
    begin
      if not FReader.ReadLn(FLineBuf) then
      begin
        FCh := #0;
        Result := FCh;
        exit
      end;
      FLinePos := 0;
      FSOL := not LineCont;
      FFirstChar := FSOL;
      Inc(FLineNo);
    end;

    FCh := FLineBuf [FLinePos + 1];
    Inc(FLinePos);
    LineCont := True;
  until (FCh <> '\') or (FLinePos < Length(FLineBuf));

  if FSOL and not (FCh in Whitespace) then
  begin
    FSOL := FFirstChar;
    FFirstChar := False
  end;

  Result := FCh;
end;

function TParser.GetNonWhitespace: Char;
begin
  repeat
    Result := GetChar
  until not (Result in Whitespace)
end;

function TParser.GetToken: Boolean;
var
  st: string;

begin
  Result := True;
  FTokenType := ttUnknown;
  SkipWhitespace;
  FTokenSol := FSOL;
  if FCh in FirstIdChars then
    RawGetToken (NextIdChars, ttIdentifier)
  else
    if FCh in FirstNumChars then
    begin
      RawGetToken (NextNumChars, ttNumber);
      if (FCh = DecimalSeparator) and not FSOL then
      begin
        st := FToken;
        GetChar;
        RawGetToken (NextNumChars, ttNumber);
        FToken := st + DecimalSeparator + FToken
      end
      else
        if (FCh = 'x') and (Token = '0') then
        begin
          st := FToken;
          GetChar;
          RawGetToken (NextNumChars + ['A'..'F', 'a'..'f'], ttNumber);
          FToken := '$' + FToken;
        end

    end
    else
      if FCh = #0 then
        Result := False
      else
      begin
        FTokenChar := FCh;
        FTokenType := ttChar;
        GetChar;
      end
end;

function TParser.NextChar (ch :Char): Char;
begin
  NextToken;
  ExpectChar (ch);
  Result := FTokenChar;
end;

function TParser.NextIdentifier (const errMsg: string): string;
begin
  NextToken;
  ExpectIdentifier (errMsg);
end;

function TParser.NextInteger(const errMsg: string): Integer;
begin
  NextToken;
  Result := ExpectInteger (errMsg);
end;

function TParser.NextString(const errMsg: string): string;
begin
  NextToken;
  Result := ExpectString (errMsg);
end;

procedure TParser.NextToken;
begin
  GetToken;
end;

procedure TParser.RawGetToken(charset: TCharset; tp: Integer);
begin
  FToken := FCh;
  if FLinePos < Length(FLineBuf) then
    while GetChar in charset do
    begin
      FToken := FToken + FCh;
      if FLinePos = Length(FLineBuf) then
      begin
        GetChar;
        break
      end
    end
  else
    GetChar;
  FTokenType := tp
end;

procedure TParser.SkipLine;
begin
  if not FSOL then
  begin
    FLinePos := Length(FLineBuf);
    GetChar
  end
end;

procedure TParser.SkipWhitespace;
begin
  while FCh in Whitespace do
    GetChar;
end;

{ TCPreProcessor }

procedure TCPreProcessor.AddIdentifier(const id, line: string);
begin
  if not Defined (id) then
  begin
    CheckIdentifiers;
    FIdentifiers.AddObject(id, TStrValue.Create(line))
  end
end;

function TCPreProcessor.Calc(const st: string): TValue;
begin
  CalcExpression (st, FIdentifiers, Result);
end;

procedure TCPreProcessor.CheckIdentifiers;
begin
  if FIdentifiers = Nil then
  begin
    FIdentifiers := TStringList.Create;
    FIdentifiers.Duplicates := dupError;
    FIdentifiers.CaseSensitive := True;
    FIdentifiers.Sorted := True
  end
end;

constructor TCPreProcessor.Create(AStream: TStream);
begin
  inherited Create(AStream);

  FirstNumChars := ['0'..'9'];

  FDirectives := TStringList.Create;
  FDirectives.CaseSensitive := True;
  FDirectives.AddObject('include', TObject(dtInclude));
  FDirectives.AddObject('define',  TObject(dtDefine));
  FDirectives.AddObject('ifdef',   TObject(dtIfdef));
  FDirectives.AddObject('ifndef',  TObject(dtIfndef));
  FDirectives.AddObject('endif',   TObject(dtEndif));
  FDirectives.AddObject('undef',   TObject(dtUndef));
  FDirectives.AddObject('else',    TObject(dtElse));
  FDirectives.AddObject('if',      TObject(dtIf));
  FDirectives.AddObject('pragma',  TObject(dtPragma));
  FDirectives.AddObject('error',   TObject(dtError));
  FDirectives.Sorted := True;
end;

function TCPreProcessor.Defined(id: string): Boolean;
begin
  if FIdentifiers = Nil then
    Result := False
  else
    Result := FIdentifiers.IndexOf (id) >= 0
end;

procedure TCPreProcessor.DeleteIdentifier(const id: string);
begin
  if Defined (id) then
  begin
    CheckIdentifiers;
    FIdentifiers.Delete(FIdentifiers.IndexOf (id))
  end
  else
    raise EParser.CreateFmt('Identifier %s not found', [id]);
end;

destructor TCPreProcessor.Destroy;
var
  i: Integer;
begin
  if Assigned(FIdentifiers) then
  begin
    for i := FIdentifiers.Count - 1 downto 0 do
      FIdentifiers.Objects[i].Free;
    FIdentifiers.Free
  end;

  FDirectives.Free;

  inherited;
end;

procedure TCPreProcessor.DoDefine;
var
  id: string;
begin
  NextIdentifier ('Identifier expected in #define');
  id := FToken;
  GetRestOfLine;
  AddIdentifier (id, FToken);
end;

procedure TCPreProcessor.DoElse;
begin
  if FIfLevel > 0 then
    SkipIfElseBlock
  else
    raise EParser.Create('Unexpected #else');
  SkipLine;
end;

procedure TCPreProcessor.DoEndif;
begin
  if FIfLevel > 0 then
  begin
    Dec(FIfLevel);
    SkipLine;
  end
  else
    raise EParser.Create('Unexpected endif');

end;

procedure TCPreProcessor.DoError;
begin
  GetRestOfLine;
  if IncludeFile = '' then
    raise EErrorPragma.CreateFmt('#Error in line %d %s', [FLineNo, FToken])
  else
    raise EErrorPragma.CreateFmt('#Error in line %d:s %s', [FLineNo, IncludeFile, FToken])
end;

procedure TCPreProcessor.DoIf;
var
  expr: string;
  val: TValue;
begin
  GetRestOfLine;
  expr := FToken;

  CalcExpression (expr, FIdentifiers, val);

  if (val.tp <> vInteger) then
   raise EParser.Create('Must be an integer expression');

  Inc(FIfLevel);

  if val.iVal = 0 then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoIfDef;
begin
  NextIdentifier ('Identifier expected in ifdef');
  Inc(FIfLevel);
  SkipLine;

  if not Defined (FToken) then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoIfNDef;
begin
  NextIdentifier ('Identifier expected in ifdef');
  Inc(FIfLevel);
  SkipLine;

  if Defined (FToken) then
    SkipIfElseBlock;
  SkipLine;
end;

procedure TCPreProcessor.DoInclude;
var
  oldReader: TStreamTextReader;
  oldLinePos, oldLineNo, oldIfLevel: Integer;
  oldLineBuf: string;
  oldSOL: Boolean;
  oldCh: Char;
  oldPathName: string;
  r: TStreamTextReader;
  f: TFileStream;
  fName: string;
begin
  NextFileString ('File name expected');

  oldReader := FReader;
  oldLinePos := FLinePos;
  oldLineNo := FLineNo;
  oldLineBuf := FLineBuf;
  oldIfLevel := FIfLevel;
  oldSol := FSOL;
  oldCh := FCh;
  oldPathName := PathName;

  r := nil;
  fName := GetIncludePathName(FToken);
  f := TFileStream.Create(fName, fmOpenRead or fmShareDenyWrite);
  try
    r := TStreamTextReader.Create(f);
    FLinePos := 0;
    FLineNo := 0;
    FReader := r;
    FSOL := True;
    FFirstChar := True;
    FLineBuf := '';
    FIfLevel := 0;
    PathName := ExtractFilePath(fName);
    IncludeFile := ExtractFileName(fName);

    Parse;
  finally
    FReader := oldReader;
    FLinePos := oldLinePos;
    FLineNo := oldLineNo;
    FFirstChar := False;
    FLineBuf := oldLineBuf;
    FIfLevel := oldIfLevel;
    FSOL := oldSOL;
    FCh := oldCh;
    FPathName := oldPathName;
    r.Free;
    f.Free
  end;
  SkipLine;
end;

procedure TCPreProcessor.DoPragma;
begin
  HandlePragma(GetRestOfLine);
end;

procedure TCPreProcessor.DoUndef;
var
  id: string;
begin
  NextIdentifier ('Identifier expected in #undef');
  id := FToken;
  GetRestOfLine;

  DeleteIdentifier (id);
end;

procedure TCPreProcessor.ExpectChar(ch: Char);
begin
  inherited;

end;

function TCPreProcessor.ExpectInteger(const errMsg: string): Integer;
begin
  if TokenType = ttIdentifier then
    Resolve(FTokenType, FToken);

  Result := inherited ExpectInteger (errMsg);
end;

function TCPreProcessor.ExpectString(const errMsg: string): string;
begin
  if TokenType = ttIdentifier then
    Resolve(FTokenType, FToken);

  Result := inherited ExpectString (errMsg);
end;

function TCPreProcessor.GetIncludePathName(const FileName: string): string;
var
  st, p: string;
  ch: Char;
begin
  Result := PathName + fileName;

  if not FileExists (Result) then
  begin
    st := IncludePath;
    if st = '' then
      st := GetEnvironmentVariable('include');

    while st <> '' do
    begin
      p := SplitString (';', st);

      if p <> '' then
      begin
        ch := p [Length(p)];
        if (ch <> '\') and (ch <> ':') then
          p := p + '\';

        Result := p + fileName;
        if FileExists (Result) then
          Exit
      end
    end;

    Result := FileName
  end
end;

function TCPreProcessor.GetRestOfLine: string;
begin
  Result := '';
  if FSOL then
  begin
    FToken := '';
    exit;
  end;
  SkipWhitespace;
  FToken := '';
  repeat
    if FCh = '/' then
      if GetChar = '/' then
      begin
        SkipLine;
        break
      end
      else
        if FCh = '*' then
        begin
          GetChar;
          repeat
            if FLinePos >= Length(FLineBuf) then
              break;

            if FCh = '*' then
            begin
              if GetChar = '/' then
                break
            end
            else
              GetChar
          until FLinePos >= Length(FLineBuf)
        end
        else
          FToken := FToken + '/' + FCh
    else
      FToken := FToken + FCh;
    if FLinePos = Length(FLineBuf) then
    begin
      GetChar;
      break
    end;
    GetChar
  until False;
  FToken := Trim (FToken);
  Result := FToken;
end;

function TCPreProcessor.GetToken: Boolean;
var
  retry: Boolean;
begin
  Result := True;
  retry := True;
  while retry do
  begin
    retry := False;
    Result := inherited GetToken;
    if not Result then
    begin
      if FIfLevel <> 0 then
        raise EParser.Create('Unexpected end of file');
      Exit;
    end;

    if TokenType = ttChar then
    case FTokenChar of
      '/': if FCh = '/' then
            begin
              SkipLine;
              retry := True;
            end
            else
            if FCh = '*' then
            begin
              repeat
                if GetChar = #0 then break;
                if (FCh = '*') and (GetChar = '/') then
                  break;
              until FCh = #0;
              GetChar;
              retry := True;
            end
            else
              FTokenType := ttOpDiv;
      '#' :
            if FTokenSol then
            begin
              inherited GetToken;
              if FTokenType <> ttIdentifier then
                raise EParserError.Create('Syntax error in directive');
              HandleDirective;
              retry := True;
            end;
      '"': begin
              FToken := '';
              while FCh <> #0 do
              begin
                case FCh of
                  '"': break;
                  '\' :
                    case GetChar of
                      '"': FToken := FToken + '"';
                      'n': FToken := FToken + #10;
                      'r': FToken := FToken + #13;
                      't': FToken := FToken + #9;
                      '\': FToken := FToken + '\';
                      '0': FToken := FToken + #0;
                      else
                        raise EParserError.Create('Invalid escape sequence');
                    end;
                  else
                    FToken := FToken + FCh
                end;
                GetChar
              end;
              FTokenType := ttString;
              GetChar;
            end;
    end
  end
end;

procedure TCPreProcessor.HandleDirective;
var
  idx: Integer;
begin
  idx := FDirectives.IndexOf(LowerCase(FToken));
  if idx >= 0 then
  case Integer (FDirectives.Objects[idx]) of
    dtInclude: DoInclude;
    dtDefine : DoDefine;
    dtIfDef  : DoIfDef;
    dtIfnDef : DoIfNDef;
    dtEndif  : DoEndif;
    dtUndef  : DoUndef;
    dtElse   : DoElse;
    dtIf     : DoIf;
    dtPragma : DoPragma;
    dtError  : DoError;
    else
      raise EParser.Create('Unknown directive #' + FToken)
  end
  else
    raise EParser.Create('Unknown directive #' + FToken)
end;

procedure TCPreProcessor.HandlePragma(const st: string);
begin

end;

function TCPreProcessor.IsIdentifier(const id: string): Boolean;
begin
  Result := FIdentifiers.IndexOf(id) >= 0
end;

procedure TCPreProcessor.NextFileString(const errMsg: string);
begin
  GetChar;
  if FCh = '<' then
  begin
    GetChar;
    FToken := '';
    while FCh <> #0 do
    begin
      case FCh of
        '>': break;
        else
          FToken := FToken + FCh
      end;
      GetChar
    end;
    FTokenType := ttString
  end
  else
    if ch = '"' then
    begin
      GetChar;
      FToken := '';
      while FCh <> #0 do
      begin
        case FCh of
          '"': break;
          else
            FToken := FToken + FCh
        end;
        GetChar
      end;
      FTokenType := ttString
    end
    else
      GetToken;

  if TokenType <> ttString then
    raise EParser.Create(errMsg);
end;

function TCPreProcessor.Resolve(var TokenType: Integer; var st: string): TValue;
begin
  if (TokenType = ttIdentifier) and IsIdentifier (st) then
  begin
    CalcExpression (st, FIdentifiers, Result);

    case Result.tp of
      vString : begin st := Result.sVal; tokenType := ttString; end;
      vInteger: begin st := IntToStr (Result.iVal); tokenType := ttNumber; end;
      vReal   : begin st := FloatToStr (Result.rVal); tokenType := ttNumber; end;
    end
  end
end;

function TCPreProcessor.ResolveToken: TValue;
begin
  if (TokenType = ttNumber) or (TokenType = ttIdentifier) then
    Result := Calc (Token)
  else
    if TokenType = ttString then
    begin
      Result.tp := vString;
      Result.sVal := Token
    end
    else
      raise EParser.Create('Value expected');

end;

procedure TCPreProcessor.SkipIfElseBlock;
var
  level: Integer;
begin
  level := 0;
  repeat
    SkipWhitespace;
    if FCh = #0 then Break;
    if FCh = '#' then
    begin
      GetChar;
      inherited GetToken;
      if FToken = 'endif' then
        if level = 0 then
          break
        else
          Dec(level)
      else
        if (FToken = 'ifdef') or (FToken = 'ifndef') or (FToken = 'if') then
          Inc(level)
    end
    else
      FSOL := False;
    SkipLine
  until False;
  Dec(FIfLevel)
end;

end.
