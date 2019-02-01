unit DialogConsts;

interface

uses
  Windows, Classes, SysUtils;

const
  BUTTON_ID    = $80;
  EDIT_ID      = $81;
  STATIC_ID    = $82;
  LISTBOX_ID   = $83;
  SCROLLBAR_ID = $84;
  COMBOBOX_ID  = $85;

type
  TSZOrID = record
    isID : Boolean;
    sz : string;
    id : Integer;
  end;

  TDlgTemplateEx = packed record
    dlgVer : word;
    signature : word;
    helpID : DWORD;
    exStyle : DWORD;
    style : DWORD;
    cDlgItems : WORD;
    x, y, cx, cy : smallint;

    // Then follows menu, class, title sz or id

    // if DS_SETFONT in style then follows

(*
    pointsize : Word;
    weight : Word;
    italic : Byte;
    charset : Byte;
    typeface : TszOrID;  //(sz only!)
*)

  end;

  PDlgTemplateEx = ^TDlgTemplateEx;

  TDlgItemTemplateEx = packed record
    helpID : DWORD;
    exStyle : DWORD;
    Style : DWORD;
    x : Smallint;
    y : Smallint;
    cx : Smallint;
    cy : Smallint;
    id : Word;

    // Then follows class and title sz or ID

    // Then follows extraCount : WORD, followed by creation data
  end;
  PDlgItemTemplateEx = ^TDlgItemTemplateEx;


procedure WriteSzOrId (stream : TStream; const id : TSzOrId);
function StringToSzOrID (const st : string) : TszOrID;
procedure Pad (stream : TStream);

implementation

procedure WriteSzOrId (stream : TStream; const id : TSzOrId);
var
  w : Word;
  ws : WideString;
begin
  if id.isID then
  begin
    w := $ffff;
    stream.Write(w, SizeOf(w));

    w := id.id;
    stream.Write(w, SizeOf(w))
  end
  else
  begin
    ws := id.sz;
    stream.Write(ws[1], (Length(ws) + 1) * SizeOf(WideChar))
  end
end;

function StringToSzOrID (const st : string) : TszOrID;
var
  i : Integer;
begin
  result.isID := True;
  result.sz := st;

  for i := 1 to Length(st) do
    if not (st [i] in ['0'..'9']) then
    begin
      result.isID := False;
      break
    end;

  if result.isID then
  begin
    result.id := StrToInt(st);
    if result.id > $ffff then
      result.isID := False
  end;

  if result.isID then
    result.sz := ''
  else
    result.id := 0
end;

procedure Pad (stream : TStream);
var
  padding : Integer;
begin
  if stream.Position mod 4 <> 0 then
  begin
    padding := 0;
    stream.Write(padding, 4 - (stream.Position mod 4))
  end
end;

end.
