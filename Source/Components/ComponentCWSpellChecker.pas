unit ComponentCWSpellChecker;

interface

uses
  SysUtils, Classes, Controls, ComponentSpellChecker, ComponentCWRichEdit;

type
  TCWSpellChecker = class(TSpellChecker)
  private
    FEXRichEdit: TCustomExRichEdit;
  protected
  public
    destructor Destroy; override;
    function CheckAndShowModal (SkipFirstLine: Boolean): Integer;
  published
    property ExRichEdit: TCustomExRichEdit read FEXRichEdit write FEXRichEdit;
  end;

implementation

uses
  SpellCheckerForm;

{ TCWSpellChecker }

function TCWSpellChecker.CheckAndShowModal (SkipFirstLine: Boolean): Integer;
var
  ss, se: Integer;
  Txt: WideString;
  Suggestions: TStrings;
begin
  Result := mrOK;
  if not Assigned(ExRichEdit) then Exit;
  if Assigned(fmSpellChecker) then Exit;

  Txt := ExRichEdit.Text;
  Suggestions := TStringList.Create;
  try
    if not Check(Txt, 1, ss, se, Suggestions, SkipFirstLine) then
    begin
      fmSpellChecker := TfmSpellChecker.Create(Owner);
      fmSpellChecker.QuoteChars := QuoteChars;
      fmSpellChecker.Initialize(Self, ss, se, Suggestions);
      Result := fmSpellChecker.ShowModal;
    end
  finally
    Suggestions.Free
  end
end;

destructor TCWSpellChecker.Destroy;
begin
  if Assigned(fmSpellChecker) then
    fmSpellChecker.Close;

  inherited;
end;

end.
