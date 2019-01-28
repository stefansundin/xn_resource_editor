unit cmpCWSpellChecker;

interface

uses
  SysUtils, Classes, Controls, cmpSpellChecker, cmpCWRichEdit;

type
  TCWSpellChecker = class(TSpellChecker)
  private
    fEXRichEdit: TCustomExRichEdit;
  protected
  public
    destructor Destroy; override;
    function CheckAndShowModal (SkipFirstLine : boolean) : Integer;
  published
    property ExRichEdit : TCustomExRichEdit read fEXRichEdit write fEXRichEdit;
  end;

implementation

uses
  SpellCheckerForm;

{ TCWSpellChecker }

function TCWSpellChecker.CheckAndShowModal (SkipFirstLine : boolean) : Integer;
var
  ss, se : Integer;
  txt : WideString;
  suggestions : TStrings;
begin
  result := mrOK;
  if not Assigned (ExRichEdit) then Exit;
  if Assigned (fmSpellChecker) then Exit;

  txt := ExRichEdit.Text;
  suggestions := TStringList.Create;
  try
    if not Check (txt, 1, ss, se, suggestions, SkipFirstLine) then
    begin
      fmSpellChecker := TfmSpellChecker.Create(Owner);
      fmSpellChecker.QuoteChars := QuoteChars;
      fmSpellChecker.Initialize (self, ss, se, suggestions);
      result := fmSpellChecker.ShowModal
    end
  finally
    suggestions.Free
  end
end;

destructor TCWSpellChecker.Destroy;
begin
  if Assigned (fmSpellChecker) then
    fmSpellChecker.Close;

  inherited;
end;

end.
