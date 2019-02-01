unit SpellCheckerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, cmpCWRichEdit, cmpCWSpellChecker,
  cmpPersistentPosition;

const
  WM_SETUP = WM_USER + $200;

type
  TfmSpellChecker = class(TForm)
    LabelUnknownWord: TLabel;
    LabelSuggestions: TLabel;
    ButtonChange: TButton;
    ButtonChangeAll: TButton;
    ButtonSkipAll: TButton;
    ButtonSkip: TButton;
    ButtonAdd: TButton;
    ButtonCancel: TButton;
    ButtonFinish: TButton;
    reText: TExRichEdit;
    ListViewSuggestions: TListView;
    PersistentPosition: TPersistentPosition;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonSkipClick(Sender: TObject);
    procedure ButtonChangeClick(Sender: TObject);
    procedure ButtonChangeAllClick(Sender: TObject);
    procedure ListViewSuggestionsDblClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonSkipAllClick(Sender: TObject);
  private
    FChecker: TCWSpellChecker;
    FText: WideString;
    FBadWord: WideString;
    FBadSS: Integer;
    FIgnoreWords: TStringList;
    FSS, FSE: Integer;
    FQuoteChars: string;
    procedure ErrorFoundAt(ss, se: Integer; suggestions: TStrings);
    procedure NextWord;
    function GetChangedWord: WideString;
    procedure WmSetup (var message: TMessage); message WM_SETUP;
  protected
    procedure UpdateActions; override;
    procedure Loaded; override;
  public
    destructor Destroy; override;
    procedure Initialize(checker: TCWSpellChecker; ss, se: Integer; suggestions: TStrings);
    property QuoteChars: string read FQuoteChars write FQuoteChars;
  end;

var
  fmSpellChecker: TfmSpellChecker;

implementation

uses
  unitCharsetMap;

{$R *.dfm}

{ TfmSpellChecker }

destructor TfmSpellChecker.Destroy;
begin
  fmSpellChecker := nil;
  FIgnoreWords.Free;

  inherited;
end;

procedure TfmSpellChecker.Initialize(checker: TCWSpellChecker; ss, se: Integer; suggestions: TStrings);
begin
  FChecker := checker;
  FText := FChecker.ExRichEdit.Text;
  reText.CodePage := FChecker.ExRichEdit.CodePage;
  reText.Text := FText;
  FChecker.ExRichEdit.SetRawSelection(ss, se);
  FSS := ss;
  FSE := se;
  reText.SetRawSelection (ss, se);
  FBadWord := FChecker.ExRichEdit.SelText;

  ErrorFoundAt(ss, se, suggestions);
end;


procedure TfmSpellChecker.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree
end;

procedure TfmSpellChecker.ErrorFoundAt(ss, se: Integer;
  Suggestions: TStrings);
var
  i: Integer;
begin
  FBadSS := ss;

  ListViewSuggestions.Items.BeginUpdate;
  try
    ListViewSuggestions.Items.Clear;
    for i := 0 to Suggestions.Count - 1 do
      with ListViewSuggestions.Items.Add do
        Caption := suggestions[i];
  finally
    ListViewSuggestions.Items.EndUpdate;
  end
end;

procedure TfmSpellChecker.UpdateActions;
var
  ErrorFound: Boolean;
  CanChange: Boolean;
begin
  ErrorFound := FBadWord <> '';

  if ErrorFound then
  begin
    ButtonSkip.Enabled := True;
    ButtonSkipAll.Enabled := True;
    CanChange := (ListViewSuggestions.ItemIndex <> -1) or (reText.Text <> FText);
    ButtonChange.Enabled := CanChange;
    ButtonChangeAll.Enabled := CanChange;
  end
  else
  begin
    ButtonSkip.Enabled := False;
    ButtonSkipAll.Enabled := False;
    ButtonChange.Enabled := False;
    ButtonChangeAll.Enabled := False;
  end
end;

procedure TfmSpellChecker.ButtonSkipClick(Sender: TObject);
begin
  NextWord;
end;

procedure TfmSpellChecker.NextWord;
var
  ss, se: Integer;
  suggestions: TStrings;
  ok: Boolean;
begin
  repeat
    reText.GetRawSelection(ss, se);

    suggestions := TStringList.Create;
    try
      ok := FChecker.Check(FText, se + 1, ss, se, suggestions);
      if not ok then
      begin
        FChecker.ExRichEdit.SetRawSelection(ss, se);
        reText.SetRawSelection (ss, se);
        FBadWord := FChecker.ExRichEdit.SelText;
        if not Assigned(FIgnoreWords) or (FIgnoreWords.IndexOf(FBadWord) = -1) then
        begin
          ErrorFoundAt(ss, se, suggestions);
          break
        end
      end
      else
      begin
        ModalResult := mrOK;
        break
      end
    finally
      suggestions.Free
    end
  until False
end;

procedure TfmSpellChecker.ButtonChangeClick(Sender: TObject);
begin
  if reText.Text <> FText then
  begin
    FText := reText.Text;
    FChecker.ExRichEdit.Text := FText
  end
  else
  begin
    FChecker.ExRichEdit.SelText := ListViewSuggestions.Selected.Caption;
    FText := FChecker.ExRichEdit.Text;
    reText.SelText := ListViewSuggestions.Selected.Caption
  end;
  NextWord
end;

procedure TfmSpellChecker.ButtonChangeAllClick(Sender: TObject);
var
  ss: Integer;
  newWord: string;
begin
  if reText.Text <> FText then
    newWord := GetChangedWord
  else
    newWord := ListViewSuggestions.Selected.Caption;

  ss := FBadSS - 1;

  FText := Copy(FText, 1, ss) + StringReplace(Copy(FText, ss + 1, MaxInt), FBadWord, newWord, [rfReplaceAll, rfIgnoreCase]);
  FChecker.ExRichEdit.Text := FText;
  reText.Text := FText;

  reText.SetRawSelection(ss + 1, ss + 1 + Length(newWord));

  NextWord;
end;

procedure TfmSpellChecker.ListViewSuggestionsDblClick(Sender: TObject);
begin
  ButtonChangeClick(nil)
end;

function TfmSpellChecker.GetChangedWord: WideString;
var
  changedText: WideString;
  p, l, ew: Integer;
  ch: WideChar;
begin
  changedText := reText.Text;
  l := Length(changedText);
  p := FBadSS;

  ew := -1;

  while p < l do
  begin
    ch := changedText [p];

    if IsWideCharAlNum (ch) or (word (ch) = Word ('''')) then
      ew := p
    else
      break;

    Inc(p)
  end;

  if ew = -1 then
    Result := ''
  else
    Result := Copy(changedText, FBadSS, ew - FBadSS + 1);
end;

procedure TfmSpellChecker.ButtonAddClick(Sender: TObject);
begin
  FChecker.Add (FBadWord);
  NextWord
end;

procedure TfmSpellChecker.ButtonSkipAllClick(Sender: TObject);
begin
  if not Assigned(FIgnoreWords) then
  begin
    FIgnoreWords := TStringList.Create;
    FIgnoreWords.CaseSensitive := False
  end;
  FIgnoreWords.Add(FBadWord);
  NextWord
end;

procedure TfmSpellChecker.FormShow(Sender: TObject);
begin
  reText.SetRawSelection (FSS, FSE);
  PostMessage(handle, WM_SETUP, 0, 0);
end;

procedure TfmSpellChecker.WmSetup(var message: TMessage);
begin
end;

procedure TfmSpellChecker.Loaded;
begin
  inherited;
end;

end.
