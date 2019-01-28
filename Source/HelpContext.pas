unit HelpContext;

interface

uses Windows, Classes, SysUtils;

const
  NO_CONTEXTS = 2;
  hcAddResourceDialog = 1000;
  hcNewImageDialog = 1001;

function HelpLink (context : THelpContext) : string;

implementation

type
  THelpLink = record
    context : THelpContext;
    jump : string;
  end;

const
  HelpLinks : array [0..NO_CONTEXTS - 1] of THelpLink = (
    (context:hcAddResourceDialog; jump:'AddResourceDialog.htm'),
    (context:hcNewImageDialog; jump:'NewImage.htm')
  );

function HelpLink (context : THelpContext) : string;

  function bsearch (s, e : Integer) : string;
  var
    m : Integer;
  begin
    if e >= s then
    begin
      m := s + (e - s) div 2;

      if context > HelpLinks [m].context then
        result := bsearch (m + 1, e)
      else
        if context < HelpLinks [m].context then
          result := bsearch (s, m - 1)
        else
          result := HelpLinks [m].jump
    end
    else
      result := 'notfound.htm';
  end;

begin
  result := bsearch (0, NO_CONTEXTS - 1)
end;


end.
