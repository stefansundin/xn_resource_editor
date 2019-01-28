unit cmpHyperlinkButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ImgList;

type
  THyperlinkButton = class(TGraphicControl)
  private
    fImageIndex: Integer;
    fImages: TImageList;
    FMouseInControl : Boolean;
    FFontColor : TColor;
    FFontStyles : TFontStyles;
    fInPlace: Boolean;
    fLink: string;
    fAutoLink: Boolean;
    fParentObj: IUnknown;
    fSelectedFont: TFont;
    fSelectedFontColor: TColor;
    fSelectedFontStyles: TFontStyles;
    fSelected : boolean;
    fOnEndCapture: TNotifyEvent;
    fOnPainted: TNotifyEvent;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure SetImageIndex(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    procedure SetTransparent(const Value: Boolean);
    function GetTransparent: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure UpdateTracking;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    property ParentObj : IUnknown read fParentObj write fParentObj;
    property Canvas;

  published
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property Caption;
    property Color;
    property Font;
    property Images : TImageList read fImages write SetImages;
    property ImageIndex : Integer read fImageIndex write SetImageIndex;
    property ParentColor;
    property ParentFont;
    property SelectedFontColor : TColor read fSelectedFontColor write fSelectedFontColor;
    property SelectedFontStyles : TFontStyles read fSelectedFontStyles write fSelectedFontStyles;
    property Transparent : Boolean read GetTransparent write SetTransparent default False;
    property Visible;
    property Link : string read fLink write fLink;
    property AutoLink : Boolean read fAutoLink write fAutoLink;
    property InPlace : Boolean read fInPlace write fInPlace;
    property OnEndCapture : TNotifyEvent read fOnEndCapture write fOnEndCapture;
    property OnPainted : TNotifyEvent read fOnPainted write fOnPainted;
  end;

implementation

uses shellapi, urlmon;

{ THyperlinkButton }

procedure THyperlinkButton.Click;
var
  ext, url : string;
  done : Boolean;
  f : system.Text;
  cmd, param : string;
  pp : PChar;
  p : Integer;
  inQuote : boolean;
  ch : char;
begin
  inherited;

  if fAutoLink then
  begin
    done := False;

    if fInPlace then
    begin
      param := AnsiDequotedStr (Link, '"');
      ext := ExtractFileExt (param);

      if (CompareText (ext, '.URL') = 0) or (CompareText (ext, '.HTML') = 0) or (CompareText (ext, '.HTM') = 0) then
      begin
        if CompareText (ext, '.URL') = 0 then
        begin
          AssignFile (f, param);
          Reset (f);
          try
            ReadLn (f, url)
          finally
            CloseFile (f)
          end
        end
        else
          url := param;

        if url <> '' then
        begin
          HLinkNavigateString (fParentObj, PWideChar (WideString (url)));
          done := True
        end
      end
    end;


    if not Done then
    begin
      p := 1;
      InQuote := False;
      while p <= Length (Link) do
      begin
        ch := Link [p];
        if ch = '"' then
          InQuote := not inQuote
        else
          if ch = ' ' then
            if not InQuote then
              break;
        Inc (p)
      end;
      if p > Length (link) then
        p := 0;

      if p > 0 then
      begin
        cmd := Copy (Link, 1, p - 1);
        param := Copy (Link, p + 1, MaxInt);
        pp := PChar (param)
      end
      else
      begin
        cmd := AnsiDequotedStr (Link, '"');
        pp := Nil
      end;
      ShellExecute (TCustomForm (Owner).Handle, 'Open', PChar (cmd), pp, nil, SW_SHOWNORMAL)
    end
  end

end;

procedure THyperlinkButton.CMMouseEnter(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) and not fSelected then
  begin
    FSelected := True;

    FFontColor := Font.Color;
    FFontStyles := Font.Style;

    Font.Color := SelectedFontColor;
    Font.Style := SelectedFontStyles
  end
end;

procedure THyperlinkButton.CMMouseLeave(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    fSelected := False;
    Font.Color := FFontColor;
    Font.Style := FFontStyles
  end
end;

procedure THyperlinkButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate
end;

constructor THyperlinkButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  fSelectedFont := TFont.Create;

  Width := 64;
  Height := 16
end;

function THyperlinkButton.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure THyperlinkButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateTracking
end;

procedure THyperlinkButton.Paint;
var
  Rect: TRect;
  FontHeight: Integer;
  Flags: Longint;

begin
  Rect := GetClientRect;
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;

    Font := Self.Font;
    FontHeight := TextHeight('W');

    if Assigned (fImages) and (fImageIndex >=0) and (fImageIndex < fImages.Count) then
    begin
      fImages.Draw (Canvas, rect.Left, rect.Top, fImageIndex);
      Rect.Left := Rect.Left + fImages.Width + 8
    end;

    with Rect do
    begin
      Top := ((Bottom + Top) - FontHeight) div 2;
      Bottom := Top + FontHeight;
    end;
    Flags := DT_EXPANDTABS or DT_VCENTER or DT_LEFT or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);

    SetBkMode (Handle, Windows.TRANSPARENT);
    DrawText(Handle, PChar(Caption), -1, Rect, Flags);
  end;

  If Assigned (OnPainted) and not (csDestroying in ComponentState) then
    OnPainted (self);

end;

procedure THyperlinkButton.SetImageIndex(const Value: Integer);
begin
  if fImageIndex <> Value then
  begin
    fImageIndex := Value;
    Invalidate
  end
end;

procedure THyperlinkButton.SetImages(const Value: TImageList);
begin
  if fImages <> Value then
  begin
    fImages := Value;
    Invalidate
  end
end;

procedure THyperlinkButton.SetTransparent(const Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end
end;

procedure THyperlinkButton.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled and not (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
    begin
      Perform(CM_MOUSELEAVE, 0, 0);
      if Assigned (OnEndCapture) then
        OnEndCapture (self);
      SetCaptureControl (nil)
    end
    else
    begin
      if GetCaptureControl = nil then
      begin
        Perform(CM_MOUSEENTER, 0, 0);
        SetCaptureControl (Self)
      end
    end
  end;
end;

end.
