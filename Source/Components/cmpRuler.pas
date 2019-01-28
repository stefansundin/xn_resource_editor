unit cmpRuler;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TRulerOrientation = (ruHorizontal, ruVertical);
  TRuler = class(TCustomControl)
  private
    fSmallTickSpacing: Integer;
    fSmallTickLength: Integer;
    fSmallTicksPerLargeTick: Integer;
    fLargeTickLength: Integer;
    fDialogBox: HWND;
    procedure SetLargeTickLength(const Value: Integer);
    procedure SetOrientation(const Value: TRulerOrientation);
    procedure SetSmallTickLength(const Value: Integer);
    procedure SetSmallTickSpacing(const Value: Integer);
    procedure SetSmallTicksperLargeTick(const Value: Integer);
    function GetOrientation: TRulerOrientation;
    procedure SetDialogBox(const Value: HWND);
    { Private declarations }
  protected
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create (AOwner : TComponent); override;
    property DialogBox : HWND read fDialogBox write SetDialogBox;
  published
    property Align;
    property Anchors;
    property BevelKind default bkTile;
    property BevelInner default bvLowered;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property Color;
    property Constraints;
    property ParentColor;

    property SmallTickSpacing : Integer read fSmallTickSpacing write SetSmallTickSpacing default 10;
    property SmallTicksPerLargeTick : Integer read fSmallTicksPerLargeTick write SetSmallTicksperLargeTick default 5;
    property SmallTickLength : Integer read fSmallTickLength write SetSmallTickLength default 5;
    property LargeTickLength : Integer read fLargeTickLength write SetLargeTickLength default 10;
    property Orientation : TRulerOrientation read GetOrientation write SetOrientation stored False;
  end;

implementation

{ TRuler }

constructor TRuler.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Width := 180;
  Height := 40;
  BevelKind := bkTile;
  BevelInner := bvLowered;
  BevelOuter := bvLowered;
  fLargeTickLength := 10;
  fSmallTickLength := 5;
  fSmallTicksPerLargeTick := 5;
  fSmallTickSpacing := 10;
end;

function TRuler.GetOrientation: TRulerOrientation;
begin
  if Width > Height then
    result := ruHorizontal
  else
    result := ruVertical
end;

procedure TRuler.Loaded;
begin
  inherited;
end;

procedure TRuler.Paint;
var
  x, y : Integer;
  w, h : Integer;
  t : Integer;
  sm : Integer;
  r : TRect;
  offset : Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.Font := Font;

  w := ClientWidth;
  h := ClientHeight;

  if fDialogBox <> 0 then
    sm := fSmallTickSpacing
  else
    sm := fSmallTickSpacing;

  y := 0;
  x := 0;
  offset := 0;
  t := 0;
  
  if Orientation = ruHorizontal then
  begin
    repeat
      Inc (offset, sm);
      if fDialogBox <> 0 then
      begin
        r := Rect (0, 0, offset, 10);
        MapDialogRect (fDialogBox, r);
        x := r.Right
      end
      else
        x := offset;
      Inc (t);
      if x < w then
      begin
        Canvas.MoveTo (x, y);
        if t = fSmallTicksPerLargeTick then
        begin
          Canvas.LineTo (x, y + fLargeTickLength);
          t := 0
        end
        else
          Canvas.LineTo (x, y + fSmallTickLength)
      end
    until x >= w
  end
  else
  begin
    repeat
      Inc (offset, sm);
      if fDialogBox <> 0 then
      begin
        r := Rect (0, 0, 10, offset);
        MapDialogRect (fDialogBox, r);
        y := r.Bottom
      end
      else
        y := offset;
        
      Inc (t);
      if y < h then
      begin
        Canvas.MoveTo (x, y);
        if t = fSmallTicksPerLargeTick then
        begin
          Canvas.LineTo (x + fLargeTickLength, y);
          t := 0
        end
        else
          Canvas.LineTo (x + fSmallTickLength, y)
      end
    until y >= h
  end
end;

procedure TRuler.SetDialogBox(const Value: HWND);
begin
  fDialogBox := Value;
  invalidate
end;

procedure TRuler.SetLargeTickLength(const Value: Integer);
begin
  if value <> fLargeTickLength then
  begin
    fLargeTickLength := Value;
    Invalidate
  end
end;

procedure TRuler.SetOrientation(const Value: TRulerOrientation);
var
  h : Integer;
begin
  if value <> Orientation then
  begin
    h := Height;
    Height := Width;
    Width := h;
    Invalidate
  end
end;

procedure TRuler.SetSmallTickLength(const Value: Integer);
begin
  if value <> fSmallTickLength then
  begin
    fSmallTickLength := Value;
    Invalidate
  end
end;

procedure TRuler.SetSmallTickSpacing(const Value: Integer);
begin
  if value <> fSmallTickSpacing then
  begin
    fSmallTickSpacing := Value;
    Invalidate
  end
end;

procedure TRuler.SetSmallTicksperLargeTick(const Value: Integer);
begin
  if value <> fSmallTicksPerLargeTick then
  begin
    fSmallTicksPerLargeTick := Value;
    Invalidate
  end
end;

end.
