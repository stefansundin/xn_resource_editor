unit ComponentRuler;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TRulerOrientation = (ruHorizontal, ruVertical);

  TRuler = class(TCustomControl)
  private
    FSmallTickSpacing: Integer;
    FSmallTickLength: Integer;
    FSmallTicksPerLargeTick: Integer;
    FLargeTickLength: Integer;
    FDialogBox: HWND;
    procedure SetLargeTickLength(const Value: Integer);
    procedure SetOrientation(const Value: TRulerOrientation);
    procedure SetSmallTickLength(const Value: Integer);
    procedure SetSmallTickSpacing(const Value: Integer);
    procedure SetSmallTicksperLargeTick(const Value: Integer);
    function GetOrientation: TRulerOrientation;
    procedure SetDialogBox(const Value: HWND);
  protected
    procedure Loaded; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property DialogBox: HWND read FDialogBox write SetDialogBox;
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

    property SmallTickSpacing: Integer read FSmallTickSpacing write SetSmallTickSpacing default 10;
    property SmallTicksPerLargeTick: Integer read FSmallTicksPerLargeTick write SetSmallTicksperLargeTick default 5;
    property SmallTickLength: Integer read FSmallTickLength write SetSmallTickLength default 5;
    property LargeTickLength: Integer read FLargeTickLength write SetLargeTickLength default 10;
    property Orientation: TRulerOrientation read GetOrientation write SetOrientation stored False;
  end;

implementation

{ TRuler }

constructor TRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 180;
  Height := 40;
  BevelKind := bkTile;
  BevelInner := bvLowered;
  BevelOuter := bvLowered;
  FLargeTickLength := 10;
  FSmallTickLength := 5;
  FSmallTicksPerLargeTick := 5;
  FSmallTickSpacing := 10;
end;

function TRuler.GetOrientation: TRulerOrientation;
begin
  if Width > Height then
    Result := ruHorizontal
  else
    Result := ruVertical
end;

procedure TRuler.Loaded;
begin
  inherited;
end;

procedure TRuler.Paint;
var
  x, y: Integer;
  w, h: Integer;
  t: Integer;
  sm: Integer;
  r: TRect;
  Offset: Integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.Font := Font;

  w := ClientWidth;
  h := ClientHeight;

  if FDialogBox <> 0 then
    sm := FSmallTickSpacing
  else
    sm := FSmallTickSpacing;

  y := 0;
  x := 0;
  Offset := 0;
  t := 0;
  
  if Orientation = ruHorizontal then
  begin
    repeat
      Inc(Offset, sm);
      if FDialogBox <> 0 then
      begin
        r := Rect(0, 0, Offset, 10);
        MapDialogRect(FDialogBox, r);
        x := r.Right;
      end
      else
        x := Offset;
      Inc(t);
      if x < w then
      begin
        Canvas.MoveTo(x, y);
        if t = FSmallTicksPerLargeTick then
        begin
          Canvas.LineTo(x, y + FLargeTickLength);
          t := 0;
        end
        else
          Canvas.LineTo(x, y + FSmallTickLength);
      end
    until x >= w;
  end
  else
  begin
    repeat
      Inc(Offset, sm);
      if FDialogBox <> 0 then
      begin
        r := Rect(0, 0, 10, Offset);
        MapDialogRect(FDialogBox, r);
        y := r.Bottom;
      end
      else
        y := Offset;

      Inc(t);
      if y < h then
      begin
        Canvas.MoveTo(x, y);
        if t = FSmallTicksPerLargeTick then
        begin
          Canvas.LineTo(x + FLargeTickLength, y);
          t := 0
        end
        else
          Canvas.LineTo(x + FSmallTickLength, y);
      end
    until y >= h;
  end
end;

procedure TRuler.SetDialogBox(const Value: HWND);
begin
  FDialogBox := Value;
  Invalidate;
end;

procedure TRuler.SetLargeTickLength(const Value: Integer);
begin
  if Value <> FLargeTickLength then
  begin
    FLargeTickLength := Value;
    Invalidate;
  end
end;

procedure TRuler.SetOrientation(const Value: TRulerOrientation);
var
  h: Integer;
begin
  if Value <> Orientation then
  begin
    h := Height;
    Height := Width;
    Width := h;
    Invalidate;
  end
end;

procedure TRuler.SetSmallTickLength(const Value: Integer);
begin
  if Value <> FSmallTickLength then
  begin
    FSmallTickLength := Value;
    Invalidate;
  end
end;

procedure TRuler.SetSmallTickSpacing(const Value: Integer);
begin
  if Value <> FSmallTickSpacing then
  begin
    FSmallTickSpacing := Value;
    Invalidate;
  end
end;

procedure TRuler.SetSmallTicksperLargeTick(const Value: Integer);
begin
  if Value <> FSmallTicksPerLargeTick then
  begin
    FSmallTicksPerLargeTick := Value;
    Invalidate;
  end
end;

end.
