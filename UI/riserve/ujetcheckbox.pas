//{$A+,B-,D+,F-,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V-,W-,X+,Y+}
unit UJetCheckbox;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Buttons, stdctrls;

type
  TJetCheckbox = class(TGraphicControl)
  private
    FAutoSize: Boolean;
    FBitmapUnchecked: TBitmap;
    FBitmapChecked: TBitmap;
    FBitmapOverUnchecked: TBitmap;
    FBitmapOverChecked: TBitmap;
    FBitmapDisabled: TBitmap;
    FChecked : boolean;
    TempBitmap : TBitmap;
    MouseOver : Boolean;
    FTransparentColor : TColor;
    procedure AdjustBounds;
    procedure BitmapUncheckedChanged(Sender: TObject);
    procedure BitmapCheckedChanged(Sender: TObject);
    procedure BitmapOverUncheckedChanged(Sender: TObject);
    procedure BitmapOverCheckedChanged(Sender: TObject);
    procedure BitmapDisabledChanged(Sender: TObject);
    procedure SetBitmapChecked(Value: TBitmap);
    procedure SetBitmapUnchecked(Value: TBitmap);
    procedure SetBitmapOverUnchecked(Value: TBitmap);
    procedure SetBitmapOverChecked(Value: TBitmap);
    procedure SetBitmapDisabled(Value: TBitmap);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetChecked(Value : boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Invalidate; override;
    function PtInMask(const X, Y: Integer): Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetEnabled(Value : Boolean);
  protected
    FState: TCheckBoxState;
    FEnabled : boolean;
    procedure Toggle;
    function GetPalette: HPALETTE; override;
    procedure DrawButtonText(Canvas: TCanvas; const Caption: String; TextBounds: TRect; State: TCheckboxState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure Paint; override;
    procedure Loaded; override;
  published
    property Checked : Boolean read FChecked write SetChecked;
    property TransparentColor : TColor read FTransparentColor write FTransparentColor;
    property BitmapUnchecked: TBitmap read FBitmapUnchecked write SetBitmapUnchecked;
    property BitmapChecked: TBitmap read FBitmapChecked write SetBitmapChecked;
    property BitmapOverUnchecked: TBitmap read FBitmapOverUnchecked write SetBitmapOverUnchecked;
    property BitmapOverChecked: TBitmap read FBitmapOverChecked write SetBitmapOverChecked;
    property BitmapDisabled: TBitmap read FBitmapDisabled write SetBitmapDisabled;
    property Caption;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property Font;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

function MakeMask(const ColorBmp: TBitmap; TransparentColor: TColor): TBitmap;
var Temp: TRect;
    OldBkColor: TColorRef;
    TmpBitmap : Tbitmap;
begin
  Makemask := nil;
  TmpBitmap := TBitmap.Create;
  try
    TmpBitmap.Monochrome := True;
    TmpBitmap.Width := ColorBmp.Width;
    TmpBitmap.Height := ColorBmp.Height;
    OldBkColor := SetBkColor(ColorBmp.Canvas.Handle, ColorToRGB(TransparentColor));
    Temp := Rect(0, 0, ColorBmp.Width, ColorBmp.Height);
    TmpBitmap.Canvas.CopyMode := cmSrcCopy;
    TmpBitmap.Canvas.CopyRect(Temp, ColorBmp.Canvas, Temp);
    SetBkColor(ColorBmp.Canvas.Handle, OldBkColor);
    MakeMask := TmpBitmap;
  except
    TmpBitmap.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////////////
constructor TJetCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 50, 50);
  ControlStyle := [csCaptureMouse, csOpaque];
  FAutoSize := True;
  FBitmapUnchecked := TBitmap.Create;
  FBitmapUnchecked.OnChange := BitmapUncheckedChanged;
  FBitmapChecked := TBitmap.Create;
  FBitmapChecked.OnChange := BitmapCheckedChanged;
  FBitmapOverUnchecked := TBitmap.Create;
  FBitmapOverUnchecked.OnChange := BitmapOverUncheckedChanged;
  FBitmapOverChecked := TBitmap.Create;
  FBitmapOverChecked.OnChange := BitmapOverCheckedChanged;
  FBitmapDisabled := TBitmap.Create;
  FBitmapDisabled.OnChange := BitmapDisabledChanged;
  FTransparentColor := clWhite;
  TempBitmap := nil;
  ParentFont := True;
  FEnabled := true;
  FChecked := false;
  MouseOver := false;
  FState := cbUnchecked;
end;

destructor TJetCheckbox.Destroy;
begin
  FBitmapUnchecked.Free;
  FBitmapChecked.Free;
  FBitmapOverUnchecked.Free;
  FBitmapOverChecked.Free;
  FBitmapDisabled.Free;
  TempBitmap.Free;
  inherited Destroy;
end;

procedure TJetCheckbox.Paint;
var W, H: Integer;
    Composite, Mask, Overlay, CurrentBmp: TBitmap;
    R, NewR: TRect;
begin
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psSolid;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;

  if (FState in [cbGrayed]) then
  begin
     if Not FBitmapDisabled.Empty then CurrentBmp := FBitmapDisabled else
     CurrentBmp := FBitmapUnchecked;
  end else if (FState = cbUnchecked) and (not MouseOver) then CurrentBmp := FBitmapUnchecked
  else if (FState = cbUnchecked) and MouseOver then
  begin
    if Not FBitmapOverUnchecked.Empty then CurrentBmp := FBitmapOverUnchecked else CurrentBmp := FBitmapUnchecked;
  end else
  if (FState = cbChecked) and (not MouseOver) then CurrentBmp := FBitmapChecked else
  begin
    if not FBitmapOverChecked.empty then CurrentBmp := FBitmapOverChecked else CurrentBmp := FBitmapChecked;
  end;

  if not CurrentBmp.Empty then
  begin
    W := Width;
    H := Height;
    R := ClientRect;
    NewR := R;

    Composite := TBitmap.Create;
    Overlay := TBitmap.Create;

    try
      with Composite do
      begin
        Width := W;
        Height := H;
        Canvas.CopyMode := cmSrcCopy;
        Canvas.CopyRect(R, Self.Canvas, R);
      end;

      with Overlay do
      begin
        Width := W;
        Height := H;
        Canvas.CopyMode := cmSrcCopy;
        Canvas.Brush.Color := CurrentBmp.TransparentColor;
        Canvas.FillRect(R);
        Canvas.CopyRect(NewR, CurrentBmp.Canvas, R);
      end;

      Mask := MakeMask(Overlay, CurrentBmp.TransparentColor);
      try
        Composite.Canvas.CopyMode := cmSrcAnd;
        Composite.Canvas.CopyRect(R, Mask.Canvas, R);

        Overlay.Canvas.CopyMode := $00220326;
        Overlay.Canvas.CopyRect(R, Mask.Canvas, R);

        Composite.Canvas.CopyMode := cmSrcPaint;
        Composite.Canvas.CopyRect(R, Overlay.Canvas, R);

        Canvas.CopyMode := cmSrcCopy;
        Canvas.CopyRect(R, Composite.Canvas, R);

      finally
        Mask.Free;
      end;

    finally
      Composite.Free;
      Overlay.Free;
    end;

  end;

  if Length(Caption) > 0 then
  begin
    Canvas.Font := Self.Font;
    R := CLIENTRECT;
    DrawButtonText(Canvas, Caption, R, FState);
  end;

end;

function TJetCheckbox.PtInMask(const X, Y: Integer): Boolean;
begin
  Result := True;
  if TempBitmap <> nil then
    Result := (TempBitmap.Canvas.Pixels[X, Y] = clBlack);
end;

procedure TJetCheckbox.Toggle;
begin
  Case FState of
   cbUnchecked : FState := cbChecked;
   cbChecked : FState := cbUnchecked;
   cbGrayed :;
  end;
end;

procedure TJetCheckbox.MouseMove(Shift: TShiftState; X, Y: Integer);
var Last : Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  Last := MouseOver;
  MouseOver := PtInMask(X, Y);
  if Last <> MouseOver then
  begin
    if FBitmapUnchecked.Empty and Enabled then Invalidate else Repaint;
  end;
end;

procedure TJetCheckbox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var Clicked: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    Clicked := PtInMask(X, Y);
    if Clicked then
    begin
      MouseOver := true;
      Repaint;
    end;
  end;
end;

procedure TJetCheckbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoClick := PtInMask(X, Y);
  if DoClick then
  begin
    MouseOver := true;
    Toggle;
    Click;
    Repaint;
  end;
end;

procedure TJetCheckbox.Click;
begin
  inherited Click;
end;

function TJetCheckbox.GetPalette: HPALETTE;
begin
  Result := FBitmapUnchecked.Palette;
end;

procedure TJetCheckbox.SetBitmapUnchecked(Value: TBitmap);
begin
  FBitmapUnchecked.Assign(Value);
end;

procedure TJetCheckbox.SetBitmapChecked(Value: TBitmap);
begin
  FBitmapChecked.Assign(Value);
end;

procedure TJetCheckbox.SetBitmapOverUnchecked(Value: TBitmap);
begin
  FBitmapOverUnchecked.Assign(Value);
end;

procedure TJetCheckbox.SetBitmapOverChecked(Value: TBitmap);
begin
  FBitmapOverChecked.Assign(Value);
end;

procedure TJetCheckbox.SetBitmapDisabled(Value: TBitmap);
begin
  FBitmapDisabled.Assign(Value);
end;

procedure TJetCheckbox.BitmapUncheckedChanged(Sender: TObject);
Var Maskbmp : TBitmap;
    R : TRect;
begin
  AdjustBounds;
  MaskBmp := TBitmap.create;
  MaskBmp.Width := FBitmapUnchecked.width;
  MaskBmp.Height := FBitmapUnchecked.Height;
  R := Rect(0,0,FBitmapUnchecked.Width,FBitmapUnchecked.Height);
  MaskBmp.Canvas.CopyRect(R, FBitmapUnchecked.Canvas, R);
  FTransparentColor := FBitmapUnchecked.TransparentColor;
  TempBitmap.Free;
  TempBitmap := MakeMask(MaskBmp, FTransparentColor);
  MaskBmp.free;
  Invalidate;
end;

procedure TJetCheckbox.BitmapCheckedChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJetCheckbox.BitmapOverCheckedChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJetCheckbox.BitmapOverUncheckedChanged(Sender: TObject);
Var Maskbmp : TBitmap;
    R : TRect;
begin
  if FBitmapUnchecked.Empty then
  begin
    SetBounds(Left, Top, FBitmapOverUnchecked.Width, FBitmapOverUnchecked.Height);
    MaskBmp := TBitmap.create;
    MaskBmp.Width := FBitmapOverUnchecked.width;
    MaskBmp.Height := FBitmapOverUnchecked.Height;
    R := Rect(0,0,FBitmapOverUnchecked.Width,FBitmapOverUnchecked.Height);
    FTransparentColor := FBitmapOverUnchecked.TransparentColor;
    MaskBmp.Canvas.CopyRect(R, FBitmapOverUnchecked.Canvas, R);
    TempBitmap.Free;
    TempBitmap := MakeMask(MaskBmp, FTransparentColor);
    MaskBmp.free;
  end;
  Invalidate;
end;

procedure TJetCheckbox.BitmapDisabledChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJetCheckbox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TJetCheckbox.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJetCheckbox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJetCheckbox.CMSysColorChange(var Message: TMessage);
begin
  BitmapUncheckedChanged(Self);
  BitmapCheckedChanged(Self);
  BitmapOverUncheckedChanged(Self);
  BitmapOverCheckedChanged(Self);
  BitmapDisabledChanged(Self);
end;

procedure TJetCheckbox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TJetCheckbox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseOver := false;
  if FBitmapUnchecked.Empty and Enabled then Invalidate else Repaint;
end;

procedure TJetCheckbox.DrawButtonText(Canvas: TCanvas; const Caption: String;
  TextBounds: TRect; State: TCheckboxState);
var
  CString: array[0..255] of Char;
begin
  StrPCopy(CString, Caption);
  Canvas.Brush.Style := bsClear;
  if State = cbChecked then OffsetRect(TextBounds, 1, 1);
  DrawText(Canvas.Handle, CString, -1, TextBounds,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

procedure TJetCheckbox.Loaded;
begin
  inherited Loaded;
  if (FBitmapUnchecked <> nil) and (FBitmapUnchecked.Width > 0) and (FBitmapUnchecked.Height > 0) then
  begin
    BitmapUncheckedChanged(Self);
  end else if (FBitmapOverUnchecked <> nil) and (FBitmapOverUnchecked.Width > 0) and (FBitmapOverUnchecked.Height > 0) then
  begin
    BitmapOverUncheckedChanged(Self);
  end;
end;

procedure TJetCheckbox.AdjustBounds;
begin
  SetBounds(Left, Top, Width, Height);
end;

procedure TJetCheckbox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not (csReading in ComponentState) and FAutoSize and not FBitmapUnchecked.Empty then
  begin
    W := FBitmapUnchecked.Width;
    H := FBitmapUnchecked.Height;
  end;
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TJetCheckbox.Invalidate;
var R: TRect;
begin
  if (Visible or (csDesigning in ComponentState)) and
    (Parent <> nil) and Parent.HandleAllocated then
  begin
    R := BoundsRect;
    InvalidateRect(Parent.Handle, @R, True);
  end;
end;

procedure TJetCheckbox.SetEnabled(Value : Boolean);
begin
 if Value <> FEnabled then
 begin
   FEnabled := Value;
   if FEnabled = false then FState := cbGrayed else FState := cbUnchecked;
   Invalidate;
 end;
end;

procedure TJetCheckbox.SetChecked(Value : boolean);
begin
 if Value <> FChecked then
 begin
   FChecked := Value;
   if FChecked = true then FState := cbChecked else FState := cbUnchecked;
   Invalidate;
 end;
end;

end.
