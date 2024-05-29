//{$A+,B-,D+,F-,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V-,W-,X+,Y+}
unit ujetanimbutton;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Buttons, extctrls;

type
  TJetAnimButtonAnimStyle = (asRepeat,asPingPong,asPingPongRepeat);

  TJetAnimButton = class(TGraphicControl)
  private
    FTimer : TTimer;
    FStateChanged : boolean;
    FCounterUp : integer;
    FCounterOver : integer;
    FCounterDown : integer;
    FInterval : integer;
    FAnimated : boolean;
    FAutoSize: Boolean;
    FBitmapUp: TBitmap;
    FBitmapDown: TBitmap;
    FBitmapOver : TBitmap;
    FBitmapDisabled: TBitmap;
    FImageListUp : TImageList;
    FImageListOver : TImageList;
    FImageListDown : TImageList;
    TempBitmap : TBitmap;
    MouseOver : Boolean;
    FTransparentColor : TColor;
    FAnimStyle : TJetAnimButtonAnimStyle;
    AnimDir : integer;
    procedure AdjustBounds;
    procedure BitmapUpChanged(Sender: TObject);
    procedure BitmapDownChanged(Sender: TObject);
    procedure BitmapOverChanged(Sender: TObject);
    procedure BitmapDisabledChanged(Sender: TObject);
    procedure SetBitmapDown(Value: TBitmap);
    procedure SetBitmapUp(Value: TBitmap);
    procedure SetBitmapOver(Value: TBitmap);
    procedure SetBitmapDisabled(Value: TBitmap);
    procedure SetImageListUp(Value : TImageList);
    procedure SetImageListOver(Value : TImageList);
    procedure SetImageListDown(Value : TImageList);
    procedure SetInterval(Value : integer);
    procedure SetAnimated(Value : Boolean);
    procedure SetAnimStyle(Value : TJetAnimButtonAnimStyle);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure TimerEvent(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Invalidate; override;
    function PtInMask(const X, Y: Integer): Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  protected
    FState: TButtonState;
    FEnabled : boolean;
    function GetPalette: HPALETTE; override;
    procedure SetEnabled(Value : Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: String; TextBounds: TRect; State: TButtonState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure Paint; override;
    procedure Loaded; override;
  published
    property Animated: boolean read FAnimated  write SetAnimated;
    property TransparentColor : TColor read FTransparentColor write FTransparentColor;
//    property BitmapUp: TBitmap read FBitmapUp write SetBitmapUp;
    property BitmapDisabled: TBitmap read FBitmapDisabled write SetBitmapDisabled;
    property ImageListUp: TImageList read FImageListUp write SetImageListUp;
    property ImageListDown: TImageList read FImageListDown write SetImageListDown;
    property ImageListOver: TImageList read FImageListOver write SetImageListOver;
    property Interval : integer read FInterval write SetInterval;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property AnimStyle : TJetAnimButtonAnimStyle read FAnimStyle write SetAnimStyle;
    property Caption;
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
constructor TJetAnimButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 50, 50);
  ControlStyle := [csCaptureMouse, csOpaque];
  FAnimated := False;
  FInterval := 100;
  FCounterUp := 0;
  FCounterOver := 0;
  FCounterDown := 0;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := FAnimated;
  FTimer.OnTimer := TimerEvent;
  FTimer.Interval := FInterval;
  FImageListUp := nil;
  FImageListDown := nil;
  FImageListOver := nil;
  FAutoSize := True;
  FBitmapUp := TBitmap.Create;
  FBitmapUp.OnChange := BitmapUpChanged;
  FBitmapDown := TBitmap.Create;
  FBitmapDown.OnChange := BitmapDownChanged;
  FBitmapOver := TBitmap.Create;
  FBitmapOver.OnChange := BitmapOverChanged;
  FBitmapDisabled := TBitmap.Create;
  FBitmapDisabled.OnChange := BitmapDisabledChanged;
  FTransparentColor := clWhite;
  TempBitmap := nil;
  ParentFont := True;
  FEnabled := true;
  MouseOver := false;
  FState := bsUp;
  FAnimStyle := asRepeat;
  FStateChanged := false;
end;

destructor TJetAnimButton.Destroy;
begin
  FBitmapUp.Free;
  FBitmapOver.Free;
  FBitmapDown.Free;
  FBitmapDisabled.Free;
  TempBitmap.Free;
  FTimer.free;
  inherited Destroy;
end;

procedure TJetAnimButton.SetAnimStyle(Value : TJetAnimButtonAnimStyle);
begin
  if Value <> FAnimStyle then
  begin
     FAnimStyle := Value;
     FStateChanged := true;
  end;
end;

procedure TJetAnimButton.SetBitmapUp(Value: TBitmap);
begin
  FBitmapUp.Assign(Value);
end;

procedure TJetAnimButton.SetBitmapDown(Value: TBitmap);
begin
  FBitmapDown.Assign(Value);
end;

procedure TJetAnimButton.SetBitmapOver(Value: TBitmap);
begin
  FBitmapOver.Assign(Value);
end;

procedure TJetAnimButton.SetBitmapDisabled(Value: TBitmap);
begin
  FBitmapDisabled.Assign(Value);
end;

procedure TJetAnimButton.BitmapUpChanged(Sender: TObject);
Var Maskbmp : TBitmap;
    R : TRect;
begin
  AdjustBounds;
  MaskBmp := TBitmap.create;
  MaskBmp.Width := FBitmapUp.width;
  MaskBmp.Height := FBitmapUp.Height;
  R := Rect(0,0,FBitmapUp.Width,FBitmapUp.Height);
  MaskBmp.Canvas.CopyRect(R, FBitmapUp.Canvas, R);
  FTransparentColor := FBitmapUp.TransparentColor;
  TempBitmap.Free;
  TempBitmap := MakeMask(MaskBmp, FTransparentColor);
  MaskBmp.free;
  Invalidate;
end;

procedure TJetAnimButton.BitmapDownChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJetAnimButton.BitmapOverChanged(Sender: TObject);
Var Maskbmp : TBitmap;
    R : TRect;
begin
  if FBitmapUp.Empty then
  begin
    SetBounds(Left, Top, FBitmapOver.Width, FBitmapOver.Height);
    MaskBmp := TBitmap.create;
    MaskBmp.Width := FBitmapOver.width;
    MaskBmp.Height := FBitmapOver.Height;
    R := Rect(0,0,FBitmapOver.Width,FBitmapOver.Height);
    FTransparentColor := FBitmapOver.TransparentColor;
    MaskBmp.Canvas.CopyRect(R, FBitmapOver.Canvas, R);
    TempBitmap.Free;
    TempBitmap := MakeMask(MaskBmp, FTransparentColor);
    MaskBmp.free;
  end;
  Invalidate;
end;

procedure TJetAnimButton.BitmapDisabledChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJetAnimButton.SetImageListUp(Value : TImageList);
begin
 FImageListUp := Value;
 FstateChanged := true;
 if (csDesigning in componentstate) then
 begin
    if (FImageListUp <> nil) then FImageListUp.getbitmap(0,FBitmapUp)
    else begin
        FBitmapUp.free;
        FBitmapUp := TBitmap.Create;
        FBitmapUp.OnChange := BitmapUpChanged;
    end;
    Invalidate;
 end;
end;

procedure TJetAnimButton.SetImageListOver(Value : TImageList);
begin
 FImageListOver := Value;
 FstateChanged := true;
end;

procedure TJetAnimButton.SetImageListDown(Value : TImageList);
begin
 FImageListDown := Value;
 FstateChanged := true;
end;

procedure TJetAnimButton.SetAnimated(Value: Boolean);
begin
  if FAnimated <> Value then
  begin
    FAnimated := Value;
    FstateChanged := true;
    FTimer.Enabled := FAnimated;
  end;
end;

procedure TJetAnimButton.SetInterval(Value: integer);
begin
  if FInterval <> Value then begin
    FInterval := Value;
    if Animated then begin
      FTimer.Enabled := False;
      FTimer.Interval := Value;
      FTimer.Enabled := True;
    end
    else begin
      FTimer.Interval := Value;
    end;
  end;
end;

procedure TJetAnimButton.TimerEvent(Sender: TObject);
begin
 if FEnabled and MouseOver and not ((csDesigning in ComponentState) or (FState = bsDown)) then
 begin
   if FStateChanged then
   begin
     FCounterOver := 0;
     AnimDir := 1;
     FStateChanged := false;
   end;
   if FCounterOver < 0 then FCounterOver := 0;
   if (FImageListOver <> nil) then FImageListOver.GetBitmap(FCounterOver,FBitmapOver);
   Repaint;
   FCounterOver := FCounterOver + AnimDir;
   if (FImageListOver <> nil) and ((FCounterOver >= FImageListOver.count-1) or (FCounterOver <= 0)) then
   begin
     case FAnimStyle of
       asPingPong : AnimDir := AnimDir * -1;
     end;
   end;
   if (FImageListOver <> nil) and ((FCounterOver >= FImageListOver.count) or (FCounterOver < 0)) then
   begin
     case FAnimStyle of
       asPingPongRepeat  : AnimDir := AnimDir * -1;
               asRepeat  : begin
                            AnimDir := 1;
                            FCounterOver := 0;
                           end;
     end;
   end;
 end else
 if FEnabled and (FState = bsDown) and not (csDesigning in ComponentState) then
 begin
   if FStateChanged then
   begin
     FCounterDown := 0;
     AnimDir := 1;
     FStateChanged := false;
   end;
   if FCounterDown < 0 then FCounterDown := 0;
   if (FImageListDown <> nil) then FImageListDown.GetBitmap(FCounterDown,FBitmapDown);
   Repaint;
   FCounterDown := FCounterDown + AnimDir;
   if (FImageListDown <> nil) and ((FCounterDown >= FImageListDown.count-1) or (FCounterDown <= 0)) then
   begin
     case FAnimStyle of
       asPingPong : AnimDir := AnimDir * -1;
     end;
   end;
   if (FImageListDown <> nil) and ((FCounterDown >= FImageListDown.count) or (FCounterDown < 0)) then
   begin
     case FAnimStyle of
       asPingPongRepeat  : AnimDir := AnimDir * -1;
               asRepeat  : begin
                            AnimDir := 1;
                            FCounterDown := 0;
                           end;
     end;
   end;
 end else
 if FEnabled and (FState = bsUp) and not (csDesigning in ComponentState) then
 begin
   if FStateChanged then
   begin
     FCounterUp := 0;
     AnimDir := 1;
     FStateChanged := false;
   end;
   if FCounterUp < 0 then FCounterUp := 0;
   if (FImageListUp <> nil) then FImageListUp.GetBitmap(FCounterUp,FBitmapUp);
   Repaint;
   FCounterUp := FCounterUp + AnimDir;
   if (FImageListUp <> nil) and ((FCounterUp >= FImageListUp.count-1) or (FCounterUp <= 0)) then
   begin
     case FAnimStyle of
       asPingPong : AnimDir := AnimDir * -1;
     end;
   end;
   if (FImageListUp <> nil) and ((FCounterUp >= FImageListUp.count) or (FCounterUp < 0)) then
   begin
     case FAnimStyle of
       asPingPongRepeat  : AnimDir := AnimDir * -1;
               asRepeat  : begin
                            AnimDir := 1;
                            FCounterUp := 0;
                           end;
     end;
   end;
 end;
end;

procedure TJetAnimButton.Paint;
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

  if (FState in [bsDisabled, bsExclusive]) then
  begin
     if Not FBitmapDisabled.Empty then CurrentBmp := FBitmapDisabled else CurrentBmp := FBitmapUp;
  end else
  if (FState = bsUp) and (not MouseOver) then CurrentBmp := FBitmapUp
  else
  if (FState = bsUp) and MouseOver then
  begin
    if Not FBitmapOver.Empty then CurrentBmp := FBitmapOver else CurrentBmp := FBitmapUp;
  end else
  begin
    if Not FBitmapDown.Empty then CurrentBmp := FBitmapDown else CurrentBmp := FBitmapUp;
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

function TJetAnimButton.PtInMask(const X, Y: Integer): Boolean;
begin
  Result := True;
  if TempBitmap <> nil then
    Result := (TempBitmap.Canvas.Pixels[X, Y] = clBlack);
end;

procedure TJetAnimButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var Last : Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  Last := MouseOver;
  MouseOver := PtInMask(X, Y);
  if (Last <> MouseOver) and (FState <> bsDown) then
  begin
    FStateChanged := True;
    if (not mouseover) and animated then //skip redraw on mouse over an animated
    begin
       if FBitmapUp.Empty and Enabled then Invalidate;
    end else
    if (not mouseover) and (not animated) then
    begin
       if FBitmapUp.Empty and Enabled then Invalidate else repaint;
    end else
    if mouseover and (not animated) then repaint;
  end;
end;

procedure TJetAnimButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var Clicked: Boolean;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    Clicked := PtInMask(X, Y);
    if Clicked then
    begin
      FStateChanged := true;
      FState := bsDown;
      if FBitmapDown.Empty and Enabled then Invalidate else
      if (not FBitmapDown.Empty ) and (not Animated) and Enabled then Repaint;
    end;
  end;
end;

procedure TJetAnimButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoClick := PtInMask(X, Y);
  if (FState = bsDown) then
    begin
      FStateChanged := true;
      FState := bsUp;
      if FBitmapUp.Empty and Enabled then Invalidate else
      if (not FBitmapUp.Empty ) and (not Animated) and Enabled then Repaint else
      if Doclick and (not Animated) then repaint;
    end;
    if DoClick then
    begin
      MouseOver := true;
      Click;
    end;
end;

procedure TJetAnimButton.Click;
begin
  inherited Click;
end;

function TJetAnimButton.GetPalette: HPALETTE;
begin
  Result := FBitmapUp.Palette;
end;

procedure TJetAnimButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TJetAnimButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJetAnimButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJetAnimButton.CMSysColorChange(var Message: TMessage);
begin
  BitmapDisabledChanged(Self);
end;

procedure TJetAnimButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TJetAnimButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseOver := false;
  if (Fstate=bsDown) then FStateChanged := false else FStateChanged := True;
  if FBitmapUp.Empty and (not(Fstate=bsDown)) and Enabled then Invalidate else
  if (not FBitmapUp.Empty ) and (not Animated) and Enabled then Repaint;
end;

procedure TJetAnimButton.DrawButtonText(Canvas: TCanvas; const Caption: String;
  TextBounds: TRect; State: TButtonState);
var
  CString: array[0..255] of Char;
begin
  StrPCopy(CString, Caption);
  Canvas.Brush.Style := bsClear;
  if State = bsDown then OffsetRect(TextBounds, 1, 1);
  DrawText(Canvas.Handle, CString, -1, TextBounds,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

procedure TJetAnimButton.Loaded;
begin
  inherited Loaded;
  if FImageListUp <> nil then FImageListUp.GetBitmap(0,FBitmapUp);
  if (FBitmapUp <> nil) and (FBitmapUp.Width > 0) and (FBitmapUp.Height > 0) then
  begin
    BitmapUpChanged(Self);
    if FImageListDown <> nil then FImageListDown.GetBitmap(0,FBitmapDown);
    if FImageListOver <> nil then FImageListOver.GetBitmap(0,FBitmapOver);
  end else
  begin
    if FImageListDown <> nil then FImageListDown.GetBitmap(0,FBitmapDown);
    if FImageListOver <> nil then FImageListOver.GetBitmap(0,FBitmapOver);
     if (FBitmapOver <> nil) and (FBitmapOver.Width > 0) and (FBitmapOver.Height > 0) then
     begin
       BitmapOverChanged(Self);
     end;
  end;
end;

procedure TJetAnimButton.AdjustBounds;
begin
  SetBounds(Left, Top, Width, Height);
end;

procedure TJetAnimButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not (csReading in ComponentState) and FAutoSize and not FBitmapUp.Empty then
  begin
    W := FBitmapUp.Width;
    H := FBitmapUp.Height;
  end;
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TJetAnimButton.Invalidate;
var R: TRect;
begin
  if (Visible or (csDesigning in ComponentState)) and
    (Parent <> nil) and Parent.HandleAllocated then
  begin
    R := BoundsRect;
    InvalidateRect(Parent.Handle, @R, True);
  end;
end;

procedure TJetAnimButton.SetEnabled(Value : Boolean);
begin
 if Value <> FEnabled then
 begin
   FEnabled := Value;
   if FEnabled = false then FState := bsDisabled else FState := bsUp;
   Invalidate;
 end;
end;


end.
