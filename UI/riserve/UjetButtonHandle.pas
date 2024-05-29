// (C) 1999 Jimmy Theo Weku.
// 2003 Massimo Magnano
//         Fixed some little bugs
//         Added Blinking Effect
//         Added GroupIndex, AllowAllUp, AllowMoreDown
// 2003 Beppe Grimaldi (grimalkin@ciaoweb.it)
//         Added routines (move, down, up) and classes (TMouseWinControl)
//            for capturing mouse events in & out of TForm.

//{$A+,B-,D+,F-,G+,I+,K+,L+,N+,P+,Q-,R-,S+,T-,V-,W-,X+,Y+}
unit UjetButton;

interface

{$define RTDebug}

uses
  {$ifdef RTDebug}
    RTDebug,
  {$endif}
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Buttons, ExtCtrls;

type
  TBlinkEvent = procedure (Sender :TObject; isBlink :Boolean) of object;

  TMouseWinControl = class(TWinControl)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
  end;

  TJetButton = class(TGraphicControl)
  private
    FAutoSize         : Boolean;
    FBitmapUp         : TBitmap;
    FBitmapDown       : TBitmap;
    FBitmapOver       : TBitmap;
    FBitmapDisabled   : TBitmap;
    rBitmapBlink      : TBitmap;
    TempBitmap        : TBitmap;
    MouseOver         : Boolean;
    FTransparentColor : TColor;
    rAllowAllUp,
    rAllowMoreDown    :Boolean;
    rOnBlink          :TBlinkEvent;
    rHandle           :HWnd;

    procedure AdjustBounds;
    procedure BitmapUpChanged(Sender: TObject);
    procedure BitmapDownChanged(Sender: TObject);
    procedure BitmapOverChanged(Sender: TObject);
    procedure BitmapDisabledChanged(Sender: TObject);
    procedure BitmapBlinkChanged(Sender: TObject);
    procedure SetBitmapDown(Value: TBitmap);
    procedure SetBitmapUp(Value: TBitmap);
    procedure SetBitmapOver(Value: TBitmap);
    procedure SetBitmapDisabled(Value: TBitmap);
    procedure SetBitmapBlink(Value: TBitmap);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function PtInMask(const X, Y: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  protected
    FState,
    FMouseState  :TButtonState;   
    FEnabled,
    rGrouped,
    isBlink,
    rBlinkRemain :Boolean;
    rGroupIndex  :Integer;
    BlinkTimer   :TTimer;
    rBlinkStopAfter,
    BlinkCounter :DWord;

    function GetPalette: HPALETTE; override;
    procedure SetEnabled(Value : Boolean); override;
    procedure DrawButtonText(Canvas: TCanvas; const Caption: String; TextBounds: TRect; State: TButtonState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure Click; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure SetState(Value: TButtonState); virtual;
    procedure SetStateNoGroup(Value: TButtonState); virtual;
    procedure SetGroupIndex(Value: Integer);
    function GetAllowAllUp :Boolean;
    procedure SetAllowAllUp(Value :Boolean);
    function GetAllowMoreDown :Boolean;
    procedure SetAllowMoreDown(Value :Boolean);
    procedure SetBlinking(Value: Boolean); virtual;
    function GetBlinking: Boolean; virtual;
    procedure SetBlinkInterval(Value: DWord); virtual;
    function GetBlinkInterval: DWord; virtual;
    procedure BlinkEvent(Sender: TObject); virtual;
  published
    property TransparentColor : TColor read FTransparentColor write FTransparentColor;
    property BitmapUp: TBitmap read FBitmapUp write SetBitmapUp;
    property BitmapDown: TBitmap read FBitmapDown write SetBitmapDown;
    property BitmapOver: TBitmap read FBitmapOver write SetBitmapOver;
    property BitmapDisabled: TBitmap read FBitmapDisabled write SetBitmapDisabled;
    property BitmapBlink: TBitmap read rBitmapBlink write SetBitmapBlink;
    property Blinking :Boolean read GetBlinking write SetBlinking;
    property BlinkInterval :DWord read GetBlinkInterval write SetBlinkInterval;
    property BlinkStopAfter :DWord read rBlinkStopAfter write rBlinkStopAfter;
    property BlinkRemain :Boolean read rBlinkRemain write rBlinkRemain;
    property State :TButtonState read FState write SetState default bsUp;
    property GroupIndex :Integer read rGroupIndex write SetGroupIndex;
    property AllowAllUp     :Boolean read GetAllowAllUp write SetAllowAllUp;
    property AllowMoreDown  :Boolean read GetAllowMoreDown write SetAllowMoreDown;
    property Caption;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property Font;
    property ShowHint;
    property Visible;
    property PopupMenu;
    //property MouseControlWindow :TMouseWinControl read MouseWindow;

    //Eventi....
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnContextPopup;
    property OnBlink :TBlinkEvent read rOnBlink write rOnBlink;
  end;

  TButtonsListCollectionItem =class (TCollectionItem)
  public
     Button :TJetButton;
  end;

  TButtonsList =class (TCollection)
  public
    AllowAllUp,
    AllowMoreDown  :Boolean;

    function Down(Button: TJetButton): Boolean;
    function Up(Button:TJetButton):Boolean;
    procedure AddButton(Button: TJetButton);
    function FindButton(Button: TJetButton): TButtonsListCollectionItem;
    procedure RemoveButton(Button: TJetButton);
  end;

  TGroupsListCollectionItem =class (TCollectionItem)
  public
     ButtonList :TButtonsList;
     GroupIndex :Integer;

     constructor Create(Collection: TCollection); override;
     destructor Destroy; override;
  end;

  TGroupsList =class (TCollection)
  public
    function GetGroup(GroupIndex :Integer) :TButtonsList;
    procedure RemoveGroup(GroupIndex :Integer);
    function FindGroup(GroupIndex: Integer): TGroupsListCollectionItem;
  end;


Var
   GroupsList :TGroupsList =Nil;

implementation

uses ObjectInstance;

Type
    TAccessWinControl=class(TWinControl)
    end;


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
constructor TJetButton.Create(AOwner: TComponent);
var
  tempOwner :TComponent;
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 50, 50);
  ControlStyle := [csCaptureMouse, csOpaque];
  FAutoSize := True;
  FBitmapUp := TBitmap.Create;
  FBitmapUp.OnChange := BitmapUpChanged;
  FBitmapDown := TBitmap.Create;
  FBitmapDown.OnChange := BitmapDownChanged;
  FBitmapOver := TBitmap.Create;
  FBitmapOver.OnChange := BitmapOverChanged;
  FBitmapDisabled := TBitmap.Create;
  FBitmapDisabled.OnChange := BitmapDisabledChanged;
  rBitmapBlink := TBitmap.Create;
  rBitmapBlink.OnChange := BitmapBlinkChanged;
  FTransparentColor := clWhite;
  TempBitmap := nil;
  ParentFont := True;
  FEnabled := true;
  MouseOver := false;
  rGroupIndex :=0; //No Group
  FState := bsUp;
  BlinkTimer :=TTimer.Create(Self);
  BlinkTimer.Interval :=1500;
  BlinkTimer.OnTimer :=BlinkEvent;
  BlinkTimer.Enabled :=False;
  rOnBlink :=Nil;
  isBlink :=False;
  BlinkCounter :=1;
  rBlinkStopAfter :=6;
  rBlinkRemain :=False;

  ObjectInstance.AllocateHWnd(Self.WndProc, Self.ClassName, Self.Name, 0, 0, 50, 50);

  tempOwner := TComponent(Owner);
  while not(tempOwner is TWinControl) do
    tempOwner := tempOwner.Owner;

  if (tempOwner<>Nil)
  then Windows.SetParent(rHandle, TWinControl(tempOwner).Handle);
end;

destructor TJetButton.Destroy;
begin
  BlinkTimer.Free;
  FBitmapUp.Free;
  FBitmapDown.Free;
  FBitmapOver.Free;
  FBitmapDisabled.Free;
  TempBitmap.Free;
  if (rGroupIndex<>0) then GroupsList.GetGroup(rGroupIndex).RemoveButton(Self);
  ObjectInstance.DeallocateHWnd(rHandle);
  inherited Destroy;
end;


procedure TJetButton.Paint;
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

  if (isBlink)
  then begin
            if not(rBitmapBlink.Empty)
            then CurrentBmp := rBitmapBlink
            else CurrentBmp := FBitmapUp;
       end
  else if (FState in [bsDisabled, bsExclusive])
       then begin
              if not(FBitmapDisabled.Empty)
              then CurrentBmp := FBitmapDisabled
              else CurrentBmp := FBitmapUp;
             end
       else if (FState = bsUp) and (not MouseOver)
            then CurrentBmp := FBitmapUp
            else if (FState = bsUp) and MouseOver
                 then begin
                           if not(FBitmapOver.Empty)
                           then CurrentBmp := FBitmapOver
                           else CurrentBmp := FBitmapUp;
                       end
                 else begin
                           if not(FBitmapDown.Empty)
                           then CurrentBmp := FBitmapDown
                           else CurrentBmp := FBitmapUp;
                       end;

  if not(CurrentBmp.Empty) then
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

function TJetButton.PtInMask(const X, Y: Integer): Boolean;
begin
  Result := True;
  if TempBitmap <> nil then
    Result := (TempBitmap.Canvas.Pixels[X, Y] = clBlack);
end;

procedure TJetButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
   Last : Boolean;
   Pt :TPoint;

begin
  Last := MouseOver;
  MouseOver := PtInMask(X, Y);

  if MouseOver
  then inherited MouseMove(Shift, X, Y);

  if (Last <> MouseOver)
    then begin
            if (Fstate = bsDown) and not (ssLeft in Shift)
              then begin
                      ReleaseCapture();
                      {$ifdef RTDebug}
                        RTAssert(0, true, 'MOVE REL case1 ' + Self.Name, '', 0);
                      {$endif}
                   end
              else begin
                      if MouseOver
                        then begin
                                SetCapture(rHandle);
                                {$ifdef RTDebug}
                                  RTAssert(0, true, 'MOVE SET case2 ' + Self.Name, '', 0);
                                {$endif}
                             end
                        else begin
                                ReleaseCapture();
                                {$ifdef RTDebug}
                                  RTAssert(0, true, 'MOVE REL case2 ' + Self.Name, '', 0);
                                {$endif}
                             end;
                      if (FBitmapUp.Empty and Enabled)
                        then Invalidate
                        else Repaint;
                   end;
         end
    else begin
            if MouseOver
            then begin
                      if (GetCapture <> rHandle)
                      then begin
                                SetCapture(rHandle);
                                {$ifdef RTDebug}
                                    RTAssert(0, true, 'MOVE SET case3 ' + Self.Name, '', 0);
                                {$endif}
                           end;
                 end
            else begin
                      if (GetCapture = rHandle)
                      then begin
                                ReleaseCapture;
                                {$ifdef RTDebug}
                                   RTAssert(0, true, 'MOVE REL case3 ' + Self.Name, '', 0);
                                {$endif}
                           end;
                 end;
         end;

    if not(MouseOver)
    then begin
            if (GetCapture = rHandle) then
            begin
                 ReleaseCapture;
                 {$ifdef RTDebug}
                   RTAssert(0, true, 'MOVE REL case not(Over) ' + Self.Name, '', 0);
                 {$endif}
            end;

            Pt.X :=X;
            Pt.Y :=Y;
            Pt :=Self.Parent.ScreenToClient(Self.ClientToScreen(Pt));
            TAccessWinControl(Parent).MouseMove(Shift, Pt.X, Pt.Y);
                 {$ifdef RTDebug}
                   RTAssert(0, true, 'Parent MouseMove ' + Self.Name, '', 0);
                 {$endif}
       end;
end;

procedure TJetButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

Var
   Pt :TPoint;

begin
  MouseOver :=PtInMask(X, Y);
  if MouseOver
  then begin
            inherited MouseDown(Button, Shift, X, Y);

            if (GetCapture <> rHandle) then
            begin
                 SetCapture(rHandle);
                 {$ifdef RTDebug}
                    RTAssert(0, true, 'DOWN SET ' + Self.Name, '', 0);
                 {$endif}
            end;

            if Not(Enabled) then Exit;

            if (rGroupIndex=0) and (Button=mbLeft) then
            begin
                 FState := bsDown;
                 Repaint;
            end;
       end
  else begin
            if (GetCapture = rHandle) then
            begin
                 ReleaseCapture;
                 {$ifdef RTDebug}
                   RTAssert(0, true, 'DOWN REL ' + Self.Name, '', 0);
                 {$endif}
            end;

            Pt.X :=X;
            Pt.Y :=Y;
            Pt :=Self.Parent.ScreenToClient(Self.ClientToScreen(Pt));
            TAccessWinControl(Parent).MouseDown(Button, Shift, Pt.X, Pt.Y);
                 {$ifdef RTDebug}
                   RTAssert(0, true, 'Parent MouseDown ' + Self.Name, '', 0);
                 {$endif}
       end;
end;

procedure TJetButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

Var
   Pt :TPoint;

begin
  MouseOver :=PtInMask(X, Y);
  if MouseOver then
  begin
      inherited MouseUp(Button, Shift, X, Y);

      if (GetCapture <> rHandle) then
        begin
           SetCapture(rHandle);
           {$ifdef RTDebug}
             RTAssert(0, true, 'UP SET ' + Self.Name, '', 0);
           {$endif}
        end;

      if Not(Enabled) then Exit;

      if (Button=mbLeft)
      then begin
              SetBlinking(False);
              if (rGroupIndex<>0)
              then begin
                      if (FState=bsUp)
                      then begin
                                if not(GroupsList.GetGroup(rGroupIndex).Down(Self))
                                then Exit;
                            end
                      else begin
                                if not(GroupsList.GetGroup(rGroupIndex).Up(Self))
                                then Exit;
                            end;
                   end
              else begin
                      if (FState = bsDown) then
                      begin
                         FState := bsUp;
                         Repaint;
                       end;
                   end;
              if FBitmapUp.Empty
                 then Invalidate
                 else Repaint; //provare una riga sopra.

              MouseOver := True;
              Click;  // genera l'evento 'OnClick'
           end;
   end
  else begin
            if (GetCapture = rHandle) then
            begin
                 ReleaseCapture;
                 {$ifdef RTDebug}
                   RTAssert(0, true, 'UP REL ' + Self.Name, '', 0);
                 {$endif}
            end;

            Pt.X :=X;
            Pt.Y :=Y;
            Pt :=Self.Parent.ScreenToClient(Self.ClientToScreen(Pt));
            TAccessWinControl(Parent).MouseUp(Button, Shift, Pt.X, Pt.Y);
                 {$ifdef RTDebug}
                   RTAssert(0, true, 'Parent MouseUp ' + Self.Name, '', 0);
                 {$endif}
       end;
end;

procedure TJetButton.Click;
begin
  inherited Click;
end;

procedure TJetButton.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
Var
   Pt :TPoint;

begin
     if MouseOver
     then inherited DoContextPopup(MousePos, Handled)
     else begin
               Pt :=Self.Parent.ScreenToClient(Self.ClientToScreen(MousePos));
               TAccessWinControl(Self.Parent).DoContextPopup(Pt, Handled);
          end;
end;

function TJetButton.GetPalette: HPALETTE;
begin
  Result := FBitmapUp.Palette;
end;

procedure TJetButton.SetBitmapUp(Value: TBitmap);
begin
  FBitmapUp.Assign(Value);
end;

procedure TJetButton.SetBitmapDown(Value: TBitmap);
begin
  FBitmapDown.Assign(Value);
end;

procedure TJetButton.SetBitmapOver(Value: TBitmap);
begin
  FBitmapOver.Assign(Value);
end;

procedure TJetButton.SetBitmapDisabled(Value: TBitmap);
begin
  FBitmapDisabled.Assign(Value);
end;

procedure TJetButton.SetBitmapBlink(Value: TBitmap);
begin
  rBitmapBlink.Assign(Value);
end;

procedure TJetButton.BitmapUpChanged(Sender: TObject);
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

procedure TJetButton.BitmapDownChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJetButton.BitmapOverChanged(Sender: TObject);
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

procedure TJetButton.BitmapDisabledChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJetButton.BitmapBlinkChanged(Sender: TObject);
begin
  Invalidate;
end;


procedure TJetButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;

procedure TJetButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJetButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJetButton.CMSysColorChange(var Message: TMessage);
begin
  BitmapUpChanged(Self);
  BitmapDownChanged(Self);
  BitmapOverChanged(Self);
  BitmapDisabledChanged(Self);
end;

procedure TJetButton.CMMouseEnter(var Message: TMessage);
var
  NeedRepaint : Boolean;
  Pt          : TPoint;

begin
  GetCursorPos(Pt);
  Pt :=ScreenToClient(Pt);
  MouseOver := PtInMask(Pt.X, Pt.Y);

  if MouseOver
  then inherited;

  NeedRepaint := not MouseOver and Enabled and (DragMode <> dmAutomatic) and (GetCapture = 0);

  { Windows XP introduced hot states also for non-flat buttons. }
  if (NeedRepaint) and not (csDesigning in ComponentState) then
  begin
    //MouseOver := True;
    if Enabled then
      Repaint;
  end;

  if MouseOver
  then SetCapture(rHandle)
  else ReleaseCapture;

  {$ifdef RTDebug}
    RTAssert(0, MouseOver, 'CMMouseENTER SET ' + Self.Name, 'CMMouseENTER REL ' + Self.Name, 0);
  {$endif}
end;

procedure TJetButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseOver := False;
  if (Fstate = bsDown) then exit;
  if FBitmapUp.Empty and Enabled then Invalidate else Repaint;

  ReleaseCapture;
  {$ifdef RTDebug}
    RTAssert(0, MouseOver, 'CMMouseLeave REL ?Over' + Self.Name, 'CMMouseLeave REL ' + Self.Name, 0);
  {$endif}
end;

procedure TJetButton.DrawButtonText(Canvas: TCanvas; const Caption: String;
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

procedure TJetButton.Loaded;
begin
  inherited Loaded;
  if (FBitmapUp <> nil) and (FBitmapUp.Width > 0) and (FBitmapUp.Height > 0) then
  begin
    BitmapUpChanged(Self);
  end else if (FBitmapOver <> nil) and (FBitmapOver.Width > 0) and (FBitmapOver.Height > 0) then
  begin
    BitmapOverChanged(Self);
  end;
  if (rGroupIndex<>0) then
  begin
       SetAllowAllUp(rAllowAllUp);
       SetAllowMoreDown(rAllowMoreDown);
   end;
end;

procedure TJetButton.AdjustBounds;
begin
  SetBounds(Left, Top, Width, Height);
end;

procedure TJetButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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

  if IsWindow(rHandle)
  then SetWindowPos(rHandle, 0, Left, Top, Width, Height, SWP_NOSENDCHANGING);
end;

procedure TJetButton.Invalidate;
var R: TRect;
begin
  if (Visible or (csDesigning in ComponentState)) and
    (Parent <> nil) and Parent.HandleAllocated then
  begin
    R := BoundsRect;
    InvalidateRect(Parent.Handle, @R, True);
  end;
end;

procedure TJetButton.SetEnabled(Value : Boolean);
begin
 if Value <> FEnabled then
 begin
   FEnabled := Value;
   if FEnabled = false then FState := bsDisabled else FState := bsUp;
   Invalidate;
 end;
end;

procedure TJetButton.SetState(Value: TButtonState);
begin
     if (Value <> FState) then
     begin
          if (rGroupIndex<>0) and not(csLoading in ComponentState)
          then begin
                    if (Value=bsDown)
                    then begin
                            if not(GroupsList.GetGroup(rGroupIndex).Down(Self)) then exit;
                          end
                    else if (Value=bsUp)
                         then begin
                                 if not(GroupsList.GetGroup(rGroupIndex).Up(Self)) then exit;
                               end;
               end;
          FState :=Value;
          Invalidate;
      end;
end;

procedure TJetButton.SetStateNoGroup(Value: TButtonState);
begin
     if (Value <> FState) then
     begin
          FState :=Value;
          Invalidate;
      end;
end;

procedure TJetButton.SetGroupIndex(Value: Integer);
begin
     if (Value<>rGroupIndex) then
     begin
          if (rGroupIndex<>0) then GroupsList.GetGroup(rGroupIndex).RemoveButton(Self);
          if (Value<>0)
          then GroupsList.GetGroup(Value).AddButton(Self);
          rGroupIndex :=Value;
          Invalidate;
      end;
end;

function TJetButton.GetAllowAllUp :Boolean;
begin
     if (rGroupIndex<>0)
     then Result :=GroupsList.GetGroup(rGroupIndex).AllowAllUp
     else Result :=False;
end;

procedure TJetButton.SetAllowAllUp(Value :Boolean);
begin
     if (csLoading in ComponentState)
     then rAllowAllUp :=Value
     else if (rGroupIndex<>0) then GroupsList.GetGroup(rGroupIndex).AllowAllUp :=Value;
end;

function TJetButton.GetAllowMoreDown :Boolean;
begin
     if (rGroupIndex<>0)
     then Result :=GroupsList.GetGroup(rGroupIndex).AllowMoreDown
     else Result :=False;
end;

procedure TJetButton.SetAllowMoreDown(Value :Boolean);
begin
     if (csLoading in ComponentState)
     then rAllowMoreDown :=Value
     else if (rGroupIndex<>0) then GroupsList.GetGroup(rGroupIndex).AllowMoreDown :=Value;
end;

procedure TJetButton.SetBlinking(Value: Boolean);
begin
     if rBlinkRemain and (Value=False)
     then begin
               isBlink :=False;
               Invalidate;
          end;     
     if (Value<>BlinkTimer.Enabled) then
     begin
          BlinkCounter :=1;
          BlinkTimer.Enabled :=Value;
          isBlink :=Value;
          Invalidate;
      end;
end;

function TJetButton.GetBlinking: Boolean;
begin
     Result :=BlinkTimer.Enabled;
end;

procedure TJetButton.SetBlinkInterval(Value: DWord);
begin
     BlinkTimer.Interval :=(Value div 2);
end;

function TJetButton.GetBlinkInterval: DWord;
begin
     Result :=BlinkTimer.Interval*2;
end;


procedure TJetButton.BlinkEvent(Sender: TObject);
begin
     isBlink :=not(isBlink);
     if isBlink
     then Inc(BlinkCounter)
     else begin
               BlinkTimer.Enabled :=not((BlinkCounter=rBlinkStopAfter));
               if rBlinkRemain and not(BlinkTimer.Enabled)
               then isBLink :=True;
          end;

     if Assigned(rOnBlink)
     then rOnBlink(Self, isBlink);

     Invalidate;
end;

//==============================================================================


procedure TButtonsList.AddButton(Button: TJetButton);
begin
     TButtonsListCollectionItem(Self.Add).Button :=Button;
end;

function TButtonsList.FindButton(Button: TJetButton): TButtonsListCollectionItem;
begin
   Result := Nil; // provvisorio
end;

procedure TButtonsList.RemoveButton(Button: TJetButton);
Var
   toDel :TButtonsListCollectionItem;

begin
     toDel :=Self.FindButton(Button);
     if (toDel<>Nil) then Self.Delete(toDel.Index);
end;

function TButtonsList.Down(Button: TJetButton): Boolean;
Var
   Current :TJetButton;
   i       :Integer;

begin
     Result :=False;

     for i:=0 to Self.Count-1 do
     begin
          Current :=TButtonsListCollectionItem(Self.Items[i]).Button;
          if (Current<>Button)
          then begin
                    if not(AllowMoreDown) then Current.SetStateNoGroup(bsUp);
               end     
          else begin
                    Current.SetStateNoGroup(bsDown);
                    Result :=True;
               end;
      end;
end;

function TButtonsList.Up(Button:TJetButton):Boolean;
Var
   Current    :TJetButton;
   i,
   btnCounter :Integer;

begin
     Result :=False;

     btnCounter :=0;
     for i:=0 to Self.Count-1 do
     begin
          Current :=TButtonsListCollectionItem(Self.Items[i]).Button;
          if (Current.State=bsUp) then Inc(btnCounter);
      end;
     if AllowAllUp
     then begin
               Button.SetStateNoGroup(bsUp);
               Result :=True;
          end
     else begin
               if not(btnCounter=(Self.Count-1)) then     //<>1
               begin
                    Button.SetStateNoGroup(bsUp);
                    Result :=True;
                end;
          end;
end;

constructor TGroupsListCollectionItem.Create(Collection: TCollection);
begin
     inherited Create(Collection);
     ButtonList :=TButtonsList.Create(TButtonsListCollectionItem);
     ButtonList.AllowAllUp :=False;
     ButtonList.AllowMoreDown :=False;
end;

destructor TGroupsListCollectionItem.Destroy;
begin
     ButtonList.Free;
     inherited Destroy;
end;


function TGroupsList.FindGroup(GroupIndex: Integer): TGroupsListCollectionItem;
Var
   i     :Integer;

begin
     Result :=Nil;
     for i:=0 to Self.Count-1 do
     begin
          if (TGroupsListCollectionItem(Self.Items[i]).GroupIndex=GroupIndex)
          then begin
                    Result :=TGroupsListCollectionItem(Self.Items[i]);
                    Exit;
                end;
      end;
end;

function TGroupsList.GetGroup(GroupIndex :Integer) :TButtonsList;
Var
   toGet :TGroupsListCollectionItem;

begin
     toGet :=Self.FindGroup(GroupIndex);
     if (toGet=Nil) then begin
                              toGet :=TGroupsListCollectionItem(Add);
                              toGet.GroupIndex :=GroupIndex;
                          end;
     Result :=toGet.ButtonList;
end;

procedure TGroupsList.RemoveGroup(GroupIndex :Integer);
Var
   toDel :TGroupsListCollectionItem;

begin
     toDel :=Self.FindGroup(GroupIndex);
     if (toDel<>Nil) then Self.Delete(toDel.Index);
end;

// **********************************************************************************
// **********************************************************************************

procedure TMouseWinControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
   P :TPoint;
begin
     P := Point(X, Y);
     P := TJetButton(Owner).ScreenToClient(ClientToScreen(P));
     TJetButton(Owner).MouseMove(Shift, P.X, P.Y);
end;

procedure TMouseWinControl.MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
var
   P :TPoint;
begin
     P := Point(X, Y);
     P := TJetButton(Owner).ScreenToClient(ClientToScreen(P));
     TJetButton(Owner).MouseDown(Button, Shift, P.X, P.Y);
end;

procedure TMouseWinControl.MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
var
   P :TPoint;
begin
     P := Point(X, Y);
     P := TJetButton(Owner).ScreenToClient(ClientToScreen(P));
     TJetButton(Owner).MouseUp(Button, Shift, P.X, P.Y);
end;

// **********************************************************************************
// **********************************************************************************

initialization
  GroupsList :=TGroupsList.Create(TGroupsListCollectionItem);

finalization
  GroupsList.Free;
  GroupsList :=Nil;

end.
