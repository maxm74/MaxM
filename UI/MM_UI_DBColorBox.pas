unit MM_UI_DBColorBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ColorBox, StdCtrls, LCLStrConsts, LCLType, db;

type
  { TMM_UI_DBColorBox }

  TMM_UI_DBColorBox = class;
  TDBGetColorsEvent = procedure(Sender: TMM_UI_DBColorBox; Items: TStrings) of object;
  TDBGetColorNameEvent = function(Sender: TMM_UI_DBColorBox; AColor: TColor):String of object;

  TMM_UI_DBColorBox = class(TDBComboBox)
  private
    FDefaultColorColor: TColor;
    FNoneColorColor: TColor;
    FOnGetColors: TDBGetColorsEvent;
    FStyle: TColorBoxStyle;
    FSelected: TColor;
    rOnGetColorName: TDBGetColorNameEvent;
    function GetColor(Index : Integer): TColor;
    function GetColorName(Index: Integer): string;
    function GetSelected: TColor;
    procedure SetDefaultColorColor(const AValue: TColor);
    procedure SetNoneColorColor(const AValue: TColor);
    procedure SetSelected(Value: TColor);
    procedure SetStyle(const AValue: TColorBoxStyle); reintroduce;
    procedure ColorProc(const s: AnsiString);
    procedure UpdateCombo;
    function GetPrettyColorName(ColorName: String): String;
  protected
    procedure DataChange(Sender: TObject); override;
    procedure UpdateData(Sender: TObject); override;

    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure SetColorList;
    procedure Loaded; override;
    procedure InitializeWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoGetColors; virtual;
    procedure CloseUp; override;
    function PickCustomColor: Boolean; virtual;

    property ReadOnly;
  public
    constructor Create(AOwner: TComponent); override;

    property Colors[Index: Integer]: TColor read GetColor;
    property ColorNames[Index: Integer]: string read GetColorName;
  published
    property Style: TColorBoxStyle read FStyle write SetStyle
      default [cbStandardColors, cbExtendedColors, cbSystemColors];
    property Selected: TColor read GetSelected write SetSelected default clNone;
    property DefaultColorColor: TColor read FDefaultColorColor write SetDefaultColorColor default clBlack;
    property NoneColorColor: TColor read FNoneColorColor write SetNoneColorColor default clMenu;
    property OnGetColors: TDBGetColorsEvent read FOnGetColors write FOnGetColors;
    property OnGetColorName: TDBGetColorNameEvent read rOnGetColorName write rOnGetColorName;

    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemWidth;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

procedure Register;

implementation

uses LazLoggerBase;

procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_DBColorBox]);
end;

function TMM_UI_DBColorBox.GetPrettyColorName(ColorName: String): String;

  function FindInMap(ColorName: String; out NewColorName: String): Boolean;
  var
    Color: TColor;
  begin
    Result := IdentToColor(ColorName, Color);
    if Result then
    begin
      if Assigned(rOnGetColorName)
      then NewColorName :=rOnGetColorName(Self, Color)
      else begin
                { workaround for a bug in fpc 2.2.2 }
                if Color=clScrollBar then
                  NewColorName := rsScrollBarColorCaption
                else
                    case Color of
                    clBlack                   : NewColorName := rsBlackColorCaption;
                    clMaroon                  : NewColorName := rsMaroonColorCaption;
                    clGreen                   : NewColorName := rsGreenColorCaption;
                    clOlive                   : NewColorName := rsOliveColorCaption;
                    clNavy                    : NewColorName := rsNavyColorCaption;
                    clPurple                  : NewColorName := rsPurpleColorCaption;
                    clTeal                    : NewColorName := rsTealColorCaption;
                    clGray                    : NewColorName := rsGrayColorCaption;
                    clSilver                  : NewColorName := rsSilverColorCaption;
                    clRed                     : NewColorName := rsRedColorCaption;
                    clLime                    : NewColorName := rsLimeColorCaption;
                    clYellow                  : NewColorName := rsYellowColorCaption;
                    clBlue                    : NewColorName := rsBlueColorCaption;
                    clFuchsia                 : NewColorName := rsFuchsiaColorCaption;
                    clAqua                    : NewColorName := rsAquaColorCaption;
                    clWhite                   : NewColorName := rsWhiteColorCaption;
                    clMoneyGreen              : NewColorName := rsMoneyGreenColorCaption;
                    clSkyBlue                 : NewColorName := rsSkyBlueColorCaption;
                    clCream                   : NewColorName := rsCreamColorCaption;
                    clMedGray                 : NewColorName := rsMedGrayColorCaption;
                    clNone                    : NewColorName := rsNoneColorCaption;
                    clDefault                 : NewColorName := rsDefaultColorCaption;
                    clBackground              : NewColorName := rsBackgroundColorCaption;
                    clActiveCaption           : NewColorName := rsActiveCaptionColorCaption;
                    clInactiveCaption         : NewColorName := rsInactiveCaptionColorCaption;
                    clMenu                    : NewColorName := rsMenuColorCaption;
                    clWindow                  : NewColorName := rsWindowColorCaption;
                    clWindowFrame             : NewColorName := rsWindowFrameColorCaption;
                    clMenuText                : NewColorName := rsMenuTextColorCaption;
                    clWindowText              : NewColorName := rsWindowTextColorCaption;
                    clCaptionText             : NewColorName := rsCaptionTextColorCaption;
                    clActiveBorder            : NewColorName := rsActiveBorderColorCaption;
                    clInactiveBorder          : NewColorName := rsInactiveBorderColorCaption;
                    clAppWorkspace            : NewColorName := rsAppWorkspaceColorCaption;
                    clHighlight               : NewColorName := rsHighlightColorCaption;
                    clHighlightText           : NewColorName := rsHighlightTextColorCaption;
                    clBtnFace                 : NewColorName := rsBtnFaceColorCaption;
                    clBtnShadow               : NewColorName := rsBtnShadowColorCaption;
                    clGrayText                : NewColorName := rsGrayTextColorCaption;
                    clBtnText                 : NewColorName := rsBtnTextColorCaption;
                    clInactiveCaptionText     : NewColorName := rsInactiveCaptionText;
                    clBtnHighlight            : NewColorName := rsBtnHighlightColorCaption;
                    cl3DDkShadow              : NewColorName := rs3DDkShadowColorCaption;
                    cl3DLight                 : NewColorName := rs3DLightColorCaption;
                    clInfoText                : NewColorName := rsInfoTextColorCaption;
                    clInfoBk                  : NewColorName := rsInfoBkColorCaption;
                    clHotLight                : NewColorName := rsHotLightColorCaption;
                    clGradientActiveCaption   : NewColorName := rsGradientActiveCaptionColorCaption;
                    clGradientInactiveCaption : NewColorName := rsGradientInactiveCaptionColorCaption;
                    clMenuHighlight           : NewColorName := rsMenuHighlightColorCaption;
                    clMenuBar                 : NewColorName := rsMenuBarColorCaption;
                    clForm                    : NewColorName := rsFormColorCaption;
                    else
                     Result := False;
                    end;
           end;
      end;
  end;

begin
  // check in color map
  if not FindInMap(ColorName, Result) then
  begin
    Result := ColorName;
    if Copy(Result, 1, 2) = 'cl' then
      Delete(Result, 1, 2);
  end;
end;

{------------------------------------------------------------------------------
  Method:   TMM_UI_DBColorBox.Create
  Params:   AOwner
  Returns:  Nothing

  Use Create to create an instance of TMM_UI_DBColorBox and initialize all properties
  and variables.

 ------------------------------------------------------------------------------}
constructor TMM_UI_DBColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := csOwnerDrawFixed;

  FStyle := [cbStandardColors, cbExtendedColors, cbSystemColors];
  FNoneColorColor := clMenu;
  FDefaultColorColor := clBlack;
  FSelected := clNone;

  SetColorList;
end;
{------------------------------------------------------------------------------
  Method:   TMM_UI_DBColorBox.GetSelected
  Params:   None
  Returns:  TColor

  Use GetSelected to convert the item selected into a system color.

 ------------------------------------------------------------------------------}
function TMM_UI_DBColorBox.GetSelected: TColor;
begin
  if HandleAllocated then
  begin
    if ItemIndex <> -1 then
    begin
      Result := Colors[ItemIndex];
      // keep FSelected in sync
      if FSelected <> Result then
      begin
        // DebugLn('WARNING TMM_UI_DBColorBox: FSelected out of sync with Colors[0]');
        FSelected := Result;
      end;
    end else
      Result := FSelected;
  end
  else
    Result := FSelected;
end;

procedure TMM_UI_DBColorBox.SetDefaultColorColor(const AValue: TColor);
begin
  if FDefaultColorColor <> AValue then
  begin
    FDefaultColorColor := AValue;
    invalidate;
  end;
end;

procedure TMM_UI_DBColorBox.SetNoneColorColor(const AValue: TColor);
begin
  if FNoneColorColor <> AValue then
  begin
    FNoneColorColor := AValue;
    invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TMM_UI_DBColorBox.GetColor
  Params:   Index
  Returns:  Color at position Index

  Used as read procedure from Colors property.

 ------------------------------------------------------------------------------}

function TMM_UI_DBColorBox.GetColor(Index : Integer): TColor;
begin
  Result := PtrInt(Items.Objects[Index])
end;

function TMM_UI_DBColorBox.GetColorName(Index: Integer): string;
begin
  Result := Items[Index];
end;

{------------------------------------------------------------------------------
  Method:   TMM_UI_DBColorBox.SetSelected
  Params:   Value
  Returns:  Nothing

  Use SetSelected to set the item in the ColorBox when appointed a color
  from code.

 ------------------------------------------------------------------------------}
procedure TMM_UI_DBColorBox.SetSelected(Value: TColor);
begin
  if (FSelected <> Value) then
  begin
       (*FSelected := Value;
       UpdateCombo;
       *)
       //Self.Select;
       Self.Field.DataSet.Edit;
       Self.Field.AsInteger := Integer(Value);
       //inherited Change; //On Windows Infinite Loop
  end;
end;

procedure TMM_UI_DBColorBox.SetStyle(const AValue: TColorBoxStyle);
begin
  if (FStyle <> AValue) then
  begin
       FStyle := AValue;
       SetColorList;
  end;
end;

procedure TMM_UI_DBColorBox.ColorProc(const s: AnsiString);
var
  AColor: TColor;
  Index: Integer;
  ColorCaption: String;
begin
  if IdentToColor(s, AColor) then
  begin
    if AColor = clWhite then
      AColor := AColor;
    // check clDefault
    if not (cbIncludeDefault in Style) and (AColor = clDefault) then
      Exit;
    // check clNone
    if not (cbIncludeNone in Style) and (AColor = clNone) then
      Exit;
    // check System colors
    if not (cbSystemColors in Style) and ((AColor and SYS_COLOR_BASE) <> 0) then
      Exit;
    // check Standard, Extended colors
    if ([cbStandardColors, cbExtendedColors] * Style <> [cbStandardColors, cbExtendedColors]) and
        ColorIndex(AColor, Index) then
    begin
      if not (cbStandardColors in Style) and (Index < StandardColorsCount) then
        Exit;
      if not (cbExtendedColors in Style) and
          (Index < StandardColorsCount + ExtendedColorCount) and
          (Index >= StandardColorsCount) then
        Exit;
    end;

    if cbPrettyNames in Style then
      ColorCaption := GetPrettyColorName(s)
    else
      ColorCaption := s;

    Items.AddObject(ColorCaption, TObject(PtrInt(AColor)));
  end;
end;

procedure TMM_UI_DBColorBox.UpdateCombo;
var
  c: integer;
begin
  if HandleAllocated then
  begin
    for c := Ord(cbCustomColor in Style) to Items.Count - 1 do
    begin
      if Colors[c] = FSelected then
      begin
        ItemIndex := c;
        Exit;
      end;
    end;
    if cbCustomColor in Style then
    begin
      Items.Objects[0] := TObject(PtrInt(FSelected));
      ItemIndex := 0;
      Invalidate;
    end
    else
      ItemIndex := -1;
  end;
end;

procedure TMM_UI_DBColorBox.DataChange(Sender: TObject);
Var
  theField :TField;

begin
  theField :=Self.Field;

  if Assigned(theField) and not(theField.IsNull)
  then Self.FSelected := TColor(theField.AsInteger)
  else Self.FSelected := clNone;

  UpdateCombo;
end;

procedure TMM_UI_DBColorBox.UpdateData(Sender: TObject);
Var
  theField :TField;

begin
  theField :=Self.Field;

  if (Self.Selected = clNone)
  then theField.Clear
  else theField.AsInteger := Integer(Self.Selected);
end;

{------------------------------------------------------------------------------
  Method:   TMM_UI_DBColorBox.DrawItem
  Params:   Index, Rect, State
  Returns:  Nothing

  Use DrawItem to customdraw an item in the ColorBox. A color preview is drawn
  and the item rectangle is made smaller and given to the inherited method to
  draw the corresponding text. The Brush color and Pen color where changed and
  reset to their original values.

 ------------------------------------------------------------------------------}
procedure TMM_UI_DBColorBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  BrushColor, PenColor, NewColor: TColor;
begin
  //DebugLn('TMM_UI_DBColorBox.DrawItem ',Name,':',IntToStr(Index));

  if Index = -1 then
    Exit;

  r.top := Rect.top + 3;
  r.bottom := Rect.bottom - 3;
  r.left := Rect.left + 3;
  r.right := r.left + 14;
  Exclude(State, odBackgroundPainted); //odPainted);

  with Canvas do
  begin
    FillRect(Rect);

    BrushColor := Brush.Color;
    PenColor := Pen.Color;

    NewColor := Self.Colors[Index];

    if NewColor = clNone then
      NewColor := NoneColorColor
    else
    if NewColor = clDefault then
      NewColor := DefaultColorColor;

    Brush.Color := NewColor;
    Pen.Color := clBlack;

    Rectangle(BidiFlipRect(r, Rect, UseRightToLeftAlignment));

    Brush.Color := BrushColor;
    Pen.Color := PenColor;
  end;
  r := Rect;
  r.left := r.left + 20;

  inherited DrawItem(Index, BidiFlipRect(r, Rect, UseRightToLeftAlignment), State);
end;

{------------------------------------------------------------------------------
  Method:   TMM_UI_DBColorBox.SetColorList
  Params:   None
  Returns:  Nothing

  Use SetColorList to fill the itemlist in the ColorBox with the right color
  entries. Based on the value of the Palette property.

 ------------------------------------------------------------------------------}
procedure TMM_UI_DBColorBox.SetColorList;
var
  OldSelected: Integer;

begin
  // we need to wait while we finish loading since we depend on style and OnGetColors event
  if (csLoading in ComponentState) then
    Exit;

  OldSelected := FSelected;
  with Items do
  begin
    Clear;
    if cbCustomColor in Style then
      Items.AddObject(rsCustomColorCaption, TObject(PtrInt(clBlack)));
    GetColorValues(@ColorProc);
    if (cbCustomColors in Style) then
      DoGetColors;
  end;
  Selected := OldSelected;
end;

procedure TMM_UI_DBColorBox.Loaded;
begin
  inherited Loaded;

  SetColorList;
end;

procedure TMM_UI_DBColorBox.InitializeWnd;
begin
  inherited InitializeWnd;

  UpdateCombo;
end;

procedure TMM_UI_DBColorBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  //We Want a Drop Down List but we can not set TCustomComboBox(ReadOnly) :=True;
  Params.Style := Params.Style and not(CBS_DROPDOWN);
  Params.Style := Params.Style or CBS_DROPDOWNLIST;
end;

procedure TMM_UI_DBColorBox.DoGetColors;
begin
  if Assigned(OnGetColors) then
    OnGetColors(Self, Items)
end;

procedure TMM_UI_DBColorBox.CloseUp;
begin
  if (cbCustomColor in Style) and (ItemIndex = 0) then // custom color has been selected
    PickCustomColor;
  if (ItemIndex <> -1) then
    Selected := Colors[ItemIndex];

  inherited CloseUp;
end;

function TMM_UI_DBColorBox.PickCustomColor: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    Result := False;
    Exit;
  end;

  with TColorDialog.Create(Self) do
  begin
    Color := Colors[0];
    Result := Execute;
    if Result then
    begin
      Items.Objects[0] := TObject(PtrInt(Color));
      invalidate;
    end;
    Free;
  end;
end;


end.
