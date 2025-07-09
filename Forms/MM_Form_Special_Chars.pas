unit MM_Form_Special_Chars;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, Buttons, ButtonPanel;

{ TFormSpecialChars }

type
  TFormSpecialChars = class(TForm)
    btInsert: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbInsiemi: TComboBox;
    lbCurChar: TLabel;
    edHex: TLabeledEdit;
    edDec: TLabeledEdit;
    edText: TLabeledEdit;
    sgChars: TStringGrid;
    procedure btInsertClick(Sender: TObject);
    procedure cbInsiemiChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgCharsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure sgCharsSelection(Sender: TObject; aCol, aRow: Integer);
  private
    { private declarations }
    internalSel: Boolean;
    curChar: WideChar;

    procedure UpdateCharInfo(AChar: WideChar);

  public
    { public declarations }
    function Execute(var SelectedText: WideString): TModalResult;
  end;

var
  FormSpecialChars: TFormSpecialChars = nil;

function FormSpecialChars_CanEdit(insControl: TControl; excludeIntDataType: Boolean=True): Boolean;

function FormSpecialCharsShow(var SelectedText: WideString): TModalResult; overload;
function FormSpecialCharsShow(insControl: TControl): WideString; overload;


implementation

{$R *.lfm}

uses MM_Special_Chars, db, DbCtrls, DBExtCtrls, LazUTF8, GroupedEdit, EditBtn, Spin;

type
  TCustomEditAcc = class(TCustomEdit)
    public
      property AutoSelect;
  end;

function FormSpecialChars_CanEdit(insControl: TControl; excludeIntDataType: Boolean): Boolean;
var
  fType: TFieldType;

begin
  Result :=(insControl is TCustomEdit) and not(TCustomEdit(insControl).ReadOnly);
  if Result and excludeIntDataType
  then begin
         if (insControl is TCustomFloatSpinEdit)
         then Result :=False
         else
         if (insControl is TDBEdit)
         then fType :=TDBEdit(insControl).Field.DataType
         else
         if (insControl is TDBMemo)
         then fType :=TDBMemo(insControl).Field.DataType
         else
         if (insControl is TGEEdit)
         then begin
                   //if (TGEEdit(insControl).Owner is TDBDateEdit)
                   //then fType :=TDBDateEdit(TGEEdit(insControl).Owner).Field.DataType
                   //else
                   if (TGEEdit(insControl).Owner is TDateEdit)
                   then Result :=False
                   else fType :=ftUnknown;
              end
         else fType :=ftUnknown;

         Result := Result and not(fType in [ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftBCD,
                                          ftDate, ftTime, ftDateTime, ftAutoInc, ftLargeint, ftTimeStamp]);
       end;
end;

function FormSpecialCharsShow(var SelectedText: WideString): TModalResult;
begin
  if (FormSpecialChars = nil)
  then FormSpecialChars :=TFormSpecialChars.Create(Application);

  Result :=FormSpecialChars.Execute(SelectedText);

  FreeAndNil(FormSpecialChars);
end;

function FormSpecialCharsShow(insControl: TControl): WideString; overload;
var
   curActive: TControl;
   oldAutoSelect: Boolean;
   oldCaretPos: TPoint;     //Provare in Windows se Serve

   procedure DBEditSetSelText(Val: WideString);
   var
      OldText, NewText: WideString;
      OldPos: Integer;

   begin
     OldPos := TDBEdit(insControl).SelStart;
     OldText := TDBEdit(insControl).Field.AsWideString;
     NewText := UTF8Copy(OldText, 1, OldPos) +
                Val +
                UTF8Copy(OldText, OldPos + TDBEdit(insControl).SelLength + 1, MaxInt);
     if not(TDBEdit(insControl).DataSource.State in [dsInsert, dsEdit])
     then TDBEdit(insControl).DataSource.Edit;
     TDBEdit(insControl).Field.AsWideString := NewText;
     TDBEdit(insControl).SelStart := OldPos + UTF8Length(Val);
   end;

   procedure DBMemoSetSelText(Val: WideString);
   var
      OldText, NewText: WideString;
      OldPos: Integer;

   begin
     OldPos := TDBMemo(insControl).SelStart;
     OldText := TDBMemo(insControl).Field.AsWideString;
     NewText := UTF8Copy(OldText, 1, OldPos) +
                Val +
                UTF8Copy(OldText, OldPos + TDBMemo(insControl).SelLength + 1, MaxInt);
     if not(TDBMemo(insControl).DataSource.State in [dsInsert, dsEdit])
     then TDBMemo(insControl).DataSource.Edit;
     TDBMemo(insControl).Field.AsWideString := NewText;
     TDBMemo(insControl).SelStart := OldPos + UTF8Length(Val);
   end;

begin
   Result :='';

   if (insControl is TCustomEdit)
   then begin
             oldAutoSelect :=TCustomEditAcc(insControl).AutoSelect;
             TCustomEditAcc(insControl).AutoSelect :=False;
             //oldCaretPos :=TCustomEditAcc(insControl).CaretPos;
        end
   else exit;

   if (FormSpecialCharsShow(Result) = mrOk) then
   begin

      if (insControl is TDBMemo) //DBMemo have the same Method but is not derived from TDBEdit...
      then begin
                if (TDBMemo(insControl).DataSource <> nil)
                then DBMemoSetSelText(Result);
           end
      else
      if (insControl is TDBEdit)
      then begin
                if (TDBEdit(insControl).DataSource <> nil)
                then DBEditSetSelText(Result);
           end
      else
      if (insControl is TCustomEdit)
      then begin
                TCustomEditAcc(insControl).SelText :=Result;
            end
      else begin
            end;
   end;

   if (insControl is TCustomEdit)
   then begin
             TCustomEditAcc(insControl).AutoSelect :=oldAutoSelect;
             //TCustomEditAcc(insControl).CaretPos :=oldCaretPos;
        end
   else begin
        end;
end;

{ TFormSpecialChars }

procedure TFormSpecialChars.FormCreate(Sender: TObject);
var
   iSpecialChars, icurChar: Integer;
   iCol, iRow: Word;

   procedure AdjustRowCol;
   begin
     if (iCol = sgChars.ColCount)
     then begin
            inc(iRow);
            if (iRow = sgChars.RowCount)
            then sgChars.RowCount :=sgChars.RowCount+1;
            iCol :=0;
          end;
   end;

begin
  iCol :=0; iRow :=0;
  for iSpecialChars :=0 to length(SpecialChars)-1 do
  begin
    AdjustRowCol;
    cbInsiemi.AddItem(SpecialChars_Caption[iSpecialChars], TObject(Pointer(iRow*$10000+iCol)));
    for icurChar:=1 to length(SpecialChars[iSpecialChars]^) do
    begin
      AdjustRowCol;

      curChar :=SpecialChars[iSpecialChars]^[icurChar];
      sgChars.Cells[iCol, iRow] :=curChar;
      sgChars.Objects[iCol, iRow] :=TObject(Pointer(iSpecialChars));

      inc(iCol);
    end;
  end;
  (*
  for icurChar:=$01 to $FF do
  begin
    AdjustRowCol;
    sgChars.Cells[iCol, iRow] :=WideChar(icurChar);
    sgChars.Objects[iCol, iRow] :=nil;
    inc(iCol);
  end;
  *)
end;

procedure TFormSpecialChars.FormShow(Sender: TObject);
begin
  curChar :=WideString(sgChars.Cells[0, 0])[1];
  UpdateCharInfo(curChar);
  cbInsiemi.ItemIndex :=0;
end;

procedure TFormSpecialChars.sgCharsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (WideString(sgChars.Cells[aCol, aRow]) <> '');
end;

procedure TFormSpecialChars.cbInsiemiChange(Sender: TObject);
var
   aRow, aCol: Word;

begin
  aRow :=Hi(DWord(Pointer(cbInsiemi.Items.Objects[cbInsiemi.ItemIndex])));
  aCol :=Lo(DWord(Pointer(cbInsiemi.Items.Objects[cbInsiemi.ItemIndex])));
  sgChars.TopRow :=aRow;
  sgChars.Row :=aRow;
  sgChars.Col  :=aCol;
end;

procedure TFormSpecialChars.btInsertClick(Sender: TObject);
begin
  edText.Text :=edText.Text+curChar;
end;

procedure TFormSpecialChars.sgCharsSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if (WideString(sgChars.Cells[aCol, aRow]) <> '') then
  begin
    curChar :=WideString(sgChars.Cells[aCol, aRow])[1];
    UpdateCharInfo(curChar);
    cbInsiemi.ItemIndex :=Integer(Pointer(sgChars.Objects[aCol, aRow]));
  end;
end;

procedure TFormSpecialChars.UpdateCharInfo(AChar: WideChar);
begin
  lbCurChar.Caption :=AChar;
  edHex.Text :=IntToHex(Integer(AChar), Sizeof(WideChar)*2);
  edDec.Text :=IntToStr(Integer(AChar));
end;

function TFormSpecialChars.Execute(var SelectedText: WideString): TModalResult;
begin
  Result :=ShowModal;
  if (Result = mrOk)
  then if (edText.Text = '')
       then SelectedText :=curChar
       else SelectedText :=edText.Text;
end;

end.

