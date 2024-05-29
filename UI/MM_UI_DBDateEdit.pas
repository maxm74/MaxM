unit MM_UI_DBDateEdit;

{$mode objfpc}{$H+}
//{$define DebugInfo}

interface

uses
  Classes, SysUtils, LResources, db, Forms, Controls, Graphics, Dialogs,
  DBExtCtrls, EditBtn;

const
     clMyRed = TColor($0033FF);
     cInvalidValueMessage ='Invalid Date';

type
  { TMM_UI_DBDateEdit }

  TMM_UI_DBDateEdit = class(TDBDateEdit)
  protected
     rIsValid,
     rNullIsValid,
     rButtonVisible,
     rIsNull,
     rInvalidLockFocus: Boolean;
     rInvalidValueColor: TColor;
     rInvalidValueMessage: String;
     EditorDefColor: TColor;

     //Copied from lcl because is private...(why not Protected?)
     function TextToDate(AText: String; ADefault: TDateTime): TDateTime;
     function Get_IsValid: Boolean;

     procedure EditEditingDone; override;
     procedure EditExit; override;
     function CalcButtonVisible: Boolean; override;

  public
     constructor Create(AOwner: TComponent); override;
     procedure Loaded; override;

     property IsValid: Boolean read Get_IsValid;
     property IsNull: Boolean read rIsNull;

  published
     function MM_UI_HighlighterCanProcess(isActive :Boolean): Boolean;
     function MM_UI_HighlighterGetStdColor: TColor;

     property ButtonVisible: Boolean read rButtonVisible write rButtonVisible;
     property InvalidValueColor: TColor read rInvalidValueColor write rInvalidValueColor default clMyRed;
     property InvalidValueMessage: String read rInvalidValueMessage write rInvalidValueMessage;
     property NullIsValid: Boolean read rNullIsValid write rNullIsValid default True;
     property InvalidLockFocus: Boolean read rInvalidLockFocus write rInvalidLockFocus default True;
  end;


  { TMM_UI_DateEdit }

  TMM_UI_DateEdit = class(TDateEdit)
  protected
     rIsValid,
     rNullIsValid,
     rButtonVisible,
     rIsNull,
     rInvalidLockFocus: Boolean;
     rInvalidValueColor: TColor;
     rInvalidValueMessage: String;
     EditorDefColor: TColor;

     function TextToDate(AText: String; ADefault: TDateTime): TDateTime;
     function Get_IsValid: Boolean;

     procedure EditEditingDone; override;
     procedure EditExit; override;
     function CalcButtonVisible: Boolean; override;

  public
     constructor Create(AOwner: TComponent); override;
     procedure Loaded; override;

     property IsValid: Boolean read Get_IsValid;
     property IsNull: Boolean read rIsNull;

  published
     function MM_UI_HighlighterCanProcess(isActive :Boolean): Boolean;
     function MM_UI_HighlighterGetStdColor: TColor;

     property ButtonVisible: Boolean read rButtonVisible write rButtonVisible;
     property InvalidValueColor: TColor read rInvalidValueColor write rInvalidValueColor default clMyRed;
     property InvalidValueMessage: String read rInvalidValueMessage write rInvalidValueMessage;
     property NullIsValid: Boolean read rNullIsValid write rNullIsValid default True;
     property InvalidLockFocus: Boolean read rInvalidLockFocus write rInvalidLockFocus default True;
  end;

procedure Register;

implementation

{$ifdef DebugInfo}
uses LCLProc;
{$endif}

procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_DBDateEdit, TMM_UI_DateEdit]);
end;

Function ParseDate(S : String; Order : TDateOrder; Def: TDateTime) : TDateTime;

Var
  P,N1,N2,N3 : Integer;
  B : Boolean;

begin
  Result:=Def;
  P:=Pos(DefaultFormatSettings.DateSeparator,S);
  If (P=0) then
    Exit;
  N1:=StrToIntDef(Copy(S,1,P-1),-1);
  If (N1=-1) then Exit;
  Delete(S,1,P);
  P:=Pos(DefaultFormatSettings.DateSeparator,S);
  If (P=0) then
    Exit;
  N2:=StrToIntDef(Copy(S,1,P-1),-1);
  If (N1=0) then Exit;
  Delete(S,1,P);
  N3:=StrToIntDef(S,-1);
  If (N3=-1) then
    exit;
  Case Order of
    doYMD : B:=TryEncodeDate(N1,N2,N3,Result);
    doMDY : B:=TryEncodeDate(N3,N1,N2,Result);
    doDMY : B:=TryEncodeDate(N3,N2,N1,Result);
    else B:=false;
  end;
  If not B then // Not sure if TryEncodeDate touches Result.
    Result:=Def;
end;

// Tries to parse string when DateOrder = doNone when string maybe contains
// literal day or monthnames. For example when ShortDateFormat = 'dd-mmm-yyy'
// Returns NullDate upon failure.
function ParseDateNoPredefinedOrder(SDate: String; FS: TFormatSettings): TDateTime;
var
  Fmt: String;
  DPos, MPos, YPos: SizeInt;
  DStr, MStr, YStr: String;
  LD, LM, LY: LongInt;
  DD, MM, YY: Word;
const
  Digits = ['0'..'9'];

  procedure GetPositions(out DPos, MPos, YPos: SizeInt);
  begin
    DStr := '';
    MStr := '';
    YStr := '';
    DPos := Pos('D', Fmt);
    MPos := Pos('M', Fmt);
    YPos := Pos('Y', Fmt);
    if (YPos = 0) or (MPos = 0) or (DPos = 0) then Exit;
    if (YPos > DPos) then YPos := 3 else YPos := 1;
    if (DPos < MPos) then
    begin
      if (YPos = 3) then
      begin
        DPos := 1;
        MPos := 2;
      end
      else
      begin
        DPos := 2;
        MPos := 3;
      end;
    end
    else
    begin
      if (YPos = 3) then
      begin
        DPos := 2;
        MPos := 1;
      end
      else
      begin
        DPos := 3;
        MPos := 2;
      end;
    end;
  end;

  procedure ReplaceLiterals;
  var
    i, P: Integer;
    Sub: String;
  begin
    if (Pos('MMMM',Fmt) > 0) then
    begin //long monthnames
      //writeln('Literal monthnames');
      for i := 1 to 12 do
      begin
        Sub := FS.LongMonthNames[i];
        P := Pos(Sub, SDate);
        if (P > 0) then
        begin
          Delete(SDate, P, Length(Sub));
          Insert(IntToStr(i), SDate, P);
          Break;
        end;
      end;
    end
    else
    begin
      if (Pos('MMM',Fmt) > 0) then
      begin //short monthnames
        for i := 1 to 12 do
        begin
          Sub := FS.ShortMonthNames[i];
          P := Pos(Sub, SDate);
          if (P > 0) then
          begin
            Delete(SDate, P, Length(Sub));
            Insert(IntToStr(i), SDate, P);
            Break;
          end;
        end;
      end;
    end;

    if (Pos('DDDD',Fmt) > 0) then
    begin  //long daynames
      //writeln('Literal daynames');
      for i := 1 to 7 do
      begin
        Sub := FS.LongDayNames[i];
        P := Pos(Sub, SDate);
        if (P > 0) then
        begin
          Delete(SDate, P, Length(Sub));
          Break;
        end;
      end;
    end
    else
    begin
      if (Pos('DDD',Fmt) > 0) then
      begin //short daynames
        for i := 1 to 7 do
        begin
          Sub := FS.ShortDayNames[i];
          P := Pos(Sub, SDate);
          if (P > 0) then
          begin
            Delete(SDate, P, Length(Sub));
            Break;
          end;
        end;
      end;
    end;
    SDate := Trim(SDate);
    //writeln('ReplaceLiterals -> ',SDate);
  end;

  procedure Split(out DStr, MStr, YStr: String);
  var
    i, P: Integer;
    Sep: Set of Char;
    Sub: String;
  begin
    DStr := '';
    MStr := '';
    YStr := '';
    Sep := [];
    for i :=  1 to Length(Fmt) do
      if not (Fmt[i] in Digits) then Sep := Sep + [Fmt[i]];
    //get fist part
    P := 1;
    while (P <= Length(SDate)) and (SDate[P] in Digits) do Inc(P);
    Sub := Copy(SDate, 1, P-1);
    Delete(SDate, 1, P);
    if (DPos = 1) then DStr := Sub else if (MPos = 1) then MStr := Sub else YStr := Sub;
    //get second part
    if (SDate = '') then Exit;
    while (Length(SDate) > 0) and (SDate[1] in Sep) do Delete(SDate, 1, 1);
    if (SDate = '') then Exit;
    P := 1;
    while (P <= Length(SDate)) and (SDate[P] in Digits) do Inc(P);
    Sub := Copy(SDate, 1, P-1);
    Delete(SDate, 1, P);
    if (DPos = 2) then DStr := Sub else if (MPos = 2) then MStr := Sub else YStr := Sub;
    //get thirdpart
    if (SDate = '') then Exit;
    while (Length(SDate) > 0) and (SDate[1] in Sep) do Delete(SDate, 1, 1);
    if (SDate = '') then Exit;
    Sub := SDate;
    if (DPos = 3) then DStr := Sub else if (MPos = 3) then MStr := Sub else YStr := Sub;
  end;

  procedure AdjustYear(var YY: Word);
  var
    CY, CM, CD: Word;
  begin
    DecodeDate(Date, CY, CM, CD);
    LY := CY Mod 100;
    CY := CY - LY;
    if ((YY - LY) <= 50) then
      YY := CY + YY
    else
      YY := CY + YY - 100;
  end;

begin
  Result := NullDate;  //assume failure
  if (Length(SDate) < 5) then Exit; //y-m-d is minimum we support
  Fmt := UpperCase(FS.ShortDateFormat); //only care about y,m,d so this will do
  GetPositions(DPos, MPos, YPos);
  ReplaceLiterals;
  if (not (SDate[1] in Digits)) or (not (SDate[Length(SDate)] in Digits)) then Exit;
  Split(Dstr, MStr, YStr);
  if not TryStrToInt(DStr, LD) or
     not TryStrToInt(Mstr, LM) or
     not TryStrToInt(YStr, LY) then Exit;
  DD := LD;
  MM := LM;
  YY := LY;
  if (YY < 100) and (Pos('YYYY', UpperCase(Fmt)) = 0) then
  begin
    AdjustYear(YY);
  end;
  if not TryEncodeDate(YY, MM, DD, Result) then
    Result := NullDate;
end;


{ TMM_UI_DBDateEdit }

function TMM_UI_DBDateEdit.TextToDate(AText: String; ADefault: TDateTime): TDateTime;
var
  FS: TFormatSettings;
begin
  if Assigned(OnCustomDate) then
    OnCustomDate(Self, AText);
  if (DateOrder = doNone) then
  begin
    FS := DefaultFormatSettings;
    if (Self.DateFormat <> '') then
      FS.ShortDateFormat := Self.DateFormat;
    if not TryStrToDate(AText, Result, FS) then
    begin
      Result := ParseDateNoPredefinedOrder(AText, FS);
      if (Result = NullDate) then Result := ADefault;
    end;
  end
  else
    Result := ParseDate(AText,DateOrder,ADefault)
end;

function TMM_UI_DBDateEdit.Get_IsValid: Boolean;
var
   txt, nullTxt :String;

begin
     txt :=Trim(Text);

     {$ifdef DebugInfo}
       debugln('TMM_UI_DBDateEdit.Get_IsValid ('+Self.Name+'):"'+txt+'"');
     {$endif}

     if (Self.DateOrder = doNone)
     then nullTxt :=''
     else nullTxt :=(DefaultFormatSettings.DateSeparator+'  '+DefaultFormatSettings.DateSeparator);

     rIsNull := (txt = nullTxt);
     if rIsNull
     then rIsValid := rNullIsValid
     else rIsValid := not(TextToDate(txt, NullDate) = NullDate);

     //rIsValid := (rNullIsValid and (txt = nullTxt)) or not((txt <> nullTxt) and (TextToDate(txt, NullDate) = NullDate));

     if not(csDesigning in ComponentState)
     then if rIsValid
          then Color :=EditorDefColor
          else Color :=rInvalidValueColor;

     {$ifdef DebugInfo}
       debugln('TMM_UI_DBDateEdit.Get_IsValid ('+Self.Name+'):  rIsValid='+BoolToStr(rIsValid, true)+', rIsNull='+BoolToStr(rIsNull, true));
     {$endif}

     Result :=rIsValid;
end;

procedure TMM_UI_DBDateEdit.EditEditingDone;
var
   txt, nullTxt :String;

begin
  {$ifdef DebugInfo}
    debugln('TMM_UI_DBDateEdit.EditEditingDone ('+Self.Name+')');
  {$endif}

     inherited EditEditingDone;

     if Get_IsValid
     then begin
               //Work Around for a Bug "Set Date, after Clear it" did not Clear the Field DateTime
               txt :=Trim(Text);

               if (Self.DateOrder = doNone)
               then nullTxt :=''
               else nullTxt :=(DefaultFormatSettings.DateSeparator+'  '+DefaultFormatSettings.DateSeparator);

               if (txt = nullTxt)
               then if Self.Field.DataSet.State in [dsInsert, dsEdit]
                    then Self.Field.Clear;
           end
     else begin
             (*  if (rInvalidValueMessage <> '')
               then Dialogs.MessageDlg(rInvalidValueMessage+#13#10+#13#10+Self.Text, mtError, [mbOk], 0);
               if (rInvalidLockFocus)
               then Self.SetFocus; *)
           end;
end;

procedure TMM_UI_DBDateEdit.EditExit;
begin
  {$ifdef DebugInfo}
    debugln('TMM_UI_DBDateEdit.EditExit ('+Self.Name+')');
  {$endif}

  if not(csDesigning in ComponentState) and not(Get_IsValid) then
  begin
       if (rInvalidValueMessage <> '')
       then Dialogs.MessageDlg(rInvalidValueMessage+#13#10+#13#10+Self.Text, mtError, [mbOk], 0);
       if (rInvalidLockFocus)
       then Self.SetFocus;
   end;

  inherited EditExit;
end;

function TMM_UI_DBDateEdit.CalcButtonVisible: Boolean;
begin
     Result := (csDesigning in ComponentState) or
               (rButtonVisible and inherited CalcButtonVisible);
end;

constructor TMM_UI_DBDateEdit.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);

     rInvalidValueColor :=clMyRed;
     rInvalidValueMessage :=''; //cInvalidValueMessage;
     rNullIsValid :=True;
     rIsNull :=True;
     rInvalidLockFocus :=True;
end;

procedure TMM_UI_DBDateEdit.Loaded;
begin
     inherited Loaded;

     EditorDefColor :=Color;
end;

function TMM_UI_DBDateEdit.MM_UI_HighlighterCanProcess(isActive: Boolean): Boolean;
begin
     Result :=IsValid;
end;

function TMM_UI_DBDateEdit.MM_UI_HighlighterGetStdColor: TColor;
begin
     Result :=EditorDefColor;
end;

{ TMM_UI_DateEdit }

function TMM_UI_DateEdit.TextToDate(AText: String; ADefault: TDateTime): TDateTime;
var
  FS: TFormatSettings;
begin
  if Assigned(OnCustomDate) then
    OnCustomDate(Self, AText);
  if (DateOrder = doNone) then
  begin
    FS := DefaultFormatSettings;
    if (Self.DateFormat <> '') then
      FS.ShortDateFormat := Self.DateFormat;
    if not TryStrToDate(AText, Result, FS) then
    begin
      Result := ParseDateNoPredefinedOrder(AText, FS);
      if (Result = NullDate) then Result := ADefault;
    end;
  end
  else
    Result := ParseDate(AText,DateOrder,ADefault)
end;

function TMM_UI_DateEdit.Get_IsValid: Boolean;
var
   txt, nullTxt :String;

begin
     txt :=Trim(Text);

     {$ifdef DebugInfo}
       debugln('TMM_UI_DateEdit.Get_IsValid ('+Self.Name+'):"'+txt+'"');
     {$endif}

     if (Self.DateOrder = doNone)
     then nullTxt :=''
     else nullTxt :=(DefaultFormatSettings.DateSeparator+'  '+DefaultFormatSettings.DateSeparator);

     rIsNull := (txt = nullTxt);
     if rIsNull
     then rIsValid := rNullIsValid
     else rIsValid := not(TextToDate(txt, NullDate) = NullDate);

     //rIsValid :=(rNullIsValid and (txt = nullTxt)) or not((txt <> nullTxt) and (TextToDate(txt, NullDate) = NullDate));

     if not(csDesigning in ComponentState)
     then if rIsValid
          then Color :=EditorDefColor
          else Color :=rInvalidValueColor;

     {$ifdef DebugInfo}
       debugln('TMM_UI_DateEdit.Get_IsValid ('+Self.Name+'):  rIsValid='+BoolToStr(rIsValid, true)+', rIsNull='+BoolToStr(rIsNull, true));
     {$endif}

     Result :=rIsValid;
end;

procedure TMM_UI_DateEdit.EditEditingDone;
var
   txt, nullTxt :String;

begin
  {$ifdef DebugInfo}
    debugln('TMM_UI_DateEdit.EditEditingDone ('+Self.Name+')');
  {$endif}

     inherited EditEditingDone;

     if Get_IsValid
     then begin
               //Work Around for a Bug "Set Date, after Clear it" did not Clear the Field DateTime
               txt :=Trim(Text);

               if (Self.DateOrder = doNone)
               then nullTxt :=''
               else nullTxt :=(DefaultFormatSettings.DateSeparator+'  '+DefaultFormatSettings.DateSeparator);

               if (txt = nullTxt)
               then Self.Date := NullDate;
           end
     else begin
             (*  if (rInvalidValueMessage <> '')
               then Dialogs.MessageDlg(rInvalidValueMessage+#13#10+#13#10+Self.Text, mtError, [mbOk], 0);
               if (rInvalidLockFocus)
               then Self.SetFocus;
               *)
           end;
end;

procedure TMM_UI_DateEdit.EditExit;
begin
  {$ifdef DebugInfo}
    debugln('TMM_UI_DateEdit.EditExit ('+Self.Name+')');
  {$endif}

  if not(csDesigning in ComponentState) and not(Get_IsValid) then
  begin
       if (rInvalidValueMessage <> '')
       then Dialogs.MessageDlg(rInvalidValueMessage+#13#10+#13#10+Self.Text, mtError, [mbOk], 0);
       if (rInvalidLockFocus)
       then Self.SetFocus;
  end;

  inherited EditExit;
end;

function TMM_UI_DateEdit.CalcButtonVisible: Boolean;
begin
     Result := (csDesigning in ComponentState) or
               (rButtonVisible and inherited CalcButtonVisible);
end;

function TMM_UI_DateEdit.MM_UI_HighlighterCanProcess(isActive: Boolean): Boolean;
begin
     Result :=IsValid;
end;

function TMM_UI_DateEdit.MM_UI_HighlighterGetStdColor: TColor;
begin
     Result :=EditorDefColor;
end;

constructor TMM_UI_DateEdit.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);

     rIsValid :=True;
     rInvalidValueColor :=clMyRed;
     rInvalidValueMessage :=''; //cInvalidValueMessage;
     rNullIsValid :=True;
     rIsNull :=True;
     rInvalidLockFocus :=True;
end;

procedure TMM_UI_DateEdit.Loaded;
begin
     inherited Loaded;

     EditorDefColor :=Color;
end;

end.
