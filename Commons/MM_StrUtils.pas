//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2005-2015                                   ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_StrUtils.pas
//
//  Description : Some Useful Strings functions.
//
//******************************************************************************

unit MM_StrUtils;
{$mode objfpc}{$H+}

interface

type
   TStringArray = array of string;

function EncodeControlChars(const s: String): String;
function DecodeControlChars(const s: String): String;

function EncodeControlChars_Editable(const s: String): String;
function DecodeControlChars_Editable(const s: String): String;

procedure GetPascalConst(const Line :String; var ConstName, ConstValue :String);

procedure ExplodeStrArray(const s: String; const Delimiter:Char; var _array: TStringArray);
function ImplodeStrArray(const Delimiter:Char; const _array: TStringArray): String;

procedure VersionStrToInt(const s: String; var Ver, VerSub: Integer);

implementation

uses SysUtils;

function EncodeControlChars(const s: String): String;
var
   i, iLen  :Integer;
   c        :Char;

begin
    Result := '';
    iLen := Length(s);
    i := 1;
    while i<=iLen do begin
      c := s[i];
      case c of
         // Tab character
        #9:  Result := Result+'\t';
         // Linefeed character. Skip nexct Carriage Return char, if any
        #10: begin
          Result := Result+'\r';
          //if (i<iLen) and (s[i+1]=#13) then Inc(i);
        end;
         // Carriage Return character. Skip next Linefeed char, if any
        #13: begin
          Result := Result+'\n';
          //if (i<iLen) and (s[i+1]=#10) then Inc(i);
        end;
         // Backslash. Just duplicate it
        '\': Result := Result+'\\';
         // Equal. Encoded for problems in ini files
        '=': Result := Result+'\e';
         // All control characters having no special names represent as '\00' escape sequence; add directly all others
        else if c<#32 then Result := Result+Format('\%.2d', [Byte(c)]) else Result := Result+c;
      end;
      Inc(i);
    end;
end;

function DecodeControlChars(const s: String): String;
var
   i, iLen: Integer;
   c: Char;
   bEscape: Boolean;

begin
    Result := '';
    iLen := Length(s);
    i := 1;
    while i<=iLen do begin
      c := s[i];
      bEscape := False;
      if (c='\') and (i<iLen) then
        case s[i+1] of
           // An escaped charcode '\00'
          '0'..'9': if (i<iLen-1) and (s[i+2] in ['0'..'9']) then begin
            Result := Result+Char((Byte(s[i+1])-Byte('0'))*10+(Byte(s[i+2])-Byte('0')));
            Inc(i, 2);
            bEscape := True;
          end;
          '\': begin
            Result := Result+'\';
            Inc(i);
            bEscape := True;
          end;
          'r': begin
            Result := Result+#10;
            Inc(i);
            bEscape := True;
          end;
          'n': begin
            Result := Result+#13;
            Inc(i);
            bEscape := True;
          end;
          't': begin
            Result := Result+#9;
            Inc(i);
            bEscape := True;
          end;
          'e': begin
            Result := Result+'=';
            Inc(i);
            bEscape := True;
          end;
        end;
      if not bEscape then Result := Result+c;
      Inc(i);
    end;
end;

function EncodeControlChars_Editable(const s: String): String;
var
   i, iLen  :Integer;
   c        :Char;

begin
    Result := '';
    iLen := Length(s);
    i := 1;
    while i<=iLen do
    begin
      c := s[i];
      Case c of
      '\' :begin
                if not(s[i+1] in ['t', 'n']) and
                   not((s[i+1] in ['0'..'9']) and (i+2<=iLen) and (s[i+2] in ['0'..'9']))
                then begin
                          Result := Result+'\\';
                          inc(i);
                     end
                else Result := Result+c;
           end;
      '=' : begin
                 Result := Result+'\e';
                 inc(i);
            end;
      else Result := Result+c;
      end;
      Inc(i);
    end;
end;

function DecodeControlChars_Editable(const s: String): String;
var
   i, iLen: Integer;
   c: Char;
   bEscape: Boolean;

begin
    Result := '';
    iLen := Length(s);
    i := 1;
    while i<=iLen do
    begin
      c := s[i];
      bEscape := False;
      if (c='\') and
         (i<iLen)
      then case s[i+1] of
           '\': begin
                     Result := Result+'\';
                     Inc(i);
                     bEscape := True;
                end;
           'e': begin
                     Result := Result+'=';
                     Inc(i);
                     bEscape := True;
                end;
           end;
      if not(bEscape)
      then Result := Result+c;
      Inc(i);
    end;
end;


procedure GetPascalConst(const Line :String; var ConstName, ConstValue :String);
const
     Separators =['''', '#', '+', ' ', ';'];

var
   EQPos       :Integer;
   resultValue :String;
   i, iLen     :Integer;
   inQuote     :Boolean;
   next,
   cur          :Char;
   NumVal       :String;

begin
     ConstName  :='';
     ConstValue :='';

     EQPos := Pos('=', Line);
     ConstName :=Copy(Line, 1, EQPos-1);
     ConstName :=Trim(ConstName);
     if (ConstName<>'') then
     begin
        ConstValue :=Trim(Copy(Line, EQPos+1, MaxInt));
        resultValue :='';

        iLen :=Length(ConstValue);
        i:=1;
        inQuote :=False;
        while (i<=iLen) do
        begin
             cur :=ConstValue[i];
             if (cur='''')
             then begin
                       try
                          next :=ConstValue[i+1];
                       except
                          next :=#0;
                       end;
                       inc(i);
                       if (next<>'''')
                       then begin
                                 inQuote :=not(inQuote);
                                 Continue;
                            end;
                  end;
             if not(inQuote)
             then Case cur of
                  '#'  : begin
                              inc(i);
                              NumVal :='';
                              cur :=ConstValue[i];
                              while not(cur in Separators) do
                              begin
                                   NumVal :=NumVal+cur;
                                   inc(i);
                                   cur :=ConstValue[i];
                              end;
                              resultValue :=resultValue + Char(StrToInt(NumVal));
                         end;
                  '+',
                  ' '  : inc(i);
                  ';'  : Break;
                  end
             else begin
                       resultValue :=resultValue + cur;
                       inc(i);
                  end;
        end;
        ConstValue :=resultValue;
     end;
end;

procedure ExplodeStrArray(const s: String; const Delimiter: Char; var _array: TStringArray);
var
   iChar, iStr, lStr: Integer;
   curStr: String;

begin
     lStr :=Length(s);
     SetLength(_array, 0);
     curStr :='';
     iStr :=0;
     lStr :=Length(s);
     for iChar :=1 to lStr do
     begin
       if (s[iChar] = Delimiter) or (iChar = lStr)
       then begin
                 inc(iStr);
                 SetLength(_array, iStr);
                 _array[iStr-1] :=curStr;
                 curStr :='';
             end
       else curStr :=curStr+s[iChar];
     end;
end;

function ImplodeStrArray(const Delimiter: Char; const _array: TStringArray): String;
var
   iStr: Integer;

begin
     Result :='';
     for iStr:=Low(_array) to High(_array) do
     begin
          if (Result <> '')
          then Result :=Result+Delimiter;
          if (_array[iStr] <> '')
          then Result :=Result+_array[iStr];
      end;
end;

procedure VersionStrToInt(const s: String; var Ver, VerSub: Integer);
var
   pPos, ppPos: Integer;

begin
  Ver:= 0;
  VerSub:= 0;

  try
     pPos:= Pos('.', s);
     if (pPos > 0) then
     begin
       Ver:= StrToInt(Copy(s, 0, pPos-1));
       ppPos:= Pos('.', s, pPos+1);
       if (ppPos > 0)
       then VerSub:= StrToInt(Copy(s, pPos+1, ppPos-1))
       else VerSub:= StrToInt(Copy(s, pPos+1, 255));
     end;
  except

  end;
end;

end.
