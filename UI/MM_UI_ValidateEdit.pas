unit MM_UI_ValidateEdit;

interface

uses
  Classes, SysUtils, DbCtrls, LMessages, Graphics, Dialogs, MaskEdit
  {$IFDEF DEBUG_LOG}
  , LCLProc
  {$ENDIF}
  {$ifdef DEBUG_DATEEDIT}, sharedlogger {$endif};

type

  //todo: implement RecoverMode
  //TRecoverMode = (rmNone, rmClear, rmRestore);

  TValidateDataEvent = procedure(Sender: TObject; var NewData: String; var IsValid: Boolean) of object;

  TValueChangeEvent = procedure(Sender: TObject; StateChanged: Boolean) of object;

  TDBValidateEditOption = (voNullValueAsError);

  TDBValidateEditOptions = set of TDBValidateEditOption;

  { TCustomDBValidateEdit }

  TCustomDBValidateEdit = class(TDBEdit)
  protected
    rDataLink: TFieldDataLink;
    rInvalidValueColor: TColor;
    rInvalidValueMessage: String;
    rOptions: TDBValidateEditOptions;
    rValueIsValid: Boolean;

    function Get_ValueIsValid: Boolean;
    procedure ValidateEdit; override;
    function BuildEditMask: String; virtual; abstract;
    function DoValidateData(var NewData: String): Boolean; virtual; abstract;

    property CustomEditMask;
    property EditMask;
    property SpaceChar;
  public
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;

    property ValueIsValid :Boolean read Get_ValueIsValid;
  published
    property InvalidValueColor: TColor read rInvalidValueColor write rInvalidValueColor default clWindow;
    property InvalidValueMessage: String read rInvalidValueMessage write rInvalidValueMessage;
    property Options: TDBValidateEditOptions read rOptions write rOptions default [];
  end;

  { TDBValidateEdit }

  TDBValidateEdit = class(TCustomDBValidateEdit)
  protected
    rOnValidateData: TValidateDataEvent;

    function BuildEditMask: String; override;
    function DoValidateData(var NewData: String): Boolean; override;
  published
    property EditMask;
    property OnValidateData: TValidateDataEvent read rOnValidateData write rOnValidateData;
  end;

  { TDBDateMaskEdit }

  TDBDateMaskEdit = class(TCustomDBValidateEdit)
  protected
    rDateSeparator :Char;

    procedure SetDateSeparator(AValue: Char);
    function BuildEditMask: String; override;
    function DoValidateData(var NewData: String): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DateSeparator :Char read rDateSeparator write SetDateSeparator;
  end;

  { TDateMaskEdit }

  TDateMaskEdit = class(TMaskEdit)
  protected
    rInvalidValueColor: TColor;
    rInvalidValueMessage: String;
    rOnValidateData: TValidateDataEvent;
    rOptions: TDBValidateEditOptions;
    rValueIsValid: Boolean;
    rDateSeparator :Char;

    function Get_AsDate: TDateTime;
    function Get_ValueIsValid: Boolean;
    procedure SetDateSeparator(AValue: Char);
    function BuildEditMask: String; virtual;
    procedure ValidateEdit; override;
    procedure Loaded; override;

    function DoValidateData(var NewData: String): Boolean; virtual;

    property EditMask;
    property SpaceChar;
  public
    constructor Create(AOwner: TComponent); override;

    property AsDate :TDateTime read Get_AsDate;
    property ValueIsValid :Boolean read Get_ValueIsValid;
  published
    property InvalidValueColor: TColor read rInvalidValueColor write rInvalidValueColor default clWindow;
    property InvalidValueMessage: String read rInvalidValueMessage write rInvalidValueMessage;
    property Options: TDBValidateEditOptions read rOptions write rOptions default [];
    property DateSeparator :Char read rDateSeparator write SetDateSeparator;

    //Events
    property OnValidateData: TValidateDataEvent read rOnValidateData write rOnValidateData;
  end;
  
procedure Register;
function _BuildEditMask(ADateSeparator: Char): String;
function _BuildNullMask(ADateSeparator: Char): String;

implementation

uses
  strutils, db;
  
procedure Register;
begin
  RegisterComponents('MaxM_UI',[TDBDateMaskEdit, TDBValidateEdit, TDateMaskEdit]);
end;

function _BuildEditMask(ADateSeparator: Char): String;
var
  S: String;
  i, FieldCount: Integer;

begin
  S := '';
  FieldCount := 0;
  for i := 1 to Length(ShortDateFormat) do
  begin
    if ShortDateFormat[i] in ['M', 'm', 'D', 'd', 'Y', 'y'] then
    begin
      S := S + '9';
      Inc(FieldCount);
    end
    else
    begin
      //add an extra character to avoid date fields with only one character
      if FieldCount = 1 then
        S := S + '9';
      FieldCount := 0;
      S := S + ADateSeparator;
    end;
  end;
  //the last field has only one character
  if FieldCount = 1 then
    S := S + '9';
  Result := S; //'!' + S + ';1;_';
end;

function _BuildNullMask(ADateSeparator: Char): String;
var
  S: String;
  i, FieldCount: Integer;

begin
  S := '';
  FieldCount := 0;
  for i := 1 to Length(ShortDateFormat) do
  begin
    if ShortDateFormat[i] in ['M', 'm', 'D', 'd', 'Y', 'y'] then
    begin
      S := S + ' ';
      Inc(FieldCount);
    end
    else
    begin
      //add an extra character to avoid date fields with only one character
      if FieldCount = 1 then
        S := S + ' ';
      FieldCount := 0;
      S := S + ADateSeparator;
    end;
  end;
  //the last field has only one character
  if FieldCount = 1 then
    S := S + ' ';
  Result := S; //'!' + S + ';1;_';
end;

{ TDateMaskEdit }

function TDateMaskEdit.Get_AsDate: TDateTime;
var
  D: TDateTime;

begin
     if TryStrToDate(Text, D)
     then Result :=D
     else raise Exception.Create('Invalid Date : '+Text);
end;

function TDateMaskEdit.Get_ValueIsValid: Boolean;
begin
     ValidateEdit;
     Result :=rValueIsValid;
end;

procedure TDateMaskEdit.SetDateSeparator(AValue: Char);
begin
  if (rDateSeparator<>AValue) then
  begin
       rDateSeparator:=AValue;
       EditMask :=BuildEditMask;
  end;
end;

function TDateMaskEdit.BuildEditMask: String;
begin
  if not(csDesigning in ComponentState)
  then Result := '!' + _BuildEditMask(rDateSeparator) + ';1;_';
end;


procedure TDateMaskEdit.ValidateEdit;
var
  S: String;
  OldValueIsValid: Boolean;

begin
  inherited ValidateEdit;

  if not(csDesigning in ComponentState) then
  begin
    S := Text;
    OldValueIsValid := rValueIsValid;
    rValueIsValid := DoValidateData(S);

    if rValueIsValid
    then begin
              Color := clWindow;
          end
    else begin
              if (rInvalidValueMessage <> '')
              then ShowMessage(AnsiReplaceText(rInvalidValueMessage, '$(NewValue)', S));
              Color := rInvalidValueColor;
              //Self.SetFocus;
         end;
  end;
end;

procedure TDateMaskEdit.Loaded;
begin
  inherited Loaded;

  EditMask :=BuildEditMask;
end;

function TDateMaskEdit.DoValidateData(var NewData: String): Boolean;
var
  D: TDateTime;

begin
     Result := TryStrToDate(NewData, D);
     if Assigned(rOnValidateData)
     then rOnValidateData(Self, NewData, Result);
end;

constructor TDateMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  rInvalidValueColor := clWindow;
  rDateSeparator :=SysUtils.DateSeparator;
end;

{ TDBDateMaskEdit }

procedure TDBDateMaskEdit.SetDateSeparator(AValue: Char);
begin
  if (rDateSeparator <> AValue)
  then rDateSeparator:=AValue;
end;

function TDBDateMaskEdit.BuildEditMask: String;
begin
  if not(csDesigning in ComponentState)
  then begin
            Result := '!' + _BuildEditMask(rDateSeparator) + ';1;_';
        end;
end;

function TDBDateMaskEdit.DoValidateData(var NewData: String): Boolean;
var
  D: TDateTime;
begin
  Result := TryStrToDate(NewData, D);
end;

constructor TDBDateMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.rDateSeparator :=SysUtils.DateSeparator;
end;

{ TCustomDBValidateEdit }

function TCustomDBValidateEdit.Get_ValueIsValid: Boolean;
begin
     ValidateEdit;
     Result :=rValueIsValid;
end;

procedure TCustomDBValidateEdit.ValidateEdit;
var
  S: String;
  OldValueIsValid: Boolean;

begin
  if not(csDesigning in ComponentState) then
  begin
    S := Text;
    OldValueIsValid := rValueIsValid;
    rValueIsValid := DoValidateData(S);

    if rValueIsValid
    then begin
              Color := clWindow;
          end
    else begin
              if (rInvalidValueMessage <> '')
              then ShowMessage(AnsiReplaceText(rInvalidValueMessage, '$(NewValue)', S));
              Color := rInvalidValueColor;
              //Self.SetFocus;
         end;
  end;
end;

procedure TCustomDBValidateEdit.Loaded;
begin
  inherited Loaded;

  rDataLink := TFieldDataLink(Perform(CM_GETDATALINK, 0, 0));
  CustomEditMask :=True;
  EditMask :=BuildEditMask;
end;

constructor TCustomDBValidateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  rInvalidValueColor := clWindow;
end;

{ TDBValidateEdit }

function TDBValidateEdit.BuildEditMask: String;
begin
     Result := EditMask;
end;

function TDBValidateEdit.DoValidateData(var NewData: String): Boolean;
begin
  Result := True;
  if Assigned(rOnValidateData) then
    rOnValidateData(Self, NewData, Result);
end;

end.

