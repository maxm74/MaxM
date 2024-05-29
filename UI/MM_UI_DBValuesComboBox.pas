unit MM_UI_DBValuesComboBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ColorBox, StdCtrls, LCLStrConsts, LCLType, db;

type
  { TMM_UI_DBValuesComboBox }

  TMM_UI_DBValuesComboBox = class(TDBComboBox)
  private
    procedure SetValues(AValue: TStrings);
  protected
    FSelected: String;
    rValues: TStrings;
    function GetSelected: String;
    procedure SetSelected(Value: String);
    procedure UpdateCombo;

    procedure DataChange(Sender: TObject); override;
    procedure UpdateData(Sender: TObject); override;

    procedure Loaded; override;
    procedure InitializeWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CloseUp; override;

    property ReadOnly;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Values: TStrings read rValues write SetValues;
    property Selected: String read GetSelected write SetSelected;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_DBValuesComboBox]);
end;

constructor TMM_UI_DBValuesComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csDropDownList;

  FSelected := '';
  rValues :=TStringList.Create;
end;

destructor TMM_UI_DBValuesComboBox.Destroy;
begin
  FreeAndNil(rValues);

  inherited Destroy;
end;

procedure TMM_UI_DBValuesComboBox.SetValues(AValue: TStrings);
begin
  if (AValue <> rValues)
  then rValues.Assign(AValue);
end;

function TMM_UI_DBValuesComboBox.GetSelected: String;
begin
  Result := FSelected;
  if HandleAllocated then
  begin
    if (ItemIndex >= 0) and (rValues.Count > ItemIndex) then
    begin
         Result := rValues[ItemIndex];
         // keep FSelected in sync
         if (FSelected <> Result)
         then FSelected := Result;
     end;
  end;
end;

procedure TMM_UI_DBValuesComboBox.SetSelected(Value: String);
begin
  if (FSelected <> Value) then
  begin
       (*FSelected := Value;
       UpdateCombo;
       *)
       //Self.Select;
       Self.Field.DataSet.Edit;
       Self.Field.AsString := Value;
       //inherited Change; //On Windows Infinite Loop
  end;
end;

procedure TMM_UI_DBValuesComboBox.UpdateCombo;
var
  c: integer;
begin
  if HandleAllocated then
  begin
    for c := 0 to Items.Count - 1 do
    begin
      if (rValues.Count > c)
      then begin
                if (rValues[c] = FSelected) then
                begin
                     ItemIndex := c;
                     Exit;
                 end;
            end
      else Exit;
    end;
  end;
end;

procedure TMM_UI_DBValuesComboBox.DataChange(Sender: TObject);
Var
  theField :TField;

begin
  theField :=Self.Field;

  if Assigned(theField) and not(theField.IsNull)
  then Self.FSelected := theField.AsString
  else Self.FSelected := '';

  UpdateCombo;
end;

procedure TMM_UI_DBValuesComboBox.UpdateData(Sender: TObject);
Var
  theField :TField;

begin
  theField :=Self.Field;

  if (Self.Selected = '')
  then theField.Clear
  else theField.AsString := Self.Selected;
end;

procedure TMM_UI_DBValuesComboBox.Loaded;
begin
  inherited Loaded;
end;

procedure TMM_UI_DBValuesComboBox.InitializeWnd;
begin
  inherited InitializeWnd;

  UpdateCombo;
end;

procedure TMM_UI_DBValuesComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  //We Want a Drop Down List but we can not set TCustomComboBox(ReadOnly) :=True;
  Params.Style := Params.Style and not(CBS_DROPDOWN);
  Params.Style := Params.Style or CBS_DROPDOWNLIST;
end;

procedure TMM_UI_DBValuesComboBox.CloseUp;
begin
  if (ItemIndex >= 0) and (rValues.Count > ItemIndex)
  then Selected := rValues[ItemIndex];

  inherited CloseUp;
end;

end.
