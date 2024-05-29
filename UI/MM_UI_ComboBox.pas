unit MM_UI_ComboBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Variants;

type

  { TMM_UI_ComboBox }

  TMM_UI_ComboBox = class(TComboBox)
  private
    procedure SetSelectedValue(AValue: Variant);
  protected
    function GetSelectedValue: Variant;
    function GetValue(const Index: Integer): Variant;
    procedure SetValue(const Index: Integer; AValue: Variant);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function AddItem(const Item: String; ItemValue: Variant):Integer; virtual; overload;
    procedure Clear; override;

    property Values[const Index: Integer]: Variant read GetValue write SetValue;
    property SelectedValue :Variant read GetSelectedValue write SetSelectedValue;
  end;

procedure Register;

implementation

Type
    { TVariantObject }
    TVariantObject = class(TObject)
      public
        Value :Variant;

        constructor Create(AValue :Variant);
    end;


procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_ComboBox]);
end;

{ TVariantObject }

constructor TVariantObject.Create(AValue: Variant);
begin
     inherited Create;
     Self.Value :=AValue;
end;

{ TMM_UI_ComboBox }

procedure TMM_UI_ComboBox.SetSelectedValue(AValue: Variant);
var
   i :Integer;

begin
     for i:=0 to Items.Count-1 do
     begin
          if (GetValue(i) = AValue) then
          begin
               ItemIndex :=i;
               Break;
           end;
      end;
end;

function TMM_UI_ComboBox.GetSelectedValue: Variant;
begin
     Result :=GetValue(ItemIndex);
end;

function TMM_UI_ComboBox.GetValue(const Index: Integer): Variant;
Var
   valObj :TVariantObject;

begin
     if (Index >= 0) and (Self.Items.Objects[Index] <> nil) then
     begin
          valObj :=TVariantObject(Self.Items.Objects[Index]);
          if (valObj <> nil)
          then Result :=valObj.Value
          else Result :=Variants.Null;
      end
     else Result :=Variants.Null;
end;

procedure TMM_UI_ComboBox.SetValue(const Index: Integer; AValue: Variant);
Var
   valObj :TVariantObject;

begin
     valObj :=TVariantObject(Self.Items.Objects[Index]);
     if (valObj <> nil)
     then valObj.Value :=AValue
     else Self.Items.Objects[Index] :=TVariantObject.Create(AValue);
end;

constructor TMM_UI_ComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  //Maybe True but Lost Data of Items in InitializeWnd (FItems.Free)....
  TStringList(Self.Items).OwnsObjects :=False;
end;

destructor TMM_UI_ComboBox.Destroy;
begin
  inherited Destroy;
end;

function TMM_UI_ComboBox.AddItem(const Item: String; ItemValue: Variant):Integer;
begin
     Result :=Items.AddObject(Item, TVariantObject.Create(ItemValue));
end;

procedure TMM_UI_ComboBox.Clear;
begin
     //TStringList(Self.Items).OwnsObjects :=True;
     inherited Clear;
     //TStringList(Self.Items).OwnsObjects :=False;
end;

end.
