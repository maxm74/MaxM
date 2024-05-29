//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2014-2015                                   ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_VariantsStringList.pas
//
//  Description : TStringList descendent that store Variants Values.
//
//******************************************************************************

unit MM_VariantsStringList;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants;

type
    { TVariantsStringList }
    TVariantsStringList =class(TStringList)
    protected
      rSortDescending: Boolean;

      function GetValue(const Index: Integer): Variant;
      procedure SetValue(const Index: Integer; AValue: Variant);
      function DoCompareText(const s1,s2 : string) : PtrInt; override;
      procedure SetSortDescending(AValue: Boolean);

      property Objects;
    public
       constructor Create;
       function Add(const S: String; Value: Variant):Integer; virtual; overload;
       procedure Append(const S: string; Value: Variant); virtual; overload;
       procedure AddStrings(TheStrings: TStrings); overload; override;
       procedure AddStrings(const TheStrings: array of string; TheValues: array of Variant); virtual; overload;
       procedure Insert(Index: Integer; const S: string; Value: Variant); virtual; overload;
       function Find(const Value: Variant; Out Index: Integer): Boolean; virtual; overload;
       function IndexOfValue(AValue: Variant): Integer; virtual;

       property Values[const Index: Integer]: Variant read GetValue write SetValue;
       property SortDescending: Boolean read rSortDescending write SetSortDescending;
    end;

implementation

Type
    { TVariantObject }
    TVariantObject = class(TObject)
      public
        Value :Variant;

        constructor Create(AValue :Variant);
    end;

procedure TVariantsStringList.SetSortDescending(AValue: Boolean);
begin
  if (rSortDescending <> AValue) then
  begin
       rSortDescending :=AValue;

       if Self.Sorted
       then Sort;
  end;
end;

{ TVariantsStringList }
function TVariantsStringList.GetValue(const Index: Integer): Variant;
Var
   valObj :TVariantObject;

begin
     if (Index >= 0) and (Self.Objects[Index] <> nil) then
     begin
          valObj :=TVariantObject(Self.Objects[Index]);
          if (valObj <> nil)
          then Result :=valObj.Value
          else Result :=Variants.Null;
      end
     else Result :=Variants.Null;
end;

procedure TVariantsStringList.SetValue(const Index: Integer; AValue: Variant);
Var
   valObj :TVariantObject;

begin
     if (Index >= 0) then
     begin
          valObj :=TVariantObject(Self.Objects[Index]);
          if (valObj <> nil)
          then valObj.Value :=AValue
          else Self.Objects[Index] :=TVariantObject.Create(AValue);
     end;
end;

constructor TVariantsStringList.Create;
begin
     inherited Create;
     Self.OwnsObjects :=True;
end;

function TVariantsStringList.Add(const S: String; Value: Variant): Integer;
begin
     Result :=Add(S);
     SetValue(Result, Value);
end;

procedure TVariantsStringList.Append(const S: string; Value: Variant);
begin
     Add(S, Value);
end;

procedure TVariantsStringList.AddStrings(TheStrings: TStrings);
Var
  Runner : longint;

begin
  try
    Beginupdate;
    for Runner:=0 to TheStrings.Count-1 do
    begin
         if (TheStrings.Objects[Runner] is TVariantObject)
         then Self.Add(TheStrings[Runner], TVariantObject(TheStrings.Objects[Runner]).Value)
         else Self.Add(TheStrings[Runner]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TVariantsStringList.AddStrings(const TheStrings: array of string; TheValues: array of Variant);
Var
   Runner : longint;

begin
  try
    Beginupdate;
    if Count + High(TheStrings)+1 > Capacity then
      Capacity := Count + High(TheStrings)+1;
    For Runner:=Low(TheStrings) to High(TheStrings) do
      Self.Add(TheStrings[Runner], TheValues[Runner]);
  finally
    EndUpdate;
  end;
end;

procedure TVariantsStringList.Insert(Index: Integer; const S: string; Value: Variant);
begin
     Insert(Index, S);
     SetValue(Index, Value);
end;

function TVariantsStringList.Find(const Value: Variant; Out Index: Integer): Boolean;
begin
     Index :=0;
     While (Index<Count) and (Value<>Self.GetValue(Index)) do inc(Index);
     Result :=(Index < Count);
end;


function TVariantsStringList.IndexOfValue(AValue: Variant): Integer;
begin
     if not(Find(AValue, Result))
     then Result :=-1;
end;

function TVariantsStringList.DoCompareText(const s1,s2 : string) : PtrInt;
begin
     Result :=inherited DoCompareText(s1, s2);

     if rSortDescending
     then Result :=-Result; // ;-)
end;

{ TVariantObject }
constructor TVariantObject.Create(AValue: Variant);
begin
     inherited Create;
     Self.Value :=AValue;
end;



end.

