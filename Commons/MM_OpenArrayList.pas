unit MM_OpenArrayList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TOpenArrayList }

  generic TOpenArrayList<T, K> = class(TNoRefCountObject)
  type
    TInfo = record
      Key: K;
      Data: T;
    end;
   PData = ^T;
  protected
    rList: array of TInfo;

    function Get(const aKey: K) : PData; overload;
    function Get(Index: Integer) : PData; overload;
    function GetKey(Index: Integer) : K;
    function GetCount: Integer;

    function FreeElement(var aData: T): Boolean; virtual;
    function CompData(aData1, aData2: T): Integer; virtual;  //0 = , -1 AData1 < AData2, 1 AData1 > AData2

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const aKey: K; const aData: T): Integer;
    function Del(const aKey: K): Boolean; overload;
    function Del(const aData: T): Boolean; overload;
    function Del(const aIndex: Integer): Boolean; overload;

    function FindByKey(const aKey: K): Integer;
    function Find(const aData: T): Integer;

    property Count: Integer read GetCount;

    property DataByKey [const aKey: K]: PData read Get;
    property Data [const aIndex: Integer]: PData read Get;
    property Key [const aIndex: Integer]: K read GetKey;
  end;



implementation

{ TOpenArrayList }

function TOpenArrayList.Get(const aKey: K): PData;
var
   r : Integer;

begin
  Result:= Nil;

  r:= FindByKey(aKey);
  if (r > -1)
  then Result:= @rList[r].Data;
end;

function TOpenArrayList.Get(Index: Integer): PData;
begin
  if (Index >= 0) and (Index < Length(rList))
  then Result:= @rList[Index].Data
  else Result:= Nil;
end;

function TOpenArrayList.GetKey(Index: Integer): K;
begin
  if (Index >= 0) and (Index < Length(rList))
  then Result:= rList[Index].Key
  else Result:= '';
end;

function TOpenArrayList.GetCount: Integer;
begin
  Result:= Length(rList);
end;

function TOpenArrayList.FreeElement(var aData: T): Boolean;
begin
  Result:= True;
end;

function TOpenArrayList.CompData(aData1, aData2: T): Integer;
begin
  Result:= -1;
end;

constructor TOpenArrayList.Create;
begin
  inherited Create;

  rList:= Nil;
end;

destructor TOpenArrayList.Destroy;
var
   i: Integer;

begin
  for i:=0 to Length(rList)-1 do
  begin
    try
       FreeElement(rList[i].Data);

    except
    end;
  end;

  try
     rList:= Nil;
  except
  end;

  inherited Destroy;
end;

function TOpenArrayList.Add(const aKey: K; const aData: T): Integer;
begin
  Result:= FindByKey(aKey);

  if (Result = -1) then
  begin
    Result:= Length(rList);
    SetLength(rList, Result+1);

    rList[Result].Key:= aKey;
    rList[Result].Data:= aData;
  end;
end;

function TOpenArrayList.Del(const aKey: K): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= FindByKey(aKey);
  if (r > -1) then
  begin
    Result:= FreeElement(rList[r].Data);

    Delete(rList, r, 1);
    Result:= True;
  end;
end;

function TOpenArrayList.Del(const aData: T): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aData);
  if (r > -1) then
  begin
    Result:= FreeElement(rList[r].Data);

    Delete(rList, r, 1);
    Result:= True;
  end;
end;

function TOpenArrayList.Del(const aIndex: Integer): Boolean;
begin
  Result:= False;
  if (aIndex > -1) then
  begin
    Result:= FreeElement(rList[aIndex].Data);

    Delete(rList, aIndex, 1);
    Result:= True;
  end;
end;

function TOpenArrayList.FindByKey(const aKey: K): Integer;
var
  i: Integer;

begin
  Result:= -1;
  for i:=0 to Length(rList)-1 do
    if (rList[i].Key = aKey) then
    begin
      Result:= i; break;
    end;
end;

function TOpenArrayList.Find(const aData: T): Integer;
var
  i: Integer;

begin
  Result:= -1;
  for i:=0 to Length(rList)-1 do
    if (CompData(rList[i].Data, aData) = 0) then
    begin
      Result:= i; break;
    end;
end;

end.

