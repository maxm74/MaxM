unit MM_OpenArrayList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testutils;

type
   { TOpenArray }
  generic TOpenArray<T> = class(TNoRefCountObject)
  protected
    rList: array of T;

    function GetByIndex(aIndex: DWord) : T; virtual;
    procedure PutByIndex(const aIndex: DWord; aData: T); virtual;

    function FreeElement(var aData: T): Boolean; virtual;
    function CompData(aData1, aData2: T): Integer; virtual;  //0 = , -1 AData1 < AData2, 1 AData1 > AData2

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const aData: T): DWord; virtual; overload;
    function Add(const ACount: DWord; const ADataArray: array of T): Boolean; virtual; overload;

    function CopyFrom(const ACount: DWord; const ADataArray: array of T): Boolean; virtual;

    function Del(const aData: T): Boolean; virtual; overload;
    function Del(const aIndex: DWord): Boolean; virtual; overload;

    function Clear: Boolean; virtual;

    function Find(const aData: T): Integer; virtual;

    function GetCount: DWord; virtual; stdcall;
    function Get(const aIndex: DWord; out aData: T): Boolean; virtual; overload;
    function Put(const aIndex: DWord; const aData: T): Boolean; virtual; overload;

    property Count: DWord read GetCount;

    property Data[const aIndex: DWord]: T read GetByIndex write PutByIndex; default;
  end;

  generic IOpenArrayR<T> = interface
    function GetCount: DWord; stdcall;
    function Get(const AIndex: DWord; out aData: T): Boolean; stdcall;
  end;

  generic IOpenArrayW<T> = interface
    function Add(const aData: T): DWord;  stdcall;
    function Put(const AIndex: DWord; var aData: T): Boolean; stdcall;
    function CopyFrom(const ACount: DWord; const AArray: array of T): Boolean; stdcall;
    function Del(const aData: T): Boolean; stdcall;
  end;

  TOpenArrayString = class(specialize TOpenArray<String>);
  IOpenArrayStringR = interface(specialize IOpenArrayR<String>) end;
  IOpenArrayStringW = interface(specialize IOpenArrayW<String>) end;

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

    function Get(const aKey: K) : PData; virtual; overload;
    function GetByIndex(Index: DWord) : PData; virtual;
    function GetKey(Index: DWord) : K; virtual;

    function FreeElement(var aData: T): Boolean; virtual;
    function CompData(aData1, aData2: T): Integer; virtual;  //0 = , -1 AData1 < AData2, 1 AData1 > AData2

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const aKey: K; const aData: T): Integer; virtual; overload;
    function Add(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean; virtual; overload;

    function CopyFrom(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean; virtual;

    function Del(const aKey: K): Boolean; virtual; overload;
    function Del(const aData: T): Boolean; virtual; overload;
    function Del(const aIndex: DWord): Boolean; virtual; overload;

    function Clear: Boolean; virtual;

    function FindByKey(const aKey: K): Integer; virtual;
    function Find(const aData: T): Integer; virtual;

    function GetCount: DWord; virtual; stdcall;
    function Get(const aIndex: DWord; out aData: T): Boolean; virtual; overload;
    function GetByKey(const aKey: K; out aData: T): Boolean; virtual;

    property Count: DWord read GetCount;

    property DataByKey[const aKey: K]: PData read Get;
    property Data[const aIndex: DWord]: PData read GetByIndex; default;
    property Key[const aIndex: DWord]: K read GetKey;
  end;

  generic IOpenArrayListR<T, K> = interface
    function GetCount: DWord; stdcall;
    function Get(const AIndex: DWord; out aData: T): Boolean; stdcall;
    function GetByKey(const aKey: K; out aData: T): Boolean; stdcall;
  end;

  generic IOpenArrayListW<T, K> = interface
    function Put(const AIndex: DWord; var aData: T): Boolean; stdcall;
    function PutByKey(const aKey: K; var aData: T): Boolean; stdcall;
    function CopyFrom(const ACount: DWord; const AArray: array of T): Boolean; stdcall;
  end;

  TKeyString = type String;
  TOpenArrayListString = class(specialize TOpenArrayList<String, TKeyString>);

implementation

uses SysConst;

{ TOpenArray }

function TOpenArray.GetByIndex(aIndex: DWord): T;
begin
  if (aIndex < Length(rList))
  then Result:= rList[aIndex]
  else raise EListError.Create(Format(SListIndexError, [aIndex]));
end;

procedure TOpenArray.PutByIndex(const aIndex: DWord; aData: T);
begin
  if (aIndex < Length(rList))
  then rList[aIndex]:= aData
  else raise EListError.Create(Format(SListIndexError, [aIndex]));
end;

function TOpenArray.Get(const aIndex: DWord; out aData: T): Boolean;
begin
  aData:= Default(T);
  try
     aData:= GetByIndex(aIndex);
     Result:=True;
  except
    Result:= False;
  end;
end;

function TOpenArray.Put(const aIndex: DWord; const aData: T): Boolean;
begin
  try
     PutByIndex(aIndex, aData);
     Result:=True;
  except
    Result:= False;
  end;
end;

function TOpenArray.GetCount: DWord; stdcall;
begin
  Result:= Length(rList);
end;

function TOpenArray.FreeElement(var aData: T): Boolean;
begin
  Result:= True;
end;

function TOpenArray.CompData(aData1, aData2: T): Integer;
begin
  Result:= -1;
end;

constructor TOpenArray.Create;
begin
  inherited Create;

  rList:= Nil;
end;

destructor TOpenArray.Destroy;
begin
  Clear;

  inherited Destroy;
end;

function TOpenArray.Add(const aData: T): DWord;
begin
  Result:= Length(rList);
  SetLength(rList, Result+1);

  rList[Result]:= aData;
end;

function TOpenArray.Add(const ACount: DWord; const ADataArray: array of T): Boolean;
var
   i: Integer;

begin
  Result:= True;

  for i:=Low(ADataArray) to High(ADataArray) do
  try
     Add(ADataArray[i]);
  except
      Result:= False;
      break;
  end;
end;

function TOpenArray.CopyFrom(const ACount: DWord; const ADataArray: array of T): Boolean;
begin
  Result:= Clear and Add(ACount, ADataArray);
end;

function TOpenArray.Del(const aData: T): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aData);
  if (r > -1) then
  begin
    Result:= FreeElement(rList[r]);

    Delete(rList, r, 1);
    Result:= True;
  end;
end;

function TOpenArray.Del(const aIndex: DWord): Boolean;
begin
  Result:= False;
  if (aIndex < Length(rList)) then
  begin
    Result:= FreeElement(rList[aIndex]);

    Delete(rList, aIndex, 1);
    Result:= True;
  end;
end;

function TOpenArray.Clear: Boolean;
var
   i: Integer;

begin
  Result:= True;

  for i:=0 to Length(rList)-1 do
  begin
    try
       FreeElement(rList[i]);

    except
      Result:= False;
    end;
  end;

  try
     rList:= Nil;
  except
    Result:= False;
  end;
end;

function TOpenArray.Find(const aData: T): Integer;
var
  i: Integer;

begin
  Result:= -1;
  for i:=0 to Length(rList)-1 do
    if (CompData(rList[i], aData) = 0) then
    begin
      Result:= i; break;
    end;
end;

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

function TOpenArrayList.GetByIndex(Index: DWord): PData;
begin
  if (Index < Length(rList))
  then Result:= @rList[Index].Data
  else raise EListError.Create(Format(SListIndexError, [Index]));
end;

function TOpenArrayList.Get(const aIndex: DWord; out aData: T): Boolean;
var
   resData: PData;

begin
  aData:= Default(T);
  try
     resData:= GetByIndex(aIndex);
     Result:= (resData<>nil);
     if Result then aData:= resData^;

  except
    Result:= False;
  end;
end;

function TOpenArrayList.GetKey(Index: DWord): K;
begin
  if (Index < Length(rList))
  then Result:= rList[Index].Key
  else Result:= Default(K);
end;

function TOpenArrayList.GetByKey(const aKey: K; out aData: T): Boolean;
var
   resData: PData;

begin
  aData:= Default(T);
  try
     resData:= Get(aKey);
     Result:= (resData<>nil);
     if Result then aData:= resData^;

  except
    Result:= False;
  end;
end;

function TOpenArrayList.GetCount: DWord; stdcall;
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
begin
  Clear;

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

function TOpenArrayList.Add(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean;
var
   i: Integer;

begin
  Result:= True;

  for i:=Low(AKeyArray) to High(AKeyArray) do
  try
     Add(AKeyArray[i], ADataArray[i]);
  except
      Result:= False;
      break;
  end;
end;

function TOpenArrayList.CopyFrom(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean;
begin
  Result:= Clear and Add(ACount, AKeyArray, ADataArray);
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

function TOpenArrayList.Del(const aIndex: DWord): Boolean;
begin
  Result:= False;
  if (aIndex < Length(rList)) then
  begin
    Result:= FreeElement(rList[aIndex].Data);

    Delete(rList, aIndex, 1);
    Result:= True;
  end;
end;

function TOpenArrayList.Clear: Boolean;
var
   i: Integer;

begin
  Result:= True;

  for i:=0 to Length(rList)-1 do
  begin
    try
       FreeElement(rList[i].Data);

    except
      Result:= False;
    end;
  end;

  try
     rList:= Nil;
  except
    Result:= False;
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

