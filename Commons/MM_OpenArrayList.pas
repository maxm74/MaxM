unit MM_OpenArrayList;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils {$ifdef fpc}, testutils {$else}, Types, RTLConsts{$endif};

type
   { TOpenArray }
  TOpenArray<T> = class(TNoRefCountObject)
  type
    PData = ^T;

  protected
    rList: array of T;
    rSelectedIndex: Integer;

    function GetSelected: PData; virtual;

    function GetByIndex(const aIndex: DWord) : T; virtual;
    procedure PutByIndex(const aIndex: DWord; aData: T); virtual;

    function FreeElement(var aData: T): Boolean; virtual;
    function CompData(aData1, aData2: T): Integer; virtual;  //0 = , -1 AData1 < AData2, 1 AData1 > AData2

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const aData: T): DWord; overload; virtual; stdcall;
    function Add(const ACount: DWord; const ADataArray: array of T): Boolean; overload; virtual; stdcall;
    function Add(const aIndex: DWord; const aData: T): Boolean; overload; virtual; stdcall;

    function CopyFrom(const ACount: DWord; const ADataArray: array of T): Boolean; virtual;

    function Del(const aData: T): Boolean; overload; virtual; stdcall;
    function Del(const aIndex: DWord): Boolean; overload; virtual; stdcall;

    function Clear: Boolean; overload; virtual; stdcall;
    function Clear(PreserveSelected: Boolean): Boolean; overload; virtual; stdcall;

    function Find(const aData: T): Integer; virtual; stdcall;

    function GetCount: DWord; virtual; stdcall;
    function Get(const aIndex: DWord; out aData: T): Boolean; overload; virtual; stdcall;

    function GetSelectedIndex: Integer; virtual; stdcall;
    procedure SetSelectedIndex(const aIndex: Integer); virtual; stdcall;
    function Select(const aData: T): Integer; overload; virtual; stdcall;

    property Count: DWord read GetCount;

    property Data[const aIndex: DWord]: T read GetByIndex write PutByIndex; default;

    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property Selected: PData read GetSelected;
  end;

  IOpenArrayR<T> = interface
    function Find(const aData: T): Integer; stdcall;

    function GetCount: DWord; stdcall;
    function Get(const AIndex: DWord; out aData: T): Boolean; stdcall;

    function GetSelectedIndex: Integer; stdcall;
    procedure SetSelectedIndex(const aIndex: Integer); stdcall;
    function Select(const aData: T): Integer; overload; stdcall;
  end;

  IOpenArrayW<T> = interface
    function Add(const aData: T): DWord; overload; stdcall;
    function Add(const ACount: DWord; const ADataArray: array of T): Boolean; overload; stdcall;
    function Add(const aIndex: DWord; const aData: T): Boolean; overload; stdcall;

    function CopyFrom(const ACount: DWord; const ADataArray: array of T): Boolean; stdcall;

    function Del(const aData: T): Boolean; overload; stdcall;
    function Del(const aIndex: DWord): Boolean; overload; stdcall;

    function Clear: Boolean; overload; stdcall;
    function Clear(PreserveSelected: Boolean): Boolean; overload; stdcall;
  end;

  TOpenArrayString = class(TOpenArray<String>);
  IOpenArrayStringR = interface(IOpenArrayR<String>) end;
  IOpenArrayStringW = interface(IOpenArrayW<String>) end;

  { TOpenArrayList }
  TOpenArrayList<T, K> = class(TNoRefCountObject)
  type
    TInfo = record
      Key: K;
      Data: T;
    end;
   PData = ^T;

  protected
    rList: array of TInfo;
    rSelectedIndex: Integer;

    function GetSelected: PData; virtual;

    function Get(const aKey: K) : PData; overload; virtual;
    function GetByIndex(const Index: DWord) : PData; virtual;
    function GetKey(const Index: DWord) : K; virtual;

    function FreeElement(var aData: T): Boolean; virtual;
    function CompData(aData1, aData2: T): Integer; virtual;  //0 = , -1 AData1 < AData2, 1 AData1 > AData2

  public
    constructor Create;
    destructor Destroy; override;

    function Add(const aKey: K; const aData: T): Integer; overload; virtual; stdcall;
    function Add(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean; overload; virtual; stdcall;

    function CopyFrom(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean; virtual; stdcall;

    function Del(const aKey: K): Boolean; overload; virtual; stdcall;
    function Del(const aData: T): Boolean; overload; virtual; stdcall;
    function Del(const aIndex: DWord): Boolean; overload; virtual; stdcall;

    function Clear: Boolean; overload; virtual; stdcall;
    function Clear(PreserveSelected: Boolean): Boolean; overload; virtual; stdcall;

    function FindByKey(const aKey: K): Integer; virtual; stdcall;
    function Find(const aData: T): Integer; virtual; stdcall;

    function GetCount: DWord; virtual; stdcall;
    function Get(const aIndex: DWord; out aData: T): Boolean; overload; virtual; stdcall;
    function Get(const aIndex: DWord; out aData: T; out aKey: K): Boolean; overload; virtual; stdcall;
    function Get(const aKey: K; out aData: T): Boolean; overload; virtual; stdcall;

    function GetSelectedIndex: Integer; virtual; stdcall;
    procedure SetSelectedIndex(const aIndex: Integer); virtual; stdcall;

    function Select(const aKey: K): Integer; overload; virtual; stdcall;
    function Select(const aData: T): Integer; overload; virtual; stdcall;

    property Count: DWord read GetCount;

    property DataByKey[const aKey: K]: PData read Get;
    property Data[const aIndex: DWord]: PData read GetByIndex; default;
    property Key[const aIndex: DWord]: K read GetKey;

    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property Selected: PData read GetSelected;
  end;

  IOpenArrayListR<T, K> = interface
    function FindByKey(const aKey: K): Integer; stdcall;
    function Find(const aData: T): Integer; stdcall;

    function GetCount: DWord; stdcall;
    function Get(const AIndex: DWord; out aData: T): Boolean; overload; stdcall;
    function Get(const aIndex: DWord; out aData: T; out aKey: K): Boolean; overload; stdcall;
    function Get(const aKey: K; out aData: T): Boolean; overload; stdcall;

    function GetSelectedIndex: Integer; stdcall;
    procedure SetSelectedIndex(const aIndex: Integer); stdcall;

    function Select(const aKey: K): Integer; overload; stdcall;
    function Select(const aData: T): Integer; overload; stdcall;
  end;

  IOpenArrayListW<T, K> = interface
    function Add(const aKey: K; const aData: T): Integer; overload; stdcall;
    function Add(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean; overload; stdcall;

    function CopyFrom(const ACount: DWord; const AKeyArray: array of K; const ADataArray: array of T): Boolean; stdcall;

    function Del(const aKey: K): Boolean; overload; stdcall;
    function Del(const aData: T): Boolean; overload; stdcall;
    function Del(const aIndex: DWord): Boolean; overload; stdcall;

    function Clear: Boolean; overload; stdcall;
    function Clear(PreserveSelected: Boolean): Boolean; overload; stdcall;
  end;

  TKeyString = type String;
  TOpenArrayListString = class(TOpenArrayList<String, TKeyString>);

implementation

uses SysConst;

{ TOpenArray }

function TOpenArray<T>.GetByIndex(const aIndex: DWord): T;
begin
  if (aIndex < Length(rList))
  then Result:= rList[aIndex]
  else raise EListError.Create(Format(SListIndexError, [aIndex]));
end;

procedure TOpenArray<T>.PutByIndex(const aIndex: DWord; aData: T);
begin
  if (aIndex < Length(rList))
  then rList[aIndex]:= aData
  else raise EListError.Create(Format(SListIndexError, [aIndex]));
end;

function TOpenArray<T>.Get(const aIndex: DWord; out aData: T): Boolean; stdcall;
begin
  aData:= Default(T);
  try
     aData:= GetByIndex(aIndex);
     Result:=True;
  except
    Result:= False;
  end;
end;

function TOpenArray<T>.GetSelected: PData;
begin
  if (rSelectedIndex >= 0) and (rSelectedIndex < Length(rList))
  then Result:= @rList[rSelectedIndex] //if T was a pointer or a class it could return the pointer itself or the class, is there a way?
  else Result:= nil;
end;

function TOpenArray<T>.GetSelectedIndex: Integer; stdcall;
begin
  Result:= rSelectedIndex;
end;

procedure TOpenArray<T>.SetSelectedIndex(const aIndex: Integer); stdcall;
begin
  if (aIndex <> rSelectedIndex) then
    if (aIndex >= 0) and (aIndex < Length(rList))
    then rSelectedIndex:= aIndex
    else rSelectedIndex:= -1
end;

function TOpenArray<T>.Select(const aData: T): Integer; stdcall;
var
   iSelect: Integer;

begin
  Result:= -1;
  iSelect:= Find(aData);
  if (iSelect > -1) then
  begin
    SelectedIndex:= iSelect;
    Result:= SelectedIndex;
  end;
  //else leave the world as it is
end;

function TOpenArray<T>.Add(const aIndex: DWord; const aData: T): Boolean;
  stdcall;
begin
  try
     PutByIndex(aIndex, aData);
     Result:=True;
  except
    Result:= False;
  end;
end;

function TOpenArray<T>.GetCount: DWord; stdcall;
begin
  Result:= Length(rList);
end;

function TOpenArray<T>.FreeElement(var aData: T): Boolean;
begin
  Result:= True;
end;

function TOpenArray<T>.CompData(aData1, aData2: T): Integer;
begin
  if (aData1 = aData2)
  then Result:= 0
  else Result:= -1;
end;

constructor TOpenArray<T>.Create;
begin
  inherited Create;

  rList:= Nil;
  rSelectedIndex:= -1;
end;

destructor TOpenArray<T>.Destroy;
begin
  Clear;

  inherited Destroy;
end;

function TOpenArray<T>.Add(const aData: T): DWord; stdcall;
begin
  Result:= Length(rList);
  SetLength(rList, Result+1);

  rList[Result]:= aData;
end;

function TOpenArray<T>.Add(const ACount: DWord; const ADataArray: array of T): Boolean; stdcall;
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

function TOpenArray<T>.CopyFrom(const ACount: DWord; const ADataArray: array of T): Boolean;
begin
  Result:= Clear and Add(ACount, ADataArray);
end;

function TOpenArray<T>.Del(const aData: T): Boolean; stdcall;
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

function TOpenArray<T>.Del(const aIndex: DWord): Boolean; stdcall;
begin
  Result:= False;
  if (aIndex < Length(rList)) then
  begin
    Result:= FreeElement(rList[aIndex]);

    Delete(rList, aIndex, 1);
    Result:= True;
  end;
end;

function TOpenArray<T>.Clear: Boolean; stdcall;
var
   i: Integer;

begin
  Result:= True;

  rSelectedIndex:= -1;

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

function TOpenArray<T>.Clear(PreserveSelected: Boolean): Boolean; stdcall;
var
   i: Integer;

begin
  Result:= True;

  rSelectedIndex:= -1;

  for i:=0 to Length(rList)-1 do
  begin
    if not(PreserveSelected and (i = rSelectedIndex)) then
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

function TOpenArray<T>.Find(const aData: T): Integer; stdcall;
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

function TOpenArrayList<T, K>.Get(const aKey: K): PData;
var
   r : Integer;

begin
  Result:= Nil;

  r:= FindByKey(aKey);
  if (r > -1)
  then Result:= @rList[r].Data;
end;

function TOpenArrayList<T, K>.GetByIndex(const Index: DWord): PData;
begin
  if (Index < Length(rList))
  then Result:= @rList[Index].Data
  else raise EListError.Create(Format(SListIndexError, [Index]));
end;

function TOpenArrayList<T, K>.Get(const aIndex: DWord; out aData: T): Boolean; stdcall;
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

function TOpenArrayList<T, K>.Get(const aIndex: DWord; out aData: T; out aKey: K): Boolean; stdcall;
begin
  aData:= Default(T);
  aKey:= Default(K);

  Result:= (aIndex < Length(rList));

  if Result then
  begin
    aData:= rList[aIndex].Data;
    aKey:= rList[aIndex].Key;
  end;
end;

function TOpenArrayList<T, K>.GetKey(const Index: DWord): K;
begin
  if (Index < Length(rList))
  then Result:= rList[Index].Key
  else Result:= Default(K);
end;

function TOpenArrayList<T, K>.Get(const aKey: K; out aData: T): Boolean;
  stdcall;
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

function TOpenArrayList<T, K>.GetSelected: PData;
begin
  if (rSelectedIndex >= 0) and (rSelectedIndex < Length(rList))
  then Result:= @rList[rSelectedIndex].Data //if T was a pointer or a class it could return the pointer itself or the class, is there a way?
  else Result:= nil;
end;

function TOpenArrayList<T, K>.GetSelectedIndex: Integer; stdcall;
begin
  Result:= rSelectedIndex;
end;

procedure TOpenArrayList<T, K>.SetSelectedIndex(const aIndex: Integer); stdcall;
begin
  if (aIndex <> rSelectedIndex) then
    if (aIndex >= 0) and (aIndex < Length(rList))
    then rSelectedIndex:= aIndex
    else rSelectedIndex:= -1
end;

function TOpenArrayList<T, K>.Select(const aKey: K): Integer; stdcall;
var
   iSelect: Integer;

begin
  Result:= -1;
  iSelect:= FindByKey(aKey);
  if (iSelect > -1) then
  begin
    SelectedIndex:= iSelect;
    Result:= SelectedIndex;
  end;
  //else leave the world as it is
end;

function TOpenArrayList<T, K>.Select(const aData: T): Integer; stdcall;
var
   iSelect: Integer;

begin
  Result:= -1;
  iSelect:= Find(aData);
  if (iSelect > -1) then
  begin
    SelectedIndex:= iSelect;
    Result:= SelectedIndex;
  end;
  //else leave the world as it is
end;

function TOpenArrayList<T, K>.GetCount: DWord; stdcall;
begin
  Result:= Length(rList);
end;

function TOpenArrayList<T, K>.FreeElement(var aData: T): Boolean;
begin
  Result:= True;
end;

function TOpenArrayList<T, K>.CompData(aData1, aData2: T): Integer;
begin
  if (aData1 = aData2)
  then Result:= 0
  else Result:= -1;
end;

constructor TOpenArrayList<T, K>.Create;
begin
  inherited Create;

  rList:= Nil;
  rSelectedIndex:= -1;
end;

destructor TOpenArrayList<T, K>.Destroy;
begin
  Clear;

  inherited Destroy;
end;

function TOpenArrayList<T, K>.Add(const aKey: K; const aData: T): Integer;
  stdcall;
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

function TOpenArrayList<T, K>.Add(const ACount: DWord;
  const AKeyArray: array of K; const ADataArray: array of T): Boolean; stdcall;
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

function TOpenArrayList<T, K>.CopyFrom(const ACount: DWord;
  const AKeyArray: array of K; const ADataArray: array of T): Boolean; stdcall;
begin
  Result:= Clear and Add(ACount, AKeyArray, ADataArray);
end;

function TOpenArrayList<T, K>.Del(const aKey: K): Boolean; stdcall;
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

function TOpenArrayList<T, K>.Del(const aData: T): Boolean; stdcall;
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

function TOpenArrayList<T, K>.Del(const aIndex: DWord): Boolean; stdcall;
begin
  Result:= False;
  if (aIndex < Length(rList)) then
  begin
    Result:= FreeElement(rList[aIndex].Data);

    Delete(rList, aIndex, 1);
    Result:= True;
  end;
end;

function TOpenArrayList<T, K>.Clear: Boolean; stdcall;
var
   i: Integer;

begin
  Result:= True;

  rSelectedIndex:= -1;

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

function TOpenArrayList<T, K>.Clear(PreserveSelected: Boolean): Boolean;
  stdcall;
var
   i: Integer;

begin
  Result:= True;

  rSelectedIndex:= -1;

  for i:=0 to Length(rList)-1 do
  begin
    if not(PreserveSelected and (i = rSelectedIndex)) then
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

function TOpenArrayList<T, K>.FindByKey(const aKey: K): Integer; stdcall;
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

function TOpenArrayList<T, K>.Find(const aData: T): Integer; stdcall;
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

