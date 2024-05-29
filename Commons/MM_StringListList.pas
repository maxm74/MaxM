//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2015                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_StringListList.pas
//
//  Description : TStringList descendent that store TStringList Objects.
//
//******************************************************************************
//
//TODO: store also a TStringListList Objects so i can do an infinite level tree

unit MM_StringListList;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    { TStringListList }
    TStringListList = class(TStringList)
    private
      function GetList(Index: Integer): TStringList;
      procedure SetList(Index: Integer; AValue: TStringList);
    protected
       property Objects;
    public
       constructor Create;
       procedure LoadFromIniFile(const AFileName: string; AEscapeLineFeeds : Boolean = False);
       function FindListContaining(AString: String; out Index, IndexInList: Integer): TStringList;
       function GetValueOf(AName: String; PrependSection: Boolean = False; PrependSeparator: String ='-'): String;

       property Lists[Index: Integer]: TStringList read GetList write SetList;
    end;

implementation

uses MM_IniFiles;

{ TStringListList }

function TStringListList.GetList(Index: Integer): TStringList;
begin
     Result :=TStringList(GetObject(Index));
end;

procedure TStringListList.SetList(Index: Integer; AValue: TStringList);
begin
     PutObject(Index, AValue);
end;

constructor TStringListList.Create;
begin
     inherited Create;
     Self.OwnsObjects :=True;
end;

procedure TStringListList.LoadFromIniFile(const AFileName: string; AEscapeLineFeeds: Boolean);
var
   xIniFile :TMM_IniFile;
   iSection :Integer;
   curList :TStringList;

begin
     try
        xIniFile :=TMM_IniFile.Create(AFileName, AEscapeLineFeeds);
        xIniFile.ReadSections(Self);
        for iSection :=0 to Self.Count-1 do
        begin
             curList :=TStringList.Create;
             xIniFile.ReadSectionValues(Self.Strings[iSection], curList);
             Lists[iSection] :=curList;
        end;
     finally
        xIniFile.Free;
     end;
end;

function TStringListList.FindListContaining(AString: String; out Index, IndexInList: Integer): TStringList;
var
   curList :TStringList;
   iList: Integer;

begin
     Result :=nil;
     iList :=0;
     Index :=-1;
     IndexInList :=-1;
     while (iList < Count) and (Result = nil) do
     begin
          curList :=TStringList(GetObject(iList));
          if (curList <> nil) then
          begin
               IndexInList :=curList.IndexOfName(AString);
               if (IndexInList > -1) then
               begin
                    Result :=curList;
                    Index :=iList;
                end;
          end;

          iList:=iList+1;
      end;
end;

function TStringListList.GetValueOf(AName: String; PrependSection: Boolean; PrependSeparator: String): String;
var
   curList :TStringList;
   Index :Integer;
   IndexInList :Integer;

begin
     Result :='';
     curList :=FindListContaining(AName, Index, IndexInList);
     if (curList <> nil)
     then if (PrependSection)
          then Result :=Self.Strings[Index]+PrependSeparator+curList.Values[AName]
          else Result :=curList.Values[AName];
end;

end.

