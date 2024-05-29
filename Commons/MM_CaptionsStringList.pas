//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2005                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_CaptionsStringList.pas
//
//  Description : 
//
//******************************************************************************
unit MM_CaptionsStringList;
{$mode objfpc}
{$H+}

interface
uses SysUtils, Classes, MM_IniFiles;

type
    TIndexCaption =record
                         Caption,
                         Name      :String;
                   end;
    PIndexCaption =^TIndexCaption;

    { TCaptionsStringList }

    TCaptionsStringList = class(TStringList)
    public
       function SearchByCaption(Caption :String) :TIndexCaption; virtual;
       function SearchByName(Name :String) :TIndexCaption; virtual;
       procedure LoadFromINI(Section, FileName: String); virtual;
    end;

implementation

function TCaptionsStringList.SearchByCaption(Caption :String) :TIndexCaption;
Var
   index      :Integer;
   xUpCaption :String;
   searching  :String;

begin
     Result.Caption :=Caption;
     Result.Name :='';
     if (Self.Count>0) then
     begin
          xUpCaption :=UpperCase(Caption);

          for index:=0 to (Self.Count-1) do
          begin
               searching :=Self.Values[Self.Names[index]];
               if (UpperCase(searching)=xUpCaption)
               then begin
                         Result.Name :=searching;
                         Break;
                     end;
           end;
      end;
end;

function TCaptionsStringList.SearchByName(Name :String) :TIndexCaption;
begin
     Result.Name :=Name;
     if (Self.Count>0) then
     begin
          Result.Caption :=Self.Values[Name];

          if (Result.Caption='')
          then Result.Caption :=Name;
      end
     else Result.Caption :=Name;
end;

procedure TCaptionsStringList.LoadFromINI(Section, FileName: String);
var
   theINI :TMM_IniFile;

begin
  try
     if FileExists(FileName) then
     begin
          theINI :=TMM_IniFile.Create(FileName);
          if theINI.SectionExists(Section) then
          begin
               theINI.ReadSectionValues(Section, Self);
           end;
          theINI.Free;
     end;
  except
     Clear;
  end;
end;



end.
 
