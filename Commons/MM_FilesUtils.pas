//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2015                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_FilesUtils.pas
//
//  Description : Some Files Useful Functions.
//
//******************************************************************************

unit MM_FilesUtils;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    // TODO: flsSortDateTime
    TFilesListSortType = (flsSortNone, flsSortNumeric, flsSortName);

function GetFilesCount(ADir: String; AExt: String) :Longint;
function GetFilesInDir(ADir: String; AExt: String;
                       SortType: TFilesListSortType = flsSortName;
                       Duplicates: TDuplicates = dupIgnore; CaseSensitive: Boolean = True): TStringList;

//Folders Routines
procedure CopyFileOnPath(SourcePath, DestPath, AWildList:String; ARecursive :Boolean = True);
procedure DeleteFileOnPath(BasePath, AWildList:String; ARecursive :Boolean = True);

implementation
uses LazFileUtils, FileUtil, Masks;

type
    TLongintStringList = class(TStringList)
    protected
      function DoCompareText(const s1,s2 : string) : PtrInt; override;
    end;

function TLongintStringList.DoCompareText(const s1, s2: string): PtrInt;
var
i1, i2 :LongInt;

begin
 try
    i1 :=StrToInt(Copy(s1, 1, Pos(ExtensionSeparator, s1)-1));
    i2 :=StrToInt(Copy(s2, 1, Pos(ExtensionSeparator, s2)-1));
    if (i1 = i2)
    then Result :=0
    else if (i1 > i2)
         then Result :=1
         else Result :=-1;
 except
    Result:=inherited DoCompareText(s1, s2);
 end;
end;


function GetFilesCount(ADir: String; AExt: String): Longint;
var
  err: Integer;
  fileInfo: TSearchRec;

begin
  Result :=0;
  if not(ADir[Length(ADir)] in AllowDirectorySeparators)
  then ADir :=ADir+DirectorySeparator;

  try
     err :=FindFirst(ADir+'*'+ExtensionSeparator+AExt, faAnyFile, fileInfo);
     while (err = 0) do
     begin
          if (fileInfo.Name[1] <> '.')
          then  inc(Result);

          err :=FindNext(fileInfo);
      end;

  finally
     FindClose(fileInfo);
  end;
end;

function GetFilesInDir(ADir: String; AExt: String;
                       SortType: TFilesListSortType;
                       Duplicates: TDuplicates; CaseSensitive: Boolean): TStringList;
var
  err: Integer;
  fileInfo: TSearchRec;

begin
  Result :=nil;
  if not(ADir[Length(ADir)] in AllowDirectorySeparators)
  then ADir :=ADir+DirectorySeparator;

  try
     Case SortType of
     flsSortNone: begin
                       Result :=TStringList.Create;
                       Result.Sorted :=False;
                   end;
     flsSortNumeric: begin
                          Result :=TLongintStringList.Create;
                          Result.Sorted :=True;
                      end;
     flsSortName: begin
                       Result :=TStringList.Create;
                       Result.Sorted :=True;
                   end;
     end;
     Result.CaseSensitive :=CaseSensitive;
     Result.Duplicates :=Duplicates;

     err :=FindFirst(ADir+'*'+ExtensionSeparator+AExt, faAnyFile, fileInfo);
     while (err = 0) do
     begin
          if (fileInfo.Name[1] <> '.')
          then Result.Add(Fileinfo.Name);

          err :=FindNext(fileInfo);
      end;

  finally
     FindClose(fileInfo);
  end;
end;

procedure CopyFileOnPath(SourcePath, DestPath, AWildList :String; ARecursive :Boolean = True);
var
   fileInfo   :TSearchRec;
   err        :Integer;
   CanCopy,
   IsDir      :Boolean;

begin
   err :=FindFirst(SourcePath+DirectorySeparator+'*', faAnyFile, fileInfo);
   while (err=0) do
   begin
        if (fileInfo.Name[1] <> '.') then  //non è [.] o [..]
        begin
             IsDir  :=((fileInfo.Attr and faDirectory)<>0);

             //CanCopy :=MatchesMaskList(fileInfo.Name, AWildList);  testare il codice condizionale sotto
             {$IFDEF Windows}
             CanCopy :=MatchesWindowsMaskList(fileInfo.Name, AWildList);
             {$ELSE}
             CanCopy :=MatchesMaskList(fileInfo.Name, AWildList);
             {$ENDIF}

             if IsDir
             then begin
                    if ARecursive
                    then CopyFileOnPath(SourcePath+DirectorySeparator+fileInfo.Name,
                                        DestPath+DirectorySeparator+fileInfo.Name,
                                        AWildList, True);
                  end
             else if CanCopy
                  then FileUtil.CopyFile(SourcePath+DirectorySeparator+fileInfo.Name,
                                         DestPath+DirectorySeparator+fileInfo.Name,
                                         [cffOverwriteFile, cffCreateDestDirectory]);
        end;
        err :=FindNext(fileInfo);
   end;
   FindClose(fileInfo);
end;

procedure DeleteFileOnPath(BasePath, AWildList: String; ARecursive: Boolean = True);
var
   fileInfo   :TSearchRec;
   err        :Integer;
   CanDel,
   IsDir      :Boolean;

begin
   err :=FindFirst(BasePath+DirectorySeparator+'*', faAnyFile, fileInfo);
   while (err=0) do
   begin
        if (fileInfo.Name[1] <> '.') then  //non è [.] o [..]
        begin
             IsDir  :=((fileInfo.Attr and faDirectory)<>0);
             CanDel :=MatchesMaskList(fileInfo.Name, AWildList);
             if IsDir
             then begin
                    if ARecursive
                    then DeleteFileOnPath(BasePath+DirectorySeparator+fileInfo.Name,
                                          AWildList, True);
                  end
             else if CanDel
                  then LazFileUtils.DeleteFileUTF8(BasePath+DirectorySeparator+fileInfo.Name);
        end;
        err :=FindNext(fileInfo);
   end;
   FindClose(fileInfo);
end;

end.

