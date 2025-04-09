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
    TFilesListSortType = (flsSortNone, flsSortNumeric, flsSortName, flsSortNatural);

function GetFilesCount(ADir: String; AExt: String) :Longint;

function GetFilesInDir(ADir: String; AExt: String;
                       SortType: TFilesListSortType = flsSortName;
                       (*Duplicates: TDuplicates = dupIgnore;*) CaseSensitive: Boolean = True): TStringList; overload;
function GetFilesInDir(BaseDir: String; Recursive: Boolean;
                       EnumAttr: Integer; EnumFilter: String;
                       SortType: TFilesListSortType; CaseSensitive: Boolean;
                       ReturnFullPath: Boolean): TStringList; overload;


//Folders Routines
procedure CopyFileOnPath(SourcePath, DestPath, AWildList:String; ARecursive :Boolean = True);
procedure DeleteFileOnPath(BasePath, AWildList:String; ARecursive :Boolean = True);

implementation

uses StrUtils, LazFileUtils, FileUtil, Masks;

function NaturalCompare(aList: TStringList; aIndex1, aIndex2: Integer): Integer;
begin
 Result:= NaturalCompareText(StringReplace(aList[aIndex1], ExtractFileExt(aList[aIndex1]), '', []),
                             StringReplace(aList[aIndex2], ExtractFileExt(aList[aIndex2]), '', []));
 // Result:= NaturalCompareText(aList[aIndex1], aList[aIndex2]);
end;

function LongintCompare(aList: TStringList; aIndex1, aIndex2: Integer): Integer;
var
   i1, i2: LongInt;
   s1, s2: String;

begin
 try
    s1:= aList[aIndex1];
    s2:= aList[aIndex2];

    i1 :=StrToInt(Copy(s1, 1, Pos(ExtensionSeparator, s1)-1));
    i2 :=StrToInt(Copy(s2, 1, Pos(ExtensionSeparator, s2)-1));
    if (i1 = i2)
    then Result :=0
    else if (i1 > i2)
         then Result :=1
         else Result :=-1;
 except
    Result:= NaturalCompareText(s1, s2);
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
                       (*Duplicates: TDuplicates;*) CaseSensitive: Boolean): TStringList;
var
  err: Integer;
  fileInfo: TSearchRec;

begin
  (*
  Result :=nil;
  if not(ADir[Length(ADir)] in AllowDirectorySeparators)
  then ADir :=ADir+DirectorySeparator;

  try
     Result:= TStringList.Create;
     Result.Duplicates :=Duplicates;

     err :=FindFirst(ADir+'*'+ExtensionSeparator+AExt, faAnyFile, fileInfo);
     while (err = 0) do
     begin
          if (fileInfo.Name[1] <> '.')
          then Result.Add(Fileinfo.Name);

          err :=FindNext(fileInfo);
      end;

     Case SortType of
       flsSortNone: Result.Sorted:= False;
       flsSortName: Result.Sorted:= True;
       flsSortNumeric: Result.CustomSort(@LongintCompare);
       flsSortNatural: Result.CustomSort(@NaturalCompare);
     end;

  finally
     FindClose(fileInfo);
  end;
  *)
  Result:= GetFilesInDir(ADir, False, faAnyFile, '*'+ExtensionSeparator+AExt, SortType, CaseSensitive, False);
end;

function GetFilesInDir(BaseDir: String; Recursive: Boolean;
                       EnumAttr: Integer; EnumFilter: String;
                       SortType: TFilesListSortType; CaseSensitive: Boolean;
                       ReturnFullPath: Boolean): TStringList;
var
   fileInfo: TSearchRec;
   err, i,
   len, lenC: Integer;
   subFiles,
   recFiles: TStringList;
   CanAdd: Boolean;

begin
  Result :=nil;
  try
     Result:= TStringList.Create;

     if Recursive
     then recFiles:= TStringList.Create
     else recFiles:= nil;

     err :=FindFirst(BaseDir+DirectorySeparator+'*', faAnyFile, fileInfo);
     while (err=0) do
     begin
       if (fileInfo.Name[1] <> '.') then  //not [.] or [..]
       begin
         CanAdd :=((fileInfo.Attr and EnumAttr) <>0) and
                   MatchesMaskList(fileInfo.Name, EnumFilter, ';', CaseSensitive);

         if ((fileInfo.Attr and faDirectory)<>0)
         then begin
                if Recursive then
                try
                  subFiles:= GetFilesInDir(BaseDir+DirectorySeparator+fileInfo.Name, Recursive,
                                           EnumAttr, EnumFilter,
                                           SortType, CaseSensitive,
                                           ReturnFullPath);

                  if not(ReturnFullPath) then //Add as Relative Path (Add .\ ?)
                    for i:=0 to subFiles.Count-1 do
                      subFiles[i]:= fileInfo.Name+DirectorySeparator+subFiles[i];

                  recFiles.AddStrings(subFiles);

                finally
                  subFiles.Free;
                end;

              end
         else if CanAdd then Result.Add(fileInfo.Name);
       end;

       err :=FindNext(fileInfo);
     end;

     Case SortType of
       flsSortNone: Result.Sorted:= False;
       flsSortName: Result.Sort;
       flsSortNumeric: Result.CustomSort(@LongintCompare);
       flsSortNatural: Result.CustomSort(@NaturalCompare);
     end;

     if ReturnFullPath then
       for i:=0 to Result.Count-1 do Result[i]:= BaseDir+DirectorySeparator+Result[i];

     //Insert Recursive Files as First
     if (recFiles <> nil) then
       for i:=recFiles.Count-1 downto 0 do Result.Insert(0, recFiles[i]);

  finally
    FindClose(fileInfo);
    if (recFiles <> nil) then recFiles.Free;
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

