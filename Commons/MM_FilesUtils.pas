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
                       CaseSensitive: Boolean = True): TStringList; overload;
function GetFilesInDir(BaseDir: String; Recursive: Boolean;
                       EnumAttr: Integer; EnumFilter: String;
                       SortType: TFilesListSortType; CaseSensitive: Boolean;
                       ReturnFullPath: Boolean): TStringList; overload;

//Get a Free File Name based on AFileName+AFileExt if file exists then ' (num)' or ' - num' is added
function GetFileFreeName(BaseDir, AFileName, AFileExt: String;
                         useParentheses: Boolean; IntDigits: Word): String;

//Folders Routines
procedure CopyFileOnPath(SourcePath, DestPath, AWildList:String; ARecursive :Boolean = True);
procedure DeleteFileOnPath(BasePath, AWildList:String; ARecursive :Boolean = True);

implementation

uses Math, StrUtils, LazFileUtils, FileUtil, Masks;

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
  then ADir:= ADir+DirectorySeparator;

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
                       CaseSensitive: Boolean): TStringList;
begin
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
     if not(BaseDir[Length(BaseDir)] in AllowDirectorySeparators)
     then BaseDir:= BaseDir+DirectorySeparator;

     Result:= TStringList.Create;

     if Recursive
     then recFiles:= TStringList.Create
     else recFiles:= nil;

     err :=FindFirst(BaseDir+'*', faAnyFile, fileInfo);
     while (err=0) do
     begin
       if (fileInfo.Name[1] <> '.') then  //not [.] or [..]
       begin
         CanAdd:= ((fileInfo.Attr and EnumAttr) <>0) and
                  //testare il codice condizionale sotto
                  {$IFDEF Windows}
                  MatchesWindowsMaskList(fileInfo.Name, EnumFilter, ';', CaseSensitive);
                  {$ELSE}
                  MatchesMaskList(fileInfo.Name, EnumFilter, ';', CaseSensitive);
                  {$ENDIF}

         if ((fileInfo.Attr and faDirectory)<>0)
         then begin
                if Recursive then
                try
                  subFiles:= GetFilesInDir(BaseDir+fileInfo.Name+DirectorySeparator, Recursive,
                                           EnumAttr, EnumFilter,
                                           SortType, CaseSensitive,
                                           ReturnFullPath);

                  if not(ReturnFullPath) then //Add as Relative Path (Add .\ ?)
                    for i:=0 to subFiles.Count-1 do
                      subFiles[i]:= fileInfo.Name+DirectorySeparator+subFiles[i];

                  len:= subFiles.Count;
                  lenC:= recFiles.Count;
                  recFiles.AddStrings(subFiles);
                  lenC:= recFiles.Count;

                finally
                  subFiles.Free;
                end;

              end
         else if CanAdd then Result.Add(fileInfo.Name);
       end;

       err :=FindNext(fileInfo);
     end;

     lenC:= Result.Count;

     Case SortType of
       flsSortNone: Result.Sorted:= False;
       flsSortName: Result.Sort;
       flsSortNumeric: Result.CustomSort(@LongintCompare);
       flsSortNatural: Result.CustomSort(@NaturalCompare);
     end;

     lenC:= Result.Count;

    if ReturnFullPath then
       for i:=0 to Result.Count-1 do Result[i]:= BaseDir+Result[i];

    lenC:= Result.Count;

    //Insert Recursive Files as First
    if (recFiles <> nil) then
      for i:=recFiles.Count-1 downto 0 do Result.Insert(0, recFiles[i]);

    lenC:= Result.Count;

  finally
    FindClose(fileInfo);
    if (recFiles <> nil) then recFiles.Free;
  end;
end;

function GetFileFreeName(BaseDir, AFileName, AFileExt: String;
                         useParentheses: Boolean; IntDigits: Word): String;
var
   i,
   maxI: Integer;
   formatString: String;
   finded: Boolean;

begin
  if not(BaseDir[Length(BaseDir)] in AllowDirectorySeparators)
  then BaseDir:= BaseDir+DirectorySeparator;

  Result:= AFileName+AFileExt;
  if FileExists(BaseDir+Result) then
  begin
    finded:= False;
    i:= 1;
    maxI:= 10**IntDigits;

    if useParentheses
    then formatString:= ' (%.'+IntToStr(IntDigits)+'d)'
    else formatString:= ' - %.'+IntToStr(IntDigits)+'d';

    repeat
      Result:= AFileName+Format(formatString, [i])+AFileExt;
      finded:= not(FileExists(BaseDir+Result));

      inc(i);
    until finded or (i >= maxI);
    if not(finded) then Result:= '';
  end;
end;

procedure CopyFileOnPath(SourcePath, DestPath, AWildList :String; ARecursive :Boolean = True);
var
   fileInfo   :TSearchRec;
   err        :Integer;
   CanCopy,
   IsDir      :Boolean;

begin
   if not(SourcePath[Length(SourcePath)] in AllowDirectorySeparators)
   then SourcePath:= SourcePath+DirectorySeparator;

   if not(DestPath[Length(DestPath)] in AllowDirectorySeparators)
   then DestPath:= DestPath+DirectorySeparator;

   err:= FindFirst(SourcePath+'*', faAnyFile, fileInfo);
   while (err=0) do
   begin
        if (fileInfo.Name[1] <> '.') then  //non è [.] o [..]
        begin
             IsDir:= ((fileInfo.Attr and faDirectory)<>0);

             //testare il codice condizionale sotto
             {$IFDEF Windows}
             CanCopy:= MatchesWindowsMaskList(fileInfo.Name, AWildList);
             {$ELSE}
             CanCopy :=MatchesMaskList(fileInfo.Name, AWildList);
             {$ENDIF}

             if IsDir
             then begin
                    if ARecursive
                    then CopyFileOnPath(SourcePath+fileInfo.Name+DirectorySeparator,
                                        DestPath+fileInfo.Name+DirectorySeparator,
                                        AWildList, True);
                  end
             else if CanCopy
                  then FileUtil.CopyFile(SourcePath+fileInfo.Name,
                                         DestPath+fileInfo.Name,
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
   if not(BasePath[Length(BasePath)] in AllowDirectorySeparators)
   then BasePath:= BasePath+DirectorySeparator;

   err :=FindFirst(BasePath+'*', faAnyFile, fileInfo);
   while (err=0) do
   begin
        if (fileInfo.Name[1] <> '.') then  //non è [.] o [..]
        begin
             IsDir  :=((fileInfo.Attr and faDirectory)<>0);
             //testare il codice condizionale sotto
             {$IFDEF Windows}
             CanDel:= MatchesWindowsMaskList(fileInfo.Name, AWildList);
             {$ELSE}
             CanDel :=MatchesMaskList(fileInfo.Name, AWildList);
             {$ENDIF}

             if IsDir
             then begin
                    if ARecursive
                    then DeleteFileOnPath(BasePath+fileInfo.Name+DirectorySeparator,
                                          AWildList, True);
                  end
             else if CanDel
                  then LazFileUtils.DeleteFileUTF8(BasePath+fileInfo.Name);
        end;
        err :=FindNext(fileInfo);
   end;
   FindClose(fileInfo);
end;

end.

