unit MM_UI_EnumFiles;
{$mode objfpc}
{$H+}

interface
uses
  SysUtils, Classes, MM_DelphiCompatibility, Menus, Masks, StrUtils;

type
  TMM_UI_EnumFilesItemClick =procedure(Sender :TObject; Item:TMenuItem; FileName :String) of object;

  TMM_UI_EnumFilesGetCaption =procedure(Sender :TObject; FileName :String; var theCaption :String; var CanAdd :Boolean) of object;
  TMM_UI_EnumFilesSorting =(soNone, soAscending, soDescending);

  { TMM_UI_EnumFilesINMenuItem }

  TMM_UI_EnumFilesINMenuItem = class(TComponent)
  private
    procedure SetImageIndex_Dir(AValue: Integer);
    procedure SetImageIndex_File(AValue: Integer);
  protected
     rSorted: TMM_UI_EnumFilesSorting;
     rMenuItem,
     rSelectedItem: TMenuItem;
     rBasePath,
     //BaseDir,
     rSelectedPath,
     rEnumFilter,
     rDefaultItem: String;
     rABSBasePaths,
     rBasePaths: TStringList;
     rEnumAttr,
     rImageIndex_Dir,
     rImageIndex_File: Integer;
     rOnItemClick: TMM_UI_EnumFilesItemClick;
     rOnGetCaption: TMM_UI_EnumFilesGetCaption;
     rOnUpdateDone: TNotifyEvent;
     rDeleteExtFromCaption,
     rUpdateMenuAfterLoaded,
     rCheckedStyle,
     rRecursive,
     rAutoMenuItemVisible: Boolean;

     procedure SetSorted(AValue: TMM_UI_EnumFilesSorting);
     procedure SetMenuItem(Value: TMenuItem); virtual;
     procedure SetBasePath(Value: String); virtual;
     procedure SetBasePaths(Value: TStringList); virtual;
     procedure Loaded; override;
     procedure UpdateMenuItem(theItem: TMenuItem; DefaultClick: Boolean); virtual;
     procedure SetEnumFilter(Value: String); virtual;
     procedure SetEnumAttr(Value: Integer); virtual;
     procedure SetRecursive(Value: Boolean); virtual;
     procedure BuildABSPaths;
  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure UpdateOnBasePath(DefaultClick: Boolean);
     procedure ItemClick(Sender: TObject); virtual;
     function FindItem(Paths: array of String): TMenuItem;
     procedure FillListByItem(AItem: TMenuItem; List: TStrings; ClearList: Boolean=True);

     property SelectedItem: TMenuItem read rSelectedItem;
     property SelectedPath: String read rSelectedPath;
  published
     property MenuItem: TMenuItem read rMenuItem write SetMenuItem;
     property BasePath: String read rBasePath write SetBasePath;
     property BasePaths: TStringList read rBasePaths write SetBasePaths;
     property EnumFilter: String read rEnumFilter write SetEnumFilter;
     property DefaultItem: String read rDefaultItem write rDefaultItem;
     property EnumAttr: Integer read rEnumAttr write SetEnumAttr default faAnyfile;
     property DeleteExtFromCaption: Boolean read rDeleteExtFromCaption write rDeleteExtFromCaption;
     property CheckedStyle: Boolean read  rCheckedStyle write rCheckedStyle;
     property UpdateMenuAfterLoaded: Boolean read rUpdateMenuAfterLoaded write rUpdateMenuAfterLoaded default True;
     property Recursive: Boolean read rRecursive write SetRecursive default False;
     property AutoMenuItemVisible: Boolean read rAutoMenuItemVisible write rAutoMenuItemVisible;
     property Sorted: TMM_UI_EnumFilesSorting read rSorted write SetSorted default soNone;
     property ImageIndex_File: Integer read rImageIndex_File write SetImageIndex_File default -1;
     property ImageIndex_Dir: Integer read rImageIndex_Dir write SetImageIndex_Dir default -1;

     //Events
     property OnItemClick: TMM_UI_EnumFilesItemClick read rOnItemClick write rOnItemClick;
     property OnGetCaption: TMM_UI_EnumFilesGetCaption read rOnGetCaption write rOnGetCaption;
     property OnUpdateDone: TNotifyEvent read rOnUpdateDone write rOnUpdateDone;
  end;

procedure Register;

implementation

uses MM_VariantsStringList
    {$IF FPC_FULLVERSION<30300}, MM_StrUtils {$ENDIF}
    ;

procedure Register;
begin
     RegisterComponents('MaxM_UI', [TMM_UI_EnumFilesINMenuItem]);
end;

constructor TMM_UI_EnumFilesINMenuItem.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     rMenuItem :=Nil;
     rSelectedItem :=Nil;
     rBasePaths :=TStringList.Create;
     rABSBasePaths:= TStringList.Create;
     rBasePaths.Add('.');
     rEnumFilter :='*.*';
     rEnumAttr :=faAnyFile;
     rRecursive :=False;
     rUpdateMenuAfterLoaded :=True;
     rImageIndex_Dir :=-1;
     rImageIndex_File :=-1;
     rBasePath:= '';
end;

destructor TMM_UI_EnumFilesINMenuItem.Destroy;
begin
  rABSBasePaths.Free;
  rBasePaths.Free;

  inherited Destroy;
end;

procedure TMM_UI_EnumFilesINMenuItem.ItemClick(Sender: TObject);
begin
     rSelectedPath :=TMenuItem(Sender).Hint; //rItems.Values[TMenuItem(Sender).Tag];
     if assigned(rOnItemClick)
     then rOnItemClick(Self, TMenuItem(Sender), TMenuItem(Sender).Hint);

     if rCheckedStyle then
     begin
          //Avoid bugs on old fpc versions (solved by Me)
          if (rSelectedItem<>Nil)
          then rSelectedItem.Checked :=False;

          rSelectedItem :=TMenuItem(Sender);
          rSelectedItem.Checked :=True;
      end;
end;

procedure TMM_UI_EnumFilesINMenuItem.Loaded;
begin
     inherited Loaded;

     if not(csDesigning in ComponentState)
     then BuildABSPaths;

     if rUpdateMenuAfterLoaded
     then UpdateMenuItem(rMenuItem, True);
end;

procedure TMM_UI_EnumFilesINMenuItem.BuildABSPaths;
Var
   index :Integer;
   currValue :String;

begin
     if (rBasePath = '')
     then rBasePath:= ExtractFilePath(ParamStr(0));

     if (rBasePath[Length(rBasePath)] in AllowDirectorySeparators)
     then SetLength(rBasePath, Length(rBasePath)-1);

     rABSBasePaths.Clear;

     for index :=0 to rBasePaths.Count-1 do
     begin
          currValue :=rBasePaths.Strings[index];

          if (currValue<>'') then
          begin
               //Convert Paths to Current System
               DoDirSeparators(currValue);

               {$IF FPC_FULLVERSION<30300}
               currValue:= RelativePathToFullPath(rBasePath, currValue);
               {$ELSE}
               currValue:= ExpandFileName(currValue, rBasePath);
               {$ENDIF}

               if (currValue[Length(currValue)] in AllowDirectorySeparators)
               then SetLength(currValue, Length(currValue)-1);

               (*
               if (currValue[1]='.') then
               begin
                    Delete(currValue, 1, 1);
                    if (currValue<>'') then
                    begin
                         if (currValue[1] in AllowDirectorySeparators)
                         then Delete(currValue, 1, 1);

                         currValue :=rBasePath+DirectorySeparator+currValue;
                    end;
                end;
                rBasePaths.Strings[index] :=currValue;
               *)

               rABSBasePaths.Add(currValue);
           end;
      end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetBasePath(Value: String);
begin
  if (rBasePath <> Value) then
  begin
    rBasePath:= Value;
    if not(csDesigning in ComponentState) and
       not(csLoading in ComponentState)
    then begin
              BuildABSPaths;
              UpdateMenuItem(rMenuItem, True);
          end;
  end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetBasePaths(Value: TStringList);
begin
     rBasePaths.Clear;
     rBasePaths.AddStrings(Value);
     if not(csDesigning in ComponentState) and
        not(csLoading in ComponentState)
     then begin
               BuildABSPaths;
               UpdateMenuItem(rMenuItem, True);
           end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetEnumAttr(Value: Integer);
begin
     if (Value<>rEnumAttr) then
     begin
          rEnumAttr :=Value;

          if not(csDesigning in ComponentState) and
             not(csLoading in ComponentState)
          then UpdateMenuItem(rMenuItem, True);
      end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetEnumFilter(Value: String);
begin
     if (Value<>rEnumFilter) then
     begin
          rEnumFilter :=Value;

          if not(csDesigning in ComponentState) and
             not(csLoading in ComponentState)
          then UpdateMenuItem(rMenuItem, True);
      end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetImageIndex_File(AValue: Integer);
begin
  if (rImageIndex_File <> AValue)
  then rImageIndex_File := AValue;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetImageIndex_Dir(AValue: Integer);
begin
  if (rImageIndex_Dir <> AValue)
  then rImageIndex_Dir := AValue;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetSorted(AValue: TMM_UI_EnumFilesSorting);
begin
  if (rSorted<>AValue) then
  begin
       rSorted:=AValue;

       if not(csDesigning in ComponentState) and
          not(csLoading in ComponentState)
       then UpdateMenuItem(rMenuItem, True);
  end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetMenuItem(Value: TMenuItem);
begin
     if (Value<>rMenuItem) then
     begin
          if not(csDesigning in ComponentState) and
             not(csLoading in ComponentState)
          then UpdateMenuItem(Value, True);

          rMenuItem :=Value;
      end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetRecursive(Value: Boolean);
begin
     if (Value<>rRecursive) then
     begin
          rRecursive :=Value;

          if not(csDesigning in ComponentState) and
             not(csLoading in ComponentState)
          then UpdateMenuItem(rMenuItem, True);
      end;
end;

(*

procedure TMM_UI_EnumFilesINMenuItem.UpdateMenuItem(theItem :TMenuItem;
                                              DefaultClick :Boolean);
Var
   baseindex,
   index      :Integer;

   procedure SearchOnPath(xItem :TMenuItem; BaseDir :String);
   var
      fileInfo   :TSearchRec;
      err        :Integer;
      dupIndex   :Cardinal;
      newItem,
      dupItem    :TMenuItem;
      theCaption,
      insCaption,
      theExt     :String;
      isDefault,
      CanAdd,
      IsDir      :Boolean;

   begin
        if (BaseDir[Length(BaseDir)] in AllowDirectorySeparators)
        then SetLength(BaseDir, Length(BaseDir)-1);

        if DirectoryExists(BaseDir) then
        begin
             err :=FindFirst(BaseDir+DirectorySeparator+'*', faAnyFile, fileInfo);
             while (err=0) do
             begin
                  if (fileInfo.Name[1] <> '.') then  //non è [.] o [..]
                  begin
                       theCaption :=ExtractFileName(fileInfo.Name);
                       theExt     :=ExtractFileExt(fileInfo.Name);
                       IsDir  :=((fileInfo.Attr and faDirectory)<>0);
                       CanAdd :=((fileInfo.Attr and rEnumAttr) <>0) and
                                 MatchesMask(fileInfo.Name, rEnumFilter);
                       if IsDir and rRecursive
                       then begin
                                 dupItem :=xItem.Find(theCaption);
                                 if (dupItem<>Nil)
                                 then newItem :=dupItem
                                 else newItem :=Menus.NewItem(theCaption, 0,
                                                  False,
                                                  true,
                                                  Nil, 0,
                                                  Self.Name+'_SUB'+IntToStr(index));
                                 newItem.Tag :=-1;
                                 SearchOnPath(newItem, BaseDir+DirectorySeparator+fileInfo.Name);
                                 if newItem.Count>0
                                 then begin
                                           if (dupItem=Nil)
                                           then xItem.Add(newItem);
                                      end
                                 else newItem.Free;
                            end
                       else if CanAdd then
                            begin
                                 if rDeleteExtFromCaption
                                 then Delete(theCaption, pos(theExt, theCaption), 255);

                                 if Assigned(rOnGetCaption)
                                 then rOnGetCaption(Self, BaseDir+DirectorySeparator+fileInfo.Name,
                                               theCaption, CanAdd);

                                 if CanAdd then
                                 begin
                                      rFilesList.Add(BaseDir+DirectorySeparator+fileInfo.Name);
                                      rCaptionsList.Add(StripHotKey(theCaption));

                                      isDefault :=(rDefaultItem<>'') and
                                             (Uppercase(theCaption)=Uppercase(rDefaultItem));
                                      if (xItem<>Nil)
                                      then begin
                                                insCaption :=theCaption;
                                                dupIndex :=2;
                                                dupItem :=xItem.Find(insCaption);
                                                while (dupItem<>Nil) do
                                                begin
                                                     insCaption :=theCaption+' ('+IntToStr(dupIndex)+')';
                                                     dupItem :=xItem.Find(insCaption);
                                                     inc(dupIndex);
                                                end;
                                                newItem :=Menus.NewItem(insCaption, 0,
                                                        (rCheckedStyle and isDefault),
                                                        true,
                                                        @Self.ItemClick, 0,
                                                        Self.Name+'_'+IntToStr(index));
                                                newItem.Tag :=index;
                                                newItem.Hint :=rFilesList.Strings[index];
                                                xItem.Add(newItem);

                                                if isDefault and DefaultClick
                                                then Self.ItemClick(newItem);
                                           end
                                      else begin
                                             if isDefault and DefaultClick and
                                                Assigned(rOnItemClick)
                                             then rOnItemClick(Self, Nil, rFilesList[index], index);
                                           end;
                                      inc(index);
                                 end;
                            end;
                  end;
                  err :=FindNext(fileInfo);
             end;
        end;
   end;

begin
     rMenuItem :=theItem;
     if rMenuItem<>Nil then
     begin
          rMenuItem.Clear;
          if rAutoMenuItemVisible
          then rMenuItem.Visible :=False;
      end;
     rFilesList.Clear;
     rCaptionsList.Clear;
     rSelectedPath :='';
     index :=0;

     for baseindex :=0 to rBasePaths.Count-1 do
     begin
          if (rBasePaths.Strings[baseindex]<>'')
          then SearchOnPath(rMenuItem, rBasePaths.Strings[baseindex]);
      end;
     if Assigned(rOnUpdateDone)
     then rOnUpdateDone(Self);

     if rAutoMenuItemVisible and
        (rMenuItem<>Nil) and (rMenuItem.Count>0)
     then rMenuItem.Visible :=True;
end;

*)

procedure TMM_UI_EnumFilesINMenuItem.UpdateMenuItem(theItem: TMenuItem; DefaultClick: Boolean);
Var
   baseindex,
   index: Integer;

   procedure SearchOnPath(xItem: TMenuItem; BaseDir: String);
   var
      fileInfo: TSearchRec;
      err, i, dupIndex: Integer;
      newItem,
      dupItem: TMenuItem;
      theCaption,
      insCaption,
      theExt: String;
      isDefault,
      CanAdd,
      IsDir: Boolean;
      xItems_Files,
      xItems_Dirs: TVariantsStringList;

   begin
        //if Last char is Separator, Delete it
        if (BaseDir[Length(BaseDir)] in AllowDirectorySeparators)
        then SetLength(BaseDir, Length(BaseDir)-1);

        if DirectoryExists(BaseDir) then
        begin
          try
             xItems_Files :=TVariantsStringList.Create;
             xItems_Dirs :=TVariantsStringList.Create;

             xItems_Files.Duplicates :=dupAccept;
             if (rSorted <> soNone)
             then begin
                       xItems_Files.SortDescending := (rSorted = soDescending);
                       xItems_Files.Sorted :=True;
                   end;

             xItems_Dirs.Duplicates :=dupAccept;
             if (rSorted <> soNone)
             then begin
                       xItems_Dirs.SortDescending := (rSorted = soDescending);
                       xItems_Dirs.Sorted :=True;
                   end;

             err :=FindFirst(BaseDir+DirectorySeparator+'*', faAnyFile, fileInfo);
             while (err=0) do
             begin
                  if (fileInfo.Name[1] <> '.') then  //non è [.] o [..]
                  begin
                       theCaption :=ExtractFileName(fileInfo.Name);
                       theExt     :=ExtractFileExt(fileInfo.Name);
                       IsDir  :=((fileInfo.Attr and faDirectory)<>0);
                       CanAdd :=((fileInfo.Attr and rEnumAttr) <>0) and
                                 MatchesMask(fileInfo.Name, rEnumFilter);
                       if IsDir and rRecursive
                       then begin
                                 ////If there is a duplicated Item (?) use It, else create new
                                 if (xItems_Dirs.Find(theCaption, dupIndex))
                                 then newItem :=TMenuItem(PtrInt(xItems_Dirs.Values[dupIndex]))
                                 else begin
                                           dupIndex :=-1;
                                           inc(index);
                                           newItem :=Menus.NewItem(theCaption, 0,
                                                  False,
                                                  true,
                                                  Nil, 0,
                                                  Self.Name+'_SUB'+IntToStr(index));
                                       end;
                                 newItem.Tag :=index; //-1;
                                 newItem.ImageIndex :=rImageIndex_Dir;
                                 newItem.Hint :=BaseDir+DirectorySeparator+fileInfo.Name+DirectorySeparator;

                                 SearchOnPath(newItem, BaseDir+DirectorySeparator+fileInfo.Name);

                                 if (newItem.Count > 0)
                                 then begin
                                           if (dupIndex = -1)
                                           then xItems_Dirs.Add(theCaption, PtrInt(newItem));
                                      end
                                 else begin //if there is no Items is an empty dir, delete it
                                           dec(index);
                                           newItem.Free;
                                      end;
                            end
                       else if CanAdd then
                            begin
                                 if rDeleteExtFromCaption
                                 then Delete(theCaption, pos(theExt, theCaption), 255);

                                 if Assigned(rOnGetCaption)
                                 then rOnGetCaption(Self, BaseDir+DirectorySeparator+fileInfo.Name,
                                               theCaption, CanAdd);

                                 if CanAdd
                                 then xItems_Files.Add(StripHotKey(theCaption), BaseDir+DirectorySeparator+fileInfo.Name);
                            end;
                  end;
                  err :=FindNext(fileInfo);
             end;
             FindClose(fileInfo);

             //Add First the SubDirectories
             for i :=0 to xItems_Dirs.Count-1 do
             begin
                  newItem :=TMenuItem(PtrInt(xItems_Dirs.Values[i]));
                  xItem.Add(newItem);
              end;

             //Next Add the Files
             for i :=0 to xItems_Files.Count-1 do
             begin
                  inc(index);
                  theCaption :=xItems_Files.Strings[i];

                  isDefault :=(rDefaultItem<>'') and
                              (Uppercase(theCaption)=Uppercase(rDefaultItem));

                  //If there is a duplicated Item add a Counter in Caption
                  insCaption :=theCaption;
                  dupIndex :=2;
                  dupItem :=xItem.Find(insCaption);
                  while (dupItem<>Nil) do
                  begin
                       insCaption :=theCaption+' ('+IntToStr(dupIndex)+')';
                       dupItem :=xItem.Find(insCaption);
                       inc(dupIndex);
                  end;

                  newItem :=Menus.NewItem(insCaption, 0,
                                          (rCheckedStyle and isDefault),
                                          True,
                                          @Self.ItemClick, 0,
                                          Self.Name+'_'+IntToStr(index));
                  newItem.Tag :=index;
                  newItem.ImageIndex :=rImageIndex_File;
                  newItem.Hint :=String(xItems_Files.Values[i]);
                  xItem.Add(newItem);

                  if isDefault then
                  begin
                    rSelectedPath :=newItem.Hint;

                    if DefaultClick and Assigned(rOnItemClick)
                    then rOnItemClick(Self, newItem, newItem.Hint);
                  end;
             end;

          finally
            xItems_Files.Free;
            xItems_Dirs.Free;
          end;
        end;
   end;

begin
     rMenuItem :=theItem;
     if (rMenuItem <> Nil) then
     begin
          rMenuItem.Clear;
          if rAutoMenuItemVisible
          then rMenuItem.Visible :=False;

          rSelectedPath :='';
          index :=0;

          for baseindex :=0 to rABSBasePaths.Count-1 do
          begin
               if (rABSBasePaths.Strings[baseindex] <> '')
               then SearchOnPath(rMenuItem, rABSBasePaths.Strings[baseindex]);
           end;

          if Assigned(rOnUpdateDone)
          then rOnUpdateDone(Self);

          if rAutoMenuItemVisible and (rMenuItem.Count>0)
          then rMenuItem.Visible :=True;
      end;
end;

procedure TMM_UI_EnumFilesINMenuItem.UpdateOnBasePath(DefaultClick: Boolean);
begin
     UpdateMenuItem(rMenuItem, DefaultClick);
end;

function TMM_UI_EnumFilesINMenuItem.FindItem(Paths: array of String): TMenuItem;
Var
   i       :Integer;
   curItem :TMenuItem;

begin
     Result :=Nil;
     if (rMenuItem<>Nil) then
     begin
          curItem :=rMenuItem;
          i :=-1;
          Repeat
                Inc(i);
                curItem :=curItem.Find(Paths[i]);
          Until (curItem=Nil) or (i=High(Paths));
          Result :=curItem;
      end;
end;

procedure TMM_UI_EnumFilesINMenuItem.FillListByItem(AItem: TMenuItem; List: TStrings; ClearList: Boolean=True);
Var
   i,
   level :Integer;

   procedure _FillListByItem(xItem :TMenuItem);
   Var
      k :Integer;

   begin
        Inc(level);
        List.AddObject(DupeString(' ', level) + StripHotKey(xItem.Caption), xItem);
        for k :=0 to xItem.Count-1 do
        begin
             _FillListByItem(xItem.Items[k]);
        end;
        Dec(level);
   end;

begin
     if (AItem<>Nil) then
     begin
          if ClearList
          then List.Clear;

          level :=-1;
          for i :=0 to AItem.Count-1 do
          begin
               _FillListByItem(AItem.Items[i]);
           end;
      end;
end;

end.




