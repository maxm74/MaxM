unit MM_UI_EnumFiles;
{$mode objfpc}
{$H+}

interface
uses
  SysUtils, Classes, MM_DelphiCompatibility, Menus, Masks, StrUtils;

type
  TMM_UI_EnumFilesItemClick = procedure(Sender :TObject; Item:TMenuItem; FileName :String) of object;

  TMM_UI_EnumFilesOnCreateNode = function (Sender: TObject; AIndex: Integer; IsDir: Boolean; FileName: String; var ACaption :String): Boolean of object;
  TMM_UI_EnumFilesSorting =(soNone, soAscending, soDescending);

  { TMM_UI_EnumFiles }

  TMM_UI_EnumFiles = class(TComponent)
  private
    procedure SetImageIndex_Dir(AValue: Integer);
    procedure SetImageIndex_File(AValue: Integer);

  protected
     BaseNode: TObject;

     rSorted: TMM_UI_EnumFilesSorting;
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
     rOnCreateNode: TMM_UI_EnumFilesOnCreateNode;
     rOnUpdateDone: TNotifyEvent;
     rUpdateAfterLoaded,
     rDeleteExtFromCaption,
     rRecursive: Boolean;

     procedure SetSorted(AValue: TMM_UI_EnumFilesSorting);
     procedure SetBasePath(Value: String); virtual;
     procedure SetBasePaths(Value: TStringList); virtual;
     procedure Loaded; override;
     procedure UpdateControl; virtual;
     procedure SetEnumFilter(Value: String); virtual;
     procedure SetEnumAttr(Value: Integer); virtual;
     procedure SetRecursive(Value: Boolean); virtual;
     procedure BuildABSPaths;

     function CreateNode(AIndex: Integer; IsDir: Boolean; FullPath, ACaption: String): TObject; virtual; abstract;
     procedure DeleteNode(ANode: TObject); virtual;
     procedure AddNode(ParentNode, NewNode: TObject; AOnClick: TNotifyEvent); virtual;

  public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;

     procedure UpdateOnBasePath(DefaultClick: Boolean);
     procedure DoClick(Sender: TObject); virtual; abstract;

     property SelectedPath: String read rSelectedPath;

  published
     property BasePath: String read rBasePath write SetBasePath;
     property BasePaths: TStringList read rBasePaths write SetBasePaths;
     property EnumFilter: String read rEnumFilter write SetEnumFilter;
     property EnumAttr: Integer read rEnumAttr write SetEnumAttr default faAnyfile;
     property DeleteExtFromCaption: Boolean read rDeleteExtFromCaption write rDeleteExtFromCaption;
     property DefaultItem: String read rDefaultItem write rDefaultItem;
     property Recursive: Boolean read rRecursive write SetRecursive default False;
     property Sorted: TMM_UI_EnumFilesSorting read rSorted write SetSorted default soNone;
     property UpdateAfterLoaded: Boolean read rUpdateAfterLoaded write rUpdateAfterLoaded default True;
     property ImageIndex_File: Integer read rImageIndex_File write SetImageIndex_File default -1;
     property ImageIndex_Dir: Integer read rImageIndex_Dir write SetImageIndex_Dir default -1;

     //Events
     property OnCreateNode: TMM_UI_EnumFilesOnCreateNode read rOnCreateNode write rOnCreateNode;
     property OnUpdateDone: TNotifyEvent read rOnUpdateDone write rOnUpdateDone;
  end;

  { TMM_UI_EnumFilesINMenuItem }

  TMM_UI_EnumFilesINMenuItem = class(TMM_UI_EnumFiles)
  protected
     rMenuItem,
     rSelectedItem: TMenuItem;
     rCheckedStyle,
     rDefaultClick,
     rAutoMenuItemClear,
     rAutoMenuItemVisible: Boolean;

     procedure UpdateControl; override;

     function CreateNode(AIndex: Integer; IsDir: Boolean; FullPath, ACaption: String): TObject; override;
     procedure AddNode(ParentNode, NewNode: TObject; AOnClick: TNotifyEvent); override;

     procedure SetMenuItem(Value: TMenuItem); virtual;

  public
     constructor Create(AOwner: TComponent); override;

     procedure DoClick(Sender: TObject); override;
     function FindItem(Paths: array of String): TMenuItem;
     procedure FillListByItem(AItem: TMenuItem; List: TStrings; ClearList: Boolean=True);

     property SelectedItem: TMenuItem read rSelectedItem;

  published
     property MenuItem: TMenuItem read rMenuItem write SetMenuItem;
     property CheckedStyle: Boolean read  rCheckedStyle write rCheckedStyle;
     property DefaultClick: Boolean read rDefaultClick write rDefaultClick;
     property AutoMenuItemClear: Boolean read rAutoMenuItemClear write rAutoMenuItemClear;
     property AutoMenuItemVisible: Boolean read rAutoMenuItemVisible write rAutoMenuItemVisible;

     //Events
     property OnItemClick: TMM_UI_EnumFilesItemClick read rOnItemClick write rOnItemClick;
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

constructor TMM_UI_EnumFiles.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     rBasePaths :=TStringList.Create;
     rABSBasePaths:= TStringList.Create;
     rBasePaths.Add('.');
     rEnumFilter :='*.*';
     rEnumAttr :=faAnyFile;
     rRecursive :=False;
     rUpdateAfterLoaded :=True;
     rImageIndex_Dir :=-1;
     rImageIndex_File :=-1;
     rBasePath:= '';
end;

destructor TMM_UI_EnumFiles.Destroy;
begin
  rABSBasePaths.Free;
  rBasePaths.Free;

  inherited Destroy;
end;

procedure TMM_UI_EnumFiles.Loaded;
begin
  inherited Loaded;

  if not(csDesigning in ComponentState) then
  begin
    BuildABSPaths;
    if rUpdateAfterLoaded then UpdateControl;
  end;
end;

procedure TMM_UI_EnumFiles.BuildABSPaths;
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

procedure TMM_UI_EnumFiles.DeleteNode(ANode: TObject);
begin
  ANode.Free;
end;

procedure TMM_UI_EnumFiles.AddNode(ParentNode, NewNode: TObject; AOnClick: TNotifyEvent);
begin
end;

procedure TMM_UI_EnumFiles.SetBasePath(Value: String);
begin
  if (rBasePath <> Value) then
  begin
    rBasePath:= Value;
    if not(csDesigning in ComponentState) and
       not(csLoading in ComponentState) then
    begin
      BuildABSPaths;
      UpdateControl;
    end;
  end;
end;

procedure TMM_UI_EnumFiles.SetBasePaths(Value: TStringList);
begin
  rBasePaths.Clear;
  rBasePaths.AddStrings(Value);
  if not(csDesigning in ComponentState) and
     not(csLoading in ComponentState) then
  begin
    BuildABSPaths;
    UpdateControl;
  end;
end;

procedure TMM_UI_EnumFiles.SetEnumAttr(Value: Integer);
begin
  if (Value<>rEnumAttr) then
  begin
    rEnumAttr :=Value;

    if not(csDesigning in ComponentState) and
       not(csLoading in ComponentState)
    then UpdateControl;
  end;
end;

procedure TMM_UI_EnumFiles.SetEnumFilter(Value: String);
begin
  if (Value<>rEnumFilter) then
  begin
    rEnumFilter :=Value;

    if not(csDesigning in ComponentState) and
       not(csLoading in ComponentState)
    then UpdateControl;
  end;
end;

procedure TMM_UI_EnumFiles.SetImageIndex_File(AValue: Integer);
begin
  if (rImageIndex_File <> AValue)
  then rImageIndex_File := AValue;
end;

procedure TMM_UI_EnumFiles.SetImageIndex_Dir(AValue: Integer);
begin
  if (rImageIndex_Dir <> AValue)
  then rImageIndex_Dir := AValue;
end;

procedure TMM_UI_EnumFiles.SetSorted(AValue: TMM_UI_EnumFilesSorting);
begin
  if (rSorted<>AValue) then
  begin
    rSorted:=AValue;

    if not(csDesigning in ComponentState) and
       not(csLoading in ComponentState)
    then UpdateControl;
  end;
end;

procedure TMM_UI_EnumFiles.SetRecursive(Value: Boolean);
begin
  if (Value<>rRecursive) then
  begin
    rRecursive :=Value;

    if not(csDesigning in ComponentState) and
       not(csLoading in ComponentState)
    then UpdateControl;
  end;
end;

procedure TMM_UI_EnumFiles.UpdateControl;
var
   curNode: TObject;
   Index,
   baseIndex: Integer;

   function SearchOnPath(xNode: TObject; BaseDir: String): Integer;
   var
      fileInfo: TSearchRec;
      err, i, dupIndex: Integer;
      newNode,
      dupNode: TObject;
      theCaption,
      theExt: String;
      isDefault,
      CanAdd,
      IsDir: Boolean;
      xItems_Files,
      xItems_Dirs: TVariantsStringList;

   begin
     Result:= 0;

        //if Last char is Separator, Delete it
        if (BaseDir[Length(BaseDir)] in AllowDirectorySeparators)
        then SetLength(BaseDir, Length(BaseDir)-1);

        if DirectoryExists(BaseDir) then
        begin
          try
             xItems_Files :=TVariantsStringList.Create;
             xItems_Files.OwnsObjects:= False;
             xItems_Dirs :=TVariantsStringList.Create;
             xItems_Dirs.OwnsObjects:= False;

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
                              //If there is a duplicated Item (?) use It or create new ??

                              //if (xItems_Dirs.Find(theCaption, dupIndex))
                              //then newNode:= xItems_Dirs.Objects[dupIndex]

                              inc(Index);
                              CanAdd:= True;

                              if Assigned(rOnCreateNode)
                              then CanAdd:= rOnCreateNode(Self, Index, True, BaseDir+DirectorySeparator+fileInfo.Name, theCaption);

                              if CanAdd
                              then begin
                                     newNode:= CreateNode(Index, IsDir, BaseDir+DirectorySeparator+fileInfo.Name+DirectorySeparator, theCaption);

                                     if (SearchOnPath(newNode, BaseDir+DirectorySeparator+fileInfo.Name) > 0)
                                     then xItems_Dirs.AddObject(theCaption, newNode)
                                     else begin
                                            DeleteNode(newNode); //if there is no Items is an empty dir, delete it
                                            dec(Index);
                                          end;
                                   end
                              else dec(Index);
                            end
                       else if CanAdd then
                            begin
                              inc(Index);

                              if Assigned(rOnCreateNode)
                              then CanAdd:= rOnCreateNode(Self, Index, False, BaseDir+DirectorySeparator+fileInfo.Name, theCaption);

                              if CanAdd
                              then begin
                                     newNode:= CreateNode(Index, IsDir, BaseDir+DirectorySeparator+fileInfo.Name+DirectorySeparator, theCaption);

                                     xItems_Files.AddObject(theCaption, newNode);
                                   end
                              else dec(Index);
                            end;
                  end;
                  err :=FindNext(fileInfo);
             end;
             FindClose(fileInfo);

             //Add First the SubDirectories
             for i :=0 to xItems_Dirs.Count-1 do
               AddNode(xNode, xItems_Dirs.Objects[i], @Self.DoClick);

             //Next Add the Files
             for i :=0 to xItems_Files.Count-1 do
             begin
               AddNode(xNode, xItems_Files.Objects[i], @Self.DoClick);

               if isDefault then
               begin
                 (* rSelectedPath :=newItem.Hint;

                  if DefaultClick then DoClick(xItems_Dirs.Objects[i]);
                  *)
               end;
             end;

          finally
            Result:= xItems_Dirs.Count+xItems_Files.Count;

            xItems_Files.Free;
            xItems_Dirs.Free;
          end;
        end;
   end;

begin
  rSelectedPath:= '';

  for baseindex:=0 to rABSBasePaths.Count-1 do
    if (rABSBasePaths.Strings[baseindex] <> '')
    then SearchOnPath(BaseNode, rABSBasePaths.Strings[baseindex]);

  if Assigned(rOnUpdateDone) then rOnUpdateDone(Self);
end;

procedure TMM_UI_EnumFiles.UpdateOnBasePath(DefaultClick: Boolean);
begin
  UpdateControl;
end;

constructor TMM_UI_EnumFilesINMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  rMenuItem :=Nil;
  rSelectedItem :=Nil;
end;

procedure TMM_UI_EnumFilesINMenuItem.UpdateControl;
begin
  BaseNode:= rMenuItem;

  if rAutoMenuItemClear then rMenuItem.Clear;
  if rAutoMenuItemVisible then rMenuItem.Visible:= False;

  inherited UpdateControl;

  if rAutoMenuItemVisible and (rMenuItem.Count > 0) then rMenuItem.Visible:= True;
end;

function TMM_UI_EnumFilesINMenuItem.CreateNode(AIndex: Integer; IsDir: Boolean; FullPath, ACaption: String): TObject;
begin
  if IsDir
  then Result:= Menus.NewItem(ACaption, 0, False, True, nil, 0, Self.Name+'_DIR_'+IntToStr(AIndex))
  else Result:= Menus.NewItem(ACaption, 0, False, True, nil, 0, Self.Name+'_'+IntToStr(AIndex));

  //I shouldn't use Hint but a Path list in base class
  TMenuItem(Result).Hint:= FullPath;

  if (rDefaultItem <> '') and (Uppercase(ACaption) = Uppercase(rDefaultItem)) then
  begin
    if rDefaultClick and Assigned(rOnItemClick)
    then rOnItemClick(Self, TMenuItem(Result), FullPath);

    rSelectedItem:= TMenuItem(Result);
    rSelectedPath:= FullPath;
    if rCheckedStyle then TMenuItem(Result).Checked:= True;
  end;
end;

procedure TMM_UI_EnumFilesINMenuItem.AddNode(ParentNode, NewNode: TObject; AOnClick: TNotifyEvent);
begin
  if (NewNode <> nil) and (NewNode is TMenuItem) then
  begin
    TMenuItem(NewNode).OnClick:= AOnClick;
    if (ParentNode <> nil) and (ParentNode is TMenuItem) then TMenuItem(ParentNode).Add(TMenuItem(NewNode));
  end;
end;

procedure TMM_UI_EnumFilesINMenuItem.SetMenuItem(Value: TMenuItem);
begin
  if (Value<>rMenuItem) then
  begin
    rMenuItem :=Value;

    if not(csDesigning in ComponentState) and
       not(csLoading in ComponentState)
    then UpdateControl;
  end;
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

procedure TMM_UI_EnumFilesINMenuItem.DoClick(Sender: TObject);
begin
  if assigned(rOnItemClick)
  then rOnItemClick(Self, TMenuItem(Sender), TMenuItem(Sender).Hint);

  rSelectedItem:= TMenuItem(Sender);
  //I shouldn't use Hint but a Path list in base class
  rSelectedPath:= rSelectedItem.Hint;

  if rCheckedStyle then rSelectedItem.Checked:= True;
end;

end.




