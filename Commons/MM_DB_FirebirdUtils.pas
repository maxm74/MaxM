//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2013-2015                                   ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_DB_FirebirdUtils.pas
//
//  Description : Firebird Database specific functions/classes.
//
//******************************************************************************

unit MM_DB_FirebirdUtils;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FBAdmin;

type
    { TMM_DB_Firebird_Users }
    //Manage Users using only the Current Database connection,
    //if you want to use the console use SERVER_xxx methods

    TFirebird_User_PrivilegeType = (upSelect, upInsert, upUpdate, upDelete, upReferences);
    TFirebird_User_Privileges = set of TFirebird_User_PrivilegeType;
    TFirebird_User = record
      UserName,
      Password,
      FirstName,
      MiddleName,
      LastName,
      RoleName,
      GroupName: String;
      UnixUserID,
      UnixGroupID: Integer;
      Privileges: TFirebird_User_Privileges;
    end;
    PFirebird_User = ^TFirebird_User;

    TMM_DB_Firebird_Users = class
    protected
      rIBConnection: TIBConnection;
      rSQL: TSQLQuery;
      FieldNamesQuoteChars: TQuoteChars;

    public
      constructor Create(AIBConnection: TIBConnection);
      destructor Destroy; override;

      procedure Add_User(AUser: TFirebird_User);
      procedure Del_User(UserName: String);
      procedure Alter_User(AUser: TFirebird_User);
      procedure Get_Users(UserNames: TStringList; Sorted: Boolean=False;
                          ExcludePublic: Boolean=True;
                          ExcludeSYSDBA: Boolean=True); //Return only a User Name List without data

      function SERVER_Get_User(UserName: String; var AUser: TFirebird_User): Boolean;
      function SERVER_Add_User(AUser: TFirebird_User): Boolean;
      function SERVER_Del_User(UserName: String; RoleName: String= ''): Boolean;
      function SERVER_Alter_User(AUser: TFirebird_User): Boolean;
      procedure SERVER_Get_Users(UserNames: TStringList; Sorted: Boolean=False;
                          ExcludePublic: Boolean=True;
                          ExcludeSYSDBA: Boolean=True);


      procedure Get_Tables(TableNames: TStringList; ExcludeSys: Boolean=True; Sorted: Boolean=True);

      function Get_Privileges(UserName: String; TableName: String): TFirebird_User_Privileges;
      procedure Grant_Privileges(UserName: String; TableNames: TStringList;
                                 AUser_Privileges: TFirebird_User_Privileges);
      procedure Revoke_Privileges(UserName: String; TableNames: TStringList;
                                  AUser_Privileges: TFirebird_User_Privileges;
                                  NotIncluded: Boolean=False);
      procedure Grant_Revoke_Privileges(UserName: String; TableNames: TStringList;
                                 AUser_Privileges: TFirebird_User_Privileges);
    end;

const
     upALL: TFirebird_User_Privileges =
                         [upSelect, upInsert, upUpdate, upDelete, upReferences];

type
    TFB_VersionInfo = record
       vPlatform: String;   //WI=Windows - LI=Linux
       vBuildType: Char;    //V=Stable - T=Development
       vMajor,
       vMinor,
       vPatch,
       vBuild: Integer;
       vApp: String;
       vAppMajor,
       vAppMinor: Integer;
    end;
    PFB_VersionInfo =^TFB_VersionInfo;

function Get_Privileges(AIBConnection: TIBConnection;
                        const UserName, TableName: String): TFirebird_User_Privileges;

function User_Privileges_TO_GRANTSTR(AUser_Privileges: TFirebird_User_Privileges): String;
function User_Privileges_TO_GRANTSTR_NOT(AUser_Privileges: TFirebird_User_Privileges): String;

//Return the Major Version
function FB_SERVER_GetVersion(AHostName, AUserName, APassword: String; verInfo: PFB_VersionInfo): Integer; overload;
//Return the Version String as is
function FB_SERVER_GetVersion(AHostName, AUserName, APassword: String): String; overload;

implementation

const
     PrivilegesGRANTSTR : array [TFirebird_User_PrivilegeType] of String =
       ('SELECT', 'INSERT', 'UPDATE', 'DELETE', 'REFERENCES');
     PrivilegesCHR : array [TFirebird_User_PrivilegeType] of Char =
       ('S', 'I', 'U', 'D', 'R');


function Get_Privileges(AIBConnection: TIBConnection;
                        const UserName, TableName: String): TFirebird_User_Privileges;
var
  fbUsers :TMM_DB_Firebird_Users;

begin
     try
        fbUsers :=TMM_DB_Firebird_Users.Create(AIBConnection);
        Result :=fbUsers.Get_Privileges(UserName, TableName);
        fbUsers.Free;
     except
       Result :=[];
       fbUsers.Free;
     end;
end;

function User_Privileges_TO_GRANTSTR(AUser_Privileges: TFirebird_User_Privileges): String;
begin
   Result :='';
   if (AUser_Privileges = upALL)
   then Result :='ALL'
   else begin
             if (upSelect in AUser_Privileges)
             then Result :=PrivilegesGRANTSTR[upSelect];
             if (upInsert in AUser_Privileges)
             then begin
                       if (Result <> '')
                       then Result :=Result+', ';

                       Result :=Result+PrivilegesGRANTSTR[upInsert];
                  end;
             if (upUpdate in AUser_Privileges)
             then begin
                       if (Result <> '')
                       then Result :=Result+', ';

                       Result :=Result+PrivilegesGRANTSTR[upUpdate];
                   end;
             if (upDelete in AUser_Privileges)
             then begin
                       if (Result <> '')
                       then Result :=Result+', ';

                       Result :=Result+PrivilegesGRANTSTR[upDelete];
                   end;
             if (upReferences in AUser_Privileges)
             then begin
                       if (Result <> '')
                       then Result :=Result+', ';

                       Result :=Result+PrivilegesGRANTSTR[upReferences];
                   end;
        end;
end;

function User_Privileges_TO_GRANTSTR_NOT(AUser_Privileges: TFirebird_User_Privileges): String;
begin
     Result :='';
     if (AUser_Privileges = [])
     then Result :='ALL'
     else begin
               if not(upSelect in AUser_Privileges)
               then Result :=PrivilegesGRANTSTR[upSelect];
               if not(upInsert in AUser_Privileges)
               then begin
                         if (Result <> '')
                         then Result :=Result+', ';

                         Result :=Result+PrivilegesGRANTSTR[upInsert];
                    end;
               if not(upUpdate in AUser_Privileges)
               then begin
                         if (Result <> '')
                         then Result :=Result+', ';

                         Result :=Result+PrivilegesGRANTSTR[upUpdate];
                     end;
               if not(upDelete in AUser_Privileges)
               then begin
                         if (Result <> '')
                         then Result :=Result+', ';

                         Result :=Result+PrivilegesGRANTSTR[upDelete];
                     end;
               if not(upReferences in AUser_Privileges)
               then begin
                         if (Result <> '')
                         then Result :=Result+', ';

                         Result :=Result+PrivilegesGRANTSTR[upReferences];
                     end;
          end;
end;

function CreateFBAdmin(AIBConnection: TIBConnection): TFBAdmin; overload;
begin
  Result :=TFBAdmin.Create(nil);
  Result.Host:= AIBConnection.HostName;
  Result.Protocol := IBSPTCPIP;
  Result.User:= AIBConnection.UserName;
  Result.Password:= AIBConnection.Password;
end;

function CreateFBAdmin(AHostName, AUserName, APassword: String): TFBAdmin; overload;
begin
  Result :=TFBAdmin.Create(nil);
  Result.Host:= AHostName;
  Result.Protocol := IBSPTCPIP;
  Result.User:= AUserName;
  Result.Password:= APassword;
end;

{ TMM_DB_Firebird_Users }

constructor TMM_DB_Firebird_Users.Create(AIBConnection: TIBConnection);
begin
     inherited Create;

     rIBConnection :=AIBConnection;
     rSQL :=TSQLQuery.Create(nil);
     rSQL.Active :=False;
     rSQL.DataBase :=AIBConnection;
     rSQL.Transaction :=AIBConnection.Transaction;
     FieldNamesQuoteChars := TSQLConnection(rSQL.DataBase).FieldNameQuoteChars;
end;

destructor TMM_DB_Firebird_Users.Destroy;
begin
     rSQL.Free;

     inherited Destroy;
end;

procedure TMM_DB_Firebird_Users.Add_User(AUser: TFirebird_User);
var
   query: String;

begin
     try
        rSQL.Active :=False;
        query :='CREATE USER '+FieldNamesQuoteChars[0]+AUser.UserName+FieldNamesQuoteChars[1]+
          ' PASSWORD '+QuotedStr(AUser.Password);

        if (AUser.FirstName <> '')
        then query :=query+' FIRSTNAME '+QuotedStr(AUser.FirstName);

        if (AUser.MiddleName <> '')
        then query :=query+' MIDDLENAME '+QuotedStr(AUser.MiddleName);

        if (AUser.LastName <> '')
        then query :=query+' LASTNAME '+QuotedStr(AUser.LastName);

        rIBConnection.ExecuteDirect(query);
        TSQLTransaction(rIBConnection.Transaction).Commit;
     finally
       rSQL.Active :=False;
     end;
end;

procedure TMM_DB_Firebird_Users.Del_User(UserName: String);
var
   query: String;

begin
     try
        rSQL.Active :=False;
        query :='DROP USER '+FieldNamesQuoteChars[0]+UserName+FieldNamesQuoteChars[1];
        //rSQL.Params.ParamByName('UserName').AsString :=UserName;

        rIBConnection.ExecuteDirect(query);
        TSQLTransaction(rIBConnection.Transaction).Commit;
     finally
       rSQL.Active :=False;
     end;
end;

procedure TMM_DB_Firebird_Users.Alter_User(AUser: TFirebird_User);
var
   query: String;

begin
     try
        rSQL.Active :=False;

        query :='';
        if (AUser.Password <> '')
        then query :=query+' PASSWORD '+QuotedStr(AUser.Password);

        if (AUser.FirstName <> '')
        then query :=query+' FIRSTNAME '+QuotedStr(AUser.FirstName);

        if (AUser.MiddleName <> '')
        then query :=query+' MIDDLENAME '+QuotedStr(AUser.MiddleName);

        if (AUser.LastName <> '')
        then query :=query+' LASTNAME '+QuotedStr(AUser.LastName);

        if (query <> '')
        then begin
               query :='ALTER USER '+FieldNamesQuoteChars[0]+AUser.UserName+FieldNamesQuoteChars[1]+query;
               rIBConnection.ExecuteDirect(query);
               TSQLTransaction(rIBConnection.Transaction).Commit;
              end;
     finally
       rSQL.Active :=False;
     end;
end;

procedure TMM_DB_Firebird_Users.Get_Users(UserNames: TStringList; Sorted: Boolean; ExcludePublic: Boolean; ExcludeSYSDBA: Boolean);
var
   curUser :String;
   query   :String;

begin
     try
        rSQL.Active :=False;
        UserNames.Clear;
        query :='SELECT DISTINCT RDB$USER FROM RDB$USER_PRIVILEGES';
        if (Sorted)
        then query :=query+' ORDER BY RDB$USER ASC';

        rSQL.SQL.Text :=query;
        rSQL.Open;
        while not(rSQL.EOF) do
        begin
             curUser :=Trim(rSQL.FieldByName('RDB$USER').AsString); //Trim????

             if not( ((ExcludePublic) and (curUser='PUBLIC')) or
                     ((ExcludeSYSDBA) and (curUser='SYSDBA')) )
             then UserNames.Add(curUser);

             rSQL.Next;
        end;
     finally
       rSQL.Active :=False;
     end;
end;

function TMM_DB_Firebird_Users.SERVER_Get_User(UserName: String; var AUser: TFirebird_User): Boolean;
var
   theServer: TFBAdmin;

begin
  try
     Result :=False;
     FillChar(AUser, sizeof(TFirebird_User), 0);
     theServer :=CreateFBAdmin(rIBConnection);
     if theServer.Connect then
     begin
       Result :=theServer.GetUser(UserName, AUser.GroupName, AUser.FirstName, AUser.MiddleName, AUser.LastName, AUser.UnixUserID, AUser.UnixGroupID);
       if Result
       then AUser.UserName :=UserName;
     end;
  finally
     FreeAndNil(theServer);
  end;
end;

function TMM_DB_Firebird_Users.SERVER_Add_User(AUser: TFirebird_User): Boolean;
var
   theServer: TFBAdmin;

begin
  try
     Result :=False;
     theServer :=CreateFBAdmin(rIBConnection);
     if theServer.Connect then
     begin
       Result :=theServer.AddUser(AUser.UserName, AUser.Password, AUser.RoleName, AUser.GroupName,
                   AUser.FirstName, AUser.MiddleName, AUser.LastName, AUser.UnixUserID, AUser.UnixGroupID);
     end;
  finally
     FreeAndNil(theServer);
  end;
end;

function TMM_DB_Firebird_Users.SERVER_Del_User(UserName: String; RoleName: String=''): Boolean;
var
   theServer: TFBAdmin;

begin
  try
     Result :=False;
     theServer :=CreateFBAdmin(rIBConnection);
     if theServer.Connect then
     begin
       Result :=theServer.DeleteUser(UserName, RoleName);
     end;
  finally
     FreeAndNil(theServer);
  end;
end;

function TMM_DB_Firebird_Users.SERVER_Alter_User(AUser: TFirebird_User): Boolean;
var
   theServer: TFBAdmin;

begin
  try
     Result :=False;
     theServer :=CreateFBAdmin(rIBConnection);
     if theServer.Connect then
     begin
       Result :=theServer.AddUser(AUser.UserName, AUser.Password, AUser.RoleName, AUser.GroupName,
                   AUser.FirstName, AUser.MiddleName, AUser.LastName, AUser.UnixUserID, AUser.UnixGroupID);
     end;
  finally
     FreeAndNil(theServer);
  end;
end;

procedure TMM_DB_Firebird_Users.SERVER_Get_Users(UserNames: TStringList; Sorted: Boolean; ExcludePublic: Boolean; ExcludeSYSDBA: Boolean);
var
   theServer: TFBAdmin;
   i: Integer;
   curUser: String;

begin
     try
        theServer :=CreateFBAdmin(rIBConnection);
        if theServer.Connect then
        begin
             if theServer.GetUsers(UserNames) then
             begin
                  for i :=UserNames.Count-1 downto 0 do
                  begin
                    curUser :=UserNames[i];

                    if ((ExcludePublic) and (curUser = 'PUBLIC')) or
                         ((ExcludeSYSDBA) and (curUser = 'SYSDBA'))
                    then UserNames.Delete(i);
                  end;

                  if Sorted
                  then UserNames.Sort;
             end;
        end;
     finally
        FreeAndNil(theServer);
     end;
end;


procedure TMM_DB_Firebird_Users.Get_Tables(TableNames: TStringList; ExcludeSys: Boolean=True; Sorted: Boolean=True);
var
   curTable :String;
   query    :String;

begin
     try
        rSQL.Active :=False;
        TableNames.Clear;
        query :='SELECT DISTINCT RDB$RELATION_NAME FROM RDB$RELATIONS WHERE (';
        if (ExcludeSys)
        then query :=query+'(RDB$SYSTEM_FLAG=0) AND ';

        query :=query+'(RDB$VIEW_BLR IS NULL))';
        if (Sorted)
        then query :=query+' ORDER BY RDB$RELATION_NAME ASC';

        rSQL.SQL.Text :=query;
        rSQL.Open;
        while not(rSQL.EOF) do
        begin
             curTable :=Trim(rSQL.FieldByName('RDB$RELATION_NAME').AsString); //Trim????
             TableNames.Add(curTable);
             rSQL.Next;
        end;
     finally
       rSQL.Active :=False;
     end;
end;

function TMM_DB_Firebird_Users.Get_Privileges(UserName: String;
  TableName: String): TFirebird_User_Privileges;
var
   curPrivilege :String;

begin
     try
        Result :=[];
        rSQL.Active :=False;
        rSQL.SQL.Text :='SELECT RDB$PRIVILEGE FROM RDB$USER_PRIVILEGES '+
          'WHERE ((RDB$USER=:UserName) AND (RDB$RELATION_NAME=:TableName))';
        rSQL.Params.ParamByName('UserName').AsString :=UserName;
        rSQL.Params.ParamByName('TableName').AsString :=TableName;
        rSQL.Open;
        while not(rSQL.EOF) do
        begin
             curPrivilege :=Trim(rSQL.FieldByName('RDB$PRIVILEGE').AsString);
             Case curPrivilege of
             'S': Result :=Result + [upSelect];
             'I': Result :=Result + [upInsert];
             'U': Result :=Result + [upUpdate];
             'D': Result :=Result + [upDelete];
             'R': Result :=Result + [upReferences];
             end;
             rSQL.Next;
        end;
     finally
       rSQL.Active :=False;
     end;
end;

procedure TMM_DB_Firebird_Users.Grant_Privileges(UserName: String;
  TableNames: TStringList; AUser_Privileges: TFirebird_User_Privileges);
var
   curPrivilege: String;
   xTableNames: TStringList;
   query: String;
   i: Integer;

begin
     try
        if (TableNames = nil)
        then begin
                  xTableNames :=TStringList.Create;
                  Get_Tables(xTableNames, True);
              end
        else xTableNames :=TableNames;

        curPrivilege :=User_Privileges_TO_GRANTSTR(AUser_Privileges);

        rSQL.Active :=False;

        for i:=0 to xTableNames.Count-1 do
        begin
          query :='GRANT '+curPrivilege+
          ' ON '+FieldNamesQuoteChars[0]+xTableNames[i]+FieldNamesQuoteChars[1]+
          ' TO '+FieldNamesQuoteChars[0]+UserName+FieldNamesQuoteChars[1];

          rIBConnection.ExecuteDirect(query);
        end;

        TSQLTransaction(rIBConnection.Transaction).Commit;
     finally
       rSQL.Active :=False;
       if (TableNames = nil)
       then xTableNames.Free;
     end;
end;

procedure TMM_DB_Firebird_Users.Revoke_Privileges(UserName: String;
  TableNames: TStringList; AUser_Privileges: TFirebird_User_Privileges;
  NotIncluded: Boolean);
var
   curPrivilege :String;
   xTableNames  :TStringList;
   query :String;
   i     :Integer;

begin
     try
        if (TableNames = nil)
        then begin
                  xTableNames :=TStringList.Create;
                  Get_Tables(xTableNames, True);
              end
        else xTableNames :=TableNames;

        if (NotIncluded)
        then curPrivilege :=User_Privileges_TO_GRANTSTR_NOT(AUser_Privileges)
        else curPrivilege :=User_Privileges_TO_GRANTSTR(AUser_Privileges);

        rSQL.Active :=False;

        for i:=0 to xTableNames.Count-1 do
        begin
          query :='REVOKE '+curPrivilege+
          ' ON '+FieldNamesQuoteChars[0]+xTableNames[i]+FieldNamesQuoteChars[1]+
          ' FROM '+FieldNamesQuoteChars[0]+UserName+FieldNamesQuoteChars[1];

          rIBConnection.ExecuteDirect(query);
        end;

        TSQLTransaction(rIBConnection.Transaction).Commit;
     finally
       rSQL.Active :=False;
       if (TableNames = nil)
       then xTableNames.Free;
     end;
end;

procedure TMM_DB_Firebird_Users.Grant_Revoke_Privileges(UserName: String;
  TableNames: TStringList; AUser_Privileges: TFirebird_User_Privileges);
var
   curPrivilegeGrant,
   curPrivilegeRevoke: String;
   xTableNames: TStringList;
   query: String;
   i: Integer;

begin
     try
        if (TableNames = nil)
        then begin
                  xTableNames :=TStringList.Create;
                  Get_Tables(xTableNames, True);
              end
        else xTableNames :=TableNames;

        curPrivilegeGrant :=User_Privileges_TO_GRANTSTR(AUser_Privileges);
        curPrivilegeRevoke :=User_Privileges_TO_GRANTSTR_NOT(AUser_Privileges);

        rSQL.Active :=False;

        for i:=0 to xTableNames.Count-1 do
        begin
          if (curPrivilegeGrant <> '') then
          begin
               query :='GRANT '+curPrivilegeGrant+
                ' ON '+FieldNamesQuoteChars[0]+xTableNames[i]+FieldNamesQuoteChars[1]+
                ' TO '+FieldNamesQuoteChars[0]+UserName+FieldNamesQuoteChars[1];

               rIBConnection.ExecuteDirect(query);
          end;
          if (curPrivilegeRevoke <> '') then
          begin
               query :='REVOKE '+curPrivilegeRevoke+
                ' ON '+FieldNamesQuoteChars[0]+xTableNames[i]+FieldNamesQuoteChars[1]+
                ' FROM '+FieldNamesQuoteChars[0]+UserName+FieldNamesQuoteChars[1];

               rIBConnection.ExecuteDirect(query);
          end;
        end;

        TSQLTransaction(rIBConnection.Transaction).Commit;
     finally
       rSQL.Active :=False;
       if (TableNames = nil)
       then xTableNames.Free;
     end;
end;

function FB_SERVER_GetVersion(AHostName, AUserName, APassword: String; verInfo: PFB_VersionInfo): Integer;
var
   sVer,
   curStr: String;
   i, curPart: Integer;

begin
  Result :=-1;

  sVer :=FB_SERVER_GetVersion(AHostName, AUserName, APassword);

  //WI-V3.0.0.32483 Firebird 3.0
  //LI-V3.0.0.32483 Firebird 3.0
  //WI-V2.5.2.26540 Firebird 2.5
  curPart :=0;
  curStr :='';
  for i :=1 to length(sVer) do
  begin
    Case curPart of
    0:begin
        if (sVer[i] <> '-')
        then curStr :=curStr+sVer[i]
        else begin
               if (verInfo <> nil)
               then verInfo^.vPlatform :=curStr;

               curStr :=''; inc(curPart);
             end;
      end;
    1:begin
        if (verInfo <> nil)
        then verInfo^.vBuildType :=sVer[i];

        curStr :=''; inc(curPart);
      end;
    2..5:begin
           if (sVer[i] in ['0'..'9'])
           then curStr :=curStr+sVer[i]
           else begin
                  Case curPart of
                  2: begin
                       Result :=StrToInt(curStr);
                       if (verInfo <> nil)
                       then verInfo^.vMajor :=Result
                       else exit; //if we need only the Major we can Exit now
                     end;
                  3: if (verInfo <> nil)  //if is Nil we may not be here, but i'm paranoic
                     then verInfo^.vMinor :=StrToInt(curStr);
                  4: if (verInfo <> nil)
                     then verInfo^.vPatch :=StrToInt(curStr);
                  5: if (verInfo <> nil)
                     then verInfo^.vBuild :=StrToInt(curStr);
                  end;

                  curStr :=''; inc(curPart);
                end;
         end;
    6:begin
        if not(sVer[i] in ['0'..'9'])
        then curStr :=curStr+sVer[i]
        else begin
               if (verInfo <> nil)
               then verInfo^.vApp :=Trim(curStr);

               curStr :=sVer[i]; inc(curPart);  //This Time don't skip the separator, is part of the AppMajor
             end;
      end;
    7:begin
           if (sVer[i] in ['0'..'9'])
           then curStr :=curStr+sVer[i]
           else begin
                  if (verInfo <> nil)
                  then verInfo^.vAppMajor :=StrToInt(curStr);

                  curStr :=''; inc(curPart);
                end;
         end;
    8:begin
           if (sVer[i] in ['0'..'9'])
           then curStr :=curStr+sVer[i]
           else break; //Ignore other chars and break
         end;
    end;
  end;

  if (verInfo <> nil) and (curPart = 8)
  then verInfo^.vAppMinor :=StrToInt(curStr);
end;

function FB_SERVER_GetVersion(AHostName, AUserName, APassword: String): String; overload;
var
   theServer: TFBAdmin;

begin
  try
     Result :='';
     theServer :=CreateFBAdmin(AHostName, AUserName, APassword);
     if theServer.Connect then
     begin
       Result :=theServer.ServerVersion;
     end;
  finally
     FreeAndNil(theServer);
  end;
end;

end.

