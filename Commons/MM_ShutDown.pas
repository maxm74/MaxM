//******************************************************************************
//***                     COMMON FUNCTIONS                                   ***
//***                                                                        ***
//***        (c) Massimo Magnano 2007-2025                                   ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_ShutDown.pas
//
//  Description : Functions to ShutDown/Reboot/Hibernate/Lock Windows.
//
//******************************************************************************

unit MM_ShutDown;

interface

uses SysUtils, Windows;

const
  EWX_HYBRID_SHUTDOWN         =$00400000;
  EWX_BOOTOPTIONS             =$01000000;
  EWX_ARSO                    =$04000000;
  EWX_CHECK_SAFE_FOR_SERVER   =$08000000;
  EWX_SYSTEM_INITIATED        =$10000000;

type
    TShutDownMode = (
      sdm_SHUTDOWN,
      sdm_REBOOT,
      sdm_LOGOFF,
      sdm_LOCK,
      sdm_SUSPEND,
      sdm_HIBERNATE
    );


function ShutDown(AMode: TShutDownMode; Force: Boolean = False; AddFlag: Cardinal = 0): Boolean;

implementation

function LockWorkStation: BOOL; external 'user32' name 'LockWorkStation';
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: ByteBool): ByteBool; external 'powrprof' name 'SetSuspendState';

procedure AdjustPrivileges;
Var
   hToken :THandle;
   tkp    :TTokenPrivileges;

begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    // Get a token for this process.
    if Not(OpenProcessToken(GetCurrentProcess,
                            TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken))
    then Exit;

    //Get the LUID for the shutdown privilege.
    LookupPrivilegeValue(Nil, SE_SHUTDOWN_NAME, tkp.Privileges[0].Luid);

    tkp.PrivilegeCount := 1;  // one privilege to set
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

    //Get the shutdown privilege for this process.
    AdjustTokenPrivileges(hToken, false, @tkp, 0, nil, nil);
  end;
end;

function ShutDown(AMode: TShutDownMode; Force: Boolean; AddFlag: Cardinal): Boolean;
var
   flag: Cardinal;

begin
  Case AMode of
    sdm_SHUTDOWN: begin
       Case Win32Platform of
       VER_PLATFORM_WIN32_WINDOWS : flag:= AddFlag or EWX_SHUTDOWN;
       VER_PLATFORM_WIN32_NT      : flag:= AddFlag or EWX_POWEROFF;
       end;
       if Force then flag:= flag or EWX_FORCE;
       AdjustPrivileges;
       Result:= ExitWindowsEx(flag, 0);
    end;
    sdm_REBOOT: begin
       flag:= AddFlag or EWX_REBOOT;
       if Force then flag:= flag or EWX_FORCE;
       AdjustPrivileges;
       Result:= ExitWindowsEx(flag, 0);
    end;
    sdm_LOGOFF: begin
       flag:= AddFlag or EWX_LOGOFF;
       if Force then flag:= flag or EWX_FORCE;
       AdjustPrivileges;
       Result:= ExitWindowsEx(flag, 0);
    end;
    sdm_LOCK: Result:= LockWorkStation;
    sdm_SUSPEND: begin
       AdjustPrivileges;
       Result:= SetSuspendState(False, Force, False);
    end;
    sdm_HIBERNATE: begin
       AdjustPrivileges;
       Result:= SetSuspendState(True, Force, False);
    end;
  end;
end;


end.
