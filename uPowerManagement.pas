unit uPowerManagement;

{
  For some functions you need to get the right privileges
  on a Windows NT machine.
  (e.g: To shut down or restart windows with ExitWindowsEx or
  to change the system time)
  The following code provides a procedure to adjust the privileges.
  The AdjustTokenPrivileges() function enables or disables privileges
  in the specified access token.
}

{
  Um einige Funktionen unter Windows NT erfolgreich ausführen zu können,
  muss der ausführende Prozess die Privilegien dazu haben.
  (z.B um das System herunterzufahren mit ExitWindowsEx oder die
  Systemzeit zu ändern) AdjustTokenPrivileges() passt die Privilegien an.
}

interface

uses
  System.SysUtils, Winapi.Windows, Vcl.Forms, Winapi.Messages;

type
  TPowerManagement = class
  private
    // Enables or disables privileges debending on the bEnabled
    // Aktiviert oder deaktiviert Privilegien, abhängig von bEnabled
    class function NTSetPrivilege(sPrivilege: string; bEnabled: Boolean): Boolean;
  public
    class function CanShutdown : Boolean;
    class function CanSuspend : Boolean;
    class function CanHibernate : Boolean;

    class procedure PowerOff;
    class procedure Restart;
    class procedure Suspend;
    class procedure Hibernate;
    class procedure ScreenPowerOff;
  end;

// NT Defined Privileges from winnt.h
const
  SE_CREATE_TOKEN_NAME = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME = 'SeMachineAccountPrivilege';
  SE_TCB_NAME = 'SeTcbPrivilege';
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME = 'SeBackupPrivilege';
  SE_RESTORE_NAME = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
  SE_DEBUG_NAME = 'SeDebugPrivilege';
  SE_AUDIT_NAME = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME = 'SeManageVolumePrivilege';

implementation

function SetSuspendState(hibernate, forcecritical, disablewakeevent: boolean): boolean; stdcall; external 'powrprof.dll' name 'SetSuspendState';
function IsHibernateAllowed: boolean; stdcall; external 'powrprof.dll' name 'IsPwrHibernateAllowed';
function IsPwrSuspendAllowed: Boolean; stdcall; external 'powrprof.dll' name 'IsPwrSuspendAllowed';
function IsPwrShutdownAllowed: Boolean; stdcall; external 'powrprof.dll' name 'IsPwrShutdownAllowed';

{ TPowerManagement }

class function TPowerManagement.CanHibernate: Boolean;
begin
  Result := IsHibernateAllowed;
end;

class function TPowerManagement.CanShutdown: Boolean;
begin
  Result := IsPwrShutdownAllowed;
end;

class function TPowerManagement.CanSuspend: Boolean;
begin
  Result := IsPwrSuspendAllowed;
end;

class procedure TPowerManagement.Hibernate;
begin
  SetSuspendState(True, False, False);
end;

class function TPowerManagement.NTSetPrivilege(sPrivilege: string;
  bEnabled: Boolean): Boolean;
var
  hToken: THandle;
  TokenPriv: TOKEN_PRIVILEGES;
  PrevTokenPriv: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
begin
  Result := True;
  // Only for Windows NT/2000/XP and later.
  if not (Win32Platform = VER_PLATFORM_WIN32_NT) then
    Exit;

//  Result := False;

  // obtain the processes token
  if OpenProcessToken(GetCurrentProcess(),
    TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  begin
    try
      // Get the locally unique identifier (LUID) .
      if LookupPrivilegeValue(nil, PChar(sPrivilege),
        TokenPriv.Privileges[0].Luid) then
      begin
        TokenPriv.PrivilegeCount := 1; // one privilege to set

        case bEnabled of
          True: TokenPriv.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED;
          False: TokenPriv.Privileges[0].Attributes := 0;
        end;

        ReturnLength := 0; // replaces a var parameter
        PrevTokenPriv := TokenPriv;

        // enable or disable the privilege

        AdjustTokenPrivileges(hToken, False, TokenPriv, SizeOf(PrevTokenPriv),
          PrevTokenPriv, ReturnLength);
      end;
    finally
      CloseHandle(hToken);
    end;
  end;
  // test the return value of AdjustTokenPrivileges.
  Result := GetLastError = ERROR_SUCCESS;
  if not Result then
    raise Exception.Create(SysErrorMessage(GetLastError));

end;

class procedure TPowerManagement.PowerOff;
begin
  NTSetPrivilege(SE_SHUTDOWN_NAME, True);
  ExitWindowsEx(EWX_SHUTDOWN or EWX_FORCE, 0);
end;

class procedure TPowerManagement.Restart;
begin
  NTSetPrivilege(SE_SHUTDOWN_NAME, True);
  ExitWindowsEx(EWX_REBOOT or EWX_FORCE, 0);
end;

class procedure TPowerManagement.ScreenPowerOff;
const
  MONITOR_ON      = -1;
  MONITOR_OFF     =  2;
  MONITOR_STANDBY =  1;
begin
// To turn off the monitor
  SendMessage(Application.Handle, WM_SYSCOMMAND, SC_MONITORPOWER, MONITOR_OFF);

// To turn on the monitor
//  SendMessage(Application.Handle, WM_SYSCOMMAND, SC_MONITORPOWER, MONITOR_ON);

// To set the monitor on standby
//  SendMessage(Application.Handle, WM_SYSCOMMAND, SC_MONITORPOWER, MONITOR_STANDBY);
end;

class procedure TPowerManagement.Suspend;
begin
  SetSuspendState(False, False, False);
end;

end.
