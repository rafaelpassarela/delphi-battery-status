program BatteryStatus;

uses
  Vcl.Forms,
  uFormPrincipal in 'uFormPrincipal.pas' {FormBatteryStatus},
  uPowerManagement in 'uPowerManagement.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBatteryStatus, FormBatteryStatus);
  Application.Run;
end.
