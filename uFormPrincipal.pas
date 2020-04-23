unit uFormPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  System.IniFiles, Vcl.ExtCtrls, Vcl.Samples.Gauges, System.DateUtils,
  Vcl.Buttons, System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList,
  uPowerManagement;

type
  TPinMode = (piDefault, piOn, piOff);

  TFormBatteryStatus = class(TForm)
    PanelActions: TPanel;
    SpeedButtonPowerOff: TSpeedButton;
    SpeedButtonRestart: TSpeedButton;
    SpeedButtonHibernate: TSpeedButton;
    SpeedButtonSuspend: TSpeedButton;
    PanelNormal: TPanel;
    LabelInterval: TLabel;
    GaugeStatus: TGauge;
    LabelStatus: TLabel;
    SpinEditInterval: TSpinEdit;
    CheckBoxStayOnTop: TCheckBox;
    TimerStatus: TTimer;
    ImageListActions: TImageList;
    ActionListPoweControl: TActionList;
    ActionPowerOff: TAction;
    ActionSuspend: TAction;
    ActionHibernate: TAction;
    ActionRestart: TAction;
    ImageListModes: TImageList;
    ActionListModes: TActionList;
    ActionPinOff: TAction;
    ActionPinOn: TAction;
    SpeedButtonMode: TSpeedButton;
    PanelReduzido: TPanel;
    GaugeStatusReduzido: TGauge;
    LabelStatusReduzido: TLabel;
    SpeedButtonClose: TSpeedButton;
    ActionClose: TAction;
    procedure FormCreate(Sender: TObject);
    procedure SpinEditIntervalChange(Sender: TObject);
    procedure CheckBoxStayOnTopClick(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionPowerOffExecute(Sender: TObject);
    procedure ActionSuspendExecute(Sender: TObject);
    procedure ActionRestartExecute(Sender: TObject);
    procedure ActionHibernateExecute(Sender: TObject);
    procedure ActionPinOffExecute(Sender: TObject);
    procedure ActionPinOnExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FConfigName : string;
    FLastPercent : Integer;
    FLastTime : TDateTime;
    FStatusMens : string;
    FPinMode : TPinMode;
    procedure SaveIni;
    procedure LoadIni;

    procedure DoSave;
    procedure UpdateStatusControls;
    procedure GetBatteryLevel;

    function ShowConfirmatinDialog(const AMessage : string) : Boolean;
    function SecToTime(const ASec : Cardinal) : string;
    function GetBatteryFlag(const AFlag : Integer) : string;
    procedure SetPinMode(const Value: TPinMode);
  public
    { Public declarations }
    property PinMode : TPinMode read FPinMode write SetPinMode;
  end;

var
  FormBatteryStatus: TFormBatteryStatus;

implementation

const
  C_COR_SEM_BATERIA = $00AEFFFF;
  C_COR_BATERIA_CARREGANDO = $00E1FFD2;
  C_COR_BATERIA_CRITICA = $008080FF;


{$R *.dfm}

procedure TFormBatteryStatus.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormBatteryStatus.ActionHibernateExecute(Sender: TObject);
begin
  if ShowConfirmatinDialog('Deseja realmente hibernar?') then
    TPowerManagement.Hibernate;
end;

procedure TFormBatteryStatus.ActionPinOffExecute(Sender: TObject);
begin
  // off deixa on
  PinMode := piOn;
end;

procedure TFormBatteryStatus.ActionPinOnExecute(Sender: TObject);
begin
  // on deixa off
  PinMode := piOff;
end;

procedure TFormBatteryStatus.ActionPowerOffExecute(Sender: TObject);
begin
  if ShowConfirmatinDialog('Deseja realmente desligar?') then
    TPowerManagement.PowerOff;
end;

procedure TFormBatteryStatus.ActionRestartExecute(Sender: TObject);
begin
  if ShowConfirmatinDialog('Deseja realmente reiniciar?') then
    TPowerManagement.Restart;
end;

procedure TFormBatteryStatus.ActionSuspendExecute(Sender: TObject);
begin
  if ShowConfirmatinDialog('Deseja realmente suspender?') then
    TPowerManagement.Suspend;
end;

procedure TFormBatteryStatus.CheckBoxStayOnTopClick(Sender: TObject);
begin
  DoSave;
end;

procedure TFormBatteryStatus.DoSave;
begin
  if Self.Active then
    SaveIni;

  UpdateStatusControls;
end;

procedure TFormBatteryStatus.FormCreate(Sender: TObject);
begin
  PanelReduzido.Left := PanelNormal.Left;
  PanelReduzido.Top := PanelNormal.Top;

  FPinMode := piDefault;

  FConfigName := StringReplace(Application.ExeName, '.exe', '.ini', [rfIgnoreCase]);

  LoadIni;

  UpdateStatusControls;
  FLastPercent := -1;
  TimerStatusTimer( TimerStatus );

  ActionHibernate.Enabled := TPowerManagement.CanHibernate;
  ActionSuspend.Enabled := TPowerManagement.CanSuspend;
  ActionPowerOff.Enabled := TPowerManagement.CanShutdown;
end;

procedure TFormBatteryStatus.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
  end;
end;

procedure TFormBatteryStatus.FormShow(Sender: TObject);
begin
  TimerStatus.Enabled := True;
end;

function TFormBatteryStatus.GetBatteryFlag(const AFlag: Integer): string;
var
  lStatusColor : TColor;
begin
  lStatusColor := clBtnFace;

  case AFlag of
    1: Result := 'Bateria está saudável.';
    2: Result := 'A bateria está funcionando com pouca energia!';
    4: begin
         Result := 'Bateria está no crítico, por favor, carregue!!!';
         lStatusColor := C_COR_BATERIA_CRITICA;
       end;
    8,
    10: begin
         Result := 'Bateria está carregando...';
         lStatusColor := C_COR_BATERIA_CARREGANDO;
       end;
    9: Result := 'Bateria está saudável e carregando...';
    128:
      begin
        Result := 'O sistema não tem bateria.';
        lStatusColor := C_COR_SEM_BATERIA;
      end;
  else
    Result := Format('Desconhecido [%d].', [AFlag]);
  end;

  if (not (AFlag in [8,10])) and (GaugeStatus.Progress <= 10) then
    lStatusColor := C_COR_BATERIA_CRITICA;

  // avoid form blink
  if Self.Color <> lStatusColor then
    Self.Color := lStatusColor;
end;

procedure TFormBatteryStatus.GetBatteryLevel;
var
  lSysPowerStatus: TSystemPowerStatus;
  lStatus: string;
begin
  GetSystemPowerStatus(lSysPowerStatus);

  if Boolean(lSysPowerStatus.ACLineStatus) then
    lStatus := 'Não está rodando na bateria.'
  else
    lStatus := 'Rodando na bateria';

  GaugeStatus.Progress := lSysPowerStatus.BatteryLifePercent;
  GaugeStatusReduzido.Progress := GaugeStatus.Progress;

  LabelStatus.Caption := lStatus + sLineBreak
    + 'Life Time: ' + SecToTime(lSysPowerStatus.BatteryLifeTime) + sLineBreak
    + 'Full Life Time: ' + SecToTime(lSysPowerStatus.BatteryFullLifeTime) + sLineBreak
    + GetBatteryFlag(lSysPowerStatus.BatteryFlag) + sLineBreak
    + FStatusMens;

  LabelStatusReduzido.Caption := FStatusMens;
  LabelStatusReduzido.Hint := LabelStatus.Caption;
end;

procedure TFormBatteryStatus.LoadIni;
begin
  with TIniFile.Create(FConfigName) do
  try
    SpinEditInterval.Value := ReadInteger('config', 'interval', 3);
    CheckBoxStayOnTop.Checked := ReadBool('config', 'stay.on.top', True);
    PinMode := TPinMode(ReadInteger('config', 'pin.mode', 0));
  finally
    Free;
  end;
end;

procedure TFormBatteryStatus.SaveIni;
begin
  with TIniFile.Create(FConfigName) do
  try
    WriteInteger('config', 'interval', SpinEditInterval.Value);
    WriteInteger('config', 'pin.mode', Ord(PinMode));
    WriteBool('config', 'stay.on.top', CheckBoxStayOnTop.Checked);
  finally
    Free;
  end;
end;

function TFormBatteryStatus.SecToTime(const ASec: Cardinal): string;
var
  H, M, S: Cardinal;
begin
  if ASec <= 0 then
    Exit('N/A -')
  else if ASec > 5000000000 then
    Exit('N/A +');

   H := ASec div 3600;
   M := ASec div 60 - H * 60;
   S := ASec - (H * 3600 + M * 60);

   if H > 99 then
     H := 99;

   Result := Format('%2.2d:%2.2d:%2.2d', [H, M, S]);
end;

procedure TFormBatteryStatus.SetPinMode(const Value: TPinMode);
begin
  if (Value <> FPinMode) or (Value = piDefault) then
  begin
    FPinMode := Value;

    if Value = piOn then
    begin
      PanelNormal.Visible := False;
      PanelReduzido.Visible := True;
      SpeedButtonClose.Visible := True;
      SpeedButtonMode.Action := ActionPinOn;

      SpeedButtonMode.Top := 26;
      SpeedButtonClose.Top := 3;

      BorderStyle := bsNone;

      Self.Width := 528;
      Self.Height := 48;

      PanelActions.Left := 282;
      PanelActions.Top := 3;
    end else
    begin
      PanelNormal.Visible := True;
      PanelReduzido.Visible := False;
      SpeedButtonClose.Visible := False;
      SpeedButtonMode.Action := ActionPinOff;

      SpeedButtonMode.Top := 3;

      BorderStyle := bsSingle;

      Self.Width := 287;
      Self.Height := 297;

      PanelActions.Left := 33;
      PanelActions.Top := 220;
    end;

    DoSave;
  end;
end;

function TFormBatteryStatus.ShowConfirmatinDialog(
  const AMessage: string): Boolean;
begin
  Result := Application.MessageBox(PChar(AMessage), 'Atenção', MB_YESNO + MB_ICONWARNING) = IDYES;
end;

procedure TFormBatteryStatus.SpinEditIntervalChange(Sender: TObject);
begin
  DoSave;
end;

procedure TFormBatteryStatus.TimerStatusTimer(Sender: TObject);
var
  lTimeDif : Int64;
  lPercDif : Integer;
  lRest : Integer;
begin
  GetBatteryLevel;
  if (FLastPercent <> GaugeStatus.Progress) then
  begin
    if FLastPercent <> -1 then
    begin
      // quanto tempo demorou para avancar
      lTimeDif := SecondsBetween(Now, FLastTime);
      // carregando
      if GaugeStatus.Progress > FLastPercent then
      begin
        lPercDif := GaugeStatus.Progress - FLastPercent;
        lRest := 100 - GaugeStatus.Progress;
        FStatusMens := 'carga completa';
      end else
      begin
        lPercDif := FLastPercent - GaugeStatus.Progress;
        lRest := GaugeStatus.Progress;
        FStatusMens := 'descarregar.';
      end;

      lRest := (lRest * lTimeDif) * lPercDif;

      FStatusMens := Format('Aproximadamente %s para %s%s%d%% a cada %d segundo(s)', [
        SecToTime(lRest),
        FStatusMens,
        sLineBreak,
        lPercDif,
        lTimeDif ]);
    end;
    FLastPercent := GaugeStatus.Progress;
    FLastTime := Now;
  end;
end;

procedure TFormBatteryStatus.UpdateStatusControls;
var
  lTipoForm: TFormStyle;
begin
  TimerStatus.Interval := SpinEditInterval.Value * 1000;
  if CheckBoxStayOnTop.Checked then
    lTipoForm := fsStayOnTop
  else
    lTipoForm := fsNormal;

  if Self.FormStyle <> lTipoForm then
    Self.FormStyle := lTipoForm;
end;

end.
