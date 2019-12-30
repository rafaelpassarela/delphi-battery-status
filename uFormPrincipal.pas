unit uFormPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  System.IniFiles, Vcl.ExtCtrls, Vcl.Samples.Gauges, System.DateUtils;

type
  TForm1 = class(TForm)
    TimerStatus: TTimer;
    LabelInterval: TLabel;
    SpinEditInterval: TSpinEdit;
    CheckBoxStayOnTop: TCheckBox;
    GaugeStatus: TGauge;
    LabelStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SpinEditIntervalChange(Sender: TObject);
    procedure CheckBoxStayOnTopClick(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FConfigName : string;
    FLastPercent : Integer;
    FLastTime : TDateTime;
    FStatusMens : string;
    procedure SaveIni;
    procedure LoadIni;

    procedure DoSave;
    procedure UpdateStatusControls;
    procedure GetBatteryLevel;

    function SecToTime(const ASec : Cardinal) : string;
    function GetBatteryFlag(const AFlag : Integer) : string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

const
  C_COR_SEM_BATERIA = $00AEFFFF;
  C_COR_BATERIA_CARREGANDO = $00E1FFD2;
  C_COR_BATERIA_CRITICA = $008080FF;


{$R *.dfm}

procedure TForm1.CheckBoxStayOnTopClick(Sender: TObject);
begin
  DoSave;
end;

procedure TForm1.DoSave;
begin
  if Self.Active then
    SaveIni;

  UpdateStatusControls;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConfigName := StringReplace(Application.ExeName, '.exe', '.ini', [rfIgnoreCase]);
  LoadIni;

  UpdateStatusControls;
  FLastPercent := -1;
  TimerStatusTimer( TimerStatus );
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TimerStatus.Enabled := True;
end;

function TForm1.GetBatteryFlag(const AFlag: Integer): string;
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

procedure TForm1.GetBatteryLevel;
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

  LabelStatus.Caption := lStatus + sLineBreak
    + 'Life Time: ' + SecToTime(lSysPowerStatus.BatteryLifeTime) + sLineBreak
    + 'Full Life Time: ' + SecToTime(lSysPowerStatus.BatteryFullLifeTime) + sLineBreak
    + GetBatteryFlag(lSysPowerStatus.BatteryFlag) + sLineBreak
    + FStatusMens;
end;

procedure TForm1.LoadIni;
begin
  with TIniFile.Create(FConfigName) do
  try
    SpinEditInterval.Value := ReadInteger('config', 'interval', 3);
    CheckBoxStayOnTop.Checked := ReadBool('config', 'stay.on.top', True);
  finally
    Free;
  end;
end;

procedure TForm1.SaveIni;
begin
  with TIniFile.Create(FConfigName) do
  try
    WriteInteger('config', 'interval', SpinEditInterval.Value);
    WriteBool('config', 'stay.on.top', CheckBoxStayOnTop.Checked);
  finally
    Free;
  end;
end;

function TForm1.SecToTime(const ASec: Cardinal): string;
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

procedure TForm1.SpinEditIntervalChange(Sender: TObject);
begin
  DoSave;
end;

procedure TForm1.TimerStatusTimer(Sender: TObject);
var
  lTimeDif : Int64;
  lPercDif : Integer;
  lRest : Integer;
begin
  GetBatteryLevel;
  if FLastPercent <> GaugeStatus.Progress then
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

procedure TForm1.UpdateStatusControls;
begin
  TimerStatus.Interval := SpinEditInterval.Value * 1000;
  if CheckBoxStayOnTop.Checked then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;
end;

end.
