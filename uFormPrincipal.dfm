object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Battery Status'
  ClientHeight = 201
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    259
    201)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelInterval: TLabel
    Left = 8
    Top = 11
    Width = 179
    Height = 13
    Caption = 'Tempo de Atualiza'#231#227'o (em segundos)'
  end
  object GaugeStatus: TGauge
    Left = 8
    Top = 64
    Width = 242
    Height = 57
    Progress = 0
  end
  object LabelStatus: TLabel
    Left = 8
    Top = 127
    Width = 242
    Height = 67
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'Aguardando...'
    WordWrap = True
    ExplicitHeight = 41
  end
  object SpinEditInterval: TSpinEdit
    Left = 193
    Top = 8
    Width = 57
    Height = 22
    MaxLength = 3
    MaxValue = 300
    MinValue = 1
    TabOrder = 0
    Value = 3
    OnChange = SpinEditIntervalChange
  end
  object CheckBoxStayOnTop: TCheckBox
    Left = 8
    Top = 30
    Width = 97
    Height = 17
    Caption = 'Sempre no Topo'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBoxStayOnTopClick
  end
  object TimerStatus: TTimer
    Enabled = False
    OnTimer = TimerStatusTimer
    Left = 128
    Top = 32
  end
end
