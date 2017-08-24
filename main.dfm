object FormQuote: TFormQuote
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'GVOnlineQuoteHelper'
  ClientHeight = 300
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    500
    300)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelCity: TLabel
    Left = 135
    Top = 267
    Width = 57
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'LabelCity'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MemoLog: TMemo
    Left = 8
    Top = 8
    Width = 484
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ButtonClear: TButton
    Left = 417
    Top = 267
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Clear'
    TabOrder = 1
    OnClick = ButtonClearClick
  end
  object ButtonWebsite: TButton
    Left = 336
    Top = 267
    Width = 75
    Height = 25
    Caption = 'Website'
    TabOrder = 2
    OnClick = ButtonWebsiteClick
  end
  object ComboBoxServer: TComboBox
    Left = 8
    Top = 267
    Width = 121
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemIndex = 0
    TabOrder = 3
    Text = #49436#48260#47484' '#49440#53469#54616#49464#50836
    OnChange = ComboBoxServerChange
    Items.Strings = (
      #49436#48260#47484' '#49440#53469#54616#49464#50836
      #50640#51060#47112#45348
      #54260#46972#47532#49828
      #54764#47112#45348)
  end
  object TimerLoop: TTimer
    Enabled = False
    Interval = 250
    OnTimer = TimerLoopTimer
    Left = 16
    Top = 16
  end
end
