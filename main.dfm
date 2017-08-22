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
    Left = 8
    Top = 270
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
    Top = 31
    Width = 484
    Height = 230
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
  object RadioButtonEirene: TRadioButton
    Left = 8
    Top = 8
    Width = 113
    Height = 17
    Caption = #50640#51060#47112#45348
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object RadioButtonPolaris: TRadioButton
    Left = 127
    Top = 8
    Width = 113
    Height = 17
    BiDiMode = bdLeftToRight
    Caption = #54260#46972#47532#49828
    Enabled = False
    ParentBiDiMode = False
    TabOrder = 3
  end
  object RadioButtonHelen: TRadioButton
    Left = 246
    Top = 8
    Width = 113
    Height = 17
    Caption = #54764#47112#45348
    Enabled = False
    TabOrder = 4
  end
  object ButtonWebsite: TButton
    Left = 336
    Top = 267
    Width = 75
    Height = 25
    Caption = 'Website'
    TabOrder = 5
    OnClick = ButtonWebsiteClick
  end
  object TimerLoop: TTimer
    Interval = 250
    OnTimer = TimerLoopTimer
    Left = 16
    Top = 16
  end
end
