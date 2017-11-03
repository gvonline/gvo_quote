object FormQuote: TFormQuote
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'GVOnlineQuoteHelper'
  ClientHeight = 373
  ClientWidth = 492
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    492
    373)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelCity: TLabel
    Left = 135
    Top = 340
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
    ExplicitTop = 267
  end
  object ButtonClear: TButton
    Left = 409
    Top = 340
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #51473#51648
    TabOrder = 0
    OnClick = ButtonClearClick
  end
  object ButtonWebsite: TButton
    Left = 328
    Top = 340
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Website'
    TabOrder = 1
    OnClick = ButtonWebsiteClick
  end
  object ComboBoxServer: TComboBox
    Left = 8
    Top = 340
    Width = 121
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemIndex = 0
    TabOrder = 2
    Text = #49436#48260#47484' '#49440#53469#54616#49464#50836
    OnChange = ComboBoxServerChange
    Items.Strings = (
      #49436#48260#47484' '#49440#53469#54616#49464#50836
      #50640#51060#47112#45348
      #54260#46972#47532#49828
      #54764#47112#45348)
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 476
    Height = 326
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = #49688#51665#44592
      DesignSize = (
        468
        298)
      object MemoLog: TMemo
        Left = 3
        Top = 3
        Width = 462
        Height = 292
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = #44160#49353
      ImageIndex = 1
      DesignSize = (
        468
        298)
      object LabelCities: TLabel
        Left = 8
        Top = 8
        Width = 33
        Height = 13
        Caption = #46020#49884#47749
      end
      object LabelGoods: TLabel
        Left = 8
        Top = 39
        Width = 33
        Height = 13
        Caption = #44368#50669#54408
      end
      object ButtonSearch: TButton
        Left = 392
        Top = 3
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #44160#49353
        Default = True
        Enabled = False
        TabOrder = 0
        OnClick = ButtonSearchClick
      end
      object EditCities: TEdit
        Left = 54
        Top = 5
        Width = 236
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object EditGoods: TEdit
        Left = 54
        Top = 36
        Width = 332
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object ListViewSearch: TListView
        Left = 3
        Top = 65
        Width = 464
        Height = 232
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <>
        ReadOnly = True
        RowSelect = True
        TabOrder = 3
        ViewStyle = vsReport
        OnColumnClick = ListViewSearchColumnClick
        OnCompare = ListViewSearchCompare
        OnCustomDrawSubItem = ListViewCustomDrawSubItem
        OnDblClick = ListViewSearchDblClick
      end
      object ButtonReset: TButton
        Left = 392
        Top = 34
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #52488#44592#54868
        Enabled = False
        TabOrder = 4
        OnClick = ButtonResetClick
      end
      object ComboBoxShortcut: TComboBox
        Left = 296
        Top = 5
        Width = 90
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        Enabled = False
        ItemIndex = 0
        TabOrder = 5
        Text = #51600#44200#52286#44592
        OnChange = ComboBoxShortcutChange
        Items.Strings = (
          #51600#44200#52286#44592)
      end
    end
  end
  object TimerLoop: TTimer
    Enabled = False
    Interval = 250
    OnTimer = TimerLoopTimer
    Left = 280
    Top = 336
  end
end
