object FormQuote: TFormQuote
  Left = 390
  Height = 400
  Top = 143
  Width = 500
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = '시세 도우미'
  ClientHeight = 400
  ClientWidth = 500
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  Font.CharSet = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '굴림체'
  Font.Pitch = fpFixed
  Font.Quality = fqDraft
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '1.6.4.0'
  object LabelCity: TLabel
    Left = 142
    Height = 12
    Top = 372
    Width = 54
    Anchors = [akLeft, akBottom]
    Caption = 'LabelCity'
    Font.CharSet = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '굴림체'
    Font.Pitch = fpFixed
    Font.Quality = fqDraft
    ParentColor = False
    ParentFont = False
  end
  object ButtonClear: TButton
    Left = 417
    Height = 25
    Top = 367
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = '중지'
    OnClick = ButtonClearClick
    TabOrder = 0
  end
  object ButtonWebsite: TButton
    Left = 336
    Height = 25
    Top = 367
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Website'
    OnClick = ButtonWebsiteClick
    TabOrder = 1
  end
  object ComboBoxServer: TComboBox
    Left = 8
    Height = 20
    Top = 368
    Width = 129
    Anchors = [akLeft, akBottom]
    ItemHeight = 12
    ItemIndex = 0
    Items.Strings = (
      '서버를 선택하세요'
      '에이레네'
      '폴라리스'
      '헬레네'
    )
    OnChange = ComboBoxServerChange
    Style = csDropDownList
    TabOrder = 2
    Text = '서버를 선택하세요'
  end
  object PageControl1: TPageControl
    Left = 8
    Height = 353
    Top = 8
    Width = 484
    ActivePage = TabSheet1
    Anchors = [akTop, akLeft, akRight, akBottom]
    TabIndex = 0
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = '수집기'
      ClientHeight = 327
      ClientWidth = 476
      object MemoLog: TMemo
        Left = 3
        Height = 317
        Top = 3
        Width = 469
        Anchors = [akTop, akLeft, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = '검색'
      ClientHeight = 327
      ClientWidth = 476
      ImageIndex = 1
      object LabelCities: TLabel
        Left = 8
        Height = 12
        Top = 8
        Width = 36
        Caption = '도시명'
        ParentColor = False
      end
      object LabelGoods: TLabel
        Left = 8
        Height = 12
        Top = 39
        Width = 36
        Caption = '교역품'
        ParentColor = False
      end
      object ButtonSearch: TButton
        Left = 392
        Height = 25
        Top = 3
        Width = 75
        Anchors = [akTop, akRight]
        Caption = '검색'
        Default = True
        Enabled = False
        OnClick = ButtonSearchClick
        TabOrder = 0
      end
      object EditCities: TEdit
        Left = 54
        Height = 20
        Top = 5
        Width = 236
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 1
      end
      object EditGoods: TEdit
        Left = 54
        Height = 20
        Top = 36
        Width = 332
        Anchors = [akTop, akLeft, akRight]
        TabOrder = 2
      end
      object ListViewSearch: TListView
        Left = 3
        Height = 257
        Top = 65
        Width = 469
        Anchors = [akTop, akLeft, akRight, akBottom]
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
        Height = 25
        Top = 34
        Width = 75
        Anchors = [akTop, akRight]
        Caption = '초기화'
        Enabled = False
        OnClick = ButtonResetClick
        TabOrder = 4
      end
      object ComboBoxShortcut: TComboBox
        Left = 296
        Height = 20
        Top = 5
        Width = 90
        Anchors = [akTop, akRight]
        Enabled = False
        ItemHeight = 12
        ItemIndex = 0
        Items.Strings = (
          '즐겨찾기'
        )
        OnChange = ComboBoxShortcutChange
        Style = csDropDownList
        TabOrder = 5
        Text = '즐겨찾기'
      end
    end
  end
  object TimerLoop: TTimer
    Enabled = False
    Interval = 250
    OnTimer = TimerLoopTimer
    left = 232
    top = 344
  end
end
