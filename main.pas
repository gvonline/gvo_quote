unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.JSON,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Generics.Collections,
  GameState, Request, Vcl.ComCtrls;

type
  TPixelsFunc = function(bitmap: TBitmap; X, Y: Integer): Word;

  TFormQuote = class(TForm)
    MemoLog: TMemo;
    ButtonClear: TButton;
    TimerLoop: TTimer;
    LabelCity: TLabel;
    ButtonWebsite: TButton;
    ComboBoxServer: TComboBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ListViewSearch: TListView;
    EditCities: TEdit;
    EditGoods: TEdit;
    ButtonSearch: TButton;
    LabelCities: TLabel;
    LabelGoods: TLabel;
    ButtonReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerLoopTimer(Sender: TObject);
    procedure ButtonWebsiteClick(Sender: TObject);
    procedure ComboBoxServerChange(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ListViewSearchColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewSearchCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ListViewSearchDblClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
  const
    GameClassName = 'Greate Voyages Online Game MainFrame';
    TitleOffsetX = 113;
    TitleOffsetY = -223;
    ConferenceOffsetX = 8;
    ConferenceOffsetY = 8;
    CityOffsetX = 34;
    CityOffsetY = 8;
    FlagOffsetX = 8;
    FlagOffsetY = 8;
    FlagWidth = 24;
    FlagHeight = 16;
    ReadPixelCount = 16;
    CaptureWatingTime = 3000;
  private
    { Private declarations }
    Request: TRequest;
    WindowList: TObjectDictionary<HWND, TGameState>;
    FontTable: array of array of Word;
    FontMap, FontIndex: array of Word;
    FontWidth: array of Byte;
    FontCount: Word;
    CityNames: TStringList;
    CurrentWindow: HWND;
    Delay: Integer;
    ColumnSortIndex: Integer;
    ColumnSortWas: Integer;
    ColumnSortAscending: Boolean;
    ColumnType: Integer;
    procedure LoadFont;
    procedure FindCityName(bmp: TBitmap);
    procedure FindChatMessage(bmp: TBitmap);
    procedure FindTrade(bmp: TBitmap);
    procedure WriteLog(msg: String);
    procedure WriteLogForColor(Bmp: TBitmap; pointX, increaseX, pointY, increaseY: Integer);
    function FindFlagImage(bmp: TBitmap): Integer;
    function GetText(bmp: TBitmap; X, Y, Width: Integer; PixelsFunc: TPixelsFunc): String;
    function GetServer: String;
    function NaturalOrderCompareString(const A1, A2: string; ACaseSensitive: Boolean): Integer;
    function GetPassedTimeString(const PassedTime: Int64): String;
    function GetQuoteStatusString(const QuoteStatus: Int8): String;
    function GetResistStatusString(const ResistStatus: Int8): String;

  public
    { Public declarations }
  end;

var
  FormQuote: TFormQuote;

implementation

{$R *.dfm}
{$POINTERMATH ON}

uses
  Math, StrUtils, ShellApi, DateUtils, TypInfo;

function GetWhitePoints(bmp: TBitmap; fromX, fromY: Integer): Word;
var
  count: Integer;
  Line: PRGBTriple;
  color: TColor;
begin
  result := 0;
  for count := 0 to 14 do
  begin
    Line := bmp.ScanLine[fromY + count];
    color := RGB(Line[fromX].rgbtRed, Line[fromX].rgbtGreen, Line[fromX].rgbtBlue);
    result := result shl 1;
    if color = clWhite then
    begin
      result := result or 1;
    end;
  end;
  result := result shl 1;
end;

function GetBlackPoints(bmp: TBitmap; fromX, fromY: Integer): Word;
var
  count: Integer;
  Line: PRGBTriple;
  color: TColor;
begin
  result := 0;
  for count := 0 to 14 do
  begin
    Line := bmp.ScanLine[fromY + count];
    color := RGB(Line[fromX].rgbtRed, Line[fromX].rgbtGreen, Line[fromX].rgbtBlue);
    result := result shl 1;
    if color = clBlack then
    begin
      result := result or 1;
    end;
  end;
  result := result shl 1;
end;

function GetBrightPoints(bmp: TBitmap; fromX, fromY: Integer): Word;
var
  count: Integer;
  Line: PRGBTriple;
  high: Byte;
begin
  result := 0;
  for count := 0 to 14 do
  begin
    Line := bmp.ScanLine[fromY + count];
    high := Max(Line[fromX].rgbtRed, Max(Line[fromX].rgbtGreen, Line[fromX].rgbtBlue));
    result := result shl 1;
    if high > 160 then
    begin
      result := result or 1;
    end;
  end;
  result := result shl 1;
end;

function ComparePixelColor(bmp: TBitmap; X, Y: Integer; R, G, B: Byte): Bool;
var
  chkR, chkG, chkB, count: Integer;
  Line: PRGBTriple;
begin
  result := False;
  Line := bmp.ScanLine[Y];
  if ABS(Integer(Line[X].rgbtRed) - R) > 8 then
    exit;
  if ABS(Integer(Line[X].rgbtGreen) - G) > 8 then
    exit;
  if ABS(Integer(Line[X].rgbtBlue) - B) > 8 then
    exit;
  result := True;
end;

function WordToBinStr(value: Word): String;
var
  length: Integer;
begin
  length := 16;
  SetLength(result, length);
  while length > 0 do
  begin
    if odd(value) then
      Result[length] := '1'
    else
      Result[length] := '0';
    value := value shr 1;
    dec(length);
  end;
end;

procedure TFormQuote.FormCreate(Sender: TObject);
var
  CityName: String;
  Latest, Version: String;
  ExeName: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
  Build: Word;
begin
  CurrentWindow := 0;
  WindowList := TObjectDictionary<HWND, TGameState>.Create([doOwnsValues]);
  MemoLog.Lines.Clear;
  LabelCity.Caption := '';
  //WriteLog('FormCreate');
  LoadFont;
  CityName := ExtractFilePath(Application.ExeName) + 'CityNames.txt';
  CityNames := TStringList.Create;
  CityNames.Sorted := True;
  CityNames.Duplicates := dupIgnore;
  CityNames.LoadFromFile(CityName);
  Request := TRequest.Create;

  ExeName := Application.ExeName;
  Size := GetFileVersionInfoSize(PChar(ExeName), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(ExeName), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Version := Format('%d.%d #%d', [
    HiWord(FixedPtr.dwProductVersionMS), //Major
    LoWord(FixedPtr.dwProductVersionMS), //Minor
    LoWord(FixedPtr.dwFileVersionLS)]); //Build
  Caption := Caption + ' ' + Version;

  Latest := Request.GetVersion;
  if Latest = '' then
  begin
    MemoLog.Lines.Add('');
    MemoLog.Lines.Add('      서버와 연결되지 않았습니다.');
    MemoLog.Lines.Add('');
    ComboBoxServer.Enabled := False;
  end
  else if Latest <> Version then
  begin
    MemoLog.Lines.Add('');
    MemoLog.Lines.Add('      새 버전이 있습니다.');
    MemoLog.Lines.Add('      웹사이트에서 다운받으세요.');
    MemoLog.Lines.Add('');
    ComboBoxServer.Enabled := False;
    ButtonClear.Enabled := False;
  end
  else
  begin
    MemoLog.Lines.Add('');
    MemoLog.Lines.Add('      환영합니다.');
    MemoLog.Lines.Add('      자세한 내용은 웹사이트를 확인하세요.');
    MemoLog.Lines.Add('');
  end;
end;

procedure TFormQuote.FormDestroy(Sender: TObject);
begin
  CurrentWindow := 0;
  WindowList.Free;
  CityNames.Free;
  Request.Terminate;
end;

procedure TFormQuote.ButtonResetClick(Sender: TObject);
var
  i: Integer;
begin
  EditCities.Text := '';
  EditGoods.Text := '';
  ListViewSearch.Clear;
  if ListViewSearch.Columns.Count > 0 then
  begin
    for i := ListViewSearch.Columns.Count - 1 downto 0 do
      ListViewSearch.Column[i].Destroy;
  end;
end;

procedure TFormQuote.ButtonSearchClick(Sender: TObject);
const
  ColumnNamesForDashboard: array[0..3] of String = ('도시명', '등록건수', '도시상태', '갱신시간');
  ColumnWidthForDashboard: array[0..3] of Integer = (    120,         60,        160,        100);
  ColumnNamesForCommon: array[0..8] of String = ('품명', '품목', '시세', '시세상태', '시세갱신시간', '도시명', '내성항', '도시상태', '상태갱신시간');
  ColumnWidthForCommon: array[0..8] of Integer = (   80,     50,     40,         60,            100,      100,       50,        150,            120);
var
  i, j, ColumnCount: Integer;
  JSONString, JSONPath, Text: String;
  JSONArray: TJSONArray;
  JSONRow: TJSONObject;
  ColumnNames: Tarray<String>;
  ColumnWidth: Tarray<Integer>;
begin
  ColumnType := 0;
  ColumnSortWas := -1;
  ColumnCount := 0;
  JSONString := Request.GetSearchResult(GetServer, EditCities.Text, EditGoods.Text);
  JSONArray := TJSONObject.ParseJSONValue(JSONString) as TJSONArray;
  for i := 0 to JSONArray.size - 1 do
  begin
    JSONRow := JSONArray.Get(i) as TJSONObject;
    ColumnCount := JSONRow.Count;
    SetLength(ColumnNames, ColumnCount);
    SetLength(ColumnWidth, ColumnCount);
    if ColumnCount = Length(ColumnNamesForCommon) then
    begin
      for j := 0 to ColumnCount -1 do
      begin
        ColumnType := 1;
        ColumnNames[j] := ColumnNamesForCommon[j];
        ColumnWidth[j] := ColumnWidthForCommon[j];
      end;
    end
    else
    begin
      for j := 0 to ColumnCount -1 do
      begin
        ColumnType := 2;
        ColumnNames[j] := ColumnNamesForDashboard[j];
        ColumnWidth[j] := ColumnWidthForDashboard[j];
      end;
    end;
    break;
  end;

  ListViewSearch.Clear;
  if ListViewSearch.Columns.Count > 0 then
  begin
    for i := ListViewSearch.Columns.Count - 1 downto 0 do
      ListViewSearch.Column[i].Destroy;
  end;

  if ColumnCount = 0 then
    exit;

  for i := 0 to ColumnCount - 1 do
  begin
    ListViewSearch.Columns.Add.Caption := ColumnNames[i];
    ListViewSearch.Columns.Items[i].Width := ColumnWidth[i];
  end;

  for i := 0 to JSONArray.size - 1 do
  begin
    with ListViewSearch.Items.Add do
    begin
      for j := 0 to ColumnCount - 1 do
      begin
        JSONPath := Format('[%d].%s', [i, ColumnNames[j]]);
        Text := JSONArray.GetValue<String>(JSONPath);

        if EndsStr('갱신시간', ColumnNames[j]) then
        begin
          if Text = '0' then
            Text := ''
          else if Text <> '' then
            Text := GetPassedTimeString(StrToInt(Text));
        end
        else if '시세상태' = ColumnNames[j] then
        begin
          if Text <> '' then
            Text := GetQuoteStatusString(StrToInt(Text));
        end
        else if '내성항' = ColumnNames[j] then
        begin
          if Text <> '' then
            Text := GetResistStatusString(StrToInt(Text));
        end;

        if j = 0 then
          Caption := Text
        else
          SubItems.Add(Text);
      end;
      for j := 0 to ColumnCount - 1 do
      begin
        JSONPath := Format('[%d].%s', [i, ColumnNames[j]]);
        SubItems.Add(JSONArray.GetValue<String>(JSONPath));
      end;
    end;
  end;
end;

procedure TFormQuote.ButtonWebsiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://gvonline.ga', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormQuote.ComboBoxServerChange(Sender: TObject);
begin
  ComboBoxServer.Enabled := (ComboBoxServer.ItemIndex <= 0);
  ButtonSearch.Enabled := not ComboBoxServer.Enabled;
  ButtonReset.Enabled := ButtonSearch.Enabled;
  TimerLoop.Enabled := not ComboBoxServer.Enabled;
end;

procedure TFormQuote.ButtonClearClick(Sender: TObject);
begin
  CurrentWindow := 0;
  WindowList.Clear;
  LabelCity.Caption := '';
  ComboBoxServer.Enabled := True;
  ButtonSearch.Enabled := not ComboBoxServer.Enabled;
  ButtonReset.Enabled := ButtonSearch.Enabled;
  ComboBoxServer.ItemIndex := 0;
  TimerLoop.Enabled := False;
  MemoLog.Lines.Clear;
  MemoLog.Lines.Add('');
  MemoLog.Lines.Add('      프로그램 동작을 일시적으로 중지 하였습니다.');
  MemoLog.Lines.Add('      서버를 선택 하여 주십시오.');
  MemoLog.Lines.Add('');
end;

procedure TFormQuote.TimerLoopTimer(Sender: TObject);
var
  ClassName: string;
  Handle: HWND;
  WindowRect: TRect;
  //WindowPoint: TPoint;
  DC: HDC;
  Bmp: TBitmap;
begin
  Handle := GetForegroundWindow;
  SetLength(ClassName, 255);
  SetLength(ClassName, GetClassName(Handle, PChar(ClassName), Length(ClassName)));

  if className <> GameClassName then
  begin
    exit;
  end;

  TimerLoop.Enabled := False;
  if Handle <> CurrentWindow then
  begin
    CurrentWindow := Handle;
    if WindowList.ContainsKey(Handle) then
    begin
      LabelCity.Caption := '도시명: ' + WindowList[Handle].CityName;
    end else begin
      WindowList.Add(CurrentWindow, TGameState.Create(CurrentWindow));
      LabelCity.Caption := '';
    end;
  end;
{
  WindowPoint.X := 0;
  WindowPoint.Y := 0;
  WinApi.Windows.GetClientRect(Handle, WindowRect);
  Winapi.Windows.ClientToScreen(Handle, WindowPoint);
  DC := GetWindowDC(GetDesktopWindow);
  Bmp := TBitmap.Create;
  Bmp.Height := WindowRect.Height;
  Bmp.Width := WindowRect.Width;
  Bmp.PixelFormat := pf24bit;
  BitBlt(Bmp.Canvas.Handle, 0, 0, WindowRect.Width, WindowRect.Height, DC, WindowPoint.X, WindowPoint.Y, SRCCOPY);
  ReleaseDC(Handle, DC);
}
  WinApi.Windows.GetClientRect(Handle, WindowRect);
  Bmp := TBitmap.Create;
  Bmp.Height := WindowRect.Height;
  Bmp.Width := WindowRect.Width;
  Bmp.PixelFormat := pf24bit;
  DC := GetDC(Handle);
  BitBlt(Bmp.Canvas.Handle, 0, 0, WindowRect.Width, WindowRect.Height, DC, 0, 0, SRCCOPY);
  ReleaseDC(Handle, DC);

  //WriteLog('FindCityName');
  FindCityName(Bmp);
  if (CurrentWindow = Handle) and (WindowList[Handle].CityName <> '') then
  begin
    //WriteLog('FindChatMessage');
    FindChatMessage(Bmp);
    //WriteLog('FindTrade');
    FindTrade(Bmp);
  end;
  //WriteLog('End');

  Bmp.Free;
  TimerLoop.Enabled := True;
end;

procedure TFormQuote.ListViewSearchColumnClick(Sender: TObject; Column: TListColumn);
begin
  ColumnSortIndex := Column.Index + ListViewSearch.Columns.Count;

  if ColumnSortWas = ColumnSortIndex then
    ColumnSortAscending := not ColumnSortAscending
  else
    ColumnSortAscending := False;

  ColumnSortWas := ColumnSortIndex;
  (Sender as TCustomListView).AlphaSort;
end;

procedure TFormQuote.ListViewSearchCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  i: Integer;
begin
  if ColumnSortIndex > 0 then
  begin
    i := ColumnSortIndex - 1;
    Compare := NaturalOrderCompareString(Item1.SubItems[i], Item2.SubItems[i], True);
  end
  else
  begin
    Compare := NaturalOrderCompareString(Item1.Caption, Item2.Caption, True);
  end;

  if not ColumnSortAscending then
    Compare := -Compare;
end;

procedure TFormQuote.ListViewSearchDblClick(Sender: TObject);
var
  HitTests: THitTests;
  HitTest: THitTest;
  Text, Temp: String;
  SubItems: TArray<String>;
  i, ColumnX, ClickX : Integer;
  ListViewCursosPos: TPoint;
  SelectedItem: TListItem;
  ScrollInfo: TScrollInfo;
begin
  if ColumnType = 0 then
    exit;

  SelectedItem := ListViewSearch.Selected;
  if SelectedItem = nil then
    exit;

  ListViewCursosPos := ListViewSearch.ScreenToClient(Mouse.CursorPos);
  HitTests := ListViewSearch.GetHitTestInfoAt(ListViewCursosPos.X, ListViewCursosPos.Y);

  {
  Text := '';
  for HitTest in HitTests do
  begin
    Temp := GetEnumName(TypeInfo(THitTest), Integer(HitTest));
    Text := Format('%s %s | ', [Text, Temp]);
  end;
  WriteLog(Format('[%d, %d] %s', [ListViewCursosPos.X, ListViewCursosPos.Y, Text]));
  }

  if HitTests <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon] then
  begin
    Text := SelectedItem.Caption;
    SubItems := SelectedItem.SubItems.ToStringArray;
  end
  else
    exit;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(ListViewSearch.Handle, SBS_HORZ, ScrollInfo);
  ClickX := ListViewCursosPos.X + ScrollInfo.nPos;

  i := 0;
  ColumnX := 0;
  if ColumnType = 1 then
  begin
    for i := 0 to ListViewSearch.Columns.Count - 1 do
    begin
      if ListViewSearch.Columns.Items[i].Caption = '도시명' then
        break;
      ColumnX := ColumnX + ListViewSearch.Columns.Items[i].Width;
    end;
  end;

  if ColumnType = 2 then
  begin
    EditCities.Text := Text;
    EditGoods.Text := '';
  end
  else if ColumnType = 1 then
  begin
    if (i = 0) or (ClickX < ColumnX) then
    begin
      EditCities.Text := '';
      EditGoods.Text := Text;
    end
    else
    begin
      EditCities.Text := SubItems[i - 1];
      EditGoods.Text := '';
    end;
  end;
  ButtonSearchClick(nil);
end;

{ PRIVATE }

procedure TFormQuote.LoadFont;
var
  FontName: String;
  FontFile: TFileStream;
  i, ch: Word;
  width: Byte;
begin
  FontName := ExtractFilePath(Application.ExeName) + 'font.dat';
  FontFile := TFileStream.Create(FontName, fmOpenRead);
  FontFile.Read(FontCount, 2);

  SetLength(FontTable, FontCount, 16);
  SetLength(FontMap, FontCount);
  SetLength(FontWidth, FontCount);
  SetLength(FontIndex, 65536);
  FillChar(FontIndex[0], SizeOf(Word) * Length(FontIndex), $FF);

  for i := 0 to FontCount - 1 do
  begin
    FontFile.Read(ch, 2);
    FontFile.Read(width, 1);
    FontFile.Read(FontTable[i, 0], width * 2);

    FontMap[i] := ch;
    FontIndex[ch] := i;
    FontWidth[i] := width;
  end;
  FontFile.Free;

  //WriteLog(Format('load font, font count: %d', [FontCount]));
end;

procedure TFormQuote.FindCityName(bmp: TBitmap);
var
  flagX, pointX, pointY, count, step, item, charAt, index, width: Integer;
  ch: Word;
  fail: array of Bool;
  font: array[0..15] of Word;
  city: WideString;
  check: Bool;
begin
  if CurrentWindow = 0 then
    exit;

  flagX := FindFlagImage(bmp);

  if flagX = 0 then
  begin
    // Screen is fade out
    WindowList[CurrentWindow].CityName := '';
    LabelCity.Caption := '';
  end;

  if flagX <= 0 then
    exit;

  if flagX = FlagOffsetX then
  begin
    pointX := CityOffsetX;
    pointY := CityOffsetY;
  end else begin
    pointX := ConferenceOffsetX;
    pointY := ConferenceOffsetY;
  end;

  count := CityNames.Count;
  SetLength(fail, count);
  FillChar(fail[0], SizeOf(Bool) * Length(fail), 0);
  charAt := 0;

  while count > 1 do
  begin
    check := True;
    for step := 0 to 15 do
    begin
      font[step] := getWhitePoints(bmp, pointX + step, pointY);
      check := check and (font[step] <> $FFFE);
    end;
    if not check then
    begin
      // font is all white(is impossible)
      //WriteLog('FindCityName: font is all white(is impossible)');
      count := 0;
      break;
    end;

    for item := 0 to Length(fail) - 1 do
    begin
      if fail[item] then
        continue;

      city := CityNames[item];
      if Length(city) < (charAt + 1) then
      begin
        fail[item] := True;
        continue;
      end;
      ch := Word(city[charAt + 1]);
      index := FontIndex[ch];
      if index = $FFFF then
      begin
        WriteLog('invalid font: ' + IntToHex(ch, 4));
        continue;
      end;
      width := FontWidth[index];

      for step := 0 to width - 1 do
      begin
        if (FontTable[index, step] and font[step]) <> FontTable[index, step] then
        begin
          fail[item] := True;
          dec(count);
          break;
        end;
      end;
    end;

    inc(pointX, 16);
    inc(charAt);
  end;

  if count <> 1 then
  begin
    WindowList[CurrentWindow].CityName := '';
    LabelCity.Caption := '';
    exit;
  end;

  for item := 0 to Length(fail) - 1 do
  begin
    if fail[item] then
      continue;
    //WriteLog('FindCityName: ' + CityNames[item]);
    LabelCity.Caption := '도시명: ' + CityNames[item];
    WindowList[CurrentWindow].CityName := CityNames[item];
    break;
  end;
end;

procedure TFormQuote.FindChatMessage(bmp: TBitmap);
var
  State: TGameState;
  pointX, pointY, limitY, line: Integer;
  log, status: String;
begin
  if CurrentWindow = 0 then
    exit;

  limitY := bmp.Height - 32;
  pointY := limitY - 20 * 5;
  for line := 0 to 4 do
  begin
    pointX := 12;
    log := GetText(bmp, pointX, pointY, 568, @GetBrightPoints);

    if StartsStr('교역소주인      ：', log) then
    begin
      status := Trim(Copy(log, 13, Length(log)));
    end;

    if StartsStr('교역소의 도제   ：', log) then
    begin
      status := Trim(Copy(log, 12, Length(log)));
    end;

    if StartsStr('항구관리        ：', log) or
       StartsStr('항구안내원      ：', log) or
       StartsStr('역장            ：', log) or
       StartsStr('마부            ：', log) then
    begin
      status := '';
    end;

    //WriteLog(Trim(log));
    inc(pointY, 20);
  end;

  State := WindowList[CurrentWindow];
  if (status <> '') and (State.CityName <> '') and
    (State.CityStatus <> (State.CityName + '::' + status)) then
  begin
    State.CityStatus := State.CityName + '::' + status;
    WriteLog(State.CityName + ' 교역소 :: ' + status);
    Request.SendCityInfo(GetServer, State.CityName, status);
  end;
end;

procedure TFormQuote.FindTrade(bmp: TBitmap);
var
  State: TGameState;
  centerX, centerY, pointX, pointY, limitY, Selected, item, X, Status: Integer;
  Title, Name, Quote, StatusString: String;
  QuoteNum: Integer;
  TopLine, BottomLine: PRGBTriple;
  TopColor, BottomColor, LineColor: TColor;
  r, g, b: Byte;
  match: Bool;
begin
  if CurrentWindow = 0 then
    exit;

  State := WindowList[CurrentWindow];
  if State.CityName = '' then
  begin
    exit;
  end;

  centerX := bmp.Width div 2;
  centerY := bmp.Height div 2;

  pointX := centerX - 281; // 119;
  pointY := centerY - 223; // 77;
  Title := Trim(GetText(bmp, pointX, pointY, 50, @GetBlackPoints));
  if Title <> '소유물품' then
  begin
    pointX := centerX - 226; // 174;
    pointY := centerY - 223; // 77;
    Title := Trim(GetText(bmp, pointX, pointY, 50, @GetBlackPoints));
  end;
  if Title <> '소유물품' then
  begin
    State.Selected := '';
    exit;
  end;
  //WriteLog('Title: ' + Title);

  pointX := centerX - 329; // 71;
  pointY := centerY - 200; // 100;
  limitY := pointY + 230;
  Selected := -1;

  while pointY < (limitY - 48) do
  begin
    TopLine := bmp.ScanLine[pointY];
    BottomLine := bmp.ScanLine[pointY + 47];
    r := TopLine[pointX].rgbtRed;
    g := TopLine[pointX].rgbtGreen;
    b := TopLine[pointX].rgbtBlue;
    LineColor := RGB(r, g, b);
    match := ((r shr 2) = (g shr 2)) and ((r shr 2) = (b shr 2)) and (r > 120);

    if not match then
    begin
      inc(pointY);
      continue;
    end;

    for X := pointX to pointX + 47 do
    begin
      TopColor := RGB(TopLine[x].rgbtRed, TopLine[x].rgbtGreen, TopLine[x].rgbtBlue);
      BottomColor := RGB(BottomLine[x].rgbtRed, BottomLine[x].rgbtGreen, BottomLine[x].rgbtBlue);
      if (TopColor <> LineColor) or (BottomColor <> LineColor) then
      begin
        match := False;
        break;
      end;
    end;

    if not match then
    begin
      inc(pointY);
      continue;
    end;

    Name := Trim(GetText(bmp, pointX + 52, pointY + 6, 180, GetWhitePoints));
    if Name = '호박' then
    begin
      if ComparePixelColor(bmp, pointX + 16, pointY + 16, 221, 221, 221) then
        Name := '호박(보석)'
      else
      if ComparePixelColor(bmp, pointX + 16, pointY + 16, 187, 119, 0) then
        Name := '호박(식료품)'
      else
      begin
        Name := '';
        WriteLogForColor(bmp, pointX, 16, pointY, 16);
      end;
    end;

    if Name = '' then
    begin
      inc(pointY);
      continue;
    end;

    Quote := Trim(GetText(bmp, pointX + 169, pointY + 26, 80, GetWhitePoints));
    if (Length(Quote) < 4) or (Quote[1] <> '(') or
      (Copy(Quote, Length(Quote) - 1, 2) <> '％)') then
    begin
      inc(pointY);
      continue;
    end;

    Quote := Trim(Copy(Quote, 2, Length(Quote) - 3));
    if not TryStrToInt(Quote, QuoteNum) then
    begin
      inc(pointY);
      continue;
    end;

    if ComparePixelColor(bmp, pointX + 240, pointY + 27, 8, 8, 8) and
       ComparePixelColor(bmp, pointX + 229, pointY + 38, 8, 8, 8) then
    begin
      Status := 1;
      StatusString := '▲';
    end
    else
    if ComparePixelColor(bmp, pointX + 240, pointY + 32, 8, 8, 8) and
       ComparePixelColor(bmp, pointX + 229, pointY + 36, 8, 8, 8) then
    begin
      Status := 0;
      StatusString := '';
    end
    else
    if ComparePixelColor(bmp, pointX + 240, pointY + 40, 8, 8, 8) and
       ComparePixelColor(bmp, pointX + 229, pointY + 29, 8, 8, 8) then
    begin
      Status := -1;
      StatusString := '▼';
    end
    else
    begin
      WriteLogForColor(bmp, pointX, 240, pointY, 27);
      WriteLogForColor(bmp, pointX, 229, pointY, 38);
      WriteLogForColor(bmp, pointX, 240, pointY, 32);
      WriteLogForColor(bmp, pointX, 229, pointY, 36);
      WriteLogForColor(bmp, pointX, 240, pointY, 40);
      WriteLogForColor(bmp, pointX, 229, pointY, 29);
      inc(pointY);
      continue;
    end;

    if BottomLine[pointX + 48].rgbtGreen > 150 then
    begin
      if State.Selected <> Name then
      begin
        Delay := 1;
        State.Selected := Name;
      end;
      Selected := pointY;
      //WriteLog('Selected: ' + Name);
    end;

    if State.UpdateItem(State.CityName, Name, QuoteNum, Status) then
    begin
      //WriteLog(Format('Goods %d: %s %s %s %d (Selected: %d)', [pointY, State.CityName, Name, Quote, Status, Selected]));
      WriteLog(Format('%s :: %s (%s%%) %s', [State.CityName, Name, Quote, StatusString]));
      Request.SendItemInfo(GetServer, State.CityName, Name, Quote, IntToStr(Status));
    end;

    inc(pointY, 56);
  end;

  if Selected = -1 then
  begin
    State.Selected := '';
    exit;
  end;

  if Delay > 0 then
  begin
    Dec(Delay);
    //WriteLog('Wait for delay');
    exit;
  end;

  Title := Trim(GetText(bmp, centerX + TitleOffsetX, centerY + TitleOffsetY, 120, @GetBlackPoints));
  if Title <> '인근 도시 시세' then
    exit;
  //WriteLog('Title: ' + Title);

  pointX := centerX + 34; // 434;
  pointY := centerY - 193; // 107;

  for item := 0 to 4 do
  begin
    TopLine := bmp.ScanLine[pointY + item * 56];
    BottomLine := bmp.ScanLine[pointY + item * 56 + 47];
    r := TopLine[pointX].rgbtRed;
    g := TopLine[pointX].rgbtGreen;
    b := TopLine[pointX].rgbtBlue;
    LineColor := RGB(r, g, b);
    match := ((r shr 2) = (g shr 2)) and ((r shr 2) = (b shr 2)) and (r > 120);

    if not match then
    begin
      continue;
    end;

    for X := pointX to pointX + 47 do
    begin
      TopColor := RGB(TopLine[x].rgbtRed, TopLine[x].rgbtGreen, TopLine[x].rgbtBlue);
      BottomColor := RGB(BottomLine[x].rgbtRed, BottomLine[x].rgbtGreen, BottomLine[x].rgbtBlue);
      if (TopColor <> LineColor) or (BottomColor <> LineColor) then
      begin
        match := False;
        break;
      end;
    end;

    if not match then
    begin
      //WriteLog(Format('item %d is broken', [item]));
      continue;
    end;

    Name := Trim(GetText(bmp, pointX + 52, pointY + item * 56 + 6, 180, GetWhitePoints));
    if (Name = '') or (Name = '???') then
    begin
      //WriteLog(Format('item %d is unknown city', [item]));
      continue;
    end;

    Quote := Trim(GetText(bmp, pointX + 169, pointY + item * 56 + 26, 80, GetWhitePoints));
    if (Length(Quote) < 4) or (Quote[1] <> '(') or
      (Copy(Quote, Length(Quote) - 1, 2) <> '％)') then
    begin
      continue;
    end;

    Quote := Trim(Copy(Quote, 2, Length(Quote) - 3));
    if not TryStrToInt(Quote, QuoteNum) then
    begin
      continue;
    end;

    if ComparePixelColor(bmp, pointX + 240, pointY + item * 56 + 27, 8, 8, 8) and
       ComparePixelColor(bmp, pointX + 229, pointY + item * 56 + 38, 8, 8, 8) then
    begin
      Status := 1;
      StatusString := '▲';
    end
    else
    if ComparePixelColor(bmp, pointX + 240, pointY + item * 56 + 32, 8, 8, 8) and
       ComparePixelColor(bmp, pointX + 229, pointY + item * 56 + 36, 8, 8, 8) then
    begin
      Status := 0;
      StatusString := '';
    end
    else
    if ComparePixelColor(bmp, pointX + 240, pointY + item * 56 + 40, 8, 8, 8) and
       ComparePixelColor(bmp, pointX + 229, pointY + item * 56 + 29, 8, 8, 8) then
    begin
      Status := -1;
      StatusString := '▼';
    end
    else
    begin
      WriteLogForColor(bmp, pointX, 240, pointY + item * 56, 27);
      WriteLogForColor(bmp, pointX, 229, pointY + item * 56, 38);
      WriteLogForColor(bmp, pointX, 240, pointY + item * 56, 32);
      WriteLogForColor(bmp, pointX, 229, pointY + item * 56, 36);
      WriteLogForColor(bmp, pointX, 240, pointY + item * 56, 40);
      WriteLogForColor(bmp, pointX, 229, pointY + item * 56, 29);
      continue;
    end;

    if State.UpdateItem(Name, State.Selected, QuoteNum, Status) then
    begin
      //WriteLog(Format('Goods %d: %s %s %s %d (Selected: %d)', [item, Name, State.Selected, Quote, Status, Selected]));
      WriteLog(Format('%s :: %s (%s%%) %s', [Name, State.Selected, Quote, StatusString]));
      Request.SendItemInfo(GetServer, Name, State.Selected, Quote, IntToStr(Status));
    end;
  end;
end;

procedure TFormQuote.WriteLog(msg: String);
var
  dateString: String;
begin
  dateString := formatdatetime('yyyy/mm/dd hh:mm:ss', Now);
  MemoLog.Lines.Add(Format('%s - %s', [dateString, msg]));
end;

procedure TFormQuote.WriteLogForColor(Bmp: TBitmap; pointX, increaseX, pointY, increaseY: Integer);
var
  VerticalLine: PRGBTriple;
  X, Y: Integer;
begin
  X := pointX + increaseX;
  Y := pointY + increaseY;
  VerticalLine := bmp.ScanLine[Y];
  WriteLog(Format('오류보고 :: 색상불일치 {%d, %d} R %d G %d B %d', [
    increaseX, increaseY,
    Integer(VerticalLine[X].rgbtRed),
    Integer(VerticalLine[X].rgbtGreen),
    Integer(VerticalLine[X].rgbtBlue)]));
end;

function TFormQuote.FindFlagImage(bmp: TBitmap): Integer;
var
  TopLine, BottomLine, VerticalLine: PRGBTriple;
  TopColor, BottomColor, LeftColor, RightColor: TColor;
  X, Y, W, Left: Integer;
  Match: Bool;
begin
  Result := -1;
  TopLine := bmp.ScanLine[FlagOffsetY];
  BottomLine := bmp.ScanLine[FlagOffsetY + FlagHeight - 1];
  W := 0;

  for X := 0 to 299 do
  begin
    TopColor := RGB(TopLine[x].rgbtRed, TopLine[x].rgbtGreen, TopLine[x].rgbtBlue);
    BottomColor := RGB(BottomLine[x].rgbtRed, BottomLine[x].rgbtGreen, BottomLine[x].rgbtBlue);
    if (TopColor <> clBlack) or (BottomColor <> clBlack) then
    begin
      W := 0;
      continue;
    end;

    inc(W);
    if W < FlagWidth then
    begin
      continue;
    end;

    Left := X - FlagWidth + 1;
    Match := True;
    for Y := FlagOffsetY + 1 to FlagOffsetY + FlagHeight - 1 do
    begin
      VerticalLine := bmp.ScanLine[Y];
      LeftColor := RGB(VerticalLine[Left].rgbtRed, VerticalLine[Left].rgbtGreen, VerticalLine[Left].rgbtBlue);
      RightColor := RGB(VerticalLine[X].rgbtRed, VerticalLine[X].rgbtGreen, VerticalLine[X].rgbtBlue);
      if (LeftColor <> clBlack) or (RightColor <> clBlack) then
      begin
        Match := False;
        break;
      end;
    end;

    if Match then
    begin
      Result := Left;
      break;
    end;
  end;
end;

function TFormQuote.GetText(bmp: TBitmap; X: Integer; Y: Integer; Width: Integer; PixelsFunc: TPixelsFunc): String;
var
  limit, step: Integer;
  a, b, font, index, mid: Word;
  match: Bool;
begin
  Result := '';
  {
  a := $2026;
  if FontIndex[a] <> $FFFF then
  begin
    index := FontIndex[a];
    for step := 0 to 15 do
      WriteLog(WordToBinStr(FontTable[index, step]) + ' - ' + IntToStr(step) + ' font');
  end;
  }
  limit := X + Width;
  while X < limit do
  begin
    a := 0;
    b := FontCount - 1;
    match := True;
    for step := 0 to 15 do
    begin
      font := PixelsFunc(bmp, X + step, Y);
      //WriteLog(WordToBinStr(font) + ' - ' + IntToStr(X) + 'bmp');

      if font > FontTable[a, step] then
      begin
        //WriteLog(Format('%d + %d x %d, s1: %d - %d, %d > %d', [X, step, Y, a, b, font, FontTable[a, step]]));
        //Application.ProcessMessages;
        index := b;
        while a < index do
        begin
          mid := (a + index + 1) div 2;
          if font <= FontTable[mid, step] then
            index := mid - 1
          else
            a := mid;
        end;
        if (font > FontTable[a, step]) and (a < (FontCount - 1)) then
          inc(a);
      end;

      if font < FontTable[b, step] then
      begin
        //WriteLog(Format('%d + %d x %d, s2: %d - %d, %d < %d', [X, step, Y, a, b, font, FontTable[b, step]]));
        //Application.ProcessMessages;
        index := a;
        while index < b do
        begin
          mid := (index + b) div 2;
          if font >= FontTable[mid, step] then
            index := mid + 1
          else
            b := mid;
        end;
        if (font < FontTable[b, step]) and (b > 0) then
          dec(b);
        //WriteLog(Format('%d + %d x %d, s3: %d - %d, %d <= %d <= %d', [X, step, Y, a, b, FontTable[a, step], font, FontTable[b, step]]));
        //Application.ProcessMessages;
      end;

      match := match and (font = FontTable[a, step]);

      if (step = 7) and match and (FontWidth[a] = 8) then
        break;

      if not match then
        break;
    end;

    if match then
    begin
      Result := Result + WideChar(FontMap[a]);
      inc(X, FontWidth[a]);
    end
    else
    begin
      //Result := Result + Format('(%d: %d - %d, %s - %s)', [X, a, b, WideChar(FontMap[a]), WideChar(FontMap[b])]);
      Result := Result + '_';
      inc(X, 8);
    end;
  end;
end;

function TFormQuote.GetServer: String;
begin
  if ComboBoxServer.ItemIndex = 1 then
    Result := 'eirene'
  else if ComboBoxServer.ItemIndex = 2 then
    Result := 'polaris'
  else if ComboBoxServer.ItemIndex = 3 then
    Result := 'helen'
  else
    Result := '';
end;

function TFormQuote.GetPassedTimeString(const PassedTime: Int64): String;
begin
  Result := '';
  if PassedTime <= 0 then
    exit
  else if PassedTime < 60 then
    Result := Format('%d초 전', [PassedTime])
  else if PassedTime < 3600 then
    Result := Format('%d분 %d초 전', [Floor(PassedTime div 60), PassedTime mod 60])
  else
    Result := Format('%d시간 전', [Floor(PassedTime div 3600)]);
end;

function TFormQuote.GetQuoteStatusString(const QuoteStatus: Int8): String;
begin
  Result := '';
  if QuoteStatus > 0 then
    Result := '▲'
  else if QuoteStatus < 0 then
    Result := '▼';
end;

function TFormQuote.GetResistStatusString(const ResistStatus: Int8): String;
begin
  Result := '';
  if ResistStatus = 1 then
    Result := '★';
end;

// http://yypbd.tistory.com/590
function TFormQuote.NaturalOrderCompareString(const A1, A2: string; ACaseSensitive: Boolean): Integer;
var
  Str1, Str2: PChar;
  Pos1, Pos2: Integer;
  EndPos1, EndPos2: Integer;
begin
  Str1 := PChar(A1);
  Str2 := PChar(A2);

  Pos1 := -1;
  Pos2 := -1;

  while True do
  begin
    Inc( Pos1 );
    Inc( Pos2 );

    if (Str1[Pos1] = #0) and (Str2[Pos2] = #0) then
    begin
      Result := 0;
      Exit;
    end
    else if Str1[Pos1] = #0 then
    begin
      Result := -1;
      Exit;
    end
    else if Str2[Pos2] = #0 then
    begin
      Result := 1;
      Exit;
    end;

    if (Str1[Pos1] >= '0') and (Str1[Pos1] <= '9') and
       (Str2[Pos2] >= '0') and (Str2[Pos2] <= '9') then
    begin
      EndPos1 := Pos1;
      repeat
        Inc(EndPos1);
      until not ((Str1[EndPos1] >= '0') and (Str1[EndPos1] <= '9'));

      EndPos2 := Pos2;
      repeat
        Inc(EndPos2);
      until not ((Str2[EndPos2] >= '0') and (Str2[EndPos2] <= '9'));

      while True do
      begin
        if EndPos1 - Pos1 = EndPos2 - Pos2 then
        begin
          // 이부분이 숫자비교임. StrToInt 한 다음에 빼도 될 것임
          Result := CompareStr( Copy(Str1, Pos1+1, EndPos1 - Pos1),  Copy(Str2, Pos2+1, EndPos1 - Pos1) ) ;

          if Result = 0 then
          begin
            Pos1 := EndPos1 - 1;
            Pos2 := EndPos2 - 1;
            Break;
          end
          else
          begin
            Exit;
          end;
        end
        else if EndPos1 - Pos1 > EndPos2 - Pos2 then
        begin
          if Str1[Pos1] = '0' then
            Inc(Pos1)
          else
          begin
            Result := 1;
            Exit;
          end;
        end
        else
        begin
          if Str2[Pos2] = '0' then
            Inc( Pos2 )
          else
          begin
            Result := -1;
            Exit;
          end;
        end;
      end;
    end
    else
    begin
      if ACaseSensitive then
        Result := CompareStr( Copy(Str1, Pos1, 1), Copy(Str2, Pos2, 1) )
      else
        Result := CompareText( Copy(Str1, Pos1, 1), Copy(Str2, Pos2, 1) );

      if Result <> 0 then
        Exit;
    end;
  end;
end;

end.
