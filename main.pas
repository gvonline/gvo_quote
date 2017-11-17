unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GameState, Request, ComCtrls,
  lazutf8, fgl, fpjson, jsonparser, jsonscanner, IntfGraphics;

type
  TPixelsFunc = function(lazImage: TLazIntfImage; X, Y: Integer): Word;

  { TFormQuote }

  TFormQuote = class(TForm)
    ButtonClear: TButton;
    ButtonReset: TButton;
    ButtonSearch: TButton;
    ButtonWebsite: TButton;
    ComboBoxServer: TComboBox;
    ComboBoxShortcut: TComboBox;
    EditCities: TEdit;
    EditGoods: TEdit;
    LabelCities: TLabel;
    LabelCity: TLabel;
    LabelGoods: TLabel;
    ListViewSearch: TListView;
    MemoLog: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TimerLoop: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerLoopTimer(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonWebsiteClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ComboBoxServerChange(Sender: TObject);
    procedure ComboBoxShortcutChange(Sender: TObject);
    procedure ListViewSearchColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewSearchDblClick(Sender: TObject);
    procedure ListViewSearchCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure ListViewCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
  const
    GameClassName = 'Greate Voyages Online Game MainFrame';
    OtherCitiesOffsetX = 113;
    OtherCitiesOffsetY = -223;
    ConferenceOffsetX = 8;
    ConferenceOffsetY = 8;
    CityOffsetX = 34;
    CityOffsetY = 8;
    FlagOffsetX = 8;
    FlagOffsetY = 8;
    FlagWidth = 24;
    FlagHeight = 16;
  private
    { Private declarations }
    CityNames: TStringList;
    CityShortcuts: TStringList;
    ColumnSortAscending: Boolean;
    ColumnSortIndex: Integer;
    ColumnSortWas: Integer;
    ColumnType: Integer;
    CurrentWindow: HWND;
    Delay: Integer;
    FontCount: Word;
    FontMap, FontIndex: array of Word;
    FontTable: array of array of Word;
    FontWidth: array of Byte;
    GoodsShortcuts: TStringList;
    Request: TRequest;
    WindowList: TFPGMapObject<HWND, TGameState>;
    procedure FindChatMessage(lazImage: TLazIntfImage);
    procedure FindCityName(lazImage: TLazIntfImage);
    function FindFlagImage(lazImage: TLazIntfImage): Integer;
    procedure FindTrade(lazImage: TLazIntfImage);
    function GetPassedTimeString(const PassedTime: Int64): String;
    function GetQuoteStatusString(const QuoteStatus: Int8): String;
    function GetResistStatusString(const ResistStatus: Int8): String;
    function GetServer: String;
    function GetText(lazImage: TLazIntfImage; X, Y, Width: Integer; PixelsFunc: TPixelsFunc): String;
    procedure LoadFont;
    function NaturalOrderCompareString(const A1, A2: string; ACaseSensitive: Boolean): Integer;
    procedure ReadSearchShortcuts(SearchShortcutsPath: String);
    procedure WriteLog(msg: String);
    procedure WriteLogForColor(lazImage: TLazIntfImage; pointX, increaseX, pointY, increaseY: Integer);

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

function GetBlackPoints(lazImage: TLazIntfImage; fromX, fromY: Integer): Word;
var
  count: Integer;
  Line: PRGBTriple;
  color: TColor;
begin
  result := 0;
  for count := 0 to 14 do
  begin
    Line := lazImage.GetDataLineStart(fromY + count);
    color := RGB(Line[fromX].rgbtRed, Line[fromX].rgbtGreen, Line[fromX].rgbtBlue);
    result := result shl 1;
    if color = clBlack then
    begin
      result := result or 1;
    end;
  end;
  result := result shl 1;
end;

function GetBrightPoints(lazImage: TLazIntfImage; fromX, fromY: Integer): Word;
var
  count: Integer;
  Line: PRGBTriple;
  high: Byte;
begin
  result := 0;
  for count := 0 to 14 do
  begin
    Line := lazImage.GetDataLineStart(fromY + count);
    high := Max(Line[fromX].rgbtRed, Max(Line[fromX].rgbtGreen, Line[fromX].rgbtBlue));
    result := result shl 1;
    if high > 160 then
    begin
      result := result or 1;
    end;
  end;
  result := result shl 1;
end;
function GetWhitePoints(lazImage: TLazIntfImage; fromX, fromY: Integer): Word;
var
  count: Integer;
  Line: PRGBTriple;
  color: TColor;
begin
  result := 0;
  for count := 0 to 14 do
  begin
    Line := lazImage.GetDataLineStart(fromY + count);
    color := RGB(Line[fromX].rgbtRed, Line[fromX].rgbtGreen, Line[fromX].rgbtBlue);
    result := result shl 1;
    if color = clWhite then
    begin
      result := result or 1;
    end;
  end;
  result := result shl 1;
end;

function CompareColor(R1, G1, B1: Byte; R2, G2, B2: Byte): Bool;
begin
  result := False;
  if ABS(Integer(R1) - R2) > 8 then
     exit;
  if ABS(Integer(G1) - G2) > 8 then
     exit;
  if ABS(Integer(B1) - B2) > 8 then
     exit;
  result := True;
end;

function ComparePixelColor(lazImage: TLazIntfImage; X, Y: Integer; R, G, B: Byte): Bool;
var
  Line: PRGBTriple;
begin
  result := False;
  Line := lazImage.GetDataLineStart(Y);
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

{ TFormQuote Public ========================================================== }

procedure TFormQuote.FormCreate(Sender: TObject);
var
  CityNamesPath, FontPath, SearchShortcutsPath: String;
  Latest, Version: String;
  ExeName: string;
  Size, Dummy: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  CurrentWindow := 0;
  WindowList := TFPGMapObject<HWND, TGameState>.Create(True);
  MemoLog.Lines.Clear;
  LabelCity.Caption := '';
  //WriteLog('FormCreate');

  FontPath := ExtractFilePath(Application.ExeName) + 'Font.dat';
  CityNamesPath := ExtractFilePath(Application.ExeName) + 'CityNames.txt';
  SearchShortcutsPath := ExtractFilePath(Application.ExeName) + 'SearchShortcuts.txt';

  if not fileexists(FontPath) then
  begin
    MessageBoxW(0, 'Font.dat 파일을 찾을 수 없습니다.', '오류', MB_OK+MB_ICONERROR);
    Application.Terminate;
    exit;
  end;

  if not fileexists(CityNamesPath) then
  begin
    MessageBoxW(0, 'CityNames.txt 파일을 찾을 수 없습니다.', '오류', MB_OK+MB_ICONERROR);
    Application.Terminate;
    exit;
  end;

  if not fileexists(SearchShortcutsPath) then
  begin
    MessageBoxW(0, 'SearchShortcuts.txt 파일을 찾을 수 없습니다.', '오류', MB_OK+MB_ICONERROR);
    Application.Terminate;
    exit;
  end;

  LoadFont;

  CityNames := TStringList.Create;
  CityNames.Sorted := True;
  CityNames.Duplicates := dupIgnore;
  CityNames.LoadFromFile(CityNamesPath);
  CityNames.Sorted := False;
  if Copy(CityNames[0], 1, 3) = #$EF#$BB#$BF then
  begin
    CityNames[0] := Copy(CityNames[0], 4, Length(CityNames[0]));
  end;

  CityShortcuts := TStringList.Create;
  CityShortcuts.Add('');
  GoodsShortcuts := TStringList.Create;
  GoodsShortcuts.Add('');

  ReadSearchShortcuts(SearchShortcutsPath);

  Request := TRequest.Create;

  ExeName := Application.ExeName;
  Dummy := 0;
  Size := GetFileVersionInfoSize(PChar(ExeName), Dummy);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(ExeName), 0, Size, PByte(Buffer)) then
    RaiseLastOSError;
  if not VerQueryValue(PByte(Buffer), '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Version := Format('%d.%d #%d', [
    HiWord(FixedPtr^.dwProductVersionMS), //Major
    LoWord(FixedPtr^.dwProductVersionMS), //Minor
    LoWord(FixedPtr^.dwFileVersionLS)]); //Build
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
  if Assigned(WindowList) then
    FreeAndNil(WindowList);
  if Assigned(CityNames) then
    FreeAndNil(CityNames);
  if Assigned(CityShortcuts) then
    FreeAndNil(CityShortcuts);
  if Assigned(GoodsShortcuts) then
    FreeAndNil(GoodsShortcuts);
  if Assigned(Request) then
    Request.Terminate;
end;

procedure TFormQuote.TimerLoopTimer(Sender: TObject);
var
  ClassName: string;
  Handle: HWND;
  state: TGameState;
  WindowRect: TRect;
  //WindowPoint: TPoint;
  DC: HDC;
  Bmp: TBitmap;
  Captured: Boolean;
  lazImage: TLazIntfImage;
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
    if WindowList.TryGetData(Handle, state) then
    begin
      LabelCity.Caption := '도시명: ' + state.CityName;
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
  Captured := Windows.GetClientRect(Handle, WindowRect);
  if (not Captured) or (WindowRect.Width < 800) or (WindowRect.Height < 600) then
  begin
    //WriteLog('not Captured');
    exit;
  end;

  Bmp := TBitmap.Create;
  Bmp.Height := WindowRect.Height;
  Bmp.Width := WindowRect.Width;
  Bmp.PixelFormat := pf24bit;
  DC := GetDC(Handle);
  Captured := BitBlt(Bmp.Canvas.Handle, 0, 0, WindowRect.Width, WindowRect.Height, DC, 0, 0, SRCCOPY);
  ReleaseDC(Handle, DC);

  lazImage := Bmp.CreateIntfImage;
  Bmp.Free;

  if Captured then
  begin
    //WriteLog('FindCityName');
    FindCityName(lazImage);
    if (CurrentWindow = Handle) and (WindowList[Handle].CityName <> '') then
    begin
      //WriteLog('FindChatMessage');
      FindChatMessage(lazImage);
      //WriteLog('FindTrade');
      FindTrade(lazImage);
    end;
    //WriteLog('End');
  end;

  lazImage.Free;
  TimerLoop.Enabled := True;
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

procedure TFormQuote.ButtonWebsiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://gvonline.ga', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormQuote.ButtonSearchClick(Sender: TObject);
const
  ColumnNamesForDashboard: array[0..3] of String = ('도시명', '등록건수', '도시상태', '갱신시간');
  ColumnWidthForDashboard: array[0..3] of Integer = (    120,         60,        160,        100);
  ColumnNamesForCommon: array[0..8] of String = ('품명', '품목', '시세', '시세상태', '시세갱신시간', '도시명', '내성항', '도시상태', '상태갱신시간');
  ColumnWidthForCommon: array[0..8] of Integer = (   80,     50,     40,         60,            100,      100,       50,        150,            120);
var
  i, j, ColumnCount: Integer;
  JSONString, Text: String;
  JSONParser: TJSONParser;
  JSONArray: TJSONArray;
  JSONRow: TJSONObject;
  ColumnNames: Array of String;
  ColumnWidth: Array of Integer;
begin
  ColumnType := 0;
  ColumnSortWas := -1;
  ColumnCount := 0;
  JSONString := Request.GetSearchResult(GetServer, EditCities.Text, EditGoods.Text);
  JSONParser := TJSONParser.Create(JSONString, [joUTF8]);
  JSONArray := TJSONArray(JSONParser.Parse);
  for i := 0 to JSONArray.Count - 1 do
  begin
    JSONRow := TJSONObject(JSONArray.Items[i]);
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

  ListViewSearch.Items.BeginUpdate;
  for i := 0 to JSONArray.Count - 1 do
  begin
    with ListViewSearch.Items.Add do
    begin
      for j := 0 to ColumnCount - 1 do
      begin
        JSONRow := TJSONObject(JSONArray.Items[i]);
        Text := JSONRow.Get(ColumnNames[j]);

        if AnsiEndsStr('갱신시간', ColumnNames[j]) then
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
        SubItems.Add(JSONRow.Get(ColumnNames[j]));
      end;
    end;
  end;

  ListViewSearch.Items.EndUpdate;
end;

procedure TFormQuote.ButtonResetClick(Sender: TObject);
var
  i: Integer;
begin
  ComboBoxShortcut.ItemIndex := 0;
  ComboBoxShortcut.Enabled := False;
  ComboBoxShortcut.Style := csDropDown;
  ComboBoxShortcut.Style := csDropDownList;
  ComboBoxShortcut.Enabled := True;
  EditCities.Text := '';
  EditGoods.Text := '';
  ListViewSearch.Clear;
  if ListViewSearch.Columns.Count > 0 then
  begin
    for i := ListViewSearch.Columns.Count - 1 downto 0 do
      ListViewSearch.Column[i].Destroy;
  end;
end;

procedure TFormQuote.ComboBoxServerChange(Sender: TObject);
begin
  ComboBoxServer.Enabled := (ComboBoxServer.ItemIndex <= 0);
  ButtonSearch.Enabled := not ComboBoxServer.Enabled;
  ButtonReset.Enabled := ButtonSearch.Enabled;
  TimerLoop.Enabled := not ComboBoxServer.Enabled;
end;

procedure TFormQuote.ComboBoxShortcutChange(Sender: TObject);
begin
  EditCities.Text := CityShortcuts[ComboBoxShortcut.ItemIndex];
  EditGoods.Text := GoodsShortcuts[ComboBoxShortcut.ItemIndex];
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

procedure TFormQuote.ListViewSearchDblClick(Sender: TObject);
var
  HitTests: THitTests;
  Text: String;
  SubItems: TStringList;
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
    SubItems := TStringList.Create;
    SubItems.Assign(SelectedItem.SubItems);
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

  SubItems.Free;
  ButtonSearchClick(nil);
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

procedure TFormQuote.ListViewCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Text: String;
begin
  if (SubItem = 3) or (SubItem = 6) then
  begin
    Text := Item.SubItems.Strings[SubItem - 1];
    if Text = '▲' then
      Sender.Canvas.Font.Color := clMaroon
    else if Text = '▼' then
      Sender.Canvas.Font.Color := clNavy
    else if Text = '★' then
      Sender.Canvas.Font.Color := clOlive
    else
      Sender.Canvas.Font.Color := clDefault;
    exit;
  end;
  Sender.Canvas.Font.Color := clDefault;
end;

{ TFormQuote Private ========================================================= }

procedure TFormQuote.FindChatMessage(lazImage: TLazIntfImage);
var
  State: TGameState;
  pointX, pointY, limitY, line: Integer;
  log, status: String;
begin
  if CurrentWindow = 0 then
    exit;

  limitY := lazImage.Height - 32;
  pointY := limitY - 20 * 5;
  for line := 0 to 4 do
  begin
    pointX := 12;
    log := GetText(lazImage, pointX, pointY, 568, @GetBrightPoints);

    if AnsiStartsStr('교역소주인      ：', log) then
    begin
      status := Trim(UTF8Copy(log, 13, UTF8Length(log)));
    end;

    if AnsiStartsStr('교역소의 도제   ：', log) then
    begin
      status := Trim(UTF8Copy(log, 12, UTF8Length(log)));
    end;

    if AnsiStartsStr('항구관리        ：', log) or
       AnsiStartsStr('항구안내원      ：', log) or
       AnsiStartsStr('역장            ：', log) or
       AnsiStartsStr('마부            ：', log) then
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

procedure TFormQuote.FindCityName(lazImage: TLazIntfImage);
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

  flagX := FindFlagImage(lazImage);

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
      font[step] := getWhitePoints(lazImage, pointX + step, pointY);
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

function TFormQuote.FindFlagImage(lazImage: TLazIntfImage): Integer;
var
  TopLine, BottomLine, VerticalLine: PRGBTriple;
  TopColor, BottomColor, LeftColor, RightColor: TColor;
  X, Y, W, Left: Integer;
  Match: Bool;
begin
  Result := -1;
  TopLine := lazImage.GetDataLineStart(FlagOffsetY);
  BottomLine := lazImage.GetDataLineStart(FlagOffsetY + FlagHeight - 1);
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
      VerticalLine := lazImage.GetDataLineStart(Y);
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

procedure TFormQuote.FindTrade(lazImage: TLazIntfImage);
var
  State: TGameState;
  centerX, centerY, pointX, pointY, limitY, Selected, item, X, Status: Integer;
  Title, Name, Quote, StatusString: String;
  QuoteNum: Integer;
  TopLine, BottomLine: PRGBTriple;
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

  centerX := lazImage.Width div 2;
  centerY := lazImage.Height div 2;

  pointX := centerX - 281; // 119;
  pointY := centerY - 223; // 77;
  Title := Trim(GetText(lazImage, pointX, pointY, 50, @GetBlackPoints));
  if Title <> '소유물품' then
  begin
    pointX := centerX - 226; // 174;
    pointY := centerY - 223; // 77;
    Title := Trim(GetText(lazImage, pointX, pointY, 50, @GetBlackPoints));
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
    TopLine := lazImage.GetDataLineStart(pointY);
    BottomLine := lazImage.GetDataLineStart(pointY + 47);
    r := TopLine[pointX].rgbtRed;
    g := TopLine[pointX].rgbtGreen;
    b := TopLine[pointX].rgbtBlue;
    match := (abs(r - g) < 8) and (abs(r - b) < 8) and (abs(g - b) < 8) and (r > 120);

    if not match then
    begin
      inc(pointY);
      continue;
    end;

    for X := pointX to pointX + 47 do
    begin
      if not CompareColor(TopLine[x].rgbtRed, TopLine[x].rgbtGreen, TopLine[x].rgbtBlue, r, g, b) or
         not CompareCOlor(BottomLine[x].rgbtRed, BottomLine[x].rgbtGreen, BottomLine[x].rgbtBlue, r, g, b) then
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

    Name := Trim(GetText(lazImage, pointX + 52, pointY + 6, 180, GetWhitePoints));
    if Name = '호박' then
    begin
      if ComparePixelColor(lazImage, pointX + 16, pointY + 16, 221, 221, 221) then
        Name := '호박(보석)'
      else
        Name := '호박(식료품)';
    end;

    if Name = '' then
    begin
      inc(pointY);
      continue;
    end;

    Quote := Trim(GetText(lazImage, pointX + 169, pointY + 26, 80, GetWhitePoints));
    if (UTF8Length(Quote) < 4) or (Quote[1] <> '(') or
      (UTF8Copy(Quote, UTF8Length(Quote) - 1, 2) <> '％)') then
    begin
      inc(pointY);
      continue;
    end;

    Quote := Trim(UTF8Copy(Quote, 2, UTF8Length(Quote) - 3));
    if not TryStrToInt(Quote, QuoteNum) then
    begin
      inc(pointY);
      continue;
    end;

    if ComparePixelColor(lazImage, pointX + 240, pointY + 27, 8, 8, 8) and
       ComparePixelColor(lazImage, pointX + 229, pointY + 38, 8, 8, 8) then
    begin
      Status := 1;
      StatusString := '▲';
    end
    else
    if ComparePixelColor(lazImage, pointX + 240, pointY + 32, 8, 8, 8) and
       ComparePixelColor(lazImage, pointX + 229, pointY + 36, 8, 8, 8) then
    begin
      Status := 0;
      StatusString := '';
    end
    else
    if ComparePixelColor(lazImage, pointX + 240, pointY + 40, 8, 8, 8) and
       ComparePixelColor(lazImage, pointX + 229, pointY + 29, 8, 8, 8) then
    begin
      Status := -1;
      StatusString := '▼';
    end
    else
    begin
      WriteLogForColor(lazImage, pointX, 240, pointY, 27);
      WriteLogForColor(lazImage, pointX, 229, pointY, 38);
      WriteLogForColor(lazImage, pointX, 240, pointY, 32);
      WriteLogForColor(lazImage, pointX, 229, pointY, 36);
      WriteLogForColor(lazImage, pointX, 240, pointY, 40);
      WriteLogForColor(lazImage, pointX, 229, pointY, 29);
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

  Title := Trim(GetText(lazImage, centerX + OtherCitiesOffsetX, centerY + OtherCitiesOffsetY, 120, @GetBlackPoints));
  if Title <> '인근 도시 시세' then
    exit;
  //WriteLog('Title: ' + Title);

  pointX := centerX + 34; // 434;
  pointY := centerY - 193; // 107;

  for item := 0 to 4 do
  begin
    TopLine := lazImage.GetDataLineStart(pointY + item * 56);
    BottomLine := lazImage.GetDataLineStart(pointY + item * 56 + 47);
    r := TopLine[pointX].rgbtRed;
    g := TopLine[pointX].rgbtGreen;
    b := TopLine[pointX].rgbtBlue;
    match := (abs(r - g) < 8) and (abs(r - b) < 8) and (abs(g - b) < 8) and (r > 120);

    if not match then
    begin
      continue;
    end;

    for X := pointX to pointX + 47 do
    begin
      if not CompareColor(TopLine[x].rgbtRed, TopLine[x].rgbtGreen, TopLine[x].rgbtBlue, r, g, b) or
         not CompareCOlor(BottomLine[x].rgbtRed, BottomLine[x].rgbtGreen, BottomLine[x].rgbtBlue, r, g, b) then
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

    Name := Trim(GetText(lazImage, pointX + 52, pointY + item * 56 + 6, 180, GetWhitePoints));
    if (Name = '') or (Name = '???') then
    begin
      //WriteLog(Format('item %d is unknown city', [item]));
      continue;
    end;

    Quote := Trim(GetText(lazImage, pointX + 169, pointY + item * 56 + 26, 80, GetWhitePoints));
    if (UTF8Length(Quote) < 4) or (Quote[1] <> '(') or
      (UTF8Copy(Quote, UTF8Length(Quote) - 1, 2) <> '％)') then
    begin
      continue;
    end;

    Quote := Trim(UTF8Copy(Quote, 2, UTF8Length(Quote) - 3));
    if not TryStrToInt(Quote, QuoteNum) then
    begin
      continue;
    end;

    if ComparePixelColor(lazImage, pointX + 240, pointY + item * 56 + 27, 8, 8, 8) and
       ComparePixelColor(lazImage, pointX + 229, pointY + item * 56 + 38, 8, 8, 8) then
    begin
      Status := 1;
      StatusString := '▲';
    end
    else
    if ComparePixelColor(lazImage, pointX + 240, pointY + item * 56 + 32, 8, 8, 8) and
       ComparePixelColor(lazImage, pointX + 229, pointY + item * 56 + 36, 8, 8, 8) then
    begin
      Status := 0;
      StatusString := '';
    end
    else
    if ComparePixelColor(lazImage, pointX + 240, pointY + item * 56 + 40, 8, 8, 8) and
       ComparePixelColor(lazImage, pointX + 229, pointY + item * 56 + 29, 8, 8, 8) then
    begin
      Status := -1;
      StatusString := '▼';
    end
    else
    begin
      WriteLogForColor(lazImage, pointX, 240, pointY + item * 56, 27);
      WriteLogForColor(lazImage, pointX, 229, pointY + item * 56, 38);
      WriteLogForColor(lazImage, pointX, 240, pointY + item * 56, 32);
      WriteLogForColor(lazImage, pointX, 229, pointY + item * 56, 36);
      WriteLogForColor(lazImage, pointX, 240, pointY + item * 56, 40);
      WriteLogForColor(lazImage, pointX, 229, pointY + item * 56, 29);
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

function TFormQuote.GetText(lazImage: TLazIntfImage; X, Y, Width: Integer; PixelsFunc: TPixelsFunc): String;
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
      font := PixelsFunc(lazImage, X + step, Y);
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

procedure TFormQuote.ReadSearchShortcuts(SearchShortcutsPath: String);
var
  ReadFile: TStringList;
  i, Max, PosIndex: Integer;
  Text: String;
begin
  CityShortcuts.Clear;
  CityShortcuts.Add('');
  GoodsShortcuts.Clear;
  GoodsShortcuts.Add('');
  ComboBoxShortcut.Items.Clear;
  ComboBoxShortcut.Items.Add('즐겨찾기');

  ReadFile := TStringList.Create;
  ReadFile.LoadFromFile(SearchShortcutsPath);
  Max := ReadFile.Count - 1;
  if Max < 1 then
    exit;

  if Copy(ReadFile[0], 1, 3) = #$EF#$BB#$BF then
  begin
    ReadFile[0] := Copy(ReadFile[0], 4, Length(ReadFile[0]));
  end;

  for i := 0 to Max do
  begin
    PosIndex := Pos('=', ReadFile[i]);
    if PosIndex < 1 then
      continue;
    ComboBoxShortcut.Items.Add(Trim(Copy(ReadFile[i], 1, (PosIndex - 1))));
    Text := Trim(Copy(ReadFile[i], (PosIndex + 1), Length(ReadFile[i])));

    PosIndex := Pos('+', Text);
    if PosIndex < 1 then
    begin
      CityShortcuts.Add(Text);
      GoodsShortcuts.Add('');
      continue;
    end;

    CityShortcuts.Add(Trim(Copy(Text, 1, (PosIndex - 1))));
    GoodsShortcuts.Add(Trim(Copy(Text, (PosIndex + 1), Length(Text))));
  end;

  ReadFile.Free;

  ComboBoxShortcut.ItemIndex := 0;
  ComboBoxShortcut.Enabled := True;
end;

procedure TFormQuote.WriteLog(msg: String);
var
  dateString: String;
begin
  dateString := formatdatetime('yyyy/mm/dd hh:mm:ss', Now);
  MemoLog.Lines.Add(Format('%s - %s', [dateString, msg]));
end;

procedure TFormQuote.WriteLogForColor(lazImage: TLazIntfImage; pointX, increaseX, pointY, increaseY: Integer);
var
  VerticalLine: PRGBTriple;
  X, Y: Integer;
begin
  X := pointX + increaseX;
  Y := pointY + increaseY;
  VerticalLine := lazImage.GetDataLineStart(Y);
  WriteLog(Format('오류보고 :: 색상불일치 {%d, %d} R %d G %d B %d', [
    increaseX, increaseY,
    Integer(VerticalLine[X].rgbtRed),
    Integer(VerticalLine[X].rgbtGreen),
    Integer(VerticalLine[X].rgbtBlue)]));
end;

end.
