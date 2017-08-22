unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Generics.Collections,
  GameState, Request;

type
  TPixelsFunc = function(bitmap: TBitmap; X, Y: Integer): Word;

  TFormQuote = class(TForm)
    MemoLog: TMemo;
    ButtonClear: TButton;
    TimerLoop: TTimer;
    LabelCity: TLabel;
    RadioButtonEirene: TRadioButton;
    RadioButtonPolaris: TRadioButton;
    RadioButtonHelen: TRadioButton;
    ButtonWebsite: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerLoopTimer(Sender: TObject);
    procedure ButtonWebsiteClick(Sender: TObject);
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
    procedure LoadFont;
    procedure FindCityName(bmp: TBitmap);
    procedure FindChatMessage(bmp: TBitmap);
    procedure FindTrade(bmp: TBitmap);
    procedure WriteLog(msg: String);
    function FindFlagImage(bmp: TBitmap): Integer;
    function GetText(bmp: TBitmap; X, Y, Width: Integer; PixelsFunc: TPixelsFunc): String;
    function GetServer: String;
  public
    { Public declarations }
  end;

var
  FormQuote: TFormQuote;

implementation

{$R *.dfm}
{$POINTERMATH ON}

uses
  Math, StrUtils, ShellApi;

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
    WriteLog('서버와 연결되지 않았습니다.');
  end
  else if Latest <> Version then
  begin
    WriteLog('새 버전이 있습니다. 웹사이트에서 다운받으세요.');
  end
  else
  begin
    WriteLog('환영합니다. 자세한 내용은 웹사이트를 확인하세요.');
  end;
end;

procedure TFormQuote.FormDestroy(Sender: TObject);
begin
  CurrentWindow := 0;
  WindowList.Free;
  CityNames.Free;
  Request.Terminate;
end;

procedure TFormQuote.ButtonWebsiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://gvonline.ga', nil, nil, SW_SHOWNORMAL);
end;

procedure TFormQuote.ButtonClearClick(Sender: TObject);
begin
  CurrentWindow := 0;
  WindowList.Clear;
  LabelCity.Caption := '';
  MemoLog.Lines.Clear;
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
      StartsStr('항구안내원      ：', log) then
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
  TopLine, BottomLine, ArrowLine: PRGBTriple;
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
    if Name = '' then
    begin
      inc(pointY);
      continue;
    end;

    Quote := Trim(GetText(bmp, pointX + 169, pointY + 26, 80, GetWhitePoints));
    if Quote = '' then
    begin
      inc(pointY);
      continue;
    end;

    Quote := Copy(Quote, 2, Length(Quote) - 3);
    if Length(Quote) > 3 then
    begin
      inc(pointY);
      continue;
    end;

    ArrowLine := bmp.ScanLine[pointY + 33];
    if ArrowLine[pointX + 237].rgbtRed > 200 then
    begin
      Status := 1;
      StatusString := '▲';
    end
    else if ArrowLine[pointX + 237].rgbtBlue < 200 then
    begin
      Status := 0;
      StatusString := '';
    end
    else
    begin
      Status := -1;
      StatusString := '▼';
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

    if State.UpdateItem(State.CityName, Name, StrToInt(Quote), Status) then
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
    if Quote = '' then
    begin
      continue;
    end;

    Quote := Copy(Quote, 2, Length(Quote) - 3);
    if Length(Quote) > 3 then
    begin
      continue;
    end;

    ArrowLine := bmp.ScanLine[pointY + item * 56 + 33];
    if ArrowLine[pointX + 237].rgbtRed > 200 then
    begin
      Status := 1;
      StatusString := '▲';
    end
    else if ArrowLine[pointX + 237].rgbtBlue < 200 then
    begin
      Status := 0;
      StatusString := '';
    end
    else
    begin
      Status := -1;
      StatusString := '▼';
    end;

    if State.UpdateItem(Name, State.Selected, StrToInt(Quote), Status) then
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
  if RadioButtonEirene.Checked then
    Result := 'eirene'
  else if RadioButtonPolaris.Checked then
    Result := 'polaris'
  else if RadioButtonHelen.Checked then
    Result := 'helen'
  else
    Result := '';
end;

end.
