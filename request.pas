unit request;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, HTTPSend;

type
  TRequest = class(TThread)
  const
    URL = 'http://quote.gvonline.ga';
    VersionURL = 'http://quote.gvonline.ga/latest';
  private
    Params: TStringList;
    Lock: TCriticalSection;
    Event: TEvent;
    HTTP: THTTPSend;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendCityInfo(Server, CityName, CityStatus: String);
    procedure SendItemInfo(Server, CityName, ItemName, ItemQuote, ItemStatus: String);
    function GetVersion: String;
  end;

implementation

uses
  synacode, synautil;

constructor TRequest.Create;
begin
  FreeOnTerminate := True;
  HTTP := THTTPSend.Create;
  Params := TStringList.Create;
  Lock := TCriticalSection.Create;
  Event := TEvent.Create(nil, True, False, '');
  inherited Create(False);
end;

destructor TRequest.Destroy;
begin
  inherited Destroy;
  Event.SetEvent;
  Event.Free;
  Lock.Free;
  Params.Free;
  HTTP.Free;
end;

procedure TRequest.Execute;
var
  Param: AnsiString;
begin
  while not Terminated do
  begin
    Lock.Enter;
    if Params.Count > 0 then
    begin
      Param := Params[0];
      Params.Delete(0);
      Lock.Leave;

      HTTP.Document.Clear;
      HTTP.Document.Write(BytesOf(Param), Length(Param));
      HTTP.MimeType := 'application/x-www-form-urlencoded';
      HTTP.HTTPMethod('POST', URL);
    end
    else
    begin
      Lock.Leave;
      Event.WaitFor(10000);
      Event.ResetEvent;
    end;
  end;
end;

procedure TRequest.SendCityInfo(Server, CityName, CityStatus: String);
var
  Param: String;
begin
  Param := 'server=' + EncodeURLElement(UTF8Encode(Server));
  Param := Param + '&city_name=' + EncodeURLElement(UTF8Encode(CityName));
  Param := Param + '&city_status=' + EncodeURLElement(UTF8Encode(CityStatus));

  Lock.Enter;
  Params.Add(Param);
  Lock.Leave;
  Event.SetEvent;
end;

procedure TRequest.SendItemInfo(Server, CityName, ItemName, ItemQuote, ItemStatus: String);
var
  Param: String;
begin
  Param := 'server=' + EncodeURLElement(UTF8Encode(Server));
  Param := Param + '&city_name=' + EncodeURLElement(UTF8Encode(CityName));
  Param := Param + '&item_name=' + EncodeURLElement(UTF8Encode(ItemName));
  Param := Param + '&sale_quote=' + EncodeURLElement(UTF8Encode(ItemQuote));
  Param := Param + '&sale_status=' + EncodeURLElement(UTF8Encode(ItemStatus));

  Lock.Enter;
  Params.Add(Param);
  Lock.Leave;
  Event.SetEvent;
end;

function TRequest.GetVersion: String;
var
  Text: TStringList;
begin
  Text := TStringList.Create;
  if HTTPGetText(VersionURL, Text) and (text.Count > 0) then
    Result := UTF8toString(Text[0])
  else
    Result := '';
  Text.Free;
end;

end.
