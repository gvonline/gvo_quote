unit gamestate;

interface

uses
  System.SysUtils, Winapi.Windows, Generics.Collections;

type
  TGameState = class
  private
    fHandle: HWND;
    fCityName: String;
    fCityStatus: String;
    fItems: TDictionary<String, String>;
    fSelected: String;
  public
    constructor Create(Handle: HWND);
    destructor Destroy; override;
    procedure Clear;
    function UpdateItem(CityName, ItemName: String; Quote, Status: Integer): Bool;
    property Handle: HWND read fHandle;
    property CityName: String read fCityName write fCityName;
    property CityStatus: String read fCityStatus write fCityStatus;
    property Selected: String read fSelected write fSelected;
  end;

implementation

constructor TGameState.Create(Handle: HWND);
begin
  fHandle := Handle;
  fItems := TDictionary<String, String>.Create;
end;

destructor TGameState.Destroy;
begin
  fItems.Free;
  inherited Destroy;
end;

procedure TGameState.Clear;
begin
  fCityName := '';
  fCityStatus := '';
  fSelected := '';
  fItems.Clear;
end;

function TGameState.UpdateItem(CityName, ItemName: String; Quote, Status: Integer): Bool;
var
  Key, Value: String;
begin
  Result := False;
  Key := CityName + '::' + ItemName;
  Value := IntToStr(Quote) + '::' + IntToStr(Status);
  if not fItems.ContainsKey(Key) then
  begin
    fItems.Add(Key, Value);
    Result := True;
  end
  else if fItems[Key] <> Value then
  begin
    fItems[Key] := Value;
    Result := True;
  end;
end;

end.
