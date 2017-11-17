unit gamestate;

interface

uses
  SysUtils, Windows, contnrs;

type
  TGameState = class
  private
    fHandle: HWND;
    fCityName: String;
    fCityStatus: String;
    fItems: TFPStringHashTable;
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
  fItems := TFPStringHashTable.Create;
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
  if fItems[Key] <> Value then
  begin
    fItems[Key] := Value;
    Result := True;
  end;
end;

end.
