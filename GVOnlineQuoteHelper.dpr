program GVOnlineQuoteHelper;



uses
  Vcl.Forms,
  Windows,
  main in 'main.pas' {FormQuote},
  gamestate in 'gamestate.pas',
  request in 'request.pas';

{$R *.res}

begin
  CreateMutex(nil, true, '547F8B98-116B-41FC-A858-1CB9DDC752BF');
  if GetLastError = ERROR_ALREADY_EXISTS then
    exit;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormQuote, FormQuote);
  Application.Run;
end.
