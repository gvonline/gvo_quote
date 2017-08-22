program GVOnlineQuoteHelper;



uses
  Vcl.Forms,
  main in 'main.pas' {FormQuote},
  gamestate in 'gamestate.pas',
  request in 'request.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormQuote, FormQuote);
  Application.Run;
end.
