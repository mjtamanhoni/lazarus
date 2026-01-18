unit uBase.Router;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Horse;

type

  { TBaseRoute }

  TBaseRoute = class
  public
    class procedure Load();
  end;

implementation

{ TBaseRoute }

uses
  uUsuario.Service;

procedure OnStatus(aReq:THorseRequest; aRes:THorseResponse);
begin
  aRes.ContentType('text/html')
    .Send(Format('<h1>Server on-line - Horse version <i>%s</i></h1>',[THorse.Version]));
end;

procedure OnTeste(aReq:THorseRequest; aRes:THorseResponse);
begin
  aRes.ContentType('text/html')
    .Send('Endpoint teste');
end;

procedure OnLogin(aReq:THorseRequest; aRes:THorseResponse);
begin
  aRes.ContentType('text/html')
    .Send(TUsuarioService.New.Login(aReq.Body));
end;

class procedure TBaseRoute.Load();
begin
  THorse.Get('/', OnStatus);
  THorse.Get('/teste', OnTeste);

  THorse.Post('/login',OnLogin);
end;

end.

