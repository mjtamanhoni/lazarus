unit uBase.Router;

{$mode Delphi}{$H+}

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

procedure OnUsuarioGet(aReq:THorseRequest; aRes:THorseResponse);
var
  FId :Integer;
  FLogin :String;
  FNome :String;
  FStatus :Integer;
begin

  FId := StrToIntDef(aReq.Query['id'],0);
  FLogin := aReq.Query['login'];
  FNome := aReq.Query['nome'];
  FStatus := StrToIntDef(aReq.Query['status'],1);

  aRes.ContentType('application/json')
    .Send(TUsuarioService.New.UsuarioGet(FId,FLogin,FNome,FStatus));

  {
  aRes.ContentType('application/json')
    .Send(TUsuarioService.New.UsuarioGet(StrToIntDef(aReq.Params['id'],0),
                                         aReq.Params['login'],
                                         aReq.Params['nome']));
  }
end;

class procedure TBaseRoute.Load();
begin
  THorse.Get('/', OnStatus);
  THorse.Get('/teste', OnTeste);

  THorse.Post('/login',OnLogin);
  THorse
    .Get('/usuario',OnUsuarioGet);
end;

end.

