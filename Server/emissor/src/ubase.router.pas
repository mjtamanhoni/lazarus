unit uBase.Router;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Horse, fpjson;

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

procedure OnStatus(Req:THorseRequest; Res:THorseResponse);
begin
  Res.ContentType('text/html')
    .Send(Format('<h1>Server on-line - Horse version <i>%s</i></h1>',[THorse.Version]));
end;

procedure OnTeste(Req:THorseRequest; Res:THorseResponse);
begin
  Res.ContentType('text/html')
    .Send('Endpoint teste');
end;

procedure OnLogin(Req:THorseRequest; Res:THorseResponse);
begin
  Res.ContentType('text/html')
    .Send(TUsuarioService.New.Login(Req.Body));
end;

procedure OnUsuarioGet(Req:THorseRequest; Res:THorseResponse);
var
  FId :Integer;
  FLogin :String;
  FNome :String;
  FStatus :Integer;
begin
  FId := StrToIntDef(Req.Query['id'],0);
  FLogin := Req.Query['login'];
  FNome := Req.Query['nome'];
  FStatus := StrToIntDef(Req.Query['status'],1);

  Res.ContentType('application/json')
      .Send(TUsuarioService.New.UsuarioGet(FId,FLogin,FNome,FStatus));
end;

procedure OnUsuarioPost(Req:THorseRequest; Res:THorseResponse);
begin
  Res.ContentType('application/json')
      .Send(TUsuarioService.New.UsuarioPost(Req.Body));
end;

procedure OnUsuarioPut(Req:THorseRequest; Res:THorseResponse);
begin
  Res.ContentType('application/json')
      .Send(TUsuarioService.New.UsuarioPut(Req.Body));
end;

procedure OnUsuarioDel(Req:THorseRequest; Res:THorseResponse);
var
  FId :Integer;
begin
  FId := StrToIntDef(Req.Query['id'],0);
  Res.ContentType('application/json')
      .Send(TUsuarioService.New.UsuarioDelete(FId));
end;

class procedure TBaseRoute.Load();
begin
  THorse.Get('/', OnStatus);
  THorse.Get('/teste', OnTeste);

  THorse.Post('/login',OnLogin);
  THorse.Get('/usuario',OnUsuarioGet)
        .Post('/usuario',OnUsuarioPost)
        .Put('/usuario',OnUsuarioPut)
        .Delete('/usuario',OnUsuarioDel);
end;

end.

