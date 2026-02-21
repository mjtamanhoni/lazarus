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
  uUsuario.Service,uEmpresa.Service;

{$Region 'Servidor'}
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
{$EndRegion 'Servidor'}

{$Region 'Usuário'}
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
{$EndRegion 'Usuário'}

{$Region 'Empresa'}
procedure OnEmpresaGet(Req:THorseRequest; Res:THorseResponse);
var
  FId :Integer;
  FCNPJ :String;
  FRazaoSocial :String;
  FNomeFantasia :String;
  FStatus :Integer;
begin
  FId := StrToIntDef(Req.Query['id'],0);
  FCNPJ := Req.Query['cnpj'];
  FRazaoSocial := Req.Query['rasaoSocial'];
  FNomeFantasia := Req.Query['nomeFantasia'];
  FStatus := StrToIntDef(Req.Query['status'],1);

  Res.ContentType('application/json')
      .Send(TEmpresaService.New.EmpresaGet(FId,FRazaoSocial,FNomeFantasia,FCNPJ,FStatus));
end;

procedure OnValida_CNPJ_Empresa(Req:THorseRequest; Res:THorseResponse);
var
  FCNPJ :String;
begin
  FCNPJ := Req.Query['cnpj'];
  Res.ContentType('application/json')
      .Send(TEmpresaService.New.Valida_CNPJ_Empresa(FCNPJ));

end;

procedure OnEmpresaPost(Req:THorseRequest; Res:THorseResponse);
begin
  Res.ContentType('application/json')
      .Send(TEmpresaService.New.EmpresaPost(Req.Body));
end;

procedure OnEmpresaPut(Req:THorseRequest; Res:THorseResponse);
begin
  Res.ContentType('application/json')
      .Send(TEmpresaService.New.EmpresaPut(Req.Body));
end;

procedure OnEmpresaDel(Req:THorseRequest; Res:THorseResponse);
var
  FId :Integer;
  FCNPJ :String;
begin
  FId := StrToIntDef(Req.Query['id'],0);
  FCNPJ := Req.Query['cnpj'];
  Res.ContentType('application/json')
      .Send(TEmpresaService.New.EmpresaDelete(FId,FCNPJ));
end;

procedure OnEmpresaDel_End(Req:THorseRequest; Res:THorseResponse);
var
  FId_Empresa :Integer;
  FId_End :Integer;
begin
  FId_Empresa := StrToIntDef(Req.Query['idEmpresa'],0);
  FId_End := StrToIntDef(Req.Query['idEndereco'],0);

  Res.ContentType('application/json')
      .Send(TEmpresaService.New.EmpresaDelete_End(FId_Empresa,FId_End));
end;

procedure OnEmpresaDel_DB(Req:THorseRequest; Res:THorseResponse);
var
  FId_Empresa :Integer;
  FId_DB :Integer;
begin
  FId_Empresa := StrToIntDef(Req.Query['idEmpresa'],0);
  FId_DB := StrToIntDef(Req.Query['idDBanco'],0);

  Res.ContentType('application/json')
      .Send(TEmpresaService.New.EmpresaDelete_DB(FId_Empresa,FId_DB));
end;

{$EndRegion 'Empresa}

class procedure TBaseRoute.Load();
begin
  THorse.Get('/', OnStatus);
  THorse.Get('/teste', OnTeste);

{$Region 'Usuário'}
  THorse.Post('/login',OnLogin);
  THorse.Get('/usuario',OnUsuarioGet)
        .Post('/usuario',OnUsuarioPost)
        .Put('/usuario',OnUsuarioPut)
        .Delete('/usuario',OnUsuarioDel);
{$EndRegion 'Usuário'}

{$Region 'Empresa'}
  THorse.Get('/empresa',OnEmpresaGet)
        .Get('empresa/validaCnpj',OnValida_CNPJ_Empresa)
        .Post('/empresa',OnEmpresaPost)
        .Put('/empresa',OnEmpresaPut)
        .Delete('/empresa',OnEmpresaDel)
        .Delete('/empresa/endereco',OnEmpresaDel_End)
        .Delete('/empresa/dBanco',OnEmpresaDel_DB);
{$EndRegion 'Empresa'}

end;

end.

