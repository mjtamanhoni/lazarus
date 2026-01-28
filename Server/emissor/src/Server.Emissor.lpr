program Server.Emissor;

{$mode delphi}{$H+}

uses
  Classes, SysUtils,
  Horse, Horse.JWT, Horse.Jhonson,
  zcomponent,
  uBase.Router, uUsuario.Service, uCripto_Descrito, uDM, uBase.Functions,
  uEmpresa.Service;

procedure OnListen();
begin
  WriteLn(Format('Server active on port: %d',[THorse.Port]));
  WriteLn(Format('Executable in Folder: %s',[ParamStr(0)]));
  WriteLn(Format('Developed by: %s',['MJT-System']));
  WriteLn(Format('E-Mail: %s',['mjtamanhoni@gmail.com']));
  WriteLn(Format('Mobile phone (WhatsApp): %s',['(27) 98833-7323']));
  WriteLn(Format('Server started on: %s',[FormatDateTime('dd/mm/yyy hh:nn:ss',Now)]));

  SaveLog(sLineBreak + '============================================================================',False);
  SaveLog(Format('Server started on: %s, on port %d',[FormatDateTime('dd/mm/yyy "at" hh:nn:ss',Now),THorse.Port]));
  SaveLog('----------------------------------------------------------------------------',False)
end;

begin
  //Depois mudar a senha 'Horse_2026', por algo mais complexo (CNPJ do cliente)
  //Validador de Token
  THorse.Use(HorseJWT(C_SECRET_JWT,THorseJWTConfig.New.SkipRoutes([
             '/'
  	     ,'/login'
             ,'/usuario'
             ,'/empresa']
  )));

  //Lendo as rotas..
  TBaseRoute.Load();

  //Start...
  THorse.Listen(9095, OnListen);
end.


