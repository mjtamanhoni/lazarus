unit uUsuario.Service;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  fpjson,  //Trabalhar com Json
  jsonparser,
  ezthreads, //Realiza um parse de uma string para um objeto Json
  ZDataset,
  DataSet.Serialize, TypInfo,
  uCripto_Descrito, uDM, uBase.Functions;


type
  IUsuarioService = interface
    ['{62A73D68-B994-4060-8FE3-AF7304CBC5A4}'] //Ctrl+Shift+G - GUID (Globally Unique Identifier)
    function Login(aJSonString: String):String;
    function UsuarioGet(
    	     const AId:Integer;
             const ALogin:String;
             const ANome:String;
             const AStatus:Integer):String;
    function UsuarioPost(
    	     const AJSon :String):String;
    function UsuarioPut(
    	     const AJSon :String):String;
    function UsuarioDelete(
    	     const AId:Integer):String;
  end;


  { TUsuarioService }

  //Classe está herdando de TInterfacedObject e vai implementar o IUsuarioService
  TUsuarioService = class(TInterfacedObject, IUsuarioService)
  public
    constructor Create;
    destructor destroy; override;

    class function New:IUsuarioService;

    function Login(aJSonString: String):String;
    function UsuarioGet(
    	     const AId:Integer;
             const ALogin:String;
             const ANome:String;
             const AStatus:Integer):String;
    function UsuarioPost(
    	     const AJSon :String):String;
    function UsuarioPut(
    	     const AJSon :String):String;
    function UsuarioDelete(
    	     const AId:Integer):String;

  end;

implementation

{ TUsuarioService }

uses
  LazJWT;

constructor TUsuarioService.Create;
begin

end;

destructor TUsuarioService.destroy;
begin
  inherited destroy;
end;

class function TUsuarioService.New: IUsuarioService;
begin
  Result := Self.Create;
end;

function TUsuarioService.Login(aJSonString: String): String;
var
  FJSonobject :TJSONObject;
  FJSon :TJSONObject;
  FJSonRetorno :TJSONObject;
  FCNPJ :String; //Cnpj/Cpf
  FUsuario :String;
  FPassword :String;
  FDM :TDM;
  FQuery :TZQuery;
  FToken :String;
begin
  try
    try
      FDM := TDM.Create(Nil);
      FQuery := FDM.GetQuery;

      FJSonRetorno := TJSONObject.Create;
      FJSon := TJSONObject.Create;

      if aJSonString.IsEmpty and not aJSonString.StartsWith('{') and not aJSonString.EndsWith('}') then
        Raise Exception.Create('JSon Inválido!');

      FJSonobject := TJSONObject(GetJSON(aJSonString));

      if (FJSonobject.Find('cnpj') = Nil) then
        Raise Exception.Create('CNPJ não encontrado');
      if (FJSonobject.Find('usuario') = Nil) then
        Raise Exception.Create('Usuário/Senha não encontrado');
      if (FJSonobject.Find('password') = Nil) then
        Raise Exception.Create('Usuário/Senha não encontrado');

      FCNPJ := FJSonobject['cnpj'].AsString;
      FUsuario := FJSonobject['usuario'].AsString;
      FPassword := FJSonobject['password'].AsString;

      //Validar Usuário e PassWord no banco de dados...
      FQuery.SQL.Add('select ');
      FQuery.SQL.Add('  u.* ');
      FQuery.SQL.Add('from public.usuarios u ');
      FQuery.SQL.Add('where  u.login = :login; ');
      FQuery.ParamByName('login').AsString := FUsuario;
      FQuery.Open;
      if FQuery.IsEmpty then
        raise Exception.Create('Usuário não localizado.');

      //Valida senha depois
      if Descriptografar(FPassword) <> Descriptografar(FQuery.FieldByName('senha').AsString) then
        raise Exception.Create('Senha não confere.');

      FJSon := FQuery.ToJSONObject;

      FToken := TLazJWT.New
        .SecretJWT(C_SECRET_JWT)
        .Exp(DateTimeToUnix(IncHour(Now,1)))
        .AddClaim('cnpj',FCNPJ)
        .AddClaim('login',FUsuario)
        .AddClaim('password',FPassword)
        .AddClaim('nome',FQuery.FieldByName('nome').AsString)
        .AddClaim('ativo',FQuery.FieldByName('ativo').AsInteger)
        .Token;

      FJSon.Add('token',FToken);

      FJSonRetorno.Add('success',True);
      FJSonRetorno.Add('data',FJSon);

      Result := FJSonRetorno.AsJSON;

    except
      on E:Exception do
      begin
        SaveLog(E.Message);
        Result := '{"success":false,"message":"'+E.Message+'"}';
      end;
    end;
  finally
    FreeAndNil(FQuery);
    FreeAndNil(FDM);
    FreeAndNil(FJSonRetorno);
  end;
end;

function TUsuarioService.UsuarioGet(
         const AId: Integer;
         const ALogin:String;
         const ANome:String;
         const AStatus:Integer):String;
var
  FJSonobject :TJSONObject;
  FDM :TDM;
  FQuery :TZQuery;
begin
  try
    try
      FDM := TDM.Create(Nil);
      FQuery := FDM.GetQuery;
      FJSonobject := TJSONObject.Create;

      FQuery.SQL.Add('select ');
      FQuery.SQL.Add('  u.* ');
      FQuery.SQL.Add('from public.usuarios u ');
      FQuery.SQL.Add('where 1=1 ');
      if AId > 0 then
      begin
        FQuery.SQL.Add('  and u.id_usuario = :id_usuario');
        FQuery.ParamByName('id_usuario').AsInteger := AId;
      end;
      if not ALogin.IsEmpty then
      begin
        FQuery.SQL.Add('  and u.login = :login');
        FQuery.ParamByName('login').AsString := ALogin;
      end;
      if not ANome.IsEmpty then
      begin
        FQuery.SQL.Add('  and u.nome like :nome');
        FQuery.ParamByName('nome').AsString := '%'+ANome+'%';
      end;
      if AStatus in [0,1] then
      begin
        FQuery.SQL.Add('  and u.ativo = :ativo');
        FQuery.ParamByName('ativo').AsInteger := AStatus;
      end;
      FQuery.SQL.Add('order by ');
      FQuery.SQL.Add('  u.id_usuario; ');
      FQuery.Open;

      if FQuery.IsEmpty then
        raise Exception.Create('Usuário não localizado')
      else
      begin
        FJSonobject.Add('success',True);
        FJSonobject.Add('data',FQuery.ToJSONArray);
      end;

      Result := FJSonobject.AsJSON;

    except
      on E :Exception do
      begin
        FJSonobject.Add('success',False);
        FJSonobject.Add('message',E.Message);
        Result := FJSonobject.AsJSON;
        SaveLog(E.Message);
      end;
    end;
  finally
    FreeAndNil(FJSonobject);
    FreeAndNil(FQuery);
    FreeAndNil(FDM);

    //É destruído assim junto FJsonobject
    //FreeAndNil(FJsonTexto);
  end;

end;

function TUsuarioService.UsuarioPost(const AJSon: String): String;
var
  FJson :TJSONObject;
  FDm :TDM;
  FQuery :TZQuery;
begin
  try
    try
      FDm := TDM.Create(Nil);
      FQuery := FDm.GetQuery;

      FDm.ZConnection.StartTransaction;

      if AJSon.IsEmpty and not AJSon.StartsWith('{') and not AJSon.EndsWith('}') then
        Raise Exception.Create('JSon Inválido!');

      FJson := TJSONObject(GetJSON(AJSon));

      if Trim(FJson['login'].AsString) = '' then
        raise Exception.Create('Login não informado: este campo é obrigatório.');
      if Trim(FJson['senha'].AsString) = '' then
        raise Exception.Create('Login não informada: este campo é obrigatório.');

      FQuery.SQL.Add('insert into public.usuarios ( ');
      FQuery.SQL.Add('  login ');
      FQuery.SQL.Add('  ,senha ');
      FQuery.SQL.Add('  ,nome ');
      FQuery.SQL.Add('  ,email ');
      if not FJson['id_perfil'].IsNull then
        FQuery.SQL.Add('  ,id_perfil ');
      FQuery.SQL.Add(') values( ');
      FQuery.SQL.Add('  :login ');
      FQuery.SQL.Add('  ,:senha ');
      FQuery.SQL.Add('  ,:nome ');
      FQuery.SQL.Add('  ,:email ');
      if not FJson['id_perfil'].IsNull then
        FQuery.SQL.Add('  ,:id_perfil ');
      FQuery.SQL.Add('); ');
      FQuery.ParamByName('login').AsString := FJson['login'].AsString;
      FQuery.ParamByName('senha').AsString := FJson['senha'].AsString;
      FQuery.ParamByName('nome').AsString := FJson['nome'].AsString;
      FQuery.ParamByName('email').AsString := FJson['email'].AsString;
      if not FJson['id_perfil'].IsNull then
        FQuery.ParamByName('id_perfil').AsInteger := FJson['id_perfil'].AsInteger;
      FQuery.ExecSQL;

      FDm.ZConnection.Commit;

      Result :='{"success":true,"message":"Usuário inserido com sucesso"}';
    except
      on E:Exception do
      begin
        FDm.ZConnection.Rollback;
        SaveLog(E.Message);
        Result :='{"success":false,"message":"'+E.Message+'"}';
      end;
    end;
  finally
    FreeAndNil(FDm);
    FreeAndNil(FQuery);
  end;
end;

function TUsuarioService.UsuarioPut(const AJSon: String): String;
var
  FJson :TJSONObject;
  FDm :TDM;
  FQuery :TZQuery;
begin
  try
    try
      FDm := TDM.Create(Nil);
      FQuery := FDm.GetQuery;

      FDm.ZConnection.StartTransaction;

      if AJSon.IsEmpty and not AJSon.StartsWith('{') and not AJSon.EndsWith('}') then
        Raise Exception.Create('JSon Inválido!');

      FJson := TJSONObject(GetJSON(AJSon));

      FQuery.SQL.Add('update public.usuarios set ');
      FQuery.SQL.Add('  login = :login ');
      FQuery.SQL.Add('  ,senha = :senha ');
      FQuery.SQL.Add('  ,nome = :nome ');
      FQuery.SQL.Add('  ,email = :email ');
      FQuery.SQL.Add('  ,ativo = :ativo ');
      if not FJson['id_perfil'].IsNull then
        FQuery.SQL.Add('  ,id_perfil = :id_perfil ');
      FQuery.SQL.Add('where id_usuario = :id_usuario; ');
      FQuery.ParamByName('login').AsString := FJson['login'].AsString;
      FQuery.ParamByName('senha').AsString := FJson['senha'].AsString;
      FQuery.ParamByName('nome').AsString := FJson['nome'].AsString;
      FQuery.ParamByName('email').AsString := FJson['email'].AsString;
      FQuery.ParamByName('ativo').AsInteger := FJson['ativo'].AsInteger;
      if not FJson['id_perfil'].IsNull then
        FQuery.ParamByName('id_perfil').AsInteger := FJson['id_perfil'].AsInteger;
      FQuery.ExecSQL;

      FDm.ZConnection.Commit;

      Result :='{"success":true,"message":"Usuário alterado com sucesso"}';
    except
      on E:Exception do
      begin
        FDm.ZConnection.Rollback;
        SaveLog(E.Message);
        Result :='{"success":false,"message":"'+E.Message+'"}';
      end;
    end;
  finally
    FreeAndNil(FDm);
    FreeAndNil(FQuery);
  end;
end;

function TUsuarioService.UsuarioDelete(const AId: Integer): String;
var
  FJson :TJSONObject;
  FDm :TDM;
  FQuery :TZQuery;
begin
  try
    try
      FDm := TDM.Create(Nil);
      FQuery := FDm.GetQuery;

      FDm.ZConnection.StartTransaction;

      if AId = 0 then
        raise Exception.Create('Não foi informado o ID do usuário');

      FQuery.SQL.Add('delete from public.usuarios where id_usuario = :id_usuario;');
      FQuery.ParamByName('id_usuario').AsInteger := AId;
      FQuery.ExecSQL;

      FDm.ZConnection.Commit;

      Result :='{"success":true,"message":"Usuário excluído com sucesso"}';

    except
      on E:Exception do
      begin
        FDm.ZConnection.Rollback;
        SaveLog(E.Message);
        Result :='{"success":false,"message":"'+E.Message+'"}';
      end;
    end;
  finally
    FreeAndNil(FDm);
    FreeAndNil(FQuery);
  end;
end;

end.

