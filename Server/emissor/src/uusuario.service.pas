unit uUsuario.Service;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  fpjson,  //Trabalhar com Json
  jsonparser,
  ezthreads, //Realiza um parse de uma string para um objeto Json
  ZDataset,
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
  FCNPJ :String; //Cnpj/Cpf
  FUsuario :String;
  FPassword :String;
begin

  try
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

    Result := TLazJWT.New
      .SecretJWT(C_SECRET_JWT)
      .Exp(DateTimeToUnix(IncHour(Now,1)))
      .AddClaim('cnpj',FCNPJ)
      .AddClaim('usuario',FUsuario)
      .AddClaim('password',FPassword)
      .Token;
  except
    on E:Exception do
      Result := E.Message;
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
      FJSonobject.Add('success',True);

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
        FQuery.SQL.Add('  and u.nome = :nome');
        FQuery.ParamByName('nome').AsString := ANome;
      end;
      if AStatus in [0,1] then
      begin
        FQuery.SQL.Add('  and u.ativo = :ativo');
        FQuery.ParamByName('ativo').AsInteger := AStatus;
      end;

      //FJSonobject.Add('data',FJsonTexto);
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

end.




(*
function TUsuarioService.UsuarioGet(
         const AId: Integer;
         const ALogin:String;
         const ANome:String):String;
var
  FJSonobject :TJSONObject;
  FJsonTexto :TJSONObject;
  FDM :TDM;
begin
  try
    try
      FJSonobject := TJSONObject.Create;
      {
      FJSonobject.Add('success',True);

      FJsonTexto := TJSONObject.Create;
      FJsonTexto.Add('Id',AId);
      FJsonTexto.Add('Login',ALogin);
      FJsonTexto.Add('Nome',ANome);

      FJSonobject.Add('data',FJsonTexto);
      Result := FJSonobject.AsJSON;
      }
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
    //FreeAndNil(FJsonTexto);
  end;

end;

*)
