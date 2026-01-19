unit uUsuario.Service;

{$mode Delphi}

interface

uses
  Classes, SysUtils, DateUtils,
  fpjson, //Trabalhar com Json
  jsonparser; //Realiza um parse de uma string para um objeto Json


type
  IUsuarioService = interface
    ['{62A73D68-B994-4060-8FE3-AF7304CBC5A4}'] //Ctrl+Shift+G - GUID (Globally Unique Identifier)
    function Login(aJSonString: String):String;
  end;


  { TUsuarioService }

  //Classe está herdando de TInterfacedObject e vai implementar o IUsuarioService
  TUsuarioService = class(TInterfacedObject, IUsuarioService)
  public
    constructor Create;
    destructor destroy; override;
    class function New:IUsuarioService;
    function Login(aJSonString: String):String;

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
      .SecretJWT('Horse_2026')
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

end.

