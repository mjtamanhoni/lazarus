unit uEmpresa.Service;

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
  IEmpresaService = interface
    ['{44025F87-B18E-4E0D-A304-9D783A40F9CD}'] //Ctrl+Shift+G - GUID (Globally Unique Identifier)
    function EmpresaGet(
    	     const AId:Integer;
             const ARazaoSocial:String;
             const ANomeFantasia:String;
             const ACNPJ:String;
             const AStatus:Integer):String;
    function EmpresaPost(
    	     const AJSon :String):String;
    function EmpresaPut(
    	     const AJSon :String):String;
    function EmpresaDelete(
    	     const AId:Integer;
             const ACNPJ:String):String;
  end;


  { TEmpresaService }

  //Classe está herdando de TInterfacedObject e vai implementar o IEmpresaService
  TEmpresaService = class(TInterfacedObject, IEmpresaService)
  public
    constructor Create;
    destructor destroy; override;

    class function New:IEmpresaService;

    function EmpresaGet(
    	     const AId:Integer;
             const ARazaoSocial:String;
             const ANomeFantasia:String;
             const ACNPJ:String;
             const AStatus:Integer):String;
    function EmpresaPost(
    	     const AJSon :String):String;
    function EmpresaPut(
    	     const AJSon :String):String;
    function EmpresaDelete(
    	     const AId:Integer;
             const ACNPJ:String):String;

  end;

implementation

uses
  LazJWT;

{ TEmpresaService }

constructor TEmpresaService.Create;
begin

end;

destructor TEmpresaService.destroy;
begin
  inherited destroy;
end;

class function TEmpresaService.New: IEmpresaService;
begin
  Result := Self.Create;
end;

function TEmpresaService.EmpresaGet(
         const AId: Integer;
  	 const ARazaoSocial: String;
         const ANomeFantasia: String;
         const ACNPJ: String;
  	 const AStatus: Integer): String;
var
  FJSonObject :TJSONObject;
  FDM :TDM;
  FQuery :TZQuery;
begin
  try
    try
      FDM := TDM.Create(Nil);
      FQuery := FDM.GetQuery;

      FJSonobject := TJSONObject.Create;

      FQuery.SQL.Add('select ');
      FQuery.SQL.Add('  e.* ');
      FQuery.SQL.Add('from public.empresa e ');
      FQuery.SQL.Add('where 1=1 ');
      if AId > 0 then
      begin
        FQuery.SQL.Add('  AND e.id_empresa = :id_empresa ');
        FQuery.ParamByName('id_empresa').AsInteger := AId;
      end;
      if Trim(ACNPJ) <> '' then
      begin
        FQuery.SQL.Add('  and e.cnpj  = :cnpj ');
        FQuery.ParamByName('cnpj').AsString := ACNPJ;
      end;
      if Trim(ARazaoSocial) <> '' then
      begin
        FQuery.SQL.Add('  and e.razao_social = :razao_social ');
        FQuery.ParamByName('razao_social').AsString := ARazaoSocial;
      end;
      if Trim(ANomeFantasia) <> '' then
      begin
        FQuery.SQL.Add('  and e.nome_fantasia = :nome_fantasia ');
        FQuery.ParamByName('nome_fantasia').AsString := ANomeFantasia;
      end;
      if AStatus in [0,1] then
      begin
        FQuery.SQL.Add('  and e.ativo = :ativo');
        FQuery.ParamByName('ativo').AsInteger := AStatus;
      end;
      FQuery.SQL.Add('order by ');
      FQuery.SQL.Add('  e.id_empresa; ');

      FQuery.Open;

      if FQuery.IsEmpty then
        raise Exception.Create('Empresa não localizada')
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
  end;
end;

function TEmpresaService.EmpresaPost(const AJSon: String): String;
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

      if Trim(FJson['cnpj'].AsString) = '' then
        raise Exception.Create('CNPJ não informado: este campo é obrigatório.');
      if Trim(FJson['razaoSocial'].AsString) = '' then
        raise Exception.Create('Razão Social não informada: este campo é obrigatório.');
      if Trim(FJson['inscricaoEstadual'].AsString) = '' then
        raise Exception.Create('Inscrição Estadual não informada: este campo é obrigatório.');
      if Trim(FJson['regimeTributario'].AsString) = '' then
        raise Exception.Create('Regime Tributário não informado: este campo é obrigatório.');

      FQuery.SQL.Add('INSERT INTO public.empresa ( ');
      FQuery.SQL.Add('  razao_social ');
      FQuery.SQL.Add('  ,nome_fantasia ');
      FQuery.SQL.Add('  ,cnpj ');
      FQuery.SQL.Add('  ,inscricao_estadual ');
      FQuery.SQL.Add('  ,inscricao_municipal ');
      FQuery.SQL.Add('  ,regime_tributario ');
      FQuery.SQL.Add('  ,crt ');
      FQuery.SQL.Add('  ,email ');
      FQuery.SQL.Add('  ,telefone ');
      FQuery.SQL.Add('  ,site ');
      FQuery.SQL.Add('  ,celular ');
      FQuery.SQL.Add(') VALUES( ');
      FQuery.SQL.Add('  :razao_social ');
      FQuery.SQL.Add('  ,:nome_fantasia ');
      FQuery.SQL.Add('  ,:cnpj ');
      FQuery.SQL.Add('  ,:inscricao_estadual ');
      FQuery.SQL.Add('  ,:inscricao_municipal ');
      FQuery.SQL.Add('  ,:regime_tributario ');
      FQuery.SQL.Add('  ,:crt ');
      FQuery.SQL.Add('  ,:email ');
      FQuery.SQL.Add('  ,:telefone ');
      FQuery.SQL.Add('  ,:site ');
      FQuery.SQL.Add('  ,:celular ');
      FQuery.SQL.Add('); ');
      FQuery.ParamByName('razao_social').AsString := FJson['razaoSocial'].AsString;
      if ((Trim(FJson['nomeFantasia'].AsString) = '') or (FJson['nomeFantasia'].IsNull)) then
        FQuery.ParamByName('nome_fantasia').Clear
      else
        FQuery.ParamByName('nome_fantasia').AsString := FJson['nomeFantasia'].AsString;
      FQuery.ParamByName('cnpj').AsString := FJson['cnpj'].AsString;
      FQuery.ParamByName('inscricao_estadual').AsString := FJson['inscricaoEstadual'].AsString;
      if ((Trim(FJson['inscricaoMunicipal'].AsString) = '') or (FJson['inscricaoMunicipal'].IsNull)) then
        FQuery.ParamByName('inscricao_municipal').Clear
      else
        FQuery.ParamByName('inscricao_municipal').AsString := FJson['inscricaoMunicipal'].AsString;
      if ((Trim(FJson['regimeTributario'].AsString) = '') or (FJson['regimeTributario'].IsNull)) then
        FQuery.ParamByName('regime_tributario').Clear
      else
        FQuery.ParamByName('regime_tributario').AsString := FJson['regimeTributario'].AsString;
      FQuery.ParamByName('crt').AsString := FJson['crt'].AsString;
      if ((Trim(FJson['email'].AsString) = '') or (FJson['email'].IsNull)) then
        FQuery.ParamByName('email').Clear
      else
        FQuery.ParamByName('email').AsString := FJson['email'].AsString;
      if ((Trim(FJson['telefone'].AsString) = '') or (FJson['telefone'].IsNull)) then
        FQuery.ParamByName('telefone').Clear
      else
        FQuery.ParamByName('telefone').AsString := FJson['telefone'].AsString;
      if ((Trim(FJson['site'].AsString) = '') or (FJson['site'].IsNull)) then
        FQuery.ParamByName('site').Clear
      else
        FQuery.ParamByName('site').AsString := FJson['site'].AsString;
      if ((Trim(FJson['celular'].AsString) = '') or (FJson['celular'].IsNull)) then
        FQuery.ParamByName('celular').Clear
      else
        FQuery.ParamByName('celular').AsString := FJson['celular'].AsString;
      FQuery.ExecSQL;

      FDm.ZConnection.Commit;

      Result :='{"success":true,"message":"Empresa inserida com sucesso"}';
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

function TEmpresaService.EmpresaPut(const AJSon: String): String;
var
  FJson :TJSONObject;
  FDm :TDM;
  FQuery :TZQuery;
begin
  try
    try
      FDm := TDM.Create(Nil);
      FQuery := FDm.GetQuery;

      //FDm.ZConnection.StartTransaction;
      FDm.ZTransaction.StartTransaction;

      if AJSon.IsEmpty and not AJSon.StartsWith('{') and not AJSon.EndsWith('}') then
        Raise Exception.Create('JSon Inválido!');

      FJson := TJSONObject(GetJSON(AJSon));

      if ((FJson['idEmpresa'].IsNull) or (FJson['idEmpresa'].AsInteger = 0)) then
        raise Exception.Create('Id da Empresa não informado');

      FQuery.SQL.Add('update public.empresa set ');
      FQuery.SQL.Add('  razao_social = :razao_social ');
      FQuery.SQL.Add('  ,nome_fantasia = :nome_fantasia ');
      FQuery.SQL.Add('  ,cnpj = :cnpj ');
      FQuery.SQL.Add('  ,inscricao_estadual = :inscricao_estadual ');
      FQuery.SQL.Add('  ,inscricao_municipal = :inscricao_municipal ');
      FQuery.SQL.Add('  ,regime_tributario = :regime_tributario ');
      FQuery.SQL.Add('  ,crt = :crt ');
      FQuery.SQL.Add('  ,email = :email ');
      FQuery.SQL.Add('  ,telefone = :telefone ');
      FQuery.SQL.Add('  ,site = :site ');
      FQuery.SQL.Add('  ,ativo = :ativo ');
      FQuery.SQL.Add('  ,celular = :celular ');
      FQuery.SQL.Add('where id_empresa = :id_empresa; ');
      FQuery.ParamByName('id_empresa').AsInteger := FJson['idEmpresa'].AsInteger;
      FQuery.ParamByName('razao_social').AsString := FJson['razaoSocial'].AsString;
      FQuery.ParamByName('nome_fantasia').AsString := FJson['nomeFantasia'].AsString;
      FQuery.ParamByName('cnpj').AsString := RemoverMascara(FJson['cnpj'].AsString);
      FQuery.ParamByName('inscricao_estadual').AsString := FJson['inscricaoEstadual'].AsString;
      FQuery.ParamByName('inscricao_municipal').AsString := FJson['inscricaoMunicipal'].AsString;
      FQuery.ParamByName('regime_tributario').AsString := FJson['regimeTributario'].AsString;
      FQuery.ParamByName('crt').AsString := FJson['crt'].AsString;
      FQuery.ParamByName('email').AsString := FJson['email'].AsString;
      FQuery.ParamByName('telefone').AsString := FJson['telefone'].AsString;
      FQuery.ParamByName('site').AsString := FJson['site'].AsString;
      FQuery.ParamByName('celular').AsString := FJson['celular'].AsString;
      FQuery.ParamByName('ativo').AsInteger := FJson['ativo'].AsInteger;

      SaveLog(FQuery.SQL.Text);
      SaveLog('id_empresa: ' + IntToStr(FQuery.ParamByName('id_empresa').AsInteger) + sLineBreak +
              'razao_social: ' + FQuery.ParamByName('razao_social').AsString + sLineBreak +
              'nome_fantasia: ' + FQuery.ParamByName('nome_fantasia').AsString + sLineBreak +
              'cnpj: ' + FQuery.ParamByName('cnpj').AsString + sLineBreak +
              'inscricao_estaudal: ' + FQuery.ParamByName('inscricao_estadual').AsString + sLineBreak +
              'inscricao_muinicipal: ' + FQuery.ParamByName('inscricao_municipal').AsString + sLineBreak +
              'regime_tributario: ' + FQuery.ParamByName('regime_tributario').AsString + sLineBreak +
              'crt: ' + FQuery.ParamByName('crt').AsString + sLineBreak +
              'email: ' + FQuery.ParamByName('email').AsString + sLineBreak +
              'telefone: ' + FQuery.ParamByName('telefone').AsString + sLineBreak +
              'site: ' + FQuery.ParamByName('site').AsString + sLineBreak +
              'celular: ' + FQuery.ParamByName('celular').AsString + sLineBreak +
              'ativo: ' + IntToStr(FQuery.ParamByName('ativo').AsInteger));


      FQuery.ExecSQL;

      //FDm.ZConnection.Commit;
      FDm.ZTransaction.Commit;

      Result :='{"success":true,"message":"Empresa utualizada com sucesso"}';
    except
      on E:Exception do
      begin
        //FDm.ZConnection.Rollback;
        FDm.ZTransaction.Rollback;
        SaveLog(E.Message);
        Result :='{"success":false,"message":"'+E.Message+'"}';
      end;
    end;
  finally
    FreeAndNil(FDm);
    FreeAndNil(FQuery);
  end;
end;

function TEmpresaService.EmpresaDelete(
         const AId: Integer;
         const ACNPJ: String): String;
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

      if ((AId = 0) and (Trim(ACNPJ) <> '')) then
        raise Exception.Create('Não foi informado o ID e ou CNPJ da empresa');

      FQuery.SQL.Add('delete from public.empresa ');
      if AId > 0 then
      begin
        FQuery.SQL.Add(' where id_empresa = :id_empresa;');
        FQuery.ParamByName('id_empresa').AsInteger := AId;
      end
      else if Trim(ACNPJ) <> '' then
      begin
        FQuery.SQL.Add(' where cnpj = :cnpj;');
        FQuery.ParamByName('cnpj').AsString := ACNPJ;
      end;

      FQuery.ExecSQL;
      FDm.ZConnection.Commit;

      Result :='{"success":true,"message":"Empresa excluída com sucesso"}';

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

