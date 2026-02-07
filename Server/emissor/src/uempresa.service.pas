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
    function EmpresaPost(const AJSon :String):String;
    function EmpresaPut(const AJSon :String):String;
    function EmpresaDelete(const AId:Integer; const ACNPJ:String):String;
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
    function EmpresaPost(const AJSon :String):String;
    function EmpresaPut(const AJSon :String):String;
    function EmpresaDelete(const AId:Integer; const ACNPJ:String):String;

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
  FLista :String;
begin
  try
    try
      FDM := TDM.Create(Nil);
      FQuery := FDM.GetQuery;

      FJSonobject := TJSONObject.Create;

      FQuery.SQL.Add('select ');
      FQuery.SQL.Add('  e.* ');
      FQuery.SQL.Add('  ,cast(case e.crt ');
      FQuery.SQL.Add('     when ''1'' then ''Simples Nacional'' ');
      FQuery.SQL.Add('     when ''2'' then ''Simples Nacional - excesso de sublimite de receita bruta'' ');
      FQuery.SQL.Add('     when ''3'' then ''Regime Normal (Lucro Presumido ou Real)'' ');
      FQuery.SQL.Add('     when ''4'' then ''Microempreendedor Individual (MEI)'' ');
      FQuery.SQL.Add('  end as varchar(255)) as crt_desc ');
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
        raise Exception.Create('Empresa não localizada');

      FJSonobject.Add('success',True);

      //Empresa...
      FJSonobject.Add('data',FQuery.ToJSONArray);

      //Lista de empresas...
      FLista := '';
      FQuery.Close;
      FQuery.SQL.Clear;
      FQuery.Sql.Add('select ');
      FQuery.Sql.Add('	string_agg(e.id_empresa::text,'','') as empresas ');
      FQuery.Sql.Add('from public.empresa e ');
      FQuery.Sql.Add('where 1=1 ');
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
      FQuery.Open;
      if ((not FQuery.IsEmpty) and (not FQuery.FieldByName('empresas').IsNull)) then
        FLista := FQuery.FieldByName('empresas').AsString;

      if Trim(FLista) <> '' then
      begin
        //Endereços...
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.Sql.Add('select ');
        FQuery.Sql.Add('  ee.* ');
        FQuery.Sql.Add('  ,cast(case ee.tipo_endereco ');
        FQuery.Sql.Add('     when 0 then ''Comercial'' ');
        FQuery.Sql.Add('     when 1 then ''Residencial'' ');
        FQuery.Sql.Add('     when 2 then ''Entrega (Shipping)'' ');
        FQuery.Sql.Add('     when 3 then ''Cobrança/Faturamento'' ');
        FQuery.Sql.Add('     when 4 then ''Correspondência'' ');
        FQuery.Sql.Add('     when 5 then ''Endereço de Instalação (quando envolve serviços técnicos), endereço rural'' ');
        FQuery.Sql.Add('     when 6 then ''Endereço Rural (para produtores)'' ');
        FQuery.Sql.Add('     when 7 then ''Endereço Temporário (Eventos, obras)'' ');
        FQuery.Sql.Add('  end as varchar(255)) as tipo_endereco_desc ');
        FQuery.Sql.Add('from public.endereco_empresa ee ');
        FQuery.Sql.Add('where ee.id_empresa in ('+FLista+') ');
        FQuery.Sql.Add('order by ');
        FQuery.Sql.Add('  ee.id_empresa ');
        FQuery.Sql.Add('  ,ee.id_endereco; ');
        FQuery.Open;
        FJSonobject.Add('endereco',FQuery.ToJSONArray);

        //Contas bancárias...
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.Sql.Add('select ');
        FQuery.Sql.Add('  db.* ');
        FQuery.Sql.Add('  ,cast(case db.tipo_conta ');
        FQuery.Sql.Add('     when 0 then ''Corrente (PF/PJ) - Movimentação diária'' ');
        FQuery.Sql.Add('     when 1 then ''Poupança (PF) - Guardar e render dinheiro'' ');
        FQuery.Sql.Add('     when 2 then ''Salário (PF) - Receber salário/benefícios'' ');
        FQuery.Sql.Add('     when 3 then ''Universitária/Jovem (Estudantes/jovens) - Condições especiais'' ');
        FQuery.Sql.Add('     when 4 then ''PJ/MEI (Empresas/autônomos) - Gestão financeira empresarial'' ');
        FQuery.Sql.Add('     when 5 then ''Digital (PF/PJ) - Movimentação online'' ');
        FQuery.Sql.Add('     when 6 then ''Investimento (PF/PJ) - Aplicações financeiras'' ');
        FQuery.Sql.Add('     when 7 then ''Conjunta (PF) - Compartilhar recursos'' ');
        FQuery.Sql.Add('  end as varchar(255)) as tipo_conta_desc ');
        FQuery.Sql.Add('from public.dados_bancarios db ');
        FQuery.Sql.Add('where db.id_empresa in ('+FLista+') ');
        FQuery.Sql.Add('order by ');
        FQuery.Sql.Add('  db.id_empresa ');
        FQuery.Sql.Add('  ,db.id_banco; ');
        FQuery.Open;
        FJSonobject.Add('contaBancaria',FQuery.ToJSONArray);

        //Certificado digital...
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.Sql.Add('select ');
        FQuery.Sql.Add('  cd.* ');
        FQuery.Sql.Add('  ,cast(case cd.tipo ');
        FQuery.Sql.Add('     when ''0'' then ''Certificado Digital A1'' ');
        FQuery.Sql.Add('     when ''1'' then ''Certificado Digital A3'' ');
        FQuery.Sql.Add('  end as varchar(255)) as tipo_desc ');
        FQuery.Sql.Add('from public.certificado_digital cd ');
        FQuery.Sql.Add('where cd.id_empresa in ('+FLista+') ');
        FQuery.Sql.Add('order by ');
        FQuery.Sql.Add('  cd.id_empresa ');
        FQuery.Sql.Add('  ,cd.id_certificado; ');
        FQuery.Open;
        FJSonobject.Add('certificadoDigital',FQuery.ToJSONArray);
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
  FJson_End :TJSONArray;  //Endereço
  FJson_CB :TJSONArray;  //Conta bancária
  FJson_CD :TJSONObject;  //Certificado digital
  FId_Empresa :Integer;
  FDm :TDM;
  FQuery :TZQuery;
  I :Integer;
begin
  try
    try
      FDm := TDM.Create(Nil);
      FQuery := FDm.GetQuery;

      FDm.ZConnection.StartTransaction;

      if AJSon.IsEmpty and not AJSon.StartsWith('{') and not AJSon.EndsWith('}') then
        Raise Exception.Create('JSon Inválido!');

      SaveLog(AJSon);

      FJson := TJSONObject(GetJSON(AJSon));
      FJson_End := TJSONArray(GetJSON(FJson['endereco'].AsJSON));
      FJson_CB := TJSONArray(GetJSON(FJson['contaBancaria'].AsJSON));
      FJson_CD := TJSONObject(GetJSON(FJson['certificadoDigital'].AsJSON));

      FId_Empresa := 0;

      if Trim(FJson['cnpj'].AsString) = '' then
        raise Exception.Create('CNPJ não informado: este campo é obrigatório.');
      if Trim(FJson['razaoSocial'].AsString) = '' then
        raise Exception.Create('Razão Social não informada: este campo é obrigatório.');
      if Trim(FJson['inscricaoEstadual'].AsString) = '' then
        raise Exception.Create('Inscrição Estadual não informada: este campo é obrigatório.');
      if Trim(FJson['regimeTributario'].AsString) = '' then
        raise Exception.Create('Regime Tributário não informado: este campo é obrigatório.');

      {$Region 'Empresa'}
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
      FQuery.SQL.Add(') ');
      FQuery.SQL.Add('RETURNING id_empresa; ');
      FQuery.ParamByName('razao_social').AsString := FJson.Strings['razaoSocial'];
      if (FJson.Nulls['nomeFantasia']) then
        FQuery.ParamByName('nome_fantasia').Clear
      else
        FQuery.ParamByName('nome_fantasia').AsString := FJson.Strings['nomeFantasia'];
      FQuery.ParamByName('cnpj').AsString := FJson.Strings['cnpj'];
      FQuery.ParamByName('inscricao_estadual').AsString := FJson.Strings['inscricaoEstadual'];
      if (FJson.Nulls['inscricaoMunicipal']) then
        FQuery.ParamByName('inscricao_municipal').Clear
      else
        FQuery.ParamByName('inscricao_municipal').AsString := FJson.Strings['inscricaoMunicipal'];
      if (FJson.Nulls['regimeTributario']) then
        FQuery.ParamByName('regime_tributario').Clear
      else
        FQuery.ParamByName('regime_tributario').AsString := FJson.Strings['regimeTributario'];
      FQuery.ParamByName('crt').AsString := FJson.Strings['crt'];
      if (FJson.Nulls['email']) then
        FQuery.ParamByName('email').Clear
      else
        FQuery.ParamByName('email').AsString := FJson.Strings['email'];
      if (FJson.Nulls['telefone']) then
        FQuery.ParamByName('telefone').Clear
      else
        FQuery.ParamByName('telefone').AsString := FJson.Strings['telefone'];
      if (FJson.Nulls['site']) then
        FQuery.ParamByName('site').Clear
      else
        FQuery.ParamByName('site').AsString := FJson.Strings['site'];
      if (FJson.Nulls['celular']) then
        FQuery.ParamByName('celular').Clear
      else
        FQuery.ParamByName('celular').AsString := FJson.Strings['celular'];
      FQuery.Open;
      FId_Empresa := FQuery.FieldByName('id_empresa').AsInteger;
      {$EndRegion 'Empresa'}

      {$Region 'Endereço'}
      for I := 0 to Pred(FJson_End.Count) do
      begin
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.SQL.Add('INSERT INTO public.endereco_empresa ( ');
        FQuery.SQL.Add('	id_empresa ');
        FQuery.SQL.Add('	,logradouro ');
        FQuery.SQL.Add('	,numero ');
        FQuery.SQL.Add('	,complemento ');
        FQuery.SQL.Add('	,bairro ');
        FQuery.SQL.Add('	,municipio ');
        FQuery.SQL.Add('	,codigo_municipio_ibge ');
        FQuery.SQL.Add('	,uf ');
        FQuery.SQL.Add('	,cep ');
        FQuery.SQL.Add('	,pais ');
        FQuery.SQL.Add('	,codigo_pais_ibge ');
        FQuery.SQL.Add('	,tipo_endereco ');
        FQuery.SQL.Add(') VALUES( ');
        FQuery.SQL.Add('	:id_empresa ');
        FQuery.SQL.Add('	,:logradouro ');
        FQuery.SQL.Add('	,:numero ');
        FQuery.SQL.Add('	,:complemento ');
        FQuery.SQL.Add('	,:bairro ');
        FQuery.SQL.Add('	,:municipio ');
        FQuery.SQL.Add('	,:codigo_municipio_ibge ');
        FQuery.SQL.Add('	,:uf ');
        FQuery.SQL.Add('	,:cep ');
        FQuery.SQL.Add('	,:pais ');
        FQuery.SQL.Add('	,:codigo_pais_ibge ');
        FQuery.SQL.Add('	,:tipo_endereco ');
        FQuery.SQL.Add('); ');
        FQuery.ParamByName('id_empresa').AsInteger := FId_Empresa;
        FQuery.ParamByName('logradouro').AsString := FJson_End.Objects[I].Strings['logradouro'];
        FQuery.ParamByName('numero').AsString := FJson_End.Objects[I].Strings['numero'];
        FQuery.ParamByName('complemento').AsString := FJson_End.Objects[I].Strings['complemento'];
        FQuery.ParamByName('bairro').AsString := FJson_End.Objects[I].Strings['bairro'];
        FQuery.ParamByName('municipio').AsString := FJson_End.Objects[I].Strings['municipio'];
        FQuery.ParamByName('codigo_municipio_ibge').AsString := FJson_End.Objects[I].Strings['codigomunicipioibge'];
        FQuery.ParamByName('uf').AsString := FJson_End.Objects[I].Strings['uf'];
        FQuery.ParamByName('cep').AsString := FJson_End.Objects[I].Strings['cep'];
        FQuery.ParamByName('pais').AsString := FJson_End.Objects[I].Strings['pais'];
        FQuery.ParamByName('codigo_pais_ibge').AsString := FJson_End.Objects[I].Strings['codigopaisibge'];
        FQuery.ParamByName('tipo_endereco').AsInteger := FJson_End.Objects[I].Integers['tipoendereco'];
        FQuery.ExecSQL;
      end;
      {$EndRegion 'Endereço'}

      {$Region 'Conta Bancária'}
      for I := 0 to Pred(FJson_CB.Count) do
      begin
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.SQL.Add('INSERT INTO public.dados_bancarios( ');
	FQuery.SQL.Add('  id_empresa ');
	FQuery.SQL.Add('  ,banco ');
	FQuery.SQL.Add('  ,agencia ');
	FQuery.SQL.Add('  ,conta ');
	FQuery.SQL.Add('  ,tipo_conta ');
        FQuery.SQL.Add(') VALUES( ');
	FQuery.SQL.Add('  :id_empresa ');
	FQuery.SQL.Add('  ,:banco ');
	FQuery.SQL.Add('  ,:agencia ');
	FQuery.SQL.Add('  ,:conta ');
	FQuery.SQL.Add('  ,:tipo_conta ');
        FQuery.SQL.Add('); ');
        FQuery.ParamByName('id_empresa').AsInteger := FId_Empresa;
        FQuery.ParamByName('banco').AsString := FJson_CB.Objects[I].Strings['banco'];
        FQuery.ParamByName('agencia').AsString := FJson_CB.Objects[I].Strings['agencia'];
        FQuery.ParamByName('conta').AsString := FJson_CB.Objects[I].Strings['conta'];
        FQuery.ParamByName('tipo_conta').AsInteger := FJson_CB.Objects[I].Integers['tipoconta'];
        FQuery.ExecSQL;
      end;
      {$EndRegion 'Conta Bancária'}

      {$Region 'Certificado Digital'}
      FQuery.Close;
      FQuery.SQL.Clear;
      FQuery.SQL.Add('INSERT INTO public.certificado_digital( ');
      FQuery.SQL.Add('	id_empresa ');
      FQuery.SQL.Add('	,tipo ');
      FQuery.SQL.Add('	,validade ');
      FQuery.SQL.Add('	,caminho_arquivo ');
      FQuery.SQL.Add('	,senha ');
      FQuery.SQL.Add(') VALUES( ');
      FQuery.SQL.Add('	:id_empresa ');
      FQuery.SQL.Add('	,:tipo ');
      FQuery.SQL.Add('	,:validade ');
      FQuery.SQL.Add('	,:caminho_arquivo ');
      FQuery.SQL.Add('	,:senha ');
      FQuery.SQL.Add('); ');
      FQuery.ParamByName('id_empresa').AsInteger := FId_Empresa;
      FQuery.ParamByName('tipo').AsInteger := FJson_CD.Integers['tipo'];
      FQuery.ParamByName('validade').AsDate := DateOf(FJson_CD.Floats['validade']);
      FQuery.ParamByName('caminho_arquivo').AsString := FJson_CD.Strings['caminhoArquivo'];
      FQuery.ParamByName('senha').AsString := FJson_CD.Strings['senha'];
      FQuery.ExecSQL;
      {$EndRegion 'Certificado Digital'}


      FDm.ZConnection.Commit;

      Result :='{"success":true,"message":"Empresa inserida com sucesso. ID: ' + IntToStr(FId_Empresa) + '"}';
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


      if AJSon.IsEmpty and not AJSon.StartsWith('{') and not AJSon.EndsWith('}') then
        Raise Exception.Create('JSon Inválido!');

      FJson := TJSONObject(GetJSON(AJSon));

      if ((FJson['idEmpresa'].IsNull) or (FJson['idEmpresa'].AsInteger = 0)) then
        raise Exception.Create('Id da Empresa não informado');

      FDm.ZTransaction.StartTransaction;

      //Atualizando empresa...
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
      FQuery.ExecSQL;

      //Atualizando Endereço...

      //Atualizando Conta Bancária...

      //Atualizando Certificado...


      FDm.ZTransaction.Commit;

      Result :='{"success":true,"message":"Empresa utualizada com sucesso"}';
    except
      on E:Exception do
      begin
        FDm.ZTransaction.Rollback;
        SaveLog(E.Message);
        Result :='{"success":false,"message":"'+E.Message+'"}';
      end;
    end;
  finally
    FreeAndNil(FQuery);
    FreeAndNil(FDm);
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

