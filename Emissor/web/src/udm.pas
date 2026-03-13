unit udm;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ZConnection, DateUtils,
  ZDataset, IniFiles, Variants, uBase.Functions,
  fpjson, DataSet.Serialize, RESTRequest4D, jsonparser;

type

  { TDM }

  TDM = class(TDataModule)
    ZConnection: TZConnection;
    ZQuery1: TZQuery;
    ZTransaction: TZTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FIniFile :TIniFile;
  published
    class procedure CreateInstance;
    procedure DestroyInstance;
  public
    { Public declarations }
    procedure ConectarBanco;
    function GetQuery:TZQuery;
    function Sequencial(const aTabela:String;const aId_Empresa:Integer=0;const aId_Usuario:Integer=0):Integer;
    function Sequencial_Dinamico(const aTabela: String; const aFields: array of String; const aValues: array of Variant): Integer;
  end;

  { TDM_Empresa }

  TDM_Empresa = class
  private
  public
    function Empresa_Delete(const aId_Empresa: Integer): Boolean;
    function Empresa_PostPut(const aJSon:TJSONObject): Boolean;
    procedure Empresa_Get(
      const FQuery :TZQuery;
      const aId_Empresa:Integer=0;
      const aRazao_Social:String='';
      const aFantasia:String='';
      const aCNPJ:String='');
  end;



function DM: TDM;

implementation

uses
  D2Bridge.Instance, EmissorWebApp;

{$R *.lfm}

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  FIniFile := TIniFile.Create(ConfigFile);
  ConectarBanco;
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FIniFile);
end;

class procedure TDM.CreateInstance;
begin
  D2BridgeInstance.CreateInstance(self);
end;

function DM: TDM;
begin
  result := (D2BridgeInstance.GetInstance(TDM) as TDM);
end;

procedure TDM.DestroyInstance;
begin
  D2BridgeInstance.DestroyInstance(self);
end;

procedure TDM.ConectarBanco;
begin
  try
    ZConnection.Protocol := '';
    ZConnection.HostName := '';
    ZConnection.Port     := 0;
    ZConnection.Database := '';
    ZConnection.User     := '';
    ZConnection.Password := '';
    ZConnection.LibraryLocation:= '';
    ZConnection.Disconnect;

    //Validando parâmetros...
    if Trim(FIniFile.ReadString('DataBase','Protocol','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Protocolo não informado');
    if Trim(FIniFile.ReadString('DataBase','HostName','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'HostName não informado');
    if Trim(FIniFile.ReadString('DataBase','Port','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Porta não informada');
    if Trim(FIniFile.ReadString('DataBase','Database','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Banco de Dados não informado');
    if Trim(FIniFile.ReadString('DataBase','User','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Usuário não informado');
    if Trim(FIniFile.ReadString('DataBase','Password','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Senha não informada');
    if Trim(FIniFile.ReadString('DataBase','LibraryLocation','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Biblioteca não informada');

    ZConnection.Protocol := Trim(FIniFile.ReadString('DataBase','Protocol','')); // protocolo Zeos
    ZConnection.HostName := Trim(FIniFile.ReadString('DataBase','HostName',''));
    ZConnection.Port     := StrToIntDef(Trim(FIniFile.ReadString('DataBase','Port','0')),0);
    ZConnection.Database := Trim(FIniFile.ReadString('DataBase','Database',''));
    ZConnection.User     := Trim(FIniFile.ReadString('DataBase','User',''));
    ZConnection.Password := Trim(FIniFile.ReadString('DataBase','Password',''));
    ZConnection.LibraryLocation := Trim(FIniFile.ReadString('DataBase','LibraryLocation',''));
    ZConnection.LoginPrompt := False;

    ZConnection.Connect; // tenta conectar

    if not ZConnection.Connected then
      raise Exception.Create('Banco de Dados não Conectado');

  except
    on E:Exception do
       GravarLogJSON(Self.Name,'Banco de Dados', 'ConectarBanco', E);
  end;

end;

function TDM.GetQuery: TZQuery;
begin
  Result := TZQuery.Create(Nil);
  Result.Connection := ZConnection;
  Result.Transaction := ZTransaction;
  Result.Close;
  Result.SQL.Clear;
end;

function TDM.Sequencial(const aTabela: String; const aId_Empresa: Integer; const aId_Usuario: Integer): Integer;
var
  FQuery :TZQuery;
begin
  try
    try
      Result := 0;

      if aId_Empresa = 0 then
        raise Exception.Create('O id da Empresa é obrigatório');
      if Trim(aTabela) = '' then
        raise Exception.Create('O nome da Tabela é obrigatório');

      FQuery := GetQuery;
      FQuery.SQL.Add('SELECT obter_sequencial(:id_empresa, :id_usuario, :nome_tabela)');
      FQuery.ParamByName('id_empresa').AsInteger := aId_Empresa;
      FQuery.ParamByName('id_usuario').AsInteger := aId_Usuario;
      FQuery.ParamByName('nome_tabela').AsString := aTabela;
      FQuery.Open;

      Result := FQuery.Fields[0].AsInteger;

    except
      on E:Exception do
        raise Exception.Create('Gera sequencial.' + sLineBreak + E.Message);
    end;
  finally
    FreeAndNil(FQuery);
  end;

end;

function TDM.Sequencial_Dinamico(const aTabela: String; const aFields: array of String; const aValues: array of Variant): Integer;
var
  i: Integer;
  SQL: String;
begin
  // SQL base
  SQL := 'SELECT COALESCE(MAX(CODIGO), 0) + 1 FROM ' + aTabela + ' WHERE 1=1';

  // Adiciona os filtros dinamicamente
  for i := 0 to High(aFields) do
  begin
    // Tratando o valor para SQL (VarToStr ou tratamento de tipos)
    //SQL := SQL + ' AND ' + aFields[i] + ' = ' + VarToSQLStr(aValues[i]);
  end;

  // Execução no componente de Query (Ex: SQLQuery1)
  //SQLQuery1.Close;
  //SQLQuery1.SQL.Text := SQL;
  //SQLQuery1.Open;

  //Result := SQLQuery1.Fields[0].AsInteger;

end;

{ TDM_Empresa }

function TDM_Empresa.Empresa_Delete(const aId_Empresa: Integer): Boolean;
var
  FDM :TDM;
  FQuery :TZQuery;
begin
  try
    try
      Result := True;

      FDM := TDM.Create(Nil);
      FDM.ConectarBanco;;

      FQuery := FDM.GetQuery;
      FQuery.Sql.Add('DELETE FROM public.empresa ');
      if aId_Empresa > 0 then
        FQuery.Sql.Add('WHERE id_empresa = ' + aId_Empresa.ToString);
      FQuery.ExecSQL;
    except
      On E:Exception do
      begin
        Result := False;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    FreeAndNil(FQuery);
    FreeAndNil(FDM);
  end;
end;

function TDM_Empresa.Empresa_PostPut(const aJSon: TJSONObject): Boolean;
var
  FDM :TDM;
  FQuery :TZQuery;
  FJSon_Empresa :TJSONArray;
  FJSon_Endereco :TJSONArray;
  FJSon_DBanco :TJSONArray;
  FJSon_Certificado :TJSONArray;
  I :Integer;

begin
  try
    try
      Result := True;

      FDM := TDM.Create(Nil);
      FDM.ConectarBanco;;

      FQuery := FDM.GetQuery;

      FJSon_Empresa := TJSONArray(GetJSON(aJSon['data'].AsJSON));
      FJSon_Endereco := TJSONArray(GetJSON(aJSon['endereco'].AsJSON));
      FJSon_DBanco := TJSONArray(GetJSON(aJSon['contaBancaria'].AsJSON));
      FJSon_Certificado := TJSONArray(GetJSON(aJSon['certificadoDigital'].AsJSON));


      //Inserindo empresa...
      for I := 0 to Pred(FJSon_Empresa.Count) do
      begin
        //Excluindo empresa e tabelas associadas...
        Empresa_Delete(FJSon_Empresa.Objects[I].Integers['idEmpresa']);

        FQuery.Close;
        FQuery.Sql.Clear;
        FQuery.Sql.Add('INSERT INTO public.empresa ( ');
        FQuery.Sql.Add('	id_empresa ');
        FQuery.Sql.Add('	,razao_social ');
        FQuery.Sql.Add('	,nome_fantasia ');
        FQuery.Sql.Add('	,cnpj ');
        FQuery.Sql.Add('	,inscricao_estadual ');
        FQuery.Sql.Add('	,inscricao_municipal ');
        FQuery.Sql.Add('	,regime_tributario ');
        FQuery.Sql.Add('	,crt ');
        FQuery.Sql.Add('	,email ');
        FQuery.Sql.Add('	,telefone ');
        FQuery.Sql.Add('	,site ');
        FQuery.Sql.Add('	,data_cadastro ');
        FQuery.Sql.Add('	,ativo ');
        FQuery.Sql.Add('	,celular ');
        FQuery.Sql.Add(') VALUES( ');
        FQuery.Sql.Add('	:id_empresa ');
        FQuery.Sql.Add('	,:razao_social ');
        FQuery.Sql.Add('	,:nome_fantasia ');
        FQuery.Sql.Add('	,:cnpj ');
        FQuery.Sql.Add('	,:inscricao_estadual ');
        FQuery.Sql.Add('	,:inscricao_municipal ');
        FQuery.Sql.Add('	,:regime_tributario ');
        FQuery.Sql.Add('	,:crt ');
        FQuery.Sql.Add('	,:email ');
        FQuery.Sql.Add('	,:telefone ');
        FQuery.Sql.Add('	,:site ');
        FQuery.Sql.Add('	,:data_cadastro ');
        FQuery.Sql.Add('	,:ativo ');
        FQuery.Sql.Add('	,:celular ');
        FQuery.Sql.Add('); ');
        FQuery.ParamByName('id_empresa').AsInteger := FJSon_Empresa.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('razao_social').AsString := FJSon_Empresa.Objects[I].Strings['razaoSocial'];
        FQuery.ParamByName('nome_fantasia').AsString := FJSon_Empresa.Objects[I].Strings['nomeFantasia'];
        FQuery.ParamByName('cnpj').AsString := FJSon_Empresa.Objects[I].Strings['cnpj'];
        FQuery.ParamByName('inscricao_estadual').AsString := FJSon_Empresa.Objects[I].Strings['inscricaoEstadual'];
        FQuery.ParamByName('inscricao_municipal').AsString := FJSon_Empresa.Objects[I].Strings['inscricaoMunicipal'];
        FQuery.ParamByName('regime_tributario').AsString := FJSon_Empresa.Objects[I].Strings['regimeTributario'];
        FQuery.ParamByName('crt').AsString := FJSon_Empresa.Objects[I].Strings['crt'];
        FQuery.ParamByName('email').AsString := FJSon_Empresa.Objects[I].Strings['email'];
        FQuery.ParamByName('telefone').AsString := FJSon_Empresa.Objects[I].Strings['telefone'];
        FQuery.ParamByName('site').AsString := FJSon_Empresa.Objects[I].Strings['site'];
        FQuery.ParamByName('data_cadastro').AsDateTime := ISO8601ToDateDef(FJSon_Empresa.Objects[I].Strings['dataCadastro'],0);
        FQuery.ParamByName('ativo').AsInteger := FJSon_Empresa.Objects[I].Integers['ativo'];
        FQuery.ParamByName('celular').AsString := FJSon_Empresa.Objects[I].Strings['celular'];
        FQuery.ExecSQL;
      end;

      //Inserindo endereço da empresa...
      for I := 0 to Pred(FJSon_Endereco.Count) do
      begin
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.Sql.Add('INSERT INTO public.endereco_empresa ( ');
        FQuery.Sql.Add('	id_endereco ');
        FQuery.Sql.Add('	,id_empresa ');
        FQuery.Sql.Add('	,logradouro ');
        FQuery.Sql.Add('	,numero ');
        FQuery.Sql.Add('	,complemento ');
        FQuery.Sql.Add('	,bairro ');
        FQuery.Sql.Add('	,municipio ');
        FQuery.Sql.Add('	,codigo_municipio_ibge ');
        FQuery.Sql.Add('	,uf ');
        FQuery.Sql.Add('	,cep ');
        FQuery.Sql.Add('	,pais ');
        FQuery.Sql.Add('	,codigo_pais_ibge ');
        FQuery.Sql.Add('	,tipo_endereco ');
        FQuery.Sql.Add(') VALUES( ');
        FQuery.Sql.Add('	:id_endereco ');
        FQuery.Sql.Add('	,:id_empresa ');
        FQuery.Sql.Add('	,:logradouro ');
        FQuery.Sql.Add('	,:numero ');
        FQuery.Sql.Add('	,:complemento ');
        FQuery.Sql.Add('	,:bairro ');
        FQuery.Sql.Add('	,:municipio ');
        FQuery.Sql.Add('	,:codigo_municipio_ibge ');
        FQuery.Sql.Add('	,:uf ');
        FQuery.Sql.Add('	,:cep ');
        FQuery.Sql.Add('	,:pais ');
        FQuery.Sql.Add('	,:codigo_pais_ibge ');
        FQuery.Sql.Add('	,:tipo_endereco ');
        FQuery.Sql.Add('); ');
        FQuery.ParamByName('id_endereco').AsInteger := FJSon_Endereco.Objects[I].Integers['idEndereco'];
        FQuery.ParamByName('id_empresa').AsInteger := FJSon_Endereco.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('logradouro').AsString := FJSon_Endereco.Objects[I].Strings['logradouro'];
        FQuery.ParamByName('numero').AsString := FJSon_Endereco.Objects[I].Strings['numero'];
        FQuery.ParamByName('complemento').AsString := FJSon_Endereco.Objects[I].Strings['complemento'];
        FQuery.ParamByName('bairro').AsString := FJSon_Endereco.Objects[I].Strings['bairro'];
        FQuery.ParamByName('municipio').AsString := FJSon_Endereco.Objects[I].Strings['municipio'];
        FQuery.ParamByName('codigo_municipio_ibge').AsString := FJSon_Endereco.Objects[I].Strings['codigoMunicipioIbge'];
        FQuery.ParamByName('uf').AsString := FJSon_Endereco.Objects[I].Strings['uf'];
        FQuery.ParamByName('cep').AsString := FJSon_Endereco.Objects[I].Strings['cep'];
        FQuery.ParamByName('pais').AsString := FJSon_Endereco.Objects[I].Strings['pais'];
        FQuery.ParamByName('codigo_pais_ibge').AsString := FJSon_Endereco.Objects[I].Strings['codigoPaisIbge'];
        FQuery.ParamByName('tipo_endereco').AsInteger := FJSon_Endereco.Objects[I].Integers['tipoEndereco'];
        FQuery.ExecSQL;
      end;

      //Inserindo dados bancários da empresa...
      for I := 0 to Pred(FJSon_DBanco.Count) do
      begin
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.Sql.Add('INSERT INTO public.dados_bancarios ( ');
        FQuery.Sql.Add('	id_banco ');
        FQuery.Sql.Add('	,id_empresa ');
        FQuery.Sql.Add('	,banco ');
        FQuery.Sql.Add('	,agencia ');
        FQuery.Sql.Add('	,conta ');
        FQuery.Sql.Add('	,tipo_conta ');
        FQuery.Sql.Add(') VALUES( ');
        FQuery.Sql.Add('	:id_banco ');
        FQuery.Sql.Add('	,:id_empresa ');
        FQuery.Sql.Add('	,:banco ');
        FQuery.Sql.Add('	,:agencia ');
        FQuery.Sql.Add('	,:conta ');
        FQuery.Sql.Add('	,:tipo_conta ');
        FQuery.Sql.Add('); ');
        FQuery.ParamByName('id_banco').AsInteger := FJSon_DBanco.Objects[I].Integers['idBanco'];
        FQuery.ParamByName('id_empresa').AsInteger := FJSon_DBanco.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('banco').AsString := FJSon_DBanco.Objects[I].Strings['banco'];
        FQuery.ParamByName('agencia').AsString := FJSon_DBanco.Objects[I].Strings['agencia'];
        FQuery.ParamByName('conta').AsString := FJSon_DBanco.Objects[I].Strings['conta'];
        FQuery.ParamByName('tipo_conta').AsInteger := FJSon_DBanco.Objects[I].Integers['tipoConta'];
        FQuery.ExecSQL;
      end;

      //Inserindo dados do certificado...
      for I := 0 to Pred(FJSon_Certificado.Count) do
      begin
        FQuery.Close;
        FQuery.SQL.Clear;
        FQuery.Sql.Add('INSERT INTO public.certificado_digital ( ');
        FQuery.Sql.Add('	id_certificado ');
        FQuery.Sql.Add('	,id_empresa ');
        FQuery.Sql.Add('	,tipo ');
        FQuery.Sql.Add('	,validade ');
        FQuery.Sql.Add('	,caminho_arquivo ');
        FQuery.Sql.Add('	,senha ');
        FQuery.Sql.Add(') VALUES ');
        FQuery.Sql.Add('	:id_certificado ');
        FQuery.Sql.Add('	,:id_empresa ');
        FQuery.Sql.Add('	,:tipo ');
        FQuery.Sql.Add('	,:validade ');
        FQuery.Sql.Add('	,:caminho_arquivo ');
        FQuery.Sql.Add('	,:senha ');
        FQuery.Sql.Add('); ');
        FQuery.ParamByName('id_certificado').AsInteger := FJSon_Certificado.Objects[I].Integers['idCertificado'];
        FQuery.ParamByName('id_empresa').AsInteger := FJSon_Certificado.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('tipo').AsString := FJSon_Certificado.Objects[I].Strings['tipo'];
        FQuery.ParamByName('validade').AsDateTime := StrISOToDateTime(FJSon_Certificado.Objects[I].Strings['validade']);
        FQuery.ParamByName('caminho_arquivo').AsString := FJSon_Certificado.Objects[I].Strings['caminhoArquivo'];
        FQuery.ParamByName('senha').AsString := FJSon_Certificado.Objects[I].Strings['senha'];
        FQuery.ExecSQL;
      end;

    except
      On E:Exception do
      begin
        Result := False;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    FreeAndNil(FQuery);
    FreeAndNil(FDM);
  end;
end;

procedure TDM_Empresa.Empresa_Get(
  const FQuery :TZQuery;
  const aId_Empresa: Integer;
  const aRazao_Social: String;
  const aFantasia: String;
  const aCNPJ: String);
begin
  try
    try
      FQuery.DisableControls;

      FQuery.Close;
      FQuery.Sql.Clear;
      FQuery.Sql.Add('SELECT ');
      FQuery.Sql.Add('  e.* ');
      FQuery.Sql.Add('  ,case e.crt ');
      FQuery.Sql.Add('    when ''1'' then ''Simples Nacional'' ');
      FQuery.Sql.Add('    when ''2'' then ''Simples Nacional - excesso de sublimite de receita bruta'' ');
      FQuery.Sql.Add('    when ''3'' then ''Regime Normal (Lucro Presumido ou Real)'' ');
      FQuery.Sql.Add('    when ''4'' then ''Microempreendedor Individual (MEI)'' ');
      FQuery.Sql.Add('  end crt_desc ');
      FQuery.Sql.Add('  ,case e.ativo ');
      FQuery.Sql.Add('    when 0 then ''Inativo'' ');
      FQuery.Sql.Add('    when 1 then ''Ativo'' ');
      FQuery.Sql.Add('  end ativo_desc ');
      FQuery.Sql.Add('FROM public.empresa e ');
      FQuery.Sql.Add('where 1=1 ');
      if aId_Empresa > 0 then
        FQuery.Sql.Add('  and e.id_empresa = ' + aId_Empresa.ToString);
      if Trim(aRazao_Social) <> '' then
        FQuery.Sql.Add('  and e.razao_social = ' + QuotedStr(aRazao_Social));
      if Trim(aFantasia) <> '' then
        FQuery.Sql.Add('  and e.nome_fantasia = ' + QuotedStr(aFantasia));
      if Trim(aCNPJ) <> '' then
        FQuery.Sql.Add('  and e.cnpj = ' + QuotedStr(RemoverMascara(aCNPJ)));
      FQuery.Open;
    except
      On E:Exception do
        raise Exception.Create(E.Message);
    end;
  finally
    FQuery.EnableControls;
    //FreeAndNil(FQuery);
  end;
end;

end.
