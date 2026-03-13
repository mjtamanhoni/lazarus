unit udm;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ZConnection,
  ZDataset, IniFiles, Variants, uBase.Functions,
  fpjson, DataSet.Serialize, RESTRequest4D, jsonparser;
;

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

  { TEmpresa }

  TEmpresa = class
  private
  public
    function Empresa_Delete(const aId_Empresa: Integer): Boolean;
    function Empresa_PostPut(const aJSon:TJSONArray): Boolean;
    function Empresa_Get(
      const aId_Empresa:Integer=0;
      const aRazao_Social:String='';
      const aFantasia:String='';
      const aCNPJ:String=''):TZQuery;
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

{ TEmpresa }

function TEmpresa.Empresa_Delete(const aId_Empresa: Integer): Boolean;
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

function TEmpresa.Empresa_PostPut(const aJSon: TJSONArray): Boolean;
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

      //Excluindo empresa e tabelas associadas...
      Empresa_Delete(0);

      //Inserindo empresa...
      for I := 0 to Pred(AJSon_Empresa.Count) do
      begin
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
        FQuery.ParamByName('id_empresa').AsInteger := AJSon_Empresa.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('razao_social').AsString := AJSon_Empresa.Objects[I].Strings['razaoSocial'];
        FQuery.ParamByName('nome_fantasia').AsString := AJSon_Empresa.Objects[I].Strings['nomeFantasia'];
        FQuery.ParamByName('cnpj').AsString := AJSon_Empresa.Objects[I].Strings['cnpj'];
        FQuery.ParamByName('inscricao_estadual').AsString := AJSon_Empresa.Objects[I].Strings['inscricaoEstadual'];
        FQuery.ParamByName('inscricao_municipal').AsString := AJSon_Empresa.Objects[I].Strings['inscricaoMunicipal'];
        FQuery.ParamByName('regime_tributario').AsString := AJSon_Empresa.Objects[I].Strings['regimeTributario'];
        FQuery.ParamByName('crt').AsString := AJSon_Empresa.Objects[I].Strings['crt'];
        FQuery.ParamByName('email').AsString := AJSon_Empresa.Objects[I].Strings['email'];
        FQuery.ParamByName('telefone').AsString := AJSon_Empresa.Objects[I].Strings['telefone'];
        FQuery.ParamByName('site').AsString := AJSon_Empresa.Objects[I].Strings['site'];
        FQuery.ParamByName('data_cadastro').AsDateTime := ISO8601ToDateDef(AJSon_Empresa.Objects[I].Strings['dataCadastro'],0);
        FQuery.ParamByName('ativo').AsInteger := AJSon_Empresa.Objects[I].Integers['ativo'];
        FQuery.ParamByName('celular').AsString := AJSon_Empresa.Objects[I].Strings['celular'];
        FQuery.ExecSQL;
      end;

      //Inserindo endereço da empresa...
      for I := 0 to Pred(AJSon_Endereco.Count) do
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
        FQuery.ParamByName('id_endereco').AsInteger := AJSon_Endereco.Objects[I].Integers['idEndereco'];
        FQuery.ParamByName('id_empresa').AsInteger := AJSon_Endereco.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('logradouro').AsString := AJSon_Endereco.Objects[I].Strings['logradouro'];
        FQuery.ParamByName('numero').AsString := AJSon_Endereco.Objects[I].Strings['numero'];
        FQuery.ParamByName('complemento').AsString := AJSon_Endereco.Objects[I].Strings['complemento'];
        FQuery.ParamByName('bairro').AsString := AJSon_Endereco.Objects[I].Strings['bairro'];
        FQuery.ParamByName('municipio').AsString := AJSon_Endereco.Objects[I].Strings['municipio'];
        FQuery.ParamByName('codigo_municipio_ibge').AsString := AJSon_Endereco.Objects[I].Strings['codigoMunicipioIbge'];
        FQuery.ParamByName('uf').AsString := AJSon_Endereco.Objects[I].Strings['uf'];
        FQuery.ParamByName('cep').AsString := AJSon_Endereco.Objects[I].Strings['cep'];
        FQuery.ParamByName('pais').AsString := AJSon_Endereco.Objects[I].Strings['pais'];
        FQuery.ParamByName('codigo_pais_ibge').AsString := AJSon_Endereco.Objects[I].Strings['codigoPaisIbge'];
        FQuery.ParamByName('tipo_endereco').AsInteger := AJSon_Endereco.Objects[I].Integers['tipoEndereco'];
        FQuery.ExecSQL;
      end;

      //Inserindo dados bancários da empresa...
      for I := 0 to Pred(AJSon_CBanco.Count) do
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
        FQuery.ParamByName('id_banco').AsInteger := AJSon_CBanco.Objects[I].Integers['idBanco'];
        FQuery.ParamByName('id_empresa').AsInteger := AJSon_CBanco.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('banco').AsString := AJSon_CBanco.Objects[I].Strings['banco'];
        FQuery.ParamByName('agencia').AsString := AJSon_CBanco.Objects[I].Strings['agencia'];
        FQuery.ParamByName('conta').AsString := AJSon_CBanco.Objects[I].Strings['conta'];
        FQuery.ParamByName('tipo_conta').AsInteger := AJSon_CBanco.Objects[I].Integers['tipoConta'];
        FQuery.ExecSQL;
      end;

      //Inserindo dados do certificado...
      for I := 0 to Pred(AJSon_Certificado.Count) do
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
        FQuery.ParamByName('id_certificado').AsInteger := AJSon_Certificado.Objects[I].Integers['idCertificado'];
        FQuery.ParamByName('id_empresa').AsInteger := AJSon_Certificado.Objects[I].Integers['idEmpresa'];
        FQuery.ParamByName('tipo').AsString := AJSon_Certificado.Objects[I].Strings['tipo'];
        FQuery.ParamByName('validade').AsDateTime := StrISOToDateTime(AJSon_Certificado.Objects[I].Strings['validade']);
        FQuery.ParamByName('caminho_arquivo').AsString := AJSon_Certificado.Objects[I].Strings['caminhoArquivo'];
        FQuery.ParamByName('senha').AsString := AJSon_Certificado.Objects[I].Strings['senha'];
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

function TEmpresa.Empresa_Get(const aId_Empresa: Integer;
  const aRazao_Social: String; const aFantasia: String; const aCNPJ: String
  ): TZQuery;
begin

end;

end.
