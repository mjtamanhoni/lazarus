unit uBase.DataSets;

{$mode Delphi}

interface

uses
  Classes, SysUtils, DB, BufDataset,DBGrids, uBase.Functions;

procedure ConfigColGridAut(const Grid: TDBGrid; const Dataset: TDataSet);
procedure Conf_Coluna_DBGrid(
  const aDBGrid: TDBGrid;
  const aCaption: String;
  const aWidth: Integer;
  const aPos :INteger;
  const aAlignment:Integer=0);

type

  { TEmpresa }

  TEmpresa = class
  private
  public
    class procedure Criar_DataSet_Empresa(const aDataSet :TBufDataset);
    class procedure Criar_DataSet_Endereco(const aDataSet :TBufDataset);
    class procedure Criar_DataSet_CBanco(const aDataSet :TBufDataset);
    class procedure Criar_DataSet_Certificado(const aDataSet :TBufDataset);
  end;

  { TUsuario }

  TUsuario = class
  private
  public
    class procedure Criar_DataSet_Usuario(const aDataSet :TBufDataset);
    class procedure Criar_DataSet_Permissoes(const aDataSet :TBufDataset);
  end;

implementation

procedure Conf_Coluna_DBGrid(
  const aDBGrid: TDBGrid;
  const aCaption: String;
  const aWidth: Integer;
  const aPos :INteger;
  const aAlignment:Integer=0);
begin
  try
    aDBGrid.Columns[aPos].Title.Caption := aCaption;
    aDBGrid.Columns[aPos].Width := aWidth;
    aDBGrid.Columns[aPos].Title.Alignment := taCenter;

    case aAlignment of
      0:aDBGrid.Columns[aPos].Alignment := taLeftJustify;
      1:aDBGrid.Columns[aPos].Alignment := taCenter;
      2:aDBGrid.Columns[aPos].Alignment := taRightJustify;
    end;

  except
    On E:Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure ConfigColGridAut(const Grid: TDBGrid; const Dataset: TDataSet);
var
  Col: TColumn;
  Field: TField;
  Largura: Integer;
begin
  if not Assigned(Grid) or not Assigned(Dataset) then
    Exit;

  // Limpa colunas existentes (opcional - comente se quiser manter algumas personalizadas)
  //Grid.Columns.Clear;

  try
    Dataset.DisableControls;
    try
      for Field in Dataset.Fields do
      begin
        Col := Grid.Columns.Add;
        Col.FieldName := Field.FieldName;
        Col.Title.Caption := Field.DisplayLabel;

        // Se DisplayLabel estiver vazio, usa o FieldName
        if Trim(Col.Title.Caption) = '' then
          Col.Title.Caption := Field.FieldName;

        // Define largura automática baseada no tipo e tamanho do campo
        case Field.DataType of
          ftString, ftMemo, ftWideString:
            begin
              Largura := Field.Size * 8;  // Aproximadamente 8 pixels por caractere
              if Largura < 80 then Largura := 80;
              if Largura > 350 then Largura := 350; // Limite máximo
            end;

          ftInteger, ftSmallint, ftLargeint:
            Largura := 65;  // Largura fixa para números inteiros

          ftFloat, ftCurrency:
            Largura := 100; // Com casas decimais

          ftDate, ftTime, ftDateTime:
            Largura := 120; // Formato de data/hora

          ftBoolean:
            Largura := 60;  // Checkbox ou texto "Sim/Não"

          else
            Largura := 100; // Default para tipos desconhecidos
        end;

        Col.Width := Largura;

        // Alinhamento automático
        case Field.DataType of
          ftInteger, ftSmallint, ftLargeint, ftFloat, ftCurrency:
            Col.Alignment := taRightJustify;
          ftBoolean:
            Col.Alignment := taCenter;
          else
            Col.Alignment := taLeftJustify;
        end;
      end;

      // Opcional: Ajusta colunas para preencher o grid
      //Grid.AutoFillColumns := True;
      //Grid.Options := Grid.Options + [dgAutoSizeColumns];

    finally
      Dataset.EnableControls;
    end;

  except
    on E:Exception do
      raise Exception.Create('Configurando DbGrid ['+Grid.Name+']: ' + E.Message);
  end;

end;

{ TEmpresa }

class procedure TEmpresa.Criar_DataSet_Empresa(const aDataSet: TBufDataset);
begin
  try
    if aDataSet.Active then
      aDataSet.Close;

    aDataSet.FieldDefs.Clear;
    aDataSet.FieldDefs.Add('idEmpresa',ftInteger,0,True);
    aDataSet.FieldDefs.Add('razaoSocial',ftString,255,True);
    aDataSet.FieldDefs.Add('nomeFantasia',ftString,255);
    aDataSet.FieldDefs.Add('cnpj',ftString,14,True);
    aDataSet.FieldDefs.Add('inscricaoEstadual',ftString,20);
    aDataSet.FieldDefs.Add('inscricaoMunicipal',ftString,20);
    aDataSet.FieldDefs.Add('regimeTributario',ftString,50);
    aDataSet.FieldDefs.Add('crt',ftString,1);
    aDataSet.FieldDefs.Add('email',ftString,255);
    aDataSet.FieldDefs.Add('telefone',ftString,20);
    aDataSet.FieldDefs.Add('site',ftString,255);
    aDataSet.FieldDefs.Add('dataCadastro',ftDateTime,0);
    aDataSet.FieldDefs.Add('ativo',ftInteger,0,True);
    aDataSet.FieldDefs.Add('celular',ftString,20);
    aDataSet.FieldDefs.Add('crtDesc',ftString,255);

    aDataSet.CreateDataset;
    aDataSet.Open;

    //Definindo nome das colunas....
    aDataSet.FieldByName('idEmpresa').DisplayLabel := 'Id';
    aDataSet.FieldByName('razaoSocial').DisplayLabel := 'Razão Social';
    aDataSet.FieldByName('nomeFantasia').DisplayLabel := 'Nome Fantasia';
    aDataSet.FieldByName('cnpj').DisplayLabel := 'Documento';
    aDataSet.FieldByName('inscricaoEstadual').DisplayLabel := 'Inscrição Estadual';
    aDataSet.FieldByName('inscricaoMunicipal').DisplayLabel := 'Inscrição Municipal';
    aDataSet.FieldByName('regimeTributario').DisplayLabel := 'Regime Tributário';
    aDataSet.FieldByName('crt').DisplayLabel := 'C.R.T.';
    aDataSet.FieldByName('email').DisplayLabel := 'E-Mail';
    aDataSet.FieldByName('telefone').DisplayLabel := 'Telefone';
    aDataSet.FieldByName('site').DisplayLabel := 'Site';
    aDataSet.FieldByName('dataCadastro').DisplayLabel := 'Cadastro';
    aDataSet.FieldByName('ativo').DisplayLabel := 'Ativo';
    aDataSet.FieldByName('celular').DisplayLabel := 'Celular';
    aDataSet.FieldByName('crtDesc').DisplayLabel := 'Descrição CRT';
  except
    on E:Exception do
      raise Exception.Create('Cria Dataset da Empresa: ' + E.Message);
  end;
end;

class procedure TEmpresa.Criar_DataSet_Endereco(const aDataSet: TBufDataset);
begin
  try
    if aDataSet.Active then
      aDataSet.Close;

    aDataSet.FieldDefs.Clear;
    aDataSet.FieldDefs.Add('idEndereco',ftInteger,0,True);
    aDataSet.FieldDefs.Add('idEmpresa',ftInteger,0,True);
    aDataSet.FieldDefs.Add('logradouro',ftString,255,True);
    aDataSet.FieldDefs.Add('numero',ftString,20);
    aDataSet.FieldDefs.Add('complemento',ftString,100);
    aDataSet.FieldDefs.Add('bairro',ftString,100);
    aDataSet.FieldDefs.Add('municipio',ftString,100,True);
    aDataSet.FieldDefs.Add('codigoMunicipioIbge',ftString,7);
    aDataSet.FieldDefs.Add('uf',ftString,2,True);
    aDataSet.FieldDefs.Add('cep',ftString,8,True);
    aDataSet.FieldDefs.Add('pais',ftString,100);
    aDataSet.FieldDefs.Add('codigoPaisIbge',ftString,4);
    aDataSet.FieldDefs.Add('tipoEndereco',ftInteger,0);
    aDataSet.FieldDefs.Add('tipoEnderecoDesc',ftString,255);

    aDataSet.CreateDataset;
    aDataSet.Open;

    //Definindo nome das colunas....
    aDataSet.FieldByName('idEndereco').DisplayLabel := 'Id';
    aDataSet.FieldByName('idEmpresa').DisplayLabel := 'Id. Empresa';
    aDataSet.FieldByName('logradouro').DisplayLabel := 'Logradouro';
    aDataSet.FieldByName('numero').DisplayLabel := 'Número';
    aDataSet.FieldByName('complemento').DisplayLabel := 'Complemento';
    aDataSet.FieldByName('bairro').DisplayLabel := 'Bairro';
    aDataSet.FieldByName('municipio').DisplayLabel := 'Município';
    aDataSet.FieldByName('codigoMunicipioIbge').DisplayLabel := 'IBGE';
    aDataSet.FieldByName('uf').DisplayLabel := 'UF';
    aDataSet.FieldByName('cep').DisplayLabel := 'CEP';
    aDataSet.FieldByName('pais').DisplayLabel := 'Pais';
    aDataSet.FieldByName('codigoPaisIbge').DisplayLabel := 'IBGE';
    aDataSet.FieldByName('tipoEndereco').DisplayLabel := 'Tp. Endereço';
    aDataSet.FieldByName('tipoEnderecoDesc').DisplayLabel := 'Tp. Endereço';

  except
    on E:Exception do
      raise Exception.Create('Cria Dataset do Endereço da Empresa: ' + E.Message);
  end;
end;

class procedure TEmpresa.Criar_DataSet_CBanco(const aDataSet: TBufDataset);
begin
  try
    if aDataSet.Active then
      aDataSet.Close;

    aDataSet.FieldDefs.Clear;
    aDataSet.FieldDefs.Add('idBanco',ftInteger,0,True);
    aDataSet.FieldDefs.Add('idEmpresa',ftInteger,0,True);
    aDataSet.FieldDefs.Add('banco',ftString,100);
    aDataSet.FieldDefs.Add('agencia',ftString,20);
    aDataSet.FieldDefs.Add('conta',ftString,20);
    aDataSet.FieldDefs.Add('tipoConta',ftInteger,0);
    aDataSet.FieldDefs.Add('tipoContaDesc',ftString,255);

    aDataSet.CreateDataset;
    aDataSet.Open;

    //Definindo nome das colunas....
    aDataSet.FieldByName('idBanco').DisplayLabel := 'Id';
    aDataSet.FieldByName('idEmpresa').DisplayLabel := 'Id. Empresa';
    aDataSet.FieldByName('banco').DisplayLabel := 'Banco';
    aDataSet.FieldByName('agencia').DisplayLabel := 'Agência';
    aDataSet.FieldByName('conta').DisplayLabel := 'Conta';
    aDataSet.FieldByName('tipoConta').DisplayLabel := 'Tipo';
    aDataSet.FieldByName('tipoContaDesc').DisplayLabel := 'Tipo';

  except
    on E:Exception do
      raise Exception.Create('Cria Dataset da Conta Bancária da Empresa: ' + E.Message);
  end;
end;

class procedure TEmpresa.Criar_DataSet_Certificado(const aDataSet: TBufDataset);
begin
  try
    if aDataSet.Active then
      aDataSet.Close;

    aDataSet.FieldDefs.Clear;
    aDataSet.FieldDefs.Add('idCertificado',ftInteger,0,True);
    aDataSet.FieldDefs.Add('idEmpresa',ftInteger,0,True);
    aDataSet.FieldDefs.Add('tipo',ftString,50);
    aDataSet.FieldDefs.Add('validade',ftDate,0);
    aDataSet.FieldDefs.Add('caminhoArquivo',ftString,255);
    aDataSet.FieldDefs.Add('senha',ftString,255);
    aDataSet.FieldDefs.Add('tipoDesc',ftString,255);

    aDataSet.CreateDataset;
    aDataSet.Open;

    //Definindo nome das colunas....
    aDataSet.FieldByName('idCertificado').DisplayLabel := 'Id';
    aDataSet.FieldByName('idEmpresa').DisplayLabel := 'Id. Empresa';
    aDataSet.FieldByName('tipo').DisplayLabel := 'Tipo';
    aDataSet.FieldByName('validade').DisplayLabel := 'Validade';
    aDataSet.FieldByName('caminhoArquivo').DisplayLabel := 'Caminho';
    aDataSet.FieldByName('senha').DisplayLabel := 'Senha';
    aDataSet.FieldByName('tipoDesc').DisplayLabel := 'Tipo';

  except
    on E:Exception do
      raise Exception.Create('Cria Dataset do Certificado digital da Empresa: ' + E.Message);
  end;
end;

{ TUsuario }

class procedure TUsuario.Criar_DataSet_Usuario(const aDataSet: TBufDataset);
begin
  try
    if aDataSet.Active then
      aDataSet.Close;

    aDataSet.FieldDefs.Clear;
    aDataSet.FieldDefs.Add('idUsuario',ftInteger,0,True);
    aDataSet.FieldDefs.Add('login',ftString,50,True);
    aDataSet.FieldDefs.Add('senha',ftString,255,True);
    aDataSet.FieldDefs.Add('nome',ftString,255,True);
    aDataSet.FieldDefs.Add('email',ftString,100);
    aDataSet.FieldDefs.Add('ativo',ftInteger,0,True);
    aDataSet.FieldDefs.Add('dataCadastro',ftDateTime,0);
    aDataSet.FieldDefs.Add('ultimoAcesso',ftDateTime,0);
    aDataSet.FieldDefs.Add('idPerfil',ftInteger,0);
    aDataSet.FieldDefs.Add('idEmpresa',ftInteger,0,True);
    aDataSet.FieldDefs.Add('cnpj',ftString,14);
    aDataSet.FieldDefs.Add('razaoSocial',ftString,255);
    aDataSet.FieldDefs.Add('nomeFantasia',ftString,255);
    aDataSet.FieldDefs.Add('nomePerfil',ftString,50);
    aDataSet.FieldDefs.Add('descricaoPerfil',ftString,500);

    aDataSet.CreateDataset;
    aDataSet.Open;

    //Definindo nome das colunas....
    aDataSet.FieldByName('idUsuario').DisplayLabel := 'Id';
    aDataSet.FieldByName('login').DisplayLabel := 'Login';
    aDataSet.FieldByName('senha').DisplayLabel := 'Senha';
    aDataSet.FieldByName('nome').DisplayLabel := 'Nome';
    aDataSet.FieldByName('email').DisplayLabel := 'E-Mail';
    aDataSet.FieldByName('ativo').DisplayLabel := 'Ativo';
    aDataSet.FieldByName('dataCadastro').DisplayLabel := 'Cadastro';
    aDataSet.FieldByName('ultimoAcesso').DisplayLabel := 'Último Acesso';
    aDataSet.FieldByName('idPerfil').DisplayLabel := 'Id. Perfil';
    aDataSet.FieldByName('idEmpresa').DisplayLabel := 'Id. Empresa';
    aDataSet.FieldByName('cnpj').DisplayLabel := 'CNPJ/CPF Empresa';
    aDataSet.FieldByName('razaoSocial').DisplayLabel := 'Razão Social';
    aDataSet.FieldByName('nomeFantasia').DisplayLabel := 'Nome Fantasia';
    aDataSet.FieldByName('nomePerfil').DisplayLabel := 'Perfil';
    aDataSet.FieldByName('descricaoPerfil').DisplayLabel := 'Descrição do Perfil';
  except
    on E:Exception do
      raise Exception.Create('Cria Dataset do Usuário: ' + E.Message);
  end;
end;

class procedure TUsuario.Criar_DataSet_Permissoes(const aDataSet: TBufDataset);
begin
  try
    if aDataSet.Active then
      aDataSet.Close;

    aDataSet.FieldDefs.Clear;
    aDataSet.FieldDefs.Add('acao',ftString,100,True);
    aDataSet.FieldDefs.Add('visualizar',ftInteger,0,True);
    aDataSet.FieldDefs.Add('incluir',ftInteger,0,True);
    aDataSet.FieldDefs.Add('alterar',ftInteger,0,True);
    aDataSet.FieldDefs.Add('excluir',ftInteger,0,True);
    aDataSet.FieldDefs.Add('imprimir',ftInteger,0,True);
    {
    aDataSet.FieldDefs.Add('exportar',ftInteger,0,True);
    aDataSet.FieldDefs.Add('importar',ftInteger,0,True);
    aDataSet.FieldDefs.Add('aprovar_rejeitar',ftInteger,0,True);
    aDataSet.FieldDefs.Add('anexar_upload',ftInteger,0,True);
    aDataSet.FieldDefs.Add('pesquisar_filtrar',ftInteger,0,True);
    aDataSet.FieldDefs.Add('notificar_enviar',ftInteger,0,True);
    aDataSet.FieldDefs.Add('auditar_historico',ftInteger,0,True);
    aDataSet.FieldDefs.Add('executar_processos',ftInteger,0,True);
    aDataSet.FieldDefs.Add('alterar_status',ftInteger,0,True);
    aDataSet.FieldDefs.Add('alterar_situacao_negocio',ftInteger,0,True);
    }
    aDataSet.CreateDataset;
    aDataSet.Open;

    //Definindo nome das colunas....
    aDataSet.FieldByName('acao').DisplayLabel := 'Ação';
    aDataSet.FieldByName('visualizar').DisplayLabel := 'Visualizar';
    aDataSet.FieldByName('incluir').DisplayLabel := 'Incluir';
    aDataSet.FieldByName('alterar').DisplayLabel := 'Alterar';
    aDataSet.FieldByName('excluir').DisplayLabel := 'Excluir';
    aDataSet.FieldByName('imprimir').DisplayLabel := 'Imprimir';
    {
    aDataSet.FieldByName('exportar').DisplayLabel := 'Exportar';
    aDataSet.FieldByName('importar').DisplayLabel := 'Importar';
    aDataSet.FieldByName('aprovar_rejeitar').DisplayLabel := 'Aprovar/Rejeitar';
    aDataSet.FieldByName('anexar_upload').DisplayLabel := 'Anexar/Upload';
    aDataSet.FieldByName('pesquisar_filtrar').DisplayLabel := 'Pesquisar/Filtrar';
    aDataSet.FieldByName('notificar_enviar').DisplayLabel := 'Notificar/Enviar';
    aDataSet.FieldByName('auditar_historico').DisplayLabel := 'Auditar Histórico';
    aDataSet.FieldByName('executar_processos').DisplayLabel := 'Executar Processos';
    aDataSet.FieldByName('alterar_status').DisplayLabel := 'Alterar Status';
    aDataSet.FieldByName('alterar_situacao_negocio').DisplayLabel := 'Alterar Situação de Negócio';
    }
  except
    on E:Exception do
      raise Exception.Create('Cria Dataset das permissões do Usuário: ' + E.Message);
  end;
end;

end.

