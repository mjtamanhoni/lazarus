unit uBase.DataSets;

{$mode Delphi}

interface

uses
  Classes, SysUtils, DB, BufDataset,DBGrids;

procedure ConfigColGridAut(const Grid: TDBGrid; const Dataset: TDataSet);

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

implementation

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

end.

