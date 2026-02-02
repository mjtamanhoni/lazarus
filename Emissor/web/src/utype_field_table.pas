unit uType_Field_Table;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type


  { TEmpresa_Fields }

  TEmpresa_Fields = class
  private
    class var Fativo: Integer;
    class var Fcelular: String;
    class var Fcnpj: String;
    class var Fcrt: String;
    class var Fdata_cadastro: TDateTime;
    class var Femail: String;
    class var Fid_empresa: Integer;
    class var Finscricao_estadual: String;
    class var Finscricao_municipal: String;
    class var Fnome_fantasia: String;
    class var Frazao_social: String;
    class var Fregime_tributario: String;
    class var Fsite: String;
    class var Ftelefone: String;
  public
    class property id_empresa: Integer read Fid_empresa write Fid_empresa;
    class property razao_social: String read Frazao_social write Frazao_social;
    class property nome_fantasia: String read Fnome_fantasia write Fnome_fantasia;
    class property cnpj: String read Fcnpj write Fcnpj;
    class property inscricao_estadual: String read Finscricao_estadual write Finscricao_estadual;
    class property inscricao_municipal: String read Finscricao_municipal write Finscricao_municipal;
    class property regime_tributario: String read Fregime_tributario write Fregime_tributario;
    class property crt: String read Fcrt write Fcrt;
    class property email: String read Femail write Femail;
    class property telefone: String read Ftelefone write Ftelefone;
    class property site: String read Fsite write Fsite;
    class property data_cadastro: TDateTime read Fdata_cadastro write Fdata_cadastro;
    class property ativo: Integer read Fativo write Fativo;
    class property celular: String read Fcelular write Fcelular;
  end;

  { TEmpEnd_Fields }

  TEmpEnd_Fields = class
  private
    class var Fbairro: String;
    class var Fcep: String;
    class var Fcodigo_municipio_ibge: String;
    class var Fcodigo_pais_ibge: String;
    class var Fcomplemento: String;
    class var Fid_empresa: Integer;
    class var Fid_endereco: Integer;
    class var Flogradouro: String;
    class var Fmunicipio: String;
    class var Fnumero: String;
    class var Fpais: String;
    class var Ftipo_endereco: Integer;
    class var Fuf: String;
  public
    class property id_endereco :Integer read Fid_endereco write Fid_endereco;
    class property id_empresa :Integer read Fid_empresa write Fid_empresa;
    class property logradouro :String read Flogradouro write Flogradouro;
    class property numero :String read Fnumero write Fnumero;
    class property complemento :String read Fcomplemento write Fcomplemento;
    class property bairro :String read Fbairro write Fbairro;
    class property municipio :String read Fmunicipio write Fmunicipio;
    class property codigo_municipio_ibge :String read Fcodigo_municipio_ibge write Fcodigo_municipio_ibge;
    class property uf :String read Fuf write Fuf;
    class property cep :String read Fcep write Fcep;
    class property pais :String read Fpais write Fpais;
    class property codigo_pais_ibge :String read Fcodigo_pais_ibge write Fcodigo_pais_ibge;
    class property tipo_endereco :Integer read Ftipo_endereco write Ftipo_endereco;
  end;

  { TEmpCB_Fields }

  TEmpCB_Fields = class
  private
    class var Fagencia: String;
    class var Fbanco: String;
    class var Fconta: String;
    class var Fid_banco: Integer;
    class var Fid_empresa: Integer;
    class var Ftipo_conta: Integer;
  public
    class property id_banco :Integer read Fid_banco write Fid_banco;
    class property id_empresa :Integer read Fid_empresa write Fid_empresa;
    class property banco :String read Fbanco write Fbanco;
    class property agencia :String read Fagencia write Fagencia;
    class property conta :String read Fconta write Fconta;
    class property tipo_conta :Integer read Ftipo_conta write Ftipo_conta;
  end;

  { TEmpCD_Fields }

  TEmpCD_Fields = class
  private
    class var Fcaminho_arquivo: String;
    class var Fid_certificado: Integer;
    class var Fid_empresa: Integer;
    class var Fsenha: String;
    class var Ftipo: String;
    class var Fvalidade: TDate;
  public
    class property id_certificado :Integer read Fid_certificado write Fid_certificado;
    class property id_empresa :Integer read Fid_empresa write Fid_empresa;
    class property tipo :String read Ftipo write Ftipo;
    class property validade :TDate read Fvalidade write Fvalidade;
    class property caminho_arquivo :String read Fcaminho_arquivo write Fcaminho_arquivo;
    class property senha :String read Fsenha write FSenha;
  end;

implementation

end.

