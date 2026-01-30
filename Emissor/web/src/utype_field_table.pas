unit uType_Field_Table;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type

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

implementation

end.

