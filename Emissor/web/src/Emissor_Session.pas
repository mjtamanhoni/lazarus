unit Emissor_Session;

interface

uses
  SysUtils, Classes,
  Prism.SessionBase;

type

  { TEmissorSession }

  TEmissorSession = class(TPrismSessionBase)
  private
    Fbairro: String;
    Fcep: String;
    fCNPJ_Empresa: String;
    Fcodigo_municipio_ibge: String;
    Fcodigo_pais_ibge: String;
    Fcomplemento: String;
    Fid_empresa: Integer;
    Fid_endereco: Integer;
    fID_Usuario: Integer;
    fLogin_Usuario: String;
    Flogradouro: String;
    Fmunicipio: String;
    fNome_Usuario: String;
    Fnumero: String;
    Fpais: String;
    Ftipo_endereco: Integer;
    fToken_Server: String;
    Fuf: String;
    fUsuario_Ativo: Integer;

  public
   constructor Create(APrismSession: TPrismSession); override;  //OnNewSession
   destructor Destroy; override; //OnCloseSession

   //Propriedades utilizadas no sistema...
   {$Region 'Acesso ao Sistema'}
     property CNPJ_Empresa :String read fCNPJ_Empresa write fCNPJ_Empresa;
     property ID_Usuario :Integer read fID_Usuario write fID_Usuario;
     property Nome_Usuario :String read fNome_Usuario write fNome_Usuario;
     property Login_Usuario :String read fLogin_Usuario write fLogin_Usuario;
     property Usuario_Ativo :Integer read fUsuario_Ativo write fUsuario_Ativo;
     property Token_Server :String read fToken_Server write fToken_Server;
   {$EndRegion 'Acesso ao Sistema'}

   {$Region 'Empresa - Endereço'}
     property id_endereco :Integer read Fid_endereco write Fid_endereco;
     property logradouro :String read Flogradouro write Flogradouro;
     property id_empresa :Integer read Fid_empresa write Fid_empresa;
     property numero :String read Fnumero write Fnumero;
     property complemento :String read Fcomplemento write Fcomplemento;
     property bairro :String read Fbairro write Fbairro;
     property municipio :String read Fmunicipio write Fmunicipio;
     property codigo_municipio_ibge :String read Fcodigo_municipio_ibge write Fcodigo_municipio_ibge;
     property uf :String read Fuf write Fuf;
     property cep :String read Fcep write Fcep;
     property pais :String read Fpais write Fpais;
     property codigo_pais_ibge :String read Fcodigo_pais_ibge write Fcodigo_pais_ibge;
     property tipo_endereco :Integer read Ftipo_endereco write Ftipo_endereco;
   {$EndRegion 'Empresa - Endereço'}

  end;


implementation

Uses
  D2Bridge.Instance,
  EmissorWebApp;

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF} 

constructor TEmissorSession.Create(APrismSession: TPrismSession); //OnNewSession
begin
 inherited;

 //Your code

end;

destructor TEmissorSession.Destroy; //OnCloseSession
begin
 //Close ALL DataBase connection
 //Ex: Dm.DBConnection.Close;

 inherited;
end;

end.

