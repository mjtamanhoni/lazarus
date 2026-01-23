unit Emissor_Session;

interface

uses
  SysUtils, Classes,
  Prism.SessionBase;

type

  { TEmissorSession }

  TEmissorSession = class(TPrismSessionBase)
  private
    fCNPJ_Empresa: String;
    fID_Usuario: Integer;
    fLogin_Usuario: String;
    fNome_Usuario: String;
    fToken_Server: String;
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

