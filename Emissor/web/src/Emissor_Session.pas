unit Emissor_Session;

interface

uses
  SysUtils, Classes,
  Prism.SessionBase,
  uType_Field_Table;

type

  { TEmissorSession }

  TEmissorSession = class(TPrismSessionBase)
  private
    fCNPJ_Empresa: String;
    FEmpCB_Fields: TEmpCB_Fields;
    FEmpCD_Fields: TEmpCD_Fields;
    FEmpEnd_Fields: TEmpEnd_Fields;
    FEmpresa_Fields: TEmpresa_Fields;
    fID_Usuario: Integer;
    fLogin_Usuario: String;
    fNome_Usuario: String;
    fToken_Server: String;
    fUsuario_Ativo: Integer;
    FUsuario_Fields: TUsuario_Fields;

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
     property Usuario_Fields :TUsuario_Fields read FUsuario_Fields write FUsuario_Fields;
   {$EndRegion 'Acesso ao Sistema'}

   {$Region 'Empresa - Endereço'}
     property Empresa_Fields :TEmpresa_Fields read FEmpresa_Fields write FEmpresa_Fields;
     property EmpEnd_Fields :TEmpEnd_Fields read FEmpEnd_Fields write FEmpEnd_Fields;
     property EmpCB_Fields :TEmpCB_Fields read FEmpCB_Fields write FEmpCB_Fields;
     property EmpCD_Fields :TEmpCD_Fields read FEmpCD_Fields write FEmpCD_Fields;
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

