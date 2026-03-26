unit uDm.ACBR;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ACBrValidador,
  ACBrCEP, ACBrConsultaCNPJ, IniFiles, uBase.Functions;

type
  TCNPJ = Record
    EmpresaTipo :String;
    RazaoSocial :String;
    Porte :String;
    Abertura :TDate;
    Fantasia :String;
    Endereco :String;
    Numero :String;
    Complemento :String;
    Bairro :String;
    Cidade :String;
    UF :String;
    CEP :String;
    Situacao :String;
    CNAE1 :String;
    EndEletronico :String;
    Telefone :String;
  end;

  { TDM_ACBr }
  TDM_ACBr = class(TDataModule)
    ACBrCEP: TACBrCEP;
    ACBrConsultaCNPJ: TACBrConsultaCNPJ;
    ACBrValidador: TACBrValidador;
    procedure ACBrCEPBuscaEfetuada(Sender: TObject);
  private
    { Private declarations }
    FIniFile :TIniFile;
  published
    class procedure CreateInstance;
    procedure DestroyInstance;
  public
    { Public declarations }
    procedure Buscar_CEP(const ACep:String);
    function Busca_Dados_CNPJ(const aCJPJ:String) :TCNPJ;
  end;

function DM_ACBr: TDM_ACBr;

implementation

uses
  D2Bridge.Instance, EmissorWebApp;

{$R *.lfm}

procedure TDM_ACBr.ACBrCEPBuscaEfetuada(Sender: TObject);
var
  I :Integer;
begin
  try

    with Emissor.EmpEnd_Fields do
    begin
      if ACBrCEP.Enderecos.Count >= 1 then
      begin
         //For I := 0 to ACBrCEP1.Enderecos.Count-1 do
         //begin
           I := 0;
           cep := ACBrCEP.Enderecos[I].CEP;
           logradouro := ACBrCEP.Enderecos[I].Logradouro;
           complemento := ACBrCEP.Enderecos[I].Complemento;
           bairro := ACBrCEP.Enderecos[I].Bairro;
           municipio := ACBrCEP.Enderecos[I].Municipio;
           codigo_municipio_ibge := ACBrCEP.Enderecos[I].IBGE_Municipio;
           uf := ACBrCEP.Enderecos[I].UF;
         //end;
       end;
    end;
  except
    On E:Exception do
      raise Exception.Create(E.Message);
  end;
end;

class procedure TDM_ACBr.CreateInstance;
begin
  D2BridgeInstance.CreateInstance(self);
end;

function DM_ACBr: TDM_ACBr;
begin
  result := (D2BridgeInstance.GetInstance(TDM_ACBr) as TDM_ACBr);
end;

procedure TDM_ACBr.DestroyInstance;
begin
  D2BridgeInstance.DestroyInstance(self);
end;

procedure TDM_ACBr.Buscar_CEP(const ACep: String);
begin
  try
    try
      FIniFile := TIniFile.Create(ConfigFile);
      ACBrCEP.WebService := TACBrCEPWebService(FIniFile.ReadInteger('ACBR.CEP','PROVEDOR',0)) ;
      ACBrCEP.ChaveAcesso := FIniFile.ReadString('ACBR.CEP','CHAVE','');
      ACBrCEP.Usuario     := FIniFile.ReadString('ACBR.CEP','USUARIO','');
      ACBrCEP.Senha       := FIniFile.ReadString('ACBR.CEP','SENHA','');
      ACBrCEP.ProxyHost := FIniFile.ReadString('ACBR.CEP','HOST','');
      ACBrCEP.ProxyPort := FIniFile.ReadString('ACBR.CEP','PORTA','');
      {
      ACBrCEP.ProxyUser := FIniFile.ReadString('ACBR.CEP','USUARIO','');
      ACBrCEP.ProxyPass := FIniFile.ReadString('ACBR.CEP','SENHA','');
      }
      ACBrCEP.BuscarPorCEP(ACep);
    except
      On E:Exception do
        raise Exception.Create('Buscar CEP: ' + sLineBreak + E.Message);
    end;
  finally
    if Assigned(FIniFile) then
      FreeAndNil(FIniFile);
  end;
end;

function TDM_ACBr.Busca_Dados_CNPJ(const aCJPJ: String): TCNPJ;
begin
  try
    try
      FIniFile := TIniFile.Create(ConfigFile);

      with Result do
      begin
        EmpresaTipo := '';
        RazaoSocial := '';
        Porte := '';
        Abertura := 0;
        Fantasia := '';
        Endereco := '';
        Numero := '';
        Complemento := '';
        Bairro := '';
        Complemento := '';
        Cidade := '';
        UF := '';
        CEP := '';
        Situacao := '';
        CNAE1 := '';
        EndEletronico := '';
        Telefone := '';
      end;

      ACBrConsultaCNPJ.Provedor := TACBrCNPJProvedorWS(FIniFile.ReadInteger('ACBR.CNPJ','PROVEDOR',0));
      ACBrConsultaCNPJ.ProxyHost:= FIniFile.ReadString('ACBR.CNPJ','HOST','');
      ACBrConsultaCNPJ.ProxyPort:= FIniFile.ReadString('ACBR.CNPJ','PORTA','');
      ACBrConsultaCNPJ.ProxyUser:= FIniFile.ReadString('ACBR.CNPJ','USUARIO','');
      ACBrConsultaCNPJ.ProxyPass:= FIniFile.ReadString('ACBR.CNPJ','SENHA','');
      if ACBrConsultaCNPJ.Provedor = cwsNenhum then
         raise EACBrConsultaCNPJException.Create('Nenhum provedor Selecionado!');

      if ACBrConsultaCNPJ.Consulta(aCJPJ) then
      begin
        with Result do
        begin
          EmpresaTipo := ACBrConsultaCNPJ.EmpresaTipo;
          RazaoSocial := ACBrConsultaCNPJ.RazaoSocial;
          Porte := ACBrConsultaCNPJ.Porte;
          Abertura := ACBrConsultaCNPJ.Abertura;
          Fantasia := ACBrConsultaCNPJ.Fantasia;
          Endereco := ACBrConsultaCNPJ.Endereco;
          Numero := ACBrConsultaCNPJ.Numero;
          Complemento := ACBrConsultaCNPJ.Complemento;
          Bairro := ACBrConsultaCNPJ.Bairro;
          Cidade := ACBrConsultaCNPJ.Cidade;
          UF := ACBrConsultaCNPJ.UF;
          CEP := ACBrConsultaCNPJ.CEP;
          Situacao := ACBrConsultaCNPJ.Situacao;
          CNAE1 := ACBrConsultaCNPJ.CNAE1;
          EndEletronico := ACBrConsultaCNPJ.EndEletronico;
          Telefone := ACBrConsultaCNPJ.Telefone;
        end;
      end;
    except
      On E:Exception do
        raise Exception.Create('Buscando dados do CNPJ.' + sLineBreak + E.Message);
    end;

  finally
    if Assigned(FIniFile) then
      FreeAndNil(FIniFile);
  end;
end;

end.
