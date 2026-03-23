unit uCad.Empresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, memds, DB, BufDataset, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ComboEx, DBGrids, EditBtn, DateTimePicker,
  D2Bridge.Forms, ACBrValidador, IniFiles, fpjson, DataSet.Serialize,
  RESTRequest4D, jsonparser, uCad.Empresa.Endereco, ucad.empresa.DadosBancarios,
  uDM.ACBr, uBase.Functions, uBase.DataSets, Forms, ZDataset,
  ZAbstractRODataset, ubase.functions.objetos, udm, LCLType;

type

  { TfrmCadEmpresa }

  TfrmCadEmpresa = class(TD2BridgeForm)
    btFechar: TButton;
    btConfirmar: TButton;
    btEnd_Add: TButton;
    btCB_Add: TButton;
    cbativo: TComboBox;
    cbtipo: TComboBox;
    dsEndereco: TDataSource;
    dsConta: TDataSource;
    DBGrid_End: TDBGrid;
    edcelular: TEdit;
    edvalidade: TDateTimePicker;
    edsenha: TEdit;
    DBGrid_DB: TDBGrid;
    edcaminho_arquivo: TEdit;
    edcrt: TEdit;
    edemail: TEdit;
    edid_certificado: TEdit;
    edsite: TEdit;
    edtelefone: TEdit;
    cbregime_tributario: TComboBox;
    edinscricao_estadual: TEdit;
    edinscricao_municipal: TEdit;
    edid_empresa: TEdit;
    edcnpj: TEdit;
    edrazao_social: TEdit;
    ednome_fantasia: TEdit;
    lbsenha: TLabel;
    lbcelular: TLabel;
    lbvalidade: TLabel;
    lbcaminho_arquivo: TLabel;
    lbemail: TLabel;
    lbativo: TLabel;
    lbid_certificado: TLabel;
    lbsite: TLabel;
    lbtelefone: TLabel;
    lbregime_tributario: TLabel;
    lbinscricao_estadual: TLabel;
    lbid_empresa: TLabel;
    lbcnpj: TLabel;
    lbinscricao_municipal: TLabel;
    lbrazao_social: TLabel;
    lbnome_fantasia: TLabel;
    lbcrt: TLabel;
    lbtipo: TLabel;
    pnEnd_Footer: TPanel;
    pnCB_Footer: TPanel;
    pnsenha: TPanel;
    pnCDRow3: TPanel;
    pncelular: TPanel;
    pnvalidade: TPanel;
    pncaminho_arquivo: TPanel;
    pnCDRow1: TPanel;
    pnCDRow2: TPanel;
    pnEndRow1: TPanel;
    pnemail: TPanel;
    pnativo: TPanel;
    pnid_certificado: TPanel;
    pnsite: TPanel;
    pnRow006: TPanel;
    pntelefone: TPanel;
    pnregime_tributario: TPanel;
    pninscricao_estadual: TPanel;
    pnid_empresa: TPanel;
    pncnpj: TPanel;
    pninscricao_municipal: TPanel;
    pnrazao_social: TPanel;
    pnnome_fantasia: TPanel;
    pncrt: TPanel;
    pnRow001: TPanel;
    pcPrincipal: TPageControl;
    pnDetail: TPanel;
    pnFooter: TPanel;
    pnRow002: TPanel;
    pnRow003: TPanel;
    pnRow004: TPanel;
    pnRow005: TPanel;
    pntipo: TPanel;
    tsEmpresa: TTabSheet;
    tsEndereco: TTabSheet;
    tsDadosBancarios: TTabSheet;
    tsCertificadoDigital: TTabSheet;
    ZQContaBancariaagencia: TZRawStringField;
    ZQContaBancariabanco: TZRawStringField;
    ZQContaBancariaconta: TZRawStringField;
    ZQContaBancariaid_banco: TZIntegerField;
    ZQContaBancariaid_empresa: TZIntegerField;
    ZQContaBancariatipo_conta: TZIntegerField;
    ZQContaBancariatipo_conta_desc: TZRawCLobField;
    ZQEndereco: TZQuery;
    ZQContaBancaria: TZQuery;
    ZQEnderecobairro: TZRawStringField;
    ZQEnderecocep: TZRawStringField;
    ZQEnderecocodigo_municipio_ibge: TZRawStringField;
    ZQEnderecocodigo_pais_ibge: TZRawStringField;
    ZQEnderecocomplemento: TZRawStringField;
    ZQEnderecoid_empresa: TZIntegerField;
    ZQEnderecoid_endereco: TZIntegerField;
    ZQEnderecologradouro: TZRawStringField;
    ZQEnderecomunicipio: TZRawStringField;
    ZQEndereconumero: TZRawStringField;
    ZQEnderecopais: TZRawStringField;
    ZQEnderecotipo_endereco: TZIntegerField;
    ZQEnderecotipo_endereco_desc: TZRawCLobField;
    ZQEnderecouf: TZRawStringField;
    ZQ_Endereco1: TZQuery;
    procedure btFecharClick(Sender: TObject);
    procedure btCB_AddClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
    procedure btEnd_AddClick(Sender: TObject);
    procedure cbativoKeyPress(Sender: TObject; var Key: char);
    procedure cbregime_tributarioChange(Sender: TObject);
    procedure cbregime_tributarioKeyPress(Sender: TObject; var Key: char);
    procedure cbtipoKeyPress(Sender: TObject; var Key: char);
    procedure edcaminho_arquivoKeyPress(Sender: TObject; var Key: char);
    procedure edcelularKeyPress(Sender: TObject; var Key: char);
    procedure edcnpjExit(Sender: TObject);
    procedure edcnpjKeyPress(Sender: TObject; var Key: char);
    procedure edcrtKeyPress(Sender: TObject; var Key: char);
    procedure edemailKeyPress(Sender: TObject; var Key: char);
    procedure edid_certificadoKeyPress(Sender: TObject; var Key: char);
    procedure edid_empresaKeyPress(Sender: TObject; var Key: char);
    procedure edinscricao_estadualKeyPress(Sender: TObject; var Key: char);
    procedure edinscricao_municipalKeyPress(Sender: TObject; var Key: char);
    procedure ednome_fantasiaKeyPress(Sender: TObject; var Key: char);
    procedure edrazao_socialKeyPress(Sender: TObject; var Key: char);
    procedure edtelefoneKeyPress(Sender: TObject; var Key: char);
    procedure edvalidadeKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FfrmCad_Empresa_Endereco :TfrmCad_Empresa_Endereco;
    FfrmCad_Empresa_DadosBancarios :TfrmCad_Empresa_DadosBancarios;
    fDM_ACBr :TDM_Acbr;
    fDmPrincipal :TDM;

    FHost :String;
    FIniFile :TIniFile;

    Fid_endereco: Integer;
    Flogradouro: String;

    procedure OnClick_Edit_End;
    procedure OnClick_Delete_End(const aId_Empresa,aId_Endereco: Integer);
    procedure OnClick_Edit_CBanco;
    procedure OnClick_Delete_CBanco(const aId_Empresa,aId_CBando: Integer);


    procedure Clear_Parans(const AClear:Integer);
    procedure Gravar;

  public
    { Public declarations }

    property id_endereco :Integer read Fid_endereco write Fid_endereco;
    property logradouro :String read Flogradouro write Flogradouro;

    procedure Clear_Fields;
    procedure Listar_Endereco(const aIdEmpresa:String);
    procedure Listar_DadosBancarios(const aIdEmpresa:String);
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
    procedure Upload(AFiles: TStrings; Sender: TObject); override;
    procedure CellButtonClick(APrismDBGrid: TPrismDBGrid; APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer); overload; override;
  end;

function frmCadEmpresa: TfrmCadEmpresa;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmCadEmpresa: TfrmCadEmpresa;
begin
  result := (TfrmCadEmpresa.GetInstance as TfrmCadEmpresa);
end;

procedure TfrmCadEmpresa.btFecharClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCadEmpresa.btCB_AddClick(Sender: TObject);
begin
  try
    FfrmCad_Empresa_DadosBancarios.Clear_Fields;
    Emissor.Empresa_Fields.id_empresa := StrToIntDef(edid_empresa.Text,0);
    ShowPopupModal('Popup' + FfrmCad_Empresa_DadosBancarios.Name);
    Listar_DadosBancarios(edid_empresa.Text);
  except
    on E:Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'btCB_AddClick',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmCadEmpresa.btConfirmarClick(Sender: TObject);
begin
  Gravar;
  //Close;
end;

procedure TfrmCadEmpresa.Gravar;
var
  {
  fResp :IResponse;
  fRet :String;
  fRetorno :TJSONObject;

  fEmpresa :TJSONObject;
  fCertificado :TJSONObject;
  }
  fId_Empresa :Integer;
  FQuery :TZQuery;
  fId_Certificado :Integer;
  fDm :TDM;

begin
  try
    try
      fDm := TDM.Create(Self);

      //Inserindo dados da empresa...
      FQuery := fDm.GetQuery;
      fId_Empresa := 0;
      if StrToIntDef(edid_empresa.Text,0) = 0 then
        fId_Empresa := fDm.Sequencial('public.empresa')
      else
        fId_Empresa := StrToIntDef(edid_empresa.Text,0);

      FQuery.Sql.Add('INSERT INTO public.empresa ( ');
      FQuery.Sql.Add('  id_empresa ');
      FQuery.Sql.Add('  ,razao_social ');
      FQuery.Sql.Add('  ,nome_fantasia ');
      FQuery.Sql.Add('  ,cnpj ');
      FQuery.Sql.Add('  ,inscricao_estadual ');
      FQuery.Sql.Add('  ,inscricao_municipal ');
      FQuery.Sql.Add('  ,regime_tributario ');
      FQuery.Sql.Add('  ,crt ');
      FQuery.Sql.Add('  ,email ');
      FQuery.Sql.Add('  ,telefone ');
      FQuery.Sql.Add('  ,site ');
      FQuery.Sql.Add('  ,data_cadastro ');
      FQuery.Sql.Add('  ,ativo ');
      FQuery.Sql.Add('  ,celular ');
      FQuery.Sql.Add(') VALUES ( ');
      FQuery.Sql.Add('  :id_empresa ');
      FQuery.Sql.Add('  ,:razao_social ');
      FQuery.Sql.Add('  ,:nome_fantasia ');
      FQuery.Sql.Add('  ,:cnpj ');
      FQuery.Sql.Add('  ,:inscricao_estadual ');
      FQuery.Sql.Add('  ,:inscricao_municipal ');
      FQuery.Sql.Add('  ,:regime_tributario ');
      FQuery.Sql.Add('  ,:crt ');
      FQuery.Sql.Add('  ,:email ');
      FQuery.Sql.Add('  ,:telefone ');
      FQuery.Sql.Add('  ,:site ');
      FQuery.Sql.Add('  ,:data_cadastro ');
      FQuery.Sql.Add('  ,:ativo ');
      FQuery.Sql.Add('  ,:celular ');
      FQuery.Sql.Add(') ON CONFLICT (id_empresa) ');
      FQuery.Sql.Add('DO UPDATE SET ');
      FQuery.Sql.Add('  razao_social = :razao_social ');
      FQuery.Sql.Add('  ,nome_fantasia = :nome_fantasia ');
      FQuery.Sql.Add('  ,cnpj = :cnpj ');
      FQuery.Sql.Add('  ,inscricao_estadual = :inscricao_estadual ');
      FQuery.Sql.Add('  ,inscricao_municipal = :inscricao_municipal ');
      FQuery.Sql.Add('  ,regime_tributario = :regime_tributario ');
      FQuery.Sql.Add('  ,crt = :crt ');
      FQuery.Sql.Add('  ,email = :email ');
      FQuery.Sql.Add('  ,telefone = :telefone ');
      FQuery.Sql.Add('  ,site = :site ');
      FQuery.Sql.Add('  ,data_cadastro = :data_cadastro ');
      FQuery.Sql.Add('  ,ativo = :ativo ');
      FQuery.Sql.Add('  ,celular = :celular; ');
      FQuery.ParamByName('id_empresa').AsInteger := fId_Empresa;
      FQuery.ParamByName('razao_social').AsString := edrazao_social.Text;
      if Trim(ednome_fantasia.Text) = '' then
        FQuery.ParamByName('nome_fantasia').Clear
      else
        FQuery.ParamByName('nome_fantasia').AsString := ednome_fantasia.Text;
      FQuery.ParamByName('cnpj').AsString := edcnpj.Text;
      if Trim(edinscricao_estadual.Text) = '' then
        FQuery.ParamByName('inscricao_estadual').Clear
      else
        FQuery.ParamByName('inscricao_estadual').AsString := edinscricao_estadual.Text;
      if Trim(edinscricao_municipal.Text) = '' then
        FQuery.ParamByName('inscricao_municipal').Clear
      else
        FQuery.ParamByName('inscricao_municipal').AsString := edinscricao_municipal.Text;
      if Trim(cbregime_tributario.Text) = '' then
        FQuery.ParamByName('regime_tributario').Clear
      else
        FQuery.ParamByName('regime_tributario').AsString := cbregime_tributario.Text;
      if Trim(edcrt.Text) = '' then
        FQuery.ParamByName('crt').Clear
      else
        FQuery.ParamByName('crt').AsString := edcrt.Text;
      if Trim(edemail.Text) = '' then
        FQuery.ParamByName('email').Clear
      else
        FQuery.ParamByName('email').AsString := edemail.Text;
      if Trim(edtelefone.Text) = '' then
        FQuery.ParamByName('telefone').Clear
      else
        FQuery.ParamByName('telefone').AsString := edtelefone.Text;
      if Trim(edsite.Text) = '' then
        FQuery.ParamByName('site').Clear
      else
        FQuery.ParamByName('site').AsString := edsite.Text;
      FQuery.ParamByName('data_cadastro').AsDateTime := Now;
      FQuery.ParamByName('ativo').AsInteger := cbativo.ItemIndex;
      if Trim(edcelular.Text) = '' then
        FQuery.ParamByName('celular').Clear
      else
        FQuery.ParamByName('celular').AsString := edcelular.Text;
      FQuery.ExecSQL;
      edid_empresa.Text := fId_Empresa.ToString;


      //Salvando certificado digital...
      fId_Certificado := 0;
      if Trim(edcaminho_arquivo.Text) <> '' then
      begin
        //Sequencial certificado
        if StrToIntDef(edid_certificado.Text,0) = 0 then
          fId_Certificado := fDm.Sequencial('public.certificado_digital',fId_Empresa)
        else
          fId_Certificado := StrToIntDef(edid_certificado.Text,0);

        //Inserindo informações
        FQuery.Close;
        FQuery.Sql.Clear;
        FQuery.Sql.Add('INSERT INTO public.certificado_digital ( ');
        FQuery.Sql.Add('  id_certificado ');
        FQuery.Sql.Add('  ,id_empresa ');
        FQuery.Sql.Add('  ,tipo ');
        FQuery.Sql.Add('  ,validade ');
        FQuery.Sql.Add('  ,caminho_arquivo ');
        FQuery.Sql.Add('  ,senha ');
        FQuery.Sql.Add(') VALUES( ');
        FQuery.Sql.Add('  :id_certificado ');
        FQuery.Sql.Add('  ,:id_empresa ');
        FQuery.Sql.Add('  ,:tipo ');
        FQuery.Sql.Add('  ,:validade ');
        FQuery.Sql.Add('  ,:caminho_arquivo ');
        FQuery.Sql.Add('  ,:senha ');
        FQuery.Sql.Add(') ON CONFLICT (id_empresa,id_certificado) DO ');
        FQuery.Sql.Add('UPDATE  SET ');
        FQuery.Sql.Add('  tipo =  :tipo ');
        FQuery.Sql.Add('  ,validade = :validade ');
        FQuery.Sql.Add('  ,caminho_arquivo = :caminho_arquivo ');
        FQuery.Sql.Add('  ,senha = :senha; ');
        FQuery.ParamByName('id_empresa').AsInteger := fId_Empresa;
        FQuery.ParamByName('id_certificado').AsInteger := fId_Certificado;
        FQuery.ParamByName('tipo').AsInteger := cbativo.ItemIndex;
        FQuery.ParamByName('validade').AsDate := edvalidade.Date;
        FQuery.ParamByName('caminho_arquivo').AsString := edcaminho_arquivo.Text;
        FQuery.ParamByName('senha').AsString := edsenha.Text;
        FQuery.ExecSQL;
      end;

      if MessageDlg('O cadastro da empresa foi concluído com êxito. Gostaria de atualizar os dados de Endereço e Bancários neste momento?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrNo then
        Close;


      (*
      fEmpresa := TJSONObject.Create;
      fCertificado := TJSONObject.Create;

      if Trim(FHost) = '' then
        raise Exception.Create('Host não informado');

      {$Region 'Montando JSon'}
        //Empresa...
        fEmpresa.Add('idEmpresa',StrToIntDef(edid_empresa.Text,0));
        fEmpresa.Add('razaoSocial',edrazao_social.Text);
        fEmpresa.Add('nomeFantasia',ednome_fantasia.Text);
        fEmpresa.Add('cnpj',RemoverMascara(edcnpj.Text));
        fEmpresa.Add('inscricaoEstadual',edinscricao_estadual.Text);
        fEmpresa.Add('inscricaoMunicipal',edinscricao_municipal.Text);
        fEmpresa.Add('regimeTributario',cbregime_tributario.Text);
        fEmpresa.Add('crt',edcrt.Text);
        fEmpresa.Add('email',edemail.Text);
        fEmpresa.Add('telefone',edtelefone.Text);
        fEmpresa.Add('site',edsite.Text);
        fEmpresa.Add('dataCadastro',Now);
        fEmpresa.Add('ativo',cbativo.ItemIndex);
        fEmpresa.Add('celular',edcelular.Text);

        //Endereço da Empresa...
        fEmpresa.Add('endereco',memD_Endereco.ToJSONArray);

        //Contas bancárias...
        fEmpresa.Add('contaBancaria',memD_CBanco.ToJSONArray);

        //Certificado...
        fCertificado.Add('idCertificado',StrToIntDef(edid_certificado.Text,0));
        fCertificado.Add('idEmpresa',StrToIntDef(edid_empresa.Text,0));
        fCertificado.Add('tipo',cbtipo.ItemIndex);
        fCertificado.Add('validade',edvalidade.Date);
        fCertificado.Add('caminhoArquivo',edcaminho_arquivo.Text);
        fCertificado.Add('senha',edsenha.Text);
        fEmpresa.Add('certificadoDigital',fCertificado);

      {$EndRegion 'Montando JSon'}

      {$Region 'Enviando dados para o Servidor'}
        if fEmpresa.Count = 0 then
          raise Exception.Create('Não há dados para serem salvos.');

        if StrToIntDef(edid_empresa.Text,0) = 0 then
        begin
          FResp := TRequest.New.BaseURL(FHost)
                   .Resource('empresa')
                   .AddBody(fEmpresa)
                   .Accept('application/json')
                   .Post;
        end
        else
        begin
          FResp := TRequest.New.BaseURL(FHost)
                   .Resource('empresa')
                   .AddBody(fEmpresa)
                   .Accept('application/json')
                   .Put;
        end;

        fRet := '';
        fRet := fResp.Content;
        if Trim(fRet) = '' then
          raise Exception.Create('Não houve retorno do Server');

        fRetorno := TJSONObject(GetJSON(fRet));

        if fRetorno['success'].AsBoolean = False then
          raise Exception.Create(fRetorno['message'].AsString);

      {$EndRegion 'Enviando dados para o Servidor'}
      *)
    except
      on E: Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'Gravar',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOk],0);
      end;
    end;
  finally
    if Assigned(FQuery) then
      FreeAndNil(FQuery);
    if Assigned(fDm) then
      FreeAndNil(fDm);
  end;
end;

procedure TfrmCadEmpresa.btEnd_AddClick(Sender: TObject);
begin
  try
    FfrmCad_Empresa_Endereco.Clear_Fields;
    Emissor.Empresa_Fields.id_empresa := StrToIntDef(edid_empresa.Text,0);
    ShowPopupModal('Popup' + FfrmCad_Empresa_Endereco.Name);
    Listar_Endereco(edid_empresa.Text);
  except
    on E:Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'btEnd_AddClick',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmCadEmpresa.cbativoKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edrazao_social);
end;

procedure TfrmCadEmpresa.Clear_Parans(const AClear:Integer);
begin
  case AClear of
    0:begin
        with Emissor.EmpEnd_Fields do
        begin
          id_endereco := 0;
          logradouro := '';
          numero := '';
          complemento := '';
          bairro := '';
          municipio := '';
          codigo_municipio_ibge := '';
          uf := '';
          cep := '';
          pais := '';
          codigo_pais_ibge := '';
          tipo_endereco := 0;
        end;
    end;
    1:begin
        with Emissor.EmpCB_Fields do
        begin
          id_banco := 0;
          id_empresa := 0;
          banco := '';
          agencia := '';
          conta := '';
          tipo_conta := 0;
        end;
    end;
  end;
end;

procedure TfrmCadEmpresa.cbregime_tributarioChange(Sender: TObject);
begin
  case cbregime_tributario.ItemIndex of
    0:edcrt.Text := '1';
    1:edcrt.Text := '2';
    2,3:edcrt.Text := '3';
    4:edcrt.Text := '4';
  end;
end;

procedure TfrmCadEmpresa.cbregime_tributarioKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edcrt);
end;

procedure TfrmCadEmpresa.cbtipoKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edvalidade);
end;

procedure TfrmCadEmpresa.edcaminho_arquivoKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edsenha);
end;

procedure TfrmCadEmpresa.edcelularKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edemail);
end;

procedure TfrmCadEmpresa.edcnpjExit(Sender: TObject);
begin
  try
    try
      fDM_ACBr := TDM_Acbr.Create(Nil);

      if Trim(edcnpj.Text) = '' then
        Exit;

      case Length(RemoverMascara(edcnpj.Text)) of
        11:fDM_ACBr.ACBrValidador.TipoDocto := docCPF;
        14:fDM_ACBr.ACBrValidador.TipoDocto := docCNPJ;
        else
	  raise Exception.Create('Documento inválido');
      end;

      fDM_ACBr.ACBrValidador.Documento := RemoverMascara(edcnpj.Text);
      if fDM_ACBr.ACBrValidador.Validar then
        edcnpj.Text := fDM_ACBr.ACBrValidador.Formatar
      else
        raise Exception.Create('Documento inválido');

    finally
      FreeAndNil(fDM_ACBr);
    end;
  except
    on E :Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'edcnpjExit',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmCadEmpresa.edcnpjKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edinscricao_estadual);
end;

procedure TfrmCadEmpresa.edcrtKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edtelefone);
end;

procedure TfrmCadEmpresa.edemailKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edsite);
end;

procedure TfrmCadEmpresa.edid_certificadoKeyPress(Sender: TObject; var Key: char
  );
begin
  if Key = #13 then EnterAsTab(Self.cbtipo);
end;

procedure TfrmCadEmpresa.edid_empresaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edcnpj);
end;

procedure TfrmCadEmpresa.edinscricao_estadualKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edinscricao_municipal);
end;

procedure TfrmCadEmpresa.edinscricao_municipalKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.cbativo);
end;

procedure TfrmCadEmpresa.ednome_fantasiaKeyPress(Sender: TObject; var Key: char
  );
begin
  if Key = #13 then EnterAsTab(Self.cbregime_tributario);
end;

procedure TfrmCadEmpresa.edrazao_socialKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.ednome_fantasia);
end;

procedure TfrmCadEmpresa.edtelefoneKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edcelular);
end;

procedure TfrmCadEmpresa.edvalidadeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edcaminho_arquivo);
end;

procedure TfrmCadEmpresa.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with Emissor.Empresa_Fields do
  begin
    id_empresa := StrToIntDef(edid_empresa.Text,0);
    razao_social := edrazao_social.Text;
    nome_fantasia := ednome_fantasia.Text;
    cnpj := edcnpj.Text;
    inscricao_estadual := edinscricao_estadual.Text;
    inscricao_municipal := edinscricao_municipal.Text;
    regime_tributario := cbregime_tributario.Text;
    crt := edcrt.Text;
    email := edemail.Text;
    telefone := edtelefone.Text;
    site := edsite.Text;
    ativo := cbativo.ItemIndex;
    celular := edcelular.Text;
  end;
end;

procedure TfrmCadEmpresa.FormCreate(Sender: TObject);
begin
  try
    try

      FIniFile := TIniFile.Create(ConfigFile);
      fDmPrincipal := TDM.Create(Self);

      {
      FHost := '';
      FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');
      if Trim(FHost) = '' then
        raise Exception.Create('Host de acesso ao servidor não informado.');
      }

      ZQContaBancaria.Connection := fDmPrincipal.ZConnection;
      ZQEndereco.Connection := fDmPrincipal.ZConnection;

    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'FormCreate',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
  end;
end;

procedure TfrmCadEmpresa.FormDestroy(Sender: TObject);
begin
  if Assigned(fDmPrincipal) then
    FreeAndNil(fDmPrincipal);

  FreeAndNil(FIniFile);
end;

procedure TfrmCadEmpresa.OnClick_Delete_End(const aId_Empresa,aId_Endereco: Integer);
var
{
  fResp :IResponse;
  fRet :String;
  fBody :TJSONObject;
}
  fQuery :TZQuery;
  fDm :TDM;
begin
  try
    try
      fDm := TDM.Create(Self);

      if MessageDlg('Deseja excluir o Endereço selecionado?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
      begin
        fQuery := fDm.GetQuery;
        fQuery.Sql.Add('DELETE FROM public.endereco_empresa ');
        fQuery.Sql.Add('WHERE id_empresa = ' + edid_empresa.Text);
        fQuery.Sql.Add('  AND id_endereco = ' + ZQEndereco.FieldByName('id_endereco').AsString);
        fQuery.ExecSQL;

        Listar_Endereco(edid_empresa.Text);
        {
        fResp := TRequest.New.BaseURL(FHost)
      	         .AddParam('idEmpresa',memD_Endereco.FieldByName('idempresa').AsString)
      	         .AddParam('idEndereco',memD_Endereco.FieldByName('idendereco').AsString)
                 .Resource('empresa/endereco')
                 .Accept('application/json')
                 .Delete;

        fRet := '';
        fRet := fResp.Content;
        fBody := TJSONObject(GetJSON(fRet));
        if fBody['success'].AsBoolean = False then
          raise Exception.Create(fBody['message'].AsString)
        else
          MessageDlg(fBody['message'].AsString,TMsgDlgType.mtInformation,[mbOK],0);

        memD_Endereco.DisableControls;
        try
      	   memD_Endereco.Delete;
           memD_Endereco.Open;
           //memD_Endereco.Refresh;
        finally
          DBGrid_End.Refresh;
          memD_Endereco.EnableControls;
        end;
        }
      end;
    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'OnClick_Delete_End',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;

  finally
    if Assigned(fQuery) then
      FreeAndNil(fQuery);
    if Assigned(fDm) then
      FreeAndNil(fDm);
  end;
end;

procedure TfrmCadEmpresa.OnClick_Edit_CBanco;
begin
  try
    FfrmCad_Empresa_DadosBancarios.edid_banco.Text := ZQContaBancaria.FieldByName('id_banco').AsString;
    FfrmCad_Empresa_DadosBancarios.cbtipo_conta.ItemIndex := ZQContaBancaria.FieldByName('tipo_conta').AsInteger;
    FfrmCad_Empresa_DadosBancarios.edbanco.Text := ZQContaBancaria.FieldByName('banco').AsString;
    FfrmCad_Empresa_DadosBancarios.edagencia.Text := ZQContaBancaria.FieldByName('agencia').AsString;;
    FfrmCad_Empresa_DadosBancarios.edconta.Text := ZQContaBancaria.FieldByName('conta').AsString;
    Emissor.Empresa_Fields.id_empresa := StrToIntDef(edid_empresa.Text,0);
    ShowPopupModal('Popup' + FfrmCad_Empresa_DadosBancarios.Name);

    Listar_DadosBancarios(edid_empresa.Text);
  except
    on E:Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'OnClick_Edit_CBanco',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmCadEmpresa.OnClick_Delete_CBanco(const aId_Empresa,aId_CBando: Integer);
var
  {
  fResp :IResponse;
  fRet :String;
  fBody :TJSONObject;
  }
  fQuery :TZQuery;
  fDm :TDM;
begin
  try
    try
      fDm := TDM.Create(Self);

      if MessageDlg('Deseja excluir a Conta Bancária selecionada?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
      begin
        fQuery := fDm.GetQuery;
        fQuery.SQL.Add('DELETE FROM public.dados_bancarios ');
        fQuery.SQL.Add('WHERE id_empresa = ' + edid_empresa.Text);
        fQuery.SQL.Add('  AND id_banco = ' + ZQContaBancaria.FieldByName('id_banco').AsString);
        fQuery.ExecSQL;

        Listar_DadosBancarios(edid_empresa.Text);

        {
        fResp := TRequest.New.BaseURL(FHost)
      	         .AddParam('idEmpresa',memD_Endereco.FieldByName('idempresa').AsString)
      	         .AddParam('idDBanco',memD_Endereco.FieldByName('idbanco').AsString)
                 .Resource('empresa/dBanco')
                 .Accept('application/json')
                 .Delete;

        fRet := '';
        fRet := fResp.Content;
        fBody := TJSONObject(GetJSON(fRet));
        if fBody['success'].AsBoolean = False then
          raise Exception.Create(fBody['message'].AsString)
        else
          MessageDlg(fBody['message'].AsString,TMsgDlgType.mtInformation,[mbOK],0);
        memD_CBanco.Delete;
        }
      end;
    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'OnClick_Delete_CBanco',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
    if Assigned(fQuery) then
      FreeAndNil(fQuery);
    if Assigned(fDm) then
      FreeAndNil(fDm);
  end;

end;

procedure TfrmCadEmpresa.Clear_Fields;
begin
  //Dados da empresa...
  edid_empresa.Clear;
  edcnpj.Clear;
  edinscricao_estadual.Clear;
  edinscricao_municipal.Clear;
  cbativo.ItemIndex := -1;
  edrazao_social.Clear;
  ednome_fantasia.Clear;
  cbregime_tributario.ItemIndex := -1;
  edcrt.Clear;
  edtelefone.Clear;
  edcelular.Clear;
  edemail.Clear;
  edsite.Clear;

  //Endereço
  ZQEndereco.Close;

  //Dados Bancários...
  ZQContaBancaria.Close;

  //Certificado digital...
  edid_certificado.Clear;
  cbtipo.ItemIndex := -1;
  edvalidade.Date := Date;
  edcaminho_arquivo.Clear;
  edsenha.Clear;
end;

procedure TfrmCadEmpresa.Listar_Endereco(const aIdEmpresa: String);
begin
  try
    ZQEndereco.Close;
    ZQEndereco.Sql.Clear;
    ZQEndereco.Sql.Add('SELECT ');
    ZQEndereco.Sql.Add('  ee.* ');
    ZQEndereco.Sql.Add('  ,case ee.tipo_endereco ');
    ZQEndereco.Sql.Add('    when 0 then ''Comercial'' ');
    ZQEndereco.Sql.Add('    when 1 then ''Residencial'' ');
    ZQEndereco.Sql.Add('    when 2 then ''Entrega (Shipping)'' ');
    ZQEndereco.Sql.Add('    when 3 then ''Cobrança/Faturamento'' ');
    ZQEndereco.Sql.Add('    when 4 then ''Correspondência'' ');
    ZQEndereco.Sql.Add('    when 5 then ''Endereço de Instalação (quando envolve serviços técnicos), endereço rural'' ');
    ZQEndereco.Sql.Add('    when 6 then ''Endereço Rural (para produtores)'' ');
    ZQEndereco.Sql.Add('    when 7 then ''Endereço Temporário (Eventos, obras)'' ');
    ZQEndereco.Sql.Add('  end tipo_endereco_desc ');
    ZQEndereco.Sql.Add('FROM public.endereco_empresa ee ');
    ZQEndereco.Sql.Add('where ee.id_empresa = ' + aIdEmpresa);
    ZQEndereco.Sql.Add('order by ee.id_endereco; ');
    ZQEndereco.Open;
  except
    On E:Exception do
    begin
      raise Exception.Create('Listar endereço:' +sLineBreak+ E.Message);
    end;
  end;
end;

procedure TfrmCadEmpresa.Listar_DadosBancarios(const aIdEmpresa: String);
begin
  try
    ZQContaBancaria.Close;
    ZQContaBancaria.SQL.Clear;
    ZQContaBancaria.SQL.Add('select ');
    ZQContaBancaria.SQL.Add('  db.* ');
    ZQContaBancaria.SQL.Add('  ,case db.tipo_conta ');
    ZQContaBancaria.SQL.Add('    when 0 then ''Corrente (PF/PJ) - Movimentação diária'' ');
    ZQContaBancaria.SQL.Add('    when 1 then ''Poupança (PF) - Guardar e render dinheiro'' ');
    ZQContaBancaria.SQL.Add('    when 2 then ''Salário (PF) - Receber salário/benefícios'' ');
    ZQContaBancaria.SQL.Add('    when 3 then ''Universitária/Jovem (Estudantes/jovens) - Condições especiais'' ');
    ZQContaBancaria.SQL.Add('    when 4 then ''PJ/MEI (Empresas/autônomos) - Gestão financeira empresarial'' ');
    ZQContaBancaria.SQL.Add('    when 5 then ''Digital (PF/PJ) - Movimentação online'' ');
    ZQContaBancaria.SQL.Add('    when 6 then ''Investimento (PF/PJ) - Aplicações financeiras'' ');
    ZQContaBancaria.SQL.Add('    when 7 then ''Conjunta (PF) - Compartilhar recursos'' ');
    ZQContaBancaria.SQL.Add('  end tipo_conta_desc ');
    ZQContaBancaria.SQL.Add('from public.dados_bancarios db ');
    ZQContaBancaria.SQL.Add('where db.id_empresa = ' + aIdEmpresa);
    ZQContaBancaria.SQL.Add('order by ');
    ZQContaBancaria.SQL.Add('  db.id_banco; ');
    ZQContaBancaria.Open;
  except
    On E:Exception do
    begin
      raise Exception.Create('Listar dados bancários:' +sLineBreak+ E.Message);
    end;
  end;
end;

procedure TfrmCadEmpresa.ExportD2Bridge;
begin
  inherited;

  Title := Self.Caption;

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  FfrmCad_Empresa_Endereco := TfrmCad_Empresa_Endereco.Create(Self);
  D2Bridge.AddNested(FfrmCad_Empresa_Endereco);

  FfrmCad_Empresa_DadosBancarios := TfrmCad_Empresa_DadosBancarios.Create(Self);
  D2Bridge.AddNested(FfrmCad_Empresa_DadosBancarios);

  with D2Bridge.Items.add do
  begin
    {Yours Controls}
    with Row.Items.Add do
    begin
      with Row.Items.Add do
      begin
        with Tabs('TabControl1') do
        begin
          with AddTab(pcPrincipal.Pages[0].Caption).Items.Add do
          begin
            with Card.Items.Add do
            begin
              with Row.Items.Add do
              begin
                FormGroup(lbid_empresa.Caption,CSSClass.Col.colsize1).AddLCLObj(edid_empresa);
                FormGroup(lbcnpj.Caption,CSSClass.Col.colsize3).AddLCLObj(edcnpj,'ValidationGravar',True);
                FormGroup(lbinscricao_estadual.Caption,CSSClass.Col.colsize3).AddLCLObj(edinscricao_estadual);
                FormGroup(lbinscricao_municipal.Caption,CSSClass.Col.colsize3).AddLCLObj(edinscricao_municipal);
                FormGroup(lbativo.Caption,CSSClass.Col.colsize2).AddLCLObj(cbativo,'ValidationGravar',True);
              end;

              with Row.Items.Add do
                FormGroup(lbrazao_social.Caption,CSSClass.Col.colsize12).AddLCLObj(edrazao_social,'ValidationGravar',True);

              with Row.Items.Add do
                FormGroup(lbnome_fantasia.Caption,CSSClass.Col.colsize12).AddLCLObj(ednome_fantasia);

              with Row.Items.Add do
              begin
                FormGroup(lbregime_tributario.Caption,CSSClass.Col.colsize4).AddLCLObj(cbregime_tributario);
                FormGroup(lbcrt.Caption,CSSClass.Col.colsize2).AddLCLObj(edcrt);
                FormGroup(lbtelefone.Caption,CSSClass.Col.colsize3).AddLCLObj(edtelefone);
                FormGroup(lbcelular.Caption,CSSClass.Col.colsize3).AddLCLObj(edcelular);
              end;

              with Row.Items.Add do
                FormGroup(lbemail.Caption,CSSClass.Col.colsize12).AddLCLObj(edemail);

              with Row.Items.Add do
                FormGroup(lbsite.Caption,CSSClass.Col.colsize12).AddLCLObj(edsite);
            end;
          end;

          with AddTab(pcPrincipal.Pages[1].Caption).Items.Add do
          begin
            with Card.Items.Add do
            begin
              with Row(CSSClass.DivHtml.Align_Left + ' ' + CSSClass.Space.margim_bottom3 + ' ' + CSSClass.Space.margim_left3).Items.Add do
                LCLObj(btEnd_Add, CSSClass.Button.add + CSSClass.Col.colsize1);
              with Row.Items.Add do
                LCLObj(DBGrid_End);
            end;
          end;

          with AddTab(pcPrincipal.Pages[2].Caption).Items.Add do
          begin
            with Card.Items.Add do
            begin
              with Row(CSSClass.DivHtml.Align_Left + ' ' + CSSClass.Space.margim_bottom3 + ' ' + CSSClass.Space.margim_left3).Items.Add do
                LCLObj(btCB_Add, CSSClass.Button.add + CSSClass.Col.colsize1);
              with Row.Items.Add do
                LCLObj(DBGrid_DB);
            end;
          end;

          with AddTab(pcPrincipal.Pages[3].Caption).Items.Add do
          begin
            with Card.Items.Add do
            begin
              with Row.Items.Add do
              begin
                FormGroup(lbid_certificado.Caption,CSSClass.Col.colsize1).AddLCLObj(edid_certificado);
                FormGroup(lbtipo.Caption,CSSClass.Col.colsize9).AddLCLObj(cbtipo);
                FormGroup(lbvalidade.Caption,CSSClass.Col.colsize2).AddLCLObj(edvalidade);
              end;

              with Row.Items.Add do
                FormGroup(lbsenha.Caption,CSSClass.Col.colsize6).AddLCLObj(edsenha);

              with PanelGroup('Selecione o arquivo do Certificado', '', false, CSSClass.Col.colsize12).Items.Add do
              begin
                with Row(CSSClass.Space.margim_bottom3).Items.Add do
                  FormGroup('',CSSClass.Col.colsize12).AddLCLObj(edcaminho_arquivo);
                with Row.Items.Add do
                  Upload('Selecione o Certificado','pfx,p12');
              end;
            end;
          end;
        end;
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar, 'ValidationGravar',False, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btFechar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;

    with Popup('Popup' + FfrmCad_Empresa_Endereco.Name,'Endereço da Empresa',True,CSSClass.Popup.ExtraLarge).Items.Add do
      Nested(FfrmCad_Empresa_Endereco);
    with Popup('Popup' + FfrmCad_Empresa_DadosBancarios.Name,'Contas Bancárias da Empresas',True,CSSClass.Popup.ExtraLarge).Items.Add do
      Nested(FfrmCad_Empresa_DadosBancarios);

  end;

end;

procedure TfrmCadEmpresa.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  if PrismControl.VCLComponent = DBGrid_End then
  begin
    with PrismControl.AsDBGrid do
    begin
      PrismControl.AsDBGrid.RecordsPerPage := 5;
      with Columns.Add do
      begin
        ColumnIndex := 0;
        Title := D2Bridge.LangNav.Button.CaptionOptions;
        Width := 45;

        //Create Popup + Button
        with Buttons.Add do
        begin
          ButtonModel:= TButtonModel.list;
          Caption := '';

          //Edit
          with Add do
          begin
            ButtonModel:= TButtonModel.Edit;
          end;

          //Delete
          with Add do
          begin
            ButtonModel:= TButtonModel.Delete;
          end;
        end;
      end;
    end;
  end;

  if PrismControl.VCLComponent = DBGrid_DB then
  begin
    PrismControl.AsDBGrid.RecordsPerPage := 5;
    with PrismControl.AsDBGrid do
    begin
      with Columns.Add do
      begin
        ColumnIndex := 0;
        Title := D2Bridge.LangNav.Button.CaptionOptions;
        Width := 45;

        //Create Popup + Button
        with Buttons.Add do
        begin
          ButtonModel:= TButtonModel.list;
          Caption := '';

          //Edit
          with Add do
          begin
            ButtonModel:= TButtonModel.Edit;
          end;

          //Delete
          with Add do
          begin
            ButtonModel:= TButtonModel.Delete;
          end;
        end;
      end;
    end;
  end;

  if PrismControl.VCLComponent = edtelefone then
    PrismControl.AsEdit.TextMask :=  '''mask'' : ''(99)9999-9999''';
  if PrismControl.VCLComponent = edcelular then
    PrismControl.AsEdit.TextMask := TPrismTextMask.BrazilPhone;//'''mask'' : ''(99)9999-9999''';
  if PrismControl.VCLComponent = edemail then
    PrismControl.AsEdit.TextMask:= TPrismTextMask.Email;


  //Change Init Property of Prism Controls
  {
  if PrismControl.VCLComponent = edid_empresa then
    PrismControl.AsEdit.DataType:= TPrismFieldType.PrismFieldTypeInteger;

  if PrismControl.IsDBGrid then
  begin
    PrismControl.AsDBGrid.RecordsPerPage:= 10;
    PrismControl.AsDBGrid.MaxRecords:= 2000;
  end;
  }
end;

procedure TfrmCadEmpresa.RenderD2Bridge(const PrismControl: TPrismControl;
  var HTMLControl: string);
begin
  inherited;

  //Intercept HTML
  {
  if PrismControl.VCLComponent = edid_empresa then
  begin
    HTMLControl:= '</>';
  end;
  }
end;

procedure TfrmCadEmpresa.Upload(AFiles: TStrings; Sender: TObject);
begin
  //inherited Upload(AFiles, Sender);
  edcaminho_arquivo.Text := EndPath + AFiles[0];
end;

procedure TfrmCadEmpresa.CellButtonClick(APrismDBGrid: TPrismDBGrid; APrismCellButton: TPrismDBGridColumnButton;
  AColIndex: Integer; ARow: Integer);
begin
  //inherited CellButtonClick(APrismDBGrid, APrismCellButton, AColIndex, ARow);
  if APrismDBGrid.VCLComponent = DBGrid_End then
  begin
    if APrismCellButton.Identify = TButtonModel.Edit.Identity then OnClick_Edit_End;
    //if APrismCellButton.Identify = TButtonModel.Delete.Identity then OnClick_Delete_End(memD_Endereco.FieldByName('idendereco').AsInteger);
  end;

  if APrismDBGrid.VCLComponent = DBGrid_DB then
  begin
    if APrismCellButton.Identify = TButtonModel.Edit.Identity then OnClick_Edit_CBanco;
    //if APrismCellButton.Identify = TButtonModel.Delete.Identity then OnClick_Delete_CBanco(memD_CBanco.FieldByName('idbanco').AsInteger);
  end;

end;

procedure TfrmCadEmpresa.OnClick_Edit_End;
begin
  try
    if not ZQEndereco.IsEmpty then
    begin
      FfrmCad_Empresa_Endereco.edid_endereco.Text := ZQEndereco.FieldByName('id_endereco').AsString;
      FfrmCad_Empresa_Endereco.cbtipo_endereco.ItemIndex := ZQEndereco.FieldByName('tipo_endereco').AsInteger;
      FfrmCad_Empresa_Endereco.edcep.Text := ZQEndereco.FieldByName('cep').AsString;
      FfrmCad_Empresa_Endereco.edlogradouro.Text := ZQEndereco.FieldByName('logradouro').AsString;
      FfrmCad_Empresa_Endereco.ednumero.Text := ZQEndereco.FieldByName('numero').AsString;
      FfrmCad_Empresa_Endereco.edcomplemento.Text := ZQEndereco.FieldByName('complemento').AsString;
      FfrmCad_Empresa_Endereco.edbairro.Text := ZQEndereco.FieldByName('bairro').AsString;
      FfrmCad_Empresa_Endereco.edmunicipio.Text := ZQEndereco.FieldByName('municipio').AsString;
      FfrmCad_Empresa_Endereco.edcodigo_municipio_ibge.Text := ZQEndereco.FieldByName('codigo_municipio_ibge').AsString;
      FfrmCad_Empresa_Endereco.eduf.Text := ZQEndereco.FieldByName('uf').AsString;
      FfrmCad_Empresa_Endereco.edpais.Text := ZQEndereco.FieldByName('pais').AsString;
      FfrmCad_Empresa_Endereco.edcodigo_pais_ibge.Text := ZQEndereco.FieldByName('codigo_pais_ibge').AsString;
      Emissor.Empresa_Fields.id_empresa := StrToIntDef(edid_empresa.Text,0);
      ShowPopupModal('Popup' + FfrmCad_Empresa_Endereco.Name);

      Listar_Endereco(edid_empresa.Text);
    end;

  except
    on E:Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'OnClick_Edit_End',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

end.
