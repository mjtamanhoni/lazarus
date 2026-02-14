unit uEmpresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, memds, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DateUtils,
  DBGrids, EditBtn, Menus, ZDataset, D2Bridge.Forms, Forms, IniFiles, BufDataset,
  fpjson,
  DataSet.Serialize,
  RESTRequest4D,
  jsonparser,
  uBase.Functions, uDM.ACBr,
  uPrincipal, uCad.Empresa, uBase.DataSets;

type

  { TfrmEmpresa }

  TfrmEmpresa = class(TfrmPrincipal)
    btNovo: TButton;
    btSelPesquisa: TButton;
    dsRegistro: TDataSource;
    DBGrid_Empresa: TDBGrid;
    edPesquisar: TEdit;
    miCNPJ_CPF: TMenuItem;
    miNomeFantasia: TMenuItem;
    miRazaoSocial: TMenuItem;
    miId: TMenuItem;
    pnHeader: TPanel;
    pnDetail: TPanel;
    pnFooter: TPanel;
    pnFiltro: TPanel;
    pnTipoFiltro2: TPanel;
    pMenu_Filtro: TPopupMenu;
    procedure btNovoClick(Sender: TObject);
    procedure edPesquisarKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miRazaoSocialClick(Sender: TObject);
  private
    { Private declarations }
    FfrmCadEmpresa :TfrmCadEmpresa;

    FHost :String;
    FIniFile :TIniFile;

    memD_Empresas :TBufDataset;
    memD_Endereco :TBufDataset;
    memD_CBanco :TBufDataset;
    memD_Certificado :TBufDataset;

    procedure Create_DataSet;
    procedure Adiciona_Dados(const AJSon_Empresa,AJSon_Endereco,AJSon_CBanco,AJSon_Certificado:TJSONArray);

    procedure Pesquisar;
    procedure OnClick_Edit(const AId: Integer; const ANome:String);
    procedure OnClick_Delete(const AId: Integer; const ANome:String);
    procedure OnClick_Print(const AId: Integer; const ANome:String);
  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
    procedure CellButtonClick(APrismDBGrid: TPrismDBGrid; APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer); overload; override;
  end;

function frmEmpresa: TfrmEmpresa;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmEmpresa: TfrmEmpresa;
begin
  result := (TfrmEmpresa.GetInstance as TfrmEmpresa);
end;

procedure TfrmEmpresa.FormCreate(Sender: TObject);
begin
  try
    try
    FHost := '';
    FIniFile := TIniFile.Create(ConfigFile);
    FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');

    //CriaDataset_Empresa;
    memD_Empresas := TBufDataset.Create(Self);
    memD_Endereco := TBufDataset.Create(Self);
    memD_CBanco := TBufDataset.Create(Self);
    memD_Certificado := TBufDataset.Create(Self);

    if Trim(FHost) = '' then
      raise Exception.Create('Host de acesso ao servidor não informado.');

    except
      on E :Exception do
      	 MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  finally
  end;
end;

procedure TfrmEmpresa.FormDestroy(Sender: TObject);
begin
  FreeAndNil(memD_Empresas);
  FreeAndNil(memD_Endereco);
  FreeAndNil(memD_CBanco);
  FreeAndNil(memD_Certificado);
  FreeAndNil(FIniFile);
end;

procedure TfrmEmpresa.FormShow(Sender: TObject);
begin
  Pesquisar;
end;

procedure TfrmEmpresa.edPesquisarKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Pesquisar;
end;

procedure TfrmEmpresa.btNovoClick(Sender: TObject);
begin
  FfrmCadEmpresa.Clear_Fields;
  FfrmCadEmpresa.pcPrincipal.ActivePageIndex := 0;
  ShowPopupModal('Popup' + FfrmCadEmpresa.Name);
  Pesquisar;
end;

procedure TfrmEmpresa.miRazaoSocialClick(Sender: TObject);
begin
  edPesquisar.Tag := TMenuItem(Sender).Tag;
  case TMenuItem(Sender).Tag of
    0:edPesquisar.TextHint := 'Pesquisar pelo ID da Empresa';
    1:edPesquisar.TextHint := 'Pesquisar pela Razão Social da Empresa';
    2:edPesquisar.TextHint := 'Pesquisar pelo Nome Fantasia da Empresa';
    3:edPesquisar.TextHint := 'Pesquisar pela CNPJ/CPF';
  end;
  Pesquisar;

end;

procedure TfrmEmpresa.Create_DataSet;
var
  FEmpresa :TEmpresa;
begin
  FEmpresa := TEmpresa.Create;
  try
    try
      //Criando bufdataset - Empresa
      FEmpresa.Criar_DataSet_Empresa(memD_Empresas);
      dsRegistro.DataSet := memD_Empresas;
      DBGrid_Empresa.DataSource := dsRegistro;
      ConfigColGridAut(DBGrid_Empresa,memD_Empresas);

      //Criando bufdataset - Endereço
      FEmpresa.Criar_DataSet_Endereco(memD_Endereco);

      //Criando bufdataset - Conta bancária
      FEmpresa.Criar_DataSet_CBanco(memD_CBanco);

      //Criando bufdataset - Certificado digital
      FEmpresa.Criar_DataSet_Certificado(memD_Certificado);
    except
      on E:Exception do
      begin
        SaveLog(E.Message);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;

  finally
    FreeAndNil(FEmpresa)
  end;
end;

procedure TfrmEmpresa.Adiciona_Dados(const AJSon_Empresa,AJSon_Endereco,AJSon_CBanco,AJSon_Certificado: TJSONArray);
var
  I :Integer;
begin
  try
    try
      memD_Empresas.DisableControls;
      memD_Endereco.DisableControls;
      memD_CBanco.DisableControls;

      if AJSon_Empresa.Count = 0 then
        raise Exception.Create('Não há empresas para listar.');

      if not memD_Empresas.Active then
        Create_DataSet;

      //Adicionando dados da empresa...
      for I := 0 to Pred(AJSon_Empresa.Count) do
      begin
        memD_Empresas.Append;
        memD_Empresas.FieldByName('idEmpresa').AsInteger := AJSon_Empresa.Objects[I].Integers['idEmpresa'];
        memD_Empresas.FieldByName('razaoSocial').AsString := AJSon_Empresa.Objects[I].Strings['razaoSocial'];
        memD_Empresas.FieldByName('nomeFantasia').AsString := AJSon_Empresa.Objects[I].Strings['nomeFantasia'];
        memD_Empresas.FieldByName('cnpj').AsString := AJSon_Empresa.Objects[I].Strings['cnpj'];
        memD_Empresas.FieldByName('inscricaoEstadual').AsString := AJSon_Empresa.Objects[I].Strings['inscricaoEstadual'];
        memD_Empresas.FieldByName('inscricaoMunicipal').AsString := AJSon_Empresa.Objects[I].Strings['inscricaoMunicipal'];
        memD_Empresas.FieldByName('regimeTributario').AsString := AJSon_Empresa.Objects[I].Strings['regimeTributario'];
        memD_Empresas.FieldByName('crt').AsString := AJSon_Empresa.Objects[I].Strings['crt'];
        memD_Empresas.FieldByName('email').AsString := AJSon_Empresa.Objects[I].Strings['email'];
        memD_Empresas.FieldByName('telefone').AsString := AJSon_Empresa.Objects[I].Strings['telefone'];
        memD_Empresas.FieldByName('site').AsString := AJSon_Empresa.Objects[I].Strings['site'];
        memD_Empresas.FieldByName('dataCadastro').AsDateTime := ISO8601ToDateDef(AJSon_Empresa.Objects[I].Strings['dataCadastro'],0);
        memD_Empresas.FieldByName('ativo').AsInteger := AJSon_Empresa.Objects[I].Integers['ativo'];
        memD_Empresas.FieldByName('celular').AsString := AJSon_Empresa.Objects[I].Strings['celular'];
        memD_Empresas.FieldByName('crtDesc').AsString := AJSon_Empresa.Objects[I].Strings['crtDesc'];
        memD_Empresas.Post;
      end;
      memD_Empresas.EnableControls;

      //Adicionando dados do endereço...
      for I := 0 to Pred(AJSon_Endereco.Count) do
      begin
        memD_Endereco.Append;
        memD_Endereco.FieldByName('idEndereco').AsInteger := AJSon_Endereco.Objects[I].Integers['idEndereco'];
        memD_Endereco.FieldByName('idEmpresa').AsInteger := AJSon_Endereco.Objects[I].Integers['idEmpresa'];
        memD_Endereco.FieldByName('logradouro').AsString := AJSon_Endereco.Objects[I].Strings['logradouro'];
        memD_Endereco.FieldByName('numero').AsString := AJSon_Endereco.Objects[I].Strings['numero'];
        memD_Endereco.FieldByName('complemento').AsString := AJSon_Endereco.Objects[I].Strings['complemento'];
        memD_Endereco.FieldByName('bairro').AsString := AJSon_Endereco.Objects[I].Strings['bairro'];
        memD_Endereco.FieldByName('municipio').AsString := AJSon_Endereco.Objects[I].Strings['municipio'];
        memD_Endereco.FieldByName('codigoMunicipioIbge').AsString := AJSon_Endereco.Objects[I].Strings['codigoMunicipioIbge'];
        memD_Endereco.FieldByName('uf').AsString := AJSon_Endereco.Objects[I].Strings['uf'];
        memD_Endereco.FieldByName('cep').AsString := AJSon_Endereco.Objects[I].Strings['cep'];
        memD_Endereco.FieldByName('pais').AsString := AJSon_Endereco.Objects[I].Strings['pais'];
        memD_Endereco.FieldByName('codigoPaisIbge').AsString := AJSon_Endereco.Objects[I].Strings['codigoPaisIbge'];
        memD_Endereco.FieldByName('tipoEndereco').AsInteger := AJSon_Endereco.Objects[I].Integers['tipoEndereco'];
        memD_Endereco.FieldByName('tipoEnderecoDesc').AsString := AJSon_Endereco.Objects[I].Strings['tipoEnderecoDesc'];
        memD_Endereco.Post;
      end;

      //Adicionando dados da conta bancária...
      for I := 0 to Pred(AJSon_CBanco.Count) do
      begin
        memD_CBanco.Append;
        memD_CBanco.FieldByName('idBanco').AsInteger := AJSon_CBanco.Objects[I].Integers['idBanco'];
        memD_CBanco.FieldByName('idEmpresa').AsInteger := AJSon_CBanco.Objects[I].Integers['idEmpresa'];
        memD_CBanco.FieldByName('banco').AsString := AJSon_CBanco.Objects[I].Strings['banco'];
        memD_CBanco.FieldByName('agencia').AsString := AJSon_CBanco.Objects[I].Strings['agencia'];
        memD_CBanco.FieldByName('conta').AsString := AJSon_CBanco.Objects[I].Strings['conta'];
        memD_CBanco.FieldByName('tipoConta').AsInteger := AJSon_CBanco.Objects[I].Integers['tipoConta'];
        memD_CBanco.FieldByName('tipoContaDesc').AsString := AJSon_CBanco.Objects[I].Strings['tipoContaDesc'];
        memD_CBanco.Post;
      end;

      //Adicionando dados do certificado...
      memD_Certificado.DisableControls;
      for I := 0 to Pred(AJSon_Certificado.Count) do
      begin
        memD_Certificado.Append;
        memD_Certificado.FieldByName('idCertificado').AsInteger := AJSon_Certificado.Objects[I].Integers['idCertificado'];
        memD_Certificado.FieldByName('idEmpresa').AsInteger := AJSon_Certificado.Objects[I].Integers['idEmpresa'];
        memD_Certificado.FieldByName('tipo').AsString := AJSon_Certificado.Objects[I].Strings['tipo'];
        memD_Certificado.FieldByName('validade').AsDateTime := StrISOToDateTime(AJSon_Certificado.Objects[I].Strings['validade']);
        memD_Certificado.FieldByName('caminhoArquivo').AsString := AJSon_Certificado.Objects[I].Strings['caminhoArquivo'];
        memD_Certificado.FieldByName('senha').AsString := AJSon_Certificado.Objects[I].Strings['senha'];
        memD_Certificado.FieldByName('tipoDesc').AsString := AJSon_Certificado.Objects[I].Strings['tipoDesc'];
        memD_Certificado.Post;
      end;

    except
      on E:Exception do
        raise Exception.Create('Adiciona dados da empresa: ' + sLineBreak + E.Message);
    end;
  finally
    memD_Empresas.EnableControls;
    memD_Endereco.EnableControls;
    memD_CBanco.EnableControls;
    memD_Certificado.EnableControls;
  end;
end;

procedure TfrmEmpresa.Pesquisar;
var
  FResp :IResponse;
  FRet :String;
  FBody :TJSONObject;

  FJSon_Empresa :TJSONArray;
  FJSon_Endereco :TJSONArray;
  FJSon_DBanco :TJSONArray;
  FJSon_Certificado :TJSONArray;

  FTipoPesquisa:String;
  x:Integer;

  FId :Integer;
begin
  try
    try
      Create_DataSet;

      FTipoPesquisa := '';
      case edPesquisar.Tag of
        0:begin
          if not ApenasNumeros(edPesquisar.Text) then
            raise Exception.Create('Para realizar o filtro usando o ID,  não pode haver letras no texto da pesquisa');
          FTipoPesquisa := 'id';
        end;
        1:FTipoPesquisa := 'razaoSocial';
        2:FTipoPesquisa := 'nomeFantasia';
        3:FTipoPesquisa := 'cnpj';
      end;

      if Trim(FHost) = '' then
        raise Exception.Create('Host não informado');

      if Trim(FTipoPesquisa) <> '' then
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .AddParam(FTipoPesquisa,edPesquisar.Text)
                 .Resource('empresa')
                 .Accept('application/json')
                 .Get;
      end
      else
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .Resource('empresa')
                 .Accept('application/json')
                 .Get;
      end;

      FRet := '';
      FRet := FResp.Content;
      FBody := TJSONObject(GetJSON(FRet));
      if FBody['success'].AsBoolean = False then
        raise Exception.Create(FBody['message'].AsString);

      FJSon_Empresa := TJSONArray(GetJSON(FBody['data'].AsJSON));
      FJSon_Endereco := TJSONArray(GetJSON(FBody['endereco'].AsJSON));
      FJSon_DBanco := TJSONArray(GetJSON(FBody['contaBancaria'].AsJSON));
      FJSon_Certificado := TJSONArray(GetJSON(FBody['certificadoDigital'].AsJSON));

      //Inserindo informações...
      Adiciona_Dados(FJSon_Empresa,FJSon_Endereco,FJSon_DBanco,FJSon_Certificado);

    except
      on E: Exception do
      begin
        MessageDlg(E.Message,TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);
        SaveLog(E.Message);
      end;
    end;
  finally
  end;
end;


procedure TfrmEmpresa.OnClick_Delete(const AId: Integer; const ANome:String);
var
  fResp :IResponse;
  fRet :String;
  fBody :TJSONObject;
begin
  try
    if MessageDlg('Deseja excluir a Empresa selecionada?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      fResp := TRequest.New.BaseURL(FHost)
      	       .AddParam('id',memD_Empresas.FieldByName('idEmpresa').AsString)
               .Resource('empresa')
               .Accept('application/json')
               .Delete;

      fRet := '';
      fRet := fResp.Content;
      fBody := TJSONObject(GetJSON(fRet));
      if fBody['success'].AsBoolean = False then
        raise Exception.Create(fBody['message'].AsString)
      else
      begin
        MessageDlg(fBody['message'].AsString,TMsgDlgType.mtInformation,[mbOK],0);
        Pesquisar;
      end;

    end;
  except
    on E :Exception do
    begin
      SaveLog('Excluindo Empresa: ' + E.Message);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmEmpresa.OnClick_Print(const AId: Integer; const ANome:String);
begin
  MessageDlg('Imprimindo: ' + AId.ToString + ' - ' + ANome,TMsgDlgType.mtInformation,[mbOK],0);
end;

procedure TfrmEmpresa.ExportD2Bridge;
begin
  inherited;

  Title := 'Empresas';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  //Formulário de cadastro de empresas....
  FfrmCadEmpresa := TfrmCadEmpresa.Create(Self);
  D2Bridge.AddNested(FfrmCadEmpresa);

  with D2Bridge.Items.add do
  begin
    with Row.Items.Add do
    begin
      with HTMLDIV(CSSClass.Col.colsize10).Items.Add do
      begin
        with Row(CSSClass.Space.margim_bottom3).Items.Add do
        begin
          With FormGroup('',CSSClass.Col.colsize12).Items.Add do
          begin
            LCLObj(edPesquisar);
            LCLObj(btSelPesquisa, PopupMenu, CSSClass.Button.search);
          end;
        end;
      end;

      with HTMLDIV(CSSClass.Col.colsize2).Items.Add do
      begin
        with Row(CSSClass.Space.margim_bottom3 + ' ' + CSSClass.Space.margim_top1).Items.Add do
        begin
          with HTMLDIV(CSSClass.Text.Align.right).Items.Add do
            LCLObj(btNovo, CSSClass.Button.add);
        end;
      end;
    end;

    with Row.Items.Add do
    begin
      with HTMLDIV(CSSClass.Col.colsize12).Items.Add do
      begin
        with Row.Items.Add do
        begin
          with PanelGroup('Listagem','',False,CSSClass.Col.colsize12).Items.Add do
            LCLObj(DBGrid_Empresa);
        end;
      end;
    end;

    with Popup('Popup' + FfrmCadEmpresa.Name,'Cadastro de Empresas',True,CSSClass.Popup.ExtraLarge).Items.Add do
      Nested(FfrmCadEmpresa);
  end;

end;

procedure TfrmEmpresa.OnClick_Edit(const AId: Integer; const ANome:String);
begin
  try
    try
      //Atualizando dados principais...
      FfrmCadEmpresa.Clear_Fields;
      FfrmCadEmpresa.edid_empresa.Text := memD_Empresas.FieldByName('idEmpresa').AsString;
      FfrmCadEmpresa.edcnpj.Text := memD_Empresas.FieldByName('cnpj').AsString;
      FfrmCadEmpresa.edinscricao_estadual.Text := memD_Empresas.FieldByName('inscricaoEstadual').AsString;
      FfrmCadEmpresa.edinscricao_municipal.Text := memD_Empresas.FieldByName('inscricaoMunicipal').AsString;
      FfrmCadEmpresa.cbativo.ItemIndex := memD_Empresas.FieldByName('ativo').AsInteger;
      FfrmCadEmpresa.edrazao_social.Text := memD_Empresas.FieldByName('razaoSocial').AsString;
      FfrmCadEmpresa.ednome_fantasia.Text := memD_Empresas.FieldByName('nomeFantasia').AsString;
      FfrmCadEmpresa.cbregime_tributario.ItemIndex := FfrmCadEmpresa.cbregime_tributario.Items.IndexOf(memD_Empresas.FieldByName('regimeTributario').AsString);
      FfrmCadEmpresa.edcrt.Text := memD_Empresas.FieldByName('crt').AsString;
      FfrmCadEmpresa.edtelefone.Text := memD_Empresas.FieldByName('telefone').AsString;
      FfrmCadEmpresa.edcelular.Text := memD_Empresas.FieldByName('celular').AsString;
      FfrmCadEmpresa.edemail.Text := memD_Empresas.FieldByName('email').AsString;
      FfrmCadEmpresa.edsite.Text := memD_Empresas.FieldByName('site').AsString;

      if not memD_Empresas.IsEmpty then
      begin
        FfrmCadEmpresa.Create_DataSet;

        //Endereço...
        FfrmCadEmpresa.memD_Endereco.DisableControls;
        memD_Endereco.Filter := 'idEmpresa = ' + memD_Empresas.FieldByName('idEmpresa').AsString;
        memD_Endereco.Filtered := True;
        memD_Endereco.First;
	while not memD_Endereco.EOF do
        begin
          //Adicionando dados da empresa...
            FfrmCadEmpresa.memD_Endereco.Append;
            FfrmCadEmpresa.memD_Endereco.FieldByName('idEndereco').AsInteger := memD_Endereco.FieldByName('idEndereco').AsInteger;
            FfrmCadEmpresa.memD_Endereco.FieldByName('idEmpresa').AsInteger := memD_Endereco.FieldByName('idEmpresa').AsInteger;
            FfrmCadEmpresa.memD_Endereco.FieldByName('logradouro').AsString := memD_Endereco.FieldByName('logradouro').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('numero').AsString := memD_Endereco.FieldByName('numero').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('complemento').AsString := memD_Endereco.FieldByName('complemento').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('bairro').AsString := memD_Endereco.FieldByName('bairro').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('municipio').AsString := memD_Endereco.FieldByName('municipio').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('codigoMunicipioIbge').AsString := memD_Endereco.FieldByName('codigoMunicipioIbge').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('uf').AsString := memD_Endereco.FieldByName('uf').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('cep').AsString := memD_Endereco.FieldByName('cep').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('pais').AsString := memD_Endereco.FieldByName('pais').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('codigoPaisIbge').AsString := memD_Endereco.FieldByName('codigoPaisIbge').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('tipoEndereco').AsInteger := memD_Endereco.FieldByName('tipoEndereco').AsInteger;
            FfrmCadEmpresa.memD_Endereco.FieldByName('tipoEnderecoDesc').AsString := memD_Endereco.FieldByName('tipoEnderecoDesc').AsString;
            FfrmCadEmpresa.memD_Endereco.Post;

            memD_Endereco.Next;
        end;
        FfrmCadEmpresa.memD_Endereco.EnableControls;

        //Contas bancárias...
        FfrmCadEmpresa.memD_CBanco.DisableControls;
        memD_CBanco.Filter := 'idEmpresa = ' + memD_Empresas.FieldByName('idEmpresa').AsString;
        memD_CBanco.Filtered := True;
        memD_CBanco.First;
        while not memD_CBanco.EOF do
        begin
          FfrmCadEmpresa.memD_CBanco.Append;
          FfrmCadEmpresa.memD_CBanco.FieldByName('idBanco').AsInteger := memD_CBanco.FieldByName('idBanco').AsInteger;
          FfrmCadEmpresa.memD_CBanco.FieldByName('idEmpresa').AsInteger := memD_CBanco.FieldByName('idEmpresa').AsInteger;
          FfrmCadEmpresa.memD_CBanco.FieldByName('banco').AsString := memD_CBanco.FieldByName('banco').AsString;
          FfrmCadEmpresa.memD_CBanco.FieldByName('agencia').AsString := memD_CBanco.FieldByName('agencia').AsString;
          FfrmCadEmpresa.memD_CBanco.FieldByName('conta').AsString := memD_CBanco.FieldByName('conta').AsString;
          FfrmCadEmpresa.memD_CBanco.FieldByName('tipoConta').AsInteger := memD_CBanco.FieldByName('tipoConta').AsInteger;
          FfrmCadEmpresa.memD_CBanco.FieldByName('tipoContaDesc').AsString := memD_CBanco.FieldByName('tipoContaDesc').AsString;
          FfrmCadEmpresa.memD_CBanco.Post;
          memD_CBanco.Next;
        end;
        FfrmCadEmpresa.memD_CBanco.EnableControls;

        //Certificado digital...
        FfrmCadEmpresa.edid_certificado.Text := memD_Certificado.FieldByName('idCertificado').AsString;
        FfrmCadEmpresa.cbtipo.ItemIndex := memD_Certificado.FieldByName('tipo').AsInteger;
        FfrmCadEmpresa.edvalidade.Date := memD_Certificado.FieldByName('validade').AsDateTime;
        FfrmCadEmpresa.edcaminho_arquivo.Text := memD_Certificado.FieldByName('caminhoArquivo').AsString;
        FfrmCadEmpresa.edsenha.Text := memD_Certificado.FieldByName('senha').AsString;;

      end;
      FfrmCadEmpresa.pcPrincipal.ActivePageIndex := 0;
      ShowPopupModal('Popup' + FfrmCadEmpresa.Name);

      //Informações atualizadas pelos registros da tela de cadastro...
      Pesquisar;

    except
      on E :Exception do
      begin
        SaveLog('Editando empresa. ' + E.Message);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
    memD_Endereco.Filtered := False;
    memD_CBanco.Filtered := False;
  end;
end;

procedure TfrmEmpresa.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
    if PrismControl.VCLComponent = DBGrid_Empresa then
    begin
      with PrismControl.AsDBGrid do
      begin
        PrismControl.AsDBGrid.RecordsPerPage := 10;
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

            //Print
            with Add do
            begin
              ButtonModel:= TButtonModel.Print;
            end;
          end;
        end;
      end;
    end;
  //Change Init Property of Prism Controls
  {
  if PrismControl.VCLComponent = edFiltro then
    PrismControl.AsEdit.DataType:= TPrismFieldType.PrismFieldTypeInteger;

  if PrismControl.IsDBGrid then
  begin
    PrismControl.AsDBGrid.RecordsPerPage:= 10;
    PrismControl.AsDBGrid.MaxRecords:= 2000;
  end;
  }
end;

procedure TfrmEmpresa.RenderD2Bridge(const PrismControl: TPrismControl;
  var HTMLControl: string);
begin
  inherited;

  //Intercept HTML
  {
  if PrismControl.VCLComponent = edFiltro then
  begin
    HTMLControl:= '</>';
  end;
  }
end;

procedure TfrmEmpresa.CellButtonClick(APrismDBGrid: TPrismDBGrid;
  APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer  );
begin
  //inherited CellButtonClick(APrismDBGrid, APrismCellButton, AColIndex, ARow);
  if APrismDBGrid.VCLComponent = DBGrid_Empresa then
  begin
    if APrismCellButton.Identify = TButtonModel.Edit.Identity then
      OnClick_Edit(APrismDBGrid.DataSource.DataSet.FieldByName('idEmpresa').AsInteger,
                   APrismDBGrid.DataSource.DataSet.FieldByName('razaoSocial').AsString);

    if APrismCellButton.Identify = TButtonModel.Delete.Identity then
      OnClick_Delete(APrismDBGrid.DataSource.DataSet.FieldByName('idEmpresa').AsInteger,
                     APrismDBGrid.DataSource.DataSet.FieldByName('razaoSocial').AsString);

    if APrismCellButton.Identify = TButtonModel.Print.Identity then
      OnClick_Print(APrismDBGrid.DataSource.DataSet.FieldByName('idEmpresa').AsInteger,
                    APrismDBGrid.DataSource.DataSet.FieldByName('razaoSocial').AsString);
  end;
end;

end.
