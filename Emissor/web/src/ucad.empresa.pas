unit uCad.Empresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, memds, DB, BufDataset, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ComboEx, DBGrids, EditBtn, DateTimePicker,
  D2Bridge.Forms, ACBrValidador, IniFiles, fpjson, DataSet.Serialize,
  RESTRequest4D, jsonparser, uCad.Empresa.Endereco, ucad.empresa.DadosBancarios,
  uDM.ACBr, uBase.Functions, uBase.DataSets;

type

  { TfrmCadEmpresa }

  TfrmCadEmpresa = class(TD2BridgeForm)
    btCancelar: TButton;
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
    procedure btCancelarClick(Sender: TObject);
    procedure btCB_AddClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
    procedure btEnd_AddClick(Sender: TObject);
    procedure cbregime_tributarioChange(Sender: TObject);
    procedure edcnpjExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FfrmCad_Empresa_Endereco :TfrmCad_Empresa_Endereco;
    FfrmCad_Empresa_DadosBancarios :TfrmCad_Empresa_DadosBancarios;
    fDM_ACBr :TDM_Acbr;

    FHost :String;
    Fid_endereco: Integer;
    FIniFile :TIniFile;
    Flogradouro: String;

    procedure OnClick_Edit_End;
    procedure OnClick_Delete_End(const AId_Endereco: Integer);
    procedure OnClick_Edit_CBanco;
    procedure OnClick_Delete_CBanco(const AId_Endereco: Integer);


    procedure Clear_Parans(const AClear:Integer);
    procedure Gravar;
    procedure Retorna_DBanco;
    procedure Retorna_Endereco;

  public
    { Public declarations }
    memD_Endereco :TBufDataset;
    memD_CBanco :TBufDataset;

    property id_endereco :Integer read Fid_endereco write Fid_endereco;
    property logradouro :String read Flogradouro write Flogradouro;

    procedure Create_DataSet;
    procedure Clear_Fields;

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

procedure TfrmCadEmpresa.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCadEmpresa.btCB_AddClick(Sender: TObject);
begin
  try
    try
      FfrmCad_Empresa_DadosBancarios.Clear_Fields;
      ShowPopupModal('Popup' + FfrmCad_Empresa_DadosBancarios.Name);
      Retorna_DBanco;
    except
      on E:Exception do
      begin
        SaveLog(E.Message);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
    Clear_Parans(1);
    memD_CBanco.EnableControls;
  end;
end;

procedure TfrmCadEmpresa.btConfirmarClick(Sender: TObject);
begin
  Gravar;
  Close;
end;

procedure TfrmCadEmpresa.Gravar;
var
  fResp :IResponse;
  fRet :String;
  fRetorno :TJSONObject;

  fEmpresa :TJSONObject;
  fCertificado :TJSONObject;
begin
  try
    try
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

        //SaveLog(fEmpresa.AsJSON);

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

      Close;
    except
      on E: Exception do
      begin
        SaveLog('Gravar: ' + E.Message);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOk],0);
      end;
    end;
  finally
  end;
end;

procedure TfrmCadEmpresa.Create_DataSet;
var
  FEmpresa :TEmpresa;
begin
  FEmpresa := TEmpresa.Create;
  try
    try
      //Criando bufdataset - Endereço
      FEmpresa.Criar_DataSet_Endereco(memD_Endereco);
      dsEndereco.DataSet := memD_Endereco;
      DBGrid_End.DataSource := dsEndereco;
      ConfigColGridAut(DBGrid_End,memD_Endereco);

      //Criando bufdataset - Conta bancária
      FEmpresa.Criar_DataSet_CBanco(memD_CBanco);
      dsConta.DataSet := memD_CBanco;
      DBGrid_DB.DataSource := dsConta;
      ConfigColGridAut(DBGrid_DB,memD_CBanco);

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

procedure TfrmCadEmpresa.btEnd_AddClick(Sender: TObject);
begin
  try
    try
      FfrmCad_Empresa_Endereco.Clear_Fields;
      ShowPopupModal('Popup' + FfrmCad_Empresa_Endereco.Name);
      Retorna_Endereco;
    except
      on E:Exception do
      begin
        SaveLog(E.Message);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
    Clear_Parans(0);
  end;
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
      MessageDlg(E.Message,TMsgDlgType.mtWarning,[mbOK],0);
  end;
end;

procedure TfrmCadEmpresa.FormCreate(Sender: TObject);
begin
  try
    try
      FHost := '';
      FIniFile := TIniFile.Create(ConfigFile);
      FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');

      memD_Endereco := TBufDataset.Create(Self);
      memD_CBanco := TBufDataset.Create(Self);

      Create_DataSet;

      if Trim(FHost) = '' then
        raise Exception.Create('Host de acesso ao servidor não informado.');
    except
      on E :Exception do
      	 MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  finally
  end;
end;

procedure TfrmCadEmpresa.FormDestroy(Sender: TObject);
begin
  FreeAndNil(memD_Endereco);
  FreeAndNil(memD_CBanco);
  FreeAndNil(FIniFile);
end;

procedure TfrmCadEmpresa.OnClick_Delete_End(const AId_Endereco: Integer);
var
  fResp :IResponse;
  fRet :String;
  fBody :TJSONObject;
begin
  try
    if MessageDlg('Deseja excluir o Endereço selecionado?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
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

procedure TfrmCadEmpresa.OnClick_Edit_CBanco;
begin
  try
    FfrmCad_Empresa_DadosBancarios.edid_banco.Text := IntToStr(memD_CBanco.FieldByName('idbanco').AsInteger);
    FfrmCad_Empresa_DadosBancarios.cbtipo_conta.ItemIndex := memD_CBanco.FieldByName('tipoconta').AsInteger;
    FfrmCad_Empresa_DadosBancarios.edbanco.Text := memD_CBanco.FieldByName('banco').AsString;
    FfrmCad_Empresa_DadosBancarios.edagencia.Text := memD_CBanco.FieldByName('agencia').AsString;
    FfrmCad_Empresa_DadosBancarios.edconta.Text := memD_CBanco.FieldByName('conta').AsString;

    ShowPopupModal('Popup' + FfrmCad_Empresa_DadosBancarios.Name);
    memD_CBanco.Delete;
    Retorna_DBanco;
  except
    on E:Exception do
    begin
      SaveLog('Editando dados Bancários: ' + E.Message);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;

end;

procedure TfrmCadEmpresa.OnClick_Delete_CBanco(const AId_Endereco: Integer);
begin

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

  //Endereço - Dados Bancários...
  Create_DataSet;

  //Certificado digital...
  edid_certificado.Clear;
  cbtipo.ItemIndex := -1;
  edvalidade.Date := Date;
  edcaminho_arquivo.Clear;
  edsenha.Clear;
end;

procedure TfrmCadEmpresa.ExportD2Bridge;
begin
  inherited;

  Title := 'Cadastro de Empresas';

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
                FormGroup(lbcnpj.Caption,CSSClass.Col.colsize3).AddLCLObj(edcnpj);
                FormGroup(lbinscricao_estadual.Caption,CSSClass.Col.colsize3).AddLCLObj(edinscricao_estadual);
                FormGroup(lbinscricao_municipal.Caption,CSSClass.Col.colsize3).AddLCLObj(edinscricao_municipal);
                FormGroup(lbativo.Caption,CSSClass.Col.colsize2).AddLCLObj(cbativo);
              end;

              with Row.Items.Add do
                FormGroup(lbrazao_social.Caption,CSSClass.Col.colsize12).AddLCLObj(edrazao_social);

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
      VCLObj(btConfirmar, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
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
    if APrismCellButton.Identify = TButtonModel.Delete.Identity then OnClick_Delete_End(memD_Endereco.FieldByName('idendereco').AsInteger);
  end;

  if APrismDBGrid.VCLComponent = DBGrid_DB then
  begin
    if APrismCellButton.Identify = TButtonModel.Edit.Identity then OnClick_Edit_CBanco;
    if APrismCellButton.Identify = TButtonModel.Delete.Identity then OnClick_Delete_CBanco(memD_CBanco.FieldByName('idbanco').AsInteger);
  end;

end;

procedure TfrmCadEmpresa.OnClick_Edit_End;
begin
  try
    FfrmCad_Empresa_Endereco.edid_endereco.Text := IntToStr(memD_Endereco.FieldByName('idendereco').AsInteger);
    FfrmCad_Empresa_Endereco.cbtipo_endereco.ItemIndex := memD_Endereco.FieldByName('tipoendereco').AsInteger;
    FfrmCad_Empresa_Endereco.edcep.Text := memD_Endereco.FieldByName('cep').AsString;
    FfrmCad_Empresa_Endereco.edlogradouro.Text := memD_Endereco.FieldByName('logradouro').AsString;
    FfrmCad_Empresa_Endereco.ednumero.Text := memD_Endereco.FieldByName('numero').AsString;
    FfrmCad_Empresa_Endereco.edcomplemento.Text := memD_Endereco.FieldByName('complemento').AsString;
    FfrmCad_Empresa_Endereco.edbairro.Text := memD_Endereco.FieldByName('bairro').AsString;
    FfrmCad_Empresa_Endereco.edmunicipio.Text := memD_Endereco.FieldByName('municipio').AsString;
    FfrmCad_Empresa_Endereco.edcodigo_municipio_ibge.Text := memD_Endereco.FieldByName('codigomunicipioibge').AsString;
    FfrmCad_Empresa_Endereco.eduf.Text := memD_Endereco.FieldByName('uf').AsString;
    FfrmCad_Empresa_Endereco.edpais.Text := memD_Endereco.FieldByName('pais').AsString;
    FfrmCad_Empresa_Endereco.edcodigo_pais_ibge.Text := memD_Endereco.FieldByName('codigopaisibge').AsString;

    ShowPopupModal('Popup' + FfrmCad_Empresa_Endereco.Name);
    memD_Endereco.Delete;
    Retorna_Endereco;

  except
    on E:Exception do
    begin
      SaveLog('Editando Endereço: ' + E.Message);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmCadEmpresa.Retorna_Endereco;
begin
  try
    memD_Endereco.DisableControls;
    with Emissor.EmpEnd_Fields do
    begin
      if Trim(logradouro) <> '' then
      begin
        memD_Endereco.Append;
        memD_Endereco.FieldByName('idEndereco').AsInteger := id_endereco;
        memD_Endereco.FieldByName('idEmpresa').AsInteger := StrToIntDef(edid_empresa.Text,0);
        memD_Endereco.FieldByName('logradouro').AsString := logradouro;
        memD_Endereco.FieldByName('numero').AsString := numero;
        memD_Endereco.FieldByName('complemento').AsString := complemento;
        memD_Endereco.FieldByName('bairro').AsString := bairro;
        memD_Endereco.FieldByName('municipio').AsString := municipio;
        memD_Endereco.FieldByName('codigoMunicipioIbge').AsString := codigo_municipio_ibge;
        memD_Endereco.FieldByName('uf').AsString := uf;
        memD_Endereco.FieldByName('cep').AsString := cep;
        memD_Endereco.FieldByName('pais').AsString := pais;
        memD_Endereco.FieldByName('codigoPaisIbge').AsString := codigo_pais_ibge;
        memD_Endereco.FieldByName('tipoEndereco').AsInteger := tipo_endereco;
        memD_Endereco.FieldByName('tipoEnderecoDesc').AsString := tipo_endereco_desc;
        memD_Endereco.Post;
      end;
    end;
    memD_Endereco.EnableControls;

  except
    on E:Exception do
      raise Exception.Create('Retorna Endereço: ' + E.Message);
  end;
end;

procedure TfrmCadEmpresa.Retorna_DBanco;
begin
  try
    memD_CBanco.DisableControls;
    with Emissor.EmpCB_Fields do
    begin
      if Trim(conta) <> '' then
      begin
        memD_CBanco.Append;
        memD_CBanco.FieldByName('idBanco').AsInteger := id_banco;
        memD_CBanco.FieldByName('idEmpresa').AsInteger := StrToIntDef(edid_empresa.Text,0);
        memD_CBanco.FieldByName('banco').AsString := banco;
        memD_CBanco.FieldByName('agencia').AsString := agencia;
        memD_CBanco.FieldByName('conta').AsString := conta;
        memD_CBanco.FieldByName('tipoConta').AsInteger := tipo_conta;
        memD_CBanco.FieldByName('tipoContaDesc').AsString := tipo_conta_desc;
        memD_CBanco.Post;
      end;
    end;

    memD_CBanco.EnableControls;
  except
    on E:Exception do
      raise Exception.Create('Retorna dados Bancários: ' + E.Message);
  end;
end;

end.
