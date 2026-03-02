unit uCad.Usuario;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  D2Bridge.Forms, IniFiles, fpjson, DataSet.Serialize, RESTRequest4D,
  jsonparser, uDM.ACBr, uBase.Functions, uBase.DataSets, uCripto_Descrito,
  Forms, Menus, ComCtrls, ComboEx, ubase.functions.objetos;

type

  { TfrmCad_Usuario }

  TfrmCad_Usuario = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    btidPerfil: TButton;
    cbativo: TComboBox;
    ComboBox1: TComboBox;
    ComboBoxEx1: TComboBoxEx;
    edemail: TEdit;
    edidPerfil_Desc: TEdit;
    edidUsuario: TEdit;
    edsenha: TEdit;
    edlogin: TEdit;
    ednome: TEdit;
    edidPerfil: TEdit;
    lbativo: TLabel;
    lbemail: TLabel;
    lbidUsuario: TLabel;
    lbsenha: TLabel;
    lblogin: TLabel;
    lbnome: TLabel;
    lbidPerfil: TLabel;
    pnPermissoes: TPanel;
    pcPrincipal: TPageControl;
    pnativo: TPanel;
    pnDetail: TPanel;
    pnemail: TPanel;
    pnFooter: TPanel;
    pnidUsuario: TPanel;
    pnsenha: TPanel;
    pnlogin: TPanel;
    pnRow001: TPanel;
    pnRow002: TPanel;
    pnRow003: TPanel;
    pnRow004: TPanel;
    pnRow005: TPanel;
    pnRow006: TPanel;
    pnnome: TPanel;
    pnidPerfil: TPanel;
    tsCadastro: TTabSheet;
    tsPermissoes: TTabSheet;
    procedure btCancelarClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
    procedure btidPerfilClick(Sender: TObject);
    procedure cbalterarChange(Sender: TObject);
    procedure cbalterar_situacao_negocioChange(Sender: TObject);
    procedure cbalterar_statusChange(Sender: TObject);
    procedure cbanexar_uploadChange(Sender: TObject);
    procedure cbaprovar_rejeitarChange(Sender: TObject);
    procedure cbativoKeyPress(Sender: TObject; var Key: char);
    procedure cbauditar_historicoChange(Sender: TObject);
    procedure cbexcluirChange(Sender: TObject);
    procedure cbexecutar_processosChange(Sender: TObject);
    procedure cbexportarChange(Sender: TObject);
    procedure cbimportarChange(Sender: TObject);
    procedure cbimprimirChange(Sender: TObject);
    procedure cbincluirChange(Sender: TObject);
    procedure cbnotificar_enviarChange(Sender: TObject);
    procedure cbpesquisar_filtrarChange(Sender: TObject);
    procedure cbvisualizarChange(Sender: TObject);
    procedure edemailKeyPress(Sender: TObject; var Key: char);
    procedure edidUsuarioKeyPress(Sender: TObject; var Key: char);
    procedure edloginKeyPress(Sender: TObject; var Key: char);
    procedure ednomeKeyPress(Sender: TObject; var Key: char);
    procedure edsenhaKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FHost :String;
    FIniFile :TIniFile;


    procedure Gravar;
    procedure Return_Fields(const aTipo:Integer);

  public
    { Public declarations }
    procedure Clear_Fields;

  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
  end;

function frmCad_Usuario: TfrmCad_Usuario;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmCad_Usuario: TfrmCad_Usuario;
begin
  result := (TfrmCad_Usuario.GetInstance as TfrmCad_Usuario);
end;

procedure TfrmCad_Usuario.btCancelarClick(Sender: TObject);
begin
  try
    Return_Fields(0);
    Close;
  except
    on E: Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption, 'btCancelarClick', E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOk],0);
    end;
  end;
end;

procedure TfrmCad_Usuario.btConfirmarClick(Sender: TObject);
begin
  try
    Gravar;
    Return_Fields(1);
    Close;
  except
    on E: Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'btConfirmarClick',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOk],0);
    end;
  end;
end;

procedure TfrmCad_Usuario.btidPerfilClick(Sender: TObject);
begin
  //
end;

procedure TfrmCad_Usuario.cbalterarChange(Sender: TObject);
begin
  if cbalterar.Checked then
    cbalterar.Tag := 1
  else
    cbalterar.Tag := 0;
end;

procedure TfrmCad_Usuario.cbalterar_situacao_negocioChange(Sender: TObject);
begin
  if cbalterar_situacao_negocio.Checked then
    cbalterar_situacao_negocio.Tag := 1
  else
    cbalterar_situacao_negocio.Tag := 0;

end;

procedure TfrmCad_Usuario.cbalterar_statusChange(Sender: TObject);
begin
  if cbalterar_status.Checked then
    cbalterar_status.Tag := 1
  else
    cbalterar_status.Tag := 0;

end;

procedure TfrmCad_Usuario.cbanexar_uploadChange(Sender: TObject);
begin
  if cbanexar_upload.Checked then
    cbanexar_upload.Tag := 1
  else
    cbanexar_upload.Tag := 0;

end;

procedure TfrmCad_Usuario.cbaprovar_rejeitarChange(Sender: TObject);
begin
  if cbaprovar_rejeitar.Checked then
    cbaprovar_rejeitar.Tag := 1
  else
    cbaprovar_rejeitar.Tag := 0;

end;

procedure TfrmCad_Usuario.cbativoKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edlogin);
end;

procedure TfrmCad_Usuario.cbauditar_historicoChange(Sender: TObject);
begin
  if cbauditar_historico.Checked then
    cbauditar_historico.Tag := 1
  else
    cbauditar_historico.Tag := 0;

end;

procedure TfrmCad_Usuario.cbexcluirChange(Sender: TObject);
begin
  if cbexcluir.Checked then
    cbexcluir.Tag := 1
  else
    cbexcluir.Tag := 0;
end;

procedure TfrmCad_Usuario.cbexecutar_processosChange(Sender: TObject);
begin
  if cbexecutar_processos.Checked then
    cbexecutar_processos.Tag := 1
  else
    cbexecutar_processos.Tag := 0;

end;

procedure TfrmCad_Usuario.cbexportarChange(Sender: TObject);
begin
  if cbexportar.Checked then
    cbexportar.Tag := 1
  else
    cbexportar.Tag := 0;

end;

procedure TfrmCad_Usuario.cbimportarChange(Sender: TObject);
begin
  if cbimportar.Checked then
    cbimportar.Tag := 1
  else
    cbimportar.Tag := 0;

end;

procedure TfrmCad_Usuario.cbimprimirChange(Sender: TObject);
begin
  if cbimprimir.Checked then
    cbimprimir.Tag := 1
  else
    cbimprimir.Tag := 0;

end;

procedure TfrmCad_Usuario.cbincluirChange(Sender: TObject);
begin
  if cbincluir.Checked then
    cbincluir.Tag := 1
  else
    cbincluir.Tag := 0;
end;

procedure TfrmCad_Usuario.cbnotificar_enviarChange(Sender: TObject);
begin
  if cbnotificar_enviar.Checked then
    cbnotificar_enviar.Tag := 1
  else
    cbnotificar_enviar.Tag := 0;

end;

procedure TfrmCad_Usuario.cbpesquisar_filtrarChange(Sender: TObject);
begin
  if cbpesquisar_filtrar.Checked then
    cbpesquisar_filtrar.Tag := 1
  else
    cbpesquisar_filtrar.Tag := 0;

end;

procedure TfrmCad_Usuario.cbvisualizarChange(Sender: TObject);
begin
  if cbvisualizar.Checked then
    cbvisualizar.Tag := 1
  else
    cbvisualizar.Tag := 0;
end;

procedure TfrmCad_Usuario.edemailKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edidPerfil);
end;

procedure TfrmCad_Usuario.edidUsuarioKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.cbativo);
end;

procedure TfrmCad_Usuario.edloginKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edsenha);
end;

procedure TfrmCad_Usuario.ednomeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edemail);
end;

procedure TfrmCad_Usuario.edsenhaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.ednome);
end;

procedure TfrmCad_Usuario.FormCreate(Sender: TObject);
begin
  try
    try
      FHost := '';
      FIniFile := TIniFile.Create(ConfigFile);
      FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');

      if Trim(FHost) = '' then
        raise Exception.Create('Host de acesso ao servidor não informado.');
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

procedure TfrmCad_Usuario.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FIniFile);
end;

procedure TfrmCad_Usuario.Gravar;
var
  fResp :IResponse;
  fRet :String;
  fRetorno :TJSONObject;
  fJson_Usuario :TJSONObject;
  fJson_Permissao :TJSONObject;
begin
  try
    try
      fJson_Usuario := TJSONObject.Create;
      fJson_Permissao := TJSONObject.Create;

      if Trim(FHost) = '' then
        raise Exception.Create('Host não informado');

      //Adicionando pares do Usuário...
      fJson_Usuario.Add('idEmpresa',Emissor.Usuario_Fields.id_empresa);
      fJson_Usuario.Add('idUsuario',StrToIntDef(edidUsuario.Text,0));
      fJson_Usuario.Add('idPerfil',StrToIntDef(edidPerfil.Text,0));
      fJson_Usuario.Add('login',edlogin.Text);
      fJson_Usuario.Add('senha',Criptografar(edsenha.Text));
      fJson_Usuario.Add('nome',ednome.Text);
      fJson_Usuario.Add('email',edemail.Text);
      fJson_Usuario.Add('ativo',cbativo.ItemIndex);


      if fJson_Usuario.Count = 0 then
        raise Exception.Create('Não há dados a serem salvos');

      if StrToIntDef(edidUsuario.Text,0) = 0 then
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .Resource('usuario')
                 .AddBody(fJson_Usuario)
                 .Accept('application/json')
                 .Post;
      end
      else
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .Resource('usuario')
                 .AddBody(fJson_Usuario)
                 .Accept('application/json')
                 .Put;
      end;

      fRet := '';
      fRet := fResp.Content;
      if Trim(fRet) = '' then
        raise Exception.Create('Não houve retorno do Server');

      fRetorno := TJSONObject(GetJSON(fRet));

      if fRetorno['success'].AsBoolean = False then
        raise Exception.Create('Erro servidor: ' + sLineBreak + fRetorno['message'].AsString);

    except
      on E: Exception do
        raise Exception.Create(E.Message);
    end;
  finally
  end;
end;

procedure TfrmCad_Usuario.Return_Fields(const aTipo: Integer);
begin
  try
    with Emissor.Usuario_Fields do
    begin
      id_usuario := 0;
      login := '';
      senha := '';
      nome := '';
      email := '';
      ativo := 1;
      data_cadastro := 0;
      ultimo_acesso := 0;
      id_perfil := 0;
      id_perfil_desc := '';
    end;

    if aTipo = 1 then
    begin
      with Emissor.Usuario_Fields do
      begin
        id_usuario := StrToIntDef(edidUsuario.Text,0);
        login := edlogin.Text;
        senha := edsenha.Text;
        nome := ednome.Text;
        email := edemail.Text;
        ativo := cbativo.ItemIndex;
        data_cadastro := Now;
        ultimo_acesso := Now;
        id_perfil := StrToIntDef(edidPerfil.Text,1);
        id_perfil_desc := edidPerfil_Desc.Text;
      end;
    end;
  except
    on E:Exception do
      raise Exception.Create('Return_Fields: ' + E.Message);
  end;

end;

procedure TfrmCad_Usuario.ExportD2Bridge;
begin
  inherited;

  Title := Self.Caption;

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
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
              FormGroup(lbidUsuario.Caption,CSSClass.Col.colsize2).AddLCLObj(edidUsuario);
              FormGroup(lbativo.Caption,CSSClass.Col.colsize2).AddLCLObj(cbativo,'ValidationGravar',True);
            end;
            with Row.Items.Add do
              FormGroup(lblogin.Caption,CSSClass.Col.colsize12).AddLCLObj(edlogin,'ValidationGravar',True);
            with Row.Items.Add do
              FormGroup(lbsenha.Caption,CSSClass.Col.colsize12).AddLCLObj(edsenha,'ValidationGravar',True);
            with Row.Items.Add do
              FormGroup(lbnome.Caption,CSSClass.Col.colsize12).AddLCLObj(ednome,'ValidationGravar',True);
            with Row.Items.Add do
              FormGroup(lbemail.Caption,CSSClass.Col.colsize12).AddLCLObj(edemail,'ValidationGravar',True);
            with Row.Items.Add do
            begin
              With FormGroup(lbidPerfil.Caption,CSSClass.Col.colsize3).Items.Add do
              begin
                LCLObj(edidPerfil,'ValidationGravar',True);
                LCLObj(btidPerfil,CSSClass.Button.search);
              end;
              FormGroup('',CSSClass.Col.colsize6).AddLCLObj(edidPerfil_Desc);
              FormGroup('',CSSClass.Col.colsize3).AddLCLObj(ComboBox1);
            end;
          end;
        end;

        with AddTab(pcPrincipal.Pages[1].Caption).Items.Add do
        begin
          with Card.Items.Add do
          begin
            with Row.Items.Add do
              LCLObj(cbvisualizar);
            with Row.Items.Add do
              LCLObj(cbincluir);
            with Row.Items.Add do
              LCLObj(cbalterar);
            with Row.Items.Add do
              LCLObj(cbexcluir);
            with Row.Items.Add do
              LCLObj(cbimprimir);
            with Row.Items.Add do
              LCLObj(cbexportar);
            with Row.Items.Add do
              LCLObj(cbimportar);
            with Row.Items.Add do
              LCLObj(cbaprovar_rejeitar);
            with Row.Items.Add do
              LCLObj(cbanexar_upload);
            with Row.Items.Add do
              LCLObj(cbpesquisar_filtrar);
            with Row.Items.Add do
              LCLObj(cbnotificar_enviar);
            with Row.Items.Add do
              LCLObj(cbauditar_historico);
            with Row.Items.Add do
              LCLObj(cbexecutar_processos);
            with Row.Items.Add do
              LCLObj(cbalterar_status);
            with Row.Items.Add do
              LCLObj(cbalterar_situacao_negocio);
          end;
        end;
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar,'ValidationGravar',False, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;
  end;

end;

procedure TfrmCad_Usuario.Clear_Fields;
begin
  edidUsuario.Clear;
  cbativo.ItemIndex := -1;
  edlogin.Clear;
  edsenha.Clear;
  ednome.Clear;
  edemail.Clear;
  edidPerfil.Clear;
  edidPerfil_Desc.Clear;
end;

procedure TfrmCad_Usuario.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  if PrismControl.VCLComponent = edemail then
    PrismControl.AsEdit.TextMask:= TPrismTextMask.Email;

  //Change Init Property of Prism Controls
  {
  if PrismControl.VCLComponent = Edit1 then
    PrismControl.AsEdit.DataType:= TPrismFieldType.PrismFieldTypeInteger;

  if PrismControl.IsDBGrid then
  begin
    PrismControl.AsDBGrid.RecordsPerPage:= 10;
    PrismControl.AsDBGrid.MaxRecords:= 2000;
  end;
  }
end;

procedure TfrmCad_Usuario.RenderD2Bridge(const PrismControl: TPrismControl;
  var HTMLControl: string);
begin
  inherited;

  //Intercept HTML
  {
  if PrismControl.VCLComponent = Edit1 then
  begin
    HTMLControl:= '</>';
  end;
  }
end;

end.
