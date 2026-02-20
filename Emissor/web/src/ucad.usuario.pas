unit uCad.Usuario;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,  
  D2Bridge.Forms,
  IniFiles, fpjson, DataSet.Serialize, RESTRequest4D, jsonparser,
  uDM.ACBr, uBase.Functions, uBase.DataSets,uCripto_Descrito, Forms;

type

  { TfrmCad_Usuario }

  TfrmCad_Usuario = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    btidPerfil: TButton;
    cbativo: TComboBox;
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
    procedure btCancelarClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
    procedure btidPerfilClick(Sender: TObject);
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
      SaveLog(Self.Caption + ' [' + Self.Name + '] ' + sLineBreak + '    Fechar formulário: ' + E.Message);
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
      SaveLog(Self.Caption + ' [' + Self.Name + '] - Gravar: ' + E.Message);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOk],0);
    end;
  end;
end;

procedure TfrmCad_Usuario.btidPerfilClick(Sender: TObject);
begin
  //
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
        SaveLog(Self.Caption + ' [' + Self.Name + '] - Criando Formulário: ' + E.Message);
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
begin
  try
    try
      fJson_Usuario := TJSONObject.Create;

      if Trim(FHost) = '' then
        raise Exception.Create('Host não informado');

      //Adicionando pares...
      fJson_Usuario.Add('idUsuario',StrToIntDef(edidUsuario.Text,0));
      fJson_Usuario.Add('login',edlogin.Text);
      fJson_Usuario.Add('senha',Criptografar(edsenha.Text));
      fJson_Usuario.Add('nome',ednome.Text);
      fJson_Usuario.Add('email',edemail.Text);
      fJson_Usuario.Add('ativo',cbativo.ItemIndex);
      fJson_Usuario.Add('dataCadastro',Now);
      fJson_Usuario.Add('ultimoAcesso',Now);
      fJson_Usuario.Add('idPerfil',StrToIntDef(edidPerfil.Text,0));
      fJson_Usuario.Add('idEmpresa',Emissor.Usuario_Fields.id_empresa);

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
        raise Exception.Create(fRetorno['message'].AsString);

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

  Title := Self.Caption + ' [' + Self.Name + ']';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    with Row.Items.Add do
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
          FormGroup('',CSSClass.Col.colsize9).AddLCLObj(edidPerfil_Desc);
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
