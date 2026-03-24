unit uConfig.ACBr;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  ACBrCEP, ACBrConsultaCNPJ, D2Bridge.Forms, TypInfo, IniFiles,

  uDM.ACBr, uBase.Functions;

type

  { TfrmConfig_ACBr }

  TfrmConfig_ACBr = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    cbbProvedor: TComboBox;
    cbxWS: TComboBox;
    edChaveWS: TEdit;
    edPass: TEdit;
    edProxyHost: TEdit;
    edProxyPass: TEdit;
    edProxyPort: TEdit;
    edProxyUser: TEdit;
    edtHost: TEdit;
    edtPort: TEdit;
    edtSenha: TEdit;
    edtUsuario: TEdit;
    edUser: TEdit;
    gbProx: TGroupBox;
    gbCEP_WebService: TGroupBox;
    gbProx1: TGroupBox;
    lbCNPJProvedor: TLabel;
    lbtHost: TLabel;
    lbSenha: TLabel;
    lbPort: TLabel;
    lbUsuario: TLabel;
    lbWS: TLabel;
    lbUser: TLabel;
    lbPass: TLabel;
    lbProxyHost: TLabel;
    lbProxyPort: TLabel;
    lbProxyUser: TLabel;
    lbProxyPass: TLabel;
    lbChaveWS: TLabel;
    pnCEP_Prox1: TPanel;
    pnCNPJ_Provedor: TPanel;
    pnCNPJ: TPanel;
    pnCEP_Prox: TPanel;
    pnCEP_WebService: TPanel;
    pnCEP: TPanel;
    pcPrincipal: TPageControl;
    pnDetail: TPanel;
    pnFooter: TPanel;
    tsCNPJ: TTabSheet;
    tsTab_CEP: TTabSheet;
    procedure btCancelarClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fDm_ACBr :TDM_Acbr;
    FIniFile :TIniFile;

    procedure LerConfig;
    procedure Provedor_CEP;
    procedure Provedor_CNPJ;
    { Private declarations }
  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; 
      var HTMLControl: string); override;
  end;

function frmConfig_ACBr: TfrmConfig_ACBr;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmConfig_ACBr: TfrmConfig_ACBr;
begin
  result := (TfrmConfig_ACBr.GetInstance as TfrmConfig_ACBr);
end;

procedure TfrmConfig_ACBr.btConfirmarClick(Sender: TObject);
begin
  //Configurações do CEP
  FIniFile.WriteInteger('ACBR.CEP','PROVEDOR',cbxWS.ItemIndex);
  FIniFile.WriteString('ACBR.CEP','CHAVE',edChaveWS.Text);
  FIniFile.WriteString('ACBR.CEP','USUARIO',edUser.Text);
  FIniFile.WriteString('ACBR.CEP','SENHA',edPass.Text);
  FIniFile.WriteString('ACBR.CEP','HOST',edProxyHost.Text);
  FIniFile.WriteString('ACBR.CEP','PORTA',edProxyPort.Text);
  FIniFile.WriteString('ACBR.CEP','USUARIO',edProxyUser.Text);
  FIniFile.WriteString('ACBR.CEP','SENHA',edProxyPass.Text);

  //Configurações do ACBr
  FIniFile.WriteInteger('ACBR.CNPJ','PROVEDOR',cbbProvedor.ItemIndex);
  FIniFile.WriteString('ACBR.CNPJ','CHAVE',edChaveWS.Text);
  FIniFile.WriteString('ACBR.CNPJ','USUARIO',edUser.Text);
  FIniFile.WriteString('ACBR.CNPJ','SENHA',edPass.Text);
  FIniFile.WriteString('ACBR.CNPJ','HOST',edProxyHost.Text);
  FIniFile.WriteString('ACBR.CNPJ','PORTA',edProxyPort.Text);
  FIniFile.WriteString('ACBR.CNPJ','USUARIO',edProxyUser.Text);
  FIniFile.WriteString('ACBR.CNPJ','SENHA',edProxyPass.Text);

  Close;
end;

procedure TfrmConfig_ACBr.FormCreate(Sender: TObject);
begin
  fDm_ACBr := TDM_Acbr.Create(Self);
  FIniFile := TIniFile.Create(ConfigFile);

  Provedor_CEP;
  Provedor_CNPJ;

  LerConfig;
end;

procedure TfrmConfig_ACBr.LerConfig;
begin
  try
    //Configurações do CEP
    cbxWS.ItemIndex := FIniFile.ReadInteger('ACBR.CEP','PROVEDOR',0);
    edChaveWS.Text := FIniFile.ReadString('ACBR.CEP','CHAVE','');
    edUser.Text := FIniFile.ReadString('ACBR.CEP','USUARIO','');
    edPass.Text := FIniFile.ReadString('ACBR.CEP','SENHA','');
    edProxyHost.Text := FIniFile.ReadString('ACBR.CEP','HOST','');
    edProxyPort.Text := FIniFile.ReadString('ACBR.CEP','PORTA','');
    edProxyUser.Text := FIniFile.ReadString('ACBR.CEP','USUARIO','');
    edProxyPass.Text := FIniFile.ReadString('ACBR.CEP','SENHA','');

    //Configurações do ACBr
    cbbProvedor.ItemIndex := FIniFile.ReadInteger('ACBR.CNPJ','PROVEDOR',0);
    edChaveWS.Text := FIniFile.ReadString('ACBR.CNPJ','CHAVE','');
    edUser.Text := FIniFile.ReadString('ACBR.CNPJ','USUARIO','');
    edPass.Text := FIniFile.ReadString('ACBR.CNPJ','SENHA','');
    edProxyHost.Text := FIniFile.ReadString('ACBR.CNPJ','HOST','');
    edProxyPort.Text := FIniFile.ReadString('ACBR.CNPJ','PORTA','');
    edProxyUser.Text := FIniFile.ReadString('ACBR.CNPJ','USUARIO','');
    edProxyPass.Text := FIniFile.ReadString('ACBR.CNPJ','SENHA','');
  except
    On E:Exception do
    begin
      MessageDlg('Lendo configurações' +sLineBreak+ E.Message, TMsgDlgType.mtError, [mbok], 0);
      GravarLogJSON(Self.Name,Self.Caption,'LerConfig',E);
    end;
  end;
end;

procedure TfrmConfig_ACBr.FormDestroy(Sender: TObject);
begin
  if Assigned(fDm_ACBr) then
    FreeAndNil(fDm_ACBr);
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
end;

procedure TfrmConfig_ACBr.Provedor_CEP;
var
  I: TACBrCEPWebService;
begin
  try
    cbxWS.Items.Clear ;
    For I := Low(TACBrCEPWebService) to High(TACBrCEPWebService) do
       cbxWS.Items.Add(GetEnumName(TypeInfo(TACBrCEPWebService), integer(I) ) ) ;

  except
    On E:Exception do
    begin
      MessageDlg('Listando provedores do CEP' +sLineBreak+ E.Message, TMsgDlgType.mtError, [mbok], 0);
      GravarLogJSON(Self.Name,Self.Caption,'Provedor_CEP',E);
    end;
  end;
end;

procedure TfrmConfig_ACBr.Provedor_CNPJ;
var
  Provedor :TACBrCNPJProvedorWS;
begin
  try
    cbbProvedor.Items.Clear;

    for Provedor := Low(TACBrCNPJProvedorWS) to High(TACBrCNPJProvedorWS) do
      cbbProvedor.Items.Add(GetEnumName(TypeInfo(Provedor),Integer(Provedor)));

    cbbProvedor.ItemIndex := 0;

  except
    On E:Exception do
    begin
      MessageDlg('Listando provedores do CNPJ' +sLineBreak+ E.Message, TMsgDlgType.mtError, [mbok], 0);
      GravarLogJSON(Self.Name,Self.Caption,'Provedor_CNPJ',E);
    end;
  end;

end;

procedure TfrmConfig_ACBr.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmConfig_ACBr.ExportD2Bridge;
begin
  inherited;

  Title := 'Configurações do ACBr';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
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
                with PanelGroup('WebService', '', false, CSSClass.Col.colsize12).Items.Add do
                begin
                  with Row.Items.Add do
                  begin
                    FormGroup(lbWS.Caption,CSSClass.Col.colsize3).AddLCLObj(cbxWS);
                    FormGroup(lbChaveWS.Caption,CSSClass.Col.colsize5).AddLCLObj(edChaveWS);
                    FormGroup(lbUser.Caption,CSSClass.Col.colsize2).AddLCLObj(edUser);
                    FormGroup(lbPass.Caption,CSSClass.Col.colsize2).AddLCLObj(edPass);
                  end;
                end;
              end;

              with Row.Items.Add Do
              begin
                with PanelGroup('Proxy', '', false, CSSClass.Col.colsize12).Items.Add do
                begin
                  with Row.Items.Add do
                  begin
                    FormGroup(lbProxyHost.Caption,CSSClass.Col.colsize6).AddLCLObj(edProxyHost);
                    FormGroup(lbProxyPort.Caption,CSSClass.Col.colsize2).AddLCLObj(edProxyPort);
                    FormGroup(lbProxyUser.Caption,CSSClass.Col.colsize2).AddLCLObj(edProxyUser);
                    FormGroup(lbProxyPass.Caption,CSSClass.Col.colsize2).AddLCLObj(edProxyPass);
                  end;
                end;
              end;
            end;
          end;

          with AddTab(pcPrincipal.Pages[1].Caption).Items.Add do
          begin
            with Card.Items.Add do
            begin
              with Row.Items.Add Do
						    FormGroup(lbCNPJProvedor.Caption,CSSClass.Col.colsize12).AddLCLObj(cbbProvedor);

              with Row.Items.Add Do
              begin
                with PanelGroup('Proxy', '', false, CSSClass.Col.colsize12).Items.Add do
                begin
                  with Row.Items.Add do
                  begin
                    FormGroup(lbtHost.Caption,CSSClass.Col.colsize6).AddLCLObj(edtHost);
                    FormGroup(lbPort.Caption,CSSClass.Col.colsize2).AddLCLObj(edtPort);
                    FormGroup(lbUsuario.Caption,CSSClass.Col.colsize2).AddLCLObj(edtUsuario);
                    FormGroup(lbSenha.Caption,CSSClass.Col.colsize2).AddLCLObj(edtSenha);
                  end;
                end;
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

  end;

end;

procedure TfrmConfig_ACBr.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

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

procedure TfrmConfig_ACBr.RenderD2Bridge(const PrismControl: TPrismControl;
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
