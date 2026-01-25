unit uCad.Empresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, memds, DB, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ComboEx, DBGrids, EditBtn, DateTimePicker, D2Bridge.Forms,
  uCad.Empresa.Endereco,ucad.empresa.DadosBancarios;

type

  { TfrmCadEmpresa }

  TfrmCadEmpresa = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    btcaminho_arquivo: TButton;
    btEnd_Add: TButton;
    btCB_Add: TButton;
    cbativo: TComboBox;
    cbtipo: TComboBox;
    edvalidade: TDateTimePicker;
    edsenha: TEdit;
    DBGrid_DB: TDBGrid;
    dsEndereco: TDataSource;
    DBGrid_End: TDBGrid;
    dsDadosBancarios: TDataSource;
    edcaminho_arquivo: TEdit;
    edcrt: TEdit;
    edemail: TEdit;
    edid_certificado: TEdit;
    edsite: TEdit;
    edtelefone: TEdit;
    edregime_tributario: TComboBox;
    edinscricao_estadual: TEdit;
    edinscricao_municipal: TEdit;
    edid_empresa: TEdit;
    edcnpj: TEdit;
    edrazao_social: TEdit;
    ednome_fantasia: TEdit;
    lbsenha: TLabel;
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
    mdDadosBancariosdb_agencia: TStringField;
    mdDadosBancariosdb_banco: TStringField;
    mdDadosBancariosdb_conta: TStringField;
    mdDadosBancariosdb_id_banco: TLongintField;
    mdDadosBancariosdb_tipo_conta: TLongintField;
    mdDadosBancariosdb_tipo_conta_desc: TStringField;
    mdEndereco: TMemDataset;
    mdEnderecoend_bairro: TStringField;
    mdEnderecoend_cep: TStringField;
    mdEnderecoend_codigo_municipio_ibge: TStringField;
    mdEnderecoend_codigo_pais_ibge: TStringField;
    mdEnderecoend_complemento: TStringField;
    mdEnderecoend_id_endereco: TLongintField;
    mdEnderecoend_logradouro: TStringField;
    mdEnderecoend_municipio: TStringField;
    mdEnderecoend_numero: TStringField;
    mdEnderecoend_pais: TStringField;
    mdEnderecoend_tipo_endereco: TLongintField;
    mdEnderecoend_tipo_endereco_desc: TStringField;
    mdEnderecoend_uf: TStringField;
    mdDadosBancarios: TMemDataset;
    pnEnd_Footer: TPanel;
    pnCB_Footer: TPanel;
    pnsenha: TPanel;
    pnCDRow3: TPanel;
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
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FfrmCad_Empresa_Endereco :TfrmCad_Empresa_Endereco;
    FfrmCad_Empresa_DadosBancarios :TfrmCad_Empresa_DadosBancarios;

  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; 
      var HTMLControl: string); override;
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
  ShowPopupModal('Popup' + FfrmCad_Empresa_DadosBancarios.Name);
end;

procedure TfrmCadEmpresa.btConfirmarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCadEmpresa.btEnd_AddClick(Sender: TObject);
begin
  ShowPopupModal('Popup' + FfrmCad_Empresa_Endereco.Name);
end;

procedure TfrmCadEmpresa.FormShow(Sender: TObject);
begin
  pcPrincipal.ActivePage := tsEmpresa;
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
                FormGroup(lbregime_tributario.Caption,CSSClass.Col.colsize10).AddLCLObj(edregime_tributario);
                FormGroup(lbcrt.Caption,CSSClass.Col.colsize2).AddLCLObj(edcrt);
              end;

              with Row.Items.Add do
              begin
                FormGroup(lbemail.Caption,CSSClass.Col.colsize9).AddLCLObj(edemail);
                FormGroup(lbtelefone.Caption,CSSClass.Col.colsize3).AddLCLObj(edtelefone);
              end;

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
              begin
                With FormGroup(lbcaminho_arquivo.Caption,CSSClass.Col.colsize12).Items.Add do
                begin
                  LCLObj(edcaminho_arquivo);
                  LCLObj(btcaminho_arquivo, PopupMenu, CSSClass.Button.folderopen);
                end;
              end;
              with Row.Items.Add do
                FormGroup(lbsenha.Caption,CSSClass.Col.colsize6).AddLCLObj(edsenha);
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

end.
