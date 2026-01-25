unit uCad.Empresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  ComboEx, D2Bridge.Forms;

type

  { TfrmCadEmpresa }

  TfrmCadEmpresa = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    cbativo: TComboBox;
    edcrt: TEdit;
    edemail: TEdit;
    edsite: TEdit;
    edtelefone: TEdit;
    edregime_tributario: TComboBox;
    edinscricao_estadual: TEdit;
    edinscricao_municipal: TEdit;
    edid_empresa: TEdit;
    edcnpj: TEdit;
    edrazao_social: TEdit;
    ednome_fantasia: TEdit;
    lbemail: TLabel;
    lbativo: TLabel;
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
    pnemail: TPanel;
    pnativo: TPanel;
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
    tsEmpresa: TTabSheet;
    tsEndereco: TTabSheet;
    tsDadosBancarios: TTabSheet;
    tsCertificadoDigital: TTabSheet;
    procedure btCancelarClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
  private
    { Private declarations }
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

procedure TfrmCadEmpresa.btConfirmarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCadEmpresa.ExportD2Bridge;
begin
  inherited;

  Title := 'Cadastro de Empresas';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    {Yours Controls}
    with Row.Items.Add do
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

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;
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
