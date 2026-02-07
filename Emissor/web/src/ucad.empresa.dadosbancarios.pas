unit ucad.empresa.DadosBancarios;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,  
  D2Bridge.Forms,
  uType_Field_Table,
  DataSet.Serialize,
  uBase.Validation, uBase.Functions;

type

  { TfrmCad_Empresa_DadosBancarios }

  TfrmCad_Empresa_DadosBancarios = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    cbtipo_conta: TComboBox;
    edconta: TEdit;
    edid_banco: TEdit;
    edbanco: TEdit;
    edagencia: TEdit;
    lbconta: TLabel;
    lbid_banco: TLabel;
    lbbanco: TLabel;
    lbagencia: TLabel;
    lbtipo_conta: TLabel;
    pnconta: TPanel;
    pnDetail: TPanel;
    pnDBRow1: TPanel;
    pnDBRow2: TPanel;
    pnFooter: TPanel;
    pnid_banco: TPanel;
    pnbanco: TPanel;
    pnagencia: TPanel;
    pntipo_conta: TPanel;
    procedure btCancelarClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Clear_Fields;
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; 
      var HTMLControl: string); override;
  end;

function frmCad_Empresa_DadosBancarios: TfrmCad_Empresa_DadosBancarios;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmCad_Empresa_DadosBancarios: TfrmCad_Empresa_DadosBancarios;
begin
  result := (TfrmCad_Empresa_DadosBancarios.GetInstance as TfrmCad_Empresa_DadosBancarios);
end;

procedure TfrmCad_Empresa_DadosBancarios.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCad_Empresa_DadosBancarios.btConfirmarClick(Sender: TObject);
begin
  try
    with Emissor.EmpCB_Fields do
    begin
      id_banco := StrToIntDef(edid_banco.Text,0);
      id_empresa := 0;
      banco := edbanco.Text;
      agencia := edagencia.Text;
      conta := edbanco.Text;
      tipo_conta := cbtipo_conta.ItemIndex;
      tipo_conta_desc := cbtipo_conta.Text;
    end;
    Close;
  except
    on E:Exception do
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
  end;
end;

procedure TfrmCad_Empresa_DadosBancarios.Clear_Fields;
begin
  edid_banco.Clear;
  cbtipo_conta.ItemIndex := -1;
  edbanco.Clear;
  edagencia.Clear;
  edconta.Clear;
end;

procedure TfrmCad_Empresa_DadosBancarios.ExportD2Bridge;
begin
  inherited;

  Title := 'Dados Bancários da Empresa';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    {Yours Controls}
    with Card.Items.Add do
    begin
      with Row.Items.Add do
      begin
        FormGroup(lbid_banco.Caption,CSSClass.Col.colsize1).AddLCLObj(edid_banco);
        FormGroup(lbtipo_conta.Caption,CSSClass.Col.colsize11).AddLCLObj(cbtipo_conta);
      end;
      with Row.Items.Add do
      begin
        FormGroup(lbbanco.Caption,CSSClass.Col.colsize7).AddLCLObj(edbanco);
        FormGroup(lbagencia.Caption,CSSClass.Col.colsize2).AddLCLObj(edagencia);
        FormGroup(lbconta.Caption,CSSClass.Col.colsize3).AddLCLObj(edconta);
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;
  end;

end;

procedure TfrmCad_Empresa_DadosBancarios.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TfrmCad_Empresa_DadosBancarios.RenderD2Bridge(const PrismControl: TPrismControl;
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
