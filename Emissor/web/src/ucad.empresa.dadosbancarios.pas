unit ucad.empresa.DadosBancarios;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ZDataset,
  D2Bridge.Forms, uType_Field_Table, DataSet.Serialize, uBase.Validation,
  uBase.Functions, ubase.functions.objetos, udm;

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
    procedure cbtipo_contaKeyPress(Sender: TObject; var Key: char);
    procedure edagenciaKeyPress(Sender: TObject; var Key: char);
    procedure edbancoKeyPress(Sender: TObject; var Key: char);
    procedure edid_bancoKeyPress(Sender: TObject; var Key: char);
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
var
  fDm :TDM;
  fQuery :TZQuery;
  fId_DadosBanco :Integer;
begin
  try
    try
      fDm := TDM.Create(Self);
      fQuery := fDm.GetQuery;

      fId_DadosBanco := 0;
      if StrToIntDef(edid_banco.Text,0) = 0 then
        fId_DadosBanco := fDm.Sequencial('public.dados_bancarios',Emissor.Empresa_Fields.id_empresa)
      else
        fId_DadosBanco := StrToIntDef(edid_banco.Text,0);

      fQuery.Sql.Add('INSERT INTO public.dados_bancarios ( ');
      fQuery.Sql.Add('  id_banco ');
      fQuery.Sql.Add('  ,id_empresa ');
      fQuery.Sql.Add('  ,banco ');
      fQuery.Sql.Add('  ,agencia ');
      fQuery.Sql.Add('  ,conta ');
      fQuery.Sql.Add('  ,tipo_conta ');
      fQuery.Sql.Add(') VALUES( ');
      fQuery.Sql.Add('  :id_banco ');
      fQuery.Sql.Add('  ,:id_empresa ');
      fQuery.Sql.Add('  ,:banco ');
      fQuery.Sql.Add('  ,:agencia ');
      fQuery.Sql.Add('  ,:conta ');
      fQuery.Sql.Add('  ,:tipo_conta ');
      fQuery.Sql.Add(') ON CONFLICT (id_empresa, id_banco) DO ');
      fQuery.Sql.Add('UPDATE SET ');
      fQuery.Sql.Add('  banco = :banco ');
      fQuery.Sql.Add('  ,agencia = :agencia ');
      fQuery.Sql.Add('  ,conta = :conta ');
      fQuery.Sql.Add('  ,tipo_conta = :tipo_conta; ');
      fQuery.ParamByName('id_banco').AsInteger := fId_DadosBanco;
      fQuery.ParamByName('id_empresa').AsInteger := Emissor.Empresa_Fields.id_empresa;
      fQuery.ParamByName('banco').AsString := edbanco.Text;
      fQuery.ParamByName('agencia').AsString := edagencia.Text;
      fQuery.ParamByName('conta').AsString := edconta.Text;
      fQuery.ParamByName('tipo_conta').AsInteger := cbtipo_conta.ItemIndex;
      fQuery.ExecSQL;

      Close;
    except
      on E:Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'Gravar',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOk],0);
      end;
    end;
  finally
    if Assigned(fQuery) then
      FreeAndNil(fQuery);
    if Assigned(fDm) then
      FreeAndNil(fDm);
  end;
end;

procedure TfrmCad_Empresa_DadosBancarios.cbtipo_contaKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edbanco);
end;

procedure TfrmCad_Empresa_DadosBancarios.edagenciaKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edconta);
end;

procedure TfrmCad_Empresa_DadosBancarios.edbancoKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edagencia);
end;

procedure TfrmCad_Empresa_DadosBancarios.edid_bancoKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.cbtipo_conta);
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
        FormGroup(lbtipo_conta.Caption,CSSClass.Col.colsize11).AddLCLObj(cbtipo_conta,'ValidarCampos_DB',True);
      end;
      with Row.Items.Add do
      begin
        FormGroup(lbbanco.Caption,CSSClass.Col.colsize7).AddLCLObj(edbanco,'ValidarCampos_DB',True);
        FormGroup(lbagencia.Caption,CSSClass.Col.colsize2).AddLCLObj(edagencia,'ValidarCampos_DB',True);
        FormGroup(lbconta.Caption,CSSClass.Col.colsize3).AddLCLObj(edconta,'ValidarCampos_DB',True);
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar,'ValidarCampos_DB',False, CSSClass.Button.save + CSSClass.Col.colsize2);
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
