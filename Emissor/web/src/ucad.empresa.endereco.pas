unit uCad.Empresa.Endereco;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,  
  D2Bridge.Forms,
  uType_Field_Table,
  DataSet.Serialize,
  uBase.Validation, uBase.Functions;

type

  { TfrmCad_Empresa_Endereco }

  TfrmCad_Empresa_Endereco = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    btCep: TButton;
    cbtipo_endereco: TComboBox;
    edbairro: TEdit;
    edcep: TEdit;
    edcodigo_municipio_ibge: TEdit;
    edcodigo_pais_ibge: TEdit;
    edcomplemento: TEdit;
    edid_endereco: TEdit;
    edlogradouro: TEdit;
    edmunicipio: TEdit;
    ednumero: TEdit;
    edpais: TEdit;
    eduf: TEdit;
    lbbairro: TLabel;
    lbcep: TLabel;
    lbcodigo_municipio_ibge: TLabel;
    lbcodigo_pais_ibge: TLabel;
    lbcomplemento: TLabel;
    lbid_endereco: TLabel;
    lblogradouro: TLabel;
    lbmunicipio: TLabel;
    lbnumero: TLabel;
    lbpais: TLabel;
    lbtipo_endereco: TLabel;
    lbuf: TLabel;
    pnbairro: TPanel;
    pncep: TPanel;
    pncodigo_municipio_ibge: TPanel;
    pncodigo_pais_ibge: TPanel;
    pncomplemento: TPanel;
    pnDetail: TPanel;
    pnEndRow1: TPanel;
    pnEndRow2: TPanel;
    pnEndRow3: TPanel;
    pnEndRow4: TPanel;
    pnEndRow5: TPanel;
    pnFooter: TPanel;
    pnid_endereco: TPanel;
    pnlogradouro: TPanel;
    pnmunicipio: TPanel;
    pnnumero: TPanel;
    pnpais: TPanel;
    pntipo_endereco: TPanel;
    pnuf: TPanel;
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

function frmCad_Empresa_Endereco: TfrmCad_Empresa_Endereco;

implementation

uses
  EmissorWebApp, uCad.Empresa;

{$R *.lfm}

function frmCad_Empresa_Endereco: TfrmCad_Empresa_Endereco;
begin
  result := (TfrmCad_Empresa_Endereco.GetInstance as TfrmCad_Empresa_Endereco);
end;

procedure TfrmCad_Empresa_Endereco.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCad_Empresa_Endereco.btConfirmarClick(Sender: TObject);
begin
  try
    //Incluir em variávies e depois inserir no mdEndereco
    with Emissor.EmpEnd_Fields do
    begin
      id_endereco := StrToIntDef(edid_endereco.Text,0);
      logradouro := edlogradouro.Text;
      numero := ednumero.Text;
      complemento := edcomplemento.Text;
      bairro := edbairro.Text;
      municipio := edmunicipio.Text;
      codigo_municipio_ibge := edcodigo_municipio_ibge.Text;
      uf := eduf.Text;
      cep := edcep.Text;
      pais := edpais.Text;
      codigo_pais_ibge := edcodigo_pais_ibge.Text;
      tipo_endereco := cbtipo_endereco.ItemIndex;
      tipo_endereco_desc := cbtipo_endereco.Text;
    end;

    Close;
  except
    on E:Exception do
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
  end;
end;

procedure TfrmCad_Empresa_Endereco.Clear_Fields;
begin
  edid_endereco.Clear;
  cbtipo_endereco.ItemIndex := -1;
  edcep.Clear;
  edlogradouro.Clear;
  ednumero.Clear;
  edcomplemento.Clear;
  edbairro.Clear;
  edmunicipio.Clear;
  edcodigo_municipio_ibge.Clear;
  eduf.Clear;
  edpais.Clear;
  edcodigo_pais_ibge.Clear;
end;

procedure TfrmCad_Empresa_Endereco.ExportD2Bridge;
begin
  inherited;

  Title := 'Endereço da Empresa';

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
        FormGroup(lbid_endereco.Caption,CSSClass.Col.colsize1).AddLCLObj(edid_endereco);
        FormGroup(lbtipo_endereco.Caption,CSSClass.Col.colsize8).AddLCLObj(cbtipo_endereco);
        With FormGroup(lbcep.Caption,CSSClass.Col.colsize3).Items.Add do
        begin
          LCLObj(edcep);
          LCLObj(btCep, PopupMenu, CSSClass.Button.search);
        end;
      end;
      with Row.Items.Add do
      begin
        FormGroup(lblogradouro.Caption,CSSClass.Col.colsize10).AddLCLObj(edlogradouro);
        FormGroup(lbnumero.Caption,CSSClass.Col.colsize2).AddLCLObj(ednumero);
      end;
      with Row.Items.Add do
        FormGroup(lbcomplemento.Caption,CSSClass.Col.colsize12).AddLCLObj(edcomplemento);
      with Row.Items.Add do
      begin
        FormGroup(lbbairro.Caption,CSSClass.Col.colsize5).AddLCLObj(edbairro);
        FormGroup(lbmunicipio.Caption,CSSClass.Col.colsize4).AddLCLObj(edmunicipio);
        FormGroup(lbcodigo_municipio_ibge.Caption,CSSClass.Col.colsize2).AddLCLObj(edcodigo_municipio_ibge);
        FormGroup(lbuf.Caption,CSSClass.Col.colsize1).AddLCLObj(eduf);
      end;
      with Row.Items.Add do
      begin
        FormGroup(lbpais.Caption,CSSClass.Col.colsize10).AddLCLObj(edpais);
        FormGroup(lbcodigo_pais_ibge.Caption,CSSClass.Col.colsize2).AddLCLObj(edcodigo_pais_ibge);
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;
  end;

end;

procedure TfrmCad_Empresa_Endereco.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  if PrismControl.VCLComponent = edcep then
    PrismControl.AsEdit.TextMask := TPrismTextMask.BrazilCep;

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

procedure TfrmCad_Empresa_Endereco.RenderD2Bridge(const PrismControl: TPrismControl;
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
