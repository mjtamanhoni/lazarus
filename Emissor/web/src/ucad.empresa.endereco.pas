unit uCad.Empresa.Endereco;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ZDataset,
  D2Bridge.Forms, uType_Field_Table, DataSet.Serialize, uBase.Validation,
  uBase.Functions, ubase.functions.objetos, udm, Forms;

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
    procedure cbtipo_enderecoKeyPress(Sender: TObject; var Key: char);
    procedure edbairroKeyPress(Sender: TObject; var Key: char);
    procedure edcepKeyPress(Sender: TObject; var Key: char);
    procedure edcodigo_municipio_ibgeKeyPress(Sender: TObject; var Key: char);
    procedure edcomplementoKeyPress(Sender: TObject; var Key: char);
    procedure edid_enderecoKeyPress(Sender: TObject; var Key: char);
    procedure edlogradouroKeyPress(Sender: TObject; var Key: char);
    procedure edmunicipioKeyPress(Sender: TObject; var Key: char);
    procedure ednumeroKeyPress(Sender: TObject; var Key: char);
    procedure edpaisKeyPress(Sender: TObject; var Key: char);
    procedure edufKeyPress(Sender: TObject; var Key: char);
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
var
  fQuery :TZQuery;
  fId_Endereco :Integer;
  fDm :TDM;

begin
  try
    try
      fDm := TDM.Create(Self);

      //Selecionando sequencial...
      if StrToIntDef(edid_endereco.Text,0) = 0 then
        fId_Endereco := fDm.Sequencial('public.endereco_empresa',Emissor.Empresa_Fields.id_empresa,0)
      else
        fId_Endereco := StrToIntDef(edid_endereco.Text,0);

      //Incluir em variávies e depois inserir no mdEndereco
      fQuery := fDm.GetQuery;
      fQuery.SQL.Add('INSERT INTO public.endereco_empresa ( ');
      fQuery.SQL.Add('	id_endereco ');
      fQuery.SQL.Add('  ,id_empresa ');
      fQuery.SQL.Add('  ,logradouro ');
      fQuery.SQL.Add('  ,numero ');
      fQuery.SQL.Add('  ,complemento ');
      fQuery.SQL.Add('  ,bairro ');
      fQuery.SQL.Add('  ,municipio ');
      fQuery.SQL.Add('  ,codigo_municipio_ibge ');
      fQuery.SQL.Add('  ,uf ');
      fQuery.SQL.Add('  ,cep ');
      fQuery.SQL.Add('  ,pais ');
      fQuery.SQL.Add('  ,codigo_pais_ibge ');
      fQuery.SQL.Add('  ,tipo_endereco ');
      fQuery.SQL.Add(') VALUES ( ');
      fQuery.SQL.Add('	:id_endereco ');
      fQuery.SQL.Add('  ,:id_empresa ');
      fQuery.SQL.Add('  ,:logradouro ');
      fQuery.SQL.Add('  ,:numero ');
      fQuery.SQL.Add('  ,:complemento ');
      fQuery.SQL.Add('  ,:bairro ');
      fQuery.SQL.Add('  ,:municipio ');
      fQuery.SQL.Add('  ,:codigo_municipio_ibge ');
      fQuery.SQL.Add('  ,:uf ');
      fQuery.SQL.Add('  ,:cep ');
      fQuery.SQL.Add('  ,:pais ');
      fQuery.SQL.Add('  ,:codigo_pais_ibge ');
      fQuery.SQL.Add('  ,:tipo_endereco ');
      fQuery.SQL.Add(') ON CONFLICT (id_endereco,id_empresa) DO ');
      fQuery.SQL.Add('UPDATE SET ');
      fQuery.SQL.Add('  logradouro = :logradouro ');
      fQuery.SQL.Add('  ,numero = :numero ');
      fQuery.SQL.Add('  ,complemento = :complemento ');
      fQuery.SQL.Add('  ,bairro = :bairro ');
      fQuery.SQL.Add('  ,municipio = :municipio ');
      fQuery.SQL.Add('  ,codigo_municipio_ibge = :codigo_municipio_ibge ');
      fQuery.SQL.Add('  ,uf = :uf ');
      fQuery.SQL.Add('  ,cep = :cep ');
      fQuery.SQL.Add('  ,pais = :pais ');
      fQuery.SQL.Add('  ,codigo_pais_ibge = :codigo_pais_ibge ');
      fQuery.SQL.Add('  ,tipo_endereco = :tipo_endereco; ');
      fQuery.ParamByName('id_endereco').AsInteger := fId_Endereco;
      fQuery.ParamByName('id_empresa').AsInteger := Emissor.Empresa_Fields.id_empresa;
      fQuery.ParamByName('logradouro').AsString := edlogradouro.Text;
      if Trim(ednumero.Text) = '' then
        fQuery.ParamByName('numero').Clear
      else
        fQuery.ParamByName('numero').AsString := ednumero.Text;
      if Trim(edcomplemento.Text) = '' then
        fQuery.ParamByName('complemento').Clear
      else
        fQuery.ParamByName('complemento').AsString := edcomplemento.Text;
      if Trim(edbairro.Text) = '' then
        fQuery.ParamByName('bairro').Clear
      else
        fQuery.ParamByName('bairro').AsString := edbairro.Text;
      fQuery.ParamByName('municipio').AsString := edmunicipio.Text;
      if Trim(edcodigo_municipio_ibge.Text) = '' then
        fQuery.ParamByName('codigo_municipio_ibge').Clear
      else
        fQuery.ParamByName('codigo_municipio_ibge').AsString := edcodigo_municipio_ibge.Text;
      fQuery.ParamByName('uf').AsString := eduf.Text;
      fQuery.ParamByName('cep').AsString := RemoverMascara(edcep.Text);
      if Trim(edpais.Text) = '' then
        fQuery.ParamByName('pais').Clear
      else
        fQuery.ParamByName('pais').AsString := edpais.Text;
      if Trim(edcodigo_pais_ibge.Text) = '' then
        fQuery.ParamByName('codigo_pais_ibge').Clear
      else
        fQuery.ParamByName('codigo_pais_ibge').AsString := edcodigo_pais_ibge.Text;
      fQuery.ParamByName('tipo_endereco').AsInteger := cbtipo_endereco.ItemIndex;
      fQuery.ExecSQL;

      {
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
        cep := RemoverMascara(edcep.Text);
        pais := edpais.Text;
        codigo_pais_ibge := edcodigo_pais_ibge.Text;
        tipo_endereco := cbtipo_endereco.ItemIndex;
        tipo_endereco_desc := cbtipo_endereco.Text;
      end;
      }
      Close;
    except
      on E:Exception do
      begin
        MessageDlg(E.Message, TMsgDlgType.mtError, [mbok], 0);
        GravarLogJSON(Self.Name,Self.Caption,'btConfirmarClick',E);
      end;
    end;
  finally
    if Assigned(fDm) then
      FreeAndNil(fDm);
    if Assigned(fQuery) then
      FreeAndNil(fQuery);
  end;
end;

procedure TfrmCad_Empresa_Endereco.cbtipo_enderecoKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edcep);
end;

procedure TfrmCad_Empresa_Endereco.edbairroKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edmunicipio);
end;

procedure TfrmCad_Empresa_Endereco.edcepKeyPress(Sender: TObject; var Key: char
  );
begin
  if Key = #13 then EnterAsTab(Self.edlogradouro);
end;

procedure TfrmCad_Empresa_Endereco.edcodigo_municipio_ibgeKeyPress(
  Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.eduf);
end;

procedure TfrmCad_Empresa_Endereco.edcomplementoKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edbairro);
end;

procedure TfrmCad_Empresa_Endereco.edid_enderecoKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.cbtipo_endereco);
end;

procedure TfrmCad_Empresa_Endereco.edlogradouroKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.ednumero);
end;

procedure TfrmCad_Empresa_Endereco.edmunicipioKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edcodigo_municipio_ibge);
end;

procedure TfrmCad_Empresa_Endereco.ednumeroKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edcomplemento);
end;

procedure TfrmCad_Empresa_Endereco.edpaisKeyPress(Sender: TObject; var Key: char
  );
begin
  if Key = #13 then EnterAsTab(Self.edcodigo_pais_ibge);
end;

procedure TfrmCad_Empresa_Endereco.edufKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then EnterAsTab(Self.edpais);
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
          LCLObj(edcep,'ValidarCampos_EmpEnd',True);
          LCLObj(btCep, PopupMenu, CSSClass.Button.search);
        end;
      end;
      with Row.Items.Add do
      begin
        FormGroup(lblogradouro.Caption,CSSClass.Col.colsize10).AddLCLObj(edlogradouro,'ValidarCampos_EmpEnd',True);
        FormGroup(lbnumero.Caption,CSSClass.Col.colsize2).AddLCLObj(ednumero);
      end;
      with Row.Items.Add do
        FormGroup(lbcomplemento.Caption,CSSClass.Col.colsize12).AddLCLObj(edcomplemento);
      with Row.Items.Add do
      begin
        FormGroup(lbbairro.Caption,CSSClass.Col.colsize5).AddLCLObj(edbairro);
        FormGroup(lbmunicipio.Caption,CSSClass.Col.colsize4).AddLCLObj(edmunicipio,'ValidarCampos_EmpEnd',True);
        FormGroup(lbcodigo_municipio_ibge.Caption,CSSClass.Col.colsize2).AddLCLObj(edcodigo_municipio_ibge);
        FormGroup(lbuf.Caption,CSSClass.Col.colsize1).AddLCLObj(eduf,'ValidarCampos_EmpEnd',True);
      end;
      with Row.Items.Add do
      begin
        FormGroup(lbpais.Caption,CSSClass.Col.colsize10).AddLCLObj(edpais);
        FormGroup(lbcodigo_pais_ibge.Caption,CSSClass.Col.colsize2).AddLCLObj(edcodigo_pais_ibge);
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar,'ValidarCampos_EmpEnd',False, CSSClass.Button.save + CSSClass.Col.colsize2);
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
