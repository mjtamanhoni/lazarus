unit uformularios.Cad;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ZDataset,
  D2Bridge.Forms, IniFiles, udm, uBase.Functions;

type

  { TfrmFormularios_Cad }

  TfrmFormularios_Cad = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    cbstatus: TComboBox;
    edid_form: TEdit;
    ednome: TEdit;
    lbstatus: TLabel;
    lbdescricao: TLabel;
    lbid_form: TLabel;
    lbnome: TLabel;
    memdescricao: TMemo;
    pnativo: TPanel;
    pnFooter: TPanel;
    pnidUsuario: TPanel;
    pnlogin: TPanel;
    pnRow001: TPanel;
    pnRow002: TPanel;
    procedure btCancelarClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FIniFile :TIniFile;
    FDm :TDM;

    procedure Gravar;
  public
    { Public declarations }
    procedure Clear_Fields;
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; 
      var HTMLControl: string); override;
  end;

function frmFormularios_Cad: TfrmFormularios_Cad;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmFormularios_Cad: TfrmFormularios_Cad;
begin
  result := (TfrmFormularios_Cad.GetInstance as TfrmFormularios_Cad);
end;

procedure TfrmFormularios_Cad.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFormularios_Cad.btConfirmarClick(Sender: TObject);
begin
  try
    Gravar;
    Close;
  except
    on E: Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'btConfirmarClick',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOk],0);
    end;
  end;
end;

procedure TfrmFormularios_Cad.FormCreate(Sender: TObject);
begin
  try
	  FIniFile := TIniFile.Create(ConfigFile);
  	FDm := TDM.Create(Nil);
	except
    on E :Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'FormCreate',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmFormularios_Cad.FormDestroy(Sender: TObject);
begin
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  if Assigned(FDm) then
    FreeAndNil(FDm);
end;

procedure TfrmFormularios_Cad.Gravar;
var
  fQuery :TZQuery;
  fIdForm :Integer;
begin
  try
    try
      fIdForm := 0;

      if Trim(edid_form.Text) = '' then
        fIdForm := FDm.Sequencial('public.formularios', Emissor.Empresa_Fields.id_empresa)
      else
        fIdForm := StrToIntDef(edid_form.Text,0);


      fQuery := FDm.GetQuery;
      fQuery.SQL.Add('INSERT INTO public.formularios( ');
      fQuery.SQL.Add('  id_form ');
      fQuery.SQL.Add('  ,nome ');
      fQuery.SQL.Add('  ,descricao ');
      fQuery.SQL.Add('  ,status ');
      fQuery.SQL.Add(') values ( ');
      fQuery.SQL.Add('  :id_form ');
      fQuery.SQL.Add('  ,:nome ');
      fQuery.SQL.Add('  ,:descricao ');
      fQuery.SQL.Add('  ,:status ');
      fQuery.SQL.Add(') ON CONFLICT (id_form) DO ');
      fQuery.SQL.Add('UPDATE SET ');
      fQuery.SQL.Add('  nome = :nome ');
      fQuery.SQL.Add('  ,descricao = :descricao ');
      fQuery.SQL.Add('  ,status = :status; ');
      fQuery.ParamByName('id_form').AsInteger := fIdForm;
      fQuery.ParamByName('nome').AsString := ednome.Text;
      fQuery.ParamByName('descricao').AsString := memdescricao.Text;
      fQuery.ParamByName('status').AsInteger := cbstatus.ItemIndex;
      fQuery.ExecSQL;
    except
      on E: Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'Gravar',E);
        MessageDlg(E.Message,TMsgDlgType.mtWarning,[mbOK],0);
      end;
    end;
  finally
  end;
end;

procedure TfrmFormularios_Cad.Clear_Fields;
begin
  edid_form.Clear;
  ednome.Clear;
  memdescricao.Lines.Clear;
  cbstatus.ItemIndex := -1;
end;

procedure TfrmFormularios_Cad.ExportD2Bridge;
begin
  inherited;

  Title := 'Cadastro de Formulários';

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
          FormGroup(lbid_form.Caption,CSSClass.Col.colsize2).AddLCLObj(edid_form);
          FormGroup(lbnome.Caption,CSSClass.Col.colsize8).AddLCLObj(ednome,'ValidationForms',True);
          FormGroup(lbstatus.Caption,CSSClass.Col.colsize2).AddLCLObj(cbstatus,'ValidationForms',True);
        end;
        with Row.Items.Add do
          FormGroup(lbdescricao.Caption,CSSClass.Col.colsize12).AddLCLObj(memdescricao);
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar,'ValidationForms',False, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;
  end;

end;

procedure TfrmFormularios_Cad.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TfrmFormularios_Cad.RenderD2Bridge(const PrismControl: TPrismControl;
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
