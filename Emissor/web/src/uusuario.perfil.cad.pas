unit uusuario.perfil.Cad;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ZDataset,
  D2Bridge.Forms, IniFiles, udm, uBase.Functions;

type

  { TfrmUsuarioPerfil_Cad }

  TfrmUsuarioPerfil_Cad = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    edid_perfil: TEdit;
    ednome_perfil: TEdit;
    lbid_perfil: TLabel;
    lbnome_perfil: TLabel;
    lbdescricao: TLabel;
    memdescricao: TMemo;
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

function frmUsuarioPerfil_Cad: TfrmUsuarioPerfil_Cad;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmUsuarioPerfil_Cad: TfrmUsuarioPerfil_Cad;
begin
  result := (TfrmUsuarioPerfil_Cad.GetInstance as TfrmUsuarioPerfil_Cad);
end;

procedure TfrmUsuarioPerfil_Cad.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmUsuarioPerfil_Cad.btConfirmarClick(Sender: TObject);
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

procedure TfrmUsuarioPerfil_Cad.FormCreate(Sender: TObject);
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

procedure TfrmUsuarioPerfil_Cad.FormDestroy(Sender: TObject);
begin
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  if Assigned(FDm) then
    FreeAndNil(FDm);
end;

procedure TfrmUsuarioPerfil_Cad.Gravar;
var
  fQuery :TZQuery;
  fIdPerfil :Integer;
begin
  try
    try
      fIdPerfil := 0;

      if Trim(edid_perfil.Text) = '' then
        fIdPerfil := FDm.Sequencial('public.perfis')
      else
        fIdPerfil := StrToIntDef(edid_perfil.Text,0);


      fQuery := FDm.GetQuery;
      fQuery.SQL.Add('INSERT INTO public.perfis ( ');
      fQuery.SQL.Add('  id_perfil ');
      fQuery.SQL.Add('  ,nome_perfil ');
      fQuery.SQL.Add('  ,descricao ');
      fQuery.SQL.Add(') VALUES ( ');
      fQuery.SQL.Add('  :id_perfil ');
      fQuery.SQL.Add('  ,:nome_perfil ');
      fQuery.SQL.Add('  ,:descricao ');
      fQuery.SQL.Add(') ON CONFLICT (id_perfil) DO ');
      fQuery.SQL.Add('UPDATE SET ');
      fQuery.SQL.Add('  nome_perfil = :nome_perfil ');
      fQuery.SQL.Add('  ,descricao = :descricao; ');
      fQuery.ParamByName('id_perfil').AsInteger := fIdPerfil;
      fQuery.ParamByName('nome_perfil').AsString := ednome_perfil.Text;
      fQuery.ParamByName('descricao').AsString := memdescricao.Text;
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

procedure TfrmUsuarioPerfil_Cad.Clear_Fields;
begin
  edid_perfil.Clear;
  ednome_perfil.Clear;
  memdescricao.Lines.Clear;
end;

procedure TfrmUsuarioPerfil_Cad.ExportD2Bridge;
begin
  inherited;

  Title := 'Cadastro de Perfil de Usuários';

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
          FormGroup(lbid_perfil.Caption,CSSClass.Col.colsize2).AddLCLObj(edid_perfil);
          FormGroup(lbnome_perfil.Caption,CSSClass.Col.colsize10).AddLCLObj(ednome_perfil,'ValidationPerfil',True);
        end;
        with Row.Items.Add do
          FormGroup(lbdescricao.Caption,CSSClass.Col.colsize12).AddLCLObj(memdescricao);
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar,'ValidationPerfil',False, CSSClass.Button.save + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;
  end;

end;

procedure TfrmUsuarioPerfil_Cad.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TfrmUsuarioPerfil_Cad.RenderD2Bridge(const PrismControl: TPrismControl;
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
