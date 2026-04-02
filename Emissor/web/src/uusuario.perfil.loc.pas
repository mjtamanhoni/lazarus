unit uUsuario.Perfil.Loc;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, Menus, ZDataset, ZAbstractRODataset, D2Bridge.Forms, IniFiles,
  udm, uBase.Functions;

type

  { TfrmUsuario_Perfil_Loc }

  TfrmUsuario_Perfil_Loc = class(TD2BridgeForm)
    btCancelar: TButton;
    btConfirmar: TButton;
    btSelPesquisa: TButton;
    DBGrid_Principal: TDBGrid;
    dsRegistro: TDataSource;
    edPesquisar: TEdit;
    miID: TMenuItem;
    miNome: TMenuItem;
    pmPesquisar: TPopupMenu;
    pnDetail: TPanel;
    pnFiltro: TPanel;
    pnFooter: TPanel;
    pnHeader: TPanel;
    pnTipoFiltro2: TPanel;
    ZQRegistros: TZQuery;
    ZQRegistrosdescricao: TZRawStringField;
    ZQRegistrosid_perfil: TZIntegerField;
    ZQRegistrosnome_perfil: TZRawStringField;
    procedure btCancelarClick(Sender: TObject);
    procedure btConfirmarClick(Sender: TObject);
    procedure edPesquisarKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miNomeClick(Sender: TObject);
  private
    { Private declarations }
    FDM :TDM;
    FIniFile :TIniFile;

    procedure Pesquisar;
  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; 
      var HTMLControl: string); override;
  end;

function frmUsuario_Perfil_Loc: TfrmUsuario_Perfil_Loc;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmUsuario_Perfil_Loc: TfrmUsuario_Perfil_Loc;
begin
  result := (TfrmUsuario_Perfil_Loc.GetInstance as TfrmUsuario_Perfil_Loc);
end;

procedure TfrmUsuario_Perfil_Loc.FormShow(Sender: TObject);
begin
  with Emissor.Perfil_Usuario do
  begin
    id_perfil := 0;
    nome_perfil := '';
    descricao := '';
  end;

  Pesquisar;
end;

procedure TfrmUsuario_Perfil_Loc.edPesquisarKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #13 then
    Pesquisar;
end;

procedure TfrmUsuario_Perfil_Loc.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmUsuario_Perfil_Loc.btConfirmarClick(Sender: TObject);
begin
  with Emissor.Perfil_Usuario do
  begin
    id_perfil := ZQRegistrosid_perfil.AsInteger;
    nome_perfil := ZQRegistrosnome_perfil.AsString;
    descricao := ZQRegistrosdescricao.AsString;
  end;
	Close;
end;

procedure TfrmUsuario_Perfil_Loc.FormCreate(Sender: TObject);
begin
  FIniFile := TIniFile.Create(ConfigFile);
  FDM := TDM.Create(Self);
  ZQRegistros.Connection := FDM.ZConnection;

end;

procedure TfrmUsuario_Perfil_Loc.FormDestroy(Sender: TObject);
begin
  if Assigned(FDM) then
    FreeAndNil(FDM);
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
end;

procedure TfrmUsuario_Perfil_Loc.miNomeClick(Sender: TObject);
begin
  edPesquisar.Tag := TMenuItem(Sender).Tag;
  case TMenuItem(Sender).Tag of
    0:edPesquisar.TextHint := 'Pesquisar pelo ID do Perfil';
    1:edPesquisar.TextHint := 'Pesquisar pela Nome do Perfil';
  end;
  Pesquisar;
end;

procedure TfrmUsuario_Perfil_Loc.Pesquisar;
begin
  try
    try
      ZQRegistros.DisableControls;

      ZQRegistros.Close;
      ZQRegistros.SQL.Clear;
      ZQRegistros.SQL.Add('select ');
      ZQRegistros.SQL.Add('  p.* ');
      ZQRegistros.SQL.Add('from public.perfis p ');
      ZQRegistros.SQL.Add('where 1=1 ');
      if Trim(edPesquisar.Text) <> '' then
      begin
        case edPesquisar.Tag of
          0:begin
            if not ApenasNumeros(edPesquisar.Text) then
              raise Exception.Create('Para realizar o filtro usando o ID,  não pode haver letras no texto da pesquisa');
            ZQRegistros.SQL.Add('  and p.id_perfil = ' + edPesquisar.Text);
          end;
          1:ZQRegistros.SQL.Add('  and p.nome_perfil like ' + QuotedStr('%'+edPesquisar.Text+'%'));
        end;
      end;
      ZQRegistros.SQL.Add('order by p.id_perfil ');
      ZQRegistros.Open;
    except
      on E: Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'Pesquisar',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);
      end;
    end;
  finally
    ZQRegistros.EnableControls;
  end;
end;

procedure TfrmUsuario_Perfil_Loc.ExportD2Bridge;
begin
  inherited;


  Title := Self.Caption;

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    {Yours Controls}
    with Row.Items.Add do
    begin
      with HTMLDIV(CSSClass.Col.colsize12).Items.Add do
      begin
        with Row(CSSClass.Space.margim_bottom3).Items.Add do
        begin
          With FormGroup('',CSSClass.Col.colsize12).Items.Add do
          begin
            LCLObj(btSelPesquisa, PopupMenu, CSSClass.Button.search);
            LCLObj(edPesquisar);
          end;
        end;
      end;
    end;

    with Row.Items.Add do
    begin
      with HTMLDIV(CSSClass.Col.colsize12).Items.Add do
      begin
        with Row.Items.Add do
        begin
          with PanelGroup('Listagem','',False,CSSClass.Col.colsize12).Items.Add do
            LCLObj(DBGrid_Principal);
        end;
      end;
    end;

    with Row(CSSClass.DivHtml.Align_Center).Items.Add do
    begin
      VCLObj(btConfirmar,CSSClass.Button.check + CSSClass.Col.colsize2);
      VCLObj(btCancelar, CSSClass.Button.cancel + CSSClass.Col.colsize2);
    end;
  end;

end;

procedure TfrmUsuario_Perfil_Loc.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  if PrismControl.VCLComponent = DBGrid_Principal then
  begin
    with PrismControl.AsDBGrid do
      PrismControl.AsDBGrid.RecordsPerPage := 10;
  end;

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

procedure TfrmUsuario_Perfil_Loc.RenderD2Bridge(const PrismControl: TPrismControl;
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
