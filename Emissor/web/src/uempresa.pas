unit uEmpresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, memds, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, EditBtn, Menus, ZDataset, D2Bridge.Forms;

type

  { TfrmEmpresa }

  TfrmEmpresa = class(TD2BridgeForm)
    btNovo: TButton;
    btFechar: TButton;
    BtPrint: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    edPesquisar: TEditButton;
    MemDataset: TMemDataset;
    miCNPJ_CPF: TMenuItem;
    miNomeFantasia: TMenuItem;
    miRazaoSocial: TMenuItem;
    miId: TMenuItem;
    pnHeader: TPanel;
    pnDetail: TPanel;
    pnFooter: TPanel;
    pnFiltro: TPanel;
    pnTipoFiltro2: TPanel;
    pMenu_Filtro: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure miRazaoSocialClick(Sender: TObject);
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

function frmEmpresa: TfrmEmpresa;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmEmpresa: TfrmEmpresa;
begin
  result := (TfrmEmpresa.GetInstance as TfrmEmpresa);
end;

procedure TfrmEmpresa.FormCreate(Sender: TObject);
begin
  (*Esse é um exemplo...

  MemDataset.Close;
  MemDataset.Open;;

  MemDataset.Insert;
  MemDatasetidEmpresa.AsInteger := 1;
  MemDatasetNome.AsString := 'MARCOS';
  MemDataset.Post;

  MemDataset.Insert;
  MemDatasetidEmpresa.AsInteger := 2;
  MemDatasetNome.AsString := 'SIMONE';
  MemDataset.Post;

  MemDataset.Insert;
  MemDatasetidEmpresa.AsInteger := 3;
  MemDatasetNome.AsString := 'GABRIEL';
  MemDataset.Post;

  MemDataset.Insert;
  MemDatasetidEmpresa.AsInteger := 4;
  MemDatasetNome.AsString := 'NICOLAS';
  MemDataset.Post;
  *)
end;

procedure TfrmEmpresa.miRazaoSocialClick(Sender: TObject);
begin
  edPesquisar.Tag := TMenuItem(Sender).Tag;
  case TMenuItem(Sender).Tag of
    0:edPesquisar.TextHint := 'Pesquisar pelo ID da Empresa';
    1:edPesquisar.TextHint := 'Pesquisar pela Razão Social da Empresa';
    2:edPesquisar.TextHint := 'Pesquisar pelo Nome Fantasia da Empresa';
    3:edPesquisar.TextHint := 'Pesquisar pela Região do Município';
    4:edPesquisar.TextHint := 'Pesquisar pela sigla da UF do Município';
  end;
  Pesquisar;

end;

procedure TfrmEmpresa.ExportD2Bridge;
begin
  inherited;

  Title := 'Empresas';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    LCLObj(DBGrid);
    {Yours Controls}
  end;

end;

procedure TfrmEmpresa.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

  //Change Init Property of Prism Controls
  {
  if PrismControl.VCLComponent = edFiltro then
    PrismControl.AsEdit.DataType:= TPrismFieldType.PrismFieldTypeInteger;

  if PrismControl.IsDBGrid then
  begin
    PrismControl.AsDBGrid.RecordsPerPage:= 10;
    PrismControl.AsDBGrid.MaxRecords:= 2000;
  end;
  }
end;

procedure TfrmEmpresa.RenderD2Bridge(const PrismControl: TPrismControl;
  var HTMLControl: string);
begin
  inherited;

  //Intercept HTML
  {
  if PrismControl.VCLComponent = edFiltro then
  begin
    HTMLControl:= '</>';
  end;
  }
end;

end.
