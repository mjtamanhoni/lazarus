unit uEmpresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, memds, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DBGrids, ZDataset, D2Bridge.Forms;

type

  { TfrmEmpresa }

  TfrmEmpresa = class(TD2BridgeForm)
    btNovo: TButton;
    btFechar: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    edFiltro: TEdit;
    lbFiltro: TLabel;
    MemDataset: TMemDataset;
    pnHeader: TPanel;
    pnDetail: TPanel;
    pnFooter: TPanel;
    pnFiltro: TPanel;
    pnTipoFiltro2: TPanel;
    procedure FormCreate(Sender: TObject);
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
