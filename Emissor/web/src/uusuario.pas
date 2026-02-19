unit uUsuario;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uPrincipal, DB,
  D2Bridge.Forms;

type

  { TfrmUsuario }

  TfrmUsuario = class(TfrmPrincipal)
    btNovo: TButton;
    btSelPesquisa: TButton;
    DBGrid_Empresa: TDBGrid;
    dsRegistro: TDataSource;
    edPesquisar: TEdit;
    pnDetail: TPanel;
    pnFiltro: TPanel;
    pnFooter: TPanel;
    pnHeader: TPanel;
    pnTipoFiltro2: TPanel;
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

function frmUsuario: TfrmUsuario;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmUsuario: TfrmUsuario;
begin
  result := (TfrmUsuario.GetInstance as TfrmUsuario);
end;

procedure TfrmUsuario.ExportD2Bridge;
begin
  inherited;

  Title := 'My D2Bridge Form';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    {Yours Controls}
  end;

end;

procedure TfrmUsuario.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TfrmUsuario.RenderD2Bridge(const PrismControl: TPrismControl;
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
