unit uPrincipal;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Menus,  
  D2Bridge.Forms;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TD2BridgeForm)
    MainMenu1: TMainMenu;
    menuCadastro: TMenuItem;
    menuCad_Empresa: TMenuItem;
    menuCadUsuario: TMenuItem;
    menuCad_Usu_Usuario: TMenuItem;
    menuCad_Usu_Perfil: TMenuItem;
    menuCad_Usu_PermissaoAcao: TMenuItem;
    menuCad_Usu_PermissaoTela: TMenuItem;
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

function frmPrincipal: TfrmPrincipal;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmPrincipal: TfrmPrincipal;
begin
  result := (TfrmPrincipal.GetInstance as TfrmPrincipal);
end;

procedure TfrmPrincipal.ExportD2Bridge;
begin
  inherited;

  Title := 'Emissor de Nota Fiscai e Cupom Fiscal';

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.add do
  begin
    {Yours Controls}
    SideMenu(MainMenu1);
  end;

end;

procedure TfrmPrincipal.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TfrmPrincipal.RenderD2Bridge(const PrismControl: TPrismControl;
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


{
FResp := TRequest.New.BaseURL(FHost)
         .Resource('empresa')
         //.TokenBearer(Emissor.Token_Server)
         .AddBody(fEmpresa)
         .Accept('application/json')
         .Post;


}



