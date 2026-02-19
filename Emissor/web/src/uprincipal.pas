unit uPrincipal;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Menus,  
  D2Bridge.Forms;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TD2BridgeForm)
    mnuCadUsu_Acao: TMenuItem;
    mnuCadUsu_Tela: TMenuItem;
    mnuDesconectar: TMenuItem;
    mnuConfig: TMenuItem;
    mnuRelatorios: TMenuItem;
    mnuDashboard: TMenuItem;
    mnuMovimento: TMenuItem;
    mmPrincipal: TMainMenu;
    mnuCadastro: TMenuItem;
    mnuCad_Empresa: TMenuItem;
    mnuCadUsuario: TMenuItem;
    mnuCad_Usu_Usuario: TMenuItem;
    mnuCad_Usu_Perfil: TMenuItem;
    mnuCad_Usu_Permissoes: TMenuItem;
    procedure mnuCad_EmpresaClick(Sender: TObject);
    procedure mnuDesconectarClick(Sender: TObject);
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
  EmissorWebApp, uEmpresa;

{$R *.lfm}

function frmPrincipal: TfrmPrincipal;
begin
  result := (TfrmPrincipal.GetInstance as TfrmPrincipal);
end;

procedure TfrmPrincipal.mnuDesconectarClick(Sender: TObject);
begin
  if MessageDlg('Deseja encerrar a sessão atual e realizar um novo login?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
  begin
    if IsD2BridgeContext then
      Session.Close(True);
  end;
end;

procedure TfrmPrincipal.mnuCad_EmpresaClick(Sender: TObject);
begin
  if frmEmpresa = nil then
    TfrmEmpresa.CreateInstance;
  frmEmpresa.Show;
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
    SideMenu(mmPrincipal);
  end;

end;

procedure TfrmPrincipal.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;

  if PrismControl.VCLComponent = mmPrincipal then
  begin
    with PrismControl.AsSideMenu do
    begin
      PrismControl.AsSideMenu.Color := clBlue;
      {$Region 'Principal'}
      PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCadastro).Icon := 'fa-solid fa-address-card';
      PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuMovimento).Icon := 'fa-solid fa-money-bill-transfer';
      PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuDashboard).Icon := 'fa-solid fa-chart-line';
      PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuRelatorios).Icon := 'fa-solid fa-print';
      PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuConfig).Icon := 'fa-solid fa-gear';
      PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuDesconectar).Icon := 'fa-solid fa-arrow-right-from-bracket';
      {$EndRegion 'Principal'}

      {$Region 'Usuarios'}
        PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCadUsuario).Icon := 'fa-solid fa-users';
        PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCad_Usu_Usuario).Icon := 'fa-solid fa-users';
        PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCad_Usu_Perfil).Icon := 'fa-solid fa-address-card';
        PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCad_Usu_Permissoes).Icon := 'fa-solid fa-lock-open'; //<i class="fa-solid fa-lock-open"></i>
        PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCadUsu_Tela).Icon := 'fa-solid fa-laptop-file';
        PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCadUsu_Acao).Icon := 'fa-solid fa-list';
      {$EndRegion 'Usuarios'}

      {$Region 'Empresa'}
        PrismControl.AsSideMenu.MenuItemFromVCLComponent(mnuCad_Empresa).Icon := 'fa-solid fa-building';
      {$EndRegion 'Empresa'}

    end;
  end;


  //https://fontawesome.com/icons/laptop

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



