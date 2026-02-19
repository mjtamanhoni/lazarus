unit uCad.Usuario;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,  
  D2Bridge.Forms, uPrincipal;

type

  { TfrmCad_Usuario }

  TfrmCad_Usuario = class(TfrmPrincipal)
    btCancelar: TButton;
    btConfirmar: TButton;
    btidPerfil: TButton;
    cbativo: TComboBox;
    edemail: TEdit;
    edidPerfil_Desc: TEdit;
    edidUsuario: TEdit;
    edsenha: TEdit;
    edlogin: TEdit;
    ednome: TEdit;
    edidPerfil: TEdit;
    lbativo: TLabel;
    lbemail: TLabel;
    lbidUsuario: TLabel;
    lbsenha: TLabel;
    lblogin: TLabel;
    lbnome: TLabel;
    lbidPerfil: TLabel;
    pnativo: TPanel;
    pnDetail: TPanel;
    pnemail: TPanel;
    pnFooter: TPanel;
    pnidUsuario: TPanel;
    pnsenha: TPanel;
    pnlogin: TPanel;
    pnRow001: TPanel;
    pnRow002: TPanel;
    pnRow003: TPanel;
    pnRow004: TPanel;
    pnRow005: TPanel;
    pnRow006: TPanel;
    pnnome: TPanel;
    pnidPerfil: TPanel;
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

function frmCad_Usuario: TfrmCad_Usuario;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmCad_Usuario: TfrmCad_Usuario;
begin
  result := (TfrmCad_Usuario.GetInstance as TfrmCad_Usuario);
end;

procedure TfrmCad_Usuario.Clear_Fields;
begin
  //
end;

procedure TfrmCad_Usuario.ExportD2Bridge;
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

procedure TfrmCad_Usuario.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TfrmCad_Usuario.RenderD2Bridge(const PrismControl: TPrismControl;
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
