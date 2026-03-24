unit uConfig.ACBr;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  ACBrCEP, ACBrConsultaCNPJ, D2Bridge.Forms;

type

  { TForm2 }

  TForm2 = class(TD2BridgeForm)
    ACBrCEP: TACBrCEP;
    ACBrConsultaCNPJ: TACBrConsultaCNPJ;
    Button1: TButton;
    Button2: TButton;
    cbbProvedor: TComboBox;
    cbxWS: TComboBox;
    edChaveWS: TEdit;
    edPass: TEdit;
    edProxyHost: TEdit;
    edProxyPass: TEdit;
    edProxyPort: TEdit;
    edProxyUser: TEdit;
    edtHost: TEdit;
    edtPort: TEdit;
    edtSenha: TEdit;
    edtUsuario: TEdit;
    edUser: TEdit;
    gbProx: TGroupBox;
    gbCEP_WebService: TGroupBox;
    gbProx1: TGroupBox;
    lbCNPJProvedor: TLabel;
    lbtHost: TLabel;
    lbSenha: TLabel;
    lbPort: TLabel;
    lbUsuario: TLabel;
    lbWS: TLabel;
    lbUser: TLabel;
    lbPass: TLabel;
    lbProxyHost: TLabel;
    lbProxyPort: TLabel;
    lbProxyUser: TLabel;
    lbProxyPass: TLabel;
    lbChaveWS: TLabel;
    pnCEP_Prox1: TPanel;
    pnCNPJ_Provedor: TPanel;
    pnCNPJ: TPanel;
    pnCEP_Prox: TPanel;
    pnCEP_WebService: TPanel;
    pnCEP: TPanel;
    pcPrincipal: TPageControl;
    pnDetail: TPanel;
    pnFooter: TPanel;
    tsCNPJ: TTabSheet;
    tsTab_CEP: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

function Form2: TForm2;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function Form2: TForm2;
begin
  result := (TForm2.GetInstance as TForm2);
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.ExportD2Bridge;
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

procedure TForm2.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TForm2.RenderD2Bridge(const PrismControl: TPrismControl; 
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
