unit uUsuario;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, memds, DB, Controls, Graphics, Dialogs, StdCtrls, DBGrids,
  ZDataset, D2Bridge.Forms,
  fpjson,  //Trabalhar com Json
  jsonparser,
  DataSet.Serialize, TypInfo;

type

  { TForm2 }

  TForm2 = class(TD2BridgeForm)
    Button1: TButton;
    DBGrid1: TDBGrid;
    MemDataset_User: TMemDataset;
    MemDataset_UserMemDataset1_Codigo: TLongintField;
    MemDataset_UserMemDataset1_Nome: TStringField;
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
