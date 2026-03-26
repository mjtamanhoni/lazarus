unit uUsuario.Perfil;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBGrids,
  Menus, ZDataset, ZAbstractRODataset, D2Bridge.Forms, uPrincipal, DB, udm,
  uBase.Functions, LR_Class, LR_Desgn, LR_PGrid, LR_DBSet, LR_DSet, lrPDFExport,
  IniFiles;

type

  { TfrmUsuario_Perfil }

  TfrmUsuario_Perfil = class(TfrmPrincipal)
    btNovo: TButton;
    btSelPesquisa: TButton;
    DBGrid_Principal: TDBGrid;
    dsRegistro: TDataSource;
    edPesquisar: TEdit;
    frDBDataSet1: TfrDBDataSet;
    frReport1: TfrReport;
    frUserDataset1: TfrUserDataset;
    lrPDFExport1: TlrPDFExport;
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
    procedure OnClick_Edit(const AId: Integer; const ANome:String);
    procedure OnClick_Delete(const AId: Integer; const ANome:String);
    procedure OnClick_Print(const AId: Integer; const ANome:String);

  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; 
      var HTMLControl: string); override;
  end;

function frmUsuario_Perfil: TfrmUsuario_Perfil;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmUsuario_Perfil: TfrmUsuario_Perfil;
begin
  result := (TfrmUsuario_Perfil.GetInstance as TfrmUsuario_Perfil);
end;

procedure TfrmUsuario_Perfil.FormShow(Sender: TObject);
begin
  Pesquisar;
end;

procedure TfrmUsuario_Perfil.miNomeClick(Sender: TObject);
begin
  edPesquisar.Tag := TMenuItem(Sender).Tag;
  case TMenuItem(Sender).Tag of
    0:edPesquisar.TextHint := 'Pesquisar pelo ID do Perfil';
    1:edPesquisar.TextHint := 'Pesquisar pela Nome do Perfil';
  end;
  Pesquisar;
end;

procedure TfrmUsuario_Perfil.edPesquisarKeyPress(Sender: TObject; var Key: char );
begin
  if Key = #13 then
    Pesquisar;

end;

procedure TfrmUsuario_Perfil.FormCreate(Sender: TObject);
begin
  FIniFile := TIniFile.Create(ConfigFile);
  FDM := TDM.Create(Self);
  ZQRegistros.Connection := FDM.ZConnection;
end;

procedure TfrmUsuario_Perfil.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDM);
  FreeAndNil(FIniFile);
end;

procedure TfrmUsuario_Perfil.Pesquisar;
begin
  try
    try
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
  end;
end;

procedure TfrmUsuario_Perfil.OnClick_Edit(const AId: Integer; const ANome: String);
begin

end;

procedure TfrmUsuario_Perfil.OnClick_Delete(const AId: Integer; const ANome: String);
begin

end;

procedure TfrmUsuario_Perfil.OnClick_Print(const AId: Integer; const ANome: String);
begin

end;

procedure TfrmUsuario_Perfil.ExportD2Bridge;
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
      with HTMLDIV(CSSClass.Col.colsize10).Items.Add do
      begin
        with Row(CSSClass.Space.margim_bottom3).Items.Add do
        begin
          With FormGroup('',CSSClass.Col.colsize12).Items.Add do
          begin
            LCLObj(edPesquisar);
            LCLObj(btSelPesquisa, PopupMenu, CSSClass.Button.search);
          end;
        end;
      end;

      with HTMLDIV(CSSClass.Col.colsize2).Items.Add do
      begin
        with Row(CSSClass.Space.margim_bottom3 + ' ' + CSSClass.Space.margim_top1).Items.Add do
        begin
          with HTMLDIV(CSSClass.Text.Align.right).Items.Add do
            LCLObj(btNovo, CSSClass.Button.add);
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
  end;

end;

procedure TfrmUsuario_Perfil.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  if PrismControl.VCLComponent = DBGrid_Principal then
  begin
    with PrismControl.AsDBGrid do
    begin
      PrismControl.AsDBGrid.RecordsPerPage := 10;

      with Columns.Add do
      begin
        ColumnIndex := 0;
        Title := D2Bridge.LangNav.Button.CaptionOptions;
        Width := 45;

        //Create Popup + Button
        with Buttons.Add do
        begin
          ButtonModel:= TButtonModel.list;
          Caption := '';

          //Edit
          with Add do
          begin
            ButtonModel:= TButtonModel.Edit;
          end;

          //Delete
          with Add do
          begin
            ButtonModel:= TButtonModel.Delete;
          end;

          //Print
          with Add do
          begin
            ButtonModel:= TButtonModel.Print;
          end;
        end;
      end;
    end;
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

procedure TfrmUsuario_Perfil.RenderD2Bridge(const PrismControl: TPrismControl;
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
