unit uFormularios;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls,
  DBGrids, ZDataset, ZAbstractRODataset, D2Bridge.Forms, uPrincipal, DB, udm,
  uBase.Functions, LR_Class, LR_Desgn, LR_PGrid, LR_DBSet, LR_DSet, lrPDFExport,
  lr_e_pdf, IniFiles, PReport,

  uformularios.Cad;

type

  { TfrmFormularios }

  TfrmFormularios = class(TfrmPrincipal)
    btGerarPDF: TButton;
    btNovo: TButton;
    btSelPesquisa: TButton;
    DBGrid_Principal: TDBGrid;
    dsRegistro: TDataSource;
    edPesquisar: TEdit;
    frDBDS_Registros: TfrDBDataSet;
    frReport_Registros: TfrReport;
    miID: TMenuItem;
    miNome: TMenuItem;
    pmPesquisar: TPopupMenu;
    pnDetail: TPanel;
    pnFiltro: TPanel;
    pnFooter: TPanel;
    pnHeader: TPanel;
    pnTipoFiltro2: TPanel;
    ZQRegistros: TZQuery;
    ZQRegistrosdescricao: TZRawCLobField;
    ZQRegistrosid_form: TZIntegerField;
    ZQRegistrosnome: TZRawStringField;
    ZQRegistrosstatus: TZIntegerField;
    ZQRegistrosstatus_desc: TZRawCLobField;
    procedure btGerarPDFClick(Sender: TObject);
    procedure btNovoClick(Sender: TObject);
    procedure edPesquisarKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miNomeClick(Sender: TObject);
  private
    { Private declarations }
    FDM :TDM;
    FIniFile :TIniFile;

    FfrmFormularios_Cad :TfrmFormularios_Cad;

    procedure Pesquisar;
    procedure OnClick_Edit;
    procedure OnClick_Delete;
  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
    procedure CellButtonClick(APrismDBGrid: TPrismDBGrid; APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer); overload; override;
  end;

function frmFormularios: TfrmFormularios;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmFormularios: TfrmFormularios;
begin
  result := (TfrmFormularios.GetInstance as TfrmFormularios);
end;

procedure TfrmFormularios.FormShow(Sender: TObject);
begin
  Pesquisar;
end;

procedure TfrmFormularios.edPesquisarKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Pesquisar;
end;

procedure TfrmFormularios.FormCreate(Sender: TObject);
begin
  FIniFile := TIniFile.Create(ConfigFile);
  FDM := TDM.Create(Self);
  ZQRegistros.Connection := FDM.ZConnection;
end;

procedure TfrmFormularios.FormDestroy(Sender: TObject);
begin
  if Assigned(FDM) then
    FreeAndNil(FDM);
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
end;

procedure TfrmFormularios.btNovoClick(Sender: TObject);
begin
  try
    FfrmFormularios_Cad.Clear_Fields;
    ShowPopupModal('Popup' + FfrmFormularios_Cad.Name);
  except
    On E:Exception do
    begin
      MessageDlg(E.Message, TMsgDlgType.mtError, [mbok], 0);
      GravarLogJSON(Self.Name,Self.Caption,'btNovoClick',E);
    end;
  end;

  Pesquisar;
end;

procedure TfrmFormularios.btGerarPDFClick(Sender: TObject);
var
  fArquivo :String;
  fs: TFileStream;
begin
  try
    try
      fArquivo := '';
      fArquivo := EndPDF + Self.Name + '.PDF';
      frReport_Registros.LoadFromFile(EndReport + frReport_Registros.Title);
      frReport_Registros.PrepareReport;

      fs := TFileStream.Create(fArquivo,fmCreate);

      if frReport_Registros.ExportTo(TfrTNPDFExportFilter,fs,True) then
        MessageDlg('Arquivo PDF gerado com suecesso:' + sLineBreak + fArquivo,TMsgDlgType.mtInformation,[mbOK],0);

    except
      On E:Exception do
      begin
        MessageDlg(E.Message, TMsgDlgType.mtError, [mbok], 0);
        GravarLogJSON(Self.Name,Self.Caption,'btGerarPDFClick',E);
      end;
    end;
  finally
    if Assigned(fs) then
      FreeAndNil(fs);
  end;
end;

procedure TfrmFormularios.miNomeClick(Sender: TObject);
begin
  edPesquisar.Tag := TMenuItem(Sender).Tag;
  case TMenuItem(Sender).Tag of
    0:edPesquisar.TextHint := 'Pesquisar pelo ID do Formulário';
    1:edPesquisar.TextHint := 'Pesquisar pela Nome do Formulário';
  end;
  Pesquisar;
end;

procedure TfrmFormularios.Pesquisar;
begin
  try
    try
      ZQRegistros.DisableControls;

      ZQRegistros.Close;
      ZQRegistros.SQL.Clear;
      ZQRegistros.SQL.Add('select ');
      ZQRegistros.SQL.Add('  f.* ');
      ZQRegistros.SQL.Add('  ,case f.status ');
      ZQRegistros.SQL.Add('    when 0 then ''Inativo'' ');
      ZQRegistros.SQL.Add('    when 1 then ''Ativo'' ');
      ZQRegistros.SQL.Add('  end status_descv ');
      ZQRegistros.SQL.Add('from public.formularios f ');
      ZQRegistros.SQL.Add('where 1=1 ');
      if Trim(edPesquisar.Text) <> '' then
      begin
        case edPesquisar.Tag of
          0:begin
            if not ApenasNumeros(edPesquisar.Text) then
              raise Exception.Create('Para realizar o filtro usando o ID,  não pode haver letras no texto da pesquisa');
            ZQRegistros.SQL.Add('  and f.id_form = ' + edPesquisar.Text);
          end;
          1:ZQRegistros.SQL.Add('  and f.nome like ' + QuotedStr('%'+edPesquisar.Text+'%'));
        end;
      end;
      ZQRegistros.SQL.Add('order by f.id_form; ');
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

procedure TfrmFormularios.OnClick_Edit;
begin
  try
    try
      //Atualizando dados principais...
      FfrmFormularios_Cad.Clear_Fields;
      if not ZQRegistros.IsEmpty then
      begin
        //FfrmUsuarioPerfil_Cad.edid_perfil.Text := ZQRegistrosid_perfil.AsString;
        //FfrmUsuarioPerfil_Cad.ednome_perfil.Text := ZQRegistrosnome_perfil.AsString;
        //FfrmUsuarioPerfil_Cad.memdescricao.Text := ZQRegistrosdescricao.AsString;
        ShowPopupModal('Popup' + FfrmFormularios_Cad.Name);
        Pesquisar;
      end
      else
        raise Exception.Create('Formulário não localizado');
    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'OnClick_Edit',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
  end;
end;

procedure TfrmFormularios.OnClick_Delete;
var
  fQuery :TZQuery;
begin
  try
    try
      if MessageDlg('Deseja excluir o Formulário selecionado?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
      begin
        fQuery := FDM.GetQuery;
        fQuery.Sql.Add('DELETE FROM public.formularios WHERE id_form = :id_form);');
        fQuery.ParamByName('id_perfil').AsInteger := ZQRegistrosid_form.AsInteger;
				fQuery.ExecSql;
        Pesquisar;
      end;
    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'OnClick_Delete',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
    if Assigned(fQuery) then
      FreeAndNil(fQuery);
  end;
end;

procedure TfrmFormularios.ExportD2Bridge;
begin
  inherited;

  Title := Self.Caption;

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  //Formulário de cadastro....
  FfrmFormularios_Cad := TfrmFormularios_Cad.Create(Self);
  D2Bridge.AddNested(FfrmFormularios_Cad);

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
            LCLObj(btNovo, CSSClass.Button.add);
            LCLObj(btGerarPDF, CSSClass.Button.filePDF);
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

    with Popup('Popup' + FfrmFormularios_Cad.Name,'Cadastro de Formulários',True,CSSClass.Popup.ExtraLarge).Items.Add do
      Nested(FfrmFormularios_Cad);
  end;

end;

procedure TfrmFormularios.InitControlsD2Bridge(const PrismControl: TPrismControl);
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

procedure TfrmFormularios.RenderD2Bridge(const PrismControl: TPrismControl;
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

procedure TfrmFormularios.CellButtonClick(APrismDBGrid: TPrismDBGrid;
  APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer
  );
begin
  //inherited CellButtonClick(APrismDBGrid, APrismCellButton, AColIndex, ARow);
  try
    if APrismDBGrid.VCLComponent = DBGrid_Principal then
    begin
      if APrismCellButton.Identify = TButtonModel.Edit.Identity then
        OnClick_Edit;

      if APrismCellButton.Identify = TButtonModel.Delete.Identity then
        OnClick_Delete;
    end;
  except
    On E:Exception do
    begin
      MessageDlg(E.Message, TMsgDlgType.mtError, [mbok], 0);
      GravarLogJSON(Self.Name,Self.Caption,'CellButtonClick',E);
    end;
  end;
end;

end.
