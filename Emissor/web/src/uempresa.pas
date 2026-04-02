unit uEmpresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, memds, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateUtils, DBGrids, EditBtn, Menus, ZDataset, ZAbstractRODataset,
  D2Bridge.Forms, Forms, IniFiles, BufDataset, fpjson, DataSet.Serialize,
  RESTRequest4D, jsonparser, uBase.Functions, fpwebdata, extjsxml, LR_DBSet,
  LR_Class, lrPDFExport, lr_e_pdf, RLReport, uDM.ACBr, uPrincipal, uCad.Empresa,
  uBase.DataSets, uDM, LCLIntf, LCLType, LazHelpHTML, PReport;

type

  { TfrmEmpresa }

  TfrmEmpresa = class(TfrmPrincipal)
    btNovo: TButton;
    btSelPesquisa: TButton;
    btGerarPDF: TButton;
    dsRegistro: TDataSource;
    DBGrid_Empresa: TDBGrid;
    edPesquisar: TEdit;
    frDBDS_Registros: TfrDBDataSet;
    frReport_Registros: TfrReport;
    miID: TMenuItem;
    miRazaoSocial: TMenuItem;
    miFantasia: TMenuItem;
    miDocumento: TMenuItem;
    pnHeader: TPanel;
    pnDetail: TPanel;
    pnFooter: TPanel;
    pnFiltro: TPanel;
    pnTipoFiltro2: TPanel;
    pmPesquisar: TPopupMenu;
    ZQRegistros: TZQuery;
    ZQRegistrosativo: TZIntegerField;
    ZQRegistrosativo_desc: TZRawCLobField;
    ZQRegistroscelular: TZRawStringField;
    ZQRegistroscnpj: TZRawStringField;
    ZQRegistroscrt: TZRawStringField;
    ZQRegistroscrt_desc: TZRawCLobField;
    ZQRegistrosdata_cadastro: TZDateTimeField;
    ZQRegistrosemail: TZRawStringField;
    ZQRegistrosid_empresa: TZIntegerField;
    ZQRegistrosinscricao_estadual: TZRawStringField;
    ZQRegistrosinscricao_municipal: TZRawStringField;
    ZQRegistrosnome_fantasia: TZRawStringField;
    ZQRegistrosqtd_user: TZInt64Field;
    ZQRegistrosrazao_social: TZRawStringField;
    ZQRegistrosregime_tributario: TZRawStringField;
    ZQRegistrossite: TZRawStringField;
    ZQRegistrostelefone: TZRawStringField;
    procedure btGerarPDFClick(Sender: TObject);
    procedure btNovoClick(Sender: TObject);
    procedure edPesquisarKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miRazaoSocialClick(Sender: TObject);
  private
    { Private declarations }
    FfrmCadEmpresa :TfrmCadEmpresa;
    FDM :TDM;

    FHost :String;
    FIniFile :TIniFile;

    FJSon_Empresa :TJSONArray;
    FJSon_Certificado :TJSONArray;
    FJSon_Endereco :TJSONArray;
    FJSon_ContaBancaria :TJSONArray;

    procedure AjustarColunas(DBGrid: TDBGrid);
    procedure Pesquisar;
    procedure OnClick_Edit(const AId: Integer; const ANome:String);
    procedure OnClick_Delete(const AId: Integer; const ANome:String);
    procedure OnClick_Print(const AId: Integer; const ANome:String);

  public
    { Public declarations }
  protected
    procedure ExportD2Bridge; override;
    procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
    procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
    procedure CellButtonClick(APrismDBGrid: TPrismDBGrid; APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer); overload; override;
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
  try
    try
    FHost := '';
    FIniFile := TIniFile.Create(ConfigFile);
    FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');

    FDM := TDM.Create(Self);
    ZQRegistros.Connection := FDM.ZConnection;

    if Trim(FHost) = '' then
      raise Exception.Create('Host de acesso ao servidor não informado.');

    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'FormCreate',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
  end;
end;

procedure TfrmEmpresa.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDM);
  FreeAndNil(FIniFile);
end;

procedure TfrmEmpresa.FormShow(Sender: TObject);
begin
  Pesquisar;
end;

procedure TfrmEmpresa.edPesquisarKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Pesquisar;
end;

procedure TfrmEmpresa.btNovoClick(Sender: TObject);
begin
  try
    FfrmCadEmpresa.Clear_Fields;
    FfrmCadEmpresa.pcPrincipal.ActivePageIndex := 0;
    FfrmCadEmpresa.tsEndereco.Enabled := False;
    FfrmCadEmpresa.tsDadosBancarios.Enabled := False;
    FfrmCadEmpresa.tsCertificadoDigital.Enabled := False;
    ShowPopupModal('Popup' + FfrmCadEmpresa.Name);
  except
    On E:Exception do
    begin
      MessageDlg(E.Message, TMsgDlgType.mtError, [mbok], 0);
      GravarLogJSON(Self.Name,Self.Caption,'btNovoClick',E);
    end;
  end;

  Pesquisar;
end;

procedure TfrmEmpresa.btGerarPDFClick(Sender: TObject);
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

procedure TfrmEmpresa.miRazaoSocialClick(Sender: TObject);
begin
  edPesquisar.Tag := TMenuItem(Sender).Tag;
  case TMenuItem(Sender).Tag of
    0:edPesquisar.TextHint := 'Pesquisar pelo ID da Empresa';
    1:edPesquisar.TextHint := 'Pesquisar pela Razão Social da Empresa';
    2:edPesquisar.TextHint := 'Pesquisar pelo Nome Fantasia da Empresa';
    3:edPesquisar.TextHint := 'Pesquisar pela CNPJ/CPF';
  end;
  Pesquisar;
end;

procedure TfrmEmpresa.Pesquisar;
var
  FResp :IResponse;
  FRet :String;
  FBody :TJSONObject;

  FTipoPesquisa:String;
  x:Integer;

  FId :Integer;

begin
  try
    try
      ZQRegistros.DisableControls;
      ZQRegistros.Close;
      ZQRegistros.SQL.Clear;
      ZQRegistros.SQL.Add('SELECT ');
      ZQRegistros.SQL.Add('  e.* ');
      ZQRegistros.SQL.Add('  ,case e.crt ');
      ZQRegistros.SQL.Add('    when ''1'' then ''Simples Nacional'' ');
      ZQRegistros.SQL.Add('    when ''2'' then ''Simples Nacional - excesso de sublimite de receita bruta'' ');
      ZQRegistros.SQL.Add('    when ''3'' then ''Regime Normal (Lucro Presumido ou Real)'' ');
      ZQRegistros.SQL.Add('    when ''4'' then ''Microempreendedor Individual (MEI)'' ');
      ZQRegistros.SQL.Add('  end crt_desc ');
      ZQRegistros.SQL.Add('  ,case e.ativo ');
      ZQRegistros.SQL.Add('    when 0 then ''Inativo'' ');
      ZQRegistros.SQL.Add('    when 1 then ''Ativo'' ');
      ZQRegistros.SQL.Add('  end ativo_desc ');
      ZQRegistros.SQL.Add('  ,coalesce(u.qtd_user,0) as qtd_user ');
      ZQRegistros.SQL.Add('from  public.empresa e ');
      ZQRegistros.SQL.Add('  left join (select ');
      ZQRegistros.SQL.Add('               u.id_empresa ');
      ZQRegistros.SQL.Add('               ,count(u.id_usuario) as qtd_user ');
      ZQRegistros.SQL.Add('             from public.usuarios u ');
      ZQRegistros.SQL.Add('             group by 1) u on u.id_empresa = e.id_empresa ');
      ZQRegistros.SQL.Add('where 1=1 ');
      if Trim(edPesquisar.Text) <> '' then
      begin
        case edPesquisar.Tag of
          0:begin
            if not ApenasNumeros(edPesquisar.Text) then
              raise Exception.Create('Para realizar o filtro usando o ID,  não pode haver letras no texto da pesquisa');
            ZQRegistros.SQL.Add('  and e.id_empresa = ' + edPesquisar.Text);
          end;
          1:ZQRegistros.SQL.Add('  and e.razao_social like ' + QuotedStr('%'+edPesquisar.Text+'%'));
          2:ZQRegistros.SQL.Add('  and e.nome_fantasia like ' + QuotedStr('%'+edPesquisar.Text+'%'));
          3:ZQRegistros.SQL.Add('  and e.cnpj = ' + edPesquisar.Text);
        end;
      end;
      ZQRegistros.SQL.Add('order by e.id_empresa ');
      ZQRegistros.Open;
    (* PESQUISA REALIZADA USANDO SERVIDOR HORSE

      ZQRegistros.Close;

      FTipoPesquisa := '';
      case edPesquisar.Tag of
        0:begin
          if not ApenasNumeros(edPesquisar.Text) then
            raise Exception.Create('Para realizar o filtro usando o ID,  não pode haver letras no texto da pesquisa');
          FTipoPesquisa := 'id';
        end;
        1:FTipoPesquisa := 'razaoSocial';
        2:FTipoPesquisa := 'nomeFantasia';
        3:FTipoPesquisa := 'cnpj';
      end;

      if Trim(FHost) = '' then
        raise Exception.Create('Host não informado');

      if Trim(FTipoPesquisa) <> '' then
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .AddParam(FTipoPesquisa,edPesquisar.Text)
                 .Resource('empresa')
                 .Accept('application/json')
                 .Get;
      end
      else
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .Resource('empresa')
                 .Accept('application/json')
                 .Get;
      end;

      FRet := '';
      FRet := FResp.Content;
      FBody := TJSONObject(GetJSON(FRet));
      if FBody['success'].AsBoolean = False then
        raise Exception.Create(FBody['message'].AsString);

      //Empresa...
      FJSon_Empresa := TJSONArray(GetJSON(FBody['data'].AsJSON));
      ZQRegistros.LoadFromJSON(FJSon_Empresa);
      AjustarColunas(DBGrid_Empresa);

      //Endereço...
      FJSon_Endereco := TJSONArray(GetJSON(FBody['endereco'].AsJSON));
      ZMT_Endereco.LoadFromJSON(FJSon_Endereco);

      //Dados bancários...
      FJSon_ContaBancaria := TJSONArray(GetJSON(FBody['contaBancaria'].AsJSON));
      ZMT_ContaBancaria.LoadFromJSON(FJSon_ContaBancaria);

      //Certificado Digital...
      FJSon_Certificado := TJSONArray(GetJSON(FBody['certificadoDigital'].AsJSON));
      ZMT_Certificado.LoadFromJSON(FJSon_Certificado);
      *)
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

procedure TfrmEmpresa.OnClick_Delete(const AId: Integer; const ANome:String);
var
  {
  fResp :IResponse;
  fRet :String;
  fBody :TJSONObject;
  }
  fQuery :TZQuery;
begin
  try
    try
      if MessageDlg('Deseja excluir a Empresa selecionada?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
      begin
        fQuery := FDM.GetQuery;
        fQuery.Sql.Add('DELETE FROM public.empresa WHERE id_empresa = :id_empresa;');
        fQuery.ParamByName('id_empresa').AsInteger := ZQRegistrosid_empresa.AsInteger;
				fQuery.ExecSql;
        Pesquisar;
        {
        fResp := TRequest.New.BaseURL(FHost)
      	         .AddParam('id',ZQRegistros.FieldByName('idEmpresa').AsString)
                 .Resource('empresa')
                 .Accept('application/json')
                 .Delete;

        fRet := '';
        fRet := fResp.Content;
        fBody := TJSONObject(GetJSON(fRet));
        if fBody['success'].AsBoolean = False then
          raise Exception.Create(fBody['message'].AsString)
        else
        begin
          MessageDlg(fBody['message'].AsString,TMsgDlgType.mtInformation,[mbOK],0);
          Pesquisar;
        end;
        }
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

procedure TfrmEmpresa.OnClick_Print(const AId: Integer; const ANome:String);
var
  fArquivo :String;
  fs: TFileStream;
begin
  fArquivo := '';
  fArquivo := EndReport + 'Empresas.PDF';
  frReport_Registros.LoadFromFile(EndReport + frReport_Registros.Title);
  frReport_Registros.PrepareReport;

  fs := TFileStream.Create(fArquivo,fmCreate);

	if frReport_Registros.ExportTo(TfrTNPDFExportFilter,fs,True) then
    MessageDlg('PDF Gerado com Sucesso.',TMsgDlgType.mtInformation,[mbOK],0);
end;

procedure TfrmEmpresa.ExportD2Bridge;
begin
  inherited;

  Title := Self.Caption;

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  //Formulário de cadastro de empresas....
  FfrmCadEmpresa := TfrmCadEmpresa.Create(Self);
  D2Bridge.AddNested(FfrmCadEmpresa);

  with D2Bridge.Items.add do
  begin
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
            LCLObj(DBGrid_Empresa);
        end;
      end;
    end;

    with Popup('Popup' + FfrmCadEmpresa.Name,'Cadastro de Empresas',True,CSSClass.Popup.ExtraLarge).Items.Add do
      Nested(FfrmCadEmpresa);
  end;

end;

procedure TfrmEmpresa.OnClick_Edit(const AId: Integer; const ANome:String);
var
  fQuery :TZQuery;
begin
  try
    try
      fQuery := FDM.GetQuery;

      //Atualizando dados principais...
      FfrmCadEmpresa.Clear_Fields;
      FfrmCadEmpresa.edid_empresa.Text := ZQRegistros.FieldByName('id_empresa').AsString;
      FfrmCadEmpresa.edcnpj.Text := ZQRegistros.FieldByName('cnpj').AsString;
      FfrmCadEmpresa.edinscricao_estadual.Text := ZQRegistros.FieldByName('inscricao_estadual').AsString;
      FfrmCadEmpresa.edinscricao_municipal.Text := ZQRegistros.FieldByName('inscricao_municipal').AsString;
      FfrmCadEmpresa.cbativo.ItemIndex := ZQRegistros.FieldByName('ativo').AsInteger;
      FfrmCadEmpresa.edrazao_social.Text := ZQRegistros.FieldByName('razao_social').AsString;
      FfrmCadEmpresa.ednome_fantasia.Text := ZQRegistros.FieldByName('nome_fantasia').AsString;
      FfrmCadEmpresa.cbregime_tributario.ItemIndex := FfrmCadEmpresa.cbregime_tributario.Items.IndexOf(ZQRegistros.FieldByName('regime_tributario').AsString);
      FfrmCadEmpresa.edcrt.Text := ZQRegistros.FieldByName('crt').AsString;
      FfrmCadEmpresa.edtelefone.Text := ZQRegistros.FieldByName('telefone').AsString;
      FfrmCadEmpresa.edcelular.Text := ZQRegistros.FieldByName('celular').AsString;
      FfrmCadEmpresa.edemail.Text := ZQRegistros.FieldByName('email').AsString;
      FfrmCadEmpresa.edsite.Text := ZQRegistros.FieldByName('site').AsString;

      if not ZQRegistros.IsEmpty then
      begin
        FfrmCadEmpresa.pcPrincipal.ActivePageIndex := 0;

        //Listando endereços...
        FfrmCadEmpresa.Listar_Endereco(ZQRegistros.FieldByName('id_empresa').AsString);

        //Listando dados do banco...
        FfrmCadEmpresa.Listar_DadosBancarios(ZQRegistros.FieldByName('id_empresa').AsString);

        //Certificado digital...
        fQuery.SQL.Add('select ');
        fQuery.SQL.Add('  cd.* ');
        fQuery.SQL.Add('from public.certificado_digital cd ');
        fQuery.SQL.Add('where cd.id_empresa = ' + ZQRegistros.FieldByName('id_empresa').AsString);
        fQuery.SQL.Add('order by ');
        fQuery.SQL.Add('  cd.id_certificado; ');
        fQuery.Open;
        if not fQuery.IsEmpty then
        begin
          FfrmCadEmpresa.edid_certificado.Text := fQuery.FieldByName('id_certificado').AsString;
          FfrmCadEmpresa.cbtipo.ItemIndex := fQuery.FieldByName('tipo').AsInteger;
          FfrmCadEmpresa.edvalidade.Date := fQuery.FieldByName('validade').AsDateTime;
          FfrmCadEmpresa.edcaminho_arquivo.Text := fQuery.FieldByName('caminho_arquivo').AsString;
          FfrmCadEmpresa.edsenha.Text := fQuery.FieldByName('senha').AsString;
        end;

        FfrmCadEmpresa.tsEndereco.Enabled := True;
        FfrmCadEmpresa.tsDadosBancarios.Enabled := True;
        FfrmCadEmpresa.tsCertificadoDigital.Enabled := True;
        ShowPopupModal('Popup' + FfrmCadEmpresa.Name);

        Pesquisar;
      end
      else
        raise Exception.Create('Empresa não localizada');
    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'OnClick_Edit',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
    if Assigned(fQuery) then
      FreeAndNil(fQuery);
  end;
end;

procedure TfrmEmpresa.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
    if PrismControl.VCLComponent = DBGrid_Empresa then
    begin
      with PrismControl.AsDBGrid do
      begin
        PrismControl.AsDBGrid.RecordsPerPage := 10;
        with Columns.ColumnByDataField('ATIVO_DESC') do
        begin
	        HTML := '<span class="badge ${value === ''Ativo'' ? ''bg-success'' : ''bg-danger''} rounded-pill p-2" style="width: 5em;">${value}</span>';
        end;

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
            {
            with Add do
            begin
              ButtonModel:= TButtonModel.FilePDF;
            end;
            }
          end;
        end;
      end;
    end;
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

procedure TfrmEmpresa.CellButtonClick(APrismDBGrid: TPrismDBGrid;
  APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer  );
begin
  //inherited CellButtonClick(APrismDBGrid, APrismCellButton, AColIndex, ARow);
  try

      if APrismDBGrid.VCLComponent = DBGrid_Empresa then
      begin
        if APrismCellButton.Identify = TButtonModel.Edit.Identity then
          OnClick_Edit(APrismDBGrid.DataSource.DataSet.FieldByName('ID_EMPRESA').AsInteger,
                       APrismDBGrid.DataSource.DataSet.FieldByName('RAZAO_SOCIAL').AsString);

        if APrismCellButton.Identify = TButtonModel.Delete.Identity then
          OnClick_Delete(APrismDBGrid.DataSource.DataSet.FieldByName('ID_EMPRESA').AsInteger,
                         APrismDBGrid.DataSource.DataSet.FieldByName('RAZAO_SOCIAL').AsString);

        if APrismCellButton.Identify = TButtonModel.Print.Identity then
          OnClick_Print(APrismDBGrid.DataSource.DataSet.FieldByName('ID_EMPRESA').AsInteger,
                        APrismDBGrid.DataSource.DataSet.FieldByName('RAZAO_SOCIAL').AsString);
      end;
  except
    On E:Exception do
    begin
      MessageDlg(E.Message, TMsgDlgType.mtError, [mbok], 0);
      GravarLogJSON(Self.Name,Self.Caption,'CellButtonClick',E);
    end;
  end;
end;

procedure TfrmEmpresa.AjustarColunas(DBGrid: TDBGrid);
var
  i: Integer;
begin
  try
    for i := 0 to DBGrid.Columns.Count - 1 do
    begin
      //SaveLog(DBGrid.Columns[i].FieldName);
      if DBGrid.Columns[i].FieldName = 'ID_EMPRESA' then
        Conf_Coluna_DBGrid(DBGrid,'Id',65,I)
      else if DBGrid.Columns[i].FieldName = 'RAZAO_SOCIAL' then
        Conf_Coluna_DBGrid(DBGrid,'Razão Social',300,I)
      else if DBGrid.Columns[i].FieldName = 'NOME_FANTASIA' then
        Conf_Coluna_DBGrid(DBGrid,'Nome Fantasia',300,I)
      else if DBGrid.Columns[i].FieldName = 'CNPJ' then
        Conf_Coluna_DBGrid(DBGrid,'CNPJ / CPF',200,I)
      else if DBGrid.Columns[i].FieldName = 'INSCRICAO_ESTADUAL' then
        Conf_Coluna_DBGrid(DBGrid,'Insnc. Estadual',150,I)
      else if DBGrid.Columns[i].FieldName = 'INSCRICAO_MUNICIPAL' then
        Conf_Coluna_DBGrid(DBGrid,'Insnc. Municipal',150,I)
      else if DBGrid.Columns[i].FieldName = 'REGIME_TRIBUTARIO' then
        Conf_Coluna_DBGrid(DBGrid,'Regime Tributário',200,I)
      else if DBGrid.Columns[i].FieldName = 'CRT' then
        Conf_Coluna_DBGrid(DBGrid,'Cod. CRT',100,I)
      else if DBGrid.Columns[i].FieldName = 'EMAIL' then
        Conf_Coluna_DBGrid(DBGrid,'E-Mail',300,I)
      else if DBGrid.Columns[i].FieldName = 'TELEFONE' then
        Conf_Coluna_DBGrid(DBGrid,'Telefone',150,I)
      else if DBGrid.Columns[i].FieldName = 'SITE' then
        Conf_Coluna_DBGrid(DBGrid,'Site',300,I)
      else if DBGrid.Columns[i].FieldName = 'DATA_CADASTRO' then
        Conf_Coluna_DBGrid(DBGrid,'Cadastro',150,I)
      else if DBGrid.Columns[i].FieldName = 'ATIVO' then
        Conf_Coluna_DBGrid(DBGrid,'Ativo',65,I,0,False)
      else if DBGrid.Columns[i].FieldName = 'CELULAR' then
        Conf_Coluna_DBGrid(DBGrid,'Celular',150,I)
      else if DBGrid.Columns[i].FieldName = 'CRT_DESC' then
        Conf_Coluna_DBGrid(DBGrid,'Descrição CRT',200,I)
      else if DBGrid.Columns[i].FieldName = 'ATIVO_DESC' then
        Conf_Coluna_DBGrid(DBGrid,'Status',65,I,1);
    end;
  except
    On E:Exception do
      raise Exception.Create(E.Message);
  end;
end;


end.
