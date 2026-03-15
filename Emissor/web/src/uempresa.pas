unit uEmpresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, memds, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateUtils, DBGrids, EditBtn, Menus, ZDataset, ZAbstractRODataset,
  D2Bridge.Forms, Forms, IniFiles, BufDataset, fpjson, DataSet.Serialize,
  RESTRequest4D, jsonparser, uBase.Functions, uDM.ACBr, uPrincipal,
  uCad.Empresa, uBase.DataSets, uDM;

type

  { TfrmEmpresa }

  TfrmEmpresa = class(TfrmPrincipal)
    btNovo: TButton;
    btSelPesquisa: TButton;
    dsRegistro: TDataSource;
    DBGrid_Empresa: TDBGrid;
    edPesquisar: TEdit;
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
    ZMT_Registro: TZMemTable;
    ZMT_Endereco: TZMemTable;
    ZMT_ContaBancaria: TZMemTable;
    ZMT_Certificado: TZMemTable;
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
  FfrmCadEmpresa.Clear_Fields;
  FfrmCadEmpresa.pcPrincipal.ActivePageIndex := 0;
  ShowPopupModal('Popup' + FfrmCadEmpresa.Name);
  Pesquisar;
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

  FJSon_Empresa :TJSONArray;
  FJSon_Endereco :TJSONArray;
  FJSon_ContaBancaria :TJSONArray;
  FJSon_Certificado :TJSONArray;

begin
  try
    try

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
      ZMT_Registro.LoadFromJSON(FJSon_Empresa);
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

procedure TfrmEmpresa.OnClick_Delete(const AId: Integer; const ANome:String);
var
  fResp :IResponse;
  fRet :String;
  fBody :TJSONObject;
begin
  try
    if MessageDlg('Deseja excluir a Empresa selecionada?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      fResp := TRequest.New.BaseURL(FHost)
      	       .AddParam('id',ZMT_Registro.FieldByName('idEmpresa').AsString)
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

    end;
  except
    on E :Exception do
    begin
      GravarLogJSON(Self.Name,Self.Caption,'OnClick_Delete',E);
      MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmEmpresa.OnClick_Print(const AId: Integer; const ANome:String);
begin
  MessageDlg('Imprimindo: ' + AId.ToString + ' - ' + ANome,TMsgDlgType.mtInformation,[mbOK],0);
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
            LCLObj(DBGrid_Empresa);
        end;
      end;
    end;

    with Popup('Popup' + FfrmCadEmpresa.Name,'Cadastro de Empresas',True,CSSClass.Popup.ExtraLarge).Items.Add do
      Nested(FfrmCadEmpresa);
  end;

end;

procedure TfrmEmpresa.OnClick_Edit(const AId: Integer; const ANome:String);
begin
  try
    try
      //Atualizando dados principais...

      FfrmCadEmpresa.Clear_Fields;
      FfrmCadEmpresa.edid_empresa.Text := ZMT_Registro.FieldByName('ID_EMPRESA').AsString;
      FfrmCadEmpresa.edcnpj.Text := ZMT_Registro.FieldByName('CNPJ').AsString;
      FfrmCadEmpresa.edinscricao_estadual.Text := ZMT_Registro.FieldByName('INSCRICAO_ESTADUAL').AsString;
      FfrmCadEmpresa.edinscricao_municipal.Text := ZMT_Registro.FieldByName('INSCRICAO_MUNICIPAL').AsString;
      FfrmCadEmpresa.cbativo.ItemIndex := ZMT_Registro.FieldByName('ATIVO').AsInteger;
      FfrmCadEmpresa.edrazao_social.Text := ZMT_Registro.FieldByName('RAZAO_SOCIAL').AsString;
      FfrmCadEmpresa.ednome_fantasia.Text := ZMT_Registro.FieldByName('NOME_FANTASIA').AsString;
      FfrmCadEmpresa.cbregime_tributario.ItemIndex := FfrmCadEmpresa.cbregime_tributario.Items.IndexOf(ZMT_Registro.FieldByName('REGIME_TRIBUTARIO').AsString);
      FfrmCadEmpresa.edcrt.Text := ZMT_Registro.FieldByName('CRT').AsString;
      FfrmCadEmpresa.edtelefone.Text := ZMT_Registro.FieldByName('TELEFONE').AsString;
      FfrmCadEmpresa.edcelular.Text := ZMT_Registro.FieldByName('CELULAR').AsString;
      FfrmCadEmpresa.edemail.Text := ZMT_Registro.FieldByName('EMAIL').AsString;
      FfrmCadEmpresa.edsite.Text := ZMT_Registro.FieldByName('SITE').AsString;
      {
      if not ZMT_Registro.IsEmpty then
      begin

        FfrmCadEmpresa.Create_DataSet;

        //Endereço...
        FfrmCadEmpresa.memD_Endereco.DisableControls;

        memD_Endereco.Filter := 'idEmpresa = ' + ZQRegistro.FieldByName('idEmpresa').AsString;
        memD_Endereco.Filtered := True;
        memD_Endereco.First;
	while not memD_Endereco.EOF do
        begin
          //Adicionando dados da empresa...
            FfrmCadEmpresa.memD_Endereco.Append;
            FfrmCadEmpresa.memD_Endereco.FieldByName('idEndereco').AsInteger := memD_Endereco.FieldByName('idEndereco').AsInteger;
            FfrmCadEmpresa.memD_Endereco.FieldByName('idEmpresa').AsInteger := memD_Endereco.FieldByName('idEmpresa').AsInteger;
            FfrmCadEmpresa.memD_Endereco.FieldByName('logradouro').AsString := memD_Endereco.FieldByName('logradouro').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('numero').AsString := memD_Endereco.FieldByName('numero').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('complemento').AsString := memD_Endereco.FieldByName('complemento').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('bairro').AsString := memD_Endereco.FieldByName('bairro').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('municipio').AsString := memD_Endereco.FieldByName('municipio').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('codigoMunicipioIbge').AsString := memD_Endereco.FieldByName('codigoMunicipioIbge').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('uf').AsString := memD_Endereco.FieldByName('uf').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('cep').AsString := memD_Endereco.FieldByName('cep').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('pais').AsString := memD_Endereco.FieldByName('pais').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('codigoPaisIbge').AsString := memD_Endereco.FieldByName('codigoPaisIbge').AsString;
            FfrmCadEmpresa.memD_Endereco.FieldByName('tipoEndereco').AsInteger := memD_Endereco.FieldByName('tipoEndereco').AsInteger;
            FfrmCadEmpresa.memD_Endereco.FieldByName('tipoEnderecoDesc').AsString := memD_Endereco.FieldByName('tipoEnderecoDesc').AsString;
            FfrmCadEmpresa.memD_Endereco.Post;

            memD_Endereco.Next;
        end;
        FfrmCadEmpresa.memD_Endereco.EnableControls;

        //Contas bancárias...
        FfrmCadEmpresa.memD_CBanco.DisableControls;
        memD_CBanco.Filter := 'idEmpresa = ' + ZQRegistro.FieldByName('idEmpresa').AsString;
        memD_CBanco.Filtered := True;
        memD_CBanco.First;
        while not memD_CBanco.EOF do
        begin
          FfrmCadEmpresa.memD_CBanco.Append;
          FfrmCadEmpresa.memD_CBanco.FieldByName('idBanco').AsInteger := memD_CBanco.FieldByName('idBanco').AsInteger;
          FfrmCadEmpresa.memD_CBanco.FieldByName('idEmpresa').AsInteger := memD_CBanco.FieldByName('idEmpresa').AsInteger;
          FfrmCadEmpresa.memD_CBanco.FieldByName('banco').AsString := memD_CBanco.FieldByName('banco').AsString;
          FfrmCadEmpresa.memD_CBanco.FieldByName('agencia').AsString := memD_CBanco.FieldByName('agencia').AsString;
          FfrmCadEmpresa.memD_CBanco.FieldByName('conta').AsString := memD_CBanco.FieldByName('conta').AsString;
          FfrmCadEmpresa.memD_CBanco.FieldByName('tipoConta').AsInteger := memD_CBanco.FieldByName('tipoConta').AsInteger;
          FfrmCadEmpresa.memD_CBanco.FieldByName('tipoContaDesc').AsString := memD_CBanco.FieldByName('tipoContaDesc').AsString;
          FfrmCadEmpresa.memD_CBanco.Post;
          memD_CBanco.Next;
        end;
        FfrmCadEmpresa.memD_CBanco.EnableControls;

        //Certificado digital...
        FfrmCadEmpresa.edid_certificado.Text := memD_Certificado.FieldByName('idCertificado').AsString;
        FfrmCadEmpresa.cbtipo.ItemIndex := memD_Certificado.FieldByName('tipo').AsInteger;
        FfrmCadEmpresa.edvalidade.Date := memD_Certificado.FieldByName('validade').AsDateTime;
        FfrmCadEmpresa.edcaminho_arquivo.Text := memD_Certificado.FieldByName('caminhoArquivo').AsString;
        FfrmCadEmpresa.edsenha.Text := memD_Certificado.FieldByName('senha').AsString;;

      end;
      }
      FfrmCadEmpresa.pcPrincipal.ActivePageIndex := 0;
      ShowPopupModal('Popup' + FfrmCadEmpresa.Name);

      //Informações atualizadas pelos registros da tela de cadastro...
      Pesquisar;

    except
      on E :Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'OnClick_Edit',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;
  finally
    //memD_Endereco.Filtered := False;
    //memD_CBanco.Filtered := False;
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
	  //HTML := '<span class="badge ${value === 1 ? ''fa-solid fa-diagram-successor text-success fa-2x'' : ''bg-danger''} rounded-pill p-2" style="width: 7em;">${value}</span>';
	  //HTML := '<span class="badge ${value === 1 ? ''fa-solid fa-circle-check text-success fa-1x'' : ''fa-sharp fa-solid fa-circle-xmark text-danger fa-1x''} rounded-pill p-2" style="width: 7em;">${value}</span>';
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
  if APrismDBGrid.VCLComponent = DBGrid_Empresa then
  begin
    if APrismCellButton.Identify = TButtonModel.Edit.Identity then
      OnClick_Edit(APrismDBGrid.DataSource.DataSet.FieldByName('idEmpresa').AsInteger,
                   APrismDBGrid.DataSource.DataSet.FieldByName('razaoSocial').AsString);

    if APrismCellButton.Identify = TButtonModel.Delete.Identity then
      OnClick_Delete(APrismDBGrid.DataSource.DataSet.FieldByName('idEmpresa').AsInteger,
                     APrismDBGrid.DataSource.DataSet.FieldByName('razaoSocial').AsString);

    if APrismCellButton.Identify = TButtonModel.Print.Identity then
      OnClick_Print(APrismDBGrid.DataSource.DataSet.FieldByName('idEmpresa').AsInteger,
                    APrismDBGrid.DataSource.DataSet.FieldByName('razaoSocial').AsString);
  end;
end;

procedure TfrmEmpresa.AjustarColunas(DBGrid: TDBGrid);
var
  i: Integer;
begin
  try
    for i := 0 to DBGrid.Columns.Count - 1 do
    begin
      SaveLog(DBGrid.Columns[i].FieldName);
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
