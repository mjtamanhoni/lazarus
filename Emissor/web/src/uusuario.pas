unit uUsuario;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, memds, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DateUtils,
  DBGrids, EditBtn, Menus, ZDataset, D2Bridge.Forms, Forms, IniFiles, BufDataset,
  fpjson,
  DataSet.Serialize,
  RESTRequest4D,
  jsonparser,
  uBase.Functions, uDM.ACBr,
  uPrincipal, uCad.Usuario, uBase.DataSets,uCripto_Descrito;

type

  { TfrmUsuario }

  TfrmUsuario = class(TfrmPrincipal)
    btNovo: TButton;
    btSelPesquisa: TButton;
    DBGrid_Usuario: TDBGrid;
    dsRegistro: TDataSource;
    edPesquisar: TEdit;
    miNome: TMenuItem;
    miID: TMenuItem;
    miLogin: TMenuItem;
    pmPesquisar: TPopupMenu;
    pnDetail: TPanel;
    pnFiltro: TPanel;
    pnFooter: TPanel;
    pnHeader: TPanel;
    pnTipoFiltro2: TPanel;
    procedure btNovoClick(Sender: TObject);
    procedure edPesquisarKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miIDClick(Sender: TObject);
  private
    { Private declarations }
    FfrmCad_Usuario :TfrmCad_Usuario;

    FHost :String;
    FIniFile :TIniFile;

    memD_Ususario :TBufDataset;

    procedure Create_DataSet;
    procedure Adiciona_Dados(const AJSon_Usuario:TJSONArray);

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

function frmUsuario: TfrmUsuario;

implementation

uses
  EmissorWebApp;

{$R *.lfm}

function frmUsuario: TfrmUsuario;
begin
  result := (TfrmUsuario.GetInstance as TfrmUsuario);
end;

procedure TfrmUsuario.FormCreate(Sender: TObject);
begin
  try
    try
    FHost := '';
    FIniFile := TIniFile.Create(ConfigFile);
    FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');
    if Trim(FHost) = '' then
      raise Exception.Create('Host de acesso ao servidor não informado.');

    //CriaDataset_Empresa;
    memD_Ususario := TBufDataset.Create(Self);

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

procedure TfrmUsuario.edPesquisarKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Pesquisar;
end;

procedure TfrmUsuario.btNovoClick(Sender: TObject);
begin
  FfrmCad_Usuario.Clear_Fields;
  FfrmCad_Usuario.cbativo.ItemIndex := 1;
  ShowPopupModal('Popup' + FfrmCad_Usuario.Name);
  Pesquisar;
end;

procedure TfrmUsuario.FormDestroy(Sender: TObject);
begin
  FreeAndNil(memD_Ususario);
  FreeAndNil(FIniFile);
end;

procedure TfrmUsuario.FormShow(Sender: TObject);
begin
  Pesquisar;
end;

procedure TfrmUsuario.miIDClick(Sender: TObject);
begin
  edPesquisar.Tag := TMenuItem(Sender).Tag;
  case TMenuItem(Sender).Tag of
    0:edPesquisar.TextHint := 'Pesquisar pelo ID do Usuário';
    1:edPesquisar.TextHint := 'Pesquisar peloa Login do Usuário';
    2:edPesquisar.TextHint := 'Pesquisar pelo Nome do Usuário';
  end;
  Pesquisar;
end;

procedure TfrmUsuario.Create_DataSet;
var
  FUsuario :TUsuario;
begin
  FUsuario := TUsuario.Create;
  try
    try
      //Criando bufdataset - Empresa
      FUsuario.Criar_DataSet_Usuario(memD_Ususario);
      dsRegistro.DataSet := memD_Ususario;
      DBGrid_Usuario.DataSource := dsRegistro;

      //Configurando grid...
      ConfigColGridAut(DBGrid_Usuario,memD_Ususario);

      //Escondendo colunas....
      DBGrid_Usuario.Columns.Items[DBGrid_Usuario.SelectedIndex].Field.DataSet.FieldByName('senha').Visible := False;
      DBGrid_Usuario.Columns.Items[DBGrid_Usuario.SelectedIndex].Field.DataSet.FieldByName('idPerfil').Visible := False;
      DBGrid_Usuario.Columns.Items[DBGrid_Usuario.SelectedIndex].Field.DataSet.FieldByName('idEmpresa').Visible := False;

    except
      on E:Exception do
      begin
        GravarLogJSON(Self.Name,Self.Caption,'Create_DataSet',E);
        MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
      end;
    end;

  finally
    FreeAndNil(FUsuario)
  end;
end;

procedure TfrmUsuario.Adiciona_Dados(const AJSon_Usuario: TJSONArray);
var
  I :Integer;
begin
  try
    try
      memD_Ususario.DisableControls;

      if AJSon_Usuario.Count = 0 then
        raise Exception.Create('Não há empresas para listar.');

      if not memD_Ususario.Active then
        Create_DataSet;

      //Adicionando dados da empresa...
      for I := 0 to Pred(AJSon_Usuario.Count) do
      begin
        memD_Ususario.Append;
        memD_Ususario.FieldByName('idUsuario').AsInteger := AJSon_Usuario.Objects[I].Integers['idUsuario'];
        memD_Ususario.FieldByName('login').AsString := AJSon_Usuario.Objects[I].Strings['login'];
        memD_Ususario.FieldByName('senha').AsString := Descriptografar(AJSon_Usuario.Objects[I].Strings['senha']);
        memD_Ususario.FieldByName('nome').AsString := AJSon_Usuario.Objects[I].Strings['nome'];
        memD_Ususario.FieldByName('email').AsString := AJSon_Usuario.Objects[I].Strings['email'];
        memD_Ususario.FieldByName('ativo').AsInteger := AJSon_Usuario.Objects[I].Integers['ativo'];
        memD_Ususario.FieldByName('dataCadastro').AsDateTime := ISO8601ToDateDef(AJSon_Usuario.Objects[I].Strings['dataCadastro'],0);
        if not AJSon_Usuario.Objects[I].Nulls['ultimoAcesso'] then
          memD_Ususario.FieldByName('ultimoAcesso').AsDateTime := ISO8601ToDateDef(AJSon_Usuario.Objects[I].Strings['ultimoAcesso'],0);
        memD_Ususario.FieldByName('idPerfil').AsInteger := AJSon_Usuario.Objects[I].Integers['idPerfil'];
        memD_Ususario.FieldByName('idEmpresa').AsInteger := AJSon_Usuario.Objects[I].Integers['idEmpresa'];
        memD_Ususario.FieldByName('cnpj').AsString := AJSon_Usuario.Objects[I].Strings['cnpj'];
        memD_Ususario.FieldByName('razaoSocial').AsString := AJSon_Usuario.Objects[I].Strings['razaoSocial'];
        memD_Ususario.FieldByName('nomeFantasia').AsString := AJSon_Usuario.Objects[I].Strings['nomeFantasia'];
        memD_Ususario.FieldByName('nomePerfil').AsString := AJSon_Usuario.Objects[I].Strings['nomePerfil'];
        memD_Ususario.FieldByName('descricaoPerfil').AsString := AJSon_Usuario.Objects[I].Strings['descricaoPerfil'];
        memD_Ususario.Post;
      end;
    except
      on E:Exception do
        raise Exception.Create('Adiciona dados da empresa: ' + sLineBreak + E.Message);
    end;
  finally
    memD_Ususario.EnableControls;
  end;
end;

procedure TfrmUsuario.Pesquisar;
var
  FResp :IResponse;
  FRet :String;
  FBody :TJSONObject;
  FJSon_Usuario :TJSONArray;
  FTipoPesquisa:String;
  x:Integer;

  FId :Integer;
begin
  try
    try
      Create_DataSet;

      FTipoPesquisa := '';
      case edPesquisar.Tag of
        0:begin
          if not ApenasNumeros(edPesquisar.Text) then
            raise Exception.Create('Para realizar o filtro usando o ID,  não pode haver letras no texto da pesquisa');
          FTipoPesquisa := 'id';
        end;
        1:FTipoPesquisa := 'login';
        2:FTipoPesquisa := 'nome';
      end;

      if Trim(FHost) = '' then
        raise Exception.Create('Host não informado');

      if Trim(FTipoPesquisa) <> '' then
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .AddParam(FTipoPesquisa,edPesquisar.Text)
                 .Resource('usuario')
                 .Accept('application/json')
                 .Get;
      end
      else
      begin
        FResp := TRequest.New.BaseURL(FHost)
                 .Resource('usuario')
                 .Accept('application/json')
                 .Get;
      end;

      FRet := '';
      FRet := FResp.Content;
      FBody := TJSONObject(GetJSON(FRet));
      if FBody['success'].AsBoolean = False then
        raise Exception.Create(FBody['message'].AsString);

      FJSon_Usuario := TJSONArray(GetJSON(FBody['data'].AsJSON));

      //Inserindo informações...
      Adiciona_Dados(FJSon_Usuario);

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

procedure TfrmUsuario.OnClick_Edit(const AId: Integer; const ANome: String);
begin
  try
    try
      //Atualizando dados principais...
      FfrmCad_Usuario.Clear_Fields;
      FfrmCad_Usuario.edidUsuario.Text := memD_Ususario.FieldByName('idUsuario').AsString;
      FfrmCad_Usuario.edlogin.Text := memD_Ususario.FieldByName('login').AsString;
      FfrmCad_Usuario.edsenha.Text := memD_Ususario.FieldByName('senha').AsString;
      FfrmCad_Usuario.ednome.Text := memD_Ususario.FieldByName('nome').AsString;
      FfrmCad_Usuario.cbativo.ItemIndex := memD_Ususario.FieldByName('ativo').AsInteger;
      FfrmCad_Usuario.edemail.Text := memD_Ususario.FieldByName('email').AsString;
      FfrmCad_Usuario.edidPerfil.Text := memD_Ususario.FieldByName('idPerfil').AsString;
      FfrmCad_Usuario.edidPerfil_Desc.Text := memD_Ususario.FieldByName('nomePerfil').AsString;
      ShowPopupModal('Popup' + FfrmCad_Usuario.Name);

      Pesquisar;

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

procedure TfrmUsuario.OnClick_Delete(const AId: Integer; const ANome: String);
var
  fResp :IResponse;
  fRet :String;
  fBody :TJSONObject;
begin
  try
    if MessageDlg('Deseja excluir o usuário selecionada?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      fResp := TRequest.New.BaseURL(FHost)
               .AddParam('idEmpresa',memD_Ususario.FieldByName('idEmpresa').AsString)
      	       .AddParam('idUsuario',memD_Ususario.FieldByName('idUsuario').AsString)
               .Resource('usuario')
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

procedure TfrmUsuario.OnClick_Print(const AId: Integer; const ANome: String);
begin
  //
end;

procedure TfrmUsuario.ExportD2Bridge;
begin
  inherited;

  Title := Self.Caption;

  //TemplateClassForm:= TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  //Formulário de cadastro de empresas....
  FfrmCad_Usuario := TfrmCad_Usuario.Create(Self);
  D2Bridge.AddNested(FfrmCad_Usuario);

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
            LCLObj(DBGrid_Usuario);
        end;
      end;
    end;

    with Popup('Popup' + FfrmCad_Usuario.Name,'Cadastro de Usuários',True,CSSClass.Popup.ExtraLarge).Items.Add do
      Nested(FfrmCad_Usuario);
  end;

end;

procedure TfrmUsuario.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  if PrismControl.VCLComponent = DBGrid_Usuario then
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

procedure TfrmUsuario.CellButtonClick(APrismDBGrid: TPrismDBGrid;
  APrismCellButton: TPrismDBGridColumnButton; AColIndex: Integer; ARow: Integer
  );
begin
  //inherited CellButtonClick(APrismDBGrid, APrismCellButton, AColIndex, ARow);
  if APrismDBGrid.VCLComponent = DBGrid_Usuario then
  begin
    if APrismCellButton.Identify = TButtonModel.Edit.Identity then
      OnClick_Edit(APrismDBGrid.DataSource.DataSet.FieldByName('idusuario').AsInteger,
                   APrismDBGrid.DataSource.DataSet.FieldByName('nome').AsString);

    if APrismCellButton.Identify = TButtonModel.Delete.Identity then
      OnClick_Delete(APrismDBGrid.DataSource.DataSet.FieldByName('idUsuario').AsInteger,
                     APrismDBGrid.DataSource.DataSet.FieldByName('nome').AsString);

    if APrismCellButton.Identify = TButtonModel.Print.Identity then
      OnClick_Print(APrismDBGrid.DataSource.DataSet.FieldByName('idUsuario').AsInteger,
                    APrismDBGrid.DataSource.DataSet.FieldByName('nome').AsString);
  end;

end;

end.
