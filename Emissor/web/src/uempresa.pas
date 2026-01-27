unit uEmpresa;

{ Copyright 2026 / 2027 D2Bridge Framework by Talis Jonatas Gomes }

interface

uses
  Classes, SysUtils, DB, memds, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DateUtils,
  DBGrids, EditBtn, Menus, ZDataset, D2Bridge.Forms, Forms, IniFiles,
  fpjson,
  DataSet.Serialize,
  RESTRequest4D,
  jsonparser,
  uBase.Functions, uDM.ACBr,
  uPrincipal, uCad.Empresa;

type

  { TfrmEmpresa }

  TfrmEmpresa = class(TfrmPrincipal)
    btNovo: TButton;
    btSelPesquisa: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    edPesquisar: TEdit;
    mdRegistro: TMemDataset;
    mdRegistroativo: TLongintField;
    mdRegistrocelular: TStringField;
    mdRegistrocnpj: TStringField;
    mdRegistrocrt: TStringField;
    mdRegistrodata_cadastro: TDateTimeField;
    mdRegistroemail: TStringField;
    mdRegistroid_empresa: TLongintField;
    mdRegistroinscricao_estadual: TStringField;
    mdRegistroinscricao_municipal: TStringField;
    mdRegistronome_fantasia: TStringField;
    mdRegistrorazao_social: TStringField;
    mdRegistroregime_tributario: TStringField;
    mdRegistrosite: TStringField;
    mdRegistrotelefone: TStringField;
    miCNPJ_CPF: TMenuItem;
    miNomeFantasia: TMenuItem;
    miRazaoSocial: TMenuItem;
    miId: TMenuItem;
    pnHeader: TPanel;
    pnDetail: TPanel;
    pnFooter: TPanel;
    pnFiltro: TPanel;
    pnTipoFiltro2: TPanel;
    pMenu_Filtro: TPopupMenu;
    procedure btNovoClick(Sender: TObject);
    procedure edPesquisarKeyPress(Sender: TObject; var Key: char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miRazaoSocialClick(Sender: TObject);
  private
    { Private declarations }
    FfrmCadEmpresa :TfrmCadEmpresa;

    FHost :String;
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

    if Trim(FHost) = '' then
      raise Exception.Create('Host de acesso ao servidor não informado.');


    except
      on E :Exception do
      	 MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
    end;
  finally
  end;


  (*Esse é um exemplo...

  MemDataset.Close;
  mdRegistro.Open;;

  mdRegistro.Insert;
  MemDatasetidEmpresa.AsInteger := 1;
  MemDatasetNome.AsString := 'MARCOS';
  mdRegistro.Post;

  mdRegistro.Insert;
  MemDatasetidEmpresa.AsInteger := 2;
  MemDatasetNome.AsString := 'SIMONE';
  mdRegistro.Post;

  mdRegistro.Insert;
  MemDatasetidEmpresa.AsInteger := 3;
  MemDatasetNome.AsString := 'GABRIEL';
  mdRegistro.Post;

  mdRegistro.Insert;
  MemDatasetidEmpresa.AsInteger := 4;
  MemDatasetNome.AsString := 'NICOLAS';
  mdRegistro.Post;
  *)
end;

procedure TfrmEmpresa.FormShow(Sender: TObject);
begin
  Pesquisar;
end;

procedure TfrmEmpresa.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FreeAndNil(FIniFile);
end;

procedure TfrmEmpresa.edPesquisarKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    Pesquisar;
end;

procedure TfrmEmpresa.btNovoClick(Sender: TObject);
begin
  ShowPopupModal('Popup' + FfrmCadEmpresa.Name);
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
  FDados :TJSONArray;
  FTipoPesquisa:String;
  x:Integer;

  FId :Integer;
begin
  try
    try
      mdRegistro.DisableControls;

      mdRegistro.Close;
      mdRegistro.Open;
      if not mdRegistro.IsEmpty then
      begin
        mdRegistro.First;
        while not mdRegistro.EOF do
	  mdRegistro.Delete;
      end;

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

      FDados := TJSONArray(GetJSON(FBody['data'].AsJSON));

      PopularMemDataDoJSON(FDados,mdRegistro);

    except on E: Exception do
      MessageDlg(E.Message,TMsgDlgType.mtError,[TMsgDlgBtn.mbOK],0);
    end;
  finally
    mdRegistro.EnableControls;
  end;
end;


procedure TfrmEmpresa.OnClick_Delete(const AId: Integer; const ANome:String);
begin
  MessageDlg('Excluirndo: ' + AId.ToString + ' - ' + ANome,TMsgDlgType.mtInformation,[mbOK],0);
end;

procedure TfrmEmpresa.OnClick_Print(const AId: Integer; const ANome:String);
begin
  MessageDlg('Imprimindo: ' + AId.ToString + ' - ' + ANome,TMsgDlgType.mtInformation,[mbOK],0);
end;

procedure TfrmEmpresa.ExportD2Bridge;
begin
  inherited;

  Title := 'Empresas';

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
            LCLObj(DBGrid);
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
    FfrmCadEmpresa.Clear_Fields;
    FfrmCadEmpresa.edid_empresa.Text := mdRegistroid_empresa.AsString;
    FfrmCadEmpresa.edcnpj.Text := mdRegistrocnpj.AsString;
    FfrmCadEmpresa.edinscricao_estadual.Text := mdRegistroinscricao_estadual.AsString;
    FfrmCadEmpresa.edinscricao_municipal.Text := mdRegistroinscricao_municipal.AsString;
    FfrmCadEmpresa.cbativo.ItemIndex := mdRegistroativo.AsInteger;
    FfrmCadEmpresa.edrazao_social.Text := mdRegistrorazao_social.AsString;
    FfrmCadEmpresa.ednome_fantasia.Text := mdRegistronome_fantasia.AsString;
    FfrmCadEmpresa.cbregime_tributario.ItemIndex := FfrmCadEmpresa.cbregime_tributario.Items.IndexOf(mdRegistroregime_tributario.AsString);
    FfrmCadEmpresa.edcrt.Text := mdRegistrocrt.AsString;
    FfrmCadEmpresa.edtelefone.Text := mdRegistrotelefone.AsString;
    FfrmCadEmpresa.edcelular.Text := mdRegistrocelular.AsString;
    FfrmCadEmpresa.edemail.Text := mdRegistroemail.AsString;
    FfrmCadEmpresa.edsite.Text := mdRegistrosite.AsString;

    ShowPopupModal('Popup' + FfrmCadEmpresa.Name);

    Pesquisar;

  except
    on E :Exception do
       MessageDlg(E.Message,TMsgDlgType.mtError,[mbOK],0);
  end;
end;

procedure TfrmEmpresa.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
    if PrismControl.VCLComponent = DBGrid then
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
  if APrismDBGrid.VCLComponent = DBGrid then
  begin
    if APrismCellButton.Identify = TButtonModel.Edit.Identity then
      OnClick_Edit(APrismDBGrid.DataSource.DataSet.FieldByName('id_empresa').AsInteger,
                   APrismDBGrid.DataSource.DataSet.FieldByName('razao_social').AsString);

    if APrismCellButton.Identify = TButtonModel.Delete.Identity then
      OnClick_Delete(APrismDBGrid.DataSource.DataSet.FieldByName('id_empresa').AsInteger,
                     APrismDBGrid.DataSource.DataSet.FieldByName('razao_social').AsString);

    if APrismCellButton.Identify = TButtonModel.Print.Identity then
      OnClick_Print(APrismDBGrid.DataSource.DataSet.FieldByName('id_empresa').AsInteger,
                    APrismDBGrid.DataSource.DataSet.FieldByName('razao_social').AsString);
  end;
end;

end.
