unit Unit_Login;

{$MODE delphi}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Dialogs, ComCtrls, Menus,
  ExtCtrls, StdCtrls, ACBrValidador, IniFiles, D2Bridge.Forms,
  fpjson,
  DataSet.Serialize,
  RESTRequest4D,
  jsonparser,
  uCad.Empresa,
  uCad.Usuario,
  uBase.Functions,
  uCripto_Descrito, uDM.ACBr; //Declare D2Bridge.Forms always in the last unit

type

  { TForm_Login }

  TForm_Login = class(TD2BridgeForm)
   Button_ShowPass: TButton;
   btCadEmpresa: TButton;
   btCadUsuario: TButton;
   edCNPJ: TEdit;
    Panel1: TPanel;
    Image_Logo: TImage;
    Label_Login: TLabel;
    Edit_UserName: TEdit;
    Edit_Password: TEdit;
    Button_Login: TButton;
    Image_BackGround: TImage;
    procedure btCadEmpresaClick(Sender: TObject);
    procedure btCadUsuarioClick(Sender: TObject);
    procedure Button_LoginClick(Sender: TObject);
    procedure Button_ShowPassClick(Sender: TObject);
    procedure edCNPJExit(Sender: TObject);
    procedure edCNPJKeyPress(Sender: TObject; var Key: char);
    procedure Edit_PasswordKeyPress(Sender: TObject; var Key: char);
    procedure Edit_UserNameKeyPress(Sender: TObject; var Key: char);
  private
    fDM_ACBr :TDM_Acbr;
    FfrmCadEmpresa :TfrmCadEmpresa;
    FfrmCad_Usuario :TfrmCad_Usuario;
    function Confere_Doc_Existe(const aDocumento: String):Boolean;

  public

  protected
   procedure ExportD2Bridge; override;
   procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
   procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
  end;

Function Form_Login: TForm_Login;

implementation

Uses
   EmissorWebApp, uPrincipal, uEmpresa;

Function Form_Login: TForm_Login;
begin
 Result:= TForm_Login(TForm_Login.GetInstance);
end;

{$R *.lfm}

{ TForm_Login }

procedure TForm_Login.Button_LoginClick(Sender: TObject);
var
  FIniFile :TIniFile;
  FHost :String;
  FResp :IResponse;
  FRet :String;
  FRetorno :TJSONObject;
  FDados :TJSONObject;
  FJSon :TJSONObject;
begin
  //Validando campos...
  try
   try
     FHost := '';
     FIniFile := TIniFile.Create(ConfigFile);
     FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');
     //http://localhost:9095/login
     if Trim(FHost) = '' then
       raise Exception.Create('Host de acesso ao servidor não informado.');

     FJSon := TJSONObject.Create;
     FJSon.Add('cnpj',edCNPJ.Text);
     FJSon.Add('usuario',Edit_UserName.Text);
     FJSon.Add('password', Criptografar(Edit_Password.Text));

     FResp := TRequest.New.BaseURL(FHost)
     	   .Resource('login')
           .AddBody(FJSon)
           .Accept('application/json')
           .Post;

     FRet := '';
     FRet := FResp.Content;
     FRetorno := TJSONObject(GetJSON(FRet));
     if FRetorno['success'].AsBoolean = False then
       raise Exception.Create(FRetorno['message'].AsString);

     FDados := TJSONObject(GetJSON(FRetorno['data'].AsJSON));
     Emissor.CNPJ_Empresa := edCNPJ.Text;
     Emissor.ID_Usuario := FDados['idUsuario'].AsInteger;
     Emissor.Nome_Usuario := FDados['nome'].AsString;
     Emissor.Login_Usuario := FDados['login'].AsString;
     Emissor.Usuario_Ativo := FDados['ativo'].AsInteger;
     Emissor.Token_Server := FDados['token'].AsString;

     if frmPrincipal = nil then
       TfrmPrincipal.CreateInstance;
     frmPrincipal.Show;

   except
     On E:Exception do
     begin
       MessageDlg(E.Message, TMsgDlgType.mtError, [mbok], 0);
       SaveLog(E.Message);
     end;
   end;

  finally
    FreeAndNil(FJSon);
    FreeAndNil(FIniFile);
  end;
end;

procedure TForm_Login.btCadEmpresaClick(Sender: TObject);
begin
  FfrmCadEmpresa.Clear_Fields;
  FfrmCadEmpresa.pcPrincipal.ActivePageIndex := 0;
  FfrmCadEmpresa.cbativo.ItemIndex := 1;
  ShowPopupModal('Popup' + FfrmCadEmpresa.Name);
  edCNPJ.Text := Emissor.Empresa_Fields.cnpj;
end;

procedure TForm_Login.btCadUsuarioClick(Sender: TObject);
begin
 FfrmCad_Usuario.cbativo.ItemIndex := 1;
 Emissor.Usuario_Fields.id_empresa := 1;
 ShowPopupModal('Popup' + FfrmCad_Usuario.Name);
 Edit_UserName.Text := Emissor.Login_Usuario;
 Edit_UserName.Enabled := True;
 Edit_Password.Enabled := True;
 btCadUsuario.Enabled := False;
end;

procedure TForm_Login.Button_ShowPassClick(Sender: TObject);
begin
 if Edit_Password.PasswordChar = '*' then
 begin
  Edit_Password.PasswordChar:= #0;

  if IsD2BridgeContext then
   D2Bridge.PrismControlFromVCLObj(Edit_Password).AsEdit.DataType:= TPrismFieldType.PrismFieldTypeString;
 end else
 begin
  Edit_Password.PasswordChar:= '*';

  if IsD2BridgeContext then
   D2Bridge.PrismControlFromVCLObj(Edit_Password).AsEdit.DataType:= TPrismFieldType.PrismFieldTypePassword;
 end;
end;

procedure TForm_Login.edCNPJExit(Sender: TObject);
//var
//  fDocumento :String;
begin
 (*
  try
    try

      fDM_ACBr := TDM_Acbr.Create(Nil);

      if Trim(edCNPJ.Text) = '' then
        Exit;

      fDocumento := '';
      fDocumento := RemoverMascara(edCNPJ.Text);

      case Length(fDocumento) of
        11:fDM_ACBr.ACBrValidador.TipoDocto := docCPF;
        14:fDM_ACBr.ACBrValidador.TipoDocto := docCNPJ;
        else
	  raise Exception.Create('Documento inválido');
      end;

      fDM_ACBr.ACBrValidador.Documento := edCNPJ.Text;
      if fDM_ACBr.ACBrValidador.Validar then
        edCNPJ.Text := fDM_ACBr.ACBrValidador.Formatar
      else
        raise Exception.Create('Documento inválido');

      //Verifica se o documento [CNPJ/CPF] já está cadastrado e se já possui usuários...
      Confere_Doc_Existe(edCNPJ.Text);

      if Edit_UserName.CanFocus then
        Edit_UserName.SetFocus;

    finally
      //FreeAndNil(fDM_ACBr);
    end;
  except
    on E :Exception do
    begin
      SaveLog(Self.Caption + ' [' + Self.Name + '] ' + sLineBreak + '    Valida Documento: ' + E.Message);
      MessageDlg(E.Message,TMsgDlgType.mtWarning,[mbOK],0);
    end;
  end;
  *)
end;

function TForm_Login.Confere_Doc_Existe(const aDocumento:String):Boolean;
var
  FIniFile :TIniFile;
  FHost :String;
  FResp :IResponse;
  FRet :String;
  FRetorno :TJSONObject;
  FDados :TJSONArray;
  fDocumento :String;
begin
  try
    try
     Result := True;

     fDM_ACBr := TDM_Acbr.Create(Nil);

     if Trim(edCNPJ.Text) = '' then
     begin
       Result := False;
       Exit;
     end;

     fDocumento := '';
     fDocumento := RemoverMascara(edCNPJ.Text);

     case Length(fDocumento) of
       11:fDM_ACBr.ACBrValidador.TipoDocto := docCPF;
       14:fDM_ACBr.ACBrValidador.TipoDocto := docCNPJ;
       else
         raise Exception.Create('Documento inválido');
     end;

     fDM_ACBr.ACBrValidador.Documento := edCNPJ.Text;
     if fDM_ACBr.ACBrValidador.Validar then
       edCNPJ.Text := fDM_ACBr.ACBrValidador.Formatar
     else
       raise Exception.Create('Documento inválido');


     FHost := '';
     FIniFile := TIniFile.Create(ConfigFile);
     FHost := FIniFile.ReadString('SERVER','HOST','') + ':' + FIniFile.ReadString('SERVER','PORT','');
     if Trim(FHost) = '' then
       raise Exception.Create('Host de acesso ao servidor não informado.');

     FResp := TRequest.New.BaseURL(FHost)
     	        .AddParam('cnpj',RemoverMascara(edCNPJ.Text))
     	        .Resource('empresa/validaCnpj')
                .Accept('application/json')
                .Get;

     FRet := '';
     FRet := FResp.Content;
     FRetorno := TJSONObject(GetJSON(FRet));
     if FRetorno['success'].AsBoolean = False then
       raise Exception.Create(FRetorno['message'].AsString);


     FDados := TJSONArray(GetJSON(FRetorno['data'].AsJSON));

     if FDados.Count = 0 then
       raise Exception.Create('Empresa não cadastrada');

     with Emissor.Empresa_Fields do
     begin
       id_empresa := FDados.Objects[0].Integers['idEmpresa'];
       razao_social := FDados.Objects[0].Strings['razaoSocial'];
       nome_fantasia := FDados.Objects[0].Strings['nomeFantasia'];
       cnpj := FDados.Objects[0].Strings['cnpj'];
       inscricao_estadual := FDados.Objects[0].Strings['inscricaoEstadual'];
       inscricao_municipal := FDados.Objects[0].Strings['inscricaoMunicipal'];
       regime_tributario := FDados.Objects[0].Strings['regimeTributario'];
       crt := FDados.Objects[0].Strings['crt'];
       email := FDados.Objects[0].Strings['email'];
       telefone := FDados.Objects[0].Strings['telefone'];
       site := FDados.Objects[0].Strings['site'];
       data_cadastro := StrISOToDateTime(FDados.Objects[0].Strings['dataCadastro']);
       ativo := FDados.Objects[0].Integers['ativo'];
       celular := FDados.Objects[0].Strings['celular'];
     end;

     Edit_UserName.Enabled := (FDados.Objects[0].Integers['qtdUser'] > 0);
     Edit_Password.Enabled := (FDados.Objects[0].Integers['qtdUser'] > 0);
     btCadUsuario.Enabled := (FDados.Objects[0].Integers['qtdUser'] = 0);

    except
      on E:Exception do
      begin
        Result := False;
        SaveLog(Self.Caption + ' [' + Self.Name + '] ' + sLineBreak + '    Valida Documento: ' + E.Message);
        MessageDlg(E.Message,TMsgDlgType.mtWarning,[mbOK],0);
      end;
    end;

  finally
    FreeAndNil(fDM_ACBr);
    FreeAndNil(FIniFile);
  end;
end;

procedure TForm_Login.edCNPJKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Confere_Doc_Existe(edCNPJ.Text) then
    begin
      //PularCompoenente(Edit_UserName)
      if Edit_UserName.CanFocus then
      	 Edit_UserName.SetFocus;
    end;
  end;
end;

procedure TForm_Login.Edit_PasswordKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Button_Login.CanFocus then
       Button_Login.SetFocus;

  end;
end;

procedure TForm_Login.Edit_UserNameKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    if Edit_Password.CanFocus then
      Edit_Password.SetFocus;
  end;
end;

procedure TForm_Login.ExportD2Bridge;
begin
 inherited;

 Title:= 'Emissor';
 SubTitle:= 'Acessar o Sistema';

 //Cadastro de Empresa...
 FfrmCadEmpresa := TfrmCadEmpresa.Create(Self);
 D2Bridge.AddNested(FfrmCadEmpresa);

 //Cadastro de usuário...
 FfrmCad_Usuario := TfrmCad_Usuario.Create(Self);
 D2Bridge.AddNested(FfrmCad_Usuario);

 //Background color
 D2Bridge.HTML.Render.BodyStyle:= 'background-color: #f0f0f0;';

 //TemplateClassForm:= TD2BridgeFormTemplate;
 D2Bridge.FrameworkExportType.TemplateMasterHTMLFile:= '';
 D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

 //Export yours Controls
 with D2Bridge.Items.add do
 begin
  //Image Backgroup Full Size *Use also ImageFromURL...
  ImageFromTImage(Image_BackGround, CSSClass.Image.Image_BG20_FullSize);

  with Card do
  begin
   CSSClasses:= CSSClass.Card.Card_Center;

   ImageICOFromTImage(Image_Logo, CSSClass.Col.ColSize4);

   with BodyItems.Add do
   begin
    with Row.Items.Add do
      Col.Add.LCLObj(Label_Login);

    with Row.Items.Add do
    begin
      with Col.Items.Add do
      begin
        With FormGroup('',CSSClass.Col.colsize12).Items.Add do
        begin
          LCLObj(edCNPJ, 'ValidationLogin', true);
          LCLObj(btCadEmpresa, CSSClass.Button.add);
        end;
      end;
    end;

    with Row.Items.Add do
    begin
      with Col.Items.Add() do
      begin
        With FormGroup('',CSSClass.Col.colsize12).Items.Add do
        begin
          LCLObj(Edit_UserName, 'ValidationLogin', true);
          LCLObj(btCadUsuario, CSSClass.Button.add);
        end;
      end;
    end;

    with Row.Items.Add do
    begin
      with Col.Items.add do //Example Edit + Button same row and col
      begin
        With FormGroup('',CSSClass.Col.colsize12).Items.Add do
        begin
          LCLObj(Edit_Password, 'ValidationLogin', true);
          LCLObj(Button_ShowPass, CSSClass.Button.view);
        end;
      end;
    end;

    with Row.Items.Add do
     Col.Add.VCLObj(Button_Login, 'ValidationLogin', false, CSSClass.Col.colsize12);

   end;

  end;

  //Cadastro de empresa...
  with Popup('Popup' + FfrmCadEmpresa.Name,'Cadastro de Empresas',True,CSSClass.Popup.ExtraLarge).Items.Add do
    Nested(FfrmCadEmpresa);
  //Cadastro de usuário...
  with Popup('Popup' + FfrmCad_Usuario.Name,'Cadastro de Usuários',True,CSSClass.Popup.ExtraLarge).Items.Add do
    Nested(FfrmCad_Usuario);

 end;
end;

procedure TForm_Login.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
 inherited;

end;

procedure TForm_Login.RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string);
begin
 inherited;
 {
  if PrismControl.VCLComponent = Edit1 then
  begin
   HTMLControl:= '</>';
  end;
 }
end;

end.
