unit Unit_Login;

{$MODE delphi}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Dialogs, ComCtrls,Menus, ExtCtrls, StdCtrls, IniFiles,
  D2Bridge.Forms,
  fpjson, DataSet.Serialize, RESTRequest4D, jsonparser,
  uBase.Functions, uCripto_Descrito; //Declare D2Bridge.Forms always in the last unit

type

  { TForm_Login }

  TForm_Login = class(TD2BridgeForm)
   Button_ShowPass: TButton;
   edCNPJ: TEdit;
    Panel1: TPanel;
    Image_Logo: TImage;
    Label_Login: TLabel;
    Edit_UserName: TEdit;
    Edit_Password: TEdit;
    Button_Login: TButton;
    Image_BackGround: TImage;
    procedure Button_LoginClick(Sender: TObject);
    procedure Button_ShowPassClick(Sender: TObject);
  private

  public

  protected
   procedure ExportD2Bridge; override;
   procedure InitControlsD2Bridge(const PrismControl: TPrismControl); override;
   procedure RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string); override;
  end;

Function Form_Login: TForm_Login;

implementation

Uses
   EmissorWebApp, Unit1, uPrincipal;

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
  (*
  {
      "success": true,
      "data": {
          "idUsuario": 1,
          "login": "mjtamanhoni@gmail.com",
          "senha": "iTo6MMPDdi5iODIyIDQyV3tXgzE5NzSJOjoww8N2LmI4MTIgNDJXe1eD",
          "nome": "MARCOS JOSÉ TAMANHONI",
          "email": "mjtamanhoni@gmail.com",
          "ativo": 1,
          "dataCadastro": "2026-01-20T17:30:16.598Z",
          "ultimoAcesso": null,
          "idPerfil": null,
          "token": "eyJhbGciOiAiSFMyNTYiLCAidHlwIjogIkpXVCJ9.eyAiZXhwIiA6IDE3NjkxMDYyMjYsICJjbnBqIiA6ICI1NTEzNDY4ODAwMDE1NyIsICJsb2dpbiIgOiAibWp0YW1hbmhvbmlAZ21haWwuY29tIiwgInBhc3N3b3JkIiA6ICJNR3cwUjhNeU5ER3R3ejh5TVRKNk1rWlVMekU1TnpReWJEUkh3ekkxTnEzRFB6STBNM294UmxRdiIsICJub21lIiA6ICJNQVJDT1MgSk9Tw4kgVEFNQU5IT05JIiwgImF0aXZvIiA6IDEgfQ.sVp6Aow5RJ8puxzhfTT48zv_6ls36p21fPoeYIV5p-8"
      }
  }
  *)
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

procedure TForm_Login.ExportD2Bridge;
begin
 inherited;

 Title:= 'My D2Bridge Web Application';
 SubTitle:= 'My WebApp';

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
     Col.Add.VCLObj(Label_Login);

    with Row.Items.Add do
     Col.Add.VCLObj(edCNPJ, 'ValidationLogin', true);

    with Row.Items.Add do
     Col.Add.VCLObj(Edit_UserName, 'ValidationLogin', true);

    with Row.Items.Add do
     with Col.Items.add do //Example Edit + Button same row and col
     begin
      VCLObj(Edit_Password, 'ValidationLogin', true);
      VCLObj(Button_ShowPass, CSSClass.Button.view);
     end;

    with Row.Items.Add do
     Col.Add.VCLObj(Button_Login, 'ValidationLogin', false, CSSClass.Col.colsize12);
   end;

  end;
 end;
end;

procedure TForm_Login.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
 inherited;

end;

procedure TForm_Login.RenderD2Bridge(const PrismControl: TPrismControl; var HTMLControl: string);
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
