program D2BridgeWebAppLCL;

{$mode delphi}{$H+}

{$IFDEF D2BRIDGE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads, clocale,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, zcomponent, uBase.Functions, uCripto_Descrito,   
  D2BridgeFormTemplate,	
  Emissor_Session,
  EmissorWebApp,
  Unit_Login in 'Unit_Login.pas' {Form_Login},
Unit_D2Bridge_Server_Console in 'Unit_D2Bridge_Server_Console.pas',

  
  unit1, D2Bridge.ServerControllerBase, Prism.Session, uPrincipal,
uBase.Validation, uDM.ACBr, uCad.Empresa, uEmpresa, uCad.Empresa.Endereco,
ucad.empresa.DadosBancarios
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  TD2BridgeServerConsole.Run
  
end.

