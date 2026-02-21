program Server.Emissor;

{$mode delphi}{$H+}

uses
  Classes, SysUtils, Windows,
  Horse, Horse.JWT, Horse.Jhonson,
  zcomponent,
  uBase.Router, uUsuario.Service, uCripto_Descrito, uDM, uBase.Functions,
  uEmpresa.Service;

{$R *.res}

function GetVersionString(const ValueName: string): string;
var
  Size, Handle: DWORD;
  Buffer: Pointer;
  Len: UINT;
  Value: PChar;
begin
  Result := '';
  Size := GetFileVersionInfoSize(PChar(ParamStr(0)), Handle);
  if Size > 0 then
  begin
    GetMem(Buffer, Size);
    try
      if GetFileVersionInfo(PChar(ParamStr(0)), Handle, Size, Buffer) then
      begin
        // O código de idioma/charset padrão é 040904E4 (US English, Unicode)
        if VerQueryValue(Buffer,
          PChar('\StringFileInfo\040904E4\' + ValueName),
          Pointer(Value), Len) then
        begin
          Result := Value;
        end;
      end;
    finally
      FreeMem(Buffer, Size);
    end;
  end;
end;


function GetFileVersionInfoStr: string;
var
  Size, Handle: DWORD;
  Buffer: Pointer;
  FixedFileInfo: PVSFixedFileInfo;
  Len: UINT;
begin
  Result := '';
  // Obtém o tamanho do bloco de versão
  Size := GetFileVersionInfoSize(PChar(ParamStr(0)), Handle);
  if Size > 0 then
  begin
    GetMem(Buffer, Size);
    try
      if GetFileVersionInfo(PChar(ParamStr(0)), Handle, Size, Buffer) then
      begin
        if VerQueryValue(Buffer, '\', Pointer(FixedFileInfo), Len) then
        begin
          Result := Format('%d.%d.%d.%d',
            [HiWord(FixedFileInfo.dwFileVersionMS),
             LoWord(FixedFileInfo.dwFileVersionMS),
             HiWord(FixedFileInfo.dwFileVersionLS),
             LoWord(FixedFileInfo.dwFileVersionLS)]);
        end;
      end;
    finally
      FreeMem(Buffer, Size);
    end;
  end;
end;

procedure OnListen();
begin
  WriteLn(Format('Server name: %s',['E M I S S O R']));
  WriteLn('');
  Writeln(Format('File Version: %s', [GetFileVersionInfoStr]));
  Writeln(Format('Company Name: %s', [GetVersionString('CompanyName')]));
  Writeln(Format('File Description: %s', [GetVersionString('FileDescription')]));
  Writeln(Format('Legal Copyright: %s', [GetVersionString('LegalCopyright')]));
  Writeln(Format('Product Name: %s', [GetVersionString('ProductName')]));
  WriteLn(Format('E-Mail: %s',['mjtamanhoni@gmail.com']));
  WriteLn(Format('Mobile phone (WhatsApp): %s',['(27) 98833-7323']));

  WriteLn('');
  WriteLn(Format('Executable in folder: %s',[ParamStr(0)]));
  WriteLn(Format('Server active on port: %d',[THorse.Port]));
  WriteLn(Format('Server started on: %s',[FormatDateTime('dd/mm/yyy hh:nn:ss',Now)]));

  SaveLog(sLineBreak + '============================================================================',False);
  SaveLog(Format('Server started on: %s, on port %d',[FormatDateTime('dd/mm/yyy "at" hh:nn:ss',Now),THorse.Port]));
  SaveLog('----------------------------------------------------------------------------',False)
end;

begin
  //Depois mudar a senha 'Horse_2026', por algo mais complexo (CNPJ do cliente)
  //Validador de Token
  THorse.Use(HorseJWT(C_SECRET_JWT,THorseJWTConfig.New.SkipRoutes([
             '/'
  	     ,'/login'
             ,'/usuario'
             ,'/empresa'
             ,'/empresa/endereco'
             ,'/empresa/dBanco'
             ,'/empresa/validaCnpj'
             ]
  )));

  //Lendo as rotas..
  TBaseRoute.Load();

  //Start...
  THorse.Listen(9095, OnListen);
end.


