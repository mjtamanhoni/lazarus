unit uDM;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  ZConnection, ZDataset,

  uBase.Functions;

type

  { TDM }

  TDM = class(TDataModule)
    ZConnection: TZConnection;
    ZQuery1: TZQuery;
    ZTransaction: TZTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FIniFile :TIniFile;
  public
    procedure ConectarBanco;
    function GetQuery:TZQuery;
  end;

var
  DM: TDM;

implementation

{$R *.lfm}

{ TDM }

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  FIniFile := TIniFile.Create(ConfigFile);
  ConectarBanco;
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FIniFile);
end;

procedure TDM.ConectarBanco;
begin
  try
    ZConnection.Protocol := '';
    ZConnection.HostName := '';
    ZConnection.Port     := 0;
    ZConnection.Database := '';
    ZConnection.User     := '';
    ZConnection.Password := '';
    ZConnection.LibraryLocation:= '';
    ZConnection.Disconnect;

    //Validando parâmetros...
    if Trim(FIniFile.ReadString('DataBase','Protocol','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Protocolo não informado');
    if Trim(FIniFile.ReadString('DataBase','HostName','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'HostName não informado');
    if Trim(FIniFile.ReadString('DataBase','Port','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Porta não informada');
    if Trim(FIniFile.ReadString('DataBase','Database','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Banco de Dados não informado');
    if Trim(FIniFile.ReadString('DataBase','User','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Usuário não informado');
    if Trim(FIniFile.ReadString('DataBase','Password','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Senha não informada');
    if Trim(FIniFile.ReadString('DataBase','LibraryLocation','')) = '' then
      raise Exception.Create('Conexão do Banco de Dados.' + sLineBreak + 'Biblioteca não informada');

    ZConnection.Protocol := Trim(FIniFile.ReadString('DataBase','Protocol','')); // protocolo Zeos
    ZConnection.HostName := Trim(FIniFile.ReadString('DataBase','HostName',''));
    ZConnection.Port     := StrToIntDef(Trim(FIniFile.ReadString('DataBase','Port','0')),0);
    ZConnection.Database := Trim(FIniFile.ReadString('DataBase','Database',''));
    ZConnection.User     := Trim(FIniFile.ReadString('DataBase','User',''));
    ZConnection.Password := Trim(FIniFile.ReadString('DataBase','Password',''));
    ZConnection.LibraryLocation:= Trim(FIniFile.ReadString('DataBase','LibraryLocation',''));
    ZConnection.LoginPrompt := False;

    ZConnection.Connect; // tenta conectar

    if not ZConnection.Connected then
      raise Exception.Create('Banco de Dados não Conectado');

  except
    on E:Exception do
      SaveLog(E.Message);
  end;
end;

function TDM.GetQuery: TZQuery;
begin
  Result := TZQuery.Create(Nil);
  Result.Connection := ZConnection;
  Result.Transaction := ZTransaction;
  Result.Close;
  Result.SQL.Clear;
end;

end.

