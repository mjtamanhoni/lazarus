unit uDM;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, ZConnection, ZDataset, Variants,
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
    function Sequencial(const aTabela:String;const aId_Empresa:Integer=0;const aId_Usuario:Integer=0):Integer;
    function Sequencial_Dinamico(const aTabela: String; const aFields: array of String; const aValues: array of Variant): Integer;
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
    ZConnection.LibraryLocation := Trim(FIniFile.ReadString('DataBase','LibraryLocation',''));
    ZConnection.LoginPrompt := False;

    ZConnection.Connect; // tenta conectar

    if not ZConnection.Connected then
      raise Exception.Create('Banco de Dados não Conectado');

  except
    on E:Exception do
       GravarLogJSON(Self.Name,'Banco de Dados', 'ConectarBanco', E);
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

function TDM.Sequencial(const aTabela:String;const aId_Empresa:Integer=0;const aId_Usuario:Integer=0):Integer;
var
  FQuery :TZQuery;
begin
  try
    try
      Result := 0;

      if aId_Empresa = 0 then
        raise Exception.Create('O id da Empresa é obrigatório');
      if Trim(aTabela) = '' then
        raise Exception.Create('O nome da Tabela é obrigatório');

      FQuery := GetQuery;
      FQuery.SQL.Add('SELECT obter_sequencial(:id_empresa, :id_usuario, :nome_tabela)');
      FQuery.ParamByName('id_empresa').AsInteger := aId_Empresa;
      FQuery.ParamByName('id_usuario').AsInteger := aId_Usuario;
      FQuery.ParamByName('nome_tabela').AsString := aTabela;
      FQuery.Open;

      Result := FQuery.Fields[0].AsInteger;

    except
      on E:Exception do
        raise Exception.Create('Gera sequencial.' + sLineBreak + E.Message);
    end;
  finally
    FreeAndNil(FQuery);
  end;
end;

function TDM.Sequencial_Dinamico(
  const aTabela: String;
  const aFields: array of String;
  const aValues: array of Variant): Integer;
var
  i: Integer;
  SQL: String;
begin
  // SQL base
  SQL := 'SELECT COALESCE(MAX(CODIGO), 0) + 1 FROM ' + aTabela + ' WHERE 1=1';

  // Adiciona os filtros dinamicamente
  for i := 0 to High(aFields) do
  begin
    // Tratando o valor para SQL (VarToStr ou tratamento de tipos)
    //SQL := SQL + ' AND ' + aFields[i] + ' = ' + VarToSQLStr(aValues[i]);
  end;

  // Execução no componente de Query (Ex: SQLQuery1)
  //SQLQuery1.Close;
  //SQLQuery1.SQL.Text := SQL;
  //SQLQuery1.Open;

  //Result := SQLQuery1.Fields[0].AsInteger;

end;

end.

