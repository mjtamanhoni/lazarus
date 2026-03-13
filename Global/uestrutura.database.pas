unit uEstrutura.Database;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset;

type

  { TCreateDatabase }

  TCreateDatabase = class
  private
    FDatabaseName: String;
    FUser: string;
    FPassword: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
  public
    constructor Create(
      const aDataBaseName, aUser, aPassword: string;
      const aProtocol: string = 'postgresql-7';
      const aHostName: string = 'localhost';
      const aPort: Integer = 5432;
      const aDatabase: string = 'postgres');

    procedure EnsureDatabaseExists;
    procedure EnsureObjectsExist(const ASQL: string);
    procedure ExecSQLScript(const ASQL: string);
  end;



implementation


{ TCreateDatabase }

constructor TCreateDatabase.Create(
  const aDataBaseName, aUser, aPassword: string;
  const aProtocol: string;
  const aHostName: string;
  const aPort: Integer;
  const aDatabase: string);
begin
  FUser := aUser;
  FPassword := aPassword;
  FProtocol := aProtocol;
  FHostName := aHostName;
  FPort := aPort;
  FDatabase := aDatabase;
  FDatabaseName := aDataBaseName;
end;

procedure TCreateDatabase.EnsureDatabaseExists;
var
  Conn: TZConnection;
  Query: TZQuery;
  DBExists: Boolean;
begin
  Conn := TZConnection.Create(nil);
  Query := TZQuery.Create(nil);
  try
    Conn.Protocol := FProtocol;
    Conn.HostName := FHostName;
    Conn.Port := FPort;
    Conn.User := FUser;
    Conn.Password := FPassword;
    Conn.Database := FDatabase; // normalmente "postgres"
    Conn.LibraryLocation := 'C:\Program Files\PostgreSQL\psqlODBC\bin\libpq.dll';
    Conn.Connect;

    Query.Connection := Conn;
    Query.SQL.Text := 'SELECT 1 FROM pg_database WHERE datname = ' + QuotedStr(FDatabaseName);
    Query.Open;
    DBExists := not Query.IsEmpty;
    Query.Close;

    if not DBExists then
    begin
      Query.SQL.Text :=
        'CREATE DATABASE ' + FDatabaseName +
        'WITH OWNER = ' + FUser + ' ' +
        'ENCODING = ''UTF8'' ' +
        'LC_COLLATE = ''Portuguese_Brazil.1252'' ' +
        'LC_CTYPE = ''Portuguese_Brazil.1252'' ' +
        'LOCALE_PROVIDER = ''libc'' ' +
        'TABLESPACE = pg_default ' +
        'CONNECTION LIMIT = -1 ' +
        'IS_TEMPLATE = False;';
      Query.ExecSQL;

      Query.SQL.Text :=
        'COMMENT ON DATABASE '+FDatabaseName+' IS ' +
        '''Controle de emissão de NFe, NFCe, CTe'';';
      Query.ExecSQL;
    end;
  finally
    Query.Free;
    Conn.Free;
  end;
end;

procedure TCreateDatabase.EnsureObjectsExist(const ASQL: string);
var
  Conn: TZConnection;
  Query: TZQuery;
begin
  Conn := TZConnection.Create(nil);
  Query := TZQuery.Create(nil);
  try
    Conn.Protocol := FProtocol;
    Conn.HostName := FHostName;
    Conn.Port := FPort;
    Conn.User := FUser;
    Conn.Password := FPassword;
    Conn.Database := FDatabaseName; // conecta no banco criado
    Conn.Connect;

    Query.Connection := Conn;

    (* //Exemplo de criação de tabela
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS public.sequencial (' +
      'id_empresa integer NOT NULL,' +
      'id_usuario integer,' +
      'tabela text NOT NULL,' +
      'sequencial integer NOT NULL,' +
      'CONSTRAINT pk_sequencial PRIMARY KEY (id_empresa, id_usuario, tabela)' +
      ');';
    *)
    Query.SQL.Text := ASQL;

    Query.ExecSQL;

    // Aqui você pode adicionar outros comandos (ALTER, CREATE SEQUENCE, etc.)
  finally
    Query.Free;
    Conn.Free;
  end;
end;

procedure TCreateDatabase.ExecSQLScript(const ASQL: string);
begin

end;

end.

