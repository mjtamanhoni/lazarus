unit uCreateDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZConnection, ZDataset;

procedure EnsureDatabaseExists;
procedure ExecSQLScript(const ASQL: string);

implementation

procedure EnsureDatabaseExists;
var
  Conn: TZConnection;
  Query: TZQuery;
  DBExists: Boolean;
begin
  Conn := TZConnection.Create(nil);
  Query := TZQuery.Create(nil);
  try
    // Conexão inicial no banco postgres (administrativo)
    Conn.Protocol := 'postgresql-7';
    Conn.HostName := 'localhost';
    Conn.Port := 5432;
    Conn.User := 'postgres';
    Conn.Password := 'sua_senha';
    Conn.Database := 'postgres'; // banco administrativo
    Conn.Connect;

    Query.Connection := Conn;

    // Verifica se o banco emissor já existe
    Query.SQL.Text := 'SELECT 1 FROM pg_database WHERE datname = ''emissor'';';
    Query.Open;
    DBExists := not Query.IsEmpty;
    Query.Close;

    if not DBExists then
    begin
      Query.SQL.Text :=
        'CREATE DATABASE emissor ' +
        'WITH OWNER = postgres ' +
        'ENCODING = ''UTF8'' ' +
        'LC_COLLATE = ''Portuguese_Brazil.1252'' ' +
        'LC_CTYPE = ''Portuguese_Brazil.1252'' ' +
        'LOCALE_PROVIDER = ''libc'' ' +
        'TABLESPACE = pg_default ' +
        'CONNECTION LIMIT = -1 ' +
        'IS_TEMPLATE = False;';
      Query.ExecSQL;

      Query.SQL.Text :=
        'COMMENT ON DATABASE emissor IS ' +
        '''Controle de emissão de NFe, NFCe, CTe'';';
      Query.ExecSQL;
    end;
  finally
    Query.Free;
    Conn.Free;
  end;
end;

procedure ExecSQLScript(const ASQL: string);
var
  Conn: TZConnection;
  Query: TZQuery;
begin
  Conn := TZConnection.Create(nil);
  Query := TZQuery.Create(nil);
  try
    // Conecta ao banco emissor
    Conn.Protocol := 'postgresql-7';
    Conn.HostName := 'localhost';
    Conn.Port := 5432;
    Conn.User := 'postgres';
    Conn.Password := 'sua_senha';
    Conn.Database := 'emissor';
    Conn.Connect;

    Query.Connection := Conn;

    // Executa o script recebido
    Query.SQL.Text := ASQL;
    Query.ExecSQL;
  finally
    Query.Free;
    Conn.Free;
  end;
end;


end.
