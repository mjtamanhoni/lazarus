unit uPermissoes.Lista;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

function Get_ListOfPermissions:TStringList;

implementation

function Get_ListOfPermissions: TStringList;
begin
  Result := TStringList.Create;

  Result.Add('Lista de Empresas');
  Result.Add('Cadastro de Empresas');
  Result.Add('Lista de Usuários');
  Result.Add('Cadastro de Usuários');

end;

end.

