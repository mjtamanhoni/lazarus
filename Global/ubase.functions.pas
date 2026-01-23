unit uBase.Functions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Math;

{$Region 'Funçoes'}
function EndPath:String;
function NameEXE(aSemExtensao:Boolean=True):String;
function ConfigFile:String;
function LogFile:String;
function PreencherEsquerda(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
function PreencherDireita(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
function PreencherCentro(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
function RemoverMascara(const S: string): string;
{$EndRegion}

{$Region 'Procedures'}
procedure SaveLog(
  const aMessage:String;
  const ADataHora:Boolean=True);
{$EndRegion}

implementation

function EndPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function NameEXE(aSemExtensao:Boolean=True): String;
begin
  if aSemExtensao then
    Result := ChangeFileExt(ExtractFileName(ParamStr(0)),'')
  else
    Result := ExtractFileName(ParamStr(0));
end;

function ConfigFile: String;
begin
  Result := EndPath + NameEXE + '.INI';
end;

function LogFile: String;
begin
  Result := EndPath + NameEXE + '.LOG';
end;

function PreencherEsquerda(const Texto: string; Tamanho: Integer; Caractere: Char): string;
begin
  if Length(Texto) >= Tamanho then
    Result := Copy(Texto, 1, Tamanho)
  else
    Result := StringOfChar(Caractere, Tamanho - Length(Texto)) + Texto;
end;

function PreencherDireita(const Texto: string; Tamanho: Integer; Caractere: Char): string;
begin
  if Length(Texto) >= Tamanho then
    Result := Copy(Texto, 1, Tamanho)
  else
    Result := Texto + StringOfChar(Caractere, Tamanho - Length(Texto));
end;

function PreencherCentro(const Texto: string; Tamanho: Integer; Caractere: Char): string;
var
  EspacosEsq, EspacosDir: Integer;
begin
  if Length(Texto) >= Tamanho then
    Result := Copy(Texto, 1, Tamanho)
  else
  begin
    EspacosEsq := (Tamanho - Length(Texto)) div 2;
    EspacosDir := Tamanho - Length(Texto) - EspacosEsq;
    Result := StringOfChar(Caractere, EspacosEsq) + Texto + StringOfChar(Caractere, EspacosDir);
  end;
end;

procedure SaveLog(
  const aMessage: String;
  const ADataHora:Boolean=True);
var
  F: TextFile;
begin
  AssignFile(F, LogFile);
  try
    if FileExists(LogFile) then
      Append(F)
    else
      Rewrite(F);

    if ADataHora then
       WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ' + aMessage)
    else
      WriteLn(F,aMessage);

  finally
    CloseFile(F);
  end;
end;

function RemoverMascara(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then
      Result := Result + S[I];
end;


end.

