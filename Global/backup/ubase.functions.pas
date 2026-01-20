unit uBase.Functions;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Math;

{$Region 'Funçoes'}
function EndPath:String;
function NameEXE(aSemExtensao:Boolean=True):String;
function ConfigFile:String;
function LogFile:String;
{$EndRegion}

{$Region 'Procedures'}
procedure SaveLog(const aMessage:String;const ADataHora:Boolean=True);
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

procedure SaveLog(const aMessage: String; const ADataHora:Boolean=True);
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

end.

