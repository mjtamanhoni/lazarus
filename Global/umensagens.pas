unit uMensagens;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, LCLType
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  ;

{ Mensagens comuns com botões em português }

function MsgInfo(const AMensagem: string; const ATitulo: string = 'Informação'): Integer;
function MsgErro(const AMensagem: string; const ATitulo: string = 'Erro'): Integer;
function MsgAviso(const AMensagem: string; const ATitulo: string = 'Atenção'): Integer;
function MsgConfirmacao(const AMensagem: string; const ATitulo: string = 'Confirmação'): Boolean;
function MsgSimNaoCancelar(const AMensagem: string; const ATitulo: string = 'Confirmação'): Integer; // Retorna mrYes, mrNo, mrCancel
function MsgOKCancelar(const AMensagem: string; const ATitulo: string = 'Confirmação'): Boolean;

implementation

{$IFDEF MSWINDOWS}
// No Windows, usamos a API nativa → botões automaticamente em português
function WinMessageBox(const AMensagem, ATitulo: string; const AFlags: Cardinal): Integer;
begin
  Result := Windows.MessageBox(0, PChar(AMensagem), PChar(ATitulo), AFlags);
end;
{$ENDIF}

function MsgInfo(const AMensagem: string; const ATitulo: string = 'Informação'): Integer;
begin
  {$IFDEF D2BRIDGE}
    Result := MessageDlg(AMensagem, TMsgDlgType.mtInformation, [mbOK], 0);    //MessageDlg('Mensagem Executada', TMsgDlgType.mtInformation, [mbok], 0);
  {$ELSE}
    Result := WinMessageBox(AMensagem, ATitulo, MB_OK or MB_ICONINFORMATION);
  {$ENDIF}
end;

function MsgErro(const AMensagem: string; const ATitulo: string = 'Erro'): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := WinMessageBox(AMensagem, ATitulo, MB_OK or MB_ICONERROR);
  {$ELSE}
    Result := MessageDlg(ATitulo, AMensagem, mtError, [mbOK], 0);
  {$ENDIF}
end;

function MsgAviso(const AMensagem: string; const ATitulo: string = 'Atenção'): Integer;
begin
  {$IFDEF D2BRIDGE}
    Result := MessageDlg(ATitulo, AMensagem, mtWarning, [mbOK], 0);
  {$ELSE}
    Result := WinMessageBox(AMensagem, ATitulo, MB_OK or MB_ICONWARNING);
  {$ENDIF}
end;

function MsgConfirmacao(const AMensagem: string; const ATitulo: string = 'Confirmação'): Boolean;
var
  Res: Integer;
begin
  {$IFDEF MSWINDOWS}
    Res := WinMessageBox(AMensagem, ATitulo, MB_YESNO or MB_ICONQUESTION);
    Result := (Res = IDYES);
  {$ELSE}
    Res := MessageDlg(ATitulo, AMensagem, mtConfirmation, [mbYes, mbNo], 0, ['&Sim', '&Não']);
    Result := (Res = mrYes);
  {$ENDIF}
end;

function MsgSimNaoCancelar(const AMensagem: string; const ATitulo: string = 'Confirmação'): Integer;
begin
  {$IFDEF MSWINDOWS}
    Result := WinMessageBox(AMensagem, ATitulo, MB_YESNOCANCEL or MB_ICONQUESTION);
  {$ELSE}
    Result := MessageDlg(ATitulo, AMensagem, mtConfirmation,
      [mbYes, mbNo, mbCancel], 0, ['&Sim', '&Não', '&Cancelar']);
  {$ENDIF}
end;

function MsgOKCancelar(const AMensagem: string; const ATitulo: string = 'Confirmação'): Boolean;
var
  Res: Integer;
begin
  {$IFDEF MSWINDOWS}
    Res := WinMessageBox(AMensagem, ATitulo, MB_OKCANCEL or MB_ICONQUESTION);
    Result := (Res = IDOK);
  {$ELSE}
    Res := MessageDlg(ATitulo, AMensagem, mtConfirmation, [mbOK, mbCancel], 0, ['&OK', '&Cancelar']);
    Result := (Res = mrOK);
  {$ENDIF}
end;

end.
