unit uWaitDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls
  {$IFDEF WINDOWS}
  , Windows
  {$ENDIF}
  , LCLIntf, LCLType; // Para GetTickCount e compatibilidade

procedure WaitBegin(const Titulo: string = 'Processando...'; const Total: Integer = 0; const AllowCancel: Boolean = True);
procedure WaitUpdateStatus(const Msg: string);
procedure WaitUpdateProgress(const Posicao: Integer);
procedure WaitUpdateCount(const Atual, Total: Integer; ATimeUpdate: Integer = 0);
function WaitCancelled: Boolean;
procedure WaitEnd;

implementation

var
  GForm: TForm;
  GLblTitulo: TLabel;
  GLblStatus: TLabel;
  GLblCount: TLabel;
  GLblCancelHint: TLabel;
  GLblTime: TLabel;
  GLblPercent: TLabel;
  GProgress: TProgressBar;
  GBtnOK: TButton;
  GMainEnabledTarget: HWND;
  GTotal: Integer;
  GCancelled: Boolean;
  GAllowCancel: Boolean;
  GStartTick: DWORD;
  GCurrent: Integer;
  GTimer: TTimer;

type
  TWaitHandler = class(TComponent)
  public
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$IFDEF WINDOWS}
    procedure TimerTick(Sender: TObject);
    {$ENDIF}
    procedure ButtonOKClick(Sender: TObject);
  end;

var
  GHandler: TWaitHandler;

procedure CreateWaitForm(const Titulo: string; Total: Integer; AllowCancel: Boolean);
begin
  GForm := TForm.Create(nil);
  GForm.BorderStyle := bsDialog;
  GForm.BorderIcons := [];
  GForm.Position := poScreenCenter;
  GForm.FormStyle := fsStayOnTop;
  GForm.Caption := Titulo;
  GForm.ClientWidth := 500;
  GForm.ClientHeight := 220;
  GForm.Color := clWhite;

  GLblTitulo := TLabel.Create(GForm);
  GLblTitulo.Parent := GForm;
  GLblTitulo.Left := 20;
  GLblTitulo.Top := 20;
  GLblTitulo.Caption := Titulo;
  GLblTitulo.Font.Style := [fsBold];
  GLblTitulo.Font.Size := 14;
  GLblTitulo.Font.Color := clNavy;

  GLblStatus := TLabel.Create(GForm);
  GLblStatus.Parent := GForm;
  GLblStatus.Left := 20;
  GLblStatus.Top := 60;
  GLblStatus.Width := GForm.ClientWidth - 40;
  GLblStatus.Caption := 'Iniciando...';

  GProgress := TProgressBar.Create(GForm);
  GProgress.Parent := GForm;
  GProgress.Left := 20;
  GProgress.Top := 90;
  GProgress.Width := GForm.ClientWidth - 140;
  GProgress.Min := 0;
  GProgress.Max := Total;

  GLblPercent := TLabel.Create(GForm);
  GLblPercent.Parent := GForm;
  GLblPercent.Left := GProgress.Left + GProgress.Width + 10;
  GLblPercent.Top := GProgress.Top + 5;
  GLblPercent.Caption := '0%';
  GLblPercent.Font.Style := [fsBold];
  GLblPercent.Font.Color := clBlue;

  GLblCount := TLabel.Create(GForm);
  GLblCount.Parent := GForm;
  GLblCount.Left := 20;
  GLblCount.Top := 120;
  GLblCount.Caption := '0 de 0';

  GLblTime := TLabel.Create(GForm);
  GLblTime.Parent := GForm;
  GLblTime.Left := 20;
  GLblTime.Top := 145;
  GLblTime.Width := GForm.ClientWidth - 40;
  GLblTime.Alignment := taRightJustify;
  GLblTime.Caption := 'Decorrido: 00:00:00 - Restante: 00:00:00';

  GBtnOK := TButton.Create(GForm);
  GBtnOK.Parent := GForm;
  GBtnOK.Caption := 'OK';
  GBtnOK.Width := 100;
  GBtnOK.Height := 35;
  GBtnOK.Left := (GForm.ClientWidth - GBtnOK.Width) div 2;
  GBtnOK.Top := GForm.ClientHeight - 55;
  GBtnOK.Visible := False;
  GBtnOK.ModalResult := mrOK;
  //GBtnOK.OnClick := GHandler.ButtonOKClick;

  if AllowCancel then
  begin
    GLblCancelHint := TLabel.Create(GForm);
    GLblCancelHint.Parent := GForm;
    GLblCancelHint.Left := 20;
    GLblCancelHint.Top := GForm.ClientHeight - 30;
    GLblCancelHint.Caption := 'Pressione Ctrl+C para cancelar';
    GLblCancelHint.Font.Color := clGray;
  end;

  GTotal := Total;
  GAllowCancel := AllowCancel;

  {$IFDEF WINDOWS}
  if Assigned(Application.MainForm) then
    GMainEnabledTarget := Application.MainForm.Handle
  else
    GMainEnabledTarget := 0;
  if GMainEnabledTarget <> 0 then
    EnableWindow(GMainEnabledTarget, False);
  {$ELSE}
  GMainEnabledTarget := 0;
  {$ENDIF}

  GCancelled := False;
  GStartTick := GetTickCount;
  GCurrent := 0;

  GHandler := TWaitHandler.Create(nil);
  GForm.KeyPreview := True;
  //GForm.OnKeyDown := GHandler.FormKeyDown;

  GTimer := TTimer.Create(nil);
  GTimer.Interval := 1000;
  {$IFDEF WINDOWS}
  //GTimer.OnTimer := GHandler.TimerTick;
  {$ENDIF}
  GTimer.Enabled := True;

  GForm.Show;
end;

procedure WaitBegin(const Titulo: string = 'Processando...'; const Total: Integer = 0; const AllowCancel: Boolean = True);
begin
  if Assigned(GForm) then
    WaitEnd;
  CreateWaitForm(Titulo, Total, AllowCancel);
end;

procedure WaitUpdateStatus(const Msg: string);
begin
  if Assigned(GLblStatus) then
  begin
    GLblStatus.Caption := Msg;
    GForm.Update;
    Application.ProcessMessages;
  end;
end;

procedure WaitUpdateProgress(const Posicao: Integer);
begin
  if Assigned(GProgress) then
  begin
    GProgress.Position := Posicao;
    GForm.Update;
    Application.ProcessMessages;
  end;
end;

procedure WaitUpdateCount(const Atual, Total: Integer; ATimeUpdate: Integer = 0);
var
  ElapsedMs: DWORD;
  H, M, S: Integer;
  ElapsedStr, RemainingStr: string;
  AvgMsPerItem: Double;
  Percentual: Integer;
begin
  if not Assigned(GForm) then Exit;

  if Total > 0 then GTotal := Total;
  GCurrent := Atual;

  if Assigned(GProgress) then
  begin
    GProgress.Max := GTotal;
    GProgress.Position := GCurrent;
  end;

  if GTotal > 0 then
    Percentual := Round((GCurrent / GTotal) * 100)
  else
    Percentual := 0;
  if Assigned(GLblPercent) then
    GLblPercent.Caption := Format('%d%%', [Percentual]);

  ElapsedMs := GetTickCount - GStartTick;
  S := ElapsedMs div 1000;
  H := S div 3600;
  M := (S mod 3600) div 60;
  S := S mod 60;
  ElapsedStr := Format('%.2d:%.2d:%.2d', [H, M, S]);

  RemainingStr := '00:00:00';
  if (GTotal > 0) and (GCurrent > 0) then
  begin
    AvgMsPerItem := ElapsedMs / GCurrent;
    ElapsedMs := Round(AvgMsPerItem * (GTotal - GCurrent));
    S := ElapsedMs div 1000;
    H := S div 3600;
    M := (S mod 3600) div 60;
    S := S mod 60;
    RemainingStr := Format('%.2d:%.2d:%.2d', [H, M, S]);
  end;

  if Assigned(GLblCount) then
    GLblCount.Caption := FormatFloat('#,##0', GCurrent) + ' de ' + FormatFloat('#,##0', GTotal);

  if Assigned(GLblTime) then
    GLblTime.Caption := 'Decorrido: ' + ElapsedStr + ' - Restante: ' + RemainingStr;

  GForm.Update;
  Application.ProcessMessages;
end;

function WaitCancelled: Boolean;
begin
  Result := GAllowCancel and GCancelled;
end;

procedure TWaitHandler.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = Ord('C')) then
  begin
    if MessageDlg('Deseja cancelar a operação?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      GCancelled := True;
  end;
end;

procedure TWaitHandler.TimerTick(Sender: TObject);
begin
  WaitUpdateCount(GCurrent, GTotal);
end;

procedure TWaitHandler.ButtonOKClick(Sender: TObject);
begin
  // Nada aqui - ModalResult faz o ShowModal retornar
end;

procedure WaitEnd;
begin
  {$IFDEF WINDOWS}
  if GMainEnabledTarget <> 0 then
    EnableWindow(GMainEnabledTarget, True);
  {$ENDIF}

  if Assigned(GTimer) then
  begin
    GTimer.Enabled := False;
    FreeAndNil(GTimer);
  end;

  if Assigned(GBtnOK) then
  begin
    GBtnOK.Visible := True;
    GBtnOK.BringToFront;
    GBtnOK.SetFocus;
  end;

  if Assigned(GForm) then
    GForm.ShowModal; // Aguarda clique no OK

  FreeAndNil(GLblTitulo);
  FreeAndNil(GLblStatus);
  FreeAndNil(GLblCount);
  FreeAndNil(GLblTime);
  FreeAndNil(GLblCancelHint);
  FreeAndNil(GLblPercent);
  FreeAndNil(GProgress);
  FreeAndNil(GBtnOK);
  FreeAndNil(GForm);
  FreeAndNil(GHandler);

  GMainEnabledTarget := 0;
  GTotal := 0;
  GCancelled := False;
  GAllowCancel := False;
  GCurrent := 0;
end;

end.



(*
----------------------------COMO USAR--------------------------------

WaitBegin('Importando dados...', 1000, True);

try
  for i := 1 to 1000 do
  begin
    WaitUpdateStatus('Processando item ' + IntToStr(i));
    WaitUpdateCount(i, 1000);
    Sleep(5); // simulação
  end;
finally
  WaitEnd; // Mostra botão OK e aguarda clique
end;

*)
