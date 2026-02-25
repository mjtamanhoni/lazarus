unit ubase.functions.objetos;

{$mode Delphi}

interface

uses
  Classes, SysUtils, D2Bridge.Forms, Forms, Controls;

procedure AvancarFoco(AForm: TD2BridgeForm);
procedure EnterAsTab(Sender: TObject);



implementation

procedure AvancarFoco(AForm: TD2BridgeForm);
begin
  // O parâmetro CurControl como Nil indica que deve começar do controle atual.
  // GoForward = True (Avança)
  // CheckTabStop = True (Pula controles onde TabStop é False, como Labels)
  AForm.SelectNext(AForm.ActiveControl, True, True);
end;

procedure EnterAsTab(Sender: TObject);
begin
  if (Sender is TWinControl) then
  begin
    if TWinControl(Sender).CanFocus then
       TWinControl(Sender).SetFocus;
  end;
end;





end.

