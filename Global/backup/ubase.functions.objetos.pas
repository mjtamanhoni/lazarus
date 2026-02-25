unit ubase.functions.objetos;

{$mode Delphi}

interface

uses
  Classes, SysUtils, D2Bridge.Forms, Forms, Controls;

procedure AvancarFoco(AForm: TD2BridgeForm);
procedure EnterAsTab(Sender: TObject; var Key: Char);



implementation

procedure AvancarFoco(AForm: TD2BridgeForm);
begin
  // O parâmetro CurControl como Nil indica que deve começar do controle atual.
  // GoForward = True (Avança)
  // CheckTabStop = True (Pula controles onde TabStop é False, como Labels)
  AForm.SelectNext(AForm.ActiveControl, True, True);
end;


procedure EnterAsTab(Sender: TObject; var Key: Char);
var
  Form: TCustomForm;
begin
  if Key = #13 then
  begin
    Key := #0; // evita beep
    if Sender is TWinControl then
    begin
      Form := TCustomForm(TWinControl(Sender).Owner);
      if Assigned(Form) then
        Form.SelectNext(TWinControl(Sender), True, True);
    end;
  end;
end;





end.

