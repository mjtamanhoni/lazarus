unit uCripto;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  base64, DateUtils, uCripto_Descrito;

type

  { TfrmCripto }

  TfrmCripto = class(TForm)
    btCriptografa: TButton;
    btDescriptografa: TButton;
    edSenha: TEdit;
    edSenhaCriptografada: TEdit;
    edSenhaDescriptografada: TEdit;
    procedure btCriptografaClick(Sender: TObject);
    procedure btDescriptografaClick(Sender: TObject);
  private
  public

  end;

var
  frmCripto: TfrmCripto;

implementation

{$R *.lfm}

{ TfrmCripto }

procedure TfrmCripto.btCriptografaClick(Sender: TObject);
begin
  try
    edSenhaCriptografada.Text := Criptografar(edSenha.Text);
  except
    On E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfrmCripto.btDescriptografaClick(Sender: TObject);
begin
  try
    edSenhaDescriptografada.Text := Descriptografar(edSenhaCriptografada.Text);
  except
    On E:Exception do
      ShowMessage(E.Message);
  end;
end;

end.

