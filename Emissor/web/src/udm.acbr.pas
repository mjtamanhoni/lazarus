unit uDM.ACBr;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ACBrValidador;

type

  { TDM_Acbr }

  TDM_Acbr = class(TDataModule)
    ACBrValidador: TACBrValidador;
  private

  public

  end;

var
  DM_Acbr: TDM_Acbr;

implementation

{$R *.lfm}

end.

