unit uBase.Validation;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math;

type
  TACBrValTipoDocto = ( docCPF, docCNPJ, docUF, docInscEst, docNumCheque, docPIS,
                        docCEP, docCartaoCredito, docSuframa, docGTIN, docRenavam,
                        docEmail, docCNH, docPrefixoGTIN, docCAEPF, docPlacaMercosul,
                        docCNS ) ;


function Validation_ALL(const aContent:String; aDocumentType:TACBrValTipoDocto):Boolean;

implementation

function Validation_ALL(const aContent: String; aDocumentType: TACBrValTipoDocto): Boolean;
begin

end;


end.

