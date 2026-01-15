unit uFuncoes;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uMensagens, Math,
  IdHashMessageDigest, IdHMAC, IdSSLOpenSSL, base64;

type
  TTipoErro = (teMensagem, teLog, teAmbos);

// Nome do arquivo de configuração INI do sistema
const
  C_ARQUIVO_CONFIG = 'Config_Emissor.INI';
  C_ARQUIVO_LOG = 'Erro_Emissor.log';

///  Valida CPF ou CNPJ brasileiro.
///  Aceita string com ou sem máscara (pontos, traços, barras).
///  Retorna True se o documento for válido.
function ValidarDocumento(const Doc: string): Boolean;

///  Preenche uma string à esquerda com um caractere até atingir o tamanho desejado.
/// <param name="Texto">Conteúdo original</param>
/// <param name="Tamanho">Tamanho final desejado</param>
/// <param name="Caractere">Caractere de preenchimento (padrão = espaço)</param>
function PreencherEsquerda(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;

///  Preenche uma string à direita com um caractere até atingir o tamanho desejado.
function PreencherDireita(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;

///  Centraliza uma string preenchendo com caractere à esquerda e à direita.
function PreencherCentro(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;

///  Valida Inscrição Estadual brasileira por UF.
///  Aceita "ISENTO" (case insensitive) como válido em qualquer UF.
///  Suporta todos os 27 estados brasileiros.
/// <param name="UF">Sigla da UF em maiúsculas (ex: 'SP', 'MG', 'RJ')</param>
/// <param name="IE">Número da Inscrição Estadual (com ou sem máscara)</param>
/// <returns>True se válida ou "ISENTO", False se inválida</returns>
function ValidarInscricaoEstadual(const UF, IE: string): Boolean;

///  Trata exceções e erros de forma centralizada.
/// <param name="E">Exceção capturada (pode ser nil se for erro manual)</param>
/// <param name="Mensagem">Mensagem personalizada para o usuário</param>
/// <param name="Tipo">O que fazer: mensagem, log ou ambos</param>
/// <param name="UnitName">Nome da unit onde ocorreu (opcional, para log)</param>
/// <param name="Line">Número da linha (opcional, use __LINE__ se quiser)</param>
procedure TratarErro_Geral(E: Exception; const Mensagem: string; Tipo: TTipoErro = teAmbos;
          const UnitName: string = ''; Line: Integer = 0);

///  Grava uma mensagem personalizada no log (sem exceção)
procedure Log(const Msg: string; const UnitName: string = ''; Line: Integer = 0);


function GerarTokenSimples(const CNPJ: string; const ChaveSecreta: string): string;

implementation

function RemoverMascara(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then
      Result := Result + S[I];
end;

function ValidarCPF(const CPF: string): Boolean;
var
  Digitos: string;
  I, Soma, Resto, Digito1, Digito2: Integer;
begin
  Digitos := RemoverMascara(CPF);
  if Length(Digitos) <> 11 then
    Exit(False);

  // Verifica sequência repetida
  if (Digitos = '00000000000') or (Digitos = '11111111111') or
     (Digitos = '22222222222') or (Digitos = '33333333333') or
     (Digitos = '44444444444') or (Digitos = '55555555555') or
     (Digitos = '66666666666') or (Digitos = '77777777777') or
     (Digitos = '88888888888') or (Digitos = '99999999999') then
    Exit(False);

  // Primeiro dígito verificador
  Soma := 0;
  for I := 1 to 9 do
    Soma := Soma + StrToInt(Digitos[I]) * (10 - I + 1);
  Resto := (Soma * 10) mod 11;
  if (Resto = 10) or (Resto = 11) then Resto := 0;
  Digito1 := StrToInt(Digitos[10]);
  if Resto <> Digito1 then Exit(False);

  // Segundo dígito verificador
  Soma := 0;
  for I := 1 to 10 do
    Soma := Soma + StrToInt(Digitos[I]) * (11 - I + 1);
  Resto := (Soma * 10) mod 11;
  if (Resto = 10) or (Resto = 11) then Resto := 0;
  Digito2 := StrToInt(Digitos[11]);
  Result := Resto = Digito2;
end;

function ValidarCNPJ(const CNPJ: string): Boolean;
var
  Digitos: string;
  I, Soma, Resto, Digito1, Digito2: Integer;
  Peso: array[1..12] of Integer = (5,4,3,2,9,8,7,6,5,4,3,2);
begin
  Digitos := RemoverMascara(CNPJ);
  if Length(Digitos) <> 14 then
    Exit(False);

  // Verifica sequência repetida
  if (Digitos = '00000000000000') or (Digitos = '11111111111111') or
     (Digitos = '22222222222222') or (Digitos = '33333333333333') or
     (Digitos = '44444444444444') or (Digitos = '55555555555555') or
     (Digitos = '66666666666666') or (Digitos = '77777777777777') or
     (Digitos = '88888888888888') or (Digitos = '99999999999999') then
    Exit(False);

  // Primeiro dígito verificador
  Soma := 0;
  for I := 1 to 12 do
    Soma := Soma + StrToInt(Digitos[I]) * Peso[I];
  Resto := Soma mod 11;
  if Resto < 2 then Digito1 := 0 else Digito1 := 11 - Resto;
  if Digito1 <> StrToInt(Digitos[13]) then Exit(False);

  // Segundo dígito verificador
  Soma := 0;
  for I := 1 to 13 do
  begin
    if I <= 12 then
      Soma := Soma + StrToInt(Digitos[I]) * Peso[I]
    else
      Soma := Soma + StrToInt(Digitos[I]) * 9;
  end;
  Resto := Soma mod 11;
  if Resto < 2 then Digito2 := 0 else Digito2 := 11 - Resto;
  Result := Digito2 = StrToInt(Digitos[14]);
end;

function ValidarDocumento(const Doc: string): Boolean;
var
  Limpo: string;
begin
  Limpo := RemoverMascara(Doc);
  if Length(Limpo) = 11 then
    Result := ValidarCPF(Limpo)
  else if Length(Limpo) = 14 then
    Result := ValidarCNPJ(Limpo)
  else
    Result := False;
end;

function PreencherEsquerda(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
begin
  if Length(Texto) >= Tamanho then
    Result := Copy(Texto, 1, Tamanho)
  else
    Result := StringOfChar(Caractere, Tamanho - Length(Texto)) + Texto;
end;

function PreencherDireita(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
begin
  if Length(Texto) >= Tamanho then
    Result := Copy(Texto, 1, Tamanho)
  else
    Result := Texto + StringOfChar(Caractere, Tamanho - Length(Texto));
end;

function PreencherCentro(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
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

// === SP - São Paulo ===
function ValidarIE_SP(const IE: string): Boolean;
var
  D: string;
  I, Soma, Resto, Dig1, Dig2: Integer;
begin
  D := RemoverMascara(IE);
  if (Length(D) <> 8) and (Length(D) <> 12) then
    Exit(False);
  if Length(D) = 8 then // Produtor rural
  begin
    if D[1] <> 'P' then Exit(False);
    D := Copy(D, 2, 8) + '000';
  end;

  Soma := 0;
  for I := 1 to 8 do
    Soma := Soma + StrToInt(D[I]) * (10 - I + IfThen(I > 1,1,0) + IfThen(I > 6,1,0));

  //Soma := Soma + StrToInt(D[I]) * (10 - I + (I > 1 ? 1 : 0) + (I > 6 ? 1 : 0));

  Resto := Soma mod 11;
  Dig1 := IfThen(Resto >= 10, 0, Resto);

  Soma := 0;
  for I := 1 to 11 do
    Soma := Soma + StrToInt(D[I]) * (12 - I + IfThen(I > 9,1,0));

  Resto := Soma mod 11;
  Dig2 := IfThen(Resto >= 10, 0, Resto);

  Result := (Dig1 = StrToInt(D[9])) and (Dig2 = StrToInt(D[12]));
end;

// === MG - Minas Gerais ===
function ValidarIE_MG(const IE: string): Boolean;
var
  D: string;
  I, Soma, Resto, Dig1, Dig2: Integer;
  Prod: string;
begin
  D := RemoverMascara(IE);
  if Length(D) <> 13 then Exit(False);

  Prod := Copy(D, 1, 11) + '0' + Copy(D, 12, 2);
  Soma := 0;
  for I := 1 to 12 do
    Soma := Soma + StrToInt(Prod[I]) * IfThen(I mod 2 = 0, 2, 1);

  Resto := Soma mod 10;
  Dig1 := IfThen(Resto = 0, 0, 10 - Resto);

  Soma := StrToInt(D[1]) * 3 + StrToInt(D[2]) * 2 + StrToInt(D[3]) * 11 + StrToInt(D[4]) * 10 +
          StrToInt(D[5]) * 9 + StrToInt(D[6]) * 8 + StrToInt(D[7]) * 7 + StrToInt(D[8]) * 6 +
          StrToInt(D[9]) * 5 + StrToInt(D[10]) * 4 + StrToInt(D[11]) * 3 + Dig1 * 2;

  Resto := Soma mod 11;
  Dig2 := IfThen(Resto = 0, 0, 11 - Resto);

  Result := (Dig1 = StrToInt(D[12])) and (Dig2 = StrToInt(D[13]));
end;

// === RJ - Rio de Janeiro ===
function ValidarIE_RJ(const IE: string): Boolean;
var
  D: string;
  Soma, Resto, Dig: Integer;
begin
  D := RemoverMascara(IE);
  if Length(D) <> 8 then Exit(False);

  Soma := StrToInt(D[1]) * 2 + StrToInt(D[2]) * 7 + StrToInt(D[3]) * 6 + StrToInt(D[4]) * 5 +
          StrToInt(D[5]) * 4 + StrToInt(D[6]) * 3 + StrToInt(D[7]) * 2;

  Resto := Soma mod 11;
  Dig := IfThen(Resto < 2, 0, 11 - Resto);

  Result := Dig = StrToInt(D[8]);
end;

// === RS - Rio Grande do Sul ===
function ValidarIE_RS(const IE: string): Boolean;
var
  D: string;
  Soma, Resto, Dig: Integer;
begin
  D := RemoverMascara(IE);
  if (Length(D) < 9) or (Length(D) > 10) then Exit(False);

  Soma := 0;
  Soma := Soma + StrToInt(D[1]) * 2;
  Soma := Soma + StrToInt(D[2]) * 9;
  Soma := Soma + StrToInt(D[3]) * 8;
  Soma := Soma + StrToInt(D[4]) * 7;
  Soma := Soma + StrToInt(D[5]) * 6;
  Soma := Soma + StrToInt(D[6]) * 5;
  Soma := Soma + StrToInt(D[7]) * 4;
  Soma := Soma + StrToInt(D[8]) * 3;
  Soma := Soma + StrToInt(D[9]) * 2;

  Resto := Soma mod 11;
  Dig := IfThen(Resto < 2, 0, 11 - Resto);

  Result := Dig = StrToInt(D[10]);
end;

// === PR - Paraná ===
function ValidarIE_PR(const IE: string): Boolean;
var
  D: string;
  Soma1, Soma2, Resto1, Resto2, Dig1, Dig2: Integer;
begin
  D := RemoverMascara(IE);
  if Length(D) <> 10 then Exit(False);

  Soma1 := StrToInt(D[1]) * 3 + StrToInt(D[2]) * 2 + StrToInt(D[3]) * 7 + StrToInt(D[4]) * 6 +
           StrToInt(D[5]) * 5 + StrToInt(D[6]) * 4 + StrToInt(D[7]) * 3 + StrToInt(D[8]) * 2;

  Resto1 := Soma1 mod 11;
  Dig1 := IfThen(Resto1 < 2, 0, 11 - Resto1);
  if Dig1 <> StrToInt(D[9]) then Exit(False);

  Soma2 := StrToInt(D[1]) * 4 + StrToInt(D[2]) * 3 + StrToInt(D[3]) * 2 + StrToInt(D[4]) * 7 +
           StrToInt(D[5]) * 6 + StrToInt(D[6]) * 5 + StrToInt(D[7]) * 4 + StrToInt(D[8]) * 3 +
           StrToInt(D[9]) * 2;

  Resto2 := Soma2 mod 11;
  Dig2 := IfThen(Resto2 < 2, 0, 11 - Resto2);

  Result := Dig2 = StrToInt(D[10]);
end;

// === SC - Santa Catarina ===
function ValidarIE_SC(const IE: string): Boolean;
var
  D: string;
  Soma, Resto, Dig: Integer;
begin
  D := RemoverMascara(IE);
  if Length(D) <> 9 then Exit(False);

  Soma := StrToInt(D[1]) * 9 + StrToInt(D[2]) * 8 + StrToInt(D[3]) * 7 + StrToInt(D[4]) * 6 +
          StrToInt(D[5]) * 5 + StrToInt(D[6]) * 4 + StrToInt(D[7]) * 3 + StrToInt(D[8]) * 2;

  Resto := Soma mod 11;
  Dig := IfThen(Resto < 2, 0, 11 - Resto);

  Result := Dig = StrToInt(D[9]);
end;

// === ES - Espírito Santo ===
function ValidarIE_ES(const IE: string): Boolean;
var
  D: string;
  Soma, Resto, Dig: Integer;
begin
  D := RemoverMascara(IE);
  if Length(D) <> 9 then Exit(False);

  Soma := StrToInt(D[1]) * 9 + StrToInt(D[2]) * 8 + StrToInt(D[3]) * 7 + StrToInt(D[4]) * 6 +
          StrToInt(D[5]) * 5 + StrToInt(D[6]) * 4 + StrToInt(D[7]) * 3 + StrToInt(D[8]) * 2;

  Resto := Soma mod 11;
  Dig := IfThen(Resto < 2, 0, 11 - Resto);

  Result := Dig = StrToInt(D[9]);
end;

// === BA - Bahia ===
function ValidarIE_BA(const IE: string): Boolean;
var
  D: string;
  Modulo: Integer;
  Soma, Resto, Dig1, Dig2, I: Integer;
begin
  D := RemoverMascara(IE);
  if (Length(D) <> 8) and (Length(D) <> 9) then Exit(False);

  if Length(D) = 8 then Modulo := 10 else Modulo := 11;

  Soma := 0;
  for I := 1 to Length(D)-2 do
    Soma := Soma + StrToInt(D[I]) * (Length(D) - I + 1);

  Resto := Soma mod Modulo;
  Dig2 := IfThen(Resto < 2, 0, Modulo - Resto);
  if Dig2 <> StrToInt(D[Length(D)]) then Exit(False);

  Soma := 0;
  for I := 1 to Length(D)-1 do
    Soma := Soma + StrToInt(D[I]) * (Length(D) - I + 2);

  Resto := Soma mod Modulo;
  Dig1 := IfThen(Resto < 2, 0, Modulo - Resto);

  Result := Dig1 = StrToInt(D[Length(D)-1]);
end;

// === Demais estados (padrão genérico ou específico)
function ValidarIE_Outros(const IE: string): Boolean;
begin
  // Para estados sem regra específica, aceita apenas se for numérico e tamanho padrão
  Result := (Length(RemoverMascara(IE)) in [8,9,10,12,14]);
end;

// Função principal
function ValidarInscricaoEstadual(const UF, IE: string): Boolean;
var
  IETrim: string;
begin
  IETrim := Trim(UpperCase(IE));

  // Aceita "ISENTO" em qualquer UF
  if (IETrim = 'ISENTO') or (IETrim = 'ISENTA') then
    Exit(True);

  case AnsiUpperCase(Trim(UF)) of
    'AC': Result := ValidarIE_Outros(IE); // Acre (padrão 13 dígitos)
    'AL': Result := ValidarIE_Outros(IE); // Alagoas
    'AP': Result := ValidarIE_Outros(IE); // Amapá
    'AM': Result := ValidarIE_Outros(IE); // Amazonas
    'BA': Result := ValidarIE_BA(IE);
    'CE': Result := ValidarIE_Outros(IE); // Ceará
    'DF': Result := ValidarIE_Outros(IE); // Distrito Federal
    'ES': Result := ValidarIE_ES(IE);
    'GO': Result := ValidarIE_Outros(IE); // Goiás
    'MA': Result := ValidarIE_Outros(IE); // Maranhão
    'MT': Result := ValidarIE_Outros(IE); // Mato Grosso
    'MS': Result := ValidarIE_Outros(IE); // Mato Grosso do Sul
    'MG': Result := ValidarIE_MG(IE);
    'PA': Result := ValidarIE_Outros(IE); // Pará
    'PB': Result := ValidarIE_Outros(IE); // Paraíba
    'PR': Result := ValidarIE_PR(IE);
    'PE': Result := ValidarIE_Outros(IE); // Pernambuco
    'PI': Result := ValidarIE_Outros(IE); // Piauí
    'RJ': Result := ValidarIE_RJ(IE);
    'RN': Result := ValidarIE_Outros(IE); // Rio Grande do Norte
    'RS': Result := ValidarIE_RS(IE);
    'RO': Result := ValidarIE_Outros(IE); // Rondônia
    'RR': Result := ValidarIE_Outros(IE); // Roraima
    'SC': Result := ValidarIE_SC(IE);
    'SP': Result := ValidarIE_SP(IE);
    'SE': Result := ValidarIE_Outros(IE); // Sergipe
    'TO': Result := ValidarIE_Outros(IE); // Tocantins
    else
      Result := False; // UF não reconhecida
  end;
end;

procedure GravarLog(const Msg: string);
var
  F: TextFile;
  LogPath: string;
begin
  LogPath := ExtractFilePath(ParamStr(0)) + C_ARQUIVO_LOG;
  AssignFile(F, LogPath);
  try
    if FileExists(LogPath) then
      Append(F)
    else
      Rewrite(F);
    WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg);
  finally
    CloseFile(F);
  end;
end;

procedure TratarErro_Geral(E: Exception; const Mensagem: string; Tipo: TTipoErro;
  const UnitName: string; Line: Integer);
var
  MsgLog, MsgUsuario: string;
begin
  // Monta mensagem para log
  MsgLog := Mensagem;
  if Assigned(E) then
    MsgLog := MsgLog + ' | Erro: ' + E.Message;
  if UnitName <> '' then
    MsgLog := MsgLog + ' | Unit: ' + UnitName;
  if Line > 0 then
    MsgLog := MsgLog + ' | Linha: ' + IntToStr(Line);

  // Monta mensagem para usuário
  MsgUsuario := Mensagem;
  if Assigned(E) then
    MsgUsuario := MsgUsuario + sLineBreak + sLineBreak + 'Detalhes: ' + E.Message;

  // Executa conforme o tipo solicitado
  case Tipo of
    teMensagem: MsgErro(MsgUsuario);
    teLog: GravarLog(MsgLog);
    teAmbos:
      begin
        MsgErro(MsgUsuario);
        GravarLog(MsgLog);
      end;
  end;
end;

procedure Log(const Msg: string; const UnitName: string = ''; Line: Integer = 0);
var
  MsgLog: string;
begin
  MsgLog := Msg;
  if UnitName <> '' then
    MsgLog := MsgLog + ' | Unit: ' + UnitName;
  if Line > 0 then
    MsgLog := MsgLog + ' | Linha: ' + IntToStr(Line);
  GravarLog(MsgLog);
end;

function GerarTokenSimples(const CNPJ: string; const ChaveSecreta: string ): string;
begin
     //
end;



end.


(*

COMO USAR
uses
  uFuncoes;

// Exemplo de uso
if ValidarDocumento('123.456.789-00') then
  ShowMessage('CPF válido!');

ShowMessage(PreencherEsquerda('123', 10, '0'));     // '0000000123'
ShowMessage(PreencherDireita('123', 10, '0'));      // '1230000000'
ShowMessage(PreencherCentro('123', 10, '-'));       // '---123----'

if ValidarInscricaoEstadual('SP', '110.042.490.114') then
  ShowMessage('IE SP válida');

if ValidarInscricaoEstadual('MG', '0623079040081') then
  ShowMessage('IE MG válida');

if ValidarInscricaoEstadual('RJ', '78.775.719') then
  ShowMessage('IE RJ válida');

if ValidarInscricaoEstadual('PR', '1234567850') then
  ShowMessage('IE PR válida');

if ValidarInscricaoEstadual('SC', 'ISENTO') then
  ShowMessage('Aceito como ISENTO');


COMO USAR
uses
  uTratamentoErro;

// Exemplo 1: Apenas mensagem ao usuário
try
  StrToInt('abc');
except
  on E: EConvertError do
    TratarErro(E, 'Valor inválido informado.', teMensagem);
end;

// Exemplo 2: Apenas log (não incomoda o usuário)
try
  // alguma operação silenciosa
except
  on E: Exception do
    TratarErro(E, 'Falha ao carregar configuração.', teLog, 'frmPrincipal', 150);
end;

// Exemplo 3: Mensagem + log (padrão recomendado)
try
  // código crítico
  DividirPorZero;
except
  on E: Exception do
    TratarErro(E, 'Ocorreu um erro inesperado.', teAmbos, 'uFuncoes', __LINE__);
end;

// Exemplo 4: Apenas log personalizado
Log('Usuário logado: admin', 'frmLogin', 45);



  *)





