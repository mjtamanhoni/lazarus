unit uCripto_Descrito;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Math, DateUtils, base64;

const
  LetrasMinusculas   = 'abcdefghijklmnopqrstuvwxyz';
  LetrasMaiusculas   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Numeros            = '0123456789';
  Especiais          = '~!@#$%^&*()_+-=[]{}|;:''",./<>?';
  AcentosMinus       = 'áàãâéêíóôõúç';
  AcentosMaius       = 'ÁÀÃÂÉÊÍÓÔÕÚÇ';

function Criptografar(const aTexto: string): string;
function Descriptografar(const aTexto: string): string;

implementation

function SubstituirString(const S: string; ProbabilidadeTroca: Byte = 60): string;
var
  TodosChars: string;
  i: Integer;
  r: Integer;
begin
  try
    // Todos os caracteres possíveis para substituição
    TodosChars :=
      LetrasMinusculas +
      LetrasMaiusculas +
      Numeros +
      Especiais +
      AcentosMinus +
      AcentosMaius;

    SetLength(Result, Length(S));

    Randomize;  // só precisa chamar uma vez (pode mover para inicialização do programa)

    for i := 1 to Length(S) do
    begin
      // Decide se troca ou mantém o caractere original
      if Random(100) < ProbabilidadeTroca then
      begin
        // Troca por um caractere aleatório da lista completa
        r := Random(Length(TodosChars)) + 1;
        Result[i] := TodosChars[r];
      end
      else
      begin
        // Mantém o caractere original
        Result[i] := S[i];
      end;
    end;
  except
    On E:Exception do
      Raise Exception.Create(E.Message);
  end;
end;

function Embaralhar(const S: string): string;
var
  I, J: Integer;
  Temp: Char;
  Arr: array of Char;
begin
  try
    Randomize;
    // Copia a string para um array de caracteres
    SetLength(Arr, Length(S));
    for I := 1 to Length(S) do
      Arr[I-1] := S[I];

    // Algoritmo de Fisher-Yates para embaralhar
    for I := High(Arr) downto 1 do
    begin
      J := Random(I+1);
      Temp := Arr[I];
      Arr[I] := Arr[J];
      Arr[J] := Temp;
    end;

    // Reconstrói a string
    Result := '';
    for I := 0 to High(Arr) do
      Result := Result + Arr[I];
  except
    On E:Exception do
      Raise Exception.Create(E.Message);
  end;
end;

function Compl_Start:String;
var
  FDataAtual :String;
  FDataAtual_Embaralhada :String;
  FDataAtual_Substituida :String;
begin
  try
    //Obtem data atual (Data do início da senha)
    FDataAtual := DateTimeToStr(Now);
    //Embaralhar data atual...
    FDataAtual_Embaralhada := Embaralhar(FDataAtual);
    //Substituir strings da data atual Embaralhada...
    FDataAtual_Substituida := SubstituirString(FDataAtual_Embaralhada);
    Result := FDataAtual_Substituida;
  except
    On E:Exception do
      Raise Exception.Create(E.Message);
  end;
end;

function Compl_End:String;
var
  FNumero :Integer;
  FDataNova :String;
  FDataNova_Embaralhada :String;
  FDataNova_Substituida :String;
begin
  try
    //Gerar número aleatório para alterar a data final
    FNumero := (Random(2001) - 1000);
    //Gera nova data (data do fim da senha
    FDataNova := DateTimeToStr(IncDay(IncHour(IncMinute(IncSecond(Now,FNumero),FNumero),FNumero),FNumero));
    //Embaralhar data nova
    FDataNova_Embaralhada := Embaralhar(FDataNova);
    //Substituir data nova
    FDataNova_Substituida := SubstituirString(FDataNova_Embaralhada);
    Result := FDataNova_Substituida;
  except
    On E:Exception do
      Raise Exception.Create(E.Message);
  end;
end;

function RemoverInicioEFim(const Texto: string; RemoverInicio, RemoverFim: Integer): string;
begin
  try
    Result := '';

    if RemoverInicio < 0 then RemoverInicio := 0;
    if RemoverFim < 0 then RemoverFim := 0;

    if RemoverInicio + RemoverFim >= Length(Texto) then
      Raise Exception.Create('Remoção maior que o tamanho da string');  // ou raise Exception.Create('Remoção maior que o tamanho da string');

    Result := Copy(Texto,
                   RemoverInicio + 1,
                   Length(Texto) - RemoverInicio - RemoverFim);
  except
    On E:Exception do
      Raise Exception.Create(E.Message);
  end;
end;


function Criptografar(const aTexto: string): string;
begin
  try
    if aTexto.IsEmpty then
      Raise Exception.Create('Não foi informado o conteúdo');

    Result := EncodeStringBase64(Compl_Start + aTexto + Compl_End);
  except
    On E:Exception do
      Raise Exception.Create(E.Message);
  end;
end;

function Descriptografar(const aTexto: string): string;
begin
  try
    if aTexto.IsEmpty then
      Raise Exception.Create('Não foi informado o conteúdo');

    Result := RemoverInicioEFim(DecodeStringBase64(aTexto),19,19);
  except
    On E:Exception do
      Raise Exception.Create(E.Message);
  end;
end;


end.

