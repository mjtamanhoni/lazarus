unit uCriptografiaSenha;

{$mode Delphi}

interface

uses
  Classes, SysUtils,
  DCPrijndael, DCPsha256,
  base64;  // ← unit que tem EncodeStringBase64 e DecodeStringBase64

const
  // Em produção: carregue de arquivo externo ou variável de ambiente!
  CHAVE_FIXA = 'kX9pQvL2mW8rT5jF3nY6hB0cZ4qA7eD1gU2iO5lS8vR3tP6wM9xN4kJ7yF2bH';

function CriptografarSenha(const Senha: string): string;
function DescriptografarSenha(const TextoCriptografado: string): string;

implementation

function DerivarChave(const ChaveSecreta: string): TBytes;
var
  Hash: TDCP_sha256;
begin
  Hash := TDCP_sha256.Create(nil);
  try
    Hash.Init;
    Hash.UpdateStr(ChaveSecreta);
    SetLength(Result, 32); // AES-256 = 32 bytes
    Hash.Final(Result[0]);
  finally
    Hash.Free;
  end;
end;

function CriptografarSenha(const Senha: string): string;
var
  Cipher: TDCP_rijndael;
  Dados, Cripto: TBytes;
  Key: TBytes;
  TamCripto: Integer;
  TempStr: AnsiString;
begin
  Key := DerivarChave(CHAVE_FIXA);
  Dados := TEncoding.UTF8.GetBytes(Senha);

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(Key, 256, nil);
    SetLength(Cripto, Length(Dados) + Cipher.BlockSize);
    Cipher.EncryptCBC(Dados[0], Cripto[0], Length(Dados));

    // Remove padding (simplificado - assume padding zero)
    TamCripto := Length(Cripto);
    while (TamCripto > 0) and (Cripto[TamCripto-1] = 0) do
      Dec(TamCripto);
    SetLength(Cripto, TamCripto);

    // Converte TBytes para AnsiString e codifica
    SetLength(TempStr, Length(Cripto));
    Move(Cripto[0], TempStr[1], Length(Cripto));
    Result := EncodeStringBase64(TempStr);
  finally
    Cipher.Free;
  end;
end;

function DescriptografarSenha(const TextoCriptografado: string): string;
var
  Cipher: TDCP_rijndael;
  Dados, Cripto: TBytes;
  Key: TBytes;
  TamDados: Integer;
  TempStr: AnsiString;
begin
  Key := DerivarChave(CHAVE_FIXA);

  // Decodifica Base64 → retorna AnsiString
  TempStr := DecodeStringBase64(TextoCriptografado);

  // Converte AnsiString para TBytes
  SetLength(Cripto, Length(TempStr));
  Move(TempStr[1], Cripto[0], Length(TempStr));

  Cipher := TDCP_rijndael.Create(nil);
  try
    Cipher.Init(Key, 256, nil);
    SetLength(Dados, Length(Cripto) + Cipher.BlockSize);
    Cipher.DecryptCBC(Cripto[0], Dados[0], Length(Cripto));

    // Remove padding
    TamDados := Length(Cripto);
    while (TamDados > 0) and (Dados[TamDados-1] = 0) do
      Dec(TamDados);
    SetLength(Dados, TamDados);

    Result := TEncoding.UTF8.GetString(Dados);
  finally
    Cipher.Free;
  end;
end;

end.
