unit uBase.Functions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Math, DB, memds, fpjson, jsonparser, Variants, DateUtils;

{$Region 'Funçoes'}
function EndPath:String;
function NameEXE(aSemExtensao:Boolean=True):String;
function ConfigFile:String;
function LogFile:String;
function PreencherEsquerda(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
function PreencherDireita(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
function PreencherCentro(const Texto: string; Tamanho: Integer; Caractere: Char = ' '): string;
function RemoverMascara(const S: string): string;
function ApenasNumeros(const Texto: string): Boolean;
procedure PreencherDataSetDeJSONArray(
  const AJSONArray: TJSONArray;
  const ADataSet: TDataSet;
  const IgnorarCampos: array of string); // campos opcionais para ignorar
procedure PopularMemDataDoJSON(FDados: TJSONArray; mdRegistro: TMemDataset);

{$EndRegion}

{$Region 'Procedures'}
procedure SaveLog(
  const aMessage:String;
  const ADataHora:Boolean=True);
{$EndRegion}

implementation

function CamelToSnake(const Campo: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Campo) do
  begin
    if CharInSet(Campo[I], ['A'..'Z']) then
    begin
      if I > 1 then
        Result := Result + '_';
      Result := Result + LowerCase(Campo[I]);
    end
    else
      Result := Result + Campo[I];
  end;
end;

function EndPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function NameEXE(aSemExtensao:Boolean=True): String;
begin
  if aSemExtensao then
    Result := ChangeFileExt(ExtractFileName(ParamStr(0)),'')
  else
    Result := ExtractFileName(ParamStr(0));
end;

function ConfigFile: String;
begin
  Result := EndPath + NameEXE + '.INI';
end;

function LogFile: String;
begin
  Result := EndPath + NameEXE + '.LOG';
end;

function PreencherEsquerda(const Texto: string; Tamanho: Integer; Caractere: Char): string;
begin
  if Length(Texto) >= Tamanho then
    Result := Copy(Texto, 1, Tamanho)
  else
    Result := StringOfChar(Caractere, Tamanho - Length(Texto)) + Texto;
end;

function PreencherDireita(const Texto: string; Tamanho: Integer; Caractere: Char): string;
begin
  if Length(Texto) >= Tamanho then
    Result := Copy(Texto, 1, Tamanho)
  else
    Result := Texto + StringOfChar(Caractere, Tamanho - Length(Texto));
end;

function PreencherCentro(const Texto: string; Tamanho: Integer; Caractere: Char): string;
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

procedure SaveLog(
  const aMessage: String;
  const ADataHora:Boolean=True);
var
  F: TextFile;
begin
  AssignFile(F, LogFile);
  try
    if FileExists(LogFile) then
      Append(F)
    else
      Rewrite(F);

    if ADataHora then
       WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ': ' + aMessage)
    else
      WriteLn(F,aMessage);

  finally
    CloseFile(F);
  end;
end;

function RemoverMascara(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    if S[I] in ['0'..'9'] then
      Result := Result + S[I];
end;

function ApenasNumeros(const Texto: string): Boolean;
var
  i: Integer;
begin
  Result := (Length(Texto) > 0); // se for vazio, retorna False
  for i := 1 to Length(Texto) do
  begin
    if not (Texto[i] in ['0'..'9']) then
    begin
      Result := False;
      Exit; // encontrou algo que não é número
    end;
  end;
end;

procedure PreencherDataSetDeJSONArray(
  const AJSONArray: TJSONArray;
  const ADataSet: TDataSet;
  const IgnorarCampos: array of string); // campos opcionais para ignorar
var
  i, j: Integer;
  JsonObj: TJSONObject;
  Field: TField;
  Valor: Variant;
  CampoIgnorado: Boolean;
  NomeCampo: string;
begin
  if not Assigned(AJSONArray) or not Assigned(ADataSet) then
    raise Exception.Create('JSONArray ou DataSet não informado');

  ADataSet.DisableControls;
  try
    for i := 0 to AJSONArray.Count - 1 do
    begin
      JsonObj := AJSONArray.Items[i] as TJSONObject;
      if not Assigned(JsonObj) then Continue;

      ADataSet.Append;

      for j := 0 to ADataSet.FieldCount - 1 do
      begin
        Field := ADataSet.Fields[j];
        NomeCampo := Field.FieldName;

        // Ignora campos da lista (opcional)
        (*
        CampoIgnorado := False;
        for var Ignorado in IgnorarCampos do
          if SameText(NomeCampo, Ignorado) then
          begin
            CampoIgnorado := True;
            Break;
          end;
          *)
        if CampoIgnorado then Continue;

        // Verifica se o campo existe no JSON
        if JsonObj.Find(NomeCampo) <> nil then
        begin
          Valor := JsonObj.Get(NomeCampo);

          // Trata nulo
          if VarIsNull(Valor) or VarIsEmpty(Valor) then
          begin
            Field.Clear;
            Continue;
          end;

          // Valida e atribui conforme o tipo do campo do dataset
          case Field.DataType of
            ftString,
            ftMemo,
            ftWideString,
            ftFixedChar:
              Field.AsString := Valor;

            ftInteger,
            ftSmallint,
            ftWord,
            ftLargeint:
              Field.AsInteger := StrToIntDef(VarToStr(Valor), 0);

            ftFloat,
            ftCurrency,
            ftBCD,
            ftFMTBcd:
              Field.AsFloat := StrToFloatDef(VarToStr(Valor), 0.0);

            ftBoolean:
              Field.AsBoolean := (VarToStr(Valor) = 'true') or (VarToStr(Valor) = '1');

            ftDate,
            ftDateTime,
            ftTimeStamp:
            begin
              if VarIsStr(Valor) then
                Field.AsDateTime := ISO8601ToDate(VarToStr(Valor))
              else
                Field.AsDateTime := VarToDateTime(Valor);
            end;

            else
              // Tipo não tratado → tenta conversão genérica
              try
                Field.Value := Valor;
              except
                Field.Clear;
              end;
          end;
        end
        else
          Field.Clear; // Campo não existe no JSON → limpa
      end;

      ADataSet.Post;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

procedure PopularMemDataDoJSON(FDados: TJSONArray; mdRegistro: TMemDataset);
var
  x: Integer;
  JsonObj: TJSONObject;
  FieldName, FieldNameDS, Valor: string;
  I: Integer;
begin
  mdRegistro.DisableControls;
  try
    mdRegistro.Close;
    mdRegistro.Open;

    for x := 0 to Pred(FDados.Count) do
    begin
      JsonObj := FDados[x] as TJSONObject;

      mdRegistro.Insert;

      // Percorre todos os pares do objeto JSON
      for I := 0 to JsonObj.Count - 1 do
      begin
        FieldName := JsonObj.Names[I];                 // ex: "idEmpresa"
        FieldNameDS := CamelToSnake(FieldName);          // → "id_empresa"

        // Verifica se o campo existe no dataset
        if mdRegistro.FieldDefs.IndexOf(FieldNameDS) >= 0 then
        begin
          if JsonObj[FieldName].IsNull then
            mdRegistro.FieldByName(FieldNameDS).Clear
          else
          begin
            Valor := JsonObj[FieldName].Value;       // Valor como string
            // Converte para o tipo correto do campo
            case mdRegistro.FieldByName(FieldNameDS).DataType of
              ftInteger, ftSmallint, ftLargeint:
                mdRegistro.FieldByName(FieldNameDS).AsInteger := StrToIntDef(Valor, 0);

              ftString, ftMemo:
                mdRegistro.FieldByName(FieldNameDS).AsString := Valor;

              ftDateTime:
                mdRegistro.FieldByName(FieldNameDS).AsDateTime := ISO8601ToDateDef(Valor, 0);

              ftDate:
                mdRegistro.FieldByName(FieldNameDS).AsDateTime := DateOf(Valor, 0);

              //DateOf(FJson_CD.Floats['validade']);

              ftFloat, ftCurrency:
                mdRegistro.FieldByName(FieldName).AsFloat := StrToFloatDef(Valor, 0);

              ftBoolean:
                mdRegistro.FieldByName(FieldNameDS).AsBoolean := StrToBoolDef(Valor, False);

              else
                mdRegistro.FieldByName(FieldNameDS).AsString := Valor; // fallback
            end;
          end;
        end;
      end;

      mdRegistro.Post;
    end;
  finally
    mdRegistro.EnableControls;
  end;
end;




end.

