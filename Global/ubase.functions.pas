unit uBase.Functions;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Math, DB, memds, fpjson, jsonparser, Variants, DateUtils,
  FileInfo, winpeimagereader;

{$Region 'Funçoes'}
function EndPath:String;
function NameEXE(aSemExtensao:Boolean=True):String;
function ConfigFile:String;
function LogFile:String;
function LogFile_JSon: String;
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
function StrISOToDateTime(const DataStr: string): TDateTime;

{$EndRegion}

{$Region 'Procedures'}
procedure SaveLog(
  const aMessage: String;
  const ADataHora:Boolean=True);
function GetVersionValue(const AKey: string): string;
procedure GravarLogJSON_(const AForm,ADesc,AUnit: string; const E: Exception);
procedure GravarLogJSON(const AForm, ADesc, AUnit: string; const E: Exception);
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

function LogFile_JSon: String;
begin
  Result := EndPath + NameEXE + FormatDateTime('yyyymm', Now) + '_Log.json'
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

              ftDate,ftDateTime:
                mdRegistro.FieldByName(FieldNameDS).AsDateTime := ISO8601ToDateDef(Valor, 0);

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

function StrISOToDateTime(const DataStr: string): TDateTime;
begin
  Result := 0;

  if Trim(DataStr) = '' then Exit;

  try
    // Tenta converter diretamente no formato ISO (YYYY-MM-DD)
    Result := EncodeDate(
      StrToInt(Copy(DataStr, 1, 4)),     // Ano
      StrToInt(Copy(DataStr, 6, 2)),     // Mês
      StrToInt(Copy(DataStr, 9, 2))      // Dia
    );

    // Se tiver hora (YYYY-MM-DDTHH:MM:SS), converte também
    if Pos('T', DataStr) > 0 then
    begin
      Result := Result + EncodeTime(
        StrToInt(Copy(DataStr, 12, 2)),  // Hora
        StrToInt(Copy(DataStr, 15, 2)),  // Minuto
        StrToInt(Copy(DataStr, 18, 2)),  // Segundo
        0                                // Milissegundo
      );
    end;
  except
    on E: Exception do
      raise Exception.Create('Converte Data: ' + E.Message);
  end;
end;

procedure GravarLogJSON_(const AForm,ADesc,AUnit: string; const E: Exception);
var
  ArquivoLog: TStringList;
  CaminhoDiretorio, NomeArquivo: string;
  ArrayJSON: TJSONArray;
  ObjetoErro: TJSONObject;
  ConteudoExistente: string;
  NomeForm, CaptionForm: string;
begin
  NomeForm := AForm;
  CaptionForm := ADesc;

  //CaminhoDiretorio := ExtractFilePath(ParamStr(0)) + 'logs';
  //if not DirectoryExists(CaminhoDiretorio) then ForceDirectories(CaminhoDiretorio);

  // Formato: Log_23022026.json
  //NomeArquivo := CaminhoDiretorio + DirectorySeparator + 'Log_' + FormatDateTime('ddmmyyyy', Now) + '.json';
  NomeArquivo := LogFile_JSon;

  ArquivoLog := TStringList.Create;
  try
    if FileExists(NomeArquivo) then
    begin
      ArquivoLog.LoadFromFile(NomeArquivo);
      ConteudoExistente := ArquivoLog.Text;
      if ConteudoExistente = '' then ConteudoExistente := '[]';
      ArrayJSON := GetJSON(ConteudoExistente) as TJSONArray;
    end
    else
      ArrayJSON := TJSONArray.Create;

    try
      ObjetoErro := TJSONObject.Create;
      ObjetoErro.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
      ObjetoErro.Add('company_name', GetVersionValue('CompanyName'));
      ObjetoErro.Add('description', GetVersionValue('FileDescription'));
      ObjetoErro.Add('copyright', GetVersionValue('LegalCopyright'));
      ObjetoErro.Add('product_name', GetVersionValue('ProductName'));
      ObjetoErro.Add('version', GetVersionValue('FileVersion'));
      ObjetoErro.Add('usuario_pc', GetEnvironmentVariable('USERNAME'));
      ObjetoErro.Add('form_name', NomeForm);
      ObjetoErro.Add('caption_name',CaptionForm);
      ObjetoErro.Add('unit', AUnit);
      ObjetoErro.Add('exception_class', E.ClassName);
      ObjetoErro.Add('message', E.Message);

      ArrayJSON.Add(ObjetoErro);

      // Salva com indentação para ser legível
      ArquivoLog.Text := ArrayJSON.FormatJSON();
      ArquivoLog.SaveToFile(NomeArquivo);
    finally
      ArrayJSON.Free;
    end;
  finally
    ArquivoLog.Free;
  end;
end;

function GetVersionValue(const AKey: string): string;
var
  FileVerInfo: TFileVersionInfo;
begin
  Result := 'N/A';
  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    // O StringFileInfo retorna os valores baseados nas chaves do projeto
    Result := FileVerInfo.VersionStrings.Values[AKey];
  finally
    FileVerInfo.Free;
  end;
end;

procedure GravarLogJSON(const AForm, ADesc, AUnit: string; const E: Exception);
var
  ArquivoLog: TStringList;
  NomeArquivo: string;
  ArrayRaiz, ArrayLogsDia: TJSONArray;
  ObjetoDia, ObjetoErro: TJSONObject;
  ConteudoExistente: string;
  I: Integer;
  DataHoje: string;
  EncontrouData: Boolean;
begin
  DataHoje := FormatDateTime('yyyy-mm-dd', Now);
  NomeArquivo := LogFile_JSon; // Certifique-se que esta variável/constante global existe
  EncontrouData := False;

  ArquivoLog := TStringList.Create;
  try
    // 1. Carregar ou Criar a Raiz (Array principal)
    if FileExists(NomeArquivo) then
    begin
      ArquivoLog.LoadFromFile(NomeArquivo);
      ConteudoExistente := ArquivoLog.Text;
      if (ConteudoExistente = '') then ConteudoExistente := '[]';
      ArrayRaiz := GetJSON(ConteudoExistente) as TJSONArray;
    end
    else
      ArrayRaiz := TJSONArray.Create;

    try
      // 2. Procurar se já existe um objeto para a data de hoje
      ObjetoDia := nil;
      for I := 0 to ArrayRaiz.Count - 1 do
      begin
        if ArrayRaiz.Objects[I].Strings['date'] = DataHoje then
        begin
          ObjetoDia := ArrayRaiz.Objects[I];
          EncontrouData := True;
          Break;
        end;
      end;

      // 3. Se não encontrar a data, cria o cabeçalho do dia
      if not Assigned(ObjetoDia) then
      begin
        ObjetoDia := TJSONObject.Create;
        ObjetoDia.Add('date', DataHoje);
        ObjetoDia.Add('company_name', GetVersionValue('CompanyName'));
        ObjetoDia.Add('description', GetVersionValue('FileDescription'));
        ObjetoDia.Add('copyright', GetVersionValue('LegalCopyright'));
        ObjetoDia.Add('product_name', GetVersionValue('ProductName'));
        ObjetoDia.Add('version', GetVersionValue('FileVersion'));
        ObjetoDia.Add('usuario_pc', GetEnvironmentVariable('USERNAME'));

        // Cria o array onde os erros do dia ficarão
        ArrayLogsDia := TJSONArray.Create;
        ObjetoDia.Add('logs', ArrayLogsDia);

        ArrayRaiz.Add(ObjetoDia);
      end
      else
      begin
        // Se já existe, pegamos o array de logs existente
        ArrayLogsDia := ObjetoDia.Arrays['logs'];
      end;

      // 4. Criar o objeto do erro específico
      ObjetoErro := TJSONObject.Create;
      ObjetoErro.Add('time', FormatDateTime('hh:nn:ss', Now));
      ObjetoErro.Add('form_name', AForm);
      ObjetoErro.Add('caption_name', ADesc);
      ObjetoErro.Add('unit', AUnit);
      ObjetoErro.Add('exception_class', E.ClassName);
      ObjetoErro.Add('message', E.Message);

      // 5. Adicionar o erro ao array do dia
      ArrayLogsDia.Add(ObjetoErro);

      // 6. Salvar no arquivo
      ArquivoLog.Text := ArrayRaiz.FormatJSON();
      ArquivoLog.SaveToFile(NomeArquivo);

    finally
      ArrayRaiz.Free;
    end;
  finally
    ArquivoLog.Free;
  end;
end;




end.

