unit uGerar_JSon_De_Excel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, ole2, comserv, // OLE no Lazarus
  fpjson, jsonparser, // JSON no Free Pascal
  uWaitDialog; // Sua unit de wait já convertida

function Gerar_JSON_Do_Excel(const ExcelPath: string; const Aba, LinhaInicio: Integer; ACompNome: string = ''): string;
function SanitizeText(const Input: string): string;
function ExcelCellToString(const CellValue: Variant): string;
function AdicionaSufixo(const Caminho, Sufixo: string): string;
procedure MostrarJsonBonito(const JsonText: string; AMemo: TMemo);

implementation

const
  MaxLineLength = 200;

function Gerar_JSON_Do_Excel(const ExcelPath: string; const Aba, LinhaInicio: Integer; ACompNome: string = ''): string;
var
  Excel, Workbook, Sheet: Variant;
  Row, LastRow, Col: Integer;
  Colunas: TStringList;
  Valor: Variant;
  JSONArr: TJSONArray;
  JSONObj: TJSONObject;
  JSONText, JSONFilePath: string;
  JSONFile: TStringList;
  FTotal: Integer;
begin
  Colunas := TStringList.Create;
  JSONArr := TJSONArray.Create;
  JSONFile := TStringList.Create;
  FTotal := 0;
  try
    try
      FTotal := 0;
      WaitBegin('Gerando arquivo: ' + ExtractFileName(ExcelPath), FTotal, True);

      // Criação do Excel via OLE no Lazarus
      Excel := CreateOleObject('Excel.Application');
      Excel.Visible := False;
      Workbook := Excel.Workbooks.Open(ExcelPath);
      Sheet := Workbook.Worksheets.Item[Aba]; // Aba por índice

      LastRow := Sheet.UsedRange.Rows.Count;

      // Captura nomes das colunas (linha 1)
      Col := 1;
      while not VarIsClear(Sheet.Cells[1, Col].Value) do
      begin
        Colunas.Add(SanitizeText(Trim(VarToStr(Sheet.Cells[1, Col].Value))));
        Inc(Col);
      end;

      FTotal := LastRow - LinhaInicio + 1;
      for Row := LinhaInicio to LastRow do
      begin
        if WaitCancelled then
        begin
          WaitUpdateStatus('Cancelado pelo usuário');
          Break;
        end;

        WaitUpdateStatus('Registro: ' + ExcelCellToString(Sheet.Cells[Row, 1].Value));
        WaitUpdateProgress(Row - LinhaInicio + 1);
        WaitUpdateCount(Row - LinhaInicio + 1, FTotal);

        JSONObj := TJSONObject.Create;
        for Col := 1 to Colunas.Count do
        begin
          Valor := Sheet.Cells[Row, Col].Value;
          if VarIsNull(Valor) or VarIsClear(Valor) then
            JSONObj.Add(Colunas[Col - 1], TJSONNull.Create)
          else
            JSONObj.Add(Colunas[Col - 1], VarToStr(Valor));
        end;
        JSONArr.Add(JSONObj);
      end;

      Workbook.Close(False);
      Excel.Quit;

      JSONText := JSONArr.AsJSON;
      Result := JSONText;

      if Trim(ACompNome) <> '' then
        JSONFilePath := ChangeFileExt(ExtractFilePath(ExcelPath) + ACompNome, '.json')
      else
        JSONFilePath := ChangeFileExt(ExcelPath, '.json');

      JSONFile.Text := JSONText;
      JSONFile.SaveToFile(JSONFilePath);

    except
      on E: Exception do
        raise Exception.Create('Erro ao gerar JSON: ' + E.Message);
    end;
  finally
    Colunas.Free;
    JSONArr.Free;
    JSONFile.Free;
    WaitEnd;
  end;
end;

function SanitizeText(const Input: string): string;
const
  Acentos: array[0..11] of Char = ('á','à','ã','â','é','ê','í','ó','ô','õ','ú','ç');
  SemAcento: array[0..11] of Char = ('a','a','a','a','e','e','i','o','o','o','u','c');
  AcentosMaius: array[0..11] of Char = ('Á','À','Ã','Â','É','Ê','Í','Ó','Ô','Õ','Ú','Ç');
  SemAcentoMaius: array[0..11] of Char = ('A','A','A','A','E','E','I','O','O','O','U','C');
var
  i: Integer;
begin
  Result := Input;

  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '_', [rfReplaceAll]);

  for i := Low(Acentos) to High(Acentos) do
    Result := StringReplace(Result, Acentos[i], SemAcento[i], [rfReplaceAll]);
  for i := Low(AcentosMaius) to High(AcentosMaius) do
    Result := StringReplace(Result, AcentosMaius[i], SemAcentoMaius[i], [rfReplaceAll]);

  i := 1;
  while i <= Length(Result) do
  begin
    if not (Result[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Delete(Result, i, 1)
    else
      Inc(i);
  end;
end;

function ExcelCellToString(const CellValue: Variant): string;
begin
  Result := '';

  if VarIsNull(CellValue) or VarIsClear(CellValue) then
    Exit;

  case VarType(CellValue) and varTypeMask of
    varEmpty, varNull: Result := '';
    varSmallint, varInteger, varByte, varInt64: Result := IntToStr(CellValue);
    varSingle, varDouble, varCurrency: Result := FormatFloat('#,##0.##', CellValue);
    varDate: Result := FormatDateTime('dd/mm/yyyy hh:nn:ss', CellValue);
    varOleStr, varString: Result := CellValue;
    varBoolean: if CellValue then Result := 'VERDADEIRO' else Result := 'FALSO';
    else
      try
        Result := VarToStr(CellValue);
      except
        Result := '';
      end;
  end;

  Result := Trim(Result);
end;

function AdicionaSufixo(const Caminho, Sufixo: string): string;
var
  Nome, Ext: string;
begin
  Nome := ChangeFileExt(ExtractFileName(Caminho), '');
  Ext := ExtractFileExt(Caminho);
  Result := ExtractFilePath(Caminho) + Nome + Sufixo + Ext;
end;

procedure MostrarJsonBonito(const JsonText: string; AMemo: TMemo);
var
  Parser: TJSONParser;
  Data: TJSONData;
  Formatted: string;
begin
  if Trim(JsonText) = '' then
  begin
    AMemo.Lines.Clear;
    AMemo.Lines.Add('// JSON vazio');
    Exit;
  end;

  Parser := TJSONParser.Create(JsonText, [joUTF8, joStrict]);
  try
    try
      Data := Parser.Parse;
      if Data = nil then
      begin
        AMemo.Lines.Clear;
        AMemo.Lines.Add('// JSON inválido ou vazio');
        Exit;
      end;

      try
        // Formata com indentação (2 espaços)
        Formatted := Data.FormatJSON([foSingleLineArray, foSingleLineObject], 2);

        AMemo.Lines.Clear;
        AMemo.ScrollBars := ssBoth;
        AMemo.WordWrap := False;
        AMemo.Lines.Text := Formatted;
      finally
        Data.Free;
      end;
    except
      on E: EJSON do
      begin
        AMemo.Lines.Clear;
        AMemo.Lines.Add('// Erro ao processar JSON:');
        AMemo.Lines.Add(E.Message);
        AMemo.Lines.Add('');
        AMemo.Lines.Add(JsonText); // Mostra o JSON bruto para debug
      end;
    end;
  finally
    Parser.Free;
  end;
end;

end.



(*-----------------------COMO USAR--------------------------
var
  JsonStr: string;
begin
  JsonStr := Gerar_JSON_Do_Excel('C:\Temp\planilha.xlsx', 1, 2);

  // Mostra no Memo formatado e bonito
  MostrarJsonBonito(JsonStr, Memo1);
end;
*)
