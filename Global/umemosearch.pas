unit uMemoSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls; // StdCtrls da LCL (não Vcl)

type
  TMemoSearcher = class
  private
    FMemo: TMemo;
    FText: string;
    FCaseSensitive: Boolean;
    FWholeWord: Boolean;
    FWrap: Boolean;
    FPos: Integer;
    function GetMemoText: string;
    function IsWordChar(C: Char): Boolean;
    function MatchIsWholeWord(const Haystack: string; Index: Integer; MatchLen: Integer): Boolean;
    function FindFrom(StartAt: Integer): Integer;
    procedure SelectAt(Index: Integer);
  public
    constructor Create(AMemo: TMemo);
    function Start(const SearchText: string; CaseSensitive, WholeWord, Wrap: Boolean): Boolean;
    function FindNext: Boolean;
  end;

implementation

uses
  StrUtils; // PosEx está aqui no Lazarus

constructor TMemoSearcher.Create(AMemo: TMemo);
begin
  inherited Create;
  FMemo := AMemo;
  FPos := 1;
end;

function TMemoSearcher.GetMemoText: string;
begin
  Result := FMemo.Lines.Text;
end;

function TMemoSearcher.IsWordChar(C: Char): Boolean;
begin
  Result := C in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;

function TMemoSearcher.MatchIsWholeWord(const Haystack: string; Index: Integer; MatchLen: Integer): Boolean;
var
  PrevIdx, NextIdx: Integer;
  PrevChar, NextChar: Char;
begin
  PrevIdx := Index - 1;
  NextIdx := Index + MatchLen;
  Result := True;

  if PrevIdx >= 1 then
  begin
    PrevChar := Haystack[PrevIdx];
    if IsWordChar(PrevChar) then
      Exit(False);
  end;

  if NextIdx <= Length(Haystack) then
  begin
    NextChar := Haystack[NextIdx];
    if IsWordChar(NextChar) then
      Exit(False);
  end;
end;

function TMemoSearcher.FindFrom(StartAt: Integer): Integer;
var
  Hay, Needle: string;
begin
  Hay := GetMemoText;
  Needle := FText;

  if not FCaseSensitive then
  begin
    Hay := AnsiLowerCase(Hay);
    Needle := AnsiLowerCase(Needle);
  end;

  Result := PosEx(Needle, Hay, StartAt);
  if (Result > 0) and FWholeWord then
  begin
    while (Result > 0) and not MatchIsWholeWord(Hay, Result, Length(Needle)) do
      Result := PosEx(Needle, Hay, Result + 1);
  end;
end;

procedure TMemoSearcher.SelectAt(Index: Integer);
begin
  FMemo.SelStart := Index - 1;
  FMemo.SelLength := Length(FText);
  FMemo.SetFocus;
end;

function TMemoSearcher.Start(const SearchText: string; CaseSensitive, WholeWord, Wrap: Boolean): Boolean;
var
  StartAt, Found: Integer;
begin
  FText := SearchText;
  FCaseSensitive := CaseSensitive;
  FWholeWord := WholeWord;
  FWrap := Wrap;

  if FText = '' then
    Exit(False);

  StartAt := FMemo.SelStart + FMemo.SelLength + 1;
  if StartAt < 1 then StartAt := 1;

  Found := FindFrom(StartAt);
  if (Found = 0) and FWrap then
    Found := FindFrom(1);

  Result := Found > 0;
  if Result then
  begin
    FPos := Found + Length(FText);
    SelectAt(Found);
  end;
end;

function TMemoSearcher.FindNext: Boolean;
var
  Found: Integer;
begin
  if FText = '' then
    Exit(False);

  Found := FindFrom(FPos);
  if (Found = 0) and FWrap then
    Found := FindFrom(1);

  Result := Found > 0;
  if Result then
  begin
    FPos := Found + Length(FText);
    SelectAt(Found);
  end;
end;

end.



(*
---------------------------Como Usar------------------------------
var
  Searcher: TMemoSearcher;
begin
  Searcher := TMemoSearcher.Create(Memo1);
  try
    if Searcher.Start('palavra', False, True, True) then
      ShowMessage('Encontrado!')
    else
      ShowMessage('Não encontrado.');

    // Procurar próximo
    if Searcher.FindNext then
      ShowMessage('Próxima ocorrência encontrada!');
  finally
    Searcher.Free;
  end;
end;
*)


