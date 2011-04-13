unit CodepageUtils;
{$WEAKPACKAGEUNIT ON}
(*
  Утилиты для работы с кодировками и преобразованием между ними строк.
*)

interface
uses SysUtils, StrUtils, Windows;

//Превращает один символ в Wide/Ansi
function ToWideChar(c: AnsiChar; cp: cardinal): WideChar;
function ToChar(c: WideChar; cp: cardinal): AnsiChar;

//Превращает строку в Wide/Ansi
function ToWideString(s: AnsiString; cp: cardinal): WideString;
function ToString(s: WideString; cp: cardinal): AnsiString;

//Превращает буфер заданной длины в Wide/Ansi
function BufToWideString(s: PAnsiChar; len: integer; cp: cardinal): WideString;
function BufToString(s: PWideChar; len: integer; cp: cardinal): AnsiString;

//Меняет кодировку Ansi-строки
function Convert(s: AnsiString; cpIn, cpOut: cardinal): AnsiString;

//Меняет кодировку Ansi-строки с системной на консольную и наоборот
function WinToOEM(s: AnsiString): AnsiString; inline;
function OEMToWin(s: AnsiString): AnsiString; inline;

implementation

function ToWideChar(c: AnsiChar; cp: cardinal): WideChar;
begin
  if MultiByteToWideChar(cp, 0, @c, 1, @Result, 2) = 0 then
    RaiseLastOsError;
end;

function ToChar(c: WideChar; cp: cardinal): AnsiChar;
begin
  if WideCharToMultiByte(cp, 0, @c, 2, @Result, 1, nil, nil) = 0 then
    RaiseLastOsError;
end;

function ToWideString(s: AnsiString; cp: cardinal): WideString;
begin
  Result := BufToWideString(PAnsiChar(s), Length(s), cp);
end;

function ToString(s: WideString; cp: cardinal): AnsiString;
begin
  Result := BufToString(PWideChar(s), Length(s), cp);
end;

function BufToWideString(s: PAnsiChar; len: integer; cp: cardinal): WideString;
var size: integer;
begin
  if s^=#00 then begin
    Result := '';
    exit;
  end;

  size := MultiByteToWideChar(cp, 0, s, len, nil, 0);
  if size=0 then
    RaiseLastOsError;
  SetLength(Result, size);
  if MultiByteToWideChar(cp, 0, s, len, pwidechar(Result), size) = 0 then
    RaiseLastOsError;
end;

function BufToString(s: PWideChar; len: integer; cp: cardinal): AnsiString;
var size: integer;
begin
  if s^=#00 then begin
    Result := '';
    exit;
  end;

  size := WideCharToMultiByte(cp, 0, s, len, nil, 0, nil, nil);
  if size=0 then
    RaiseLastOsError;
  SetLength(Result, size);
  if WideCharToMultiByte(cp, 0, s, len, PAnsiChar(Result), size, nil, nil) = 0 then
    RaiseLastOsError;
end;

function Convert(s: AnsiString; cpIn, cpOut: cardinal): AnsiString;
begin
  Result := ToString(ToWideString(s, cpIn), cpOut);
end;

//Переводит строку из текущей кодировки системы в текущую кодировку консоли.
function WinToOEM(s: AnsiString): AnsiString;
begin
  Result := Convert(s, CP_ACP, CP_OEMCP);
end;

//Переводит строку из текущей кодировки консоли в текущую кодировку системы.
function OEMToWin(s: AnsiString): AnsiString;
begin
  Result := Convert(s, CP_OEMCP, CP_ACP);
end;

end.
