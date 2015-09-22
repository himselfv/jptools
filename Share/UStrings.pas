unit UStrings;
{ Defines common string types and routines on all supported compilers.

The code everywhere mostly expects us running on Unicode Delphi, with String type being in UTF16.
However, on some compilers we may need to use custom Unicode types, so for maximal compability
use UnicodeString and UnicodeChar explicitly. }

interface

type
{$IFNDEF UNICODE}
  UnicodeString = WideString;
{$ENDIF}
  UnicodeChar = WideChar;
  PUnicodeChar = PWideChar;
  TStringArray = array of string;



function SplitStr(s: string; cnt: integer; ch: char=','): TStringArray; overload;
function SplitStr(s: string; ch: char=','): TStringArray; overload;

function UTrim(const S: UnicodeString; const Chars: UnicodeString): UnicodeString; overload;

function StartsStr(const substr, str: string): boolean; overload; inline;
function StartsStr(substr, str: PChar): boolean; overload;

function repl(const s: string; const sub, repl: string): string; overload;


{ Character processing }

type //Character types for EvalChar
  TEvalCharType = (
    EC_UNKNOWN          = 0, // unrecognized
    EC_IDG_CHAR         = 1, // ideographic char
    EC_HIRAGANA         = 2, // hiragana
    EC_KATAKANA         = 3, // katakana
    EC_IDG_PUNCTUATION  = 4, // ideographic punctuation
    EC_IDG_OTHER        = 5, // ideographic other
    EC_LATIN_FW         = 6, // full-width latin
    EC_LATIN_HW         = 7, // half-width latin
    EC_KATAKANA_HW      = 8, // half-width katakana
    EC_BOPOMOFO         = 9  // bopomofo
  );
  TEvalCharTypes = set of TEvalCharType;

function EvalChar(char: WideChar): TEvalCharType; overload; inline;
function EvalChar(const char: UnicodeString): TEvalCharType; overload; inline;

//Returns a set of (1 shl EC_*) flags indicating some chars are present in string
function EvalChars(const chars: UnicodeString): TEvalCharTypes;

implementation
uses SysUtils;

//Same but doesn't add it anywhere
function SplitStr(s: string; cnt: integer; ch:char): TStringArray;
var i:integer;
begin
  SetLength(Result, cnt);
  i:=0;
  while i<cnt do
  begin
    if pos(ch,s)>0 then
    begin
      Result[i] := copy(s,1,pos(ch,s)-1);
      delete(s,1,pos(ch,s));
    end else
    begin
      Result[i] := s;
      s:='';
    end;
    inc(i);
  end;
end;

//Same but does not use fixed number of parts
function SplitStr(s: string; ch: char=','): TStringArray;
var i: integer;
begin
  SetLength(Result, 0);
  i := pos(ch,s);
  while i>0 do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := copy(s, 1, i-1);
    delete(s, 1, i);
    i := pos(ch,s);
  end;
  if s<>'' then begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := s;
  end;
end;


//<= 0 if not found
function UCharPos(const Ch: WideChar; const Str: UnicodeString): integer;
begin
  Result := 1;
  while Result<=Length(Str) do begin
    if Ch=Str[Result] then
      exit;
    Inc(Result);
  end;
  Result := -1;
end;

function UTrim(const S: UnicodeString; const Chars: UnicodeString): UnicodeString;
var i_l, i_r: integer;
begin
 //Left side
  i_l:=1;
  while i_l<=Length(s) do begin
    if UCharPos(s[i_l], Chars)<=0 then
      break;
    Inc(i_l);
  end;
 //Right side
  i_r:=Length(s);
  while i_r>=1 do begin
    if UCharPos(s[i_r], Chars)<=0 then
      break;
    Dec(i_r);
  end;

  if (i_l=1) and (i_r=Length(s)) then
    Result := S
  else
  if i_r<i_l then
    Result := ''
  else
    Result := Copy(S,i_l,i_r-i_l+1);
end;


//True if str starts with substr. Delphi version is slow.
function StartsStr(const substr, str: string): boolean;
begin
  Result := StartsStr(PChar(substr), PChar(str));
end;

function StartsStr(substr, str: PChar): boolean;
begin
  if substr=nil then
    Result := true
  else
  if str=nil then
    Result := false
  else begin
    while (substr^<>#00) and (substr^=str^) do begin
      Inc(substr);
      Inc(str);
    end;
    Result := (substr^=#00);
  end;
end;


function repl(const s:string;const sub,repl:string):string;
var i_pos: integer;
begin
  Result := s;
  i_pos := pos(sub, Result);
  while i_pos>0 do begin
    Result:=copy(Result,1,i_pos-1) + repl
      +copy(Result,i_pos+length(sub),length(Result)-i_pos+1-length(sub));
    i_pos := pos(sub,Result);
  end;
end;


{ Character processing }

function EvalChar(char: WideChar): TEvalCharType;
var ch: Word absolute char;
begin
  if ch=$3005 then result:=EC_IDG_CHAR else // kurikaeshi
  if (ch>=$3000) and (ch<=$303F) then result:=EC_IDG_PUNCTUATION else
  if (ch>=$3040) and (ch<=$309F) then result:=EC_HIRAGANA else
  if (ch>=$30A0) and (ch<=$30FF) then result:=EC_KATAKANA else
  if (ch>=$3100) and (ch<=$312F) then result:=EC_BOPOMOFO else
  if (ch>=$3200) and (ch<=$33FF) then result:=EC_IDG_OTHER else
  if (ch>=$3400) and (ch<=$9FFF) then result:=EC_IDG_CHAR else
  if (ch>=$F900) and (ch<=$FAFF) then result:=EC_IDG_CHAR else
  if (ch>=$FE30) and (ch<=$FE4F) then result:=EC_IDG_PUNCTUATION else
  if (ch>=$FF00) and (ch<=$FF5F) then result:=EC_LATIN_FW else
  if (ch>=$FF60) and (ch<=$FF9F) then result:=EC_KATAKANA_HW else
  if {(ch>=$0000) and} (ch<=$007F) then result:=EC_LATIN_HW else //first part always true
  result:=EC_UNKNOWN;
end;

function EvalChar(const char: UnicodeString): TEvalCharType;
begin
  Result := EvalChar(WideChar(PWideChar(char)^));
end;

function EvalChars(const chars: UnicodeString): TEvalCharTypes;
var i: integer;
begin
  Result := [];
  for i := 1 to Length(chars) do
    Result := Result + [EvalChar(chars[i])];
end;


end.