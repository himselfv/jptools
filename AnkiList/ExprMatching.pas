unit ExprMatching;
{
Parses expression and reading records.
Sometimes in places where we expect expression, a clarification is needed
and reading is added in one of various ways:
  <ruby>側<rt>そば</rt></ruby>
  側 [そば]
  側（そば）
  側・そば
Sometimes it's excessive (e.g. it was only a visual clue and we have this data
from another column), at other times important.

We also support multi-value splitting here, e.g.
  側 [がわ、そば]
In case someone thinks several readings/writings are interchangeable, but only
to a limited extent. We do not support full-blown EDICT style kana-to-kanji
matching:
  側、侧 [がわ(侧、側)、そば(側)] --- not supported

Usage:
  SetInlineReadingPattern('your inline-reading regex'); //optionally
  read := StripInlineReading(expr);
  read := StripInlineReading(expr);
}

interface
uses UniStrUtils;

//Overrides default expression/reading separtor
procedure SetExpressionSeparator(const ASep: char);
procedure SetReadingSeparator(const ASep: char);

//Overrides default matching patterns with this regex. It must match all patterns
//and put expression into \k'expr' and reading into \k'read'
procedure SetExpressionPattern(const pattern: string);
procedure SetReadingPattern(const pattern: string);

//Matches ONE expression inplace, removing everything not matched. If reading
//was provided, it's returned in read.
procedure MatchExpression(var expr: string; out read: string);
procedure MatchReading(var expr: string);

type
  TExpression = record
    expr: string;
    readings: TStringArray;
  end;
  PExpression = ^TExpressions;
  TExpressions = array of TExpression;

//Splits the string with the appropriate separator and matches all items
//with MatchExpression/Reading.
function ParseExpressions(const expr: string): TExpressions; overload;
function ParseExpressions(const expr, read: string): TExpressions; overload;


implementation
uses SysUtils, RegularExpressionsCore, RegexUtils;

var
  ExpressionSeparator: char = '、';
  ReadingSeparator: char = #00; //same as expression

 //Default expression pattern
  ptExpression: string = '(?J)(?|'
    +'<ruby>(?''expr''[^<>]*)<rt>(?''read''[^<>]*)</rt></ruby>'+'|'
    +'(?''expr''[^\[]*)\s*\[(?''read''[^\]]*)\]'+'|'
    +'(?''expr''[^（]*)\s*（(?''read''[^）]*)）'+'|'
    +'(?''expr''[^・]*)\s*・s*(?''read''.*)'
    +'?)';
  ptReading: string = '';

  preExpression: TPerlRegEx = nil;
  preReading: TPerlRegEx = nil;

procedure SetExpressionSeparator(const ASep: char);
begin
  ExpressionSeparator := ASep;
end;

procedure SetReadingSeparator(const ASep: char);
begin
  ReadingSeparator := ASep;
end;

procedure SetExpressionPattern(const pattern: string);
begin
  ptExpression := pattern;
  FreeAndNil(preExpression);
end;

procedure SetReadingPattern(const pattern: string);
begin
  ptExpression := pattern;
  FreeAndNil(preReading);
end;

procedure MatchExpression(var expr: string; out read: string);
var id: integer;
begin
  read := '';
  if ptExpression='' then exit;

  if preExpression=nil then
    preExpression := Regex(ptExpression);
  preExpression.Subject := UTF8String(expr);
  if not preExpression.Match then exit;

  id := preExpression.NamedGroup('expr');
  if id<0 then exit; //no expr group
  expr := string(preExpression.Groups[id]);

  id := preExpression.NamedGroup('read');
  if id>=0 then
    read := string(preExpression.Groups[id]);
end;

procedure MatchReading(var expr: string);
var id: integer;
begin
  if ptReading='' then exit;

  if preReading=nil then
    preReading := Regex(ptReading);
  preReading.Subject := UTF8String(expr);
  if not preReading.Match then exit;

  id := preReading.NamedGroup('expr');
  if id<0 then exit;//no expr group
  expr := string(preExpression.Groups[id]);
end;

function SplitReadings(const read: string): TStringArray;
begin
  if ReadingSeparator=#00 then
    Result := StrSplit(PChar(read), ExpressionSeparator)
  else
    Result := StrSplit(PChar(read), ReadingSeparator);
end;

function ParseExpressions(const expr: string): TExpressions;
var parts: TStringArray;
  i: integer;
  read: string;
begin
  parts := StrSplit(PChar(expr), ExpressionSeparator);
  SetLength(Result, Length(parts));
  for i := 0 to Length(parts)-1 do begin
    Result[i].expr := parts[i];
    MatchExpression(Result[i].expr, read);
    if read<>'' then
      Result[i].readings := SplitReadings(read);
  end;
end;

function ParseExpressions(const expr, read: string): TExpressions;
var readings: TStringArray;
  i: integer;
begin
  Result := ParseExpressions(expr);

  readings := SplitReadings(read);
  for i := 0 to Length(readings)-1 do
    MatchReading(readings[i]);

 //Copy common readings to expressions where no exact readings are given
  for i := 0 to Length(Result)-1 do
    if Result[i].readings=nil then
      Result[i].readings := readings;
end;

end.
