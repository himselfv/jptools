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

type
  TReading = record
    read: string;
    expressions: array of integer; //expression indexes
  end;
  PReading = ^TReading;
  TReadings = array of TReading;

//Converts "kanji with attached kana" to "kana with attached kanji"
function ExpressionsToReadings(const expr: TExpressions): TReadings;


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

function FindReading(const AReadings: TStringArray; const read: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(AReadings)-1 do
    if AReadings[i]=read then begin
      Result := i;
      break;
    end;
end;

//Merges all readings used in expressions to a single list
function MergeReadings(const expr: TExpressions): TStringArray;
var i, j: integer;
begin
  SetLength(Result, 0);
  for i := 0 to Length(expr)-1 do
    for j := 0 to Length(expr[i].readings)-1 do
      if FindReading(Result, expr[i].readings[j])<0 then begin
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := expr[i].readings[j];
      end;
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
      Result[i].readings := SplitReadings(read)
    else
      SetLength(Result[i].readings, 0);
  end;
end;

function ParseExpressions(const expr, read: string): TExpressions;
var readings, r2: TStringArray;
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

 //Check that all the external readings are used or it's a warning
  r2 := MergeReadings(Result);
  for i := 0 to Length(r2)-1 do
    if FindReading(readings, r2[i])<0 then
      writeln(ErrOutput, 'Warning: reading '+r2[i]+' not assigned to any kanji in "'+expr+' : '+read+'"');
end;

function ExpressionsToReadings(const expr: TExpressions): TReadings;
var readings: TStringArray;
  i, j: integer;
begin
  readings := MergeReadings(expr);
  SetLength(Result, Length(readings));

  for i := 0 to Length(readings)-1 do begin
    Result[i].read := readings[i];
    SetLength(Result[i].expressions, 0);
    for j := 0 to Length(expr)-1 do
      if FindReading(expr[j].readings, readings[i])>=0 then begin
        SetLength(Result[i].expressions, Length(Result[i].expressions)+1);
        Result[i].expressions[Length(Result[i].expressions)-1] := j;
      end;
    if Length(Result[i].expressions)=Length(expr) then //all expressions matched
      SetLength(Result[i].expressions, 0); //informal "all matched"
  end;
end;

end.
