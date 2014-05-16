unit ExprMatching;
{
Sometimes in places where we expect expression, a clarification is needed
and reading is added in one of various ways:
  <ruby>側<rt>そば</rt></ruby>
  側 [そば]
  側（そば）
  側・そば
Sometimes it's excessive (e.g. it was only a visual clue and we have this data
from another column) but at other times important.

Usage:
  SetInlineReadingPattern('your inline-reading regex'); //optionally
  read := StripInlineReading(expr);
  read := StripInlineReading(expr);

TODO: We may later support "multiple matching" here too
}

interface

//Overrides default matching patterns with this regex. It must match all patterns
//and put expression into \k'expr' and reading into \k'read'
procedure SetExpressionPattern(const pattern: string);
procedure SetReadingPattern(const pattern: string);

//Matches the expression inplace, removing everything not matched. If reading
//was provided, it's returned in read.
procedure MatchExpression(var expr: string; out read: string);
procedure MatchReading(var expr: string);

implementation
uses SysUtils, RegularExpressionsCore, RegexUtils;

var
 //Default expression pattern
  pExpression: string = '(?J)(?|'
    +'<ruby>(?''expr''[^<>]*)<rt>(?''read''[^<>]*)</rt></ruby>'+'|'
    +'(?''expr''[^\[]*)\s*\[(?''read''[^\]]*)\]'+'|'
    +'(?''expr''[^（]*)\s*（(?''read''[^）]*)）'+'|'
    +'(?''expr''[^・]*)\s*・s*(?''read''.*)'
    +'?)';
  pReading: string = '';

  preExpression: TPerlRegEx = nil;
  preReading: TPerlRegEx = nil;

procedure SetExpressionPattern(const pattern: string);
begin
  pExpression := pattern;
  FreeAndNil(preExpression);
end;

procedure SetReadingPattern(const pattern: string);
begin
  pExpression := pattern;
  FreeAndNil(preReading);
end;

procedure MatchExpression(var expr: string; out read: string);
var id: integer;
begin
  read := '';
  if pExpression='' then exit;

  if preExpression=nil then
    preExpression := Regex(pExpression);
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
  if pReading='' then exit;

  if preReading=nil then
    preReading := Regex(pReading);
  preReading.Subject := UTF8String(expr);
  if not preReading.Match then exit;

  id := preReading.NamedGroup('expr');
  if id<0 then exit;//no expr group
  expr := string(preExpression.Groups[id]);
end;

end.
