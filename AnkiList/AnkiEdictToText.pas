unit AnkiEdictToText;
{
Prints EDICT entry in several common text formats used in these utilities.
}

interface
uses UniStrUtils, Edict;

function EdictSensesToText(entry: PEdictEntry): string;
function EdictEntryToXml(entry: PEdictEntry): string;

implementation
uses SysUtils, StrUtils, JWBEdictMarkers;

function EdictSensesToText(entry: PEdictEntry): string;
begin
  Result := UniReplaceStr(entry.AllSenses, '/', ', ');
end;

function EdictEntryToXml(entry: PEdictEntry): string;
var i, j: integer;
  parts: UniStrUtils.TStringArray;
  kanji: PKanjiEntry;
  kana: PKanaEntry;
  sense: PSenseEntry;
begin
 //Note: Everything must be escaped. "&" often appears in fields.

  Result := '<entry>'
    +'<ref>'+HtmlEscape(entry.ref)+'</ref>';
  for i := 0 to Length(entry.kanji)-1 do begin
    kanji := @entry.kanji[i];
    Result := Result+'<expr>'+HtmlEscape(kanji.kanji);
    for j := 1 to Length(kanji.markers) do
      Result := Result + '<mark>' + HtmlEscape(GetMarkEdict(kanji.markers[j]))+'</mark>';
    Result := Result + '</expr>';
  end;

  for i := 0 to Length(entry.kana)-1 do begin
    kana := @entry.kana[i];
    Result := Result+'<read>'+HtmlEscape(kana.kana);
    for j := 1 to Length(kana.markers) do
      Result := Result + '<mark>' + HtmlEscape(GetMarkEdict(kana.markers[j]))+'</mark>';
    Result := Result+'</read>';
  end;

  for i := 0 to Length(entry.senses)-1 do begin
    sense := @entry.senses[i];
    Result := Result+'<sense>';
    parts := StrSplit(PChar(sense.text), '/');
    for j := 0 to Length(parts)-1 do
      Result := Result+'<gloss>'+HtmlEscape(Trim(parts[j]))+'</gloss>';
    for j := 1 to Length(sense.pos) do
      Result := Result+'<pos>'+HtmlEscape(GetMarkEdict(sense.pos[j]))+'</pos>';
    for j := 1 to Length(sense.markers) do
      Result := Result+'<mark>'+HtmlEscape(GetMarkEdict(sense.markers[j]))+'</mark>';
    Result := Result+'</sense>';
  end;

  if entry.pop then
    Result := Result + '<pop />';

  Result := Result + '</entry>';
end;

end.
