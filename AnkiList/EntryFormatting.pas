unit EntryFormatting;
{
Formats EDICT entries in several common text formats used in these utilities.
}

interface
uses UniStrUtils, Edict;

{
XSLT formatting.
Usage:
  xslt := TEntryFormatter.Create('filename.xslt');
  Result := xslt.Transform(entry);
  Result := xslt.Transform(EdictEntryToXml(entry1) + EdictEntryToXml(entry2));
}

var
  OutputXml: boolean;

procedure SetXsltSchema(const AFilename: string);
procedure FreeXsltSchema;
function FormatEntry(AEntry: PEdictEntry): WideString;
procedure FormatEntryAdd(var AText: string; AEntry: PEdictEntry);
function FormatEntryFinalize(const AText: string): string;


implementation
uses SysUtils, StrUtils, ActiveX, XmlDoc, XmlIntf, EdictReader;

function EdictSensesToText(entry: PEdictEntry): string; forward;
function EdictEntryToXml(entry: PEdictEntry): string; forward;

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
      Result := Result + '<mark>' + HtmlEscape(GetMarkerText(kanji.markers[j]))+'</mark>';
    Result := Result + '</expr>';
  end;

  for i := 0 to Length(entry.kana)-1 do begin
    kana := @entry.kana[i];
    Result := Result+'<read>'+HtmlEscape(kana.kana);
    for j := 1 to Length(kana.markers) do
      Result := Result + '<mark>' + HtmlEscape(GetMarkerText(kana.markers[j]))+'</mark>';
    Result := Result+'</read>';
  end;

  for i := 0 to Length(entry.senses)-1 do begin
    sense := @entry.senses[i];
    Result := Result+'<sense>';
    parts := StrSplit(PChar(sense.text), '/');
    for j := 0 to Length(parts)-1 do
      Result := Result+'<gloss>'+HtmlEscape(Trim(parts[j]))+'</gloss>';
    for j := 1 to Length(sense.pos) do
      Result := Result+'<pos>'+HtmlEscape(GetMarkerText(sense.pos[j]))+'</pos>';
    for j := 1 to Length(sense.markers) do
      Result := Result+'<mark>'+HtmlEscape(GetMarkerText(sense.markers[j]))+'</mark>';
    Result := Result+'</sense>';
  end;

  if entry.pop then
    Result := Result + '<pop />';

  Result := Result + '</entry>';
end;


{ Entry formatting }

var
  iInp, iXsl: IXMLDocument;

procedure SetXsltSchema(const AFilename: string);
begin
  FreeXsltSchema;
  if AFilename<>'' then begin
    CoInitialize(nil);
    iInp := TXMLDocument.Create(nil);
    iXsl := LoadXMLDocument(AFilename);
    OutputXml := true;
  end;
end;

procedure FreeXsltSchema;
begin
  iInp := nil;
 //Release
  if iXsl<>nil then begin
    iXsl := nil;
    CoUninitialize;
  end;
end;

function FormatEntryXml(const entryXml: UnicodeString): WideString;
begin
  if iXsl<>nil then begin
    iInp.LoadFromXML(entryXml);
    iInp.Node.TransformNode(iXsl.Node, Result);
  end else
    Result := entryXml;
end;

//Formats EDICT entry according to current settings
function FormatEntry(AEntry: PEdictEntry): WideString;
begin
  if not OutputXml then
    Result := EdictSensesToText(AEntry)
  else
    Result := FormatEntryXml(EdictEntryToXml(AEntry));
end;

//Add() several entries to format them together as a single operation -
//this is supported by XSLT schema.
procedure FormatEntryAdd(var AText: string; AEntry: PEdictEntry);
var entry_text: string;
begin
  if not OutputXml then begin
    entry_text := EdictSensesToText(AEntry);
    if AText<>'' then
      AText := AText + '; ' + entry_text
    else
      AText := entry_text;
  end else begin
    entry_text := EdictEntryToXml(AEntry);
    AText := AText + entry_text;
   //will call xslt at the end
  end;
end;

//Finalize formatting for entries you have added together with FormatEntryAdd().
function FormatEntryFinalize(const AText: string): string;
begin
  if not OutputXml then
    Result := AText
  else
    Result := FormatEntryXml(AText);
end;

end.
