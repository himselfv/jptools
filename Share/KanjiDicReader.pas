unit KanjiDicReader;
{
Reads KANJIDIC format.
  # comment
  亜 3021 U4e9c [fields] [readings] [T1 readings] [T2 ...] [meanings]
See http://www.csse.monash.edu.au/~jwb/kanjidic_doc.html

This is a parser, not a database. It's optimized for a single pass decoding.
If you need something like:
  KanjiDic.GetKanji('..').Readings
Then parse KANJIDIC with this parser and store the data in easily accessible
format.

Reading entries:
  var entry: TKanjidicEntry;
  while input.ReadLn(s) do begin
    ParseKanjidicLine(s, @entry);
    DoStuffWithEntry(@entry);
  end;
For speed, it's important that you allocate a single TKanjidicEntry and do not
realloc it with every line.

Accessing fields:
  entry.readings[0].JoinKuns(', '); //a list of all common kuns
  entry.readings[2].JoinOns(', '); //a list of rare ons
  entry.JoinMeanings(', ');
  entry.TryGetIntValue('J', x);
  x := entry.GetIntValueDef('G', 5);
}

interface
uses SysUtils, FastArray;

{ Do not pass here lines starting with this character.
 Alternatively, use IsKanjidicComment() }
const
  KANJIDIC_COMMENT: WideChar = '#';

{ Some reasonable values. Increase if it's not enough one day }
const
  MaxReadingClasses = 3;

type
  EKanjidicParsingException = class(Exception);

 { NOTE: This can be made much faster if we assume all field keys are AnsiStrings
    with at most 4 symbols (VERY reasonable assumption).
    We can just declare key as integer, and even have some sort of table to map
    integer(key_string) to one of sequential pre-allocated cells, i.e.
      integer('U') -> 0
      integer('K') -> 1
    This will all work in constant time.
    We can also have some constant time uppercase functions }

  TFieldEntry = record
    key: string; //always lowercase
    values: TArray<string>; //all values so far are ansi
    procedure Reset;
    procedure Copy(const ASource: TFieldEntry);
    procedure AddValue(const value: string); inline;
    function Join(const sep: string): string; inline;
  end;
  PFieldEntry = ^TFieldEntry;

  TReadingClassEntry = record
    key: string;
    ons: TArray<UnicodeString>;
    kuns: TArray<UnicodeString>;
    procedure Reset;
    procedure Copy(const ASource: TReadingClassEntry);
    procedure AddOn(const value: UnicodeString); inline;
    procedure AddKun(const value: UnicodeString); inline;
    function JoinOns(const sep: UnicodeString): UnicodeString; inline;
    function JoinKuns(const sep: UnicodeString): UnicodeString; inline;
  end;
  PReadingClassEntry = ^TReadingClassEntry;

  TKanjidicEntry = record
    kanji: UnicodeString; //it may be wider than a char
    jis: string; //JIS key
    fields: TArray<TFieldEntry>;
   { Kanjidic supports several reading classes: T0 the default, T1 and T2.
    There's no _used because there's a fixed number of them. }
    readings: array[0..MaxReadingClasses-1] of TReadingClassEntry;
    meanings: TArray<UnicodeString>; //we support multilingual kanjidics
    procedure Reset;
    procedure Copy(const ASource: TKanjidicEntry);
    procedure AddField(const key: string; const value: string);
    procedure AddMeaning(const value: UnicodeString); inline;
    function GetField(const key: string): PFieldEntry;
    function TryGetStrValue(const key: string; out value: UnicodeString): boolean;
    function GetStrValueDef(const key: string; const def: string = ''): UnicodeString;
    function TryGetIntValue(const key: string; out value: integer): boolean;
    function GetIntValueDef(const key: string; const def: integer = 0): integer;
    function JoinMeanings(const sep: UnicodeString): UnicodeString; inline;
   //Shortcuts for ease of access
    function GetPinyin: PFieldEntry;
    function GetKoreanReadings: PFieldEntry;
  end;
  PKanjidicEntry = ^TKanjidicEntry;

{ Returns true if the line in question is a Kanjidic comment line.
 You could have done this by yourself, but here goes. }
function IsKanjidicComment(const s: UnicodeString): boolean;

{ Parses a valid non-empty non-comment Kanjidic line and populates the record }
procedure ParseKanjidicLine(const s: UnicodeString; ed: PKanjidicEntry);

{ Composes Kanjidic line from parsed data }
function ComposeKanjidicLine(ed: PKanjidicEntry): UnicodeString;

implementation

resourcestring
  eNoKanjiFieldInRecord = 'No kanji field in record';
  eNoJisFieldInRecord = 'No jis field in record';
  eBrokenMeaningBrakets = 'Broken meaning brakets';
  eUnsupportedReadingClass = 'Unsupported reading class: %s';

{ Entries }

procedure TFieldEntry.Reset;
begin
  key := '';
  values.Clear;
end;

procedure TFieldEntry.Copy(const ASource: TFieldEntry);
var i: integer;
begin
  Self.Reset;
  Self.key := ASource.key;
  Self.values.Reset;
  for i := 0 to ASource.values.Count-1 do
    Self.values.Add(ASource.values[i]);
end;

procedure TFieldEntry.AddValue(const value: string);
begin
  values.Add(value);
end;

function TFieldEntry.Join(const sep: string): string;
begin
  Result := FastArray.Join(values, sep);
end;

procedure TReadingClassEntry.Reset;
begin
  ons.Clear;
  kuns.Clear;
end;

procedure TReadingClassEntry.Copy(const ASource: TReadingClassEntry);
var i: integer;
begin
  Self.Reset;
  Self.key := ASource.key;
  Self.ons.Reset;
  for i := 0 to ASource.ons.Count-1 do
    Self.ons.Add(ASource.ons[i]);
  for i := 0 to ASource.kuns.Count-1 do
    Self.kuns.Add(ASource.kuns[i]);
end;

procedure TReadingClassEntry.AddOn(const value: UnicodeString);
begin
  ons.Add(value);
end;

procedure TReadingClassEntry.AddKun(const value: UnicodeString);
begin
  kuns.Add(value);
end;

function TReadingClassEntry.JoinOns(const sep: UnicodeString): UnicodeString;
begin
  Result := FastArray.Join(ons, sep);
end;

function TReadingClassEntry.JoinKuns(const sep: UnicodeString): UnicodeString;
begin
  Result := FastArray.Join(kuns, sep);
end;

procedure TKanjidicEntry.Reset;
var i: integer;
begin
  kanji := '';
  jis := '';
  fields.Clear;
  meanings.Clear;
  for i := 0 to Length(readings) - 1 do
    readings[i].Reset;
end;

procedure TKanjidicEntry.Copy(const ASource: TKanjidicEntry);
var i: integer;
begin
  Self.Reset;
  Self.kanji := ASource.kanji;
  Self.jis := ASource.jis;
  Self.fields.Reset;
  for i := 0 to ASource.fields.Count-1 do
    Self.fields.AddNew^.Copy(ASource.fields[i]);
  for i := 0 to Length(ASource.readings)-1 do
    Self.readings[i].Copy(ASource.readings[i]);
  for i := 0 to ASource.meanings.Count-1 do
    Self.meanings.Add(ASource.meanings[i]);
end;

procedure TKanjidicEntry.AddField(const key: string; const value: string);
var field: PFieldEntry;
begin
  field := GetField(key);
  if field=nil then begin
    field := PFieldEntry(fields.AddNew());
    field^.Reset;
    field^.key := AnsiLowerCase(key);
  end;
  field^.AddValue(value);
end;

procedure TKanjidicEntry.AddMeaning(const value: UnicodeString);
begin
  meanings.Add(value);
end;

function TKanjidicEntry.GetField(const key: string): PFieldEntry;
var i: integer;
  tmp: string;
begin
  Result := nil;
  tmp := AnsiLowerCase(key);
  for i := 0 to fields.Length - 1 do
    if fields[i].key=tmp then begin
      Result := PFieldEntry(fields.GetPointer(i));
      break;
    end;
end;

function TKanjidicEntry.TryGetStrValue(const key: string; out value: UnicodeString): boolean;
var field: PFieldEntry;
begin
  field := GetField(key);
  if (field=nil) or (field.values.Length<=0) then
    Result := false
  else begin
    Result := true;
    value := field.values[0];
  end;
end;

{ Returns first value for the field, or empty string }
function TKanjidicEntry.GetStrValueDef(const key: string; const def: UnicodeString = ''): UnicodeString;
begin
  if not TryGetStrValue(key, Result) then
    Result := def;
end;

function TKanjidicEntry.TryGetIntValue(const key: string; out value: integer): boolean;
var str: UnicodeString;
begin
  Result := TryGetStrValue(key, str) and TryStrToInt(str, value);
end;

function TKanjidicEntry.GetIntValueDef(const key: string; const def: integer = 0): integer;
begin
  if not TryGetIntValue(key, Result) then
    Result := def;
end;

function TKanjidicEntry.JoinMeanings(const sep: UnicodeString): UnicodeString;
begin
  Result := Join(meanings, sep);
end;

function TKanjidicEntry.GetPinyin: PFieldEntry;
begin
  Result := GetField('Y');
end;

function TKanjidicEntry.GetKoreanReadings: PFieldEntry;
begin
  Result := GetField('W');
end;

function IsKanjidicComment(const s: UnicodeString): boolean;
var pc: PWideChar;
begin
  pc := PWideChar(integer(s)); //do not uniquestr on cast
  if pc=nil then begin
    Result := false;
    exit;
  end;
  while pc^=' ' do Inc(pc);
  Result := pc^=KANJIDIC_COMMENT;
end;

function ReadNextWord(var pc: PWideChar; out word: UnicodeString): boolean;
var stop_char: WideChar;
begin
  while pc^=' ' do Inc(pc);
  if pc^=#00 then begin
   //Nothing to read
    Result := false;
    exit;
  end;
 //Otherwise there is something
  Result := true;

  if pc^='{' then
    stop_char := '}'
  else
    stop_char := ' ';

  while (pc^<>#00) and (pc^<>stop_char) do begin
    word := word + pc^;
    Inc(pc);
  end;
  if pc^<>#00 then begin
   //We eat the stop_char. We must do this with curly bracket, and even if it's space it's okay.
    if stop_char<>' ' then
      word := word + stop_char;
    Inc(pc);
  end;
end;

function IsKatakana(const ch: WideChar): boolean; inline;
begin
  Result := (Word(ch)>=$30A0) and (Word(ch)<=$30FF);
end;

function IsHiragana(const ch: WideChar): boolean; inline;
begin
  Result := (Word(ch)>=$3040) and (Word(ch)<=$309F);
end;

procedure ParseKanjidicLine(const s: UnicodeString; ed: PKanjidicEntry);
var
  rclass: integer;
  pc: PWideChar;
  word: UnicodeString;
  pref: string;

begin
  ed.Reset;
  rclass := 0;
  pc := PWideChar(integer(s)); //do not uniquestr on cast
  if pc=nil then exit;

  if not ReadNextWord(pc, ed.kanji) then
    raise EKanjidicParsingException.Create(eNoKanjiFieldInRecord);
  if not ReadNextWord(pc, word) then
    raise EKanjidicParsingException.Create(eNoJisFieldInRecord);
  ed.jis := string(word);

 //Rest is dynamic
  while ReadNextWord(pc, word) do begin
    if Length(word)<=0 then continue;
    pref := '';

   //Meaning
    if word[1]='{' then begin
      if word[Length(word)]<>'}' then
        raise EKanjidicParsingException.Create(eBrokenMeaningBrakets);
      ed.AddMeaning(copy(word, 2, Length(word)-2));
    end else
   //Readings
    if IsKatakana(word[1]) then
      ed.readings[rclass].AddOn(word)
    else
    if IsHiragana(word[1]) then
      ed.readings[rclass].AddKun(word)
    else
   //Reading class, e.g. T0, T1
    if (Length(word)=2) and (word[1]='T') and (word[2]>='0') and (word[2]<='9') then begin
      rclass := Ord(word[2])-Ord('0');
      if rclass>Length(ed.readings) then
        raise EKanjidicParsingException.CreateFmt(eUnsupportedReadingClass, [word]);
    end else
   //Normal field
    begin
      if word[1]='X' then begin
       { Cross-references are normal fields with added X at the start:
           N123 -> XN123
         We remove X to parse keys uniformly, and then append it back.
         Note that some keys (i.e. J) have different meanings with crossrefs }
        delete(word,1,1);
        pref := 'X';
      end;

     //Most field keys are 1-char, but some arent:
     //D* keys are two chars (there's a lot of second char versions and more plannet)
      if (word[1]='D') and (Length(word)>=2) then
        ed.AddField(pref+word[1]+word[2], copy(word, 3, Length(word)-2))
      else
     //In addition to I keys there are IN
      if (word[1]='I') and (Length(word)>=2) and (word[2]='N') then
        ed.AddField(pref+word[1]+word[2], copy(word, 3, Length(word)-2))
      else
     //MN and MP
      if (word[1]='M') and (Length(word)>=2) and ((word[2]='N') or (word[2]='P')) then
        ed.AddField(pref+word[1]+word[2], copy(word, 3, Length(word)-2))
      else
     //ZPP, ZSP, ZBP, ZRP
      if (word[1]='Z') and (Length(word)>=3) and (word[3]='P')
      and ((word[2]='P') or (word[2]='S') or (word[2]='B') or (word[2]='R')) then
        ed.AddField(pref+word[1]+word[2]+word[3], copy(word, 4, Length(word)-3))
      else
        ed.AddField(pref+word[1], copy(word, 2, Length(word)-1));
    end;
  end;
end;

function ComposeKanjidicLine(ed: PKanjidicEntry): UnicodeString;
var i, j: integer;
begin
  Result := ed.kanji + ' ' + ed.jis;

 //Fields
  for i := 0 to ed.fields.Count-1 do
    for j := 0 to ed.fields[i].values.Count-1 do
      Result := Result + ' ' + ed.fields[i].key+ed.fields[i].values[j];

 //Readings
  for i := 0 to Length(ed.readings)-1 do begin
    if (ed.readings[i].ons.Count<=0) and (ed.readings[i].kuns.Count<=0) then
      continue;
    if (i>0) then
      Result := Result + ' T'+IntToStr(i);
    for j := 0 to ed.readings[i].ons.Count-1 do
      Result := Result + ' ' + ed.readings[i].ons[j];
    for j := 0 to ed.readings[i].kuns.Count-1 do
      Result := Result + ' ' + ed.readings[i].kuns[j];
  end;

 //Meanings
  for i := 0 to ed.meanings.Count-1 do
    Result := Result + ' {' + ed.meanings[i] + '}';
end;

end.
