unit EdictReader;
{
Reads EDICT2 and CC-EDICT format.
Both are multi-kanji, multi-kana, multi-sense articles in a general form:
  kanji1 kanji2 [reading1 reading2] /sense1/sense2/

See precise format description in comments below.
}

//NOTE: Both EDICT2 and CC-EDICT parsers misreplace / with ; in cases like AC/DC
//  Nothing I can do about it.

{$DEFINE CASE_SENSITIVE_MARKERS}
{ Markers inside one dic are insensitive, but EDICT's (P) is not ENAMDICT's (p).
 If this breaks anything or when there's case-euqivalent dupes, we'll implement better way }

interface
uses SysUtils;

{ EDICT and ENAMDIC support markers such as (P) or (adj-na). They have different
 sets of markers but we support a union for simplicity.
 If other EDICT-style dictionaries have other markers we'll see what we can do. }

type
 //We support up to 255 markers for now.
  TMarker = AnsiChar;
  TMarkers = AnsiString;

  TMarkerFlag = (
    mfPos,   //it's important to distinguish POS markers from non-POS markers:
      //they are handled differently (see JWBEdictReader.pas)
      //other marker types are not so important, but they are kept separately by JMDic
    mfField, //field of application
    mfDial,  //dialect
    mfPop    //"popular"
  );
  TMarkerFlags = set of TMarkerFlag;

  TMarkerDef = record
    m: string;             //marker text without brackets
    f: TMarkerFlags;
  end;
  PMarkerDef = ^TMarkerDef;

  TMarkerList = array of TMarkerDef;

const
 //Keep deprecated markers if we want to support older EDICTs.
  Markers: array[1..126] of TMarkerDef = (
   //Part of Speech Marking
    (m: 'adj-i'; f: [mfPos]),
    (m: 'adj-na'; f: [mfPos]),
    (m: 'adj-no'; f: [mfPos]),
    (m: 'adj-pn'; f: [mfPos]),
    (m: 'adj-s'; f: [mfPos]), //deprecated
    (m: 'adj-t'; f: [mfPos]),
    (m: 'adj-f'; f: [mfPos]),
    (m: 'adj'; f: [mfPos]),
    (m: 'adv'; f: [mfPos]),
    (m: 'adv-n'; f: [mfPos]),
    (m: 'adv-to'; f: [mfPos]),
    (m: 'aux'; f: [mfPos]),
    (m: 'aux-v'; f: [mfPos]),
    (m: 'aux-adj'; f: [mfPos]),
    (m: 'conj'; f: [mfPos]),
    (m: 'ctr'; f: [mfPos]),
    (m: 'exp'; f: [mfPos]),
    (m: 'int'; f: [mfPos]),
    (m: 'iv'; f: [mfPos]),
    (m: 'n'; f: [mfPos]),
    (m: 'n-adv'; f: [mfPos]),
    (m: 'n-pref'; f: [mfPos]),
    (m: 'n-suf'; f: [mfPos]),
    (m: 'n-t'; f: [mfPos]),
    (m: 'neg'; f: [mfPos]), //deprecated
    (m: 'neg-v'; f: [mfPos]), //deprecated
    (m: 'num'; f: [mfPos]),
    (m: 'pn'; f: [mfPos]),
    (m: 'pref'; f: [mfPos]),
    (m: 'prt'; f: [mfPos]),
    (m: 'suf'; f: [mfPos]),
    (m: 'v1'; f: [mfPos]),
    (m: 'v2a-s'; f: [mfPos]),
    (m: 'v4h'; f: [mfPos]),
    (m: 'v4r'; f: [mfPos]),
    (m: 'v5'; f: [mfPos]),
    (m: 'v5aru'; f: [mfPos]),
    (m: 'v5b'; f: [mfPos]),
    (m: 'v5g'; f: [mfPos]),
    (m: 'v5k'; f: [mfPos]),
    (m: 'v5k-s'; f: [mfPos]),
    (m: 'v5m'; f: [mfPos]),
    (m: 'v5n'; f: [mfPos]),
    (m: 'v5r'; f: [mfPos]),
    (m: 'v5r-i'; f: [mfPos]),
    (m: 'v5s'; f: [mfPos]),
    (m: 'v5t'; f: [mfPos]),
    (m: 'v5u'; f: [mfPos]),
    (m: 'v5u-s'; f: [mfPos]),
    (m: 'v5uru'; f: [mfPos]),
    (m: 'v5z'; f: [mfPos]),
    (m: 'vz'; f: [mfPos]),
    (m: 'vi'; f: [mfPos]),
    (m: 'vk'; f: [mfPos]),
    (m: 'vn'; f: [mfPos]),
    (m: 'vs'; f: [mfPos]),
    (m: 'vs-c'; f: [mfPos]),
    (m: 'vs-i'; f: [mfPos]),
    (m: 'vs-s'; f: [mfPos]),
    (m: 'vt'; f: [mfPos]),

   //Field of Application
    (m: 'Buddh'; f: [mfField]),
    (m: 'MA'; f: [mfField]),
    (m: 'comp'; f: [mfField]),
    (m: 'food'; f: [mfField]),
    (m: 'geom'; f: [mfField]),
    (m: 'gram'; f: [mfField]),
    (m: 'ling'; f: [mfField]),
    (m: 'math'; f: [mfField]),
    (m: 'mil'; f: [mfField]),
    (m: 'physics'; f: [mfField]),

   //Miscellaneous Markings
    (m: 'X'),
    (m: 'abbr'),
    (m: 'arch'),
    (m: 'ateji'),
    (m: 'chn'),
    (m: 'col'),
    (m: 'derog'),
    (m: 'eK'),
    (m: 'ek'),
    (m: 'fam'),
    (m: 'fem'),
    (m: 'gikun'),
    (m: 'hon'),
    (m: 'hum'),
    (m: 'ik'),
    (m: 'iK'),
    (m: 'id'),
    (m: 'io'),
    (m: 'm-sl'),
    (m: 'male'),
    (m: 'male-sl'),
    (m: 'oK'),
    (m: 'obs'),
    (m: 'obsc'),
    (m: 'ok'),
    (m: 'on-mim'),
    (m: 'poet'),
    (m: 'pol'),
    (m: 'rare'),
    (m: 'sens'),
    (m: 'sl'),
    (m: 'uK'),
    (m: 'uk'),
    (m: 'vulg'),

   //Word Priority Marking
    (m: 'P'; f: [mfPop]),

   //Regional Words
    (m: 'kyb'; f: [mfDial]),
    (m: 'osb'; f: [mfDial]),
    (m: 'ksb'; f: [mfDial]),
    (m: 'ktb'; f: [mfDial]),
    (m: 'tsb'; f: [mfDial]),
    (m: 'thb'; f: [mfDial]),
    (m: 'tsug'; f: [mfDial]),
    (m: 'kyu'; f: [mfDial]),
    (m: 'rkb'; f: [mfDial]),

   //Enamdict
    (m: 's'; f: []), //surname
    (m: 'p'; f: []), //place-name
    (m: 'u'; f: []), //person name, either given or surname, as-yet unclassified
    (m: 'g'; f: []), //given name, as-yet not classified by sex
    (m: 'f'; f: []), //female given name
    (m: 'm'; f: []), //male given name
    (m: 'h'; f: []), //full (usually family plus given) name of a particular person
    (m: 'pr'; f: []), //product name
    (m: 'c'; f: []), //company name
    (m: 'o'; f: []), //organization name
    (m: 'st'; f: []), //stations
    (m: 'wk'; f: []) //work of literature, art, film, etc.
  );

//If your EDICT-compatible dictionary uses other markers, build your own table
//and pass it to the parser.


function FindMarkDef(m: string): TMarker; //returns marker # or #00
function GetMarkerText(id: TMarker): string; inline;


//We use fixed arrays for speed, increase as needed
const
  MaxKanji = 20; //max seen: 14
  MaxKana = 20; //must be not less than MaxKanji
  MaxSense = 50; //at most I've seen 27 in EDICT2

type
  EEdictParsingException = class(Exception);

  TKanjiEntry = record
    kanji: UnicodeString;
    markers: TMarkers;
    procedure Reset;
  end;
  PKanjiEntry = ^TKanjiEntry;

  TKanaEntry = record
    kana: UnicodeString;
    kanji: array[0..MaxKanji-1] of UnicodeString;
    kanji_used: integer;
    markers: TMarkers;
    procedure Reset;
    function AddKanji: PUnicodeString;
  end;
  PKanaEntry = ^TKanaEntry;

  TSenseEntry = record
    pos: TMarkers;
    markers: TMarkers;
    text: UnicodeString;
    procedure Reset;
  end;
  PSenseEntry = ^TSenseEntry;

  TEdictArticle = record
    ref: string;
    kanji: array[0..MaxKanji-1] of TKanjiEntry;
    kanji_used: integer;
    kana: array[0..MaxKana-1] of TKanaEntry;
    kana_used: integer;
    senses: array[0..MaxSense-1] of TSenseEntry;
    senses_used: integer;
    pop: boolean;
    procedure Reset;
    function AddKanji: PKanjiEntry;
    function AddKana: PKanaEntry;
    function AddSense: PSenseEntry;
    procedure TrimEverything;
  end;
  PEdictArticle = ^TEdictArticle;

//Parses EDICT2, EDICT
procedure ParseEdict2Line(const s:UnicodeString; ed: PEdictArticle);

//Parses CC-EDICT, various stages of CEDICT evolution
procedure ParseCCEdictLine(const s:UnicodeString; ed: PEdictArticle);

implementation
uses JWBStrings;

function FindMarkDef(m: string): TMarker;
var i: integer;
begin
  Result := #00;
  for i := Low(Markers) to High(Markers) do
   {$IFDEF CASE_SENSITIVE_MARKERS}
    if SysUtils.SameStr(markers[i].m, m) then begin
   {$ELSE}
    if markers[i].m = m then begin
   {$ENDIF}
      Result := AnsiChar(i);
      break;
    end;
end;

function GetMarkerText(id: TMarker): string;
begin
  Result := Markers[byte(id)].m;
end;

procedure TKanjiEntry.Reset;
begin
  kanji := '';
  markers := '';
end;

procedure TKanaEntry.Reset;
begin
  kana := '';
  kanji_used := 0;
  markers := '';
end;

function TKanaEntry.AddKanji: PUnicodeString;
begin
  Inc(kanji_used);
  if kanji_used>Length(kanji) then
    raise EEdictParsingException.Create('KanaEntry: Cannot add one more kanji');
  Result := @kanji[kanji_used-1];
  Result^ := '';
end;

procedure TSenseEntry.Reset;
begin
  pos := '';
  markers := '';
  text := '';
end;

procedure TEdictArticle.Reset;
begin
  ref := '';
  kanji_used := 0;
  kana_used := 0;
  senses_used := 0;
  pop := false;
end;

function TEdictArticle.AddKanji: PKanjiEntry;
begin
  Inc(kanji_used);
  if kanji_used>Length(kanji) then
    raise EEdictParsingException.Create('EdictArticle: Cannot add one more kanji');
  Result := @kanji[kanji_used-1];
  Result^.Reset;
end;

function TEdictArticle.AddKana: PKanaEntry;
begin
  Inc(kana_used);
  if kana_used>Length(kana) then
    raise EEdictParsingException.Create('EdictArticle: Cannot add one more kana');
  Result := @kana[kana_used-1];
  Result^.Reset;
end;

function TEdictArticle.AddSense: PSenseEntry;
begin
  Inc(senses_used);
  if senses_used>Length(senses) then
    raise EEdictParsingException.Create('EdictArticle: Cannot add one more meaning');
  Result := @senses[senses_used-1];
  Result^.Reset;
end;

procedure TEdictArticle.TrimEverything;
var i,j: integer;
begin
  for i := 0 to kanji_used - 1 do
    kanji[i].kanji := UTrim(kanji[i].kanji);
  for i := 0 to kana_used - 1 do begin
    kana[i].kana := UTrim(kana[i].kana);
    for j := 0 to kana[i].kanji_used - 1 do
      kana[i].kanji[j] := UTrim(kana[i].kanji[j]);
  end;
  for i := 0 to senses_used - 1 do
    senses[i].text := UTrim(senses[i].text);
end;

function IsNumeric(const s:UnicodeString): boolean;
var i: integer;
begin
  Result := true;
  for i := 1 to Length(s) do
    if (s[i]<'0') or (s[i]>'9') then begin
      Result := false;
      exit;
    end;
end;

{
EDICT2/EDICT:
Example of string:
  いい加減(P);好い加減;好加減(io) [いいかげん]
  /(adj-na) (1) (uk) irresponsible/perfunctory/careless
  /(2) lukewarm/half-baked/halfhearted/vague
  /(3) (See いい加減にする) reasonable/moderate (usu. in suggestions or orders)
  /(adv) (4) considerably/quite/rather/pretty
  /(P)
  /EntL1277440X/

Features of EDICT2 to watch out for:

Multi-kanji, multi-reading, with specific pairings:
  あっと言う間;あっという間;あっとゆう間
  [あっというま(あっと言う間,あっという間); あっとゆうま(あっと言う間,あっとゆう間)]
  /(exp) (See あっと言う間に) a blink of time/the time it takes to say "Ah!"
  /EntL2208410/

Common markers for some kanji/readings:
Separate (P) markers for article and readings:
(P) marker for article applies to all of its entries
  あり得ない(P);有り得ない(P);有得ない [ありえない]
  /(adj-i) (uk) (See 有り得る・ありうる) impossible/unlikely/improbable/(P)
  /EntL2109610X/

POS markers apply to "all senses starting with this one":
Non-POS markers apply only to current sense:
  いい事[いいこと] /(exp,n) (1) good thing/nice thing
    /(2) (usu. as ～をいいことに(して)) good excuse/good grounds/good opportunity
    /(int) (3) (fem) interjection used to impress an idea or to urge a response/EntL2583070/

Sequence '/(' can occur legitimately:
  /(expression of) effort
}

procedure ParseEdict2Line(const s:UnicodeString; ed: PEdictArticle);
const
  EH_KANJI = 1;
  EH_KANA = 2;
  EH_KANAKANJI = 3;
  EH_SENSE = 4;
var
  curkanji: PKanjiEntry;
  curkana: PKanaEntry;
  curkanakanji: PUnicodeString;
  cursense: PSenseEntry;
  nextsense: TSenseEntry; //current part of the sense, still not commited
  curmark: UnicodeString; //current marker word, to be tested
  inmarker: integer;
  markdef: TMarker;
  markch: WideChar; //if inmarker, this holds the marker opening braket type -- only for senses
  markopen: boolean; //if set, some of the markers weren't found and we added a marker braket back to curtext -- have to close it
  commpos: TMarkers; //common POS markers -- carried over from the previous sense
  eh: integer;
  ch: WideChar;
  i: integer;

  procedure CommitNextSense;
  begin
   //detect ref
    if (upos(' ', nextsense.text)=0) and (Length(nextsense.text)>4)
    and (nextsense.text[1]='E') and (nextsense.text[2]='n') and (nextsense.text[3]='t') and (nextsense.text[4]='L') then begin
      ed.ref := nextsense.text;
    end else begin
      if nextsense.text<>'' then // stuff like /(P)/ can leave us with empty text
        if cursense^.text<>'' then
          cursense^.text := cursense^.text + '/' + UTrim(nextsense.text)
        else
          cursense^.text := UTrim(nextsense.text);
      cursense^.markers := cursense^.markers + nextsense.markers;
      cursense^.pos := cursense^.pos + nextsense.pos;
    end;
    nextsense.Reset;
  end;

  procedure NewSense;
  begin
    //Commit old sense
    if cursense.pos = '' then
      cursense.pos := commpos
    else
      commpos := cursense.pos; //new pos markers
    //Add sense
    cursense := ed.AddSense;
  end;

begin
  ed.Reset;
  eh := EH_KANJI;
  curkanji := ed.AddKanji;
  curkana := nil;
  curkanakanji := nil;
  cursense := nil;
  nextsense.Reset;
  inmarker := 0;
  markopen := false;
  markch := #00;
  commpos := '';

  i := 1;
  while i<=Length(s) do begin
    ch := s[i];
    if (eh=EH_KANJI) and (ch='(') then Inc(inmarker) else
    if (eh=EH_KANJI) and (inmarker=1) and ((ch=',') or (ch=')')) then begin //only on first level of depth
      markdef := FindMarkDef(UTrim(curmark));
     //recognized EDICT marker
      if markdef <> #00 then
        curkanji.markers := curkanji.markers + markdef;
     //there's nothing we can do if it's unrecognized -- have to just drop it
      curmark := '';
      if ch=')' then
        Dec(inmarker);
    end else
    if (eh=EH_KANJI) and (inmarker>1) and (ch=')') then Dec(inmarker) else
    if (eh=EH_KANJI) and (inmarker>0) then curmark := curmark + ch else
    if (eh=EH_KANJI) and (inmarker>0) and ((ch=';')or(ch='[')or(ch='/')) then //safety
      raise EEdictParsingException.Create('Invalid characters in kanji markers')
    else
    if (eh=EH_KANJI) and (ch=';') then curkanji := ed.AddKanji else
    if (eh=EH_KANJI) and (ch='[') then begin eh := EH_KANA; curkana := ed.AddKana; end else
    if (eh=EH_KANJI) and (ch='/') then begin eh := EH_SENSE; cursense := ed.AddSense; end else
    if (eh=EH_KANJI) then curkanji.kanji := curkanji.kanji + ch else
    if (eh=EH_KANA) and (ch='(') then begin
      if (inmarker<=0) and (EvalChar(s[i+1])=EC_IDG_CHAR) then begin
        eh := EH_KANAKANJI;
        curkanakanji := curkana.AddKanji;
      end else
        Inc(inmarker);
    end else
    if (eh=EH_KANA) and (inmarker=1) and ((ch=',') or (ch=')')) then begin //only on first level of depth
      markdef := FindMarkDef(UTrim(curmark));
     //recognized EDICT marker
      if markdef <> #00 then
        curkana.markers := curkana.markers + markdef;
      curmark := '';
      if ch=')' then
        Dec(inmarker);
    end else
    if (eh=EH_KANA) and (inmarker>1) and (ch=')') then Dec(inmarker) else
    if (eh=EH_KANA) and (inmarker>0) then curmark := curmark + ch else
    if (eh=EH_KANJI) and (inmarker>0) and ((ch=';')or(ch=']')) then //safety
      raise EEdictParsingException.Create('Invalid characters in kanji markers')
    else
    if (eh=EH_KANA) and (ch=';') then curkana := ed.AddKana else
    if (eh=EH_KANA) and (ch=']') then begin eh := EH_SENSE; cursense := ed.AddSense; end else
    if (eh=EH_KANA) then curkana.kana := curkana.kana + ch else
    if (eh=EH_KANAKANJI) and (ch=',') then curkanakanji := curkana.AddKanji else
    if (eh=EH_KANAKANJI) and (ch=')') then eh := EH_KANA else
    if (eh=EH_KANAKANJI) then curkanakanji^ := curkanakanji^ + ch else
   {
    We use a special approach for meaning.
    Anything we read, we put into 'curpart'. When we encounter a '/', we flush
    'curpart' into 'current meaning'.
    If, inside a part, we encounter (2), (3) etc, then it's a new part, and
    we replace 'current meaning' with a new one.
    When we encounter markers, we strip known ones and leave unrecognized ones.
  }
    if (eh=EH_SENSE) and (ch='/') then begin
      CommitNextSense();
    end else
    if (eh=EH_SENSE) and ((ch='(')or(ch='{')) then begin
      Inc(inmarker);
      if (inmarker=1) then begin
        markch:=ch;
        markopen:=false;
        curmark:='';
      end;
    end else
    if (eh=EH_SENSE) and (inmarker=1) and ((ch=',')or(ch=')')or(ch='}')) then begin //only on first level
     //New entry
      if IsNumeric(curmark) then
        if curmark='1' then
          begin end //first entry doesn't need any additional slot, but we still handle it to cut the (1) mark
        else
          NewSense()
      else
     //(P) marker
      if curmark='P' then begin
        ed.pop := true;
      end else begin
        markdef := FindMarkDef(UTrim(curmark));
       //recognized EDICT marker
        if markdef <> #00 then begin
          if mfPos in markers[byte(markdef)].f then
            nextsense.pos := nextsense.pos + markdef
          else
            nextsense.markers := nextsense.markers + markdef;
        end else
       //unrecognized marker or normal text
        begin
          if not markopen then begin
            nextsense.text := nextsense.text + markch + curmark; //no trim
            markopen := true;
          end else
            nextsense.text := nextsense.text + ',' + curmark;
        end;
      end;

      curmark := '';
      if ch<>',' then begin
        if markopen then
          case markch of
            '(': nextsense.text := nextsense.text + ')';
            '{': nextsense.text := nextsense.text + '}';
          end;
        Dec(inmarker);
      end;
    end else
    if (eh=EH_SENSE) and (inmarker>1) and ((ch=')')or(ch='}')) then Dec(inmarker) else
    if (eh=EH_SENSE) and (inmarker>0) then curmark := curmark + ch else
    if (eh=EH_SENSE) then nextsense.text := nextsense.text + ch;

    Inc(i);
  end;

  if (eh=EH_SENSE) and (nextsense.text<>'') or (nextsense.markers<>'') or (nextsense.pos<>'') then //shouldnt happen
    CommitNextSense();

 //No sense part => 100% invalid line, helps to catch wrong encodings
  if eh<>EH_SENSE then
    raise EEdictParsingException.Create('Invalid EDICT line.');

  ed.TrimEverything;
end;

{
CCEDICT is in a similar but different format:

Kanji versions are separated by space, not ';':
  授計 授计 [shou4 ji4] /to confide a plan to sb/
There are at most two of these (traditional and simplified).
Kanji versions can be the same:
  授予 授予 [shou4 yu3] /to award/to confer/

There's at most one reading, but it contains spaces:
  授受不親 授受不亲 [shou4 shou4 bu4 qin1]
Reading is in pinyin, but can contain english letters (I've only seen capital ones):
  AA制 AA制 [A A zhi4] /to split the bill/to go Dutch/
Can contain commas or · dots when kanji contain those:
  一日不見，如隔三秋 一日不见，如隔三秋 [yi1 ri4 bu4 jian4 , ru2 ge2 san1 qiu1]

Markers are not supported.

EDICT separates senses and glosses:
  /(1) falter/waver/(2) flap
CC-EDICT doesn't, unrelated and related senses are formatted the same:
  /numerical range/interval/taxation band/
  /tired/exhausted/wretched/

Therefore we are either to show ALL glosses as separate senses:
  (1) tired; (2) exhausted; (3) wretched
Or to show them as a single sense:
  numerical range; interval; taxation band
Second one probably looks better.
}
procedure DeleteDuplicateKanji(ed: PEdictArticle); forward;
procedure ParseCCEdictLine(const s:UnicodeString; ed: PEdictArticle);
const
  EH_KANJI = 1;
  EH_KANA = 2;
  EH_SENSE = 3;

var
  curkanji: PKanjiEntry;
  curkana: PKanaEntry;
  cursense: PSenseEntry;
  eh: integer;
  i: integer;
  ch: WideChar;

begin
  ed.Reset;
  eh := EH_KANJI;
  curkanji := nil;
  curkana := nil;
  cursense := nil;

  i := 1;
  while i<=Length(s) do begin
    ch := s[i];

    if (eh=EH_KANJI) and (ch=' ') then curkanji := nil {kanji over, but perhaps there will be no another kanji} else
    if (eh=EH_KANJI) and (ch='[') then begin eh := EH_KANA; curkana := ed.AddKana; end else
    if (eh=EH_KANJI) and (ch='/') then begin eh := EH_SENSE; cursense := ed.AddSense; end else
    if (eh=EH_KANJI) then begin
      if curkanji=nil then curkanji := ed.AddKanji;
      curkanji.kanji := curkanji.kanji + ch;
    end else
    if (eh=EH_KANA) and (ch=']') then begin eh := EH_SENSE; cursense := nil; end else
    if (eh=EH_KANA) then curkana.kana := curkana.kana + ch else
    if (eh=EH_SENSE) and (cursense=nil) and (ch='/') then cursense := ed.AddSense else //first time we encounter / we just start a sense
    if (eh=EH_SENSE) and (cursense=nil) then begin { skip until sense start } end else
    if (eh=EH_SENSE) then cursense.text := cursense.text + ch;

    Inc(i);
  end;

 //Senses usually end on /, so we replaced that with ;
  if (cursense<>nil) and (Length(cursense.text)>0) and (cursense.text[Length(cursense.text)]='/') then
    delete(cursense.text, Length(cursense.text), 1);

 //No sense part => 100% invalid line, helps to catch wrong encodings
  if eh<>EH_SENSE then
    raise EEdictParsingException.Create('Invalid CEDICT line.');

  ed.TrimEverything;

  //Simplified kanji could be the same as traditional, so kill off duplicates
  DeleteDuplicateKanji(ed);
end;

//Removes duplicate kanji. Checks only the kanji itself and not markers or kana
//attachments, so not valid for EDICT2.
procedure DeleteDuplicateKanji(ed: PEdictArticle);
var i,j: integer;
  dupshift: integer; //how many cells to skip
begin
  dupshift := 0;
  for i := 0 to ed.kanji_used - 1 do begin
    j := 0;
    while j+dupshift<i do begin
      if SysUtils.SameStr(ed.kanji[i].kanji,ed.kanji[j].kanji) then
        break;
      Inc(j);
    end;

    if j+dupshift<i then //found match
      Inc(dupshift) //skip this one too
    else //valid cell, shift it to the left
    if dupshift>0 then
      ed.kanji[i-dupshift]:=ed.kanji[i];
  end;
end;

end.
