program AnkiWordList;
{
Parses a word/expression list - one expression at a line, possibly
tab-separated table with other unrelated fields.

Builds a card list in a format suitable for importing to Anki:
  expression      translations

Intended usage:
 1. Make a word list for studying either manually or by other means.
 2. Generate Anki cards with this tool.
 3. Import to Anki.

To update existing deck with translations from another dictionary:
 1. Export the notes into tab-separated text file.
 2. Run this tool and generate another tab-separated file with translations.
 3. Create additional note field in Anki.
 4. Import new file, pasting translations into the new field.

Requires any EDICT-compatible dictionary.
}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, StrUtils, UniStrUtils, ConsoleToolbox, JWBIO,
  EdictReader, Edict, FastArray, SearchSort, EntryFormatting, ExprMatching;


const
  //In the priority order
  CommonSuffixes: string = 'する をする に な の で と にする になる になって '
    +'なる した とした として ある のある である';

type
  TQuery = record
   { For now we ignore <ruby>kanji<rt>kana constructs.
    In the future we need to expand this struct to allow for edict-style
    kana->kanji  assignments, then parse <ruby> into this structure.
    All the usual edict rules apply (no explicit kana = all kana etc.) }
    expr_text: string;
    expr: TStringArray;
    read_text: string;
    read: TStringArray;
    procedure Reset;
  end;
  PQuery = ^TQuery;

  TMatch = record
    entry: PEdictEntry;
    scoreKanji: double;
    scoreKana: double;
  end;
  PMatch = ^TMatch;

  TMatchMode = (mmBest, mmMultiple, mmSplit);

  TMatchUniformityFlag = (
    muMultiKanji,   //has matches with different kanji
    muMultiKana,    //has matches with different kana
    muMultiTail     //has matches with different tails
  );
  TMatchUniformityFlags = set of TMatchUniformityFlag;

  TOutputEntry = record
    key: string;
    expr: string;
    read: string;
    text: string;
    procedure Reset(const AKey: string);
  end;
  POutputEntry = ^TOutputEntry;

  TAnkiWordList = class(TCommandLineApp)
  protected
    outp: TStreamEncoder;
    err: TStreamEncoder; //also has unicode
  protected
    EdictFile: string;
    Edict: TEdict;
  protected
    Files: array of string;
    ExprColumn: integer;
    ReadColumn: integer;
    ExprSep: char;
    ReadSep: char;
  protected
    OutputFile: string;
    MatchMode: TMatchMode;
    Suffixes: TUniStringArray;
    procedure Init; override;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    procedure SplitQuery(AExpr, ARead: string; out query: TQuery);
    function MatchKanji(const AEntry: PEdictEntry; const AKanji: TStringArray): integer;
    function MatchKana(const AEntry: PEdictEntry; const AKana: TStringArray): integer;
    function FindMatches(const query: TQuery): TArray<TMatch>;
    procedure SortMatches(var AMatches: TArray<TMatch>);
    function ScanMatchUniformity(const AMatches: TArray<TMatch>): TMatchUniformityFlags;
    procedure AddToOutput(var AOutput: TOutputEntry; AQuery: PQuery; AMatch: PMatch;
      AUniformity: TMatchUniformityFlags);
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string; outp: TStreamEncoder);
  end;

procedure TAnkiWordList.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -e EDICT          specify EDICT file (otherwise EDICT)');
  writeln('');
  writeln('Input:');
  writeln('  -te <column>      zero-based. Take expressions from this column, if tab-separated (default is 0)');
  writeln('  -tr <column>      Take readings from this column, if tab-separated (default is 1, if none available set to -1)');
  writeln('  -es <text>        Expression separator. If specified, multiple ways of writing an expression can be listed.');
  writeln('  -rs <text>        Reading separator. By default expression separator is used.');
  writeln('  -er regex         use this regex (PCRE) to match expressions (return "expr" and "read").');
  writeln('  -rr regex         use this regex (PCRE) to match readings (return "read").');
  writeln('');
  writeln(ProgramName + 'can also match entries from the dictionary with common suffixes (~ni, ~de etc)');
  writeln('  -suffixes <list>  space-separated list of suffixes (replaces the default one)');
  writeln('  -nosuffixes       disables suffix matching');
  writeln('');
  writeln('Output:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -xml              output xml instead of the default plaintext');
  writeln('  -xslt <filename>  format dictionary entries according to this XSLT schema (XML)');
  writeln('  -match=<mode>     if there are multiple matches:');
  writeln('    best            output ONE best match');
  writeln('    multiple        output all matches as a single result');
  writeln('    split           output multiple matches as multiple results');
end;

procedure TAnkiWordList.Init;
begin
  ExprColumn := 0;
  ReadColumn := 1;
  MatchMode := mmBest;
  Suffixes := StrSplit(PChar(CommonSuffixes), ' ');
end;

function TAnkiWordList.HandleSwitch(const s: string; var i: integer): boolean;
var tmp: string;
begin
  if s='-e' then begin
    if i>=ParamCount then BadUsage('-e needs dictionary name');
    Inc(i);
    EdictFile := ParamStr(i);
    Result := true
  end else

  if s='-te' then begin
    if i>=ParamCount then BadUsage('-te needs column number');
    Inc(i);
    ExprColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-tr' then begin
    if i>=ParamCount then BadUsage('-tr needs column number');
    Inc(i);
    ReadColumn := StrToInt(ParamStr(i));
    Result := true
  end else

  if s='-es' then begin
    if i>=ParamCount then BadUsage('-es needs separator value');
    Inc(i);
    SetExpressionSeparator(ParamStr(i)[1]);
    Result := true
  end else
  if s='-er' then begin
    if i>=ParamCount then BadUsage('-er needs separator value');
    Inc(i);
    SetReadingSeparator(ParamStr(i)[1]);
    Result := true
  end else
  if s='-er' then begin
    if i>=ParamCount then BadUsage('-er needs regex');
    Inc(i);
    SetExpressionPattern(ParamStr(i));
    Result := true
  end else
  if s='-rr' then begin
    if i>=ParamCount then BadUsage('-rr needs regex');
    Inc(i);
    SetReadingPattern(ParamStr(i));
    Result := true
  end else

  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-xml' then begin
    OutputXml := true;
    Result := true
  end else
  if s='-xslt' then begin
    if i>=ParamCount then BadUsage('-xslt needs XSLT schema filename');
    Inc(i);
    OutputXml := true;
    SetXsltSchema(ParamStr(i));
    Result := true
  end else
  if StartsText('-match=', s) then begin
    tmp := copy(s, Length('-match=')+1, MaxInt);
    if tmp='best' then
      MatchMode := mmBest
    else
    if tmp='multiple' then
      MatchMode := mmMultiple
    else
    if tmp='split' then
      MatchMode := mmSplit
    else
      BadUsage('Invalid match mode: '+tmp);
    Result := true;
  end else
  if s='-suffixes' then begin
    if i>=ParamCount then BadUsage('-suffixes needs a suffix list string');
    Inc(i);
    Self.Suffixes := StrSplit(PChar(ParamStr(i)), ' ');
    Result := true;
  end else
  if s='-nosuffixes' then begin
    SetLength(Self.Suffixes, 0);
    Result := true;
  end else
    Result := inherited;
end;

function TAnkiWordList.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := s;
  Result := true;
end;

procedure TAnkiWordList.Run;
var i: integer;
begin
  if Length(Files)<=0 then BadUsage('Specify input files.');

  if (readSep=#00) and (exprSep<>#00) then
    readSep := exprSep;

  err := ErrorConsoleWriter();
  err.WriteLn('Loading dictionary');
  Edict := TEdict.Create;
  if EdictFile='' then begin
    if FileExists('EDICT2') then
      Edict.LoadFromFile('EDICT2')
    else
      Edict.LoadFromFile('EDICT');
  end else
    Edict.LoadFromFile(EdictFile);

  err.WriteLn(IntToStr(edict.EntryCount)+' entries loaded.');

  if OutputFile<>'' then
    outp := UnicodeFileWriter(OutputFile)
  else
    outp := ConsoleWriter;
  outp.WriteBom;
  for i := 0 to Length(Files)-1 do
    ParseFile(Files[i], outp);
  FreeAndNil(outp); //flush

  FreeAndNil(err);
end;


{
Each entry in the dictionary has a number of writing:reading pairs as a flat list.
We don't care about further substructure (W1[R1,R2],W2[R3] -> W1:R1, W1:R2, W2:R3)

Same with input files.

Input may or may not have readings. Expression may be kanji or kana-only kana.
If readings are given, we should only match mismatching readings if no matching
reading entries are found.

Typical input cases and what we want to see:
1 kanji + 1 kana          -> entries with this pair
1 kanji [no kana column]  -> entries with this kanji + any readings, grouped by readings
1 kana only               -> all entries which have this either as kana-only (preferred),
                             or as kanji:kana, grouped by kanji
1 kanji + multiple readings [same sense]
                          -> all entries with either of these pairs, best ones (matching multiple readings) first
multiple kanji for the same reading
                          -> all entries with either of these pairs
}


{ Parse expr/read into a query - a bunch of fully separated expression/reading
 alternatives }

procedure TQuery.Reset;
begin
  SetLength(expr, 0);
  SetLength(read, 0);
end;

{ Expression and reading may contain stuff which needs to be trimmed }
function TrimExpr(const AExpr: string): string;
var i: integer;
begin
  Result := AExpr;

 { For now we don't properly parse <ruby> tags, just strip it. See comments
  for TQuery }
 //TODO: Parse <ruby> tags here or in SplitQuery().
  if StartsText('<ruby>',Result) and EndsText('</ruby>',Result) then begin
    delete(Result,1,Length('<ruby>'));
    delete(Result,Length(Result)-Length('<ruby>'),MaxInt);
  end;
  i := pos('<rt>',Result);
  if i>0 then
    delete(Result, i, MaxInt);

 //TODO: Maybe we shouldn't delete these here; we need to try these simplifications
 //  as we build a list of guesses with a dictionary.
 //  Simplified guesses should weigh less.

  if copy(Result,1,1)='～' then
    delete(Result,1,1); //todo: and set postf flag
  if copy(Result,Length(Result),1)='～' then
    delete(Result,Length(Result),1); //todo: and set pref flag
end;

function TrimRead(const ARead: string): string;
begin
  Result := ARead;
  if copy(Result,1,1)='～' then
    delete(Result,1,1); //todo: and set postf flag
  if copy(Result,Length(Result),1)='～' then
    delete(Result,Length(Result),1); //todo: and set pref flag
end;

{ Parses Expression and Reading fields from input file and splits it according
 to all supported markup.
 Reading may be empty, either due to Reading column not specified, or just
 reading field missing in this particular record. }
procedure TAnkiWordList.SplitQuery(AExpr, ARead: string; out query: TQuery);
var i: integer;
begin
  query.Reset;
  query.expr_text := AExpr;
  query.read_text := ARead;
  query.expr := StrSplit(PChar(AExpr), ExprSep); //OK if it's #00
  query.read := StrSplit(PChar(ARead), ReadSep); //OK if it's #00
  for i := 0 to Length(query.read)-1 do
    query.read[i] := TrimRead(query.read[i]);
end;


//How many AKanji variants AEntry matches.
function TAnkiWordList.MatchKanji(const AEntry: PEdictEntry; const AKanji: TStringArray): integer;
var i, j: integer;
begin
  Result := 0;
  for i := 0 to Length(AKanji)-1 do begin
    if AEntry.GetKanjiIndex(AKanji[i])>=0 then
      Inc(Result);
    //We also have to check suffixes. Cross-suffix matching is allowed too, and even nice!
    for j := 0 to Length(Self.Suffixes)-1 do
      if AEntry.GetKanjiIndex(AKanji[i]+Self.Suffixes[j])>=0 then
        Inc(Result);
  end;
end;

//How many AKana variants AEntry matches.
function TAnkiWordList.MatchKana(const AEntry: PEdictEntry; const AKana: TStringArray): integer;
var i, j: integer;
begin
  Result := 0;
  for i := 0 to Length(AKana)-1 do begin
    if AEntry.GetKanaIndex(AKana[i])>=0 then
      Inc(Result);
    //We also have to check suffixes. Cross-suffix matching is allowed too, and even nice!
    for j := 0 to Length(Self.Suffixes)-1 do
      if AEntry.GetKanaIndex(AKana[i]+Self.Suffixes[j])>=0 then
        Inc(Result);
  end;
end;

{ Builds a list of all potential matches, trying to be as inclusive as possible.
 Filtering comes later }
function TAnkiWordList.FindMatches(const query: TQuery): TArray<TMatch>;
var entries, entries2: TEdictEntries;
  i, j: integer;
  match: PMatch;
begin
  Result.Reset;
  if Length(query.expr)<=0 then exit; //it's even dangerous as we divide by it later

  //query.expr contains either the "kanji" part of "kanji:reading" pair,
  //or the only available part (usually "kana-only reading")

  //Find all entries
  //FindEntries searches both in kanji and in kana.
  for i := 0 to Length(query.expr)-1 do begin
    entries2 := Edict.FindEntries(query.expr[i]);
    MergeEntries(entries, entries2);
    //Check all suffix variations
    for j := 0 to Length(Self.Suffixes)-1 do begin
      entries2 := Edict.FindEntries(query.expr[i]+Self.Suffixes[j]);
      MergeEntries(entries, entries2);
    end;
    //Potentially we can also try to trim dubious symbols like "~" and fetch
    //with a lower score, but we'll need to keep track of score.
  end;

  //Score results
  for j := 0 to Length(entries)-1 do begin
    match := PMatch(Result.AddNew);
    match.entry := entries[j];
   //How many of provided kanji/kana entries it matches
    match.scoreKanji := MatchKanji(match.entry, query.expr) / Length(query.expr);

    if Length(query.read)=0 then begin
     { If no reading was provided, give preference to the entries which match
      *both* as kanji and as kana.
      E.g. prefer しかし [しかし] to 私窩子 [しかし] when matching しかし }
      if Length(match.entry.kana)<=0 then
        match.scoreKana := 1
      else
      if MatchKana(match.entry, query.expr {sic!})>0 then
        match.scoreKana := 1
      else
        match.scoreKana := 0;
    end else begin
      if Length(match.entry.kana)<=0 then //kana-only words store kana in Kanji field
        match.scoreKana := MatchKanji(match.entry, query.read) / Length(query.read)
      else
        match.scoreKana := MatchKana(match.entry, query.read) / Length(query.read);
    end;
  end;
end;

function MatchCmp(data: pointer; i1, i2: integer): integer;
var it1, it2: PMatch;
  pop1, pop2: integer;
begin
  it1 := @TArray<TMatch>(Data^).FItems[i1];
  it2 := @TArray<TMatch>(Data^).FItems[i2];
  if it1.entry.pop then pop1 := 1 else pop1 := 0;
  if it2.entry.pop then pop2 := 1 else pop2 := 0;
  Result := Trunc(
    (pop2-pop1)*1000
    + (it2.scoreKanji-it1.scoreKanji)*1000
    + (it2.scoreKana-it1.scoreKana)*1000
  );
  if Result=0 then
    Result := i1-i2; //order of appearance in the dic; sometimes more common words go first
end;

procedure MatchXch(data: pointer; i1, i2: integer);
var tmp: TMatch;
begin
  tmp := TArray<TMatch>(Data^).FItems[i1];
  TArray<TMatch>(Data^).FItems[i1] := TArray<TMatch>(Data^).FItems[i2];
  TArray<TMatch>(Data^).FItems[i2] := tmp;
end;

{ Sorts matches by relevancy }
procedure TAnkiWordList.SortMatches(var AMatches: TArray<TMatch>);
begin
  QuickSort(@AMatches, 0, AMatches.Count-1, MatchCmp, MatchXch);
end;

{ Checks whether the set of matches is uniform by kanji, kana and tails }
function TAnkiWordList.ScanMatchUniformity(const AMatches: TArray<TMatch>): TMatchUniformityFlags;
begin
  //TODO:
end;

procedure TOutputEntry.Reset(const AKey: string);
begin
  key := AKey;
  expr := '';
  read := '';
  text := '';
end;

procedure TryAddUnique(var str: string; const part, sep: string);
begin
  if pos(sep+part+sep, sep+str+sep)<=0 then
    if str<>'' then
      str := str + sep + part
    else
      str := part;
end;

procedure TAnkiWordList.AddToOutput(var AOutput: TOutputEntry; AQuery: PQuery;
  AMatch: PMatch; AUniformity: TMatchUniformityFlags);
var //i: integer;
  kanji_text,
  kana_text: string;
  tempEntry: TEdictEntry;
begin
  //TODO: Mind AUniformity and add "~tail", "reading" or "writing" or "writing[reading]"
  //  prefixes to text-formatted entries.
  //  Not sure what to do in XML mode. Add additional XML tags for these partial readings/writings?

 //If query mostly matched kana
  if MatchKana(AMatch.entry, AQuery.expr)>MatchKanji(AMatch.entry, AQuery.expr) then begin
   //then guess kanji (for now: use first, most popular one)
    kanji_text := AMatch.entry.kanji[0].kanji;
    kana_text := AQuery.expr_text;
   //TODO: More thorough kanji selection (check which kanji can be used with which reading)
  end else begin
   //else guess kana
    kanji_text := AQuery.expr_text;
    if Length(AMatch.entry.kana)>0 then
      kana_text := AMatch.entry.kana[0].kana
    else
      kana_text := AMatch.entry.kanji[0].kanji; //no kana => kana in kanji
  end;

 //For now we only keep one match
  AOutput.expr := kanji_text;
  AOutput.read := kana_text;

{  for i := 0 to Length(AMatch.entry.kanji)-1 do
    TryAddUnique(AOutput.expr, AMatch.entry.kanji[i].kanji, '、');

  for i := 0 to Length(AMatch.entry.kana)-1 do
    TryAddUnique(AOutput.read, AMatch.entry.kana[i].kana, '、'); }

  FormatEntryAdd(AOutput.text, AMatch.entry);
end;

//inp is destroyed on exit
procedure TAnkiWordList.ParseFile(const AFilename: string; outp: TStreamEncoder);
var inp: TStreamDecoder;
  ln, expr, read: string;
  query: TQuery;
  parts: TStringArray;
  EdictMatches: TArray<TMatch>;
  OutputEntries: TArray<TOutputEntry>;
  outp_text: string;
  outp_e: POutputEntry;
  i: integer;
  st: record
    lines: integer;
    expr: integer;
    outp: integer;
    multimatch: integer;
    badmatch: integer;
  end;
  MatchUniformity: TMatchUniformityFlags;
begin
  err.WriteLn('Parsing '+AFilename+'...');
  inp := OpenTextFile(AFilename);
  try
    FillChar(st, SizeOf(st), 0);

    while inp.ReadLn(ln) do begin
      Inc(st.lines);

      parts := StrSplit(PWideChar(ln),#09);
      if ExprColumn>=Length(parts) then
        continue;
      expr := Trim(parts[ExprColumn]);
      if (ReadColumn<0) or (ReadColumn=ExprColumn) or (ReadColumn>=Length(parts)) then
        read := ''
      else
        read := Trim(parts[ReadColumn]);

      Inc(st.expr);

      SplitQuery(expr, read, query);

      EdictMatches := FindMatches(query);
      if EdictMatches.Count<=0 then begin
        err.WriteLn('Not found: '+expr+' ['+read+']');
        continue;
      end;

      if EdictMatches.Count>1 then begin
        Inc(st.multimatch);
        SortMatches(EdictMatches); //even in multiple-match mode, sort by best
      end;

      if EdictMatches[0].scoreKana<=0.1 then begin
       //Effectively not a match
        Inc(st.badmatch);
        err.WriteLn('Bad match: '+expr+' ['+read+']');
      end;

      //In mmMultiple mode we may want to add ~prefixes
      if Self.MatchMode = mmMultiple then
        MatchUniformity := Self.ScanMatchUniformity(EdictMatches)
      else
      //mmSplit and mmBest are single-match-per-entry so all entries are uniform
        MatchUniformity := [];

     //Split output as configured
      outp_e := nil;
      OutputEntries.Reset;
      for i := 0 to EdictMatches.Count-1 do begin
        //mmSplit: New entry for every match
        if (i = 0) or (Self.MatchMode = mmSplit) then begin
          outp_e := POutputEntry(OutputEntries.AddNew);
          outp_e.Reset(expr);
        end;
        AddToOutput(outp_e^, @query, PMatch(EdictMatches.P[i]), MatchUniformity);
        if Self.MatchMode = mmBest then
          break;
        //mmSplit, mmMultiple: continue
      end;

     //Output
      for i := 0 to OutputEntries.Count-1 do begin
        with OutputEntries.GetPointer(i)^ do
          text := FormatEntryFinalize(text);
        outp_text := OutputEntries[i].key + #09 //key
         //Output expr and read because we don't know what we matched in each
         //case.
          + OutputEntries[i].expr + #09
          + OutputEntries[i].read + #09
          + OutputEntries[i].text;
        outp.WriteLn(outp_text);
        Inc(st.outp);
      end;

    end;
  finally
    FreeAndNil(inp);
  end;

  err.WriteLn(AFilename+' stats: '+IntToStr(st.lines)+' lines, '
    +IntToStr(st.expr)+' entries, '+IntToStr(st.outp)+' found.');
  err.WriteLn('Multiple matches: '+IntToStr(st.multimatch));
  err.WriteLn('Bad matches: '+IntToStr(st.badmatch));
end;

begin
  RunApp(TAnkiWordList);
end.
