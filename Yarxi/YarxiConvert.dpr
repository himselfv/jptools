program YarxiConvert;
{$APPTYPE CONSOLE}
{ Reads Yarxi database format and generates various things out of it.
 Requires sqlite3.dll in the same folder.
 Requires sqlite/sqlite3ds to compile }

uses
  SysUtils,
  UniStrUtils,
  ConsoleToolbox,
  JWBIO,
  FastArray,
  Yarxi,
  YarxiFmt,
  YarxiCore,
  YarxiStrings in 'YarxiStrings.pas';

type
  TYarxiConvert = class(TCommandLineApp)
  protected
    Command: string;
    Field: string;
    Yarxi: TYarxiDB;
    Output: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    procedure DumpKanjiFull(k: TKanjiRecord);
    procedure DumpCharLink(link: PCharLink; lvl: string='');
    procedure DumpKunyomi(ks: PKunReadingSet; km: PKunyomiMeaningBlock; lvl: string='');
    procedure DumpKunyomiKanjiLink(link: PKunyomiKanjiLink; lvl: string='');
    procedure DumpKunyomiMeaningClause(rc: PKunyomiMeaningClause; lvl: string='');
    procedure DumpRelatedKanji(link: PRelatedKanjiLink; lvl: string='');
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure RunKanji(const field: string);
    procedure RunTango;
  end;

procedure TYarxiConvert.ShowUsage;
begin
  writeln(ErrOutput, 'Usage: '+ProgramName+' <command> <field>');
  writeln(ErrOutput, 'Supported commands:');
  writeln(ErrOutput, '  kanji = print kanji table fields');
  writeln(ErrOutput, '  tango = print tango table');
end;

function TYarxiConvert.HandleSwitch(const s: string; var i: integer): boolean;
begin
  Result := inherited;
end;

function TYarxiConvert.HandleParam(const s: string; var i: integer): boolean;
begin
  if Command='' then
    Command := s
  else
  if Command='kanji' then
    Field := s;
  Result := true;
end;

procedure TYarxiConvert.Run;
begin
  if Command='' then
    BadUsage();

  Yarxi := TYarxiDB.Create('yarxi.db');
  KanaTran.LoadFromFile('yarxi.kcs');

  Output := ConsoleWriter();

  if Command='kanji' then
    RunKanji(Field)
  else
  if (Command='words') or (Command='tango') then
    RunTango
  else
    BadUsage('Invalid command: '+Command);

  FreeAndNil(Output);
  FreeAndNil(Yarxi);
end;

procedure TYarxiConvert.RunKanji(const field: string);
var k: TKanjiRecord;
begin
  writeln(ErrOutput, IntToStr(Yarxi.KanjiCount)+' kanji in DB.');
  writeln(ErrOutput, IntToStr(YarxiCore.Complaints)+' complaints.');
  for k in Yarxi.Kanji do
    if field='rawrusnick' then
      Output.WriteLn(k.RawKunYomi)
    else
    if field='rusnick' then
      Output.WriteLn(FastArray.Join(k.RusNicks, '/'))
    else
    if field='ons' then
      Output.WriteLn(k.JoinOns)
    else
    if field='kanaons' then
      Output.WriteLn(KanaTran.RomajiToKana('K'+k.JoinOns(' '), []))
    else
    if field='rawkunyomi' then
      Output.WriteLn(k.RawKunYomi)
    else
    if field='kunyomi' then
      Output.WriteLn(DumpKanjiKunYomi(k.KunYomi))
    else
    if field='rawrussian' then
      Output.WriteLn(k.RawRussian)
    else
{    if field='russian' then
      Output.WriteLn(DumpKanjiRussian(k.Russian))
    else}
    if field='rawcompounds' then
      Output.WriteLn(k.RawCompounds)
    else
    if field='compounds' then
      Output.WriteLn(DumpKanjiCompounds(k.Compounds))
    else
    if field='*' then
      DumpKanjiFull(k)
    else
      raise Exception.Create('Unknown field');
end;

procedure TYarxiConvert.DumpKanjiFull(k: TKanjiRecord);
var i: integer;
  ks: PKunReadingSet;
  km: PKunyomiMeaningBlock;
begin
  Output.WriteLn('#'+IntToStr(k.Nomer)+': '+k.Kanji);
  if k.RusNicks.Length>1 then
    Output.WriteLn('Nick: '+k.RusNick + ' ('+FastArray.Join(k.RusNicks, '/')+')')
  else
    Output.WriteLn('Nick: '+k.RusNick);
  Output.WriteLn('ON: '+k.JoinOns());
  Output.WriteLn('Kun:');
  if k.KunYomi.show_kuns>0 then
    Output.WriteLn('  -show only '+IntToStr(k.KunYomi.show_kuns)+' kuns.');
  if k.KunYomi.show_tango>0 then
    Output.WriteLn('  -show only '+IntToStr(k.KunYomi.show_tango)+' tango.');

  i := 0;
  while (i<Length(k.KunYomi.kun)) or (i<k.Russian.kunyomi.Length) do begin
    if i<Length(k.KunYomi.kun) then
      ks := @k.KunYomi.kun[i]
    else
      ks := nil;
    if i<k.Russian.kunyomi.Length then
      km := PKunyomiMeaningBlock(k.Russian.kunyomi.GetPointer(i))
    else
      km := nil;
    DumpKunyomi(ks, km, '  ');
    Inc(i);
  end;

  case k.Russian.fk_flag of
    fkMissing: Output.Writeln('Feldman-Konrad: Missing.');
    fkDeprecated: Output.Writeln('Feldman-Konrad: Deprecated.');
    fkOriginal: Output.Writeln('Feldman-Konrad: Original.');
    fkSimplified: Output.Writeln('Feldman-Konrad: Simplified.');
  //иначе ничего
  end;

  if k.Russian.related_kanji.Count>0 then
    Output.WriteLn('Related kanji:');
  for i := 0 to k.Russian.related_kanji.Count-1 do
    DumpRelatedKanji(PRelatedKanjiLink(k.Russian.related_kanji.GetPointer(i)), '  ');

  Output.WriteLn('');
end;

{ Любой из rs, rb может быть nil, если парного нет }
procedure TYarxiConvert.DumpKunyomi(ks: PKunReadingSet; km: PKunyomiMeaningBlock; lvl: string='');
var i: integer;
begin
 //Заголовок
  if ks=nil then
    Output.WriteLn(lvl+'!!! no kunyomi entry')
  else begin
    Output.Write('  '+ks.kanji);
    for i := 0 to Length(ks.items)-1 do begin
      Output.Write(' ['+ks.items[i].kana+'/'+ks.items[i].romaji);
      if krHidden in ks.items[i].flags then
        Output.Write('/hidden');
      if krUnchecked in ks.items[i].flags then
        Output.Write('/unchecked');
      if krSpaces in ks.items[i].flags then
        Output.Write('/spaces');
      Output.Write(']');
    end;
    if ks.latin_tail<>'' then
      Output.Write(' ~~ '+ks.latin_tail);
    Output.WriteLn('');

   //Начальная информация
    if ks.flags<>[] then begin
      Output.Write(lvl+'  flags: ');
      if ksTranscriptionUnderWord in ks.flags then Output.Write('transcr-under-word ');
      Output.WriteLn('');
    end;
  end;

 //Переводы
  if km=nil then
    Output.WriteLn(lvl+'  !!! no meaning entry')
  else begin
    if km.isNominal then
      Output.WriteLn(lvl+'  -- nominal meaning');
    for i := 0 to km.clauses.Count-1 do
      DumpKunyomiMeaningClause(PKunyomiMeaningClause(km.clauses.GetPointer(i)), lvl+'  ');
    for i := 0 to km.links.Count-1 do
      DumpKunyomiKanjiLink(PKunyomiKanjiLink(km.links.GetPointer(i)), lvl+'  ');
  end;

 //Прочая информация
  if ks<>nil then begin
    for i := 0 to ks.refs.Length-1 do
      DumpCharLink(PCharLink(ks.refs.GetPointer(i)), lvl+'  ');

    case ks.usually_in of
      uiHiragana: Output.WriteLn('    Usually in hiragana');
      uiKatakana: Output.WriteLn('    Usually in katakana');
      uiKana:  Output.WriteLn('    Usually in kana');
    end;

    for i := 0 to ks.tl_usually_in.Length-1 do
      case ks.tl_usually_in[i] of
        uiHiragana: Output.WriteLn('    '+IntToStr(i)+': Usually in hiragana');
        uiKatakana: Output.WriteLn('    '+IntToStr(i)+': Usually in katakana');
        uiKana:  Output.WriteLn('    '+IntToStr(i)+': Usually in kana');
      end;
  end;
end;

procedure TYarxiConvert.DumpCharLink(link: PCharLink; lvl: string='');
begin
  Output.Write(lvl+'link: '+link._type+' ');
  case link.pos of
    lpDefault: Output.Write('@def ');
    lpFirstLine: Output.Write('@first ');
    lpAllClauses: Output.Write('@all ');
    lpOneClause: Output.Write('@'+IntToStr(link.posfrom)+' ');
    lpFromTo: Output.Write('@'+IntToStr(link.posfrom)+'-'+IntToStr(link.posto)+' ');
    lpNewline: Output.Write('@newl ');
    lpKanji: Output.Write('@kanji ');
  end;

  Output.Write(link.text);

  if link.wordref=0 then
    Output.WriteLn('')
  else
    Output.WriteLn('='+IntToStr(link.wordref));
end;

procedure TYarxiConvert.DumpKunyomiMeaningClause(rc: PKunyomiMeaningClause; lvl: string='');
var i: integer;
  entry: PKunyomiMeaningSuffixedEntry;
begin
 //Начало кляузы. Первое вхождение в той же строчке (чаще всего оно одно),
 //последующие с отступом на следующих.
  if rc.common_clause then
    Output.Write(lvl+'Common: ')
  else
  if rc.index=0 then
    Output.Write(lvl) //Для нуля в порядке исключения ничего не пишем, т.к. самый частый вариат
  else
    Output.Write(lvl+'#'+IntToStr(rc.index)+': ');

  if rc.common_clause and (rc.index<>0) then
    Output.Write(' !!! common clause with non-zero index! ');

  for i := 0 to rc.entries.Count-1 do begin
    entry := PKunyomiMeaningSuffixedEntry(rc.entries.GetPointer(i));
    if i>0 then
      Output.Write(lvl+'  ');
    if entry.suffix<>'' then
      Output.Write('~~ '+entry.suffix+': ');
    if entry.text<>'' then
      Output.WriteLn(entry.text)
    else
      Output.WriteLn('---');
  end;
end;

procedure TYarxiConvert.DumpRelatedKanji(link: PRelatedKanjiLink; lvl: string='');
begin
  Output.Write(lvl+'link: '+link._type);
  case link.pos of
    rpBigGray: Output.Write(' @biggray');
    rpUnderKanji: Output.Write(' @underkanji');
    rpUnderNickname: Output.Write(' @undernick');
  end;
  Output.WriteLn(' '+link.text);
end;

procedure TYarxiConvert.DumpKunyomiKanjiLink(link: PKunyomiKanjiLink; lvl: string='');
begin
  Output.Write(lvl+'link: '+link._type+': '+link.text);
end;

procedure TYarxiConvert.RunTango;
var k: TTangoRecord;
begin
  writeln(ErrOutput, IntToStr(Yarxi.TangoCount)+' tango in DB.');
  writeln(ErrOutput, IntToStr(YarxiCore.Complaints)+' complaints.');

  for k in Yarxi.Tango do
    Output.WriteLn(k.Kana + #09 + k.Reading + #09 + k.Russian);
end;

begin
  RunApp(TYarxiConvert);
end.
