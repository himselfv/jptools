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
  YarxiCore;

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
    procedure DumpAdditionalKanji(link: PAdditionalKanji; lvl: string='');
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
var i, j: integer;
  kun: PKunReadingSet;
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
  for i := 0 to Length(k.KunYomi.kun)-1 do begin
    kun := @k.KunYomi.kun[i];

    for j := 0 to Length(kun.items)-1 do begin
      Output.Write('  '+kun.items[j].text+' ');
      if krIgnoreInSearch in kun.items[j].flags then
        Output.Write('ignore-on-search ');
      if krOnReading in kun.items[j].flags then
        Output.Write('on-reading ');
      Output.WriteLn('');
    end;

    Output.WriteLn(Format('    Prefix:%d, main:%d, kuri:%d', [
      kun.prefix_chars,
      kun.main_chars,
      kun.kuri_chars
    ]));

    for j := 0 to kun.optional_spans.Length-1 do
      Output.WriteLn(Format('    opt_span:%d-%d', [
        kun.optional_spans[j].op,
        kun.optional_spans[j].ed
      ]));

    for j := 0 to kun.refs.Length-1 do
      DumpCharLink(PCharLink(kun.refs.GetPointer(j)), '    ');

    if kun.flags<>[] then begin
      Output.Write('    flags: ');
      if ksHidden in kun.flags then Output.Write('hidden ');
      if ksTranscriptionUnderWord in kun.flags then Output.Write('transr-under-word ');
      if ksWithKurikaeshi in kun.flags then Output.Write('with-kuri ');
      if ksUnchecked in kun.flags then Output.Write('unchecked ');
      Output.WriteLn('');
    end;

    if kun.latin_tail<>'' then
      Output.WriteLn('    '+kun.latin_tail);

    for j := 0 to kun.additional_kanji.Length-1 do
      DumpAdditionalKanji(PAdditionalKanji(kun.additional_kanji.GetPointer(j)), '    ');

    case kun.usually_in of
      uiHiragana: Output.WriteLn('    Usually in hiragana');
      uiKatakana: Output.WriteLn('    Usually in katakana');
      uiKana:  Output.WriteLn('    Usually in kana');
    end;

    for j := 0 to kun.tl_usually_in.Length-1 do
      case kun.tl_usually_in[j] of
        uiHiragana: Output.WriteLn('    '+IntToStr(j)+': Usually in hiragana');
        uiKatakana: Output.WriteLn('    '+IntToStr(j)+': Usually in katakana');
        uiKana:  Output.WriteLn('    '+IntToStr(j)+': Usually in kana');
      end;
  end;

  Output.WriteLn('');
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

procedure TYarxiConvert.DumpAdditionalKanji(link: PAdditionalKanji; lvl: string='');
begin
  Output.Write(lvl+'add_kanji: '+IntToStr(link.pos)+' ');
  Output.Write(link.text);
  if link.kuri then
    Output.WriteLn('kuri')
  else
    Output.WriteLn('');
end;

procedure TYarxiConvert.RunTango;
var k: TTangoRecord;
begin
  writeln(ErrOutput, IntToStr(Yarxi.TangoCount)+' tango in DB.');

  for k in Yarxi.Tango do
    Output.WriteLn(k.Kana + #09 + k.Reading + #09 + k.Russian);
end;

begin
  RunApp(TYarxiConvert);
end.
