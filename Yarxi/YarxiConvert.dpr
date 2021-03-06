program YarxiConvert;
{$APPTYPE CONSOLE}
{ Reads Yarxi database format and generates various things out of it.
 Requires sqlite3.dll in the same folder.
 Requires sqlite/sqlite3ds to compile

����� ��.:
  http://code.google.com/p/yarxi-pl/
  http://code.google.com/p/yarxi-pl/source/browse/trunk/JDFormatter.pm

�����:
�������� �������:
 match_*   ����������� ������� �� ������ ������������������, false ���� � ���
           ��� �� ������� ���������
 parse_*
 Parse*    ������� �� ������ ������������������ ��� ������� ������
 Is*       true, ���� ��������� ������ ��������� � �����. ������
 Eat*      ������� �� ������ ������������������ � ����������� �������. ���������
           ������� �������, ����� ��� ���� ��������.

����� �������:
 �� �����������, ��������� ������������� �� ��������� �������, ������� �������
  ���� �������� � Yarxi.
  ���� ����� ������� �������� �������������, �������� ��� ���� FORGIVING. �����
  ����� ��������� ����� � �����, ������ ����� ������.
 ����� ������ ������� #243 �� ����� ������, ��� �������� �����������.
}

uses
  SysUtils,
  UniStrUtils,
  ConsoleToolbox,
  JWBIO,
  FastArray,
  KanjidicReader,
  Kanjidic,
  Yarxi in 'Yarxi.pas',
  YarxiCore in 'YarxiCore.pas',
  YarxiStrings in 'YarxiStrings.pas',
  YarxiRefs in 'YarxiRefs.pas',
  YarxiKanji in 'YarxiKanji.pas',
  YarxiTango in 'YarxiTango.pas',
  YarxiReadingCommon in 'YarxiReadingCommon.pas';

type
  TYarxiConvert = class(TCommandLineApp)
  protected
    Command: string;
    Field: string;
    Yarxi: TYarxiDB;
    Output: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  protected
    procedure DumpKanjiFull(k: TKanjiRecord);
    procedure DumpCharLink(link: PCharLink; lvl: string='');
    procedure DumpKunyomi(ks: PKunReadingSet; km: PKunyomiMeaningBlock; lvl: string='');
    procedure DumpKunyomiKanjiLink(link: PKunyomiKanjiLink; lvl: string='');
    procedure DumpKunyomiMeaningClause(rc: PKunyomiMeaningClause; lvl: string='');
    procedure DumpRelatedKanji(link: PRelatedKanjiLink; lvl: string='');
  protected
    procedure DumpTangoFull(k: TTangoRecord);
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure RunKanji(const field: string);
    procedure RunTango(const field: string);
    procedure RunKanjidic(const field: string);
  end;

procedure TYarxiConvert.ShowUsage;
begin
  writeln(ErrOutput, 'Usage: '+ProgramName+' <command> <field>');
  writeln(ErrOutput, 'Supported commands:');
  writeln(ErrOutput, '  kanji = print kanji table fields');
  writeln(ErrOutput, '  tango = print tango table');
  writeln(ErrOutput, '  kanjidic = build kanjidic')
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
  if (Command='kanji') or (Command='tango') then
    Field := s;
  Result := true;
end;

procedure TYarxiConvert.Run;
begin
  if Command='' then
    BadUsage();

  Yarxi := TYarxiDB.Create('yarxi.db');
  KanaTran.LoadFromFile('Hepburn-Yarxi.roma');

  Output := ConsoleWriter();

  if Command='kanji' then
    RunKanji(Field)
  else
  if Command='tango' then
    RunTango(Field)
  else
  if Command='kanjidic' then
    RunKanjidic(Field)
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
    if field='rusnick' then
      Output.WriteLn(k.RawKunYomi)
    else
    if field='ons' then
      Output.WriteLn(k.JoinOns)
    else
    if field='kanaons' then
      Output.WriteLn(KanaTran.RomajiToKana('K'+k.JoinOns(' '), []))
    else
    if field='kunyomi' then
      Output.WriteLn(k.RawKunYomi)
    else
    if (field='russian') or (field='rawrussian') then
      Output.WriteLn(k.RawRussian)
    else
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
  //����� ������
  end;

  if k.Russian.related_kanji.Count>0 then
    Output.WriteLn('Related kanji:');
  for i := 0 to k.Russian.related_kanji.Count-1 do
    DumpRelatedKanji(PRelatedKanjiLink(k.Russian.related_kanji.GetPointer(i)), '  ');

  Output.WriteLn('');
end;

{ ����� �� rs, rb ����� ���� nil, ���� ������� ��� }
procedure TYarxiConvert.DumpKunyomi(ks: PKunReadingSet; km: PKunyomiMeaningBlock; lvl: string='');
var i: integer;
begin
 //���������
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

   //��������� ����������
    if ks.flags<>[] then begin
      Output.Write(lvl+'  flags: ');
      if ksTranscriptionUnderWord in ks.flags then Output.Write('transcr-under-word ');
      Output.WriteLn('');
    end;
  end;

 //��������
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

 //������ ����������
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
 //������ ������. ������ ��������� � ��� �� ������� (���� ����� ��� ����),
 //����������� � �������� �� ���������.
  if rc.common_clause then
    Output.Write(lvl+'Common: ')
  else
  if rc.index=0 then
    Output.Write(lvl) //��� ���� � ������� ���������� ������ �� �����, �.�. ����� ������ ������
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
    if field='kana' then
      Output.WriteLn(IntToStr(k.K1)+' '+IntToStr(k.K2)+' '+IntToStr(k.K3)+' '
        +IntToStr(k.K4)+' '+k.RawKana)
    else
    if field='reading' then
      Output.WriteLn(k.RawReadings)
    else
    if field='*' then
      DumpTangoFull(k)
    else
      raise Exception.Create('Unknown field');
end;

procedure TYarxiConvert.DumpTangoFull(k: TTangoRecord);
var i: integer;
begin
 //������ �������
  Output.Write(k.Kana);
  if Length(k.Readings)>0 then begin
    Output.Write(' [');
    for i := 0 to Length(k.Readings)-1 do begin
      if i>0 then
        Output.Write(', ');
      Output.Write(RomajiToKana(k.Readings[i].text));
      if k.Readings[i].rare then
        Output.Write('/rare');
    end;
    Output.Write(']');
  end;
  Output.WriteLn('');

 //����� ������
  Output.WriteLn('');
end;

procedure TYarxiConvert.RunKanjidic(const field: string);
var kdic: TKanjiDic;
  kentry: PKanjidicEntry;
  k: TKanjiRecord;
  ke: TKanjidicEntry;
  i, j: integer;
  kset: PKunReadingSet;
begin
  kdic := TKanjidic.Create;
  try
    kdic.Load(OpenTextFile('KANJIDIC', TEUCEncoding));

    writeln(ErrOutput, IntToStr(Yarxi.KanjiCount)+' kanji in DB.');
    writeln(ErrOutput, IntToStr(YarxiCore.Complaints)+' complaints.');
    for k in Yarxi.Kanji do begin
      if (k.Nomer=0) or (k.Kanji='') then continue; //�� ��� ������ ���������
      ke.Reset;
      kentry := kdic.FindEntry(k.Kanji);
      if kentry<>nil then
        ke.Copy(kentry^);

      if k.RusNicks.Count>0 then begin
       //Replace meanings
        ke.meanings.Reset;
        for i := 0 to k.RusNicks.Count-1 do
          ke.AddMeaning(k.RusNicks[i]);
      end;

      for i := 0 to Length(ke.readings)-1 do
        ke.readings[i].Reset;

      ke.readings[0].key := '';
      ke.readings[1].key := 'T1';

      for i := 0 to Length(k.OnYomi)-1 do
        if k.OnYomi[i].rare then
          ke.readings[1].AddOn(k.OnYomi[i].kana)
        else
          ke.readings[0].AddOn(k.OnYomi[i].kana);

      for i := 0 to Length(k.KunYomi.kun)-1 do begin
        kset := @k.KunYomi.kun[i];
        for j := 0 to Length(kset.items)-1 do begin
          if krHidden in kset.items[j].flags then
            continue;
          ke.readings[0].AddKun(kset.items[j].kana);
        end;
      end;

      Output.WriteLn(ComposeKanjidicLine(@ke));
    end;

  finally
    FreeAndNil(kdic);
  end;
end;

begin
  RunApp(TYarxiConvert);
end.
