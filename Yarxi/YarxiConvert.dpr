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
  Yarxi, YarxiFmt;

type
  TYarxiConvert = class(TCommandLineApp)
  protected
    Command: string;
    Field: string;
    Yarxi: TYarxiDB;
    Output: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure RunKanji(const field: string);
    procedure RunTango;
  end;

procedure TYarxiConvert.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <command> <field>');
  writeln('Supported commands:');
  writeln('  kanji = print kanji table fields');
  writeln('  tango = print tango table');
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
  Yarxi.KanaTran.LoadFromFile('yarxi.kcs');

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
      Output.WriteLn(Yarxi.KanaTran.RomajiToKana('K'+k.JoinOns(' '), []))
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
      raise Exception.Create('Unknown field');
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
