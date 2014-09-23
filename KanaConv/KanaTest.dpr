program KanaTest;
{$APPTYPE CONSOLE}
{ Service utility for KanaConv conversion tables. }

uses
  SysUtils, ConsoleToolbox, BalancedTree, JWBKanaConv, JWBIO, StreamUtils;

type
  TConvertType = (ctKana, ctRomaji);
  TKanaTest = class(TCommandLineApp)
  protected
    Command: string;
    TableFiles: array of string;
    InputFiles: array of string;
    OutputFile: string;
    AConv: TRomajiTranslator;
    AFlags: TResolveFlags;
    AOutput: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    procedure LoadTables();
    procedure ConvertFile(AInput: TStreamDecoder; AType: TConvertType);
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure RunInfo();
    procedure RunConvert(AType: TConvertType);
  end;


procedure TKanaTest.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <command> <table1> [table2] ... [-flags]');
  writeln('Commands:');
  writeln('   info             diagnostics for the resulting conversion table');
  writeln('   tokana           convert input romaji to kana');
  writeln('   toroma           convert input kana to romaji');
  writeln('   tobopomofo       convert input pinyin to bopomofo');
  writeln('   topinyin         convert input bopomofo to pinyin');
  writeln('Flags:');
  writeln('  -i input.file     add input file (otherwise console)');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -d                delete invalid chars')
end;

function TKanaTest.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-i' then begin
    if i>=ParamCount then BadUsage('-i requires file name');
    Inc(i);
    SetLength(InputFiles, Length(InputFiles)+1);
    InputFiles[Length(InputFiles)-1] := ParamStr(i);
    Result := true;
  end else
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-d' then begin
    AFlags := AFlags + [rfDeleteInvalidChars];
    Result := true;
  end else
    Result := inherited;
end;

function TKanaTest.HandleParam(const s: string; var i: integer): boolean;
begin
  if (Command='') and not FileExists(s) then
    Command := s
  else begin
    SetLength(TableFiles, Length(TableFiles)+1);
    TableFiles[Length(TableFiles)-1] := s;
  end;
  Result := true;
end;

procedure TKanaTest.Run;
begin
  if Command='' then Command := 'info';
  if Length(TableFiles)<=0 then BadUsage('Please specify one or more conversion tables.');

  if OutputFile<>'' then
    AOutput := FileWriter(OutputFile)
  else
    AOutput := ConsoleWriter();

  if Command='info' then begin
    AConv := TKanaTranslator.Create;
    LoadTables;
    RunInfo();
  end else
  if Command='tokana' then begin
    AConv := TKanaTranslator.Create;
    LoadTables;
    RunConvert(ctKana);
  end else
  if Command='toroma' then begin
    AConv := TKanaTranslator.Create;
    LoadTables;
    RunConvert(ctRomaji);
  end else
  if Command='tobopomofo' then begin
    AConv := TKanaTranslator.Create;
    LoadTables;
    RunConvert(ctKana);
  end else
  if Command='topinyin' then begin
    AConv := TPinyinTranslator.Create;
    LoadTables;
    RunConvert(ctRomaji);
  end else
    BadUsage('Unsupported command: '+Command);

  FreeAndNil(AOutput);
end;

procedure TKanaTest.LoadTables();
var i: integer;
begin
  for i := 0 to Length(TableFiles)-1 do
    AConv.LoadFromFile(TableFiles[i]);
end;

type
  TRomajiTranslatorHack = class(TRomajiTranslator);
  TKanaTranslatorHack = class(TKanaTranslator);
  TPinyinTranslatorHack = class(TPinyinTranslator);
  TTranslationTableHack = class(TTranslationTable);

procedure TKanaTest.RunInfo;
var i: integer;
  kana: array of record
    printed_as: string;
    input_as: string;
  end;
  tt: TTranslationTableHack;
  bi: TBinTreeItem;
  ri: TRomajiIndexEntry;
  tmp_s: string;
begin
  SetLength(kana, 0);
  with TRomajiTranslatorHack(AConv) do begin
    tt := TTranslationTableHack(FTrans);
    AOutput.writeln(IntToStr(FTablesLoaded) + ' conversion tables loaded.');
    AOutput.writeln(IntToStr(FTrans.Count) + ' conversion rules.');
    AOutput.writeln(IntToStr(FReplKtr.Count) + ' K->R replacement rules.');
    AOutput.writeln(IntToStr(FReplRtk.Count) + ' R->K replacement rules.');

    SetLength(kana, tt.Count);
    for i := 0 to tt.Count-1 do begin
      kana[i].input_as := ''; //no input romaji found yet
      if Length(tt[i].Romaji)<=0 then begin
        AOutput.writeln(tt[i].Phonetic + ': no romaji');
        kana[i].printed_as := '';
      end else
        kana[i].printed_as := tt[i].Romaji[0];
    end;

   //For all romaji
    for bi in tt.FRomajiIndex do begin
      ri := TRomajiIndexEntry(bi);
      if Length(ri.entries)<=0 then
        AOutput.writeln(ri.roma+': no kana (wtf?)');

      if Length(ri.entries)>1 then begin
        tmp_s := ri.entries[0].Phonetic;
        for i := 1 to Length(ri.entries)-1 do
          tmp_s := tmp_s + ', ' + ri.entries[i].Phonetic;
        AOutput.writeln(ri.roma+': multiple kana matches: '+tmp_s)
      end;

      i := tt.IndexOf(ri.entries[0]);
      if i>=0 then begin
       //Remember either the first or the best printing option
        if (kana[i].input_as='') or (kana[i].printed_as=ri.roma) then
          kana[i].input_as := ri.roma;
      end;
    end;

    for i := 0 to Length(kana)-1 do begin
      if kana[i].input_as='' then
        AOutput.writeln(tt[i].Phonetic+': no way to input')
      else
      if kana[i].input_as<>kana[i].printed_as then
        AOutput.writeln(tt[i].Phonetic+': input as "'+kana[i].input_as+'" but printed as "'+kana[i].printed_as+'"');
    end;
  end;
end;

procedure TKanaTest.RunConvert(AType: TConvertType);
var i: integer;
begin
  if Length(InputFiles)<=0 then
    ConvertFile(ConsoleReader(), AType)
  else
    for i := 0 to Length(InputFiles)-1 do
      ConvertFile(FileReader(InputFiles[i]), AType);
end;

//Destroys AInput
procedure TKanaTest.ConvertFile(AInput: TStreamDecoder; AType: TConvertType);
var ln: string;
begin
  while AInput.ReadLn(ln) do begin
    case AType of
      ctKana: ln := AConv.RomajiToKana(ln, AFlags);
      ctRomaji: ln := AConv.KanaToRomaji(ln, AFlags);
    end;
    AOutput.WriteLn(ln);
  end;
  FreeAndNil(AInput);
end;


begin
  RunApp(TKanaTest);
end.
