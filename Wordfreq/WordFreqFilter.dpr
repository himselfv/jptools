program WordFreqFilter;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  UniStrUtils,
  ConsoleToolbox,
  JWBIO,
  FastArray,
  Edict;

type
  TWordFreqFilter = class(TCommandLineApp)
  protected
    WordfreqFile: string;
    DictFiles: TArray<string>;
    Dicts: TArray<TEdict>;
    Output: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
  end;

procedure TWordFreqFilter.ShowUsage;
begin
  writeln(ErrOutput, 'Filters wordfreq_ck-formatted file, leaving only those '
    +'records which match entries in the dictionaries.');
  writeln(ErrOutput, 'Usage: '+ProgramName+' <wordfreq_ck> [/d <edict>] [/d ...]');
  writeln(ErrOutput, '  /d    = specify dictionary (by default edict/edict2)');
end;

function TWordFreqFilter.HandleSwitch(const s: string; var i: integer): boolean;
begin
  Result := true;
  if s='/d' then begin
    Inc(i);
    if i>ParamCount() then
      BadUsage('/d requires dictionary file name');
    DictFiles.Add(ParamStr(i));
  end else
    Result := inherited;
end;

function TWordFreqFilter.HandleParam(const s: string; var i: integer): boolean;
begin
  if WordfreqFile='' then
    WordfreqFile := s
  else
    BadUsage('Two or more wordfreq files cannot be specified');
  Result := true;
end;

procedure TWordFreqFilter.Run;
var inp: TStreamDecoder;
  ln: string;
  i: integer;
  dic: TEdict;
  word: string;
  found: boolean;
begin
  if WordfreqFile='' then
    BadUsage();
  if DictFiles.Count<=0 then
    BadUsage();
  Output := ConsoleWriter();

 //Load dictionaries
  for i := 0 to DictFiles.Count-1 do begin
    dic := TEdict.Create;
    dic.LoadFromFile(DictFiles[i]);
    Dicts.Add(dic);
  end;

 //Parse
  inp := OpenTextFile(WordfreqFile, nil);
  try
    while inp.ReadLn(ln) do begin
      if (ln='') or (ln[1]='#') then begin
        Output.WriteLn(ln);
        continue;
      end;

      i := ln.IndexOf(#9);
      if i<=0 then
        raise Exception.Create('Invalid line: '+ln);

      word := copy(ln, 1, i);

      found := false;
      for i := 0 to Dicts.Count-1 do
        if Dicts[i].FindEntry(word)<>nil then begin
          found := true;
          break;
        end;

      if found then
        Output.WriteLn(ln);
    end;
  finally
    FreeAndNil(inp);
  end;
end;

begin
  RunApp(TWordFreqFilter);
end.
