program AnkiParseTagExport;
{
Parses Anki export file, processes the column with tags and creates a file
for every tag, listing all characters or expressions marked with it.

Moscow    cities russia
New-York  cities
bears     russia
=>
cities.txt
  Moscow
  New-York
russia.txt
  Moscow
  bears

TODO: Flag to update existing lists instead of overwriting (leaves missing words
  intact).
TODO: Flag to add tags from deck to existing tags but not delete the missing ones.
}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes, Generics.Collections, UniStrUtils, ConsoleToolbox, JWBIO;

type
  TOutputMode = (omKanji, omWords);

  TTagFile = class(TStringList)
  protected
    FName: string;
  public
    constructor Create(const AName: string);
    procedure Add(const AText: string); reintroduce;
    property Name: string read FName;
  end;

  TParseTagExport = class(TCommandLineApp)
  protected
    Files: array of string;
    OutputDir: string;
    OutputMode: TOutputMode;
    TagsColumn: integer;
    ExpressionColumn: integer;
    ReadingColumn: integer;
    Tags: TObjectList<TTagFile>;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    function GetTag(const AName: string): TTagFile;
  public
    procedure Init; override;
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string);
  end;

{ area jlpt2 }

procedure TParseTagExport.Init;
begin
  OutputMode := omWords;
  TagsColumn := -1;
  ExpressionColumn := 0;
  ReadingColumn := -1;
end;

procedure TParseTagExport.ShowUsage;
begin
  writeln('Parses Anki export file, processes the column with tags and creates ' +
          'a file for every tag, listing all characters / expressions marked ' +
          'with it.');
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o path\folder    specify output folder (required)');
  writeln('  -mode <mode>      file format to generate:');
  writeln('    kanji           kanji list (characters one after another)');
  writeln('    words           word list (one expression a line)');
  writeln('  -tn column        take tags from this column, zero-based (required)');
  writeln('  -te column        take expressions from this column, zero-based (optional, default = 0)');
  writeln('  -tr column        take readings from this column, zero-based (optional)');
  writeln('If -tr is specified and "-mode words" is being used, each expression ' +
          'will also have reading listed after a space.');
end;

function TParseTagExport.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires target path');
    Inc(i);
    OutputDir := ParamStr(i);
    Result := true;
  end else
  if s='-mode' then begin
    if i>=ParamCount then BadUsage('-o requires target path');
    Inc(i);
    if SameText(ParamStr(i), 'kanji') then
      OutputMode := omKanji
    else
    if SameText(ParamStr(i), 'words') then
      OutputMode := omWords
    else
      BadUsage('Invalid output mode: '+ParamStr(i));
    Result := true;
  end else
  if s='-tn' then begin
    if i>=ParamCount then BadUsage('-tn needs column number');
    Inc(i);
    TagsColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-te' then begin
    if i>=ParamCount then BadUsage('-te needs column number');
    Inc(i);
    ExpressionColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-tr' then begin
    if i>=ParamCount then BadUsage('-tr needs column number');
    Inc(i);
    ReadingColumn := StrToInt(ParamStr(i));
    Result := true
  end else
    Result := inherited;
end;

function TParseTagExport.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := s;
  Result := true;
end;

procedure TParseTagExport.Run;
var i: integer;
begin
  if OutputDir='' then
    BadUsage('Please specify output folder.');
  if TagsColumn < 0 then
    BadUsage('Please specify tag column.');

  Tags := TObjectList<TTagFile>.Create;
  try
    for i := 0 to Length(Files)-1 do
      ParseFile(Files[i]);

    ForceDirectories(OutputDir);
    for i := 0 to Tags.Count-1 do begin
      if OutputMode=omKanji then //pack
        Tags[i].Text := Tags[i].Text.Replace(#13,'').Replace(#10,'');
      Tags[i].SaveToFile(OutputDir+'\'+Tags[i].Name+'.txt', SysUtils.TEncoding.UTF8);
    end;
  finally
    FreeAndNil(Tags);
  end;
end;

procedure TParseTagExport.ParseFile(const AFilename: string);
var inp: TStreamDecoder;
  ln: string;
  key: string;
  parts: TStringArray;
  tags: TStringArray;
  i: integer;
begin
  inp := OpenTextFile(AFilename);
  try
    while inp.ReadLn(ln) do begin
      parts := StrSplit(PWideChar(ln),#09);
      if ExpressionColumn>=Length(parts) then
        continue;
      if TagsColumn>=Length(parts) then
        continue;

      key := Trim(parts[ExpressionColumn]);
      if (OutputMode=omWords) and (ReadingColumn>=0)
      and (ReadingColumn<>ExpressionColumn) and (ReadingColumn<Length(parts)) then
        key := key + ' ' + Trim(parts[ReadingColumn]);

      tags := StrSplit(PChar(Trim(parts[TagsColumn])), ' ');
      for i := 0 to Length(tags)-1 do
        GetTag(tags[i]).Add(key);
    end;
  finally
    FreeAndNil(inp);
  end;
end;

function TParseTagExport.GetTag(const AName: string): TTagFile;
var i: integer;
begin
  Result := nil;
  for i := 0 to Tags.Count-1 do
    if SameText(Tags[i].Name, AName) then begin
      Result := Tags[i];
      break;
    end;

  if Result=nil then begin
    Result := TTagFile.Create(AName.ToLower);
    Tags.Add(Result);
  end;
end;

constructor TTagFile.Create(const AName: string);
begin
  inherited Create;
  Self.Sorted := true; //speed up lookups
  FName := AName;
end;

//Adds unique
procedure TTagFile.Add(const AText: string);
begin
  if Self.IndexOf(AText)<0 then
    inherited Add(AText);
end;


begin
  RunApp(TParseTagExport);
end.

