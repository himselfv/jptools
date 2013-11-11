program RadGen;
{ Parses a list of kanji and lists all parts of characters used in each.
 Uses:
   wakan.rad    as Raine radical table
 And either:
   wakan.chr          for radical names
   wakan-types.cfg    for property type details in wakan.chr
 Or:
   yarxi.db     for radical names }

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes, ConsoleToolbox, JwbStrings, JwbIo, JwbCharData, JwbRadical, TextTable;

type
  TRadGen = class(TCommandLineApp)
  protected
    Files: array of string;
    OutputFile: UnicodeString;
    DescriptionCount: integer;
    Output: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  protected //Wakan.chr
    Chars: TTextTableCursor;
    CharProps: TCharPropertyCursor;
    procedure WakanInit;
    procedure WakanFree;
    procedure WakanLoadTypes(const AFilename: string);
    function WakanGetDesc(const AChar: char): TStringArray;
  public
    procedure Init; override;
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string);
  end;

procedure TRadGen.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -t 0/1/2          paste up to this number of descriptions (default is 1)');
end;

procedure TRadGen.Init;
begin
  DescriptionCount := 1;
end;

function TRadGen.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-t' then begin
    if i>=ParamCount then BadUsage('-t requires description count');
    Inc(i);
    DescriptionCount := StrToInt(ParamStr(i));
    Result := true;
  end else
    Result := inherited;
end;

function TRadGen.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := ParamStr(i);
  Result := true;
end;

procedure TRadGen.Run;
var AFile: string;
begin
  LoadRaineRadicals('WAKAN.RAD');
  WakanInit;

  if OutputFile<>'' then
    Output := UnicodeFileWriter(OutputFile)
  else
    Output := ConsoleWriter;
  Output.WriteBom;

  for AFile in Files do
    ParseFile(AFile);

  FreeAndNil(Output);
  WakanFree;
end;

function JoinStr(const AParts: TStringArray; const ASep: string = ', '): string;
var i: integer;
begin
  if Length(AParts)<=0 then
    Result := ''
  else
  if Length(AParts)=1 then
    Result := AParts[0]
  else begin
    Result := AParts[0];
    for i := 1 to Length(AParts) do
      Result := Result + ASep + AParts[i];
  end;
end;

procedure TRadGen.ParseFile(const AFilename: string);
var inp: TStreamDecoder;
  i: integer;
  ln: string;
  rads: string; //radical list
  expl: string; //radicals with explanations
  desc: TStringArray;
begin
  inp := FileReader(AFilename);
  while inp.ReadLn(ln) do begin
    ln := Trim(ln);
    if ln='' then continue;
    rads := RaineRadicals.GetCharRadicals(ln[1]);

    expl := '';
    for i := 1 to Length(rads) do begin
      desc := WakanGetDesc(rads[i]);
      if Length(desc)>DescriptionCount then
        SetLength(desc, DescriptionCount);

      if Length(desc)<=0 then
        expl := expl + rads[i] + ', '
      else
        expl := expl + rads[i] + ' (' + JoinStr(desc,', ')+ '), ';
    end;
    SetLength(expl, Length(expl)-2);

    Output.WriteLn(ln+#09+expl);
  end;
  FreeAndNil(inp);
end;

procedure TRadGen.WakanInit;
begin
  LoadCharData('wakan.chr');
  WakanLoadTypes('wakan-types.cfg');
  Chars := TTextTableCursor.Create(TChar);
  CharProps := TCharPropertyCursor.Create(TCharProp);
end;

procedure TRadGen.WakanFree;
begin
  FreeAndNil(CharProps);
  FreeAndNil(Chars);
end;

procedure TRadGen.WakanLoadTypes(const AFilename: string);
var sl: TStringList;
  line: string;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    for line in sl do
      AddCharPropType(line);
  finally
    FreeAndNil(sl);
  end;
end;

function TRadGen.WakanGetDesc(const AChar: char): TStringArray;
var tmp: string;
begin
  if not Chars.Locate('Unicode',AChar) then begin
    SetLength(Result,0);
    exit;
  end;

  tmp := CharProps.GetJapaneseDefinitions(Chars.Int(TCharIndex), #09);
  if tmp='' then
    SetLength(Result,0)
  else
    Result := SplitStr(tmp,#09);
end;

begin
  RunApp(TRadGen);
end.
