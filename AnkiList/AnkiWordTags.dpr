program AnkiWordTags;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes, Generics.Collections, ConsoleToolbox, JWBIO, UniStrUtils,
  FilenameUtils;

type
  TAnkiWordTags = class(TCommandLineApp)
  protected
    Files: TFilenameArray;
    OutputFile: string;
    Output: TStreamEncoder;
    Words: TStringList;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string);
  end;

procedure TAnkiWordTags.ShowUsage;
begin
  writeln('Takes a set of text files, each one listing words marked with '
    + 'a certain tag, and generates a table which for every word gives '
    + 'Anki-compatible list of all tags associated with it.');
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
end;

function TAnkiWordTags.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
    Result := inherited;
end;

function TAnkiWordTags.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := s;
  Result := true;
end;

//Anki tag list format: space separated ("area jlpt2")

procedure TAnkiWordTags.Run;
var i: integer;
begin
  if ParamCount<1 then BadUsage();
  if Length(Files)<1 then BadUsage('Input files not specified');

  Words := TStringList.Create;

  Files := ExpandFileMasks(Files, true);
  for i := 0 to Length(Files)-1 do
    ParseFile(Files[i]);

  if OutputFile<>'' then
    Output := UnicodeFileWriter(OutputFile)
  else
    Output := ConsoleWriter;
  Output.WriteBom;

  for i := 0 to Words.Count-1 do
    Output.WriteLn(Words.Names[i]+#09+Words.ValueFromIndex[i]);

  FreeAndNil(Output);
  FreeAndNil(Words);
end;

procedure TAnkiWordTags.ParseFile(const AFilename: string);
var inp: TStreamDecoder;
  tagName, ln, list: string;
  i: integer;
begin
  tagName := ChangeFileExt(ExtractFilename(AFilename), '');
  inp := OpenTextFile(AFilename);
  try
    while inp.ReadLn(ln) do begin
      ln := Trim(ln);
      if ln='' then continue;

     //If reading is provided, accept reading, else use empty reading (for consistence)
      i := pos(' ', ln);
      if i>0 then
        ln[i] := #09
      else
        ln := ln + #09;

      i := Words.IndexOfName(ln);
      if i>=0 then begin
        list := Words.ValueFromIndex[i];
        if pos(' '+ln+' ', ' '+list+' ')<=0 then
          list := list + ' ' + tagName;
      end else
        list := tagName;

      if i>=0 then
        Words.ValueFromIndex[i] := list
      else
        Words.Add(ln+'='+list);
    end;

  finally
    FreeAndNil(inp);
  end;
end;


begin
  RunApp(TAnkiWordTags);
end.
