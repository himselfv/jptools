program KanjiList;
{
Utilities for working with kanji list files, produced by KanjiStats or by other
means.
Supports basic "ordered bag of kanji", "ordered one-kanji-one-line" and
"ordered kanji=value" formats.
}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, ConsoleToolbox, StrUtils, UniStrUtils, StreamUtils, JWBIO,
  FilenameUtils;

type
  TKanjiList = class(TCommandLineApp)
  protected
    Command: UniString;
    IgnoreConsoleInput: boolean;
    InputFilenames: TFilenameArray;
    ScanSubdirs: boolean;
    OutputFile: UniString;
    ClipCount: integer;
    function HandleParam(const s: string; var i: integer): boolean; override;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure DoCount(inp: TStreamDecoder; outp: TStreamEncoder);
    procedure DoClip(inp: TStreamDecoder; outp: TStreamEncoder; AClipCount: integer);
    procedure DoSkip(inp: TStreamDecoder; outp: TStreamEncoder; AClipCount: integer);
    procedure DoPack(inp: TStreamDecoder; outp: TStreamEncoder);
    procedure DoMerge(inp: TStreamDecoder; outp: TStreamEncoder);
    procedure DoFilter(inp: TStreamDecoder; outp: TStreamEncoder);
    procedure DoDiff(inp: TStreamDecoder; outp: TStreamEncoder);
    procedure DoOrderOf(inp: TStreamDecoder; outp: TStreamEncoder);

  end;

procedure TKanjiList.ShowUsage;
begin
  writeln('Usage: ');
  writeln('  '+ProgramName+'<command> [additional files] [flags]');
  writeln('Takes a list of input files, performs some transformations and outputs the result');
  writeln('Supported input files:');
  writeln('  ordered bag of kanji');
  writeln('  ordered one-kanji-one-line');
  writeln('  ordered kanji=value');
  writeln('Commands:');
  writeln('  count = output number of entries');
  writeln('  clip <N/-N> = leaves only N first / all except N last kanji');
  writeln('  skip <N/-N> = removes N first / all except N last kanji');
  writeln('  pack = converts any other format to ordered bag of kanji');
  writeln('  merge = produces a union of unique kanji from all files');
  writeln('  filter = produces a union of kanji present in all files');
  writeln('  diff = produces a union of all kanji from input missing in any additional files');
  writeln('  orderof = produces a kanji-value file assigning each kanji its position number in the input file');
  writeln('Unsupported commands:');
  writeln('  trim = removes empty lines');
  writeln('  add <file1> [file2] [...] = joins several files without deduplication');
  writeln('     Simply use common text tools.');
  writeln('Flags:');
  writeln('  -ii               ignore console input, data is to be taken from the first additional file');
  writeln('  -o <output file>  where to output the data, otherwise console');
  writeln('  -s                scan subdirectories too');
end;

function TKanjiList.HandleParam(const s: string; var i: integer): boolean;
begin
  Result := true;
  if Command='' then begin
    Command := s;
    if (Command='clip') or (Command='skip') then begin
      Inc(i);
      if (i=ParamCount) or not TryStrToInt(ParamStr(i), ClipCount) then
        BadUsage(Command+' requires kanji count');
    end;
  end else
    AddFile(InputFilenames, s);
end;

function TKanjiList.HandleSwitch(const s: string; var i: integer): boolean;
begin
  Result := false;

  if SameText(s, '-ii') then begin
    IgnoreConsoleInput := true;
    Result := true;
  end else

  if SameText(s, '-o') then begin
    if i=ParamCount then
      BadUsage('-o requires output file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else

  if SameText(s, '-s') then begin
    ScanSubdirs := true;
    Result := true;
  end;

end;

procedure TKanjiList.Run;
var inp: TStreamDecoder;
  outp: TStreamEncoder;
begin
  if Command='' then BadUsage;

  InputFilenames := ExpandFileMasks(InputFilenames, ScanSubdirs);

  if IgnoreConsoleInput then begin
    if Length(InputFilenames)<1 then
      BadUsage('You have to specify an input file');
    inp := OpenTextFile(InputFilenames[0]);
    InputFilenames := Copy(InputFilenames, 1, MaxInt);
  end else
    inp := ConsoleReader();

  if OutputFile<>'' then begin
    outp := CreateTextFile(OutputFile, TUTF16Encoding);
    outp.WriteBom;
  end else
    outp := ConsoleWriter();

  if (Length(InputFilenames) > 0) and (Command <> 'merge') and (Command <> 'filter')
  and (Command <> 'diff') then
    BadUsage('Too many input files for this command.');

  try
    if Command='count' then
      Self.DoCount(inp, outp)
    else
    if Command='clip' then
      Self.DoClip(inp, outp, ClipCount)
    else
    if Command='skip' then
      Self.DoSkip(inp, outp, ClipCount)
    else
    if Command='pack' then
      Self.DoPack(inp, outp)
    else
    if Command='merge' then
      Self.DoMerge(inp, outp)
    else
    if Command='filter' then
      Self.DoFilter(inp, outp)
    else
    if Command='diff' then
      Self.DoDiff(inp, outp)
    else
    if Command='orderof' then
      Self.DoOrderOf(inp, outp)
    else
      BadUsage('Unrecognized command: '+Command);
  finally
    FreeAndNil(inp);
    FreeAndNil(outp);
  end;
end;

//Detects if a given file is a bag-of-kanji or line-by-line schema
function IsBagOfKanji(const AData: string): boolean; inline;
begin
 //Simply checking for #13 would give wrong results on a singe-entry line-by-line file.
 //   kanji=asd
  Result := (pos(#13, AData) <= 0) and (pos('=', AData) <= 0);
end;

function SplitLineByLine(const AData: string): TStringArray; inline;
begin
  Result := SepSplit(ReplaceStr(AData, #10, ''), #13);
end;

function JoinLineByLine(const ALines: TStringArray): string; inline;
var i: integer;
begin
  if Length(ALines) <= 0 then
    Result := ''
  else begin
    Result := ALines[0];
    for i := 1 to Length(ALines)-1 do
      Result := Result + #13#10 + ALines[i];
  end;
end;

procedure TKanjiList.DoCount(inp: TStreamDecoder; outp: TStreamEncoder);
var data: string;
  lines: TStringArray;
begin
  data := inp.ReadAll();
  if IsBagOfKanji(data) then
    outp.WriteLn(IntToStr(Length(data)))
  else begin
    lines := SplitLineByLine(data);
    outp.WriteLn(IntToStr(Length(lines)));
  end;
end;

//Leaves only N first / all except N last entries
procedure TKanjiList.DoClip(inp: TStreamDecoder; outp: TStreamEncoder; AClipCount: integer);
var data: string;
  lines: TStringArray;
begin
  data := inp.ReadAll();
  if IsBagOfKanji(data) then begin
    if AClipCount >= 0 then begin
      if AClipCount < Length(data) then
        SetLength(data, AClipCount);
    end else begin
      if -AClipCount < Length(data) then
        SetLength(data, Length(data) - (-AClipCount))
      else
        SetLength(data, 0);
    end;
    outp.Write(data);
  end else begin
    lines := SplitLineByLine(data);
    if AClipCount >= 0 then begin
      if AClipCount < Length(lines) then
        SetLength(lines, AClipCount);
    end else begin
      if -AClipCount < Length(lines) then
        SetLength(lines, Length(lines) - (-AClipCount))
      else
        SetLength(lines, 0);
    end;
    data := JoinLineByLine(lines);
    outp.Write(data);
  end;
end;

//Removes N first / all except N last entries
procedure TKanjiList.DoSkip(inp: TStreamDecoder; outp: TStreamEncoder; AClipCount: integer);
var data: string;
  lines: TStringArray;
begin
  data := inp.ReadAll();
  if IsBagOfKanji(data) then begin
    if AClipCount >= 0 then
      Delete(data, 1, AClipCount) //strings are 1-counted
    else begin
      if -AClipCount < Length(data) then
        Delete(data, 1, Length(data) - (-AClipCount));
    end;
    outp.Write(data);
  end else begin
    lines := SplitLineByLine(data);
    if AClipCount >= 0 then
      lines := Copy(lines, AClipCount, MaxInt)
    else begin
      if -AClipCount < Length(lines) then
        lines := Copy(lines, Length(lines) - (-AClipCount), MaxInt);
    end;
    data := JoinLineByLine(lines);
    outp.Write(data);
  end;
end;

function ToBagOfKanji(const AData: string): string;
var lines: TStringArray;
  line: string;
begin
  Result := '';
  lines := SplitLineByLine(AData);
  for line in lines do
    if Length(line) > 0 then
      Result := Result + line[1];
end;

procedure TKanjiList.DoPack(inp: TStreamDecoder; outp: TStreamEncoder);
var data: string;
begin
  data := inp.ReadAll();
  if IsBagOfKanji(data) then
    outp.Write(data)
  else
    outp.Write(ToBagOfKanji(data));
end;

procedure TKanjiList.DoMerge(inp: TStreamDecoder; outp: TStreamEncoder);
var data: string;
  fname: string;
  f: TStreamDecoder;
  fdata: string;
  ch: char;
begin
  data := inp.ReadAll();
 //What if main file is kanji=value, but others are bag-of-kanji? How do we
 //add new kanji to it?
 //So for simplicity, force everything to bag-of-kanji here.
  if not IsBagOfKanji(data) then
    data := ToBagOfKanji(data);

  for fname in InputFilenames do begin
    f := OpenTextFile(fname);
    try
      fdata := f.ReadAll;
      if not IsBagOfKanji(fdata) then
        fdata := ToBagOfKanji(fdata);
      for ch in fdata do
        if pos(ch, data) <= 0 then
          data := data + ch;
    finally
      FreeAndNil(f);
    end;
  end;

  outp.Write(data);
end;

//Prints kanji from inp which are present in all secondary files
procedure TKanjiList.DoFilter(inp: TStreamDecoder; outp: TStreamEncoder);
var data: string;
  lines: TStringArray;
  haveLines: boolean;
  fname: string;
  f: TStreamDecoder;
  fdata: string;
  i, offs: integer;
begin
  data := inp.ReadAll();
  if not IsBagOfKanji(data) then begin
    lines := SplitLineByLine(data);
    haveLines := true;
  end else
    haveLines := false;

  for fname in InputFilenames do begin
    f := OpenTextFile(fname);
    try
      fdata := f.ReadAll;
     //Since we cannot add anything to main file in this function,
     //secondary lists details are not important
      if not IsBagOfKanji(fdata) then
        fdata := ToBagOfKanji(fdata);

      if haveLines then begin

        for i := 0 to Length(lines)-1 do
          if (lines[i]<>'') and (pos(lines[i][1], fdata) <= 0) then
            lines[i] := '';

      end else begin

        i := 1;
        while i <= Length(data) do
          if pos(data[i], fdata) <= 0 then
            delete(data, i, 1)
          else
            Inc(i);

      end;

    finally
      FreeAndNil(f);
    end;
  end;

  if haveLines then begin
   //Remove all empty or emptied lines
    i := 0;
    offs := 0;
    while i+offs < Length(lines) do
      if lines[i+offs]='' then
        Inc(offs)
      else begin
        if offs > 0 then
          lines[i] := lines[i+offs];
        Inc(i);
      end;
    SetLength(lines, i);

    data := JoinLineByLine(lines);
  end;

  outp.Write(data);
end;

//Prints kanji from inp which are missing from any secondary file
procedure TKanjiList.DoDiff(inp: TStreamDecoder; outp: TStreamEncoder);
var data: string;
  fname: string;
  f: TStreamDecoder;
  fdata: string;
  i,offs: integer;
  missing: array of byte;
begin
  data := inp.ReadAll();
 //For simplicity, force everything to bag-of-kanji here.
  if not IsBagOfKanji(data) then
    data := ToBagOfKanji(data);

  SetLength(missing, Length(data));
  for i := 0 to Length(missing)-1 do
    missing[i] := 0;

  for fname in InputFilenames do begin
    f := OpenTextFile(fname);
    try
      fdata := f.ReadAll;
     //Since we cannot add anything to main file in this function,
     //secondary lists details are not important
      if not IsBagOfKanji(fdata) then
        fdata := ToBagOfKanji(fdata);
      for i := 1 to Length(data) do
        if pos(data[i], fdata) <= 0 then
          Inc(missing[i-1]);
    finally
      FreeAndNil(f);
    end;
  end;

 //Remove any marked kanji from data
  i := 1;
  offs := 0;
  while i+offs <= Length(data) do
    if missing[i+offs-1] <= 0 then
      Inc(offs)
    else begin
      if offs > 0 then
        data[i] := data[i+offs];
      Inc(i);
    end;
  SetLength(data, i-1);

  outp.Write(data);
end;

{ Assigns each kanji its position number in the input file explicitly.
E.g. input:
  a=unrelated_data
  b=more_data
Output:
  a=1
  b=2
Useful when you need to pass kanji order somewhere explicitly }
procedure TKanjiList.DoOrderOf(inp: TStreamDecoder; outp: TStreamEncoder);
var data: string;
  i: integer;
begin
  data := inp.ReadAll();
 //We only care about order
  if not IsBagOfKanji(data) then
    data := ToBagOfKanji(data);

  for i := 1 to Length(data) do
    outp.WriteLn(data[i]+'='+IntToStr(i));
end;

begin
  RunApp(TKanjiList);
end.

