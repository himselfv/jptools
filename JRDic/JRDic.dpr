program JRDic;
{$APPTYPE CONSOLE}
{
Утилиты для работы с JRDic/web.
}


uses
  SysUtils,
  Classes,
  Windows,
  ActiveX,
  Db,
  AdoDb,
  AdoInt,
  EdictWriter;

type
  EBadUsage = class(Exception)
  end;

procedure BadUsage(msg: UnicodeString);
begin
  raise EBadUsage.Create(msg);
end;

procedure PrintUsage;
begin
  writeln('Usage: ');
  writeln('  '+ExtractFileName(paramstr(0))+'<command>');
  writeln('Supported commands:');
  writeln('  export <filename> = export dictionary to EDICT format');
end;

var
  Command: UnicodeString;
  OutputFile: UnicodeString;

procedure ParseCommandLine;
var i: integer;
  s: UnicodeString;
begin
  Command := '';
  OutputFile := '';

  i := 1;
  while i <= ParamCount do begin
    s := ParamStr(i);
    if Length(s)=0 then begin
      Inc(i);
      continue;
    end;
    if s[1]<>'-' then begin
      if Command='' then
        Command := s
      else
      if OutputFile='' then
        OutputFile := s
      else
        BadUsage('Too many params.');
      Inc(i);
      continue;
    end;

    BadUsage('Unrecognized switch: '+s);
  end;

  if Command='' then
    BadUsage('You have to specify a command');

  if SameStr(Command, 'export') then begin
    if OutputFile='' then
      BadUsage('You have to specify an output file');
  end else
    BadUsage('Unrecognized command: '+Command);

end;

var
  Config: TStringList;

  Db: TAdoConnection;
  tblPrefix: string;

 //Database table names, escaped if needed
  tbl_Words: string;
  tbl_Tls: string;

procedure SetupDb;
begin
  Db := TAdoConnection.Create(nil);
  Db.ConnectionString := Config.Values['ConnectionString'];
  Db.Open();

  tblPrefix := Config.Values['TablePrefix'];
  tbl_Words := '`'+tblPrefix+'words`';
  tbl_Tls := '`'+tblPrefix+'tls`';
end;

procedure FreeDb;
begin
  FreeAndNil(Db);
end;

procedure repl(var s:string;const sub,rep:string);
var i_pos: integer;
begin
  i_pos := pos(sub,s);
  while i_pos>0 do begin
    s:=copy(s,1,i_pos-1)+rep+copy(s,i_pos+length(sub),length(s)-i_pos+1-length(sub));
    i_pos := pos(sub,s);
  end;
end;

{
В запросах можно использовать некоторые спец. слова (см. ниже) - они будут заменены
на имена таблиц.
}
function Query(const cmd: UnicodeString): _Recordset;
var tmp: UnicodeString;
begin
  tmp := cmd;
  repl(tmp, '{$words}', tbl_Words);
  repl(tmp, '{$tls}', tbl_Tls);

  Result := CoRecordset.Create;
  Result.Open(tmp, Db.ConnectionObject, adOpenForwardOnly, adLockReadOnly, adCmdText);
end;

procedure Run_Export(OutputFile: string);
var r: _Recordset;
  art: TEdictArticle;
  wri_jm: TJmDictWriter;
  wri_2: TEdict2Writer;
  wri_1: TEdict1Writer;
  LastId, Id: integer;
begin
  wri_jm := TJmDictWriter.Create(OutputFile+'.jmdict');
  wri_2 := TEdict2Writer.Create(OutputFile+'.edict2');
  wri_1 := TEdict1Writer.Create(OutputFile+'.edict1');

  LastId := -1;

  r := Query('SELECT {$words}.id as id, {$words}.word, tl, is_redirect FROM {$words}, {$tls} '
    +'WHERE {$tls}.word={$words}.id ORDER BY {$words}.id ASC');
  while not r.EOF do begin
    Id := r.Fields[0].Value;
    if LastId<>Id then begin
      if (LastId>0) and (art.senses_used>0) then begin
        wri_jm.Print(@art);
        wri_2.Print(@art);
        wri_1.Print(@art);
      end;
      art.Reset;
      LastId := Id;
      art.AddKanji().k := r.Fields[1].Value;
    end;

    if not boolean(r.Fields[3].Value) then begin
     //Разделения нет, так что все слова регистрируем как glosses одного sense
      if art.senses_used<1 then
        art.AddSense();
      art.senses[0].AddGloss(r.Fields[2].Value);
    end;

    r.MoveNext;
  end;

  if (LastId>0) and (art.senses_used>0) then begin
    wri_jm.Print(@art);
    wri_2.Print(@art);
    wri_1.Print(@art);
  end;


 //Output stats
  writeln('Exported records: '+IntToStr(wri_jm.AddedRecords));

  FreeAndNil(wri_1);
  FreeAndNil(wri_2);
  FreeAndNil(wri_jm);
end;

//Settings have been loaded already
procedure Run;
begin
  CoInitialize(nil);
  Config := TStringList.Create;
  Config.LoadFromFile(ChangeFileExt(ExtractFilename(paramstr(0)), '.cfg'));
  SetupDb;
  try

    if SameStr(Command, 'export') then
      Run_Export(OutputFile)
    else
      BadUsage('Unrecognized command: '+Command);

  finally
    FreeDb;
  end;
end;

begin
  if ParamCount=0 then begin
    PrintUsage;
    exit;
  end;

  try
    ParseCommandLine;
    Run;
  except
    on E: EBadUsage do begin
      writeln('Bad usage. ');
      writeln('  '+E.Message);
      PrintUsage;
    end;
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

