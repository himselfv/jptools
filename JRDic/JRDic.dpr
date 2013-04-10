program JRDic;
{$APPTYPE CONSOLE}
{
Утилиты для работы с JRDic/web.
}


uses
  SysUtils,
  Classes,
  Windows,
  Variants,
  ActiveX,
  Db,
  AdoDb,
  AdoInt,
  EdictWriter,
  JWBDic,
  JWBStrings;

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
  writeln('  autoread <EDICT> = automatically add readings from this EDICT dictionary');
end;

var
  Command: UnicodeString;

  ExportParams: record
    Filename: string;
  end;

  AutoreadParams: record
    DictFilename: string;
  end;

procedure ParseCommandLine;
var i: integer;
  s: string;
begin
  Command := '';

 //Parse
  i := 1;
  while i<=ParamCount() do begin
    s := ParamStr(i);
    if Length(s)<=0 then continue;

   //Command
    if Command='' then begin
      Command := s;

      if Command='export' then begin
        FillChar(ExportParams, sizeof(ExportParams), 0);
      end else
      if Command='autoread' then begin
        FillChar(AutoreadParams, sizeof(AutoreadParams), 0);
      end else
        BadUsage('Invalid command: "'+s+'"');

    end else

   //Non-command non-option params (filename list etc)
    begin
      if Command='export' then begin
        if ExportParams.Filename='' then
          ExportParams.Filename := ParamStr(i)
        else
          BadUsage('Invalid export param: "'+s+'"');
      end else
      if Command='autoread' then begin
        if AutoreadParams.DictFilename='' then
          AutoreadParams.DictFilename := ParamStr(i)
        else
          BadUsage('Invalid autoread param: "'+s+'"');

      end else
        BadUsage('Invalid param: "'+s+'"');

    end;

    Inc(i);
  end; //of ParamStr enumeration

 //Check that post-parsing conditions are met (non-conflicting options etc)
  if Command='' then
    BadUsage('You have to specify a command');
  if Command='export' then begin
    if ExportParams.Filename='' then
      BadUsage('export requires output filename');
  end;
  if Command='autoread' then begin
    if AutoreadParams.DictFilename='' then
      BadUsage('autoread requires input dictionary');
  end;
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
function Query(const cmd: UnicodeString; readonly: boolean = true): _Recordset;
var tmp: UnicodeString;
begin
  tmp := cmd;
  repl(tmp, '{$words}', tbl_Words);
  repl(tmp, '{$tls}', tbl_Tls);

  Result := CoRecordset.Create;
  if readonly then
    Result.Open(tmp, Db.ConnectionObject, adOpenForwardOnly, adLockReadOnly, adCmdText)
  else
    Result.Open(tmp, Db.ConnectionObject, adOpenForwardOnly, adLockOptimistic, adCmdText)
end;

procedure Run_Export(const OutputFile: string);
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

  r := Query('SELECT {$words}.id as id, {$words}.word, {$words}.reading, tl, '
    +'is_redirect FROM {$words}, {$tls} '
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
      if not VarIsNull(r.Fields[2].Value)
      and not (r.Fields[2].Value='') then
        art.AddKana().k := r.Fields[2].Value;
    end;

    if not boolean(r.Fields[4].Value) then begin
     //Разделения нет, так что все слова регистрируем как glosses одного sense
      if art.senses_used<1 then
        art.AddSense();
      art.senses[0].AddGloss(r.Fields[3].Value);
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

procedure Run_Autoread(const DictFile: string);
var edict: TJaletDic;
  cdic: TDicLookupCursor;
  r: _Recordset;
  kj: string;
begin
  edict:=TJaletDic.Create;
  edict.Offline := false;
  edict.LoadOnDemand := false;
  edict.FillInfo(DictFile);
  edict.Load;
  cdic := edict.NewLookup(mtExactMatch);
  try
    r := Query('SELECT id, word, reading FROM {$words} WHERE reading=""', {readonly=}false);
    while not r.EOF do begin
      kj := r.Fields[1].Value;
      if EvalChars(kj) and not (
        (1 shl EC_HIRAGANA)
        or (1 shl EC_KATAKANA)
        or (1 shl EC_IDG_PUNCTUATION)
        or (1 shl EC_LATIN_HW)
        or (1 shl EC_LATIN_FW)
      ) <> 0 then
      begin
        cdic.LookupKanji(kj);
        if cdic.HaveMatch then begin
          r.Fields[2].Value := cdic.GetPhonetic;
          r.Update(EmptyParam, EmptyParam);
        end;
      end;

      r.MoveNext;
    end;
  finally
    FreeAndNil(cdic);
    FreeAndNil(edict);
  end;
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
      Run_Export(ExportParams.Filename)
    else
    if SameStr(Command, 'autoread') then
      Run_Autoread(AutoreadParams.DictFilename)
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

