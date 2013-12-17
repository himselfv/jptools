unit WcExceptions;
{ Exception/warning logging routines for complicated cases where you have lots
 of parsing and lots of exceptions }

{$DEFINE JCLDEBUG}
{ Use JclDebug to add stack traces where appropriate. Requires detailed map file. }

interface
uses SysUtils, Generics.Defaults, Generics.Collections{$IFDEF JCLDEBUG}, JclDebug{$ENDIF};

{
Raise exceptions, catch them and pass to RegisterExeception.
At the end of the app call Print() to get:
EThisException:
  Message 1              213
  Message 2              7
EThatException
  Message 4              711
}

type
  TExceptionStatRecord = record
    _type: TClass;
    msg: string;
    count: integer;
  end;
  PExceptionStatRecord = ^TExceptionStatRecord;

  TExceptionStats = class(TList<PExceptionStatRecord>)
  protected
    function AddNew(E: Exception): PExceptionStatRecord;
    function Find(E: Exception): PExceptionStatRecord;
    procedure Notify(const Item: PExceptionStatRecord; Action: TCollectionNotification); override;
  protected
    function CountTotal: integer;
    function CountTypeTotal(AType: TClass; idx: integer = 0): integer;
  public
    procedure RegisterException(E: Exception);
    procedure PrintStats;
  end;

{ Default instance, but you may create private ones if you need granularity }
var
  ExceptionStats: TExceptionStats;

{ Shortcut functions }

procedure Warning(const msg: string); overload;
procedure Check(ACondition: boolean; const AErrorText: string = ''); overload; inline;
procedure Check(ACondition: boolean; const AErrorText: string; AArgs: array of const); overload;
procedure Die(const AErrorText: string = ''); overload; inline;
procedure Die(const AErrorText: string; AArgs: array of const); overload;

implementation

procedure TExceptionStats.Notify(const Item: PExceptionStatRecord; Action: TCollectionNotification);
begin
  case Action of
    cnRemoved: Dispose(Item);
  end;
end;

function TExceptionStats.AddNew(E: Exception): PExceptionStatRecord;
begin
  New(Result);
  Result._type := E.ClassType;
  Result.msg := E.Message;
  Result.count := 0;
  Add(Result);
end;

function TExceptionStats.Find(E: Exception): PExceptionStatRecord;
var i: integer;
begin
  Result := nil;
  for i := 0 to Self.Count - 1 do
    if (items[i]._type=E.ClassType)
    and (AnsiSameText(items[i].msg, E.Message)) then begin
      Result := items[i];
      break;
    end;
end;

procedure TExceptionStats.RegisterException(E: Exception);
var pr: PExceptionStatRecord;
begin
  pr := Find(E);
  if pr=nil then
    pr := AddNew(E);
  Inc(pr.count);
end;

//Pre-calculates the total count
function TExceptionStats.CountTotal: integer;
var idx: integer;
begin
  Result := 0;
  idx := 0;
  while idx<Self.Count do begin
    Inc(Result, items[idx].count);
    Inc(idx);
  end;
end;

//Calcualtes the total count for AType exception type
//idx is the starting index. Items have to be sorted by types.
function TExceptionStats.CountTypeTotal(AType: TClass; idx: integer = 0): integer;
begin
  Result := 0;
  while (idx<Self.Count) and (items[idx]._type=AType) do begin
    Inc(Result, items[idx].count);
    Inc(idx);
  end;
end;

{ Prints ln+ln_val, while padding ln_val to the end of the screen }
procedure PrintPadded(const ln, ln_val: string);
var tmp_ln: string;
begin
  tmp_ln := ln;
  while (Length(tmp_ln)+Length(ln_val)) mod 80 < 78 do
    tmp_ln := tmp_ln + ' '; //pad until ln_val ends at 78-th char
  writeln(tmp_ln + ln_val);
end;

procedure TExceptionStats.PrintStats;
var lastType: TClass;
  total: integer;
  i: integer;

  procedure FinalizeLastType;
  begin
   // writeln('');
  end;

begin
  Sort(TComparer<PExceptionStatRecord>.Construct(
    function(const Left, Right: PExceptionStatRecord): Integer
    begin
      Result := integer(Left._type) - integer(Right._type);
      if Result=0 then
        Result := AnsiCompareText(Left.msg, Right.msg);
    end
  ));

  total := CountTotal;
  if total=0 then exit;
  PrintPadded('Errors:',IntToStr(total));

  lastType := nil;
  for i := 0 to Self.Count - 1 do begin
    if lastType<>items[i]._type then begin
      FinalizeLastType;
      lastType := items[i]._type;
      total := CountTypeTotal(lastType,i);
      if total>0 then begin //else this whole section is empty
       //If this type has only one record, don't print total -- it's information duplication
        if (i>=Self.Count-1) or (items[i+1]._type<>lastType) then
          writeln(lastType.ClassName)
        else
          PrintPadded(lastType.ClassName, IntToStr(total));
      end;
    end;

    if items[i].count<=0 then continue; //nothing to print
    PrintPadded('  '+items[i].msg, IntToStr(items[i].count));
  end;
  FinalizeLastType;
end;


{ Messages }

procedure Warning(const msg: string);
{$IFDEF JCLDEBUG}
var info: TJclLocationInfo;
begin
  info := GetLocationInfo(Caller(1));
  writeln(ErrOutput, info.ProcedureName+':'+IntToStr(info.LineNumber)+': '+msg)
end;
{$ELSE}
begin
  writeln(ErrOutput, +msg);
end;
{$ENDIF}

procedure Check(ACondition: boolean; const AErrorText: string = '');
begin
  if not ACondition then
    Die(AErrorText);
end;

procedure Check(ACondition: boolean; const AErrorText: string; AArgs: array of const);
begin
  if not ACondition then
    Die(AErrorText, AArgs);
end;

procedure Die(const AErrorText: string = '');
begin
  raise Exception.Create(AErrorText);
end;

procedure Die(const AErrorText: string; AArgs: array of const);
begin
  raise Exception.CreateFmt(AErrorText, AArgs);
end;


initialization
  ExceptionStats := TExceptionStats.Create()

finalization
  FreeAndNil(ExceptionStats);

end.
