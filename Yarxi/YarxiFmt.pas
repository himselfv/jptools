unit YarxiFmt;
{ �������, ������������ � ���� ������ �����. ����� ������� ���������, ��� �����
 ��� ������ � �����. }

{$DEFINE STRICT}
{ ��������� ������ �� ��������� � �������, ������� ������������� �����������
 � ���� �����. �������������.
 ��� ����� ������ ��������� ���� �������� � �������. }

interface
uses SysUtils, Classes, UniStrUtils, FastArray;

{ �������� ������� ��� ������ �� �������� }

function pop(var s: string; const sep: char): string;
function ifpop(var s: string; const sep: char): string;
function spancopy(ps, pe: PChar): string;
function Split(const s: string; const sep: char): TStringArray; inline;
function Unquote(const s: string; op, ed: char): string;
function TryUnquote(var s: string; op, ed: char): boolean;

{ ������� �������� ���� ������ �� �����. � ���������� ���� ������� ����������
 ������� �����, ��� � WarodaiConvert. }

procedure Complain(msg: string); overload;
procedure Complain(source, msg: string); overload;
procedure Complain(source, msg, data: string); overload;


{ �� ���� ����� ������������ "��������� �������":
    <a   =>   a
    <b   =>   �
    <c   =>   �

������ �� ������� ����������, � ��������� ����� ������ �������� �������������.
�� ���� �������� ��� �����������, �� ��������� ������������� ����.
��������� ���������� �� ������� ���: <c -> <C. ��� ����������� ���� ���������
������ ���� �� ��������.
����� �� ����������.
}

function DecodeRussian(const inp: string): string;

{
���� Kanji.RusNick:
1. ��������� ���������:
  �������������*��������*
2. �������������� (�������) ������: *#*
  �������*#*�������*
 ���� ��������� ���������, ������������ �������� �� ����� ������:
  �����*�����*#*������*�����**  --- ��� �������� � �����
3. ������� ����������:
  ������� �����*#*�������� ������*
4. ������� �����: *_*
  ����*_*���������*_*��������*�������*_*����������*
  �� ��������� ����� ������ ������� ����� ����� �������, �� ���, ��� ����� _,
  ������ �� �����������.
5. ��������
  �����*!2  --- �������� �� 2-� �����
  ��!  --- ������ ��������������� ����
6. ������? (��� �������� ���������)
  ''�����''
  ''������''
  ''��''*#*''���''  --- ��������� � �������������
  ������������� ''��''  --- ������ �������

��� �����������:
  -��-  --- ������ �� ������, ������������, ��� ����
}
type
  TRusNicks = TArray<string>;

function ParseKanjiRusNick(const inp: string): TRusNicks;

{
���� Kanji.OnYomi:
������: *kana*;*kana**;*kana*
���� � ����� ��� ��������, ������ �����������������.
������ ������ ��� ����� ���� - �������.
}
type
  TOnYomiEntry = record
    kana: string; //���� ������ => �������
    rare: boolean;
  end;
  POnYomiEntry = ^TOnYomiEntry;
  TOnYomiEntries = array of TOnYomiEntry; //������ => �������

function ParseOnYomi(const inp: string): TOnYomiEntries;

{
����: Kanji.KunYomi, ���� NameReadings.
������: [� ������]|[�����]|[����]-[������]
����� ����� ����� �������������, �� ���������� ���������������:
  [� ������]||[����]
����������: || � ������ - �� ��, ��� |           (#4)
����������: -[������] �������� �������.
  [� ������]-[������]|[�� ����� ������]
  [� ������]-[������]-[������]
������ �����: $������$������$
� ������ ������:
  - ������� ������� $������,������$ (����� ��� ������)
  - ���������� $�����$
}
type
  TNameReadingType = (
    ntCommon,       //������� ������
    ntOccasional,   //"�����"
    ntRare,         //"����"
    ntHidden        //�������, ������ ��� ������
  );
  TNameReading = record
    _type: TNameReadingType;
    text: string;
  end;
  PNameReading = ^TNameReading;
  TNameReadings = array of TNameReading;

function ParseKanjiNameReadings(const inp: string): TNameReadings;
function DumpKanjiNameReadings(const AReadings: TNameReadings): string;
function DumpKanjiNameReading(const AReading: TNameReading): string;

{
����: Kanji.KunYomi. �������� � ��������� � Russian � Compounds.
������: [����]|[������ � ����������]|[������ � ������|c|�������]
����� ����� ����� �������������, ������ ���������� ���������������. ���� �����
���� ����.
}
type
  TKunYomiReadingSet = record
  end;
  PKunYomiReadingSet = ^TKunYomiReadingSet;

  TCompoundReadingSet = record
  end;
  PCompoundReadingSet = ^TCompoundReadingSet;

  TKanjiReadings = record
    kun: array of TKunYomiReadingSet;
    compound: array of TCompoundReadingSet;
    name: TNameReadings;
  end;
  PKanjiReadings = ^TKanjiReadings;

function ParseKanjiKunYomi(const inp: string): TKanjiReadings;
function DumpKanjiKunYomi(const AReadings: TKanjiReadings): string;

{
����: Kanji.Compounds. �������� � ��������� � KunYomi � Russian.
������: 1:48667,N:8502@,N:2280,N:2279,N:55637,[����]:[�����][�����]
����������� ���� N �������� "� ������".
�����:
 @   �� ���������������� ������ (������ �� ��������� � ����� ������) ������
     �������� ����� �������� ������ � �������� � ������� � ���������� � �������
     ������� (�������� �������� ������� 1)
 ^   ������������� ������ (�������� ��������������)
 *   ������������� �������� (�������� ��������)
 &   ������������� ������ � �������� (�������� � ��������, � ��������������,
     � ����� ��������� ������� ����� ����������� �����)
 #   � ���� ����� �� ������, ����� ���������� � �������
�������� ������ (� ����������� ������� �� ����������) �������� ���� �� ����������
������� ��������� ��������� ��� ���������� ������:
  1:6590,1:6579,1:[27]
C�������� � msgref, wordref ��� ���� ����.
# ������� � ������ ������ ��������, ��� ������ ������� � ��� ����� ����������
������� (#54,#67).

�������:
1. � ������������ ����� ������� ����������� _!�����_�����_1:����������,2:������.
2. ����������� � _�����-��_�����_1:����������,2:������.
3. ������: "=����� �����" ������ ������ (�������, ������ ������).
}
type
  TCompoundFlag = (cfIrregularReading, cfIrregularMeaning, cfSingular, cfHashtag);
  TCompoundFlags = set of TCompoundFlag;
  TCompoundEntry = record
    block: integer; //special purpose blocks are negative
    wordref: integer;
    msgref: integer;
    flags: TCompoundFlags;
  end;
  PCompoundEntry = ^TCompoundEntry;
  TCompoundEntries = array of TCompoundEntry;

const //special purpose blocks
  BLOCK_NAMES = -1;

function ParseKanjiCompounds(inp: string): TCompoundEntries;
function DumpKanjiCompounds(const ACompounds: TCompoundEntries): string;
function DumpKanjiCompound(const ACompound: TCompoundEntry): string;


implementation
uses StrUtils;

{ �������� ������� ��� ������ �� �������� }

{ ��������� ������ ������ �� �����������; ���������� �����������. ����
 ����������� ���, ��������� ������� ������. }
function pop(var s: string; const sep: char): string;
var i: integer;
begin
  i := pos(sep, s);
  if i<=0 then begin
    Result := s;
    s := '';
  end else begin
    Result := copy(s, 1, i-1);
    delete(s, 1, i);
  end;
end;

{ �� ��, �� ����� ����������� ���, ������ �� ���������� � ��������� �����, ���
 ����. }
function ifpop(var s: string; const sep: char): string;
var i: integer;
begin
  i := pos(sep, s);
  if i<=0 then
    Result := ''
  else begin
    Result := copy(s, 1, i-1);
    delete(s, 1, i);
  end;
end;

{ �������� ����� �������� � ps �� pe �� ������������ }
function spancopy(ps, pe: PChar): string;
var i: integer;
begin
  SetLength(Result, (NativeUInt(pe)-NativeUInt(ps)) div SizeOf(char));
  for i := 1 to Length(Result) do begin
    Result[i] := ps^;
    Inc(ps);
  end;
end;

{ ���� ����� ������� ������ ��� ������� StrSplit }
function Split(const s: string; const sep: char): TStringArray;
begin
  Result := StrSplit(PChar(s), sep);
end;

function Unquote(const s: string; op, ed: char): string;
begin
  if (Length(s)>=2) and (s[1]=op) and (s[Length(s)]=ed) then
    Result := copy(s,2,Length(s)-2);
end;

function TryUnquote(var s: string; op, ed: char): boolean;
begin
  Result := (Length(s)>=2) and (s[1]=op) and (s[Length(s)]=ed);
  if Result then
    s := copy(s,2,Length(s)-2);
end;



{ ������� ����� }

procedure Complain(msg: string);
begin
  writeln(ErrOutput, msg);
end;

procedure Complain(source, msg: string);
begin
  writeln(ErrOutput, source+': '+msg);
end;

procedure Complain(source, msg, data: string);
begin
  writeln(ErrOutput, source+': '+msg);
  writeln(ErrOutput, '  '+data);
end;



{ �������� ���� ��������� ������� � ������ ����������� �������� �������. }
function DecodeRussian(const inp: string): string;
const
  eng: string = 'abcdefghijklmnopqrstuvwxyz1234567ABCDEFGHIJKLMNOPQRSTUVWXYZ890!?=+';
  rus: string = '�������������������������������������Ũ��������������������������';
var pc, po: PChar;
  i: integer;
  found: boolean;
begin
  if inp='' then begin
    Result := '';
    exit;
  end;

  Result := '';
  SetLength(Result, Length(inp)); //not going to be bigger than that

  pc := PChar(@inp[1]);
  po := PChar(@Result[1]);
  while pc^<>#00 do begin
    if pc^='<' then begin
      Inc(pc);
      found := false;
      for i:= 1 to Length(eng) do
        if eng[i]=pc^ then begin
          po^ := rus[i];
          found := true;
          break;
        end;
      if not found then begin
        po^ := '<';
        Dec(pc);
      end;
    end else
      po^ := pc^;
    Inc(pc);
    Inc(po);
  end;
  po^ := #00;
  SetLength(Result, StrLen(PChar(Result))); //trim
end;

{ ������� ''�������'' �� �������� RusNick }
function KillQuotes(const inp: string): string;
begin
  if (Length(inp)<4) or (inp[1]<>'''') or (inp[2]<>'''')
  or (inp[Length(inp)-1]<>'''') or (inp[Length(inp)]<>'''') then
    Result := inp
  else
    Result := copy(inp, 3, Length(inp)-4);
end;

{ ��������� ������ � ���������� ������ ���� �������� ������.
 ������������ �����������. ����� ���������� ������ �����������. ������� �����������.
 ����� ���� �� ���������, �� ����������. }
function ParseKanjiRusNick(const inp: string): TRusNicks;
var tmp: string;
  i_pos: integer;
  i, i_start: integer;
begin
  Result.Clear;
  tmp := inp;

 //������������ �����
  i_pos := pos('*#*',tmp);
  if i_pos>0 then
    delete(tmp,i_pos,MaxInt);

 //������� ������ ������ � �������
  tmp := UniReplaceStr(tmp, '*_*', '*');
  tmp := UniReplaceStr(tmp, '**', '*');

 //�������� � �����
  while (Length(tmp)>0) and (tmp[Length(tmp)]='*') do
    SetLength(tmp, Length(tmp)-1);

  if tmp='' then exit;

  i := 1;
  i_start := 1;
  while i<=Length(tmp) do begin
    if tmp[i]='*' then begin
      if (i_start<i)
      and (tmp[i_start]<>'!') then //��� ���� �������� - �����
        Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)));
      i_start := i+1;
    end;
    Inc(i);
  end;

  if (i>i_start)
  and (tmp[i_start]<>'!') then
    Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)))
end;


function ParseOnYomi(const inp: string): TOnYomiEntries;
var i_beg, i_pos, cnt: integer;

  procedure PostWord;
  var i_end: integer;
  begin
    if (i_pos-i_beg<=0) or ((i_pos-i_beg=1) and (inp[i_beg]='-')) then begin
      Result[cnt].kana := ''; //kokuji
      Result[cnt].rare := false;
    end else
   { ������ ������, ���� �� �������, ������ ��-��� ������ ���� ������� �
    *��������*, � ���������� ���� �� �����.
    �� ��� ����. ��� ��� ����� ����� �������. }
    begin
      i_end := i_pos;
      Result[cnt].rare := false;
      if inp[i_beg]='*' then
        Inc(i_beg);
      if inp[i_end-1]='*' then
        Dec(i_end);
      if (i_end-1>=i_beg) and (inp[i_end-1]='*') then begin
        Result[cnt].rare := true;
        Dec(i_end);
      end;
      Result[cnt].kana := copy(inp, i_beg, i_end-i_beg);
    end;
  end;

begin
  if (inp='') or (inp='-') then begin
    SetLength(Result, 0);
    exit;
  end;

 //������� ����� ;,
  cnt := 0;
  for i_pos := 1 to Length(inp) do
    if (inp[i_pos]=';') or (inp[i_pos]=',') then
      Inc(cnt);
  SetLength(Result, cnt+1);

  cnt := 0;
  i_beg := 1;
  i_pos := 1;
  while i_pos<=Length(inp) do begin
    if (inp[i_pos]=';') or (inp[i_pos]=',') then begin
      PostWord;
      i_beg := i_pos+1;
      Inc(cnt);
    end;
    Inc(i_pos);
  end;
  PostWord;
end;

{ ��������� ���� Kanji.KunYomi. ���������� ���������� ���������� ����������
 � ������� �� .Russian � .Compounds }
function ParseKanjiKunYomi(const inp: string): TKanjiReadings;
var ln, block: string;
begin
  ln := inp;
  block := pop(ln, '|');
 //Result.kun := ParseKanjiKunReadings(block);
  block := pop(ln, '|');
 //Result.compound := ParseKanjiCompoundReadings(block);
 //The rest is names
  Result.name := ParseKanjiNameReadings(ln);
end;

function DumpKanjiKunYomi(const AReadings: TKanjiReadings): string;
begin
  Result := 'Names: '+DumpKanjiNameReadings(AReadings.name);
end;

{ ��������� ����� ������ ������ � ������ �� ���� Kanji.KunYomi }
function ParseKanjiNameReadings(const inp: string): TNameReadings;
var blockType: TNameReadingType;
  pc, ps: PChar;
  readingSetOpen: boolean; //������ $ ����� �������� | ��� -

 //ps ������ ������ �� ��������� ������� $, pc �� ��������.
  procedure CommitReading;
  begin
    if ps>=pc then exit;
    if (ps+1=pc) and (ps^=' ') then
      exit; //����� ����� ����� ������ ���������� �������, �� �� ���� �� �����
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1]._type := blockType;
    Result[Length(Result)-1].text := spancopy(ps,pc);
  end;

begin
  SetLength(Result, 0);
  if inp='' then exit;

  blockType := ntCommon;
  readingSetOpen := false;
  pc := PChar(inp);
  if pc^='|' then Inc(pc); // ������ ������ | � ������, ��. ������
  ps := pc;
  while pc^<>#00 do begin
    if pc^='|' then begin
     {$IFDEF STRICT}
      if ps<pc then
        raise Exception.Create('ParseKanjiNameReadings: No closing tag for a reading');
     {$ELSE}
      CommitReading;
     {$ENDIF}
      case blockType of
        ntCommon: blockType := ntOccasional;
        ntOccasional: blockType := ntRare;
      else //rare or hidden
        raise Exception.Create('ParseKanjiNameReadings: additional | not allowed.');
      end;
      ps := pc+1;
      readingSetOpen := false;
    end else

   //note: ����� ���� � ��� ����� ������: $asd$-bsd$  $asd-$bsd$
    if (pc^='-') and ((pc+1)^='$') and (ps>=pc) then begin
     {$IFDEF STRICT}
      if ps<pc then
        raise Exception.Create('ParseKanjiNameReadings: No closing tag for a reading');
     {$ELSE}
      CommitReading;
     {$ENDIF}
      blockType := ntHidden;
      ps := pc+1;
      readingSetOpen := false;
    end else

    if pc^='$' then begin
      CommitReading; //���� ��� �������������
      readingSetOpen := true;
      ps := pc+1;
    end else

    begin
     {$IFDEF STRICT}
      if not ReadingSetOpen then
        raise Exception.Create('ParseKanjiNameReadings: No opening tag for a reading');
     {$ENDIF}
    end;

    Inc(pc);
  end;

  CommitReading;
end;

function DumpKanjiNameReadings(const AReadings: TNameReadings): string;
var i: integer;
begin
  if Length(AReadings)<=0 then begin
    Result := '';
    exit;
  end;
  Result := DumpKanjiNameReading(AReadings[0]);
  for i := 1 to Length(AReadings)-1 do
    Result := Result + ', ' + DumpKanjiNameReading(AReadings[i]);
end;

function DumpKanjiNameReading(const AReading: TNameReading): string;
begin
  case AReading._type of
    ntCommon: Result := 'C:';
    ntOccasional: Result := 'O:';
    ntRare: Result := 'R:';
    ntHidden: Result := 'H:';
  else
    raise Exception.Create('DumpKanjiNameReading: Unexpected name reading type');
  end;
  Result := Result + AReading.text;
end;


{ ��������� ���� Kanji.Compounds. ���������� ���������� ���������� ����������
 � ������� �� .KunYomi � .Russian }
function ParseKanjiCompounds(inp: string): TCompoundEntries;
var parts: TStringArray;
  i: integer;
  block_id: string;
  ch: char;
begin
 //���� ����� ������������ ����� �������, ����� �� �����-�� ������� ����� KunYomi
 //������������ � Compounds. ��� ��� ���������� � _ � ����� ������������� _
  if (Length(inp)>0) and (inp[1]='_') then begin
    Complain('ParseKanjiCompounds', 'KunYomi leak', inp);
    parts := Split(inp, '_'); //���� ������
    if Length(parts)>0 then
      inp := parts[Length(parts)-1];
  end;

 //������� ������ ���� ������ ����� ���� "=����� �����"
  if (Length(inp)>0) and (inp[1]='=') then begin
    Complain('ParseKanjiCompounds', '= operator leak', inp);
    inp := '';
  end;

  parts := Split(inp, ',');
  SetLength(Result, Length(parts));
  if Length(parts)<=0 then exit; //������ ��������

 //������� � ������ ������ ��������, ��� ������ ������� � ��� ����� ����������
 //�����. ����������.
  if (Length(parts[0])>0) and (parts[0][1]='#') then
    delete(parts[0],1,1);

  for i := 0 to Length(parts)-1 do begin
    block_id := ifpop(parts[i], ':');
    if block_id='' then
      raise Exception.Create('ParseKanjiCompounds: no block_id separator.');
   //���� ����� ���� � ������������, 10+
    if block_id='N' then
      Result[i].block := BLOCK_NAMES
    else
      Result[i].block := StrToInt(block_id);

    Result[i].flags := [];
    while Length(parts[i])>0 do begin
      ch := parts[i][Length(parts[i])];
      if ch='@' then
        Result[i].flags := Result[i].flags + [cfSingular]
      else
      if ch='^' then
        Result[i].flags := Result[i].flags + [cfIrregularReading]
      else
      if ch='*' then
        Result[i].flags := Result[i].flags + [cfIrregularMeaning]
      else
      if ch='&' then
        Result[i].flags := Result[i].flags + [cfIrregularReading, cfIrregularMeaning]
      else
      if ch='#' then
        Result[i].flags := Result[i].flags + [cfHashtag]
      else
        break;
      delete(parts[i], Length(parts[i]), 1);
    end;

    if tryunquote(parts[i],'{','}') then
      Result[i].msgref := StrToInt(parts[i])
    else
      Result[i].wordref := StrToInt(parts[i]);
  end;
end;

function DumpKanjiCompounds(const ACompounds: TCompoundEntries): string;
var i: integer;
begin
  if Length(ACompounds)<=0 then begin
    Result := '';
    exit;
  end;
  Result := DumpKanjiCompound(ACompounds[0]);
  for i := 1 to Length(ACompounds)-1 do
    Result := Result + ', ' + DumpKanjiCompound(ACompounds[i]);
end;

function DumpKanjiCompound(const ACompound: TCompoundEntry): string;
begin
  Result := IntToStr(ACompound.block)+':'+IntToStr(ACompound.wordref);
  if ACompound.msgref>0 then
    Result := Result+':'+IntToStr(ACompound.msgref);
  if cfIrregularReading in ACompound.flags then
    Result := Result + '^';
  if cfIrregularMeaning in ACompound.flags then
    Result := Result + '*';
  if cfSingular in ACompound.flags then
    Result := Result + '@';
  if cfHashtag in ACompound.flags then
    Result := Result + '#';
end;

end.
