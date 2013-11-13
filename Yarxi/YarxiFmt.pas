unit YarxiFmt;
{ �������, ������������ � ���� ������ �����. ����� ������� ���������, ��� �����
 ��� ������ � �����. }

interface
uses SysUtils, Classes, UniStrUtils, FastArray;

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

function DecodeKanjiRusNick(const inp: string): TRusNicks;

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

function SplitOnYomi(const inp: string): TOnYomiEntries;

implementation
uses StrUtils;

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
function DecodeKanjiRusNick(const inp: string): TRusNicks;
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
      if i_start<i then
        Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)));
      i_start := i+1;
    end;
    Inc(i);
  end;

  if i>i_start then
    Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)))
end;


function SplitOnYomi(const inp: string): TOnYomiEntries;
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

end.
