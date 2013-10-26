unit YarxiFmt;

interface
uses SysUtils, Classes, UniStrUtils;

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
1. *#*: �������������� ������
  <e<f<z<g<c<3<k*#*<e<f<z<f<c<3<k* (�������*#*�������*)
}

function StripAlternativeRusNicks(const inp: string): string;
function DecodeKanjiRusNick(const inp: string): TStringArray;

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

{ ����������� �������������� �������� ��������� �������� ������, �������� ������
 �������. ����� ��� �������, ��� ������ ���� � �������� ������, ��������� ��
 ������ ������.
 ��� �����, ��� � 95% ������� ������� ������ ���� }
function StripAlternativeRusNicks(const inp: string): string;
var i: integer;
begin
  Result := inp;
  i := pos('*#*', inp);
  if i>0 then SetLength(Result, i);
end;

{ ��������� ������ � ���������� ������ ���� ��������� �������� ������.
 ������ ������� - ��������. }
function DecodeKanjiRusNick(const inp: string): TStringArray;
var i, old_i, cnt: integer;
begin
 //������� ����� ���������, ����� 10 ��� �� ��������
  i := pos('*#*', inp);
  if i<=0 then begin //fast common case
    SetLength(Result, 1);
    Result[0] := inp;
    exit;
  end;
  Inc(i, 2); //length of *#* minus 1

  cnt := 0;
  while i>1 do begin
    Inc(cnt);
    Inc(i); //skip the char itself
    i := pos('*', inp, i);
  end;

  SetLength(Result, cnt);
  i := pos('*#*', inp);

  cnt := 0;
  old_i := 1;
  while i>1 do begin
    Result[cnt] := copy(inp, old_i, i-old_i+1-1);
    if cnt=0 then
      Inc(i,3)
    else
      Inc(i);
    Inc(cnt);
    old_i := i;
    i := pos('*', inp, i);
  end;
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
