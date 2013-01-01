unit WakanDic;
{
��������� ����� ���� ���������� � �������� ������, ������� ��� ������������
��� ������ �� ������.
}

interface
uses TextTable, JWBDic, Warodai;

procedure LoadEdict(filename: string);
procedure FreeEdict;

{
������� ����������� ��������� �� ����+������ ��� ������ ����, ��� ���������� false.
}

type
  TEdictResult = record
    idx: integer;
    markers: string;
  end;

function EdictFind(kana: string; kanji: string; out ret: TEdictResult): boolean;

{
��������� ����� �������� ��� ������� �� ���� � TEntryHeader.
�������� ������� �������� ����� �������, ������� ���� � TEntryHeader ������ -
��������� ����� ���� �������, ���� ������� ��� ����� ����� ���� �� �������.

���� � ������ ������ ��������� �������, ������� ������� ��� ������ ���������
� ������� ���� ������-������.

���� � ������ ��� ������, ������� ����� ������� �� ����� ����� �����������,
�� ���� �� ����� ������ (������� ���������, ��� �� ����� ����� ������ ������).
}

type
  TEntryWordMarkers = record
    found: boolean;
    markers: string; //��� ������
    pop: boolean; //����� (P) ������� � ����� ������ ������ �������
  end;
  TEntryMarkers = array[0..MaxWords-1] of TEntryWordMarkers;

function FillWordMarkers(const word: TEntryWord; out mark: TEntryWordMarkers): boolean;
procedure FillMarkers(const hdr: TEntryHeader; out mark: TEntryMarkers);


implementation
uses SysUtils, JWBEdictMarkers;

var
  edict: TJaletDic;
  cdic: TDicCursor;

procedure LoadEdict(filename: string);
begin
  edict:=TJaletDic.Create;
  edict.Offline := false;
  edict.LoadOnDemand := false;
  edict.FillInfo(filename);
  edict.Load;
  cdic := TDicCursor.Create(edict);
end;

procedure FreeEdict;
begin
  FreeAndNil(cdic);
  FreeAndNil(edict);
end;

{
������� ������ ���������� ���������� ��������� � ���������� ��� ���� false.
kana ������ ���� � �������.
����������� ����������:
1. ���� kanji='', �� � ������� ���� ������ ���� kanji='' � ���� ������ ���������
 � ��������, � ������ ������� � ����� ����� ���� �� ������.
2. ����� kanji ������ ��������� � ��������. �� ����� ��������� ����������
 ���, ��� � �������� ��������� � ����, � ���� ������ ��� - ������.
}
function EdictFind(kana: string; kanji: string; out ret: TEdictResult): boolean;
var rcnt: integer;
  rkana: string;
begin
  if kanji<>'' then begin
    cdic.SetOrder('Kanji_Ind');
    cdic.Locate(cdic.stKanji,kanji,false);
  end else begin
    cdic.SetOrder('Phonetic_Ind');
    cdic.Locate(cdic.stSort,kana,false);
  end;

  rcnt := 0;
  while (not cdic.EOF) and(
        ((kanji='') and (cdic.Str(cdic.TDictSort)=kana))
    or  ((kanji<>'') and (cdic.Str(cdic.TDictKanji)=kanji))
  ) do begin
    Inc(rcnt);
    if kanji='' then begin
      if (rcnt>1) then begin
       //������ ��� � ������ ������ ���������� => �� ����� ������ ������� �����
        Result := false;
        exit;
      end;
      ret.idx := cdic.Int(cdic.TDictIndex);
      ret.markers := cdic.Str(cdic.TDictMarkers);
      continue;
    end;

   //����� ������ <> ''
   //���� ����� ���, ��� ����� ��������� ����, �� ������ ��� � �������.
   //����� ������ �������, ���� �� ��� �������� ���� �� ������ - ������ ���������.
    rkana := cdic.Str(cdic.TDictPhonetic);
    if (rkana=kana) or (rcnt<=1) then begin
      ret.idx := cdic.Int(cdic.TDictIndex);
      ret.markers := cdic.Str(cdic.TDictMarkers);
      if rkana=kana then break;
    end;

    cdic.Next;
  end;
  Result := rcnt>0;
end;

function FillWordMarkers(const word: TEntryWord; out mark: TEntryWordMarkers): boolean;
var i: integer;
  res: TEdictResult;
begin
  if word.s_kanji_used<=0 then begin
    Result := EdictFind(word.s_reading, '', res);
    if Result then begin
      mark.found := true;
      mark.markers := MarkersToStr(res.markers, mark.pop);
      exit;
    end;
  end else
  for i := 0 to word.s_kanji_used - 1 do
    if EdictFind(word.s_reading, word.s_kanji[i], res) then begin
      mark.found := true;
      mark.markers := MarkersToStr(res.markers, mark.pop);
      Result := true;
      exit;
    end;
  Result := false;
end;

procedure FillMarkers(const hdr: TEntryHeader; out mark: TEntryMarkers);
var i, m_idx: integer;
begin
  m_idx := -1;
  for i := 0 to hdr.words_used - 1 do
    if not FillWordMarkers(hdr.words[i], mark[i]) then
      mark[i].found := false
    else
    if m_idx<0 then
      m_idx := i;

 //��� ��� ����, ��� ������� ������� �� �����, �������� ��, ��� �����
  if m_idx>=0 then
    for i := 0 to hdr.words_used - 1 do
      if not mark[i].found then mark[i] := mark[m_idx];
end;

end.
