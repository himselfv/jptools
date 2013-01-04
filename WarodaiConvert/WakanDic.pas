unit WakanDic;
{
��������� ����� ���� ���������� � �������� ������, ������� ��� ������������
��� ������ �� ������.
}

interface
uses TextTable, JWBDic, JWBKanaConv, Warodai, WarodaiHeader, WarodaiBody;

var
  edictStats: record
    edictTagsFound: integer;  //number of articles we have clearly found a match for
    edictTagsCloned: integer; //number of articles we have cloned from a clear match (same source article)
    edictTagsUnsure: integer; //number of articles we have found a possible match for, but weren't sure and dropped it
  end;

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
  roma_t: TRomajiTranslator;
  cdic: TDicCursor;

procedure LoadEdict(filename: string);
begin
  FillChar(edictStats, sizeof(edictStats), 0);
  edict:=TJaletDic.Create;
  edict.Offline := false;
  edict.LoadOnDemand := false;
  edict.FillInfo(filename);
  edict.Load;
  cdic := TDicCursor.Create(edict);
  roma_t := TRomajiTranslator.Create;
  roma_t.LoadFromFile('c_romaji_base.kcs');
end;

procedure FreeEdict;
begin
  FreeAndNil(roma_t);
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
  if edict=nil then begin
    Result := false;
    exit;
  end;

  kana := roma_t.KanaToRomaji(kana, 1);

  if kanji<>'' then begin
    cdic.SetOrder('Kanji_Ind');
    cdic.Locate(cdic.stKanji,kanji);
  end else begin
    cdic.SetOrder('Phonetic_Ind');
    cdic.Locate(cdic.stSort,kana);
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
      cdic.Next;
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
    if not FillWordMarkers(hdr.words[i], mark[i]) then begin
      mark[i].found := false;
      mark[i].markers := '';
      mark[i].pop := false;
    end else begin
      Inc(edictStats.edictTagsFound);
      if m_idx<0 then
        m_idx := i;
    end;

 //��� ��� ����, ��� ������� ������� �� �����, �������� ��, ��� �����
  if m_idx>=0 then
    for i := 0 to hdr.words_used - 1 do
      if not mark[i].found then begin
        Inc(edictStats.edictTagsCloned);
        mark[i] := mark[m_idx];
      end;
end;

end.
