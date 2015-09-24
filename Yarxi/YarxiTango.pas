﻿unit YarxiTango;
{
Tango
  Nomer     Номер. В точности соответствует номеру записи, можно игнорировать.
  K1        Ссылка на кандзи или ноль.
  K2
  K3
  K4
  Kana      Кана, см. ParseTangoKana
  Reading   Чтение, см. ParseTangoReading
  Russian   Перевод, см. ParseTangoRussian
  Hyphens   Список позиций, где вставить знаки препинания, см. ParseTangoHyphens
  Yo        Список номеров букв "е", которые "ё", см. ParseTangoYo
}

interface


function ParseTangoKana(K1, K2, K3, K4: SmallInt; inp: string): string;


type
  TTangoReading = record
    text: string;
    rare: boolean;
    dollar: boolean; //флаг *$* - встречается в небольшом числе случаев, хз что значит
  end;
  PTangoReading = ^TTangoReading;
  TTangoReadings = array of TTangoReading;

function ParseTangoReading(inp: string): TTangoReadings;



type
  TTangoRussian = string; //тип определён так временно

function ParseTangoRussian(inp: string): TTangoRussian;

function ParseTangoHyphens(reading: TTangoReading; inp: string): TTangoReading; //может быть лучше будет редактировать на месте, а не возвращать

function ParseTangoYo(russian: TTangoRussian; inp: string): TTangoRussian; //может быть лучше будет редактировать на месте, а не возвращать

implementation
uses SysUtils, WcExceptions, KanaConv, YarxiStrings, YarxiCore, YarxiRefs,
  YarxiReadingCommon;

{
Tango.Kana
Cлово составляется так:
  K1 K2 K3 K4 (Ki идут до первого нуля)
Затем в полученный текст вставляется кана:
  2no4kara ==> 1 2 NO 3 4 KARA
Куски каны добавляются туда, куда указывает цифра перед ними.
Скобки трактуются вполне буквально:
  2(4) ==> 1 2 (3 4)
  2[4] ==> 1 2 (3 4)
Все неиспользованные кандзи добавляются в конец, то есть, можно сделать так:
  0a:kanso: ==> A:KANSO: 1 2
В кане могут быть вставлены дополнительные кандзи #номер##номер#:
  13 251 2861 251 2ha4wo#642##1850#6suru                                  #1030
  akkaharyo:kawokuchikusuru

По умолчанию кана - хирагана; катакана включается так:
  ^a:kanso: ==> A:KANSO: (катаканой)
  0^amerika1@ri ==> AMERIKA1ri (включили обратно хирагану)

Непонятно:
  0akarasama  akarasama *2прямой,ясный,открытый;честный,откровенный
  все кандзи 0, последнее -1 -- втф?
  похоже, по такой схеме пишутся все слова, которые не используют кандзи и
  НЕ катакана (пишутся хираганой, нативные)
  Хотя видимой разницы нет, если заменить на ноль.

  1^ei  akaei	#ихт.#красный хвостокол(скат),=Dasyatis akajei
  зачем такая схема? кандзи всего один, можно было не ставить 1
  видимо, ^ по умолчанию предполагает цифру перед ним (обычно это ноль),
  а т.к. здесь не ноль - её указали явно. Не знаю, что будет если сделать просто "^ei", и встречается ли такое.
}

//True, если символ - буква, которая может встречаться в ромадзи
function IsTangoRomajiChar(const ch: char): boolean;
begin
  Result := IsLatin(ch) or (ch=':'){долгота в транскрипциях}
    or (ch=''''){после буквы n}
    or (ch='(') or (ch=')')
    or (ch='[') or (ch=']') //#337
  ;
end;

//Читает транскрипцию начиная с текущей буквы
function EatTangoRomaji(var pc: PChar): string;
var ps: PChar;
begin
  ps := pc;
  while IsTangoRomajiChar(pc^) do
    Inc(pc);
  Result := spancopy(ps,pc);
end;

//Находит номер символа, которым в строке идёт i-й по счёту кандзи
//Если запрошен нулевой символ, возвращает ноль. Если символ, которого по счёту
//нет, то позицию после конца строки.
function charIdxOfKanji(s: string; i: integer): integer;
var pc: PChar;
begin
  Result := 0;
  if s='' then exit;

  pc := PChar(s);
  while i>0 do begin
    Inc(Result);
    if pc^=#00 then break;
    if IsKanji(pc^) then
      Dec(i);
    Inc(pc);
  end;
end;

function ParseTangoKana(K1, K2, K3, K4: SmallInt; inp: string): string;
var pc: PChar;
  flag_minusOne: boolean;
  flag_katakana: boolean;
  kpos: integer;
  kstr: string;
begin
  Result := '';
  flag_katakana := false;

 //Стыкуем кандзи-базу
  if K1>0 then
    Result := Result + getKanji(K1)
  else
    Check(K1=0);

  if K2>0 then begin
    Check(K1>0);
    Result := Result + getKanji(K2);
  end else
    Check(K2=0);

  if K3>0 then begin
    Check(K2>0);
    Result := Result + getKanji(K3);
  end else
    Check(K3=0);

  if K4>0 then begin
    Check(K3>0);
    Result := Result + getKanji(K4);
  end else
    if K4=-1 then
      flag_minusOne := true
    else begin
      flag_minusOne := false;
      Check(K4=0);
    end;

 //Теперь вставляем кану
  if inp='' then exit; //всё сделано
  pc := PChar(inp);

  kpos := 0;
  while pc^<>#00 do
   //Доп. кандзи в конец строки
    if pc^='#' then begin
      Inc(pc);
      Result := Result + getKanji(EatNumber(pc));
      Check(pc^='#');
      Inc(pc);
    end else
   //Переключатели катаканы-хираганы
    if pc^='^' then begin
      flag_katakana := true;
      Inc(pc);
    end else
    if pc^='@' then begin
      flag_katakana := false;
      Inc(pc);
    end else
    if IsDigit(pc^) then
      kpos := EatNumber(pc)
    else
   //Обычный блок
    begin
      kstr := EatTangoRomaji(pc);
      Check(kstr<>'', 'Пустой блок ромадзи в строчке %s', [inp]);
      if flag_katakana then
        kstr := RomajiToKana('K'+kstr)
      else
        kstr := RomajiToKana('H'+kstr);
      insert(kstr, Result, charIdxOfKanji(Result, kpos+1));
    end;
end;

{
Tango.Reading.
Формат: чтение*чтение**чтение**
  *     следующий вариант чтения
  **    прошлое чтение - редкое (напр. #54)
  *$*   х.з. что, ставлю флаг (#7197)
  *=*   альтернатива, см. CombineAlternativeReading
  *(*   далее идёт список вариантов/разбиений на слова
Список разбиений скрыт, используется только для поиска:
  agarisagari*(*agari*sagari* (#413) - будет найдено по sagari, но не по saga
Формат какой-то дебильный и неоднозначный, встречается следующее:
  aizenmyo:o:*(*myo:o:*aizen*         переставлены местами!
  eikyu:*towa**tokoshinae**(*eikyu:*towa**tokoshi*na*e***eikyu:*towa**tokoshie*** #5312
В первом случае куски строки переставлены. Получается, порядок неважен. Нахрена
тогда во втором дублировать весь базовый набор дважды?
Логика добавления звёздочек тоже никакому здравому смыслу не подчиняется.
Но, слава богу, эти разбиения не особо нужны, так что мы их пока просто
выкидываем. Если кому-то очень хочется, могут разобраться и написать парсер.
}
function IsTranscriptionChar(const ch: char): boolean;
begin
  Result := IsLatin(ch) or (ch=':'){долгота в транскрипциях}
    or (ch=''''){после буквы n}
  ;
end;

//Читает транскрипцию начиная с текущей буквы
function EatTranscription(var pc: PChar): string;
var ps: PChar;
begin
  ps := pc;
  while IsTranscriptionChar(pc^) do
    Inc(pc);
  Result := spancopy(ps,pc);
end;

function ParseTangoReading(inp: string): TTangoReadings;
var pc: PChar;
  rd: PTangoReading;
  tmp_str: string;
begin
  SetLength(Result, 0);
  if inp='' then exit;
  rd := nil;

  pc := PChar(inp);
  while pc^<>#00 do begin
    if pc[0]='(' then begin
      break; // Да пошло всё в задницу.
    end else
    if (pc[0]='*') and (pc[1]='$') then begin
      Check(rd<>nil);
      Check(not rd.dollar); //чтоб дважды не допускать
      rd.dollar := true;
      Inc(pc, 2);
    end else
    if (pc[0]='*') and (pc[1]='*') then begin
      Check(rd<>nil);
      Check(not rd.rare); //чтоб дважды не допускать
      rd.rare := true;
      Inc(pc, 2);
    end else
    if pc^='=' then begin
    { Альтернативное написание - поддерживается только ва/ха, как в Яркси }
      Check(rd<>nil, 'Нельзя дать альтернативное чтение без главного');
      Inc(pc);
      Check(pc^='*'); //можно ослабить и сделать if pc^=='*' then Inc(pc);
      Inc(pc);
      tmp_str := EatTranscription(pc);
      Check(tmp_str<>'');
      rd.text := CombineAlternativeReading(rd.text, tmp_str);
     // req_sep := true;
    end else
    if (pc[0]='*') then begin
      Inc(pc);
    end else
   //Обычный текст
    begin
      tmp_str := EatTranscription(pc);
      Check(tmp_str<>'');
      SetLength(Result, Length(Result)+1);
      rd := @Result[Length(Result)-1];
      rd.text := tmp_str;
      rd.rare := false;
    end;
  end;

end;

{
Tango.Russian
Во многом повторяет Kanji.KunyomiRussian - надо вынести общее.
 \    мусорный символ, разделяющий компоненты для поиска. Удалять.

! в начале строчки === "не проверено"

@ === специальные фразы:
3 = "и т.п."

'' === хирагана:
''no'' === (хираганой) no

# === переключение италика
#text# === text италиком

() === скобки
По умолчанию текст в скобках - италик.

(, ) === италик-скобки
(#, #) === не-италик скобки

>[цифры] === специальный тип слова
1 = мужское имя
2 = женское имя
3 = фамилия
4 = псевдоним
5 = топоним
Цифры могут объединяться: >35 === "фамилия и топоним"
Повторяться: >11 == "мужские имена"

& === начало следующего варианта
Например: &строение из камня&запор на двери
1. строение из камня
2. запор на двери

Звёздочки:
*0 === "~shita", и т.п., 0..9:
shita, suru, na, no, ni, de, to, taru, shite, shite iru
**0 === "~o suru", и т.п., 0..9:
o suru, ga aru, no aru, no nai, de aru, des, da, ni suru, ni naru, to shite
***0 === "~naru", и т.п., 0..9:
naru, kara, made, mo, wa, to suru, yori, ga shite iru, to shita, to shite iru
****0 === таких нет.
*=00 === двойной номер (их дофига, надо таблицу выдрать).
Минус перед номером значит, что весь блок в скобках:
*-4 === "(~ni)"
Закрывающая квадраная скобка после номера значит, что номер в квадратных скобках:
*4] === "~[4]"

Загадки:
\_

Примеры:
&#\муз.#\интерлюдия\(\особ.\на сямисэне между двумя песнями\)
&\оживляющий возглас\(\в песне\)
&\вставная\(#\непрошенная\#)\реплика
<i>: муз.
<i>: особ. на сямисэне между двумя песнями
<i>: в песне

1: *=00 *=01 *=02 *=03 *=04 *=05 *=06 *=07 *=08 *=09
2: *=10 *=11 *=12 *=13 *=14 *=15 *=16 *=17 *=18 *=19
3: *=20 *=21 *=22 *=23 *=24 *=25 *=26 *=27 *=28 *=29
4: *=30 *=31 *=32 *=33 *=34 *=35 *=36 *=37 *=38 *=39
}
function ParseTangoRussian(inp: string): TTangoRussian;
begin

end;

{
Tango.Hyphens
Список позиций, где следует вставить знаки препинания, напр.:
  "-7 9" + "aiduchiwoutsu" = "aiduchi-wo utsu".
Проверенные знаки:
  " "
  "-"
Также встречается, непонятно:
  (4)7            #279 видимо, скобки? проверить
  (6)9*(5)8
  3-7 9
  **(7)9          #5312 -- звёздочки?
}
function ParseTangoHyphens(reading: TTangoReading; inp: string): TTangoReading; //может быть лучше будет редактировать на месте, а не возвращать
begin

end;

{
Tango.Yo
Список номеров букв "е", которые на самом деле ё, напр.:
  "1,3" + "елки зеленые" = "ёлки зелёные"
  Разделение нужно, видимо, для поиска.
}
function ParseTangoYo(russian: TTangoRussian; inp: string): TTangoRussian; //может быть лучше будет редактировать на месте, а не возвращать
begin

end;

end.
