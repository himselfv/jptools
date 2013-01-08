unit WarodaiBody;

interface
uses Warodai;
{$INCLUDE 'Warodai.inc'}

{
Тело состоит из одной или больше групп:
  じくばり【字配り】(дзикубари)〔1-167-1-49〕
  расположение письменных знаков.

  あけくれ【明け暮れ】(акэкурэ)〔1-016-2-21〕
  1. день и ночь;
  2. день и ночь, денно и нощно, всё время, постоянно;
  …に～する отдавать <i>чему-л.</i> всё своё время.

Группа состоит из одной или больше статей:
  じくう【時空】(дзику:)〔1-167-1-46〕
  1) (<i>сокр.</i> 時間 <i>и</i> 空間) время и пространство;
  2) <i>физ.</i> время.

  じたい【自体】(дзитай)〔1-176-1-43〕
  1. 1) собственное тело;
  それ自体の重みで倒れる обрушиться под собственной тяжестью;
  2) сам по себе, как таковой;
  2. собственно говоря.

У нескольких групп или несколький статей может быть общая часть:
  じっぽう【十方】(дзиппо:)〔1-173-1-21〕
  <i>кн.</i>
  1. все направления;
  2. 1) во всех направлениях;
  2) везде, повсюду.

  じとく【自得】(дзитоку)〔1-176-2-37〕
  <i>кн.</i>
  1) самодовольство;
  2) самостоятельное приобретение;

Среди строк статьи встречаются:
- переводы:
  деликатный, щекотливый <i>(о вопросе и т. п.)</i>;
- вариации:
  ～する поднимать [большой] шум
  ～にする мариновать в мисо;
- примеры:
  デリケートな事柄 деликатный вопрос;
  話がデリケートになって来た разговор перешёл на щекотливую тему;
- комментарии:
  • Правка. Было: depot.
  • Также デポー.

Попадаются ошибочные слияния строк:
  : ～な 1) неряшливый <i>(гл. обр. в одежде)</i>;
  (<i>англ.</i> wireless) 1) радиотелеграф; радиотелеграмма;

В разных блоках на первой позиции в начале встречается стартовое двоеточие
  でっぷり(дэппури)〔1-230-2-20〕
  : ～した <i>прост.</i> дородный, толстый, жирный;
Считаем, что его можно отбросить.

Бывает так, что прямой перевод отсутствует, и все варианты - с вариацией:
  みっしゅう【密集】(миссю:)〔1-605-2-33〕
  [...]
  2): ～する <i>воен.</i> сосредоточиваться; смыкать ряды;
  密集して進む двигаться [вперёд] сомкнутым строем.
}

{
Общие строки.

Частые, разбираемые:
<i>неперех.</i>
(<i>англ.</i> advantage)
: ～する
: ～と <i>кн.</i>
: ～[の]

Исправимые:
: ～な 1) неряшливый <i>(гл. обр. в одежде)</i>;
(<i>англ.</i> wireless) 1) радиотелеграф; радиотелеграмма;
}

const
  MaxGroups = 5; //пока в словаре встречалось максимум 4
  MaxBlocks = 22; //пока в словаре встречалось максимум 20
  MaxLines = 150; //в словаре попадалось 140! правда, редко

type
  TEntryBlock = record
    num: integer;
    lines: array[0..MaxLines-1] of string;
    line_cnt: integer;
    procedure Reset;
    function AddLine: PString;
  end;
  PEntryBlock = ^TEntryBlock;

  TEntryGroup = record
    num: integer;
    common: string; //если блок "влитой" (без номера), эта строка всегда пустая
    blocks: array[0..MaxBlocks-1] of TEntryBlock;
    block_cnt: integer;
    procedure Reset;
    function AddBlock: PEntryBlock;
  end;
  PEntryGroup = ^TEntryGroup;

  TEntryBody = record
    common: string; //если группа "влитая" (без номера), эта строка всегда пустая
    groups: array[0..MaxGroups-1] of TEntryGroup;
    group_cnt: integer;
    procedure Reset;
    function AddGroup: PEntryGroup;
  end;
  PEntryBody = ^TEntryBody;

{
Читает до окончания статьи или до конца файла. Разбирает прочитанное и
корректирует что может.
При ошибке бросает exception.
Вызывать только при уверенности, что статья существует (заголовок прочитан).
}
procedure ReadBody(inp: TWarodaiReader; body: PEntryBody);

{
Иногда перевод содержит несколько вариантов, тогда они пронумерованы так:
1) перевод 1
2) перевод 2
Так может продолжаться до двухзначных чисел включительно.
Эта функция удаляет из строки такой элемент и возвращает его отдельно, или возвращает false.
}
function PopGroupNumber(var ln: string; out num: integer): boolean;
function PopBlockNumber(var ln: string; out num: integer): boolean;
function PopAlternativeId(var ln: string; out id: string): boolean;


{
Удаляет точки и точки с запятой в конце статьи (но не существенную пунктуацию,
типа вопросительных знаки и восклицаний).
}
procedure TrimEndPunctuation(var s: string);

{
Иногда в начале строки валяется лишнее двоеточие - ошмётки от исходного скана.
}
procedure TrimStartColon(var s: string);



implementation
uses SysUtils, StrUtils, UniStrUtils, StreamUtils, WarodaiTemplates;

procedure TEntryBlock.Reset;
begin
  num := 0;
  line_cnt := 0;
end;

function TEntryBlock.AddLine: PString;
begin
  Inc(line_cnt);
  if line_cnt>Length(lines)-1 then
    raise EParsingException.Create('EntryBlock: cannot add one more line');
  Result := @lines[line_cnt-1];
  Result^ := '';
end;

procedure TEntryGroup.Reset;
begin
  common := '';
  num := 0;
  block_cnt := 0;
end;

function TEntryGroup.AddBlock: PEntryBlock;
begin
  Inc(block_cnt);
  if block_cnt>Length(blocks)-1 then
    raise EParsingException.Create('EntryGroup: cannot add one more block');
  Result := @blocks[block_cnt-1];
  Result^.Reset;
end;

procedure TEntryBody.Reset;
begin
  common := '';
  group_cnt := 0;
end;

function TEntryBody.AddGroup: PEntryGroup;
begin
  Inc(group_cnt);
  if group_cnt>Length(groups)-1 then
    raise EParsingException.Create('EntryBody: cannot add one more group');
  Result := @groups[group_cnt-1];
  Result^.Reset;
end;

{ Pops 1., 2., ... etc }
function PopGroupNumber(var ln: string; out num: integer): boolean;
var i: integer; //по какой символ включительно удалять
  snum: string;
begin
  i := 0;
  if CharIsNumber(ln[1]) then
    if ln[2]='.' then begin
      i := 2;
    end else
    if CharIsNumber(ln[2]) then
      if ln[3]='.' then begin
        i := 3;
      end;
  if i<=0 then begin
    Result := false;
    exit;
  end;

  snum := copy(ln, 1, i-1);
  if not TryStrToInt(snum, num) then
    raise EParsingException.Create('Invalid block number');

 //Убираем пробелы, которые следуют за скобкой
  Inc(i);
  while (i<=Length(ln)) and (ln[i]=' ') do
    Inc(i);
  Dec(i);

  delete(ln, 1, i);
  Result := true;
end;

{ Pops 1), 2)... etc }
function PopBlockNumber(var ln: string; out num: integer): boolean;
var i: integer; //по какой символ включительно удалять
  snum: string;
begin
  i := 0;
  if CharIsNumber(ln[1]) then
    if ln[2]=')' then begin
      i := 2;
    end else
    if CharIsNumber(ln[2]) then
      if ln[3]=')' then begin
        i := 3;
      end;
  if i<=0 then begin
    Result := false;
    exit;
  end;

  snum := copy(ln, 1, i-1);
  if not TryStrToInt(snum, num) then
    raise EParsingException.Create('Invalid block number');

 //Убираем пробелы, которые следуют за скобкой
  Inc(i);
  while (i<=Length(ln)) and (ln[i]=' ') do
    Inc(i);
  Dec(i);

  delete(ln, 1, i);
  Result := true;
end;

{ Pops а), б)... etc }
function PopAlternativeId(var ln: string; out id: string): boolean;
var i: integer; //по какой символ включительно удалять
begin
  if ((ln[1]<'а') or (ln[1]>'я')) or (ln[2]<>')') then begin
    Result := false;
    exit;
  end;
  i := 2;

  id := copy(ln, 1, i-1);

 //Убираем пробелы, которые следуют за скобкой
  Inc(i);
  while (i<=Length(ln)) and (ln[i]=' ') do
    Inc(i);
  Dec(i);

  delete(ln, 1, i);
  Result := true;
end;

{ Удаляет пунктуацию в конце фразы - точки, точки с запятой, запятые, пробелы.
 Вопросительные и восклицательные знаки, а также троеточие не трогает }
procedure TrimEndPunctuation(var s: string);
var ch: Char;
  i: integer; //last symbol to keep
begin
  i := Length(s);
  if i<=0 then exit;
  ch := s[i];
  while (ch='.') or (ch=';') or (ch=',') or (ch=' ') do begin
    if (i>1) and (ch='.') and (s[i-1]='.') then break; //троеточие не трогаем
    Dec(i);
    if i<=0 then break;
    ch := s[i];
  end;
  delete(s, i+1, Length(s)-i);
end;

procedure TrimStartColon(var s: string);
var i: integer;
begin
  i := 1;
  while (i<=Length(s)) and ((s[i]=':') or (s[i]=' ')) do
    Inc(i);
  delete(s, 1, i-1);
end;


function IsOnlyKanaKanji(const s: string; i_beg: integer): boolean;
var i: integer;
begin
  Result := true;
  for i := i_beg to Length(s) do
    if (s[i]<>' ') and (s[i]<>',') and not IsKana(s[i]) and not IsKanji(s[i]) then begin
      Result := false;
      exit;
    end;
end;


{ Фикс: если номер сразу 2, и до сих пор была только автоматическая группа,
считаем её номером 1 (номер пропущен)
Все остальные случаи пропуска рапортуем как ошибку (мы их пока не умеем чинить) }
procedure AutoFixGroupNumber(body: PEntryBody; group: PEntryGroup; const num: integer);
var i: integer;
  s: PString;
begin
  if num=group.num+1 then exit; //all clear
  if (num>group.num+2)
  or ((num>group.num+1) and (num<>2)) then begin
    Inc(WarodaiStats.GroupNumberMissing);
    raise EParsingException.Create('Group number skip');
  end;

 //Остался случай 2-0
  if (group.block_cnt<=0)
  or (group.blocks[0].line_cnt<=0) then begin
    Inc(WarodaiStats.GroupNumberMissing);
    raise EParsingException.Create('Group number skip 2-0 and no previous lines, wtf.');
  end;

 //Ищем только в первом блоке, только в первой строчке
  s := @group.blocks[0].lines[0];
  i := pos(' 1.', s^);
  if i>0 then begin
    body.common := copy(s^, 1, i);
    Inc(i, Length(' 1.'));
    delete(s^, 1, i);
    s^ := Trim(s^);
   //Если строка осталась пустой - исключение (наверху к этому не готовы, если такие будут - придётся писать выкидывание их из списка)
    if s^='' then
      raise EParsingException.Create('Empty line after doing AutoFixGroupNumber');
    Inc(WarodaiStats.GroupNumberFixed);
  end else
   //Ничем не можем сделать - считаем, что он просто пропал
    Inc(WarodaiStats.GroupNumberGuessed);
  group.num := 1;
end;

{ То же самое с блоками }
procedure AutoFixBlockNumber(body: PEntryBody; group: PEntryGroup; block: PEntryBlock; const num: integer);
var i: integer;
  s: PString;
begin
  if num=block.num+1 then exit; //all clear
  if (num>block.num+2)
  or ((num>block.num+1) and (num<>2)) then begin
    Inc(WarodaiStats.BlockNumberMissing);
    raise EParsingException.Create('Block number skip');
  end;

 //Остался случай 2-0
  if (block.line_cnt<=0) then begin
    Inc(WarodaiStats.BlockNumberMissing);
    raise EParsingException.Create('Block number skip 2-0 and no previous lines, wtf.');
  end;

 //Ищем только в первом блоке, только в первой строчке
  s := @block.lines[0];
  i := pos(' 1)', s^);
  if i>0 then begin
    group.common := copy(s^, 1, i);
    Inc(i, Length(' 1)'));
    delete(s^, 1, i);
    s^ := Trim(s^);
   //Если строка осталась пустой - исключение (наверху к этому не готовы, если такие будут - придётся писать выкидывание их из списка)
    if s^='' then
      raise EParsingException.Create('Empty line after doing AutoFixBlockNumber');
    Inc(WarodaiStats.BlockNumberFixed);
  end else
   //Ничем не можем сделать - считаем, что он просто пропал
    Inc(WarodaiStats.BlockNumberGuessed);
  block.num := 1;
end;





//Патчи, которые можно применять сразу при чтении
procedure ApplyCompatPatches(var ln: string);
var i: integer;
begin

 { Пропускаем блоки вида
     <i>уст.</i> 穴賢, 穴畏, 恐惶
   Они часто портят собой число доступных переводов, и вообще они не нужны. }
  i := pos('<i>уст.</i>', ln);
  if (i=1) and IsOnlyKanaKanji(ln, i + Length('<i>уст.</i>') + 1) then begin
    ln := '';
    exit;
  end;

 { Открытые шаблоны значат, что дальнейшие записи переводов относятся
  уже не к главное статье.
  Простого способа программно понять, где начинается, а где кончается зона
  действия такого шаблона - нет. Поэтому мы такие статьи пропускаем.}
  if IsOpenTemplate(ln) then begin
    Inc(WarodaiStats.OpenTemplates);
    raise EOpenTemplate.Create('Opener variation');
  end;

end;


procedure ReadBody(inp: TWarodaiReader; body: PEntryBody);
var ln: string;
  num: integer;
  group: PEntryGroup;
  block: PEntryBlock;
  id: string;
begin
  body.Reset;
  group := body.AddGroup;
  block := group.AddBlock;

  while inp.ReadLine(ln) and (ln<>'') do begin
    if Length(ln)<2 then begin
      Inc(WarodaiStats.LinesTooShort);
      continue;
    end;

    if PopGroupNumber(ln, num) then begin
     //Исправляем пропущенные номера групп
      AutoFixGroupNumber(body, group, num);

     //Если до сих пор была только автоматическая группа, разбираем то, что в ней было
      if (body.group_cnt=1) and (group.num=0) then begin
       //Автоматическая группа в такой ситуации не имеет права содержать ничего,
       //кроме common single line
        if (group.block_cnt>1) or (block.num<>0) then begin
          Inc(WarodaiStats.ExplicitCommonGroupBlocks);
          raise EParsingException.Create('Explicit common group blocks!');
        end else
        if (block.line_cnt>1) then begin
          Inc(WarodaiStats.MultilineGroupCommon);
          raise EMultilineCommon.Create('Multiline group common');
        end;

        if block.line_cnt>0 then
          body.common := block.lines[0];
        block.Reset;
        group.Reset;
        Inc(WarodaiStats.GroupCommon);
      end else begin
        group := body.AddGroup;
        block := group.AddBlock;
      end;
      group.num := num;
    end;

    if Length(ln)<=0 then
      continue;
    if Length(ln)<2 then begin
      Inc(WarodaiStats.LinesTooShort);
      continue;
    end;

    if PopBlockNumber(ln, num) then begin
     //Исправляем пропущенные номера блоков
      AutoFixBlockNumber(body, group, block, num);

     //Если у нас до сих пор был только автоматический блок, пробуем перенести его в common
      if (group.block_cnt=1) and (block.num=0) then begin
        if block.line_cnt>1 then begin
         //Лучше пропустить эту статью, чем занести какую-нибудь хренотень типа "common 1) first meaning"
          Inc(WarodaiStats.MultilineBlockCommon);
          raise EMultilineCommon.Create('Multiline block common');
        end;

        if block.line_cnt>0 then
          group.common := block.lines[0];
        block.Reset;
        Inc(WarodaiStats.BlockCommon);
      end else
        block := group.AddBlock;
      block.num := num;
    end;

    if Length(ln)<=0 then
      continue;

    if PopAlternativeId(ln, id) then begin
      Inc(WarodaiStats.AlternativeIds);
      raise EAlternativeIds.Create('Alternative ids unsupported');
    end;

    if ln[1]='•' then begin
      Inc(WarodaiStats.Comments);
      continue; //skip comments!
    end;

   {$IFDEF ASSERT}
    //We assume comment symbols only appear as first chars of the (rest of the) line,
    //but that might not be true.
    if pos('•', ln)>0 then
      raise EParsingException.Create('Comment symbol inside the line!');
   {$ENDIF}

    TrimStartColon(ln);
    TrimEndPunctuation(ln);
    ln := Trim(ln);
    if Length(ln)<=0 then
      continue;

    ApplyCompatPatches(ln);
    if Length(ln)<=0 then
      continue;


    Inc(WarodaiStats.DataLines);
    block.AddLine^ := ln;
  end;
end;

end.
