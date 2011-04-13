unit SearchSort;
{$WEAKPACKAGEUNIT ON}

interface
uses SysUtils, UniStrUtils;

type
 //Возвращает > 0, если объект больше объекта на позиции pos. Ноль, если равны.
  TCompareProcP = function(data: pointer; obj: pointer; pos: integer): integer;
 //Обратная совместимость: Возвращает > 0, если позиция I больше объекта
  TItemCmpProc = function(Data: pointer; I: integer; Item: pointer): integer;

 //Возвращает > 0, если элемент 1 больше элемента 2. Ноль, если они равны.
  TCompareProcI = function(data: pointer; i1, i2: integer): integer;
 //Перемещает элемент I на место NewI, сдвигая остальное вниз. I всегда >= NewI.
  TMoveProcI = procedure(data: pointer; I, NewI: integer);
 //Меняет местами элементы i1 и i2
  TExchangeProcI = procedure(data: pointer; i1, i2: integer);



function LinSearch(Data: pointer; Len: integer; Item: pointer; Cmp: TItemCmpProc;
  out k: integer): Boolean;
function BinSearch(Data: pointer; Len: integer; Item: pointer; Cmp: TItemCmpProc;
  out k: integer): Boolean;

//Пузырьковая сортировка. Получает сравнивалку и перемещаловку.
procedure BubbleSort(data: pointer; len: integer; cmp: TCompareProcI; mov: TMoveProcI);

//Быстрая сортировка. Получает сравнивалку и перестанавливалку.
//Взято с http://www.vb-helper.com/dart1.htm, чуточку подправлено.
//L, R - граничные элементы, включительно.
procedure QuickSort(data: pointer; L, R: Integer; cmp: TCompareProcI; xch: TExchangeProcI);


(*
  Готовые сравнивалки.
*)

type
  TIntArray = array of integer;

//Expects Data to be TIntArray
function Cmp_IntArray_Asc(Data: pointer; I, J: integer): integer;
function Cmp_IntArray_Desc(Data: pointer; I, J: integer): integer;
procedure Mov_IntArray(Data: pointer; I, NewI: integer);
procedure Xch_IntArray(Data: pointer; i1, i2: integer);

//Expects Data to be TStringArray
function Cmp_StringArray_StrAsc(Data: pointer; I, J: integer): integer;
function Cmp_StringArray_StrDesc(Data: pointer; I, J: integer): integer;
function Cmp_StringArray_TextAsc(Data: pointer; I, J: integer): integer;
function Cmp_StringArray_TextDesc(Data: pointer; I, J: integer): integer;
procedure Mov_AnyStringArray(Data: pointer; I, NewI: integer);

//Expects Data to be TUniStringArray
function Cmp_UniStringArray_StrAsc(Data: pointer; I, J: integer): integer;
function Cmp_UniStringArray_StrDesc(Data: pointer; I, J: integer): integer;
function Cmp_UniStringArray_TextAsc(Data: pointer; I, J: integer): integer;
function Cmp_UniStringArray_TextDesc(Data: pointer; I, J: integer): integer;


implementation

function LinFindPos(data: pointer; obj: pointer; cmp: TCompareProcP; len: integer;
  out k: integer): Boolean;
var i, diff: integer;
begin
  Result := false;
  for i := 0 to Len - 1 do begin
    diff := Cmp(Data, obj, i);
    if diff<=0 then begin
      k := i;
      Result := diff=0;
      Exit;
    end;
  end;
end;

//Возвращает true, если нашли точное совпадение. В таком случае указывает на первый совпадающий элемент.
//Иначе указывает на первый элемент больше нужного.
function BinFindPos(data: pointer; obj: pointer; cmp: TCompareProcP; len: integer; out k: integer): boolean;
var k_left, k_right: integer;
  diff_val: integer;
begin
  Result := false;
  if len<=0 then exit; //Иначе потом будут странные исключительные случаи

 //Ищем бинарным поиском
  k_left := 0;
  k_right := len-1;
  k := 0;

  while k_right-k_left > 0 do begin
    k := k_left + (k_right - k_left) div 2;
    diff_val := cmp(data, obj, k);

    if diff_val < 0 then begin
      k_right := k - 1;
    end else

    if diff_val > 0 then begin
      k_left :=  k + 1;
    end else

    begin
     //Находим первый совпадающий
      repeat
        Dec(k);
      until (k < k_left) or (cmp(data, obj, k) <> 0);
      Inc(k);

      Result := true;
      exit;
    end;
  end;

 //Здесь оказываемся только если k_right=k_left (оба края включительно).
 //Проверяем последнюю клетку - указывать на неё, или на следующую?
  diff_val := cmp(data, obj, k_left);
  if diff_val < 0 then begin
    k := k_left;
    Result := false;
  end else
  if diff_val = 0 then begin
    k := k_left;
    Result := true;
  end else begin
    k := k_left+1;
    Result := false;
  end;
end;

(*
 Обратная совместимость: чуточку иные функции бинарного поиска.
*)

type
  TSearchData = record
    Data: Pointer;
    Cmp: TCompareProcP;
  end;
  PSearchData = ^TSearchData;

//Внутренний преобразователь пользовательской функции
function ItemCmpProc(Data: pointer; I: integer; Item: pointer): integer;
begin
  Result := - PSearchData(data).Cmp(PSearchData(data)^.Data, Item, I);
end;

function LinSearch(Data: pointer; Len: integer; Item: pointer; Cmp: TItemCmpProc;
  out k: integer): Boolean;
var SearchData: TSearchData;
begin
  SearchData.Data := Data;
  SearchData.Cmp := @Cmp;
  Result := LinFindPos(@SearchData, Item, @ItemCmpProc, Len, k);
end;

function BinSearch(Data: pointer; Len: integer; Item: pointer; Cmp: TItemCmpProc;
  out k: integer): Boolean;
var SearchData: TSearchData;
begin
  SearchData.Data := Data;
  SearchData.Cmp := @Cmp;
  Result := BinFindPos(@SearchData, Item, @ItemCmpProc, Len, k);
end;


//Пузырьковая сортировка. Получает сравнивалку и перемещаловку.
procedure BubbleSort(Data: pointer; Len: integer; Cmp: TCompareProcI; Mov: TMoveProcI);
var i, j: integer;
begin
  for i := 1 to Len - 1 do begin
    j := i-1;
    while (j >= 0) and (Cmp(Data, i, j) < 0) do
      Dec(j);
    Inc(j);
    if j <> i then
      Mov(data, i, j);
  end;
end;

procedure QuickSort(Data: pointer; L, R: Integer; Cmp: TCompareProcI; Xch: TExchangeProcI);
var I, J, P: Integer;
begin
  if L>R then exit;

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Cmp(Data, I, P) < 0 do
        Inc(I);
      while Cmp(Data, J, P) > 0 do
        Dec(J);
      if I <= J then
      begin
       //ExchangeItems
        if I <> J then
          Xch(Data, I, J);

        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;

    if L < J then
      QuickSort(Data, L, J, Cmp, Xch);
    L := I;
  until I >= R;
end;

function Cmp_IntArray_Asc(Data: pointer; I, J: integer): integer;
begin
  Result := TIntArray(Data)[i] - TIntArray(Data)[j];
end;

function Cmp_IntArray_Desc(Data: pointer; I, J: integer): integer;
begin
  Result := TIntArray(Data)[j] - TIntArray(Data)[i];
end;

procedure Mov_IntArray(Data: pointer; I, NewI: integer);
var tmp: integer;
begin
  tmp := TIntArray(Data)[I];
  while I > NewI do begin
    TIntArray(Data)[I] := TIntArray(Data)[I-1];
    Dec(I);
  end;
  TIntArray(Data)[NewI] := tmp;
end;

procedure Xch_IntArray(Data: pointer; i1, i2: integer);
var tmp: integer;
begin
  tmp := TIntArray(Data)[i1];
  TIntArray(Data)[i1] := TIntArray(Data)[i2];
  TIntArray(Data)[i2] := tmp;
end;



function Cmp_StringArray_StrAsc(Data: pointer; I, J: integer): integer;
begin
  Result := CompareStr(TStringArray(Data)[i], TStringArray(Data)[j]);
end;

function Cmp_StringArray_StrDesc(Data: pointer; I, J: integer): integer;
begin
  Result := CompareStr(TStringArray(Data)[j], TStringArray(Data)[i]);
end;

function Cmp_StringArray_TextAsc(Data: pointer; I, J: integer): integer;
begin
  Result := CompareText(TStringArray(Data)[i], TStringArray(Data)[j]);
end;

function Cmp_StringArray_TextDesc(Data: pointer; I, J: integer): integer;
begin
  Result := CompareText(TStringArray(Data)[j], TStringArray(Data)[i]);
end;

procedure Mov_AnyStringArray(Data: pointer; I, NewI: integer);
var tmp: integer;
  j: integer;
begin
 //Move as for IntArray: save on string reference counting
  tmp := TIntArray(Data)[I];
  for j := I downto NewI + 1 do
    TIntArray(Data)[j] := TIntArray(Data)[j-1];
  TIntArray(Data)[NewI] := tmp;
end;


function Cmp_UniStringArray_StrAsc(Data: pointer; I, J: integer): integer;
begin
 {$IF CompilerVersion >= 21}
  Result := CompareStr(TStringArray(Data)[i], TStringArray(Data)[j]);
 {$ELSE}
  Result := WideCompareStr(TWideStringArray(Data)[i], TStringArray(Data)[j]);
 {$IFEND}
end;

function Cmp_UniStringArray_StrDesc(Data: pointer; I, J: integer): integer;
begin
 {$IF CompilerVersion >= 21}
  Result := CompareStr(TStringArray(Data)[j], TStringArray(Data)[i]);
 {$ELSE}
  Result := WideCompareStr(TWideStringArray(Data)[j], TStringArray(Data)[i]);
 {$IFEND}
end;

function Cmp_UniStringArray_TextAsc(Data: pointer; I, J: integer): integer;
begin
 {$IF CompilerVersion >= 21}
  Result := CompareText(TStringArray(Data)[i], TStringArray(Data)[j]);
 {$ELSE}
  Result := WideCompareText(TWideStringArray(Data)[i], TStringArray(Data)[j]);
 {$IFEND}
end;

function Cmp_UniStringArray_TextDesc(Data: pointer; I, J: integer): integer;
begin
 {$IF CompilerVersion >= 21}
  Result := CompareText(TStringArray(Data)[j], TStringArray(Data)[i]);
 {$ELSE}
  Result := WideCompareText(TWideStringArray(Data)[j], TStringArray(Data)[i]);
 {$IFEND}
end;

end.
