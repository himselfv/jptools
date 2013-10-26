<?php
/*
JRDIC/YARXI - общие утилиты работы с базой Яркси.
*/
if (!defined("LEGIT_REQUEST")) 
  die ("This module cannot be called directly.");

/*
Формат: *kana*;kana**;*kana*
Если в конце две звёздочки, чтение малоупотребляемое.
Строка пустая или стоит тире - кокудзи.
*/
//Получает ASCII-строку в yarxi-onyomi формате.
//Возвращает массив из нуля (кокудзи) или более пар (string чтение, bool редкое)
function yarxi_decode_onyomi($s) {
	if (strlen($s)<=0)
		return FALSE;
	$parts = mb_split(';', $s);
	if ((count($parts)<=0) || ($parts[0]=='-'))
		return array();
	$ret=array();
	for ($i=0; $i<count($parts); $i++)
		//Мы знаем, что все результаты в форме *кана**, и не проверяем это.
		//Нас интересует только двойная звёздочка.
		if (mb_substr($parts[$i], -2)=='**')
			$ret[] = array(mb_substr($parts[$i], 1, -2), true);
		else
			$ret[] = array(mb_substr($parts[$i], 1, -1), false);
	return $ret;
}


/*
Формат:
	3*tsugu*^01129*|A
	0*oshi*
	||$ $
	!5?5*omoneru*|A/O-||$a$||$o-$
	33334*aware*awareppoi*awaremu*^42909*awaremi*kanashii*^52321
	0322* AI *itoshii**mederu** AIRASHII *

Первым идёт набор цифр, каждая определяет, сколько букв "покрывает" кандзи в соотв. чтении:
	334*aware*awareppoi*kanashii
0 означает "покрывает всё слово"

Дальше, разделённые *, идут чтения.
После каждого чтения через * может стоять ссылка в форме:
	^01129		см. также
	^42909		иначе
	^52321		чаще
После неё тоже ставится *. Последняя * не ставится.



*/
function yarxi_decode_kunyomi($s) {
}

?>