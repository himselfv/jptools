This set contains several command-line tools intended to help those studying kanji and Japanese language, and some libraries/units for Delphi to parse/write file formats commonly encountered when working with Japanese text.


# AnkiList
These tools generate tab-separated files suitable for importing into Anki or updating some fields of your Anki deck.

  * [AnkiKanjiList](docs/AnkiKanjiList) - converts raw kanji list to tab-separated list with ons/kuns/meanings (uses KANJIDIC compatible dictionary).

  * [AnkiWordList](docs/AnkiWordList) - converts word/expression list to tab-separated list of words and translations (uses EDICT/CEDICT compatible dictionary)

  * [AnkiExampleList](docs/AnkiList) - converts word/expression list to tab-separated list of words and example sentences (uses Tanaka corpus compatible corpus)


# [Warodai Convertor](Warodai)

Converts [Warodai](http://e-lib.ua/dic/) (big classical JP->RU dictionary) into EDICT2 and JMDict-style dictionaries which you can use in any program.

The translation is far from perfect yet but it **does** work in a way, and the resulting dictionary is usable with more than 100 000 entries.

How to use:

  * Download [warodai in a TXT format](http://e-lib.ua/dic/download/)
  * Download WarodaiConv
  * Run WarodaiConv

Or download converted dictionary:

  * [EDICT1 format](http://googledrive.com/host/0B0jSbSrihj-ySFZVdV9lem05cmc/warodai.edict1.zip)

  * **[EDICT2 format](http://googledrive.com/host/0B0jSbSrihj-ySFZVdV9lem05cmc/warodai.edict2.zip)** (recommended)

  * [JMDict XML format](http://googledrive.com/host/0B0jSbSrihj-ySFZVdV9lem05cmc/warodai.jmdict.zip)


# Miscellaneous

These tools may be usable by itself or as an example when working with their underlying libraries.

  * [KanjiStats](docs/KanjiStats): list kanji by frequency in a given text.

  * **[kanjistats\_4Gb](docs/kanjistats_4Gb)**: kanji sorted by frequency, as they appeared in 21000 of books in Japanese

  * [KanjiList](docs/KanjiList): manipulate kanji lists (trim/merge/intersect/etc)

  * [AozoraTxt](docs/AozoraTxt): strips Aozora-Ruby from the text or gives some statistical info about it.

  * [MiscTxt](docs/MiscTxt): gives some common statistical info about a text (# of kana, kanji, char and line count)

  * [YarxiKanjiInfo](docs/YarxiKanjiInfo): uses Yarxi database parser to extract kanji information.


# Libraries

Libraries in Delphi for common CJK-related tasks.

  * [JWBIO](/himselfv/jp-tools/src/tip/Share/JWBIO.pas) - fast stream reader/writer with encoding detection and a bunch of encodings out of the box, including JIS/Shift-JIS, GB, UTF16/8 and other common japanese ones.
  * [KanjidicReader](/himselfv/jp-tools/src/tip/Share/KanjiDicReader.pas): KANJIDIC style dictionary parser + [basic in-memory representation](/himselfv/jp-tools/src/tip/Share/KanjiDic.pas) ("load and use")
  * [EdictReader](/himselfv/jp-tools/src/tip/Share/EdictReader.pas): EDICT/CCEDICT dictionary format parser (very forgiving to deviations in formats) +  [in-memory representation](/himselfv/jp-tools/src/tip/Share/Edict.pas)
  * [EdictWriter](/himselfv/jp-tools/src/tip/Share/EdictWriter.pas) - programmer friendly EDICT1/EDICT2/JMDICT file generator.
  * [AozoraTxt](/himselfv/jp-tools/src/tip/AozoraTxt/) parser: - parses text files in Aozora Bunko format
  * [UnihanReader](/himselfv/jp-tools/src/tip/Share/UnihanReader.pas) - simple [Unihan database](http://unicode.org/charts/unihan.html) parser.
  * KanaConv - romaji-katakana-hiragana conversions, supports common and custom romaji schemes, using multiple at once --- not yet moved here from Wakan project.
  * YarxiReader



# Downloads

**[Latest jptools.zip](https://docs.google.com/uc?export=download&id=0B0jSbSrihj-yOXVOcnZ2ekRQYlE) (AnkiKanjiList/WordList, AozoraTxt, KanjiStats and more)**

**[All downloads](https://drive.google.com/folderview?id=0B0jSbSrihj-ya0NOdEFiZmRTNkk&usp=sharing#list)**



# Building

May be required for some projects:
 * Wakan
 * SQLite3.pas
 * SQLite3Dataset.pas

At runtime:
 * sqlite3.dll
 * EDICT2
 * kanjidic
 * radkfile
 * ewarodai.txt
 * yarxi.db