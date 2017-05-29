Provides utility function for working with kanji list files, produced by KanjiStats or by other means.
Supports basic "ordered bag of kanji", "ordered one-kanji-one-line" and
"ordered kanji=value" formats.

## Syntax
```
KanjiList.exe <command> [additional files] [flags]
Takes a list of input files, performs some transformations and outputs the result
Supported input files:
  ordered bag of kanji
  ordered one-kanji-one-line
  ordered kanji=value
Commands:
  count = output number of entries
  clip <N/-N> = leaves only N first / all except N last kanji
  skip <N/-N> = removes N first / all except N last kanji
  pack = converts any other format to ordered bag of kanji
  merge = produces a union of unique kanji from all files
  filter = produces a union of kanji present in all files
  diff = produces a union of all kanji from input missing in any additional files
  orderof = produces a kanji-value file assigning each kanji its position number in the input file

Flags:
  -ii               ignore console input, data is to be taken from the first additional file
  -o <output file>  where to output the data, otherwise console
  -s                scan subdirectories
```

## Supported file formats
#### Bag of kanji
Text file listing only kanji without any linebreaks:
```
人一見言出子大思手分気中上彼女
```

#### One kanji one line
Text file with one kanji on every line:
```
人
一
見
言
出
```

#### Kanji=value
Text file with one kanji on every line, with a value assigned to it:
```
人=6466419
一=5455106
見=4145480
言=3679558
出=3672586
```
Values can be of any kind.


## Examples
```
KanjiList clip 1500
```
Outputs first 1500 kanji / lines from input.

```
KanjiList skip 300
```
Skips 300 kanji / lines from input, then outputs the rest.

```
KanjiList clip -300 -ii mylist.txt
```
Outputs all kanji / lines from mylist.txt except for the final 300 ones.

```
KanjiList merge -ii jlpt1-4.txt jouyou1-6.txt
```
Produces a list of all kanji either in JLPT list or in first 6 levels of Jouyou Kanji.

```
KanjiList diff -ii jouyou1-6.txt learned.txt
```
Produces a list of yet unlearned kanji from the first 6 Jouyou levels.

```
cat KanjiByFrequency.txt | KanjiList clip 1000 | KanjiList diff learned.txt
```
Produces a list of those of 1000 most frequent kanji which are not yet learned.
(Assumes proper unicode handling in console, which is beyond the scope of this article)

## Download
**[KanjiList.exe](https://drive.google.com/open?id=0B6e6N2yLg25MT3RhVWRSemY0MmM)**