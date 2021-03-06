Parses text files (books) and builds a list of used kanji by frequency. It's possible to exclude already known kanjis from the list.

## Syntax
```
kanjistats.exe <file1> [file2] [...] [-k known_file] [-o output_file] [-v]
file*  File/directory name or mask.
-s     Include subdirectories.
-k     Kanji from this file will be considered learned, and ignored.
-o     Output file. If none specified, outputs to console.
-v     Enable verbose output (kanji=occurences).
```

## Purpose
Vocabularies of different authors differ greatly and generic "kanji frequency lists" found on the internet are often built from newspaper articles, mostly of political and economical profile. Not much help when you want to read this one specific book which has nothing to do with economy!

Feed the book to this tool and you'll get exactly the kanjis you need to learn to quickly improve your understanding of the text.

## Note
Encoding auto-detection usually works very well, but if you want to be 100% sure, include byte-order marks.

## Download
**[KanjiStats.exe](https://drive.google.com/open?id=0B6e6N2yLg25MSlZvNVpDOVJuQzg)**

## See also
[kanjistats\_4Gb](kanjistats_4Gb) - kanji frequency statistics generated by KanjiStats after processing roughly 21000 of general audience books.