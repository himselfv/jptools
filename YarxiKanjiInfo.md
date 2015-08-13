Similar to AnkiKanjiList. Takes a list of kanji, produces a tab-separated file listing Yarxi information for each kanji, suitable for importing to Anki.

[Yarxi](http://www.susi.ru/yarxi/) is an application and a kanji database popular in Russia.

This application requires yarxi.db which you can take from Yarxi once you have installed and run it once. It will be either in the application folder, or in your profile folder, in AppData\Roaming\Yarxi.

**Note that you CANNOT use yarxi.db or ANYTHING produced by this tool from yarxi.db anywhere but for personal use without explicit permission from Yarxi developer.**

**License given for YarxiKanjiInfo tool DOES NOT cover yarxi.db or anything produced by this tool from it.**

That said, Yarxi author is known for being benevolent and generously allowing the use of his database in some cases, so you may try your luck and contact him.

## Syntax
```
YarxiKanjiInfo.exe <file1> [file2] ... [-flags]
Flags:
  -o output.file    specify output file (otherwise console)
  -t 0/1/2          paste readings up to this rarity (default is 0)
  -ts               use html <s> tag for rare readings
```

At this time the only piece of information the tool extracts is the "nickname" portion of the kanji.


## Examples
```
YarxiKanjiInfo jlpt1.txt -t 0 -o jlpt1-annotated.txt
```
Produces the list of all JLPT1 kanji annotated with their russian nicknames taken from Yarxi.

The resulting file may be imported into Anki.


## Usage
Since the scope of information produced by this tool is insufficient for generating a full card for Anki deck, it is generally used to imbue existing deck with additional data from Yarxi:

1. Export the kanji deck into a text file from Anki.

2. Leave only the kanji column and cut the rest (for instance with Excel or any TSV editor)

3. Pack the kanji into the bag-of-kanji format (see and maybe use KanjiList tool).

4. Pass it to YarxiKanjiInfo, which produces a TSV file assigning Yarxi info to each kanji.

5. Maybe convert it to UTF-8 no BOM, to help Anki read it.

6. Create additional fields in your Anki deck for every piece of information you want to add to the deck from Yarxi.

7. Import the file into Anki, matching the notes by the kanji field, and updating the fields you've just created.


## Notes
As the tool is in the experimental stage it may produce informational messages on the standard error console. So long as there are no errors they are safe to ignore.

## Download
**[YarxiKanjiInfo.exe](http://googledrive.com/host/0B6e6N2yLg25MTlp3WkpfbG9ySGM/YarxiKanjiInfo.exe)**