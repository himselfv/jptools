Parses word/expression list and builds a list of cards with translations in tab-separated format sutiable for importing to [Anki](http://ankisrs.net/) or updating Anki deck. Uses EDICT/CEDICT compatible dictionary

# Syntax
```
Usage: AnkiWordList.exe <file1> [file2] ... [-flags]
Flags:
  -e EDICT          specify EDICT file (otherwise EDICT)

Input:
  -te <column>      zero-based. Take expressions from this column, if tab-separated (default is 0)
  -tr <column>      Take readings from this column, if tab-separated (default is 1, unless -tn is overriden, then no default)
  -es <text>        Expression separator. If specified, multiple ways of writing an expression can be listed.
  -rs <text>        Reading separator. By default expression separator is used.

Output:
  -o output.file    specify output file (otherwise console)
  -xml              output xml instead of the default plaintext
  -xslt <filename>  output xml converted by this XSLT schema
  -or               add both expression and reading for each entry
  -match=<mode>     if there are multiple matches:
    best            output ONE best match
    multiple        output all matches as a single result
    split           output multiple matches as multiple results
```

Requires EDICT2-compatible file in the same directory.

# Purpose
Same as with AnkiKanjiList but for words. You can create several deck fields with translations from different sources and show those which are available.

# Input

## Field selection
By default AnkiKanjiList assumes the file it receives is flat text and has one expression on each line:
```
階段
危ない
大きい
```

If the file is tab-separated, AnkiKanjiList assumes first column containes expression text and second column it's reading:
```
階段     かいだん
危ない    あぶな
大きい    おおきい
```

You can tell it to use different columns with `-te` and {{{-tr}}}.

**Note:** You may omit readings at all or just on some lines, but you have to have an expression column. If you only have readings, e.g.:
```
かいだん
あぶない
おおきい
```

Then use those readings _instead of expressions_.

## Expression/reading separators
Input file may list several versions of the reading or writing, e.g.
```
真面目、眞面目    まじめ、しんめんもく
```
In this case you can provide a separator text with `-es 、` and AnkiWordList will try all combinations of expression/reading against the dictionary. If several matches are found, they will be graded by their relevancy, the more provided readings/writings match the entry, the more relevant match it is.

**Note**: It is in general not a good idea to have multiple variants of the same expression in a single Anki card.

  1. **Do not:** List multiple unrelated words in a single card. **Instead:** Create several cards, one for each word.
  2. **Do not:** List several readings of a single expression which have different meanings. **Instead:** Create several cards, one for each reading.
  3. **Do not:** List obsolete writings or readings for an expression. **Instead**: Just don't list obsolete readings at all. This only distracts the learner with mostly pointless information.

One valid case when you may want to list several readings is when those readings are equivalent or have very similar meanings.

**What if there's still multiple expressions/readings in the same card?**

AnkiWordList will match all of them and generate a list of dictionary entries. Depending on what you asked it to do, it'll put the best one, all of them together into card, or create multiple cards, one for each separate entry. If multiple cards are created, each card will get only those expressions and readings related to it.

# Output
AnkiWordList outputs generated cards in a tab-separated format to a standard output, and errors as well as expressions which weren't found to error output.

# FAQ
**How to specify multiple dictionaries?**

There's no way, because you can just run the program multiple times with different dictionaries. Import the results into separate fields, then just show all of those from Anki.

# Download
**[AnkiWordList.exe](https://drive.google.com/open?id=0B6e6N2yLg25MeFVUbUNPNFlfdTg)**