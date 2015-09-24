Any .txt file placed here will trigger testing kana/pinyin conversion table of the same name.

The table will be loaded, and tested against every line from .txt file.

File format:

    # comment
    romaji = expected hiragana output
    Kromaji = expected katakana output
    hiragana/katakana = expected romaji output

Add complicated cases of misconversion which you encounter in the wild.

Q: How do I test multi-table conversions (e.g. Hepburn with Kunreishiki backup)?
A: There's no testing this. There can be no "reference results", the rules would be too binding. The lib only establishes how a single table works; with multiple whatever happens, happens.