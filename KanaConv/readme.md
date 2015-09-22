Romaji-katakana-hiragana convertor, supports common and custom romaji schemes, including using multiple at once.

Usage:

    conv := TKanaTranslator.Create;
    conv.LoadFromFile('Hepburn.roma');
    romaji := conv.KanaToRomaji('ひらがなカタカナ');
    kana := conv.RomajiToKana('hiragana katakana', [rfDeleteInvalidChars]);

Common romaji schemes are included, see Release\*.roma.

Bopomofo translator:

    conv := TPinYinTranslator.Create;
    conv.LoadFromFile('PinYin.rpy');
    bopomofo := conv.RomajiToKana('pin3yin4');

For common pinyin schemes see Release\*.rpy.

Converting between katakana and hiragana:

   hiragana := ToHiragana(katakana);

Converting between different ways of writing pinyin tones ( pin4yin4<->pínín )

   tone2 := ConvertPinYin(tone1);