unit AozoraParser;

interface
uses SysUtils, Classes, UniStrUtils, StreamUtils, JWBIO;

{
Aozora Bunko fragments:
  《furigana》
    text｜text 《furigana》
  <image inserts and tags>
    only allowed on newlines
  ［＃comment］
    ［＃改ページ］
    ［＃「さわり」に傍点］
}

type
  TAozoraParser = class
  protected
   //Иногда удаётся угадать, какое слово подписано
    procedure OnRuby(w, r: string); virtual;
    procedure OnTag(s: string); virtual;
    procedure OnComment(s: string); virtual;
    procedure OnChar(c: char); virtual;
    procedure OnStr(s: string);
  public
    procedure Parse(ASource: TStream);
  end;

  TAozoraStats = record
    CharCount: integer;
    RubyCount: integer;
    RubySz: integer;
    TagCount: integer;
    TagSz: integer;
    CommentCount: integer;
    CommentSz: integer;
  end;

  TAozoraStatParser = class(TAozoraParser)
  protected
    procedure OnRuby(w: string; s: string); override;
    procedure OnTag(s: string); override;
    procedure OnComment(s: string); override;
  public
    Stats: TAozoraStats;
  end;

  TAozoraStripParser = class(TAozoraStatParser)
  protected
    FWriter: TStreamEncoder;
    procedure OnChar(c: char); override;
  public
    procedure Parse(ASource: TStream; AOutput: TStream); reintroduce;
  end;

implementation

const
  MAX_PRERUBY_LENGTH = 100;
  MAX_RUBY_LENGTH = 50;
  MAX_TAG_LENGTH = 200;
  MAX_COMMENT_LENGTH = 200;

type
  TParsingMode = (pmNormal, pmPreruby, pmRuby, pmTag, pmComment);

procedure TAozoraParser.Parse(ASource: TStream);
var r: TStreamDecoder;
  c, prevchar: UniChar;
  pm: TParsingMode;

  PreRuby: string;

  LineCnt: integer;

  s: string;
  ln: string;
  i: integer;
begin
  pm := pmNormal;
  LineCnt := 0;
  s := '';
  PreRuby := '';

  r := OpenStream(ASource, {OwnsStream=}false);
  try
    while r.ReadLn(ln) do begin
      Inc(LineCnt);
      c := #00;

      for i := 1 to Length(ln) do begin
        prevchar := c;
        c := ln[i];

        if (pm in [pmNormal]) and (c='｜') then begin
          pm := pmPreRuby;
          PreRuby := '';
          continue;
        end;

        if (pm in [pmNormal, pmPreRuby]) and (c='《') then begin
          pm := pmRuby;
          s := '';
          continue;
        end;

        if (pm = pmPreRuby) then begin
          PreRuby := PreRuby + c;
          if Length(PreRuby)>MAX_PRERUBY_LENGTH then begin
            pm := pmNormal;
            writeln(ErrOutput, 'Broken pre-ruby found with no ruby @ line '+IntToStr(LineCnt));
            OnChar('｜');
            OnStr(PreRuby);
          end;
          continue;
        end;

        if (pm = pmRuby) and (c='》') then begin
          pm := pmNormal;
          OnRuby(PreRuby, s);
          PreRuby := '';
          continue;
        end;

        if (pm = pmRuby) then begin
          s := s + c;
          if Length(s)>MAX_RUBY_LENGTH then begin
            pm := pmNormal;
            OnRuby('', ''); //register as empty
            writeln(ErrOutput, 'Broken ruby found with no closing @ line '+IntToStr(LineCnt));
            if PreRuby<>'' then begin
              OnChar('｜');
              OnStr(PreRuby);
            end;
            OnChar('《'); //have to guess that was legal
            OnStr(s);
            PreRuby := '';
          end;
          continue;
        end;

        if (pm in [pmNormal]) and (c='<') and (prevchar=#00) then begin
          pm := pmTag;
          s := '';
          continue;
        end;

        if (pm = pmTag) and (c='>') then begin
          pm := pmNormal;
          OnTag(s);
          continue;
        end;

        if (pm = pmTag) then begin
          s := s + c;
          if Length(s)>MAX_TAG_LENGTH then begin
            pm := pmNormal;
            OnTag(''); //register as empty
            writeln(ErrOutput, 'Broken tag found with no closing @ line '+IntToStr(LineCnt));
            OnChar('<');
            OnStr(s);
          end;
          continue;
        end;


        if (pm in [pmNormal]) and (c='＃') and (prevchar='［') then begin
          pm := pmComment;
          s := '';
          continue;
        end;

        if (pm = pmComment) and (c='］') then begin
          pm := pmNormal;
          OnComment(s);
          continue;
        end;

        if (pm = pmComment) then begin
          s := s + c;
          if Length(s)>MAX_COMMENT_LENGTH then begin
            pm := pmNormal;
            OnComment(''); //register as empty
            writeln(ErrOutput, 'Broken comment found with no closing @ line '+IntToStr(LineCnt));
            OnStr('［＃');
            OnStr(s);
          end;
          continue;
        end;

       //Finally, more broken stuff detection
        if c='》' then
          writeln(ErrOutput, 'Broken ruby found with no opening @ line '+IntToStr(LineCnt));
       //can't test for > since it's used in comparisons and smiles
       //can't test for ］ since there could be legal ［s without ＃

        OnChar(c);
      end;
    end;

  finally
    FreeAndNil(r);
  end;
end;

procedure TAozoraParser.OnRuby(w, r: string);
begin
 //Normally we process W as text
  OnStr(w);
end;

procedure TAozoraParser.OnTag(s: string);
begin
end;

procedure TAozoraParser.OnComment(s: string);
begin
end;

procedure TAozoraParser.OnChar(c: char);
begin
end;

//Same as OnChar, only several characters
procedure TAozoraParser.OnStr(s: string);
var i: integer;
begin
  for i := 1 to Length(s) do
    OnChar(s[i]);
end;

procedure TAozoraStatParser.OnRuby(w, s: string);
begin
  Inc(Stats.RubyCount);
  Inc(Stats.RubySz, Length(s));
  inherited;
end;

procedure TAozoraStatParser.OnTag(s: string);
begin
  Inc(Stats.TagCount);
  Inc(Stats.TagSz, Length(s));
  inherited;
end;

procedure TAozoraStatParser.OnComment(s: string);
begin
  Inc(Stats.CommentCount);
  Inc(Stats.CommentSz, Length(s));
  inherited;
end;

procedure TAozoraStripParser.Parse(ASource: TStream; AOutput: TStream);
begin
  FreeAndNil(FWriter); //if we had one
  FWriter := WriteToStream(AOutput, false, TUTF16Encoding);
  inherited Parse(ASource);
end;

procedure TAozoraStripParser.OnChar(c: char);
begin
  if Assigned(FWriter) then
    FWriter.WriteChar(c);
  inherited;
end;

end.
