unit RegExUtils;

interface
uses RegularExpressions, RegularExpressionsCore, Generics.Collections;

const
 //Standard unicode zones - names not supported by PCRE
  pHiragana = '\x{3040}-\x{309F}';
  pKatakana = '\x{30A0}-\x{30FF}';
  pCJKUnifiedIdeographs = '\x{4E00}-\x{9FFF}';
  pCJKUnifiedIdeographsExtA = '\x{3400}-\x{4DBF}';
  pCJKUnifiedIdeographsExtB = '\x{20000}-\x{2A6DF}';
  pCJKUnifiedIdeographsExtC = '\x{2A700}-\x{2B73F}';
  pCJKSymbolsAndPunctuation = '\x{3000}-\x{303F}';

const
 //Useful unions
  pCJKChar = '(?:'+pHiragana+'|'+pKatakana+'|'
    +pCJKUnifiedIdeographs+'|'
    +pCJKUnifiedIdeographsExtA+'|'
    +pCJKUnifiedIdeographsExtB+'|'
    +pCJKUnifiedIdeographsExtC+'|'
    +pCJKSymbolsAndPunctuation+')';

type
  TPerlRegExHelper = class helper for TPerlRegEx
  public
    function HasMatches(const subj: string): boolean;
    function ReplaceMatches(const subj, repl: string): string;
    function DeleteAll(const subj: string): string;
  end;

  TRegexLib = class(TObjectDictionary<string, TRegex>)
  public
    constructor Create;
    function Get(const regex: string): TRegex;
    function Replace(const subj, regex, repl: string): string;
    procedure Replace2(var subj: string; const regex, repl: string);
    property Items[const Index: string]: TRegex read Get; default;
  end;

function Regex(const s: string): TPerlRegEx;

implementation
uses SysUtils, Generics.Defaults;

function Regex(const s: string): TPerlRegEx;
begin
  Result := TPerlRegEx.Create;
  Result.RegEx := UTF8String(s);
  Result.Compile;
  Result.Study;
end;

function TPerlRegExHelper.HasMatches(const subj: string): boolean;
begin
  Self.Subject := UTF8String(subj);
  Result := Match;
end;

function TPerlRegExHelper.ReplaceMatches(const subj, repl: string): string;
begin
  Self.Subject := UTF8String(subj);
  Self.Replacement := UTF8String(repl);
  if Self.ReplaceAll then
    Result := UnicodeString(Self.Subject) //after replacements
  else
    Result := subj;
end;

function TPerlRegExHelper.DeleteAll(const subj: string): string;
begin
  Result := Self.ReplaceMatches(subj,'');
end;

function _compareString(const Left, Right: string): Boolean;
begin
  Result := CompareStr(Left, Right)=0;
end;

function _hashString(const Value: string): Integer;
begin
  if Value='' then
    Result := 0
  else
    Result := BobJenkinsHash(Value[1], Length(Value)*SizeOf(Value[1]), 1234567);
end;

constructor TRegexLib.Create;
begin
  inherited Create(
    TDelegatedEqualityComparer<string>.Create(
      _compareString,
      _hashString
    )
  );
end;

function TRegexLib.Get(const regex: string): TRegex;
begin
  if not Self.TryGetValue(regex, Result) then begin
    Result := TRegex.Create(regex);
    Self.Add(regex, Result);
  end;
end;

function TRegexLib.Replace(const subj, regex, repl: string): string;
var obj: TRegex;
begin
  obj := Get(regex);
  Result := obj.Replace(subj, repl);
end;

procedure TRegexLib.Replace2(var subj: string; const regex, repl: string);
var obj: TRegex;
begin
  obj := Get(regex);
  subj := obj.Replace(subj, repl);
end;

end.
