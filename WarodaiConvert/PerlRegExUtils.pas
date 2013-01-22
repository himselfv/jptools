unit PerlRegExUtils;

interface
uses PerlRegEx;

const
 //Стандартные зоны Юникода - по именам в PCRE не поддерживаются
  pHiragana = '\x{3040}-\x{309F}';
  pKatakana = '\x{30A0}-\x{30FF}';
  pCJKUnifiedIdeographs = '\x{4E00}-\x{9FFF}';
  pCJKUnifiedIdeographsExtA = '\x{3400}-\x{4DBF}';
  pCJKUnifiedIdeographsExtB = '\x{20000}-\x{2A6DF}';
  pCJKUnifiedIdeographsExtC = '\x{2A700}-\x{2B73F}';
  pCJKSymbolsAndPunctuation = '\x{3000}-\x{303F}';

type
  TPerlRegExHelper = class helper for TPerlRegEx
  public
    function HasMatches(const subj: string): boolean;
    function ReplaceMatches(const subj, repl: string): string;
    function DeleteAll(const subj: string): string;
  end;

function Regex(const s: string): TPerlRegEx;

implementation

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

end.
