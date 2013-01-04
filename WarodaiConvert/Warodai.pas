unit Warodai;

interface
uses SysUtils, UniStrUtils, StreamUtils;
{$INCLUDE 'Warodai.inc'}

var
 //Общая статистика для всех Warodai-модулей
  WarodaiStats: record
    LinesRead: integer;
    LinesTooShort: integer; //article lines shorter than 2 characters --- skipped
    Comments: integer;
    DataLines: integer; //translations, variations and examples
    GroupCommon: integer; //# of articles which had group common parts
    BlockCommon: integer;
    ExplicitCommonGroupBlocks: integer; //explicit blocks were defined in common group in article where explicit groups exist
    MultilineGroupCommon: integer; //common part of multiple groups is multiline
    MultilineBlockCommon: integer; //common part of multiple blocks is multiline
  end;

type
  EParsingException = class(Exception);

type
  TWarodaiReader = class(TCharReader)
  public
    function ReadLine(out ln: string): boolean; reintroduce;
    procedure SkipArticle;
  end;

implementation

function TWarodaiReader.ReadLine(out ln: string): boolean;
begin
  Result := inherited ReadLine(ln);
  if Result then
    Inc(WarodaiStats.LinesRead);
end;

//Проматываем текущую запись до конца
procedure TWarodaiReader.SkipArticle;
var s: string;
begin
  while ReadLine(s) and (s<>'') do begin end;
end;

initialization
  FillChar(WarodaiStats, sizeof(WarodaiStats), 0);

end.
