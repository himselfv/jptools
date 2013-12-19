unit YarxiReadingCommon;

interface

function CombineAlternativeReading(const rd: string; const alt: string): string;

implementation
uses WcExceptions, YarxiStrings;

{ Совмещает основное чтение, производящее кану, с альтернативным - для отображения.
 Производит общее чтение с пометками собственного формата.
 rd и alt должны быть непустыми }
function CombineAlternativeReading(const rd: string; const alt: string): string;
const eBadAlternativeReading = 'Непонятный альтернативный вариант транскрипции: %s (исходный %s)';
var ps, pa, pb: PChar;

  procedure CommitText;
  begin
    if ps>pa then exit;
    Result := Result + spancopy(ps,pa);
    ps := pa;
  end;

begin
 { Яркси поддерживает только пару ха/ва }
  Check(Length(rd)=Length(alt));

  Result := '';
  pa := PChar(rd);
  pb := PChar(alt);
  ps := pa;
  while pa^<>#00 do begin
    if pa^=pb^ then begin
      Inc(pa);
      Inc(pb);
    end else
    if (pa^='h') and (pb^='w') then begin
      CommitText();
      Inc(pa);
      Inc(pb);
      Check((pa^=pb^) and (pa^='a'), eBadAlternativeReading, [alt, rd]);
      Inc(pa);
      Inc(pb);
      Result := Result + 'ha´';
      ps := pa;
    end else
      Die(eBadAlternativeReading, [alt, rd]);
  end;

  CommitText;
end;

end.
