unit YarxiCore;

interface
uses JWBKanaConv;

var
  KanaTran: TRomajiTranslator;

implementation
uses SysUtils;

initialization
  KanaTran := TKanaTranslator.Create;

finalization
  FreeAndNil(KanaTran);

end.
