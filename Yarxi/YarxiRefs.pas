unit YarxiRefs;
{ “аблицы соответстви€ номеров статей в базе знакам кандзи и словам.
 «агружаютс€ целиком прежде любого другого разбора, т.к. уже требуютс€ во врем€
 него. }

interface
uses BalancedTree;

type
  TIndexEntry = class(TBinTreeItem)
    FID: integer;
    FText: string;
    function CompareData(const a):Integer; override;
    function Compare(a:TBinTreeItem):Integer; override;
    procedure Copy(ToA:TBinTreeItem); override;
  end;

  TArticleIndex = class(TBinTree)
  public
    procedure Add(const AId: integer; AText: string); reintroduce;
    function Get(const AId: integer; out AText: string): boolean;
  end;

var
  KanjiRefs: TArticleIndex;

function getKanji(const AId: integer; out AText: string): boolean; inline;

{ ѕотом можно сделать аналогичный индекс и дл€ танго, но танго ещЄ нужно само
 составить из кандзи и каны }

implementation
uses SysUtils;

function TIndexEntry.CompareData(const a):Integer;
begin
  Result := integer(a)-Self.FID;
end;

function TIndexEntry.Compare(a:TBinTreeItem):Integer;
begin
  Result := TIndexEntry(a).FID-Self.FID;
end;

procedure TIndexEntry.Copy(ToA:TBinTreeItem);
begin
  TIndexEntry(ToA).FID := FID;
  TIndexEntry(ToA).FText := FText;
end;

procedure TArticleIndex.Add(const AId: integer; AText: string);
var item: TIndexEntry;
begin
  item := TIndexEntry.Create;
  item.FID := AId;
  item.FText := AText;
  inherited Add(item);
end;

function TArticleIndex.Get(const AId: integer; out AText: string): boolean;
var item: TIndexEntry;
begin
  item := TIndexEntry(Self.SearchData(AId));
  Result := item<>nil;
  if Result then
    AText := item.FText;
end;

function getKanji(const AId: integer; out AText: string): boolean;
begin
  Result := KanjiRefs.Get(AId, AText);
end;

initialization
  KanjiRefs := TArticleIndex.Create;

finalization
  FreeAndNil(KanjiRefs);

end.

