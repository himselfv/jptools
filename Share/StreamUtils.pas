unit StreamUtils;
{$WEAKPACKAGEUNIT ON}

interface
uses SysUtils, Classes, CodepageUtils, UniStrUtils, Windows;

type
 (*
   Расширяет класс TStream полезными методами.
   Сами по себе методы чтения строк не очень эффективны, так что рекомендуется
   использовать только на быстрых потоках (MemoryStream, отражённый в память файл).
 *)
  TStreamHelper = class helper for TStream
  public
    function ReadAnsiChar(out c: AnsiChar): boolean;
    function ReadWideChar(out c: WideChar): boolean;
    function ReadAnsiLine: AnsiString;
    function ReadUniLine: UniString; //assumes Windows UTF-16
    function WriteAnsiChar(c: AnsiChar): boolean;
    function WriteWideChar(c: WideChar): boolean;
    procedure WriteAnsiString(s: AnsiString);
    procedure WriteUniString(s: UniString);
  end;

(*
  Кешируемые читалка-писалка.
*)

const
  DEFAULT_CACHE_SIZE = 4096;

type
  TStreamReader = class(TObject)
  protected
    Stream: TStream;
    OwnStream: boolean;

  protected
    Cache: pbyte;
    CacheSize: integer;
    CachePtr: pbyte;
    CacheRem: integer;
    procedure UpdateCache;
    function ReadFromCache(var Buffer; Size: integer): integer;

  public
    constructor Create(AStream: TStream; AOwnStream: boolean = false);
    destructor Destroy; override;
    function Read(var Buffer; Size: integer): integer;
    function Peek(var Buffer; Size: integer): integer;
  end;

  TStreamWriter = class(TObject)
  protected
    Stream: TStream;
    OwnStream: boolean;

  protected
    Cache: pbyte;
    CacheSize: integer;
    CacheUsed: integer;
    CachePtr: pbyte;

  public
    constructor Create(AStream: TStream; AOwnStream: boolean = false);
    destructor Destroy; override;
    function Write(const Buffer; Size: integer): integer; overload;
    function Write(const Buffer): integer; overload;

    procedure FlushCache;
  end;

(*
  Кешируемые чтение-запись по одному символу.
*)

const
  BOM_UTF16LE = WideChar($FEFF);
  BOM_UTF16BE = WideChar($FFFE);
  BOM_UTF8 = WideChar($BBEF); //it's EF BB BF

type
  TCharSet = (csAnsi, csUtf16Le, csUtf16Be);

type
  TCharReader = class(TStreamReader)
  protected
    Charset: TCharSet;
    function DetectCharset: TCharset;

  public
   //Create with autodetected encoding
    constructor Create(AStream: TStream; AOwnStream: boolean = false); overload;
   //Create with specified encoding (if you don't want auto, set to your default one)
    constructor Create(AStream: TStream; ACharSet: TCharSet;
      AOwnStream: boolean = false); overload;
    function ReadChar(var c: WideChar): boolean;
    function PeekChar(var c: WideChar): boolean;
    function ReadLine(out s: UniString): boolean;
  end;

  TCharWriter = class(TStreamWriter)
  protected
    Charset: TCharset;

  public
   //Create with default encoding (UTF16LE)
    constructor Create(AStream: TStream; AOwnStream: boolean = false); overload;
   //Create with specified encoding
    constructor Create(AStream: TStream; ACharSet: TCharSet; AOwnStream: boolean = false); overload;
    destructor Destroy; override;
    procedure WriteBom;
    procedure WriteChar(const c: WideChar);
    procedure WriteChars(c: PWideChar; len: integer);
    procedure WriteString(c: UniString);
  end;

implementation

function TStreamHelper.ReadAnsiChar(out c: AnsiChar): boolean;
begin
  Result := Self.Read(c, SizeOf(c)) = SizeOf(c);
end;

function TStreamHelper.ReadWideChar(out c: WideChar): boolean;
begin
  Result := Self.Read(c, SizeOf(c)) = SizeOf(c);
end;

function TStreamHelper.ReadAnsiLine: AnsiString;
var c: AnsiChar;
begin
  Result := '';
  while ReadAnsiChar(c) and (c <> #13) and (c <> #10) do
    Result := Result + c;
end;

function TStreamHelper.ReadUniLine: UniString;
var c: UniChar;
begin
  Result := '';
  while ReadWideChar(c) and (c <> #13) and (c <> #10) do
    Result := Result + c;
end;

function TStreamHelper.WriteAnsiChar(c: AnsiChar): boolean;
begin
  Result := Self.Write(c, SizeOf(c))=SizeOf(c);
end;

function TStreamHelper.WriteWideChar(c: WideChar): boolean;
begin
  Result := Self.Write(c, SizeOf(c))=SizeOf(c);
end;

(*
 Бросает исключение, если записать строку целиком не удалось.
 Функции, которая возвращала бы false или число записанных символов, нету,
 поскольку мы не можем сказать, сколько символов записано - пишутся байты.
 А возвращать число записанных байт - слишком неудобно для клиента и может
 вызвать недопонимание, а значит ошибки.
 Если готовы писать по байтам - просто пишите стандартным Write(var Buffer).
*)
procedure TStreamHelper.WriteAnsiString(s: AnsiString);
begin
  if Write(s[1], Length(s)*SizeOf(AnsiChar)) <> Length(s)*SizeOf(AnsiChar) then
    raise Exception.Create('Cannot write a line to '+self.ClassName);
end;

procedure TStreamHelper.WriteUniString(s: UniString);
begin
  if Write(s[1], Length(s)*SizeOf(WideChar)) <> Length(s)*SizeOf(WideChar) then
    raise Exception.Create('Cannot write a line to '+self.ClassName);
end;

constructor TStreamReader.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create;
  Stream := AStream;
  OwnStream := AOwnStream;

  CacheSize := DEFAULT_CACHE_SIZE;
  GetMem(Cache, CacheSize);

 //No data loaded
  CacheRem := 0;
end;

destructor TStreamReader.Destroy;
begin
  FreeMem(Cache);
  Cache := nil;

  if OwnStream then
    FreeAndNil(Stream)
  else
    Stream := nil;
  inherited;
end;

//Сдвигает оставшиеся данные в начало кэша и докачивает ещё кусочек.
procedure TStreamReader.UpdateCache;
var DataPtr: Pbyte;
begin
 //Полная перезагрузка кэша
  if CacheRem = 0 then begin
    CacheRem := Stream.Read(Cache^, CacheSize);
    CachePtr := Cache;
    exit;
  end;

 //Частичная перезагрузка
  Move(CachePtr^, Cache^, CacheRem);
  CachePtr := Cache;

  DataPtr := PByte(cardinal(Cache) + cardinal(CacheSize));
  CacheRem := CacheRem + Stream.Read(DataPtr^, CacheSize - CacheRem)
end;

//Читает из кэша. Возвращает сколько просили, или сколько было в кэше.
function TStreamReader.ReadFromCache(var Buffer; Size: integer): integer;
begin
  if Size <= CacheRem then begin
    Move(CachePtr^, Buffer, Size);
    Inc(CachePtr, Size);
    Dec(CacheRem, Size);
    Result := Size;
    exit;
  end;

  Move(CachePtr^, Buffer, CacheRem);
  Result := CacheRem;
  CacheRem := 0;
 //CachePtr неважен
end;

function TStreamReader.Read(var Buffer; Size: integer): integer;
var Buf: PByte;
begin
  Result := ReadFromCache(Buffer, Size);
  if (Result = Size) then exit;

 //Если не всё прочлось из кэша
  Buf := PByte(cardinal(@Buffer) + cardinal(Result));
  Dec(Size, Result);

  UpdateCache;
  Result := Result + ReadFromCache(Buf^, Size);
end;

function TStreamReader.Peek(var Buffer; Size: integer): integer;
begin
 //Если данные уже есть в кэше, всё просто.
  if (Size <= CacheRem) then begin
    Move(CachePtr^, Buffer, Size);
    Result := Size;
    exit;
  end;

 //Иначе, если данные в принципе влезают в кэш, обновляем его и читаем.
  if (Size <= CacheSize) then begin
    UpdateCache;

   //После обновления данные целиком => читаем
    if (Size <= CacheRem) then begin
      Move(CachePtr^, Buffer, Size);
      Result := Size;
      exit;
    end;

   //Не целиком => читаем всё, что есть.
    Move(CachePtr^, Buffer, CacheRem);
    Result := CacheRem;
  end

 //Данные в принципе не влезают в кэш, ну тут уж никакого Peek
  else begin
    Result := 0;
    exit;
  end;
end;


constructor TStreamWriter.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create;
  Stream := AStream;
  OwnStream := AOwnStream;

  CacheSize := DEFAULT_CACHE_SIZE;
  GetMem(Cache, CacheSize);
  CachePtr := Cache;

 //No writes cached
  CacheUsed := 0;
end;

destructor TStreamWriter.Destroy;
begin
 //If something remains in cache, flush it.
  FlushCache;
  FreeMem(Cache);
  Cache := nil;

  if OwnStream then
    FreeAndNil(Stream)
  else
    Stream := nil;
  inherited;
end;

function TStreamWriter.Write(const Buffer; Size: integer): integer;
var CacheLeft: integer;
begin
  CacheLeft := CacheSize - CacheUsed;

 //Если влезает в кэш, кладём туда
  if Size <= CacheLeft then begin
    Move(Buffer, CachePtr^, Size);
    Inc(CacheUsed, Size);
    Inc(CachePtr, Size);
    Result := Size;
    exit;
  end;

 //Если влезает в "остаток текущего + новый кэш"
  if Size <= CacheLeft + CacheSize then begin
   //...дописываем и сбрасываем остаток текущего
    Move(Buffer, CachePtr^, CacheLeft);
    CacheUsed := CacheSize;
    FlushCache;

   //...а всё не поместившееся кладём в новый кэш
    Move(PByte(cardinal(@Buffer) + cardinal(CacheLeft))^, CachePtr^, Size - CacheLeft);
    Inc(CacheUsed, Size - CacheLeft);
    Inc(CachePtr, Size - CacheLeft);
    Result := Size;
    exit;
  end;

 //Если совсем уж никак не влезает, то сбрасываем кэш, а потом пишем блок целиком.
  FlushCache;
  Result := Stream.Write(Buffer, Size);
end;

function TStreamWriter.Write(const Buffer): integer;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

procedure TStreamWriter.FlushCache;
begin
  if CacheUsed > 0 then begin
    Stream.Write(Cache^, CacheUsed);
    CacheUsed := 0;
    CachePtr := Cache;
  end;
end;

function SwapChar(c: WideChar): WideChar; inline;
begin
  Result := WideChar(byte(c) shl 8 + word(c) shr 8);
end;

procedure SwapCharsIn(s: PWideChar; len: integer); inline;
var i: integer;
begin
  for i := 0 to len - 1 do begin
    s^ := SwapChar(s^);
    Inc(s);
  end;
end;

function SwapChars(s: PWideChar; len: integer): WideString;
var i: integer;
begin
  SetLength(Result, len);
  for i := 0 to len - 1 do begin
    Result[i] := SwapChar(s^);
    Inc(s);
  end;
end;


constructor TCharReader.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create(AStream, AOwnStream);
  Charset := DetectCharset;
end;

constructor TCharReader.Create(AStream: TStream; ACharSet: TCharSet;
  AOwnStream: boolean = false);
begin
  inherited Create(AStream, AOwnStream);
  Charset := ACharset;
end;

//Детектируем бом и сразу вычитываем.
function TCharReader.DetectCharset: TCharset;
var Bom: WideChar;
begin
 //No data => ANSI
  if Peek(Bom, 2) < 2 then
    Result := csAnsi
  else

  if Bom = BOM_UTF16LE then begin
    Result := csUtf16Le;
    Read(Bom, 2);
  end else

  if Bom = BOM_UTF16BE then begin
    Result := csUtf16Be;
    Read(Bom, 2);
  end else

  if Bom = BOM_UTF8 then begin
    raise Exception.Create('CharReader: UTF8 signature detected, presently not supported.');
  end else

 //No BOM => ANSI
  Result := csAnsi;
end;

function TCharReader.ReadChar(var c: WideChar): boolean;
var _c: AnsiChar;
begin
  case Charset of
    csAnsi: begin
      Result := (Read(_c, 1) = 1);
      c := ToWideChar(_c, CP_ACP);
    end;

    csUtf16Be: begin
      Result := (Read(c, 2) = 2);
      c := SwapChar(c);
    end;

  else //Utf16Le
    Result := (Read(c, 2) = 2);
  end;
end;

function TCharReader.PeekChar(var c: WideChar): boolean;
var _c: AnsiChar;
begin
  case Charset of
    csAnsi: begin
      Result := (Peek(_c, 1) = 1);
      c := ToWideChar(_c, CP_ACP);
    end;

    csUtf16Be: begin
      Result := (Peek(c, 2) = 2);
      c := SwapChar(c);
    end;

  else //Utf16Le
    Result := (Peek(c, 2) = 2);
  end;
end;

(*
  Читает строку из потока, возвращает true, если удалось.
*)
function TCharReader.ReadLine(out s: UniString): boolean;
var c: UniChar;
begin
  s := '';
  Result := ReadChar(c);
  if not Result then exit; {больше ничего нет}

 {иначе строчка точно есть}
  while Result do begin
    if c=#13 then begin
      if PeekChar(c) and (c=#10) then
        ReadChar(c);
      break;
    end else
    if c=#10 then begin
      if PeekChar(c) and (c=#13) then
        ReadChar(c);
      break;
    end else
      s := s + c;
    Result := ReadChar(c);
  end;
  Result := true; {уж что-то мы прочитали}
end;

constructor TCharWriter.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create(AStream, AOwnStream);
  Charset := csUtf16Le;
end;

constructor TCharWriter.Create(AStream: TStream; ACharSet: TCharSet;
  AOwnStream: boolean = false);
begin
  inherited Create(AStream, AOwnStream);
  CharSet := ACharSet;
end;

destructor TCharWriter.Destroy;
begin
  FlushCache;
  FreeMem(Cache);
  Cache := nil;
  inherited;
end;

procedure TCharWriter.WriteChar(const c: WideChar);
var _c: AnsiChar;
  _c_be: WideChar;
begin
  case Charset of
    csAnsi: begin
      _c := ToChar(c, CP_ACP);
      Write(_c, 1);
    end;

    csUtf16Le:
      Write(c, 2);

    csUtf16Be: begin
     //Swap bytes
      _c_be := SwapChar(c);
      Write(_c_be, 2);
    end;
  end;
end;

procedure TCharWriter.WriteChars(c: PWideChar; len: integer);
var _c: AnsiString;
  _c_be: WideString;
begin
  case Charset of
    csAnsi: begin
      _c := BufToString(c, len, CP_ACP);
      Write(_c[1], Length(_c)*SizeOf(AnsiChar));
    end;

    csUtf16Le:
      Write(c^, len*SizeOf(WideChar));

    csUtf16Be: begin
      _c_be := SwapChars(c, len);
      Write(_c_be[1], len*SizeOf(WideChar));
    end;
  end;
end;

procedure TCharWriter.WriteString(c: UniString);
begin
  WriteChars(@c[1], Length(c));
end;

procedure TCharWriter.WriteBom;
var Bom: WideChar;
begin
  case Charset of
   //Nothing to write
    csAnsi: begin end;

    csUtf16Le:
    begin
      Bom := BOM_UTF16LE;
      Write(Bom, SizeOf(Bom));
    end;

    csUtf16Be:
    begin
      Bom := BOM_UTF16BE;
      Write(Bom, SizeOf(Bom));
    end;
  end;
end;

end.
