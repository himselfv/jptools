unit StreamUtils;
{$WEAKPACKAGEUNIT ON}
(*
  Набор классов для облегчения чтения-записи в потоках,
  кеширующего чтения и т.п.
*)
{TODO: Объединить StreamReader/Writer и StreamReader2/Writer2}

interface
uses SysUtils, Classes, Graphics, UniStrUtils, Windows;

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

(*
 A class to speed up reading from low latency streams (usually file streams).

 Basically, each time you request something from file stream, it reads
 the requested data from file. Even with all the drive caches on, it's
 a kernel-mode operation nevertheless.

 This class tries to overcome the problem by reading data in large chunks.
 Whenever you request your byte or two, it reads the whole chunk and stores
 it in local memory. Next time you request another two bytes you'll get
 them right away, because they're already here.

 Please remember that you CAN'T use StreamReader on a Stream "for a while".
 The moment you read your first byte through StreamReader the underlying
 stream is NOT YOURS ANYMORE. You shouldn't make any reads to the Stream
 except than through StreamReader.
*)

const
  DEFAULT_CHUNK_SIZE = 4096;

type
  TStreamReader2 = class(TObject)
  protected
    FStream: TStream;
    FOwnStream: boolean;
    FBytesRead: int64;
  public
    property BytesRead: int64 read FBytesRead;

  protected
    flag_reallocbuf: boolean;
    FChunkSize: integer;
    procedure NewChunk;
    procedure SetChunkSize(AValue: integer);

  protected
    buf: pbyte;
    ptr: pbyte;
    adv: integer;
    rem: integer; //число оставшихся байт. Нужно, поскольку можно не прочесть весь Chunk.
    procedure ResetBuffer;
    function ReallocBuf(size: integer): boolean;
    procedure FreeBuf;

  public
    constructor Create(AStream: TStream; AOwnStream: boolean = false);
    destructor Destroy; override;
    procedure JoinStream(AStream: TStream; AOwnsStream: boolean = false);
    procedure ReleaseStream;
    function Seek(Offset: int64; Origin: TSeekOrigin): int64;
    function Read(buf: pbyte; len: integer): integer;
    function PeekByte(out b: byte): boolean;
    property ChunkSize: integer read FChunkSize write SetChunkSize;
    function Position: integer;

  protected
   //These are used for ReadStr. Instead of reallocating temporary pchar
   //every time, we just keep this buffer up while alive.
    tmpstr_ansi_s: PAnsiChar;
    tmpstr_ansi_sz: cardinal;
    tmpstr_wide_s: PWideChar;
    tmpstr_wide_sz: cardinal;
    procedure FreeTmpStr;
  public
    function TryReadLineA(var s: PAnsiChar; var sz: cardinal): boolean;
    function TryReadLineW(var s: PWideChar; var sz: cardinal): boolean;
    procedure ReadLineA(var s: PAnsiChar; var sz: cardinal);
    procedure ReadLineW(var s: PWideChar; var sz: cardinal);
    function TryReadStrA(out s: AnsiString): boolean;
    function TryReadStrW(out s: WideString): boolean;
    function ReadStrA: AnsiString;
    function ReadStrW: WideString;

  public
    function ReadAnsiChar(out c: AnsiChar): boolean;
    function ReadWideChar(out c: WideChar): boolean;
    function ReadUtf8Char(out c: WideChar): boolean;
  end;

type
  TStreamWriter2 = class(TObject)
  protected
    FStream: TStream;
    FOwnStream: boolean;
    FBytesWritten: int64;
  public
    property BytesWritten: int64 read FBytesWritten;

  protected
    flag_reallocbuf: boolean;
    FChunkSize: integer;
    procedure SetChunkSize(AValue: integer);

  protected
    buf: pbyte;
    ptr: pbyte;
    used: integer; //использованная длина буфера
    rem: integer;  //оставшаяся длина буфера == (ChunkSize - used)
    procedure FlushBuffer;
    procedure ResetBuffer;
    procedure FreeBuf;

  public
    constructor Create(AStream: TStream; AOwnStream: boolean = false);
    destructor Destroy; override;
    procedure JoinStream(AStream: TStream; AOwnsStream: boolean = false);
    procedure ReleaseStream;
    function Seek(Offset: int64; Origin: TSeekOrigin): int64;
    function Write(buf: pbyte; len: integer): integer;
    property ChunkSize: integer read FChunkSize write SetChunkSize;
    function Position: integer;
    
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
    Result[i+1] := SwapChar(s^);
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


////////////////////////////////////////////////////////////////////////////////
///  StreamReader2/Writer2

constructor TStreamReader2.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create;
  FStream := AStream;
  FOwnStream := AOwnStream;

  buf := nil;
  ptr := nil;
  adv := 0;
  rem := 0;
  SetChunkSize(DEFAULT_CHUNK_SIZE);
end;

destructor TStreamReader2.Destroy;
begin
  FreeTmpStr;
  FreeBuf;

  if Assigned(FStream) then
    if FOwnStream then
      FreeAndNil(FStream)
    else FStream := nil;

  inherited;
end;

procedure TStreamReader2.JoinStream(AStream: TStream; AOwnsStream: boolean = false);
begin
 //Освобождаем старый поток
  ReleaseStream;

 //Цепляемся к потоку
  FStream := AStream;
  FOwnStream := AOwnsStream;

 //Сбрасываем буфер на всякий случай
  ResetBuffer;
end;

//Освобождает поток, синхронизируя его положение с ожидаемым
procedure TStreamReader2.ReleaseStream;
begin
  if FStream=nil then exit;

 //Скроллим назад (треубется поддержка Seek!)
  FStream.Seek(int64(-adv), soCurrent);

 //Отпускаем поток
  FStream := nil;
  FOwnStream := false;

 //Очищаем буфер
  ResetBuffer;
end;

function TStreamReader2.Seek(Offset: int64; Origin: TSeekOrigin): int64;
begin
  Result := FStream.Seek(Offset, Origin);
  ResetBuffer;
end;

function TStreamReader2.Read(buf: pbyte; len: integer): integer;
begin
  if len=0 then begin
    Result := 0;
    exit;
  end;

 //The most common case
  if (len <= rem) then begin
    Move(ptr^, buf^, len);
    Inc(ptr, len);
    Inc(adv, len);
    Dec(rem, len);

    Inc(FBytesRead, len);
    Result := len;
    exit;
  end;

 //Read first part
  if (rem > 0) then begin
    Move(ptr^, buf^, rem);
    Inc(ptr, rem);
    Inc(adv, rem);

   //Update variables
    Inc(buf, rem);
    Dec(len, rem);

    Result := rem;
    rem := 0;
  end else
    Result := 0;

 //Download the remaining part

 //If it's smaller than a chunk, read the whole chunk
  if (len < FChunkSize) then begin
    NewChunk;

    if (rem < len) then //rem уже обновился в NewChunk
      len := rem;

    Move(ptr^, buf^, len);
    Inc(ptr, len);
    Inc(adv, len);
    Dec(rem, len);

    Inc(Result, len);
  end else
   //Else just read it from stream
    Result := Result + FStream.Read(buf^, len);

  Inc(FBytesRead, Result);    
end;

//Уж один-то байт мы всегда можем подсмотреть, если в файле осталось.
function TStreamReader2.PeekByte(out b: byte): boolean;
begin
 //Если буфер непуст, берём первый байт
  if rem >= 0 then begin
    b := ptr^;
    Result := true;
    exit;
  end;

 //Иначе буфер пуст. Качаем следующий кусочек.
  NewChunk;

  if rem >= 0 then begin
    b := ptr^;
    Result := true;
  end else
    Result := false;
end;

procedure TStreamReader2.SetChunkSize(AValue: integer);
begin
  FChunkSize := AValue;

 //If we cant' realloc right now, set delayed reallocation
  if not ReallocBuf(FChunkSize) then
    flag_reallocbuf := true;
end;

procedure TStreamReader2.NewChunk;
begin
 //Delayed reallocation
  if flag_reallocbuf then begin
    ReallocBuf(FChunkSize);
    flag_reallocbuf := false;
  end;

  rem := FStream.Read(buf^, FChunkSize);
  adv := 0;
  ptr := buf;
end;

//Сбрасывает содержимое буфера
procedure TStreamReader2.ResetBuffer;
begin
  adv := 0;
  rem := 0;
  ptr := buf;
end;

function TStreamReader2.ReallocBuf(size: integer): boolean;
begin
 //We cant decrease buffer size cause there's still data inside.
  if (adv + rem > size) then begin
    Result := false;
    exit;
  end;

  ReallocMem(buf, Size);
  ptr := pointer(integer(buf) + adv);
  Result := true;
end;

procedure TStreamReader2.FreeBuf;
begin
  if Assigned(buf) then
    FreeMem(buf);
end;

//Use this instead of underlying Stream's Position
function TStreamReader2.Position: integer;
begin
  Result := FStream.Position - rem;
end;

////////////////////////////////////////////////////////////////////////////////
///  Misc useful read functions

//Reads everything up to #13#10 into the buffer + sets terminating NULL.
//If the buffer length in chars (received in "sz") is enough, it's left as it is.
//Else the buffer is expanded, and the "sz" is updated accordingly.
function TStreamReader2.TryReadLineA(var s: PAnsiChar; var sz: cardinal): boolean;
var c: AnsiChar;
  l_used: cardinal;
begin
  l_used := 0;

  while (Read(@c, SizeOf(c))=SizeOf(c))
    and (c <> #13) do
  begin
   //Reallocate memory, if needed
    if l_used >= sz then begin
      sz := 2*sz + 10;
      ReallocMem(s, sz*SizeOf(AnsiChar));
    end;

    s[l_used] := c;
    Inc(l_used);
  end;

  if not (c=#13) then
   //If we've read nothing this time
    if l_used = 0 then begin
      Result := false;
      exit;
    end else begin
      s[l_used] := #00;
      Result := true;
      exit;
    end;

  if not (Read(@c, SizeOf(c)) = SizeOf(c))
  or not (c = #10) then
    raise Exception.Create('Illegal linebreak detected at symbol ' + IntToStr(Position));

 //Reallocate memory, if needed
  if l_used >= sz then begin
    sz := 2*sz + 1;
    ReallocMem(s, sz);
  end;

  s[l_used] := #00;
  Result := true;
end;

function TStreamReader2.TryReadLineW(var s: PWideChar; var sz: cardinal): boolean;
var c: WideChar;
  l_used: cardinal;
begin
  l_used := 0;

  while (Read(@c, SizeOf(c))=SizeOf(c))
    and (c <> #10) do
  begin
   //Reallocate memory, if needed
    if l_used >= sz then begin
      sz := 2*sz + 10;
      ReallocMem(s, sz*SizeOf(WideChar));
    end;

    s[l_used] := c;
    Inc(l_used);
  end;

  if not (c=#10) then
   //If we've read nothing this time
    if l_used = 0 then begin
      Result := false;
      exit;
    end;

 //Reallocate memory, if needed
  if l_used >= sz then begin
    sz := 2*sz + 1;
    ReallocMem(s, sz);
  end;

  s[l_used] := #00;
  Result := true;
end;

procedure TStreamReader2.ReadLineA(var s: PAnsiChar; var sz: cardinal);
begin
  if not TryReadLineA(s, sz) then
    raise Exception.Create('Cannot read line of text.');
end;

procedure TStreamReader2.ReadLineW(var s: PWideChar; var sz: cardinal);
begin
  if not TryReadLineW(s, sz) then
    raise Exception.Create('Cannot read line of text.');
end;

function TStreamReader2.ReadStrA: AnsiString;
begin
  ReadLineA(tmpstr_ansi_s, tmpstr_ansi_sz);
  Result := tmpstr_ansi_s;
end;

function TStreamReader2.ReadStrW: WideString;
begin
  ReadLineW(tmpstr_wide_s, tmpstr_wide_sz);
  Result := tmpstr_wide_s;
end;

function TStreamReader2.TryReadStrA(out s: AnsiString): boolean;
begin
  Result := TryReadLineA(tmpstr_ansi_s, tmpstr_ansi_sz);
  if Result then
    s := tmpstr_ansi_s;
end;

function TStreamReader2.TryReadStrW(out s: WideString): boolean;
begin
  Result := TryReadLineW(tmpstr_wide_s, tmpstr_wide_sz);
  if Result then
    s := tmpstr_wide_s;
end;

procedure TStreamReader2.FreeTmpStr;
begin
  FreeMem(tmpstr_ansi_s);
  FreeMem(tmpstr_wide_s);
  tmpstr_ansi_s := nil;
  tmpstr_wide_s := nil;
  tmpstr_ansi_sz := 0;
  tmpstr_wide_sz := 0;
end;

////////////////////////////////////////////////////////////////////////////////

//Пытается прочесть один байт, интерпретирует его как AnsiChar.
function TStreamReader2.ReadAnsiChar(out c: AnsiChar): boolean;
begin
  Result := (Read(@c, SizeOf(AnsiChar))=SizeOf(AnsiChar));
end;

//Пытается прочесть два байта, интерпретирует их как WideChar.
function TStreamReader2.ReadWideChar(out c: WideChar): boolean;
begin
  Result := (Read(@c, SizeOf(WideChar))=SizeOf(WideChar));
end;

const
  REPL_CHAR = WideChar($FFFD);

//Пытается прочесть несколько байт, интерпретирует их как Utf8-char, преобразует в WideChar.
//Возвращает false, если поток кончился, а прочесть не удалось.
//Возвращает REPL_CHAR, если символ не влез в UCS2 или закодирован неверно.
function TStreamReader2.ReadUtf8Char(out c: WideChar): boolean;
var c1, c2, c3: byte;
begin
  Result := (Read(@c1, 1)=1);
  if not Result then exit;

 //Один байт: 0xxxxxxx
  if (c1 and $80) = 0 then begin
    c := WideChar(c1);
    Result := true;
    exit;
  end;

 //Два байта: 110xxxxxx 10yyyyyy
  Result := (Read(@c2, 1)=1);
  if not Result then exit;

 //У ведомых байт должно быть 10xxxxxx
  if (c2 and $C0 <> $80) then begin
    c := REPL_CHAR;
    exit;
  end;

  c1 := c1 and $3F; //сбрасываем в нули два левых бита
  if (c1 and $20) = 0 then begin //не стоит третий бит
    c := WideChar((c1 shl 6) or (c2 and $3F));
    Result := true;
    exit;
  end;

 //Три байта: 1110xxxx 10yyyyyy 10zzzzzz
  Result := (Read(@c3, 1)=1);
  if not Result then exit;

 //У ведомых байт должно быть 10xxxxxx
  if (c3 and $C0 <> $80) then begin
    c := REPL_CHAR;
    exit;
  end;

  c1 := c1 and $1F; //сбрасываем в ноль третий бит
  if (c1 and $10) = 0 then begin //не стоит четвёртый бит
    c := WideChar((c1 shl 12) or ((c2 and $3F) shl 6) or (c3 and $3F));
    Result := true;
    exit;
  end;

 //Четыре байта: у нас не поддерживается. Но мы прочтём четвёртый.
  Result := (Read(@c1, 1)=1); //уже неважно, куда
  if not Result then exit;
  c := REPL_CHAR;

 //Больше четырёх байт мы в страшном сне представить не можем.
end;

////////////////////////////////////////////////////////////////////////////////
///  TStreamWriter2

constructor TStreamWriter2.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create;
  FStream := AStream;
  FOwnStream := AOwnStream;

  buf := nil;
  ptr := nil;
  used := 0;
  rem := 0;
  SetChunkSize(DEFAULT_CHUNK_SIZE);
end;

destructor TStreamWriter2.Destroy;
begin
  ReleaseStream; //с записью буфера
  FreeBuf;

  if Assigned(FStream) then
    if FOwnStream then
      FreeAndNil(FStream)
    else FStream := nil;
  inherited
end;

procedure TStreamWriter2.JoinStream(AStream: TStream; AOwnsStream: boolean = false);
begin
 //Освобождаем старый поток
  ReleaseStream;

 //Цепляемся к потоку
  FStream := AStream;
  FOwnStream := AOwnsStream;

 //Сбрасываем буфер на всякий случай
  ResetBuffer;
end;

//Освобождает поток, синхронизируя его положение с ожидаемым
procedure TStreamWriter2.ReleaseStream;
begin
  if FStream=nil then exit;

 //Сбрасываем
  FlushBuffer;

 //Отпускаем поток
  FStream := nil;
  FOwnStream := false;
end;

function TStreamWriter2.Write(buf: pbyte; len: integer): integer;
begin
  if len=0 then begin
    Result := 0;
    exit;
  end;

 //Наиболее частый случай
  if len <= rem then begin
    Move(buf^, ptr^, len);
    Inc(ptr, len);
    Inc(used, len);
    Dec(rem, len);

    Inc(FBytesWritten, len);
    Result := len;
    exit;
  end;

 //Иначе нас просят записать нечто большее. Вначале добиваем текущий буфер
  if used > 0 then begin
    if (rem > 0) then begin
      Move(buf^, ptr^, rem);
      Inc(ptr, rem);
      Inc(used, rem);

     //Update variables
      Inc(buf, rem);
      Dec(len, rem);

      Result := rem;
      rem := 0;
    end else
      Result := 0;

    FlushBuffer;
  end else
    Result := 0;


 //Если остаток меньше буфера, сохраняем его в буфер
  if len < FChunkSize then begin
    Move(buf^, ptr^, len);
    Inc(ptr, len);
    Inc(used, len);
    Dec(rem, len);

    Inc(Result, len);
  end else
  //Иначе пишем его напрямую
   Result := Result + FStream.Write(buf^, len);

  Inc(FBytesWritten, Result);
end;

function TStreamWriter2.Seek(Offset: int64; Origin: TSeekOrigin): int64;
begin
 //Сбрасываем на диск буфер
  FlushBuffer;

 //Делаем переход
  Result := FStream.Seek(Offset, Origin);
end;

procedure TStreamWriter2.SetChunkSize(AValue: integer);
begin
 //Если в новый буфер текущие данные не влезают, записываем их
  if AValue < used then
    FlushBuffer;

  FChunkSize := AValue;
  ReallocMem(buf, FChunkSize);

 //Обновляем указатель на текущий байт
  ptr := pointer(integer(buf) + used);
  rem := FChunkSize - used;
end;

//Сбрасывает на диск содержимое буфера и обнуляет его внутренность.
procedure TStreamWriter2.FlushBuffer;
begin
  if used=0 then exit;
  FStream.Write(buf^, used);
  rem := rem + used; //всё пространство буфера свободно
  used := 0;
  ptr := buf;
end;

procedure TStreamWriter2.ResetBuffer;
begin
  rem := FChunkSize;
  used := 0;
  ptr := buf;
end;

procedure TStreamWriter2.FreeBuf;
begin
  if Assigned(buf) then
    FreeMem(buf);
  used := 0;
  rem := 0;
  ptr := nil;
end;

//Use this instead of underlying Stream's Position
function TStreamWriter2.Position: integer;
begin
  Result := FStream.Position + used;
end;

end.
