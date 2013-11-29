unit StreamUtils;
{$WEAKPACKAGEUNIT ON}
{
Stream helpers. Cached readers/writers.
Some comments are in Russian, deal with it *puts on glasses*.
(c) himselfv, me@boku.ru.
}

interface
uses SysUtils, Classes{$IFDEF DCC}, Windows{$ENDIF};

type
  EStreamException = class(Exception);

const
  sCannotReadData = 'Cannot read data from the stream';
  sCannotWriteData = 'Cannot write data to the stream';

{
 A class to speed up reading from low latency streams (usually file streams).

 Every time you request something from TFileStream, it reads the data from a
 file. Even with drive cache enabled, it's a kernel-mode operation which is slow.

 This class tries to overcome the problem by reading data in large chunks.
 Whenever you request your byte or two, it reads the whole chunk and stores
 it in local memory. Next time you request another two bytes you'll get
 them right away, because they're already here.

 Please remember that you CAN'T use StreamReader on a Stream "for a while".
 The moment you read your first byte through StreamReader the underlying
 stream is NOT YOURS ANYMORE. You shouldn't make any reads to the Stream
 except than through StreamReader.
}

const
  DEFAULT_CHUNK_SIZE = 4096;

type
  TStreamReader = class(TStream)
  protected
    FStream: TStream;
    FOwnStream: boolean;
    FBytesRead: int64;
  public
    property Stream: TStream read FStream;
    property OwnStream: boolean read FOwnStream;
    property BytesRead: int64 read FBytesRead;

  protected
    flag_reallocbuf: boolean;
    FNextChunkSize: integer; //size of a chunk we'll try to read next time
    FChunkSize: integer; //size of current chunk
    buf: pbyte;   //cached data
    ptr: pbyte;   //current location in buf
    adv: integer; //number of bytes read from buf
    rem: integer; //number of bytes remaining in buf.
     //adv+rem not always equals to FChunkSize since we could have read only
     //partial chunk or FChunkSize could have been changed after that.
    function NewChunk: integer;
    function UpdateChunk: integer;
    procedure SetChunkSize(AValue: integer);
    procedure ResetBuf;
    function ReallocBuf(ASize: integer): boolean;
    procedure FreeBuf;
  public
    property ChunkSize: integer read FChunkSize write SetChunkSize;
    property ChunkBytesRemaining: integer read rem;

  public
    constructor Create(AStream: TStream; AOwnStream: boolean = false);
    destructor Destroy; override;
    procedure JoinStream(AStream: TStream; AOwnsStream: boolean = false);
    procedure ReleaseStream;

  protected
    function GetSize: Int64; override;
    function GetInternalPosition: integer;
    function LocalSeek(Offset: Int64; Origin: TSeekOrigin): boolean;
  public
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadBuf(ABuf: pbyte; ALen: integer): integer; inline;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Peek(var Buffer; ASize: integer): integer;
    function PeekByte(out b: byte): boolean;
  end;

  TStreamWriter = class(TStream)
  protected
    FStream: TStream;
    FOwnStream: boolean;
    FBytesWritten: int64;
  public
    property Stream: TStream read FStream;
    property OwnStream: boolean read FOwnStream;
    property BytesWritten: int64 read FBytesWritten;

  protected
    FChunkSize: integer;
    buf: pbyte;
    ptr: pbyte;
    used: integer;
    procedure ResetBuf;
    procedure FreeBuf;
    procedure SetChunkSize(AValue: integer);
  public
    property ChunkSize: integer read FChunkSize write SetChunkSize;

  public
    constructor Create(AStream: TStream; AOwnStream: boolean = false);
    destructor Destroy; override;
    procedure JoinStream(AStream: TStream; AOwnsStream: boolean = false);
    procedure ReleaseStream;

  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
    function GetInternalPosition: integer;

  public
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteBuf(ABuf: PByte; ALen: integer): integer; inline;
    procedure Flush;

  end;

 { TStringStream
  Stores or retrieves raw data from a string.
  If you pass a string, it will be read/updated, else internal buffer is used. }

 { Don't instantiate }
  TCustomStringStream = class(TStream)
  protected
    FPtr: PByte;
    FPos: integer; //in bytes
    function RemainingSize: integer;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

 { Single-byte strings }
  TAnsiStringStream = class(TCustomStringStream)
  protected
    FOwnBuffer: AnsiString;
    FString: PAnsiString;
    function GetString: AnsiString;
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(AString: PAnsiString = nil);
    property Data: AnsiString read GetString;
  end;

 { Double-byte strings }
  TUnicodeStringStream = class(TCustomStringStream)
  protected
    FOwnBuffer: UnicodeString;
    FString: PUnicodeString;
    function GetString: UnicodeString;
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(AString: PUnicodeString = nil);
    property Data: UnicodeString read GetString;
  end;

  TStringStream = TUnicodeStringStream;


implementation

type
 //IntPtr is missing in older versions of Delphi and it's easier to simply redeclare it
 {$IF Defined(CPUX64)}
  IntPtr = int64;
 {$ELSEIF Defined(CPUX86) OR Defined(CPU386)}
  IntPtr = integer;
 {$ELSE}
  {$MESSAGE Error 'Cannot declare IntPtr for this target platform'}
 {$IFEND}

{ TStreamReader }

constructor TStreamReader.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create;
  FStream := AStream;
  FOwnStream := AOwnStream;

  buf := nil;
  ptr := nil;
  adv := 0;
  rem := 0;
  FChunkSize := 0;
  SetChunkSize(DEFAULT_CHUNK_SIZE);
end;

destructor TStreamReader.Destroy;
begin
  FreeBuf;
  if OwnStream then
    FreeAndNil(FStream)
  else
    FStream := nil;
  inherited;
end;

procedure TStreamReader.JoinStream(AStream: TStream; AOwnsStream: boolean = false);
begin
  ReleaseStream;

  FStream := AStream;
  FOwnStream := AOwnsStream;
  ResetBuf;
end;

//Releases the stream and synchronizes it's position with the expected one
procedure TStreamReader.ReleaseStream;
begin
  if FStream=nil then exit;

 //Scroll back (Seek support required!)
  FStream.Seek(int64(-adv-rem), soCurrent);

 //Release the stream
  FStream := nil;
  FOwnStream := false;
  ResetBuf;
end;

procedure TStreamReader.SetChunkSize(AValue: integer);
begin
 //If we can't realloc right now, set delayed reallocation
  if ReallocBuf(AValue) then
    FChunkSize := AValue
  else begin
    flag_reallocbuf := true;
    FNextChunkSize := AValue;
  end;
end;

{ Downloads a complete new chunk of data. Returns the number of bytes read. }
function TStreamReader.NewChunk: integer;
begin
 //Delayed reallocation
  if flag_reallocbuf and ReallocBuf(FNextChunkSize) then begin
    FChunkSize := FNextChunkSize;
    flag_reallocbuf := false;
  end;

  rem := FStream.Read(buf^, FChunkSize);
  adv := 0;
  ptr := buf;
  Result := rem;
end;

{ Moves remaining data to the beginning of the cache and downloads more.
 Returns the number of bytes read. }
function TStreamReader.UpdateChunk: integer;
var DataPtr: Pbyte;
begin
 //Full cache download
  if rem <= 0 then begin
    Result := NewChunk;
    exit;
  end;

 //Partial download
  Move(ptr^, buf^, rem);
  ptr := buf;
  adv := 0;

  if flag_reallocbuf and ReallocBuf(FNextChunkSize) then begin
    FChunkSize := FNextChunkSize;
    flag_reallocbuf := false;
  end;

  DataPtr := PByte(cardinal(buf) + cardinal(adv+rem));
  Result := Stream.Read(DataPtr^, FChunkSize-adv-rem);
  rem := rem + Result;
end;

//Clears the contents of the cache
procedure TStreamReader.ResetBuf;
begin
  adv := 0;
  rem := 0;
  ptr := buf;
end;

function TStreamReader.ReallocBuf(ASize: integer): boolean;
begin
 //We can't decrease buffer size cause there's still data inside.
  if adv + rem > ASize then begin
    Result := false;
    exit;
  end;

  ReallocMem(buf, ASize);
  ptr := pointer(IntPtr(buf) + adv);
  Result := true;
end;

procedure TStreamReader.FreeBuf;
begin
  if Assigned(buf) then
    FreeMem(buf);
end;

function TStreamReader.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TStreamReader.GetInternalPosition: integer;
begin
  Result := FStream.Position - rem;
end;

//If possible, try to Seek inside the buffer
function TStreamReader.LocalSeek(Offset: Int64; Origin: TSeekOrigin): boolean;
var pos,sz: Int64;
begin
  if Origin=soEnd then begin
    sz := FStream.Size;
    //Convert to from beginning
    Offset := sz - Offset;
    Origin := soBeginning;
  end;

  if Origin=soBeginning then begin
    pos := FStream.Position;
    if (Offset>=pos) or (Offset<pos-adv-rem) then begin
      Result := false; //not in this chunk
      exit;
    end;
   //Convert to relative
    Offset := rem-(pos-Offset);
    Origin := soCurrent;
  end;

  if Origin=soCurrent then begin
    if (Offset<-adv) or (Offset>=rem) then begin
      Result := false;
      exit;
    end;

    adv := adv+Offset;
    rem := rem-Offset;
    Inc(ptr, Offset);
    Result := true;
  end else
    Result := false;
end;

function TStreamReader.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
 //TStream calls Seek(0,soCurrent) to determine Position, so be fast in this case
  if (Origin=soCurrent) and (Offset=0) then
    Result := GetInternalPosition()
  else
  if LocalSeek(Offset, Origin) then
    Result := GetInternalPosition()
  else begin
    Result := FStream.Seek(Offset, Origin);
    ResetBuf;
  end;
end;

function TStreamReader.Read(var Buffer; Count: Longint): Longint;
var pbuf: PByte;
begin
  if Count=0 then begin
    Result := 0;
    exit;
  end;

 //The most common case
  if Count <= rem then begin
    Move(ptr^, Buffer, Count);
    Inc(ptr, Count);
    Inc(adv, Count);
    Dec(rem, Count);

    Inc(FBytesRead, Count);
    Result := Count;
    exit;
  end;

 //Read first part
  pbuf := @Buffer;
  if rem > 0 then begin
    Move(ptr^, pbuf^, rem);
    Inc(ptr, rem);
    Inc(adv, rem);

   //Update variables
    Inc(pbuf, rem);
    Dec(Count, rem);

    Result := rem;
    rem := 0;
  end else
    Result := 0;

 //Download the remaining part

 //If it's smaller than a chunk, read the whole chunk
  if Count < FChunkSize then begin
    NewChunk;

    if rem < Count then //rem was already updated in NewChunk
      Count := rem;

    Move(ptr^, pbuf^, Count);
    Inc(ptr, Count);
    Inc(adv, Count);
    Dec(rem, Count);

    Inc(Result, Count);
  end else
   //Else just read it from stream
    Result := Result + FStream.Read(pbuf^, Count);

  Inc(FBytesRead, Result);
end;

function TStreamReader.ReadBuf(ABuf: pbyte; ALen: integer): integer;
begin
  Result := Read(ABuf^, ALen);
end;

function TStreamReader.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('StreamReader cannot write.');
end;

//Won't Peek for more than CacheSize
function TStreamReader.Peek(var Buffer; ASize: integer): integer;
begin
 //If the data is in cache, it's simple
  if ASize <= rem then begin
    Move(ptr^, Buffer, ASize);
    Result := ASize;
    exit;
  end;

 //Else return the best possible amount. Make the cache completely fresh
  if rem <= FChunkSize then
    UpdateChunk;

 //If the complete data fit, return it
  if ASize <= rem then begin
    Move(ptr^, Buffer, ASize);
    Result := ASize;
    exit;
  end;

 //Didn't fit => return all that's available
  Move(ptr^, Buffer, rem);
  Result := rem;
end;

//�� ����-�� ���� �� ������ ����� �����������, ���� � ����� ��������.
function TStreamReader.PeekByte(out b: byte): boolean;
begin
 //���� ����� ������, ���� ������ ����
  if rem >= 0 then begin
    b := ptr^;
    Result := true;
    exit;
  end;

 //����� ����� ����. ������ ��������� �������.
  NewChunk;

  if rem >= 0 then begin
    b := ptr^;
    Result := true;
  end else
    Result := false;
end;


{ TStreamWriter }

constructor TStreamWriter.Create(AStream: TStream; AOwnStream: boolean = false);
begin
  inherited Create;
  FStream := AStream;
  FOwnStream := AOwnStream;

  FChunkSize := 0;
  buf := nil;
  ptr := nil;
  used := 0;

  SetChunkSize(DEFAULT_CHUNK_SIZE);
end;

destructor TStreamWriter.Destroy;
begin
  Flush();
  FreeBuf;

  if OwnStream then
    FreeAndNil(FStream)
  else
    FStream := nil;
  inherited;
end;

procedure TStreamWriter.JoinStream(AStream: TStream; AOwnsStream: boolean = false);
begin
 //����������� ������ �����
  ReleaseStream;

 //��������� � ������
  FStream := AStream;
  FOwnStream := AOwnsStream;

 //���������� ����� �� ������ ������
  ResetBuf;
end;

//����������� �����, ������������� ��� ��������� � ���������
procedure TStreamWriter.ReleaseStream;
begin
  if FStream=nil then exit;

 //����������
  Flush;

 //��������� �����
  FStream := nil;
  FOwnStream := false;
end;

//���������� �� ���� ���������� ������ � �������� �����.
procedure TStreamWriter.Flush;
begin
  if used <= 0 then exit;
  FStream.Write(buf^, used);
  ptr := buf;
  used := 0;
end;

procedure TStreamWriter.ResetBuf;
begin
  ptr := buf;
  used := 0;
end;

procedure TStreamWriter.FreeBuf;
begin
  if Assigned(buf) then
    FreeMem(buf);
  buf := nil;
  ptr := nil;
  used := 0;
end;

procedure TStreamWriter.SetChunkSize(AValue: integer);
begin
 //���� � ����� ����� ������� ������ �� �������, ���������� �� � �����
  if AValue < used then
    Flush;

  FChunkSize := AValue;
  ReallocMem(buf, FChunkSize);

 //��������� ��������� �� ������� ����
  ptr := pointer(IntPtr(buf) + used);
end;

//Use this instead of underlying Stream's Position
function TStreamWriter.GetInternalPosition: integer;
begin
  Result := FStream.Position + used;
end;

function TStreamWriter.GetSize: Int64;
begin
  Result := FStream.Size + used;
end;

procedure TStreamWriter.SetSize(NewSize: Longint);
begin
  SetSize(int64(NewSize));
end;

procedure TStreamWriter.SetSize(const NewSize: Int64);
begin
  Flush();
  FStream.Size := NewSize;
end;

function TStreamWriter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Origin=soCurrent) and (Offset=0) then
    Result := GetInternalPosition() //TStream uses this to determine Position
  else begin
    Flush;
    Result := FStream.Seek(Offset, Origin);
  end;
end;

function TStreamWriter.Read(var Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('StreamWriter cannot read.');
end;

function TStreamWriter.Write(const Buffer; Count: Longint): Longint;
var rem: integer;
  pbuf: PByte;
begin
  if Count<=0 then begin
    Result := 0;
    exit;
  end;

 //���� ������� � ���, ����� ����
  rem := FChunkSize - used;
  if Count <= rem then begin
    Move(Buffer, ptr^, Count);
    Inc(used, Count);
    Inc(ptr, Count);
    Result := Count;
    Inc(FBytesWritten, Count);
    exit;
  end;

 //����� ��� ������ �������� ����� �������. ������� �������� ������� �����
  pbuf := @Buffer;
  if used > 0 then begin
    if rem > 0 then begin
      Move(pbuf^, ptr^, rem);
      Inc(ptr, rem);
      Inc(used, rem);

     //Update variables
      Inc(pbuf, rem);
      Dec(Count, rem);

      Result := rem;
    end else
      Result := 0;

    Flush;
  end else
    Result := 0;

 //���� ������� ������ ������, ��������� ��� � �����
  if Count < FChunkSize then begin
    Move(pbuf^, ptr^, Count);
    Inc(ptr, Count);
    Inc(used, Count);
    Inc(Result, Count);
  end else
  //����� ����� ��� ��������
   Result := Result + FStream.Write(pbuf^, Count);

  Inc(FBytesWritten, Result);
end;

function TStreamWriter.WriteBuf(ABuf: PByte; ALen: integer): integer;
begin
  Result := Write(ABuf^, ALen);
end;


{
TCustomStringStream
}

function TCustomStringStream.RemainingSize: integer;
begin
  Result := Self.Size-FPos;
end;

function TCustomStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count>RemainingSize then
    Count := RemainingSize;
  Move(PByte(IntPtr(FPtr)+FPos)^,Buffer,Count);
  Inc(FPos,Count);
  Result := Count;
end;

function TCustomStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  if RemainingSize<Count then
    SetSize(FPos+Count);
  Move(Buffer,PByte(IntPtr(FPtr)+FPos)^,Count);
  Inc(FPos,Count);
  Result := Count;
end;

function TCustomStringStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Origin=soCurrent then begin
    if Offset=0 then begin
      Result := FPos;
      exit;
    end;
    Result := FPos+Offset;
  end else
  if Origin=soEnd then begin
    Result := GetSize-Offset;
  end else
    Result := Offset;

  if Result<0 then
    Result := 0
  else
  if Result>GetSize then
    Result := GetSize;
  FPos := Result;
end;

{
TAnsiStringStream
}

constructor TAnsiStringStream.Create(AString: PAnsiString = nil);
begin
  inherited Create();
  if AString=nil then
    FString := @FOwnBuffer
  else
    FString := AString;
  FPos := 0;
  FPtr := pointer(FString^);
end;

function TAnsiStringStream.GetString: AnsiString;
begin
  Result := FString^;
end;

function TAnsiStringStream.GetSize: Int64;
begin
  Result := Length(FString^)*SizeOf(AnsiChar);
end;

procedure TAnsiStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FString^, NewSize);
  FPtr := pointer(FString^);
end;

{
TUnicodeStringStream
}

constructor TUnicodeStringStream.Create(AString: PUnicodeString = nil);
begin
  inherited Create();
  if AString=nil then
    FString := @FOwnBuffer
  else
    FString := AString;
  FPos := 0;
  FPtr := pointer(FString^);
end;

function TUnicodeStringStream.GetString: UnicodeString;
begin
  Result := FString^;
end;

function TUnicodeStringStream.GetSize: Int64;
begin
  Result := Length(FString^)*SizeOf(WideChar);
end;

procedure TUnicodeStringStream.SetSize(NewSize: Longint);
begin
  if NewSize mod 2 = 0 then
    SetLength(FString^, NewSize div SizeOf(WideChar))
  else
    SetLength(FString^, NewSize div SizeOf(WideChar) + 1); //no choice but to allocate one more symbol
  FPtr := pointer(FString^);
end;

end.
