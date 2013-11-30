unit JWBIO;
{
Encodings and stream encoders/decoders.
How to use:
1. Detecting encodings:

  if not Conv_DetectType(filename, AEncoding) then
    AEncoding := Conv_ChooseType(AEncoding);

2. Reading file:

  AReader := TFileReader.Create(filename, TUTF8Encoding.Create);

  AReader := OpenTextFile(filename, TUTF8Encoding);
 //opens cached stream, may be useful when doing seeks/detection

  AReader := OpenTextFile(filename, nil);
 //best guess encoding and open

  while AReader.ReadChar(ch) do
    ProcessChar(ch);
  line := AReader.Readln();

3. Writing to file:

  AWriter := CreateTextFile(filename, TUTF8Encoding);
  AWriter := AppendToTextFile(filename, TUTF8Encoding);
  AWriter.Writeln('Hello world');

4. Working with console:

  AReader := ConsoleReader()
  AWriter := ConsoleWriter()
}

interface
uses SysUtils, Classes, StreamUtils{$IFDEF FPC}, iostream{$ENDIF};

{$IF Defined(FPC)}
//На некоторых компиляторах нет TBytes или некоторых функций, связанных с ним
{$DEFINE OWNBYTES}
type
  TBytes = array of byte;
{$IFEND}

function Bytes(): TBytes; overload; inline;
function Bytes(b1: byte): TBytes; overload; inline;
function Bytes(b1,b2: byte): TBytes; overload; inline;
function Bytes(b1,b2,b3: byte): TBytes; overload; inline;
function Bytes(b1,b2,b3,b4: byte): TBytes; overload; inline;


type
  {
  TEncoding design dogmas.

  1.1. Encodings are stateless.
  1.2. Encodings can be parametrized:
    TMultibyteEncoding.Create('win-1251')
  Although there's a subclass of encodings which aren't:
    TUTF8Encoding
    TUTF16LEEncoding
  Some functions accept or return encoding classes instead of instances:
    function GuessEncoding(const AFilename: TFilename): CEncoding;
  If you want to use parametrized encodings this way, inherit and parametrize:
    TWin1251Encoding = class(TMultibyteEncoding);

  1.3. There can be multiple implementations of the same encoding:
    TUTF8Encoding.Create();
    TMultibyteEncoding.Create(CP_UTF8);

  1.4. If you need to uniquely identify encodings or permanently store encoding
  selection, your best bet is to assign them some standard names:
    Register(TUTF8Encoding, ['utf8','utf-8']);
    Register(TWin1251Encoding, ['win1251']);
    WriteString('LastEncoding', 'utf-8');
    fileEncoding := Find(ReadString('LastEncoding'));

  2. Encodings operate on Streams, not Buffers.
  Standard encoding design is to make a function like this:
    DecodeBytes(AFrom: PByte; ATo: PWideChar; var AFromRemaining: integer;
      var AToRemaining: integer);
  It would decode as many complete characters as possible and return the
  remainder until next time.

  This is clumsy in Delphi. We have classes to handle stream reading/writing:
    DecodeBytes(AFrom: TStream; AMaxChars: integer): UnicodeString;
  Principially it's the same, but TStream encapsulates the concept of "remaining
  bytes" in the buffer.

  2.1. Decoding stops when the stream is over.
  Instead of decrementing AFromRemaining, TEncoding moves through a stream. When
  the stream returns 0 on any read request, it is considered over (just like
  with sockets).

  2.2. Encodings leave incomplete characters unread.
  If there's 2 bytes in the input stream and a character needs 3, TEncoding must
  roll back and leave 2 bytes for the next time.

  2.3. Encodings leave surrogate parts unread.
  If TEncoding needs to return a two-character combination and there's only one
  character slot left, it must roll back and leave both chars for the next time.

  3. Encoding can return less than requested.
  E.g. 0 chars if there's not enough for a char.

  3.1. Detecting EOF is hard.
  Data left in the stream + TEncoding makes no progress + there's enough output
  buffer.

  4. Encodings require special TStreams. Explanations below.
  Use:
    TStreamBuf to wrap generic streams.
    TMemoryStream to decode from memory.

  4.1. Encodings require seek.
  Testing for BOM and reading multibyte characters requires encoding to be able
  to roll back in case there's not enough data.

  4.2. Encodings do not give up.
  If you ask for 500 chars, TEncoding will try to give you 500 chars. It's going
  to request data from the underlying stream again and again.
  Socket-like streams which return "as much as available" are going to block the
  second time they are queried.

  If you just need to decode a file, use TStreamDecoder/TStreamEncoder.
  }

  TEncodingLikelihood = (
    elIdentified, //data is self-marked as being in this encoding
    elLikely,     //data is likely to be in this encoding
    elUnlikely    //data is unlikely to be in this encoding
  );

  TEncoding = class
    constructor Create; virtual;
    class function GetBom: TBytes; virtual;
    class function ReadBom(AStream: TStream): boolean; virtual;
    procedure WriteBom(AStream: TStream); virtual;
    function Analyze(AStream: TStream): TEncodingLikelihood; virtual;
   { Implement *at least one* of Read/ReadChar.
    MaxChars is given in 2 byte positions. Surrogate pairs count as separate chars. }
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; virtual;
    function ReadChar(AStream: TStream; out AChar: WideChar): boolean; virtual;
   { Implement *at least one* of Write/WriteChar.
    Encoding descendants do not check the number of bytes written. If you care,
    wrap TStream and raise exceptions. }
    procedure Write(AStream: TStream; const AData: UnicodeString); virtual;
    procedure WriteChar(AStream: TStream; AChar: WideChar); virtual;
  end;
  CEncoding = class of TEncoding;

 { Simple encodings }
  TAsciiEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;

 {$IFDEF MSWINDOWS}
 { Encoding class based on MultiByteToWideChar/WideCharToMultiByte. Slowish but safe.
  As things are right now, it's better to use derived classes for actual encodings
  (e.g. TAcpEncoding) }
  TMultibyteEncoding = class(TEncoding)
  protected
    FCodepage: integer;
    FIllegalChar: char;
  public
    constructor Create(const ACodepage: integer); reintroduce; overload;
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;
  TAcpEncoding = class(TMultibyteEncoding)
    constructor Create; override;
  end;
 {$ENDIF}

  TUTF8Encoding = class(TEncoding)
    class function GetBom: TBytes; override;
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;

  TUTF16LEEncoding = class(TEncoding)
    class function GetBom: TBytes; override;
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;
  TUTF16Encoding = TUTF16LEEncoding;
  TUnicodeEncoding = TUTF16LEEncoding;

  TUTF16BEEncoding = class(TEncoding)
    class function GetBom: TBytes; override;
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;

  TEUCEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;

  TSJISEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;

 { New-JIS, Old-JIS and NEC-JIS differ only by Start and End markers.
  Read() covers all of them while Write() depends on markers being set by descendants below }
  TBaseJISEncoding = class(TEncoding)
  protected
    intwobyte: boolean;
    StartMark: TBytes;
    EndMark: TBytes;
    procedure _fputstart(AStream: TStream);
    procedure _fputend(AStream: TStream);
  public
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;
  TJISEncoding = class(TBaseJISEncoding)
    constructor Create; override;
  end;
  TOldJISEncoding = class(TBaseJISEncoding)
    constructor Create; override;
  end;
  TNECJISEncoding = class(TBaseJISEncoding)
    constructor Create; override;
  end;

  TGBEncoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;

  TBIG5Encoding = class(TEncoding)
    function Read(AStream: TStream; MaxChars: integer): UnicodeString; override;
    procedure Write(AStream: TStream; const AData: UnicodeString); override;
  end;


const
  INBUFSZ = 1024; //in characters
  OUTBUFSZ = 1024;

type
 { Encodings need to be able to roll back several bytes if they find incomplete
  coding sequence.
  And we don't want to query the underlying buffer every time the encoding asks,
  because with blocking streams this may introduce waits. We want predictable
  waits.
  So we use a modified StreamReader which does not query for the next block
  automatically. It only does so at our explicit command. }
  TStreamBuf = class(TStreamReader)
  public
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

  TStreamDecoder = class
  protected
    FByteBuf: TStreamBuf;
    FBuffer: UnicodeString;
    FBufferPos: integer;
    FStream: TStream;
    FOwnsStream: boolean;
    FEncoding: TEncoding;
    FByteEOF: boolean; //set when you cannot read one more byte from the stream
    procedure ConvertMoreChars;
  public
    constructor Open(AStream: TStream; AEncoding: TEncoding;
      AOwnsStream: boolean = false); overload; virtual;
    constructor Open(const AFilename: TFilename; AEncoding: TEncoding); overload;
    destructor Destroy; override;
    procedure DetachStream; //clears whatever caches the instance may have for the Stream
   { Since this may require backward seek, try not to TrySkipBom for sources where
    you do not really expect it (i.e. console) }
    procedure TrySkipBom;
    procedure Rewind(const ADontSkipBom: boolean = false);
    function EOF: boolean;
    function ReadChar(out ch: WideChar): boolean; overload;
   { If reading next position produces a surrogate pair, store it somewhere and
    return in two calls. }
    function ReadLn(out ln: UnicodeString): boolean; overload;
    function ReadChar: WideChar; overload; //#0000 if no char
    function ReadLn: UnicodeString; overload; //empty string if no string
   { Or should these raise exceptions? }
    property Stream: TStream read FStream;
    property OwnsStream: boolean read FOwnsStream;
    property Encoding: TEncoding read FEncoding;
  end;

  TStreamEncoder = class
  protected
    FBuffer: UnicodeString;
    FBufferPos: integer;
    FStream: TStream;
    FOwnsStream: boolean;
    FEncoding: TEncoding;
    function GetBufferSize: integer;
    procedure SetBufferSize(const Value: integer);
  public
    constructor Open(AStream: TStream; AEncoding: TEncoding;
      AOwnsStream: boolean = false);
    constructor CreateNew(const AFilename: TFilename; AEncoding: TEncoding);
    constructor Append(const AFilename: TFilename; AEncoding: TEncoding);
    destructor Destroy; override;
    procedure Flush; //clears whatever caches instance may have for the stream
    procedure WriteBom;
    procedure WriteChar(const ch: WideChar);
    procedure Write(const ln: UnicodeString);
    procedure WriteLn(const ln: UnicodeString);
    property Stream: TStream read FStream;
    property OwnsStream: boolean read FOwnsStream;
    property Encoding: TEncoding read FEncoding;
    property BufferSize: integer read GetBufferSize write SetBufferSize;
  end;
  CStreamEncoder = class of TStreamDecoder;

function Conv_DetectType(AStream: TStream): CEncoding; overload;
function Conv_DetectType(AStream: TStream; out AEncoding: CEncoding): boolean; overload;
function Conv_DetectType(const AFilename: TFilename): CEncoding; overload;
function Conv_DetectType(const AFilename: TFilename; out AEncoding: CEncoding): boolean; overload;

function OpenStream(const AStream: TStream; AOwnsStream: boolean; AEncoding: CEncoding = nil): TStreamDecoder;
function WriteToStream(const AStream: TStream; AOwnsStream: boolean; AEncoding: CEncoding): TStreamEncoder;
function OpenTextFile(const AFilename: TFilename; AEncoding: CEncoding = nil): TStreamDecoder; inline;
function CreateTextFile(const AFilename: TFilename; AEncoding: CEncoding): TStreamEncoder;
function AppendToTextFile(const AFilename: TFilename; AEncoding: CEncoding = nil): TStreamEncoder;

//Finds a class by it's name. Good to store encoding selection in a permanent way.
function FindEncoding(const AClassName: string): CEncoding;

//Compares binary data in files
function CompareStreams(const AStream1, AStream2: TStream): boolean;
function CompareFiles(const AFilename1, AFilename2: TFilename): boolean;

{ Compatibility functions }
function AnsiFileReader(const AFilename: TFilename): TStreamDecoder;
function UnicodeFileReader(const AFilename: TFilename): TStreamDecoder;
function ConsoleReader(AEncoding: TEncoding = nil): TStreamDecoder;
function UnicodeStreamReader(AStream: TStream; AOwnsStream: boolean = false): TStreamDecoder;
function FileReader(const AFilename: TFilename): TStreamDecoder; inline; //->Unicode on Unicode, ->Ansi on Ansi
function AnsiFileWriter(const AFilename: TFilename): TStreamEncoder;
function UnicodeFileWriter(const AFilename: TFilename): TStreamEncoder;
function ConsoleWriter(AEncoding: TEncoding = nil): TStreamEncoder;
function UnicodeStreamWriter(AStream: TStream; AOwnsStream: boolean = false): TStreamEncoder;
function FileWriter(const AFilename: TFilename): TStreamEncoder; inline; //->Unicode on Unicode, ->Ansi on Ansi

{ Misc useful functions }
function GetLineCount(AText: TStreamDecoder): integer;

implementation
uses {$IFDEF MSWINDOWS}Windows,{$ENDIF} JWBConvertTbl;

{ Various helpers }

//Swaps bytes in a word
function _swapw(const w: word): word; inline;
begin
  Result :=
    (w and $00FF) shl 8 +
    (w and $FF00) shr 8;
{ or:
    (w mod $100) shl 8 +
    (w div $100);
  dunno which is faster }
end;

{$IFDEF OWNBYTES}
function Bytes(): TBytes;
begin
  SetLength(Result, 0);
end;

function Bytes(b1: byte): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := b1;
end;

function Bytes(b1,b2: byte): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := b1;
  Result[1] := b2;
end;

function Bytes(b1,b2,b3: byte): TBytes;
begin
  SetLength(Result, 3);
  Result[0] := b1;
  Result[1] := b2;
  Result[2] := b3;
end;

function Bytes(b1,b2,b3,b4: byte): TBytes;
begin
  SetLength(Result, 4);
  Result[0] := b1;
  Result[1] := b2;
  Result[2] := b3;
  Result[3] := b4;
end;
{$ELSE}
function Bytes(): TBytes;
begin
  Result := TBytes.Create();
end;

function Bytes(b1: byte): TBytes;
begin
  Result := TBytes.Create(b1);
end;

function Bytes(b1,b2: byte): TBytes;
begin
  Result := TBytes.Create(b1,b2);
end;

function Bytes(b1,b2,b3: byte): TBytes;
begin
  Result := TBytes.Create(b1,b2,b3);
end;

function Bytes(b1,b2,b3,b4: byte): TBytes;
begin
  Result := TBytes.Create(b1,b2,b3,b4);
end;
{$ENDIF}

{ Encoding }

constructor TEncoding.Create;
begin
  inherited; { Inherit in descendants to initialize encoding }
end;

class function TEncoding.GetBom: TBytes;
begin
  Result := Bytes();
end;

{ Reads out any of the BOMs supported by this encoding and returns true,
 or returns false and returns AStream to the previous position.
 Avoid calling for streams which do not support seeking when BOM is not really
 expected. }
class function TEncoding.ReadBom(AStream: TStream): boolean;
var BOM: TBytes;
  data: TBytes;
  read_sz: integer;
  i: integer;
begin
  BOM := GetBOM();
  if Length(BOM)<=0 then begin
    Result := false;
    exit;
  end;
 { Default implementation just checks for the main BOM }
  SetLength(data, Length(BOM));
  read_sz := AStream.Read(data[0], Length(data));
  Result := true;
  for i := 0 to Length(data)-1 do
    if BOM[i]<>data[i] then begin
      Result := false;
      break;
    end;
  if not Result then
    AStream.Seek(-read_sz, soCurrent);
end;

procedure TEncoding.WriteBom(AStream: TStream);
var BOM: TBytes;
begin
  BOM := GetBOM();
  if Length(BOM)>0 then
    AStream.Write(BOM[0], Length(BOM));
end;

function TEncoding.Analyze(AStream: TStream): TEncodingLikelihood;
begin
 { Default implementation only tests for BOM }
  if ReadBom(AStream) then
    Result := elIdentified
  else
    Result := elUnlikely;
end;

function TEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if not ReadChar(AStream, Result[pos]) then break;
    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

function TEncoding.ReadChar(AStream: TStream; out AChar: WideChar): boolean;
var s: UnicodeString;
begin
  s := Read(AStream, 1);
  Result := Length(s)>=1;
  if Result then AChar := s[1];
end;

procedure TEncoding.Write(AStream: TStream; const AData: UnicodeString);
var i: integer;
begin
  for i := 1 to Length(AData) do
    WriteChar(AStream, AData[i]);
end;

procedure TEncoding.WriteChar(AStream: TStream; AChar: WideChar);
begin
  Write(AStream, AChar);
end;

procedure _fwrite1(AStream: TStream; const b: byte); inline;
begin
  AStream.Write(b, 1);
end;

procedure _fwrite2(AStream: TStream; const w: word); inline;
begin
  AStream.Write(w, 2);
end;

procedure _fwrite3(AStream: TStream; const dw: cardinal); inline;
begin
  AStream.Write(dw, 3);
end;

procedure _fwrite4(AStream: TStream; const dw: cardinal); inline;
begin
  AStream.Write(dw, 4);
end;


{ Stream buffer }

function TStreamBuf.Read(var Buffer; Count: Longint): Longint;
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

    Result := rem;
    rem := 0;
  end else
    Result := 0;

 //That was all the data we have, sorry
  Inc(FBytesRead, Result);
end;


{ Stream decoder }

constructor TStreamDecoder.Open(AStream: TStream; AEncoding: TEncoding;
  AOwnsStream: boolean = false);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FByteBuf := TStreamBuf.Create(AStream, false);
  FByteBuf.ChunkSize := 1024;
  FByteEOF := false;
  FEncoding := AEncoding;
  Self.TrySkipBom;
end;

constructor TStreamDecoder.Open(const AFilename: TFilename; AEncoding: TEncoding);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead);
  Self.Open(fs, AEncoding, {OwnsStream=}true);
end;

destructor TStreamDecoder.Destroy;
begin
  FreeAndNil(FByteBuf);
  if FOwnsStream then
    FreeAndNil(FStream)
  else
    DetachStream; //only restore position if we have to
  FreeAndNil(FEncoding);
  inherited;
end;

{ Reads more data and convert more characters. }
procedure TStreamDecoder.ConvertMoreChars;
var new_sz: integer;
  new_chunk: UnicodeString;
  bytes_read: integer;
  bytes_rem: integer;
begin
{
  Stream --> ByteBuf --> CharBuf [Buffer]
  1. ByteBuf overflows if TEncoding never converts anything, no matter how big
     chunk of bytes it's given.
  2. CharBuf overflows if someone calls ConvertMoreChars without reading those
     out.
  In both cases, no harm done but no data flows further. ReadChar may be stuck
  calling us again and again.
}

 //Read more data from the stream. We immediately convert the data, so there
 //should be close to no previous data in ByteBuf.
  bytes_read := FByteBuf.UpdateChunk;
  bytes_rem := FByteBuf.ChunkBytesRemaining;

 //Convert all the available data
  new_sz := INBUFSZ-(Length(FBuffer)-FBufferPos);
  if new_sz<=0 then exit;
  new_chunk := FEncoding.Read(FByteBuf, new_sz);
 //New chunk may be empty if not enough bytes for a single char has arrived
  FBuffer := copy(FBuffer, FBufferPos+1, MaxInt)
    + new_chunk;
  FBufferPos := 0;

 { ByteEOF means that 1. stream is EOF, 2. byte buffer is empty or contains only
  unparsable remainder.
  Test for 1? UpdateChunk produces no data.
  Test for 2? TEncoding eats no bytes.
  Complications: 1. ByteBuf overflow => UpdateChunk runs empty, 2. CharBuf
    overflow => TEncoding eats no bytes even though it could.
  Solutions: 2. Less than 2 empty chars => skip ByteEOF detection until someone
    reads those out.
    1. => condition satisfied (if 2? is also true, we're stuck forever, so EOF) }

  if (bytes_read=0) {for whatever reason, mb overflow}
  and (new_sz>=2) {there was enough space for any char/surrogate}
  and (FByteBuf.ChunkBytesRemaining>=bytes_rem) {and no bytes was read}
  then
    FByteEOF := true;
end;

procedure TStreamDecoder.DetachStream;
begin
 { Unfortunately there's no way now to know how many bytes we should roll back
  to position stream where we logically are (in characters). }
end;

procedure TStreamDecoder.TrySkipBom;
begin
  FEncoding.ReadBom(Stream);
end;

procedure TStreamDecoder.Rewind(const ADontSkipBom: boolean);
begin
  Stream.Seek(0, soBeginning);
  FBufferPos := Length(FBuffer)+1;
  FByteEOF := false;
  if not ADontSkipBom then
    TrySkipBom;
end;

{ True if there *already was* at least one ReadChar() which resulted in False,
 and no subsequent calls can be expected to succeed.
 Should not return true even if we're at EOF until someone tries to ReadChar()
 and finds it. }
function TStreamDecoder.EOF: boolean;
begin
  Result := FByteEOF and (FBufferPos>=Length(FBuffer));
end;

{ Reads another character from the stream or returns False to indicate that
 no more are available. }
function TStreamDecoder.ReadChar(out ch: WideChar): boolean;
begin
  while FBufferPos>=Length(FBuffer) do begin
    if FByteEOF then begin
      Result := false;
      exit;
    end;
    ConvertMoreChars;
  end;
  Inc(FBufferPos);
  ch := FBuffer[FBufferPos];
  Result := true;
end;

{ Reads until next CRLF or the end of the stream.
 Last line always ends with the stream. If the file ends in CRLF, it'll be
 returned as a last, empty line.
 To reproduce the content when saving, always write last line without CRLF.
 As an exception, empty file is thought to have no lines. }
function TStreamDecoder.ReadLn(out ln: UnicodeString): boolean;
var ch: WideChar;
begin
  ln := '';
  Result := not EOF; //there may be no more characters, but at least we haven't found that out yet
  while ReadChar(ch) do begin
   { Maybe a more thorough CRLF/CR/LF handling is needed }
    if ch=#$000D then begin
     //ignore
    end else
    if ch=#$000A then
      break
    else
      ln := ln + ch;
  end;
end;

function TStreamDecoder.ReadChar: WideChar;
begin
  if not ReadChar(Result) then
    Result := #0000;
end;

function TStreamDecoder.ReadLn: UnicodeString;
begin
  if not ReadLn(Result) then
    Result := '';
end;


{ Stream encoder }

constructor TStreamEncoder.Open(AStream: TStream; AEncoding: TEncoding;
  AOwnsStream: boolean = false);
begin
  inherited Create();
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FEncoding := AEncoding;
  SetLength(FBuffer, OUTBUFSZ);
  FBufferPos := 0;
end;

constructor TStreamEncoder.CreateNew(const AFilename: TFilename; AEncoding: TEncoding);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  Self.Open(fs,AEncoding,{OwnsStream=}true);
end;

constructor TStreamEncoder.Append(const AFilename: TFilename; AEncoding: TEncoding);
var fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmCreate);
  fs.Seek(0,soFromEnd);
  Self.Open(fs,AEncoding,{OwnsStream=}true);
end;

destructor TStreamEncoder.Destroy;
begin
  Flush();
  if FOwnsStream then
    FreeAndNil(FStream);
  FreeAndNil(FEncoding);
  inherited;
end;

function TStreamEncoder.GetBufferSize: integer;
begin
  Result := Length(FBuffer);
end;

procedure TStreamEncoder.SetBufferSize(const Value: integer);
begin
  Flush;
  SetLength(FBuffer, Value);
end;

procedure TStreamEncoder.Flush;
begin
  if FBufferPos>=Length(FBuffer) then
    FEncoding.Write(FStream, FBuffer)
  else
  if FBufferPos>0 then
    FEncoding.Write(FStream, copy(FBuffer, 1, FBufferPos));
  FBufferPos := 0;
end;

procedure TStreamEncoder.WriteBom;
begin
  Flush;
  FEncoding.WriteBom(FStream);
end;

procedure TStreamEncoder.WriteChar(const ch: WideChar);
begin
  if Length(FBuffer)<=0 then
    FEncoding.WriteChar(FStream, ch)
  else begin
    Inc(FBufferPos);
    FBuffer[FBufferPos] := ch;
    if FBufferPos>=Length(FBuffer) then
      Flush();
  end;
end;

procedure TStreamEncoder.Write(const ln: UnicodeString);
var fst: integer;
begin
  if ln='' then exit;
  if Length(FBuffer)>0 then begin
   //Whatever fits into buffer
    fst := Length(FBuffer)-FBufferPos;
    if fst>Length(ln) then fst := Length(ln);

    Move(ln[1], FBuffer[FBufferPos+1], fst*SizeOf(WideChar));
    Inc(FBufferPos, fst);
    if FBufferPos>=Length(FBuffer) then
      Flush();

    Inc(fst);
  end else
    fst := 1;

 //Remainder
  if fst<Length(ln)+1 then
    FEncoding.Write(FStream, copy(ln, fst, MaxInt));
end;

{ Writes a line with CRLF in the end.
 Note that if you've read the lines with ReadLn, to reproduce the file content,
 last line has to be written without CRLF. See ReadLn. }
procedure TStreamEncoder.WriteLn(const ln: UnicodeString);
begin
  Write(ln+#$000D+#$000A);
end;


{ Simple encodings }

function TAsciiEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var pos: integer;
  ac: AnsiChar;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while (MaxChars>0) and (AStream.Read(ac,1)=1) do begin
    Result[pos] := WideChar(ac);
    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TAsciiEncoding.Write(AStream: TStream; const AData: UnicodeString);
var i: integer;
  ch: AnsiChar;
begin
  for i := 1 to Length(AData) do begin
    ch := AnsiChar(AData[i]);
    AStream.Write(ch, 1);
  end;
end;

{$IFDEF MSWINDOWS}
{ A note on parametrized encodings:
 They will work, but no with any special code which assumes one encoding == one type.
 You also cannot rely on comparisons like "if Encoding1==Encoding2". }
constructor TMultibyteEncoding.Create(const ACodepage: integer);
begin
  inherited Create;
  FCodepage := ACodepage;
  FIllegalChar := '?';
end;


{ Hold on to your seats guys, this is going to be slow!

 CP_* encodings on Windows can be multibyte and MultiByteToWideChar does not
 return the number of bytes it used (only the number of *resulting* chars).
 To know where to start next time we expand sample byte by byte, until there's
 enough bytes for MultiByteToWideChar to produce one char.

 Both Multibyte and Wide encodings can have surrogate characters, so we have
 to be ready to receive more than one char position. If there's not enough
 space in Char buffer, we roll back and leave it for next time.

 If there are invalid characters, they are ignored and MBWC produces no chars.
 This is problematic, as we will continue increasing sample size without
 producing a character. Eventually we will run out of Bytes and return with no
 progress made, the caller will keep all the data and run out of buffer

 Therefore, we need to stop at some point and declare the position "invalid".
 With WideChars it's easy: surrogates longer than 2 chars don't exist. With
 Multibyte UTF-8 can go for as long as 7 chars, so we'll settle at eight.

 Note that we may not notice invalid char if MBWC encounters another valid char
 while at the same 8 bytes. No way around this. We cannot test for
 ERROR_NO_UNICODE_TRANSLATION because incomplete sequences result in the same. }

function TMultibyteEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var i, pos, conv: integer;
  inp: array[0..7] of AnsiChar;
  outp: array[0..1] of WideChar;
  found: boolean;
begin
  SetLength(Result, MaxChars);
  conv := 0;
  pos := 1;
  while MaxChars>0 do begin

    i := 0;
    found := false;
    while i<Length(inp) do begin
      if AStream.Read(inp[i],1)<>1 then begin
        AStream.Seek(-(i-1),soFromCurrent);
        break;
      end;

      conv := MultiByteToWideChar(FCodepage, 0, @inp[0], i+1, @outp[0], Length(outp));
      if conv>=1 then begin
        found := true;
        break;
      end;

      Inc(i);
    end;

    if i>=Length(inp) then begin
      Result[pos] := FIllegalChar;
      AStream.Seek(-(i-1), soCurrent); //start with the next char
      continue;
    end;

    if (not found) or (conv>MaxChars) then begin
     //not enough bytes, or
     //want to write surrogate and not enough space
      if i>0 then AStream.Seek(-i, soCurrent); //maybe next time
      break;
    end;

    for i := 0 to conv-1 do begin
      Result[pos] := outp[i];
      Inc(pos);
      Dec(MaxChars);
    end;
  end;

  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TMultibyteEncoding.Write(AStream: TStream; const AData: UnicodeString);
var buf: AnsiString;
  written: integer;
begin
  if Length(AData)=0 then exit;
  SetLength(buf, Length(AData)*7); //there aren't any encodings with more than 7 byte chars
  written := WideCharToMultiByte(FCodepage, 0, PWideChar(AData), Length(AData), PAnsiChar(buf), Length(buf), nil, nil);
  if written=0 then
    RaiseLastOsError();
  AStream.Write(buf[1], written);
end;

constructor TAcpEncoding.Create;
begin
  inherited Create();
  FCodepage := CP_ACP;
end;
{$ENDIF}

class function TUTF8Encoding.GetBOM: TBytes;
begin
  Result := Bytes($EF, $BB, $BF);
end;

function TUTF8Encoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var b: array[0..5] of byte;
 { Thought of making it packed record ( b0,b1,b2,b3,b4,b5: byte; ) for speed,
  but the assembly is identical. }
  chno: integer;
  pc: PWideChar;
  i: integer;
begin
  SetLength(Result, MaxChars);
  if MaxChars=0 then exit;

  pc := PWideChar(Result);
  while MaxChars>0 do begin
    if AStream.Read(b[0],1)<>1 then break;
    chno := 0;
    i := 0;

    if (b[0] and UTF8_MASK1)=UTF8_VALUE1 then begin
     //Most common case, single-byte char
      pc^ := WideChar(b[0]);
    end else
    if (b[0] and UTF8_MASK2)=UTF8_VALUE2 then begin
      i := AStream.Read(b[1],1);
      if i<>1 then begin
        AStream.Seek(-(i+1),soFromCurrent);
        break;
      end;
      pc^ := WideChar( (b[0] and $1f) shl 6
        + (b[1] and $3f));
    end else
    if (b[0] and UTF8_MASK3)=UTF8_VALUE3 then begin
      i := AStream.Read(b[1],2);
      if i<>2 then begin
        AStream.Seek(-(i+1),soFromCurrent);
        break;
      end;
      pc^ := WideChar((b[0] and $0f) shl 12
        + (b[1] and $3f) shl 6
        + (b[2] and $3f));
    end else
    if (b[0] and UTF8_MASK4)=UTF8_VALUE4 then
    begin
      i := AStream.Read(b[1],3);
      if i<>3 then begin
        AStream.Seek(-(i+1),soFromCurrent);
        break;
      end;
      chno := (b[0] and $0f) shl 18
        + (b[1] and $3f) shl 12
        + (b[2] and $3f) shl 6
        + (b[3] and $3f);
     //will be stored below
    end else
    if (b[0] and UTF8_MASK5)=UTF8_VALUE5 then
    begin
      i := AStream.Read(b[1],4);
      if i<>4 then begin
        AStream.Seek(-(i+1),soFromCurrent);
        break;
      end;
      chno := (b[0] and $0f) shl 24
        + (b[1] and $3f) shl 18
        + (b[2] and $3f) shl 12
        + (b[3] and $3f) shl 6
        + (b[4] and $3f);
    end else
    if (b[0] and UTF8_MASK6)=UTF8_VALUE4 then
    begin
      i := AStream.Read(b[1],5);
      if i<>5 then begin
        AStream.Seek(-(i+1),soFromCurrent);
        break;
      end;
      chno := (b[0] and $0f) shl 30
        + (b[1] and $3f) shl 24
        + (b[2] and $3f) shl 18
        + (b[3] and $3f) shl 12
        + (b[4] and $3f) shl 6
        + (b[5] and $3f);
    end else
     //Broken, unsupported character, probably a remnant of a surrogate. Skip it.
      Dec(pc);

   //Store complicated characters by setting their chno insteead
    if chno=0 then begin
      Inc(pc);
      Dec(MaxChars);
    end
    else begin
      if chno<$10000 then begin
        pc^ := WideChar(chno);
        Inc(pc);
        Dec(MaxChars);
      end else
      if MaxChars<2 then begin
        AStream.Seek(-(i+1),soFromCurrent);
        break;
      end else
      begin
        chno := chno-$10000;
        pc[0] := WideChar(UTF16_LEAD + chno shr 10);
        pc[1] := WideChar(UTF16_TRAIL + chno and $03FF);
        Inc(pc, 2);
        Dec(MaxChars, 2);
      end;
    end;

  end;

  if MaxChars>0 then
    SetLength(Result, Length(Result)-MaxChars);
end;

procedure TUTF8Encoding.Write(AStream: TStream; const AData: UnicodeString);
var i: integer;
  w: integer;
begin
  for i := 1 to Length(AData) do begin
    w := Word(AData[i]);
   //TODO: UTF16 surrogates

    if (w and UTF8_WRITE1)=0 then
      _fwrite1(AStream, w mod 256)
    else
    if (w and UTF8_WRITE2)=0 then
      _fwrite2(AStream,
          (UTF8_VALUE2 or (w shr 6))
        + (UTF8_VALUEC or (w and $3f)) shl 8
      )
    else
    //TODO: Write 3,4,5-byte sequences when needed
      _fwrite3(AStream,
          (UTF8_VALUE3 or (w shr 12))
        + (UTF8_VALUEC or ((w shr 6) and $3f)) shl 8
        + (UTF8_VALUEC or (w and $3f)) shl 16
      );
  end;
end;

class function TUTF16LEEncoding.GetBOM: TBytes;
begin
  Result := Bytes($FF, $FE);
end;

function TUTF16LEEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var read_sz: integer;
begin
  SetLength(Result, MaxChars);
  read_sz := AStream.Read(Result[1], MaxChars*SizeOf(WideChar));
  if read_sz<MaxChars*SizeOf(WideChar) then
    SetLength(Result, read_sz div SizeOf(WideChar));
end;

procedure TUTF16LEEncoding.Write(AStream: TStream; const AData: UnicodeString);
begin
  if AData<>'' then
    AStream.Write(AData[1], Length(AData)*SizeOf(WideChar));
end;

class function TUTF16BEEncoding.GetBOM: TBytes;
begin
  Result := Bytes($FE, $FF);
end;

function TUTF16BEEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var read_sz: integer;
  i: integer;
begin
  SetLength(Result, MaxChars);
  read_sz := AStream.Read(Result[1], MaxChars*SizeOf(WideChar));
  if read_sz<MaxChars*SizeOf(WideChar) then
    SetLength(Result, read_sz div SizeOf(WideChar));
  for i := 1 to Length(Result) do
    Result[i] := WideChar(_swapw(Word(Result[i])));
end;

procedure TUTF16BEEncoding.Write(AStream: TStream; const AData: UnicodeString);
var i: integer;
begin
  for i := 1 to Length(AData) do
    _fwrite2(AStream, _swapw(Word(AData[i])));
end;


{ Wakan encodings }

const IS_EUC=1;
      IS_HALFKATA=2;
      IS_SJIS1=3;
      IS_SJIS2=4;
      IS_MARU=5;
      IS_NIGORI=6;
      IS_JIS=7;
      JIS_NL=10;          // New Line char.
      JIS_CR=13;         // Carrage Return.
      JIS_ESC=27;          // Escape.
      JIS_SS2=142;         // Half-width katakana marker.

type TUTFArray=array[0..3] of byte;

function SJIS2JIS(w:word):word;
var b1,b2,adjust, rowOffset, cellOffset:byte;
begin
  b1:=w div 256;
  b2:=w mod 256;
  if b2<159 then adjust:=1 else adjust:=0;
  if b1<160 then rowOffset:=112 else rowOffset:=176;
  if adjust=1 then begin if b2>127 then cellOffset:=32 else cellOffset:=31 end else cellOffset:=126;
  b1 := ((b1-rowOffset) shl 1) - adjust;
  b2 := b2-cellOffset;
  result:=b1*256+b2;
end;

function JIS2SJIS(w:word):word;
var b1,b2,b1n:byte;
begin
  b1:=w div 256;
  b2:=w mod 256;
  if b1 mod 2<>0 then begin if b2>95 then b2:=b2+32 else b2:=b2+31 end else b2:=b2+126;
  b1n:=((b1+1) shr 1);
  if b1<95 then b1n:=b1n+112 else b1n:=b1n+176;
  result:=b1n*256+b2;
end;

function JIS2Unicode(w:word):word;
begin
  result:=0;//default case
  case w of
    $0000..$007e:result:=w; // ascii
    $0080..$00ff:result:=Table_ExtASCII[w-128];
    $2330..$237a:result:=w-$2330+$ff10; // japanese ASCII
    $2421..$2473:result:=w-$2421+$3041; // hiragana
    $2521..$2576:result:=w-$2521+$30a1; // katakana
    $2621..$2658:if w<=$2631 then result:=w-$2621+$0391 else
                 if w<=$2638 then result:=w-$2621+$0392 else
                 if w< $2641 then result:=0 else
                 if w<=$2651 then result:=w-$2621+$0391 else
                 if w<=$2658 then result:=w-$2621+$0392; // greek
    $2721..$2771:if w<=$2726 then result:=w-$2721+$0410 else
                 if w= $2727 then result:=$0401 else
                 if w<=$2741 then result:=w-$2722+$0410 else
                 if w< $2751 then result:=0 else
                 if w<=$2756 then result:=w-$2751+$0430 else
                 if w= $2757 then result:=$0451 else
                 if w<=$2771 then result:=w-$2752+$0430; // cyrillic
    $3021..$7426:if ((w and $7f)<$21) or ((w and $7f)>$7e) then result:=0 else
                   result:=Table_Kanji[((w-$3021) div 256)*94+((w-$3021) mod 256)]; // kanji
    $2121..$217e:result:=Table_Misc[w-$2121];
    $2221..$227e:result:=Table_Misc[w-$2221+94];
    $2821..$2840:result:=Table_Misc[w-$2821+94+94];
  end;
end;

function Unicode2UTF(ch:word):TUTFArray;
begin
  if (ch and UTF8_WRITE1)=0 then
  begin
    result[0]:=1;
    result[1]:=ch;
    exit;
  end;
  if (ch and UTF8_WRITE2)=0 then
  begin
    result[0]:=2;
    result[1]:=(UTF8_VALUE2 or (ch shr 6));
    result[2]:=(UTF8_VALUEC or (ch and $3f));
  end;
  result[0]:=3;
  result[1]:=UTF8_VALUE3 or (ch shr 12);
  result[2]:=UTF8_VALUEC or ((ch shr 6) and $3f);
  result[3]:=UTF8_VALUEC or (ch and $3f);
end;

function Unicode2JIS(w:word):word;
var i:integer;
begin
  result:=0;
  case w of
    $0000..$007e:result:=w; // Ascii
    $3041..$3093:result:=w-$3041+$2421; // Hiragana
    $30a1..$30f6:result:=w-$30a1+$2521; // Katakana
    $0391..$03c9:if w<=$03a1 then result:=w-$0391+$2621 else
                 if w= $03a2 then result:=0 else
                 if w<=$03a9 then result:=w-$0392+$2621 else
                 if w< $03b1 then result:=0 else
                 if w<=$03c1 then result:=w-$0391+$2621 else
                 if w= $03c2 then result:=0 else
                 if w<=$03c9 then result:=w-$0392+$2621; // greek
    $0401       :result:=$2727;
    $0451       :result:=$2757;
    $0410..$044f:if w<=$0415 then result:=w-$0410+$2721 else
                 if w<=$042f then result:=w-$0416+$2728 else
                 if w<=$0435 then result:=w-$0430+$2751 else
                 if w<=$044f then result:=w-$0436+$2758; // cyrillic
    $ff10..$ff5a:result:=w-$ff10+$2330;
    $feff       :result:=w;
    $fffe       :result:=w;
  end;
  if result<>0 then exit;
  for i:=0 to NUMBER_KANJIUNICODE-1 do if Table_Kanji[i]=w then
  begin
    result:=i div 94;
    result:=(((result+$30) shl 8) or (i-result*94)+$21);
    exit;
  end;
  for i:=0 to NUMBER_MISCUNICODE-1 do if Table_Misc[i]=w then
  begin
    case i div 94 of
      0:result:=$2121+i;
      1:result:=$2221+i-94;
      2:result:=$2821+i-94-94;
    end;
    exit;
  end;
  for i:=0 to NUMBER_EXTUNICODE-1 do if Table_ExtASCII[i]=w then
  begin
    result:=i+$80;
    exit;
  end;
  result:=0;
end;

function UTF2Unicode(b1,b2,b3,b4:byte;var inc:byte):word;
begin
  if (b1 and UTF8_MASK1)=UTF8_VALUE1 then
  begin
    result:=b1;
    inc:=1;
    exit;
  end else if (b1 and UTF8_MASK2)=UTF8_VALUE2 then
  begin
    result:=((b1 and $1f) shl 6) or (b2 and $3f);
    inc:=2;
    exit;
  end else if (b1 and UTF8_MASK3)=UTF8_VALUE3 then
  begin
    result:=((b1 and $0f) shl 12) or ((b2 and $3f) shl 6) or (b3 and $3f);
    inc:=3;
    exit;
  end else if (b1 and UTF8_MASK4)=UTF8_VALUE4 then
  begin
    result:=$ffff;
    inc:=4;
  end else begin
    Result:=b1; //because we don't know what else to do
    inc:=1;
  end;
end;

function _is(b:word;cl:byte):boolean;
begin
  case cl of
    IS_EUC:result:=(b>=161) and (b<=254);
    IS_HALFKATA:result:=(b>=161) and (b<=223);
    IS_SJIS1:result:=((b>=129) and (b<=159)) or ((b>=224) and (b<=239));
    IS_SJIS2:result:=(b>=64) and (b<=252);
    IS_MARU:result:=(b>=202) and (b<=206);
    IS_NIGORI:result:=((b>=182) and (b<=196)) or ((b>=202) and (b<=206)) or (b=179);
    IS_JIS:result:=(b and $7f00)>0;
  else result:=false;
  end;
end;

function TEUCEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var b1, b2: byte;
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if _is(b1,IS_EUC) then begin
      if AStream.Read(b2,1)<>1 then break;
      Result[pos] := WideChar(JIS2Unicode((b1*256+b2) and $7f7f));
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TEUCEncoding.Write(AStream: TStream; const AData: UnicodeString);
var i: integer;
  w: word;
begin
  for i := 1 to Length(AData) do begin
    w := Unicode2JIS(Word(AData[i]));
    if _is(w,IS_JIS) then
      _fwrite2(AStream,
          ((w shr 8) or $80)
        + ((w mod 256) or $80) shl 8
      )
    else if (w and $80)>0 then
      _fwrite2(AStream,
           JIS_SS2
        + (w and $7f) shl 8
      )
    else
      _fwrite1(AStream, w);
  end;
end;

function TSJISEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var b1, b2: byte;
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if _is(b1,IS_SJIS1) then begin
      if AStream.Read(b2,1)<>1 then break;
      if _is(b2,IS_SJIS2) then
        Result[pos] := WideChar(JIS2Unicode(SJIS2JIS(b1*256+b2)))
      else
        Result[pos] := WideChar(JIS2Unicode(b1*256+b2));
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TSJISEncoding.Write(AStream: TStream; const AData: UnicodeString);
var i: integer;
  w: word;
begin
  for i := 1 to Length(AData) do begin
    w:=Unicode2JIS(Word(AData[i]));
    if _is(w,IS_JIS) then
    begin
      w:=jis2sjis(w);
      _fwrite2(AStream, _swapw(w));
    end else
      _fwrite1(AStream, w);
  end;
end;

function TBaseJISEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var b1, b2: byte;
  inp_intwobyte: boolean;
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  inp_intwobyte := false;
  while true do begin
    if AStream.Read(b1,1)<>1 then break;

    if b1=JIS_ESC then
    begin
      if AStream.Read(b2,1)<>1 then break;
      if (b2=ord('$')) or (b2=ord('(')) then AStream.Read(b1,1); //don't care about the result
      if (b2=ord('K')) or (b2=ord('$')) then inp_intwobyte:=true else inp_intwobyte:=false;
     //Do not exit, continue to the next char
    end else begin
      if (b1=JIS_NL) or (b1=JIS_CR) then
        Result[pos] := WideChar(b1)
      else begin
        if AStream.Read(b2,1)<>1 then break;
        if inp_intwobyte then
          Result[pos] := WideChar(JIS2Unicode(b1*256+b2))
        else
          Result[pos] := WideChar(b1);
      end;

      Inc(pos);
      Dec(MaxChars);
      if MaxChars<=0 then break;
    end;
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TBaseJISEncoding.Write(AStream: TStream; const AData: UnicodeString);
var i: integer;
  w: word;
begin
  for i := 1 to Length(AData) do begin
    w := Word(AData[i]);

    if (w=13) or (w=10) then
    begin
      _fputend(AStream);
      _fwrite1(AStream, w);
    end else
    begin
      w:=Unicode2JIS(w);
      if _is(w,IS_JIS) then
      begin
        _fputstart(AStream);
        _fwrite2(AStream, _swapw(w));
      end else begin
        _fputend(AStream);
        _fwrite1(AStream, w);
      end;
    end;
  end;
end;

procedure TBaseJISEncoding._fputstart(AStream: TStream);
begin
  if intwobyte then exit;
  intwobyte:=true;
  AStream.Write(StartMark[0], Length(StartMark));
end;

procedure TBaseJISEncoding._fputend(AStream: TStream);
begin
  if not intwobyte then exit;
  intwobyte:=false;
  AStream.Write(EndMark[0], Length(EndMark));
end;

constructor TJISEncoding.Create;
begin
  inherited;
  StartMark := Bytes(JIS_ESC, ord('B'), ord('$'));
  EndMark := Bytes(JIS_ESC, ord('('), ord('J'));
end;

constructor TOldJISEncoding.Create;
begin
  inherited;
  StartMark := Bytes(JIS_ESC, ord('@'), ord('$'));
  EndMark := Bytes(JIS_ESC, ord('('), ord('J'));
end;

constructor TNECJISEncoding.Create;
begin
  inherited;
  StartMark := Bytes(JIS_ESC, ord('K'));
  EndMark := Bytes(JIS_ESC, ord('H'));
end;

function TGBEncoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var b1, b2: byte;
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if (b1>=$a1) and (b1<=$fe) then
    begin
      if AStream.Read(b2,1)<>1 then break;
      if (b2>=$a1) and (b2<=$fe) then
        Result[pos] := WideChar(Table_GB[(b1-$a0)*96+(b2-$a0)])
      else
        Result[pos] := WideChar(b1*256+b2);
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TGBEncoding.Write(AStream: TStream; const AData: UnicodeString);
var i,j: integer;
  w: word;
begin
  for j := 1 to Length(AData) do begin
    w := Word(AData[j]);
    if w<128 then
      _fwrite1(AStream, byte(w))
    else
    begin
      for i:=0 to 96*96-1 do if Table_GB[i]=w then begin
        _fwrite2(AStream,
            (i mod 96+$a0) shl 8
          + (i div 96+$a0)
        );
        exit;
      end;
      _fwrite2(AStream,
        (w mod 256) shl 8
        + (w div 256)
      );
    end;
  end;
end;

function TBIG5Encoding.Read(AStream: TStream; MaxChars: integer): UnicodeString;
var b1, b2: byte;
  pos: integer;
begin
  SetLength(Result, MaxChars);
  pos := 1;
  while MaxChars>0 do begin
    if AStream.Read(b1,1)<>1 then break;

    if (b1>=$a1) and (b1<=$fe) then
    begin
      if AStream.Read(b2,1)<>1 then break;
      if (b2>=$40) and (b2<=$7f) then
        Result[pos] := WideChar(Table_Big5[(b1-$a0)*160+(b2-$40)])
      else
      if (b2>=$a1) and (b2<=$fe) then
        Result[pos] := WideChar(Table_Big5[(b1-$a0)*160+(b2-$a0)])
      else
        Result[pos] := WideChar(b1*256+b2);
    end else
      Result[pos] := WideChar(b1);

    Inc(pos);
    Dec(MaxChars);
  end;
  if pos<Length(Result) then
    SetLength(Result, pos-1);
end;

procedure TBIG5Encoding.Write(AStream: TStream; const AData: UnicodeString);
var i, j: integer;
  w: word;
begin
  for j := 1 to Length(AData) do begin
    w := Word(AData[j]);
    if w<128 then
      _fwrite1(AStream, byte(w))
    else
    begin
      for i:=0 to 96*160-1 do if Table_GB[i]=w then begin
        _fwrite2(AStream,
            (i mod 96+$a0) shl 8
          + (i div 96+$a0)
        );
        exit;
      end;
      _fwrite2(AStream, _swapw(w));
    end;
  end;
end;

function Conv_DetectType(AStream: TStream): CEncoding;
begin
  if not Conv_DetectType(AStream, Result) then
    Result := nil;
end;

{ Detects file encoding. Returns true if it's truly detected (i.e. through BOM),
 or false if it's a best guess. }
function Conv_DetectType(AStream: TStream; out AEncoding: CEncoding): boolean;
var i,b,j:integer;
  w: word;
  eucsjis:boolean;
  asciionly:boolean;
  failed_le, failed_be: boolean;
begin
  AStream.Seek(0, soBeginning);
  AEncoding := nil;
  Result := false;

  if TUTF16LEEncoding.ReadBom(AStream) then begin
    AEncoding := TUTF16LEEncoding;
    Result := true;
    exit;
  end;

  if TUTF16BEEncoding.ReadBom(AStream) then begin
    AEncoding := TUTF16BEEncoding;
    Result := true;
    exit;
  end;

  if TUTF8Encoding.ReadBom(AStream) then begin
    AEncoding := TUTF8Encoding;
    Result := true;
    exit;
  end;

 { UTF16LE/BE first try }
  failed_le:=false;
  failed_be:=false;
  eucsjis:=true;
  while (AStream.Read(w, 2)=2) and not (failed_be and failed_le) do
  begin
    if Unicode2JIS(w)=0 then failed_le:=true;
    if Unicode2JIS(_swapw(w))=0 then failed_be:=true;
    if (w and $8080)<>$8080 then eucsjis:=false;
  end;
  if eucsjis then
    AEncoding:=nil
  else
  if failed_be and not failed_le then
    AEncoding := TUTF16LEEncoding
  else
  if failed_le and not failed_be then
    AEncoding := TUTF16BEEncoding;
  if AEncoding<>nil then exit;

  AStream.Seek(0, soBeginning);
  asciionly:=true;
  AEncoding := TUTF8Encoding;
  i := 0; //zero higher bytes
  while (AStream.Read(i, 1)=1) and (AEncoding=TUTF8Encoding) do
  begin
    b:=0;
    if (i and UTF8_MASK1)=UTF8_VALUE1 then b:=0 else
    if (i and UTF8_MASK2)=UTF8_VALUE2 then b:=1 else
    if (i and UTF8_MASK3)=UTF8_VALUE3 then b:=2 else
    if (i and UTF8_MASK4)=UTF8_VALUE4 then b:=3 else AEncoding:=nil;
    if b>0 then asciionly:=false;
    for j:=0 to b-1 do
    begin
      if AStream.Read(i, 1)=1 then //else do not drop the encoding, tolerate missing bytes, stream might have been cut short
        if (i and $c0)<>$80 then AEncoding:=nil;
    end;
  end;
  if asciionly then AEncoding:=nil;
  if AEncoding<>nil then exit;

  AStream.Seek(0, soBeginning);
  AEncoding := TAsciiEncoding;
  eucsjis:=false;
  i:=0;
  while (AEncoding = TAsciiEncoding) or ((AEncoding=nil) and eucsjis) do
  begin
    if AStream.Read(i, 1)<>1 then break;

    if i=JIS_ESC then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if i=ord('$') then
      begin
        if AStream.Read(i, 1)<>1 then i:=-1;
        if i=ord('B') then AEncoding:=TJISEncoding;
        if i=ord('@') then AEncoding:=TOldJISEncoding;
      end else
      if i=ord('K') then AEncoding:=TNECJISEncoding;
    end else if i=JIS_SS2 then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if (i>=161) and (i<=223) then begin
        AEncoding:=nil;
        eucsjis:=true;
      end
      else if (i<>127) and (i>=64) and (i<=252) then AEncoding:=TSJISEncoding;
    end else if (i>=129) and (i<=159) then AEncoding:=TSJISEncoding
    else if (i>=161) and (i<=223) then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if (i>=240) and (i<=254) then AEncoding:=TEUCEncoding
      else if (i>=161) and (i<=223) then begin
        AEncoding:=nil;
        eucsjis:=true;
      end
      else if (i>=224) and (i<=239) then
      begin
        AEncoding:=nil;
        eucsjis:=true;
        while ((i>=64) and (AEncoding=nil) and eucsjis) do
        begin
          if i>=129 then
          begin
            if (i<=141) or ((i>=143) and (i<=159)) then AEncoding:=TSJISEncoding else
            if (i>=253) and (i<=254) then AEncoding:=TEUCEncoding;
          end;
          if AStream.Read(i, 1)<>1 then break;
        end;
      end else if i<=159 then AEncoding:=TSJISEncoding;
    end else if (i>=240) and (i<=254) then AEncoding:=TEUCEncoding
    else if (i>=224) and (i<=239) then
    begin
      if AStream.Read(i, 1)<>1 then i:=-1;
      if ((i>=64) and (i<=126)) or ((i>=128) and (i<=160)) then AEncoding:=TSJISEncoding
      else if (i>=253) and (i<=254) then AEncoding:=TEUCEncoding
      else if (i>=161) and (i<=252) then begin
        AEncoding:=nil;
        eucsjis:=true;
      end;
    end;
  end;

  if (AEncoding=nil) and eucsjis then
    AEncoding:=TSJISEncoding;
end;

function Conv_DetectType(const AFilename: TFilename): CEncoding;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(
    TFileStream.Create(AFilename, fmOpenRead), true);
  try
    Result := Conv_DetectType(fsr);
  finally
    FreeAndNil(fsr);
  end;
end;

function Conv_DetectType(const AFilename: TFilename; out AEncoding: CEncoding): boolean;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(
    TFileStream.Create(AFilename, fmOpenRead), true);
  try
    Result := Conv_DetectType(fsr, AEncoding);
  finally
    FreeAndNil(fsr);
  end;
end;

function OpenStream(const AStream: TStream; AOwnsStream: boolean; AEncoding: CEncoding): TStreamDecoder;
var fsr: TStreamReader;
begin
  fsr := TStreamReader.Create(AStream, AOwnsStream);
  try
    if AEncoding=nil then
      if not Conv_DetectType(fsr, AEncoding) and (AEncoding=nil) {not even a best guess} then
        AEncoding := TAsciiEncoding;
    fsr.Seek(0, soBeginning);
    Result := TStreamDecoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;

function WriteToStream(const AStream: TStream; AOwnsStream: boolean; AEncoding: CEncoding): TStreamEncoder;
var fsr: TStreamWriter;
begin
  fsr := TStreamWriter.Create(AStream, AOwnsStream);
  try
    Result := TStreamEncoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;

function OpenTextFile(const AFilename: TFilename; AEncoding: CEncoding = nil): TStreamDecoder;
begin
  Result := OpenStream(
    TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone),
    {OwnsStream=}true,
    AEncoding
  );
end;

function CreateTextFile(const AFilename: TFilename; AEncoding: CEncoding): TStreamEncoder;
var fsr: TStreamWriter;
begin
  fsr := TStreamWriter.Create(
    TFileStream.Create(AFilename, fmCreate),
    {OwnsStream=}true
  );
  try
    Result := TStreamEncoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;

function AppendToTextFile(const AFilename: TFilename; AEncoding: CEncoding = nil): TStreamEncoder;
var fsr: TStreamWriter;
begin
  fsr := TStreamWriter.Create(
    TFileStream.Create(AFilename, fmOpenReadWrite), //read is for encoding detection
    {OwnsStream=}true
  );
  try
    if AEncoding=nil then
     //Conv_DetectType by filename since TStreamWriter cannot read
      if not Conv_DetectType(AFilename, AEncoding) and (AEncoding=nil) {not even a best guess} then
        AEncoding := TAsciiEncoding;
    fsr.Seek(0, soEnd);
    Result := TStreamEncoder.Open(fsr, AEncoding.Create, {OwnsStream=}true);
  except
    FreeAndNil(fsr);
    raise;
  end;
end;


//Finds a class by it's name. Good to store encoding selection in a permanent way.
function FindEncoding(const AClassName: string): CEncoding;
begin
 //Stupid for now
  if AClassName='TAsciiEncoding' then
    Result := TAsciiEncoding
  else
 {$IFDEF MSWINDOWS}
  if AClassName='TAcpEncoding' then
    Result := TAcpEncoding
  else
 {$ENDIF}
  if AClassName='TUTF8Encoding' then
    Result := TUTF8Encoding
  else
  if AClassName='TUTF16LEEncoding' then
    Result := TUTF16LEEncoding
  else
  if AClassName='TUTF16BEEncoding' then
    Result := TUTF16BEEncoding
  else
  if AClassName='TEUCEncoding' then
    Result := TEUCEncoding
  else
  if AClassName='TSJISEncoding' then
    Result := TSJISEncoding
  else
  if AClassName='TJISEncoding' then
    Result := TJISEncoding
  else
  if AClassName='TOldJISEncoding' then
    Result := TOldJISEncoding
  else
  if AClassName='TNECJISEncoding' then
    Result := TNECJISEncoding
  else
  if AClassName='TGBEncoding' then
    Result := TGBEncoding
  else
  if AClassName='TBIG5Encoding' then
    Result := TBIG5Encoding
  else
    Result := nil;

end;


{ Compares binary data in streams }

function CompareStreams(const AStream1, AStream2: TStream): boolean;
var
  b1, b2: byte;
  r1, r2: boolean;
begin
 { Can be easily made somewhat faster by reading in dwords and comparing
  only the number of bytes read (e.g. case 1: 2: 3: 4: if (d1 & $000F) == etc.) }
  Result := true;
  while true do begin
    r1 := (AStream1.Read(b1,1)=1);
    r2 := (AStream2.Read(b2,1)=1);
    if r1 xor r2 then
      Result := false;
    if b1<>b2 then
      Result := false;
    if (not r1) or (not Result) then //not Result => diff; not r1 => both over
      break;
  end;
end;

function CompareFiles(const AFilename1, AFilename2: TFilename): boolean;
var f1, f2: TStream;
begin
  f1 := nil;
  f2 := nil;
  try
    f1 := TStreamReader.Create(
      TFileStream.Create(AFilename1, fmOpenRead),
      true
    );
    f2 := TStreamReader.Create(
      TFileStream.Create(AFilename2, fmOpenRead),
      true
    );
    Result := CompareStreams(f1, f2);
  finally
    FreeAndNil(f2);
    FreeAndNil(f1);
  end;
end;


{ Compatibility functions }

function AnsiFileReader(const AFilename: TFilename): TStreamDecoder;
begin
 {$IFDEF MSWINDOWS}
  Result := OpenTextFile(AFilename, TAcpEncoding);
 {$ELSE}
  Result := OpenTextFile(AFilename, TAsciiEncoding);
 {$ENDIF}
end;

function UnicodeFileReader(const AFilename: TFilename): TStreamDecoder;
begin
  Result := OpenTextFile(AFilename, TUnicodeEncoding);
end;

function ConsoleReader(AEncoding: TEncoding = nil): TStreamDecoder;
var AStream: TStream;
 {$IFDEF MSWINDOWS}
  AInputHandle: THandle;
 {$ENDIF}
begin
 {$IFDEF MSWINDOWS}
  AInputHandle := GetStdHandle(STD_INPUT_HANDLE);
  AStream := THandleStream.Create(AInputHandle);
 {$ELSE}
 {$IFDEF FPC}
  AStream := TIOStream.Create(iosInput);
 {$ELSE}
  raise Exception.Create('Console reader not supported on this platform/compiler.');
 {$ENDIF}
 {$ENDIF}

  if AEncoding=nil then begin
  {$IFDEF MSWINDOWS}
  { If our outputs are redirected, we *really* do not know what to expect, so we
   default to UTF8 (allows for ASCII and interoperability).
   But if we are sure we're reading from console, we may optimize and use the
   console CP. }
    if GetFileType(AInputHandle)=FILE_TYPE_CHAR then
      AEncoding := TMultibyteEncoding.Create(GetConsoleOutputCP())
    else
      AEncoding := TUTF8Encoding.Create();
  {$ELSE}
      AEncoding := TUTF8Encoding.Create();
  {$ENDIF}
  end;

  Result := TStreamDecoder.Open(
    AStream,
    AEncoding,
    {OwnsStream=}true
  );
end;

function UnicodeStreamReader(AStream: TStream; AOwnsStream: boolean = false): TStreamDecoder;
begin
  Result := TStreamDecoder.Open(AStream, TUnicodeEncoding.Create, AOwnsStream);
end;

function FileReader(const AFilename: TFilename): TStreamDecoder;
begin
 {$IFDEF UNICODE}
  Result := UnicodeFileReader(AFilename);
 {$ELSE}
  Result := AnsiFileReader(AFilename);
 {$ENDIF}
end;

function AnsiFileWriter(const AFilename: TFilename): TStreamEncoder;
begin
 {$IFDEF MSWINDOWS}
  Result := CreateTextFile(AFilename, TAcpEncoding);
 {$ELSE}
  Result := CreateTextFile(AFilename, TAsciiEncoding);
 {$ENDIF}
end;

function UnicodeFileWriter(const AFilename: TFilename): TStreamEncoder;
begin
  Result := CreateTextFile(AFilename, TUnicodeEncoding);
end;

function ConsoleWriter(AEncoding: TEncoding): TStreamEncoder;
var AStream: TStream;
 {$IFDEF MSWINDOWS}
  AOutputHandle: THandle;
 {$ENDIF}
begin
 {$IFDEF MSWINDOWS}
  AOutputHandle := GetStdHandle(STD_OUTPUT_HANDLE);
  AStream := THandleStream.Create(AOutputHandle);
 {$ELSE}
 {$IFDEF FPC}
  AStream := TIOStream.Create(iosOutput);
 {$ELSE}
  raise Exception.Create('Console writer not supported on this platform/compiler.');
 {$ENDIF}
 {$ENDIF}

  if AEncoding=nil then begin
  {$IFDEF MSWINDOWS}
  { If our outputs are redirected, we *really* do not know which codepage to
   output in, so we use UTF8 as default.
   But if we are sure we're printing to console, we may optimize and use the
   console CP. }
    if GetFileType(AOutputHandle)=FILE_TYPE_CHAR then
      AEncoding := TMultibyteEncoding.Create(GetConsoleOutputCP())
    else
      AEncoding := TUTF8Encoding.Create();
  {$ELSE}
      AEncoding := TUTF8Encoding.Create();
  {$ENDIF}
  end;

  Result := TStreamEncoder.Open(
    AStream,
    AEncoding,
    {OwnsStream=}true
  );
end;

function UnicodeStreamWriter(AStream: TStream; AOwnsStream: boolean = false): TStreamEncoder;
begin
  Result := TStreamEncoder.Open(AStream, TUnicodeEncoding.Create, AOwnsStream);
end;

function FileWriter(const AFilename: TFilename): TStreamEncoder;
begin
 {$IFDEF UNICODE}
  Result := UnicodeFileWriter(AFilename);
 {$ELSE}
  Result := AnsiFileWriter(AFilename);
 {$ENDIF}
end;


{ Reads out everything there is in the file and counts lines. Do not forget to
 rewind later. Do not do with Console, TCP and other non-rewindable streams.
 Avoid counting lines beforehand where possible, prefer just parsing for however
 much lines there is. }
function GetLineCount(AText: TStreamDecoder): integer;
var ln: UnicodeString;
begin
  Result := 0;
  while AText.ReadLn(ln) do
    Inc(Result);
end;

end.
