unit iconv;
interface
uses
  Windows,Classes,SysUtils;

const
  iconv_lib='libiconv2.dll';

type
  size_t=Cardinal;
  iconv_t=Pointer;
  argptr=iconv_t;

  EIconvError = class(Exception)
  public
    ErrorCode: integer;
    constructor Create(AErrorCode: integer; const AMsg: string);
  end;

const
  E2BIG=7;
  EINVAL=22;
  EILSEQ2=42;
  EILSEQ=84;

const
  ICONV_TRIVIALP=0;//int *argument
  ICONV_GET_TRANSLITERATE=1;//int *argument
  ICONV_SET_TRANSLITERATE=2;//const int *argument
  ICONV_GET_DISCARD_ILSEQ=3;//Int*argument
  ICONV_SET_DISCARD_ILSEQ=4;//const Int*argument

const
  LC_ALL      = 1;
  LC_COLLATE  = 2;
  LC_CTYPE    = 3;
  LC_MONETARY = 4;
  LC_NUMERIC  = 5;
  LC_TIME     = 6;

function errno:PInteger; cdecl;external'msvcrt.dll' Name'_errno';
function setlocale(category:integer;const locale:PAnsiChar):PAnsiChar; cdecl;external'msvcrt.dll' Name'setlocale';
function iconv_open(tocode:PAnsiChar;fromcode:PAnsiChar):iconv_t; cdecl;external iconv_lib Name'libiconv_open';
function iconv_convert(cd:iconv_t;var inbuf:Pointer;var inbytesleft:size_t;var outbuf:Pointer;var outbytesleft:size_t):size_t; cdecl;external iconv_lib Name'libiconv';
function iconv_close(cd:iconv_t):Integer; cdecl;external iconv_lib Name'libiconv_close';
function iconvctl(cd:iconv_t;request:Integer;argument:argptr):Integer; cdecl;external iconv_lib Name'libiconvctl';

{ Generic iconv translation routine. }
function iconv2(cd: iconv_t; inbuf: Pointer; insz: size_t): AnsiString; overload;
function iconv2(cd: iconv_t; const ln: UnicodeString): AnsiString; overload;

implementation

constructor EIconvError.Create(AErrorCode: integer; const AMsg: string);
begin
  inherited Create(AMsg);
  ErrorCode := AErrorCode;
end;
 
procedure ConvertStreams(cd:iconv_t;InStream,OutStream:TStream);
const
  BufferSize=4096;
var
  inbuf:array[0..BufferSize*2-1] of Char;
  insize,inbufsize,inbufrest:DWORD;
  inptr:Pointer;
  function Convert:Boolean;
  var
    outbuf:array[0..BufferSize-1] of Char;
    outptr:Pointer;
    outsize:DWORD;
    res:Integer;
  begin
    Result:=True;
    outptr:=@outbuf;
    outsize:=SizeOf(outbuf);
    res:=iconv_convert(cd,inptr,insize,outptr,outsize);
    if outptr<>@outbuf then
      OutStream.WriteBuffer(outbuf,PChar(outptr)-@outbuf);
    if res=-1 then begin
      case errno^ of
        EILSEQ:raise Exception.Create('cannot convert');
        EINVAL:begin
            if (inbufsize=0)or(insize>BufferSize) then
              raise Exception.Create('incomplete character or shift sequence')
            else begin
              inbufrest:=insize;
              Move(inptr^,inbuf[BufferSize-insize],insize);
              Result:=False;
            end;
          end;
        E2BIG:;
      else
        raise Exception.Create('unknown error')
      end;
    end;
  end;
begin
  inbufrest:=0;
  while True do begin
    inbufsize:=InStream.Read(inbuf[BufferSize],BufferSize);
    if inbufsize=0 then
      if inbufrest=0 then begin
        inptr:=nil;
        Convert;
        Exit;
      end
      else
        raise Exception.Create('incomplete character or shift sequence')
    else begin
      inptr:=@inbuf[BufferSize-inbufrest];
      insize:=inbufrest+inbufsize;
      inbufrest:=0;
      while (insize>0)and Convert do
        ;
    end;
  end;
end;


function iconv2(cd: iconv_t; inbuf: pointer; insz: size_t): AnsiString;
var outbuf: pointer;
  outsz, oldsz: size_t;
  err: integer;
begin
  if insz<=0 then begin
    Result := '';
    exit;
  end;

  SetLength(Result, insz);
  outsz := Length(Result);
  outbuf := @Result[1];

  while insz>0 do begin
    if iconv_convert(cd, inbuf, insz, outbuf, outsz)=size_t(-1) then begin
      err := errno^;
      case err of
        E2BIG: begin end; //nothing: expand buffer
        EINVAL: raise EIConvError.Create(err, 'Invalid byte sequence');
        EILSEQ,
        EILSEQ2: raise EIConvError.Create(err, 'Cannot convert symbol to the target codepage');
      else
        raise EIConvError.Create(err, 'Unexpected IConv error');
      end;
    end else
      err := 0;

    if err=0 then break;
    if err=E2BIG then begin //realloc
      oldsz := size_t(Length(Result))-outsz;
      outsz := oldsz + insz*2 + 10; //should usually be enough + at least 10 bytes anyway (at least one char in any encoding)
      SetLength(Result, outsz);
      outbuf := @Result[oldsz+1];
    end;
  end;

  if insz<>0 then
    raise EIconvError.Create(-1, 'iconv completed successfully but not all data converted');
  SetLength(Result, Length(Result)-integer(outsz)); //trim
end;

function iconv2(cd: iconv_t; const ln: UnicodeString): AnsiString;
begin
  Result := iconv2(cd, @ln[1], Length(ln)*SizeOf(WideChar));
end;

end.