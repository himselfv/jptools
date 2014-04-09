unit ConsoleToolbox;
{ Basic toolbox for building command line apps in this package }

interface
uses SysUtils;

{ Cast BadUsage(err) to terminate app and output usage information, perhaps with
 a error message. }
type
  EBadUsage = class(Exception);

procedure BadUsage(const AMessage: string = '');

{ Returns the name of a current program }
function ProgramName: string;

type
  TCommandLineApp = class
  protected
    function HandleSwitch(const s: string; var i: integer): boolean; virtual;
    function HandleParam(const s: string; var i: integer): boolean; virtual;
  public
    procedure Init; virtual; //Initialize any variables
    procedure ParseCommandLine; virtual; //usually you don't need to override this
    procedure Run; virtual; //do actual work
    procedure ShowUsage; overload; virtual; //override this one preferably, the other one resorts to it
    procedure ShowUsage(const AMessage: string); overload; virtual;
  end;
  CCommandLineApp = class of TCommandLineApp;

{ Application instance. Set it if you create application object by hand }
var
  Application: TCommandLineApp;

{ Creates and runs a command line application in a default fashion.
 You may write your own version of this if you need anything more fancy. }
procedure RunApp(const AAppClass: CCommandLineApp);

implementation

procedure BadUsage(const AMessage: string);
begin
  raise EBadUsage.Create(AMessage);
end;

function ProgramName: string;
begin
  Result := ExtractFilename(ParamStr(0));
end;

{ Override to initialize any variables }
procedure TCommandLineApp.Init;
begin
end;

{ To add any after-the-parsing checks, override, call inherited then check. }
procedure TCommandLineApp.ParseCommandLine;
var i: integer;
  s: string;
begin
  i := 1;
  while i<=ParamCount do begin
    s := Trim(AnsiLowerCase(ParamStr(i)));
    if s='' then continue;

    if (s[1]='-') or (s[1]='/') then begin
      if not HandleSwitch(s, i) then
        BadUsage(s+' is not a valid switch.');
    end else
      if not HandleParam(s, i) then
        BadUsage('I do not understand what do you mean by '+s+'.');

    Inc(i);
  end;
end;

{ Override to handle switches. If your switch takes any params, you have to
 increment i accordingly. E.g. 3 params => i := i+3; at the end. }
function TCommandLineApp.HandleSwitch(const s: string; var i: integer): boolean;
begin
  Result := false;
end;

{ Override to handle params that are not switches, and not trailing params for
 switches. }
function TCommandLineApp.HandleParam(const s: string; var i: integer): boolean;
begin
  Result := false;
end;

procedure TCommandLineApp.Run;
begin
end;

{ Shows the usage information. Override if your case is more complicated }
procedure TCommandLineApp.ShowUsage;
begin
  writeln(ErrOutput, 'Usage: '+ProgramName); //and that's it.
end;

{ Shows the error message + the usage information. Usually you don't need to
 override this one. }
procedure TCommandLineApp.ShowUsage(const AMessage: string);
begin
  if AMessage<>'' then
    writeln(ErrOutput, AMessage);
  ShowUsage;
end;

procedure DumpException(E: Exception);
begin
  while E<>nil do begin
    Writeln(ErrOutput, E.ClassName, ': ', E.Message);
    if E.StackTrace<>'' then
      Writeln(ErrOutput, E.StackTrace);
    E := E.InnerException;
    if E<>nil then
      Writeln(ErrOutput, ''); //empty line
  end;
end;

procedure RunApp(const AAppClass: CCommandLineApp);
begin
  Application := AAppClass.Create;
  try
    Application.Init;
    Application.ParseCommandLine;
    Application.Run;
  except
    on E: EBadUsage do
      Application.ShowUsage(E.Message);
    on E: Exception do
      DumpException(E);
  end;
end;

end.
