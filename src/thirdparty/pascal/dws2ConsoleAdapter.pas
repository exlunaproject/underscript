unit dws2ConsoleAdapter;

interface

uses
  Variants, Classes,
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Windows,
{$ENDIF}
  dws2Comp, dws2Exprs, dws2Errors, dws2Symbols, dws2Functions, dws2compiler;

type
  TConsoleReadLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TConsoleWriteFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TConsoleWriteLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  Tdws2ConsoleUnit = class(Tdws2UnitComponent)
  protected
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;
  end;


implementation

uses SysUtils, dws2Strings;

{ TConsoleReadLnFunction }

procedure TConsoleReadLnFunction.Execute;
var
  s: string;
  c: array [0..255] of Char;
  numRead, dummy: Cardinal;
begin
{$IFDEF LINUX}

{$ELSE}
   s := VarToStr(Info['s']);
   WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), PChar(s), Length(s), dummy, nil);

   ReadConsole(GetStdHandle(STD_INPUT_HANDLE), @c, 255, numRead, nil);
   s := #10;
   WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), PChar(s), 1, dummy, nil);

   SetString(s, PChar(@c[0]), numRead);
   Info.Result := s;
{$ENDIF}
end;

{ TWriteFunction }

procedure TConsoleWriteFunction.Execute;
var
  s: string;
  dummy: Cardinal;
begin
{$IFDEF LINUX}

{$ELSE}
  s := VarToStr(Info['s']);
  WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), PChar(s), Length(s), dummy, nil);
{$ENDIF}
end;

{ TWriteLnFunction }

procedure TConsoleWriteLnFunction.Execute;
var
  s: string;
  Dummy: Cardinal;
begin
{$IFDEF LINUX}

{$ELSE}
  s := VarToStr(Info['s']) + #13#10;
  WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE), PChar(s), Length(s), dummy, nil);
{$ENDIF}
end;

{ Tdws2ConsoleType }

constructor Tdws2ConsoleUnit.Create(AOwner: TComponent);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    {$IFNDEF CONSOLE }
      Win32Check(AllocConsole);
    {$ENDIF}

    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT);
  end;
end;

destructor Tdws2ConsoleUnit.Destroy;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    FreeConsole;
end;

procedure Tdws2ConsoleUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  TConsoleReadLnFunction.Create(SymbolTable, 'ReadLn', ['s', SYS_VARIANT], SYS_STRING);
  TConsoleWriteFunction.Create(SymbolTable, 'Write', ['s', SYS_VARIANT], '');
  TConsoleWriteLnFunction.Create(SymbolTable, 'WriteLn', ['s', SYS_VARIANT], '');
end;

end.
