unit uPascal_DWS;
{
  UnderScript Delphi Web Script Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, Vcl.Forms,
{$ELSE}
  Classes, SysUtils, Forms,
{$ENDIF}
  Lua, pLua, LuaObject, CatStrings, UndHelper_Obj,
  Dws2Comp, dws2Exprs, dws2Compiler, dws2htmlfilter, UndImporter, UndConst;

type
  TUndDWS = class
    State: Plua_State;
    dws2Unit1: Tdws2Unit;
    dws1: tdelphiwebscriptII;
    dwsfilter1: tdws2htmlfilter;
    dwshtmlunit1: tdws2htmlunit;
    FPrg: TProgram;
    constructor Create(L: Plua_State);
    destructor Destroy; override;
    procedure Write(Info: TProgramInfo);
    procedure WriteLn(Info: TProgramInfo);
    procedure GetLocal(Info: TProgramInfo);
    procedure GetGlobal(Info: TProgramInfo);
    procedure SetLocal(Info: TProgramInfo);
    procedure SetGlobal(Info: TProgramInfo);
  end;

function PascalScript_Run(L: Plua_State): integer; cdecl;
function PascalWebScript_Run(L: Plua_State): integer; cdecl;
function DWSScript_Run(L: Plua_State; isfilter: Boolean): integer; cdecl;

implementation

function DWSScript_Run(L: Plua_State; isfilter: Boolean): integer; cdecl;
var
  r: TUndScriptResult;
  obj: TUndDWS;
  script, result_str: string;
  importer: TUndImporter;
  i: integer;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK = false then
    Exit;
  r.success := true;
  obj := TUndDWS.Create(L);
  importer := TUndImporter.Create(L);
  importer.EnableDebug := false;
  importer.FuncReadFormat := 'var %k:%t;%k := ' + rudLibName + '.GetL(''%k'');';
  importer.FuncWriteFormat := crlf + rudLibName + '.SetL(''%k'',%k);';
  if isfilter then
  begin // TODO: check if import is working in filter mode
    obj.dws1.Config.Filter := obj.dwsfilter1;
    importer.FuncReadFormat := '<% ' + importer.FuncReadFormat + ' %>';
    importer.FuncWriteFormat := '<%' + importer.FuncWriteFormat + '%>';
  end;
  script := pchar(lua_tostring(L, 1));
  try
    script := importer.GetScript(L, script);
  except
  end; // eats any exception

  try
    obj.FPrg.Free;
  finally
    obj.FPrg := nil;
  end;
  obj.dws1.Config.CompilerOptions := [];
  obj.FPrg := obj.dws1.Compile(script);
  for i := 0 to obj.FPrg.Msgs.Count - 1 do
  begin
    r.success := false;
    r.errormessage := obj.FPrg.Msgs.ToString;
    if rudCustomFunc_LogError <> emptystr then
      Und_LogError(L, -1, obj.FPrg.Msgs[i].AsInfo)
    else
      UndHelper.writeln(obj.FPrg.Msgs[i].AsInfo);
  end;
  obj.FPrg.Debugger := nil;
  obj.FPrg.execute;
  if isfilter then
  begin
    if obj.FPrg.Result is Tdws2DefaultResult then
      result_str := Tdws2DefaultResult(obj.FPrg.Result).Text;
    UndHelper.writeln(result_str);
  end;

  obj.Free;
  importer.Free;
  Und_PushScriptResult(L, r);
  Result := 1;
end;

function PascalScript_Run(L: Plua_State): integer; cdecl;
// Main function for execution
begin
  DWSScript_Run(L, false);
  Result := 1;
end;

function PascalWebScript_Run(L: Plua_State): integer; cdecl;
// Main function for execution
begin
  DWSScript_Run(L, true);
  Result := 1;
end;

procedure TUndDWS.writeln(Info: TProgramInfo);
begin
  UndHelper.writeln(Info['s']);
end;

procedure TUndDWS.Write(Info: TProgramInfo);
begin
  UndHelper.Write(Info['s']);
end;

procedure TUndDWS.GetLocal(Info: TProgramInfo);
begin
  Info.Result := UndHelper.GetL(Info['s']);
end;

procedure TUndDWS.GetGlobal(Info: TProgramInfo);
begin
  Info.Result := UndHelper.GetG(Info['s']);
end;

procedure TUndDWS.SetLocal(Info: TProgramInfo);
var
  v: Variant;
  s: string;
begin
  try
    v := Info.value['v'];
  except
  end; // eats any variant reading error
  // s:=v; system.writeln('setting '+info['s']+' to: '+s);
  UndHelper.SetL(Info['s'], v);
end;

procedure TUndDWS.SetGlobal(Info: TProgramInfo);
var
  v: Variant;
  s: string;
begin
  try
    v := Info.value['v'];
  except
  end; // eats any variant reading error
  UndHelper.SetG(Info['s'], v);
end;

constructor TUndDWS.Create(L: Plua_State);
  procedure Add_CustomFunctions;
  var
    func_writeln: Tdws2Function;
  var
    func_write: Tdws2Function;
  var
    func_getl: Tdws2Function;
  var
    func_getg: Tdws2Function;
  var
    func_setl: Tdws2Function;
  var
    func_setg: Tdws2Function;
  begin
    // Write
    func_write := Tdws2Function.Create(dws2Unit1.Functions);
    func_write.Name := 'Write';
    func_write.ResultType := emptystr;
    func_write.OnEval := Write;
    with tdws2parameter(func_write.Parameters.Add) do
    begin
      name := 's';
      datatype := 'string';
    end;
    // WriteLn
    func_writeln := Tdws2Function.Create(dws2Unit1.Functions);
    func_writeln.Name := 'WriteLn';
    func_writeln.ResultType := emptystr;
    func_writeln.OnEval := WriteLn;
    with tdws2parameter(func_writeln.Parameters.Add) do
    begin
      name := 's';
      datatype := 'string';
    end;
    // GetLocal
    func_getl := Tdws2Function.Create(dws2Unit1.Functions);
    func_getl.Name := 'GetL';
    func_getl.ResultType := 'variant';
    func_getl.OnEval := GetLocal;
    with tdws2parameter(func_getl.Parameters.Add) do
    begin
      name := 's';
      datatype := 'string';
    end;
    // GetGlobal
    func_getg := Tdws2Function.Create(dws2Unit1.Functions);
    func_getg.Name := 'GetG';
    func_getg.ResultType := 'variant';
    func_getg.OnEval := GetGlobal;
    with tdws2parameter(func_getg.Parameters.Add) do
    begin
      name := 's';
      datatype := 'string';
    end;
    // SetLocal
    func_setl := Tdws2Function.Create(dws2Unit1.Functions);
    func_setl.Name := 'SetL';
    func_setl.ResultType := emptystr;
    func_setl.OnEval := SetLocal;
    with tdws2parameter(func_setl.Parameters.Add) do
    begin
      name := 's';
      datatype := 'string';
    end;
    with tdws2parameter(func_setl.Parameters.Add) do
    begin
      name := 'v';
      datatype := 'variant';
    end;
    // SetGlobal
    func_setg := Tdws2Function.Create(dws2Unit1.Functions);
    func_setg.Name := 'SetG';
    func_setg.ResultType := emptystr;
    func_setg.OnEval := SetGlobal;
    with tdws2parameter(func_setg.Parameters.Add) do
    begin
      name := 's';
      datatype := 'string';
    end;
    with tdws2parameter(func_setg.Parameters.Add) do
    begin
      name := 'v';
      datatype := 'variant';
    end;
  end;

begin
  dws1 := tdelphiwebscriptII.Create(nil);
  dwsfilter1 := tdws2htmlfilter.Create(nil);
  dwshtmlunit1 := tdws2htmlunit.Create(nil);
  // begin
  dws2Unit1 := Tdws2Unit.Create(nil);
  dws2Unit1.UnitName := rudLibName;
  dws2Unit1.script := dws1;
  Add_CustomFunctions;
  // end
  dwshtmlunit1.script := dws1;
  // dws1.Config.Filter:=dwsfilter1;
  UndHelper.LuaState := L;
  State := L;
end;

destructor TUndDWS.Destroy;
begin
  dws1.Free;
  dwsfilter1.Free;
  dwshtmlunit1.Free;
  dws2Unit1.Free;
  inherited Destroy;
end;

end.
