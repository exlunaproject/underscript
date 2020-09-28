unit uJavaScript_QJS;
{
  UnderScript QuickJS Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, Windows, lua, plua, LuaObject, UndImporter, UndConst, CatStrings,
  quickjs, UndHelper_QJS, UndHelper_Obj, UndConsole, CatTime;

function JavaScriptQuick_Run(L: Plua_State): integer; cdecl;

implementation

function eval_buf(ctx : JSContext; Buf : PAnsiChar; buf_len : Integer; filename : PAnsiChar; eval_flags : Integer): Integer;
var
  val : JSValue;
  exp: JSValue;
begin
  val := JS_Eval(ctx, buf, buf_len, filename, eval_flags);
  if JS_IsException(val) then
  begin
    if rudRedirectIO = false then begin
      js_std_dump_error(ctx);
    end else begin
      exp := JS_GetException(ctx);
      if JS_IsError(ctx,exp) then
        uConsoleErrorLn(Undhelper.LuaState, -1,JS_ToCString(ctx, exp));
      JS_FreeValue(ctx, exp);
    end;
    Result := -1;
  end
  else
    Result := 0;
    JS_FreeValue(ctx, val);
end;

function eval_file(ctx : JSContext; filename : PAnsiChar; eval_flags : Integer): Integer;
var
  buf_len : size_t;
  Buf : Pointer;
begin
  buf := js_load_file(ctx, @buf_len, filename);
  if not Assigned(buf) then
  begin
    Writeln('Error While Loading : ',filename);
    exit(1);
  end;
  Result := eval_buf(ctx, buf, buf_len, filename, eval_flags);
  js_free(ctx, buf);
end;

function eval_string(ctx : JSContext; script : PAnsiChar; eval_flags : Integer): Integer;
begin
  Result := eval_buf(ctx, script, length(script), '_script.js', eval_flags);
end;

procedure RunCode(script:string);
var
  rt  : JSRuntime;
  ctx : JSContext;
  m   : JSModuleDef;
  global : JSValue;
  filename : PAnsiChar;
const
  std_hepler : PAnsiChar =
    'import * as std from ''std'';'#10+
    'import * as os from ''os'';'#10+
    'import * as Cmu from ''Cmu'';'#10+ // Underscript's Custom Module
    'globalThis.std = std;'#10+
    'globalThis.os = os;'#10+
    'globalThis.Cmu = Cmu;'#10+
    'globalThis.UConsole = new Cmu.GetConsole();';
begin
  rt := JS_NewRuntime;
  if Assigned(rt) then
  begin
    ctx := JS_NewContext(rt);
    if Assigned(rt) then
    begin
      // ES6 Module loader.
      JS_SetModuleLoaderFunc(rt, nil, @js_module_loader, nil);

      js_std_add_helpers(ctx,0,nil);
      js_init_module_std(ctx, 'std');
      js_init_module_os(ctx, 'os');

      {
        Functions init order is important
        cuz i init the class and it's obj's and constructor in
        RegisterNativeClass then i just point the Module constructor to the same one.
      }

      // Register with global object directly .
      RegisterNativeClass(ctx);

      // Register with module
      m := JS_NewCModule(ctx, PAnsiChar('Cmu'), @Emu_init);
      JS_AddModuleExport(ctx,m, PAnsiChar('GetConsole'));

      eval_buf(ctx, std_hepler, {$IFDEF FPC}strlen{$ELSE}lstrlenA{$ENDIF}(std_hepler), '<global_helper>', JS_EVAL_TYPE_MODULE);

      global := JS_GetGlobalObject(ctx);
      JS_FreeValue(ctx, global);

      //filename :=PAnsiChar(AnsiString(ParamStr(1)));
      //eval_file(ctx,filename,JS_EVAL_TYPE_GLOBAL or JS_EVAL_TYPE_MODULE);
      eval_string(ctx,PAnsiChar(AnsiString(script)),JS_EVAL_TYPE_GLOBAL or JS_EVAL_TYPE_MODULE);

      js_std_loop(ctx);

      js_std_free_handlers(rt);
      JS_FreeContext(ctx);
    end;
    JS_FreeRuntime(rt);
  end;
end;

function JavaScriptQuick_Run(L: Plua_State): integer; cdecl;
var
  r: TUndScriptResult;
  script: string;
  importer: TUndImporter;
  sw: TCatStopWatch;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK = false then
    Exit;
  r.success := true;
  undhelper.LuaState := L;
  sw := CatStopWatchNew;

  importer := TUndImporter.Create(L);
  importer.EnableDebug:=false;
  script := lua_tostring(L, 1);
  script := importer.GetScript(L, script, langint_JSQuick).completescript;
  try
    RunCode(script);
  except
    on E: Exception do begin
      r.success := false;
      r.errormessage := E.Message;
      uConsoleErrorLn(L, -1, 'JS: ' + E.Message);
    end;
  end;

  importer.free;
  Und_PushScriptResult(L, r, sw);
  result := 1;
end;

end.
