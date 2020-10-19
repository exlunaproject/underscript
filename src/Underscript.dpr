library Underscript;

{
 UnderScript Script Runner Lua library
 Copyright (c) 2013-2020 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)

 This library adds to Lua the ability to run code written in the following
 programming languages:

 JavaScript, PascalScript, Perl, PHP, Python, Ruby, TCL, VBScript, LuaJIT and
 various versions of Lua itself (5.1 to 5.4)
}

{$I Underscript.inc}
{$I CatCompactLib.inc}
{$DEFINE UNDER_ACTIVESCRIPT}
{$DEFINE UNDER_PASCAL}
{$DEFINE UNDER_PASCAL_CLASSIC}
{$DEFINE UNDER_JAVASCRIPT_QUICKJS}
{$DEFINE UNDER_PHP}
{$DEFINE UNDER_PYTHON_ENV}
{$DEFINE UNDER_RUBY}
{$DEFINE UNDER_LEGACY}

uses
  SysUtils, TypInfo, Lua, pLua, pLuaTable, CatCSCommand, UndConst, UndScriptExt,
  UndConsole,
  {$IFDEF UNDER_ACTIVESCRIPT}
  uActiveScript,
  {$ENDIF}
  {$IFDEF UNDER_PASCAL}
  uPascal_DWS,
  dws2Comp in 'thirdparty\pascal\dws2Comp.pas',
  dws2Compiler in 'thirdparty\pascal\dws2Compiler.pas',
  dws2Exprs in 'thirdparty\pascal\dws2Exprs.pas',
  dws2Symbols in 'thirdparty\pascal\dws2Symbols.pas',
  dws2Strings in 'thirdparty\pascal\dws2Strings.pas',
  dws2Stack in 'thirdparty\pascal\dws2Stack.pas',
  dws2Errors in 'thirdparty\pascal\dws2Errors.pas',
  dws2Functions in 'thirdparty\pascal\dws2Functions.pas',
  dws2Tokenizer in 'thirdparty\pascal\dws2Tokenizer.pas',
  dws2Debugger in 'thirdparty\pascal\dws2Debugger.pas',
  dws2MathFunctions in 'thirdparty\pascal\dws2MathFunctions.pas',
  dws2StringFunctions in 'thirdparty\pascal\dws2StringFunctions.pas',
  dws2TimeFunctions in 'thirdparty\pascal\dws2TimeFunctions.pas',
  dws2VariantFunctions in 'thirdparty\pascal\dws2VariantFunctions.pas',
  dws2CompStrings in 'thirdparty\pascal\dws2CompStrings.pas',
  dws2HtmlFilter in 'thirdparty\pascal\dws2HtmlFilter.pas',
  dws2StringResult in 'thirdparty\pascal\dws2StringResult.pas',
  {$ENDIF}
  {$IFDEF UNDER_PASCAL_CLASSIC}
  uPascal,
  uPSCompiler in 'thirdparty\pascal_rem\uPSCompiler.pas',
  uPSUtils in 'thirdparty\pascal_rem\uPSUtils.pas',
  uPSRuntime in 'thirdparty\pascal_rem\uPSRuntime.pas',
  uPSComponent in 'thirdparty\pascal_rem\uPSComponent.pas',
  uPSDebugger in 'thirdparty\pascal_rem\uPSDebugger.pas',
  uPSC_dll in 'thirdparty\pascal_rem\uPSC_dll.pas',
  uPSC_std in 'thirdparty\pascal_rem\uPSC_std.pas',
  uPSC_stdctrls in 'thirdparty\pascal_rem\uPSC_stdctrls.pas',
  uPSC_forms in 'thirdparty\pascal_rem\uPSC_forms.pas',
  uPSC_graphics in 'thirdparty\pascal_rem\uPSC_graphics.pas',
  uPSC_controls in 'thirdparty\pascal_rem\uPSC_controls.pas',
  uPSC_classes in 'thirdparty\pascal_rem\uPSC_classes.pas',
  uPSC_comobj in 'thirdparty\pascal_rem\uPSC_comobj.pas',
  uPSC_dateutils in 'thirdparty\pascal_rem\uPSC_dateutils.pas',
  uPSC_DB in 'thirdparty\pascal_rem\uPSC_DB.pas',
  uPSC_extctrls in 'thirdparty\pascal_rem\uPSC_extctrls.pas',
  uPSC_menus in 'thirdparty\pascal_rem\uPSC_menus.pas',
  uPSR_dll in 'thirdparty\pascal_rem\uPSR_dll.pas',
  uPSPreProcessor in 'thirdparty\pascal_rem\uPSPreProcessor.pas',
  uPSR_std in 'thirdparty\pascal_rem\uPSR_std.pas',
  uPSR_stdctrls in 'thirdparty\pascal_rem\uPSR_stdctrls.pas',
  uPSR_forms in 'thirdparty\pascal_rem\uPSR_forms.pas',
  uPSR_graphics in 'thirdparty\pascal_rem\uPSR_graphics.pas',
  uPSR_controls in 'thirdparty\pascal_rem\uPSR_controls.pas',
  uPSR_classes in 'thirdparty\pascal_rem\uPSR_classes.pas',
  uPSR_comobj in 'thirdparty\pascal_rem\uPSR_comobj.pas',
  uPSR_dateutils in 'thirdparty\pascal_rem\uPSR_dateutils.pas',
  uPSR_DB in 'thirdparty\pascal_rem\uPSR_DB.pas',
  uPSR_extctrls in 'thirdparty\pascal_rem\uPSR_extctrls.pas',
  uPSR_menus in 'thirdparty\pascal_rem\uPSR_menus.pas',
  {$ENDIF}
  {$IFDEF UNDER_PYTHON_ENV}
  uPython,
  PythonEngine in 'thirdparty\python\PythonEngine.pas',
  MethodCallBack in 'thirdparty\python\MethodCallBack.pas',
  VarPyth in 'thirdparty\python\VarPyth.pas',
  {$ENDIF}
  {$IFDEF UNDER_JAVASCRIPT_QUICKJS}
  uJavaScript_QJS,
  quickjs in 'thirdparty\js_quickjs\quickjs.pas',
  {$ENDIF}
  CatLogger, CatStrings, CatCSUtils;


{$R *.res}

function lua_run_luacode(L: plua_State):integer; cdecl;
var
  r: TUndScriptResult;
  script:string;
  error: integer;
  sw: TCatStopWatch;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
   sw := CatStopWatchNew;
   r.success := true;
   script := lua_tostring(L,1);
   luaL_loadbuffer(L, pAnsiChar(ansistring(script)), Length(ansistring(script)), pAnsiChar(''));
   error := lua_pcall(L, 0, 0, 0);
   if(error <> 0) then begin
    r.success := false;
    r.errormessage := lua_tostring(L, -1);
    lua_pop(L, 1);
    uConsoleErrorLn(L, 0, r.errormessage);
   end;
   Und_PushScriptResult(L, r, sw);
  end;
end;

// Warns if JavaScriptCore library is not available
function JavaScriptJSC_Run(L: plua_State):integer; cdecl;
begin
  luaL_error(L, 'Underscript.JSKit library not found.');
  result := 1;
end;

// Warns if SpiderMonkey library is not available
function JavaScriptJSM_Run(L: plua_State):integer; cdecl;
begin
  luaL_error(L, 'Underscript.SpiderMonkey library not found.');
  result := 1;
end;

// Warns if V8 library is not available
function JavaScriptV8_Run(L: plua_State):integer; cdecl;
begin
  luaL_error(L, 'Underscript.v8 library not found.');
  result := 1;
end;

function lua_getjavascriptfunc(L: plua_State):integer; cdecl;
const
   table : array [1..9] of luaL_Reg =
   (
   (name:'jscript';func:JavaScript_Run),
   (name:'core';func:JavaScriptJSC_Run),
   (name:'node';func:lua_run_nodejs),
   (name:'nodestrict';func:lua_run_nodejs_strict),
   (name:'quick';func:JavaScriptQuick_Run),
   (name:'spider';func:JavaScriptJSM_Run),
   (name:'v8';func:JavaScriptV8_Run),
   (name:'v8ext';func:lua_run_jsv8),
   (name:nil;func:nil)
   );
begin
   result := plua_pushcfunction_fromarray(L, lua_tostring(L,2), table);
end;

function lua_getjavascriptppfunc(L: plua_State):integer; cdecl;
const
   table : array [1..2] of luaL_Reg =
   (
   (name:'tiscript';func:lua_run_tiscript),
   (name:nil;func:nil)
   );
begin
   result := plua_pushcfunction_fromarray(L, lua_tostring(L,2), table);
end;

function lua_getluascriptfunc(L: plua_State):integer; cdecl;
const
   table : array [1..9] of luaL_Reg =
   (
   (name:'script';func:lua_run_luav51),
   (name:'script32';func:lua_run32_luav51),
   (name:'in';func:lua_run_luacode),
   (name:'jit';func:lua_run_luajit),
   (name:'v51';func:lua_run_luav51),
   (name:'v52';func:lua_run_luav52),
   (name:'v53';func:lua_run_luav53),
   (name:'v54';func:lua_run_luav54),
   (name:nil;func:nil)
   );
begin
  result := plua_pushcfunction_fromarray(L, lua_tostring(L,2), table);
end;

function lua_getjavafunc(L: plua_State):integer; cdecl;
const
   table : array [1..3] of luaL_Reg =
   (
   (name:'bsh';func:lua_run_java),
   (name:'bshcore';func:lua_run_javabshcore),
   (name:nil;func:nil)
   );
begin
  result := plua_pushcfunction_fromarray(L, lua_tostring(L,2), table);
end;

function lua_getpascalscriptfunc(L: plua_State):integer; cdecl;
const
   table : array [1..6] of luaL_Reg =
   (
   (name:'script';func:PascalScript_Run),
   (name:'webscript';func:PascalWebScript_Run),
   (name:'prog';func:PascalClassic_Run),
   (name:'func';func:PascalFunction_Run),
   (name:'short';func:PascalFunctionShort_Run),
   (name:nil;func:nil)
   );
begin
  result := plua_pushcfunction_fromarray(L, lua_tostring(L,2), table);
end;

// Experimental interpreters
function lua_getalphascriptfunc(L: plua_State):integer; cdecl;
const
   table : array [1..2] of luaL_Reg =
   (
 // This will execute the script using a installed Python in your environment
   (name:'pythonenv';func:Python_Run),
   (name:nil;func:nil)
   );
begin
  result := plua_pushcfunction_fromarray(L, lua_tostring(L,2), table);
end;

function lua_getperlscriptfunc(L: plua_State):integer; cdecl;
const
   table : array [1..3] of luaL_Reg =
   (
   (name:'script';func:lua_run_perl),
   (name:'active';func:PerlScript_Run),
   (name:nil;func:nil)
   );
begin
  result := plua_pushcfunction_fromarray(L, lua_tostring(L,2), table);
end;

function lua_getscriptfuncbyfileext(L: plua_State):integer; cdecl;
var ext:string;
const
   table : array [1..14] of luaL_Reg =
   (
   (name:'lua';func:lua_run_luav51),
   (name:'java';func:lua_run_java),
   (name:'js';func:JavaScript_Run),
   (name:'vbs';func:VBScript_Run),
   (name:'dws';func:PascalWebScript_Run),
   (name:'dpr';func:PascalClassic_Run),
   (name:'pas';func:PascalClassic_Run),
   (name:'php';func:lua_run_php),
   (name:'py';func:lua_run_python),
   (name:'pl';func:lua_run_perl),
   (name:'rb';func:lua_run_ruby),
   (name:'tis';func:lua_run_tiscript),
   (name:'tcl';func:lua_run_tcl),
   (name:nil;func:nil)
   );
begin
 ext:=lua_tostring(L,2);
 if beginswith(ext, '.') then
   ext := after(ext, '.');
 result := plua_pushcfunction_fromarray(L, ext, table);
end;

type
 TOptionType = (
  opt_handleerrors,
  opt_modulename,
  opt_usevars,
  opt_useglobals,
  opt_uselocals,
  opt_useoldactive,
  opt_redirectio
 );
function lua_getoption(L: plua_State):integer; cdecl;
var s:string;
begin
 result:=1;
 s:=lua_tostring(L,2);
 case TOptionType(GetEnumValue(TypeInfo(TOptionType), 'opt_'+lowercase(s))) of
  opt_handleerrors: lua_pushboolean(L,RudHandleErrors);
  opt_modulename: lua_pushstring(L,rudLibName);
  opt_usevars: lua_pushboolean(L,RudImportVariables);
  opt_uselocals: lua_pushboolean(L,RudImportLocals);
  opt_useglobals: lua_pushboolean(L,RudImportGlobals);
  opt_useoldactive: lua_pushboolean(L,USEOLDASPARSER);
  opt_redirectio: lua_pushboolean(L,RudRedirectIO);
 else
  result:=0;
 end;
end;
function lua_setoption(L: plua_State):integer; cdecl;
var s:string;
begin
 result:=1;
 s:=lua_tostring(L,2);
 case TOptionType(GetEnumValue(TypeInfo(TOptionType), 'opt_'+lowercase(s))) of
  opt_handleerrors: RudHandleErrors := lua_toboolean(L,3);
  opt_modulename: SetCustomModuleName(lua_tostring(L,3));
  opt_usevars: RudImportVariables := lua_toboolean(L,3);
  opt_uselocals: RudImportLocals := lua_toboolean(L,3);
  opt_useglobals: RudImportGlobals := lua_toboolean(L,3);
  opt_redirectio: RudRedirectIO := lua_toboolean(L,3);
 else
  result:=0;
 end;
end;

procedure RegisterLanguages(L: plua_State; const Tag:integer);
begin
  plua_SetFieldValueRW(L, 'alpha', @lua_getalphascriptfunc, nil, tag);
  plua_SetFieldValueRW(L, 'lua', @lua_getluascriptfunc, nil, tag);
  plua_SetFieldValueCF(L, 'pascal', @PascalScript_Run, tag);
  plua_SetFieldValueRW(L, 'pas', @lua_getpascalscriptfunc, nil, tag);
  plua_SetFieldValueCF(L, 'perl', @lua_run_perl, tag);
  plua_SetFieldValueCF(L, 'perlx', @lua_getperlscriptfunc, tag);
  plua_SetFieldValueCF(L, 'python', @lua_run_python, tag);
  plua_SetFieldValueCF(L, 'php', @lua_run_php, tag);
  plua_SetFieldValueCF(L, 'java', @lua_run_java, tag);
  plua_SetFieldValueRW(L, 'javax', @lua_getjavafunc, nil, tag);
  plua_SetFieldValueCF(L, 'javascript', @JavaScriptQuick_Run, tag);
  plua_SetFieldValueCF(L, 'jscript', @JavaScript_Run, tag);
  plua_SetFieldValueRW(L, 'js', @lua_getjavascriptfunc, nil, tag);
  plua_SetFieldValueRW(L, 'jspp', @lua_getjavascriptppfunc, nil, tag);
  plua_SetFieldValueCF(L, 'ruby', @lua_run_ruby, tag);
  plua_SetFieldValueCF(L, 'tcl', @lua_run_tcl, tag);
  plua_SetFieldValueCF(L, 'vbscript', @VBScript_Run, tag);
  plua_SetFieldValueRW(L, 'options',@lua_getoption,@lua_setoption, tag);
end;

procedure RegisterLanguageExtension(L: plua_State;const name:string);
begin
  if fileexists(extractfilepath(paramstr(0))+'\Lib\clibs\Underscript\'+name+'.dll') then begin
   lua_pushstring(L, 'require');
   lua_rawget(L, LUA_GLOBALSINDEX);
   lua_pushstring(L, 'Underscript.'+name);
   lua_pcall(L, 1, 0, 0);
  end;
end;

function luaopen_Underscript_Runner(L: plua_State):integer; cdecl;
begin
 lua_newtable(L);
 plua_SetFieldValueRW(L,'runext',@lua_getscriptfuncbyfileext,nil);
 plua_SetFieldValueRW(L,'options',@lua_getoption,@lua_setoption);
 Result := 1;
end;

function luaopen_Underscript(L: plua_State): integer; cdecl;
const
 script_table : array [1..1] of luaL_reg =
 (
 (name:nil;func:nil)
 );
begin
  lual_register(L,'requirex',@script_table);
  lual_register(L,'_script',@script_table);
  RegisterLanguages(L, cUndTag_Normal);
  lual_register(L,'_scriptq',@script_table);
  RegisterLanguages(L, cUndTag_Quiet);
  // Loads JavaScript extensions
  RegisterLanguageExtension(L, 'JSKit');
  RegisterLanguageExtension(L, 'SpiderMonkey');
  RegisterLanguageExtension(L, 'v8');
  Result := 0;
end;

// same as luaopen_Underscript but with IO redirect disabled
function luaopen_Underscript_Console(L: plua_State): integer; cdecl;
begin
  rudRedirectIO := false;
  luaopen_Underscript(L);
end;

Exports
  luaopen_Underscript,
  luaopen_Underscript_Console,
  luaopen_Underscript_Runner;

begin

end.
