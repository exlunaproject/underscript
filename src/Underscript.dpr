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

{$DEFINE UNDER_ACTIVESCRIPT}
{$DEFINE UNDER_PASCAL}
{$DEFINE UNDER_PASCAL_CLASSIC}
{$DEFINE UNDER_PHP}
{$DEFINE UNDER_PYTHON_ENV}
{$DEFINE UNDER_RUBY}  

uses
  SysUtils, TypInfo, Lua, pLua, pLuaTable, CatCSCommand, UndConst, UndScriptExt,
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
  CatTasks;

// Reduces exe size
{$IFDEF RELEASE}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
// Reduces exe size end

{$R *.res}

function lua_run_luacode(L: plua_State):integer; cdecl;
var
  r: TUndScriptResult;
  script:string;
  error: integer;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then begin
   r.success := true;
   script := lua_tostring(L,1);
   luaL_loadbuffer(L, pAnsiChar(ansistring(script)), Length(ansistring(script)), pAnsiChar(''));
   error := lua_pcall(L, 0, 0, 0);
   if(error <> 0) then begin
    r.success := false;
    r.errormessage := lua_tostring(L, -1);
    lua_pop(L, 1);
   end;
   Und_PushScriptResult(L, r);
  end;
end;

function lua_getscriptfunc(L: plua_State):integer; cdecl;
var s:string;
begin
 result:=1;
 s:=lua_tostring(L,2);
 case TScriptType(GetEnumValue(TypeInfo(TScriptType), 'lang_'+lowercase(s))) of
  lang_lua: lua_pushcfunction(L,lua_run_luav51);
  lang_luain: lua_pushcfunction(L,lua_run_luacode);
  lang_luajit: lua_pushcfunction(L,lua_run_luajit);
  lang_luav51: lua_pushcfunction(L,lua_run_luav51);
  lang_luav52: lua_pushcfunction(L,lua_run_luav52);
  lang_luav53: lua_pushcfunction(L,lua_run_luav53);
  lang_luav54: lua_pushcfunction(L,lua_run_luav54);
  {$IFDEF UNDER_ACTIVESCRIPT}
  lang_jscript: lua_pushcfunction(L,JavaScript_Run);
  lang_vbscript: lua_pushcfunction(L,VBScript_Run);
  lang_perlactive: lua_pushcfunction(L,PerlScript_Run);
  {$ENDIF}
  {$IFDEF UNDER_PASCAL}
  lang_pascalpage: lua_pushcfunction(L,PascalWebScript_Run);
  lang_pascalscript: lua_pushcfunction(L,PascalScript_Run);
  {$ENDIF}
  {$IFDEF UNDER_PASCAL_CLASSIC}
  lang_pascalprog: lua_pushcfunction(L,PascalClassic_Run);
  {$ENDIF}
  {$IFDEF UNDER_PYTHON_ENV}
  // This will execute the script using a installed Python in your environment
  lang_pythonenv: lua_pushcfunction(L,Python_Run);
  {$ENDIF}
  lang_jsnode: lua_pushcfunction(L, lua_run_nodejs);
  lang_jsnodestrict: lua_pushcfunction(L, lua_run_nodejs_strict);
  lang_jsv8: lua_pushcfunction(L, lua_run_jsv8);
  // This will execute the script using an embedded Python (if any)
  lang_python: lua_pushcfunction(L, lua_run_python);
  lang_perl: lua_pushcfunction(L, lua_run_perl);
  lang_php: lua_pushcfunction(L, lua_run_php);
  lang_ruby: lua_pushcfunction(L,lua_run_ruby);
  lang_tcl: lua_pushcfunction(L,lua_run_tcl);
 else
  result:=0;
 end;
end;

type
 TOptionType = (
  opt_modulename,
  opt_usevars,
  opt_useglobals,
  opt_uselocals,
  opt_redirectio
 );
function lua_getoption(L: plua_State):integer; cdecl;
var s:string;
begin
 result:=1;
 s:=lua_tostring(L,2);
 case TOptionType(GetEnumValue(TypeInfo(TOptionType), 'opt_'+lowercase(s))) of
  opt_modulename: lua_pushstring(L,rudLibName);
  opt_usevars: lua_pushboolean(L,RudImportVariables);
  opt_uselocals: lua_pushboolean(L,RudImportLocals);
  opt_useglobals: lua_pushboolean(L,RudImportGlobals);
  opt_redirectio: begin
   if rudCustomFunc_WriteLn<>emptystr then
   lua_pushboolean(L,true) else lua_pushboolean(L,false);
  end;
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
  opt_modulename: SetCustomModuleName(lua_tostring(L,3));
  opt_usevars: RudImportVariables := lua_toboolean(L,3);
  opt_uselocals: RudImportLocals := lua_toboolean(L,3);
  opt_useglobals: RudImportGlobals := lua_toboolean(L,3);
  opt_redirectio: RedirectIO(lua_toboolean(L,3));
 else
  result:=0;
 end;
end;

function luaopen_Underscript_Runner(L: plua_State):integer; cdecl;
begin
 lua_newtable(L);
 plua_SetFieldValue(L,'run',@lua_getscriptfunc,nil);
 plua_SetFieldValue(L,'options',@lua_getoption,@lua_setoption);
 Result := 1;
end;

function luaopen_Underscript(L: plua_State): integer; cdecl;
begin
  plua_RegisterLuaTable(L, '_script', @lua_getscriptfunc, nil);
  Result := 0;
end;

Exports
  luaopen_Underscript,
  luaopen_Underscript_Runner;

begin

end.
