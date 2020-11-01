unit UndScriptExt;

{
  UnderScript External Scripting
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, Lua, pLua, UndConst, UndImporterExt, CatUtils;

function lua_run_luajit(L: plua_State):integer; cdecl;
function lua_run_luav51(L: plua_State):integer; cdecl;
function lua_run_luav52(L: plua_State):integer; cdecl;
function lua_run_luav53(L: plua_State):integer; cdecl;
function lua_run_luav54(L: plua_State):integer; cdecl;
function lua_run_nodejs(L: plua_State):integer; cdecl;
function lua_run_nodejs_strict(L: plua_State):integer; cdecl;
function lua_run_java(L: plua_State):integer; cdecl;
function lua_run_javabshcore(L: plua_State):integer; cdecl;
function lua_run_jsv8(L: plua_State):integer; cdecl;
function lua_run_jsspidermonkey(L: plua_State):integer; cdecl;
function lua_run_js_javascriptcore(L: plua_State):integer; cdecl;
function lua_run_jspp_onux(L: plua_State):integer; cdecl;
function lua_run_quickjs(L: plua_State):integer; cdecl;
function lua_run_php(L: plua_State):integer; cdecl;
function lua_run_ruby(L: plua_State):integer; cdecl;
function lua_run_perl(L: plua_State):integer; cdecl;
function lua_run_python(L: plua_State):integer; cdecl;
function lua_run_tiscript(L: plua_State):integer; cdecl;
function lua_run_tcl(L: plua_State):integer; cdecl;
function lua_run_typescript_deno(L: plua_State):integer; cdecl;
function lua_run_csharp(L: plua_State):integer; cdecl;

function lua_run32_luav51(L: plua_State):integer; cdecl;

implementation

// ************************************************************************** //
// 64-bit Interpreters
// ************************************************************************** //

function lua_run_luajit(L: plua_State):integer; cdecl;
var lang:TUndLanguageExternal;
begin
  lang := langdef_Lua;
  lang.Command:='%u\luajit\luajit.exe';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

function lua_run_luav51(L: plua_State):integer; cdecl;
var lang:TUndLanguageExternal;
begin
  lang := langdef_Lua;
  lang.Command:='%p\lua5.1.exe';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

function lua_run_luav52(L: plua_State):integer; cdecl;
var lang:TUndLanguageExternal;
begin
  lang := langdef_Lua;
  lang.Command:='%u\lua52\lua.exe';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

function lua_run_luav53(L: plua_State):integer; cdecl;
var lang:TUndLanguageExternal;
begin
  lang := langdef_Lua;
  lang.Command:='%u\lua53\lua.exe';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

function lua_run_luav54(L: plua_State):integer; cdecl;
var lang:TUndLanguageExternal;
begin
  lang := langdef_Lua;
  lang.Command:='%u\lua54\lua.exe';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

function lua_run_nodejs(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_NodeJS);
end;

function lua_run_nodejs_strict(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_NodeJS_Strict);
end;

function lua_run_jsv8(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_V8JS);
end;

function lua_run_java(L: plua_State):integer; cdecl;
begin
 // if bsh is not available, try bsh-core
 if fileexists(extractfilepath(paramstr(0))+'\Extensions\underscript\beanshell\bsh.exe') then begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_Java);
 end else
 result := lua_run_javabshcore(L);
end;

function lua_run_javabshcore(L: plua_State):integer; cdecl;
var lang: TUndLanguageExternal;
begin
  lang := langdef_Java;
  lang.Command := '%u\beanshell\bsh-core.exe';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

function lua_run_jsspidermonkey(L: plua_State):integer; cdecl;
var lang:TUndLanguageExternal;
begin
  lang := langdef_V8JS;
  lang.Command := '%p\multipreter.exe';
  lang.Params := 'spidermonkey %f';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

function lua_run_js_javascriptcore(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_JavaScriptCore);
end;

function lua_run_quickjs(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_QuickJS);
end;

function lua_run_php(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_PHP);
end;

function lua_run_ruby(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_Ruby);
end;

function lua_run_perl(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
   RunExternalScript(L, lua_tostring(L,1), langdef_Perl);
end;

function lua_run_python(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_Python);
end;

function lua_run_tiscript(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_TIScript);
end;

function lua_run_tcl(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_TCL);
end;

function lua_run_typescript_deno(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_TypeScript_Deno);
end;

function lua_run_jspp_onux(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_JSPP_Onux);
end;

// ************************************************************************** //
// Legacy 32-bit Interpreters
// ************************************************************************** //

function lua_run_csharp(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_CSharp);
end;

function lua_run32_luav51(L: plua_State):integer; cdecl;
var lang:TUndLanguageExternal;
begin
  lang := langdef_Lua;
  lang.Command:='%u32\lua51\lua5.1.exe';
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), lang);
end;

end.
