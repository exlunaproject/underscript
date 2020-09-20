unit UndScriptExt;

{
  UnderScript External Scripting
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, Lua, pLua, UndConst, UndImporterExt;

function lua_run_luajit(L: plua_State):integer; cdecl;
function lua_run_luav51(L: plua_State):integer; cdecl;
function lua_run_luav52(L: plua_State):integer; cdecl;
function lua_run_luav53(L: plua_State):integer; cdecl;
function lua_run_luav54(L: plua_State):integer; cdecl;
function lua_run_nodejs(L: plua_State):integer; cdecl;
function lua_run_nodejs_strict(L: plua_State):integer; cdecl;
function lua_run_jsv8(L: plua_State):integer; cdecl;
function lua_run_php(L: plua_State):integer; cdecl;
function lua_run_ruby(L: plua_State):integer; cdecl;
function lua_run_perl(L: plua_State):integer; cdecl;
function lua_run_python(L: plua_State):integer; cdecl;
function lua_run_tcl(L: plua_State):integer; cdecl;

implementation

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

function lua_run_tcl(L: plua_State):integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    RunExternalScript(L, lua_tostring(L,1), langdef_TCL);
end;

end.
