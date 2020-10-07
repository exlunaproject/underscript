library SpiderMonkey;

{
 UnderScript SpiderMonkey Lua library
 Copyright (c) 2013-2020 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)

 This library extends Underscript to run JavaScript using the SpiderMonkey
 engine
}

{$I Underscript.inc}
{$I CatCompactLib.inc}

uses
  Lua, pLua, pLuaTable, UndConst,
  uJavaScript_SM,
  js15decl in 'thirdparty\js_spidermonkey\js15decl.pas',
  jsDbgServer in 'thirdparty\js_spidermonkey\jsDbgServer.pas',
  jsintf in 'thirdparty\js_spidermonkey\jsintf.pas',
  NamedPipesImpl in 'thirdparty\js_spidermonkey\NamedPipesImpl.pas',
  CatStrings;


{$R *.res}

function lua_javascriptmonkey_run(L: Plua_State): integer; cdecl;
begin
  if plua_validateargs(L, Result, [LUA_TSTRING]).OK then begin
    ReadScriptSettings(L);
    result := JavaScriptSM_Run(L);
  end;
end;

function luaopen_Underscript_SpiderMonkey(L: plua_State): integer; cdecl;
begin
  result := RegisterScriptEngine(L, 'js', 'spider', lua_javascriptmonkey_run);
end;

Exports
  luaopen_Underscript_SpiderMonkey;

begin

end.
