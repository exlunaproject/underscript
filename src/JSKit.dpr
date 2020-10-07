library JSKit;

{
 UnderScript JavaScriptCore Lua library
 Copyright (c) 2013-2020 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)

 This library extends Underscript to run JavaScript using the JavaScriptCore
 engine
}

{$I Underscript.inc}
{$I CatCompactLib.inc}

uses
  Lua, pLua, pLuaTable, UndConst,
  uJavaScript_JSC,
  JSK.Base in 'thirdparty\js_javascriptcore\JSK.Base.pas',
  JSK.API in 'thirdparty\js_javascriptcore\JSK.API.pas',
  CatStrings;


{$R *.res}

function lua_javascriptcore_run(L: Plua_State): integer; cdecl;
begin
  if plua_validateargs(L, Result, [LUA_TSTRING]).OK then begin
    ReadScriptSettings(L);
    result := JavaScriptJSC_Run(L);
  end;
end;

function luaopen_Underscript_JSKit(L: plua_State): integer; cdecl;
begin
  result := RegisterScriptEngine(L, 'js', 'core', lua_javascriptcore_run);
end;

Exports
  luaopen_Underscript_JSKit;

begin

end.
