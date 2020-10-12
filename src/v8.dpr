library v8;

{
 UnderScript V8 Lua library
 Copyright (c) 2013-2020 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)

 This library extends Underscript to run JavaScript using the V8 engine
}

{$I Underscript.inc}
{$I CatCompactLib.inc}

uses
  Lua, pLua, pLuaTable, UndConst,
  uJavaScript_V8,
  v8wrapper in 'thirdparty\js_v8\v8wrapper.pas',
  CatStrings;


{$R *.res}

function lua_javascriptv8_run(L: Plua_State): integer; cdecl;
begin
  if plua_validateargs(L, Result, [LUA_TSTRING]).OK then begin
    ReadScriptSettings(L);
    result := JavaScriptV8_Run(L);
  end;
end;

function luaopen_Underscript_v8(L: plua_State): integer; cdecl;
begin
  result := RegisterScriptEngine(L, 'js', 'v8', lua_javascriptv8_run);
end;

Exports
  luaopen_Underscript_v8;

begin

end.
