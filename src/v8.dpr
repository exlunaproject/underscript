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
  SysUtils, Lua, pLua, pLuaTable, UndConst, UndConsole,
  uJavaScript_V8, CatHTTP,
  v8wrapper in 'thirdparty\js_v8\v8wrapper.pas',
  CatStrings, CatFiles;


{$R *.res}

function lua_javascriptv8_run(L: Plua_State): integer; cdecl;
begin
  if plua_validateargs(L, Result, [LUA_TSTRING]).OK then begin
    ReadScriptSettings(L);
    result := JavaScriptV8_Run(L);
  end;
end;

function lua_module_funccall(L: plua_State): integer; cdecl;
var
  vlibname, vfieldname, res, params, js: string;
  i, argcount, luatype: integer;
begin
  vlibname := lua_tostring(L, lua_upvalueindex(1));
  vfieldname := lua_tostring(L, lua_upvalueindex(2));
  vlibname := getvalidcompname(vlibname);
  vfieldname := getvalidcompname(vfieldname);
  argcount := lua_gettop(L);
  params := emptystr;
  for i := 1 to argcount do
  begin
    luatype := lua_type(L, i);
    case luatype of
      LUA_TSTRING:
        CatAppendStr(params,'"'+JSONStringEscape(lua_tostring(L, i))+'"');
      LUA_TNUMBER:
        CatAppendStr(params,lua_tostring(L, i));
      LUA_TBOOLEAN:
        CatAppendStr(params,lua_tostring(L, i));
    end;
  end;
  js := vlibname+'.'+vfieldname+'('+params+');';
  res := RunJSPersistent(L, js);
  lua_pushstring(L, res);
  {argcount := j.GetValue('resc', 0);
  for i := 1 to argcount do
  begin
    plua_pushvariant(L, j.Values['res' + inttostr(i - 1) + 'value']);
  end; }
  result := 1;
end;

function lua_module_reader(L: plua_State): integer; cdecl;
var
  vlibname, vfieldname, json: string;
begin
  vlibname := lua_tostring(L, lua_upvalueindex(1));
  json := lua_tostring(L, lua_upvalueindex(2));
  vfieldname := lua_tostring(L, 2);

  lua_pushstring(L, vlibname);
  lua_pushstring(L, vfieldname);
  lua_pushcclosure(L, lua_module_funccall, 2);
  result := 1;
end;

function lua_module_writer(L: plua_State): integer; cdecl;
begin
  result := 0;
end;

function lua_require_js(L: plua_State): integer; cdecl;
var
  tidx: integer;
  json, jsmodulename: string;
  module: TCrossModuleCheckResult;
begin
  result := 0;
  ReadScriptSettings(L);
  json := emptystr;
  jsmodulename := CleanFilename(lua_tostring(L, 1));
  module := uConsoleCrossModuleExists(L, jsmodulename, 'js');
  if module.Exists = false then
   exit;

  RunJSPersistent(L, module.Script);
  lua_newtable(L);
  tidx := lua_gettop(L);
  lua_pushstring(L, '__index');
  lua_pushstring(L, jsmodulename);
  lua_pushstring(L, json);
  lua_pushcclosure(L, lua_module_reader, 2);
  lua_rawset(L, tidx);

  lua_pushstring(L, '__newindex');
  lua_pushstring(L, jsmodulename);
  lua_pushstring(L, json);
  lua_pushcclosure(L, lua_module_writer, 2);
  lua_rawset(L, tidx);

  lua_pushvalue(L, -1);
  lua_setmetatable(L, -2);
  result := 1;
end;

function luaopen_Underscript_v8(L: plua_State): integer; cdecl;
begin
  RegisterCrossRequire(L, 'js', lua_require_js);
  result := RegisterScriptEngine(L, 'js', 'v8', lua_javascriptv8_run);
end;

Exports
  luaopen_Underscript_v8;

begin

end.
