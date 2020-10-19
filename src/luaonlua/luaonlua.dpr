library luaonlua;

{
  LoL64 (Lua 32-bit on Lua 64-bit and vice-versa) C Library
  Copyright (c) 2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

{$I CatCompactLib.inc}

uses
  Windows, Lua, SysUtils, pLua, pLuaTable, CatTasks, CatUtils,
  CatJSON, CatFiles, CatCryptSyno, uLoL64;

{$R *.res}

function lua_module_funccall(L: plua_State): integer; cdecl;
var
  vlibname, vfieldname, res, valuekey: string;
  j: TCatJSON;
  i, argcount, luatype: integer;
begin
  vlibname := lua_tostring(L, lua_upvalueindex(1));
  vfieldname := lua_tostring(L, lua_upvalueindex(2));
  argcount := lua_gettop(L);
  j := TCatJSON.Create;
  j.SetValue('lib', vlibname);
  j.SetValue('field', vfieldname);
  j.SetValue('argc', argcount);
  for i := 1 to argcount do
  begin
    luatype := lua_type(L, i);
    valuekey := 'arg' + inttostr(i) + 'value';
    case luatype of
      LUA_TSTRING:
        j.SetValue(valuekey, lua_tostring(L, i));
      LUA_TNUMBER:
        j.SetValue(valuekey, lua_tonumber(L, i));
      LUA_TBOOLEAN:
        j.SetValue(valuekey, lua_toboolean(L, i));
    end;
    // j.SetValue('arg'+inttostr(i)+'value', plua_tovariant(L, i));
    j.SetValue('arg' + inttostr(i) + 'type', luatype);
  end;
  res := SendMessageToHost(cLOL_CALLFUNCTION, j.text);
  // lua_pushstring(L, vlibname+'.'+vfieldname);
  // lua_pushstring(L, res);
  j.Clear;
  j.text := res;
  argcount := j.GetValue('resc', 0);
  for i := 1 to argcount do
  begin
    plua_pushvariant(L, j.Values['res' + inttostr(i - 1) + 'value']);
  end;
  j.Free;
  result := 1;
end;

function lua_module_reader(L: plua_State): integer; cdecl;
var
  vlibname, vfieldname, json, retjson: string;
  jenum, j: TCatJSON;
  luatype: integer;
begin
  result := 0;
  vlibname := lua_tostring(L, lua_upvalueindex(1));
  json := lua_tostring(L, lua_upvalueindex(2));
  vfieldname := lua_tostring(L, 2);
  jenum := TCatJSON.Create(json);
  case jenum.GetValue(vfieldname, -2) of
    LUA_TNUMBER, LUA_TSTRING, LUA_TBOOLEAN:
      begin
        j := TCatJSON.Create;
        j.SetValue('lib', vlibname);
        j.SetValue('field', vfieldname);
        j.SetValue('method', 'get');
        retjson := SendMessageToHost(cLOL_ACCESSPROP, j.text);
        j.Clear;
        j.text := retjson;
        luatype := j.Values['restype'];
        case luatype of
          LUA_TSTRING:
            lua_pushstring(L, j.Values['resvalue']);
          LUA_TNUMBER:
            lua_pushnumber(L, j.Values['resvalue']);
          LUA_TBOOLEAN:
            lua_pushboolean(L, j.Values['resvalue']);
        end;
        j.Free;
        result := 1;
      end;
    LUA_TFUNCTION:
      begin
        lua_pushstring(L, vlibname);
        lua_pushstring(L, vfieldname);
        lua_pushcclosure(L, lua_module_funccall, 2);
        result := 1;
      end;
  end;
  jenum.Free;
end;

// Changes the value of a module property
// ToDo: while it can create new fields, it is not possible to read new fields
// using the reader function because of pre-built field list created when the
// module was loaded and passed as upvalueindex 2.
function lua_module_writer(L: plua_State): integer; cdecl;
var
  vlibname, vfieldname, json: string;
  j: TCatJSON;
  luatype: integer;
begin
  result := 1;
  vlibname := lua_tostring(L, lua_upvalueindex(1));
  json := lua_tostring(L, lua_upvalueindex(2));
  vfieldname := lua_tostring(L, 2);
  luatype := lua_type(L, 3);
  j := TCatJSON.Create;
  j.SetValue('lib', vlibname);
  j.SetValue('field', vfieldname);
  j.SetValue('method', 'set');
  j.SetValue('settype', luatype);
  case luatype of
    LUA_TSTRING:
      j.SetValue('setvalue', lua_tostring(L, 3));
    LUA_TNUMBER:
      j.SetValue('setvalue', lua_tonumber(L, 3));
    LUA_TBOOLEAN:
      j.SetValue('setvalue', lua_toboolean(L, 3));
    LUA_TFUNCTION, LUA_TTABLE:
      begin
      end; // Yet unsupported types
  end;
  SendMessageToHost(cLOL_ACCESSPROP, j.text);
  j.Free;
end;

procedure luahost_load(L: plua_State; conf: TLOLHostSettings;
  const delayed: boolean = true);
var
  cmd, fn: string;
begin
  if vPID_Host32 = 0 then
  begin
    fn := conf.SearchPath + conf.Exefilename;
    cmd := extractfilepath(paramstr(0)) + fn;
    if fileexists(cmd) then
    begin
      vservername := 'LUAHOST_' + inttostr(GetCurrentProcessId()) + '_' +
        RandomKey;
      vPID_Host32 := RunTask(cmd + ' run ' + vservername, false);
      vSID_Host := RandomKey;
      if delayed = true then
        CatDelay(2000);
    end
    else
    begin
      luaL_error(L, PAnsiChar(AnsiString(conf.Exefilename + ' not found.')));
    end;
  end;
  SendMessageToHost(cLOL_SETSID, vSID_Host);
end;

function lua_require_hosted(L: plua_State): integer; cdecl;
var
  tidx: integer;
  json: string;
begin
  json := SendMessageToHost(cLOL_REQUIRE, lua_tostring(L, 1));
  lua_newtable(L);
  tidx := lua_gettop(L);
  lua_pushstring(L, '__index');
  lua_pushstring(L, lua_tostring(L, 1));
  lua_pushstring(L, json);
  lua_pushcclosure(L, lua_module_reader, 2);
  lua_rawset(L, tidx);

  lua_pushstring(L, '__newindex');
  lua_pushstring(L, lua_tostring(L, 1));
  lua_pushstring(L, json);
  lua_pushcclosure(L, lua_module_writer, 2);
  lua_rawset(L, tidx);

  lua_pushvalue(L, -1);
  lua_setmetatable(L, -2);
  result := 1;
end;

function lua_require32(L: plua_State): integer; cdecl;
begin
  luahost_load(L, cLOLHost32);
  result := lua_require_hosted(L);
end;

function lua_require64(L: plua_State): integer; cdecl;
begin
  luahost_load(L, cLOLHost64);
  result := lua_require_hosted(L);
end;

// If this is same architecture calling require32() or require64(),
// just forward the call to standard require()
function lua_require_forward(L: plua_State): integer; cdecl;
var
  offset: integer;
begin
  offset := lua_gettop(L);
  lua_pushstring(L, 'require');
  lua_rawget(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, lua_tostring(L, 1));
  lua_pcall(L, 1, LUA_MULTRET, 0);
  result := lua_gettop(L) - offset;
end;

// Registers function that allows Lua 64-bit to load 32-bit modules
// or allows Lua 32-bit to load 64-bit modules
// Both require 64-bit Windows because of the WoW (Windows on Windows) support
function luaopen_luaonlua(L: plua_State): integer; cdecl;
begin
{$IFDEF WIN64}
  // I'm a 64-bit Lua app and want to be able to load some legacy 32-bit Lua
  // libraries
  lua_register(L, 'require32', lua_require32);
  lua_register(L, 'require64', lua_require_forward);
{$ELSE}
  // I'm a 32-bit app and want to be able to load 64-bit libraries
  lua_register(L, 'require64', lua_require64);
  lua_register(L, 'require32', lua_require_forward);
{$ENDIF}
  result := 0;
end;

// This method pre-loads the host executable, so when you make the first
// call to the module it will be fast
function luaopen_luaonlua_preloaded(L: plua_State): integer; cdecl;
begin
{$IFDEF WIN64}
  luahost_load(L, cLOLHost32, false); // 32-bit on 64-bit
{$ELSE}
  luahost_load(L, cLOLHost64, false); // 64-bit on 32-bit
{$ENDIF}
  result := luaopen_luaonlua(L);
end;

Exports
  luaopen_luaonlua,
  luaopen_luaonlua_preloaded;

begin

end.
