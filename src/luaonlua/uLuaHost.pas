unit uLuaHost;

{
  Lua Clib Host Application
  Copyright (c) 2020 Felipe Daragon
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

uses
  Lua, Windows, SysUtils, pLua, pLuaTable, CatCLUtils, CatStrings,
  CatJSON, CatTasks, CatMsgCromis, uLoL64, Vcl.Forms,
  Cromis.Comm.Custom, Cromis.Comm.IPC, Cromis.Threading, Cromis.AnyValue;

type
 TMsgHandler = class
   public
     function HandleCall(const str: string):string;
     function HandlePropGet(const str: string):string;
     procedure HandleRequest(const Context: ICommContext;
  const Request, Response: IMessageData);
 end;

 procedure StartLuaHost;

implementation

var
 L : Plua_State;
 srv:TCatMsgCromis;
 msg:TMsgHandler;

function TMsgHandler.HandleCall(const str: string):string;
var
  j:TCatJSON;
  tblname, funcname, valuekey: string;
  args: array of Variant;
  i, argcount, offset, rescount, luatype:integer;
begin
  result := emptystr;
  j := TCatJSON.Create(str);
  tblname := j.GetValue('lib', emptystr);
  funcname := j.GetValue('field', emptystr);
  argcount := j.GetValue('argc', 0);
  SetLength(args, argcount);
  for i := 1 to argcount do begin
    args[i-1] := j.values['arg'+inttostr(i)+'value'];
  end;

  if plua_tablefunctionexists(L,tblname, funcname, LUA_GLOBALSINDEX, true) then begin
    offset := lua_gettop(L);
    plua_tablecallfunction(L, tblname, funcname, args, nil);
    rescount := lua_gettop(L)-offset-2;
    j.Clear;
    j.SetValue('resc', rescount);
    for i := 0 to rescount-1 do begin
     luatype := lua_type(L, -(i + 1));
     valuekey := 'res'+inttostr(rescount - i - 1)+'value';
     case luatype of
       LUA_TSTRING: j.SetValue(valuekey, lua_tostring(L, -(i + 1)));
       LUA_TBOOLEAN: j.SetValue(valuekey, lua_toboolean(L, -(i + 1)));
       LUA_TNUMBER: j.SetValue(valuekey, lua_tonumber(L, -(i + 1)));
     end;
       j.SetValue('res'+inttostr(rescount - i - 1)+'type', luatype);
     //j.SetValue('res'+inttostr(rescount - i - 1)+'value', plua_tovariant(L, -(i + 1)));
    end;
    result := j.Text;
  end;
  j.Free;
end;

function TMsgHandler.HandlePropGet(const str: string):string;
var
  j:TCatJSON;
  tblname, fieldname, method: string;
  idx, luatype:integer;
begin
  result := emptystr;
  j := TCatJSON.Create(str);
  tblname := j.GetValue('lib', emptystr);
  fieldname := j.GetValue('field', emptystr);
  method := j.GetValue('method', emptystr);
  lua_getglobal(L, 'tostring');
  lua_pushstring(L, tblname);
  lua_rawget(L, LUA_GLOBALSINDEX);
  idx := lua_gettop(L);
  if method = 'set' then begin
    luatype := j.GetValue('settype', -2);
     case luatype of
       LUA_TSTRING: plua_setfieldvalueV(L, fieldname, j.GetValue('setvalue', emptystr));
       LUA_TBOOLEAN: plua_setfieldvalueV(L, fieldname, j.GetValue('setvalue', false));
       LUA_TNUMBER: plua_setfieldvalueV(L, fieldname, j.GetValue('setvalue', 0));
     end;
     j.Clear;
  end else
  if method = 'get' then begin
    j.Clear;
    luatype := plua_GetFieldValueType(L, idx, fieldname);
    case luatype of
      LUA_TSTRING: j.SetValue('resvalue',plua_GetFieldValueStr(L, idx, fieldname));
      LUA_TBOOLEAN: j.SetValue('resvalue',plua_GetFieldValueBool(L, idx, fieldname, false));
      LUA_TNUMBER: j.SetValue('resvalue',plua_GetFieldValueInt(L, idx, fieldname, 0));
    end;
    j.SetValue('restype',luatype);
  end;

  result := j.Text;
  j.Free;
end;

procedure TMsgHandler.HandleRequest(const Context: ICommContext;
  const Request, Response: IMessageData);
var cmdid:integer; command, json, sid:string;
begin
  cmdid := Request.Data.ReadInteger('CmdID');
  command := Request.Data.ReadString('Command');
  sid := Request.Data.ReadString('SID');
  if (cmdid = cLOL_SETSID) and (vSID_Host = emptystr) then begin
    vSID_Host := command;
  end;
  if vSID_Host <> SID then
    exit;
  case cmdid of
    cLOL_REQUIRE: begin
      command := GetValidCompName(command); // strips invalid characters
      plua_dostring(L, 'if '+command+' == nil then '+command+' = require("'+command+'") end');
      plua_dostring(L, '_enum32res = _enum32('+command+')');
      json := plua_GetGlobal(L, '_enum32res');
      response.ID := 'ret';
      response.Data.WriteString('json', json);
    end;
    cLOL_CALLFUNCTION: begin
      json := HandleCall(command);
      response.ID := 'ret';
      response.Data.WriteString('json',json);
    end;
    cLOL_ACCESSPROP: begin
      json := HandlePropGet(command);
      response.ID := 'ret';
      response.Data.WriteString('json',json);
      end;
    cLOL_SHUTDOWN: begin
      // ToDo: improve shutdown
      Halt(1);
    end;
  end;

end;

function lua_enum32(L: plua_State):integer; cdecl;
var j:TCatJSON;
begin
 J := TCatJson.Create;
 //if ((lua_type(L, -2) = LUA_TSTRING)) then
 //   showmessage(lua_tostring(L, -2));
 lua_pushnil(L);
        while (lua_next(L, -2) <> 0) do begin
           j.SetValue(lua_tostring(L, -2),lua_type(L, -1));
           {if(lua_iscfunction(L, -1)) then
                showmessage(lua_tostring(L, -2))
            else if(lua_isstring(L, -1)) then
                showmessage(lua_tostring(L, -2))
            else if(lua_isnumber(L, -1)) then
                showmessage(inttostr(lua_tointeger(L, -2)));
            //else if(lua_istable(L, -1)) then
            //    showmessage; }
            lua_pop(L, 1);
        end;
  lua_pushstring(L, j.Text);
  //j.SaveToFile(outjsonfilename);
  j.Free;

  {showmessage(lua_typename(L,4));
  if lua_type(L,1) = LUA_TTABLE then
  showmessage('yes');
  result := 1;       }
  result := 1;
end;

procedure StartLuaHost;
begin
 L := lua_open;
 luaL_openlibs(L);
  lua_register(L, '_enum32', lua_enum32);
  //lua_register(L, 'showmsg', lua_showmsg);
 if paramstr(1) = 'run' then begin
   if TaskRunningCount(paramstr(0),true) = 1 then begin
     msg := TMsgHandler.Create;
     srv := TCatMsgCromis.Create;
     srv.Server.ServerName := paramstr(2);
     srv.Server.stop;
     srv.Server.Start;
     srv.OnRequest := msg.HandleRequest;
     while (application.Terminated = false) do
       application.HandleMessage;

     srv.Free;
     msg.Free;
   end;
 end;
 //plua_dostring(L, 'lib=require("bit");for k, v in pairs(lib) do showmsg(k) end;enum32({test="hello"})');
 //showmessage(lua_tostring(L, -1));
 //showmessage(GetCmdLine(2));
end;

end.
