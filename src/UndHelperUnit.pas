unit UndHelperUnit;

{
 UnderScript Helper for ActiveScript Component 
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
 
 Based on psvScriptObj.pas
}

interface

uses
 ObjComAuto, Lua, pLua, Variants, SysUtils, UndConst;

type
  {$METHODINFO ON}
  TUndHelper = class(TObjectDispatch, IDispatch)
  public
    LuaState:PLua_State;
    constructor Create;
    procedure Write(s:WideString); stdcall;
    procedure WriteLn(s:WideString); stdcall;
    procedure Run(Script:AnsiString); stdcall;
    procedure FindFunc(FuncName:AnsiString); stdcall; // 2013 test
    procedure FindModFunc(TableName:AnsiString;FuncName:AnsiString); stdcall; // 2013 test
    procedure Push(v:Variant); stdcall; // 2013 test
    procedure CallFunc(args:integer); stdcall; // 2013 test
    function GetG(valName : AnsiString): Variant; stdcall;
    procedure SetG(valName : AnsiString; const AValue: Variant); stdcall;
    function GetL(valName : AnsiString): Variant; stdcall;
    procedure SetL(valName : AnsiString; const AValue: Variant); stdcall;
  end;
  {$METHODINFO OFF}

var
 UndHelper:TUndHelper;

implementation

{ TpsvAppHelper }

constructor TUndHelper.Create;
begin
  inherited Create(Self, false);
end;

procedure TUndHelper.FindModFunc(TableName:AnsiString;FuncName:AnsiString); stdcall; // experimental
var tblidx,offset:Integer;
begin
  lua_pushstring(LuaState, PChar(TableName));
  lua_rawget(LuaState, LUA_GLOBALSINDEX);
  if lua_istable(LuaState, -1) then begin
   //writeln(TableName+' table found');
   tblidx := lua_gettop(LuaState);
   // start func execution
   lua_pushstring(LuaState, FuncName);
   lua_rawget(LuaState, -2);
  end;
end;

procedure TUndHelper.FindFunc(FuncName:AnsiString); stdcall; // experimental
begin
 lua_getglobal(LuaState,PChar(FuncName));
end;

procedure TUndHelper.Push(v:Variant); stdcall; // experimental
begin
 plua_pushvariant(LuaState,v);
end;

procedure TUndHelper.CallFunc(args:integer); stdcall; // experimental
begin
 lua_pcall(LuaState, args, 0, 0);
end;

procedure TUndHelper.Run(Script:AnsiString); stdcall;
begin
  luaL_loadbuffer(LuaState, PChar(Script), Length(Script),nil);
  lua_pcall(LuaState, 0, 0, 0);
  // was pcall in lua 5.1
end;

function TUndHelper.GetL(valName : AnsiString): Variant;
var v:variant;
begin
 v:=pLua_GetLocal(LuaState,ValName);   // this is necessary if you write to the result directy it will not work
 //writeln('returning GetL:'+valname+' result:'+v);
 result:=v;
end;

function TUndHelper.GetG(valName : AnsiString): Variant;
var v:variant;
begin
 v:=pLua_GetGlobal(LuaState,ValName); 
 //writeln('returning GetG:'+valname+' result:'+v);
 result:=v;
end;

procedure TUndHelper.SetG(valName : AnsiString; const AValue: Variant);
begin
  try pLua_SetGlobal(LuaState,ValName,AValue);  except end;
end;

procedure TUndHelper.SetL(valName : AnsiString; const AValue: Variant);
begin
  try pLua_SetLocal(LuaState,ValName,AValue); except end;
end;

procedure TUndHelper.write(s:WideString);
begin
 Und_CustomWrite(LuaState,s,rudCustomFunc_Write); //system.write(s);
end;

procedure TUndHelper.writeln(s:WideString);
begin
 Und_CustomWriteLn(LuaState,s,rudCustomFunc_WriteLn); //system.writeln(s);
end;

{function TUndHelper.GetExeName: WideString;
begin
  Result := Application.ExeName;
end;}

initialization
  UndHelper:=TUndHelper.create;
finalization
  UndHelper.free;

end.
 