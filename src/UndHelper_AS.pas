unit UndHelper_AS;

{
  UnderScript Helper for ActiveScript Component
  Copyright (c) 2013-2014 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  ObjComAuto, Lua, pLua, Variants, CatJSON, CatUtils, SysUtils, UndConst;

type
{$METHODINFO ON}
  TUndHelper = class(TObjectDispatch, IDispatch)
  protected
  public
    LuaState: PLua_State;
    constructor Create;
    procedure Run(Script: String);
    procedure FindFunc(FuncName: String); // experimental
    procedure FindModFunc(TableName: String; FuncName: String); // experimental
    procedure Push(v: Variant); // experimental
    procedure CallFunc(args: integer); // experimental
    function GetG(valName: String): Variant;
    procedure SetG(valName: String; const AValue: Variant);
    function GetL(valName: String): Variant;
    procedure SetL(valName: String; const AValue: Variant);
    procedure Write(s: String);
    procedure WriteLn(s: String);
  published
  end;
{$METHODINFO OFF}

var
  UndHelper: TUndHelper;

implementation

constructor TUndHelper.Create;
begin
  inherited Create(Self, false);
end;

procedure TUndHelper.FindModFunc(TableName: String; FuncName: String);
// experimental
var
  tblidx, offset: integer;
begin
  lua_pushstring(LuaState, TableName);
  lua_rawget(LuaState, LUA_GLOBALSINDEX);
  if lua_istable(LuaState, -1) then
  begin
    // writeln(TableName+' table found');
    tblidx := lua_gettop(LuaState);
    // start func execution
    lua_pushstring(LuaState, FuncName);
    lua_rawget(LuaState, -2);
  end;
end;

procedure TUndHelper.FindFunc(FuncName: String); // experimental
begin
  lua_getglobal(LuaState, PAnsiChar(AnsiString(FuncName)));
end;

procedure TUndHelper.Push(v: Variant); // experimental
begin
  plua_pushvariant(LuaState, v);
end;

procedure TUndHelper.CallFunc(args: integer); // experimental
begin
  lua_pcall(LuaState, args, 0, 0);
end;

procedure TUndHelper.Run(Script: String);
begin
  // calls lua 5.1 pcall
  luaL_loadbuffer(LuaState, PAnsiChar(AnsiString(Script)), Length(Script), nil);
  lua_pcall(LuaState, 0, 0, 0);
end;

function TUndHelper.GetL(valName: String): Variant;
var
  v: Variant;
begin
  // this is necessary because if you write to the result directy it will not work for locals
  try
  v := pLua_GetLocal(LuaState, valName);
  except
  end;
  //writeln('debug: returning GetL:'+valname+' result:'+v);
  result := v;
end;

function TUndHelper.GetG(valName: String): Variant;
var
  v: Variant;
begin
  try
  v := pLua_GetGlobal(LuaState, valName);
  except
  end;
  // writeln('debug: returning GetG:'+valname+' result:'+v);
  result := v;
end;

procedure TUndHelper.SetG(valName: String; const AValue: Variant);
var v:variant;
begin
  v := AValue;
  try
    pLua_SetGlobal(LuaState, valName, v);
  except
  end;
end;

procedure TUndHelper.SetL(valName: String; const AValue: Variant);
var v:Variant;
begin
  v := AValue;
  //writeln('debug: setting SetL:'+valname+' value:'+avalue);
  try
    pLua_SetLocal(LuaState, valName, v);
  except
  end;
end;

procedure TUndHelper.Write(s: String);
begin
  Und_CustomWrite(LuaState, s, rudCustomFunc_Write);
end;

procedure TUndHelper.WriteLn(s: String);
begin
  //OutDebug('printwriteln:'+s+';expecting: '+rudCustomFunc_WriteLn);
  Und_CustomWriteLn(LuaState, s, rudCustomFunc_WriteLn);
end;

initialization

UndHelper := TUndHelper.Create;

finalization

UndHelper.free;

end.
