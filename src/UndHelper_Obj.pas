unit UndHelper_Obj;

{
  UnderScript Helper object
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Lua, pLua, Variants, UndConst;

type
  TUndHelper = class
  public
    LuaState: PLua_State;
    constructor Create;
    procedure Write(s: String);
    procedure WriteLn(s: String);
    procedure Run(Script: String);
    function GetG(valName: String): Variant;
    procedure SetG(valName: String; const AValue: Variant);
    function GetL(valName: String): Variant;
    procedure SetL(valName: String; const AValue: Variant);
  end;

var
  UndHelper: TUndHelper;

implementation

constructor TUndHelper.Create;
begin
  inherited Create;
end;

procedure TUndHelper.Run(Script: String);
begin
  luaL_loadbuffer(LuaState, PAnsiChar(AnsiString(Script)), Length(Script), nil);
  lua_pcall(LuaState, 0, 0, 0);
end;

procedure TUndHelper.SetL(valName: String; const AValue: Variant);
begin
  pLua_SetLocal(LuaState, valName, AValue);
end;

function TUndHelper.GetL(valName: String): Variant;
var
  v: Variant;
begin
  try
    v := pLua_GetLocal(LuaState, valName);
  except
  end;
  // writeln('v is:'+v);
  result := v;
end;

procedure TUndHelper.SetG(valName: String; const AValue: Variant);
var
  v: Variant;
begin
  v := AValue;
  try
    pLua_SetGlobal(LuaState, valName, v);
  except
  end;
end;

function TUndHelper.GetG(valName: String): Variant;
var
  v: Variant;
begin
  try
    v := pLua_GetGlobal(LuaState, valName);
  except
  end;
  result := v;
end;

procedure TUndHelper.Write(s: String);
begin
  Und_CustomWrite(LuaState, s);
end;

procedure TUndHelper.writeln(s: String);
begin
  Und_CustomWriteLn(LuaState, s);
end;

initialization

UndHelper := TUndHelper.Create;

finalization

UndHelper.free;

end.
