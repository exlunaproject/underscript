unit UndHelper_Obj; 

{
 UnderScript Helper object
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
 Lua, pLua, Variants, UndConst;

type
  TUndHelper = class
  public
    LuaState:PLua_State;
    constructor Create;
    procedure Write(s:WideString);
    procedure WriteLn(s:WideString); 
    procedure Run(Script:AnsiString); 
    function GetG(valName : AnsiString): Variant;
    procedure SetG(valName : AnsiString; const AValue: Variant);
    function GetL(valName : AnsiString): Variant;
    procedure SetL(valName : AnsiString; const AValue: Variant);
  end;

var
 UndHelper:TUndHelper;

implementation

constructor TUndHelper.Create;
begin
  inherited Create;
end;

procedure TUndHelper.Run(Script:AnsiString);
begin
  luaL_loadbuffer(LuaState, PChar(Script), Length(Script),nil);
  lua_pcall(LuaState, 0, 0, 0);
end;

procedure TUndHelper.SetL(valName : AnsiString; const AValue: Variant);
begin
  pLua_SetLocal(LuaState,ValName,AValue);
end;

function TUndHelper.GetL(valName : AnsiString): Variant;
var v:variant;
begin
 v:=pLua_GetLocal(LuaState,valname); 
 //writeln('v is:'+v);
 result:=v;
end;

procedure TUndHelper.SetG(valName : AnsiString; const AValue: Variant);
begin
  pLua_SetGlobal(LuaState,ValName,AValue);
end;

function TUndHelper.GetG(valName : AnsiString): Variant;
var v:variant;
begin
 v:=pLua_GetGlobal(LuaState,ValName);
 result:=v;
end;

procedure TUndHelper.write(s:WideString);
begin
 Und_CustomWrite(LuaState,s,rudCustomFunc_Write); //system.write(s);
end;

procedure TUndHelper.writeln(s:WideString);
begin
 Und_CustomWriteLn(LuaState,s,rudCustomFunc_WriteLn); //system.writeln(s);
end;

initialization
  UndHelper:=TUndHelper.create;
finalization
  UndHelper.free;

end.
 