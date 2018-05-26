unit uPHP;
{
 Underscript PHP Wrapper
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, php4AppUnit;

const
 cObjectName='RSimplePHP';

type
  TUndPHPWrapper = class(TLuaObject)
  private
    RequestID:integer;
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

procedure RegisterUndPHPWrapper(L : Plua_State);

implementation

function method_setvar(l : PLua_State) : Integer; cdecl;
var ht : TUndPHPWrapper;
begin
  ht:=TUndPHPWrapper(LuaToTLuaObject(l, 1));
  RegisterVariable(ht.RequestID, pansichar(lua_tostring(L,2)),pansichar(lua_tostring(L,3)));
  result := 1;
end;

function method_getvar(l : PLua_State) : Integer; cdecl;
var ht : TUndPHPWrapper;  s:string; i : integer;
begin
  ht:=TUndPHPWrapper(LuaToTLuaObject(l, 1));
  i := GetVariableSize(ht.RequestID, pansichar(lua_tostring(L,2)));
  if i > 0 then begin
     SetLength(S, i);
     GetVariable(ht.RequestID, pansichar(lua_tostring(L,2)), PansiChar(S), i);
   end;
  lua_pushstring(L, pchar(s));
  result := 1;
end;

function method_runfile(l : PLua_State) : Integer; cdecl;
var ht : TUndPHPWrapper;
begin
  ht:=TUndPHPWrapper(LuaToTLuaObject(l, 1));
  ExecutePHP(ht.RequestID, PAnsiChar(lua_tostring(L,2)));
  result := 1;
end;

function method_run(l : PLua_State) : Integer; cdecl;
var ht : TUndPHPWrapper;
begin
  ht:=TUndPHPWrapper(LuaToTLuaObject(l, 1));
  ExecuteCode(ht.RequestID, PAnsiChar(lua_tostring(L,2)));
  result := 1;
end;

function GetLuaObject(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
begin
  result := TUndPHPWrapper.Create(L, AParent);
end;

function NewLuaObject(L : PLua_State) : Integer; cdecl;
var p : TLuaObjectNewCallback;
begin
  p := @GetLuaObject;
  result := new_LuaObject(L, cObjectName, p);
end;

procedure RegisterObjectMethods(L : Plua_State; classTable : Integer);
begin
 RegisterMethod(L,'Run', @method_run, classTable);
 RegisterMethod(L,'RunFile', @method_runfile, classTable);
end;

procedure RegisterUndPHPWrapper(L: Plua_State);
begin
  RegisterTLuaObject(L, cObjectName, @NewLuaObject, @RegisterObjectMethods);
end;

constructor TUndPHPWrapper.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  InitEngine;
  RequestID := InitRequest;
end;

function TUndPHPWrapper.GetPropValue(propName: String): Variant;
 function GetResult:string;
 var s:string; i : integer;
 begin
  i := GetResultText(RequestID, nil, 0);
  if i > 0 then begin
     SetLength(S, i);
     GetResultText(RequestID, PChar(S), i);
   end;
  Result:= s;
 end;
begin
  if CompareText(propName, 'Output') = 0 then result := GetResult else
  Result:=inherited GetPropValue(propName);
end;

function TUndPHPWrapper.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  //if CompareText(propName, 'Expression') = 0 then obj.Expression.text := AValue else
  Result:=inherited SetPropValue(propName, AValue);
end;

destructor TUndPHPWrapper.Destroy;
begin
  DoneRequest(RequestID);
  inherited Destroy;
end;

end.

