unit UndHelper_AS;

{
  UnderScript Helper for ActiveScript Component
  Copyright (c) 2013-2020 Felipe Daragon
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
    procedure Debug(s: string);
    procedure Run(Script: String);
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

procedure TUndHelper.Debug(s: string);
begin
  uConsoleDebug(LuaState, s);
end;

procedure TUndHelper.Run(Script: String);
begin
  plua_dostring(LuaState, Script);
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
  uConsoleWrite(LuaState, s);
end;

procedure TUndHelper.WriteLn(s: String);
begin
  //OutDebug('printwriteln:'+s+';expecting: '+rudCustomFunc_WriteLn);
  uConsoleWriteLn(LuaState, s);
end;

initialization

UndHelper := TUndHelper.Create;

finalization

UndHelper.free;

end.
