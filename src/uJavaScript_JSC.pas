unit uJavaScript_JSC;
{
  UnderScript JavaScriptCore Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, UndImporter, UndConst, UndConsole,
  CatStrings, JSK.Base, UndHelper_Obj, vcl.dialogs;

function JavaScriptJSC_Run(L: Plua_State): integer; cdecl;

implementation

function RunJS(L: Plua_State; const script: string):string;
var
  ctx: TJSContext;
  res: TJSValue;
begin
  ctx := TJSContext.Create;
  try
    ctx.SetObject(rudLibName, UndHelper);
    res := ctx.Evaluate(script);
    if res.IsString then
      result := Res.AsString;
  finally
    ctx.Free;
  end;
end;

function JavaScriptJSC_Run(L: Plua_State): integer; cdecl;
var
  r: TUndScriptResult;
  script: string;
  importer: TUndImporter;
begin
  if plua_validateargs(L, Result, [LUA_TSTRING]).OK = false then
    Exit;
  r.success := true;
  UndHelper.LuaState := L;
  importer := TUndImporter.Create(L);
  importer.EnableDebug := false;
  script := lua_tostring(L, 1);
  script := importer.GetScript(L, script, langint_JavaScriptCore).completescript;
  try
    r.expressionresult := runjs(L, script);
  except
    on E: Exception do
    begin
      r.success := false;
      r.errormessage := E.Message;
      uConsoleErrorLn(L, -1, 'JS: ' + E.Message);
    end;
  end;

  importer.Free;
  Und_PushScriptResult(L, r);
  Result := 1;
end;

end.
