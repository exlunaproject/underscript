unit uJavaScript_V8;
{
  UnderScript V8 Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, UndImporter, UndConst, UndConsole,
  CatStrings, UndHelper_Obj, vcl.dialogs, CatLogger, v8wrapper;

function JavaScriptV8_Run(L: Plua_State): integer; cdecl;

implementation

function RunJS(L: Plua_State; const script: string):string;
var
  fengine : Tv8Engine;
  Fv8GlobalObject: Iv8Object;
  FObjectTemplate2: Tv8ObjectTemplate;
  FJsAccessableObject: Iv8Object;
begin
  FEngine := Tv8Engine.Create; // create engine
  FEngine.enter;
  Fv8GlobalObject := FEngine.GlobalObject;

  FObjectTemplate2 := FEngine.RegisterRttiClass(TUndHelper);
  FJsAccessableObject := FObjectTemplate2.CreateInstance(UndHelper);
  Fv8GlobalObject.SetObject(rudLibName, FJsAccessableObject);

  result := FEngine.eval(script);

  // leaving
  FObjectTemplate2.Free;
  Fv8GlobalObject := nil;
  FJsAccessableObject := nil;
  FEngine.leave;
  FEngine.Free;
end;

function JavaScriptV8_Run(L: Plua_State): integer; cdecl;
var
  r: TUndScriptResult;
  script: string;
  importer: TUndImporter;
  sw : TCatStopWatch;
begin
  sw := CatStopWatchNew;
  r.success := true;
  UndHelper.LuaState := L;
  importer := TUndImporter.Create(L);
  importer.EnableDebug := false;
  script := lua_tostring(L, 1);
  script := importer.GetScript(L, script, langint_V8).completescript;
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
  if beginswith(trim(r.expressionresult), ['Error:', 'RangeError:', 'ReferenceError:',
  'SyntaxError:', 'TypeError:'])
  then begin
      r.success := false;
      r.errormessage := r.expressionresult;
      uConsoleErrorLn(L, -1, 'JS: ' + r.expressionresult);
  end;


  importer.Free;
  Und_PushScriptResult(L, r, sw);
  Result := 1;
end;

initialization
  v8_init; // initialize v8 library

finalization
  v8_cleanup;

end.
