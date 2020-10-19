unit uJavaScript_V8;
{
  UnderScript V8 Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, UndImporter, UndConst,
  UndConsole, CatCSUtils,
  CatStrings, UndHelper_Obj, CatLogger, v8wrapper;

function JavaScriptV8_Run(L: Plua_State): integer; cdecl;
function RunJSPersistent(L: Plua_State; const script: string):string;

implementation

var
  feng : Tv8Engine;
  fglobal: Iv8Object;
  ftemplate: Tv8ObjectTemplate;
  fobj: Iv8Object;

procedure v8_cleanpersistent;
begin
  if feng = nil then
   Exit;
  // leaving
  ftemplate.Free;
  fglobal := nil;
  fobj := nil;
  feng.leave;
  feng.Free;
end;

function RunJSPersistent(L: Plua_State; const script: string):string;
begin
  if feng = nil then begin
   feng := Tv8Engine.Create; // create engine
   feng.enter;
   fglobal := feng.GlobalObject;

   ftemplate := feng.RegisterRttiClass(TUndHelper);
   fobj := ftemplate.CreateInstance(UndHelper);
   fglobal.SetObject(rudLibName, fobj);
  end;
  UndHelper.LuaState := L;
  result := feng.eval(script);
end;

function RunJS(L: Plua_State; const script: string):string;
var
  eng : Tv8Engine;
  global: Iv8Object;
  template: Tv8ObjectTemplate;
  obj: Iv8Object;
begin
  eng := Tv8Engine.Create; // create engine
  eng.enter;
  global := eng.GlobalObject;

  template := eng.RegisterRttiClass(TUndHelper);
  obj := template.CreateInstance(UndHelper);
  global.SetObject(rudLibName, obj);

  result := eng.eval(script);

  // leaving
  template.Free;
  global := nil;
  obj := nil;
  eng.leave;
  eng.Free;
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
  v8_cleanpersistent;
  // v8_cleanup; leave clean-up for the engine itself

end.
