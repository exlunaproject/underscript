unit uJavaScript_SM;
{
  UnderScript JavaScript SpiderMonkey Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, UndImporter, UndConsole, UndConst,
  CatStrings, js15decl,jsintf, UndHelper_Obj, vcl.dialogs, CatLogger;

type
  [JSClassName('UConsole')]
{$METHODINFO ON}
{  TUndJavaScriptSM = class(TJSClass)
    public
      //State: Plua_State;
      procedure WriteLn(s: String);
  end; }

  TJSGlobalFunctions = class
    class procedure print(s: string);
  end;
{$METHODINFO OFF}

function JavaScriptSM_Run(L: Plua_State): integer; cdecl;

implementation

function JavaScriptSM_Run(L: Plua_State): integer; cdecl;
var
  eng: TJSEngine;
  r: TUndScriptResult;
  script: string;
  importer: TUndImporter;
  sw: TCatStopWatch;
begin
  sw := CatStopWatchNew;
  r.success := true;
  eng := TJSEngine.Create;
  eng.registerGlobalFunctions(TJSGlobalFunctions);
  eng.registerClasses([TUndHelper]);
  TJSClass.CreateJSObject(UndHelper, eng, 'UConsole', [cfaInheritedMethods, cfaInheritedProperties]);
  UndHelper.LuaState := L;
  importer := TUndImporter.Create(L);
  importer.EnableDebug:=false;
  script := lua_tostring(L, 1);
  script := importer.GetScript(L, script, langint_JSSpiderMonkey).completescript;
  try
    eng.Evaluate(script);
  except
    on E: Exception do begin
      r.success := false;
      r.errormessage := E.Message;
      uConsoleErrorLn(L, -1, 'JS: ' + E.Message);
    end;
  end;
  importer.free;
  eng.Free;
  r.elapsedtime := GetStopWatchElapsedTime(sw).MsAsString;
  Und_PushScriptResult(L, r, sw);
  result := 1;
end;

{procedure TUndJavaScriptSM.WriteLn(s: String);
begin
  UndHelper.WriteLn(s);
end; }

class procedure TJSGlobalFunctions.print(s: string);
begin
  UndHelper.WriteLn(s);
end;

end.
