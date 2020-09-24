unit uJavaScript_SM;
{
  UnderScript JavaScript SpiderMonkey Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)

  Note: jsintf must handle variant type so it can be useful
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, UndImporter, UndConst, CatStrings,
  js15decl,jsintf, UndHelper_Obj, vcl.dialogs;

type
  [JSClassName('UConsole')]
{$METHODINFO ON}
  TUndJavaScriptSM = class(TJSClass)
    public
      //State: Plua_State;
      procedure WriteLn(s: String);
  end;

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
 // helper: TUndJavaScriptSM;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK = false then
    Exit;
  r.success := true;
  eng := TJSEngine.Create;
  eng.registerGlobalFunctions(TJSGlobalFunctions);
  eng.registerClasses([TUndHelper]);
  //helper:= TUndJavaScriptSM.CreateJSObject(eng, 'UConsole') ;
  //helper.State := L;
  TJSClass.CreateJSObject(UndHelper, eng, 'UConsole', [cfaInheritedMethods, cfaInheritedProperties]);
  UndHelper.LuaState := L;
  importer := TUndImporter.Create(L);
  importer.EnableDebug:=false;
  importer.FuncReadFormat := '%k = ' + rudLibName + '.GetL("%k");';
  importer.FuncWriteFormat := crlf + rudLibName + '.SetL("%k",%k);';
  script := lua_tostring(L, 1);
  script := importer.GetScript(L, script);
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
  Und_PushScriptResult(L, r);
  result := 1;
end;

procedure TUndJavaScriptSM.WriteLn(s: String);
begin
  UndHelper.WriteLn(s);
end;

class procedure TJSGlobalFunctions.print(s: string);
begin
  UndHelper.WriteLn(s);
end;

end.
