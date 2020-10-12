unit uJavaScript_JSC;
{
  UnderScript JavaScriptCore Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, UndConst,
  CatStrings, JSK.Base, UndHelper_Multi, vcl.dialogs, CatLogger;

procedure JavaScriptJSC_Run(const script:string);

implementation

function RunJS(const script: string):string;
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

procedure JavaScriptJSC_Run(const script:string);
var
  r: TUndScriptResult;
  sw : TCatStopWatch;
begin
  sw := CatStopWatchNew;
  r.success := true;
  try
    r.expressionresult := runjs(script);
  except
    on E: Exception do
    begin
      r.success := false;
      r.errormessage := E.Message;
      uConsoleWriteError(-1, 'JS: ' + E.Message);
    end;
  end;
end;

end.
