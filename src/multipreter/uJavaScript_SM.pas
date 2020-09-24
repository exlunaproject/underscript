unit uJavaScript_SM;
{
  UnderScript JavaScript SpiderMonkey Wrapper
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)

  Note: jsintf must handle variant type so it can be useful
}

interface

uses
  Classes, SysUtils, UndConst, CatStrings, js15decl, jsintf, UndHelper_Multi,
  vcl.dialogs;

type
  [JSClassName('UConsole')]
{$METHODINFO ON}
  TUndJavaScriptSM = class(TJSClass)
    public
      procedure WriteLn(s: String);
  end;

  TJSGlobalFunctions = class
    class procedure print(s: string);
  end;
{$METHODINFO OFF}

procedure JavaScriptSM_Run(const script:string);

implementation

procedure JavaScriptSM_Run(const script:string);
var
  eng: TJSEngine;
  r: TUndScriptResult;
begin
  r.success := true;
  eng := TJSEngine.Create;
  eng.registerGlobalFunctions(TJSGlobalFunctions);
  eng.registerClasses([TUndHelper]);
  //TJSClass.CreateJSObject(UndHelper, eng, cUnd, [cfaInheritedMethods, cfaInheritedProperties]);
  try
    eng.Evaluate(script);
  except
    on E: Exception do begin
      r.success := false;
      r.errormessage := E.Message;
      uConsoleWriteError(-1, 'JS: ' + E.Message);
    end;
  end;

  eng.Free;
end;

procedure TUndJavaScriptSM.WriteLn(s: String);
begin
  WriteLn(s);
end;

class procedure TJSGlobalFunctions.print(s: string);
begin
  WriteLn(s);
end;

end.
