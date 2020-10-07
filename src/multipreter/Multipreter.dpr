program Multipreter;

{
 UnderScript Multipreter
 Copyright (c) 2020 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

{$APPTYPE CONSOLE}
{$DEFINE UNDER_JAVASCRIPTCORE}

uses
 Classes, SysUtils,
 UndConst in '..\UndConst.pas',
 {$IFDEF WIN64}
 js15decl in '..\thirdparty\js_spidermonkey\js15decl.pas',
 jsDbgServer in '..\thirdparty\js_spidermonkey\jsDbgServer.pas',
 jsintf in '..\thirdparty\js_spidermonkey\jsintf.pas',
 NamedPipesImpl in '..\thirdparty\js_spidermonkey\NamedPipesImpl.pas',
 quickjs in '..\thirdparty\js_quickjs\quickjs.pas',
 uJavaScript_SM,
 uJavaScript_Quick,
  {$IFDEF UNDER_JAVASCRIPTCORE}
  uJavaScript_JSC,  
  JSK.Base in '..\thirdparty\js_javascriptcore\JSK.Base.pas',
  JSK.API in '..\thirdparty\js_javascriptcore\JSK.API.pas',
  {$ENDIF} 
 {$ELSE}
 {$ENDIF}
 CatCLUtils;

{$I CatCompactBin.inc}
{$R *.res}

var
  scriptfilename, script:string;
  sl:TStringList;

begin
  scriptfilename := emptystr;
  if paramstr(2) <> emptystr then
    scriptfilename := trim(GetCmdLine(2));

  if fileexists(scriptfilename) then begin
    sl := TStringList.Create;
    sl.LoadFromFile(scriptfilename);
    script := sl.Text;
    sl.Free;
    if paramstr(1) = 'javascriptcore' then
      JavaScriptJSC_Run(script) else
    if paramstr(1) = 'spidermonkey' then
      JavaScriptSM_Run(script) else
    if paramstr(1) = 'quickjs' then
      JavaScriptQuick_Run(script);
  end;
end.
