unit UndConsole;

{
  UnderScript Console
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  SysUtils, Lua, pLua, pLuaTable, CatStrings, CatUtils, UndConst, CatFiles,
  CatLogger;

type
  TCrossModuleCheckResult = record
    Exists:boolean;
    Script:string;
  end;

function uConsoleCrossModuleExists(L: plua_State; const module, ext:string):TCrossModuleCheckResult;
procedure uConsoleDebug(L: plua_State; s: String);
procedure uConsoleErrorLn(L: plua_State; line: integer; msg: String);
procedure uConsoleWrite(L: plua_State; s: String);
procedure uConsoleWriteLn(L: plua_State; s: String);
procedure Und_PushScriptResult(L: plua_State; res:TUndScriptResult; sw:TCatStopWatch);


implementation

procedure Und_PushScriptResult(L: plua_State; res:TUndScriptResult; sw:TCatStopWatch);
begin
 res.elapsedtime := GetStopWatchElapsedTime(sw).MsAsString;
 lua_newtable(L);
 plua_SetFieldValue(L, 'success', res.success);
 plua_SetFieldValue(L, 'errormsg', res.ErrorMessage);
 plua_SetFieldValue(L, 'expresult', res.expressionresult);
 plua_SetFieldValue(L, 'elapsedtime', res.elapsedtime);
end;

procedure uConsoleDebug(L: plua_State; s: String);
const cFuncName = 'debug';
begin
  if rudRedirectIO = true then begin
    if plua_tablefunctionexists(L, cUndConsoleLibName, cFuncName) then
      plua_tablecallfunction(L, cUndConsoleLibName, cFuncName, [s]);
  end else begin
    OutDebug(s);
  end;
end;

procedure uConsoleErrorLn(L: plua_State; line: integer; msg: String);
const cFuncName = 'errorln';
begin
  if rudRedirectIO = true then begin
    if plua_tablefunctionexists(L, cUndConsoleLibName, cFuncName) then
      plua_tablecallfunction(L, cUndConsoleLibName, cFuncName, [line, msg]);
  end;
  // else begin
  //  uConsoleWriteError(line, msg);
  // end;
  if rudHandleErrors = true then
    luaL_error(L, '_script:('+inttostr(line)+'): '+msg);
end;

function uConsoleCrossModuleExists(L: plua_State; const module, ext:string):
  TCrossModuleCheckResult;
var fn:string;
begin
  result.Script := emptystr;
  fn := extractfilepath(paramstr(0))+'\Lib\crosslang\'+module+'.'+ext;
  result.Exists := fileexists(fn);
  if (result.Exists = true) then begin
    result.Script := GetFileToStr(fn);
  end else begin
    if rudHandleErrors = true then
      luaL_error(L, 'cross-language module '''+module+''' not found.');
  end;
end;

procedure uConsoleWriteError(line: integer; msg: String);
begin
  system.WriteLn('--('+inttostr(line)+'): '+msg);
end;

procedure uConsoleWrite(L: plua_State; s: String);
const cFuncName = 'write';
begin
  if rudRedirectIO = true then begin
    if plua_tablefunctionexists(L, cUndConsoleLibName, cFuncName) then
      plua_tablecallfunction(L, cUndConsoleLibName, cFuncName, [s]);
  end else begin
    system.Write(s);
  end;
end;

procedure uConsoleWriteLn(L: plua_State; s: String);
const cFuncName = 'writeln';
begin
  if rudRedirectIO = true then begin
    if plua_tablefunctionexists(L, cUndConsoleLibName, cFuncName) then
      plua_tablecallfunction(L, cUndConsoleLibName, cFuncName, [s]);
  end else begin
    system.WriteLn(s);
  end;
end;

// ------------------------------------------------------------------------//
end.
