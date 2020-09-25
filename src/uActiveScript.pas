unit uActiveScript;
{
  UnderScript JavaScript, VBScript & other ActiveScript Wrappers
  Copyright (c) 2013-2014 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, ActiveX, CatJSRunnerAS, UndHelper_AS,
  CatStrings, UndImporter, UndConst, UndConsole, TypInfo;

const
  cDefaultLanguage: string = 'JavaScript';

type
  TActiveLanguage = (LuaScript, PerlScript, JavaScript, VBScript);

function JavaScript_Run(L: plua_State): integer; cdecl;
function PerlScript_Run(L: plua_State): integer; cdecl;
function VBScript_Run(L: plua_State): integer; cdecl;
function ActiveScript_Run(L: plua_State): integer; cdecl;

implementation

procedure PrepareLanguage(Importer: TUndImporter; Lang: string);
const
  debug = ''; // 'Und.WriteLn("reading %k");d=new Date();';
  debugend = ''; // 'Und.WriteLn("%k loaded as:"+%k);';
begin
  // Importer.EnableDebug:=true;
  Importer.EnableImport := true;
  case TActiveLanguage(GetEnumValue(TypeInfo(TActiveLanguage), Lang)) of
    JavaScript:
      begin
        Importer.FuncReadFormat := debug + '%k = %l.GetL("%k");' + debugend;
        Importer.FuncWriteFormat := crlf + '%l.SetL("%k",%k);';
      end;
    LuaScript:
      begin
        Importer.FuncReadFormat := '%k = %l:GetL("%k")' + crlf;
        Importer.FuncWriteFormat := crlf + '%l:SetL("%k",%k)';
      end;
    PerlScript:
      begin
        Importer.FuncReadFormat := '$%k = $%l->GetL("%k");';
        Importer.FuncWriteFormat := crlf + '$%l->SetL("%k",$%k);';
      end;
    VBScript:
      begin
        Importer.FuncReadFormat := '%k = %l.GetL("%k")' + crlf;
        Importer.FuncWriteFormat := crlf + '%l.SetL "%k",%k';
      end;
  else
    // language not found, disables import of variables
    Importer.EnableImport := false;
  end;
end;

type
  TScriptErrorHandler = class
  public
    scriptlanguage: string;
    procedure ScriptError(Line, Pos: integer; ASrc, ADescription: String);
  end;

procedure TScriptErrorHandler.ScriptError(Line, Pos: integer;
  ASrc, ADescription: String);
begin
  uConsoleErrorLn(UndHelper.LuaState, Line, scriptlanguage + ': ' + ADescription +
    ' [' + ASrc + ']');
  // errors.add(inttostr(line)+': '+Adescription+' ['+Asrc+']');
  //  UndHelper.writeln(scriptlanguage + ': ' + inttostr(Line) + ': ' +
  //    ADescription + ' [' + ASrc + ']');
end;

// Runs script and returns the result and error info (if any)
function Script_Run(L: plua_State; Script, Lang: string;
  iseval: boolean = false): integer;
var
  r: TUndScriptResult;
  obj: TScarlettActiveScript;
  uconsole: TUndHelper;
  Importer: TUndImporter;
  eh: TScriptErrorHandler;
begin
  r.success := true;
  CoInitialize(nil);
  eh := TScriptErrorHandler.create;
  Importer := TUndImporter.create(L);
  Importer.EnableDebug := false;
  PrepareLanguage(Importer, Lang);
  uconsole := TUndHelper.create;
  uconsole.LuaState := L; // IMPORTANT!
  UndHelper.LuaState := L; // IMPORTANT!
  obj := TScarlettActiveScript.create(UndHelper);
  obj.asw.scriptlanguage := cDefaultLanguage; // javascript
  obj.OnScriptError := eh.ScriptError;
  if Lang <> emptystr then
    obj.asw.scriptlanguage := Lang;
  eh.scriptlanguage := obj.asw.scriptlanguage;
  obj.asw.UseSafeSubset := false;
  obj.asw.AddNamedItem(rudLibName, uconsole);
  Script := lua_tostring(L, 1);
  Script := Importer.GetScript(L, Script);
  Importer.Free;
  try
    if iseval then
    begin
      r.expressionresult := obj.runexpression(Script);
    end
    else
    begin
      obj.runscript(Script);
    end;
  except
  end;

  if obj.asw_success = false then
  begin
    r.success := false;
    r.errormessage := obj.errors.Text;
    // UndHelper.writeln(obj.asw.scriptlanguage+': '+obj.errors.Text);
  end;
  obj.Free;
  eh.Free;
  CoUninitialize;
  Und_PushScriptResult(L, r);
  result := 1;
end;

// JavaScript Functions
function JavaScript_Run(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), cDefaultLanguage);
end;

function JavaScript_Eval(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), cDefaultLanguage, true);
end;

// VBScript Functions
function VBScript_Run(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), 'VBScript');
end;

function VBScript_Eval(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), 'VBScript', true);
end;

// PerlScript Functions
function PerlScript_Run(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), 'PerlScript');
end;

function PerlScript_Eval(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), 'PerlScript', true);
end;

// LuaScript Functions
function LuaScript_Run(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), 'LuaScript');
end;

function LuaScript_Eval(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 1), 'LuaScript', true);
end;

function ActiveScript_Run(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
    Script_Run(L, lua_tostring(L, 2), lua_tostring(L, 1));
end;

function ActiveScript_Eval(L: plua_State): integer; cdecl;
begin
  if plua_validateargs(L, result, [LUA_TSTRING, LUA_TSTRING]).OK then
  result := Script_Run(L, lua_tostring(L, 2), lua_tostring(L, 1), true);
end;

end.
