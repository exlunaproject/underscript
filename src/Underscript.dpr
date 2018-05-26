library Underscript;
{
 UnderScript Script Runner Lua library
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)

 This library adds to Lua the ability to run code written in multiple
 programming languages such as:
 JavaScript, PascalScript, Perl, PHP, Python, Ruby & VBScript
}

{$DEFINE UNDER_ACTIVESCRIPT}
{$DEFINE UNDER_PASCAL}
{$DEFINE UNDER_PASCAL_CLASSIC}
{$DEFINE UNDER_PHP}
{$DEFINE UNDER_PYTHON}
{$DEFINE UNDER_RUBY}

uses
  SysUtils, TypInfo, Lua, pLua, pLuaTable, CatStrings,
  {$IFDEF UNDER_ACTIVESCRIPT}
  uActiveScript,
  SyJSRunnerAS in 'thirdparty\activescript\SyJSRunnerAS.pas',
  SyActiveScript in 'thirdparty\activescript\SyActiveScript.pas',
  {$ENDIF}
  {$IFDEF UNDER_PHP}
  uPHP, uPHP_Script,
  php4AppUnit in 'thirdparty\php\php4AppUnit.pas',
  zendTypes in 'thirdparty\php\zendTypes.pas',
  PHPTypes in 'thirdparty\php\PHPTypes.pas',
  PHPAPI in 'thirdparty\php\PHPAPI.pas',
  ZENDAPI in 'thirdparty\php\ZENDAPI.pas',
  PHPLibrary in 'thirdparty\php\PHPLibrary.pas',
  PHPCustomLibrary in 'thirdparty\php\PHPCustomLibrary.pas',
  PHPCommon in 'thirdparty\php\PHPCommon.pas',
  PHPFunctions in 'thirdparty\php\PHPFunctions.pas',
  php4delphi in 'thirdparty\php\php4delphi.pas',
  DelphiFunctions in 'thirdparty\php\DelphiFunctions.pas',
  {$ENDIF}
  {$IFDEF UNDER_PYTHON}
  uPython,
  PythonEngine in 'thirdparty\python\PythonEngine.pas',
  TinyWideStrings in 'thirdparty\python\TinyWideStrings.pas',
  MethodCallBack in 'thirdparty\python\MethodCallBack.pas',
  VarPyth in 'thirdparty\python\VarPyth.pas',
  {$ENDIF}
  {$IFDEF UNDER_RUBY}
  uRuby,
  {$ENDIF}
  {$IFDEF UNDER_PASCAL}
  uDWS,
  dws2Comp in 'thirdparty\pascal\dws2Comp.pas',
  dws2Compiler in 'thirdparty\pascal\dws2Compiler.pas',
  dws2Exprs in 'thirdparty\pascal\dws2Exprs.pas',
  dws2Symbols in 'thirdparty\pascal\dws2Symbols.pas',
  dws2Strings in 'thirdparty\pascal\dws2Strings.pas',
  dws2Stack in 'thirdparty\pascal\dws2Stack.pas',
  dws2Errors in 'thirdparty\pascal\dws2Errors.pas',
  dws2Functions in 'thirdparty\pascal\dws2Functions.pas',
  dws2Tokenizer in 'thirdparty\pascal\dws2Tokenizer.pas',
  dws2Debugger in 'thirdparty\pascal\dws2Debugger.pas',
  dws2MathFunctions in 'thirdparty\pascal\dws2MathFunctions.pas',
  dws2StringFunctions in 'thirdparty\pascal\dws2StringFunctions.pas',
  dws2TimeFunctions in 'thirdparty\pascal\dws2TimeFunctions.pas',
  dws2VariantFunctions in 'thirdparty\pascal\dws2VariantFunctions.pas',
  dws2CompStrings in 'thirdparty\pascal\dws2CompStrings.pas',
  dws2HtmlFilter in 'thirdparty\pascal\dws2HtmlFilter.pas',
  dws2StringResult in 'thirdparty\pascal\dws2StringResult.pas',
  {$ENDIF}
  {$IFDEF UNDER_PASCAL_CLASSIC}
  uPascal,
  uPSCompiler in 'thirdparty\pascal\rem\uPSCompiler.pas',
  uPSUtils in 'thirdparty\pascal\rem\uPSUtils.pas',
  uPSRuntime in 'thirdparty\pascal\rem\uPSRuntime.pas',
  uPSComponent in 'thirdparty\pascal\rem\uPSComponent.pas',
  uPSDebugger in 'thirdparty\pascal\rem\uPSDebugger.pas',
  uPSC_dll in 'thirdparty\pascal\rem\uPSC_dll.pas',
  uPSC_std in 'thirdparty\pascal\rem\uPSC_std.pas',
  uPSC_stdctrls in 'thirdparty\pascal\rem\uPSC_stdctrls.pas',
  uPSC_forms in 'thirdparty\pascal\rem\uPSC_forms.pas',
  uPSC_graphics in 'thirdparty\pascal\rem\uPSC_graphics.pas',
  uPSC_controls in 'thirdparty\pascal\rem\uPSC_controls.pas',
  uPSC_classes in 'thirdparty\pascal\rem\uPSC_classes.pas',
  uPSC_comobj in 'thirdparty\pascal\rem\uPSC_comobj.pas',
  uPSC_dateutils in 'thirdparty\pascal\rem\uPSC_dateutils.pas',
  uPSC_DB in 'thirdparty\pascal\rem\uPSC_DB.pas',
  uPSC_extctrls in 'thirdparty\pascal\rem\uPSC_extctrls.pas',
  uPSC_menus in 'thirdparty\pascal\rem\uPSC_menus.pas',
  uPSR_dll in 'thirdparty\pascal\rem\uPSR_dll.pas',
  uPSPreProcessor in 'thirdparty\pascal\rem\uPSPreProcessor.pas',
  uPSR_std in 'thirdparty\pascal\rem\uPSR_std.pas',
  uPSR_stdctrls in 'thirdparty\pascal\rem\uPSR_stdctrls.pas',
  uPSR_forms in 'thirdparty\pascal\rem\uPSR_forms.pas',
  uPSR_graphics in 'thirdparty\pascal\rem\uPSR_graphics.pas',
  uPSR_controls in 'thirdparty\pascal\rem\uPSR_controls.pas',
  uPSR_classes in 'thirdparty\pascal\rem\uPSR_classes.pas',
  uPSR_comobj in 'thirdparty\pascal\rem\uPSR_comobj.pas',
  uPSR_dateutils in 'thirdparty\pascal\rem\uPSR_dateutils.pas',
  uPSR_DB in 'thirdparty\pascal\rem\uPSR_DB.pas',
  uPSR_extctrls in 'thirdparty\pascal\rem\uPSR_extctrls.pas',
  uPSR_menus in 'thirdparty\pascal\rem\uPSR_menus.pas',
  {$ENDIF}
  UndConst, UndImporter;

{$R *.res}
{$R Manifest.res}

procedure SetCustomLibName;
begin
 rudCustomFunc_WriteLn :=lowercase(rudLibName) +'_writeln';
 rudCustomFunc_Write   :=lowercase(rudLibName)   +'_write';
 rudCustomFunc_LogError:=lowercase(rudLibName)+'_logerror';
end;

procedure RedirectIO(b:boolean);
begin
 if b = true then
  SetCustomLibName
 else begin
  rudCustomFunc_WriteLn:=emptystr;
  rudCustomFunc_Write:=emptystr;
  rudCustomFunc_LogError:=emptystr;
 end;
end;

procedure SetCustomModuleName(name:string);
begin
 rudLibName := name; if rudCustomFunc_WriteLn<>emptystr then SetCustomLibName;
end;

{$IFDEF UNDER_PHP}
procedure RegisterPHP(L: plua_State);
begin
 RegisterUndPHPWrapper(L);
 RegisterUndPHPScriptWrapper(L);
end;
{$ENDIF}

type
 TOptionType = (
  opt_modulename,
  opt_usevars,
  opt_useglobals,
  opt_uselocals,
  opt_redirectio
 );
function lua_getoption(L: plua_State):integer; cdecl;
var s:string;
begin
 result:=1;
 s:=lua_tostring(L,2);
 case TOptionType(GetEnumValue(TypeInfo(TOptionType), 'opt_'+lowercase(s))) of
  opt_modulename: lua_pushstring(L,rudLibName);
  opt_usevars: lua_pushboolean(L,RudImportVariables);
  opt_uselocals: lua_pushboolean(L,RudImportLocals);
  opt_useglobals: lua_pushboolean(L,RudImportGlobals);
  opt_redirectio: begin
   if rudCustomFunc_WriteLn<>emptystr then
   lua_pushboolean(L,true) else lua_pushboolean(L,false);
  end;
 else
  result:=0;
 end;
end;
function lua_setoption(L: plua_State):integer; cdecl;
var s:string;
begin
 result:=1;
 s:=lua_tostring(L,2);
 case TOptionType(GetEnumValue(TypeInfo(TOptionType), 'opt_'+lowercase(s))) of
  opt_modulename: SetCustomModuleName(lua_tostring(L,3));
  opt_usevars: RudImportVariables := lua_toboolean(L,3);
  opt_uselocals: RudImportLocals := lua_toboolean(L,3);
  opt_useglobals: RudImportGlobals := lua_toboolean(L,3);
  opt_redirectio: RedirectIO(lua_toboolean(L,3));
 else
  result:=0;
 end;
end;

type
 TScriptType = (
  lang_jscript,
  lang_pascalpage,
  lang_pascalprog,
  lang_pascalscript,
  lang_python,
  lang_perl,
  lang_php,
  lang_ruby,
  lang_vbscript
 );
function lua_getscriptfunc(L: plua_State):integer; cdecl;
var s:string;
begin
 result:=1;
 s:=lua_tostring(L,2);
 case TScriptType(GetEnumValue(TypeInfo(TScriptType), 'lang_'+lowercase(s))) of
  {$IFDEF UNDER_ACTIVESCRIPT}
  lang_jscript: lua_pushcfunction(L,JavaScript_Run);
  lang_vbscript: lua_pushcfunction(L,VBScript_Run);
  lang_perl: lua_pushcfunction(L,PerlScript_Run);
  {$ENDIF}
  {$IFDEF UNDER_PASCAL}
  lang_pascalpage: lua_pushcfunction(L,PascalWebScript_Run);
  lang_pascalscript: lua_pushcfunction(L,PascalScript_Run);
  {$ENDIF}
  {$IFDEF UNDER_PASCAL_CLASSIC}
  lang_pascalprog: lua_pushcfunction(L,PascalClassic_Run);
  {$ENDIF}
  {$IFDEF UNDER_PYTHON}
  lang_python: lua_pushcfunction(L,Python_Run);
  {$ENDIF}
  {$IFDEF UNDER_PHP}
  lang_php: lua_pushcfunction(L,PHP_RunCode);
  {$ENDIF}
  {$IFDEF UNDER_RUBY}
  lang_ruby: lua_pushcfunction(L,Ruby_Eval);
  {$ENDIF}
 else
  result:=0;
 end;
end;

function luaopen_Underscript_Runner(L: plua_State):integer; cdecl;
const
 utils_table : array [1..1] of luaL_reg =
(
 (name:nil;func:nil) // needed by Lua
 );
begin
 lua_newtable(L);
 plua_SetFieldValue(L,'run',@lua_getscriptfunc,nil);
 plua_SetFieldValue(L,'options',@lua_getoption,@lua_setoption);
 plua_SetFieldValue(L,'utils',@utils_table);
 {
 experimental wrappers
 RegisterUndPascalWrapper(L);
 RegisterUndScriptWrapper(L); //ActiveScript
 RegisterPHP(L);
 RegisterUndPythonWrapper(L);
 RegisterUndRubyWrapper(L);  }
 Result := 1;
end;

function luaopen_Underscript(L: plua_State):integer; cdecl;
const
 main_table : array [1..1] of luaL_reg =
(
 (name:nil;func:nil) // needed by Lua
 );
begin
 plua_RegisterLuaTable(L,'_script',@lua_getscriptfunc,nil);
 Result := 0;
end;

Exports
 luaopen_Underscript,
 luaopen_Underscript_Runner;

begin
end.
