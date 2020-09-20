unit UndConst;

{
  UnderScript Constants
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  SysUtils, Lua, pLua, pLuaTable, CatStrings, CatUtils;

type
 TScriptType = (
  lang_jsnode,
  lang_jsnodestrict,
  lang_jscript,
  lang_jsv8,
  lang_lua,
  lang_luain,
  lang_luajit,
  lang_luav51,
  lang_luav52,
  lang_luav53,
  lang_luav54,
  lang_pascalpage,
  lang_pascalprog,
  lang_pascalscript,
  lang_python,
  lang_pythonenv,
  lang_perl,
  lang_perlactive,
  lang_php,
  lang_ruby,
  lang_tcl,
  lang_vbscript
 );

const
  // Important: this constant must be have the first letter uppercase because of Ruby compatibility
  cUnd = 'Underscript';
  cUnderSetPrefix='_underscript_set:';
  cLuaHexDecodeFunc = 'function string.fromhex(s) return (s:gsub("..", function (cc) return string.char(tonumber(cc, 16)) end)) end;';
  cLuaHexEncodeFunc = 'function string.tohex(s) return (s:gsub(".", function (c) return string.format("%02X", string.byte(c)) end)) end;';
  cLuaHexEncodeDecodeFuncs = cLuaHexDecodeFunc + cLuaHexEncodeFunc;
  cJSHexDecodeFunc = 'function hex2str(h) { var s = ""; for (var i = 0; i < h.length; i += 2) s += String.fromCharCode(parseInt(h.substr(i, 2), 16)); return s; }';
  cJSHexEncodeFunc = 'function str2hex(s) { var h = ""; for(var i=0;i<s.length;i++) { h += ""+s.charCodeAt(i).toString(16); } return h; }';
  cJsHexEncodeDecodeFuncs = cJSHexDecodeFunc + cJSHexEncodeFunc;

var
  rudLibName: string = cUnd;
  // this can be changed during runtime via .options table
  rudImportVariables: boolean = true;
  rudImportGlobals: boolean = false;
  rudImportLocals: boolean = true;
  rudCustomFunc_WriteLn: string = '';
  rudCustomFunc_Write: string = '';
  rudCustomFunc_LogError: string = '';

type
  TUndScriptResult = record
   success:boolean;
   errormessage:string;
   expressionresult:string;
  end;

type
  TUndLuaVariable = record
    name:string;
    value:string;
    luatype:integer;
    luatypestr:string;
  end;

type
  TUndStringEncodeFormat = (usfBase64, usfHex);

type
  TUndLanguageExternal = record
   Command: string;
   FileExt: string;
   StringFormat: string;
   VarReadFormat: string;
   FuncReadFormat: string;
   FuncWriteFormat: string;
   StringEncoder: string;
   StringDecoder: string;
   FormatScript: string;
   NilKeyword: string;
   StringEncodeFormat: TUndStringEncodeFormat;
  end;

const
 langdef_Lua: TUndLanguageExternal = (
   Command: '%p\lua5.1.exe';
   FileExt: '.lua';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: '%k = %v;';
   FuncWriteFormat: crlf+'print("\n%pt=%t,n=%k,v="..%g.."\n");';
   StringEncoder: 'string.tohex(%s)';
   StringDecoder: 'string.fromhex(%s)';
   FormatScript: cLuaHexEncodeDecodeFuncs+'%s';
   NilKeyword: 'nil';
   StringEncodeFormat: usfHex;
 );

const
 langdef_PHP: TUndLanguageExternal = (
   Command: '%u\php\php.exe';
   FileExt: '.php';
   StringFormat: '"%s"';
   VarReadFormat: '$%k';
   FuncReadFormat: '$%k = %v;';
   FuncWriteFormat: crlf+'echo("\n%pt=%t,n=%k,v=".%g."\n");';
   StringEncoder: 'base64_encode(%s)';
   StringDecoder: 'base64_decode(%s)';
   FormatScript: '<?php %s ?>';
   NilKeyword: 'NULL';
 );

const
 langdef_Ruby: TUndLanguageExternal = (
   Command: '%u\ruby\ruby.exe';
   FileExt: '.rb';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: '%k = %v;';
   FuncWriteFormat: crlf+'puts "\n%pt=%t,n=%k,v="+%g+"\n";';
   StringEncoder: 'Base64.encode64(%s)';
   StringDecoder: 'Base64.decode64(%s)';
   FormatScript: 'require "base64"; %s';
   NilKeyword: '""';
 );
 // FIXME: nil in NilKeyword generating conversion error for Ruby

const
 langdef_Perl: TUndLanguageExternal = (
   Command: '%u\perl\perl.exe';
   FileExt: '.pl';
   StringFormat: '"%s"';
   VarReadFormat: '$%k';
   FuncReadFormat: '$%k = %v;';
   FuncWriteFormat: crlf+'print("\n%pt=%t,n=%k,v=".%g."\n");';
   StringEncoder: 'unpack("H*",%s)';
   StringDecoder: 'pack("H*",%s)';
   FormatScript: '%s';
   NilKeyword: 'undef';
   StringEncodeFormat: usfHex;
 );

const
 langdef_Python: TUndLanguageExternal = (
   Command: '%u\python\python.exe';
   FileExt: '.py';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: '%k = %v;';
   FuncWriteFormat: crlf+'print("\n%pt=%t,n=%k,v="+%g+"\n");';
   StringEncoder: 'str(base64.b64encode(%s.encode("utf-8")),"utf-8")';
   StringDecoder: 'str(base64.b64decode(%s),"utf-8")';
   FormatScript: 'import base64; %s';
   NilKeyword: 'None';
 );

const
 langdef_NodeJS: TUndLanguageExternal = (
   Command: '%u\nodejs\node.exe';
   FileExt: '.js';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: '%k = %v;';
   FuncWriteFormat: crlf+'console.log("\n%pt=%t,n=%k,v="+%g+"\n");';
   StringEncoder: '(new Buffer(%s).toString("base64"))';
   StringDecoder: '(new Buffer(%s, "base64").toString("ascii"))';
   FormatScript: '%s';
   NilKeyword: 'null';
 );
// Use new Buffer() instead of Buffer.from() so it can be compatible with node
// versions older than v6

const
 langdef_NodeJS_Strict: TUndLanguageExternal = (
   Command: '%u\nodejs\node.exe';
   FileExt: '.js';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: 'let %k = %v;';
   FuncWriteFormat: crlf+'console.log("\n%pt=%t,n=%k,v="+%g+"\n");';
   StringEncoder: '(new Buffer(%s).toString("base64"))';
   StringDecoder: '(new Buffer(%s, "base64").toString("ascii"))';
   FormatScript: '''use strict''; %s';
   NilKeyword: 'null';
 );

const
 langdef_V8JS: TUndLanguageExternal = (
   Command: '%u\v8js\d8.exe';
   FileExt: '.js';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: '%k = %v;';
   FuncWriteFormat: crlf+'print("\n%pt=%t,n=%k,v="+%g+"\n");';
   StringEncoder: 'str2hex(%s)';
   StringDecoder: 'hex2str(%s)';
   FormatScript: cJsHexEncodeDecodeFuncs+' %s';
   NilKeyword: 'null';
   StringEncodeFormat: usfHex;
 );

const
 langdef_TCL: TUndLanguageExternal = (
   Command: '%u\tcl\tclsh.exe';
   FileExt: '.tcl';
   StringFormat: '"%s"';
   VarReadFormat: '$%k';
   FuncReadFormat: 'set %k %v;';
   FuncWriteFormat: crlf+'puts [join [list "%pt=%t,n=%k,v=" %g] ""];';
   StringEncoder: '[binary encode hex %s]';
   StringDecoder: '[binary decode hex %s]';
   FormatScript: '%s';
   NilKeyword: '""';
   StringEncodeFormat: usfHex;
 );

procedure Und_CustomWrite(L: plua_State; s: String; customfunc: String = '');
procedure Und_CustomWriteLn(L: plua_State; s: String; customfunc: String = '');
procedure Und_LogError(L: plua_State; line: integer; msg: String);
procedure Und_PushScriptResult(L: plua_State; res:TUndScriptResult);
procedure RedirectIO(const b:boolean);
procedure SetCustomLibName;
procedure SetCustomModuleName(name:string);


implementation

procedure Und_PushScriptResult(L: plua_State; res:TUndScriptResult);
begin
 lua_newtable(L);
 plua_SetFieldValue(L, 'success', res.success);
 plua_SetFieldValue(L, 'errormsg', res.ErrorMessage);
 plua_SetFieldValue(L, 'expresult', res.expressionresult);
end;

procedure Und_LogError(L: plua_State; line: integer; msg: String);
begin
  if rudCustomFunc_LogError = emptystr then
    exit;
  if plua_functionexists(L, rudCustomFunc_LogError) = true then begin
    lua_getglobal(L, PAnsiChar(AnsiString(rudCustomFunc_LogError)));
    plua_pushintnumber(L, line);
    lua_pushstring(L, msg);
    lua_pcall(L, 2, 0, 0)
  end;
end;

procedure Und_CustomWrite(L: plua_State; s: String; customfunc: String = '');
begin
  if customfunc <> emptystr then
  begin
    if plua_functionexists(L, customfunc) = true then begin
      lua_getglobal(L, PAnsiChar(AnsiString(customfunc)));
      lua_pushstring(L, s);
      lua_pcall(L, 1, 0, 0);
    end;
  end
  else
    system.Write(s);
end;

procedure Und_CustomWriteLn(L: plua_State; s: String; customfunc: String = '');
begin
  if customfunc <> emptystr then
  begin
    OutDebug('checking existance of:'+customfunc);
    if plua_functionexists(L, customfunc) = true then begin
      OutDebug('writeln to func:'+s);
      lua_getglobal(L, PAnsiChar(AnsiString(customfunc)));
      lua_pushstring(L, s);
      lua_pcall(L, 1, 0, 0);
    end;
  end
  else
    system.WriteLn(s);
end;

procedure RedirectIO(const b:boolean);
begin
 if b = true then
  SetCustomLibName
 else begin
  rudCustomFunc_WriteLn:=emptystr;
  rudCustomFunc_Write:=emptystr;
  rudCustomFunc_LogError:=emptystr;
 end;
end;

procedure SetCustomLibName;
begin
 rudCustomFunc_WriteLn :=lowercase(rudLibName) +'_writeln';
 rudCustomFunc_Write   :=lowercase(rudLibName)   +'_write';
 rudCustomFunc_LogError:=lowercase(rudLibName)+'_logerror';
end;

procedure SetCustomModuleName(name:string);
begin
 rudLibName := name;
 if rudCustomFunc_WriteLn<>emptystr then
   SetCustomLibName;
end;

// ------------------------------------------------------------------------//
end.
