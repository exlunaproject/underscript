unit UndConst;

{
  UnderScript Constants
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  SysUtils, CatStrings, CatUtils;

const
  ULANG_JAVA = 1;
  ULANG_JAVABSC = 2;
  ULANG_JSCORE = 3;
  ULANG_JSNODE = 4;
  ULANG_JSNODESTRICT = 5;
  ULANG_JSCRIPT = 6;
  ULANG_JSQUICK = 7;
  ULANG_JSSPIDER = 8;
  ULANG_JSV8 = 9;
  ULANG_LUAIN = 11;
  ULANG_LUAJIT = 12;
  ULANG_LUAV51 = 13;
  ULANG_LUAV52 = 14;
  ULANG_LUAV53 = 15;
  ULANG_LUAV54 = 16;
  ULANG_PASCALPAGE = 17;
  ULANG_PASCALPROG = 18;
  ULANG_PASCALSCRIPT = 19;
  ULANG_PYTHON = 20;
  ULANG_PYTHONENV = 21;
  ULANG_PERL = 22;
  ULANG_PERLACTIVE = 23;
  ULANG_PHP = 24;
  ULANG_RUBY = 25;
  ULANG_TCL = 26;
  ULANG_TISCRIPT = 27;
  ULANG_VBSCRIPT = 28;

type
 TScriptType = (
  lang_java,
  lang_javabsc,
  lang_jscore,
  lang_jsnode,
  lang_jsnodestrict,
  lang_jscript,
  lang_jsquick,
  lang_jsspider,
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
  lang_tiscript,
  lang_vbscript
 );

const
  // Important: this constant must be have the first letter uppercase because of Ruby compatibility
  cUnd = 'UConsole';
  cUndConsoleLibName = 'uconsole';
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
  rudRedirectIO: boolean = false;

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
  TUndOptions = (uoTimeout);
  TUndOptionSet = set of TUndOptions;

type
  TUndLanguageExternal = record
   Command: string;
   Params: string;
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
   Options: TUndOptionSet;
  end;

const
 langdef_Lua: TUndLanguageExternal = (
   Command: '%p\lua5.1.exe';
   FileExt: '.lua';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: '%k = %v;';
   // Note: do not remove the space before print
   FuncWriteFormat: ' print("%pt=%t,n=%k,v="..%g);';
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
   FuncWriteFormat: ';echo("\n%pt=%t,n=%k,v=".%g);';
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
   FuncWriteFormat: ';puts "%pt=%t,n=%k,v="+%g;';
   StringEncoder: 'Base64.strict_encode64(%s)';
   StringDecoder: 'Base64.strict_decode64(%s)';
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
   FuncWriteFormat: ';print("\n%pt=%t,n=%k,v=".%g);';
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
   FuncWriteFormat: crlf+'print("%pt=%t,n=%k,v="+%g);';
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
   FuncWriteFormat: ';console.log("%pt=%t,n=%k,v="+%g);';
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
   FuncWriteFormat: ';console.log("%pt=%t,n=%k,v="+%g);';
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
   FuncWriteFormat: ';print("\n%pt=%t,n=%k,v="+%g);';
   StringEncoder: 'str2hex(%s)';
   StringDecoder: 'hex2str(%s)';
   FormatScript: cJsHexEncodeDecodeFuncs+' %s';
   NilKeyword: 'null';
   StringEncodeFormat: usfHex;
 );

const
 langdef_QuickJS: TUndLanguageExternal = (
   Command: '%u\multipreter\multipreter.exe';
   Params: 'quickjs %f';
   FileExt: '.js';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: 'var %k = %v;';
   FuncWriteFormat: ';console.log("\n%pt=%t,n=%k,v="+%g);';
   StringEncoder: 'str2hex(%s)';
   StringDecoder: 'hex2str(%s)';
   FormatScript: cJsHexEncodeDecodeFuncs+' %s';
   NilKeyword: 'null';
   StringEncodeFormat: usfHex;
 );

const
 langdef_TIScript: TUndLanguageExternal = (
   Command: '%u\tiscript\tiscript.exe';
   FileExt: '.tis';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: 'var %k = %v;';
   FuncWriteFormat: ';stdout.println("\n%pt=%t,n=%k,v="+%g);';
   StringEncoder: 'Bytes.fromString(%s,"utf-8").toString("base64")';
   StringDecoder: 'Bytes.fromString(%s, "base64").toString("utf-8")';
   FormatScript: '%s';
   NilKeyword: 'null';
   Options: [uoTimeout];
 );

const
 langdef_TCL: TUndLanguageExternal = (
   Command: '%u\tcl\tclsh.exe';
   FileExt: '.tcl';
   StringFormat: '"%s"';
   VarReadFormat: '$%k';
   FuncReadFormat: 'set %k %v;';
   FuncWriteFormat: ';puts [join [list "%pt=%t,n=%k,v=" %g] ""];';
   StringEncoder: '[binary encode hex %s]';
   StringDecoder: '[binary decode hex %s]';
   FormatScript: '%s';
   NilKeyword: '""';
   StringEncodeFormat: usfHex;
 );

const
 langdef_Java: TUndLanguageExternal = (
   Command: '%u\beanshell\bsh.exe';
   FileExt: '.java';
   StringFormat: '"%s"';
   VarReadFormat: '%k';
   FuncReadFormat: '%k = %v;';
   FuncWriteFormat: ';System.out.print("\n%pt=%t,n=%k,v="+%g);';
   StringEncoder: 'Base64.getEncoder().encodeToString(%s.getBytes())';
   StringDecoder: '(new String(Base64.getDecoder().decode(%s)))';
   FormatScript: '%s';
   NilKeyword: 'null';
 );

procedure uConsoleWriteError(line: integer; msg: String);
procedure SetCustomModuleName(name:string);


implementation

procedure uConsoleWriteError(line: integer; msg: String);
begin
  system.WriteLn('--('+inttostr(line)+'): '+msg);
end;

procedure SetCustomModuleName(name:string);
begin
 rudLibName := TitleCase(name);
end;

// ------------------------------------------------------------------------//
end.
