unit UndConst;

{
  UnderScript Constants
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  SysUtils, Lua, CatStrings;

const
  // Important: this constant must be have the first letter uppercase because of Ruby compatibility
  cUnd = 'Underscript';
  cUnderSetPrefix='_underscript_set:';
  cJSHexDecodeFunc = 'function hex2str(h) { var s = ""; for (var i = 0; i < h.length; i += 2) s += String.fromCharCode(parseInt(h.substr(i, 2), 16)); return s; }';
  cJSHexEncodeFunc = 'function str2hex(s) { var h = ""; for(var i=0;i<s.length;i++) { h += ""+s.charCodeAt(i).toString(16); } return h; }';
  cJsHexEncodeDecodeFuncs = cJSHexDecodeFunc + cJSHexEncodeFunc;

var
  rudLibName: string = cUnd;
  // this can be changed during runtime via .options table
  rudImportVariables: boolean = true;
  rudImportGlobals: boolean = true;
  rudImportLocals: boolean = true;
  rudCustomFunc_WriteLn: string = '';
  rudCustomFunc_Write: string = '';
  rudCustomFunc_LogError: string = '';

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
   StringEncodeFormat: TUndStringEncodeFormat;
  end;

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
 );

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
   StringEncodeFormat: usfHex;
 );

procedure Und_CustomWrite(L: plua_State; s: String; customfunc: String = '');
procedure Und_CustomWriteLn(L: plua_State; s: String; customfunc: String = '');
procedure Und_LogError(L: plua_State; line: integer; msg: String);
procedure SetCustomLibName;
procedure SetCustomModuleName(name:string);
procedure RedirectIO(b:boolean);


implementation

procedure Und_LogError(L: plua_State; line: integer; msg: String);
begin
  if rudCustomFunc_LogError = emptystr then
    exit;
  lua_getglobal(L, PAnsiChar(rudCustomFunc_LogError));
  lua_pushinteger(L, line);
  lua_pushstring(L, msg);
  lua_pcall(L, 2, 0, 0)
end;

procedure Und_CustomWrite(L: plua_State; s: String; customfunc: String = '');
begin
  if customfunc <> emptystr then
  begin
    lua_getglobal(L, PAnsiChar(customfunc));
    lua_pushstring(L, s);
    lua_pcall(L, 1, 0, 0);
  end
  else
    system.Write(s);
end;

procedure Und_CustomWriteLn(L: plua_State; s: String; customfunc: String = '');
begin
  if customfunc <> emptystr then
  begin
    lua_getglobal(L, PAnsiChar(customfunc));
    lua_pushstring(L, s);
    lua_pcall(L, 1, 0, 0);
  end
  else
    system.WriteLn(s);
end;

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

// ------------------------------------------------------------------------//
end.
