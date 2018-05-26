unit UndConst;

{
 UnderScript Constants
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
 Lua;

const
 // Important: this constant must be have the first letter uppercase because of Ruby compatibility
 cUnd='Underscript';

var
 rudLibName:string=cUnd; // this can be changed during runtime via .options table
 rudImportVariables:boolean=true;
 rudImportGlobals:boolean=true;
 rudImportLocals:boolean=true;
 rudCustomFunc_WriteLn:string='';
 rudCustomFunc_Write:string='';
 rudCustomFunc_LogError:string='';

procedure Und_LogError(L: plua_State;line:integer;msg:String);
procedure Und_CustomWrite(L: plua_State;s:String;customfunc:String='');
procedure Und_CustomWriteLn(L: plua_State;s:String;customfunc:String='');

implementation

procedure Und_LogError(L: plua_State;line:integer;msg:String);
begin
 if rudCustomFunc_LogError='' then exit;
 lua_getglobal(L,PChar(rudCustomFunc_LogError));
 lua_pushinteger(L,line);
 lua_pushstring(L,pchar(msg));
 lua_pcall(L, 2, 0, 0)
end;

procedure Und_CustomWrite(L: plua_State;s:String;customfunc:String='');
begin
 if customfunc<>'' then begin
 lua_getglobal(L,PChar(customfunc)); 
 lua_pushstring(L,pchar(s));
 lua_pcall(L, 1, 0, 0);
 end else system.Write(s);
end;

procedure Und_CustomWriteLn(L: plua_State;s:String;customfunc:String='');
begin
 if customfunc<>'' then begin
 lua_getglobal(L,PChar(customfunc));
 lua_pushstring(L,pchar(s));
 lua_pcall(L, 1, 0, 0);
 end else system.WriteLn(s);
end;

//------------------------------------------------------------------------//
end.