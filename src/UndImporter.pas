unit UndImporter; 

{
 UnderScript Importer 
 Imports Lua variables to the script
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
 Classes, Lua, pLua, Variants, SysUtils, CatStrings, LuaUtils,
 UndConst;

type
  TUndImporter = class
  public
    EnableDebug:boolean;
    EnableImport:boolean;
    LuaState:Plua_State;
    InitScript:string;
    EndScript:string;
    FuncReadFormat:string;
    FuncWriteFormat:string;
    constructor Create(L:Plua_State);
    destructor Destroy; override;
    function GetScript(script:string):string;
    procedure Debug(s:string);
  end;

var
 FImporter:TUndImporter;

 function GetLocalsImport(L:Plua_State;LocalFormatStr:string;sl:TStringList):string;
 function ImportVariables(L:Plua_State; FormatStr:string;ImportGlobals:boolean;ImportLocals:boolean):string;
 procedure Dbg(s:string);

implementation

procedure Dbg(s:string);
begin
 if fimporter.enabledebug then writeln('[dbg] '+s);
end;

function CanAddKey(lt:integer;n:string):boolean;
begin
  result:=false;
  if lt=LUA_TSTRING then result:=true;
  if lt=LUA_TBOOLEAN then result:=true;
  if lt=LUA_TNUMBER then result:=true;
  if lt=LUA_TNIL then result:=true;
  if beginswith(n,'_') then result:=false;
  //if n='_VERSION' then result:=false;
  //if n='AST_COMPILE_ERROR_NUMBER' then result:=false;
end;

function LuaTypeToStr(LuaType:integer):string;
begin
  result:=emptystr;
  case LuaType of
  LUA_TSTRING: result:='string';
  LUA_TBOOLEAN: result:='boolean';
  LUA_TNUMBER: result:='integer';
  LUA_TNIL: result:='nil';
  end;
end;

function GetFormatedImport(formatstr,varname,luatype:string):string;
begin
 result:=replacestr(formatstr,'%k',VarName);
 result:=replacestr(result,'%t',LuaType);
end;

function GetLocalsImport(L:Plua_State;LocalFormatStr:string;sl:TStringList):string; // By Syhunt
var ar:plua_Debug;
VarName:PChar;
VarValue:Variant;
i,LuaType:integer;
LuaTypeStr:string;
found:boolean;
 procedure getloc;
 begin
    //dbg('get loc...');
    lua_pop(L,1);
    VarName:=lua_getlocal(L,@ar,i);
    LuaType:=lua_type(L, -1);
    LuaTypeStr:=LuaTypeToStr(luatype);
    //VarValue:=plua_tovariant(L, -1);
    if VarName<>nil then begin
     dbg('found local var:'+varname);
     if sl.IndexOf(VarName)=-1 then begin
      if canaddkey(luatype,VarName) then begin
      result:=result+GetFormatedImport(localformatstr,varname,LuaTypeStr);
      //result:=result+replacestr(localformatstr,'%k',VarName);
      end;
      sl.Add(VarName);
     end;
    end;
    inc(i);
 end;
begin
  found:=false;
  //dbg('getting locals for'+localformatstr);
  if lua_getstack(L,1,@ar)<>1 then Exit;
  //dbg('getting... '+LocalFormatStr);
  i:=1; getloc;
  //if varname = nil then dbg('first var is nil');
  while (VarName<>nil) and (found=false) do getloc;
  //dbg('getlocimp result:'+result);
end;

function ImportVariables(L: Plua_State; FormatStr:string;ImportGlobals:boolean;ImportLocals:boolean):string;
var Key, Value: string; sl:tstringlist; LuaType:integer; LuaTypeStr:string; // global-related
var AR: Plua_Debug; // local-related
var Index: Integer; MaxTable: Integer; SubTableMax: Integer;
var r:string;
begin
  index:=-1; maxtable:=0;subtablemax:=1; // -1, 0, 1, works with these params. do not change!
  sl:=tstringlist.create;
   // global variables...
  if ImportGlobals = true then begin
   dbg('importing globals...');
   lua_pushvalue(L, LUA_GLOBALSINDEX);
   Index := LuaAbsIndex(L, Index);
   lua_pushnil(L);
   while (lua_next(L, Index) <> 0) do begin
     LuaType:=lua_type(L, -1);
     LuaTypeStr:=LuaTypeToStr(luatype);
     Key := Dequote(LuaStackToStr(L, -2, MaxTable, SubTableMax));
     //Value := Dequote(LuaStackToStr(L, -1, MaxTable, SubTableMax));
     if sl.IndexOf(Key)=-1 then begin
      if canaddkey(luatype,key) then begin
      //dbg('added:'+key);
      result:=result+GetFormatedImport(formatstr,key,luatypestr);
      //result:=result+replacestr(formatstr,'%k',key);
      end;
      sl.Add(Key);
     end;
     //dbg('found global var:'+key);
     lua_pop(L, 1);
   end;
   lua_pop(L, 1);
  end;

  //local variables... is localformatstr is empty, only globals are imported
  if ImportLocals = true then begin
   if (lua_getstack(L, -2, @AR) =1 ) then begin
     dbg('importing locals...');
    r:=GetLocalsImport(L,formatstr,sl);
    if r<>emptystr then begin
     if importglobals then result:=result+r else result:=r;
    end;
   end;
  end;

  sl.free;
end;

procedure TUndImporter.Debug(s:string);
begin
 if enabledebug then writeln('[dbg] '+s);
end;

function TUndImporter.GetScript(script:string):string;
begin
  result:=script;
  if EnableImport=false then exit;
  if RudImportVariables=false then exit;
  debug('Getting script...');
  initscript:=ImportVariables(LuaState,FuncReadFormat,RudImportGlobals,RudImportLocals);
  debug('Init:'+initscript);
  endscript:=ImportVariables(LuaState,FuncWriteFormat,RudImportGlobals,RudImportLocals);
  debug('End:'+endscript);
  result:=initscript+script+endscript;
  debug('Done getting script.');
end;

constructor TUndImporter.Create(L:Plua_State);
begin
  inherited Create;
  EnableImport:=true;
  EnableDebug:=false;
  LuaState:=L;
  FImporter := self;
end;

destructor TUndImporter.Destroy;
begin
  inherited;
end;

end.
 