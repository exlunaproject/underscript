unit UndImporter;

{
  UnderScript Importer
  Imports Lua variables to the script
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, Lua, pLua, Variants, SysUtils, CatStrings, UndConst;

type
  TUndCompiledScript = record
     originalscript:string;
     constscript:string;
     initscript:string;
     endscript:string;
     completescript:string;
  end;

type
  TUndImporter = class
  private
    fEnableDebug: boolean;
    fEnableImport: boolean;
    fLangDef: TUndLanguageInternal;
    function CanAddKey(lt: integer; n: string): boolean;
    function GetLocalsImport(L: Plua_State;sl: TStringList): TUndCompiledScript;
    function GetFormatedImport(const s: string;v:TUndLuaVariable): string;
    function GetFormatedImports(cs:TUndCompiledScript; v: TUndLuaVariable): TUndCompiledScript;
    function ImportVariables(L: Plua_State): TUndCompiledScript;
  public
    constructor Create(L: Plua_State);
    destructor Destroy; override;
    function GetScript(L: Plua_State; Script: string;
      const lang:TUndLanguageInternal): TUndCompiledScript;
    procedure Debug(s: string);
    // properties
    property EnableDebug:boolean read fEnableDebug write fEnableDebug;
    property EnableImport:boolean read fEnableImport write fEnableImport;
  end;

implementation

function TUndImporter.CanAddKey(lt: integer; n: string): boolean;
begin
  result := false;
  case lt of
    LUA_TSTRING:
      result := true;
    LUA_TBOOLEAN:
      result := true;
    LUA_TNUMBER:
      result := true;
    LUA_TNIL:
      if (uoNoNilImport in fLangDef.Options) = false then
        result := true;
  end;
  // Do not import variables starting with underscore
  if beginswith(n, '_') then
    result := false;
  // if n='_VERSION' then result:=false;
  // if n='AST_COMPILE_ERROR_NUMBER' then result:=false;
end;

function TUndImporter.GetFormatedImport(const s: string;v:TUndLuaVariable): string;
begin
  result := s;
  result := replacestr(result, '%l', rudLibName);
  result := replacestr(result, '%k', v.name);
  result := replacestr(result, '%t', v.LuaTypeStr);
  result := replacestr(result, '%c', titlecase(v.LuaTypeStr));
end;

function TUndImporter.GetFormatedImports(cs:TUndCompiledScript; v: TUndLuaVariable): TUndCompiledScript;
begin
 result := cs;
 result.constscript := result.constscript + GetFormatedImport(fLangDef.FuncConstFormat, v);
 result.initscript := result.initscript + GetFormatedImport(fLangDef.FuncReadFormat, v);
 result.endscript := result.endscript + GetFormatedImport(fLangDef.FuncWriteFormat, v);
end;

function TUndImporter.GetLocalsImport(L: Plua_State;sl: TStringList): TUndCompiledScript;
var
  v: TUndLuaVariable;
  vname:PAnsiChar;
  ar: plua_Debug;
  i: integer;
  found: boolean;
  procedure getloc(id:integer);
  begin
    debug('get loc...'+inttostr(id));
    lua_pop(L, 1);
    vname := lua_getlocal(L, @ar, i);
    v.name := vname;
    v.LuaType := lua_type(L, -1);
    v.LuaTypeStr := plua_TypeToKeyword(v.LuaType);
    if vname <> nil then
    begin
      Debug('found local var:' + v.name);
      if sl.IndexOf(v.name) = -1 then
      begin
        if CanAddKey(v.LuaType, v.name) then
          result := GetFormatedImports(result, v);
        sl.Add(v.name);
      end;
    end;
    inc(i);
  end;

begin
  found := false;
  // dbg('getting locals for'+localformatstr);
  if lua_getstack(L, 1, @ar) <> 1 then
    Exit;
  // dbg('getting... '+LocalFormatStr);
  i := 1;
  getloc(1);
  // if varname = nil then dbg('first var is nil');
  while (vname <> nil) and (found = false) do
    getloc(2);
  // dbg('getlocimp result:'+result);
end;

function TUndImporter.ImportVariables(L: Plua_State): TUndCompiledScript;
var
  sl: TStringList;
  v: TUndLuaVariable;// global-related
  ar: plua_Debug; // local-related
  Index, MaxTable, SubTableMax: integer;
  r: TUndCompiledScript;
begin
  index := -1;
  MaxTable := 0;
  SubTableMax := 1; // -1, 0, 1, works with these params. do not change!
  sl := TStringList.Create;
  // global variables...
  if RudImportGlobals = true then
  begin
    Debug('importing globals...');
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    Index := plua_absindex(L, Index);
    lua_pushnil(L);
    while (lua_next(L, Index) <> 0) do
    begin
      v.LuaType := lua_type(L, -1);
      v.LuaTypeStr := plua_TypeToKeyword(v.LuaType);
      v.name := plua_dequote(plua_LuaStackToStr(L, -2, MaxTable, SubTableMax));
      //v.name := dequote(plua_LuaStackToStr(L, -2, MaxTable, SubTableMax));
      // Value := Dequote(LuaStackToStr(L, -1, MaxTable, SubTableMax));
      if sl.IndexOf(v.name) = -1 then
      begin
        if CanAddKey(v.LuaType, v.name) then
          result := GetFormatedImports(result, v);
        sl.Add(v.name);
      end;
      // debug('found global var:'+key);
      lua_pop(L, 1);
    end;
    lua_pop(L, 1);
  end;

  // local variables...
  if RudImportLocals = true then
  begin
    if (lua_getstack(L, -2, @ar) = 1) then
    begin
      Debug('importing locals...');
      r := GetLocalsImport(L, sl);
       if RudImportGlobals = true then begin
        // merges globals and locals import...
         result.constscript := result.constscript + r.constscript;
         result.initscript := result.initscript + r.initscript;
         result.endscript := result.endscript + r.endscript;
       end else begin
         result.constscript := r.constscript;
         result.initscript := r.initscript;
         result.endscript := r.endscript;
       end;
    end;
  end;

  sl.free;
end;

procedure TUndImporter.Debug(s: string);
begin
  if fEnableDebug then
    writeln('[dbg] ' + s);
end;

function TUndImporter.GetScript(L: Plua_State; script: string;
  const lang:TUndLanguageInternal): TUndCompiledScript;
begin
  fLangDef := Lang;
  if fEnableImport = false then
    Exit;
  if RudImportVariables = false then
    Exit;
  Debug('Getting script...');
  result := ImportVariables(L);
  result.originalscript := script;
  Debug('Const:' + result.constscript);
  Debug('Init:' + result.initscript);
  Debug('End:' + result.endscript);
  result.completescript := result.InitScript + script + result.EndScript;
  Debug('Done getting script.');
end;

constructor TUndImporter.Create(L: Plua_State);
begin
  inherited Create;
  fEnableImport := true;
  fEnableDebug := false;
  fLangDef.FuncConstFormat := emptystr;
end;

destructor TUndImporter.Destroy;
begin
  inherited;
end;

end.
