unit UndImporterExt;

{
  UnderScript External Importer
  Imports Lua variables to the script
  Copyright (c) 2013-2020 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

{$I Catarinka.inc}

uses
  Classes, Lua, pLua, Variants, SysUtils, CatStrings, CatCSCommand, CatFiles,
  CatStringLoop, CatUtils, UndConst, UndConsole, CatLogger;

type
  TUndCompiledScript = record
     originalscript:string;
     constscript:string;
     initscript:string;
     endscript:string;
     completescript:string;
  end;

type
  TUndExternal = class
  private
    fLanguage: TUndLanguageExternal;
    fEnableDebug: boolean;
    fEnableImport: boolean;
    fSilent: boolean;
    fLuaState: Plua_State;
    function CanAddKey(lt: integer; n: string): boolean;
    procedure Debug(s: string);
    procedure HandleOutput(L: Plua_State; const s: string);
    procedure HandleOutputAlt(const s: string);
    function ImportVariables(L: Plua_State): TUndCompiledScript;
    function GetFormatedImports(cs:TUndCompiledScript; v: TUndLuaVariable):
      TUndCompiledScript;
    function GetFormatedImport(const s: string;v:TUndLuaVariable): string;
    function GetLocalsImport(L: Plua_State; sl: TStringList): TUndCompiledScript;
    function StringEncode(const s:string):string;
    function StringDecode(const s:string):string;
  public
    constructor Create(L: Plua_State; Lang:TUndLanguageExternal);
    destructor Destroy; override;
    function GetScript(L: Plua_State; script: string): TUndCompiledScript;
    function RunScript(L: Plua_State; script: string): integer;
    procedure SetTag(const i:integer);
  end;

function RunExternalScript(L: Plua_State; S:string; Lang:TUndLanguageExternal):integer;

implementation

function ShortenTypeNames(const t:string):string;
begin
   result := t;
   if result = 'boolean' then
     result := 'bool';
   if result = 'integer' then
     result := 'int';
end;

function RunExternalScript(L: Plua_State; S:string; Lang:TUndLanguageExternal):integer;
var
  imp:TUndExternal;
  tag:integer;
begin
  tag := lua_tointeger(L, lua_upvalueindex(1));
  imp := TUndExternal.Create(L, Lang);
  imp.SetTag(tag);
  result := imp.RunScript(L, s);
  imp.Free;
end;

procedure TUndExternal.SetTag(const i:integer);
begin
  if i = cUndTag_Quiet then
    fSilent := true;
end;

function TUndExternal.CanAddKey(lt: integer; n: string): boolean;
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
      if (uoNoNilImport in fLanguage.Options) = false then
        result := true;
  end;
  // Do not import variables starting with underscore
  if beginswith(n, '_') then
    result := false;
  // if n='_VERSION' then result:=false;
  // if n='AST_COMPILE_ERROR_NUMBER' then result:=false;
end;

function TUndExternal.StringEncode(const s:string):string;
begin
  result := trim(s);
  case fLanguage.StringEncodeFormat of
    usfBase64: result := base64encode(result);
    usfHex: result := strtohex(result);
  end;
end;

function TUndExternal.StringDecode(const s:string):string;
begin
  result := trim(s);
  case fLanguage.StringEncodeFormat of
    usfBase64: result := base64decode(result);
    usfHex: result := hextostr(result);
  end;
end;

function TUndExternal.GetFormatedImport(const s: string;v:TUndLuaVariable): string;
var
 getter, typestr:string;
begin
  typestr := v.luatypestr;
  if uoShortTypeName in fLanguage.Options then
   typestr := ShortenTypeNames(typestr);
  getter := fLanguage.VarReadFormat;
  getter := replacestr(getter, '%k', v.name);
  result := s;
  result := replacestr(result, '%p', cUnderSetPrefix);
  result := replacestr(result, '%k', v.name);
  result := replacestr(result, '%t', typestr);
  case v.luatype of
  LUA_TNIL: v.value := fLanguage.NilKeyword;
  LUA_TSTRING: begin
    v.value := replacestr(fLanguage.StringFormat,'%s',stringencode(v.value));
    getter := replacestr(fLanguage.StringEncoder, '%s', getter);
    v.value := replacestr(fLanguage.StringDecoder, '%s', v.value);
   end;
  end;
  result := replacestr(result, '%g', getter);
  result := replacestr(result, '%v', v.value);
end;

function TUndExternal.GetFormatedImports(cs:TUndCompiledScript; v: TUndLuaVariable): TUndCompiledScript;
begin
  result := cs;
  result.constscript := result.constscript + GetFormatedImport(fLanguage.FuncConstFormat, v);
  result.initscript := result.initscript + GetFormatedImport(fLanguage.FuncReadFormat, v);
  result.endscript := result.endscript + GetFormatedImport(fLanguage.FuncWriteFormat, v);
end;

function TUndExternal.GetLocalsImport(L: Plua_State; sl: TStringList): TUndCompiledScript;
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
    v.name := string(vname);
    v.LuaType := lua_type(L, -1);
    v.LuaTypeStr := plua_typetokeyword(v.LuaType);
    // VarValue:=plua_tovariant(L, -1);
    if vname <> nil then
    begin
      Debug('found local var:' + v.name);
      if sl.IndexOf(v.name) = -1 then
      begin
        if CanAddKey(v.LuaType, v.name) then
        begin
          v.value := plua_AnyToString(L, -1);
          result := GetFormatedImports(result, v);
        end;
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
  // if v.name = nil then dbg('first var is nil');
  while (vname <> nil) and (found = false) do
    getloc(2);
   //debug('getlocimp result:'+result);
end;

function TUndExternal.ImportVariables(L: Plua_State): TUndCompiledScript;
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
    //Index := LuaAbsIndex(L, index);
    lua_pushnil(L);
    while (lua_next(L, Index) <> 0) do
    begin
      v.LuaType := lua_type(L, -1);
      v.LuaTypeStr := plua_typetokeyword(v.LuaType);
      v.name := plua_dequote(plua_LuaStackToStr(L, -2, MaxTable, SubTableMax));
      // Value := Dequote(LuaStackToStr(L, -1, MaxTable, SubTableMax));
      if sl.IndexOf(v.name) = -1 then
      begin
        if CanAddKey(v.LuaType, v.name) then
        begin
          v.value := plua_dequote(plua_LuaStackToStr(L, -1, MaxTable, SubTableMax));
          result := GetFormatedImports(result, v);
        end;
        sl.Add(v.name);
      end;
      //debug('found global var:'+v.name);
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

procedure TUndExternal.Debug(s: string);
begin
  if fEnableDebug then
    writeln('[dbg] ' + s);
end;

function TUndExternal.GetScript(L: Plua_State; script: string) : TUndCompiledScript;
begin
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
  result.completescript := result.InitScript + crlf + script + crlf + result.EndScript;
  Debug('Done getting script.');
end;

procedure TUndExternal.HandleOutput(L: Plua_State; const s: string);
var
  slp:TStringLoop;
  sl:TStringList;
  ltype:integer;
  vvalue,vname:string;
begin
  sl := TStringList.Create;
  slp := TStringLoop.Create;
  slp.LoadFromString(s);
  while slp.Found do begin
    if beginswith(slp.Current, cUnderSetPrefix) then begin
      sl.CommaText := after(slp.Current, cUnderSetPrefix);
      ltype := plua_keywordtotype(sl.Values['t']);
      vname := sl.Values['n'];
      vvalue := sl.Values['v'];
      case ltype of
        LUA_TBOOLEAN:
          pLua_SetLocal(L,vname,StrToBool(vvalue));
        LUA_TNUMBER:
          pLua_SetLocal(L,vname,StrToIntDef(vvalue,0));
        LUA_TNIL:
          pLua_SetLocal(L,vname,varNull);
        LUA_TSTRING: begin
          vvalue := stringdecode(vvalue);
          //writeln('setting:'+vname+'='+vvalue);
          pLua_SetLocal(L,vname,vvalue);
        end;
      end;
    end else begin

       // if rudCustomFunc_WriteLn <> emptystr then begin
        //outdebug('writeln:'+slp.Current);
        if fSilent = false then
        uConsoleWriteLn(L,slp.Current);
    end;
  end;
  slp.Free;
  sl.Free;
end;

procedure TUndExternal.HandleOutputAlt(const s: string);
begin
  HandleOutput(fLuaState, s);
end;

function TUndExternal.RunScript(L: Plua_State; script: string):integer;
var
  r: TUndScriptResult;
  cmd: TCatCSCommand;
  fn, progdir, underpath, command, params: string;
  sl: TStringList;
  timeout: integer;
  sw: TCatStopWatch;
begin
  sw := CatStopWatchNew;
  result := 1;
  timeout := 0;
  if uoTimeout in fLanguage.Options then
    timeout := 10000;

  r.success := true;
  //path := 'R:\Win64\Extensions\underscript\';
  progdir := extractfilepath(paramstr(0));
  underpath := progdir+'Extensions\underscript';
  command := fLanguage.Command;
  command := replacestr(command, '%u', underpath);
  command := replacestr(command, '%p', progdir);
  script:=(GetScript(L, script)).completescript;

  if pos('%',fLanguage.FormatScript) <> 0 then
  script := replacestr(fLanguage.FormatScript, '%s', script);

  fn := GetTempFile(fLanguage.FileExt);
  params := fn;
  if pos('%',fLanguage.Params) <> 0 then
  params := replacestr(fLanguage.Params, '%f', fn);

  sl := TStringList.Create;
  sl.Text := script;
  sl.SaveToFile(fn);
  if fileexists(command) then begin
    cmd := TCatCSCommand.Create;
    cmd.OnOutput := HandleOutputAlt;
    cmd.Timeout := timeout;
    {$IFDEF DXE2_OR_UP}
    RunCmdWithCallBack(command, params,
      procedure(const Line: string)
      begin
        //outdebug('out:'+Line);
        HandleOutput(L, Line);
      end
    , timeout);
   {$ELSE}
     cmd.Run(command, fn);
   {$ENDIF}
    cmd.free;
  end else begin
    r.success := false;
    r.errormessage := command+' not available in path.';
    //uConsoleErrorLn(L, -1, r.errormessage);
    luaL_error(L, PAnsiChar(AnsiString(r.errormessage)));
  end;
  sl.free;
  deletefile(fn);
  Und_PushScriptResult(L, r, sw);
end;

constructor TUndExternal.Create(L: Plua_State; Lang:TUndLanguageExternal);
begin
  inherited Create;
  fEnableImport := true;
  fEnableDebug := false;
  fSilent := false;
  fLuaState := L;
  fLanguage := Lang;
end;

destructor TUndExternal.Destroy;
begin
  inherited;
end;

end.
