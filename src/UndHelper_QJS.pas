{
  Native Class Demo - With module or Global context.
}

unit UndHelper_QJS;
{$IFDEF FPC}
  {$mode delphi}{$H+}{$M+}
  {$PackRecords C}
{$ENDIF}
interface

uses
  sysutils, CatStrings, quickjs, undhelper_obj;

var
  API_Class_id : JSClassID = 0;
  API_Class_Proto : JSValue;
  JClass : JSClassDef = (class_name:'GetConsole';finalizer:nil;gc_mark:nil;call:nil;exotic:nil);
  tab : array [0..8] of JSCFunctionListEntry;

procedure Emu_mod_init(ctx : JSContext; m : JSModuleDef); cdecl;
function Emu_init(ctx : JSContext; m : JSModuleDef): Integer;cdecl;

procedure RegisterNativeClass(ctx : JSContext); cdecl;

implementation

function jsdebug(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  s: PAnsiChar;
begin
  Result := JS_UNDEFINED;
  if argc = 1 then
  begin
    s := JS_ToCString(ctx, argv[0]);
    undhelper.debug(s);
    JS_FreeCString(ctx, s);
  end;
end;

function jswrite(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  s: PAnsiChar;
begin
  Result := JS_UNDEFINED;
  if argc = 1 then
  begin
    s := JS_ToCString(ctx, argv[0]);
    undhelper.write(s);
    JS_FreeCString(ctx, s);
  end;
end;

function jswriteln(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  s: PAnsiChar;
begin
  Result := JS_UNDEFINED;
  if argc = 1 then
  begin
    s := JS_ToCString(ctx, argv[0]);
    undhelper.writeln(s);
    JS_FreeCString(ctx, s);
  end;
end;

function jsgetlstring(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  name: PAnsiChar;
begin
  Result := JS_UNDEFINED;
  if argc = 1 then
  begin
    name := JS_ToCString(ctx, argv[0]);
    Result := JS_NewString(ctx, PAnsiChar(AnsiString(undhelper.GetLString(name))));
    JS_FreeCString(ctx, name);
  end;
end;

function jssetlstring(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  name, avalue: PAnsiChar;
begin
  Result := JS_UNDEFINED;
  if argc = 2 then
  begin
    name := JS_ToCString(ctx, argv[0]);
    avalue := JS_ToCString(ctx, argv[1]);
    undhelper.SetLString(name, avalue);
    JS_FreeCString(ctx, name);
    JS_FreeCString(ctx, avalue);
  end;
end;

function jsgetlboolean(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  name: PAnsiChar;
begin
  Result := JS_UNDEFINED;
  if argc = 1 then
  begin
    name := JS_ToCString(ctx, argv[0]);
    Result := JS_NewBool(ctx, undhelper.GetLBoolean(name));
    JS_FreeCString(ctx, name);
  end;
end;

function jssetlboolean(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  name: PAnsiChar;
  avalue: boolean;
begin
  Result := JS_UNDEFINED;
  if argc = 2 then
  begin
    name := JS_ToCString(ctx, argv[0]);
    avalue := JS_ToBool(ctx, argv[1]) <> 0;
    undhelper.SetLBoolean(name, avalue);
    JS_FreeCString(ctx, name);
  end;
end;

function jsgetlinteger(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  name: PAnsiChar;
begin
  Result := JS_UNDEFINED;
  if argc = 1 then
  begin
    name := JS_ToCString(ctx, argv[0]);
    //writeln('is:'+inttostr(undhelper.GetLInteger(name)));
    Result := JS_NewBigInt64(ctx, undhelper.GetLInteger(name));
    JS_FreeCString(ctx, name);
  end;
end;

function jssetlinteger(ctx : JSContext; {%H-}this_val : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
var
  name: PAnsiChar;
  avalue: string;
begin
  Result := JS_UNDEFINED;
  if argc = 2 then
  begin
    name := JS_ToCString(ctx, argv[0]);
    avalue := JS_ToCString(ctx, argv[1]);
    if IsInteger(avalue) then
    undhelper.SetLInteger(name, StrToInt(avalue));
    JS_FreeCString(ctx, name);
  end;
end;

function CConstructor(ctx : JSContext; new_target : JSValueConst; argc : Integer; argv : PJSValueConstArr): JSValue; cdecl;
begin
  Result := JS_NewObjectClass(ctx,API_Class_id);
  // New Array for every new instance.
  JS_DefinePropertyValueStr(ctx,Result,'args',JS_NewArray(ctx),JS_PROP_CONFIGURABLE or JS_PROP_WRITABLE);
end;

procedure Emu_mod_init(ctx : JSContext; m : JSModuleDef); cdecl;
var
  obj : JSValue;
begin
  obj := JS_NewCFunction2(ctx, @CConstructor, PAnsiChar('GetConsole'), 1, JS_CFUNC_constructor, 0);
  JS_SetModuleExport(ctx, m, PAnsiChar('GetConsole'), obj);
end;

function Emu_init(ctx : JSContext; m : JSModuleDef): Integer;cdecl;
begin
  Emu_mod_init(ctx,m);
  Result := 0;
end;

procedure RegisterNativeClass(ctx : JSContext); cdecl;
var
  obj,global : JSValue;
begin

  // Create New Class id.
  JS_NewClassID(@API_Class_id);
  // Create the Class Name and other stuff.
  JS_NewClass(JS_GetRuntime(ctx),API_Class_id,@JClass);

  // Properties list.
  //tab[0] := JS_CFUNC_DEF('install', 1, JSCFunctionType(@install));
  //tab[1] := JS_PROP_INT32_DEF('version', 1337, JS_PROP_CONFIGURABLE);
  tab[0] := JS_CFUNC_DEF('GetLString', 1, @jsgetlstring);
  tab[1] := JS_CFUNC_DEF('SetLString', 1, @jssetlstring);
  tab[2] := JS_CFUNC_DEF('GetLBoolean', 1, @jsgetlboolean);
  tab[3] := JS_CFUNC_DEF('SetLBoolean', 1, @jssetlboolean);
  tab[4] := JS_CFUNC_DEF('GetLInteger', 1, @jsgetlinteger);
  tab[5] := JS_CFUNC_DEF('SetLInteger', 1, @jssetlinteger);
  tab[6] := JS_CFUNC_DEF('Write', 1, @jswrite);
  tab[7] := JS_CFUNC_DEF('WriteLn', 1, @jswriteln);
  tab[8] := JS_CFUNC_DEF('Debug', 1, @jsdebug);

  // New Object act as Prototype for the Class.
  API_Class_Proto := JS_NewObject(ctx);

  // Set list of Properties to the prototype Object.
  JS_SetPropertyFunctionList(ctx,API_Class_Proto,@tab,Length(tab));


  // Set the Prototype to the Class.
  JS_SetClassProto(ctx, API_Class_id, API_Class_Proto);

  // Set the Class native constructor.
  obj := JS_NewCFunction2(ctx, @CConstructor, 'GetConsole', 1, JS_CFUNC_constructor, 0);

  // Add the Class to Global Object so we can use it.
  global := JS_GetGlobalObject(ctx);
  JS_SetPropertyStr(ctx,global,'GetConsole',obj);
  JS_FreeValue(ctx,global);
end;

end.

