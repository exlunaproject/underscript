unit uRuby;
{
 UnderScript Ruby Wrapper
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
 Classes, SysUtils, lua, plua, LuaObject, RubyEval, RubyWrapper,
 UndHelperUnit, uConv, UndConst, UndImporter, CatStrings;

type
  TUndRubyWrapper = class(TLuaObject)
  private
    obj:TRubyEval;
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

procedure RegisterUndRubyWrapper(L : Plua_State);
procedure RegisterRubyFunctions(LuaState: PLua_State);
function Ruby_Eval(L: plua_State):integer; cdecl;

implementation

uses
 RbType;

var
 Ruby_Initialized:boolean=false;
 obj:TRubyEval;

procedure MyRubyPrint(msg:string);
begin
 UndHelper.WriteLn(msg);
end;

function Ruby_Eval(L: plua_State):integer; cdecl; // Main function for execution
var r:variant; script:string; importer:TUndImporter;
begin
  UndHelper.LuaState:=L; // important
  if Ruby_Initialized=false then begin
   Ruby_Initialized:=true;
   obj := TRubyEval.Create(nil);
   rubyprintproc:=MyRubyPrint;
   RegisterRubyFunctions(L);
   //RemoveIORedirection;  // was creating an issue after printing x times... write, 'Bad file descriptor'
   rb_define_global_function('puts', @DelphiIO_puts, -2); // must be -2 so that it can have any number of arguments
  end;
  importer:=TUndImporter.create(L);
  //importer.EnableDebug:=true;
  importer.FuncReadFormat:='%k = '+rudLibName+'.GetL("%k")'+crlf;
  importer.FuncWriteFormat:=crlf+rudLibName+'.SetL("%k",%k)';
  script:=lua_tostring(L,1);
  script:=importer.GetScript(script); // imports
  try obj.EvalExpression(script); except end; // eats any exception
  //if obj.Output<>emptystr then writeln(obj.Output);
  if obj.ErrorInfo<>emptystr then begin
   if rudCustomFunc_LogError<>emptystr then
   Und_LogError(L,-1,obj.ErrorInfo)
   else writeln(obj.errorinfo);
  end;
  //obj.free; -> moved to finalization
  importer.free;
  //plua_pushvariant(L, r);
  result:=1;
end;

function method_evalexpression(l : PLua_State) : Integer; cdecl;
var ht : TUndRubyWrapper; r:variant;
begin
  ht:=TUndRubyWrapper(LuaToTLuaObject(l, 1));
  resetoutput;
  r:= ht.obj.evalexpression(lua_tostring(L,2));
  plua_pushvariant(L, r);
  result := 1;
end;

function method_run(l : PLua_State) : Integer; cdecl;
var ht : TUndRubyWrapper;
begin
  ht:=TUndRubyWrapper(LuaToTLuaObject(l, 1));
  resetoutput;
  ht.obj.Expression.Text:=pchar(lua_tostring(L,2));
  plua_pushvariant(L, ht.obj.execute);
  result := 1;
end;

procedure RegisterUndRubyWrapper(L: Plua_State);
const cObjectName='RRuby';
 function new_callback(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
 begin
   result := TUndRubyWrapper.Create(L, AParent);
 end;
 function new_XCL(L : PLua_State) : Integer; cdecl;
 var p : TLuaObjectNewCallback;
 begin
   p := @new_callback;
   result := new_LuaObject(L, cObjectName, p);
 end;
 procedure register_methods(L : Plua_State; classTable : Integer);
 begin
  RegisterMethod(L,'Eval', @method_evalexpression, classTable);
  RegisterMethod(L,'Run', @method_run, classTable);
 end;
begin
  RegisterTLuaObject(L,cObjectName, @new_XCL, @register_methods);
end;

procedure RegisterRubyFunctions(LuaState: PLua_State);
var cUnd:Tvalue;
 function Und_Run(This, v: Tvalue): Tvalue; cdecl;
 begin
  UndHelper.run(dl_string(v)); result := ap_bool(true);
 end;
 function Und_WriteLn(This, v: Tvalue): Tvalue; cdecl;
 var str: Tvalue;
 begin
   UndHelper.writeln(dl_variant(v)); result := ap_bool(true);
 end;
 function Und_Write(This, v: Tvalue): Tvalue; cdecl;
 begin
  UndHelper.write(dl_variant(v)); result := ap_bool(true);
 end;
 function Und_GetGlobal(This, v: Tvalue): Tvalue; cdecl;
 var str: Tvalue; s:string;
 begin
  try s:=Undhelper.getg(dl_string(v)); except end;
  if Length(S) = 0 then result := Qnil else result := rb_str_new2(PChar(S));
  rb_lastline_set(result);
 end;
 function Und_SetGlobal(This, v, v2: Tvalue): Tvalue; cdecl;
 begin
  try Undhelper.setg(dl_String(v),dl_Variant(v2)); except end;
  result := ap_bool(true); // from macroimp.pas
 end;
 function Und_SetLocal(This, v, v2: Tvalue): Tvalue; cdecl;
 begin
  try Undhelper.setl(dl_String(v),dl_Variant(v2)); except end;  // eats strange ruby error 'can't convert false into string'
  result := ap_bool(true);
 end;
 function Und_GetLocal(This, v: Tvalue): Tvalue; cdecl;
 var str: Tvalue; s:variant;
 begin
  try s:=Undhelper.getl(dl_string(v)); except end; // eats strange ruby error 'can't convert false into string'
  result:=ap_variant(s);
  //if Length(S) = 0 then result := Qnil else result := rb_str_new2(PChar(S));
  rb_lastline_set(result);
 end;
 function Und_FindFunc(This, v: Tvalue): Tvalue; cdecl;
 begin
   UndHelper.findfunc(dl_string(v)); result := ap_bool(true);
 end;
 function Und_FindModFunc(This, v, v2: Tvalue): Tvalue; cdecl;
 begin
   UndHelper.findmodfunc(dl_string(v),dl_string(v2)); result := ap_bool(true);
 end;
 function Und_Push(This, v: Tvalue): Tvalue; cdecl;
 begin
   UndHelper.push(dl_variant(v)); result := ap_bool(true);
 end;
 function Und_CallFunc(This, v: Tvalue): Tvalue; cdecl;
 begin
   UndHelper.CallFunc(dl_integer(v)); result := ap_bool(true);
 end;
begin
  cUnd:=rb_define_module(pchar(rudLibName));
  rb_define_module_function(cUnd, 'FindFunc', @Und_FindFunc, 1);
  rb_define_module_function(cUnd, 'FindModFunc', @Und_FindModFunc, 2);
  rb_define_module_function(cUnd, 'Push', @Und_Push, 1);
  rb_define_module_function(cUnd, 'CallFunc', @Und_CallFunc, 1);
  rb_define_module_function(cUnd, 'GetL', @Und_GetLocal, 1);
  rb_define_module_function(cUnd, 'SetL', @Und_SetLocal, 2);
  rb_define_module_function(cUnd, 'GetG', @Und_GetGlobal, 1);
  rb_define_module_function(cUnd, 'SetG', @Und_SetGlobal, 2);
  rb_define_module_function(cUnd, 'WriteLn', @Und_WriteLn, 1);
  rb_define_module_function(cUnd, 'Write', @Und_Write, 1);
  rb_define_module_function(cUnd, 'Run', @Und_Run, 1);
end;

// Lua Object ******************************************************************
constructor TUndRubyWrapper.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TRubyEval.Create(nil);
  UndHelper.LuaState:=LuaState;
  RegisterRubyFunctions(LuaState);
end;

function TUndRubyWrapper.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'ErrorMsg') = 0 then result := obj.ErrorInfo else
  if CompareText(propName, 'Output') = 0 then result := obj.Output else
  if CompareText(propName, 'Success') = 0 then result := obj.EvalOk else
  //if CompareText(propName, 'Expression') = 0 then result := obj.Expression.text else
  Result:=inherited GetPropValue(propName);
end;

function TUndRubyWrapper.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  //if CompareText(propName, 'Expression') = 0 then obj.Expression.text := AValue else
  Result:=inherited SetPropValue(propName, AValue);
end;

destructor TUndRubyWrapper.Destroy;
begin
  obj.Free;
  inherited Destroy;
end;

initialization

finalization
 if obj<>nil then obj.free;

end.

