unit uDWS;
{
 UnderScript Delphi Web Script Wrapper
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, sysUtils, lua, plua, LuaObject, uPSCompiler, uPSRuntime, CatStrings, forms,
  UndHelper_Obj, Dws2Comp,dws2Exprs,dws2Compiler,dws2htmlfilter, UndImporter,
  UndConst;

type
   TUndDWS = class
    State:Plua_State;
    dws2Unit1: Tdws2Unit;
    dws1:tdelphiwebscriptII;
    dwsfilter1:tdws2htmlfilter;
    dwshtmlunit1:tdws2htmlunit;
    FPrg: TProgram;
    constructor Create(L : PLua_State);
    destructor Destroy; override;
    procedure Write(Info: TProgramInfo);
    procedure WriteLn(Info: TProgramInfo);
    procedure GetLocal(Info: TProgramInfo);
    procedure GetGlobal(Info: TProgramInfo);
    procedure SetLocal(Info: TProgramInfo);
    procedure SetGlobal(Info: TProgramInfo);
   end;

type
  TUndDWSWrapper = class(TLuaObject)
  private
    Pas:TUndDWS;
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

procedure RegisterUndPascalWrapper(L : Plua_State);
function PascalScript_Run(L: plua_State):integer; cdecl;
function PascalWebScript_Run(L: plua_State):integer; cdecl;
function DWSScript_Run(L: plua_State;isfilter:boolean):integer; cdecl;

implementation

function DWSScript_Run(L: plua_State;isfilter:boolean):integer; cdecl;
var obj:TUndDWS; r:variant; script,result_str:string; importer:TUndImporter; i:integer;
begin
  obj := TUndDWS.Create(L);
  importer:=TUndImporter.create(L);
  importer.EnableDebug:=false;
  importer.FuncReadFormat:='var %k:%t;%k := '+rudLibName+'.GetL(''%k'');';
  importer.FuncWriteFormat:=crlf+rudLibName+'.SetL(''%k'',%k);';
  if isfilter then begin // import not working... see later why
  obj.dws1.Config.Filter:=obj.dwsfilter1;
  importer.FuncReadFormat:='<% '+importer.FuncReadFormat+' %>';
  importer.FuncWriteFormat:='<%' +importer.FuncWriteFormat+ '%>';
  end;
  script:=pchar(lua_tostring(L,1));
  try script:=importer.GetScript(script); except end; // eats any exception
  //Undhelper.WriteLn('script:'+script); //debug

  try obj.FPrg.Free; finally obj.FPrg := nil; end;
  obj.dws1.Config.CompilerOptions := [];
  obj.FPrg := obj.dws1.Compile(script);
  for i := 0 to obj.FPrg.Msgs.Count - 1 do begin
   if rudCustomFunc_LogError<>emptystr then
   Und_LogError(L,-1,obj.FPrg.Msgs[i].AsInfo)
   else UndHelper.writeln(obj.FPrg.Msgs[i].AsInfo);
  end;
  obj.FPrg.Debugger := nil;
  obj.FPrg.execute;
  if isfilter then begin
   if obj.FPrg.Result is Tdws2DefaultResult then
   result_str:=Tdws2DefaultResult(obj.FPrg.Result).Text;
   UndHelper.writeln(result_str);
  end;

  obj.free;
  importer.free;
  //plua_pushvariant(L, r);
  result:=1;
end;

function PascalScript_Run(L: plua_State):integer; cdecl; // Main function for execution
begin
  DWSScript_Run(L,false);  result:=1;
end;

function PascalWebScript_Run(L: plua_State):integer; cdecl; // Main function for execution
begin
  DWSScript_Run(L,true);  result:=1;
end;

{function method_run(l : PLua_State) : Integer; cdecl;
var o : TUndDWSWrapper; s,data:string;
  procedure OutputMessages;
  var l: Longint;
  begin
    for l := 0 to o.pas.PSScript.CompilerMessageCount - 1 do
    o.pas.errormsg:=o.pas.errormsg+('Compiler: '+ o.pas.PSScript.CompilerErrorToStr(l));
  end;
begin
  o:=TUndDWSWrapper(LuaToTLuaObject(l, 1));
  s:=pchar(lua_tostring(L,2));
  o.pas.Output:=emptystr;  o.pas.ErrorMsg:=emptystr;
  o.pas.PSScript.Script.Text:=s;
  if o.pas.PSScript.Compile then begin
   o.pas.success:= o.pas.PSScript.Execute;
  end else o.pas.Success:=false; // else writeln('Failed to compile.');
  OutputMessages;
  result := 1;
end;  }

procedure TUndDWS.WriteLn(Info: TProgramInfo);
begin
 UndHelper.writeln(info['s']);
end;

procedure TUndDWS.Write(Info: TProgramInfo);
begin
 UndHelper.write(info['s']);
end;

procedure TUndDWS.GetLocal(Info: TProgramInfo);
begin
 info.Result:=Undhelper.GetL(info['s']);
end;

procedure TUndDWS.GetGlobal(Info: TProgramInfo);
begin
 info.Result:=Undhelper.GetG(info['s']);
end;

procedure TUndDWS.SetLocal(Info: TProgramInfo);
var v:variant; s:string;
begin
 try v:=info.value['v']; except end; // eats any variant reading error
 //s:=v; system.writeln('setting '+info['s']+' to: '+s);
 Undhelper.SetL(info['s'], v);
end;

procedure TUndDWS.SetGlobal(Info: TProgramInfo);
var v:variant; s:string;
begin
 try v:=info.value['v']; except end; // eats any variant reading error
 Undhelper.SetG(info['s'], v);
end;

constructor TUndDWS.Create(L : PLua_State);
 procedure Add_CustomFunctions;
 var func_writeln:Tdws2Function;
 var func_write:Tdws2Function;
 var func_getl:Tdws2Function;
 var func_getg:Tdws2Function;
 var func_setl:Tdws2Function;
 var func_setg:Tdws2Function;
 begin
  // Write
  func_write:=Tdws2Function.Create(dws2unit1.Functions);
  func_write.Name:='Write';
  func_write.ResultType:=emptystr;
  func_write.OnEval:=Write;
  with tdws2parameter(func_write.Parameters.Add) do begin
   name:='s'; datatype:='string';
  end;
  // WriteLn
  func_writeln:=Tdws2Function.Create(dws2unit1.Functions);
  func_writeln.Name:='WriteLn';
  func_writeln.ResultType:=emptystr;
  func_writeln.OnEval:=WriteLn;
  with tdws2parameter(func_writeln.Parameters.Add) do begin
   name:='s'; datatype:='string';
  end;
  // GetLocal
  func_getl:=Tdws2Function.Create(dws2unit1.Functions);
  func_getl.Name:='GetL';
  func_getl.ResultType:='variant';
  func_getl.OnEval:=GetLocal;
  with tdws2parameter(func_getl.Parameters.Add) do begin
   name:='s'; datatype:='string';
  end;
  // GetGlobal
  func_getg:=Tdws2Function.Create(dws2unit1.Functions);
  func_getg.Name:='GetG';
  func_getg.ResultType:='variant';
  func_getg.OnEval:=GetGlobal;
  with tdws2parameter(func_getg.Parameters.Add) do begin
   name:='s'; datatype:='string';
  end;
  // SetLocal
  func_setl:=Tdws2Function.Create(dws2unit1.Functions);
  func_setl.Name:='SetL';
  func_setl.ResultType:=emptystr;
  func_setl.OnEval:=SetLocal;
  with tdws2parameter(func_setl.Parameters.Add) do begin
   name:='s'; datatype:='string';
  end;
  with tdws2parameter(func_setl.Parameters.Add) do begin
   name:='v'; datatype:='variant';
  end;
  // SetGlobal
  func_setg:=Tdws2Function.Create(dws2unit1.Functions);
  func_setg.Name:='SetG';
  func_setg.ResultType:=emptystr;
  func_setg.OnEval:=SetGlobal;
  with tdws2parameter(func_setg.Parameters.Add) do begin
   name:='s'; datatype:='string';
  end;
  with tdws2parameter(func_setg.Parameters.Add) do begin
   name:='v'; datatype:='variant';
  end;
 end;
begin
  dws1:=tdelphiwebscriptII.create(nil);
  dwsfilter1:=tdws2htmlfilter.create(nil);
  dwshtmlunit1:=tdws2htmlunit.Create(nil);
  // begin
  dws2unit1:=Tdws2Unit.Create(nil);
  dws2unit1.UnitName:=rudLibName;
  dws2unit1.script:=dws1;
  Add_CustomFunctions;
  // end
  dwshtmlunit1.Script:=dws1;
  //dws1.Config.Filter:=dwsfilter1;
  UndHelper.LuaState:=L;
  State:=L;
end;

destructor TUndDWS.Destroy;
begin
  dws1.Free;
  dwsfilter1.Free;
  dwshtmlunit1.Free;
  dws2unit1.free;
  inherited Destroy;
end;

constructor TUndDWSWrapper.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  pas:=TUndDWS.Create(LuaState);
end;

function TUndDWSWrapper.GetPropValue(propName: String): Variant;
begin
 { if CompareText(propName, 'ErrorMsg') = 0 then result := pas.ErrorMsg else
  if CompareText(propName, 'Output') = 0 then result := pas.Output else
  if CompareText(propName, 'Success') = 0 then result := pas.Success else   }
  Result:=inherited GetPropValue(propName);
end;

function TUndDWSWrapper.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  //if CompareText(propName, 'Expression') = 0 then obj.Expression.text := AValue else
  Result:=inherited SetPropValue(propName, AValue);
end;

procedure RegisterUndPascalWrapper(L: Plua_State);
const
 cObjectName='RPascalScript';
 procedure Register_Methods(L : Plua_State; classTable : Integer);
 begin
  //RegisterMethod(L,'eval', @method_evalstring, classTable);
  //RegisterMethod(L,'run', @method_run, classTable);
 end;
 function GetLuaObject(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
 begin
   result := TUndDWSWrapper.Create(L, AParent);
 end;
 function new_Object(L : PLua_State) : Integer; cdecl;
 var p : TLuaObjectNewCallback;
 begin
   p := @GetLuaObject;
   result := new_LuaObject(L,cObjectName, p);
 end;
begin
 RegisterTLuaObject(L,cObjectName,@new_Object, @Register_Methods);
end;

destructor TUndDWSWrapper.Destroy;
begin
  pas.free;
  inherited Destroy;
end;

end.

