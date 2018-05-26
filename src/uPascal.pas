unit uPascal;

{
 UnderScript Pascal Wrapper
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, sysUtils, lua, plua, LuaObject, uPSCompiler, uPSRuntime, CatStrings,
  forms, UndHelper_Obj, uPSComponent, UndImporter, UndConst;

type
   TUndPascal = class
    PSScript: TPSScript;
    State:Plua_State;
    Output:string;
    ErrorMsg:string;
    Success:boolean;
    RedirectIO:boolean;
    constructor Create(L : PLua_State);
    destructor Destroy; override;
    procedure CompImport(Sender: TObject; x: TIFPSPascalcompiler);
    procedure ExecImport(Sender: TObject; Exec: TIFPSExec;x: TIFPSRuntimeClassImporter);
    procedure Compile(Sender: TPSScript);
    procedure Execute(Sender: TPSScript);
    procedure MyWriteln(s: string);
    procedure MyWrite(s: string);
   end;

type
  TUndPascalWrapper = class(TLuaObject)
  private
    Pas:TUndPascal;
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

procedure RegisterUndPascalWrapper(L : Plua_State);
function PascalClassic_Run(L: plua_State):integer; cdecl;

implementation

uses uPSR_std, uPSC_std,
  uPSR_stdctrls, uPSC_stdctrls,
  uPSR_forms, uPSC_forms,
  uPSC_graphics, uPSR_graphics,
  uPSC_controls, uPSR_controls,
  uPSC_classes, uPSR_classes,
  uPSC_comobj, uPSR_comobj,
  uPSC_dateutils, uPSR_dateutils,
  uPSC_DB, uPSR_DB,
  uPSC_extctrls, uPSR_extctrls,
  uPSC_menus, uPSR_menus,
  uPascal_UndHelper_Obj,
  uPSC_dll, uPSR_dll;

{function method_evalstring(l : PLua_State) : Integer; cdecl;
var ht : TUndPascalWrapper; r:string;
begin
  ht:=TUndPascalWrapper(LuaToTLuaObject(l, 1));
  r:= ht.obj.EvalStringAsStr(pchar(lua_tostring(L,2)));
  lua_pushstring(L, pchar(r));
  result := 1;
end;  }

function PascalClassic_Run(L: plua_State):integer; cdecl; // Main function for execution
var obj:TUndPascal; r:variant; script:string; importer:TUndImporter;
  procedure OutputMessages;
  var i: Longint;
  begin
    for i := 0 to obj.PSScript.CompilerMessageCount - 1 do
    Und_LogError(L,i,'Pascal: '+ obj.PSScript.CompilerErrorToStr(i));
  end;
begin
  obj := TUndPascal.Create(L);
  importer:=TUndImporter.create(L);
  //importer.EnableDebug:=true;
  importer.FuncReadFormat:='%k = '+rudLibName+'.GetL("%k");'+crlf;
  importer.FuncWriteFormat:=crlf+rudLibName+'.SetL("%k",%k);';
  script:=lua_tostring(L,1);
  //script:=importer.GetScript(script); 
  obj.PSScript.Script.Text:=script;
  if obj.PSScript.Compile then begin
     obj.success:= obj.PSScript.Execute;
  end else obj.Success:=false;
  if obj.success=false then OutputMessages;//writeln(obj.errormsg);
  //OutputMessages;
  obj.free;
  importer.free;
  //plua_pushvariant(L, r);
  result:=1;
end;

function method_run(l : PLua_State) : Integer; cdecl;
var o : TUndPascalWrapper; s,data:string;
  procedure OutputMessages;
  var l: Longint;
  begin
    for l := 0 to o.pas.PSScript.CompilerMessageCount - 1 do
    o.pas.errormsg:=o.pas.errormsg+('Compiler: '+ o.pas.PSScript.CompilerErrorToStr(l));
  end;
begin
  o:=TUndPascalWrapper(LuaToTLuaObject(l, 1));
  s:=pchar(lua_tostring(L,2));
  o.pas.Output:=emptystr;  o.pas.ErrorMsg:=emptystr;
  o.pas.PSScript.Script.Text:=s;
  if o.pas.PSScript.Compile then begin
   o.pas.success:= o.pas.PSScript.Execute;
  end else o.pas.Success:=false; // else writeln('Failed to compile.');
  OutputMessages;
  result := 1;
end;

procedure RegisterUndPascalWrapper(L: Plua_State);
const cObjectName='RPascal';
 procedure Register_Methods(L : Plua_State; classTable : Integer);
 begin
  //RegisterMethod(L,'eval', @method_evalstring, classTable);
  RegisterMethod(L,'run', @method_run, classTable);
 end;
 function GetLuaObject(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
 begin
   result := TUndPascalWrapper.Create(L, AParent);
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

procedure TUndPascal.CompImport(Sender: TObject; x: TIFPSPascalcompiler);
begin
 SIRegister_Std(x);
 SIRegister_Classes(x, True);
 SIRegister_Graphics(x, True);
 SIRegister_Controls(x);
 SIRegister_stdctrls(x);
 SIRegister_Forms(x);
 SIRegister_ComObj(x);
 SIRegister_DB(x);
 SIRegister_ExtCtrls(x);
 SIRegister_Menus(x);
 SIRegister_UndHelper_Obj(x);
 RegisterDateTimeLibrary_C(x);  //test
end;

procedure TUndPascal.ExecImport(Sender: TObject; Exec: TIFPSExec;x: TIFPSRuntimeClassImporter);
begin
 RIRegister_Std(x);
 RIRegister_Classes(x, True);
 RIRegister_Graphics(x, True);
 RIRegister_Controls(x);
 RIRegister_stdctrls(x);
 RIRegister_Forms(x);
 RIRegister_DB(x);
 RIRegister_ExtCtrls(x);
 RIRegister_Menus(x);
 RIRegister_ComObj(exec);
 RIRegister_UndHelper_Obj(x);
 RegisterDateTimeLibrary_R(exec); 
 RegisterClassLibraryRuntime(exec,x);
 RegisterDLLRuntime(exec);
end;

procedure TUndPascal.MyWriteln(s: string);
begin
  if redirectio then begin
   if output=emptystr then output:=s else output:=crlf+output+s;
  end else Undhelper.writeln(s);
end;

procedure TUndPascal.MyWrite(s: string);
begin
  if redirectio then output:=output+s else  Undhelper.write(s);
end;

procedure TUndPascal.Compile(Sender: TPSScript);
begin
  //Sender.AddFunction(@MyReadln, 'function Readln(question: string): string;');
  Sender.Comp.OnExternalProc := @DllExternalProc;
  Sender.AddMethod(Self,@TUndPascal.MyWriteln, 'procedure Writeln(s: string);');
  Sender.AddMethod(Self,@TUndPascal.MyWrite, 'procedure Write(s: string);');
  Sender.AddRegisteredVariable('Application', 'TApplication');
  Sender.AddRegisteredVariable(rudLibName, 'TUndHelper');
end;

procedure TUndPascal.Execute(Sender: TPSScript);
begin
  PSScript.SetVarToInstance('APPLICATION', Application);
  PSScript.SetVarToInstance(uppercase(rudLibName), UndHelper);
end;

constructor TUndPascal.Create(L : PLua_State);
begin
  PSScript := TPSScript.Create(nil);
  //PSScript.Comp.AllowNoBegin:=true;
  //PSScript.Comp.AllowNoEnd:=true;
  //PSScript.Comp.AllowUnit:=false;
  PSScript.OnCompImport:=CompImport;
  PSScript.OnExecImport:=ExecImport;
  PSScript.OnCompile:=Compile;
  PSScript.OnExecute:=Execute;
  //PSScript.CompilerOptions:=PSScript.CompilerOptions-[icAllowNoBegin];
  //PSScript.CompilerOptions:=PSScript.CompilerOptions-[icAllowNoEnd];
  RedirectIO:=false;
  UndHelper.LuaState:=L;
  State:=L;
end;

destructor TUndPascal.Destroy;
begin
  PSScript.Free;
  inherited Destroy;
end;

// Lua Object ******************************************************************
constructor TUndPascalWrapper.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  pas:=TUndPascal.Create(LuaState);
  pas.RedirectIO:=true;
end;

function TUndPascalWrapper.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'ErrorMsg') = 0 then result := pas.ErrorMsg else
  if CompareText(propName, 'Output') = 0 then result := pas.Output else
  if CompareText(propName, 'Success') = 0 then result := pas.Success else
  Result:=inherited GetPropValue(propName);
end;

function TUndPascalWrapper.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  //if CompareText(propName, 'Expression') = 0 then obj.Expression.text := AValue else
  Result:=inherited SetPropValue(propName, AValue);
end;

destructor TUndPascalWrapper.Destroy;
begin
  pas.free;
  inherited Destroy;
end;

end.

