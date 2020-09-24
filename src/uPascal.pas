unit uPascal;

{
 UnderScript Pascal Wrapper
 Copyright (c) 2013-2020 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, System.SysUtils, Vcl.Forms,
{$ELSE}
  Classes, SysUtils, Forms,
{$ENDIF}
  lua, plua, LuaObject, uPSCompiler, uPSRuntime, CatStrings, UndConsole,
  UndHelper_Obj, uPSComponent, UndImporter, UndConst;

type
   TUndPascal = class
    PSScript: TPSScript;
    State:Plua_State;
    Output:string;
    ErrorMsg:string;
    Success:boolean;
    constructor Create(L : PLua_State);
    destructor Destroy; override;
    procedure CompImport(Sender: TObject; x: TIFPSPascalcompiler);
    procedure ExecImport(Sender: TObject; Exec: TIFPSExec;x: TIFPSRuntimeClassImporter);
    procedure Compile(Sender: TPSScript);
    procedure Execute(Sender: TPSScript);
    procedure MyWriteln(s: string);
    procedure MyWrite(s: string);
    procedure MyDebug(s: string);
   end;

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
  UndHelper_REM,
  uPSC_dll, uPSR_dll;

// Main function for execution of Pascal script
function PascalClassic_Run(L: plua_State):integer; cdecl;
var
  r: TUndScriptResult;
  obj:TUndPascal;
  rv:variant;
  script:string;
  importer:TUndImporter;
  procedure HandleError;
  var i: Longint;
  begin
    r.success := false;
    r.errormessage := emptystr;
    for i := 0 to obj.PSScript.CompilerMessageCount - 1 do begin
      uConsoleErrorLn(L,i,'Pascal: '+ obj.PSScript.CompilerErrorToStr(i));
      r.errormessage := r.errormessage+crlf+obj.PSScript.CompilerErrorToStr(i);
    end;
  end;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK = false then
    Exit;
  r.success := true;
  obj := TUndPascal.Create(L);
  importer:=TUndImporter.create(L);
  //importer.EnableDebug:=true;
  importer.FuncReadFormat:='%k = '+rudLibName+'.GetL("%k");'+crlf;
  importer.FuncWriteFormat:=crlf+rudLibName+'.SetL("%k",%k);';
  script:=lua_tostring(L,1);
  //script:=importer.GetScript(L, script);
  obj.PSScript.Script.Text:=script;
  if obj.PSScript.Compile then begin
     obj.success:= obj.PSScript.Execute;
     //writeln(booltoyn(obj.Success));
  end else obj.Success:=false;

  if obj.success=false then
    HandleError;//writeln(obj.errormsg);
  //OutputMessages;
  obj.free;
  importer.free;
  Und_PushScriptResult(L, r);
  result:=1;
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
  Undhelper.writeln(s);
end;

procedure TUndPascal.MyWrite(s: string);
begin
  Undhelper.write(s);
end;

procedure TUndPascal.MyDebug(s: string);
begin
  Undhelper.Debug(s);
end;

procedure TUndPascal.Compile(Sender: TPSScript);
begin
  //Sender.AddFunction(@MyReadln, 'function Readln(question: string): string;');
  Sender.Comp.OnExternalProc := @DllExternalProc;
  Sender.AddMethod(Self,@TUndPascal.MyDebug, 'procedure Debug(s: string);');
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
  UndHelper.LuaState:=L;
  State:=L;
end;

destructor TUndPascal.Destroy;
begin
  PSScript.Free;
  inherited Destroy;
end;

end.

