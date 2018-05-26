unit uPHP_Script;
{
 Underscript PHP Script Wrapper
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, UndHelperUnit, phpLibrary, php4delphi,
  PHPFunctions, phpCustomLibrary, PHPCommon, CatStrings, UndImporter,
  UndConst, SyDLLUtils;

const
 cObjectName='RPHP';

type
  TUndPHPScriptWrapper = class(TLuaObject)
  private
    obj: TpsvPHP;
    Output:string;
    ErrorMsg:string;
    Success:boolean;
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

type
   TMyPHPEngine = class // dummy class
    Engine: TPHPEngine;
    PHPLibrary:TPHPLibrary;
    ErrorMsg:string;
    constructor Create(L : PLua_State);
    destructor Destroy; override;
    procedure PHPLogMessage(Sender : TObject; AText : AnsiString);
    procedure PHPErrorEvent(Sender : TObject; AText : AnsiString;
        AType : TPHPErrorType; AFileName : AnsiString; ALineNo : integer);
    procedure PHPLib_GetLocal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
    procedure PHPLib_SetLocal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
    procedure PHPLib_GetGlobal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
    procedure PHPLib_SetGlobal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
    procedure PHPLib_Run(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
    procedure PHPLib_WriteLn(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
    procedure PHPLib_Write(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
   end;

var
 PHP_Initialized:boolean=false;
 FPHPEngine:TMyPHPEngine;
 obj:TpsvPHP;

procedure RegisterUndPHPScriptWrapper(L : Plua_State);
procedure CreateUndPHPScriptEngine(L : Plua_State);
function PHP_RunCode(L: plua_State):integer; cdecl;

implementation

function PHP_RunCode(L: plua_State):integer; cdecl;
const PHPBegin='<?'; PHPEnd='?>';
var ret:string; script:string; importer:TUndImporter;
begin
  UndHelper.LuaState:=L; // important
  if PHP_Initialized = false then begin
   PHP_Initialized:=true;
   CreateUndPHPScriptEngine(L);
   obj := TpsvPHP.Create(nil);
  end;
  importer:=TUndImporter.create(L);
  //importer.EnableDebug:=true;
  script:=lua_tostring(L,1);
  importer.FuncReadFormat:='$%k = '+lowercase(rudLibName)+'_getl(''%k'');';
  importer.FuncWriteFormat:=crlf+lowercase(rudLibName)+'_setl(''%k'',$%k);';
  if (pos(PHPBegin,script)<>0) and (pos(PHPEnd,script)<>0) then begin
   importer.FuncReadFormat:=PHPBegin+importer.FuncReadFormat+PHPEnd;
   importer.FuncWriteFormat:=PHPBegin+importer.FuncWriteFormat+PHPEnd;
  end;
  try script:=importer.GetScript(script); except end; // processa
  FPHPEngine.ErrorMsg:=emptystr;
  try ret:=obj.runcode(PAnsiChar(script)); except end;
  if ret<>emptystr then UndHelper.writeln(ret);
  if rudCustomFunc_LogError=emptystr then begin
   if FPHPEngine.ErrorMsg<>emptystr then writeln('PHP: '+FPHPEngine.ErrorMsg);
  end;
  //if obj.ErrorInfo<>emptystr then writeln(obj.errorinfo);
  //obj.free; -> moved to finalization
  importer.free;
  //plua_pushvariant(L, r);  
  result:=1;
end;

procedure CreateUndPHPScriptEngine(L : Plua_State);
begin
 if FPHPEngine = nil then
 FPHPEngine:=TMyPHPEngine.Create(L); // must be done once
end;

procedure TMyPHPEngine.PHPLib_GetLocal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
var v:Variant;
begin
  try v:=Undhelper.getl(Parameters[0].value); except end; // eats error, same issue happening with UndRuby... fix later.
  returnvalue:=v;
end;

procedure TMyPHPEngine.PHPLib_SetLocal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
begin
  Undhelper.setl(Parameters[0].Value,Parameters[1].Value);
end;

procedure TMyPHPEngine.PHPLib_GetGlobal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
begin
  returnvalue:=Undhelper.getg(Parameters[0].Value);
end;

procedure TMyPHPEngine.PHPLib_SetGlobal(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
begin
  Undhelper.setg(Parameters[0].Value,Parameters[1].Value);
end;

procedure TMyPHPEngine.PHPLib_Run(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
begin
  Undhelper.run(Parameters[0].Value);
end;

procedure TMyPHPEngine.PHPLib_WriteLn(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
begin
  Undhelper.writeln(Parameters[0].Value);
end;

procedure TMyPHPEngine.PHPLib_Write(Sender: TObject;
   Parameters: TFunctionParams; var ReturnValue: Variant; ZendVar : TZendVariable;
   TSRMLS_DC: Pointer);
begin
  Undhelper.write(Parameters[0].Value);
end;

procedure TMyPHPEngine.PHPLogMessage(Sender : TObject; AText : AnsiString);
begin
 //writeln('log:'+atext);
end;

procedure TMyPHPEngine.PHPErrorEvent(Sender : TObject; AText : AnsiString;
        AType : TPHPErrorType; AFileName : AnsiString; ALineNo : integer);
var s:string;
 function PHPErrorTypeToString(t:TPHPErrorType):string;
 begin
  case t of
  etError, etCoreError, etCompileError, etUserError:result:='Fatal error';
  etWarning, etCoreWarning, etCompileWarning, etUserWarning:result:='Warning';
  etParse:result:='Parse error';
  etNotice, etUserNotice:result:='Notice';
  etUnknown:result:='Unknown error';
  end;
 end;
begin
    s:=PHPErrorTypeToString(atype)+': '+atext+' in '+afilename+' on line '+inttostr(alineno);
    if rudCustomFunc_LogError<>emptystr then Und_LogError(UndHelper.LuaState,alineno,s);
    if errormsg=emptystr then errormsg:=s else errormsg:=crlf+errormsg+s;
end;

constructor TMyPHPEngine.Create(L : PLua_State);
begin
  // must come before Engine.StartupEngine or it will not work!
  PHPLibrary:=TPHPLibrary.Create(nil);
  PHPLibrary.Name:=rudLibName;

  with PHPLibrary.Functions.Add do begin
   functionname:=lowercase(rudLibName)+'_getg'; OnExecute:=PHPLib_GetGlobal;
    with Parameters.add do ParamType := tpString;
  end;
  with PHPLibrary.Functions.Add do begin
   functionname:=lowercase(rudLibName)+'_setg'; OnExecute:=PHPLib_SetGlobal;
    with Parameters.add do ParamType := tpString;
    with Parameters.add do ParamType := tpUnknown; // variant type
  end;
  with PHPLibrary.Functions.Add do begin
   functionname:=lowercase(rudLibName)+'_getl'; OnExecute:=PHPLib_GetLocal;
    with Parameters.add do ParamType := tpString;
  end;
  with PHPLibrary.Functions.Add do begin
   functionname:=lowercase(rudLibName)+'_setl'; OnExecute:=PHPLib_SetLocal;
    with Parameters.add do ParamType := tpString;
    with Parameters.add do ParamType := tpUnknown; // variant type
  end;
  with PHPLibrary.Functions.Add do begin
   functionname:=lowercase(rudLibName)+'_run'; OnExecute:=PHPLib_Run;
    with Parameters.add do ParamType := tpString;
  end;
  with PHPLibrary.Functions.Add do begin
   functionname:=lowercase(rudLibName)+'_writeln'; OnExecute:=PHPLib_WriteLn;
    with Parameters.add do ParamType := tpString;
  end;
  with PHPLibrary.Functions.Add do begin
   functionname:=lowercase(rudLibName)+'_write'; OnExecute:=PHPLib_Write;
    with Parameters.add do ParamType := tpString;
  end;
  Engine:=TPHPEngine.Create(nil);
  Engine.RegisterGlobals:=true;
  Engine.DLLFolder:=extractfilepath(GetDllFilename);
  Engine.OnLogMessage:=PHPLogMessage;
  Engine.OnScriptError:=PHPErrorEvent;
  Engine.HandleErrors:=true;
  Engine.StartupEngine;
  Undhelper.LuaState:=L;
end;

destructor TMyPHPEngine.Destroy;
begin
  PHPLibrary.Free;
  Engine.ShutdownAndWaitFor;  //PHPEngine.ShutdownEngine;
  Engine.Free;
end;

function method_setvar(l : PLua_State) : Integer; cdecl;  // TO FIX: not working...
var ht : TUndPHPScriptWrapper;
begin
  ht:=TUndPHPScriptWrapper(LuaToTLuaObject(l, 1));
  try
  ht.obj.Variables.ByName(pchar(lua_tostring(L,2))).value:=PAnsiChar(lua_tostring(L,3));
  except end;
  result := 1;
end;

function method_getvar(l : PLua_State) : Integer; cdecl;// TO FIX: not working...
var ht : TUndPHPScriptWrapper;  s:string;
begin
  ht:=TUndPHPScriptWrapper(LuaToTLuaObject(l, 1));
  try
  s:=ht.obj.Variables.ByName(PAnsiChar(lua_tostring(L,2))).value;
  except
    //on E : Exception do writeln(E.ClassName+';'+E.Message);
  end;
  lua_pushstring(L, pchar(s));
  result := 1;
end;

function method_runfile(l : PLua_State) : Integer; cdecl;
var ht : TUndPHPScriptWrapper; //s:string; i : integer;
begin
  CreateUndPHPScriptEngine(L);
  ht:=TUndPHPScriptWrapper(LuaToTLuaObject(l, 1));
  ht.ErrorMsg:=emptystr;
  ht.output:=ht.obj.execute(PAnsiChar(lua_tostring(L,2)));
  ht.ErrorMsg:=FPHPEngine.ErrorMsg;
  ht.success:=(ht.ErrorMsg=emptystr);
  result := 1;
end;

function method_run(l : PLua_State) : Integer; cdecl;
var ht : TUndPHPScriptWrapper;
begin
  CreateUndPHPScriptEngine(L);
  ht:=TUndPHPScriptWrapper(LuaToTLuaObject(l, 1));
  ht.ErrorMsg:=emptystr;
  ht.output:=ht.obj.runcode(PAnsiChar(lua_tostring(L,2)));
  ht.ErrorMsg:=FPHPEngine.ErrorMsg;
  ht.success:=(ht.ErrorMsg=emptystr);
  result := 1;
end;

function GetLuaObject(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
begin
  result := TUndPHPScriptWrapper.Create(L, AParent);
end;

function NewLuaObject(L : PLua_State) : Integer; cdecl;
var p : TLuaObjectNewCallback;
begin
  p := @GetLuaObject;
  result := new_LuaObject(L, cObjectName, p);
end;

procedure RegisterObjectMethods(L : Plua_State; classTable : Integer);
begin
 RegisterMethod(L,'Run', @method_run, classTable);
 RegisterMethod(L,'RunFile', @method_runfile, classTable);
end;

procedure RegisterUndPHPScriptWrapper(L: Plua_State);
begin
  RegisterTLuaObject(L, cObjectName, @NewLuaObject, @RegisterObjectMethods);
end;

constructor TUndPHPScriptWrapper.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj := TpsvPHP.Create(nil);
  Success:=false;
end;

function TUndPHPScriptWrapper.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'ErrorMsg') = 0 then result := ErrorMsg else
  if CompareText(propName, 'Output') = 0 then result := OutPut else
  if CompareText(propName, 'Success') = 0 then result := Success else
  Result:=inherited GetPropValue(propName);
end;

function TUndPHPScriptWrapper.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  //if CompareText(propName, 'HandleErrors') = 0 then phpengine.HandleErrors := Boolean(AValue) else
  Result:=inherited SetPropValue(propName, AValue);
end;

destructor TUndPHPScriptWrapper.Destroy;
begin
  Obj.free;
  inherited Destroy;
end;

initialization

finalization
 if obj<>nil then obj.free;
 FPHPEngine.Free;

end.

