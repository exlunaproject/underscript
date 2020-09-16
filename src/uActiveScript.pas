unit uActiveScript;
{
 UnderScript JavaScript, VBScript & other ActiveScript Wrappers
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)

 ToDo: Needs cleanup
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, ActiveX, CatJSRunnerAS, UndHelperUnit,
  CatStrings, UndImporter, UndConst, TypInfo;

const
 cObjectName:string='RActiveScript';
 cDefaultLanguage:string='JavaScript';

type
  TUndScriptWrapper = class(TLuaObject)
  private
    obj:TScarlettActiveScript;
    hasReadOnlyLang:boolean;
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

procedure RegisterUndScriptWrapper(L : Plua_State);
//procedure RegisterScriptFunctions(L : Plua_State);
function Script_Run(L: plua_State;Script,Lang:string;iseval:boolean=false):integer;
procedure PrepareLanguage(Importer:TUndImporter;Lang:string);
function JavaScript_Run(L: plua_State):integer; cdecl;
function PerlScript_Run(L: plua_State):integer; cdecl;
function VBScript_Run(L: plua_State):integer; cdecl;

implementation

type
 TLanguage = (LuaScript,PerlScript,JavaScript,VBScript);
procedure PrepareLanguage(Importer:TUndImporter;Lang:string);
const
 debug='';//'Und.WriteLn("reading %k");d=new Date();';
 debugend='';//'Und.WriteLn("%k loaded as:"+%k);';
begin
  //Importer.EnableDebug:=true;
  Importer.EnableImport:=true;
  case TLanguage(GetEnumValue(TypeInfo(TLanguage), lang)) of
    JavaScript: begin
    importer.FuncReadFormat:=debug+'%k = %l.GetL("%k");'+debugend;
    importer.FuncWriteFormat:=crlf+'%l.SetL("%k",%k);';
    end;
    LuaScript:begin
    importer.FuncReadFormat:='%k = %l:GetL("%k")'+crlf;
    importer.FuncWriteFormat:=crlf+'%l:SetL("%k",%k)';
    end;
    PerlScript:begin
    importer.FuncReadFormat:='$%k = $%l->GetL("%k");';
    importer.FuncWriteFormat:=crlf+'$%l->SetL("%k",$%k);';
    end;
    VBScript:begin
    importer.FuncReadFormat:='%k = %l.GetL("%k")'+crlf;
    importer.FuncWriteFormat:=crlf+'%l.SetL "%k",%k';
    end;
  else
   // language not found, disables import of variables
   Importer.EnableImport:=false;
  end;
end;

// JavaScript Functions
function JavaScript_Run(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),cDefaultLanguage);
end;
function JavaScript_Eval(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),cDefaultLanguage,true);
end;
// VBScript Functions
function VBScript_Run(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),'VBScript');
end;
function VBScript_Eval(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),'VBScript',true);
end;
// PerlScript Functions
function PerlScript_Run(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),'PerlScript');
end;
function PerlScript_Eval(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),'PerlScript',true);
end;
// LuaScript Functions
function LuaScript_Run(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),'LuaScript');
end;
function LuaScript_Eval(L: plua_State):integer; cdecl;
begin
 result:=Script_Run(L,lua_tostring(L,1),'LuaScript',true);
end;
// ActiveScript Functions // eg:  Und.ActiveScript.Run('LuaScript','(someluascript...)')
function ActiveScript_Run(L: plua_State):integer; cdecl;
begin
 Script_Run(L,lua_tostring(L,2),lua_tostring(L,1)); result:=1;
end;
function ActiveScript_Eval(L: plua_State):integer; cdecl;
begin 
 Script_Run(L,lua_tostring(L,2),lua_tostring(L,1),true); result:=1;
end;

type
  TScriptErrorHandler = class
  public
   scriptlanguage:string;
   procedure ScriptError(Sender: TObject; Line, Pos: Integer; ASrc, ADescription: String);
  end;
procedure TScriptErrorHandler.ScriptError(Sender: TObject; Line, Pos: Integer; ASrc, ADescription: String);
begin
 Und_LogError(UndHelper.LuaState,line,scriptlanguage+': '+Adescription+' ['+Asrc+']');
 //errors.add(inttostr(line)+': '+Adescription+' ['+Asrc+']');
 if rudCustomFunc_LogError=emptystr then
 UndHelper.writeln(scriptlanguage+': '+inttostr(line)+': '+Adescription+' ['+Asrc+']');
end;

// Main Functions, important: not to be called directly
// runs and prints the output or errorinfo
function Script_Run(L: plua_State; Script, Lang:string; iseval:boolean=false):integer;
var
 obj:TScarlettActiveScript;
 r:string;
 W: TUndHelper;
 Importer: TUndImporter;
 ErrorHandler:TScriptErrorHandler;
begin
  CoInitialize(nil);
  ErrorHandler:=TScriptErrorHandler.create;
  importer:=TUndImporter.create(L);
  Importer.EnableDebug:=false;
  PrepareLanguage(Importer,lang);
  w:=TUndHelper.Create;
  w.LuaState:=L; // IMPORTANT!
  UndHelper.LuaState:=L; // IMPORTANT!
  obj := TScarlettActiveScript.Create(UndHelper);
  obj.asw.scriptlanguage:=cDefaultLanguage; //javascript
  obj.asw.OnError:=ErrorHandler.ScriptError;
  if lang<>emptystr then obj.asw.scriptlanguage:=Lang;
  errorhandler.scriptlanguage:=obj.asw.scriptlanguage;
  obj.asw.UseSafeSubset:=false;
  obj.asw.AddNamedItem(rudLibName, w);
  script:=lua_tostring(L,1);
  script:=importer.GetScript(L, script);
  importer.Free;
  if iseval then begin
    r:=obj.runexpression(script);
  end else begin
    obj.runscript(script);
  end;
  //if obj.asw_success =false then UndHelper.writeln(obj.asw.scriptlanguage+': '+obj.errors.Text);
  obj.Free;
  ErrorHandler.free;
  result:=1;
  CoUninitialize;
end;

function method_runexpression(l : PLua_State) : Integer; cdecl;
var ht : TUndScriptWrapper; r:string;
begin
  ht:=TUndScriptWrapper(LuaToTLuaObject(l, 1));
  try
  r:=ht.obj.runexpression(PAnsiChar(lua_tostring(L,2)));
  except end;
  lua_pushstring(L, pchar(r)); // result
  result := 1;
end;

function method_exec(l : PLua_State) : Integer; cdecl;
var ht : TUndScriptWrapper;
begin
  ht:=TUndScriptWrapper(LuaToTLuaObject(l, 1));
  try
  ht.obj.RunScript(PChar(lua_tostring(L,2)));
  except end;
  result := 1;
end;

function GetLuaObject(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
var o:TUndScriptWrapper;
begin
  o := TUndScriptWrapper.Create(L, AParent);
  o.hasReadOnlyLang:=false;
  result:=o;
end;

function GetLuaObject_JavaScript(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
var o:TUndScriptWrapper;
begin
  o := TUndScriptWrapper.Create(L, AParent);
  o.obj.asw.ScriptLanguage:='JavaScript';
  result:=o;
end;

function GetLuaObject_VBScript(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
var o:TUndScriptWrapper;
begin
  o := TUndScriptWrapper.Create(L, AParent);
  o.obj.asw.ScriptLanguage:='VBScript';
  result:=o;
end;

function GetLuaObject_PerlScript(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
var o:TUndScriptWrapper;
begin
  o := TUndScriptWrapper.Create(L, AParent);
  o.obj.asw.ScriptLanguage:='PerlScript';
  result:=o;
end;

function NewLuaObject(L : PLua_State) : Integer; cdecl;
var p : TLuaObjectNewCallback;
begin
  p := @GetLuaObject;
  result := new_LuaObject(L, cObjectName, p);
end;

procedure RegisterObjectMethods(L : Plua_State; classTable : Integer);
begin
 RegisterMethod(L,'Run', @method_exec, classTable);
 RegisterMethod(L,'Eval', @method_runexpression, classTable);
end;

procedure RegisterUndScriptWrapper(L: Plua_State);
const
 cObjectName_JS='RJavaScript';
 cObjectName_VB='RVBScript';
 cObjectName_Perl='RPerlScript';
 function NewLuaObject_JavaScript(L : PLua_State) : Integer; cdecl;
 var p : TLuaObjectNewCallback;
 begin
   p := @GetLuaObject_JavaScript;
   result := new_LuaObject(L, cObjectName_JS, p);
 end;
 function NewLuaObject_VBScript(L : PLua_State) : Integer; cdecl;
 var p : TLuaObjectNewCallback;
 begin
   p := @GetLuaObject_VBScript;
   result := new_LuaObject(L, cObjectName_VB, p);
 end;
 function NewLuaObject_PerlScript(L : PLua_State) : Integer; cdecl;
 var p : TLuaObjectNewCallback;
 begin
   p := @GetLuaObject_PerlScript;
   result := new_LuaObject(L, cObjectName_Perl, p);
 end;
begin
  RegisterTLuaObject(L, cObjectName, @NewLuaObject, @RegisterObjectMethods);
  RegisterTLuaObject(L, cObjectName_JS, @NewLuaObject_JavaScript, @RegisterObjectMethods);
  RegisterTLuaObject(L, cObjectName_VB, @NewLuaObject_VBScript, @RegisterObjectMethods);
  RegisterTLuaObject(L, cObjectName_Perl, @NewLuaObject_PerlScript, @RegisterObjectMethods);
  //RegisterScriptFunctions(L);
end;

{ TXCLObject }

constructor TUndScriptWrapper.Create(LuaState: PLua_State; AParent: TLuaObject);
var W : TUndHelper;//IDispatch;
begin
  inherited Create(LuaState, AParent);
  CoInitialize(nil);
  obj := TScarlettActiveScript.Create(AParent);
  obj.asw.scriptlanguage:=cDefaultLanguage;
  obj.asw.UseSafeSubset:=false;
  W := TUndHelper.Create;
  W.LuaState:=L;
  hasReadOnlyLang:=true;
  obj.asw.AddNamedItem(rudLibName, W);
end;

function TUndScriptWrapper.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'ErrorMsg') = 0 then result := obj.errors.Text else
  if CompareText(propName, 'Language') = 0 then result := obj.asw.ScriptLanguage else
  if CompareText(propName, 'Success') = 0 then result := obj.asw_success else
  //if CompareText(propName, 'Output') = 0 then result := GetResult else
  Result:=inherited GetPropValue(propName);
end;

function TUndScriptWrapper.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  if CompareText(propName, 'Language') = 0 then begin
   if hasReadOnlyLang=false then
   obj.asw.ScriptLanguage := AValue
  end else
  Result:=inherited SetPropValue(propName, AValue);
end;

destructor TUndScriptWrapper.Destroy;
begin
  obj.Free;
  CoUninitialize;
  inherited Destroy;
end;

end.

